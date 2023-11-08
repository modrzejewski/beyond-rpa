module ccjac_block_13_dav_part1
use eom_cc3_13_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2019-06-21 13:11:34 UTC.
!
contains
subroutine ccjac_13_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, a1, b0, c0, i0, i1, j0, j1, k0, k1
integer :: n0ab, n0abc, n0ac, n0acd, n0ad
integer :: n0bc, n0cd, n0ij, n0ijl, n0ik
integer :: n0ikl, n0il, n0jl, n0kl
integer :: n1ab, n1abc, n1ac, n1acd, n1ad
integer :: n1bc, n1cd, n1ij, n1ijl, n1ik
integer :: n1ikl, n1il, n1jl, n1kl
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket, iket2, ibra2
integer :: braoffset, ketoffset
integer :: qbj, qbj2
integer :: qck, qck2
integer :: q00
integer :: nj, nb, nk, nc, nl, nd
integer :: mj, mb, mk, mc, ml, md
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

nj = n1j - n0j + 1
nb = n1b - n0b + 1
nk = n1k - n0k + 1
nc = n1c - n0c + 1
nl = n1l - n0l + 1
nd = n1d - n0d + 1
mj = 1
mb = nj
mk = nj * nb
mc = nj * nb * nk
ml = nj * nb * nk * nc
md = nj * nb * nk * nc * nl

n0ab = max(n0a, n0b)
n0abc = max(n0a, n0b, n0c)
n0ac = max(n0a, n0c)
n0acd = max(n0a, n0c, n0d)
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0cd = max(n0c, n0d)
n0ij = max(n0i, n0j)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1ab = min(n1a, n1b)
n1abc = min(n1a, n1b, n1c)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1cd = min(n1c, n1d)
n1ij = min(n1i, n1j)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1il = min(n1i, n1l)
n1jl = min(n1j, n1l)
n1kl = min(n1k, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, l == i
! No equalities independent of the above can hold.
!
d_aiajckdi: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajckdi: do c = c0, n1c
if (c == d) cycle c_aiajckdi
k_aiajckdi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdi: do j = n0j, n1j
if (j == k) cycle j_aiajckdi
a0 = max(c + 1, n0ab)
a_aiajckdi: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdi: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aiajckdi
if (j > k .and. k > i) cycle i_aiajckdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, j, c, k, d, i)
jac_ibra_iket = eom_cc3_13_trans_aiajckdi(j, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdi
end do a_aiajckdi
end do j_aiajckdi
end do k_aiajckdi
end do c_aiajckdi
end do d_aiajckdi
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k, l
! Equalities: b == a, j == i
! No equalities independent of the above can hold.
!
d_aiaickdl: do d = n0d, n1d
l_aiaickdl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c0 = max(d + 1, n0c)
c_aiaickdl: do c = c0, n1c
if (c == d) cycle c_aiaickdl
k_aiaickdl: do k = n0k, n1k
if (k == l) cycle k_aiaickdl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickdl: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiaickdl
i_aiaickdl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aiaickdl
if (i > k .and. k > l) exit i_aiaickdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, i, c, k, d, l)
!print*, ketoffset, offset, iket2
jac_ibra_iket = eom_cc3_13_trans_aiaickdl(c, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickdl
end do a_aiaickdl
end do k_aiaickdl
end do c_aiaickdl
end do l_aiaickdl
end do d_aiaickdl
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, l
! Equalities: b == a, k == i
! No equalities independent of the above can hold.
!
d_aiajcidl: do d = n0d, n1d
l_aiajcidl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c0 = max(d + 1, n0c)
c_aiajcidl: do c = c0, n1c
if (c == d) cycle c_aiajcidl
j_aiajcidl: do j = n0j, n1j
if (j == l) cycle j_aiajcidl
a0 = max(c + 1, n0ab)
a_aiajcidl: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcidl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aiajcidl
if (j > i .and. i > l) cycle i_aiajcidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, j, c, i, d, l)
jac_ibra_iket = eom_cc3_13_trans_aiajcidl(j, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidl
end do a_aiajcidl
end do j_aiajcidl
end do c_aiajcidl
end do l_aiajcidl
end do d_aiajcidl
!
! Elementary loop  4
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
a0 = max(d + 1, n0ac)
a1 = min(b - 1, n1ac)
a_aibjakdi: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjakdi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdi: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakdi
if (j > k .and. k > i) cycle i_aibjakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, a, k, d, i)
jac_ibra_iket = eom_cc3_13_trans_aibjakdi(b, j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdi
end do a_aibjakdi
end do j_aibjakdi
end do b_aibjakdi
end do k_aibjakdi
end do d_aibjakdi
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, l
! Equalities: c == a, j == i
! No equalities independent of the above can hold.
!
d_aibiakdl: do d = n0d, n1d
l_aibiakdl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aibiakdl: do k = n0k, n1k
if (k == l) cycle k_aibiakdl
b_aibiakdl: do b = n0b, n1b
if (b == d) cycle b_aibiakdl
a0 = max(d + 1, n0ac)
a1 = min(b - 1, n1ac)
a_aibiakdl: do a = a0, a1
if (a == b .or. a == d) cycle a_aibiakdl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibiakdl
if (i > k .and. k > l) exit i_aibiakdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, i, a, k, d, l)
jac_ibra_iket = eom_cc3_13_trans_aibiakdl(b, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakdl
end do a_aibiakdl
end do b_aibiakdl
end do k_aibiakdl
end do l_aibiakdl
end do d_aibiakdl
!
! Elementary loop  6
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
a0 = max(d + 1, n0ac)
a1 = min(b - 1, n1ac)
a_aibjaidl: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjaidl
i_aibjaidl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaidl
if (j > i .and. i > l) cycle i_aibjaidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, a, i, d, l)
jac_ibra_iket = eom_cc3_13_trans_aibjaidl(b, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidl
end do a_aibjaidl
end do j_aibjaidl
end do b_aibjaidl
end do l_aibjaidl
end do d_aibjaidl
!
! Elementary loop  7
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
a1 = min(c - 1, n1ad)
a_aibjckai: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibjckai
i_aibjckai: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckai
if (j > k .and. k > i) cycle i_aibjckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, c, k, a, i)
jac_ibra_iket = eom_cc3_13_trans_aibjckai(b, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckai
end do a_aibjckai
end do j_aibjckai
end do b_aibjckai
end do k_aibjckai
end do c_aibjckai
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, l
! Equalities: d == a, j == i
! No equalities independent of the above can hold.
!
l_aibickal: do l = n0l, n1l
c_aibickal: do c = n0c, n1c
k_aibickal: do k = n0k, n1k
if (k == l) cycle k_aibickal
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickal: do b = b0, n1b
if (b == c) cycle b_aibickal
a1 = min(c - 1, n1ad)
a_aibickal: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibickal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibickal: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibickal
if (i > k .and. k > l) exit i_aibickal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, i, c, k, a, l)
jac_ibra_iket = eom_cc3_13_trans_aibickal(b, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickal
end do a_aibickal
end do b_aibickal
end do k_aibickal
end do c_aibickal
end do l_aibickal
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
b0 = max(c + 1, n0b)
b_aibjcial: do b = b0, n1b
if (b == c) cycle b_aibjcial
j_aibjcial: do j = n0j, n1j
if (j == l) cycle j_aibjcial
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ad)
a_aibjcial: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibjcial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcial: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcial
if (j > i .and. i > l) cycle i_aibjcial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, c, i, a, l)
jac_ibra_iket = eom_cc3_13_trans_aibjcial(b, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcial
end do a_aibjcial
end do j_aibjcial
end do b_aibjcial
end do c_aibjcial
end do l_aibjcial
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, l == i
! No equalities independent of the above can hold.
!
d_aiajakdi: do d = n0d, n1d
k_aiajakdi: do k = n0k, n1k
j0 = max(k + 1, n0j)
j_aiajakdi: do j = j0, n1j
if (j == k) cycle j_aiajakdi
a0 = max(d + 1, n0abc)
a_aiajakdi: do a = a0, n1abc
if (a == d) cycle a_aiajakdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0il)
i_aiajakdi: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, j, a, k, d, i)
jac_ibra_iket = eom_cc3_13_trans_aiajakdi(a, j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakdi
end do a_aiajakdi
end do j_aiajakdi
end do k_aiajakdi
end do d_aiajakdi
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, k, l
! Equalities: b == a, c == a, j == i
! No equalities independent of the above can hold.
!
d_aiaiakdl: do d = n0d, n1d
l_aiaiakdl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k1 = min(l - 1, n1k)
k_aiaiakdl: do k = n0k, k1
if (k == l) cycle k_aiaiakdl
a0 = max(d + 1, n0abc)
a_aiaiakdl: do a = a0, n1abc
if (a == d) cycle a_aiaiakdl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aiaiakdl: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aiaiakdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, i, a, k, d, l)
jac_ibra_iket = eom_cc3_13_trans_aiaiakdl(a, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiakdl
end do a_aiaiakdl
end do k_aiaiakdl
end do l_aiaiakdl
end do d_aiaiakdl
!
! Elementary loop  12
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
i1 = min(j - 1, l - 1, n1ik)
i_aiajaidl: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aiajaidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, j, a, i, d, l)
jac_ibra_iket = eom_cc3_13_trans_aiajaidl(a, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidl
end do a_aiajaidl
end do j_aiajaidl
end do l_aiajaidl
end do d_aiajaidl
!
! Elementary loop  13
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
a1 = min(b - 1, n1ad)
a_aibjbkai: do a = n0ad, a1
if (a == b) cycle a_aibjbkai
i0 = max(k + 1, n0il)
i_aibjbkai: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, b, k, a, i)
jac_ibra_iket = eom_cc3_13_trans_aibjbkai(b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkai
end do a_aibjbkai
end do j_aibjbkai
end do b_aibjbkai
end do k_aibjbkai
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: d == a, c == b, j == i
! No equalities independent of the above can hold.
!
l_aibibkal: do l = n0l, n1l
k1 = min(l - 1, n1k)
k_aibibkal: do k = n0k, k1
if (k == l) cycle k_aibibkal
b_aibibkal: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ad)
a_aibibkal: do a = n0ad, a1
if (a == b) cycle a_aibibkal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibibkal: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibibkal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, i, b, k, a, l)
jac_ibra_iket = eom_cc3_13_trans_aibibkal(b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkal
end do a_aibibkal
end do b_aibibkal
end do k_aibibkal
end do l_aibibkal
!
! Elementary loop  15
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
a1 = min(b - 1, n1ad)
a_aibjbial: do a = n0ad, a1
if (a == b) cycle a_aibjbial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, l - 1, n1ik)
i_aibjbial: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjbial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, b, i, a, l)
jac_ibra_iket = eom_cc3_13_trans_aibjbial(b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbial
end do a_aibjbial
end do j_aibjbial
end do b_aibjbial
end do l_aibjbial
!
! Elementary loop  16
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
i1 = min(k - 1, n1il)
i_aiajckci: do i = n0il, i1
if (i == j .or. i == k) cycle i_aiajckci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, j, c, k, c, i)
jac_ibra_iket = eom_cc3_13_trans_aiajckci(j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckci
end do a_aiajckci
end do j_aiajckci
end do k_aiajckci
end do c_aiajckci
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == c, j == i
! No equalities independent of the above can hold.
!
l_aiaickcl: do l = n0l, n1l
c_aiaickcl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
k0 = max(l + 1, n0k)
k_aiaickcl: do k = k0, n1k
if (k == l) cycle k_aiaickcl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickcl: do a = a0, n1ab
if (a == c) cycle a_aiaickcl
i1 = min(k - 1, n1ij)
i_aiaickcl: do i = n0ij, i1
if (i == k .or. i == l) cycle i_aiaickcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, i, c, k, c, l)
jac_ibra_iket = eom_cc3_13_trans_aiaickcl(c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickcl
end do a_aiaickcl
end do k_aiaickcl
end do c_aiaickcl
end do l_aiaickcl
!
! Elementary loop  18
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
i0 = max(l + 1, j + 1, n0ik)
i_aiajcicl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcicl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, j, c, i, c, l)
jac_ibra_iket = eom_cc3_13_trans_aiajcicl(j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicl
end do a_aiajcicl
end do j_aiajcicl
end do c_aiajcicl
end do l_aiajcicl
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aiaickdi: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiaickdi: do c = c0, n1c
if (c == d) cycle c_aiaickdi
k_aiaickdi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickdi: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiaickdi
i_aiaickdi: do i = n0ijl, n1ijl
if (i == k) cycle i_aiaickdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, i, c, k, d, i)
jac_ibra_iket = eom_cc3_13_trans_aiaickdi(i, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickdi
end do a_aiaickdi
end do k_aiaickdi
end do c_aiaickdi
end do d_aiaickdi
!
! Elementary loop  20
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
a0 = max(c + 1, n0ab)
a_aiajcidj: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcidj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidj: do i = n0ik, n1ik
if (i == j) cycle i_aiajcidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, j, c, i, d, j)
jac_ibra_iket = eom_cc3_13_trans_aiajcidj(j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidj
end do a_aiajcidj
end do j_aiajcidj
end do c_aiajcidj
end do d_aiajcidj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aiajcidi: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcidi: do c = c0, n1c
if (c == d) cycle c_aiajcidi
j_aiajcidi: do j = n0j, n1j
a0 = max(c + 1, n0ab)
a_aiajcidi: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcidi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidi: do i = n0ikl, n1ikl
if (i == j) cycle i_aiajcidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, j, c, i, d, i)
jac_ibra_iket = eom_cc3_13_trans_aiajcidi(i, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidi
end do a_aiajcidi
end do j_aiajcidi
end do c_aiajcidi
end do d_aiajcidi
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, j == i, l == k
! No equalities independent of the above can hold.
!
d_aiaickdk: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiaickdk: do c = c0, n1c
if (c == d) cycle c_aiaickdk
k_aiaickdk: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickdk: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiaickdk
i_aiaickdk: do i = n0ij, n1ij
if (i == k) cycle i_aiaickdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, i, c, k, d, k)
jac_ibra_iket = eom_cc3_13_trans_aiaickdk(c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickdk
end do a_aiaickdk
end do k_aiaickdk
end do c_aiaickdk
end do d_aiaickdk
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, l == i
! No equalities independent of the above can hold.
!
k_aibjakai: do k = n0k, n1k
b_aibjakai: do b = n0b, n1b
j1 = min(k - 1, n1j)
j_aibjakai: do j = n0j, j1
if (j == k) cycle j_aibjakai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acd)
a_aibjakai: do a = n0acd, a1
if (a == b) cycle a_aibjakai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1il)
i_aibjakai: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, a, k, a, i)
jac_ibra_iket = eom_cc3_13_trans_aibjakai(a, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakai
end do a_aibjakai
end do j_aibjakai
end do b_aibjakai
end do k_aibjakai
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == a, j == i
! No equalities independent of the above can hold.
!
l_aibiakal: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibiakal: do k = k0, n1k
if (k == l) cycle k_aibiakal
b_aibiakal: do b = n0b, n1b
a1 = min(b - 1, n1acd)
a_aibiakal: do a = n0acd, a1
if (a == b) cycle a_aibiakal
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(k - 1, n1ij)
i_aibiakal: do i = n0ij, i1
if (i == k .or. i == l) cycle i_aibiakal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, i, a, k, a, l)
jac_ibra_iket = eom_cc3_13_trans_aibiakal(a, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakal
end do a_aibiakal
end do b_aibiakal
end do k_aibiakal
end do l_aibiakal
!
! Elementary loop  25
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
a1 = min(b - 1, n1acd)
a_aibjaial: do a = n0acd, a1
if (a == b) cycle a_aibjaial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, j + 1, n0ik)
i_aibjaial: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, a, i, a, l)
jac_ibra_iket = eom_cc3_13_trans_aibjaial(a, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaial
end do a_aibjaial
end do j_aibjaial
end do b_aibjaial
end do l_aibjaial
!
! Elementary loop  26
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
a0 = max(d + 1, n0ac)
a1 = min(b - 1, n1ac)
a_aibiakdi: do a = a0, a1
if (a == b .or. a == d) cycle a_aibiakdi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdi: do i = n0ijl, n1ijl
if (i == k) cycle i_aibiakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, i, a, k, d, i)
jac_ibra_iket = eom_cc3_13_trans_aibiakdi(i, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakdi
end do a_aibiakdi
end do b_aibiakdi
end do k_aibiakdi
end do d_aibiakdi
!
! Elementary loop  27
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
a0 = max(d + 1, n0ac)
a1 = min(b - 1, n1ac)
a_aibjaidj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjaidj
i_aibjaidj: do i = n0ik, n1ik
if (i == j) cycle i_aibjaidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, a, i, d, j)
jac_ibra_iket = eom_cc3_13_trans_aibjaidj(b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidj
end do a_aibjaidj
end do j_aibjaidj
end do b_aibjaidj
end do d_aibjaidj
!
! Elementary loop  28
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
a0 = max(d + 1, n0ac)
a1 = min(b - 1, n1ac)
a_aibjaidi: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjaidi
i_aibjaidi: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjaidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, a, i, d, i)
jac_ibra_iket = eom_cc3_13_trans_aibjaidi(i, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidi
end do a_aibjaidi
end do j_aibjaidi
end do b_aibjaidi
end do d_aibjaidi
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == k
! No equalities independent of the above can hold.
!
d_aibiakdk: do d = n0d, n1d
k_aibiakdk: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibiakdk: do b = n0b, n1b
if (b == d) cycle b_aibiakdk
a0 = max(d + 1, n0ac)
a1 = min(b - 1, n1ac)
a_aibiakdk: do a = a0, a1
if (a == b .or. a == d) cycle a_aibiakdk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdk: do i = n0ij, n1ij
if (i == k) cycle i_aibiakdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, i, a, k, d, k)
jac_ibra_iket = eom_cc3_13_trans_aibiakdk(b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakdk
end do a_aibiakdk
end do b_aibiakdk
end do k_aibiakdk
end do d_aibiakdk
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickai: do c = n0c, n1c
k_aibickai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickai: do b = b0, n1b
if (b == c) cycle b_aibickai
a1 = min(c - 1, n1ad)
a_aibickai: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibickai
i_aibickai: do i = n0ijl, n1ijl
if (i == k) cycle i_aibickai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, i, c, k, a, i)
jac_ibra_iket = eom_cc3_13_trans_aibickai(i, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickai
end do a_aibickai
end do b_aibickai
end do k_aibickai
end do c_aibickai
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
function mu3_mem(b, j, c, k, d, l)
integer :: mu3_mem
integer, intent(in) :: d, l, b, j, c, k
mu3_mem = offset + (j-n0j)+mj + (b-n0b)*mb + (k-n0k)*mk + (c-n0c)*mc + (l-n0l)*ml  &
+ (d-n0d)*md
end function mu3_mem
end subroutine ccjac_13_dav_part1
end module ccjac_block_13_dav_part1
