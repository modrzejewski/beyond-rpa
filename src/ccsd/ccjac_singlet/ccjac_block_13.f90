module ccjac_block_13
use eom_cc3_13_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:22:40 UTC.
!
contains
 
subroutine ccjac_13(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
double precision, dimension(:,:), intent(out)       :: jac
double precision, dimension(:), intent(in)          :: eorb
double precision, dimension(:, :), intent(in)       :: t1
double precision, dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: bra0, ket0
!
! Local variables
!
integer :: a, b, c, d
integer :: i, j, k, l
integer :: ai, bj, ck, dl
integer :: i0, i1
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket
integer :: braoffset, ketoffset
integer :: qbj, qbj2
integer :: qck, qck2
integer :: q00
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
!
! Elementary loop 1
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, l
! Equalities: b == a, k == i
! No equalities independent of the above can hold.
!
d_aiajcidl: do d = nvirt0, nvirt1
l_aiajcidl: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aiajcidl: do c = d + 1, nvirt1
j_aiajcidl: do j = nocc0, nocc1
if (j == l) cycle j_aiajcidl
a_aiajcidl: do a = c + 1, nvirt1
if (a == d) cycle a_aiajcidl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidl: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aiajcidl
if (j > i .and. i > l) cycle i_aiajcidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiajcidl(eorb, t2, t1, nocc, nactive, a, i, j, c, d, l)
end do i_aiajcidl
end do a_aiajcidl
end do j_aiajcidl
end do c_aiajcidl
end do l_aiajcidl
end do d_aiajcidl
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, l == i
! No equalities independent of the above can hold.
!
d_aiajckdi: do d = nvirt0, nvirt1
c_aiajckdi: do c = d + 1, nvirt1
k_aiajckdi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdi: do j = nocc0, nocc1
if (j == k) cycle j_aiajckdi
a_aiajckdi: do a = c + 1, nvirt1
if (a == d) cycle a_aiajckdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdi: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aiajckdi
if (j > k .and. k > i) cycle i_aiajckdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiajckdi(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdi
end do a_aiajckdi
end do j_aiajckdi
end do k_aiajckdi
end do c_aiajckdi
end do d_aiajckdi
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k, l
! Equalities: b == a, j == i
! No equalities independent of the above can hold.
!
d_aiaickdl: do d = nvirt0, nvirt1
l_aiaickdl: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aiaickdl: do c = d + 1, nvirt1
k_aiaickdl: do k = nocc0, nocc1
if (k == l) cycle k_aiaickdl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a_aiaickdl: do a = c + 1, nvirt1
if (a == d) cycle a_aiaickdl
i_aiaickdl: do i = nocc0, nocc1
if (i == k .or. i == l) cycle i_aiaickdl
if (i > k .and. k > l) exit i_aiaickdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiaickdl(eorb, t2, t1, nocc, nactive, a, i, c, k, d, l)
end do i_aiaickdl
end do a_aiaickdl
end do k_aiaickdl
end do c_aiaickdl
end do l_aiaickdl
end do d_aiaickdl
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, k == i
! No equalities independent of the above can hold.
!
d_aibjaidl: do d = nvirt0, nvirt1
l_aibjaidl: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaidl: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjaidl
j_aibjaidl: do j = nocc0, nocc1
if (j == l) cycle j_aibjaidl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaidl: do a = d + 1, b - 1
i_aibjaidl: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjaidl
if (j > i .and. i > l) cycle i_aibjaidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjaidl(eorb, t2, t1, nocc, nactive, a, i, b, j, d, l)
end do i_aibjaidl
end do a_aibjaidl
end do j_aibjaidl
end do b_aibjaidl
end do l_aibjaidl
end do d_aibjaidl
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, l == i
! No equalities independent of the above can hold.
!
d_aibjakdi: do d = nvirt0, nvirt1
k_aibjakdi: do k = nocc0, nocc1
b_aibjakdi: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjakdi
j_aibjakdi: do j = nocc0, nocc1
if (j == k) cycle j_aibjakdi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakdi: do a = d + 1, b - 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdi: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakdi
if (j > k .and. k > i) cycle i_aibjakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjakdi(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjakdi
end do a_aibjakdi
end do j_aibjakdi
end do b_aibjakdi
end do k_aibjakdi
end do d_aibjakdi
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, l
! Equalities: c == a, j == i
! No equalities independent of the above can hold.
!
d_aibiakdl: do d = nvirt0, nvirt1
l_aibiakdl: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aibiakdl: do k = nocc0, nocc1
if (k == l) cycle k_aibiakdl
b_aibiakdl: do b = nvirt0, nvirt1
if (b == d) cycle b_aibiakdl
a_aibiakdl: do a = d + 1, b - 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdl: do i = nocc0, nocc1
if (i == k .or. i == l) cycle i_aibiakdl
if (i > k .and. k > l) exit i_aibiakdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibiakdl(eorb, t2, t1, nocc, nactive, a, i, b, k, d, l)
end do i_aibiakdl
end do a_aibiakdl
end do b_aibiakdl
end do k_aibiakdl
end do l_aibiakdl
end do d_aibiakdl
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjcial: do l = nocc0, nocc1
c_aibjcial: do c = nvirt0, nvirt1
b_aibjcial: do b = c + 1, nvirt1
j_aibjcial: do j = nocc0, nocc1
if (j == l) cycle j_aibjcial
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcial: do a = nvirt0, c - 1
if (a == b) cycle a_aibjcial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcial: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcial
if (j > i .and. i > l) cycle i_aibjcial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjcial(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcial
end do a_aibjcial
end do j_aibjcial
end do b_aibjcial
end do c_aibjcial
end do l_aibjcial
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, l == i
! No equalities independent of the above can hold.
!
c_aibjckai: do c = nvirt0, nvirt1
k_aibjckai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckai: do b = c + 1, nvirt1
j_aibjckai: do j = nocc0, nocc1
if (j == k) cycle j_aibjckai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckai: do a = nvirt0, c - 1
if (a == b) cycle a_aibjckai
i_aibjckai: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckai
if (j > k .and. k > i) cycle i_aibjckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjckai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end do i_aibjckai
end do a_aibjckai
end do j_aibjckai
end do b_aibjckai
end do k_aibjckai
end do c_aibjckai
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, l
! Equalities: d == a, j == i
! No equalities independent of the above can hold.
!
l_aibickal: do l = nocc0, nocc1
c_aibickal: do c = nvirt0, nvirt1
k_aibickal: do k = nocc0, nocc1
if (k == l) cycle k_aibickal
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickal: do b = c + 1, nvirt1
a_aibickal: do a = nvirt0, c - 1
if (a == b) cycle a_aibickal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibickal: do i = nocc0, nocc1
if (i == k .or. i == l) cycle i_aibickal
if (i > k .and. k > l) exit i_aibickal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibickal(eorb, t2, t1, nocc, nactive, a, i, b, c, k, l)
end do i_aibickal
end do a_aibickal
end do b_aibickal
end do k_aibickal
end do c_aibickal
end do l_aibickal
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, k == i
! No equalities independent of the above can hold.
!
d_aiajaidl: do d = nvirt0, nvirt1
l_aiajaidl: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajaidl: do j = nocc0, nocc1
if (j == l) cycle j_aiajaidl
a_aiajaidl: do a = d + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j, l)
i_aiajaidl: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiajaidl(eorb, t2, t1, nocc, nactive, a, i, j, d, l)
end do i_aiajaidl
end do a_aiajaidl
end do j_aiajaidl
end do l_aiajaidl
end do d_aiajaidl
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, l == i
! No equalities independent of the above can hold.
!
d_aiajakdi: do d = nvirt0, nvirt1
k_aiajakdi: do k = nocc0, nocc1
j_aiajakdi: do j = k + 1, nocc1
a_aiajakdi: do a = d + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajakdi: do i = k + 1, nocc1
if (i == j) cycle i_aiajakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiajakdi(eorb, t2, t1, nocc, nactive, a, i, j, k, d)
end do i_aiajakdi
end do a_aiajakdi
end do j_aiajakdi
end do k_aiajakdi
end do d_aiajakdi
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, k, l
! Equalities: b == a, c == a, j == i
! No equalities independent of the above can hold.
!
d_aiaiakdl: do d = nvirt0, nvirt1
l_aiaiakdl: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aiaiakdl: do k = nocc0, l - 1
a_aiaiakdl: do a = d + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiaiakdl: do i = k + 1, nocc1
if (i == l) cycle i_aiaiakdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiaiakdl(eorb, t2, t1, nocc, nactive, a, i, k, d, l)
end do i_aiaiakdl
end do a_aiaiakdl
end do k_aiaiakdl
end do l_aiaiakdl
end do d_aiaiakdl
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, k == i
! No equalities independent of the above can hold.
!
l_aibjbial: do l = nocc0, nocc1
b_aibjbial: do b = nvirt0, nvirt1
j_aibjbial: do j = nocc0, nocc1
if (j == l) cycle j_aibjbial
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbial: do a = nvirt0, b - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j, l)
i_aibjbial: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjbial(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjbial
end do a_aibjbial
end do j_aibjbial
end do b_aibjbial
end do l_aibjbial
!
! Elementary loop 14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, l == i
! No equalities independent of the above can hold.
!
k_aibjbkai: do k = nocc0, nocc1
b_aibjbkai: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkai: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkai: do a = nvirt0, b - 1
i_aibjbkai: do i = k + 1, nocc1
if (i == j) cycle i_aibjbkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjbkai(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkai
end do a_aibjbkai
end do j_aibjbkai
end do b_aibjbkai
end do k_aibjbkai
!
! Elementary loop 15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: d == a, c == b, j == i
! No equalities independent of the above can hold.
!
l_aibibkal: do l = nocc0, nocc1
k_aibibkal: do k = nocc0, l - 1
b_aibibkal: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkal: do a = nvirt0, b - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibibkal: do i = k + 1, nocc1
if (i == l) cycle i_aibibkal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibibkal(eorb, t2, t1, nocc, nactive, a, i, b, k, l)
end do i_aibibkal
end do a_aibibkal
end do b_aibibkal
end do k_aibibkal
end do l_aibibkal
!
! Elementary loop 16
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == c, k == i
! No equalities independent of the above can hold.
!
l_aiajcicl: do l = nocc0, nocc1
c_aiajcicl: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
j_aiajcicl: do j = nocc0, nocc1
if (j == l) cycle j_aiajcicl
a_aiajcicl: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(l, j)
i_aiajcicl: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiajcicl(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcicl
end do a_aiajcicl
end do j_aiajcicl
end do c_aiajcicl
end do l_aiajcicl
!
! Elementary loop 17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == c, l == i
! No equalities independent of the above can hold.
!
c_aiajckci: do c = nvirt0, nvirt1
k_aiajckci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckci: do j = nocc0, k - 1
a_aiajckci: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckci: do i = nocc0, k - 1
if (i == j) cycle i_aiajckci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiajckci(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckci
end do a_aiajckci
end do j_aiajckci
end do k_aiajckci
end do c_aiajckci
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == c, j == i
! No equalities independent of the above can hold.
!
l_aiaickcl: do l = nocc0, nocc1
c_aiaickcl: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
k_aiaickcl: do k = l + 1, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a_aiaickcl: do a = c + 1, nvirt1
i_aiaickcl: do i = nocc0, k - 1
if (i == l) cycle i_aiaickcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiaickcl(eorb, t2, t1, nocc, nactive, a, i, c, k, l)
end do i_aiaickcl
end do a_aiaickcl
end do k_aiaickcl
end do c_aiaickcl
end do l_aiaickcl
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aiajcidi: do d = nvirt0, nvirt1
c_aiajcidi: do c = d + 1, nvirt1
j_aiajcidi: do j = nocc0, nocc1
a_aiajcidi: do a = c + 1, nvirt1
if (a == d) cycle a_aiajcidi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidi: do i = nocc0, nocc1
if (i == j) cycle i_aiajcidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiajcidi(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcidi
end do a_aiajcidi
end do j_aiajcidi
end do c_aiajcidi
end do d_aiajcidi
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidj: do d = nvirt0, nvirt1
c_aiajcidj: do c = d + 1, nvirt1
j_aiajcidj: do j = nocc0, nocc1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcidj: do a = c + 1, nvirt1
if (a == d) cycle a_aiajcidj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidj: do i = nocc0, nocc1
if (i == j) cycle i_aiajcidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiajcidj(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcidj
end do a_aiajcidj
end do j_aiajcidj
end do c_aiajcidj
end do d_aiajcidj
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, j == i, l == k
! No equalities independent of the above can hold.
!
d_aiaickdk: do d = nvirt0, nvirt1
c_aiaickdk: do c = d + 1, nvirt1
k_aiaickdk: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
a_aiaickdk: do a = c + 1, nvirt1
if (a == d) cycle a_aiaickdk
i_aiaickdk: do i = nocc0, nocc1
if (i == k) cycle i_aiaickdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiaickdk(eorb, t2, t1, nocc, nactive, a, i, c, k, d)
end do i_aiaickdk
end do a_aiaickdk
end do k_aiaickdk
end do c_aiaickdk
end do d_aiaickdk
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aiaickdi: do d = nvirt0, nvirt1
c_aiaickdi: do c = d + 1, nvirt1
k_aiaickdi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a_aiaickdi: do a = c + 1, nvirt1
if (a == d) cycle a_aiaickdi
i_aiaickdi: do i = nocc0, nocc1
if (i == k) cycle i_aiaickdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiaickdi(eorb, t2, t1, nocc, nactive, a, i, c, k, d)
end do i_aiaickdi
end do a_aiaickdi
end do k_aiaickdi
end do c_aiaickdi
end do d_aiaickdi
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjaial: do l = nocc0, nocc1
b_aibjaial: do b = nvirt0, nvirt1
j_aibjaial: do j = nocc0, nocc1
if (j == l) cycle j_aibjaial
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaial: do a = nvirt0, b - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l, j)
i_aibjaial: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjaial(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjaial
end do a_aibjaial
end do j_aibjaial
end do b_aibjaial
end do l_aibjaial
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, l == i
! No equalities independent of the above can hold.
!
k_aibjakai: do k = nocc0, nocc1
b_aibjakai: do b = nvirt0, nvirt1
j_aibjakai: do j = nocc0, k - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakai: do a = nvirt0, b - 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakai: do i = nocc0, k - 1
if (i == j) cycle i_aibjakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjakai(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjakai
end do a_aibjakai
end do j_aibjakai
end do b_aibjakai
end do k_aibjakai
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == a, j == i
! No equalities independent of the above can hold.
!
l_aibiakal: do l = nocc0, nocc1
k_aibiakal: do k = l + 1, nocc1
b_aibiakal: do b = nvirt0, nvirt1
a_aibiakal: do a = nvirt0, b - 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibiakal: do i = nocc0, k - 1
if (i == l) cycle i_aibiakal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibiakal(eorb, t2, t1, nocc, nactive, a, i, b, k, l)
end do i_aibiakal
end do a_aibiakal
end do b_aibiakal
end do k_aibiakal
end do l_aibiakal
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjaidi: do d = nvirt0, nvirt1
b_aibjaidi: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjaidi
j_aibjaidi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaidi: do a = d + 1, b - 1
i_aibjaidi: do i = nocc0, nocc1
if (i == j) cycle i_aibjaidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjaidi(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjaidi
end do a_aibjaidi
end do j_aibjaidi
end do b_aibjaidi
end do d_aibjaidi
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj: do d = nvirt0, nvirt1
b_aibjaidj: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjaidj
j_aibjaidj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaidj: do a = d + 1, b - 1
i_aibjaidj: do i = nocc0, nocc1
if (i == j) cycle i_aibjaidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjaidj(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjaidj
end do a_aibjaidj
end do j_aibjaidj
end do b_aibjaidj
end do d_aibjaidj
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == k
! No equalities independent of the above can hold.
!
d_aibiakdk: do d = nvirt0, nvirt1
k_aibiakdk: do k = nocc0, nocc1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibiakdk: do b = nvirt0, nvirt1
if (b == d) cycle b_aibiakdk
a_aibiakdk: do a = d + 1, b - 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdk: do i = nocc0, nocc1
if (i == k) cycle i_aibiakdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibiakdk(eorb, t2, t1, nocc, nactive, a, i, b, k, d)
end do i_aibiakdk
end do a_aibiakdk
end do b_aibiakdk
end do k_aibiakdk
end do d_aibiakdk
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibiakdi: do d = nvirt0, nvirt1
k_aibiakdi: do k = nocc0, nocc1
b_aibiakdi: do b = nvirt0, nvirt1
if (b == d) cycle b_aibiakdi
a_aibiakdi: do a = d + 1, b - 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdi: do i = nocc0, nocc1
if (i == k) cycle i_aibiakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibiakdi(eorb, t2, t1, nocc, nactive, a, i, b, k, d)
end do i_aibiakdi
end do a_aibiakdi
end do b_aibiakdi
end do k_aibiakdi
end do d_aibiakdi
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjciai: do c = nvirt0, nvirt1
b_aibjciai: do b = c + 1, nvirt1
j_aibjciai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciai: do a = nvirt0, c - 1
if (a == b) cycle a_aibjciai
i_aibjciai: do i = nocc0, nocc1
if (i == j) cycle i_aibjciai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjciai(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciai
end do a_aibjciai
end do j_aibjciai
end do b_aibjciai
end do c_aibjciai
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciaj: do c = nvirt0, nvirt1
b_aibjciaj: do b = c + 1, nvirt1
j_aibjciaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciaj: do a = nvirt0, c - 1
if (a == b) cycle a_aibjciaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciaj: do i = nocc0, nocc1
if (i == j) cycle i_aibjciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjciaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciaj
end do a_aibjciaj
end do j_aibjciaj
end do b_aibjciaj
end do c_aibjciaj
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == k
! No equalities independent of the above can hold.
!
c_aibickak: do c = nvirt0, nvirt1
k_aibickak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickak: do b = c + 1, nvirt1
a_aibickak: do a = nvirt0, c - 1
if (a == b) cycle a_aibickak
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibickak: do i = nocc0, nocc1
if (i == k) cycle i_aibickak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibickak(eorb, t2, t1, nocc, nactive, a, i, b, c, k)
end do i_aibickak
end do a_aibickak
end do b_aibickak
end do k_aibickak
end do c_aibickak
!
! Elementary loop 33
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickai: do c = nvirt0, nvirt1
k_aibickai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickai: do b = c + 1, nvirt1
a_aibickai: do a = nvirt0, c - 1
if (a == b) cycle a_aibickai
i_aibickai: do i = nocc0, nocc1
if (i == k) cycle i_aibickai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibickai(eorb, t2, t1, nocc, nactive, a, i, b, c, k)
end do i_aibickai
end do a_aibickai
end do b_aibickai
end do k_aibickai
end do c_aibickai
!
! Elementary loop 34
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aiajaidi: do d = nvirt0, nvirt1
j_aiajaidi: do j = nocc0, nocc1
a_aiajaidi: do a = d + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajaidi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiajaidi(eorb, t2, t1, nocc, nactive, a, i, j, d)
end do i_aiajaidi
end do a_aiajaidi
end do j_aiajaidi
end do d_aiajaidi
!
! Elementary loop 35
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajaidj: do d = nvirt0, nvirt1
j_aiajaidj: do j = nocc0, nocc1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajaidj: do a = d + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajaidj: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiajaidj(eorb, t2, t1, nocc, nactive, a, i, j, d)
end do i_aiajaidj
end do a_aiajaidj
end do j_aiajaidj
end do d_aiajaidj
!
! Elementary loop 36
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, k
! Equalities: b == a, c == a, j == i, l == k
! No equalities independent of the above can hold.
!
d_aiaiakdk: do d = nvirt0, nvirt1
k_aiaiakdk: do k = nocc0, nocc1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
a_aiaiakdk: do a = d + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiaiakdk: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiaiakdk(eorb, t2, t1, nocc, nactive, a, i, k, d)
end do i_aiaiakdk
end do a_aiaiakdk
end do k_aiaiakdk
end do d_aiaiakdk
!
! Elementary loop 37
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, k
! Equalities: b == a, c == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aiaiakdi: do d = nvirt0, nvirt1
k_aiaiakdi: do k = nocc0, nocc1
a_aiaiakdi: do a = d + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiaiakdi: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiaiakdi(eorb, t2, t1, nocc, nactive, a, i, k, d)
end do i_aiaiakdi
end do a_aiaiakdi
end do k_aiaiakdi
end do d_aiaiakdi
!
! Elementary loop 38
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjbiai: do b = nvirt0, nvirt1
j_aibjbiai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiai: do a = nvirt0, b - 1
i_aibjbiai: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjbiai(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbiai
end do a_aibjbiai
end do j_aibjbiai
end do b_aibjbiai
!
! Elementary loop 39
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbiaj: do b = nvirt0, nvirt1
j_aibjbiaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiaj: do a = nvirt0, b - 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbiaj: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjbiaj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbiaj
end do a_aibjbiaj
end do j_aibjbiaj
end do b_aibjbiaj
!
! Elementary loop 40
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == k
! No equalities independent of the above can hold.
!
k_aibibkak: do k = nocc0, nocc1
b_aibibkak: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkak: do a = nvirt0, b - 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibibkak: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibibkak(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkak
end do a_aibibkak
end do b_aibibkak
end do k_aibibkak
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibibkai: do k = nocc0, nocc1
b_aibibkai: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkai: do a = nvirt0, b - 1
i_aibibkai: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibibkai(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkai
end do a_aibibkai
end do b_aibibkai
end do k_aibibkai
!
! Elementary loop 42
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, l
! Equalities: b == a, d == c, j == i, k == i
! No equalities independent of the above can hold.
!
l_aiaicicl: do l = nocc0, nocc1
c_aiaicicl: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
a_aiaicicl: do a = c + 1, nvirt1
i_aiaicicl: do i = l + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiaicicl(eorb, t2, t1, nocc, nactive, a, i, c, l)
end do i_aiaicicl
end do a_aiaicicl
end do c_aiaicicl
end do l_aiaicicl
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajcicj: do c = nvirt0, nvirt1
j_aiajcicj: do j = nocc0, nocc1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcicj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcicj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiajcicj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcicj
end do a_aiajcicj
end do j_aiajcicj
end do c_aiajcicj
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, l == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjci: do c = nvirt0, nvirt1
j_aiajcjci: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjci: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjci: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiajcjci(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjci
end do a_aiajcjci
end do j_aiajcjci
end do c_aiajcjci
!
! Elementary loop 45
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == c, j == i, l == i
! No equalities independent of the above can hold.
!
c_aiaickci: do c = nvirt0, nvirt1
k_aiaickci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a_aiaickci: do a = c + 1, nvirt1
i_aiaickci: do i = nocc0, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aiaickci(eorb, t2, t1, nocc, nactive, a, i, c, k)
end do i_aiaickci
end do a_aiaickci
end do k_aiaickci
end do c_aiaickci
!
! Elementary loop 46
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == a, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibiaial: do l = nocc0, nocc1
b_aibiaial: do b = nvirt0, nvirt1
a_aibiaial: do a = nvirt0, b - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibiaial: do i = l + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibiaial(eorb, t2, t1, nocc, nactive, a, i, b, l)
end do i_aibiaial
end do a_aibiaial
end do b_aibiaial
end do l_aibiaial
!
! Elementary loop 47
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaiaj: do b = nvirt0, nvirt1
j_aibjaiaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaiaj: do a = nvirt0, b - 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjaiaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjaiaj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjaiaj
end do a_aibjaiaj
end do j_aibjaiaj
end do b_aibjaiaj
!
! Elementary loop 48
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajai: do b = nvirt0, nvirt1
j_aibjajai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajai: do a = nvirt0, b - 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajai: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibjajai(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjajai
end do a_aibjajai
end do j_aibjajai
end do b_aibjajai
!
! Elementary loop 49
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == a, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibiakai: do k = nocc0, nocc1
b_aibiakai: do b = nvirt0, nvirt1
a_aibiakai: do a = nvirt0, b - 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakai: do i = nocc0, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + mu3(bj, ck, dl)
jac(ibra, iket) = eom_cc3_13_trans_aibiakai(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibiakai
end do a_aibiakai
end do b_aibiakai
end do k_aibiakai
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
end subroutine ccjac_13
end module ccjac_block_13
