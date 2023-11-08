module ccjac_block_31_part1
use eom_cc3_31_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:36:33 UTC.
!
contains
 
subroutine ccjac_31_part1(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k, l
! Equalities: d == b
! No equalities independent of the above can hold.
!
l_aibjckbl: do l = nocc0, nocc1
c_aibjckbl: do c = nvirt0, nvirt1
k_aibjckbl: do k = nocc0, nocc1
if (k == l) cycle k_aibjckbl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbl: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjckbl: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjckbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbl: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbl
i_aibjckbl: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjckbl
if (i > j .and. j > k) exit i_aibjckbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckbl
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckbl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckbl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckbl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckbl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckbl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
end if
end do i_aibjckbl
end do a_aibjckbl
end do j_aibjckbl
end do b_aibjckbl
end do k_aibjckbl
end do c_aibjckbl
end do l_aibjckbl
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, l
! Equalities: d == a
! No equalities independent of the above can hold.
!
l_aibjckal: do l = nocc0, nocc1
c_aibjckal: do c = nvirt0, nvirt1
k_aibjckal: do k = nocc0, nocc1
if (k == l) cycle k_aibjckal
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckal: do b = c + 1, nvirt1
j_aibjckal: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjckal
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckal: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckal: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjckal
if (i > j .and. j > k) exit i_aibjckal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckal
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckal(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckal(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckal(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckal(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckal(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
end if
end do i_aibjckal
end do a_aibjckal
end do j_aibjckal
end do b_aibjckal
end do k_aibjckal
end do c_aibjckal
end do l_aibjckal
!
! Elementary loop 3
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, k, l
! Equalities: d == c
! No equalities independent of the above can hold.
!
l_aibjckcl: do l = nocc0, nocc1
c_aibjckcl: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
k_aibjckcl: do k = nocc0, nocc1
if (k == l) cycle k_aibjckcl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckcl: do b = c + 1, nvirt1
j_aibjckcl: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjckcl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcl: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckcl
i_aibjckcl: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjckcl
if (i > j .and. j > k) exit i_aibjckcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckcl
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckcl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckcl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckcl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckcl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckcl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
end if
end do i_aibjckcl
end do a_aibjckcl
end do j_aibjckcl
end do b_aibjckcl
end do k_aibjckcl
end do c_aibjckcl
end do l_aibjckcl
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: k, i, j
! Equalities: l == k
! No equalities independent of the above can hold.
!
d_aibjckdk: do d = nvirt0, nvirt1
c_aibjckdk: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdk
k_aibjckdk: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdk: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdk
j_aibjckdk: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdk: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdk
i_aibjckdk: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdk
if (i > j .and. j > k) exit i_aibjckdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckdk
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckdk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckdk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckdk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckdk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckdk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdk
end do a_aibjckdk
end do j_aibjckdk
end do b_aibjckdk
end do k_aibjckdk
end do c_aibjckdk
end do d_aibjckdk
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, i, k
! Equalities: l == j
! No equalities independent of the above can hold.
!
d_aibjckdj: do d = nvirt0, nvirt1
c_aibjckdj: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdj
k_aibjckdj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdj: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdj
j_aibjckdj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdj
i_aibjckdj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdj
if (i > j .and. j > k) exit i_aibjckdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckdj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckdj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckdj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckdj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckdj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckdj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdj
end do a_aibjckdj
end do j_aibjckdj
end do b_aibjckdj
end do k_aibjckdj
end do c_aibjckdj
end do d_aibjckdj
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, k
! Equalities: l == i
! No equalities independent of the above can hold.
!
d_aibjckdi: do d = nvirt0, nvirt1
c_aibjckdi: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdi
k_aibjckdi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdi: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdi
j_aibjckdi: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdi: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdi
i_aibjckdi: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdi
if (i > j .and. j > k) exit i_aibjckdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckdi
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckdi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckdi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckdi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckdi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckdi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdi
end do a_aibjckdi
end do j_aibjckdi
end do b_aibjckdi
end do k_aibjckdi
end do c_aibjckdi
end do d_aibjckdi
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, l
! Equalities: b == a, d == a
! No equalities independent of the above can hold.
!
l_aiajckal: do l = nocc0, nocc1
c_aiajckal: do c = nvirt0, nvirt1
k_aiajckal: do k = nocc0, nocc1
if (k == l) cycle k_aiajckal
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckal: do j = nocc0, k - 1
if (j == l) cycle j_aiajckal
a_aiajckal: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajckal: do i = j + 1, nocc1
if (i == k .or. i == l) cycle i_aiajckal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajckal(eorb, t2, t1, nocc, nactive, a, i, j, c, k, l)
end do i_aiajckal
end do a_aiajckal
end do j_aiajckal
end do k_aiajckal
end do c_aiajckal
end do l_aiajckal
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, l
! Equalities: b == a, d == c
! No equalities independent of the above can hold.
!
l_aiajckcl: do l = nocc0, nocc1
c_aiajckcl: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
k_aiajckcl: do k = nocc0, nocc1
if (k == l) cycle k_aiajckcl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckcl: do j = nocc0, k - 1
if (j == l) cycle j_aiajckcl
a_aiajckcl: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckcl: do i = j + 1, nocc1
if (i == k .or. i == l) cycle i_aiajckcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajckcl(eorb, t2, t1, nocc, nactive, a, i, j, c, k, l)
end do i_aiajckcl
end do a_aiajckcl
end do j_aiajckcl
end do k_aiajckcl
end do c_aiajckcl
end do l_aiajckcl
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: k, i, j
! Equalities: b == a, l == k
! No equalities independent of the above can hold.
!
d_aiajckdk: do d = nvirt0, nvirt1
c_aiajckdk: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajckdk
k_aiajckdk: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdk: do j = nocc0, k - 1
a_aiajckdk: do a = c + 1, nvirt1
if (a == d) cycle a_aiajckdk
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdk: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajckdk(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdk
end do a_aiajckdk
end do j_aiajckdk
end do k_aiajckdk
end do c_aiajckdk
end do d_aiajckdk
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, i, k
! Equalities: b == a, l == j
! No equalities independent of the above can hold.
!
d_aiajckdj: do d = nvirt0, nvirt1
c_aiajckdj: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajckdj
k_aiajckdj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdj: do j = nocc0, k - 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckdj: do a = c + 1, nvirt1
if (a == d) cycle a_aiajckdj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajckdj(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdj
end do a_aiajckdj
end do j_aiajckdj
end do k_aiajckdj
end do c_aiajckdj
end do d_aiajckdj
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, l == i
! No equalities independent of the above can hold.
!
d_aiajckdi: do d = nvirt0, nvirt1
c_aiajckdi: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajckdi
k_aiajckdi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdi: do j = nocc0, k - 1
a_aiajckdi: do a = c + 1, nvirt1
if (a == d) cycle a_aiajckdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdi: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajckdi(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdi
end do a_aiajckdi
end do j_aiajckdi
end do k_aiajckdi
end do c_aiajckdi
end do d_aiajckdi
!
! Elementary loop 12
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k, l
! Equalities: c == b, d == b
! No equalities independent of the above can hold.
!
l_aibjbkbl: do l = nocc0, nocc1
k_aibjbkbl: do k = nocc0, nocc1
if (k == l) cycle k_aibjbkbl
b_aibjbkbl: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjbkbl: do j = k + 1, nocc1
if (j == l) cycle j_aibjbkbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbl: do a = b + 1, nvirt1
i_aibjbkbl: do i = nocc0, j - 1
if (i == k .or. i == l) cycle i_aibjbkbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbkbl(eorb, t2, t1, nocc, nactive, a, i, b, j, k, l)
end do i_aibjbkbl
end do a_aibjbkbl
end do j_aibjbkbl
end do b_aibjbkbl
end do k_aibjbkbl
end do l_aibjbkbl
!
! Elementary loop 13
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, l
! Equalities: d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjcjbl: do l = nocc0, nocc1
c_aibjcjbl: do c = nvirt0, nvirt1
b_aibjcjbl: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcjbl: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbl: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbl
i_aibjcjbl: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcjbl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjbl
end do a_aibjcjbl
end do j_aibjcjbl
end do b_aibjcjbl
end do c_aibjcjbl
end do l_aibjcjbl
!
! Elementary loop 14
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjcibl: do l = nocc0, nocc1
c_aibjcibl: do c = nvirt0, nvirt1
b_aibjcibl: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcibl: do j = nocc0, nocc1
if (j == l) cycle j_aibjcibl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibl: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibl
i_aibjcibl: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcibl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcibl
end do a_aibjcibl
end do j_aibjcibl
end do b_aibjcibl
end do c_aibjcibl
end do l_aibjcibl
!
! Elementary loop 15
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: k, i, j
! Equalities: d == b, l == k
! No equalities independent of the above can hold.
!
c_aibjckbk: do c = nvirt0, nvirt1
k_aibjckbk: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbk: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbk: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbk: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbk
i_aibjckbk: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbk
if (i > j .and. j > k) exit i_aibjckbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckbk
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbk
end do a_aibjckbk
end do j_aibjckbk
end do b_aibjckbk
end do k_aibjckbk
end do c_aibjckbk
!
! Elementary loop 16
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, k
! Equalities: d == b, l == j
! No equalities independent of the above can hold.
!
c_aibjckbj: do c = nvirt0, nvirt1
k_aibjckbj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbj: do b = c + 1, nvirt1
j_aibjckbj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbj
i_aibjckbj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbj
if (i > j .and. j > k) exit i_aibjckbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckbj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbj
end do a_aibjckbj
end do j_aibjckbj
end do b_aibjckbj
end do k_aibjckbj
end do c_aibjckbj
!
! Elementary loop 17
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, l == i
! No equalities independent of the above can hold.
!
c_aibjckbi: do c = nvirt0, nvirt1
k_aibjckbi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbi: do b = c + 1, nvirt1
j_aibjckbi: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbi
i_aibjckbi: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbi
if (i > j .and. j > k) exit i_aibjckbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckbi
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbi
end do a_aibjckbi
end do j_aibjckbi
end do b_aibjckbi
end do k_aibjckbi
end do c_aibjckbi
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: d == a, c == b
! No equalities independent of the above can hold.
!
l_aibjbkal: do l = nocc0, nocc1
k_aibjbkal: do k = nocc0, nocc1
if (k == l) cycle k_aibjbkal
b_aibjbkal: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkal: do j = k + 1, nocc1
if (j == l) cycle j_aibjbkal
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkal: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjbkal: do i = nocc0, j - 1
if (i == k .or. i == l) cycle i_aibjbkal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbkal(eorb, t2, t1, nocc, nactive, a, i, b, j, k, l)
end do i_aibjbkal
end do a_aibjbkal
end do j_aibjbkal
end do b_aibjbkal
end do k_aibjbkal
end do l_aibjbkal
!
! Elementary loop 19
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: k, i, j
! Equalities: c == b, l == k
! No equalities independent of the above can hold.
!
d_aibjbkdk: do d = nvirt0, nvirt1
k_aibjbkdk: do k = nocc0, nocc1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkdk: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjbkdk
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdk: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdk: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbkdk
i_aibjbkdk: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbkdk(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdk
end do a_aibjbkdk
end do j_aibjbkdk
end do b_aibjbkdk
end do k_aibjbkdk
end do d_aibjbkdk
!
! Elementary loop 20
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, k
! Equalities: c == b, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdj: do d = nvirt0, nvirt1
k_aibjbkdj: do k = nocc0, nocc1
b_aibjbkdj: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjbkdj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdj: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdj: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbkdj
i_aibjbkdj: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbkdj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdj
end do a_aibjbkdj
end do j_aibjbkdj
end do b_aibjbkdj
end do k_aibjbkdj
end do d_aibjbkdj
!
! Elementary loop 21
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, l == i
! No equalities independent of the above can hold.
!
d_aibjbkdi: do d = nvirt0, nvirt1
k_aibjbkdi: do k = nocc0, nocc1
b_aibjbkdi: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjbkdi
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdi: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbkdi
i_aibjbkdi: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbkdi(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdi
end do a_aibjbkdi
end do j_aibjbkdi
end do b_aibjbkdi
end do k_aibjbkdi
end do d_aibjbkdi
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, k == j
! No equalities independent of the above can hold.
!
l_aibjcjal: do l = nocc0, nocc1
c_aibjcjal: do c = nvirt0, nvirt1
b_aibjcjal: do b = c + 1, nvirt1
j_aibjcjal: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjal
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjal: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjal: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcjal(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjal
end do a_aibjcjal
end do j_aibjcjal
end do b_aibjcjal
end do c_aibjcjal
end do l_aibjcjal
!
! Elementary loop 23
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
a_aibjcial: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcial: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcial(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcial
end do a_aibjcial
end do j_aibjcial
end do b_aibjcial
end do c_aibjcial
end do l_aibjcial
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j
! Equalities: d == a, l == k
! No equalities independent of the above can hold.
!
c_aibjckak: do c = nvirt0, nvirt1
k_aibjckak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckak: do b = c + 1, nvirt1
j_aibjckak: do j = nocc0, nocc1
if (j == k) cycle j_aibjckak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckak: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckak
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckak: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckak
if (i > j .and. j > k) exit i_aibjckak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckak
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckak
end do a_aibjckak
end do j_aibjckak
end do b_aibjckak
end do k_aibjckak
end do c_aibjckak
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: d == a, l == j
! No equalities independent of the above can hold.
!
c_aibjckaj: do c = nvirt0, nvirt1
k_aibjckaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaj: do b = c + 1, nvirt1
j_aibjckaj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckaj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckaj
if (i > j .and. j > k) exit i_aibjckaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckaj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckaj
end do a_aibjckaj
end do j_aibjckaj
end do b_aibjckaj
end do k_aibjckaj
end do c_aibjckaj
!
! Elementary loop 26
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
a_aibjckai: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckai
i_aibjckai: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckai
if (i > j .and. j > k) exit i_aibjckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckai
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckai
end do a_aibjckai
end do j_aibjckai
end do b_aibjckai
end do k_aibjckai
end do c_aibjckai
!
! Elementary loop 27
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, i, l
! Equalities: d == c, k == j
! No equalities independent of the above can hold.
!
l_aibjcjcl: do l = nocc0, nocc1
c_aibjcjcl: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b_aibjcjcl: do b = c + 1, nvirt1
j_aibjcjcl: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjcl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjcl: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjcl
i_aibjcjcl: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcjcl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjcl
end do a_aibjcjcl
end do j_aibjcjcl
end do b_aibjcjcl
end do c_aibjcjcl
end do l_aibjcjcl
!
! Elementary loop 28
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, l
! Equalities: d == c, k == i
! No equalities independent of the above can hold.
!
l_aibjcicl: do l = nocc0, nocc1
c_aibjcicl: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b_aibjcicl: do b = c + 1, nvirt1
j_aibjcicl: do j = nocc0, nocc1
if (j == l) cycle j_aibjcicl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicl: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcicl
i_aibjcicl: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcicl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcicl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcicl
end do a_aibjcicl
end do j_aibjcicl
end do b_aibjcicl
end do c_aibjcicl
end do l_aibjcicl
!
! Elementary loop 29
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: k, i, j
! Equalities: d == c, l == k
! No equalities independent of the above can hold.
!
c_aibjckck: do c = nvirt0, nvirt1
k_aibjckck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckck: do b = c + 1, nvirt1
j_aibjckck: do j = nocc0, nocc1
if (j == k) cycle j_aibjckck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckck: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckck
i_aibjckck: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckck
if (i > j .and. j > k) exit i_aibjckck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckck
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckck
end do a_aibjckck
end do j_aibjckck
end do b_aibjckck
end do k_aibjckck
end do c_aibjckck
!
! Elementary loop 30
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, i, k
! Equalities: d == c, l == j
! No equalities independent of the above can hold.
!
c_aibjckcj: do c = nvirt0, nvirt1
k_aibjckcj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckcj: do b = c + 1, nvirt1
j_aibjckcj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckcj
i_aibjckcj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckcj
if (i > j .and. j > k) exit i_aibjckcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckcj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckcj
end do a_aibjckcj
end do j_aibjckcj
end do b_aibjckcj
end do k_aibjckcj
end do c_aibjckcj
!
! Elementary loop 31
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, k
! Equalities: d == c, l == i
! No equalities independent of the above can hold.
!
c_aibjckci: do c = nvirt0, nvirt1
k_aibjckci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckci: do b = c + 1, nvirt1
j_aibjckci: do j = nocc0, nocc1
if (j == k) cycle j_aibjckci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckci
i_aibjckci: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckci
if (i > j .and. j > k) exit i_aibjckci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
if (i > j) then
      if (j > k) then
            exit i_aibjckci
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_31_trans_aibjckci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_31_trans_aibjckci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_31_trans_aibjckci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_31_trans_aibjckci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_31_trans_aibjckci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckci
end do a_aibjckci
end do j_aibjckci
end do b_aibjckci
end do k_aibjckci
end do c_aibjckci
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, i
! Equalities: k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjcjdj: do d = nvirt0, nvirt1
c_aibjcjdj: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcjdj
b_aibjcjdj: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcjdj
j_aibjcjdj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcjdj
i_aibjcjdj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcjdj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdj
end do a_aibjcjdj
end do j_aibjcjdj
end do b_aibjcjdj
end do c_aibjcjdj
end do d_aibjcjdj
!
! Elementary loop 33
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdi: do d = nvirt0, nvirt1
c_aibjcjdi: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcjdi
b_aibjcjdi: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcjdi
j_aibjcjdi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdi: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcjdi
i_aibjcjdi: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcjdi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdi
end do a_aibjcjdi
end do j_aibjcjdi
end do b_aibjcjdi
end do c_aibjcjdi
end do d_aibjcjdi
!
! Elementary loop 34
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjcidi: do d = nvirt0, nvirt1
c_aibjcidi: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcidi
b_aibjcidi: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcidi
j_aibjcidi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidi: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcidi
i_aibjcidi: do i = nocc0, nocc1
if (i == j) cycle i_aibjcidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcidi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcidi
end do a_aibjcidi
end do j_aibjcidi
end do b_aibjcidi
end do c_aibjcidi
end do d_aibjcidi
!
! Elementary loop 35
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidj: do d = nvirt0, nvirt1
c_aibjcidj: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcidj
b_aibjcidj: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcidj
j_aibjcidj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcidj
i_aibjcidj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcidj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcidj
end do a_aibjcidj
end do j_aibjcidj
end do b_aibjcidj
end do c_aibjcidj
end do d_aibjcidj
!
! Elementary loop 36
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aiajcjal: do l = nocc0, nocc1
c_aiajcjal: do c = nvirt0, nvirt1
j_aiajcjal: do j = nocc0, nocc1
if (j == l) cycle j_aiajcjal
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjal: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajcjal: do i = j + 1, nocc1
if (i == l) cycle i_aiajcjal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aiajcjal(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcjal
end do a_aiajcjal
end do j_aiajcjal
end do c_aiajcjal
end do l_aiajcjal
!
! Elementary loop 37
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajcial: do l = nocc0, nocc1
c_aiajcial: do c = nvirt0, nvirt1
j_aiajcial: do j = nocc0, nocc1
if (j == l) cycle j_aiajcial
a_aiajcial: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajcial: do i = j + 1, nocc1
if (i == l) cycle i_aiajcial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajcial(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcial
end do a_aiajcial
end do j_aiajcial
end do c_aiajcial
end do l_aiajcial
!
! Elementary loop 38
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j
! Equalities: b == a, d == a, l == k
! No equalities independent of the above can hold.
!
c_aiajckak: do c = nvirt0, nvirt1
k_aiajckak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckak: do j = nocc0, k - 1
a_aiajckak: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckak: do i = j + 1, nocc1
if (i == k) cycle i_aiajckak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajckak(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckak
end do a_aiajckak
end do j_aiajckak
end do k_aiajckak
end do c_aiajckak
!
! Elementary loop 39
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == a, l == j
! No equalities independent of the above can hold.
!
c_aiajckaj: do c = nvirt0, nvirt1
k_aiajckaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaj: do j = nocc0, k - 1
a_aiajckaj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajckaj(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckaj
end do a_aiajckaj
end do j_aiajckaj
end do k_aiajckaj
end do c_aiajckaj
!
! Elementary loop 40
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, l == i
! No equalities independent of the above can hold.
!
c_aiajckai: do c = nvirt0, nvirt1
k_aiajckai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckai: do j = nocc0, k - 1
a_aiajckai: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckai: do i = j + 1, nocc1
if (i == k) cycle i_aiajckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajckai(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckai
end do a_aiajckai
end do j_aiajckai
end do k_aiajckai
end do c_aiajckai
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == c, k == j
! No equalities independent of the above can hold.
!
l_aiajcjcl: do l = nocc0, nocc1
c_aiajcjcl: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
j_aiajcjcl: do j = nocc0, nocc1
if (j == l) cycle j_aiajcjcl
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjcl: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjcl: do i = j + 1, nocc1
if (i == l) cycle i_aiajcjcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aiajcjcl(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcjcl
end do a_aiajcjcl
end do j_aiajcjcl
end do c_aiajcjcl
end do l_aiajcjcl
!
! Elementary loop 42
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
i_aiajcicl: do i = j + 1, nocc1
if (i == l) cycle i_aiajcicl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajcicl(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcicl
end do a_aiajcicl
end do j_aiajcicl
end do c_aiajcicl
end do l_aiajcicl
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j
! Equalities: b == a, d == c, l == k
! No equalities independent of the above can hold.
!
c_aiajckck: do c = nvirt0, nvirt1
k_aiajckck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckck: do j = nocc0, k - 1
a_aiajckck: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckck: do i = j + 1, nocc1
if (i == k) cycle i_aiajckck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajckck(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckck
end do a_aiajckck
end do j_aiajckck
end do k_aiajckck
end do c_aiajckck
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == c, l == j
! No equalities independent of the above can hold.
!
c_aiajckcj: do c = nvirt0, nvirt1
k_aiajckcj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckcj: do j = nocc0, k - 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckcj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckcj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajckcj(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckcj
end do a_aiajckcj
end do j_aiajckcj
end do k_aiajckcj
end do c_aiajckcj
!
! Elementary loop 45
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
i_aiajckci: do i = j + 1, nocc1
if (i == k) cycle i_aiajckci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajckci(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckci
end do a_aiajckci
end do j_aiajckci
end do k_aiajckci
end do c_aiajckci
!
! Elementary loop 46
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, i
! Equalities: b == a, k == j, l == j
! No equalities independent of the above can hold.
!
d_aiajcjdj: do d = nvirt0, nvirt1
c_aiajcjdj: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajcjdj
j_aiajcjdj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjdj: do a = c + 1, nvirt1
if (a == d) cycle a_aiajcjdj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjdj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aiajcjdj(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcjdj
end do a_aiajcjdj
end do j_aiajcjdj
end do c_aiajcjdj
end do d_aiajcjdj
!
! Elementary loop 47
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aiajcjdi: do d = nvirt0, nvirt1
c_aiajcjdi: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajcjdi
j_aiajcjdi: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjdi: do a = c + 1, nvirt1
if (a == d) cycle a_aiajcjdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjdi: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aiajcjdi(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcjdi
end do a_aiajcjdi
end do j_aiajcjdi
end do c_aiajcjdi
end do d_aiajcjdi
!
! Elementary loop 48
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aiajcidi: do d = nvirt0, nvirt1
c_aiajcidi: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajcidi
j_aiajcidi: do j = nocc0, nocc1
a_aiajcidi: do a = c + 1, nvirt1
if (a == d) cycle a_aiajcidi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidi: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajcidi(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcidi
end do a_aiajcidi
end do j_aiajcidi
end do c_aiajcidi
end do d_aiajcidi
!
! Elementary loop 49
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidj: do d = nvirt0, nvirt1
c_aiajcidj: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajcidj
j_aiajcidj: do j = nocc0, nocc1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcidj: do a = c + 1, nvirt1
if (a == d) cycle a_aiajcidj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajcidj(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcidj
end do a_aiajcidj
end do j_aiajcidj
end do c_aiajcidj
end do d_aiajcidj
!
! Elementary loop 50
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjbibl: do l = nocc0, nocc1
b_aibjbibl: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjbibl: do j = nocc0, nocc1
if (j == l) cycle j_aibjbibl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbibl: do a = b + 1, nvirt1
i_aibjbibl: do i = nocc0, j - 1
if (i == l) cycle i_aibjbibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbibl(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjbibl
end do a_aibjbibl
end do j_aibjbibl
end do b_aibjbibl
end do l_aibjbibl
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
end subroutine ccjac_31_part1
end module ccjac_block_31_part1
