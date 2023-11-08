module ccjac_block_32_part2
use eom_cc3_32_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:41:38 UTC.
!
contains
 
subroutine ccjac_32_part2(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: a, b, c, d, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, i0, j1
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
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, k
! Equalities: e == b, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjckdjbi: do d = nvirt0, nvirt1
c_aibjckdjbi: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdjbi
k_aibjckdjbi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdjbi: do b = c + 1, d - 1
j_aibjckdjbi: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdjbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdjbi: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdjbi
i_aibjckdjbi: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdjbi
if (i > j .and. j > k) exit i_aibjckdjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdjbi
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckdjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckdjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckdjbi
end do a_aibjckdjbi
end do j_aibjckdjbi
end do b_aibjckdjbi
end do k_aibjckdjbi
end do c_aibjckdjbi
end do d_aibjckdjbi
!
! Elementary loop 2
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, k, j
! Equalities: e == b, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibjckdkbi: do d = nvirt0, nvirt1
c_aibjckdkbi: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdkbi
k_aibjckdkbi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdkbi: do b = c + 1, d - 1
j_aibjckdkbi: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdkbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdkbi: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdkbi
i_aibjckdkbi: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdkbi
if (i > j .and. j > k) exit i_aibjckdkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdkbi
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckdkbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckdkbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckdkbi
end do a_aibjckdkbi
end do j_aibjckdkbi
end do b_aibjckdkbi
end do k_aibjckdkbi
end do c_aibjckdkbi
end do d_aibjckdkbi
!
! Elementary loop 3
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: j, k, i
! Equalities: e == b, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjckdjbk: do d = nvirt0, nvirt1
c_aibjckdjbk: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdjbk
k_aibjckdjbk: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdjbk: do b = c + 1, d - 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckdjbk: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdjbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdjbk: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdjbk
i_aibjckdjbk: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdjbk
if (i > j .and. j > k) exit i_aibjckdjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdjbk
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckdjbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckdjbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdjbk
end do a_aibjckdjbk
end do j_aibjckdjbk
end do b_aibjckdjbk
end do k_aibjckdjbk
end do c_aibjckdjbk
end do d_aibjckdjbk
!
! Elementary loop 4
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: j, k, i
! Equalities: e == b, m == j, l == k
! No equalities independent of the above can hold.
!
d_aibjckdkbj: do d = nvirt0, nvirt1
c_aibjckdkbj: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdkbj
k_aibjckdkbj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdkbj: do b = c + 1, d - 1
j_aibjckdkbj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdkbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdkbj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdkbj
i_aibjckdkbj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdkbj
if (i > j .and. j > k) exit i_aibjckdkbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdkbj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckdkbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckdkbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckdkbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckdkbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckdkbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdkbj
end do a_aibjckdkbj
end do j_aibjckdkbj
end do b_aibjckdkbj
end do k_aibjckdkbj
end do c_aibjckdkbj
end do d_aibjckdkbj
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, m
! Equalities: b == a, d == a, e == a, l == i
! No equalities independent of the above can hold.
!
m_aiajckaiam: do m = nocc0, nocc1
c_aiajckaiam: do c = nvirt0, nvirt1
k_aiajckaiam: do k = nocc0, nocc1
if (k == m) cycle k_aiajckaiam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiam: do j = nocc0, k - 1
if (j == m) cycle j_aiajckaiam
a_aiajckaiam: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j, m)
i_aiajckaiam: do i = i0 + 1, nocc1
if (i == k) cycle i_aiajckaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckaiam(eorb, t2, t1, nocc, nactive, a, i, j, c, k, m)
end do i_aiajckaiam
end do a_aiajckaiam
end do j_aiajckaiam
end do k_aiajckaiam
end do c_aiajckaiam
end do m_aiajckaiam
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, l
! Equalities: b == a, d == a, e == a, m == i
! No equalities independent of the above can hold.
!
l_aiajckalai: do l = nocc0, nocc1
c_aiajckalai: do c = nvirt0, nvirt1
k_aiajckalai: do k = nocc0, nocc1
if (k == l) cycle k_aiajckalai
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckalai: do j = nocc0, k - 1
if (j == l) cycle j_aiajckalai
a_aiajckalai: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajckalai: do i = j + 1, l - 1
if (i == k) cycle i_aiajckalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckalai(eorb, t2, t1, nocc, nactive, a, i, j, c, k, l)
end do i_aiajckalai
end do a_aiajckalai
end do j_aiajckalai
end do k_aiajckalai
end do c_aiajckalai
end do l_aiajckalai
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, m
! Equalities: b == a, d == a, e == a, l == j
! No equalities independent of the above can hold.
!
m_aiajckajam: do m = nocc0, nocc1
c_aiajckajam: do c = nvirt0, nvirt1
k_aiajckajam: do k = nocc0, nocc1
if (k == m) cycle k_aiajckajam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckajam: do j = m + 1, k - 1
a_aiajckajam: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajckajam: do i = j + 1, nocc1
if (i == k .or. i == m) cycle i_aiajckajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckajam(eorb, t2, t1, nocc, nactive, a, i, j, c, k, m)
end do i_aiajckajam
end do a_aiajckajam
end do j_aiajckajam
end do k_aiajckajam
end do c_aiajckajam
end do m_aiajckajam
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, l
! Equalities: b == a, d == a, e == a, m == j
! No equalities independent of the above can hold.
!
l_aiajckalaj: do l = nocc0, nocc1
c_aiajckalaj: do c = nvirt0, nvirt1
k_aiajckalaj: do k = nocc0, nocc1
if (k == l) cycle k_aiajckalaj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(l, k)
j_aiajckalaj: do j = nocc0, j1 - 1
a_aiajckalaj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckalaj: do i = j + 1, nocc1
if (i == k .or. i == l) cycle i_aiajckalaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckalaj(eorb, t2, t1, nocc, nactive, a, i, j, c, k, l)
end do i_aiajckalaj
end do a_aiajckalaj
end do j_aiajckalaj
end do k_aiajckalaj
end do c_aiajckalaj
end do l_aiajckalaj
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j, m
! Equalities: b == a, d == a, e == a, l == k
! No equalities independent of the above can hold.
!
m_aiajckakam: do m = nocc0, nocc1
c_aiajckakam: do c = nvirt0, nvirt1
k_aiajckakam: do k = m + 1, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckakam: do j = nocc0, k - 1
if (j == m) cycle j_aiajckakam
a_aiajckakam: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajckakam: do i = j + 1, nocc1
if (i == k .or. i == m) cycle i_aiajckakam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckakam(eorb, t2, t1, nocc, nactive, a, i, j, c, k, m)
end do i_aiajckakam
end do a_aiajckakam
end do j_aiajckakam
end do k_aiajckakam
end do c_aiajckakam
end do m_aiajckakam
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j, l
! Equalities: b == a, d == a, e == a, m == k
! No equalities independent of the above can hold.
!
l_aiajckalak: do l = nocc0, nocc1
c_aiajckalak: do c = nvirt0, nvirt1
k_aiajckalak: do k = nocc0, l - 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckalak: do j = nocc0, k - 1
if (j == l) cycle j_aiajckalak
a_aiajckalak: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckalak: do i = j + 1, nocc1
if (i == k .or. i == l) cycle i_aiajckalak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckalak(eorb, t2, t1, nocc, nactive, a, i, j, c, k, l)
end do i_aiajckalak
end do a_aiajckalak
end do j_aiajckalak
end do k_aiajckalak
end do c_aiajckalak
end do l_aiajckalak
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaiaj: do c = nvirt0, nvirt1
k_aibjckaiaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaiaj: do b = c + 1, nvirt1
j_aibjckaiaj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaiaj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckaiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckaiaj: do i = j + 1, nocc1
if (i == k) cycle i_aibjckaiaj
if (i > j .and. j > k) exit i_aibjckaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckaiaj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckaiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckaiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckaiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckaiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckaiaj
end do a_aibjckaiaj
end do j_aibjckaiaj
end do b_aibjckaiaj
end do k_aibjckaiaj
end do c_aibjckaiaj
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckaiak: do c = nvirt0, nvirt1
k_aibjckaiak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaiak: do b = c + 1, nvirt1
j_aibjckaiak: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaiak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaiak: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckaiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckaiak: do i = k + 1, nocc1
if (i == j) cycle i_aibjckaiak
if (i > j .and. j > k) exit i_aibjckaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckaiak
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckaiak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckaiak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckaiak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckaiak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckaiak
end do a_aibjckaiak
end do j_aibjckaiak
end do b_aibjckaiak
end do k_aibjckaiak
end do c_aibjckaiak
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == a, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckajai: do c = nvirt0, nvirt1
k_aibjckajai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajai: do b = c + 1, nvirt1
j_aibjckajai: do j = nocc0, nocc1
if (j == k) cycle j_aibjckajai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckajai: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckajai
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajai: do i = nocc0, j - 1
if (i == k) cycle i_aibjckajai
if (i > j .and. j > k) exit i_aibjckajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckajai
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckajai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckajai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckajai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckajai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckajai
end do a_aibjckajai
end do j_aibjckajai
end do b_aibjckajai
end do k_aibjckajai
end do c_aibjckajai
!
! Elementary loop 14
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckakai: do c = nvirt0, nvirt1
k_aibjckakai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakai: do b = c + 1, nvirt1
j_aibjckakai: do j = nocc0, nocc1
if (j == k) cycle j_aibjckakai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckakai: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckakai
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakai: do i = nocc0, k - 1
if (i == j) cycle i_aibjckakai
if (i > j .and. j > k) exit i_aibjckakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckakai
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckakai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckakai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckakai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckakai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckakai
end do a_aibjckakai
end do j_aibjckakai
end do b_aibjckakai
end do k_aibjckakai
end do c_aibjckakai
!
! Elementary loop 15
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: d == a, e == a, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckajak: do c = nvirt0, nvirt1
k_aibjckajak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajak: do b = c + 1, nvirt1
j_aibjckajak: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckajak: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckajak
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckajak: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckajak
if (i > j .and. j > k) exit i_aibjckajak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckajak
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckajak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckajak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckajak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckajak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckajak
end do a_aibjckajak
end do j_aibjckajak
end do b_aibjckajak
end do k_aibjckajak
end do c_aibjckajak
!
! Elementary loop 16
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: d == a, e == a, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckakaj: do c = nvirt0, nvirt1
k_aibjckakaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakaj: do b = c + 1, nvirt1
j_aibjckakaj: do j = nocc0, k - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckakaj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckakaj
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckakaj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckakaj
if (i > j .and. j > k) exit i_aibjckakaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckakaj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckakaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckakaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckakaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckakaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckakaj
end do a_aibjckakaj
end do j_aibjckakaj
end do b_aibjckakaj
end do k_aibjckakaj
end do c_aibjckakaj
!
! Elementary loop 17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, m
! Equalities: b == a, d == a, e == c, l == i
! No equalities independent of the above can hold.
!
m_aiajckaicm: do m = nocc0, nocc1
c_aiajckaicm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aiajckaicm: do k = nocc0, nocc1
if (k == m) cycle k_aiajckaicm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaicm: do j = nocc0, k - 1
if (j == m) cycle j_aiajckaicm
a_aiajckaicm: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaicm: do i = j + 1, nocc1
if (i == k .or. i == m) cycle i_aiajckaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckaicm(eorb, t2, t1, nocc, nactive, a, i, j, c, k, m)
end do i_aiajckaicm
end do a_aiajckaicm
end do j_aiajckaicm
end do k_aiajckaicm
end do c_aiajckaicm
end do m_aiajckaicm
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, m
! Equalities: b == a, d == a, e == c, l == j
! No equalities independent of the above can hold.
!
m_aiajckajcm: do m = nocc0, nocc1
c_aiajckajcm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aiajckajcm: do k = nocc0, nocc1
if (k == m) cycle k_aiajckajcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckajcm: do j = nocc0, k - 1
if (j == m) cycle j_aiajckajcm
a_aiajckajcm: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajcm: do i = j + 1, nocc1
if (i == k .or. i == m) cycle i_aiajckajcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckajcm(eorb, t2, t1, nocc, nactive, a, i, j, c, k, m)
end do i_aiajckajcm
end do a_aiajckajcm
end do j_aiajckajcm
end do k_aiajckajcm
end do c_aiajckajcm
end do m_aiajckajcm
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, l
! Equalities: b == a, d == a, e == c, m == j
! No equalities independent of the above can hold.
!
l_aiajckalcj: do l = nocc0, nocc1
c_aiajckalcj: do c = nvirt0, nvirt1
k_aiajckalcj: do k = nocc0, nocc1
if (k == l) cycle k_aiajckalcj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckalcj: do j = nocc0, k - 1
if (j == l) cycle j_aiajckalcj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckalcj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajckalcj: do i = j + 1, nocc1
if (i == k .or. i == l) cycle i_aiajckalcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckalcj(eorb, t2, t1, nocc, nactive, a, i, j, c, k, l)
end do i_aiajckalcj
end do a_aiajckalcj
end do j_aiajckalcj
end do k_aiajckalcj
end do c_aiajckalcj
end do l_aiajckalcj
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j, m
! Equalities: b == a, d == a, e == c, l == k
! No equalities independent of the above can hold.
!
m_aiajckakcm: do m = nocc0, nocc1
c_aiajckakcm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aiajckakcm: do k = nocc0, nocc1
if (k == m) cycle k_aiajckakcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckakcm: do j = nocc0, k - 1
if (j == m) cycle j_aiajckakcm
a_aiajckakcm: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakcm: do i = j + 1, nocc1
if (i == k .or. i == m) cycle i_aiajckakcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckakcm(eorb, t2, t1, nocc, nactive, a, i, j, c, k, m)
end do i_aiajckakcm
end do a_aiajckakcm
end do j_aiajckakcm
end do k_aiajckakcm
end do c_aiajckakcm
end do m_aiajckakcm
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j, l
! Equalities: b == a, d == a, e == c, m == k
! No equalities independent of the above can hold.
!
l_aiajckalck: do l = nocc0, nocc1
c_aiajckalck: do c = nvirt0, nvirt1
k_aiajckalck: do k = nocc0, nocc1
if (k == l) cycle k_aiajckalck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckalck: do j = nocc0, k - 1
if (j == l) cycle j_aiajckalck
a_aiajckalck: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajckalck: do i = j + 1, nocc1
if (i == k .or. i == l) cycle i_aiajckalck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckalck(eorb, t2, t1, nocc, nactive, a, i, j, c, k, l)
end do i_aiajckalck
end do a_aiajckalck
end do j_aiajckalck
end do k_aiajckalck
end do c_aiajckalck
end do l_aiajckalck
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajckaiej: do e = nvirt0, nvirt1
c_aiajckaiej: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajckaiej
k_aiajckaiej: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiej: do j = nocc0, k - 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c, e)
a_aiajckaiej: do a = a0 + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaiej: do i = j + 1, nocc1
if (i == k) cycle i_aiajckaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckaiej(eorb, t2, t1, nocc, nactive, a, i, j, c, k, e)
end do i_aiajckaiej
end do a_aiajckaiej
end do j_aiajckaiej
end do k_aiajckaiej
end do c_aiajckaiej
end do e_aiajckaiej
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, l == i, m == k
! No equalities independent of the above can hold.
!
e_aiajckaiek: do e = nvirt0, nvirt1
c_aiajckaiek: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajckaiek
k_aiajckaiek: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiek: do j = nocc0, k - 1
a0 = max(c, e)
a_aiajckaiek: do a = a0 + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaiek: do i = j + 1, nocc1
if (i == k) cycle i_aiajckaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckaiek(eorb, t2, t1, nocc, nactive, a, i, j, c, k, e)
end do i_aiajckaiek
end do a_aiajckaiek
end do j_aiajckaiek
end do k_aiajckaiek
end do c_aiajckaiek
end do e_aiajckaiek
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajckajei: do e = nvirt0, nvirt1
c_aiajckajei: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajckajei
k_aiajckajei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckajei: do j = nocc0, k - 1
a0 = max(c, e)
a_aiajckajei: do a = a0 + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajei: do i = j + 1, nocc1
if (i == k) cycle i_aiajckajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckajei(eorb, t2, t1, nocc, nactive, a, i, j, c, k, e)
end do i_aiajckajei
end do a_aiajckajei
end do j_aiajckajei
end do k_aiajckajei
end do c_aiajckajei
end do e_aiajckajei
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, m == i, l == k
! No equalities independent of the above can hold.
!
e_aiajckakei: do e = nvirt0, nvirt1
c_aiajckakei: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajckakei
k_aiajckakei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckakei: do j = nocc0, k - 1
a0 = max(c, e)
a_aiajckakei: do a = a0 + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakei: do i = j + 1, nocc1
if (i == k) cycle i_aiajckakei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckakei(eorb, t2, t1, nocc, nactive, a, i, j, c, k, e)
end do i_aiajckakei
end do a_aiajckakei
end do j_aiajckakei
end do k_aiajckakei
end do c_aiajckakei
end do e_aiajckakei
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, l == j, m == k
! No equalities independent of the above can hold.
!
e_aiajckajek: do e = nvirt0, nvirt1
c_aiajckajek: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajckajek
k_aiajckajek: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckajek: do j = nocc0, k - 1
a0 = max(c, e)
a_aiajckajek: do a = a0 + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajek: do i = j + 1, nocc1
if (i == k) cycle i_aiajckajek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckajek(eorb, t2, t1, nocc, nactive, a, i, j, c, k, e)
end do i_aiajckajek
end do a_aiajckajek
end do j_aiajckajek
end do k_aiajckajek
end do c_aiajckajek
end do e_aiajckajek
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, m == j, l == k
! No equalities independent of the above can hold.
!
e_aiajckakej: do e = nvirt0, nvirt1
c_aiajckakej: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajckakej
k_aiajckakej: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckakej: do j = nocc0, k - 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c, e)
a_aiajckakej: do a = a0 + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakej: do i = j + 1, nocc1
if (i == k) cycle i_aiajckakej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckakej(eorb, t2, t1, nocc, nactive, a, i, j, c, k, e)
end do i_aiajckakej
end do a_aiajckakej
end do j_aiajckakej
end do k_aiajckakej
end do c_aiajckakej
end do e_aiajckakej
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, m
! Equalities: d == a, c == b, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjbkaibm: do m = nocc0, nocc1
k_aibjbkaibm: do k = nocc0, nocc1
if (k == m) cycle k_aibjbkaibm
b_aibjbkaibm: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbkaibm: do j = k + 1, nocc1
if (j == m) cycle j_aibjbkaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkaibm: do a = b + 1, nvirt1
i_aibjbkaibm: do i = nocc0, j - 1
if (i == k .or. i == m) cycle i_aibjbkaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkaibm(eorb, t2, t1, nocc, nactive, a, i, b, j, k, m)
end do i_aibjbkaibm
end do a_aibjbkaibm
end do j_aibjbkaibm
end do b_aibjbkaibm
end do k_aibjbkaibm
end do m_aibjbkaibm
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: d == a, c == b, e == b, m == i
! No equalities independent of the above can hold.
!
l_aibjbkalbi: do l = nocc0, nocc1
k_aibjbkalbi: do k = nocc0, nocc1
if (k == l) cycle k_aibjbkalbi
b_aibjbkalbi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkalbi: do j = k + 1, nocc1
if (j == l) cycle j_aibjbkalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkalbi: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjbkalbi: do i = nocc0, j - 1
if (i == k .or. i == l) cycle i_aibjbkalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkalbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k, l)
end do i_aibjbkalbi
end do a_aibjbkalbi
end do j_aibjbkalbi
end do b_aibjbkalbi
end do k_aibjbkalbi
end do l_aibjbkalbi
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, m
! Equalities: d == a, c == b, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjbkajbm: do m = nocc0, nocc1
k_aibjbkajbm: do k = nocc0, nocc1
if (k == m) cycle k_aibjbkajbm
b_aibjbkajbm: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbkajbm: do j = k + 1, nocc1
if (j == m) cycle j_aibjbkajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkajbm: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbkajbm: do i = nocc0, j - 1
if (i == k .or. i == m) cycle i_aibjbkajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkajbm(eorb, t2, t1, nocc, nactive, a, i, b, j, k, m)
end do i_aibjbkajbm
end do a_aibjbkajbm
end do j_aibjbkajbm
end do b_aibjbkajbm
end do k_aibjbkajbm
end do m_aibjbkajbm
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, l
! Equalities: d == a, c == b, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjbkalbj: do l = nocc0, nocc1
k_aibjbkalbj: do k = nocc0, nocc1
if (k == l) cycle k_aibjbkalbj
b_aibjbkalbj: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkalbj: do j = k + 1, nocc1
if (j == l) cycle j_aibjbkalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkalbj: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjbkalbj: do i = nocc0, j - 1
if (i == k .or. i == l) cycle i_aibjbkalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkalbj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, l)
end do i_aibjbkalbj
end do a_aibjbkalbj
end do j_aibjbkalbj
end do b_aibjbkalbj
end do k_aibjbkalbj
end do l_aibjbkalbj
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j, l
! Equalities: d == a, c == b, e == b, m == k
! No equalities independent of the above can hold.
!
l_aibjbkalbk: do l = nocc0, nocc1
k_aibjbkalbk: do k = nocc0, nocc1
if (k == l) cycle k_aibjbkalbk
b_aibjbkalbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkalbk: do j = k + 1, nocc1
if (j == l) cycle j_aibjbkalbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkalbk: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjbkalbk: do i = nocc0, j - 1
if (i == k .or. i == l) cycle i_aibjbkalbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkalbk(eorb, t2, t1, nocc, nactive, a, i, b, j, k, l)
end do i_aibjbkalbk
end do a_aibjbkalbk
end do j_aibjbkalbk
end do b_aibjbkalbk
end do k_aibjbkalbk
end do l_aibjbkalbk
!
! Elementary loop 33
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, m
! Equalities: d == a, e == c, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjciaicm: do m = nocc0, nocc1
c_aibjciaicm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b_aibjciaicm: do b = c + 1, nvirt1
j_aibjciaicm: do j = nocc0, nocc1
if (j == m) cycle j_aibjciaicm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjciaicm: do a = a0 + 1, nvirt1
i_aibjciaicm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjciaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciaicm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjciaicm
end do a_aibjciaicm
end do j_aibjciaicm
end do b_aibjciaicm
end do c_aibjciaicm
end do m_aibjciaicm
!
! Elementary loop 34
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: d == a, e == c, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjckaici: do c = nvirt0, nvirt1
k_aibjckaici: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaici: do b = c + 1, nvirt1
j_aibjckaici: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaici
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckaici: do a = a0 + 1, nvirt1
i_aibjckaici: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckaici
if (i > j .and. j > k) exit i_aibjckaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckaici
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckaici(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckaici(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckaici(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckaici(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckaici
end do a_aibjckaici
end do j_aibjckaici
end do b_aibjckaici
end do k_aibjckaici
end do c_aibjckaici
!
! Elementary loop 35
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, m
! Equalities: d == a, e == c, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjaicm: do m = nocc0, nocc1
c_aibjcjaicm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b_aibjcjaicm: do b = c + 1, nvirt1
j_aibjcjaicm: do j = nocc0, nocc1
if (j == m) cycle j_aibjcjaicm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjcjaicm: do a = a0 + 1, nvirt1
i_aibjcjaicm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcjaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjaicm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcjaicm
end do a_aibjcjaicm
end do j_aibjcjaicm
end do b_aibjcjaicm
end do c_aibjcjaicm
end do m_aibjcjaicm
!
! Elementary loop 36
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: d == a, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaicj: do c = nvirt0, nvirt1
k_aibjckaicj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaicj: do b = c + 1, nvirt1
j_aibjckaicj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckaicj: do a = a0 + 1, nvirt1
i_aibjckaicj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckaicj
if (i > j .and. j > k) exit i_aibjckaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckaicj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckaicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckaicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckaicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckaicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckaicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckaicj
end do a_aibjckaicj
end do j_aibjckaicj
end do b_aibjckaicj
end do k_aibjckaicj
end do c_aibjckaicj
!
! Elementary loop 37
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k, j
! Equalities: d == a, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckaick: do c = nvirt0, nvirt1
k_aibjckaick: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaick: do b = c + 1, nvirt1
j_aibjckaick: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaick
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckaick: do a = a0 + 1, nvirt1
i_aibjckaick: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckaick
if (i > j .and. j > k) exit i_aibjckaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckaick
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckaick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckaick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckaick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckaick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckaick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckaick
end do a_aibjckaick
end do j_aibjckaick
end do b_aibjckaick
end do k_aibjckaick
end do c_aibjckaick
!
! Elementary loop 38
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, l
! Equalities: d == a, e == c, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjcialci: do l = nocc0, nocc1
c_aibjcialci: do c = nvirt0, nvirt1
b_aibjcialci: do b = c + 1, nvirt1
j_aibjcialci: do j = nocc0, nocc1
if (j == l) cycle j_aibjcialci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjcialci: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialci: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcialci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcialci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcialci
end do a_aibjcialci
end do j_aibjcialci
end do b_aibjcialci
end do c_aibjcialci
end do l_aibjcialci
!
! Elementary loop 39
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, l
! Equalities: d == a, e == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjcialcj: do l = nocc0, nocc1
c_aibjcialcj: do c = nvirt0, nvirt1
b_aibjcialcj: do b = c + 1, nvirt1
j_aibjcialcj: do j = nocc0, nocc1
if (j == l) cycle j_aibjcialcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjcialcj: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialcj: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcialcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcialcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcialcj
end do a_aibjcialcj
end do j_aibjcialcj
end do b_aibjcialcj
end do c_aibjcialcj
end do l_aibjcialcj
!
! Elementary loop 40
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: d == a, e == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckajci: do c = nvirt0, nvirt1
k_aibjckajci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajci: do b = c + 1, nvirt1
j_aibjckajci: do j = nocc0, nocc1
if (j == k) cycle j_aibjckajci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckajci: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajci: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckajci
if (i > j .and. j > k) exit i_aibjckajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckajci
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckajci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckajci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckajci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckajci
end do a_aibjckajci
end do j_aibjckajci
end do b_aibjckajci
end do k_aibjckajci
end do c_aibjckajci
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, l
! Equalities: d == a, e == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjalci: do l = nocc0, nocc1
c_aibjcjalci: do c = nvirt0, nvirt1
b_aibjcjalci: do b = c + 1, nvirt1
j_aibjcjalci: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjalci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjcjalci: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalci: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjalci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjalci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjalci
end do a_aibjcjalci
end do j_aibjcjalci
end do b_aibjcjalci
end do c_aibjcjalci
end do l_aibjcjalci
!
! Elementary loop 42
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k, j
! Equalities: d == a, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckakci: do c = nvirt0, nvirt1
k_aibjckakci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakci: do b = c + 1, nvirt1
j_aibjckakci: do j = nocc0, nocc1
if (j == k) cycle j_aibjckakci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckakci: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakci: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckakci
if (i > j .and. j > k) exit i_aibjckakci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckakci
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckakci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckakci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckakci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckakci
end do a_aibjckakci
end do j_aibjckakci
end do b_aibjckakci
end do k_aibjckakci
end do c_aibjckakci
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, m
! Equalities: d == a, e == c, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjcjajcm: do m = nocc0, nocc1
c_aibjcjajcm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b_aibjcjajcm: do b = c + 1, nvirt1
j_aibjcjajcm: do j = nocc0, nocc1
if (j == m) cycle j_aibjcjajcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjcjajcm: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajcm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcjajcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjajcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcjajcm
end do a_aibjcjajcm
end do j_aibjcjajcm
end do b_aibjcjajcm
end do c_aibjcjajcm
end do m_aibjcjajcm
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, k
! Equalities: d == a, e == c, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjckajcj: do c = nvirt0, nvirt1
k_aibjckajcj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajcj: do b = c + 1, nvirt1
j_aibjckajcj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckajcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckajcj: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajcj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckajcj
if (i > j .and. j > k) exit i_aibjckajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckajcj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckajcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckajcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckajcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckajcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckajcj
end do a_aibjckajcj
end do j_aibjckajcj
end do b_aibjckajcj
end do k_aibjckajcj
end do c_aibjckajcj
!
! Elementary loop 45
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, k, i
! Equalities: d == a, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckajck: do c = nvirt0, nvirt1
k_aibjckajck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajck: do b = c + 1, nvirt1
j_aibjckajck: do j = nocc0, nocc1
if (j == k) cycle j_aibjckajck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckajck: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajck: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckajck
if (i > j .and. j > k) exit i_aibjckajck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckajck
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckajck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckajck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckajck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckajck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckajck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckajck
end do a_aibjckajck
end do j_aibjckajck
end do b_aibjckajck
end do k_aibjckajck
end do c_aibjckajck
!
! Elementary loop 46
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, l
! Equalities: d == a, e == c, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjcjalcj: do l = nocc0, nocc1
c_aibjcjalcj: do c = nvirt0, nvirt1
b_aibjcjalcj: do b = c + 1, nvirt1
j_aibjcjalcj: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjalcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjcjalcj: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalcj: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjalcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjalcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjalcj
end do a_aibjcjalcj
end do j_aibjcjalcj
end do b_aibjcjalcj
end do c_aibjcjalcj
end do l_aibjcjalcj
!
! Elementary loop 47
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, k, i
! Equalities: d == a, e == c, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckakcj: do c = nvirt0, nvirt1
k_aibjckakcj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakcj: do b = c + 1, nvirt1
j_aibjckakcj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckakcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckakcj: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakcj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckakcj
if (i > j .and. j > k) exit i_aibjckakcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckakcj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckakcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckakcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckakcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckakcj
end do a_aibjckakcj
end do j_aibjckakcj
end do b_aibjckakcj
end do k_aibjckakcj
end do c_aibjckakcj
!
! Elementary loop 48
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: k, i, j
! Equalities: d == a, e == c, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibjckakck: do c = nvirt0, nvirt1
k_aibjckakck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakck: do b = c + 1, nvirt1
j_aibjckakck: do j = nocc0, nocc1
if (j == k) cycle j_aibjckakck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckakck: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakck: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckakck
if (i > j .and. j > k) exit i_aibjckakck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckakck
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckakck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckakck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckakck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckakck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckakck
end do a_aibjckakck
end do j_aibjckakck
end do b_aibjckakck
end do k_aibjckakck
end do c_aibjckakck
!
! Elementary loop 49
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbkaiej: do e = nvirt0, nvirt1
k_aibjbkaiej: do k = nocc0, nocc1
b_aibjbkaiej: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjbkaiej
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaiej: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjbkaiej: do a = a0 + 1, nvirt1
i_aibjbkaiej: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkaiej(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjbkaiej
end do a_aibjbkaiej
end do j_aibjbkaiej
end do b_aibjbkaiej
end do k_aibjbkaiej
end do e_aibjbkaiej
!
! Elementary loop 50
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k, j
! Equalities: d == a, c == b, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibjbkaiek: do e = nvirt0, nvirt1
k_aibjbkaiek: do k = nocc0, nocc1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkaiek: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjbkaiek
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaiek: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjbkaiek: do a = a0 + 1, nvirt1
i_aibjbkaiek: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkaiek(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjbkaiek
end do a_aibjbkaiek
end do j_aibjbkaiek
end do b_aibjbkaiek
end do k_aibjbkaiek
end do e_aibjbkaiek
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
end subroutine ccjac_32_part2
end module ccjac_block_32_part2
