module ccjac_block_32_part1
use eom_cc3_32_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:41:38 UTC.
!
contains
 
subroutine ccjac_32_part1(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: a0, b0
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
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k, m
! Equalities: d == a, e == c, l == i
! No equalities independent of the above can hold.
!
m_aibjckaicm: do m = nocc0, nocc1
c_aibjckaicm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckaicm: do k = nocc0, nocc1
if (k == m) cycle k_aibjckaicm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaicm: do b = c + 1, nvirt1
j_aibjckaicm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjckaicm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckaicm: do a = a0 + 1, nvirt1
i_aibjckaicm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjckaicm
if (i > j .and. j > k) exit i_aibjckaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckaicm
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckaicm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckaicm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckaicm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckaicm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckaicm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
end if
end do i_aibjckaicm
end do a_aibjckaicm
end do j_aibjckaicm
end do b_aibjckaicm
end do k_aibjckaicm
end do c_aibjckaicm
end do m_aibjckaicm
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k, l
! Equalities: d == a, e == c, m == i
! No equalities independent of the above can hold.
!
l_aibjckalci: do l = nocc0, nocc1
c_aibjckalci: do c = nvirt0, nvirt1
k_aibjckalci: do k = nocc0, nocc1
if (k == l) cycle k_aibjckalci
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckalci: do b = c + 1, nvirt1
j_aibjckalci: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjckalci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckalci: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalci: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjckalci
if (i > j .and. j > k) exit i_aibjckalci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckalci
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckalci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckalci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
end if
end do i_aibjckalci
end do a_aibjckalci
end do j_aibjckalci
end do b_aibjckalci
end do k_aibjckalci
end do c_aibjckalci
end do l_aibjckalci
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, k, m
! Equalities: d == a, e == c, l == j
! No equalities independent of the above can hold.
!
m_aibjckajcm: do m = nocc0, nocc1
c_aibjckajcm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckajcm: do k = nocc0, nocc1
if (k == m) cycle k_aibjckajcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajcm: do b = c + 1, nvirt1
j_aibjckajcm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjckajcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckajcm: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajcm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjckajcm
if (i > j .and. j > k) exit i_aibjckajcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckajcm
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckajcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckajcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
end if
end do i_aibjckajcm
end do a_aibjckajcm
end do j_aibjckajcm
end do b_aibjckajcm
end do k_aibjckajcm
end do c_aibjckajcm
end do m_aibjckajcm
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, k, l
! Equalities: d == a, e == c, m == j
! No equalities independent of the above can hold.
!
l_aibjckalcj: do l = nocc0, nocc1
c_aibjckalcj: do c = nvirt0, nvirt1
k_aibjckalcj: do k = nocc0, nocc1
if (k == l) cycle k_aibjckalcj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckalcj: do b = c + 1, nvirt1
j_aibjckalcj: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjckalcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckalcj: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalcj: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjckalcj
if (i > j .and. j > k) exit i_aibjckalcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckalcj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckalcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckalcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckalcj
end do a_aibjckalcj
end do j_aibjckalcj
end do b_aibjckalcj
end do k_aibjckalcj
end do c_aibjckalcj
end do l_aibjckalcj
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: k, i, j, m
! Equalities: d == a, e == c, l == k
! No equalities independent of the above can hold.
!
m_aibjckakcm: do m = nocc0, nocc1
c_aibjckakcm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckakcm: do k = nocc0, nocc1
if (k == m) cycle k_aibjckakcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakcm: do b = c + 1, nvirt1
j_aibjckakcm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjckakcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckakcm: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakcm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjckakcm
if (i > j .and. j > k) exit i_aibjckakcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckakcm
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckakcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckakcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckakcm
end do a_aibjckakcm
end do j_aibjckakcm
end do b_aibjckakcm
end do k_aibjckakcm
end do c_aibjckakcm
end do m_aibjckakcm
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: k, i, j, l
! Equalities: d == a, e == c, m == k
! No equalities independent of the above can hold.
!
l_aibjckalck: do l = nocc0, nocc1
c_aibjckalck: do c = nvirt0, nvirt1
k_aibjckalck: do k = nocc0, nocc1
if (k == l) cycle k_aibjckalck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckalck: do b = c + 1, nvirt1
j_aibjckalck: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjckalck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjckalck: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalck: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjckalck
if (i > j .and. j > k) exit i_aibjckalck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckalck
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckalck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckalck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckalck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckalck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckalck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
end if
end do i_aibjckalck
end do a_aibjckalck
end do j_aibjckalck
end do b_aibjckalck
end do k_aibjckalck
end do c_aibjckalck
end do l_aibjckalck
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, m
! Equalities: d == a, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjckaibm: do m = nocc0, nocc1
c_aibjckaibm: do c = nvirt0, nvirt1
k_aibjckaibm: do k = nocc0, nocc1
if (k == m) cycle k_aibjckaibm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibm: do b = c + 1, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjckaibm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjckaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaibm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckaibm
i_aibjckaibm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjckaibm
if (i > j .and. j > k) exit i_aibjckaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckaibm
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckaibm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckaibm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckaibm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckaibm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckaibm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
end if
end do i_aibjckaibm
end do a_aibjckaibm
end do j_aibjckaibm
end do b_aibjckaibm
end do k_aibjckaibm
end do c_aibjckaibm
end do m_aibjckaibm
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, l
! Equalities: d == a, e == b, m == i
! No equalities independent of the above can hold.
!
l_aibjckalbi: do l = nocc0, nocc1
c_aibjckalbi: do c = nvirt0, nvirt1
k_aibjckalbi: do k = nocc0, nocc1
if (k == l) cycle k_aibjckalbi
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckalbi: do b = c + 1, nvirt1
j_aibjckalbi: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjckalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckalbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckalbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalbi: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjckalbi
if (i > j .and. j > k) exit i_aibjckalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckalbi
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckalbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckalbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckalbi
end do a_aibjckalbi
end do j_aibjckalbi
end do b_aibjckalbi
end do k_aibjckalbi
end do c_aibjckalbi
end do l_aibjckalbi
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k, m
! Equalities: d == a, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjckajbm: do m = nocc0, nocc1
c_aibjckajbm: do c = nvirt0, nvirt1
k_aibjckajbm: do k = nocc0, nocc1
if (k == m) cycle k_aibjckajbm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajbm: do b = c + 1, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjckajbm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjckajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckajbm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajbm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjckajbm
if (i > j .and. j > k) exit i_aibjckajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckajbm
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckajbm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckajbm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
end if
end do i_aibjckajbm
end do a_aibjckajbm
end do j_aibjckajbm
end do b_aibjckajbm
end do k_aibjckajbm
end do c_aibjckajbm
end do m_aibjckajbm
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k, l
! Equalities: d == a, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjckalbj: do l = nocc0, nocc1
c_aibjckalbj: do c = nvirt0, nvirt1
k_aibjckalbj: do k = nocc0, nocc1
if (k == l) cycle k_aibjckalbj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckalbj: do b = c + 1, nvirt1
j_aibjckalbj: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjckalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckalbj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckalbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalbj: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjckalbj
if (i > j .and. j > k) exit i_aibjckalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckalbj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckalbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckalbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckalbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckalbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckalbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
end if
end do i_aibjckalbj
end do a_aibjckalbj
end do j_aibjckalbj
end do b_aibjckalbj
end do k_aibjckalbj
end do c_aibjckalbj
end do l_aibjckalbj
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j, m
! Equalities: d == a, e == b, l == k
! No equalities independent of the above can hold.
!
m_aibjckakbm: do m = nocc0, nocc1
c_aibjckakbm: do c = nvirt0, nvirt1
k_aibjckakbm: do k = nocc0, nocc1
if (k == m) cycle k_aibjckakbm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakbm: do b = c + 1, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjckakbm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjckakbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckakbm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckakbm
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakbm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjckakbm
if (i > j .and. j > k) exit i_aibjckakbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckakbm
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckakbm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckakbm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckakbm
end do a_aibjckakbm
end do j_aibjckakbm
end do b_aibjckakbm
end do k_aibjckakbm
end do c_aibjckakbm
end do m_aibjckakbm
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j, l
! Equalities: d == a, e == b, m == k
! No equalities independent of the above can hold.
!
l_aibjckalbk: do l = nocc0, nocc1
c_aibjckalbk: do c = nvirt0, nvirt1
k_aibjckalbk: do k = nocc0, nocc1
if (k == l) cycle k_aibjckalbk
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckalbk: do b = c + 1, nvirt1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckalbk: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjckalbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckalbk: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckalbk
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalbk: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjckalbk
if (i > j .and. j > k) exit i_aibjckalbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckalbk
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckalbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckalbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
end if
end do i_aibjckalbk
end do a_aibjckalbk
end do j_aibjckalbk
end do b_aibjckalbk
end do k_aibjckalbk
end do c_aibjckalbk
end do l_aibjckalbk
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, k
! Equalities: d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjckaiej: do e = nvirt0, nvirt1
c_aibjckaiej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckaiej
k_aibjckaiej: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaiej: do b = c + 1, nvirt1
if (b == e) cycle b_aibjckaiej
j_aibjckaiej: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjckaiej: do a = a0 + 1, nvirt1
if (a == c) cycle a_aibjckaiej
i_aibjckaiej: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckaiej
if (i > j .and. j > k) exit i_aibjckaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckaiej
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckaiej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckaiej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckaiej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckaiej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckaiej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end if
end do i_aibjckaiej
end do a_aibjckaiej
end do j_aibjckaiej
end do b_aibjckaiej
end do k_aibjckaiej
end do c_aibjckaiej
end do e_aibjckaiej
!
! Elementary loop 14
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, k, j
! Equalities: d == a, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibjckaiek: do e = nvirt0, nvirt1
c_aibjckaiek: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckaiek
k_aibjckaiek: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaiek: do b = c + 1, nvirt1
if (b == e) cycle b_aibjckaiek
j_aibjckaiek: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaiek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjckaiek: do a = a0 + 1, nvirt1
if (a == c) cycle a_aibjckaiek
i_aibjckaiek: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckaiek
if (i > j .and. j > k) exit i_aibjckaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckaiek
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckaiek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckaiek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckaiek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckaiek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckaiek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end if
end do i_aibjckaiek
end do a_aibjckaiek
end do j_aibjckaiek
end do b_aibjckaiek
end do k_aibjckaiek
end do c_aibjckaiek
end do e_aibjckaiek
!
! Elementary loop 15
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, k
! Equalities: d == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjckajei: do e = nvirt0, nvirt1
c_aibjckajei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckajei
k_aibjckajei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajei: do b = c + 1, nvirt1
if (b == e) cycle b_aibjckajei
j_aibjckajei: do j = nocc0, nocc1
if (j == k) cycle j_aibjckajei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjckajei: do a = a0 + 1, nvirt1
if (a == c) cycle a_aibjckajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajei: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckajei
if (i > j .and. j > k) exit i_aibjckajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckajei
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckajei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckajei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end if
end do i_aibjckajei
end do a_aibjckajei
end do j_aibjckajei
end do b_aibjckajei
end do k_aibjckajei
end do c_aibjckajei
end do e_aibjckajei
!
! Elementary loop 16
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, k, j
! Equalities: d == a, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibjckakei: do e = nvirt0, nvirt1
c_aibjckakei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckakei
k_aibjckakei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakei: do b = c + 1, nvirt1
if (b == e) cycle b_aibjckakei
j_aibjckakei: do j = nocc0, nocc1
if (j == k) cycle j_aibjckakei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjckakei: do a = a0 + 1, nvirt1
if (a == c) cycle a_aibjckakei
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakei: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckakei
if (i > j .and. j > k) exit i_aibjckakei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckakei
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckakei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckakei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckakei
end do a_aibjckakei
end do j_aibjckakei
end do b_aibjckakei
end do k_aibjckakei
end do c_aibjckakei
end do e_aibjckakei
!
! Elementary loop 17
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: j, k, i
! Equalities: d == a, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjckajek: do e = nvirt0, nvirt1
c_aibjckajek: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckajek
k_aibjckajek: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajek: do b = c + 1, nvirt1
if (b == e) cycle b_aibjckajek
j_aibjckajek: do j = nocc0, nocc1
if (j == k) cycle j_aibjckajek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjckajek: do a = a0 + 1, nvirt1
if (a == c) cycle a_aibjckajek
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajek: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckajek
if (i > j .and. j > k) exit i_aibjckajek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckajek
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckajek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckajek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end if
end do i_aibjckajek
end do a_aibjckajek
end do j_aibjckajek
end do b_aibjckajek
end do k_aibjckajek
end do c_aibjckajek
end do e_aibjckajek
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: j, k, i
! Equalities: d == a, m == j, l == k
! No equalities independent of the above can hold.
!
e_aibjckakej: do e = nvirt0, nvirt1
c_aibjckakej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckakej
k_aibjckakej: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakej: do b = c + 1, nvirt1
if (b == e) cycle b_aibjckakej
j_aibjckakej: do j = nocc0, nocc1
if (j == k) cycle j_aibjckakej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjckakej: do a = a0 + 1, nvirt1
if (a == c) cycle a_aibjckakej
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakej: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckakej
if (i > j .and. j > k) exit i_aibjckakej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckakej
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckakej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckakej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckakej
end do a_aibjckakej
end do j_aibjckakej
end do b_aibjckakej
end do k_aibjckakej
end do c_aibjckakej
end do e_aibjckakej
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, k
! Equalities: e == a, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjckdiaj: do d = nvirt0, nvirt1
c_aibjckdiaj: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdiaj
k_aibjckdiaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdiaj: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdiaj
j_aibjckdiaj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdiaj: do a = b + 1, d - 1
if (a == c) cycle a_aibjckdiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckdiaj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdiaj
if (i > j .and. j > k) exit i_aibjckdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdiaj
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckdiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckdiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdiaj
end do a_aibjckdiaj
end do j_aibjckdiaj
end do b_aibjckdiaj
end do k_aibjckdiaj
end do c_aibjckdiaj
end do d_aibjckdiaj
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, k, j
! Equalities: e == a, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjckdiak: do d = nvirt0, nvirt1
c_aibjckdiak: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdiak
k_aibjckdiak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdiak: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdiak
j_aibjckdiak: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdiak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdiak: do a = b + 1, d - 1
if (a == c) cycle a_aibjckdiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckdiak: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdiak
if (i > j .and. j > k) exit i_aibjckdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdiak
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckdiak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckdiak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckdiak
end do a_aibjckdiak
end do j_aibjckdiak
end do b_aibjckdiak
end do k_aibjckdiak
end do c_aibjckdiak
end do d_aibjckdiak
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, k
! Equalities: e == a, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjckdjai: do d = nvirt0, nvirt1
c_aibjckdjai: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdjai
k_aibjckdjai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdjai: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdjai
j_aibjckdjai: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdjai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdjai: do a = b + 1, d - 1
if (a == c) cycle a_aibjckdjai
i_aibjckdjai: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdjai
if (i > j .and. j > k) exit i_aibjckdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdjai
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckdjai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckdjai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckdjai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckdjai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckdjai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdjai
end do a_aibjckdjai
end do j_aibjckdjai
end do b_aibjckdjai
end do k_aibjckdjai
end do c_aibjckdjai
end do d_aibjckdjai
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, k, j
! Equalities: e == a, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibjckdkai: do d = nvirt0, nvirt1
c_aibjckdkai: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdkai
k_aibjckdkai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdkai: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdkai
j_aibjckdkai: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdkai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdkai: do a = b + 1, d - 1
if (a == c) cycle a_aibjckdkai
i_aibjckdkai: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdkai
if (i > j .and. j > k) exit i_aibjckdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdkai
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckdkai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckdkai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckdkai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckdkai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckdkai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdkai
end do a_aibjckdkai
end do j_aibjckdkai
end do b_aibjckdkai
end do k_aibjckdkai
end do c_aibjckdkai
end do d_aibjckdkai
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, k, i
! Equalities: e == a, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjckdjak: do d = nvirt0, nvirt1
c_aibjckdjak: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdjak
k_aibjckdjak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdjak: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdjak
j_aibjckdjak: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdjak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdjak: do a = b + 1, d - 1
if (a == c) cycle a_aibjckdjak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckdjak: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdjak
if (i > j .and. j > k) exit i_aibjckdjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdjak
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckdjak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckdjak(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckdjak
end do a_aibjckdjak
end do j_aibjckdjak
end do b_aibjckdjak
end do k_aibjckdjak
end do c_aibjckdjak
end do d_aibjckdjak
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, k, i
! Equalities: e == a, m == j, l == k
! No equalities independent of the above can hold.
!
d_aibjckdkaj: do d = nvirt0, nvirt1
c_aibjckdkaj: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdkaj
k_aibjckdkaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdkaj: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdkaj
j_aibjckdkaj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdkaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdkaj: do a = b + 1, d - 1
if (a == c) cycle a_aibjckdkaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckdkaj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdkaj
if (i > j .and. j > k) exit i_aibjckdkaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdkaj
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckdkaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckdkaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdkaj
end do a_aibjckdkaj
end do j_aibjckdkaj
end do b_aibjckdkaj
end do k_aibjckdkaj
end do c_aibjckdkaj
end do d_aibjckdkaj
!
! Elementary loop 25
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j, k
! Equalities: d == c, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjckciej: do e = nvirt0, nvirt1
c_aibjckciej: do c = e + 1, nvirt1
k_aibjckciej: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckciej: do b = c + 1, nvirt1
if (b == e) cycle b_aibjckciej
j_aibjckciej: do j = nocc0, nocc1
if (j == k) cycle j_aibjckciej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckciej: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjckciej
i_aibjckciej: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckciej
if (i > j .and. j > k) exit i_aibjckciej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckciej
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckciej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckciej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end if
end do i_aibjckciej
end do a_aibjckciej
end do j_aibjckciej
end do b_aibjckciej
end do k_aibjckciej
end do c_aibjckciej
end do e_aibjckciej
!
! Elementary loop 26
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, k, j
! Equalities: d == c, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibjckciek: do e = nvirt0, nvirt1
c_aibjckciek: do c = e + 1, nvirt1
k_aibjckciek: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckciek: do b = c + 1, nvirt1
if (b == e) cycle b_aibjckciek
j_aibjckciek: do j = nocc0, nocc1
if (j == k) cycle j_aibjckciek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckciek: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjckciek
i_aibjckciek: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckciek
if (i > j .and. j > k) exit i_aibjckciek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckciek
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckciek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckciek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end if
end do i_aibjckciek
end do a_aibjckciek
end do j_aibjckciek
end do b_aibjckciek
end do k_aibjckciek
end do c_aibjckciek
end do e_aibjckciek
!
! Elementary loop 27
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j, k
! Equalities: d == c, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjckcjei: do e = nvirt0, nvirt1
c_aibjckcjei: do c = e + 1, nvirt1
k_aibjckcjei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckcjei: do b = c + 1, nvirt1
if (b == e) cycle b_aibjckcjei
j_aibjckcjei: do j = nocc0, nocc1
if (j == k) cycle j_aibjckcjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcjei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjckcjei
i_aibjckcjei: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckcjei
if (i > j .and. j > k) exit i_aibjckcjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckcjei
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckcjei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckcjei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckcjei
end do a_aibjckcjei
end do j_aibjckcjei
end do b_aibjckcjei
end do k_aibjckcjei
end do c_aibjckcjei
end do e_aibjckcjei
!
! Elementary loop 28
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, k, j
! Equalities: d == c, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibjckckei: do e = nvirt0, nvirt1
c_aibjckckei: do c = e + 1, nvirt1
k_aibjckckei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckckei: do b = c + 1, nvirt1
if (b == e) cycle b_aibjckckei
j_aibjckckei: do j = nocc0, nocc1
if (j == k) cycle j_aibjckckei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckckei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjckckei
i_aibjckckei: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckckei
if (i > j .and. j > k) exit i_aibjckckei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckckei
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckckei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckckei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckckei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckckei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckckei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end if
end do i_aibjckckei
end do a_aibjckckei
end do j_aibjckckei
end do b_aibjckckei
end do k_aibjckckei
end do c_aibjckckei
end do e_aibjckckei
!
! Elementary loop 29
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: j, k, i
! Equalities: d == c, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjckcjek: do e = nvirt0, nvirt1
c_aibjckcjek: do c = e + 1, nvirt1
k_aibjckcjek: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckcjek: do b = c + 1, nvirt1
if (b == e) cycle b_aibjckcjek
j_aibjckcjek: do j = nocc0, nocc1
if (j == k) cycle j_aibjckcjek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcjek: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjckcjek
i_aibjckcjek: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckcjek
if (i > j .and. j > k) exit i_aibjckcjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckcjek
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckcjek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckcjek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckcjek
end do a_aibjckcjek
end do j_aibjckcjek
end do b_aibjckcjek
end do k_aibjckcjek
end do c_aibjckcjek
end do e_aibjckcjek
!
! Elementary loop 30
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: j, k, i
! Equalities: d == c, m == j, l == k
! No equalities independent of the above can hold.
!
e_aibjckckej: do e = nvirt0, nvirt1
c_aibjckckej: do c = e + 1, nvirt1
k_aibjckckej: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckckej: do b = c + 1, nvirt1
if (b == e) cycle b_aibjckckej
j_aibjckckej: do j = nocc0, nocc1
if (j == k) cycle j_aibjckckej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckckej: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjckckej
i_aibjckckej: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckckej
if (i > j .and. j > k) exit i_aibjckckej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckckej
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckckej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckckej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckckej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckckej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckckej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end if
end do i_aibjckckej
end do a_aibjckckej
end do j_aibjckckej
end do b_aibjckckej
end do k_aibjckckej
end do c_aibjckckej
end do e_aibjckckej
!
! Elementary loop 31
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k, m
! Equalities: d == b, e == c, l == i
! No equalities independent of the above can hold.
!
m_aibjckbicm: do m = nocc0, nocc1
c_aibjckbicm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckbicm: do k = nocc0, nocc1
if (k == m) cycle k_aibjckbicm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbicm: do b = c + 1, nvirt1
j_aibjckbicm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjckbicm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbicm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbicm
i_aibjckbicm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjckbicm
if (i > j .and. j > k) exit i_aibjckbicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbicm
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbicm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbicm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckbicm
end do a_aibjckbicm
end do j_aibjckbicm
end do b_aibjckbicm
end do k_aibjckbicm
end do c_aibjckbicm
end do m_aibjckbicm
!
! Elementary loop 32
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k, l
! Equalities: d == b, e == c, m == i
! No equalities independent of the above can hold.
!
l_aibjckblci: do l = nocc0, nocc1
c_aibjckblci: do c = nvirt0, nvirt1
k_aibjckblci: do k = nocc0, nocc1
if (k == l) cycle k_aibjckblci
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckblci: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjckblci: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjckblci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckblci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckblci
i_aibjckblci: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjckblci
if (i > j .and. j > k) exit i_aibjckblci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckblci
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckblci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckblci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
end if
end do i_aibjckblci
end do a_aibjckblci
end do j_aibjckblci
end do b_aibjckblci
end do k_aibjckblci
end do c_aibjckblci
end do l_aibjckblci
!
! Elementary loop 33
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i, k, m
! Equalities: d == b, e == c, l == j
! No equalities independent of the above can hold.
!
m_aibjckbjcm: do m = nocc0, nocc1
c_aibjckbjcm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckbjcm: do k = nocc0, nocc1
if (k == m) cycle k_aibjckbjcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbjcm: do b = c + 1, nvirt1
j_aibjckbjcm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjckbjcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjcm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbjcm
i_aibjckbjcm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjckbjcm
if (i > j .and. j > k) exit i_aibjckbjcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbjcm
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbjcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbjcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbjcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbjcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbjcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
end if
end do i_aibjckbjcm
end do a_aibjckbjcm
end do j_aibjckbjcm
end do b_aibjckbjcm
end do k_aibjckbjcm
end do c_aibjckbjcm
end do m_aibjckbjcm
!
! Elementary loop 34
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i, k, l
! Equalities: d == b, e == c, m == j
! No equalities independent of the above can hold.
!
l_aibjckblcj: do l = nocc0, nocc1
c_aibjckblcj: do c = nvirt0, nvirt1
k_aibjckblcj: do k = nocc0, nocc1
if (k == l) cycle k_aibjckblcj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckblcj: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjckblcj: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjckblcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckblcj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckblcj
i_aibjckblcj: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjckblcj
if (i > j .and. j > k) exit i_aibjckblcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckblcj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckblcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckblcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckblcj
end do a_aibjckblcj
end do j_aibjckblcj
end do b_aibjckblcj
end do k_aibjckblcj
end do c_aibjckblcj
end do l_aibjckblcj
!
! Elementary loop 35
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: k, i, j, m
! Equalities: d == b, e == c, l == k
! No equalities independent of the above can hold.
!
m_aibjckbkcm: do m = nocc0, nocc1
c_aibjckbkcm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckbkcm: do k = nocc0, nocc1
if (k == m) cycle k_aibjckbkcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbkcm: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkcm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjckbkcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbkcm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbkcm
i_aibjckbkcm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjckbkcm
if (i > j .and. j > k) exit i_aibjckbkcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbkcm
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbkcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbkcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
end if
end do i_aibjckbkcm
end do a_aibjckbkcm
end do j_aibjckbkcm
end do b_aibjckbkcm
end do k_aibjckbkcm
end do c_aibjckbkcm
end do m_aibjckbkcm
!
! Elementary loop 36
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: k, i, j, l
! Equalities: d == b, e == c, m == k
! No equalities independent of the above can hold.
!
l_aibjckblck: do l = nocc0, nocc1
c_aibjckblck: do c = nvirt0, nvirt1
k_aibjckblck: do k = nocc0, nocc1
if (k == l) cycle k_aibjckblck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckblck: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjckblck: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjckblck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckblck: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckblck
i_aibjckblck: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjckblck
if (i > j .and. j > k) exit i_aibjckblck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckblck
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckblck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckblck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckblck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckblck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckblck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
end if
end do i_aibjckblck
end do a_aibjckblck
end do j_aibjckblck
end do b_aibjckblck
end do k_aibjckblck
end do c_aibjckblck
end do l_aibjckblck
!
! Elementary loop 37
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j, k
! Equalities: e == c, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjckdicj: do d = nvirt0, nvirt1
c_aibjckdicj: do c = nvirt0, d - 1
k_aibjckdicj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdicj: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdicj
j_aibjckdicj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdicj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdicj
i_aibjckdicj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdicj
if (i > j .and. j > k) exit i_aibjckdicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdicj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckdicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckdicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckdicj
end do a_aibjckdicj
end do j_aibjckdicj
end do b_aibjckdicj
end do k_aibjckdicj
end do c_aibjckdicj
end do d_aibjckdicj
!
! Elementary loop 38
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, k, j
! Equalities: e == c, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjckdick: do d = nvirt0, nvirt1
c_aibjckdick: do c = nvirt0, d - 1
k_aibjckdick: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdick: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdick
j_aibjckdick: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdick
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdick: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdick
i_aibjckdick: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdick
if (i > j .and. j > k) exit i_aibjckdick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdick
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckdick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckdick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckdick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckdick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckdick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdick
end do a_aibjckdick
end do j_aibjckdick
end do b_aibjckdick
end do k_aibjckdick
end do c_aibjckdick
end do d_aibjckdick
!
! Elementary loop 39
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j, k
! Equalities: e == c, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjckdjci: do d = nvirt0, nvirt1
c_aibjckdjci: do c = nvirt0, d - 1
k_aibjckdjci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdjci: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdjci
j_aibjckdjci: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdjci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdjci: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdjci
i_aibjckdjci: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdjci
if (i > j .and. j > k) exit i_aibjckdjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdjci
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckdjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckdjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdjci
end do a_aibjckdjci
end do j_aibjckdjci
end do b_aibjckdjci
end do k_aibjckdjci
end do c_aibjckdjci
end do d_aibjckdjci
!
! Elementary loop 40
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, k, j
! Equalities: e == c, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibjckdkci: do d = nvirt0, nvirt1
c_aibjckdkci: do c = nvirt0, d - 1
k_aibjckdkci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdkci: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdkci
j_aibjckdkci: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdkci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdkci: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdkci
i_aibjckdkci: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdkci
if (i > j .and. j > k) exit i_aibjckdkci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdkci
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckdkci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckdkci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdkci
end do a_aibjckdkci
end do j_aibjckdkci
end do b_aibjckdkci
end do k_aibjckdkci
end do c_aibjckdkci
end do d_aibjckdkci
!
! Elementary loop 41
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: j, k, i
! Equalities: e == c, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjckdjck: do d = nvirt0, nvirt1
c_aibjckdjck: do c = nvirt0, d - 1
k_aibjckdjck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdjck: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdjck
j_aibjckdjck: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdjck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdjck: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdjck
i_aibjckdjck: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdjck
if (i > j .and. j > k) exit i_aibjckdjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdjck
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckdjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckdjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckdjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckdjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckdjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdjck
end do a_aibjckdjck
end do j_aibjckdjck
end do b_aibjckdjck
end do k_aibjckdjck
end do c_aibjckdjck
end do d_aibjckdjck
!
! Elementary loop 42
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: j, k, i
! Equalities: e == c, m == j, l == k
! No equalities independent of the above can hold.
!
d_aibjckdkcj: do d = nvirt0, nvirt1
c_aibjckdkcj: do c = nvirt0, d - 1
k_aibjckdkcj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdkcj: do b = c + 1, nvirt1
if (b == d) cycle b_aibjckdkcj
j_aibjckdkcj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdkcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdkcj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdkcj
i_aibjckdkcj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdkcj
if (i > j .and. j > k) exit i_aibjckdkcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdkcj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckdkcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckdkcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckdkcj
end do a_aibjckdkcj
end do j_aibjckdkcj
end do b_aibjckdkcj
end do k_aibjckdkcj
end do c_aibjckdkcj
end do d_aibjckdkcj
!
! Elementary loop 43
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, k
! Equalities: d == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjckbiej: do e = nvirt0, nvirt1
c_aibjckbiej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckbiej
k_aibjckbiej: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c, e)
b_aibjckbiej: do b = b0 + 1, nvirt1
j_aibjckbiej: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbiej: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjckbiej
i_aibjckbiej: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbiej
if (i > j .and. j > k) exit i_aibjckbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbiej
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbiej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbiej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckbiej
end do a_aibjckbiej
end do j_aibjckbiej
end do b_aibjckbiej
end do k_aibjckbiej
end do c_aibjckbiej
end do e_aibjckbiej
!
! Elementary loop 44
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, k, j
! Equalities: d == b, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibjckbiek: do e = nvirt0, nvirt1
c_aibjckbiek: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckbiek
k_aibjckbiek: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c, e)
b_aibjckbiek: do b = b0 + 1, nvirt1
j_aibjckbiek: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbiek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbiek: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjckbiek
i_aibjckbiek: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbiek
if (i > j .and. j > k) exit i_aibjckbiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbiek
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbiek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbiek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckbiek
end do a_aibjckbiek
end do j_aibjckbiek
end do b_aibjckbiek
end do k_aibjckbiek
end do c_aibjckbiek
end do e_aibjckbiek
!
! Elementary loop 45
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, k
! Equalities: d == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjckbjei: do e = nvirt0, nvirt1
c_aibjckbjei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckbjei
k_aibjckbjei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c, e)
b_aibjckbjei: do b = b0 + 1, nvirt1
j_aibjckbjei: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjckbjei
i_aibjckbjei: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbjei
if (i > j .and. j > k) exit i_aibjckbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbjei
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbjei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbjei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbjei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbjei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbjei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end if
end do i_aibjckbjei
end do a_aibjckbjei
end do j_aibjckbjei
end do b_aibjckbjei
end do k_aibjckbjei
end do c_aibjckbjei
end do e_aibjckbjei
!
! Elementary loop 46
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, k, j
! Equalities: d == b, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibjckbkei: do e = nvirt0, nvirt1
c_aibjckbkei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckbkei
k_aibjckbkei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c, e)
b_aibjckbkei: do b = b0 + 1, nvirt1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkei: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbkei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbkei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjckbkei
i_aibjckbkei: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbkei
if (i > j .and. j > k) exit i_aibjckbkei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbkei
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbkei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbkei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end if
end do i_aibjckbkei
end do a_aibjckbkei
end do j_aibjckbkei
end do b_aibjckbkei
end do k_aibjckbkei
end do c_aibjckbkei
end do e_aibjckbkei
!
! Elementary loop 47
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: j, k, i
! Equalities: d == b, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjckbjek: do e = nvirt0, nvirt1
c_aibjckbjek: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckbjek
k_aibjckbjek: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c, e)
b_aibjckbjek: do b = b0 + 1, nvirt1
j_aibjckbjek: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbjek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjek: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjckbjek
i_aibjckbjek: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbjek
if (i > j .and. j > k) exit i_aibjckbjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbjek
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbjek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbjek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbjek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbjek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbjek(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end if
end do i_aibjckbjek
end do a_aibjckbjek
end do j_aibjckbjek
end do b_aibjckbjek
end do k_aibjckbjek
end do c_aibjckbjek
end do e_aibjckbjek
!
! Elementary loop 48
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: j, k, i
! Equalities: d == b, m == j, l == k
! No equalities independent of the above can hold.
!
e_aibjckbkej: do e = nvirt0, nvirt1
c_aibjckbkej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckbkej
k_aibjckbkej: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c, e)
b_aibjckbkej: do b = b0 + 1, nvirt1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkej: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbkej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbkej: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjckbkej
i_aibjckbkej: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbkej
if (i > j .and. j > k) exit i_aibjckbkej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbkej
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbkej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbkej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end if
end do i_aibjckbkej
end do a_aibjckbkej
end do j_aibjckbkej
end do b_aibjckbkej
end do k_aibjckbkej
end do c_aibjckbkej
end do e_aibjckbkej
!
! Elementary loop 49
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, k
! Equalities: e == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjckdibj: do d = nvirt0, nvirt1
c_aibjckdibj: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdibj
k_aibjckdibj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdibj: do b = c + 1, d - 1
j_aibjckdibj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdibj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdibj
i_aibjckdibj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdibj
if (i > j .and. j > k) exit i_aibjckdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdibj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckdibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckdibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckdibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckdibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckdibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdibj
end do a_aibjckdibj
end do j_aibjckdibj
end do b_aibjckdibj
end do k_aibjckdibj
end do c_aibjckdibj
end do d_aibjckdibj
!
! Elementary loop 50
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, k, j
! Equalities: e == b, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjckdibk: do d = nvirt0, nvirt1
c_aibjckdibk: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjckdibk
k_aibjckdibk: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdibk: do b = c + 1, d - 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckdibk: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdibk: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdibk
i_aibjckdibk: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdibk
if (i > j .and. j > k) exit i_aibjckdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckdibk
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckdibk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckdibk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end if
end do i_aibjckdibk
end do a_aibjckdibk
end do j_aibjckdibk
end do b_aibjckdibk
end do k_aibjckdibk
end do c_aibjckdibk
end do d_aibjckdibk
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
end subroutine ccjac_32_part1
end module ccjac_block_32_part1
