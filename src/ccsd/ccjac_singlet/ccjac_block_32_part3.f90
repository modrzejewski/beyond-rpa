module ccjac_block_32_part3
use eom_cc3_32_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:41:38 UTC.
!
contains
 
subroutine ccjac_32_part3(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: a0, i1, j0
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
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbkajei: do e = nvirt0, nvirt1
k_aibjbkajei: do k = nocc0, nocc1
b_aibjbkajei: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjbkajei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkajei: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjbkajei: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbkajei: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkajei(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjbkajei
end do a_aibjbkajei
end do j_aibjbkajei
end do b_aibjbkajei
end do k_aibjbkajei
end do e_aibjbkajei
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, k, i
! Equalities: d == a, c == b, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjbkajek: do e = nvirt0, nvirt1
k_aibjbkajek: do k = nocc0, nocc1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkajek: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjbkajek
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkajek: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjbkajek: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbkajek: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkajek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkajek(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjbkajek
end do a_aibjbkajek
end do j_aibjbkajek
end do b_aibjbkajek
end do k_aibjbkajek
end do e_aibjbkajek
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjciaibm: do m = nocc0, nocc1
c_aibjciaibm: do c = nvirt0, nvirt1
b_aibjciaibm: do b = c + 1, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjciaibm: do j = nocc0, nocc1
if (j == m) cycle j_aibjciaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciaibm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciaibm
i_aibjciaibm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjciaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciaibm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjciaibm
end do a_aibjciaibm
end do j_aibjciaibm
end do b_aibjciaibm
end do c_aibjciaibm
end do m_aibjciaibm
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == b, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjckaibi: do c = nvirt0, nvirt1
k_aibjckaibi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibi: do b = c + 1, nvirt1
j_aibjckaibi: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaibi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaibi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckaibi
i_aibjckaibi: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckaibi
if (i > j .and. j > k) exit i_aibjckaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckaibi
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckaibi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckaibi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckaibi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckaibi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckaibi
end do a_aibjckaibi
end do j_aibjckaibi
end do b_aibjckaibi
end do k_aibjckaibi
end do c_aibjckaibi
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjaibm: do m = nocc0, nocc1
c_aibjcjaibm: do c = nvirt0, nvirt1
b_aibjcjaibm: do b = c + 1, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjaibm: do j = nocc0, nocc1
if (j == m) cycle j_aibjcjaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjaibm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjaibm
i_aibjcjaibm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcjaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjaibm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcjaibm
end do a_aibjcjaibm
end do j_aibjcjaibm
end do b_aibjcjaibm
end do c_aibjcjaibm
end do m_aibjcjaibm
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaibj: do c = nvirt0, nvirt1
k_aibjckaibj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibj: do b = c + 1, nvirt1
j_aibjckaibj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaibj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckaibj
i_aibjckaibj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckaibj
if (i > j .and. j > k) exit i_aibjckaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckaibj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckaibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckaibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckaibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckaibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckaibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckaibj
end do a_aibjckaibj
end do j_aibjckaibj
end do b_aibjckaibj
end do k_aibjckaibj
end do c_aibjckaibj
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckaibk: do c = nvirt0, nvirt1
k_aibjckaibk: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibk: do b = c + 1, nvirt1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckaibk: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaibk: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckaibk
i_aibjckaibk: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckaibk
if (i > j .and. j > k) exit i_aibjckaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckaibk
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckaibk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckaibk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckaibk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckaibk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckaibk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckaibk
end do a_aibjckaibk
end do j_aibjckaibk
end do b_aibjckaibk
end do k_aibjckaibk
end do c_aibjckaibk
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjcialbi: do l = nocc0, nocc1
c_aibjcialbi: do c = nvirt0, nvirt1
b_aibjcialbi: do b = c + 1, nvirt1
j_aibjcialbi: do j = nocc0, nocc1
if (j == l) cycle j_aibjcialbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcialbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialbi: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcialbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcialbi
end do a_aibjcialbi
end do j_aibjcialbi
end do b_aibjcialbi
end do c_aibjcialbi
end do l_aibjcialbi
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjcialbj: do l = nocc0, nocc1
c_aibjcialbj: do c = nvirt0, nvirt1
b_aibjcialbj: do b = c + 1, nvirt1
j_aibjcialbj: do j = nocc0, nocc1
if (j == l) cycle j_aibjcialbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcialbj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcialbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialbj: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcialbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcialbj
end do a_aibjcialbj
end do j_aibjcialbj
end do b_aibjcialbj
end do c_aibjcialbj
end do l_aibjcialbj
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckajbi: do c = nvirt0, nvirt1
k_aibjckajbi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajbi: do b = c + 1, nvirt1
j_aibjckajbi: do j = nocc0, nocc1
if (j == k) cycle j_aibjckajbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckajbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajbi: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckajbi
if (i > j .and. j > k) exit i_aibjckajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckajbi
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckajbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckajbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckajbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckajbi
end do a_aibjckajbi
end do j_aibjckajbi
end do b_aibjckajbi
end do k_aibjckajbi
end do c_aibjckajbi
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckakbi: do c = nvirt0, nvirt1
k_aibjckakbi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakbi: do b = c + 1, nvirt1
j_aibjckakbi: do j = nocc0, nocc1
if (j == k) cycle j_aibjckakbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckakbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckakbi
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakbi: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckakbi
if (i > j .and. j > k) exit i_aibjckakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckakbi
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckakbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckakbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckakbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckakbi
end do a_aibjckakbi
end do j_aibjckakbi
end do b_aibjckakbi
end do k_aibjckakbi
end do c_aibjckakbi
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, m
! Equalities: d == a, e == b, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjcjajbm: do m = nocc0, nocc1
c_aibjcjajbm: do c = nvirt0, nvirt1
b_aibjcjajbm: do b = c + 1, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjajbm: do j = nocc0, nocc1
if (j == m) cycle j_aibjcjajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjajbm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajbm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcjajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjajbm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcjajbm
end do a_aibjcjajbm
end do j_aibjcjajbm
end do b_aibjcjajbm
end do c_aibjcjajbm
end do m_aibjcjajbm
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: d == a, e == b, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjckajbj: do c = nvirt0, nvirt1
k_aibjckajbj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajbj: do b = c + 1, nvirt1
j_aibjckajbj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckajbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckajbj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckajbj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajbj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckajbj
if (i > j .and. j > k) exit i_aibjckajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckajbj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckajbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckajbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckajbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckajbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckajbj
end do a_aibjckajbj
end do j_aibjckajbj
end do b_aibjckajbj
end do k_aibjckajbj
end do c_aibjckajbj
!
! Elementary loop 14
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: d == a, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckajbk: do c = nvirt0, nvirt1
k_aibjckajbk: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajbk: do b = c + 1, nvirt1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckajbk: do j = nocc0, nocc1
if (j == k) cycle j_aibjckajbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckajbk: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckajbk
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajbk: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckajbk
if (i > j .and. j > k) exit i_aibjckajbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckajbk
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckajbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckajbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckajbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckajbk
end do a_aibjckajbk
end do j_aibjckajbk
end do b_aibjckajbk
end do k_aibjckajbk
end do c_aibjckajbk
!
! Elementary loop 15
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjcjalbj: do l = nocc0, nocc1
c_aibjcjalbj: do c = nvirt0, nvirt1
b_aibjcjalbj: do b = c + 1, nvirt1
j_aibjcjalbj: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjalbj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjalbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalbj: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjalbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjalbj
end do a_aibjcjalbj
end do j_aibjcjalbj
end do b_aibjcjalbj
end do c_aibjcjalbj
end do l_aibjcjalbj
!
! Elementary loop 16
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: d == a, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckakbj: do c = nvirt0, nvirt1
k_aibjckakbj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakbj: do b = c + 1, nvirt1
j_aibjckakbj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckakbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckakbj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckakbj
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakbj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckakbj
if (i > j .and. j > k) exit i_aibjckakbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckakbj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckakbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckakbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckakbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckakbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckakbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckakbj
end do a_aibjckakbj
end do j_aibjckakbj
end do b_aibjckakbj
end do k_aibjckakbj
end do c_aibjckakbj
!
! Elementary loop 17
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j
! Equalities: d == a, e == b, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibjckakbk: do c = nvirt0, nvirt1
k_aibjckakbk: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakbk: do b = c + 1, nvirt1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckakbk: do j = nocc0, nocc1
if (j == k) cycle j_aibjckakbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckakbk: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckakbk
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakbk: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckakbk
if (i > j .and. j > k) exit i_aibjckakbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckakbk
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckakbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckakbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckakbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckakbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckakbk
end do a_aibjckakbk
end do j_aibjckakbk
end do b_aibjckakbk
end do k_aibjckakbk
end do c_aibjckakbk
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjciaiei: do e = nvirt0, nvirt1
c_aibjciaiei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjciaiei
b_aibjciaiei: do b = c + 1, nvirt1
if (b == e) cycle b_aibjciaiei
j_aibjciaiei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjciaiei: do a = a0 + 1, nvirt1
if (a == c) cycle a_aibjciaiei
i_aibjciaiei: do i = nocc0, nocc1
if (i == j) cycle i_aibjciaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciaiei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjciaiei
end do a_aibjciaiei
end do j_aibjciaiei
end do b_aibjciaiei
end do c_aibjciaiei
end do e_aibjciaiei
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjciaiej: do e = nvirt0, nvirt1
c_aibjciaiej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjciaiej
b_aibjciaiej: do b = c + 1, nvirt1
if (b == e) cycle b_aibjciaiej
j_aibjciaiej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjciaiej: do a = a0 + 1, nvirt1
if (a == c) cycle a_aibjciaiej
i_aibjciaiej: do i = nocc0, nocc1
if (i == j) cycle i_aibjciaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciaiej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjciaiej
end do a_aibjciaiej
end do j_aibjciaiej
end do b_aibjciaiej
end do c_aibjciaiej
end do e_aibjciaiej
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjaiej: do e = nvirt0, nvirt1
c_aibjcjaiej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcjaiej
b_aibjcjaiej: do b = c + 1, nvirt1
if (b == e) cycle b_aibjcjaiej
j_aibjcjaiej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjcjaiej: do a = a0 + 1, nvirt1
if (a == c) cycle a_aibjcjaiej
i_aibjcjaiej: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjaiej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcjaiej
end do a_aibjcjaiej
end do j_aibjcjaiej
end do b_aibjcjaiej
end do c_aibjcjaiej
end do e_aibjcjaiej
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjcjajei: do e = nvirt0, nvirt1
c_aibjcjajei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcjajei
b_aibjcjajei: do b = c + 1, nvirt1
if (b == e) cycle b_aibjcjajei
j_aibjcjajei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjcjajei: do a = a0 + 1, nvirt1
if (a == c) cycle a_aibjcjajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajei: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjajei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcjajei
end do a_aibjcjajei
end do j_aibjcjajei
end do b_aibjcjajei
end do c_aibjcjajei
end do e_aibjcjajei
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: j, i
! Equalities: d == a, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjajej: do e = nvirt0, nvirt1
c_aibjcjajej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcjajej
b_aibjcjajej: do b = c + 1, nvirt1
if (b == e) cycle b_aibjcjajej
j_aibjcjajej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjcjajej: do a = a0 + 1, nvirt1
if (a == c) cycle a_aibjcjajej
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajej: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjajej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjajej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcjajej
end do a_aibjcjajej
end do j_aibjcjajej
end do b_aibjcjajej
end do c_aibjcjajej
end do e_aibjcjajej
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajckdiaj: do d = nvirt0, nvirt1
c_aiajckdiaj: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajckdiaj
k_aiajckdiaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdiaj: do j = nocc0, k - 1
a_aiajckdiaj: do a = c + 1, d - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdiaj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckdiaj(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdiaj
end do a_aiajckdiaj
end do j_aiajckdiaj
end do k_aiajckdiaj
end do c_aiajckdiaj
end do d_aiajckdiaj
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k, j
! Equalities: b == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
d_aiajckdiak: do d = nvirt0, nvirt1
c_aiajckdiak: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajckdiak
k_aiajckdiak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdiak: do j = nocc0, k - 1
a_aiajckdiak: do a = c + 1, d - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckdiak: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckdiak(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdiak
end do a_aiajckdiak
end do j_aiajckdiak
end do k_aiajckdiak
end do c_aiajckdiak
end do d_aiajckdiak
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajckdjai: do d = nvirt0, nvirt1
c_aiajckdjai: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajckdjai
k_aiajckdjai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdjai: do j = nocc0, k - 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckdjai: do a = c + 1, d - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdjai: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckdjai(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdjai
end do a_aiajckdjai
end do j_aiajckdjai
end do k_aiajckdjai
end do c_aiajckdjai
end do d_aiajckdjai
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k, j
! Equalities: b == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
d_aiajckdkai: do d = nvirt0, nvirt1
c_aiajckdkai: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajckdkai
k_aiajckdkai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdkai: do j = nocc0, k - 1
a_aiajckdkai: do a = c + 1, d - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdkai: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckdkai(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdkai
end do a_aiajckdkai
end do j_aiajckdkai
end do k_aiajckdkai
end do c_aiajckdkai
end do d_aiajckdkai
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, k, i
! Equalities: b == a, e == a, l == j, m == k
! No equalities independent of the above can hold.
!
d_aiajckdjak: do d = nvirt0, nvirt1
c_aiajckdjak: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajckdjak
k_aiajckdjak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdjak: do j = nocc0, k - 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckdjak: do a = c + 1, d - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckdjak: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckdjak(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdjak
end do a_aiajckdjak
end do j_aiajckdjak
end do k_aiajckdjak
end do c_aiajckdjak
end do d_aiajckdjak
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, k, i
! Equalities: b == a, e == a, m == j, l == k
! No equalities independent of the above can hold.
!
d_aiajckdkaj: do d = nvirt0, nvirt1
c_aiajckdkaj: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajckdkaj
k_aiajckdkaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdkaj: do j = nocc0, k - 1
a_aiajckdkaj: do a = c + 1, d - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdkaj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdkaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckdkaj(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdkaj
end do a_aiajckdkaj
end do j_aiajckdkaj
end do k_aiajckdkaj
end do c_aiajckdkaj
end do d_aiajckdkaj
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: e == a, c == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbkdiaj: do d = nvirt0, nvirt1
k_aibjbkdiaj: do k = nocc0, nocc1
b_aibjbkdiaj: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjbkdiaj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdiaj: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdiaj: do a = b + 1, d - 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbkdiaj: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkdiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdiaj
end do a_aibjbkdiaj
end do j_aibjbkdiaj
end do b_aibjbkdiaj
end do k_aibjbkdiaj
end do d_aibjbkdiaj
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: e == a, c == b, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdjai: do d = nvirt0, nvirt1
k_aibjbkdjai: do k = nocc0, nocc1
b_aibjbkdjai: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjbkdjai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdjai: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdjai: do a = b + 1, d - 1
i_aibjbkdjai: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkdjai(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdjai
end do a_aibjbkdjai
end do j_aibjbkdjai
end do b_aibjbkdjai
end do k_aibjbkdjai
end do d_aibjbkdjai
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, j
! Equalities: e == a, c == b, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibjbkdkai: do d = nvirt0, nvirt1
k_aibjbkdkai: do k = nocc0, nocc1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkdkai: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjbkdkai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdkai: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdkai: do a = b + 1, d - 1
i_aibjbkdkai: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkdkai(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdkai
end do a_aibjbkdkai
end do j_aibjbkdkai
end do b_aibjbkdkai
end do k_aibjbkdkai
end do d_aibjbkdkai
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, k, i
! Equalities: e == a, c == b, m == j, l == k
! No equalities independent of the above can hold.
!
d_aibjbkdkaj: do d = nvirt0, nvirt1
k_aibjbkdkaj: do k = nocc0, nocc1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkdkaj: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjbkdkaj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdkaj: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdkaj: do a = b + 1, d - 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbkdkaj: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdkaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkdkaj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdkaj
end do a_aibjbkdkaj
end do j_aibjbkdkaj
end do b_aibjbkdkaj
end do k_aibjbkdkaj
end do d_aibjbkdkaj
!
! Elementary loop 33
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjcidiai: do d = nvirt0, nvirt1
c_aibjcidiai: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcidiai
b_aibjcidiai: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcidiai
j_aibjcidiai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidiai: do a = b + 1, d - 1
if (a == c) cycle a_aibjcidiai
i_aibjcidiai: do i = nocc0, nocc1
if (i == j) cycle i_aibjcidiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcidiai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcidiai
end do a_aibjcidiai
end do j_aibjcidiai
end do b_aibjcidiai
end do c_aibjcidiai
end do d_aibjcidiai
!
! Elementary loop 34
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdiaj: do d = nvirt0, nvirt1
c_aibjcjdiaj: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcjdiaj
b_aibjcjdiaj: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcjdiaj
j_aibjcjdiaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdiaj: do a = b + 1, d - 1
if (a == c) cycle a_aibjcjdiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjdiaj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjdiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdiaj
end do a_aibjcjdiaj
end do j_aibjcjdiaj
end do b_aibjcjdiaj
end do c_aibjcjdiaj
end do d_aibjcjdiaj
!
! Elementary loop 35
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidjai: do d = nvirt0, nvirt1
c_aibjcidjai: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcidjai
b_aibjcidjai: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcidjai
j_aibjcidjai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidjai: do a = b + 1, d - 1
if (a == c) cycle a_aibjcidjai
i_aibjcidjai: do i = nocc0, nocc1
if (i == j) cycle i_aibjcidjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcidjai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcidjai
end do a_aibjcidjai
end do j_aibjcidjai
end do b_aibjcidjai
end do c_aibjcidjai
end do d_aibjcidjai
!
! Elementary loop 36
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjcjdjai: do d = nvirt0, nvirt1
c_aibjcjdjai: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcjdjai
b_aibjcjdjai: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcjdjai
j_aibjcjdjai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdjai: do a = b + 1, d - 1
if (a == c) cycle a_aibjcjdjai
i_aibjcjdjai: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjdjai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdjai
end do a_aibjcjdjai
end do j_aibjcjdjai
end do b_aibjcjdjai
end do c_aibjcjdjai
end do d_aibjcjdjai
!
! Elementary loop 37
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, i
! Equalities: e == a, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdjaj: do d = nvirt0, nvirt1
c_aibjcjdjaj: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcjdjaj
b_aibjcjdjaj: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcjdjaj
j_aibjcjdjaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdjaj: do a = b + 1, d - 1
if (a == c) cycle a_aibjcjdjaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjdjaj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjdjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjdjaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdjaj
end do a_aibjcjdjaj
end do j_aibjcjdjaj
end do b_aibjcjdjaj
end do c_aibjcjdjaj
end do d_aibjcjdjaj
!
! Elementary loop 38
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, k
! Equalities: b == a, d == c, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajckcjei: do e = nvirt0, nvirt1
c_aiajckcjei: do c = e + 1, nvirt1
k_aiajckcjei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckcjei: do j = nocc0, k - 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckcjei: do a = c + 1, nvirt1
if (a == e) cycle a_aiajckcjei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckcjei: do i = j + 1, nocc1
if (i == k) cycle i_aiajckcjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckcjei(eorb, t2, t1, nocc, nactive, a, i, j, c, k, e)
end do i_aiajckcjei
end do a_aiajckcjei
end do j_aiajckcjei
end do k_aiajckcjei
end do c_aiajckcjei
end do e_aiajckcjei
!
! Elementary loop 39
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k, j
! Equalities: b == a, d == c, m == i, l == k
! No equalities independent of the above can hold.
!
e_aiajckckei: do e = nvirt0, nvirt1
c_aiajckckei: do c = e + 1, nvirt1
k_aiajckckei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckckei: do j = nocc0, k - 1
a_aiajckckei: do a = c + 1, nvirt1
if (a == e) cycle a_aiajckckei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckckei: do i = j + 1, nocc1
if (i == k) cycle i_aiajckckei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckckei(eorb, t2, t1, nocc, nactive, a, i, j, c, k, e)
end do i_aiajckckei
end do a_aiajckckei
end do j_aiajckckei
end do k_aiajckckei
end do c_aiajckckei
end do e_aiajckckei
!
! Elementary loop 40
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, k, i
! Equalities: b == a, d == c, l == j, m == k
! No equalities independent of the above can hold.
!
e_aiajckcjek: do e = nvirt0, nvirt1
c_aiajckcjek: do c = e + 1, nvirt1
k_aiajckcjek: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckcjek: do j = nocc0, k - 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckcjek: do a = c + 1, nvirt1
if (a == e) cycle a_aiajckcjek
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckcjek: do i = j + 1, nocc1
if (i == k) cycle i_aiajckcjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckcjek(eorb, t2, t1, nocc, nactive, a, i, j, c, k, e)
end do i_aiajckcjek
end do a_aiajckcjek
end do j_aiajckcjek
end do k_aiajckcjek
end do c_aiajckcjek
end do e_aiajckcjek
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, k, i
! Equalities: b == a, d == c, m == j, l == k
! No equalities independent of the above can hold.
!
e_aiajckckej: do e = nvirt0, nvirt1
c_aiajckckej: do c = e + 1, nvirt1
k_aiajckckej: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckckej: do j = nocc0, k - 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckckej: do a = c + 1, nvirt1
if (a == e) cycle a_aiajckckej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckckej: do i = j + 1, nocc1
if (i == k) cycle i_aiajckckej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckckej(eorb, t2, t1, nocc, nactive, a, i, j, c, k, e)
end do i_aiajckckej
end do a_aiajckckej
end do j_aiajckckej
end do k_aiajckckej
end do c_aiajckckej
end do e_aiajckckej
!
! Elementary loop 42
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajckdicj: do d = nvirt0, nvirt1
c_aiajckdicj: do c = nvirt0, d - 1
k_aiajckdicj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdicj: do j = nocc0, k - 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckdicj: do a = c + 1, nvirt1
if (a == d) cycle a_aiajckdicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdicj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckdicj(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdicj
end do a_aiajckdicj
end do j_aiajckdicj
end do k_aiajckdicj
end do c_aiajckdicj
end do d_aiajckdicj
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k, j
! Equalities: b == a, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
d_aiajckdick: do d = nvirt0, nvirt1
c_aiajckdick: do c = nvirt0, d - 1
k_aiajckdick: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdick: do j = nocc0, k - 1
a_aiajckdick: do a = c + 1, nvirt1
if (a == d) cycle a_aiajckdick
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdick: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckdick(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdick
end do a_aiajckdick
end do j_aiajckdick
end do k_aiajckdick
end do c_aiajckdick
end do d_aiajckdick
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, k, i
! Equalities: b == a, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
d_aiajckdjck: do d = nvirt0, nvirt1
c_aiajckdjck: do c = nvirt0, d - 1
k_aiajckdjck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdjck: do j = nocc0, k - 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckdjck: do a = c + 1, nvirt1
if (a == d) cycle a_aiajckdjck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdjck: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckdjck(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdjck
end do a_aiajckdjck
end do j_aiajckdjck
end do k_aiajckdjck
end do c_aiajckdjck
end do d_aiajckdjck
!
! Elementary loop 45
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, k, i
! Equalities: b == a, e == c, m == j, l == k
! No equalities independent of the above can hold.
!
d_aiajckdkcj: do d = nvirt0, nvirt1
c_aiajckdkcj: do c = nvirt0, d - 1
k_aiajckdkcj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdkcj: do j = nocc0, k - 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckdkcj: do a = c + 1, nvirt1
if (a == d) cycle a_aiajckdkcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdkcj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdkcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckdkcj(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdkcj
end do a_aiajckdkcj
end do j_aiajckdkcj
end do k_aiajckdkcj
end do c_aiajckdkcj
end do d_aiajckdkcj
!
! Elementary loop 46
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k, m
! Equalities: c == b, d == b, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjbkbibm: do m = nocc0, nocc1
k_aibjbkbibm: do k = nocc0, nocc1
if (k == m) cycle k_aibjbkbibm
b_aibjbkbibm: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbkbibm: do j = k + 1, nocc1
if (j == m) cycle j_aibjbkbibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbibm: do a = b + 1, nvirt1
i_aibjbkbibm: do i = m + 1, j - 1
if (i == k) cycle i_aibjbkbibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbibm(eorb, t2, t1, nocc, nactive, a, i, b, j, k, m)
end do i_aibjbkbibm
end do a_aibjbkbibm
end do j_aibjbkbibm
end do b_aibjbkbibm
end do k_aibjbkbibm
end do m_aibjbkbibm
!
! Elementary loop 47
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k, l
! Equalities: c == b, d == b, e == b, m == i
! No equalities independent of the above can hold.
!
l_aibjbkblbi: do l = nocc0, nocc1
k_aibjbkblbi: do k = nocc0, nocc1
if (k == l) cycle k_aibjbkblbi
b_aibjbkblbi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjbkblbi: do j = k + 1, nocc1
if (j == l) cycle j_aibjbkblbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkblbi: do a = b + 1, nvirt1
i1 = min(l, j)
i_aibjbkblbi: do i = nocc0, i1 - 1
if (i == k) cycle i_aibjbkblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkblbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k, l)
end do i_aibjbkblbi
end do a_aibjbkblbi
end do j_aibjbkblbi
end do b_aibjbkblbi
end do k_aibjbkblbi
end do l_aibjbkblbi
!
! Elementary loop 48
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, k, m
! Equalities: c == b, d == b, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjbkbjbm: do m = nocc0, nocc1
k_aibjbkbjbm: do k = nocc0, nocc1
if (k == m) cycle k_aibjbkbjbm
b_aibjbkbjbm: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(k, m)
j_aibjbkbjbm: do j = j0 + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbjbm: do a = b + 1, nvirt1
i_aibjbkbjbm: do i = nocc0, j - 1
if (i == k .or. i == m) cycle i_aibjbkbjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbjbm(eorb, t2, t1, nocc, nactive, a, i, b, j, k, m)
end do i_aibjbkbjbm
end do a_aibjbkbjbm
end do j_aibjbkbjbm
end do b_aibjbkbjbm
end do k_aibjbkbjbm
end do m_aibjbkbjbm
!
! Elementary loop 49
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, k, l
! Equalities: c == b, d == b, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjbkblbj: do l = nocc0, nocc1
k_aibjbkblbj: do k = nocc0, nocc1
if (k == l) cycle k_aibjbkblbj
b_aibjbkblbj: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjbkblbj: do j = k + 1, l - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkblbj: do a = b + 1, nvirt1
i_aibjbkblbj: do i = nocc0, j - 1
if (i == k .or. i == l) cycle i_aibjbkblbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkblbj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, l)
end do i_aibjbkblbj
end do a_aibjbkblbj
end do j_aibjbkblbj
end do b_aibjbkblbj
end do k_aibjbkblbj
end do l_aibjbkblbj
!
! Elementary loop 50
! --------------------
! Free virtual indices: b, a
! Free occupied indices: k, i, j, m
! Equalities: c == b, d == b, e == b, l == k
! No equalities independent of the above can hold.
!
m_aibjbkbkbm: do m = nocc0, nocc1
k_aibjbkbkbm: do k = m + 1, nocc1
b_aibjbkbkbm: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbkbkbm: do j = k + 1, nocc1
if (j == m) cycle j_aibjbkbkbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbkbm: do a = b + 1, nvirt1
i_aibjbkbkbm: do i = nocc0, j - 1
if (i == k .or. i == m) cycle i_aibjbkbkbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbkbm(eorb, t2, t1, nocc, nactive, a, i, b, j, k, m)
end do i_aibjbkbkbm
end do a_aibjbkbkbm
end do j_aibjbkbkbm
end do b_aibjbkbkbm
end do k_aibjbkbkbm
end do m_aibjbkbkbm
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
end subroutine ccjac_32_part3
end module ccjac_block_32_part3
