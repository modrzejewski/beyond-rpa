module ccjac_block_32_part5
use eom_cc3_32_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:41:38 UTC.
!
contains
 
subroutine ccjac_32_part5(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: b0, i0, i1
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
! Free occupied indices: i, k, j
! Equalities: d == b, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckbkbi: do c = nvirt0, nvirt1
k_aibjckbkbi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbkbi: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkbi: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbkbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbkbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbkbi
i_aibjckbkbi: do i = nocc0, k - 1
if (i == j) cycle i_aibjckbkbi
if (i > j .and. j > k) exit i_aibjckbkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbkbi
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbkbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbkbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbkbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbkbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbkbi
end do a_aibjckbkbi
end do j_aibjckbkbi
end do b_aibjckbkbi
end do k_aibjckbkbi
end do c_aibjckbkbi
!
! Elementary loop 2
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, k, i
! Equalities: d == b, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckbjbk: do c = nvirt0, nvirt1
k_aibjckbjbk: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbjbk: do b = c + 1, nvirt1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbjbk: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjbk: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbjbk
i_aibjckbjbk: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbjbk
if (i > j .and. j > k) exit i_aibjckbjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbjbk
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbjbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbjbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbjbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbjbk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbjbk
end do a_aibjckbjbk
end do j_aibjckbjbk
end do b_aibjckbjbk
end do k_aibjckbjbk
end do c_aibjckbjbk
!
! Elementary loop 3
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, k, i
! Equalities: d == b, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckbkbj: do c = nvirt0, nvirt1
k_aibjckbkbj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbkbj: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkbj: do j = nocc0, k - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbkbj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbkbj
i_aibjckbkbj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbkbj
if (i > j .and. j > k) exit i_aibjckbkbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbkbj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbkbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbkbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbkbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbkbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbkbj
end do a_aibjckbkbj
end do j_aibjckbkbj
end do b_aibjckbkbj
end do k_aibjckbkbj
end do c_aibjckbkbj
!
! Elementary loop 4
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjcibiei: do e = nvirt0, nvirt1
c_aibjcibiei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcibiei
b0 = max(c, e)
b_aibjcibiei: do b = b0 + 1, nvirt1
j_aibjcibiei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibiei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjcibiei
i_aibjcibiei: do i = nocc0, nocc1
if (i == j) cycle i_aibjcibiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcibiei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcibiei
end do a_aibjcibiei
end do j_aibjcibiei
end do b_aibjcibiei
end do c_aibjcibiei
end do e_aibjcibiei
!
! Elementary loop 5
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjcibiej: do e = nvirt0, nvirt1
c_aibjcibiej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcibiej
b0 = max(c, e)
b_aibjcibiej: do b = b0 + 1, nvirt1
j_aibjcibiej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibiej: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjcibiej
i_aibjcibiej: do i = nocc0, nocc1
if (i == j) cycle i_aibjcibiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcibiej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcibiej
end do a_aibjcibiej
end do j_aibjcibiej
end do b_aibjcibiej
end do c_aibjcibiej
end do e_aibjcibiej
!
! Elementary loop 6
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjcibjei: do e = nvirt0, nvirt1
c_aibjcibjei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcibjei
b0 = max(c, e)
b_aibjcibjei: do b = b0 + 1, nvirt1
j_aibjcibjei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibjei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjcibjei
i_aibjcibjei: do i = nocc0, nocc1
if (i == j) cycle i_aibjcibjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcibjei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcibjei
end do a_aibjcibjei
end do j_aibjcibjei
end do b_aibjcibjei
end do c_aibjcibjei
end do e_aibjcibjei
!
! Elementary loop 7
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjcjbjei: do e = nvirt0, nvirt1
c_aibjcjbjei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcjbjei
b0 = max(c, e)
b_aibjcjbjei: do b = b0 + 1, nvirt1
j_aibjcjbjei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbjei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjcjbjei
i_aibjcjbjei: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjbjei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcjbjei
end do a_aibjcjbjei
end do j_aibjcjbjei
end do b_aibjcjbjei
end do c_aibjcjbjei
end do e_aibjcjbjei
!
! Elementary loop 8
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: j, i
! Equalities: d == b, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjbjej: do e = nvirt0, nvirt1
c_aibjcjbjej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcjbjej
b0 = max(c, e)
b_aibjcjbjej: do b = b0 + 1, nvirt1
j_aibjcjbjej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbjej: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjcjbjej
i_aibjcjbjej: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjbjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjbjej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcjbjej
end do a_aibjcjbjej
end do j_aibjcjbjej
end do b_aibjcjbjej
end do c_aibjcjbjej
end do e_aibjcjbjej
!
! Elementary loop 9
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjcidibi: do d = nvirt0, nvirt1
c_aibjcidibi: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcidibi
b_aibjcidibi: do b = c + 1, d - 1
j_aibjcidibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidibi: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcidibi
i_aibjcidibi: do i = nocc0, nocc1
if (i == j) cycle i_aibjcidibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcidibi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcidibi
end do a_aibjcidibi
end do j_aibjcidibi
end do b_aibjcidibi
end do c_aibjcidibi
end do d_aibjcidibi
!
! Elementary loop 10
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjcidibj: do d = nvirt0, nvirt1
c_aibjcidibj: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcidibj
b_aibjcidibj: do b = c + 1, d - 1
j_aibjcidibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidibj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcidibj
i_aibjcidibj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcidibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcidibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcidibj
end do a_aibjcidibj
end do j_aibjcidibj
end do b_aibjcidibj
end do c_aibjcidibj
end do d_aibjcidibj
!
! Elementary loop 11
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdibj: do d = nvirt0, nvirt1
c_aibjcjdibj: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcjdibj
b_aibjcjdibj: do b = c + 1, d - 1
j_aibjcjdibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdibj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcjdibj
i_aibjcjdibj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjdibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdibj
end do a_aibjcjdibj
end do j_aibjcjdibj
end do b_aibjcjdibj
end do c_aibjcjdibj
end do d_aibjcjdibj
!
! Elementary loop 12
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidjbi: do d = nvirt0, nvirt1
c_aibjcidjbi: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcidjbi
b_aibjcidjbi: do b = c + 1, d - 1
j_aibjcidjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidjbi: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcidjbi
i_aibjcidjbi: do i = nocc0, nocc1
if (i == j) cycle i_aibjcidjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcidjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcidjbi
end do a_aibjcidjbi
end do j_aibjcidjbi
end do b_aibjcidjbi
end do c_aibjcidjbi
end do d_aibjcidjbi
!
! Elementary loop 13
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: j, i
! Equalities: e == b, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdjbj: do d = nvirt0, nvirt1
c_aibjcjdjbj: do c = nvirt0, nvirt1
if (c == d) cycle c_aibjcjdjbj
b_aibjcjdjbj: do b = c + 1, d - 1
j_aibjcjdjbj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdjbj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcjdjbj
i_aibjcjdjbj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjdjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjdjbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdjbj
end do a_aibjcjdjbj
end do j_aibjcjdjbj
end do b_aibjcjdjbj
end do c_aibjcjdjbj
end do d_aibjcjdjbj
!
! Elementary loop 14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == a, k == i, l == i
! No equalities independent of the above can hold.
!
m_aiajciaiam: do m = nocc0, nocc1
c_aiajciaiam: do c = nvirt0, nvirt1
j_aiajciaiam: do j = nocc0, nocc1
if (j == m) cycle j_aiajciaiam
a_aiajciaiam: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j, m)
i_aiajciaiam: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciaiam(eorb, t2, t1, nocc, nactive, a, i, j, c, m)
end do i_aiajciaiam
end do a_aiajciaiam
end do j_aiajciaiam
end do c_aiajciaiam
end do m_aiajciaiam
!
! Elementary loop 15
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == a, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiajckaiai: do c = nvirt0, nvirt1
k_aiajckaiai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiai: do j = nocc0, k - 1
a_aiajckaiai: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaiai: do i = j + 1, nocc1
if (i == k) cycle i_aiajckaiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckaiai(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckaiai
end do a_aiajckaiai
end do j_aiajckaiai
end do k_aiajckaiai
end do c_aiajckaiai
!
! Elementary loop 16
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aiajcjaiam: do m = nocc0, nocc1
c_aiajcjaiam: do c = nvirt0, nvirt1
j_aiajcjaiam: do j = nocc0, nocc1
if (j == m) cycle j_aiajcjaiam
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjaiam: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j, m)
i_aiajcjaiam: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjaiam(eorb, t2, t1, nocc, nactive, a, i, j, c, m)
end do i_aiajcjaiam
end do a_aiajcjaiam
end do j_aiajcjaiam
end do c_aiajcjaiam
end do m_aiajcjaiam
!
! Elementary loop 17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckaiaj: do c = nvirt0, nvirt1
k_aiajckaiaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiaj: do j = nocc0, k - 1
a_aiajckaiaj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaiaj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckaiaj(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckaiaj
end do a_aiajckaiaj
end do j_aiajckaiaj
end do k_aiajckaiaj
end do c_aiajckaiaj
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiajckaiak: do c = nvirt0, nvirt1
k_aiajckaiak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiak: do j = nocc0, k - 1
a_aiajckaiak: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j, k)
i_aiajckaiak: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckaiak(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckaiak
end do a_aiajckaiak
end do j_aiajckaiak
end do k_aiajckaiak
end do c_aiajckaiak
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == a, k == i, m == i
! No equalities independent of the above can hold.
!
l_aiajcialai: do l = nocc0, nocc1
c_aiajcialai: do c = nvirt0, nvirt1
j_aiajcialai: do j = nocc0, nocc1
if (j == l) cycle j_aiajcialai
a_aiajcialai: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajcialai: do i = j + 1, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajcialai(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcialai
end do a_aiajcialai
end do j_aiajcialai
end do c_aiajcialai
end do l_aiajcialai
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajciajam: do m = nocc0, nocc1
c_aiajciajam: do c = nvirt0, nvirt1
j_aiajciajam: do j = m + 1, nocc1
a_aiajciajam: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajciajam: do i = j + 1, nocc1
if (i == m) cycle i_aiajciajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciajam(eorb, t2, t1, nocc, nactive, a, i, j, c, m)
end do i_aiajciajam
end do a_aiajciajam
end do j_aiajciajam
end do c_aiajciajam
end do m_aiajciajam
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == a, k == i, m == j
! No equalities independent of the above can hold.
!
l_aiajcialaj: do l = nocc0, nocc1
c_aiajcialaj: do c = nvirt0, nvirt1
j_aiajcialaj: do j = nocc0, l - 1
a_aiajcialaj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcialaj: do i = j + 1, nocc1
if (i == l) cycle i_aiajcialaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajcialaj(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcialaj
end do a_aiajcialaj
end do j_aiajcialaj
end do c_aiajcialaj
end do l_aiajcialaj
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == a, m == i, k == j
! No equalities independent of the above can hold.
!
l_aiajcjalai: do l = nocc0, nocc1
c_aiajcjalai: do c = nvirt0, nvirt1
j_aiajcjalai: do j = nocc0, nocc1
if (j == l) cycle j_aiajcjalai
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjalai: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajcjalai: do i = j + 1, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjalai(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcjalai
end do a_aiajcjalai
end do j_aiajcjalai
end do c_aiajcjalai
end do l_aiajcjalai
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiajckakai: do c = nvirt0, nvirt1
k_aiajckakai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckakai: do j = nocc0, k - 1
a_aiajckakai: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakai: do i = j + 1, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckakai(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckakai
end do a_aiajckakai
end do j_aiajckakai
end do k_aiajckakai
end do c_aiajckakai
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, m
! Equalities: b == a, d == a, e == a, k == j, l == j
! No equalities independent of the above can hold.
!
m_aiajcjajam: do m = nocc0, nocc1
c_aiajcjajam: do c = nvirt0, nvirt1
j_aiajcjajam: do j = m + 1, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjajam: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajcjajam: do i = j + 1, nocc1
if (i == m) cycle i_aiajcjajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjajam(eorb, t2, t1, nocc, nactive, a, i, j, c, m)
end do i_aiajcjajam
end do a_aiajcjajam
end do j_aiajcjajam
end do c_aiajcjajam
end do m_aiajcjajam
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == a, e == a, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajckajaj: do c = nvirt0, nvirt1
k_aiajckajaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckajaj: do j = nocc0, k - 1
a_aiajckajaj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajaj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckajaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckajaj(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckajaj
end do a_aiajckajaj
end do j_aiajckajaj
end do k_aiajckajaj
end do c_aiajckajaj
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == a, l == j, m == k
! No equalities independent of the above can hold.
!
c_aiajckajak: do c = nvirt0, nvirt1
k_aiajckajak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckajak: do j = k + 1, k - 1
a_aiajckajak: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckajak: do i = j + 1, nocc1
if (i == k) cycle i_aiajckajak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckajak(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckajak
end do a_aiajckajak
end do j_aiajckajak
end do k_aiajckajak
end do c_aiajckajak
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == a, e == a, k == j, m == j
! No equalities independent of the above can hold.
!
l_aiajcjalaj: do l = nocc0, nocc1
c_aiajcjalaj: do c = nvirt0, nvirt1
j_aiajcjalaj: do j = nocc0, l - 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjalaj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjalaj: do i = j + 1, nocc1
if (i == l) cycle i_aiajcjalaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjalaj(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcjalaj
end do a_aiajcjalaj
end do j_aiajcjalaj
end do c_aiajcjalaj
end do l_aiajcjalaj
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == a, m == j, l == k
! No equalities independent of the above can hold.
!
c_aiajckakaj: do c = nvirt0, nvirt1
k_aiajckakaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckakaj: do j = nocc0, k - 1
a_aiajckakaj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckakaj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckakaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckakaj(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckakaj
end do a_aiajckakaj
end do j_aiajckakaj
end do k_aiajckakaj
end do c_aiajckakaj
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j
! Equalities: b == a, d == a, e == a, l == k, m == k
! No equalities independent of the above can hold.
!
c_aiajckakak: do c = nvirt0, nvirt1
k_aiajckakak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckakak: do j = nocc0, k - 1
a_aiajckakak: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakak: do i = j + 1, nocc1
if (i == k) cycle i_aiajckakak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckakak(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckakak
end do a_aiajckakak
end do j_aiajckakak
end do k_aiajckakak
end do c_aiajckakak
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: d == a, e == a, c == b, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjbkaiak: do k = nocc0, nocc1
b_aibjbkaiak: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaiak: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkaiak: do a = b + 1, nvirt1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjbkaiak: do i = k + 1, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkaiak(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkaiak
end do a_aibjbkaiak
end do j_aibjbkaiak
end do b_aibjbkaiak
end do k_aibjbkaiak
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: d == a, e == a, c == b, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjbkakai: do k = nocc0, nocc1
b_aibjbkakai: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkakai: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkakai: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k, j)
i_aibjbkakai: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkakai(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkakai
end do a_aibjbkakai
end do j_aibjbkakai
end do b_aibjbkakai
end do k_aibjbkakai
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: d == a, e == a, c == b, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjbkajak: do k = nocc0, nocc1
b_aibjbkajak: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkajak: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkajak: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjbkajak: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkajak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkajak(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkajak
end do a_aibjbkajak
end do j_aibjbkajak
end do b_aibjbkajak
end do k_aibjbkajak
!
! Elementary loop 33
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjciaiai: do c = nvirt0, nvirt1
b_aibjciaiai: do b = c + 1, nvirt1
j_aibjciaiai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciaiai: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciaiai
i_aibjciaiai: do i = nocc0, nocc1
if (i == j) cycle i_aibjciaiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciaiai(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciaiai
end do a_aibjciaiai
end do j_aibjciaiai
end do b_aibjciaiai
end do c_aibjciaiai
!
! Elementary loop 34
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciaiaj: do c = nvirt0, nvirt1
b_aibjciaiaj: do b = c + 1, nvirt1
j_aibjciaiaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciaiaj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciaiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciaiaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciaiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciaiaj
end do a_aibjciaiaj
end do j_aibjciaiaj
end do b_aibjciaiaj
end do c_aibjciaiaj
!
! Elementary loop 35
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjaiaj: do c = nvirt0, nvirt1
b_aibjcjaiaj: do b = c + 1, nvirt1
j_aibjcjaiaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjaiaj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjaiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjaiaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjaiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjaiaj
end do a_aibjcjaiaj
end do j_aibjcjaiaj
end do b_aibjcjaiaj
end do c_aibjcjaiaj
!
! Elementary loop 36
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajai: do c = nvirt0, nvirt1
b_aibjciajai: do b = c + 1, nvirt1
j_aibjciajai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciajai: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciajai
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajai: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciajai(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciajai
end do a_aibjciajai
end do j_aibjciajai
end do b_aibjciajai
end do c_aibjciajai
!
! Elementary loop 37
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjajai: do c = nvirt0, nvirt1
b_aibjcjajai: do b = c + 1, nvirt1
j_aibjcjajai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjajai: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjajai
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajai: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjajai(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjajai
end do a_aibjcjajai
end do j_aibjcjajai
end do b_aibjcjajai
end do c_aibjcjajai
!
! Elementary loop 38
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i
! Equalities: d == a, e == a, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjajaj: do c = nvirt0, nvirt1
b_aibjcjajaj: do b = c + 1, nvirt1
j_aibjcjajaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjajaj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjajaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajaj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjajaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjajaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjajaj
end do a_aibjcjajaj
end do j_aibjcjajaj
end do b_aibjcjajaj
end do c_aibjcjajaj
!
! Elementary loop 39
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == c, k == i, l == i
! No equalities independent of the above can hold.
!
m_aiajciaicm: do m = nocc0, nocc1
c_aiajciaicm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
j_aiajciaicm: do j = nocc0, nocc1
if (j == m) cycle j_aiajciaicm
a_aiajciaicm: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciaicm: do i = j + 1, nocc1
if (i == m) cycle i_aiajciaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciaicm(eorb, t2, t1, nocc, nactive, a, i, j, c, m)
end do i_aiajciaicm
end do a_aiajciaicm
end do j_aiajciaicm
end do c_aiajciaicm
end do m_aiajciaicm
!
! Elementary loop 40
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == c, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiajckaici: do c = nvirt0, nvirt1
k_aiajckaici: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaici: do j = nocc0, k - 1
a_aiajckaici: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaici: do i = j + 1, nocc1
if (i == k) cycle i_aiajckaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckaici(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckaici
end do a_aiajckaici
end do j_aiajckaici
end do k_aiajckaici
end do c_aiajckaici
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == c, l == i, k == j
! No equalities independent of the above can hold.
!
m_aiajcjaicm: do m = nocc0, nocc1
c_aiajcjaicm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
j_aiajcjaicm: do j = nocc0, nocc1
if (j == m) cycle j_aiajcjaicm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjaicm: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjaicm: do i = j + 1, nocc1
if (i == m) cycle i_aiajcjaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjaicm(eorb, t2, t1, nocc, nactive, a, i, j, c, m)
end do i_aiajcjaicm
end do a_aiajcjaicm
end do j_aiajcjaicm
end do c_aiajcjaicm
end do m_aiajcjaicm
!
! Elementary loop 42
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckaicj: do c = nvirt0, nvirt1
k_aiajckaicj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaicj: do j = nocc0, k - 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckaicj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaicj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckaicj(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckaicj
end do a_aiajckaicj
end do j_aiajckaicj
end do k_aiajckaicj
end do c_aiajckaicj
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiajckaick: do c = nvirt0, nvirt1
k_aiajckaick: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaick: do j = nocc0, k - 1
a_aiajckaick: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaick: do i = j + 1, nocc1
if (i == k) cycle i_aiajckaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckaick(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckaick
end do a_aiajckaick
end do j_aiajckaick
end do k_aiajckaick
end do c_aiajckaick
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == c, k == i, m == i
! No equalities independent of the above can hold.
!
l_aiajcialci: do l = nocc0, nocc1
c_aiajcialci: do c = nvirt0, nvirt1
j_aiajcialci: do j = nocc0, nocc1
if (j == l) cycle j_aiajcialci
a_aiajcialci: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajcialci: do i = j + 1, nocc1
if (i == l) cycle i_aiajcialci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajcialci(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcialci
end do a_aiajcialci
end do j_aiajcialci
end do c_aiajcialci
end do l_aiajcialci
!
! Elementary loop 45
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajciajcm: do m = nocc0, nocc1
c_aiajciajcm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
j_aiajciajcm: do j = nocc0, nocc1
if (j == m) cycle j_aiajciajcm
a_aiajciajcm: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciajcm: do i = j + 1, nocc1
if (i == m) cycle i_aiajciajcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciajcm(eorb, t2, t1, nocc, nactive, a, i, j, c, m)
end do i_aiajciajcm
end do a_aiajciajcm
end do j_aiajciajcm
end do c_aiajciajcm
end do m_aiajciajcm
!
! Elementary loop 46
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aiajcialcj: do l = nocc0, nocc1
c_aiajcialcj: do c = nvirt0, nvirt1
j_aiajcialcj: do j = nocc0, nocc1
if (j == l) cycle j_aiajcialcj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcialcj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajcialcj: do i = j + 1, nocc1
if (i == l) cycle i_aiajcialcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajcialcj(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcialcj
end do a_aiajcialcj
end do j_aiajcialcj
end do c_aiajcialcj
end do l_aiajcialcj
!
! Elementary loop 47
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajckajci: do c = nvirt0, nvirt1
k_aiajckajci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckajci: do j = nocc0, k - 1
a_aiajckajci: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajci: do i = j + 1, nocc1
if (i == k) cycle i_aiajckajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckajci(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckajci
end do a_aiajckajci
end do j_aiajckajci
end do k_aiajckajci
end do c_aiajckajci
!
! Elementary loop 48
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aiajcjalci: do l = nocc0, nocc1
c_aiajcjalci: do c = nvirt0, nvirt1
j_aiajcjalci: do j = nocc0, nocc1
if (j == l) cycle j_aiajcjalci
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjalci: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajcjalci: do i = j + 1, nocc1
if (i == l) cycle i_aiajcjalci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjalci(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcjalci
end do a_aiajcjalci
end do j_aiajcjalci
end do c_aiajcjalci
end do l_aiajcjalci
!
! Elementary loop 49
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiajckakci: do c = nvirt0, nvirt1
k_aiajckakci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckakci: do j = nocc0, k - 1
a_aiajckakci: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakci: do i = j + 1, nocc1
if (i == k) cycle i_aiajckakci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckakci(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckakci
end do a_aiajckakci
end do j_aiajckakci
end do k_aiajckakci
end do c_aiajckakci
!
! Elementary loop 50
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, m
! Equalities: b == a, d == a, e == c, k == j, l == j
! No equalities independent of the above can hold.
!
m_aiajcjajcm: do m = nocc0, nocc1
c_aiajcjajcm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
j_aiajcjajcm: do j = nocc0, nocc1
if (j == m) cycle j_aiajcjajcm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjajcm: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjajcm: do i = j + 1, nocc1
if (i == m) cycle i_aiajcjajcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjajcm(eorb, t2, t1, nocc, nactive, a, i, j, c, m)
end do i_aiajcjajcm
end do a_aiajcjajcm
end do j_aiajcjajcm
end do c_aiajcjajcm
end do m_aiajcjajcm
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
end subroutine ccjac_32_part5
end module ccjac_block_32_part5
