module ccjac_block_32_part4
use eom_cc3_32_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:41:38 UTC.
!
contains
 
subroutine ccjac_32_part4(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
! Free virtual indices: b, a
! Free occupied indices: k, i, j, l
! Equalities: c == b, d == b, e == b, m == k
! No equalities independent of the above can hold.
!
l_aibjbkblbk: do l = nocc0, nocc1
k_aibjbkblbk: do k = nocc0, l - 1
b_aibjbkblbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkblbk: do j = k + 1, nocc1
if (j == l) cycle j_aibjbkblbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkblbk: do a = b + 1, nvirt1
i_aibjbkblbk: do i = nocc0, j - 1
if (i == k .or. i == l) cycle i_aibjbkblbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkblbk(eorb, t2, t1, nocc, nactive, a, i, b, j, k, l)
end do i_aibjbkblbk
end do a_aibjbkblbk
end do j_aibjbkblbk
end do b_aibjbkblbk
end do k_aibjbkblbk
end do l_aibjbkblbk
!
! Elementary loop 2
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, k
! Equalities: d == c, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckcicj: do c = nvirt0, nvirt1
k_aibjckcicj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckcicj: do b = c + 1, nvirt1
j_aibjckcicj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckcicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcicj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckcicj
i_aibjckcicj: do i = j + 1, nocc1
if (i == k) cycle i_aibjckcicj
if (i > j .and. j > k) exit i_aibjckcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckcicj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckcicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckcicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckcicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckcicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckcicj
end do a_aibjckcicj
end do j_aibjckcicj
end do b_aibjckcicj
end do k_aibjckcicj
end do c_aibjckcicj
!
! Elementary loop 3
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, k, j
! Equalities: d == c, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckcick: do c = nvirt0, nvirt1
k_aibjckcick: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckcick: do b = c + 1, nvirt1
j_aibjckcick: do j = nocc0, nocc1
if (j == k) cycle j_aibjckcick
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcick: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckcick
i_aibjckcick: do i = k + 1, nocc1
if (i == j) cycle i_aibjckcick
if (i > j .and. j > k) exit i_aibjckcick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckcick
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckcick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckcick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckcick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckcick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckcick
end do a_aibjckcick
end do j_aibjckcick
end do b_aibjckcick
end do k_aibjckcick
end do c_aibjckcick
!
! Elementary loop 4
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, k
! Equalities: d == c, e == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckcjci: do c = nvirt0, nvirt1
k_aibjckcjci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckcjci: do b = c + 1, nvirt1
j_aibjckcjci: do j = nocc0, nocc1
if (j == k) cycle j_aibjckcjci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcjci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckcjci
i_aibjckcjci: do i = nocc0, j - 1
if (i == k) cycle i_aibjckcjci
if (i > j .and. j > k) exit i_aibjckcjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckcjci
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckcjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckcjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckcjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckcjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckcjci
end do a_aibjckcjci
end do j_aibjckcjci
end do b_aibjckcjci
end do k_aibjckcjci
end do c_aibjckcjci
!
! Elementary loop 5
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, k, j
! Equalities: d == c, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckckci: do c = nvirt0, nvirt1
k_aibjckckci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckckci: do b = c + 1, nvirt1
j_aibjckckci: do j = nocc0, nocc1
if (j == k) cycle j_aibjckckci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckckci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckckci
i_aibjckckci: do i = nocc0, k - 1
if (i == j) cycle i_aibjckckci
if (i > j .and. j > k) exit i_aibjckckci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckckci
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckckci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckckci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckckci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckckci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckckci
end do a_aibjckckci
end do j_aibjckckci
end do b_aibjckckci
end do k_aibjckckci
end do c_aibjckckci
!
! Elementary loop 6
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, k, i
! Equalities: d == c, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckcjck: do c = nvirt0, nvirt1
k_aibjckcjck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckcjck: do b = c + 1, nvirt1
j_aibjckcjck: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcjck: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckcjck
i_aibjckcjck: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckcjck
if (i > j .and. j > k) exit i_aibjckcjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckcjck
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckcjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckcjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckcjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckcjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckcjck
end do a_aibjckcjck
end do j_aibjckcjck
end do b_aibjckcjck
end do k_aibjckcjck
end do c_aibjckcjck
!
! Elementary loop 7
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, k, i
! Equalities: d == c, e == c, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckckcj: do c = nvirt0, nvirt1
k_aibjckckcj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckckcj: do b = c + 1, nvirt1
j_aibjckckcj: do j = nocc0, k - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckckcj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckckcj
i_aibjckckcj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckckcj
if (i > j .and. j > k) exit i_aibjckckcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckckcj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckckcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckckcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckckcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckckcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckckcj
end do a_aibjckckcj
end do j_aibjckckcj
end do b_aibjckckcj
end do k_aibjckckcj
end do c_aibjckckcj
!
! Elementary loop 8
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbkbiej: do e = nvirt0, nvirt1
k_aibjbkbiej: do k = nocc0, nocc1
b_aibjbkbiej: do b = e + 1, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbiej: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbiej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbkbiej
i_aibjbkbiej: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbiej(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjbkbiej
end do a_aibjbkbiej
end do j_aibjbkbiej
end do b_aibjbkbiej
end do k_aibjbkbiej
end do e_aibjbkbiej
!
! Elementary loop 9
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, k, j
! Equalities: c == b, d == b, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibjbkbiek: do e = nvirt0, nvirt1
k_aibjbkbiek: do k = nocc0, nocc1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkbiek: do b = e + 1, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbiek: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbiek: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbkbiek
i_aibjbkbiek: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbiek(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjbkbiek
end do a_aibjbkbiek
end do j_aibjbkbiek
end do b_aibjbkbiek
end do k_aibjbkbiek
end do e_aibjbkbiek
!
! Elementary loop 10
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbkbjei: do e = nvirt0, nvirt1
k_aibjbkbjei: do k = nocc0, nocc1
b_aibjbkbjei: do b = e + 1, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbjei: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbjei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbkbjei
i_aibjbkbjei: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbjei(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjbkbjei
end do a_aibjbkbjei
end do j_aibjbkbjei
end do b_aibjbkbjei
end do k_aibjbkbjei
end do e_aibjbkbjei
!
! Elementary loop 11
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, k, j
! Equalities: c == b, d == b, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibjbkbkei: do e = nvirt0, nvirt1
k_aibjbkbkei: do k = nocc0, nocc1
b_aibjbkbkei: do b = e + 1, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbkei: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbkei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbkbkei
i_aibjbkbkei: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbkei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbkei(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjbkbkei
end do a_aibjbkbkei
end do j_aibjbkbkei
end do b_aibjbkbkei
end do k_aibjbkbkei
end do e_aibjbkbkei
!
! Elementary loop 12
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: j, k, i
! Equalities: c == b, d == b, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjbkbjek: do e = nvirt0, nvirt1
k_aibjbkbjek: do k = nocc0, nocc1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkbjek: do b = e + 1, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbjek: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbjek: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbkbjek
i_aibjbkbjek: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbjek(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjbkbjek
end do a_aibjbkbjek
end do j_aibjbkbjek
end do b_aibjbkbjek
end do k_aibjbkbjek
end do e_aibjbkbjek
!
! Elementary loop 13
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: j, k, i
! Equalities: c == b, d == b, m == j, l == k
! No equalities independent of the above can hold.
!
e_aibjbkbkej: do e = nvirt0, nvirt1
k_aibjbkbkej: do k = nocc0, nocc1
b_aibjbkbkej: do b = e + 1, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbkej: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbkej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbkbkej
i_aibjbkbkej: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbkej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbkej(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjbkbkej
end do a_aibjbkbkej
end do j_aibjbkbkej
end do b_aibjbkbkej
end do k_aibjbkbkej
end do e_aibjbkbkej
!
! Elementary loop 14
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjciciei: do e = nvirt0, nvirt1
c_aibjciciei: do c = e + 1, nvirt1
b_aibjciciei: do b = c + 1, nvirt1
if (b == e) cycle b_aibjciciei
j_aibjciciei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciciei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjciciei
i_aibjciciei: do i = nocc0, nocc1
if (i == j) cycle i_aibjciciei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciciei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjciciei
end do a_aibjciciei
end do j_aibjciciei
end do b_aibjciciei
end do c_aibjciciei
end do e_aibjciciei
!
! Elementary loop 15
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjciciej: do e = nvirt0, nvirt1
c_aibjciciej: do c = e + 1, nvirt1
b_aibjciciej: do b = c + 1, nvirt1
if (b == e) cycle b_aibjciciej
j_aibjciciej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciciej: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjciciej
i_aibjciciej: do i = nocc0, nocc1
if (i == j) cycle i_aibjciciej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciciej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjciciej
end do a_aibjciciej
end do j_aibjciciej
end do b_aibjciciej
end do c_aibjciciej
end do e_aibjciciej
!
! Elementary loop 16
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j
! Equalities: d == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjciej: do e = nvirt0, nvirt1
c_aibjcjciej: do c = e + 1, nvirt1
b_aibjcjciej: do b = c + 1, nvirt1
if (b == e) cycle b_aibjcjciej
j_aibjcjciej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjciej: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjcjciej
i_aibjcjciej: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjciej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjciej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcjciej
end do a_aibjcjciej
end do j_aibjcjciej
end do b_aibjcjciej
end do c_aibjcjciej
end do e_aibjcjciej
!
! Elementary loop 17
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j
! Equalities: d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjcicjei: do e = nvirt0, nvirt1
c_aibjcicjei: do c = e + 1, nvirt1
b_aibjcicjei: do b = c + 1, nvirt1
if (b == e) cycle b_aibjcicjei
j_aibjcicjei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicjei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjcicjei
i_aibjcicjei: do i = nocc0, nocc1
if (i == j) cycle i_aibjcicjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcicjei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcicjei
end do a_aibjcicjei
end do j_aibjcicjei
end do b_aibjcicjei
end do c_aibjcicjei
end do e_aibjcicjei
!
! Elementary loop 18
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j
! Equalities: d == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjcjcjei: do e = nvirt0, nvirt1
c_aibjcjcjei: do c = e + 1, nvirt1
b_aibjcjcjei: do b = c + 1, nvirt1
if (b == e) cycle b_aibjcjcjei
j_aibjcjcjei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjcjei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjcjcjei
i_aibjcjcjei: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjcjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjcjei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcjcjei
end do a_aibjcjcjei
end do j_aibjcjcjei
end do b_aibjcjcjei
end do c_aibjcjcjei
end do e_aibjcjcjei
!
! Elementary loop 19
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: j, i
! Equalities: d == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjcjej: do e = nvirt0, nvirt1
c_aibjcjcjej: do c = e + 1, nvirt1
b_aibjcjcjej: do b = c + 1, nvirt1
if (b == e) cycle b_aibjcjcjej
j_aibjcjcjej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjcjej: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjcjcjej
i_aibjcjcjej: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjcjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjcjej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcjcjej
end do a_aibjcjcjej
end do j_aibjcjcjej
end do b_aibjcjcjej
end do c_aibjcjcjej
end do e_aibjcjcjej
!
! Elementary loop 20
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbkdibj: do d = nvirt0, nvirt1
k_aibjbkdibj: do k = nocc0, nocc1
b_aibjbkdibj: do b = nvirt0, d - 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdibj: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdibj: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbkdibj
i_aibjbkdibj: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkdibj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdibj
end do a_aibjbkdibj
end do j_aibjbkdibj
end do b_aibjbkdibj
end do k_aibjbkdibj
end do d_aibjbkdibj
!
! Elementary loop 21
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k, j
! Equalities: c == b, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjbkdibk: do d = nvirt0, nvirt1
k_aibjbkdibk: do k = nocc0, nocc1
b_aibjbkdibk: do b = nvirt0, d - 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdibk: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdibk: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbkdibk
i_aibjbkdibk: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkdibk(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdibk
end do a_aibjbkdibk
end do j_aibjbkdibk
end do b_aibjbkdibk
end do k_aibjbkdibk
end do d_aibjbkdibk
!
! Elementary loop 22
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdjbi: do d = nvirt0, nvirt1
k_aibjbkdjbi: do k = nocc0, nocc1
b_aibjbkdjbi: do b = nvirt0, d - 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdjbi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdjbi: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbkdjbi
i_aibjbkdjbi: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkdjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdjbi
end do a_aibjbkdjbi
end do j_aibjbkdjbi
end do b_aibjbkdjbi
end do k_aibjbkdjbi
end do d_aibjbkdjbi
!
! Elementary loop 23
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k, j
! Equalities: c == b, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibjbkdkbi: do d = nvirt0, nvirt1
k_aibjbkdkbi: do k = nocc0, nocc1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkdkbi: do b = nvirt0, d - 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdkbi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdkbi: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbkdkbi
i_aibjbkdkbi: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkdkbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdkbi
end do a_aibjbkdkbi
end do j_aibjbkdkbi
end do b_aibjbkdkbi
end do k_aibjbkdkbi
end do d_aibjbkdkbi
!
! Elementary loop 24
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, k, i
! Equalities: c == b, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjbkdjbk: do d = nvirt0, nvirt1
k_aibjbkdjbk: do k = nocc0, nocc1
b_aibjbkdjbk: do b = nvirt0, d - 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdjbk: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdjbk: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbkdjbk
i_aibjbkdjbk: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkdjbk(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdjbk
end do a_aibjbkdjbk
end do j_aibjbkdjbk
end do b_aibjbkdjbk
end do k_aibjbkdjbk
end do d_aibjbkdjbk
!
! Elementary loop 25
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, k, i
! Equalities: c == b, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
d_aibjbkdkbj: do d = nvirt0, nvirt1
k_aibjbkdkbj: do k = nocc0, nocc1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkdkbj: do b = nvirt0, d - 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdkbj: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdkbj: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbkdkbj
i_aibjbkdkbj: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdkbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkdkbj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdkbj
end do a_aibjbkdkbj
end do j_aibjbkdkbj
end do b_aibjbkdkbj
end do k_aibjbkdkbj
end do d_aibjbkdkbj
!
! Elementary loop 26
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, m
! Equalities: d == b, e == c, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjcibicm: do m = nocc0, nocc1
c_aibjcibicm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b_aibjcibicm: do b = c + 1, nvirt1
j_aibjcibicm: do j = nocc0, nocc1
if (j == m) cycle j_aibjcibicm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibicm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibicm
i_aibjcibicm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcibicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcibicm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcibicm
end do a_aibjcibicm
end do j_aibjcibicm
end do b_aibjcibicm
end do c_aibjcibicm
end do m_aibjcibicm
!
! Elementary loop 27
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k
! Equalities: d == b, e == c, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjckbici: do c = nvirt0, nvirt1
k_aibjckbici: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbici: do b = c + 1, nvirt1
j_aibjckbici: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbici
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbici: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbici
i_aibjckbici: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbici
if (i > j .and. j > k) exit i_aibjckbici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbici
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbici(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbici(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbici(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbici(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbici
end do a_aibjckbici
end do j_aibjckbici
end do b_aibjckbici
end do k_aibjckbici
end do c_aibjckbici
!
! Elementary loop 28
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k
! Equalities: d == b, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckbicj: do c = nvirt0, nvirt1
k_aibjckbicj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbicj: do b = c + 1, nvirt1
j_aibjckbicj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbicj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbicj
i_aibjckbicj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbicj
if (i > j .and. j > k) exit i_aibjckbicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbicj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = 0.d+0
end if
end do i_aibjckbicj
end do a_aibjckbicj
end do j_aibjckbicj
end do b_aibjckbicj
end do k_aibjckbicj
end do c_aibjckbicj
!
! Elementary loop 29
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k, j
! Equalities: d == b, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckbick: do c = nvirt0, nvirt1
k_aibjckbick: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbick: do b = c + 1, nvirt1
j_aibjckbick: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbick
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbick: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbick
i_aibjckbick: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbick
if (i > j .and. j > k) exit i_aibjckbick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbick
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbick(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbick
end do a_aibjckbick
end do j_aibjckbick
end do b_aibjckbick
end do k_aibjckbick
end do c_aibjckbick
!
! Elementary loop 30
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, l
! Equalities: d == b, e == c, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjciblci: do l = nocc0, nocc1
c_aibjciblci: do c = nvirt0, nvirt1
b_aibjciblci: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjciblci: do j = nocc0, nocc1
if (j == l) cycle j_aibjciblci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciblci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciblci
i_aibjciblci: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjciblci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciblci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjciblci
end do a_aibjciblci
end do j_aibjciblci
end do b_aibjciblci
end do c_aibjciblci
end do l_aibjciblci
!
! Elementary loop 31
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, m
! Equalities: d == b, e == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcibjcm: do m = nocc0, nocc1
c_aibjcibjcm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b_aibjcibjcm: do b = c + 1, nvirt1
j_aibjcibjcm: do j = nocc0, nocc1
if (j == m) cycle j_aibjcibjcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibjcm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibjcm
i_aibjcibjcm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcibjcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcibjcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcibjcm
end do a_aibjcibjcm
end do j_aibjcibjcm
end do b_aibjcibjcm
end do c_aibjcibjcm
end do m_aibjcibjcm
!
! Elementary loop 32
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, l
! Equalities: d == b, e == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjciblcj: do l = nocc0, nocc1
c_aibjciblcj: do c = nvirt0, nvirt1
b_aibjciblcj: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjciblcj: do j = nocc0, nocc1
if (j == l) cycle j_aibjciblcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciblcj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciblcj
i_aibjciblcj: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjciblcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciblcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjciblcj
end do a_aibjciblcj
end do j_aibjciblcj
end do b_aibjciblcj
end do c_aibjciblcj
end do l_aibjciblcj
!
! Elementary loop 33
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k
! Equalities: d == b, e == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckbjci: do c = nvirt0, nvirt1
k_aibjckbjci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbjci: do b = c + 1, nvirt1
j_aibjckbjci: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbjci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbjci
i_aibjckbjci: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbjci
if (i > j .and. j > k) exit i_aibjckbjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbjci
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbjci
end do a_aibjckbjci
end do j_aibjckbjci
end do b_aibjckbjci
end do k_aibjckbjci
end do c_aibjckbjci
!
! Elementary loop 34
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, l
! Equalities: d == b, e == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjblci: do l = nocc0, nocc1
c_aibjcjblci: do c = nvirt0, nvirt1
b_aibjcjblci: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcjblci: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjblci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjblci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjblci
i_aibjcjblci: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjblci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjblci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjblci
end do a_aibjcjblci
end do j_aibjcjblci
end do b_aibjcjblci
end do c_aibjcjblci
end do l_aibjcjblci
!
! Elementary loop 35
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k, j
! Equalities: d == b, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckbkci: do c = nvirt0, nvirt1
k_aibjckbkci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbkci: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkci: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbkci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbkci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbkci
i_aibjckbkci: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbkci
if (i > j .and. j > k) exit i_aibjckbkci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbkci
      else if (k > i) then
            jac(ibra, iket) = 0.d+0
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbkci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbkci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbkci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbkci
end do a_aibjckbkci
end do j_aibjckbkci
end do b_aibjckbkci
end do k_aibjckbkci
end do c_aibjckbkci
!
! Elementary loop 36
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i, m
! Equalities: d == b, e == c, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjcjbjcm: do m = nocc0, nocc1
c_aibjcjbjcm: do c = nvirt0, nvirt1
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b_aibjcjbjcm: do b = c + 1, nvirt1
j_aibjcjbjcm: do j = nocc0, nocc1
if (j == m) cycle j_aibjcjbjcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbjcm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbjcm
i_aibjcjbjcm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcjbjcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjbjcm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcjbjcm
end do a_aibjcjbjcm
end do j_aibjcjbjcm
end do b_aibjcjbjcm
end do c_aibjcjbjcm
end do m_aibjcjbjcm
!
! Elementary loop 37
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i, k
! Equalities: d == b, e == c, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjckbjcj: do c = nvirt0, nvirt1
k_aibjckbjcj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbjcj: do b = c + 1, nvirt1
j_aibjckbjcj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbjcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjcj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbjcj
i_aibjckbjcj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbjcj
if (i > j .and. j > k) exit i_aibjckbjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbjcj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbjcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbjcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbjcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbjcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbjcj
end do a_aibjckbjcj
end do j_aibjckbjcj
end do b_aibjckbjcj
end do k_aibjckbjcj
end do c_aibjckbjcj
!
! Elementary loop 38
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, k, i
! Equalities: d == b, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckbjck: do c = nvirt0, nvirt1
k_aibjckbjck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbjck: do b = c + 1, nvirt1
j_aibjckbjck: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbjck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjck: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbjck
i_aibjckbjck: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbjck
if (i > j .and. j > k) exit i_aibjckbjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbjck
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbjck
end do a_aibjckbjck
end do j_aibjckbjck
end do b_aibjckbjck
end do k_aibjckbjck
end do c_aibjckbjck
!
! Elementary loop 39
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i, l
! Equalities: d == b, e == c, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjcjblcj: do l = nocc0, nocc1
c_aibjcjblcj: do c = nvirt0, nvirt1
b_aibjcjblcj: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcjblcj: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjblcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjblcj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjblcj
i_aibjcjblcj: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjblcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjblcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjblcj
end do a_aibjcjblcj
end do j_aibjcjblcj
end do b_aibjcjblcj
end do c_aibjcjblcj
end do l_aibjcjblcj
!
! Elementary loop 40
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, k, i
! Equalities: d == b, e == c, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckbkcj: do c = nvirt0, nvirt1
k_aibjckbkcj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbkcj: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkcj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbkcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbkcj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbkcj
i_aibjckbkcj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbkcj
if (i > j .and. j > k) exit i_aibjckbkcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbkcj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbkcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbkcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbkcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbkcj
end do a_aibjckbkcj
end do j_aibjckbkcj
end do b_aibjckbkcj
end do k_aibjckbkcj
end do c_aibjckbkcj
!
! Elementary loop 41
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: k, i, j
! Equalities: d == b, e == c, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibjckbkck: do c = nvirt0, nvirt1
k_aibjckbkck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbkck: do b = c + 1, nvirt1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkck: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbkck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbkck: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbkck
i_aibjckbkck: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbkck
if (i > j .and. j > k) exit i_aibjckbkck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbkck
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbkck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = 0.d+0
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbkck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbkck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbkck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbkck
end do a_aibjckbkck
end do j_aibjckbkck
end do b_aibjckbkck
end do k_aibjckbkck
end do c_aibjckbkck
!
! Elementary loop 42
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j
! Equalities: e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjcidici: do d = nvirt0, nvirt1
c_aibjcidici: do c = nvirt0, d - 1
b_aibjcidici: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcidici
j_aibjcidici: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidici: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcidici
i_aibjcidici: do i = nocc0, nocc1
if (i == j) cycle i_aibjcidici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcidici(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcidici
end do a_aibjcidici
end do j_aibjcidici
end do b_aibjcidici
end do c_aibjcidici
end do d_aibjcidici
!
! Elementary loop 43
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j
! Equalities: e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjcidicj: do d = nvirt0, nvirt1
c_aibjcidicj: do c = nvirt0, d - 1
b_aibjcidicj: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcidicj
j_aibjcidicj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidicj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcidicj
i_aibjcidicj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcidicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcidicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcidicj
end do a_aibjcidicj
end do j_aibjcidicj
end do b_aibjcidicj
end do c_aibjcidicj
end do d_aibjcidicj
!
! Elementary loop 44
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j
! Equalities: e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdicj: do d = nvirt0, nvirt1
c_aibjcjdicj: do c = nvirt0, d - 1
b_aibjcjdicj: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcjdicj
j_aibjcjdicj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdicj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcjdicj
i_aibjcjdicj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjdicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjdicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdicj
end do a_aibjcjdicj
end do j_aibjcjdicj
end do b_aibjcjdicj
end do c_aibjcjdicj
end do d_aibjcjdicj
!
! Elementary loop 45
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j
! Equalities: e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidjci: do d = nvirt0, nvirt1
c_aibjcidjci: do c = nvirt0, d - 1
b_aibjcidjci: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcidjci
j_aibjcidjci: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidjci: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcidjci
i_aibjcidjci: do i = nocc0, nocc1
if (i == j) cycle i_aibjcidjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcidjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcidjci
end do a_aibjcidjci
end do j_aibjcidjci
end do b_aibjcidjci
end do c_aibjcidjci
end do d_aibjcidjci
!
! Elementary loop 46
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j
! Equalities: e == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjcjdjci: do d = nvirt0, nvirt1
c_aibjcjdjci: do c = nvirt0, d - 1
b_aibjcjdjci: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcjdjci
j_aibjcjdjci: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdjci: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcjdjci
i_aibjcjdjci: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjdjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjdjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdjci
end do a_aibjcjdjci
end do j_aibjcjdjci
end do b_aibjcjdjci
end do c_aibjcjdjci
end do d_aibjcjdjci
!
! Elementary loop 47
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: j, i
! Equalities: e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdjcj: do d = nvirt0, nvirt1
c_aibjcjdjcj: do c = nvirt0, d - 1
b_aibjcjdjcj: do b = c + 1, nvirt1
if (b == d) cycle b_aibjcjdjcj
j_aibjcjdjcj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdjcj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcjdjcj
i_aibjcjdjcj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjdjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjdjcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdjcj
end do a_aibjcjdjcj
end do j_aibjcjdjcj
end do b_aibjcjdjcj
end do c_aibjcjdjcj
end do d_aibjcjdjcj
!
! Elementary loop 48
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckbibj: do c = nvirt0, nvirt1
k_aibjckbibj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbibj: do b = c + 1, nvirt1
j_aibjckbibj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbibj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbibj
i_aibjckbibj: do i = j + 1, nocc1
if (i == k) cycle i_aibjckbibj
if (i > j .and. j > k) exit i_aibjckbibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbibj
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbibj
end do a_aibjckbibj
end do j_aibjckbibj
end do b_aibjckbibj
end do k_aibjckbibj
end do c_aibjckbibj
!
! Elementary loop 49
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k, j
! Equalities: d == b, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckbibk: do c = nvirt0, nvirt1
k_aibjckbibk: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbibk: do b = c + 1, nvirt1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbibk: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbibk: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbibk
i_aibjckbibk: do i = k + 1, nocc1
if (i == j) cycle i_aibjckbibk
if (i > j .and. j > k) exit i_aibjckbibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbibk
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbibk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbibk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = v2_eom_cc3_32_trans_aibjckbibk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
      jac(ibra, iket) = 0.d+0
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbibk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbibk
end do a_aibjckbibk
end do j_aibjckbibk
end do b_aibjckbibk
end do k_aibjckbibk
end do c_aibjckbibk
!
! Elementary loop 50
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckbjbi: do c = nvirt0, nvirt1
k_aibjckbjbi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbjbi: do b = c + 1, nvirt1
j_aibjckbjbi: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbjbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbjbi
i_aibjckbjbi: do i = nocc0, j - 1
if (i == k) cycle i_aibjckbjbi
if (i > j .and. j > k) exit i_aibjckbjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
      if (j > k) then
            exit i_aibjckbjbi
      else if (k > i) then
            jac(ibra, iket) = v3_eom_cc3_32_trans_aibjckbjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      else
            jac(ibra, iket) = v1_eom_cc3_32_trans_aibjckbjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
      end if
else if (i > k) then
      jac(ibra, iket) = 0.d+0
else if (k > j) then
      jac(ibra, iket) = v5_eom_cc3_32_trans_aibjckbjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
else
      jac(ibra, iket) = v4_eom_cc3_32_trans_aibjckbjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end if
end do i_aibjckbjbi
end do a_aibjckbjbi
end do j_aibjckbjbi
end do b_aibjckbjbi
end do k_aibjckbjbi
end do c_aibjckbjbi
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
end subroutine ccjac_32_part4
end module ccjac_block_32_part4
