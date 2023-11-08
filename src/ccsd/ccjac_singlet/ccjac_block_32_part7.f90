module ccjac_block_32_part7
use eom_cc3_32_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:41:38 UTC.
!
contains
 
subroutine ccjac_32_part7(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajcidiaj: do d = nvirt0, nvirt1
c_aiajcidiaj: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajcidiaj
j_aiajcidiaj: do j = nocc0, nocc1
a_aiajcidiaj: do a = c + 1, d - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidiaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajcidiaj(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcidiaj
end do a_aiajcidiaj
end do j_aiajcidiaj
end do c_aiajcidiaj
end do d_aiajcidiaj
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aiajcjdiaj: do d = nvirt0, nvirt1
c_aiajcjdiaj: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajcjdiaj
j_aiajcjdiaj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjdiaj: do a = c + 1, d - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjdiaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjdiaj(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcjdiaj
end do a_aiajcjdiaj
end do j_aiajcjdiaj
end do c_aiajcjdiaj
end do d_aiajcjdiaj
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidjai: do d = nvirt0, nvirt1
c_aiajcidjai: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajcidjai
j_aiajcidjai: do j = nocc0, nocc1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcidjai: do a = c + 1, d - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidjai: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajcidjai(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcidjai
end do a_aiajcidjai
end do j_aiajcidjai
end do c_aiajcidjai
end do d_aiajcidjai
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aiajcjdjai: do d = nvirt0, nvirt1
c_aiajcjdjai: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajcjdjai
j_aiajcjdjai: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjdjai: do a = c + 1, d - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjdjai: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjdjai(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcjdjai
end do a_aiajcjdjai
end do j_aiajcjdjai
end do c_aiajcjdjai
end do d_aiajcjdjai
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, i
! Equalities: b == a, e == a, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
d_aiajcjdjaj: do d = nvirt0, nvirt1
c_aiajcjdjaj: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajcjdjaj
j_aiajcjdjaj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjdjaj: do a = c + 1, d - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjdjaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjdjaj(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcjdjaj
end do a_aiajcjdjaj
end do j_aiajcjdjaj
end do c_aiajcjdjaj
end do d_aiajcjdjaj
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: e == a, c == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibibkdiai: do d = nvirt0, nvirt1
k_aibibkdiai: do k = nocc0, nocc1
b_aibibkdiai: do b = nvirt0, nvirt1
if (b == d) cycle b_aibibkdiai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkdiai: do a = b + 1, d - 1
i_aibibkdiai: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkdiai(eorb, t2, t1, nocc, nactive, a, i, b, k, d)
end do i_aibibkdiai
end do a_aibibkdiai
end do b_aibibkdiai
end do k_aibibkdiai
end do d_aibibkdiai
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: e == a, c == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibibkdiak: do d = nvirt0, nvirt1
k_aibibkdiak: do k = nocc0, nocc1
b_aibibkdiak: do b = nvirt0, nvirt1
if (b == d) cycle b_aibibkdiak
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkdiak: do a = b + 1, d - 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibibkdiak: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkdiak(eorb, t2, t1, nocc, nactive, a, i, b, k, d)
end do i_aibibkdiak
end do a_aibibkdiak
end do b_aibibkdiak
end do k_aibibkdiak
end do d_aibibkdiak
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: e == a, c == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibibkdkai: do d = nvirt0, nvirt1
k_aibibkdkai: do k = nocc0, nocc1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibibkdkai: do b = nvirt0, nvirt1
if (b == d) cycle b_aibibkdkai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkdkai: do a = b + 1, d - 1
i_aibibkdkai: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkdkai(eorb, t2, t1, nocc, nactive, a, i, b, k, d)
end do i_aibibkdkai
end do a_aibibkdkai
end do b_aibibkdkai
end do k_aibibkdkai
end do d_aibibkdkai
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: e == a, c == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjbidiai: do d = nvirt0, nvirt1
b_aibjbidiai: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjbidiai
j_aibjbidiai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbidiai: do a = b + 1, d - 1
i_aibjbidiai: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbidiai(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjbidiai
end do a_aibjbidiai
end do j_aibjbidiai
end do b_aibjbidiai
end do d_aibjbidiai
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: e == a, c == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbidiaj: do d = nvirt0, nvirt1
b_aibjbidiaj: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjbidiaj
j_aibjbidiaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbidiaj: do a = b + 1, d - 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbidiaj: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbidiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjbidiaj
end do a_aibjbidiaj
end do j_aibjbidiaj
end do b_aibjbidiaj
end do d_aibjbidiaj
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: e == a, c == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidjai: do d = nvirt0, nvirt1
b_aibjbidjai: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjbidjai
j_aibjbidjai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbidjai: do a = b + 1, d - 1
i_aibjbidjai: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbidjai(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjbidjai
end do a_aibjbidjai
end do j_aibjbidjai
end do b_aibjbidjai
end do d_aibjbidjai
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == c, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckcicj: do c = nvirt0, nvirt1
k_aiajckcicj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckcicj: do j = nocc0, k - 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckcicj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckcicj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckcicj(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckcicj
end do a_aiajckcicj
end do j_aiajckcicj
end do k_aiajckcicj
end do c_aiajckcicj
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == c, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiajckcick: do c = nvirt0, nvirt1
k_aiajckcick: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckcick: do j = nocc0, k - 1
a_aiajckcick: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j, k)
i_aiajckcick: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckcick(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckcick
end do a_aiajckcick
end do j_aiajckcick
end do k_aiajckcick
end do c_aiajckcick
!
! Elementary loop 14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == c, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiajckckci: do c = nvirt0, nvirt1
k_aiajckckci: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckckci: do j = nocc0, k - 1
a_aiajckckci: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckckci: do i = j + 1, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckckci(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckckci
end do a_aiajckckci
end do j_aiajckckci
end do k_aiajckckci
end do c_aiajckckci
!
! Elementary loop 15
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiajciciei: do e = nvirt0, nvirt1
c_aiajciciei: do c = e + 1, nvirt1
j_aiajciciei: do j = nocc0, nocc1
a_aiajciciei: do a = c + 1, nvirt1
if (a == e) cycle a_aiajciciei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciciei: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciciei(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajciciei
end do a_aiajciciei
end do j_aiajciciei
end do c_aiajciciei
end do e_aiajciciei
!
! Elementary loop 16
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajciciej: do e = nvirt0, nvirt1
c_aiajciciej: do c = e + 1, nvirt1
j_aiajciciej: do j = nocc0, nocc1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajciciej: do a = c + 1, nvirt1
if (a == e) cycle a_aiajciciej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciciej: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciciej(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajciciej
end do a_aiajciciej
end do j_aiajciciej
end do c_aiajciciej
end do e_aiajciciej
!
! Elementary loop 17
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aiajcjciej: do e = nvirt0, nvirt1
c_aiajcjciej: do c = e + 1, nvirt1
j_aiajcjciej: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjciej: do a = c + 1, nvirt1
if (a == e) cycle a_aiajcjciej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjciej: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjciej(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajcjciej
end do a_aiajcjciej
end do j_aiajcjciej
end do c_aiajcjciej
end do e_aiajcjciej
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajcicjei: do e = nvirt0, nvirt1
c_aiajcicjei: do c = e + 1, nvirt1
j_aiajcicjei: do j = nocc0, nocc1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcicjei: do a = c + 1, nvirt1
if (a == e) cycle a_aiajcicjei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcicjei: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajcicjei(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajcicjei
end do a_aiajcicjei
end do j_aiajcicjei
end do c_aiajcicjei
end do e_aiajcicjei
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aiajcjcjei: do e = nvirt0, nvirt1
c_aiajcjcjei: do c = e + 1, nvirt1
j_aiajcjcjei: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjcjei: do a = c + 1, nvirt1
if (a == e) cycle a_aiajcjcjei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjcjei: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjcjei(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajcjcjei
end do a_aiajcjcjei
end do j_aiajcjcjei
end do c_aiajcjcjei
end do e_aiajcjcjei
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, i
! Equalities: b == a, d == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
e_aiajcjcjej: do e = nvirt0, nvirt1
c_aiajcjcjej: do c = e + 1, nvirt1
j_aiajcjcjej: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjcjej: do a = c + 1, nvirt1
if (a == e) cycle a_aiajcjcjej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjcjej: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjcjej(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajcjcjej
end do a_aiajcjcjej
end do j_aiajcjcjej
end do c_aiajcjcjej
end do e_aiajcjcjej
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aiajcidici: do d = nvirt0, nvirt1
c_aiajcidici: do c = nvirt0, d - 1
j_aiajcidici: do j = nocc0, nocc1
a_aiajcidici: do a = c + 1, nvirt1
if (a == d) cycle a_aiajcidici
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidici: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajcidici(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcidici
end do a_aiajcidici
end do j_aiajcidici
end do c_aiajcidici
end do d_aiajcidici
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajcidicj: do d = nvirt0, nvirt1
c_aiajcidicj: do c = nvirt0, d - 1
j_aiajcidicj: do j = nocc0, nocc1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcidicj: do a = c + 1, nvirt1
if (a == d) cycle a_aiajcidicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidicj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajcidicj(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcidicj
end do a_aiajcidicj
end do j_aiajcidicj
end do c_aiajcidicj
end do d_aiajcidicj
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aiajcjdicj: do d = nvirt0, nvirt1
c_aiajcjdicj: do c = nvirt0, d - 1
j_aiajcjdicj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjdicj: do a = c + 1, nvirt1
if (a == d) cycle a_aiajcjdicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjdicj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjdicj(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcjdicj
end do a_aiajcjdicj
end do j_aiajcjdicj
end do c_aiajcjdicj
end do d_aiajcjdicj
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidjci: do d = nvirt0, nvirt1
c_aiajcidjci: do c = nvirt0, d - 1
j_aiajcidjci: do j = nocc0, nocc1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcidjci: do a = c + 1, nvirt1
if (a == d) cycle a_aiajcidjci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidjci: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajcidjci(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcidjci
end do a_aiajcidjci
end do j_aiajcidjci
end do c_aiajcidjci
end do d_aiajcidjci
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aiajcjdjci: do d = nvirt0, nvirt1
c_aiajcjdjci: do c = nvirt0, d - 1
j_aiajcjdjci: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjdjci: do a = c + 1, nvirt1
if (a == d) cycle a_aiajcjdjci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjdjci: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjdjci(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcjdjci
end do a_aiajcjdjci
end do j_aiajcjdjci
end do c_aiajcjdjci
end do d_aiajcjdjci
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, i
! Equalities: b == a, e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
d_aiajcjdjcj: do d = nvirt0, nvirt1
c_aiajcjdjcj: do c = nvirt0, d - 1
j_aiajcjdjcj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjdjcj: do a = c + 1, nvirt1
if (a == d) cycle a_aiajcjdjcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjdjcj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjdjcj(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcjdjcj
end do a_aiajcjdjcj
end do j_aiajcjdjcj
end do c_aiajcjdjcj
end do d_aiajcjdjcj
!
! Elementary loop 27
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, m
! Equalities: c == b, d == b, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibibkbibm: do m = nocc0, nocc1
k_aibibkbibm: do k = nocc0, nocc1
if (k == m) cycle k_aibibkbibm
b_aibibkbibm: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a_aibibkbibm: do a = b + 1, nvirt1
i0 = max(k, m)
i_aibibkbibm: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkbibm(eorb, t2, t1, nocc, nactive, a, i, b, k, m)
end do i_aibibkbibm
end do a_aibibkbibm
end do b_aibibkbibm
end do k_aibibkbibm
end do m_aibibkbibm
!
! Elementary loop 28
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, l
! Equalities: c == b, d == b, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibibkblbi: do l = nocc0, nocc1
k_aibibkblbi: do k = nocc0, nocc1
if (k == l) cycle k_aibibkblbi
b_aibibkblbi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibibkblbi: do a = b + 1, nvirt1
i_aibibkblbi: do i = k + 1, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkblbi(eorb, t2, t1, nocc, nactive, a, i, b, k, l)
end do i_aibibkblbi
end do a_aibibkblbi
end do b_aibibkblbi
end do k_aibibkblbi
end do l_aibibkblbi
!
! Elementary loop 29
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, m
! Equalities: c == b, d == b, e == b, j == i, l == k
! No equalities independent of the above can hold.
!
m_aibibkbkbm: do m = nocc0, nocc1
k_aibibkbkbm: do k = m + 1, nocc1
b_aibibkbkbm: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a_aibibkbkbm: do a = b + 1, nvirt1
i_aibibkbkbm: do i = k + 1, nocc1
if (i == m) cycle i_aibibkbkbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkbkbm(eorb, t2, t1, nocc, nactive, a, i, b, k, m)
end do i_aibibkbkbm
end do a_aibibkbkbm
end do b_aibibkbkbm
end do k_aibibkbkbm
end do m_aibibkbkbm
!
! Elementary loop 30
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, l
! Equalities: c == b, d == b, e == b, j == i, m == k
! No equalities independent of the above can hold.
!
l_aibibkblbk: do l = nocc0, nocc1
k_aibibkblbk: do k = nocc0, l - 1
b_aibibkblbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkblbk: do a = b + 1, nvirt1
i_aibibkblbk: do i = k + 1, nocc1
if (i == l) cycle i_aibibkblbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkblbk(eorb, t2, t1, nocc, nactive, a, i, b, k, l)
end do i_aibibkblbk
end do a_aibibkblbk
end do b_aibibkblbk
end do k_aibibkblbk
end do l_aibibkblbk
!
! Elementary loop 31
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, m
! Equalities: c == b, d == b, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjbibibm: do m = nocc0, nocc1
b_aibjbibibm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbibibm: do j = nocc0, nocc1
if (j == m) cycle j_aibjbibibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbibibm: do a = b + 1, nvirt1
i_aibjbibibm: do i = m + 1, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbibibm(eorb, t2, t1, nocc, nactive, a, i, b, j, m)
end do i_aibjbibibm
end do a_aibjbibibm
end do j_aibjbibibm
end do b_aibjbibibm
end do m_aibjbibibm
!
! Elementary loop 32
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, e == b, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibjbkbibi: do k = nocc0, nocc1
b_aibjbkbibi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbibi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbibi: do a = b + 1, nvirt1
i_aibjbkbibi: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbibi(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkbibi
end do a_aibjbkbibi
end do j_aibjbkbibi
end do b_aibjbkbibi
end do k_aibjbkbibi
!
! Elementary loop 33
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjbkbibj: do k = nocc0, nocc1
b_aibjbkbibj: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbibj: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbibj: do a = b + 1, nvirt1
i_aibjbkbibj: do i = j + 1, j - 1
if (i == k) cycle i_aibjbkbibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbibj(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkbibj
end do a_aibjbkbibj
end do j_aibjbkbibj
end do b_aibjbkbibj
end do k_aibjbkbibj
!
! Elementary loop 34
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, j
! Equalities: c == b, d == b, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjbkbibk: do k = nocc0, nocc1
b_aibjbkbibk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbibk: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbibk: do a = b + 1, nvirt1
i_aibjbkbibk: do i = k + 1, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbibk(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkbibk
end do a_aibjbkbibk
end do j_aibjbkbibk
end do b_aibjbkbibk
end do k_aibjbkbibk
!
! Elementary loop 35
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjbiblbi: do l = nocc0, nocc1
b_aibjbiblbi: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjbiblbi: do j = nocc0, nocc1
if (j == l) cycle j_aibjbiblbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiblbi: do a = b + 1, nvirt1
i1 = min(j, l)
i_aibjbiblbi: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbiblbi(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjbiblbi
end do a_aibjbiblbi
end do j_aibjbiblbi
end do b_aibjbiblbi
end do l_aibjbiblbi
!
! Elementary loop 36
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, m
! Equalities: c == b, d == b, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjbibjbm: do m = nocc0, nocc1
b_aibjbibjbm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbibjbm: do j = m + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbibjbm: do a = b + 1, nvirt1
i_aibjbibjbm: do i = nocc0, j - 1
if (i == m) cycle i_aibjbibjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbibjbm(eorb, t2, t1, nocc, nactive, a, i, b, j, m)
end do i_aibjbibjbm
end do a_aibjbibjbm
end do j_aibjbibjbm
end do b_aibjbibjbm
end do m_aibjbibjbm
!
! Elementary loop 37
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjbiblbj: do l = nocc0, nocc1
b_aibjbiblbj: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjbiblbj: do j = nocc0, l - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiblbj: do a = b + 1, nvirt1
i_aibjbiblbj: do i = nocc0, j - 1
if (i == l) cycle i_aibjbiblbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbiblbj(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjbiblbj
end do a_aibjbiblbj
end do j_aibjbiblbj
end do b_aibjbiblbj
end do l_aibjbiblbj
!
! Elementary loop 38
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjbkbjbi: do k = nocc0, nocc1
b_aibjbkbjbi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbjbi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbjbi: do a = b + 1, nvirt1
i_aibjbkbjbi: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkbjbi
end do a_aibjbkbjbi
end do j_aibjbkbjbi
end do b_aibjbkbjbi
end do k_aibjbkbjbi
!
! Elementary loop 39
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, j
! Equalities: c == b, d == b, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjbkbkbi: do k = nocc0, nocc1
b_aibjbkbkbi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbkbi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbkbi: do a = b + 1, nvirt1
i1 = min(k, j)
i_aibjbkbkbi: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbkbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkbkbi
end do a_aibjbkbkbi
end do j_aibjbkbkbi
end do b_aibjbkbkbi
end do k_aibjbkbkbi
!
! Elementary loop 40
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, k
! Equalities: c == b, d == b, e == b, l == j, m == j
! No equalities independent of the above can hold.
!
k_aibjbkbjbj: do k = nocc0, nocc1
b_aibjbkbjbj: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbjbj: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbjbj: do a = b + 1, nvirt1
i_aibjbkbjbj: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbjbj(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkbjbj
end do a_aibjbkbjbj
end do j_aibjbkbjbj
end do b_aibjbkbjbj
end do k_aibjbkbjbj
!
! Elementary loop 41
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, k, i
! Equalities: c == b, d == b, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjbkbjbk: do k = nocc0, nocc1
b_aibjbkbjbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbjbk: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbjbk: do a = b + 1, nvirt1
i_aibjbkbjbk: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbjbk(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkbjbk
end do a_aibjbkbjbk
end do j_aibjbkbjbk
end do b_aibjbkbjbk
end do k_aibjbkbjbk
!
! Elementary loop 42
! --------------------
! Free virtual indices: b, a
! Free occupied indices: k, i, j
! Equalities: c == b, d == b, e == b, l == k, m == k
! No equalities independent of the above can hold.
!
k_aibjbkbkbk: do k = nocc0, nocc1
b_aibjbkbkbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbkbk: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbkbk: do a = b + 1, nvirt1
i_aibjbkbkbk: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbkbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkbkbk(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkbkbk
end do a_aibjbkbkbk
end do j_aibjbkbkbk
end do b_aibjbkbkbk
end do k_aibjbkbkbk
!
! Elementary loop 43
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjcicici: do c = nvirt0, nvirt1
b_aibjcicici: do b = c + 1, nvirt1
j_aibjcicici: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicici: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcicici
i_aibjcicici: do i = nocc0, nocc1
if (i == j) cycle i_aibjcicici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcicici(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcicici
end do a_aibjcicici
end do j_aibjcicici
end do b_aibjcicici
end do c_aibjcicici
!
! Elementary loop 44
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjcicicj: do c = nvirt0, nvirt1
b_aibjcicicj: do b = c + 1, nvirt1
j_aibjcicicj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicicj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcicicj
i_aibjcicicj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcicicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcicicj
end do a_aibjcicicj
end do j_aibjcicicj
end do b_aibjcicicj
end do c_aibjcicicj
!
! Elementary loop 45
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjcicj: do c = nvirt0, nvirt1
b_aibjcjcicj: do b = c + 1, nvirt1
j_aibjcjcicj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjcicj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjcicj
i_aibjcjcicj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjcicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjcicj
end do a_aibjcjcicj
end do j_aibjcjcicj
end do b_aibjcjcicj
end do c_aibjcjcicj
!
! Elementary loop 46
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicjci: do c = nvirt0, nvirt1
b_aibjcicjci: do b = c + 1, nvirt1
j_aibjcicjci: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicjci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcicjci
i_aibjcicjci: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcicjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcicjci
end do a_aibjcicjci
end do j_aibjcicjci
end do b_aibjcicjci
end do c_aibjcicjci
!
! Elementary loop 47
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, e == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjcjci: do c = nvirt0, nvirt1
b_aibjcjcjci: do b = c + 1, nvirt1
j_aibjcjcjci: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjcjci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjcjci
i_aibjcjcjci: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjcjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjcjci
end do a_aibjcjcjci
end do j_aibjcjcjci
end do b_aibjcjcjci
end do c_aibjcjcjci
!
! Elementary loop 48
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, i
! Equalities: d == c, e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjcjcj: do c = nvirt0, nvirt1
b_aibjcjcjcj: do b = c + 1, nvirt1
j_aibjcjcjcj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjcjcj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjcjcj
i_aibjcjcjcj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjcjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjcjcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjcjcj
end do a_aibjcjcjcj
end do j_aibjcjcjcj
end do b_aibjcjcjcj
end do c_aibjcjcjcj
!
! Elementary loop 49
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, k
! Equalities: c == b, d == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibibkbiei: do e = nvirt0, nvirt1
k_aibibkbiei: do k = nocc0, nocc1
b_aibibkbiei: do b = e + 1, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkbiei: do a = b + 1, nvirt1
if (a == e) cycle a_aibibkbiei
i_aibibkbiei: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkbiei(eorb, t2, t1, nocc, nactive, a, i, b, k, e)
end do i_aibibkbiei
end do a_aibibkbiei
end do b_aibibkbiei
end do k_aibibkbiei
end do e_aibibkbiei
!
! Elementary loop 50
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, k
! Equalities: c == b, d == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibibkbiek: do e = nvirt0, nvirt1
k_aibibkbiek: do k = nocc0, nocc1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibibkbiek: do b = e + 1, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkbiek: do a = b + 1, nvirt1
if (a == e) cycle a_aibibkbiek
i_aibibkbiek: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkbiek(eorb, t2, t1, nocc, nactive, a, i, b, k, e)
end do i_aibibkbiek
end do a_aibibkbiek
end do b_aibibkbiek
end do k_aibibkbiek
end do e_aibibkbiek
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
end subroutine ccjac_32_part7
end module ccjac_block_32_part7
