module ccjac_block_32_part8
use eom_cc3_32_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:41:38 UTC.
!
contains
 
subroutine ccjac_32_part8(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: i, j, k
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
! Free virtual indices: b, a, e
! Free occupied indices: i, k
! Equalities: c == b, d == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibibkbkei: do e = nvirt0, nvirt1
k_aibibkbkei: do k = nocc0, nocc1
b_aibibkbkei: do b = e + 1, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkbkei: do a = b + 1, nvirt1
if (a == e) cycle a_aibibkbkei
i_aibibkbkei: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkbkei(eorb, t2, t1, nocc, nactive, a, i, b, k, e)
end do i_aibibkbkei
end do a_aibibkbkei
end do b_aibibkbkei
end do k_aibibkbkei
end do e_aibibkbkei
!
! Elementary loop 2
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjbibiei: do e = nvirt0, nvirt1
b_aibjbibiei: do b = e + 1, nvirt1
j_aibjbibiei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbibiei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbibiei
i_aibjbibiei: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbibiei(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjbibiei
end do a_aibjbibiei
end do j_aibjbibiei
end do b_aibjbibiei
end do e_aibjbibiei
!
! Elementary loop 3
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbibiej: do e = nvirt0, nvirt1
b_aibjbibiej: do b = e + 1, nvirt1
j_aibjbibiej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbibiej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbibiej
i_aibjbibiej: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbibiej(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjbibiej
end do a_aibjbibiej
end do j_aibjbibiej
end do b_aibjbibiej
end do e_aibjbibiej
!
! Elementary loop 4
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbibjei: do e = nvirt0, nvirt1
b_aibjbibjei: do b = e + 1, nvirt1
j_aibjbibjei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbibjei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbibjei
i_aibjbibjei: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbibjei(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjbibjei
end do a_aibjbibjei
end do j_aibjbibjei
end do b_aibjbibjei
end do e_aibjbibjei
!
! Elementary loop 5
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibibkdibi: do d = nvirt0, nvirt1
k_aibibkdibi: do k = nocc0, nocc1
b_aibibkdibi: do b = nvirt0, d - 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkdibi: do a = b + 1, nvirt1
if (a == d) cycle a_aibibkdibi
i_aibibkdibi: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkdibi(eorb, t2, t1, nocc, nactive, a, i, b, k, d)
end do i_aibibkdibi
end do a_aibibkdibi
end do b_aibibkdibi
end do k_aibibkdibi
end do d_aibibkdibi
!
! Elementary loop 6
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibibkdibk: do d = nvirt0, nvirt1
k_aibibkdibk: do k = nocc0, nocc1
b_aibibkdibk: do b = nvirt0, d - 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkdibk: do a = b + 1, nvirt1
if (a == d) cycle a_aibibkdibk
i_aibibkdibk: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkdibk(eorb, t2, t1, nocc, nactive, a, i, b, k, d)
end do i_aibibkdibk
end do a_aibibkdibk
end do b_aibibkdibk
end do k_aibibkdibk
end do d_aibibkdibk
!
! Elementary loop 7
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, e == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibibkdkbi: do d = nvirt0, nvirt1
k_aibibkdkbi: do k = nocc0, nocc1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibibkdkbi: do b = nvirt0, d - 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkdkbi: do a = b + 1, nvirt1
if (a == d) cycle a_aibibkdkbi
i_aibibkdkbi: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkdkbi(eorb, t2, t1, nocc, nactive, a, i, b, k, d)
end do i_aibibkdkbi
end do a_aibibkdkbi
end do b_aibibkdkbi
end do k_aibibkdkbi
end do d_aibibkdkbi
!
! Elementary loop 8
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjbidibi: do d = nvirt0, nvirt1
b_aibjbidibi: do b = nvirt0, d - 1
j_aibjbidibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbidibi: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbidibi
i_aibjbidibi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbidibi(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjbidibi
end do a_aibjbidibi
end do j_aibjbidibi
end do b_aibjbidibi
end do d_aibjbidibi
!
! Elementary loop 9
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbidibj: do d = nvirt0, nvirt1
b_aibjbidibj: do b = nvirt0, d - 1
j_aibjbidibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbidibj: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbidibj
i_aibjbidibj: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbidibj(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjbidibj
end do a_aibjbidibj
end do j_aibjbidibj
end do b_aibjbidibj
end do d_aibjbidibj
!
! Elementary loop 10
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidjbi: do d = nvirt0, nvirt1
b_aibjbidjbi: do b = nvirt0, d - 1
j_aibjbidjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbidjbi: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbidjbi
i_aibjbidjbi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbidjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjbidjbi
end do a_aibjbidjbi
end do j_aibjbidjbi
end do b_aibjbidjbi
end do d_aibjbidjbi
!
! Elementary loop 11
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjcibici: do c = nvirt0, nvirt1
b_aibjcibici: do b = c + 1, nvirt1
j_aibjcibici: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibici: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibici
i_aibjcibici: do i = nocc0, nocc1
if (i == j) cycle i_aibjcibici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcibici(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcibici
end do a_aibjcibici
end do j_aibjcibici
end do b_aibjcibici
end do c_aibjcibici
!
! Elementary loop 12
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjcibicj: do c = nvirt0, nvirt1
b_aibjcibicj: do b = c + 1, nvirt1
j_aibjcibicj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibicj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibicj
i_aibjcibicj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcibicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcibicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcibicj
end do a_aibjcibicj
end do j_aibjcibicj
end do b_aibjcibicj
end do c_aibjcibicj
!
! Elementary loop 13
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbici: do c = nvirt0, nvirt1
b_aibjcjbici: do b = c + 1, nvirt1
j_aibjcjbici: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbici: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbici
i_aibjcjbici: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjbici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjbici(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjbici
end do a_aibjcjbici
end do j_aibjcjbici
end do b_aibjcjbici
end do c_aibjcjbici
!
! Elementary loop 14
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjbicj: do c = nvirt0, nvirt1
b_aibjcjbicj: do b = c + 1, nvirt1
j_aibjcjbicj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbicj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbicj
i_aibjcjbicj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjbicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjbicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjbicj
end do a_aibjcjbicj
end do j_aibjcjbicj
end do b_aibjcjbicj
end do c_aibjcjbicj
!
! Elementary loop 15
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibjci: do c = nvirt0, nvirt1
b_aibjcibjci: do b = c + 1, nvirt1
j_aibjcibjci: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibjci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibjci
i_aibjcibjci: do i = nocc0, nocc1
if (i == j) cycle i_aibjcibjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcibjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcibjci
end do a_aibjcibjci
end do j_aibjcibjci
end do b_aibjcibjci
end do c_aibjcibjci
!
! Elementary loop 16
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjbjci: do c = nvirt0, nvirt1
b_aibjcjbjci: do b = c + 1, nvirt1
j_aibjcjbjci: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbjci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbjci
i_aibjcjbjci: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjbjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjbjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjbjci
end do a_aibjcjbjci
end do j_aibjcjbjci
end do b_aibjcjbjci
end do c_aibjcjbjci
!
! Elementary loop 17
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i
! Equalities: d == b, e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjbjcj: do c = nvirt0, nvirt1
b_aibjcjbjcj: do b = c + 1, nvirt1
j_aibjcjbjcj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbjcj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbjcj
i_aibjcjbjcj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjbjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjbjcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjbjcj
end do a_aibjcjbjcj
end do j_aibjcjbjcj
end do b_aibjcjbjcj
end do c_aibjcjbjcj
!
! Elementary loop 18
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjcibibi: do c = nvirt0, nvirt1
b_aibjcibibi: do b = c + 1, nvirt1
j_aibjcibibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibibi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibibi
i_aibjcibibi: do i = nocc0, nocc1
if (i == j) cycle i_aibjcibibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcibibi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcibibi
end do a_aibjcibibi
end do j_aibjcibibi
end do b_aibjcibibi
end do c_aibjcibibi
!
! Elementary loop 19
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjcibibj: do c = nvirt0, nvirt1
b_aibjcibibj: do b = c + 1, nvirt1
j_aibjcibibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibibj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibibj
i_aibjcibibj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcibibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcibibj
end do a_aibjcibibj
end do j_aibjcibibj
end do b_aibjcibibj
end do c_aibjcibibj
!
! Elementary loop 20
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjbibj: do c = nvirt0, nvirt1
b_aibjcjbibj: do b = c + 1, nvirt1
j_aibjcjbibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbibj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbibj
i_aibjcjbibj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjbibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjbibj
end do a_aibjcjbibj
end do j_aibjcjbibj
end do b_aibjcjbibj
end do c_aibjcjbibj
!
! Elementary loop 21
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibjbi: do c = nvirt0, nvirt1
b_aibjcibjbi: do b = c + 1, nvirt1
j_aibjcibjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibjbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibjbi
i_aibjcibjbi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcibjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcibjbi
end do a_aibjcibjbi
end do j_aibjcibjbi
end do b_aibjcibjbi
end do c_aibjcibjbi
!
! Elementary loop 22
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjbjbi: do c = nvirt0, nvirt1
b_aibjcjbjbi: do b = c + 1, nvirt1
j_aibjcjbjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbjbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbjbi
i_aibjcjbjbi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjbjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjbjbi
end do a_aibjcjbjbi
end do j_aibjcjbjbi
end do b_aibjcjbjbi
end do c_aibjcjbjbi
!
! Elementary loop 23
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i
! Equalities: d == b, e == b, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjbjbj: do c = nvirt0, nvirt1
b_aibjcjbjbj: do b = c + 1, nvirt1
j_aibjcjbjbj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbjbj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbjbj
i_aibjcjbjbj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjbjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjbjbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjbjbj
end do a_aibjcjbjbj
end do j_aibjcjbjbj
end do b_aibjcjbjbj
end do c_aibjcjbjbj
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiajciaiai: do c = nvirt0, nvirt1
j_aiajciaiai: do j = nocc0, nocc1
a_aiajciaiai: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciaiai: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciaiai(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajciaiai
end do a_aiajciaiai
end do j_aiajciaiai
end do c_aiajciaiai
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajciaiaj: do c = nvirt0, nvirt1
j_aiajciaiaj: do j = nocc0, nocc1
a_aiajciaiaj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciaiaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciaiaj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajciaiaj
end do a_aiajciaiaj
end do j_aiajciaiaj
end do c_aiajciaiaj
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjaiai: do c = nvirt0, nvirt1
j_aiajcjaiai: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjaiai: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjaiai: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjaiai(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjaiai
end do a_aiajcjaiai
end do j_aiajcjaiai
end do c_aiajcjaiai
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjaiaj: do c = nvirt0, nvirt1
j_aiajcjaiaj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjaiaj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjaiaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjaiaj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjaiaj
end do a_aiajcjaiaj
end do j_aiajcjaiaj
end do c_aiajcjaiaj
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajciajaj: do c = nvirt0, nvirt1
j_aiajciajaj: do j = nocc0, nocc1
a_aiajciajaj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciajaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciajaj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajciajaj
end do a_aiajciajaj
end do j_aiajciajaj
end do c_aiajciajaj
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, d == a, e == a, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjajaj: do c = nvirt0, nvirt1
j_aiajcjajaj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjajaj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjajaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjajaj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjajaj
end do a_aiajcjajaj
end do j_aiajcjajaj
end do c_aiajcjajaj
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, e == a, c == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibibkaiai: do k = nocc0, nocc1
b_aibibkaiai: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkaiai: do a = b + 1, nvirt1
i_aibibkaiai: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkaiai(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkaiai
end do a_aibibkaiai
end do b_aibibkaiai
end do k_aibibkaiai
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, e == a, c == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibibkaiak: do k = nocc0, nocc1
b_aibibkaiak: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkaiak: do a = b + 1, nvirt1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibibkaiak: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkaiak(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkaiak
end do a_aibibkaiak
end do b_aibibkaiak
end do k_aibibkaiak
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, e == a, c == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
b_aibjbiaiai: do b = nvirt0, nvirt1
j_aibjbiaiai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiaiai: do a = b + 1, nvirt1
i_aibjbiaiai: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbiaiai(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbiaiai
end do a_aibjbiaiai
end do j_aibjbiaiai
end do b_aibjbiaiai
!
! Elementary loop 33
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, e == a, c == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbiajai: do b = nvirt0, nvirt1
j_aibjbiajai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiajai: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbiajai: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbiajai(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbiajai
end do a_aibjbiajai
end do j_aibjbiajai
end do b_aibjbiajai
!
! Elementary loop 34
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiajciaici: do c = nvirt0, nvirt1
j_aiajciaici: do j = nocc0, nocc1
a_aiajciaici: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciaici: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciaici(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajciaici
end do a_aiajciaici
end do j_aiajciaici
end do c_aiajciaici
!
! Elementary loop 35
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajciaicj: do c = nvirt0, nvirt1
j_aiajciaicj: do j = nocc0, nocc1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajciaicj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciaicj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciaicj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajciaicj
end do a_aiajciaicj
end do j_aiajciaicj
end do c_aiajciaicj
!
! Elementary loop 36
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjaici: do c = nvirt0, nvirt1
j_aiajcjaici: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjaici: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjaici: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjaici(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjaici
end do a_aiajcjaici
end do j_aiajcjaici
end do c_aiajcjaici
!
! Elementary loop 37
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjaicj: do c = nvirt0, nvirt1
j_aiajcjaicj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjaicj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjaicj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjaicj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjaicj
end do a_aiajcjaicj
end do j_aiajcjaicj
end do c_aiajcjaicj
!
! Elementary loop 38
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajciajci: do c = nvirt0, nvirt1
j_aiajciajci: do j = nocc0, nocc1
a_aiajciajci: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciajci: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciajci(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajciajci
end do a_aiajciajci
end do j_aiajciajci
end do c_aiajciajci
!
! Elementary loop 39
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajciajcj: do c = nvirt0, nvirt1
j_aiajciajcj: do j = nocc0, nocc1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajciajcj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciajcj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciajcj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajciajcj
end do a_aiajciajcj
end do j_aiajciajcj
end do c_aiajciajcj
!
! Elementary loop 40
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aiajcjajci: do c = nvirt0, nvirt1
j_aiajcjajci: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjajci: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjajci: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjajci(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjajci
end do a_aiajcjajci
end do j_aiajcjajci
end do c_aiajcjajci
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, d == a, e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjajcj: do c = nvirt0, nvirt1
j_aiajcjajcj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjajcj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjajcj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjajcj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjajcj
end do a_aiajcjajcj
end do j_aiajcjajcj
end do c_aiajcjajcj
!
! Elementary loop 42
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibibkaibi: do k = nocc0, nocc1
b_aibibkaibi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkaibi: do a = b + 1, nvirt1
i_aibibkaibi: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkaibi(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkaibi
end do a_aibibkaibi
end do b_aibibkaibi
end do k_aibibkaibi
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibibkaibk: do k = nocc0, nocc1
b_aibibkaibk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkaibk: do a = b + 1, nvirt1
i_aibibkaibk: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkaibk(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkaibk
end do a_aibibkaibk
end do b_aibibkaibk
end do k_aibibkaibk
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, e == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibibkakbi: do k = nocc0, nocc1
b_aibibkakbi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkakbi: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibibkakbi: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkakbi(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkakbi
end do a_aibibkakbi
end do b_aibibkakbi
end do k_aibibkakbi
!
! Elementary loop 45
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, e == b, j == i, l == k, m == k
! No equalities independent of the above can hold.
!
k_aibibkakbk: do k = nocc0, nocc1
b_aibibkakbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkakbk: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibibkakbk: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkakbk(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkakbk
end do a_aibibkakbk
end do b_aibibkakbk
end do k_aibibkakbk
!
! Elementary loop 46
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
b_aibjbiaibi: do b = nvirt0, nvirt1
j_aibjbiaibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiaibi: do a = b + 1, nvirt1
i_aibjbiaibi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbiaibi(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbiaibi
end do a_aibjbiaibi
end do j_aibjbiaibi
end do b_aibjbiaibi
!
! Elementary loop 47
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
b_aibjbiaibj: do b = nvirt0, nvirt1
j_aibjbiaibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiaibj: do a = b + 1, nvirt1
i_aibjbiaibj: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbiaibj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbiaibj
end do a_aibjbiaibj
end do j_aibjbiaibj
end do b_aibjbiaibj
!
! Elementary loop 48
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbiajbi: do b = nvirt0, nvirt1
j_aibjbiajbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiajbi: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbiajbi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbiajbi(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbiajbi
end do a_aibjbiajbi
end do j_aibjbiajbi
end do b_aibjbiajbi
!
! Elementary loop 49
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
b_aibjbiajbj: do b = nvirt0, nvirt1
j_aibjbiajbj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiajbj: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbiajbj: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbiajbj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbiajbj
end do a_aibjbiajbj
end do j_aibjbiajbj
end do b_aibjbiajbj
!
! Elementary loop 50
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiajcicici: do c = nvirt0, nvirt1
j_aiajcicici: do j = nocc0, nocc1
a_aiajcicici: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcicici: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajcicici(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcicici
end do a_aiajcicici
end do j_aiajcicici
end do c_aiajcicici
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
end subroutine ccjac_32_part8
end module ccjac_block_32_part8
