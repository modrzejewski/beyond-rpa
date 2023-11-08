module ccjac_block_23_part7
use eom_cc3_23_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:35:25 UTC.
!
contains
 
subroutine ccjac_23_part7(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: i0
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
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckaiaj: do c = nvirt0, nvirt1
k_aiajckaiaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiaj: do j = nocc0, nocc1
if (j == k) cycle j_aiajckaiaj
a_aiajckaiaj: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j, k)
i_aiajckaiaj: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckaiaj(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckaiaj
end do a_aiajckaiaj
end do j_aiajckaiaj
end do k_aiajckaiaj
end do c_aiajckaiaj
!
! Elementary loop 2
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
a_aiajcjaiam: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j, m)
i_aiajcjaiam: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjaiam(eorb, t2, t1, nocc, nactive, a, i, j, c, m)
end do i_aiajcjaiam
end do a_aiajcjaiam
end do j_aiajcjaiam
end do c_aiajcjaiam
end do m_aiajcjaiam
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiajckaiak: do c = nvirt0, nvirt1
k_aiajckaiak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiak: do j = nocc0, nocc1
if (j == k) cycle j_aiajckaiak
a_aiajckaiak: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j, k)
i_aiajckaiak: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckaiak(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckaiak
end do a_aiajckaiak
end do j_aiajckaiak
end do k_aiajckaiak
end do c_aiajckaiak
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == a, e == a, j == i, m == i
! No equalities independent of the above can hold.
!
l_aiaickalai: do l = nocc0, nocc1
c_aiaickalai: do c = nvirt0, nvirt1
k_aiaickalai: do k = nocc0, l - 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a_aiaickalai: do a = nvirt0, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiaickalai: do i = nocc0, l - 1
if (i == k) cycle i_aiaickalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaickalai(eorb, t2, t1, nocc, nactive, a, i, c, k, l)
end do i_aiaickalai
end do a_aiaickalai
end do k_aiaickalai
end do c_aiaickalai
end do l_aiaickalai
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, l, m
! Equalities: b == a, d == a, e == a, j == i, k == i
! No equalities independent of the above can hold.
!
m_aiaicialam: do m = nocc0, nocc1
l_aiaicialam: do l = m + 1, nocc1
c_aiaicialam: do c = nvirt0, nvirt1
a_aiaicialam: do a = nvirt0, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiaicialam: do i = nocc0, l - 1
if (i == m) cycle i_aiaicialam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaicialam(eorb, t2, t1, nocc, nactive, a, i, c, l, m)
end do i_aiaicialam
end do a_aiaicialam
end do c_aiaicialam
end do l_aiaicialam
end do m_aiaicialam
!
! Elementary loop 6
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
a_aiajcialai: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajcialai: do i = j + 1, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcialai(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcialai
end do a_aiajcialai
end do j_aiajcialai
end do c_aiajcialai
end do l_aiajcialai
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiajckakai: do c = nvirt0, nvirt1
k_aiajckakai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckakai: do j = nocc0, nocc1
if (j == k) cycle j_aiajckakai
a_aiajckakai: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakai: do i = j + 1, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckakai(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckakai
end do a_aiajckakai
end do j_aiajckakai
end do k_aiajckakai
end do c_aiajckakai
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == a, m == i, k == j
! No equalities independent of the above can hold.
!
l_aiajcjalai: do l = nocc0, nocc1
c_aiajcjalai: do c = nvirt0, nvirt1
j_aiajcjalai: do j = nocc0, l - 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjalai: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajcjalai: do i = j + 1, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjalai(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcjalai
end do a_aiajcjalai
end do j_aiajcjalai
end do c_aiajcjalai
end do l_aiajcjalai
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajciajam: do m = nocc0, nocc1
c_aiajciajam: do c = nvirt0, nvirt1
j_aiajciajam: do j = m + 1, nocc1
a_aiajciajam: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajciajam: do i = j + 1, j - 1
if (i == m) cycle i_aiajciajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajciajam(eorb, t2, t1, nocc, nactive, a, i, j, c, m)
end do i_aiajciajam
end do a_aiajciajam
end do j_aiajciajam
end do c_aiajciajam
end do m_aiajciajam
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == a, k == i, m == j
! No equalities independent of the above can hold.
!
l_aiajcialaj: do l = nocc0, nocc1
c_aiajcialaj: do c = nvirt0, nvirt1
j_aiajcialaj: do j = nocc0, l - 1
a_aiajcialaj: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcialaj: do i = j + 1, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcialaj(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcialaj
end do a_aiajcialaj
end do j_aiajcialaj
end do c_aiajcialaj
end do l_aiajcialaj
!
! Elementary loop 11
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
a_aiajcjajam: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajcjajam: do i = j + 1, nocc1
if (i == m) cycle i_aiajcjajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjajam(eorb, t2, t1, nocc, nactive, a, i, j, c, m)
end do i_aiajcjajam
end do a_aiajcjajam
end do j_aiajcjajam
end do c_aiajcjajam
end do m_aiajcjajam
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == a, l == j, m == k
! No equalities independent of the above can hold.
!
c_aiajckajak: do c = nvirt0, nvirt1
k_aiajckajak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckajak: do j = k + 1, nocc1
a_aiajckajak: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckajak: do i = j + 1, nocc1
if (i == k) cycle i_aiajckajak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckajak(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckajak
end do a_aiajckajak
end do j_aiajckajak
end do k_aiajckajak
end do c_aiajckajak
!
! Elementary loop 13
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
a_aiajckakaj: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckakaj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckakaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckakaj(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckakaj
end do a_aiajckakaj
end do j_aiajckakaj
end do k_aiajckakaj
end do c_aiajckakaj
!
! Elementary loop 14
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
a_aiajcjalaj: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjalaj: do i = j + 1, nocc1
if (i == l) cycle i_aiajcjalaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjalaj(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcjalaj
end do a_aiajcjalaj
end do j_aiajcjalaj
end do c_aiajcjalaj
end do l_aiajcjalaj
!
! Elementary loop 15
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k
! Equalities: b == a, d == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiaickaiei: do e = nvirt0, nvirt1
c_aiaickaiei: do c = nvirt0, nvirt1
if (c == e) cycle c_aiaickaiei
k_aiaickaiei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a_aiaickaiei: do a = e + 1, c - 1
i_aiaickaiei: do i = nocc0, nocc1
if (i == k) cycle i_aiaickaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaickaiei(eorb, t2, t1, nocc, nactive, a, i, c, k, e)
end do i_aiaickaiei
end do a_aiaickaiei
end do k_aiaickaiei
end do c_aiaickaiei
end do e_aiaickaiei
!
! Elementary loop 16
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aiajcjaiei: do e = nvirt0, nvirt1
c_aiajcjaiei: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajcjaiei
j_aiajcjaiei: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjaiei: do a = e + 1, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjaiei: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjaiei(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajcjaiei
end do a_aiajcjaiei
end do j_aiajcjaiei
end do c_aiajcjaiei
end do e_aiajcjaiei
!
! Elementary loop 17
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aiajcjaiej: do e = nvirt0, nvirt1
c_aiajcjaiej: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajcjaiej
j_aiajcjaiej: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjaiej: do a = e + 1, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjaiej: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjaiej(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajcjaiej
end do a_aiajcjaiej
end do j_aiajcjaiej
end do c_aiajcjaiej
end do e_aiajcjaiej
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, l
! Equalities: b == a, d == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aiaicialei: do e = nvirt0, nvirt1
l_aiaicialei: do l = nocc0, nocc1
c_aiaicialei: do c = nvirt0, nvirt1
if (c == e) cycle c_aiaicialei
a_aiaicialei: do a = e + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiaicialei: do i = nocc0, nocc1
if (i == l) cycle i_aiaicialei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaicialei(eorb, t2, t1, nocc, nactive, a, i, c, l, e)
end do i_aiaicialei
end do a_aiaicialei
end do c_aiaicialei
end do l_aiaicialei
end do e_aiaicialei
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajciajei: do e = nvirt0, nvirt1
c_aiajciajei: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajciajei
j_aiajciajei: do j = nocc0, nocc1
a_aiajciajei: do a = e + 1, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciajei: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajciajei(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajciajei
end do a_aiajciajei
end do j_aiajciajei
end do c_aiajciajei
end do e_aiajciajei
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aiajciajej: do e = nvirt0, nvirt1
c_aiajciajej: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajciajej
j_aiajciajej: do j = nocc0, nocc1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajciajej: do a = e + 1, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciajej: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajciajej(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajciajej
end do a_aiajciajej
end do j_aiajciajej
end do c_aiajciajej
end do e_aiajciajej
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, m
! Equalities: d == a, e == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibiciaiam: do m = nocc0, nocc1
c_aibiciaiam: do c = nvirt0, nvirt1
b_aibiciaiam: do b = nvirt0, nvirt1
if (b == c) cycle b_aibiciaiam
a_aibiciaiam: do a = b + 1, c - 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibiciaiam: do i = m + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiciaiam(eorb, t2, t1, nocc, nactive, a, i, b, c, m)
end do i_aibiciaiam
end do a_aibiciaiam
end do b_aibiciaiam
end do c_aibiciaiam
end do m_aibiciaiam
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciaiaj: do c = nvirt0, nvirt1
b_aibjciaiaj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjciaiaj
j_aibjciaiaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciaiaj: do a = b + 1, c - 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciaiaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjciaiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciaiaj
end do a_aibjciaiaj
end do j_aibjciaiaj
end do b_aibjciaiaj
end do c_aibjciaiaj
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjaiaj: do c = nvirt0, nvirt1
b_aibjcjaiaj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjaiaj
j_aibjcjaiaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjaiaj: do a = b + 1, c - 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjaiaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjaiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjaiaj
end do a_aibjcjaiaj
end do j_aibjcjaiaj
end do b_aibjcjaiaj
end do c_aibjcjaiaj
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l
! Equalities: d == a, e == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibicialai: do l = nocc0, nocc1
c_aibicialai: do c = nvirt0, nvirt1
b_aibicialai: do b = nvirt0, nvirt1
if (b == c) cycle b_aibicialai
a_aibicialai: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibicialai: do i = nocc0, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibicialai(eorb, t2, t1, nocc, nactive, a, i, b, c, l)
end do i_aibicialai
end do a_aibicialai
end do b_aibicialai
end do c_aibicialai
end do l_aibicialai
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajai: do c = nvirt0, nvirt1
b_aibjciajai: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjciajai
j_aibjciajai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciajai: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajai: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjciajai(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciajai
end do a_aibjciajai
end do j_aibjciajai
end do b_aibjciajai
end do c_aibjciajai
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjajai: do c = nvirt0, nvirt1
b_aibjcjajai: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjajai
j_aibjcjajai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjajai: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajai: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjajai(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjajai
end do a_aibjcjajai
end do j_aibjcjajai
end do b_aibjcjajai
end do c_aibjcjajai
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibickaibi: do c = nvirt0, nvirt1
k_aibickaibi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickaibi: do b = nvirt0, nvirt1
if (b == c) cycle b_aibickaibi
a_aibickaibi: do a = b + 1, c - 1
i_aibickaibi: do i = nocc0, nocc1
if (i == k) cycle i_aibickaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibickaibi(eorb, t2, t1, nocc, nactive, a, i, b, c, k)
end do i_aibickaibi
end do a_aibickaibi
end do b_aibickaibi
end do k_aibickaibi
end do c_aibickaibi
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickaibk: do c = nvirt0, nvirt1
k_aibickaibk: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickaibk: do b = nvirt0, nvirt1
if (b == c) cycle b_aibickaibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibickaibk: do a = b + 1, c - 1
i_aibickaibk: do i = nocc0, nocc1
if (i == k) cycle i_aibickaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibickaibk(eorb, t2, t1, nocc, nactive, a, i, b, c, k)
end do i_aibickaibk
end do a_aibickaibk
end do b_aibickaibk
end do k_aibickaibk
end do c_aibickaibk
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjaibi: do c = nvirt0, nvirt1
b_aibjcjaibi: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjaibi
j_aibjcjaibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjaibi: do a = b + 1, c - 1
i_aibjcjaibi: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjaibi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjaibi
end do a_aibjcjaibi
end do j_aibjcjaibi
end do b_aibjcjaibi
end do c_aibjcjaibi
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjaibj: do c = nvirt0, nvirt1
b_aibjcjaibj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjaibj
j_aibjcjaibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjaibj: do a = b + 1, c - 1
i_aibjcjaibj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjaibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjaibj
end do a_aibjcjaibj
end do j_aibjcjaibj
end do b_aibjcjaibj
end do c_aibjcjaibj
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l
! Equalities: d == a, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibicialbi: do l = nocc0, nocc1
c_aibicialbi: do c = nvirt0, nvirt1
b_aibicialbi: do b = nvirt0, nvirt1
if (b == c) cycle b_aibicialbi
a_aibicialbi: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibicialbi: do i = nocc0, nocc1
if (i == l) cycle i_aibicialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibicialbi(eorb, t2, t1, nocc, nactive, a, i, b, c, l)
end do i_aibicialbi
end do a_aibicialbi
end do b_aibicialbi
end do c_aibicialbi
end do l_aibicialbi
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l
! Equalities: d == a, e == b, j == i, k == i, m == l
! No equalities independent of the above can hold.
!
l_aibicialbl: do l = nocc0, nocc1
c_aibicialbl: do c = nvirt0, nvirt1
b_aibicialbl: do b = nvirt0, nvirt1
if (b == c) cycle b_aibicialbl
em = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibicialbl: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibicialbl: do i = nocc0, nocc1
if (i == l) cycle i_aibicialbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibicialbl(eorb, t2, t1, nocc, nactive, a, i, b, c, l)
end do i_aibicialbl
end do a_aibicialbl
end do b_aibicialbl
end do c_aibicialbl
end do l_aibicialbl
!
! Elementary loop 33
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajbi: do c = nvirt0, nvirt1
b_aibjciajbi: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjciajbi
j_aibjciajbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciajbi: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajbi: do i = nocc0, nocc1
if (i == j) cycle i_aibjciajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjciajbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciajbi
end do a_aibjciajbi
end do j_aibjciajbi
end do b_aibjciajbi
end do c_aibjciajbi
!
! Elementary loop 34
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjciajbj: do c = nvirt0, nvirt1
b_aibjciajbj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjciajbj
j_aibjciajbj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciajbj: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajbj: do i = nocc0, nocc1
if (i == j) cycle i_aibjciajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjciajbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciajbj
end do a_aibjciajbj
end do j_aibjciajbj
end do b_aibjciajbj
end do c_aibjciajbj
!
! Elementary loop 35
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, m
! Equalities: d == b, e == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibicibibm: do m = nocc0, nocc1
c_aibicibibm: do c = nvirt0, nvirt1
b_aibicibibm: do b = nvirt0, c - 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a_aibicibibm: do a = b + 1, nvirt1
if (a == c) cycle a_aibicibibm
i_aibicibibm: do i = m + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibicibibm(eorb, t2, t1, nocc, nactive, a, i, b, c, m)
end do i_aibicibibm
end do a_aibicibibm
end do b_aibicibibm
end do c_aibicibibm
end do m_aibicibibm
!
! Elementary loop 36
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjcibibj: do c = nvirt0, nvirt1
b_aibjcibibj: do b = nvirt0, c - 1
j_aibjcibibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibibj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibibj
i_aibjcibibj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcibibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcibibj
end do a_aibjcibibj
end do j_aibjcibibj
end do b_aibjcibibj
end do c_aibjcibibj
!
! Elementary loop 37
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjbibj: do c = nvirt0, nvirt1
b_aibjcjbibj: do b = nvirt0, c - 1
j_aibjcjbibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbibj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbibj
i_aibjcjbibj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjbibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjbibj
end do a_aibjcjbibj
end do j_aibjcjbibj
end do b_aibjcjbibj
end do c_aibjcjbibj
!
! Elementary loop 38
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, l
! Equalities: d == b, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiciblbi: do l = nocc0, nocc1
c_aibiciblbi: do c = nvirt0, nvirt1
b_aibiciblbi: do b = nvirt0, c - 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibiciblbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibiciblbi
i_aibiciblbi: do i = nocc0, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiciblbi(eorb, t2, t1, nocc, nactive, a, i, b, c, l)
end do i_aibiciblbi
end do a_aibiciblbi
end do b_aibiciblbi
end do c_aibiciblbi
end do l_aibiciblbi
!
! Elementary loop 39
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibjbi: do c = nvirt0, nvirt1
b_aibjcibjbi: do b = nvirt0, c - 1
j_aibjcibjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibjbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibjbi
i_aibjcibjbi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcibjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcibjbi
end do a_aibjcibjbi
end do j_aibjcibjbi
end do b_aibjcibjbi
end do c_aibjcibjbi
!
! Elementary loop 40
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjbjbi: do c = nvirt0, nvirt1
b_aibjcjbjbi: do b = nvirt0, c - 1
j_aibjcjbjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbjbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbjbi
i_aibjcjbjbi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjbjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjbjbi
end do a_aibjcjbjbi
end do j_aibjcjbjbi
end do b_aibjcjbjbi
end do c_aibjcjbjbi
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, e == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aiaickdiai: do d = nvirt0, nvirt1
c_aiaickdiai: do c = d + 1, nvirt1
k_aiaickdiai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a_aiaickdiai: do a = nvirt0, d - 1
if (a == c) cycle a_aiaickdiai
i_aiaickdiai: do i = nocc0, nocc1
if (i == k) cycle i_aiaickdiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaickdiai(eorb, t2, t1, nocc, nactive, a, i, c, k, d)
end do i_aiaickdiai
end do a_aiaickdiai
end do k_aiaickdiai
end do c_aiaickdiai
end do d_aiaickdiai
!
! Elementary loop 42
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
d_aiajcjdiai: do d = nvirt0, nvirt1
c_aiajcjdiai: do c = d + 1, nvirt1
j_aiajcjdiai: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjdiai: do a = nvirt0, d - 1
if (a == c) cycle a_aiajcjdiai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjdiai: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjdiai(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcjdiai
end do a_aiajcjdiai
end do j_aiajcjdiai
end do c_aiajcjdiai
end do d_aiajcjdiai
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aiajcjdiaj: do d = nvirt0, nvirt1
c_aiajcjdiaj: do c = d + 1, nvirt1
j_aiajcjdiaj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjdiaj: do a = nvirt0, d - 1
if (a == c) cycle a_aiajcjdiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjdiaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjdiaj(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcjdiaj
end do a_aiajcjdiaj
end do j_aiajcjdiaj
end do c_aiajcjdiaj
end do d_aiajcjdiaj
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, l
! Equalities: b == a, e == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aiaicidlai: do d = nvirt0, nvirt1
l_aiaicidlai: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aiaicidlai: do c = d + 1, nvirt1
a_aiaicidlai: do a = nvirt0, d - 1
if (a == c) cycle a_aiaicidlai
i_aiaicidlai: do i = nocc0, nocc1
if (i == l) cycle i_aiaicidlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaicidlai(eorb, t2, t1, nocc, nactive, a, i, c, d, l)
end do i_aiaicidlai
end do a_aiaicidlai
end do c_aiaicidlai
end do l_aiaicidlai
end do d_aiaicidlai
!
! Elementary loop 45
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidjai: do d = nvirt0, nvirt1
c_aiajcidjai: do c = d + 1, nvirt1
j_aiajcidjai: do j = nocc0, nocc1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcidjai: do a = nvirt0, d - 1
if (a == c) cycle a_aiajcidjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidjai: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcidjai(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcidjai
end do a_aiajcidjai
end do j_aiajcidjai
end do c_aiajcidjai
end do d_aiajcidjai
!
! Elementary loop 46
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
d_aiajcidjaj: do d = nvirt0, nvirt1
c_aiajcidjaj: do c = d + 1, nvirt1
j_aiajcidjaj: do j = nocc0, nocc1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcidjaj: do a = nvirt0, d - 1
if (a == c) cycle a_aiajcidjaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidjaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcidjaj(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcidjaj
end do a_aiajcidjaj
end do j_aiajcidjaj
end do c_aiajcidjaj
end do d_aiajcidjaj
!
! Elementary loop 47
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, k
! Equalities: b == a, c == a, d == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiaiakaiei: do e = nvirt0, nvirt1
k_aiaiakaiei: do k = nocc0, nocc1
a_aiaiakaiei: do a = e + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiaiakaiei: do i = nocc0, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaiakaiei(eorb, t2, t1, nocc, nactive, a, i, k, e)
end do i_aiaiakaiei
end do a_aiaiakaiei
end do k_aiaiakaiei
end do e_aiaiakaiei
!
! Elementary loop 48
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, k
! Equalities: b == a, c == a, d == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aiaiakaiek: do e = nvirt0, nvirt1
k_aiaiakaiek: do k = nocc0, nocc1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
a_aiaiakaiek: do a = e + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiaiakaiek: do i = nocc0, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaiakaiek(eorb, t2, t1, nocc, nactive, a, i, k, e)
end do i_aiaiakaiek
end do a_aiaiakaiek
end do k_aiaiakaiek
end do e_aiaiakaiek
!
! Elementary loop 49
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, l
! Equalities: b == a, c == a, d == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aiaiaialei: do e = nvirt0, nvirt1
l_aiaiaialei: do l = nocc0, nocc1
a_aiaiaialei: do a = e + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiaiaialei: do i = l + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaiaialei(eorb, t2, t1, nocc, nactive, a, i, l, e)
end do i_aiaiaialei
end do a_aiaiaialei
end do l_aiaiaialei
end do e_aiaiaialei
!
! Elementary loop 50
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, l
! Equalities: b == a, c == a, d == a, j == i, k == i, m == l
! No equalities independent of the above can hold.
!
e_aiaiaialel: do e = nvirt0, nvirt1
l_aiaiaialel: do l = nocc0, nocc1
em = (e - nvirt0) * nocc + (l - nocc0) + 1
a_aiaiaialel: do a = e + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiaiaialel: do i = l + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaiaialel(eorb, t2, t1, nocc, nactive, a, i, l, e)
end do i_aiaiaialel
end do a_aiaiaialel
end do l_aiaiaialel
end do e_aiaiaialel
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
end subroutine ccjac_23_part7
end module ccjac_block_23_part7
