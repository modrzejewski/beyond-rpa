module ccjac_block_23_part4
use eom_cc3_23_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:35:25 UTC.
!
contains
 
subroutine ccjac_23_part4(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: a0, i0, i1
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
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjcialbi: do l = nocc0, nocc1
c_aibjcialbi: do c = nvirt0, nvirt1
b_aibjcialbi: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcialbi
j_aibjcialbi: do j = nocc0, nocc1
if (j == l) cycle j_aibjcialbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcialbi: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialbi: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcialbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcialbi
end do a_aibjcialbi
end do j_aibjcialbi
end do b_aibjcialbi
end do c_aibjcialbi
end do l_aibjcialbi
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjalbi: do l = nocc0, nocc1
c_aibjcjalbi: do c = nvirt0, nvirt1
b_aibjcjalbi: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjalbi
j_aibjcjalbi: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjalbi: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalbi: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjalbi
if (j > l .and. l > i) cycle i_aibjcjalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjalbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjalbi
end do a_aibjcjalbi
end do j_aibjcjalbi
end do b_aibjcjalbi
end do c_aibjcjalbi
end do l_aibjcjalbi
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjciajbm: do m = nocc0, nocc1
c_aibjciajbm: do c = nvirt0, nvirt1
b_aibjciajbm: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjciajbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjciajbm: do j = nocc0, nocc1
if (j == m) cycle j_aibjciajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciajbm: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajbm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjciajbm
if (i > j .and. j > m) exit i_aibjciajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjciajbm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjciajbm
end do a_aibjciajbm
end do j_aibjciajbm
end do b_aibjciajbm
end do c_aibjciajbm
end do m_aibjciajbm
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l, j
! Equalities: d == a, e == b, k == i, m == l
! No equalities independent of the above can hold.
!
l_aibjcialbl: do l = nocc0, nocc1
c_aibjcialbl: do c = nvirt0, nvirt1
b_aibjcialbl: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcialbl
em = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcialbl: do j = nocc0, nocc1
if (j == l) cycle j_aibjcialbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcialbl: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialbl: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcialbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcialbl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcialbl
end do a_aibjcialbl
end do j_aibjcialbl
end do b_aibjcialbl
end do c_aibjcialbl
end do l_aibjcialbl
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjcialbj: do l = nocc0, nocc1
c_aibjcialbj: do c = nvirt0, nvirt1
b_aibjcialbj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcialbj
j_aibjcialbj: do j = nocc0, nocc1
if (j == l) cycle j_aibjcialbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcialbj: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialbj: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcialbj
if (i > l .and. l > j) exit i_aibjcialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcialbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcialbj
end do a_aibjcialbj
end do j_aibjcialbj
end do b_aibjcialbj
end do c_aibjcialbj
end do l_aibjcialbj
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: d == a, e == b, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjckajbj: do c = nvirt0, nvirt1
k_aibjckajbj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajbj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjckajbj
j_aibjckajbj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckajbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckajbj: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajbj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckajbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end do i_aibjckajbj
end do a_aibjckajbj
end do j_aibjckajbj
end do b_aibjckajbj
end do k_aibjckajbj
end do c_aibjckajbj
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, l, i
! Equalities: d == a, e == b, k == j, m == l
! No equalities independent of the above can hold.
!
l_aibjcjalbl: do l = nocc0, nocc1
c_aibjcjalbl: do c = nvirt0, nvirt1
b_aibjcjalbl: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjalbl
em = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcjalbl: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjalbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjalbl: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalbl: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjalbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjalbl(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjalbl
end do a_aibjcjalbl
end do j_aibjcjalbl
end do b_aibjcjalbl
end do c_aibjcjalbl
end do l_aibjcjalbl
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjcjalbj: do l = nocc0, nocc1
c_aibjcjalbj: do c = nvirt0, nvirt1
b_aibjcjalbj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjalbj
j_aibjcjalbj: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjalbj: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalbj: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjalbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjalbj
end do a_aibjcjalbj
end do j_aibjcjalbj
end do b_aibjcjalbj
end do c_aibjcjalbj
end do l_aibjcjalbj
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibickaiei: do e = nvirt0, nvirt1
c_aibickaiei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibickaiei
k_aibickaiei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickaiei: do b = nvirt0, nvirt1
if (b == c .or. b == e) cycle b_aibickaiei
a0 = max(b, e)
a_aibickaiei: do a = a0 + 1, c - 1
i_aibickaiei: do i = nocc0, nocc1
if (i == k) cycle i_aibickaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibickaiei(eorb, t2, t1, nocc, nactive, a, i, b, c, k, e)
end do i_aibickaiei
end do a_aibickaiei
end do b_aibickaiei
end do k_aibickaiei
end do c_aibickaiei
end do e_aibickaiei
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjaiei: do e = nvirt0, nvirt1
c_aibjcjaiei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcjaiei
b_aibjcjaiei: do b = nvirt0, nvirt1
if (b == c .or. b == e) cycle b_aibjcjaiei
j_aibjcjaiei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjcjaiei: do a = a0 + 1, c - 1
i_aibjcjaiei: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjaiei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcjaiei
end do a_aibjcjaiei
end do j_aibjcjaiei
end do b_aibjcjaiei
end do c_aibjcjaiei
end do e_aibjcjaiei
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjaiej: do e = nvirt0, nvirt1
c_aibjcjaiej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcjaiej
b_aibjcjaiej: do b = nvirt0, nvirt1
if (b == c .or. b == e) cycle b_aibjcjaiej
j_aibjcjaiej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjcjaiej: do a = a0 + 1, c - 1
i_aibjcjaiej: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjaiej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcjaiej
end do a_aibjcjaiej
end do j_aibjcjaiej
end do b_aibjcjaiej
end do c_aibjcjaiej
end do e_aibjcjaiej
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, l
! Equalities: d == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibicialei: do e = nvirt0, nvirt1
l_aibicialei: do l = nocc0, nocc1
c_aibicialei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibicialei
b_aibicialei: do b = nvirt0, nvirt1
if (b == c .or. b == e) cycle b_aibicialei
a0 = max(b, e)
a_aibicialei: do a = a0 + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibicialei: do i = nocc0, nocc1
if (i == l) cycle i_aibicialei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibicialei(eorb, t2, t1, nocc, nactive, a, i, b, c, l, e)
end do i_aibicialei
end do a_aibicialei
end do b_aibicialei
end do c_aibicialei
end do l_aibicialei
end do e_aibicialei
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjciajej: do e = nvirt0, nvirt1
c_aibjciajej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjciajej
b_aibjciajej: do b = nvirt0, nvirt1
if (b == c .or. b == e) cycle b_aibjciajej
j_aibjciajej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjciajej: do a = a0 + 1, c - 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajej: do i = nocc0, nocc1
if (i == j) cycle i_aibjciajej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjciajej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjciajej
end do a_aibjciajej
end do j_aibjciajej
end do b_aibjciajej
end do c_aibjciajej
end do e_aibjciajej
!
! Elementary loop 14
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckbibj: do c = nvirt0, nvirt1
k_aibjckbibj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbibj: do b = nvirt0, c - 1
j_aibjckbibj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbibj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbibj
i0 = max(j, k)
i_aibjckbibj: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckbibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end do i_aibjckbibj
end do a_aibjckbibj
end do j_aibjckbibj
end do b_aibjckbibj
end do k_aibjckbibj
end do c_aibjckbibj
!
! Elementary loop 15
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, m
! Equalities: d == b, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjbibm: do m = nocc0, nocc1
c_aibjcjbibm: do c = nvirt0, nvirt1
b_aibjcjbibm: do b = nvirt0, c - 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjbibm: do j = nocc0, nocc1
if (j == m) cycle j_aibjcjbibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbibm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbibm
i0 = max(m, j)
i_aibjcjbibm: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjbibm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcjbibm
end do a_aibjcjbibm
end do j_aibjcjbibm
end do b_aibjcjbibm
end do c_aibjcjbibm
end do m_aibjcjbibm
!
! Elementary loop 16
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckbjbi: do c = nvirt0, nvirt1
k_aibjckbjbi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbjbi: do b = nvirt0, c - 1
j_aibjckbjbi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckbjbi
i_aibjckbjbi: do i = nocc0, j - 1
if (i == k) cycle i_aibjckbjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckbjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end do i_aibjckbjbi
end do a_aibjckbjbi
end do j_aibjckbjbi
end do b_aibjckbjbi
end do k_aibjckbjbi
end do c_aibjckbjbi
!
! Elementary loop 17
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjblbi: do l = nocc0, nocc1
c_aibjcjblbi: do c = nvirt0, nvirt1
b_aibjcjblbi: do b = nvirt0, c - 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcjblbi: do j = nocc0, l - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjblbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjblbi
i_aibjcjblbi: do i = nocc0, l - 1
if (i == j) cycle i_aibjcjblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjblbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjblbi
end do a_aibjcjblbi
end do j_aibjcjblbi
end do b_aibjcjblbi
end do c_aibjcjblbi
end do l_aibjcjblbi
!
! Elementary loop 18
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, m
! Equalities: d == b, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcibjbm: do m = nocc0, nocc1
c_aibjcibjbm: do c = nvirt0, nvirt1
b_aibjcibjbm: do b = nvirt0, c - 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcibjbm: do j = m + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibjbm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibjbm
i_aibjcibjbm: do i = nocc0, j - 1
if (i == m) cycle i_aibjcibjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcibjbm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcibjbm
end do a_aibjcibjbm
end do j_aibjcibjbm
end do b_aibjcibjbm
end do c_aibjcibjbm
end do m_aibjcibjbm
!
! Elementary loop 19
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjciblbj: do l = nocc0, nocc1
c_aibjciblbj: do c = nvirt0, nvirt1
b_aibjciblbj: do b = nvirt0, c - 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjciblbj: do j = nocc0, l - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciblbj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciblbj
i_aibjciblbj: do i = nocc0, l - 1
if (i == j) cycle i_aibjciblbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjciblbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjciblbj
end do a_aibjciblbj
end do j_aibjciblbj
end do b_aibjciblbj
end do c_aibjciblbj
end do l_aibjciblbj
!
! Elementary loop 20
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, k
! Equalities: d == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibickbiei: do e = nvirt0, nvirt1
c_aibickbiei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibickbiei
k_aibickbiei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickbiei: do b = e + 1, c - 1
a_aibickbiei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibickbiei
i_aibickbiei: do i = nocc0, nocc1
if (i == k) cycle i_aibickbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibickbiei(eorb, t2, t1, nocc, nactive, a, i, b, c, k, e)
end do i_aibickbiei
end do a_aibickbiei
end do b_aibickbiei
end do k_aibickbiei
end do c_aibickbiei
end do e_aibickbiei
!
! Elementary loop 21
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjbiei: do e = nvirt0, nvirt1
c_aibjcjbiei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcjbiei
b_aibjcjbiei: do b = e + 1, c - 1
j_aibjcjbiei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbiei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjcjbiei
i_aibjcjbiei: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjbiei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcjbiei
end do a_aibjcjbiei
end do j_aibjcjbiei
end do b_aibjcjbiei
end do c_aibjcjbiei
end do e_aibjcjbiei
!
! Elementary loop 22
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, l
! Equalities: d == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibiciblei: do e = nvirt0, nvirt1
l_aibiciblei: do l = nocc0, nocc1
c_aibiciblei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibiciblei
b_aibiciblei: do b = e + 1, c - 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibiciblei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibiciblei
i_aibiciblei: do i = nocc0, nocc1
if (i == l) cycle i_aibiciblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiciblei(eorb, t2, t1, nocc, nactive, a, i, b, c, l, e)
end do i_aibiciblei
end do a_aibiciblei
end do b_aibiciblei
end do c_aibiciblei
end do l_aibiciblei
end do e_aibiciblei
!
! Elementary loop 23
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjcibjei: do e = nvirt0, nvirt1
c_aibjcibjei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcibjei
b_aibjcibjei: do b = e + 1, c - 1
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
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcibjei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcibjei
end do a_aibjcibjei
end do j_aibjcibjei
end do b_aibjcibjei
end do c_aibjcibjei
end do e_aibjcibjei
!
! Elementary loop 24
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcibjej: do e = nvirt0, nvirt1
c_aibjcibjej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcibjej
b_aibjcibjej: do b = e + 1, c - 1
j_aibjcibjej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibjej: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjcibjej
i_aibjcibjej: do i = nocc0, nocc1
if (i == j) cycle i_aibjcibjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcibjej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e)
end do i_aibjcibjej
end do a_aibjcibjej
end do j_aibjcibjej
end do b_aibjcibjej
end do c_aibjcibjej
end do e_aibjcibjej
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajckdiaj: do d = nvirt0, nvirt1
c_aiajckdiaj: do c = d + 1, nvirt1
k_aiajckdiaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdiaj: do j = nocc0, nocc1
if (j == k) cycle j_aiajckdiaj
a_aiajckdiaj: do a = nvirt0, d - 1
if (a == c) cycle a_aiajckdiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdiaj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdiaj
if (k > i .and. i > j) cycle i_aiajckdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckdiaj(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdiaj
end do a_aiajckdiaj
end do j_aiajckdiaj
end do k_aiajckdiaj
end do c_aiajckdiaj
end do d_aiajckdiaj
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, m
! Equalities: b == a, e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aiajcjdiam: do m = nocc0, nocc1
d_aiajcjdiam: do d = nvirt0, nvirt1
c_aiajcjdiam: do c = d + 1, nvirt1
j_aiajcjdiam: do j = nocc0, nocc1
if (j == m) cycle j_aiajcjdiam
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjdiam: do a = nvirt0, d - 1
if (a == c) cycle a_aiajcjdiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajcjdiam: do i = j + 1, nocc1
if (i == m) cycle i_aiajcjdiam
if (j > i .and. i > m) cycle i_aiajcjdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjdiam(eorb, t2, t1, nocc, nactive, a, i, j, c, d, m)
end do i_aiajcjdiam
end do a_aiajcjdiam
end do j_aiajcjdiam
end do c_aiajcjdiam
end do d_aiajcjdiam
end do m_aiajcjdiam
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajckdjai: do d = nvirt0, nvirt1
c_aiajckdjai: do c = d + 1, nvirt1
k_aiajckdjai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdjai: do j = nocc0, nocc1
if (j == k) cycle j_aiajckdjai
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckdjai: do a = nvirt0, d - 1
if (a == c) cycle a_aiajckdjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdjai: do i = j + 1, nocc1
if (i == k) cycle i_aiajckdjai
if (k > j .and. j > i) cycle i_aiajckdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckdjai(eorb, t2, t1, nocc, nactive, a, i, j, c, k, d)
end do i_aiajckdjai
end do a_aiajckdjai
end do j_aiajckdjai
end do k_aiajckdjai
end do c_aiajckdjai
end do d_aiajckdjai
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, l
! Equalities: b == a, e == a, m == i, k == j
! No equalities independent of the above can hold.
!
d_aiajcjdlai: do d = nvirt0, nvirt1
l_aiajcjdlai: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aiajcjdlai: do c = d + 1, nvirt1
j_aiajcjdlai: do j = nocc0, nocc1
if (j == l) cycle j_aiajcjdlai
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjdlai: do a = nvirt0, d - 1
if (a == c) cycle a_aiajcjdlai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjdlai: do i = j + 1, nocc1
if (i == l) cycle i_aiajcjdlai
if (j > l .and. l > i) cycle i_aiajcjdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjdlai(eorb, t2, t1, nocc, nactive, a, i, j, c, d, l)
end do i_aiajcjdlai
end do a_aiajcjdlai
end do j_aiajcjdlai
end do c_aiajcjdlai
end do l_aiajcjdlai
end do d_aiajcjdlai
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, m
! Equalities: b == a, e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajcidjam: do m = nocc0, nocc1
d_aiajcidjam: do d = nvirt0, nvirt1
c_aiajcidjam: do c = d + 1, nvirt1
j_aiajcidjam: do j = nocc0, nocc1
if (j == m) cycle j_aiajcidjam
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcidjam: do a = nvirt0, d - 1
if (a == c) cycle a_aiajcidjam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajcidjam: do i = j + 1, nocc1
if (i == m) cycle i_aiajcidjam
if (i > j .and. j > m) exit i_aiajcidjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcidjam(eorb, t2, t1, nocc, nactive, a, i, j, c, d, m)
end do i_aiajcidjam
end do a_aiajcidjam
end do j_aiajcidjam
end do c_aiajcidjam
end do d_aiajcidjam
end do m_aiajcidjam
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, l
! Equalities: b == a, e == a, k == i, m == j
! No equalities independent of the above can hold.
!
d_aiajcidlaj: do d = nvirt0, nvirt1
l_aiajcidlaj: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aiajcidlaj: do c = d + 1, nvirt1
j_aiajcidlaj: do j = nocc0, nocc1
if (j == l) cycle j_aiajcidlaj
a_aiajcidlaj: do a = nvirt0, d - 1
if (a == c) cycle a_aiajcidlaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidlaj: do i = j + 1, nocc1
if (i == l) cycle i_aiajcidlaj
if (i > l .and. l > j) exit i_aiajcidlaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcidlaj(eorb, t2, t1, nocc, nactive, a, i, j, c, d, l)
end do i_aiajcidlaj
end do a_aiajcidlaj
end do j_aiajcidlaj
end do c_aiajcidlaj
end do l_aiajcidlaj
end do d_aiajcidlaj
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, k
! Equalities: e == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibickdiai: do d = nvirt0, nvirt1
c_aibickdiai: do c = d + 1, nvirt1
k_aibickdiai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickdiai: do b = nvirt0, nvirt1
if (b == c .or. b == d) cycle b_aibickdiai
a_aibickdiai: do a = b + 1, d - 1
if (a == c) cycle a_aibickdiai
i_aibickdiai: do i = nocc0, nocc1
if (i == k) cycle i_aibickdiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibickdiai(eorb, t2, t1, nocc, nactive, a, i, b, c, k, d)
end do i_aibickdiai
end do a_aibickdiai
end do b_aibickdiai
end do k_aibickdiai
end do c_aibickdiai
end do d_aibickdiai
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdiai: do d = nvirt0, nvirt1
c_aibjcjdiai: do c = d + 1, nvirt1
b_aibjcjdiai: do b = nvirt0, nvirt1
if (b == c .or. b == d) cycle b_aibjcjdiai
j_aibjcjdiai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdiai: do a = b + 1, d - 1
if (a == c) cycle a_aibjcjdiai
i_aibjcjdiai: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjdiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjdiai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdiai
end do a_aibjcjdiai
end do j_aibjcjdiai
end do b_aibjcjdiai
end do c_aibjcjdiai
end do d_aibjcjdiai
!
! Elementary loop 33
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdiaj: do d = nvirt0, nvirt1
c_aibjcjdiaj: do c = d + 1, nvirt1
b_aibjcjdiaj: do b = nvirt0, nvirt1
if (b == c .or. b == d) cycle b_aibjcjdiaj
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
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjdiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdiaj
end do a_aibjcjdiaj
end do j_aibjcjdiaj
end do b_aibjcjdiaj
end do c_aibjcjdiaj
end do d_aibjcjdiaj
!
! Elementary loop 34
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, l
! Equalities: e == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibicidlai: do d = nvirt0, nvirt1
l_aibicidlai: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aibicidlai: do c = d + 1, nvirt1
b_aibicidlai: do b = nvirt0, nvirt1
if (b == c .or. b == d) cycle b_aibicidlai
a_aibicidlai: do a = b + 1, d - 1
if (a == c) cycle a_aibicidlai
i_aibicidlai: do i = nocc0, nocc1
if (i == l) cycle i_aibicidlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibicidlai(eorb, t2, t1, nocc, nactive, a, i, b, c, d, l)
end do i_aibicidlai
end do a_aibicidlai
end do b_aibicidlai
end do c_aibicidlai
end do l_aibicidlai
end do d_aibicidlai
!
! Elementary loop 35
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidjai: do d = nvirt0, nvirt1
c_aibjcidjai: do c = d + 1, nvirt1
b_aibjcidjai: do b = nvirt0, nvirt1
if (b == c .or. b == d) cycle b_aibjcidjai
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
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcidjai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
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
! Equalities: e == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcidjaj: do d = nvirt0, nvirt1
c_aibjcidjaj: do c = d + 1, nvirt1
b_aibjcidjaj: do b = nvirt0, nvirt1
if (b == c .or. b == d) cycle b_aibjcidjaj
j_aibjcidjaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidjaj: do a = b + 1, d - 1
if (a == c) cycle a_aibjcidjaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcidjaj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcidjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcidjaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcidjaj
end do a_aibjcidjaj
end do j_aibjcidjaj
end do b_aibjcidjaj
end do c_aibjcidjaj
end do d_aibjcidjaj
!
! Elementary loop 37
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, k
! Equalities: e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibickdibi: do d = nvirt0, nvirt1
c_aibickdibi: do c = d + 1, nvirt1
k_aibickdibi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickdibi: do b = nvirt0, d - 1
if (b == c) cycle b_aibickdibi
a_aibickdibi: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibickdibi
i_aibickdibi: do i = nocc0, nocc1
if (i == k) cycle i_aibickdibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibickdibi(eorb, t2, t1, nocc, nactive, a, i, b, c, k, d)
end do i_aibickdibi
end do a_aibickdibi
end do b_aibickdibi
end do k_aibickdibi
end do c_aibickdibi
end do d_aibickdibi
!
! Elementary loop 38
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdibi: do d = nvirt0, nvirt1
c_aibjcjdibi: do c = d + 1, nvirt1
b_aibjcjdibi: do b = nvirt0, d - 1
if (b == c) cycle b_aibjcjdibi
j_aibjcjdibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdibi: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcjdibi
i_aibjcjdibi: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjdibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjdibi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdibi
end do a_aibjcjdibi
end do j_aibjcjdibi
end do b_aibjcjdibi
end do c_aibjcjdibi
end do d_aibjcjdibi
!
! Elementary loop 39
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdibj: do d = nvirt0, nvirt1
c_aibjcjdibj: do c = d + 1, nvirt1
b_aibjcjdibj: do b = nvirt0, d - 1
if (b == c) cycle b_aibjcjdibj
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
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjdibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcjdibj
end do a_aibjcjdibj
end do j_aibjcjdibj
end do b_aibjcjdibj
end do c_aibjcjdibj
end do d_aibjcjdibj
!
! Elementary loop 40
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, l
! Equalities: e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibicidlbi: do d = nvirt0, nvirt1
l_aibicidlbi: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aibicidlbi: do c = d + 1, nvirt1
b_aibicidlbi: do b = nvirt0, d - 1
if (b == c) cycle b_aibicidlbi
a_aibicidlbi: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibicidlbi
i_aibicidlbi: do i = nocc0, nocc1
if (i == l) cycle i_aibicidlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibicidlbi(eorb, t2, t1, nocc, nactive, a, i, b, c, d, l)
end do i_aibicidlbi
end do a_aibicidlbi
end do b_aibicidlbi
end do c_aibicidlbi
end do l_aibicidlbi
end do d_aibicidlbi
!
! Elementary loop 41
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidjbi: do d = nvirt0, nvirt1
c_aibjcidjbi: do c = d + 1, nvirt1
b_aibjcidjbi: do b = nvirt0, d - 1
if (b == c) cycle b_aibjcidjbi
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
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcidjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcidjbi
end do a_aibjcidjbi
end do j_aibjcidjbi
end do b_aibjcidjbi
end do c_aibjcidjbi
end do d_aibjcidjbi
!
! Elementary loop 42
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcidjbj: do d = nvirt0, nvirt1
c_aibjcidjbj: do c = d + 1, nvirt1
b_aibjcidjbj: do b = nvirt0, d - 1
if (b == c) cycle b_aibjcidjbj
j_aibjcidjbj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidjbj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcidjbj
i_aibjcidjbj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcidjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcidjbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d)
end do i_aibjcidjbj
end do a_aibjcidjbj
end do j_aibjcidjbj
end do b_aibjcidjbj
end do c_aibjcidjbj
end do d_aibjcidjbj
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, k, m
! Equalities: b == a, c == a, d == a, j == i, l == i
! No equalities independent of the above can hold.
!
e_aiaiakaiem: do e = nvirt0, nvirt1
m_aiaiakaiem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aiaiakaiem: do k = nocc0, nocc1
if (k == m) cycle k_aiaiakaiem
a_aiaiakaiem: do a = e + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k, m)
i_aiaiakaiem: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaiakaiem(eorb, t2, t1, nocc, nactive, a, i, k, e, m)
end do i_aiaiakaiem
end do a_aiaiakaiem
end do k_aiaiakaiem
end do m_aiaiakaiem
end do e_aiaiakaiem
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, d == a, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiajakaiei: do e = nvirt0, nvirt1
k_aiajakaiei: do k = nocc0, nocc1
j_aiajakaiei: do j = nocc0, nocc1
if (j == k) cycle j_aiajakaiei
a_aiajakaiei: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajakaiei: do i = j + 1, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajakaiei(eorb, t2, t1, nocc, nactive, a, i, j, k, e)
end do i_aiajakaiei
end do a_aiajakaiei
end do j_aiajakaiei
end do k_aiajakaiei
end do e_aiajakaiei
!
! Elementary loop 45
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajakaiej: do e = nvirt0, nvirt1
k_aiajakaiej: do k = nocc0, nocc1
j_aiajakaiej: do j = nocc0, nocc1
if (j == k) cycle j_aiajakaiej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajakaiej: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k, j)
i_aiajakaiej: do i = j + 1, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajakaiej(eorb, t2, t1, nocc, nactive, a, i, j, k, e)
end do i_aiajakaiej
end do a_aiajakaiej
end do j_aiajakaiej
end do k_aiajakaiej
end do e_aiajakaiej
!
! Elementary loop 46
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, k, j
! Equalities: b == a, c == a, d == a, l == i, m == k
! No equalities independent of the above can hold.
!
e_aiajakaiek: do e = nvirt0, nvirt1
k_aiajakaiek: do k = nocc0, nocc1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j_aiajakaiek: do j = nocc0, nocc1
if (j == k) cycle j_aiajakaiek
a_aiajakaiek: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajakaiek: do i = j + 1, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajakaiek(eorb, t2, t1, nocc, nactive, a, i, j, k, e)
end do i_aiajakaiek
end do a_aiajakaiek
end do j_aiajakaiek
end do k_aiajakaiek
end do e_aiajakaiek
!
! Elementary loop 47
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, k, l
! Equalities: b == a, c == a, d == a, j == i, m == i
! No equalities independent of the above can hold.
!
e_aiaiakalei: do e = nvirt0, nvirt1
l_aiaiakalei: do l = nocc0, nocc1
k_aiaiakalei: do k = l + 1, nocc1
a_aiaiakalei: do a = e + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiaiakalei: do i = l + 1, nocc1
if (i == k) cycle i_aiaiakalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaiakalei(eorb, t2, t1, nocc, nactive, a, i, k, l, e)
end do i_aiaiakalei
end do a_aiaiakalei
end do k_aiaiakalei
end do l_aiaiakalei
end do e_aiaiakalei
!
! Elementary loop 48
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, l, m
! Equalities: b == a, c == a, d == a, j == i, k == i
! No equalities independent of the above can hold.
!
e_aiaiaialem: do e = nvirt0, nvirt1
m_aiaiaialem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l_aiaiaialem: do l = nocc0, m - 1
a_aiaiaialem: do a = e + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiaiaialem: do i = l + 1, nocc1
if (i == m) cycle i_aiaiaialem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaiaialem(eorb, t2, t1, nocc, nactive, a, i, l, e, m)
end do i_aiaiaialem
end do a_aiaiaialem
end do l_aiaiaialem
end do m_aiaiaialem
end do e_aiaiaialem
!
! Elementary loop 49
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i, m == i
! No equalities independent of the above can hold.
!
e_aiajaialei: do e = nvirt0, nvirt1
l_aiajaialei: do l = nocc0, nocc1
j_aiajaialei: do j = nocc0, nocc1
if (j == l) cycle j_aiajaialei
a_aiajaialei: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j, l)
i_aiajaialei: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaialei(eorb, t2, t1, nocc, nactive, a, i, j, l, e)
end do i_aiajaialei
end do a_aiajaialei
end do j_aiajaialei
end do l_aiajaialei
end do e_aiajaialei
!
! Elementary loop 50
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, d == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajakajei: do e = nvirt0, nvirt1
k_aiajakajei: do k = nocc0, nocc1
j_aiajakajei: do j = nocc0, k - 1
a_aiajakajei: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajakajei: do i = j + 1, nocc1
if (i == k) cycle i_aiajakajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajakajei(eorb, t2, t1, nocc, nactive, a, i, j, k, e)
end do i_aiajakajei
end do a_aiajakajei
end do j_aiajakajei
end do k_aiajakajei
end do e_aiajakajei
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
end subroutine ccjac_23_part4
end module ccjac_block_23_part4
