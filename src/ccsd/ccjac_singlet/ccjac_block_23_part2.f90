module ccjac_block_23_part2
use eom_cc3_23_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:35:25 UTC.
!
contains
 
subroutine ccjac_23_part2(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: a0, i0, i1, j0
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
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajalei: do e = nvirt0, nvirt1
l_aibjajalei: do l = nocc0, nocc1
b_aibjajalei: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjajalei
j_aibjajalei: do j = l + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjajalei: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjajalei: do i = l + 1, nocc1
if (i == j) cycle i_aibjajalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajalei(eorb, t2, t1, nocc, nactive, a, i, b, j, l, e)
end do i_aibjajalei
end do a_aibjajalei
end do j_aibjajalei
end do b_aibjajalei
end do l_aibjajalei
end do e_aibjajalei
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaiajem: do e = nvirt0, nvirt1
m_aibjaiajem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjaiajem: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjaiajem
j_aibjaiajem: do j = nocc0, m - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjaiajem: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjaiajem: do i = j + 1, nocc1
if (i == m) cycle i_aibjaiajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaiajem(eorb, t2, t1, nocc, nactive, a, i, b, j, e, m)
end do i_aibjaiajem
end do a_aibjaiajem
end do j_aibjaiajem
end do b_aibjaiajem
end do m_aibjaiajem
end do e_aibjaiajem
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjaialej: do e = nvirt0, nvirt1
l_aibjaialej: do l = nocc0, nocc1
b_aibjaialej: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjaialej
j_aibjaialej: do j = l + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjaialej: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjaialej: do i = l + 1, nocc1
if (i == j) cycle i_aibjaialej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaialej(eorb, t2, t1, nocc, nactive, a, i, b, j, l, e)
end do i_aibjaialej
end do a_aibjaialej
end do j_aibjaialej
end do b_aibjaialej
end do l_aibjaialej
end do e_aibjaialej
!
! Elementary loop 4
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
j_aibjbkbiej: do j = nocc0, nocc1
if (j == k) cycle j_aibjbkbiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbiej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbkbiej
i1 = min(k, j)
i_aibjbkbiej: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbkbiej(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjbkbiej
end do a_aibjbkbiej
end do j_aibjbkbiej
end do b_aibjbkbiej
end do k_aibjbkbiej
end do e_aibjbkbiej
!
! Elementary loop 5
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, m
! Equalities: c == b, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjbiem: do e = nvirt0, nvirt1
m_aibjbjbiem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjbjbiem: do b = e + 1, nvirt1
j_aibjbjbiem: do j = nocc0, nocc1
if (j == m) cycle j_aibjbjbiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbjbiem: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbjbiem
i1 = min(j, m)
i_aibjbjbiem: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbjbiem(eorb, t2, t1, nocc, nactive, a, i, b, j, e, m)
end do i_aibjbjbiem
end do a_aibjbjbiem
end do j_aibjbjbiem
end do b_aibjbjbiem
end do m_aibjbjbiem
end do e_aibjbjbiem
!
! Elementary loop 6
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
j_aibjbkbjei: do j = nocc0, k - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbjei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbkbjei
i_aibjbkbjei: do i = j + 1, nocc1
if (i == k) cycle i_aibjbkbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbkbjei(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjbkbjei
end do a_aibjbkbjei
end do j_aibjbkbjei
end do b_aibjbkbjei
end do k_aibjbkbjei
end do e_aibjbkbjei
!
! Elementary loop 7
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjblei: do e = nvirt0, nvirt1
l_aibjbjblei: do l = nocc0, nocc1
b_aibjbjblei: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjbjblei: do j = l + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbjblei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbjblei
i_aibjbjblei: do i = l + 1, nocc1
if (i == j) cycle i_aibjbjblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbjblei(eorb, t2, t1, nocc, nactive, a, i, b, j, l, e)
end do i_aibjbjblei
end do a_aibjbjblei
end do j_aibjbjblei
end do b_aibjbjblei
end do l_aibjbjblei
end do e_aibjbjblei
!
! Elementary loop 8
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, m
! Equalities: c == b, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbibjem: do e = nvirt0, nvirt1
m_aibjbibjem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjbibjem: do b = e + 1, nvirt1
j_aibjbibjem: do j = nocc0, m - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbibjem: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbibjem
i_aibjbibjem: do i = j + 1, nocc1
if (i == m) cycle i_aibjbibjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbibjem(eorb, t2, t1, nocc, nactive, a, i, b, j, e, m)
end do i_aibjbibjem
end do a_aibjbibjem
end do j_aibjbibjem
end do b_aibjbibjem
end do m_aibjbibjem
end do e_aibjbibjem
!
! Elementary loop 9
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbiblej: do e = nvirt0, nvirt1
l_aibjbiblej: do l = nocc0, nocc1
b_aibjbiblej: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjbiblej: do j = l + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiblej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbiblej
i_aibjbiblej: do i = l + 1, nocc1
if (i == j) cycle i_aibjbiblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbiblej(eorb, t2, t1, nocc, nactive, a, i, b, j, l, e)
end do i_aibjbiblej
end do a_aibjbiblej
end do j_aibjbiblej
end do b_aibjbiblej
end do l_aibjbiblej
end do e_aibjbiblej
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, m
! Equalities: e == a, d == c, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjciam: do m = nocc0, nocc1
c_aibjcjciam: do c = nvirt0, nvirt1
b_aibjcjciam: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjciam
j_aibjcjciam: do j = nocc0, nocc1
if (j == m) cycle j_aibjcjciam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjciam: do a = b + 1, c - 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i1 = min(j, m)
i_aibjcjciam: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjciam(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcjciam
end do a_aibjcjciam
end do j_aibjcjciam
end do b_aibjcjciam
end do c_aibjcjciam
end do m_aibjcjciam
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: e == a, d == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckcjai: do c = nvirt0, nvirt1
k_aibjckcjai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckcjai: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjckcjai
j_aibjckcjai: do j = nocc0, k - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcjai: do a = b + 1, c - 1
i_aibjckcjai: do i = j + 1, nocc1
if (i == k) cycle i_aibjckcjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckcjai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end do i_aibjckcjai
end do a_aibjckcjai
end do j_aibjckcjai
end do b_aibjckcjai
end do k_aibjckcjai
end do c_aibjckcjai
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, l
! Equalities: e == a, d == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjclai: do l = nocc0, nocc1
c_aibjcjclai: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b_aibjcjclai: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjclai
j_aibjcjclai: do j = l + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjclai: do a = b + 1, c - 1
i_aibjcjclai: do i = l + 1, nocc1
if (i == j) cycle i_aibjcjclai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjclai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjclai
end do a_aibjcjclai
end do j_aibjcjclai
end do b_aibjcjclai
end do c_aibjcjclai
end do l_aibjcjclai
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, m
! Equalities: e == a, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcicjam: do m = nocc0, nocc1
c_aibjcicjam: do c = nvirt0, nvirt1
b_aibjcicjam: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcicjam
j_aibjcicjam: do j = nocc0, m - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicjam: do a = b + 1, c - 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjcicjam: do i = j + 1, nocc1
if (i == m) cycle i_aibjcicjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcicjam(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcicjam
end do a_aibjcicjam
end do j_aibjcicjam
end do b_aibjcicjam
end do c_aibjcicjam
end do m_aibjcicjam
!
! Elementary loop 14
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k
! Equalities: e == b, d == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckcibj: do c = nvirt0, nvirt1
k_aibjckcibj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckcibj: do b = nvirt0, c - 1
j_aibjckcibj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckcibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcibj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjckcibj
i1 = min(k, j)
i_aibjckcibj: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckcibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end do i_aibjckcibj
end do a_aibjckcibj
end do j_aibjckcibj
end do b_aibjckcibj
end do k_aibjckcibj
end do c_aibjckcibj
!
! Elementary loop 15
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, m
! Equalities: e == b, d == c, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjcibm: do m = nocc0, nocc1
c_aibjcjcibm: do c = nvirt0, nvirt1
b_aibjcjcibm: do b = nvirt0, c - 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjcibm: do j = nocc0, nocc1
if (j == m) cycle j_aibjcjcibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjcibm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjcibm
i1 = min(j, m)
i_aibjcjcibm: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjcibm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcjcibm
end do a_aibjcjcibm
end do j_aibjcjcibm
end do b_aibjcjcibm
end do c_aibjcjcibm
end do m_aibjcjcibm
!
! Elementary loop 16
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, m
! Equalities: e == b, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcicjbm: do m = nocc0, nocc1
c_aibjcicjbm: do c = nvirt0, nvirt1
b_aibjcicjbm: do b = nvirt0, c - 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcicjbm: do j = nocc0, m - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicjbm: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcicjbm
i_aibjcicjbm: do i = j + 1, nocc1
if (i == m) cycle i_aibjcicjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcicjbm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcicjbm
end do a_aibjcicjbm
end do j_aibjcicjbm
end do b_aibjcicjbm
end do c_aibjcicjbm
end do m_aibjcicjbm
!
! Elementary loop 17
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, l
! Equalities: e == b, d == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjciclbj: do l = nocc0, nocc1
c_aibjciclbj: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b_aibjciclbj: do b = nvirt0, c - 1
j_aibjciclbj: do j = l + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciclbj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciclbj
i_aibjciclbj: do i = l + 1, nocc1
if (i == j) cycle i_aibjciclbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjciclbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjciclbj
end do a_aibjciclbj
end do j_aibjciclbj
end do b_aibjciclbj
end do c_aibjciclbj
end do l_aibjciclbj
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajakdiej: do e = nvirt0, nvirt1
d_aiajakdiej: do d = e + 1, nvirt1
k_aiajakdiej: do k = nocc0, nocc1
j_aiajakdiej: do j = nocc0, nocc1
if (j == k) cycle j_aiajakdiej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajakdiej: do a = d + 1, nvirt1
if (a == e) cycle a_aiajakdiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajakdiej: do i = j + 1, nocc1
if (i == k) cycle i_aiajakdiej
if (k > i .and. i > j) cycle i_aiajakdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajakdiej(eorb, t2, t1, nocc, nactive, a, i, j, k, d, e)
end do i_aiajakdiej
end do a_aiajakdiej
end do j_aiajakdiej
end do k_aiajakdiej
end do d_aiajakdiej
end do e_aiajakdiej
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, l == i, k == j
! No equalities independent of the above can hold.
!
e_aiajajdiem: do e = nvirt0, nvirt1
m_aiajajdiem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d_aiajajdiem: do d = e + 1, nvirt1
j_aiajajdiem: do j = nocc0, nocc1
if (j == m) cycle j_aiajajdiem
a_aiajajdiem: do a = d + 1, nvirt1
if (a == e) cycle a_aiajajdiem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajajdiem: do i = j + 1, nocc1
if (i == m) cycle i_aiajajdiem
if (j > i .and. i > m) cycle i_aiajajdiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajajdiem(eorb, t2, t1, nocc, nactive, a, i, j, d, e, m)
end do i_aiajajdiem
end do a_aiajajdiem
end do j_aiajajdiem
end do d_aiajajdiem
end do m_aiajajdiem
end do e_aiajajdiem
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajakdjei: do e = nvirt0, nvirt1
d_aiajakdjei: do d = e + 1, nvirt1
k_aiajakdjei: do k = nocc0, nocc1
j_aiajakdjei: do j = nocc0, nocc1
if (j == k) cycle j_aiajakdjei
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajakdjei: do a = d + 1, nvirt1
if (a == e) cycle a_aiajakdjei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajakdjei: do i = j + 1, nocc1
if (i == k) cycle i_aiajakdjei
if (k > j .and. j > i) cycle i_aiajakdjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajakdjei(eorb, t2, t1, nocc, nactive, a, i, j, k, d, e)
end do i_aiajakdjei
end do a_aiajakdjei
end do j_aiajakdjei
end do k_aiajakdjei
end do d_aiajakdjei
end do e_aiajakdjei
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aiajajdlei: do e = nvirt0, nvirt1
d_aiajajdlei: do d = e + 1, nvirt1
l_aiajajdlei: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajajdlei: do j = nocc0, nocc1
if (j == l) cycle j_aiajajdlei
a_aiajajdlei: do a = d + 1, nvirt1
if (a == e) cycle a_aiajajdlei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajajdlei: do i = j + 1, nocc1
if (i == l) cycle i_aiajajdlei
if (j > l .and. l > i) cycle i_aiajajdlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajajdlei(eorb, t2, t1, nocc, nactive, a, i, j, d, l, e)
end do i_aiajajdlei
end do a_aiajajdlei
end do j_aiajajdlei
end do l_aiajajdlei
end do d_aiajajdlei
end do e_aiajajdlei
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aiajaidjem: do e = nvirt0, nvirt1
m_aiajaidjem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d_aiajaidjem: do d = e + 1, nvirt1
j_aiajaidjem: do j = nocc0, nocc1
if (j == m) cycle j_aiajaidjem
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajaidjem: do a = d + 1, nvirt1
if (a == e) cycle a_aiajaidjem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajaidjem: do i = j + 1, nocc1
if (i == m) cycle i_aiajaidjem
if (i > j .and. j > m) exit i_aiajaidjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaidjem(eorb, t2, t1, nocc, nactive, a, i, j, d, e, m)
end do i_aiajaidjem
end do a_aiajaidjem
end do j_aiajaidjem
end do d_aiajaidjem
end do m_aiajaidjem
end do e_aiajaidjem
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aiajaidlej: do e = nvirt0, nvirt1
d_aiajaidlej: do d = e + 1, nvirt1
l_aiajaidlej: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajaidlej: do j = nocc0, nocc1
if (j == l) cycle j_aiajaidlej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajaidlej: do a = d + 1, nvirt1
if (a == e) cycle a_aiajaidlej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajaidlej: do i = j + 1, nocc1
if (i == l) cycle i_aiajaidlej
if (i > l .and. l > j) exit i_aiajaidlej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaidlej(eorb, t2, t1, nocc, nactive, a, i, j, d, l, e)
end do i_aiajaidlej
end do a_aiajaidlej
end do j_aiajaidlej
end do l_aiajaidlej
end do d_aiajaidlej
end do e_aiajaidlej
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, m
! Equalities: c == a, d == b, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjakbibm: do m = nocc0, nocc1
k_aibjakbibm: do k = nocc0, nocc1
if (k == m) cycle k_aibjakbibm
b_aibjakbibm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjakbibm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjakbibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakbibm: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(m, k)
i_aibjakbibm: do i = i0 + 1, nocc1
if (i == j) cycle i_aibjakbibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakbibm(eorb, t2, t1, nocc, nactive, a, i, b, j, k, m)
end do i_aibjakbibm
end do a_aibjakbibm
end do j_aibjakbibm
end do b_aibjakbibm
end do k_aibjakbibm
end do m_aibjakbibm
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == b, e == b, m == i
! No equalities independent of the above can hold.
!
l_aibjakblbi: do l = nocc0, nocc1
k_aibjakblbi: do k = nocc0, l - 1
b_aibjakblbi: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjakblbi: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjakblbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakblbi: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakblbi: do i = nocc0, l - 1
if (i == j .or. i == k) cycle i_aibjakblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakblbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k, l)
end do i_aibjakblbi
end do a_aibjakblbi
end do j_aibjakblbi
end do b_aibjakblbi
end do k_aibjakblbi
end do l_aibjakblbi
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l, m
! Equalities: c == a, d == b, e == b, k == i
! No equalities independent of the above can hold.
!
m_aibjaiblbm: do m = nocc0, nocc1
l_aibjaiblbm: do l = m + 1, nocc1
b_aibjaiblbm: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaiblbm: do j = nocc0, nocc1
if (j == l .or. j == m) cycle j_aibjaiblbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaiblbm: do a = b + 1, nvirt1
i_aibjaiblbm: do i = nocc0, l - 1
if (i == j .or. i == m) cycle i_aibjaiblbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaiblbm(eorb, t2, t1, nocc, nactive, a, i, b, j, l, m)
end do i_aibjaiblbm
end do a_aibjaiblbm
end do j_aibjaiblbm
end do b_aibjaiblbm
end do l_aibjaiblbm
end do m_aibjaiblbm
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, m
! Equalities: c == a, d == b, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjakbjbm: do m = nocc0, nocc1
k_aibjakbjbm: do k = nocc0, nocc1
if (k == m) cycle k_aibjakbjbm
b_aibjakbjbm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m, k)
j_aibjakbjbm: do j = j0 + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakbjbm: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjbm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjakbjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakbjbm(eorb, t2, t1, nocc, nactive, a, i, b, j, k, m)
end do i_aibjakbjbm
end do a_aibjakbjbm
end do j_aibjakbjbm
end do b_aibjakbjbm
end do k_aibjakbjbm
end do m_aibjakbjbm
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, l
! Equalities: c == a, d == b, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjakblbj: do l = nocc0, nocc1
k_aibjakblbj: do k = nocc0, l - 1
b_aibjakblbj: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjakblbj: do j = nocc0, l - 1
if (j == k) cycle j_aibjakblbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakblbj: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakblbj: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjakblbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakblbj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, l)
end do i_aibjakblbj
end do a_aibjakblbj
end do j_aibjakblbj
end do b_aibjakblbj
end do k_aibjakblbj
end do l_aibjakblbj
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k, m
! Equalities: c == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
e_aibiakbiem: do e = nvirt0, nvirt1
m_aibiakbiem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aibiakbiem: do k = nocc0, nocc1
if (k == m) cycle k_aibiakbiem
b_aibiakbiem: do b = e + 1, nvirt1
a_aibiakbiem: do a = b + 1, nvirt1
if (a == e) cycle a_aibiakbiem
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakbiem: do i = nocc0, nocc1
if (i == k .or. i == m) cycle i_aibiakbiem
if (k > i .and. i > m) cycle i_aibiakbiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakbiem(eorb, t2, t1, nocc, nactive, a, i, b, k, e, m)
end do i_aibiakbiem
end do a_aibiakbiem
end do b_aibiakbiem
end do k_aibiakbiem
end do m_aibiakbiem
end do e_aibiakbiem
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjakbiei: do e = nvirt0, nvirt1
k_aibjakbiei: do k = nocc0, nocc1
b_aibjakbiei: do b = e + 1, nvirt1
j_aibjakbiei: do j = nocc0, nocc1
if (j == k) cycle j_aibjakbiei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakbiei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjakbiei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbiei: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakbiei(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjakbiei
end do a_aibjakbiei
end do j_aibjakbiei
end do b_aibjakbiei
end do k_aibjakbiei
end do e_aibjakbiei
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjakbiej: do e = nvirt0, nvirt1
k_aibjakbiej: do k = nocc0, nocc1
b_aibjakbiej: do b = e + 1, nvirt1
j_aibjakbiej: do j = nocc0, nocc1
if (j == k) cycle j_aibjakbiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakbiej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjakbiej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbiej: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakbiej
if (k > i .and. i > j) cycle i_aibjakbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakbiej(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjakbiej
end do a_aibjakbiej
end do j_aibjakbiej
end do b_aibjakbiej
end do k_aibjakbiej
end do e_aibjakbiej
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k, l
! Equalities: c == a, d == b, j == i, m == i
! No equalities independent of the above can hold.
!
e_aibiakblei: do e = nvirt0, nvirt1
l_aibiakblei: do l = nocc0, nocc1
k_aibiakblei: do k = nocc0, nocc1
if (k == l) cycle k_aibiakblei
b_aibiakblei: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibiakblei: do a = b + 1, nvirt1
if (a == e) cycle a_aibiakblei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakblei: do i = nocc0, nocc1
if (i == k .or. i == l) cycle i_aibiakblei
if (k > l .and. l > i) cycle i_aibiakblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakblei(eorb, t2, t1, nocc, nactive, a, i, b, k, l, e)
end do i_aibiakblei
end do a_aibiakblei
end do b_aibiakblei
end do k_aibiakblei
end do l_aibiakblei
end do e_aibiakblei
!
! Elementary loop 33
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l, m
! Equalities: c == a, d == b, j == i, k == i
! No equalities independent of the above can hold.
!
e_aibiaiblem: do e = nvirt0, nvirt1
m_aibiaiblem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l_aibiaiblem: do l = nocc0, nocc1
if (l == m) cycle l_aibiaiblem
b_aibiaiblem: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibiaiblem: do a = b + 1, nvirt1
if (a == e) cycle a_aibiaiblem
i_aibiaiblem: do i = nocc0, nocc1
if (i == l .or. i == m) cycle i_aibiaiblem
if (i > l .and. l > m) exit i_aibiaiblem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaiblem(eorb, t2, t1, nocc, nactive, a, i, b, l, e, m)
end do i_aibiaiblem
end do a_aibiaiblem
end do b_aibiaiblem
end do l_aibiaiblem
end do m_aibiaiblem
end do e_aibiaiblem
!
! Elementary loop 34
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibjaiblei: do e = nvirt0, nvirt1
l_aibjaiblei: do l = nocc0, nocc1
b_aibjaiblei: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblei: do j = nocc0, nocc1
if (j == l) cycle j_aibjaiblei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaiblei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjaiblei
i_aibjaiblei: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjaiblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaiblei(eorb, t2, t1, nocc, nactive, a, i, b, j, l, e)
end do i_aibjaiblei
end do a_aibjaiblei
end do j_aibjaiblei
end do b_aibjaiblei
end do l_aibjaiblei
end do e_aibjaiblei
!
! Elementary loop 35
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjakbjei: do e = nvirt0, nvirt1
k_aibjakbjei: do k = nocc0, nocc1
b_aibjakbjei: do b = e + 1, nvirt1
j_aibjakbjei: do j = nocc0, nocc1
if (j == k) cycle j_aibjakbjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakbjei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjakbjei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjei: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakbjei
if (k > j .and. j > i) cycle i_aibjakbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakbjei(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjakbjei
end do a_aibjakbjei
end do j_aibjakbjei
end do b_aibjakbjei
end do k_aibjakbjei
end do e_aibjakbjei
!
! Elementary loop 36
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajblei: do e = nvirt0, nvirt1
l_aibjajblei: do l = nocc0, nocc1
b_aibjajblei: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjajblei: do j = nocc0, nocc1
if (j == l) cycle j_aibjajblei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajblei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjajblei
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajblei: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjajblei
if (j > l .and. l > i) cycle i_aibjajblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajblei(eorb, t2, t1, nocc, nactive, a, i, b, j, l, e)
end do i_aibjajblei
end do a_aibjajblei
end do j_aibjajblei
end do b_aibjajblei
end do l_aibjajblei
end do e_aibjajblei
!
! Elementary loop 37
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaibjem: do e = nvirt0, nvirt1
m_aibjaibjem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjaibjem: do b = e + 1, nvirt1
j_aibjaibjem: do j = nocc0, nocc1
if (j == m) cycle j_aibjaibjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaibjem: do a = b + 1, nvirt1
if (a == e) cycle a_aibjaibjem
i_aibjaibjem: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjaibjem
if (i > j .and. j > m) exit i_aibjaibjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaibjem(eorb, t2, t1, nocc, nactive, a, i, b, j, e, m)
end do i_aibjaibjem
end do a_aibjaibjem
end do j_aibjaibjem
end do b_aibjaibjem
end do m_aibjaibjem
end do e_aibjaibjem
!
! Elementary loop 38
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l, j
! Equalities: c == a, d == b, k == i, m == l
! No equalities independent of the above can hold.
!
e_aibjaiblel: do e = nvirt0, nvirt1
l_aibjaiblel: do l = nocc0, nocc1
em = (e - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaiblel: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblel: do j = nocc0, nocc1
if (j == l) cycle j_aibjaiblel
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaiblel: do a = b + 1, nvirt1
if (a == e) cycle a_aibjaiblel
i_aibjaiblel: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjaiblel
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaiblel(eorb, t2, t1, nocc, nactive, a, i, b, j, l, e)
end do i_aibjaiblel
end do a_aibjaiblel
end do j_aibjaiblel
end do b_aibjaiblel
end do l_aibjaiblel
end do e_aibjaiblel
!
! Elementary loop 39
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjaiblej: do e = nvirt0, nvirt1
l_aibjaiblej: do l = nocc0, nocc1
b_aibjaiblej: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblej: do j = nocc0, nocc1
if (j == l) cycle j_aibjaiblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaiblej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjaiblej
i_aibjaiblej: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjaiblej
if (i > l .and. l > j) exit i_aibjaiblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaiblej(eorb, t2, t1, nocc, nactive, a, i, b, j, l, e)
end do i_aibjaiblej
end do a_aibjaiblej
end do j_aibjaiblej
end do b_aibjaiblej
end do l_aibjaiblej
end do e_aibjaiblej
!
! Elementary loop 40
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjakbjej: do e = nvirt0, nvirt1
k_aibjakbjej: do k = nocc0, nocc1
b_aibjakbjej: do b = e + 1, nvirt1
j_aibjakbjej: do j = nocc0, nocc1
if (j == k) cycle j_aibjakbjej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakbjej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjakbjej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjej: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakbjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakbjej(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjakbjej
end do a_aibjakbjej
end do j_aibjakbjej
end do b_aibjakbjej
end do k_aibjakbjej
end do e_aibjakbjej
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, k, i
! Equalities: c == a, d == b, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjakbjek: do e = nvirt0, nvirt1
k_aibjakbjek: do k = nocc0, nocc1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjakbjek: do b = e + 1, nvirt1
j_aibjakbjek: do j = nocc0, nocc1
if (j == k) cycle j_aibjakbjek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakbjek: do a = b + 1, nvirt1
if (a == e) cycle a_aibjakbjek
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjek: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakbjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakbjek(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjakbjek
end do a_aibjakbjek
end do j_aibjakbjek
end do b_aibjakbjek
end do k_aibjakbjek
end do e_aibjakbjek
!
! Elementary loop 42
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjajblej: do e = nvirt0, nvirt1
l_aibjajblej: do l = nocc0, nocc1
b_aibjajblej: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjajblej: do j = nocc0, nocc1
if (j == l) cycle j_aibjajblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajblej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjajblej
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajblej: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjajblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajblej(eorb, t2, t1, nocc, nactive, a, i, b, j, l, e)
end do i_aibjajblej
end do a_aibjajblej
end do j_aibjajblej
end do b_aibjajblej
end do l_aibjajblej
end do e_aibjajblej
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, j, k
! Equalities: c == a, e == d, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjakdidj: do d = nvirt0, nvirt1
k_aibjakdidj: do k = nocc0, nocc1
b_aibjakdidj: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjakdidj
j_aibjakdidj: do j = nocc0, nocc1
if (j == k) cycle j_aibjakdidj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjakdidj: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j, k)
i_aibjakdidj: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakdidj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjakdidj
end do a_aibjakdidj
end do j_aibjakdidj
end do b_aibjakdidj
end do k_aibjakdidj
end do d_aibjakdidj
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, j, k
! Equalities: c == a, e == d, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjakdjdi: do d = nvirt0, nvirt1
k_aibjakdjdi: do k = nocc0, nocc1
b_aibjakdjdi: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjakdjdi
j_aibjakdjdi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjakdjdi: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjdi: do i = nocc0, j - 1
if (i == k) cycle i_aibjakdjdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakdjdi(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjakdjdi
end do a_aibjakdjdi
end do j_aibjakdjdi
end do b_aibjakdjdi
end do k_aibjakdjdi
end do d_aibjakdjdi
!
! Elementary loop 45
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, j, m
! Equalities: c == a, e == d, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaidjdm: do m = nocc0, nocc1
d_aibjaidjdm: do d = nvirt0, nvirt1
em = (d - nvirt0) * nocc + (m - nocc0) + 1
b_aibjaidjdm: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjaidjdm
j_aibjaidjdm: do j = m + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjaidjdm: do a = a0 + 1, nvirt1
i_aibjaidjdm: do i = nocc0, j - 1
if (i == m) cycle i_aibjaidjdm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaidjdm(eorb, t2, t1, nocc, nactive, a, i, b, j, d, m)
end do i_aibjaidjdm
end do a_aibjaidjdm
end do j_aibjaidjdm
end do b_aibjaidjdm
end do d_aibjaidjdm
end do m_aibjaidjdm
!
! Elementary loop 46
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, j, l
! Equalities: c == a, e == d, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjaidldj: do d = nvirt0, nvirt1
l_aibjaidldj: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaidldj: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjaidldj
j_aibjaidldj: do j = nocc0, l - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjaidldj: do a = a0 + 1, nvirt1
i_aibjaidldj: do i = nocc0, l - 1
if (i == j) cycle i_aibjaidldj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaidldj(eorb, t2, t1, nocc, nactive, a, i, b, j, d, l)
end do i_aibjaidldj
end do a_aibjaidldj
end do j_aibjaidldj
end do b_aibjaidldj
end do l_aibjaidldj
end do d_aibjaidldj
!
! Elementary loop 47
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, m
! Equalities: c == a, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibiakdibm: do m = nocc0, nocc1
d_aibiakdibm: do d = nvirt0, nvirt1
k_aibiakdibm: do k = nocc0, nocc1
if (k == m) cycle k_aibiakdibm
b_aibiakdibm: do b = nvirt0, d - 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b, d)
a_aibiakdibm: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdibm: do i = nocc0, nocc1
if (i == k .or. i == m) cycle i_aibiakdibm
if (k > i .and. i > m) cycle i_aibiakdibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakdibm(eorb, t2, t1, nocc, nactive, a, i, b, k, d, m)
end do i_aibiakdibm
end do a_aibiakdibm
end do b_aibiakdibm
end do k_aibiakdibm
end do d_aibiakdibm
end do m_aibiakdibm
!
! Elementary loop 48
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, e == b, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjakdibi: do d = nvirt0, nvirt1
k_aibjakdibi: do k = nocc0, nocc1
b_aibjakdibi: do b = nvirt0, d - 1
j_aibjakdibi: do j = nocc0, nocc1
if (j == k) cycle j_aibjakdibi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjakdibi: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdibi: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakdibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakdibi(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjakdibi
end do a_aibjakdibi
end do j_aibjakdibi
end do b_aibjakdibi
end do k_aibjakdibi
end do d_aibjakdibi
!
! Elementary loop 49
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjakdibj: do d = nvirt0, nvirt1
k_aibjakdibj: do k = nocc0, nocc1
b_aibjakdibj: do b = nvirt0, d - 1
j_aibjakdibj: do j = nocc0, nocc1
if (j == k) cycle j_aibjakdibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjakdibj: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdibj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakdibj
if (k > i .and. i > j) cycle i_aibjakdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakdibj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjakdibj
end do a_aibjakdibj
end do j_aibjakdibj
end do b_aibjakdibj
end do k_aibjakdibj
end do d_aibjakdibj
!
! Elementary loop 50
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, m
! Equalities: c == a, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjajdibm: do m = nocc0, nocc1
d_aibjajdibm: do d = nvirt0, nvirt1
b_aibjajdibm: do b = nvirt0, d - 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjajdibm: do j = nocc0, nocc1
if (j == m) cycle j_aibjajdibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjajdibm: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdibm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjajdibm
if (j > i .and. i > m) cycle i_aibjajdibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajdibm(eorb, t2, t1, nocc, nactive, a, i, b, j, d, m)
end do i_aibjajdibm
end do a_aibjajdibm
end do j_aibjajdibm
end do b_aibjajdibm
end do d_aibjajdibm
end do m_aibjajdibm
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
end subroutine ccjac_23_part2
end module ccjac_block_23_part2
