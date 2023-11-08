module ccjac_block_23_part6
use eom_cc3_23_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:35:25 UTC.
!
contains
 
subroutine ccjac_23_part6(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: a0, i0
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
! Free virtual indices: a, d
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, e == d, m == i, k == j
! No equalities independent of the above can hold.
!
d_aiajajdldi: do d = nvirt0, nvirt1
l_aiajajdldi: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajajdldi: do j = nocc0, l - 1
a_aiajajdldi: do a = d + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajajdldi: do i = j + 1, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajajdldi(eorb, t2, t1, nocc, nactive, a, i, j, d, l)
end do i_aiajajdldi
end do a_aiajajdldi
end do j_aiajajdldi
end do l_aiajajdldi
end do d_aiajajdldi
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, e == d, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajaidjdm: do m = nocc0, nocc1
d_aiajaidjdm: do d = nvirt0, nvirt1
em = (d - nvirt0) * nocc + (m - nocc0) + 1
j_aiajaidjdm: do j = m + 1, nocc1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajaidjdm: do a = d + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajaidjdm: do i = j + 1, j - 1
if (i == m) cycle i_aiajaidjdm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaidjdm(eorb, t2, t1, nocc, nactive, a, i, j, d, m)
end do i_aiajaidjdm
end do a_aiajaidjdm
end do j_aiajaidjdm
end do d_aiajaidjdm
end do m_aiajaidjdm
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, e == d, k == i, m == j
! No equalities independent of the above can hold.
!
d_aiajaidldj: do d = nvirt0, nvirt1
l_aiajaidldj: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajaidldj: do j = nocc0, l - 1
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajaidldj: do a = d + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajaidldj: do i = j + 1, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaidldj(eorb, t2, t1, nocc, nactive, a, i, j, d, l)
end do i_aiajaidldj
end do a_aiajaidldj
end do j_aiajaidldj
end do l_aiajaidldj
end do d_aiajaidldj
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, k
! Equalities: b == a, c == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiaiakdiei: do e = nvirt0, nvirt1
d_aiaiakdiei: do d = e + 1, nvirt1
k_aiaiakdiei: do k = nocc0, nocc1
a_aiaiakdiei: do a = d + 1, nvirt1
if (a == e) cycle a_aiaiakdiei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiaiakdiei: do i = nocc0, nocc1
if (i == k) cycle i_aiaiakdiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaiakdiei(eorb, t2, t1, nocc, nactive, a, i, k, d, e)
end do i_aiaiakdiei
end do a_aiaiakdiei
end do k_aiaiakdiei
end do d_aiaiakdiei
end do e_aiaiakdiei
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aiajajdiei: do e = nvirt0, nvirt1
d_aiajajdiei: do d = e + 1, nvirt1
j_aiajajdiei: do j = nocc0, nocc1
a_aiajajdiei: do a = d + 1, nvirt1
if (a == e) cycle a_aiajajdiei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajajdiei: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajajdiei(eorb, t2, t1, nocc, nactive, a, i, j, d, e)
end do i_aiajajdiei
end do a_aiajajdiei
end do j_aiajajdiei
end do d_aiajajdiei
end do e_aiajajdiei
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aiajajdiej: do e = nvirt0, nvirt1
d_aiajajdiej: do d = e + 1, nvirt1
j_aiajajdiej: do j = nocc0, nocc1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajajdiej: do a = d + 1, nvirt1
if (a == e) cycle a_aiajajdiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajajdiej: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajajdiej(eorb, t2, t1, nocc, nactive, a, i, j, d, e)
end do i_aiajajdiej
end do a_aiajajdiej
end do j_aiajajdiej
end do d_aiajajdiej
end do e_aiajajdiej
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, l
! Equalities: b == a, c == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aiaiaidlei: do e = nvirt0, nvirt1
d_aiaiaidlei: do d = e + 1, nvirt1
l_aiaiaidlei: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
a_aiaiaidlei: do a = d + 1, nvirt1
if (a == e) cycle a_aiaiaidlei
i_aiaiaidlei: do i = nocc0, nocc1
if (i == l) cycle i_aiaiaidlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaiaidlei(eorb, t2, t1, nocc, nactive, a, i, d, l, e)
end do i_aiaiaidlei
end do a_aiaiaidlei
end do l_aiaiaidlei
end do d_aiaiaidlei
end do e_aiaiaidlei
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajaidjei: do e = nvirt0, nvirt1
d_aiajaidjei: do d = e + 1, nvirt1
j_aiajaidjei: do j = nocc0, nocc1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajaidjei: do a = d + 1, nvirt1
if (a == e) cycle a_aiajaidjei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajaidjei: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaidjei(eorb, t2, t1, nocc, nactive, a, i, j, d, e)
end do i_aiajaidjei
end do a_aiajaidjei
end do j_aiajaidjei
end do d_aiajaidjei
end do e_aiajaidjei
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aiajaidjej: do e = nvirt0, nvirt1
d_aiajaidjej: do d = e + 1, nvirt1
j_aiajaidjej: do j = nocc0, nocc1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajaidjej: do a = d + 1, nvirt1
if (a == e) cycle a_aiajaidjej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajaidjej: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaidjej(eorb, t2, t1, nocc, nactive, a, i, j, d, e)
end do i_aiajaidjej
end do a_aiajaidjej
end do j_aiajaidjej
end do d_aiajaidjej
end do e_aiajaidjej
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: c == a, d == b, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibiakbibm: do m = nocc0, nocc1
k_aibiakbibm: do k = nocc0, nocc1
if (k == m) cycle k_aibiakbibm
b_aibiakbibm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a_aibiakbibm: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(m, k)
i_aibiakbibm: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakbibm(eorb, t2, t1, nocc, nactive, a, i, b, k, m)
end do i_aibiakbibm
end do a_aibiakbibm
end do b_aibiakbibm
end do k_aibiakbibm
end do m_aibiakbibm
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == b, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjaibibm: do m = nocc0, nocc1
b_aibjaibibm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaibibm: do j = nocc0, nocc1
if (j == m) cycle j_aibjaibibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaibibm: do a = b + 1, nvirt1
i_aibjaibibm: do i = m + 1, nocc1
if (i == j) cycle i_aibjaibibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaibibm(eorb, t2, t1, nocc, nactive, a, i, b, j, m)
end do i_aibjaibibm
end do a_aibjaibibm
end do j_aibjaibibm
end do b_aibjaibibm
end do m_aibjaibibm
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjakbibj: do k = nocc0, nocc1
b_aibjakbibj: do b = nvirt0, nvirt1
j_aibjakbibj: do j = nocc0, nocc1
if (j == k) cycle j_aibjakbibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakbibj: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j, k)
i_aibjakbibj: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakbibj(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjakbibj
end do a_aibjakbibj
end do j_aibjakbibj
end do b_aibjakbibj
end do k_aibjakbibj
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == b, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjajbibm: do m = nocc0, nocc1
b_aibjajbibm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjajbibm: do j = nocc0, nocc1
if (j == m) cycle j_aibjajbibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajbibm: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(m, j)
i_aibjajbibm: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajbibm(eorb, t2, t1, nocc, nactive, a, i, b, j, m)
end do i_aibjajbibm
end do a_aibjajbibm
end do j_aibjajbibm
end do b_aibjajbibm
end do m_aibjajbibm
!
! Elementary loop 14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, d == b, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjakbibk: do k = nocc0, nocc1
b_aibjakbibk: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjakbibk: do j = nocc0, nocc1
if (j == k) cycle j_aibjakbibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakbibk: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbibk: do i = k + 1, nocc1
if (i == j) cycle i_aibjakbibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakbibk(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjakbibk
end do a_aibjakbibk
end do j_aibjakbibk
end do b_aibjakbibk
end do k_aibjakbibk
!
! Elementary loop 15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == b, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibiakblbi: do l = nocc0, nocc1
k_aibiakblbi: do k = nocc0, l - 1
b_aibiakblbi: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibiakblbi: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakblbi: do i = nocc0, l - 1
if (i == k) cycle i_aibiakblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakblbi(eorb, t2, t1, nocc, nactive, a, i, b, k, l)
end do i_aibiakblbi
end do a_aibiakblbi
end do b_aibiakblbi
end do k_aibiakblbi
end do l_aibiakblbi
!
! Elementary loop 16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l, m
! Equalities: c == a, d == b, e == b, j == i, k == i
! No equalities independent of the above can hold.
!
m_aibiaiblbm: do m = nocc0, nocc1
l_aibiaiblbm: do l = m + 1, nocc1
b_aibiaiblbm: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a_aibiaiblbm: do a = b + 1, nvirt1
i_aibiaiblbm: do i = nocc0, l - 1
if (i == m) cycle i_aibiaiblbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaiblbm(eorb, t2, t1, nocc, nactive, a, i, b, l, m)
end do i_aibiaiblbm
end do a_aibiaiblbm
end do b_aibiaiblbm
end do l_aibiaiblbm
end do m_aibiaiblbm
!
! Elementary loop 17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjaiblbi: do l = nocc0, nocc1
b_aibjaiblbi: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblbi: do j = nocc0, nocc1
if (j == l) cycle j_aibjaiblbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaiblbi: do a = b + 1, nvirt1
i_aibjaiblbi: do i = nocc0, l - 1
if (i == j) cycle i_aibjaiblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaiblbi(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjaiblbi
end do a_aibjaiblbi
end do j_aibjaiblbi
end do b_aibjaiblbi
end do l_aibjaiblbi
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjakbjbi: do k = nocc0, nocc1
b_aibjakbjbi: do b = nvirt0, nvirt1
j_aibjakbjbi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakbjbi: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjbi: do i = nocc0, j - 1
if (i == k) cycle i_aibjakbjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakbjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjakbjbi
end do a_aibjakbjbi
end do j_aibjakbjbi
end do b_aibjakbjbi
end do k_aibjakbjbi
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, d == b, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjakbkbi: do k = nocc0, nocc1
b_aibjakbkbi: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjakbkbi: do j = nocc0, nocc1
if (j == k) cycle j_aibjakbkbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakbkbi: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbkbi: do i = nocc0, k - 1
if (i == j) cycle i_aibjakbkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakbkbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjakbkbi
end do a_aibjakbkbi
end do j_aibjakbkbi
end do b_aibjakbkbi
end do k_aibjakbkbi
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjajblbi: do l = nocc0, nocc1
b_aibjajblbi: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjajblbi: do j = nocc0, l - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajblbi: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajblbi: do i = nocc0, l - 1
if (i == j) cycle i_aibjajblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajblbi(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjajblbi
end do a_aibjajblbi
end do j_aibjajblbi
end do b_aibjajblbi
end do l_aibjajblbi
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == b, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaibjbm: do m = nocc0, nocc1
b_aibjaibjbm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaibjbm: do j = m + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaibjbm: do a = b + 1, nvirt1
i_aibjaibjbm: do i = nocc0, j - 1
if (i == m) cycle i_aibjaibjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaibjbm(eorb, t2, t1, nocc, nactive, a, i, b, j, m)
end do i_aibjaibjbm
end do a_aibjaibjbm
end do j_aibjaibjbm
end do b_aibjaibjbm
end do m_aibjaibjbm
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjaiblbj: do l = nocc0, nocc1
b_aibjaiblbj: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblbj: do j = nocc0, l - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaiblbj: do a = b + 1, nvirt1
i_aibjaiblbj: do i = nocc0, l - 1
if (i == j) cycle i_aibjaiblbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaiblbj(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjaiblbj
end do a_aibjaiblbj
end do j_aibjaiblbj
end do b_aibjaiblbj
end do l_aibjaiblbj
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, m
! Equalities: c == a, d == b, e == b, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjajbjbm: do m = nocc0, nocc1
b_aibjajbjbm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjajbjbm: do j = m + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajbjbm: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajbjbm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjajbjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajbjbm(eorb, t2, t1, nocc, nactive, a, i, b, j, m)
end do i_aibjajbjbm
end do a_aibjajbjbm
end do j_aibjajbjbm
end do b_aibjajbjbm
end do m_aibjajbjbm
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjajblbj: do l = nocc0, nocc1
b_aibjajblbj: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjajblbj: do j = nocc0, l - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajblbj: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajblbj: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjajblbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajblbj(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjajblbj
end do a_aibjajblbj
end do j_aibjajblbj
end do b_aibjajblbj
end do l_aibjajblbj
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibiakbiei: do e = nvirt0, nvirt1
k_aibiakbiei: do k = nocc0, nocc1
b_aibiakbiei: do b = e + 1, nvirt1
a_aibiakbiei: do a = b + 1, nvirt1
if (a == e) cycle a_aibiakbiei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakbiei: do i = nocc0, nocc1
if (i == k) cycle i_aibiakbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakbiei(eorb, t2, t1, nocc, nactive, a, i, b, k, e)
end do i_aibiakbiei
end do a_aibiakbiei
end do b_aibiakbiei
end do k_aibiakbiei
end do e_aibiakbiei
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibiakbiek: do e = nvirt0, nvirt1
k_aibiakbiek: do k = nocc0, nocc1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibiakbiek: do b = e + 1, nvirt1
a_aibiakbiek: do a = b + 1, nvirt1
if (a == e) cycle a_aibiakbiek
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakbiek: do i = nocc0, nocc1
if (i == k) cycle i_aibiakbiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakbiek(eorb, t2, t1, nocc, nactive, a, i, b, k, e)
end do i_aibiakbiek
end do a_aibiakbiek
end do b_aibiakbiek
end do k_aibiakbiek
end do e_aibiakbiek
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajbiei: do e = nvirt0, nvirt1
b_aibjajbiei: do b = e + 1, nvirt1
j_aibjajbiei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajbiei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjajbiei
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajbiei: do i = nocc0, nocc1
if (i == j) cycle i_aibjajbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajbiei(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjajbiei
end do a_aibjajbiei
end do j_aibjajbiei
end do b_aibjajbiei
end do e_aibjajbiei
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjajbiej: do e = nvirt0, nvirt1
b_aibjajbiej: do b = e + 1, nvirt1
j_aibjajbiej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajbiej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjajbiej
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajbiej: do i = nocc0, nocc1
if (i == j) cycle i_aibjajbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajbiej(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjajbiej
end do a_aibjajbiej
end do j_aibjajbiej
end do b_aibjajbiej
end do e_aibjajbiej
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l
! Equalities: c == a, d == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibiaiblei: do e = nvirt0, nvirt1
l_aibiaiblei: do l = nocc0, nocc1
b_aibiaiblei: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibiaiblei: do a = b + 1, nvirt1
if (a == e) cycle a_aibiaiblei
i_aibiaiblei: do i = nocc0, nocc1
if (i == l) cycle i_aibiaiblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaiblei(eorb, t2, t1, nocc, nactive, a, i, b, l, e)
end do i_aibiaiblei
end do a_aibiaiblei
end do b_aibiaiblei
end do l_aibiaiblei
end do e_aibiaiblei
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l
! Equalities: c == a, d == b, j == i, k == i, m == l
! No equalities independent of the above can hold.
!
e_aibiaiblel: do e = nvirt0, nvirt1
l_aibiaiblel: do l = nocc0, nocc1
em = (e - nvirt0) * nocc + (l - nocc0) + 1
b_aibiaiblel: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibiaiblel: do a = b + 1, nvirt1
if (a == e) cycle a_aibiaiblel
i_aibiaiblel: do i = nocc0, nocc1
if (i == l) cycle i_aibiaiblel
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaiblel(eorb, t2, t1, nocc, nactive, a, i, b, l, e)
end do i_aibiaiblel
end do a_aibiaiblel
end do b_aibiaiblel
end do l_aibiaiblel
end do e_aibiaiblel
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaibjei: do e = nvirt0, nvirt1
b_aibjaibjei: do b = e + 1, nvirt1
j_aibjaibjei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaibjei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjaibjei
i_aibjaibjei: do i = nocc0, nocc1
if (i == j) cycle i_aibjaibjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaibjei(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjaibjei
end do a_aibjaibjei
end do j_aibjaibjei
end do b_aibjaibjei
end do e_aibjaibjei
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjaibjej: do e = nvirt0, nvirt1
b_aibjaibjej: do b = e + 1, nvirt1
j_aibjaibjej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaibjej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjaibjej
i_aibjaibjej: do i = nocc0, nocc1
if (i == j) cycle i_aibjaibjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaibjej(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjaibjej
end do a_aibjaibjej
end do j_aibjaibjej
end do b_aibjaibjej
end do e_aibjaibjej
!
! Elementary loop 33
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, m
! Equalities: c == a, e == d, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibiaididm: do m = nocc0, nocc1
d_aibiaididm: do d = nvirt0, nvirt1
em = (d - nvirt0) * nocc + (m - nocc0) + 1
b_aibiaididm: do b = nvirt0, nvirt1
if (b == d) cycle b_aibiaididm
a0 = max(b, d)
a_aibiaididm: do a = a0 + 1, nvirt1
i_aibiaididm: do i = m + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaididm(eorb, t2, t1, nocc, nactive, a, i, b, d, m)
end do i_aibiaididm
end do a_aibiaididm
end do b_aibiaididm
end do d_aibiaididm
end do m_aibiaididm
!
! Elementary loop 34
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, j
! Equalities: c == a, e == d, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjajdidj: do d = nvirt0, nvirt1
b_aibjajdidj: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjajdidj
j_aibjajdidj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjajdidj: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdidj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajdidj(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjajdidj
end do a_aibjajdidj
end do j_aibjajdidj
end do b_aibjajdidj
end do d_aibjajdidj
!
! Elementary loop 35
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, l
! Equalities: c == a, e == d, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibiaidldi: do d = nvirt0, nvirt1
l_aibiaidldi: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibiaidldi: do b = nvirt0, nvirt1
if (b == d) cycle b_aibiaidldi
a0 = max(b, d)
a_aibiaidldi: do a = a0 + 1, nvirt1
i_aibiaidldi: do i = nocc0, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaidldi(eorb, t2, t1, nocc, nactive, a, i, b, d, l)
end do i_aibiaidldi
end do a_aibiaidldi
end do b_aibiaidldi
end do l_aibiaidldi
end do d_aibiaidldi
!
! Elementary loop 36
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, j
! Equalities: c == a, e == d, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjajdjdi: do d = nvirt0, nvirt1
b_aibjajdjdi: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjajdjdi
j_aibjajdjdi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjajdjdi: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdjdi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajdjdi(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjajdjdi
end do a_aibjajdjdi
end do j_aibjajdjdi
end do b_aibjajdjdi
end do d_aibjajdjdi
!
! Elementary loop 37
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibiakdibi: do d = nvirt0, nvirt1
k_aibiakdibi: do k = nocc0, nocc1
b_aibiakdibi: do b = nvirt0, d - 1
a0 = max(b, d)
a_aibiakdibi: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdibi: do i = nocc0, nocc1
if (i == k) cycle i_aibiakdibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakdibi(eorb, t2, t1, nocc, nactive, a, i, b, k, d)
end do i_aibiakdibi
end do a_aibiakdibi
end do b_aibiakdibi
end do k_aibiakdibi
end do d_aibiakdibi
!
! Elementary loop 38
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibiakdibk: do d = nvirt0, nvirt1
k_aibiakdibk: do k = nocc0, nocc1
b_aibiakdibk: do b = nvirt0, d - 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b, d)
a_aibiakdibk: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdibk: do i = nocc0, nocc1
if (i == k) cycle i_aibiakdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakdibk(eorb, t2, t1, nocc, nactive, a, i, b, k, d)
end do i_aibiakdibk
end do a_aibiakdibk
end do b_aibiakdibk
end do k_aibiakdibk
end do d_aibiakdibk
!
! Elementary loop 39
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdibi: do d = nvirt0, nvirt1
b_aibjajdibi: do b = nvirt0, d - 1
j_aibjajdibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjajdibi: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdibi: do i = nocc0, nocc1
if (i == j) cycle i_aibjajdibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajdibi(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjajdibi
end do a_aibjajdibi
end do j_aibjajdibi
end do b_aibjajdibi
end do d_aibjajdibi
!
! Elementary loop 40
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjajdibj: do d = nvirt0, nvirt1
b_aibjajdibj: do b = nvirt0, d - 1
j_aibjajdibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjajdibj: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdibj: do i = nocc0, nocc1
if (i == j) cycle i_aibjajdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajdibj(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjajdibj
end do a_aibjajdibj
end do j_aibjajdibj
end do b_aibjajdibj
end do d_aibjajdibj
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l
! Equalities: c == a, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibiaidlbi: do d = nvirt0, nvirt1
l_aibiaidlbi: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibiaidlbi: do b = nvirt0, d - 1
a0 = max(b, d)
a_aibiaidlbi: do a = a0 + 1, nvirt1
i_aibiaidlbi: do i = nocc0, nocc1
if (i == l) cycle i_aibiaidlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaidlbi(eorb, t2, t1, nocc, nactive, a, i, b, d, l)
end do i_aibiaidlbi
end do a_aibiaidlbi
end do b_aibiaidlbi
end do l_aibiaidlbi
end do d_aibiaidlbi
!
! Elementary loop 42
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l
! Equalities: c == a, e == b, j == i, k == i, m == l
! No equalities independent of the above can hold.
!
d_aibiaidlbl: do d = nvirt0, nvirt1
l_aibiaidlbl: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibiaidlbl: do b = nvirt0, d - 1
em = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b, d)
a_aibiaidlbl: do a = a0 + 1, nvirt1
i_aibiaidlbl: do i = nocc0, nocc1
if (i == l) cycle i_aibiaidlbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaidlbl(eorb, t2, t1, nocc, nactive, a, i, b, d, l)
end do i_aibiaidlbl
end do a_aibiaidlbl
end do b_aibiaidlbl
end do l_aibiaidlbl
end do d_aibiaidlbl
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidjbi: do d = nvirt0, nvirt1
b_aibjaidjbi: do b = nvirt0, d - 1
j_aibjaidjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjaidjbi: do a = a0 + 1, nvirt1
i_aibjaidjbi: do i = nocc0, nocc1
if (i == j) cycle i_aibjaidjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaidjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjaidjbi
end do a_aibjaidjbi
end do j_aibjaidjbi
end do b_aibjaidjbi
end do d_aibjaidjbi
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjaidjbj: do d = nvirt0, nvirt1
b_aibjaidjbj: do b = nvirt0, d - 1
j_aibjaidjbj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjaidjbj: do a = a0 + 1, nvirt1
i_aibjaidjbj: do i = nocc0, nocc1
if (i == j) cycle i_aibjaidjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaidjbj(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjaidjbj
end do a_aibjaidjbj
end do j_aibjaidjbj
end do b_aibjaidjbj
end do d_aibjaidjbj
!
! Elementary loop 45
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, m
! Equalities: c == b, e == d, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibibididm: do m = nocc0, nocc1
d_aibibididm: do d = nvirt0, nvirt1
em = (d - nvirt0) * nocc + (m - nocc0) + 1
b_aibibididm: do b = d + 1, nvirt1
a_aibibididm: do a = b + 1, nvirt1
if (a == d) cycle a_aibibididm
i_aibibididm: do i = m + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibibididm(eorb, t2, t1, nocc, nactive, a, i, b, d, m)
end do i_aibibididm
end do a_aibibididm
end do b_aibibididm
end do d_aibibididm
end do m_aibibididm
!
! Elementary loop 46
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, j
! Equalities: c == b, e == d, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbididj: do d = nvirt0, nvirt1
b_aibjbididj: do b = d + 1, nvirt1
j_aibjbididj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbididj: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbididj
i_aibjbididj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbididj(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjbididj
end do a_aibjbididj
end do j_aibjbididj
end do b_aibjbididj
end do d_aibjbididj
!
! Elementary loop 47
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, l
! Equalities: c == b, e == d, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibibidldi: do d = nvirt0, nvirt1
l_aibibidldi: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibibidldi: do b = d + 1, nvirt1
a_aibibidldi: do a = b + 1, nvirt1
if (a == d) cycle a_aibibidldi
i_aibibidldi: do i = nocc0, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibibidldi(eorb, t2, t1, nocc, nactive, a, i, b, d, l)
end do i_aibibidldi
end do a_aibibidldi
end do b_aibibidldi
end do l_aibibidldi
end do d_aibibidldi
!
! Elementary loop 48
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, j
! Equalities: c == b, e == d, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidjdi: do d = nvirt0, nvirt1
b_aibjbidjdi: do b = d + 1, nvirt1
j_aibjbidjdi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbidjdi: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbidjdi
i_aibjbidjdi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbidjdi(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjbidjdi
end do a_aibjbidjdi
end do j_aibjbidjdi
end do b_aibjbidjdi
end do d_aibjbidjdi
!
! Elementary loop 49
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, m
! Equalities: b == a, d == a, e == a, j == i, l == i
! No equalities independent of the above can hold.
!
m_aiaickaiam: do m = nocc0, nocc1
c_aiaickaiam: do c = nvirt0, nvirt1
k_aiaickaiam: do k = nocc0, nocc1
if (k == m) cycle k_aiaickaiam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a_aiaickaiam: do a = nvirt0, c - 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m, k)
i_aiaickaiam: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaickaiam(eorb, t2, t1, nocc, nactive, a, i, c, k, m)
end do i_aiaickaiam
end do a_aiaickaiam
end do k_aiaickaiam
end do c_aiaickaiam
end do m_aiaickaiam
!
! Elementary loop 50
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
a_aiajciaiam: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j, m)
i_aiajciaiam: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajciaiam(eorb, t2, t1, nocc, nactive, a, i, j, c, m)
end do i_aiajciaiam
end do a_aiajciaiam
end do j_aiajciaiam
end do c_aiajciaiam
end do m_aiajciaiam
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
end subroutine ccjac_23_part6
end module ccjac_block_23_part6
