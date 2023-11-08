module ccjac_block_23_part5
use eom_cc3_23_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:35:25 UTC.
!
contains
 
subroutine ccjac_23_part5(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
! Free virtual indices: a, e
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aiajajalei: do e = nvirt0, nvirt1
l_aiajajalei: do l = nocc0, nocc1
j_aiajajalei: do j = l + 1, nocc1
a_aiajajalei: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j, l)
i_aiajajalei: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajajalei(eorb, t2, t1, nocc, nactive, a, i, j, l, e)
end do i_aiajajalei
end do a_aiajajalei
end do j_aiajajalei
end do l_aiajajalei
end do e_aiajajalei
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aiajaiajem: do e = nvirt0, nvirt1
m_aiajaiajem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
j_aiajaiajem: do j = nocc0, m - 1
a_aiajaiajem: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajaiajem: do i = j + 1, nocc1
if (i == m) cycle i_aiajaiajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaiajem(eorb, t2, t1, nocc, nactive, a, i, j, e, m)
end do i_aiajaiajem
end do a_aiajaiajem
end do j_aiajaiajem
end do m_aiajaiajem
end do e_aiajaiajem
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, l, j
! Equalities: b == a, c == a, d == a, k == i, m == l
! No equalities independent of the above can hold.
!
e_aiajaialel: do e = nvirt0, nvirt1
l_aiajaialel: do l = nocc0, nocc1
em = (e - nvirt0) * nocc + (l - nocc0) + 1
j_aiajaialel: do j = nocc0, nocc1
if (j == l) cycle j_aiajaialel
a_aiajaialel: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j, l)
i_aiajaialel: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaialel(eorb, t2, t1, nocc, nactive, a, i, j, l, e)
end do i_aiajaialel
end do a_aiajaialel
end do j_aiajaialel
end do l_aiajaialel
end do e_aiajaialel
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aiajaialej: do e = nvirt0, nvirt1
l_aiajaialej: do l = nocc0, nocc1
j_aiajaialej: do j = l + 1, nocc1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajaialej: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j, l)
i_aiajaialej: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaialej(eorb, t2, t1, nocc, nactive, a, i, j, l, e)
end do i_aiajaialej
end do a_aiajaialej
end do j_aiajaialej
end do l_aiajaialej
end do e_aiajaialej
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, k
! Equalities: b == a, c == a, d == a, l == j, m == j
! No equalities independent of the above can hold.
!
e_aiajakajej: do e = nvirt0, nvirt1
k_aiajakajej: do k = nocc0, nocc1
j_aiajakajej: do j = nocc0, k - 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajakajej: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajakajej: do i = j + 1, nocc1
if (i == k) cycle i_aiajakajej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajakajej(eorb, t2, t1, nocc, nactive, a, i, j, k, e)
end do i_aiajakajej
end do a_aiajakajej
end do j_aiajakajej
end do k_aiajakajej
end do e_aiajakajej
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, k, i
! Equalities: b == a, c == a, d == a, l == j, m == k
! No equalities independent of the above can hold.
!
e_aiajakajek: do e = nvirt0, nvirt1
k_aiajakajek: do k = nocc0, nocc1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j_aiajakajek: do j = nocc0, k - 1
a_aiajakajek: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajakajek: do i = j + 1, nocc1
if (i == k) cycle i_aiajakajek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajakajek(eorb, t2, t1, nocc, nactive, a, i, j, k, e)
end do i_aiajakajek
end do a_aiajakajek
end do j_aiajakajek
end do k_aiajakajek
end do e_aiajakajek
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, l, i
! Equalities: b == a, c == a, d == a, k == j, m == l
! No equalities independent of the above can hold.
!
e_aiajajalel: do e = nvirt0, nvirt1
l_aiajajalel: do l = nocc0, nocc1
em = (e - nvirt0) * nocc + (l - nocc0) + 1
j_aiajajalel: do j = l + 1, nocc1
a_aiajajalel: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajajalel: do i = j + 1, nocc1
if (i == l) cycle i_aiajajalel
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajajalel(eorb, t2, t1, nocc, nactive, a, i, j, l, e)
end do i_aiajajalel
end do a_aiajajalel
end do j_aiajajalel
end do l_aiajajalel
end do e_aiajajalel
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, l
! Equalities: b == a, c == a, d == a, k == j, m == j
! No equalities independent of the above can hold.
!
e_aiajajalej: do e = nvirt0, nvirt1
l_aiajajalej: do l = nocc0, nocc1
j_aiajajalej: do j = l + 1, nocc1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajajalej: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajajalej: do i = j + 1, nocc1
if (i == l) cycle i_aiajajalej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajajalej(eorb, t2, t1, nocc, nactive, a, i, j, l, e)
end do i_aiajajalej
end do a_aiajajalej
end do j_aiajajalej
end do l_aiajajalej
end do e_aiajajalej
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: c == a, d == a, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibiakaibm: do m = nocc0, nocc1
k_aibiakaibm: do k = nocc0, nocc1
if (k == m) cycle k_aibiakaibm
b_aibiakaibm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a_aibiakaibm: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k, m)
i_aibiakaibm: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakaibm(eorb, t2, t1, nocc, nactive, a, i, b, k, m)
end do i_aibiakaibm
end do a_aibiakaibm
end do b_aibiakaibm
end do k_aibiakaibm
end do m_aibiakaibm
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, e == b, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibjakaibi: do k = nocc0, nocc1
b_aibjakaibi: do b = nvirt0, nvirt1
j_aibjakaibi: do j = nocc0, nocc1
if (j == k) cycle j_aibjakaibi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakaibi: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakaibi: do i = nocc0, k - 1
if (i == j) cycle i_aibjakaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakaibi(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjakaibi
end do a_aibjakaibi
end do j_aibjakaibi
end do b_aibjakaibi
end do k_aibjakaibi
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjakaibj: do k = nocc0, nocc1
b_aibjakaibj: do b = nvirt0, nvirt1
j_aibjakaibj: do j = nocc0, nocc1
if (j == k) cycle j_aibjakaibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakaibj: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k, j)
i_aibjakaibj: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakaibj(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjakaibj
end do a_aibjakaibj
end do j_aibjakaibj
end do b_aibjakaibj
end do k_aibjakaibj
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjajaibm: do m = nocc0, nocc1
b_aibjajaibm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjajaibm: do j = nocc0, nocc1
if (j == m) cycle j_aibjajaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajaibm: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j, m)
i_aibjajaibm: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajaibm(eorb, t2, t1, nocc, nactive, a, i, b, j, m)
end do i_aibjajaibm
end do a_aibjajaibm
end do j_aibjajaibm
end do b_aibjajaibm
end do m_aibjajaibm
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == a, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibiakalbi: do l = nocc0, nocc1
k_aibiakalbi: do k = l + 1, nocc1
b_aibiakalbi: do b = nvirt0, nvirt1
a_aibiakalbi: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibiakalbi: do i = l + 1, nocc1
if (i == k) cycle i_aibiakalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakalbi(eorb, t2, t1, nocc, nactive, a, i, b, k, l)
end do i_aibiakalbi
end do a_aibiakalbi
end do b_aibiakalbi
end do k_aibiakalbi
end do l_aibiakalbi
!
! Elementary loop 14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l, m
! Equalities: c == a, d == a, e == b, j == i, k == i
! No equalities independent of the above can hold.
!
m_aibiaialbm: do m = nocc0, nocc1
l_aibiaialbm: do l = nocc0, m - 1
b_aibiaialbm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a_aibiaialbm: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibiaialbm: do i = l + 1, nocc1
if (i == m) cycle i_aibiaialbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaialbm(eorb, t2, t1, nocc, nactive, a, i, b, l, m)
end do i_aibiaialbm
end do a_aibiaialbm
end do b_aibiaialbm
end do l_aibiaialbm
end do m_aibiaialbm
!
! Elementary loop 15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjaialbi: do l = nocc0, nocc1
b_aibjaialbi: do b = nvirt0, nvirt1
j_aibjaialbi: do j = nocc0, nocc1
if (j == l) cycle j_aibjaialbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaialbi: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjaialbi: do i = l + 1, nocc1
if (i == j) cycle i_aibjaialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaialbi(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjaialbi
end do a_aibjaialbi
end do j_aibjaialbi
end do b_aibjaialbi
end do l_aibjaialbi
!
! Elementary loop 16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjakajbi: do k = nocc0, nocc1
b_aibjakajbi: do b = nvirt0, nvirt1
j_aibjakajbi: do j = nocc0, k - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakajbi: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakajbi: do i = j + 1, nocc1
if (i == k) cycle i_aibjakajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakajbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjakajbi
end do a_aibjakajbi
end do j_aibjakajbi
end do b_aibjakajbi
end do k_aibjakajbi
!
! Elementary loop 17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjajalbi: do l = nocc0, nocc1
b_aibjajalbi: do b = nvirt0, nvirt1
j_aibjajalbi: do j = l + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajalbi: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjajalbi: do i = l + 1, nocc1
if (i == j) cycle i_aibjajalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajalbi(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjajalbi
end do a_aibjajalbi
end do j_aibjajalbi
end do b_aibjajalbi
end do l_aibjajalbi
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaiajbm: do m = nocc0, nocc1
b_aibjaiajbm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaiajbm: do j = nocc0, m - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaiajbm: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjaiajbm: do i = j + 1, nocc1
if (i == m) cycle i_aibjaiajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaiajbm(eorb, t2, t1, nocc, nactive, a, i, b, j, m)
end do i_aibjaiajbm
end do a_aibjaiajbm
end do j_aibjaiajbm
end do b_aibjaiajbm
end do m_aibjaiajbm
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjaialbj: do l = nocc0, nocc1
b_aibjaialbj: do b = nvirt0, nvirt1
j_aibjaialbj: do j = l + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaialbj: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjaialbj: do i = l + 1, nocc1
if (i == j) cycle i_aibjaialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaialbj(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjaialbj
end do a_aibjaialbj
end do j_aibjaialbj
end do b_aibjaialbj
end do l_aibjaialbj
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == a, e == b, l == j, m == j
! No equalities independent of the above can hold.
!
k_aibjakajbj: do k = nocc0, nocc1
b_aibjakajbj: do b = nvirt0, nvirt1
j_aibjakajbj: do j = nocc0, k - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakajbj: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakajbj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakajbj(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjakajbj
end do a_aibjakajbj
end do j_aibjakajbj
end do b_aibjakajbj
end do k_aibjakajbj
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: c == a, d == a, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjakajbk: do k = nocc0, nocc1
b_aibjakajbk: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjakajbk: do j = nocc0, k - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakajbk: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakajbk: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakajbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakajbk(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjakajbk
end do a_aibjakajbk
end do j_aibjakajbk
end do b_aibjakajbk
end do k_aibjakajbk
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, l, i
! Equalities: c == a, d == a, e == b, k == j, m == l
! No equalities independent of the above can hold.
!
l_aibjajalbl: do l = nocc0, nocc1
b_aibjajalbl: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjajalbl: do j = l + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajalbl: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjajalbl: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjajalbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajalbl(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjajalbl
end do a_aibjajalbl
end do j_aibjajalbl
end do b_aibjajalbl
end do l_aibjajalbl
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == a, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjajalbj: do l = nocc0, nocc1
b_aibjajalbj: do b = nvirt0, nvirt1
j_aibjajalbj: do j = l + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajalbj: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjajalbj: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjajalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajalbj(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjajalbj
end do a_aibjajalbj
end do j_aibjajalbj
end do b_aibjajalbj
end do l_aibjajalbj
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibiakaiei: do e = nvirt0, nvirt1
k_aibiakaiei: do k = nocc0, nocc1
b_aibiakaiei: do b = nvirt0, nvirt1
if (b == e) cycle b_aibiakaiei
a0 = max(b, e)
a_aibiakaiei: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakaiei: do i = nocc0, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakaiei(eorb, t2, t1, nocc, nactive, a, i, b, k, e)
end do i_aibiakaiei
end do a_aibiakaiei
end do b_aibiakaiei
end do k_aibiakaiei
end do e_aibiakaiei
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajaiei: do e = nvirt0, nvirt1
b_aibjajaiei: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjajaiei
j_aibjajaiei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjajaiei: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajaiei: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajaiei(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjajaiei
end do a_aibjajaiei
end do j_aibjajaiei
end do b_aibjajaiei
end do e_aibjajaiei
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjajaiej: do e = nvirt0, nvirt1
b_aibjajaiej: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjajaiej
j_aibjajaiej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjajaiej: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajaiej: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajaiej(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjajaiej
end do a_aibjajaiej
end do j_aibjajaiej
end do b_aibjajaiej
end do e_aibjajaiej
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l
! Equalities: c == a, d == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibiaialei: do e = nvirt0, nvirt1
l_aibiaialei: do l = nocc0, nocc1
b_aibiaialei: do b = nvirt0, nvirt1
if (b == e) cycle b_aibiaialei
a0 = max(b, e)
a_aibiaialei: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibiaialei: do i = l + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaialei(eorb, t2, t1, nocc, nactive, a, i, b, l, e)
end do i_aibiaialei
end do a_aibiaialei
end do b_aibiaialei
end do l_aibiaialei
end do e_aibiaialei
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaiajei: do e = nvirt0, nvirt1
b_aibjaiajei: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjaiajei
j_aibjaiajei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjaiajei: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjaiajei: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaiajei(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjaiajei
end do a_aibjaiajei
end do j_aibjaiajei
end do b_aibjaiajei
end do e_aibjaiajei
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjaiajej: do e = nvirt0, nvirt1
b_aibjaiajej: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjaiajej
j_aibjaiajej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjaiajej: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjaiajej: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaiajej(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjaiajej
end do a_aibjaiajej
end do j_aibjaiajej
end do b_aibjaiajej
end do e_aibjaiajej
!
! Elementary loop 30
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
i_aibibkbiei: do i = nocc0, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibibkbiei(eorb, t2, t1, nocc, nactive, a, i, b, k, e)
end do i_aibibkbiei
end do a_aibibkbiei
end do b_aibibkbiei
end do k_aibibkbiei
end do e_aibibkbiei
!
! Elementary loop 31
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjbiei: do e = nvirt0, nvirt1
b_aibjbjbiei: do b = e + 1, nvirt1
j_aibjbjbiei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbjbiei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbjbiei
i_aibjbjbiei: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbjbiei(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjbjbiei
end do a_aibjbjbiei
end do j_aibjbjbiei
end do b_aibjbjbiei
end do e_aibjbjbiei
!
! Elementary loop 32
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjbjbiej: do e = nvirt0, nvirt1
b_aibjbjbiej: do b = e + 1, nvirt1
j_aibjbjbiej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbjbiej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbjbiej
i_aibjbjbiej: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbjbiej(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjbjbiej
end do a_aibjbjbiej
end do j_aibjbjbiej
end do b_aibjbjbiej
end do e_aibjbjbiej
!
! Elementary loop 33
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, l
! Equalities: c == b, d == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibibiblei: do e = nvirt0, nvirt1
l_aibibiblei: do l = nocc0, nocc1
b_aibibiblei: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibibiblei: do a = b + 1, nvirt1
if (a == e) cycle a_aibibiblei
i_aibibiblei: do i = l + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibibiblei(eorb, t2, t1, nocc, nactive, a, i, b, l, e)
end do i_aibibiblei
end do a_aibibiblei
end do b_aibibiblei
end do l_aibibiblei
end do e_aibibiblei
!
! Elementary loop 34
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
i_aibjbibjei: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbibjei(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjbibjei
end do a_aibjbibjei
end do j_aibjbibjei
end do b_aibjbibjei
end do e_aibjbibjei
!
! Elementary loop 35
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjbibjej: do e = nvirt0, nvirt1
b_aibjbibjej: do b = e + 1, nvirt1
j_aibjbibjej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbibjej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjbibjej
i_aibjbibjej: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbibjej(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjbibjej
end do a_aibjbibjej
end do j_aibjbibjej
end do b_aibjbibjej
end do e_aibjbibjej
!
! Elementary loop 36
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, d == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckciaj: do c = nvirt0, nvirt1
k_aiajckciaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckciaj: do j = nocc0, nocc1
if (j == k) cycle j_aiajckciaj
a_aiajckciaj: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(k, j)
i_aiajckciaj: do i = j + 1, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckciaj(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckciaj
end do a_aiajckciaj
end do j_aiajckciaj
end do k_aiajckciaj
end do c_aiajckciaj
!
! Elementary loop 37
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, d == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajckcjai: do c = nvirt0, nvirt1
k_aiajckcjai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckcjai: do j = nocc0, k - 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckcjai: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckcjai: do i = j + 1, nocc1
if (i == k) cycle i_aiajckcjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckcjai(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckcjai
end do a_aiajckcjai
end do j_aiajckcjai
end do k_aiajckcjai
end do c_aiajckcjai
!
! Elementary loop 38
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, e == a, d == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aiajcjclai: do l = nocc0, nocc1
c_aiajcjclai: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
j_aiajcjclai: do j = l + 1, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjclai: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j, l)
i_aiajcjclai: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjclai(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcjclai
end do a_aiajcjclai
end do j_aiajcjclai
end do c_aiajcjclai
end do l_aiajcjclai
!
! Elementary loop 39
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, e == a, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajcicjam: do m = nocc0, nocc1
c_aiajcicjam: do c = nvirt0, nvirt1
j_aiajcicjam: do j = nocc0, m - 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcicjam: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajcicjam: do i = j + 1, nocc1
if (i == m) cycle i_aiajcicjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcicjam(eorb, t2, t1, nocc, nactive, a, i, j, c, m)
end do i_aiajcicjam
end do a_aiajcicjam
end do j_aiajcicjam
end do c_aiajcicjam
end do m_aiajcicjam
!
! Elementary loop 40
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, e == a, d == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aiajciclaj: do l = nocc0, nocc1
c_aiajciclaj: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
j_aiajciclaj: do j = l + 1, nocc1
a_aiajciclaj: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j, l)
i_aiajciclaj: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajciclaj(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajciclaj
end do a_aiajciclaj
end do j_aiajciclaj
end do c_aiajciclaj
end do l_aiajciclaj
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k
! Equalities: e == a, d == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibickciai: do c = nvirt0, nvirt1
k_aibickciai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickciai: do b = nvirt0, nvirt1
if (b == c) cycle b_aibickciai
a_aibickciai: do a = b + 1, c - 1
i_aibickciai: do i = nocc0, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibickciai(eorb, t2, t1, nocc, nactive, a, i, b, c, k)
end do i_aibickciai
end do a_aibickciai
end do b_aibickciai
end do k_aibickciai
end do c_aibickciai
!
! Elementary loop 42
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: e == a, d == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjciaj: do c = nvirt0, nvirt1
b_aibjcjciaj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjciaj
j_aibjcjciaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjciaj: do a = b + 1, c - 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjciaj: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjciaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjciaj
end do a_aibjcjciaj
end do j_aibjcjciaj
end do b_aibjcjciaj
end do c_aibjcjciaj
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, l
! Equalities: e == a, d == c, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiciclai: do l = nocc0, nocc1
c_aibiciclai: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b_aibiciclai: do b = nvirt0, nvirt1
if (b == c) cycle b_aibiciclai
a_aibiciclai: do a = b + 1, c - 1
i_aibiciclai: do i = l + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiciclai(eorb, t2, t1, nocc, nactive, a, i, b, c, l)
end do i_aibiciclai
end do a_aibiciclai
end do b_aibiciclai
end do c_aibiciclai
end do l_aibiciclai
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: e == a, d == c, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcicjaj: do c = nvirt0, nvirt1
b_aibjcicjaj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcicjaj
j_aibjcicjaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicjaj: do a = b + 1, c - 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcicjaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcicjaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcicjaj
end do a_aibjcicjaj
end do j_aibjcicjaj
end do b_aibjcicjaj
end do c_aibjcicjaj
!
! Elementary loop 45
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k
! Equalities: e == b, d == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibickcibi: do c = nvirt0, nvirt1
k_aibickcibi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickcibi: do b = nvirt0, c - 1
a_aibickcibi: do a = b + 1, nvirt1
if (a == c) cycle a_aibickcibi
i_aibickcibi: do i = nocc0, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibickcibi(eorb, t2, t1, nocc, nactive, a, i, b, c, k)
end do i_aibickcibi
end do a_aibickcibi
end do b_aibickcibi
end do k_aibickcibi
end do c_aibickcibi
!
! Elementary loop 46
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: e == b, d == c, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjcibi: do c = nvirt0, nvirt1
b_aibjcjcibi: do b = nvirt0, c - 1
j_aibjcjcibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjcibi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjcibi
i_aibjcjcibi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjcibi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjcibi
end do a_aibjcjcibi
end do j_aibjcjcibi
end do b_aibjcjcibi
end do c_aibjcjcibi
!
! Elementary loop 47
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, l
! Equalities: e == b, d == c, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiciclbi: do l = nocc0, nocc1
c_aibiciclbi: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b_aibiciclbi: do b = nvirt0, c - 1
a_aibiciclbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibiciclbi
i_aibiciclbi: do i = l + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiciclbi(eorb, t2, t1, nocc, nactive, a, i, b, c, l)
end do i_aibiciclbi
end do a_aibiciclbi
end do b_aibiciclbi
end do c_aibiciclbi
end do l_aibiciclbi
!
! Elementary loop 48
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: e == b, d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicjbi: do c = nvirt0, nvirt1
b_aibjcicjbi: do b = nvirt0, c - 1
j_aibjcicjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicjbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcicjbi
i_aibjcicjbi: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcicjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcicjbi
end do a_aibjcicjbi
end do j_aibjcicjbi
end do b_aibjcicjbi
end do c_aibjcicjbi
!
! Elementary loop 49
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, e == d, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajakdidj: do d = nvirt0, nvirt1
k_aiajakdidj: do k = nocc0, nocc1
j_aiajakdidj: do j = nocc0, nocc1
if (j == k) cycle j_aiajakdidj
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajakdidj: do a = d + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j, k)
i_aiajakdidj: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajakdidj(eorb, t2, t1, nocc, nactive, a, i, j, k, d)
end do i_aiajakdidj
end do a_aiajakdidj
end do j_aiajakdidj
end do k_aiajakdidj
end do d_aiajakdidj
!
! Elementary loop 50
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, e == d, l == i, k == j
! No equalities independent of the above can hold.
!
m_aiajajdidm: do m = nocc0, nocc1
d_aiajajdidm: do d = nvirt0, nvirt1
em = (d - nvirt0) * nocc + (m - nocc0) + 1
j_aiajajdidm: do j = nocc0, nocc1
if (j == m) cycle j_aiajajdidm
a_aiajajdidm: do a = d + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j, m)
i_aiajajdidm: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajajdidm(eorb, t2, t1, nocc, nactive, a, i, j, d, m)
end do i_aiajajdidm
end do a_aiajajdidm
end do j_aiajajdidm
end do d_aiajajdidm
end do m_aiajajdidm
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
end subroutine ccjac_23_part5
end module ccjac_block_23_part5
