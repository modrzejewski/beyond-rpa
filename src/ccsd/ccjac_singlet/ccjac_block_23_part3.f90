module ccjac_block_23_part3
use eom_cc3_23_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:35:25 UTC.
!
contains
 
subroutine ccjac_23_part3(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: a0, i0, j0
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
! Free virtual indices: a, b, d
! Free occupied indices: i, k, j
! Equalities: c == a, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjakdibk: do d = nvirt0, nvirt1
k_aibjakdibk: do k = nocc0, nocc1
b_aibjakdibk: do b = nvirt0, d - 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjakdibk: do j = nocc0, nocc1
if (j == k) cycle j_aibjakdibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjakdibk: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdibk: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakdibk(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjakdibk
end do a_aibjakdibk
end do j_aibjakdibk
end do b_aibjakdibk
end do k_aibjakdibk
end do d_aibjakdibk
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, l
! Equalities: c == a, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
d_aibiakdlbi: do d = nvirt0, nvirt1
l_aibiakdlbi: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aibiakdlbi: do k = nocc0, nocc1
if (k == l) cycle k_aibiakdlbi
b_aibiakdlbi: do b = nvirt0, d - 1
a0 = max(b, d)
a_aibiakdlbi: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdlbi: do i = nocc0, nocc1
if (i == k .or. i == l) cycle i_aibiakdlbi
if (k > l .and. l > i) cycle i_aibiakdlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakdlbi(eorb, t2, t1, nocc, nactive, a, i, b, k, d, l)
end do i_aibiakdlbi
end do a_aibiakdlbi
end do b_aibiakdlbi
end do k_aibiakdlbi
end do l_aibiakdlbi
end do d_aibiakdlbi
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l, m
! Equalities: c == a, e == b, j == i, k == i
! No equalities independent of the above can hold.
!
m_aibiaidlbm: do m = nocc0, nocc1
d_aibiaidlbm: do d = nvirt0, nvirt1
l_aibiaidlbm: do l = nocc0, nocc1
if (l == m) cycle l_aibiaidlbm
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibiaidlbm: do b = nvirt0, d - 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b, d)
a_aibiaidlbm: do a = a0 + 1, nvirt1
i_aibiaidlbm: do i = nocc0, nocc1
if (i == l .or. i == m) cycle i_aibiaidlbm
if (i > l .and. l > m) exit i_aibiaidlbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaidlbm(eorb, t2, t1, nocc, nactive, a, i, b, d, l, m)
end do i_aibiaidlbm
end do a_aibiaidlbm
end do b_aibiaidlbm
end do l_aibiaidlbm
end do d_aibiaidlbm
end do m_aibiaidlbm
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibjaidlbi: do d = nvirt0, nvirt1
l_aibjaidlbi: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaidlbi: do b = nvirt0, d - 1
j_aibjaidlbi: do j = nocc0, nocc1
if (j == l) cycle j_aibjaidlbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjaidlbi: do a = a0 + 1, nvirt1
i_aibjaidlbi: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjaidlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaidlbi(eorb, t2, t1, nocc, nactive, a, i, b, j, d, l)
end do i_aibjaidlbi
end do a_aibjaidlbi
end do j_aibjaidlbi
end do b_aibjaidlbi
end do l_aibjaidlbi
end do d_aibjaidlbi
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjakdjbi: do d = nvirt0, nvirt1
k_aibjakdjbi: do k = nocc0, nocc1
b_aibjakdjbi: do b = nvirt0, d - 1
j_aibjakdjbi: do j = nocc0, nocc1
if (j == k) cycle j_aibjakdjbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjakdjbi: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjbi: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakdjbi
if (k > j .and. j > i) cycle i_aibjakdjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakdjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjakdjbi
end do a_aibjakdjbi
end do j_aibjakdjbi
end do b_aibjakdjbi
end do k_aibjakdjbi
end do d_aibjakdjbi
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, m
! Equalities: c == a, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaidjbm: do m = nocc0, nocc1
d_aibjaidjbm: do d = nvirt0, nvirt1
b_aibjaidjbm: do b = nvirt0, d - 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaidjbm: do j = nocc0, nocc1
if (j == m) cycle j_aibjaidjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjaidjbm: do a = a0 + 1, nvirt1
i_aibjaidjbm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjaidjbm
if (i > j .and. j > m) exit i_aibjaidjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaidjbm(eorb, t2, t1, nocc, nactive, a, i, b, j, d, m)
end do i_aibjaidjbm
end do a_aibjaidjbm
end do j_aibjaidjbm
end do b_aibjaidjbm
end do d_aibjaidjbm
end do m_aibjaidjbm
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l, j
! Equalities: c == a, e == b, k == i, m == l
! No equalities independent of the above can hold.
!
d_aibjaidlbl: do d = nvirt0, nvirt1
l_aibjaidlbl: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaidlbl: do b = nvirt0, d - 1
em = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaidlbl: do j = nocc0, nocc1
if (j == l) cycle j_aibjaidlbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjaidlbl: do a = a0 + 1, nvirt1
i_aibjaidlbl: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjaidlbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaidlbl(eorb, t2, t1, nocc, nactive, a, i, b, j, d, l)
end do i_aibjaidlbl
end do a_aibjaidlbl
end do j_aibjaidlbl
end do b_aibjaidlbl
end do l_aibjaidlbl
end do d_aibjaidlbl
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjaidlbj: do d = nvirt0, nvirt1
l_aibjaidlbj: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaidlbj: do b = nvirt0, d - 1
j_aibjaidlbj: do j = nocc0, nocc1
if (j == l) cycle j_aibjaidlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjaidlbj: do a = a0 + 1, nvirt1
i_aibjaidlbj: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjaidlbj
if (i > l .and. l > j) exit i_aibjaidlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaidlbj(eorb, t2, t1, nocc, nactive, a, i, b, j, d, l)
end do i_aibjaidlbj
end do a_aibjaidlbj
end do j_aibjaidlbj
end do b_aibjaidlbj
end do l_aibjaidlbj
end do d_aibjaidlbj
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k
! Equalities: c == a, e == b, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjakdjbj: do d = nvirt0, nvirt1
k_aibjakdjbj: do k = nocc0, nocc1
b_aibjakdjbj: do b = nvirt0, d - 1
j_aibjakdjbj: do j = nocc0, nocc1
if (j == k) cycle j_aibjakdjbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjakdjbj: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjbj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakdjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakdjbj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjakdjbj
end do a_aibjakdjbj
end do j_aibjakdjbj
end do b_aibjakdjbj
end do k_aibjakdjbj
end do d_aibjakdjbj
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, k, i
! Equalities: c == a, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjakdjbk: do d = nvirt0, nvirt1
k_aibjakdjbk: do k = nocc0, nocc1
b_aibjakdjbk: do b = nvirt0, d - 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjakdjbk: do j = nocc0, nocc1
if (j == k) cycle j_aibjakdjbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjakdjbk: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjbk: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakdjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakdjbk(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjakdjbk
end do a_aibjakdjbk
end do j_aibjakdjbk
end do b_aibjakdjbk
end do k_aibjakdjbk
end do d_aibjakdjbk
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, l
! Equalities: c == a, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjajdlbj: do d = nvirt0, nvirt1
l_aibjajdlbj: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjajdlbj: do b = nvirt0, d - 1
j_aibjajdlbj: do j = nocc0, nocc1
if (j == l) cycle j_aibjajdlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjajdlbj: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdlbj: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjajdlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajdlbj(eorb, t2, t1, nocc, nactive, a, i, b, j, d, l)
end do i_aibjajdlbj
end do a_aibjajdlbj
end do j_aibjajdlbj
end do b_aibjajdlbj
end do l_aibjajdlbj
end do d_aibjajdlbj
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibiakdiei: do e = nvirt0, nvirt1
d_aibiakdiei: do d = e + 1, nvirt1
k_aibiakdiei: do k = nocc0, nocc1
b_aibiakdiei: do b = nvirt0, nvirt1
if (b == d .or. b == e) cycle b_aibiakdiei
a0 = max(b, d)
a_aibiakdiei: do a = a0 + 1, nvirt1
if (a == e) cycle a_aibiakdiei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdiei: do i = nocc0, nocc1
if (i == k) cycle i_aibiakdiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakdiei(eorb, t2, t1, nocc, nactive, a, i, b, k, d, e)
end do i_aibiakdiei
end do a_aibiakdiei
end do b_aibiakdiei
end do k_aibiakdiei
end do d_aibiakdiei
end do e_aibiakdiei
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjajdiej: do e = nvirt0, nvirt1
d_aibjajdiej: do d = e + 1, nvirt1
b_aibjajdiej: do b = nvirt0, nvirt1
if (b == d .or. b == e) cycle b_aibjajdiej
j_aibjajdiej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjajdiej: do a = a0 + 1, nvirt1
if (a == e) cycle a_aibjajdiej
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdiej: do i = nocc0, nocc1
if (i == j) cycle i_aibjajdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajdiej(eorb, t2, t1, nocc, nactive, a, i, b, j, d, e)
end do i_aibjajdiej
end do a_aibjajdiej
end do j_aibjajdiej
end do b_aibjajdiej
end do d_aibjajdiej
end do e_aibjajdiej
!
! Elementary loop 14
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, l
! Equalities: c == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibiaidlei: do e = nvirt0, nvirt1
d_aibiaidlei: do d = e + 1, nvirt1
l_aibiaidlei: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibiaidlei: do b = nvirt0, nvirt1
if (b == d .or. b == e) cycle b_aibiaidlei
a0 = max(b, d)
a_aibiaidlei: do a = a0 + 1, nvirt1
if (a == e) cycle a_aibiaidlei
i_aibiaidlei: do i = nocc0, nocc1
if (i == l) cycle i_aibiaidlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaidlei(eorb, t2, t1, nocc, nactive, a, i, b, d, l, e)
end do i_aibiaidlei
end do a_aibiaidlei
end do b_aibiaidlei
end do l_aibiaidlei
end do d_aibiaidlei
end do e_aibiaidlei
!
! Elementary loop 15
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j
! Equalities: c == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaidjei: do e = nvirt0, nvirt1
d_aibjaidjei: do d = e + 1, nvirt1
b_aibjaidjei: do b = nvirt0, nvirt1
if (b == d .or. b == e) cycle b_aibjaidjei
j_aibjaidjei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjaidjei: do a = a0 + 1, nvirt1
if (a == e) cycle a_aibjaidjei
i_aibjaidjei: do i = nocc0, nocc1
if (i == j) cycle i_aibjaidjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaidjei(eorb, t2, t1, nocc, nactive, a, i, b, j, d, e)
end do i_aibjaidjei
end do a_aibjaidjei
end do j_aibjaidjei
end do b_aibjaidjei
end do d_aibjaidjei
end do e_aibjaidjei
!
! Elementary loop 16
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjaidjej: do e = nvirt0, nvirt1
d_aibjaidjej: do d = e + 1, nvirt1
b_aibjaidjej: do b = nvirt0, nvirt1
if (b == d .or. b == e) cycle b_aibjaidjej
j_aibjaidjej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjaidjej: do a = a0 + 1, nvirt1
if (a == e) cycle a_aibjaidjej
i_aibjaidjej: do i = nocc0, nocc1
if (i == j) cycle i_aibjaidjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaidjej(eorb, t2, t1, nocc, nactive, a, i, b, j, d, e)
end do i_aibjaidjej
end do a_aibjaidjej
end do j_aibjaidjej
end do b_aibjaidjej
end do d_aibjaidjej
end do e_aibjaidjej
!
! Elementary loop 17
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, j, k
! Equalities: c == b, e == d, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbkdidj: do d = nvirt0, nvirt1
k_aibjbkdidj: do k = nocc0, nocc1
b_aibjbkdidj: do b = d + 1, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdidj: do j = nocc0, nocc1
if (j == k) cycle j_aibjbkdidj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdidj: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbkdidj
i0 = max(j, k)
i_aibjbkdidj: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbkdidj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdidj
end do a_aibjbkdidj
end do j_aibjbkdidj
end do b_aibjbkdidj
end do k_aibjbkdidj
end do d_aibjbkdidj
!
! Elementary loop 18
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, j, m
! Equalities: c == b, e == d, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjbjdidm: do m = nocc0, nocc1
d_aibjbjdidm: do d = nvirt0, nvirt1
em = (d - nvirt0) * nocc + (m - nocc0) + 1
b_aibjbjdidm: do b = d + 1, nvirt1
j_aibjbjdidm: do j = nocc0, nocc1
if (j == m) cycle j_aibjbjdidm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbjdidm: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbjdidm
i0 = max(m, j)
i_aibjbjdidm: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbjdidm(eorb, t2, t1, nocc, nactive, a, i, b, j, d, m)
end do i_aibjbjdidm
end do a_aibjbjdidm
end do j_aibjbjdidm
end do b_aibjbjdidm
end do d_aibjbjdidm
end do m_aibjbjdidm
!
! Elementary loop 19
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, j, k
! Equalities: c == b, e == d, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdjdi: do d = nvirt0, nvirt1
k_aibjbkdjdi: do k = nocc0, nocc1
b_aibjbkdjdi: do b = d + 1, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdjdi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdjdi: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbkdjdi
i_aibjbkdjdi: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkdjdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbkdjdi(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d)
end do i_aibjbkdjdi
end do a_aibjbkdjdi
end do j_aibjbkdjdi
end do b_aibjbkdjdi
end do k_aibjbkdjdi
end do d_aibjbkdjdi
!
! Elementary loop 20
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, j, l
! Equalities: c == b, e == d, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdldi: do d = nvirt0, nvirt1
l_aibjbjdldi: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjbjdldi: do b = d + 1, nvirt1
j_aibjbjdldi: do j = nocc0, l - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbjdldi: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbjdldi
i_aibjbjdldi: do i = nocc0, l - 1
if (i == j) cycle i_aibjbjdldi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbjdldi(eorb, t2, t1, nocc, nactive, a, i, b, j, d, l)
end do i_aibjbjdldi
end do a_aibjbjdldi
end do j_aibjbjdldi
end do b_aibjbjdldi
end do l_aibjbjdldi
end do d_aibjbjdldi
!
! Elementary loop 21
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, k
! Equalities: c == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibibkdiei: do e = nvirt0, nvirt1
d_aibibkdiei: do d = e + 1, nvirt1
k_aibibkdiei: do k = nocc0, nocc1
b_aibibkdiei: do b = d + 1, nvirt1
if (b == e) cycle b_aibibkdiei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkdiei: do a = b + 1, nvirt1
if (a == d .or. a == e) cycle a_aibibkdiei
i_aibibkdiei: do i = nocc0, nocc1
if (i == k) cycle i_aibibkdiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibibkdiei(eorb, t2, t1, nocc, nactive, a, i, b, k, d, e)
end do i_aibibkdiei
end do a_aibibkdiei
end do b_aibibkdiei
end do k_aibibkdiei
end do d_aibibkdiei
end do e_aibibkdiei
!
! Elementary loop 22
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j
! Equalities: c == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjdiei: do e = nvirt0, nvirt1
d_aibjbjdiei: do d = e + 1, nvirt1
b_aibjbjdiei: do b = d + 1, nvirt1
if (b == e) cycle b_aibjbjdiei
j_aibjbjdiei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbjdiei: do a = b + 1, nvirt1
if (a == d .or. a == e) cycle a_aibjbjdiei
i_aibjbjdiei: do i = nocc0, nocc1
if (i == j) cycle i_aibjbjdiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbjdiei(eorb, t2, t1, nocc, nactive, a, i, b, j, d, e)
end do i_aibjbjdiei
end do a_aibjbjdiei
end do j_aibjbjdiei
end do b_aibjbjdiei
end do d_aibjbjdiei
end do e_aibjbjdiei
!
! Elementary loop 23
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjbjdiej: do e = nvirt0, nvirt1
d_aibjbjdiej: do d = e + 1, nvirt1
b_aibjbjdiej: do b = d + 1, nvirt1
if (b == e) cycle b_aibjbjdiej
j_aibjbjdiej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbjdiej: do a = b + 1, nvirt1
if (a == d .or. a == e) cycle a_aibjbjdiej
i_aibjbjdiej: do i = nocc0, nocc1
if (i == j) cycle i_aibjbjdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbjdiej(eorb, t2, t1, nocc, nactive, a, i, b, j, d, e)
end do i_aibjbjdiej
end do a_aibjbjdiej
end do j_aibjbjdiej
end do b_aibjbjdiej
end do d_aibjbjdiej
end do e_aibjbjdiej
!
! Elementary loop 24
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, l
! Equalities: c == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibibidlei: do e = nvirt0, nvirt1
d_aibibidlei: do d = e + 1, nvirt1
l_aibibidlei: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibibidlei: do b = d + 1, nvirt1
if (b == e) cycle b_aibibidlei
a_aibibidlei: do a = b + 1, nvirt1
if (a == d .or. a == e) cycle a_aibibidlei
i_aibibidlei: do i = nocc0, nocc1
if (i == l) cycle i_aibibidlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibibidlei(eorb, t2, t1, nocc, nactive, a, i, b, d, l, e)
end do i_aibibidlei
end do a_aibibidlei
end do b_aibibidlei
end do l_aibibidlei
end do d_aibibidlei
end do e_aibibidlei
!
! Elementary loop 25
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j
! Equalities: c == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbidjei: do e = nvirt0, nvirt1
d_aibjbidjei: do d = e + 1, nvirt1
b_aibjbidjei: do b = d + 1, nvirt1
if (b == e) cycle b_aibjbidjei
j_aibjbidjei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbidjei: do a = b + 1, nvirt1
if (a == d .or. a == e) cycle a_aibjbidjei
i_aibjbidjei: do i = nocc0, nocc1
if (i == j) cycle i_aibjbidjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbidjei(eorb, t2, t1, nocc, nactive, a, i, b, j, d, e)
end do i_aibjbidjei
end do a_aibjbidjei
end do j_aibjbidjei
end do b_aibjbidjei
end do d_aibjbidjei
end do e_aibjbidjei
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, m
! Equalities: b == a, d == a, e == a, l == i
! No equalities independent of the above can hold.
!
m_aiajckaiam: do m = nocc0, nocc1
c_aiajckaiam: do c = nvirt0, nvirt1
k_aiajckaiam: do k = nocc0, nocc1
if (k == m) cycle k_aiajckaiam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiam: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aiajckaiam
a_aiajckaiam: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j, m, k)
i_aiajckaiam: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckaiam(eorb, t2, t1, nocc, nactive, a, i, j, c, k, m)
end do i_aiajckaiam
end do a_aiajckaiam
end do j_aiajckaiam
end do k_aiajckaiam
end do c_aiajckaiam
end do m_aiajckaiam
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, l
! Equalities: b == a, d == a, e == a, m == i
! No equalities independent of the above can hold.
!
l_aiajckalai: do l = nocc0, nocc1
c_aiajckalai: do c = nvirt0, nvirt1
k_aiajckalai: do k = nocc0, l - 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckalai: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aiajckalai
a_aiajckalai: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajckalai: do i = j + 1, l - 1
if (i == k) cycle i_aiajckalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckalai(eorb, t2, t1, nocc, nactive, a, i, j, c, k, l)
end do i_aiajckalai
end do a_aiajckalai
end do j_aiajckalai
end do k_aiajckalai
end do c_aiajckalai
end do l_aiajckalai
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l, m
! Equalities: b == a, d == a, e == a, k == i
! No equalities independent of the above can hold.
!
m_aiajcialam: do m = nocc0, nocc1
l_aiajcialam: do l = m + 1, nocc1
c_aiajcialam: do c = nvirt0, nvirt1
j_aiajcialam: do j = nocc0, nocc1
if (j == l .or. j == m) cycle j_aiajcialam
a_aiajcialam: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajcialam: do i = j + 1, l - 1
if (i == m) cycle i_aiajcialam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcialam(eorb, t2, t1, nocc, nactive, a, i, j, c, l, m)
end do i_aiajcialam
end do a_aiajcialam
end do j_aiajcialam
end do c_aiajcialam
end do l_aiajcialam
end do m_aiajcialam
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, m
! Equalities: b == a, d == a, e == a, l == j
! No equalities independent of the above can hold.
!
m_aiajckajam: do m = nocc0, nocc1
c_aiajckajam: do c = nvirt0, nvirt1
k_aiajckajam: do k = nocc0, nocc1
if (k == m) cycle k_aiajckajam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(m, k)
j_aiajckajam: do j = j0 + 1, nocc1
a_aiajckajam: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajckajam: do i = j + 1, nocc1
if (i == k .or. i == m) cycle i_aiajckajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckajam(eorb, t2, t1, nocc, nactive, a, i, j, c, k, m)
end do i_aiajckajam
end do a_aiajckajam
end do j_aiajckajam
end do k_aiajckajam
end do c_aiajckajam
end do m_aiajckajam
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, l
! Equalities: b == a, d == a, e == a, m == j
! No equalities independent of the above can hold.
!
l_aiajckalaj: do l = nocc0, nocc1
c_aiajckalaj: do c = nvirt0, nvirt1
k_aiajckalaj: do k = nocc0, l - 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckalaj: do j = nocc0, l - 1
if (j == k) cycle j_aiajckalaj
a_aiajckalaj: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckalaj: do i = j + 1, nocc1
if (i == k .or. i == l) cycle i_aiajckalaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckalaj(eorb, t2, t1, nocc, nactive, a, i, j, c, k, l)
end do i_aiajckalaj
end do a_aiajckalaj
end do j_aiajckalaj
end do k_aiajckalaj
end do c_aiajckalaj
end do l_aiajckalaj
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l, m
! Equalities: b == a, d == a, e == a, k == j
! No equalities independent of the above can hold.
!
m_aiajcjalam: do m = nocc0, nocc1
l_aiajcjalam: do l = m + 1, nocc1
c_aiajcjalam: do c = nvirt0, nvirt1
j_aiajcjalam: do j = nocc0, l - 1
if (j == m) cycle j_aiajcjalam
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjalam: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajcjalam: do i = j + 1, nocc1
if (i == l .or. i == m) cycle i_aiajcjalam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjalam(eorb, t2, t1, nocc, nactive, a, i, j, c, l, m)
end do i_aiajcjalam
end do a_aiajcjalam
end do j_aiajcjalam
end do c_aiajcjalam
end do l_aiajcjalam
end do m_aiajcjalam
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajckaiej: do e = nvirt0, nvirt1
c_aiajckaiej: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajckaiej
k_aiajckaiej: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiej: do j = nocc0, nocc1
if (j == k) cycle j_aiajckaiej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckaiej: do a = e + 1, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaiej: do i = j + 1, nocc1
if (i == k) cycle i_aiajckaiej
if (k > i .and. i > j) cycle i_aiajckaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckaiej(eorb, t2, t1, nocc, nactive, a, i, j, c, k, e)
end do i_aiajckaiej
end do a_aiajckaiej
end do j_aiajckaiej
end do k_aiajckaiej
end do c_aiajckaiej
end do e_aiajckaiej
!
! Elementary loop 33
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
e_aiajcjaiem: do e = nvirt0, nvirt1
m_aiajcjaiem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aiajcjaiem: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajcjaiem
j_aiajcjaiem: do j = nocc0, nocc1
if (j == m) cycle j_aiajcjaiem
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjaiem: do a = e + 1, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjaiem: do i = j + 1, nocc1
if (i == m) cycle i_aiajcjaiem
if (j > i .and. i > m) cycle i_aiajcjaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjaiem(eorb, t2, t1, nocc, nactive, a, i, j, c, e, m)
end do i_aiajcjaiem
end do a_aiajcjaiem
end do j_aiajcjaiem
end do c_aiajcjaiem
end do m_aiajcjaiem
end do e_aiajcjaiem
!
! Elementary loop 34
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajckajei: do e = nvirt0, nvirt1
c_aiajckajei: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajckajei
k_aiajckajei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckajei: do j = nocc0, nocc1
if (j == k) cycle j_aiajckajei
a_aiajckajei: do a = e + 1, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajei: do i = j + 1, nocc1
if (i == k) cycle i_aiajckajei
if (k > j .and. j > i) cycle i_aiajckajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajckajei(eorb, t2, t1, nocc, nactive, a, i, j, c, k, e)
end do i_aiajckajei
end do a_aiajckajei
end do j_aiajckajei
end do k_aiajckajei
end do c_aiajckajei
end do e_aiajckajei
!
! Elementary loop 35
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aiajcjalei: do e = nvirt0, nvirt1
l_aiajcjalei: do l = nocc0, nocc1
c_aiajcjalei: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajcjalei
j_aiajcjalei: do j = nocc0, nocc1
if (j == l) cycle j_aiajcjalei
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjalei: do a = e + 1, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajcjalei: do i = j + 1, nocc1
if (i == l) cycle i_aiajcjalei
if (j > l .and. l > i) cycle i_aiajcjalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjalei(eorb, t2, t1, nocc, nactive, a, i, j, c, l, e)
end do i_aiajcjalei
end do a_aiajcjalei
end do j_aiajcjalei
end do c_aiajcjalei
end do l_aiajcjalei
end do e_aiajcjalei
!
! Elementary loop 36
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aiajciajem: do e = nvirt0, nvirt1
m_aiajciajem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aiajciajem: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajciajem
j_aiajciajem: do j = nocc0, nocc1
if (j == m) cycle j_aiajciajem
a_aiajciajem: do a = e + 1, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciajem: do i = j + 1, nocc1
if (i == m) cycle i_aiajciajem
if (i > j .and. j > m) exit i_aiajciajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajciajem(eorb, t2, t1, nocc, nactive, a, i, j, c, e, m)
end do i_aiajciajem
end do a_aiajciajem
end do j_aiajciajem
end do c_aiajciajem
end do m_aiajciajem
end do e_aiajciajem
!
! Elementary loop 37
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aiajcialej: do e = nvirt0, nvirt1
l_aiajcialej: do l = nocc0, nocc1
c_aiajcialej: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajcialej
j_aiajcialej: do j = nocc0, nocc1
if (j == l) cycle j_aiajcialej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcialej: do a = e + 1, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajcialej: do i = j + 1, nocc1
if (i == l) cycle i_aiajcialej
if (i > l .and. l > j) exit i_aiajcialej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcialej(eorb, t2, t1, nocc, nactive, a, i, j, c, l, e)
end do i_aiajcialej
end do a_aiajcialej
end do j_aiajcialej
end do c_aiajcialej
end do l_aiajcialej
end do e_aiajcialej
!
! Elementary loop 38
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaiaj: do c = nvirt0, nvirt1
k_aibjckaiaj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaiaj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjckaiaj
j_aibjckaiaj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaiaj: do a = b + 1, c - 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j, k)
i_aibjckaiaj: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckaiaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end do i_aibjckaiaj
end do a_aibjckaiaj
end do j_aibjckaiaj
end do b_aibjckaiaj
end do k_aibjckaiaj
end do c_aibjckaiaj
!
! Elementary loop 39
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjaiam: do m = nocc0, nocc1
c_aibjcjaiam: do c = nvirt0, nvirt1
b_aibjcjaiam: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjaiam
j_aibjcjaiam: do j = nocc0, nocc1
if (j == m) cycle j_aibjcjaiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjaiam: do a = b + 1, c - 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m, j)
i_aibjcjaiam: do i = i0 + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjaiam(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcjaiam
end do a_aibjcjaiam
end do j_aibjcjaiam
end do b_aibjcjaiam
end do c_aibjcjaiam
end do m_aibjcjaiam
!
! Elementary loop 40
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == a, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckajai: do c = nvirt0, nvirt1
k_aibjckajai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajai: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjckajai
j_aibjckajai: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckajai: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajai: do i = nocc0, j - 1
if (i == k) cycle i_aibjckajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckajai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end do i_aibjckajai
end do a_aibjckajai
end do j_aibjckajai
end do b_aibjckajai
end do k_aibjckajai
end do c_aibjckajai
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == a, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjalai: do l = nocc0, nocc1
c_aibjcjalai: do c = nvirt0, nvirt1
b_aibjcjalai: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjalai
j_aibjcjalai: do j = nocc0, l - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjalai: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalai: do i = nocc0, l - 1
if (i == j) cycle i_aibjcjalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjalai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcjalai
end do a_aibjcjalai
end do j_aibjcjalai
end do b_aibjcjalai
end do c_aibjcjalai
end do l_aibjcjalai
!
! Elementary loop 42
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjciajam: do m = nocc0, nocc1
c_aibjciajam: do c = nvirt0, nvirt1
b_aibjciajam: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjciajam
j_aibjciajam: do j = m + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciajam: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjciajam: do i = nocc0, j - 1
if (i == m) cycle i_aibjciajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjciajam(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjciajam
end do a_aibjciajam
end do j_aibjciajam
end do b_aibjciajam
end do c_aibjciajam
end do m_aibjciajam
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == a, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjcialaj: do l = nocc0, nocc1
c_aibjcialaj: do c = nvirt0, nvirt1
b_aibjcialaj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcialaj
j_aibjcialaj: do j = nocc0, l - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcialaj: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcialaj: do i = nocc0, l - 1
if (i == j) cycle i_aibjcialaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcialaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l)
end do i_aibjcialaj
end do a_aibjcialaj
end do j_aibjcialaj
end do b_aibjcialaj
end do c_aibjcialaj
end do l_aibjcialaj
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, m
! Equalities: d == a, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibickaibm: do m = nocc0, nocc1
c_aibickaibm: do c = nvirt0, nvirt1
k_aibickaibm: do k = nocc0, nocc1
if (k == m) cycle k_aibickaibm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickaibm: do b = nvirt0, nvirt1
if (b == c) cycle b_aibickaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a_aibickaibm: do a = b + 1, c - 1
i_aibickaibm: do i = nocc0, nocc1
if (i == k .or. i == m) cycle i_aibickaibm
if (k > i .and. i > m) cycle i_aibickaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibickaibm(eorb, t2, t1, nocc, nactive, a, i, b, c, k, m)
end do i_aibickaibm
end do a_aibickaibm
end do b_aibickaibm
end do k_aibickaibm
end do c_aibickaibm
end do m_aibickaibm
!
! Elementary loop 45
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == b, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjckaibi: do c = nvirt0, nvirt1
k_aibjckaibi: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibi: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjckaibi
j_aibjckaibi: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaibi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaibi: do a = b + 1, c - 1
i_aibjckaibi: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckaibi(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end do i_aibjckaibi
end do a_aibjckaibi
end do j_aibjckaibi
end do b_aibjckaibi
end do k_aibjckaibi
end do c_aibjckaibi
!
! Elementary loop 46
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaibj: do c = nvirt0, nvirt1
k_aibjckaibj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjckaibj
j_aibjckaibj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaibj: do a = b + 1, c - 1
i_aibjckaibj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckaibj
if (k > i .and. i > j) cycle i_aibjckaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckaibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end do i_aibjckaibj
end do a_aibjckaibj
end do j_aibjckaibj
end do b_aibjckaibj
end do k_aibjckaibj
end do c_aibjckaibj
!
! Elementary loop 47
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjaibm: do m = nocc0, nocc1
c_aibjcjaibm: do c = nvirt0, nvirt1
b_aibjcjaibm: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjaibm: do j = nocc0, nocc1
if (j == m) cycle j_aibjcjaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjaibm: do a = b + 1, c - 1
i_aibjcjaibm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcjaibm
if (j > i .and. i > m) cycle i_aibjcjaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjaibm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, m)
end do i_aibjcjaibm
end do a_aibjcjaibm
end do j_aibjcjaibm
end do b_aibjcjaibm
end do c_aibjcjaibm
end do m_aibjcjaibm
!
! Elementary loop 48
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckaibk: do c = nvirt0, nvirt1
k_aibjckaibk: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibk: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjckaibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckaibk: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaibk: do a = b + 1, c - 1
i_aibjckaibk: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckaibk(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end do i_aibjckaibk
end do a_aibjckaibk
end do j_aibjckaibk
end do b_aibjckaibk
end do k_aibjckaibk
end do c_aibjckaibk
!
! Elementary loop 49
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, l
! Equalities: d == a, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibickalbi: do l = nocc0, nocc1
c_aibickalbi: do c = nvirt0, nvirt1
k_aibickalbi: do k = nocc0, nocc1
if (k == l) cycle k_aibickalbi
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickalbi: do b = nvirt0, nvirt1
if (b == c) cycle b_aibickalbi
a_aibickalbi: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibickalbi: do i = nocc0, nocc1
if (i == k .or. i == l) cycle i_aibickalbi
if (k > l .and. l > i) cycle i_aibickalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibickalbi(eorb, t2, t1, nocc, nactive, a, i, b, c, k, l)
end do i_aibickalbi
end do a_aibickalbi
end do b_aibickalbi
end do k_aibickalbi
end do c_aibickalbi
end do l_aibickalbi
!
! Elementary loop 50
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l, m
! Equalities: d == a, e == b, j == i, k == i
! No equalities independent of the above can hold.
!
m_aibicialbm: do m = nocc0, nocc1
l_aibicialbm: do l = nocc0, nocc1
if (l == m) cycle l_aibicialbm
c_aibicialbm: do c = nvirt0, nvirt1
b_aibicialbm: do b = nvirt0, nvirt1
if (b == c) cycle b_aibicialbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a_aibicialbm: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibicialbm: do i = nocc0, nocc1
if (i == l .or. i == m) cycle i_aibicialbm
if (i > l .and. l > m) exit i_aibicialbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibicialbm(eorb, t2, t1, nocc, nactive, a, i, b, c, l, m)
end do i_aibicialbm
end do a_aibicialbm
end do b_aibicialbm
end do c_aibicialbm
end do l_aibicialbm
end do m_aibicialbm
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
end subroutine ccjac_23_part3
end module ccjac_block_23_part3
