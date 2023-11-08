module ccjac_block_23_part1
use eom_cc3_23_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:35:25 UTC.
!
contains
 
subroutine ccjac_23_part1(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: a0, i0, i1, j1
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
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == b, m == i
! No equalities independent of the above can hold.
!
e_aibjakblei: do e = nvirt0, nvirt1
l_aibjakblei: do l = nocc0, nocc1
k_aibjakblei: do k = nocc0, nocc1
if (k == l) cycle k_aibjakblei
b_aibjakblei: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjakblei: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjakblei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakblei: do a = b + 1, nvirt1
if (a == e) cycle a_aibjakblei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakblei: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjakblei
if (k > l .and. l > i) cycle i_aibjakblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakblei(eorb, t2, t1, nocc, nactive, a, i, b, j, k, l, e)
end do i_aibjakblei
end do a_aibjakblei
end do j_aibjakblei
end do b_aibjakblei
end do k_aibjakblei
end do l_aibjakblei
end do e_aibjakblei
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l, m
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
e_aibjaiblem: do e = nvirt0, nvirt1
m_aibjaiblem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l_aibjaiblem: do l = nocc0, nocc1
if (l == m) cycle l_aibjaiblem
b_aibjaiblem: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblem: do j = nocc0, nocc1
if (j == l .or. j == m) cycle j_aibjaiblem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaiblem: do a = b + 1, nvirt1
if (a == e) cycle a_aibjaiblem
i_aibjaiblem: do i = nocc0, nocc1
if (i == j .or. i == l .or. i == m) cycle i_aibjaiblem
if (i > l .and. l > m) exit i_aibjaiblem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaiblem(eorb, t2, t1, nocc, nactive, a, i, b, j, l, e, m)
end do i_aibjaiblem
end do a_aibjaiblem
end do j_aibjaiblem
end do b_aibjaiblem
end do l_aibjaiblem
end do m_aibjaiblem
end do e_aibjaiblem
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, k, m
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
e_aibjakbjem: do e = nvirt0, nvirt1
m_aibjakbjem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aibjakbjem: do k = nocc0, nocc1
if (k == m) cycle k_aibjakbjem
b_aibjakbjem: do b = e + 1, nvirt1
j_aibjakbjem: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjakbjem
if (k > j .and. j > m) cycle j_aibjakbjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakbjem: do a = b + 1, nvirt1
if (a == e) cycle a_aibjakbjem
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjem: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjakbjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakbjem(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e, m)
end do i_aibjakbjem
end do a_aibjakbjem
end do j_aibjakbjem
end do b_aibjakbjem
end do k_aibjakbjem
end do m_aibjakbjem
end do e_aibjakbjem
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, k, l
! Equalities: c == a, d == b, m == j
! No equalities independent of the above can hold.
!
e_aibjakblej: do e = nvirt0, nvirt1
l_aibjakblej: do l = nocc0, nocc1
k_aibjakblej: do k = nocc0, nocc1
if (k == l) cycle k_aibjakblej
b_aibjakblej: do b = e + 1, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjakblej: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjakblej
if (k > l .and. l > j) cycle j_aibjakblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakblej: do a = b + 1, nvirt1
if (a == e) cycle a_aibjakblej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakblej: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjakblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakblej(eorb, t2, t1, nocc, nactive, a, i, b, j, k, l, e)
end do i_aibjakblej
end do a_aibjakblej
end do j_aibjakblej
end do b_aibjakblej
end do k_aibjakblej
end do l_aibjakblej
end do e_aibjakblej
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k, m
! Equalities: c == a, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjakdibm: do m = nocc0, nocc1
d_aibjakdibm: do d = nvirt0, nvirt1
k_aibjakdibm: do k = nocc0, nocc1
if (k == m) cycle k_aibjakdibm
b_aibjakdibm: do b = nvirt0, d - 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjakdibm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjakdibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjakdibm: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdibm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjakdibm
if (k > i .and. i > m) cycle i_aibjakdibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakdibm(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d, m)
end do i_aibjakdibm
end do a_aibjakdibm
end do j_aibjakdibm
end do b_aibjakdibm
end do k_aibjakdibm
end do d_aibjakdibm
end do m_aibjakdibm
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l, m
! Equalities: c == a, e == b, k == i
! No equalities independent of the above can hold.
!
m_aibjaidlbm: do m = nocc0, nocc1
d_aibjaidlbm: do d = nvirt0, nvirt1
l_aibjaidlbm: do l = nocc0, nocc1
if (l == m) cycle l_aibjaidlbm
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaidlbm: do b = nvirt0, d - 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaidlbm: do j = nocc0, nocc1
if (j == l .or. j == m) cycle j_aibjaidlbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjaidlbm: do a = a0 + 1, nvirt1
i_aibjaidlbm: do i = nocc0, nocc1
if (i == j .or. i == l .or. i == m) cycle i_aibjaidlbm
if (i > l .and. l > m) exit i_aibjaidlbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaidlbm(eorb, t2, t1, nocc, nactive, a, i, b, j, d, l, m)
end do i_aibjaidlbm
end do a_aibjaidlbm
end do j_aibjaidlbm
end do b_aibjaidlbm
end do l_aibjaidlbm
end do d_aibjaidlbm
end do m_aibjaidlbm
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k, m
! Equalities: c == a, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjakdjbm: do m = nocc0, nocc1
d_aibjakdjbm: do d = nvirt0, nvirt1
k_aibjakdjbm: do k = nocc0, nocc1
if (k == m) cycle k_aibjakdjbm
b_aibjakdjbm: do b = nvirt0, d - 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjakdjbm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjakdjbm
if (k > j .and. j > m) cycle j_aibjakdjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjakdjbm: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjbm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjakdjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakdjbm(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d, m)
end do i_aibjakdjbm
end do a_aibjakdjbm
end do j_aibjakdjbm
end do b_aibjakdjbm
end do k_aibjakdjbm
end do d_aibjakdjbm
end do m_aibjakdjbm
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k, l
! Equalities: c == a, e == b, m == j
! No equalities independent of the above can hold.
!
d_aibjakdlbj: do d = nvirt0, nvirt1
l_aibjakdlbj: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aibjakdlbj: do k = nocc0, nocc1
if (k == l) cycle k_aibjakdlbj
b_aibjakdlbj: do b = nvirt0, d - 1
j_aibjakdlbj: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjakdlbj
if (k > l .and. l > j) cycle j_aibjakdlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjakdlbj: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdlbj: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjakdlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakdlbj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d, l)
end do i_aibjakdlbj
end do a_aibjakdlbj
end do j_aibjakdlbj
end do b_aibjakdlbj
end do k_aibjakdlbj
end do l_aibjakdlbj
end do d_aibjakdlbj
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j, k
! Equalities: c == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjakdiej: do e = nvirt0, nvirt1
d_aibjakdiej: do d = e + 1, nvirt1
k_aibjakdiej: do k = nocc0, nocc1
b_aibjakdiej: do b = nvirt0, nvirt1
if (b == d .or. b == e) cycle b_aibjakdiej
j_aibjakdiej: do j = nocc0, nocc1
if (j == k) cycle j_aibjakdiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjakdiej: do a = a0 + 1, nvirt1
if (a == e) cycle a_aibjakdiej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdiej: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakdiej
if (k > i .and. i > j) cycle i_aibjakdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakdiej(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d, e)
end do i_aibjakdiej
end do a_aibjakdiej
end do j_aibjakdiej
end do b_aibjakdiej
end do k_aibjakdiej
end do d_aibjakdiej
end do e_aibjakdiej
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j, k
! Equalities: c == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjakdjei: do e = nvirt0, nvirt1
d_aibjakdjei: do d = e + 1, nvirt1
k_aibjakdjei: do k = nocc0, nocc1
b_aibjakdjei: do b = nvirt0, nvirt1
if (b == d .or. b == e) cycle b_aibjakdjei
j_aibjakdjei: do j = nocc0, nocc1
if (j == k) cycle j_aibjakdjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjakdjei: do a = a0 + 1, nvirt1
if (a == e) cycle a_aibjakdjei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjei: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjakdjei
if (k > j .and. j > i) cycle i_aibjakdjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakdjei(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d, e)
end do i_aibjakdjei
end do a_aibjakdjei
end do j_aibjakdjei
end do b_aibjakdjei
end do k_aibjakdjei
end do d_aibjakdjei
end do e_aibjakdjei
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j, m
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaidjem: do e = nvirt0, nvirt1
m_aibjaidjem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d_aibjaidjem: do d = e + 1, nvirt1
b_aibjaidjem: do b = nvirt0, nvirt1
if (b == d .or. b == e) cycle b_aibjaidjem
j_aibjaidjem: do j = nocc0, nocc1
if (j == m) cycle j_aibjaidjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjaidjem: do a = a0 + 1, nvirt1
if (a == e) cycle a_aibjaidjem
i_aibjaidjem: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjaidjem
if (i > j .and. j > m) exit i_aibjaidjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaidjem(eorb, t2, t1, nocc, nactive, a, i, b, j, d, e, m)
end do i_aibjaidjem
end do a_aibjaidjem
end do j_aibjaidjem
end do b_aibjaidjem
end do d_aibjaidjem
end do m_aibjaidjem
end do e_aibjaidjem
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j, l
! Equalities: c == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjaidlej: do e = nvirt0, nvirt1
d_aibjaidlej: do d = e + 1, nvirt1
l_aibjaidlej: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaidlej: do b = nvirt0, nvirt1
if (b == d .or. b == e) cycle b_aibjaidlej
j_aibjaidlej: do j = nocc0, nocc1
if (j == l) cycle j_aibjaidlej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, d)
a_aibjaidlej: do a = a0 + 1, nvirt1
if (a == e) cycle a_aibjaidlej
i_aibjaidlej: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjaidlej
if (i > l .and. l > j) exit i_aibjaidlej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaidlej(eorb, t2, t1, nocc, nactive, a, i, b, j, d, l, e)
end do i_aibjaidlej
end do a_aibjaidlej
end do j_aibjaidlej
end do b_aibjaidlej
end do l_aibjaidlej
end do d_aibjaidlej
end do e_aibjaidlej
!
! Elementary loop 13
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j, k
! Equalities: c == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbkdiej: do e = nvirt0, nvirt1
d_aibjbkdiej: do d = e + 1, nvirt1
k_aibjbkdiej: do k = nocc0, nocc1
b_aibjbkdiej: do b = d + 1, nvirt1
if (b == e) cycle b_aibjbkdiej
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdiej: do j = nocc0, nocc1
if (j == k) cycle j_aibjbkdiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdiej: do a = b + 1, nvirt1
if (a == d .or. a == e) cycle a_aibjbkdiej
i_aibjbkdiej: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjbkdiej
if (k > i .and. i > j) cycle i_aibjbkdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbkdiej(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d, e)
end do i_aibjbkdiej
end do a_aibjbkdiej
end do j_aibjbkdiej
end do b_aibjbkdiej
end do k_aibjbkdiej
end do d_aibjbkdiej
end do e_aibjbkdiej
!
! Elementary loop 14
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j, m
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjdiem: do e = nvirt0, nvirt1
m_aibjbjdiem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d_aibjbjdiem: do d = e + 1, nvirt1
b_aibjbjdiem: do b = d + 1, nvirt1
if (b == e) cycle b_aibjbjdiem
j_aibjbjdiem: do j = nocc0, nocc1
if (j == m) cycle j_aibjbjdiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbjdiem: do a = b + 1, nvirt1
if (a == d .or. a == e) cycle a_aibjbjdiem
i_aibjbjdiem: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjbjdiem
if (j > i .and. i > m) cycle i_aibjbjdiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbjdiem(eorb, t2, t1, nocc, nactive, a, i, b, j, d, e, m)
end do i_aibjbjdiem
end do a_aibjbjdiem
end do j_aibjbjdiem
end do b_aibjbjdiem
end do d_aibjbjdiem
end do m_aibjbjdiem
end do e_aibjbjdiem
!
! Elementary loop 15
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j, k
! Equalities: c == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbkdjei: do e = nvirt0, nvirt1
d_aibjbkdjei: do d = e + 1, nvirt1
k_aibjbkdjei: do k = nocc0, nocc1
b_aibjbkdjei: do b = d + 1, nvirt1
if (b == e) cycle b_aibjbkdjei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdjei: do j = nocc0, nocc1
if (j == k) cycle j_aibjbkdjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkdjei: do a = b + 1, nvirt1
if (a == d .or. a == e) cycle a_aibjbkdjei
i_aibjbkdjei: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjbkdjei
if (k > j .and. j > i) cycle i_aibjbkdjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbkdjei(eorb, t2, t1, nocc, nactive, a, i, b, j, k, d, e)
end do i_aibjbkdjei
end do a_aibjbkdjei
end do j_aibjbkdjei
end do b_aibjbkdjei
end do k_aibjbkdjei
end do d_aibjbkdjei
end do e_aibjbkdjei
!
! Elementary loop 16
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j, l
! Equalities: c == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjdlei: do e = nvirt0, nvirt1
d_aibjbjdlei: do d = e + 1, nvirt1
l_aibjbjdlei: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjbjdlei: do b = d + 1, nvirt1
if (b == e) cycle b_aibjbjdlei
j_aibjbjdlei: do j = nocc0, nocc1
if (j == l) cycle j_aibjbjdlei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbjdlei: do a = b + 1, nvirt1
if (a == d .or. a == e) cycle a_aibjbjdlei
i_aibjbjdlei: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjbjdlei
if (j > l .and. l > i) cycle i_aibjbjdlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjbjdlei(eorb, t2, t1, nocc, nactive, a, i, b, j, d, l, e)
end do i_aibjbjdlei
end do a_aibjbjdlei
end do j_aibjbjdlei
end do b_aibjbjdlei
end do l_aibjbjdlei
end do d_aibjbjdlei
end do e_aibjbjdlei
!
! Elementary loop 17
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, m
! Equalities: d == a, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjckaibm: do m = nocc0, nocc1
c_aibjckaibm: do c = nvirt0, nvirt1
k_aibjckaibm: do k = nocc0, nocc1
if (k == m) cycle k_aibjckaibm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibm: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjckaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjckaibm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjckaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaibm: do a = b + 1, c - 1
i_aibjckaibm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjckaibm
if (k > i .and. i > m) cycle i_aibjckaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckaibm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, m)
end do i_aibjckaibm
end do a_aibjckaibm
end do j_aibjckaibm
end do b_aibjckaibm
end do k_aibjckaibm
end do c_aibjckaibm
end do m_aibjckaibm
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l, m
! Equalities: d == a, e == b, k == i
! No equalities independent of the above can hold.
!
m_aibjcialbm: do m = nocc0, nocc1
l_aibjcialbm: do l = nocc0, nocc1
if (l == m) cycle l_aibjcialbm
c_aibjcialbm: do c = nvirt0, nvirt1
b_aibjcialbm: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcialbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcialbm: do j = nocc0, nocc1
if (j == l .or. j == m) cycle j_aibjcialbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcialbm: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialbm: do i = nocc0, nocc1
if (i == j .or. i == l .or. i == m) cycle i_aibjcialbm
if (i > l .and. l > m) exit i_aibjcialbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcialbm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l, m)
end do i_aibjcialbm
end do a_aibjcialbm
end do j_aibjcialbm
end do b_aibjcialbm
end do c_aibjcialbm
end do l_aibjcialbm
end do m_aibjcialbm
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k, l
! Equalities: d == a, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjckalbj: do l = nocc0, nocc1
c_aibjckalbj: do c = nvirt0, nvirt1
k_aibjckalbj: do k = nocc0, nocc1
if (k == l) cycle k_aibjckalbj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckalbj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjckalbj
j_aibjckalbj: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aibjckalbj
if (k > l .and. l > j) cycle j_aibjckalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckalbj: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalbj: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjckalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckalbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, l)
end do i_aibjckalbj
end do a_aibjckalbj
end do j_aibjckalbj
end do b_aibjckalbj
end do k_aibjckalbj
end do c_aibjckalbj
end do l_aibjckalbj
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l, m
! Equalities: d == a, e == b, k == j
! No equalities independent of the above can hold.
!
m_aibjcjalbm: do m = nocc0, nocc1
l_aibjcjalbm: do l = nocc0, nocc1
if (l == m) cycle l_aibjcjalbm
c_aibjcjalbm: do c = nvirt0, nvirt1
b_aibjcjalbm: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcjalbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjalbm: do j = nocc0, nocc1
if (j == l .or. j == m) cycle j_aibjcjalbm
if (j > l .and. l > m) exit j_aibjcjalbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjalbm: do a = b + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalbm: do i = nocc0, nocc1
if (i == j .or. i == l .or. i == m) cycle i_aibjcjalbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjalbm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l, m)
end do i_aibjcjalbm
end do a_aibjcjalbm
end do j_aibjcjalbm
end do b_aibjcjalbm
end do c_aibjcjalbm
end do l_aibjcjalbm
end do m_aibjcjalbm
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, k
! Equalities: d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjckaiej: do e = nvirt0, nvirt1
c_aibjckaiej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckaiej
k_aibjckaiej: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaiej: do b = nvirt0, nvirt1
if (b == c .or. b == e) cycle b_aibjckaiej
j_aibjckaiej: do j = nocc0, nocc1
if (j == k) cycle j_aibjckaiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjckaiej: do a = a0 + 1, c - 1
i_aibjckaiej: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckaiej
if (k > i .and. i > j) cycle i_aibjckaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckaiej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end do i_aibjckaiej
end do a_aibjckaiej
end do j_aibjckaiej
end do b_aibjckaiej
end do k_aibjckaiej
end do c_aibjckaiej
end do e_aibjckaiej
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, m
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjaiem: do e = nvirt0, nvirt1
m_aibjcjaiem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aibjcjaiem: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcjaiem
b_aibjcjaiem: do b = nvirt0, nvirt1
if (b == c .or. b == e) cycle b_aibjcjaiem
j_aibjcjaiem: do j = nocc0, nocc1
if (j == m) cycle j_aibjcjaiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjcjaiem: do a = a0 + 1, c - 1
i_aibjcjaiem: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcjaiem
if (j > i .and. i > m) cycle i_aibjcjaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjaiem(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e, m)
end do i_aibjcjaiem
end do a_aibjcjaiem
end do j_aibjcjaiem
end do b_aibjcjaiem
end do c_aibjcjaiem
end do m_aibjcjaiem
end do e_aibjcjaiem
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, l
! Equalities: d == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjalei: do e = nvirt0, nvirt1
l_aibjcjalei: do l = nocc0, nocc1
c_aibjcjalei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcjalei
b_aibjcjalei: do b = nvirt0, nvirt1
if (b == c .or. b == e) cycle b_aibjcjalei
j_aibjcjalei: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjalei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjcjalei: do a = a0 + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalei: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjalei
if (j > l .and. l > i) cycle i_aibjcjalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjalei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l, e)
end do i_aibjcjalei
end do a_aibjcjalei
end do j_aibjcjalei
end do b_aibjcjalei
end do c_aibjcjalei
end do l_aibjcjalei
end do e_aibjcjalei
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, l
! Equalities: d == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjcialej: do e = nvirt0, nvirt1
l_aibjcialej: do l = nocc0, nocc1
c_aibjcialej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcialej
b_aibjcialej: do b = nvirt0, nvirt1
if (b == c .or. b == e) cycle b_aibjcialej
j_aibjcialej: do j = nocc0, nocc1
if (j == l) cycle j_aibjcialej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjcialej: do a = a0 + 1, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialej: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcialej
if (i > l .and. l > j) exit i_aibjcialej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcialej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l, e)
end do i_aibjcialej
end do a_aibjcialej
end do j_aibjcialej
end do b_aibjcialej
end do c_aibjcialej
end do l_aibjcialej
end do e_aibjcialej
!
! Elementary loop 25
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, k
! Equalities: d == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjckbjei: do e = nvirt0, nvirt1
c_aibjckbjei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjckbjei
k_aibjckbjei: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckbjei: do b = e + 1, c - 1
j_aibjckbjei: do j = nocc0, nocc1
if (j == k) cycle j_aibjckbjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjckbjei
i_aibjckbjei: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckbjei
if (k > j .and. j > i) cycle i_aibjckbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckbjei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, e)
end do i_aibjckbjei
end do a_aibjckbjei
end do j_aibjckbjei
end do b_aibjckbjei
end do k_aibjckbjei
end do c_aibjckbjei
end do e_aibjckbjei
!
! Elementary loop 26
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, l
! Equalities: d == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjblei: do e = nvirt0, nvirt1
l_aibjcjblei: do l = nocc0, nocc1
c_aibjcjblei: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcjblei
b_aibjcjblei: do b = e + 1, c - 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcjblei: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjblei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjblei: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjcjblei
i_aibjcjblei: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjblei
if (j > l .and. l > i) cycle i_aibjcjblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjblei(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l, e)
end do i_aibjcjblei
end do a_aibjcjblei
end do j_aibjcjblei
end do b_aibjcjblei
end do c_aibjcjblei
end do l_aibjcjblei
end do e_aibjcjblei
!
! Elementary loop 27
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, m
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjcibjem: do e = nvirt0, nvirt1
m_aibjcibjem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aibjcibjem: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjcibjem
b_aibjcibjem: do b = e + 1, c - 1
j_aibjcibjem: do j = nocc0, nocc1
if (j == m) cycle j_aibjcibjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibjem: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjcibjem
i_aibjcibjem: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcibjem
if (i > j .and. j > m) exit i_aibjcibjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcibjem(eorb, t2, t1, nocc, nactive, a, i, b, j, c, e, m)
end do i_aibjcibjem
end do a_aibjcibjem
end do j_aibjcibjem
end do b_aibjcibjem
end do c_aibjcibjem
end do m_aibjcibjem
end do e_aibjcibjem
!
! Elementary loop 28
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, l
! Equalities: d == b, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjciblej: do e = nvirt0, nvirt1
l_aibjciblej: do l = nocc0, nocc1
c_aibjciblej: do c = nvirt0, nvirt1
if (c == e) cycle c_aibjciblej
b_aibjciblej: do b = e + 1, c - 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjciblej: do j = nocc0, nocc1
if (j == l) cycle j_aibjciblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciblej: do a = b + 1, nvirt1
if (a == c .or. a == e) cycle a_aibjciblej
i_aibjciblej: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjciblej
if (i > l .and. l > j) exit i_aibjciblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjciblej(eorb, t2, t1, nocc, nactive, a, i, b, j, c, l, e)
end do i_aibjciblej
end do a_aibjciblej
end do j_aibjciblej
end do b_aibjciblej
end do c_aibjciblej
end do l_aibjciblej
end do e_aibjciblej
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, m
! Equalities: e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjdiam: do m = nocc0, nocc1
d_aibjcjdiam: do d = nvirt0, nvirt1
c_aibjcjdiam: do c = d + 1, nvirt1
b_aibjcjdiam: do b = nvirt0, nvirt1
if (b == c .or. b == d) cycle b_aibjcjdiam
j_aibjcjdiam: do j = nocc0, nocc1
if (j == m) cycle j_aibjcjdiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdiam: do a = b + 1, d - 1
if (a == c) cycle a_aibjcjdiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjcjdiam: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcjdiam
if (j > i .and. i > m) cycle i_aibjcjdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjdiam(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d, m)
end do i_aibjcjdiam
end do a_aibjcjdiam
end do j_aibjcjdiam
end do b_aibjcjdiam
end do c_aibjcjdiam
end do d_aibjcjdiam
end do m_aibjcjdiam
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, k
! Equalities: e == a, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjckdjai: do d = nvirt0, nvirt1
c_aibjckdjai: do c = d + 1, nvirt1
k_aibjckdjai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdjai: do b = nvirt0, nvirt1
if (b == c .or. b == d) cycle b_aibjckdjai
j_aibjckdjai: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdjai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdjai: do a = b + 1, d - 1
if (a == c) cycle a_aibjckdjai
i_aibjckdjai: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdjai
if (k > j .and. j > i) cycle i_aibjckdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckdjai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end do i_aibjckdjai
end do a_aibjckdjai
end do j_aibjckdjai
end do b_aibjckdjai
end do k_aibjckdjai
end do c_aibjckdjai
end do d_aibjckdjai
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, l
! Equalities: e == a, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdlai: do d = nvirt0, nvirt1
l_aibjcjdlai: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aibjcjdlai: do c = d + 1, nvirt1
b_aibjcjdlai: do b = nvirt0, nvirt1
if (b == c .or. b == d) cycle b_aibjcjdlai
j_aibjcjdlai: do j = nocc0, nocc1
if (j == l) cycle j_aibjcjdlai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdlai: do a = b + 1, d - 1
if (a == c) cycle a_aibjcjdlai
i_aibjcjdlai: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcjdlai
if (j > l .and. l > i) cycle i_aibjcjdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjdlai(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d, l)
end do i_aibjcjdlai
end do a_aibjcjdlai
end do j_aibjcjdlai
end do b_aibjcjdlai
end do c_aibjcjdlai
end do l_aibjcjdlai
end do d_aibjcjdlai
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, m
! Equalities: e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcidjam: do m = nocc0, nocc1
d_aibjcidjam: do d = nvirt0, nvirt1
c_aibjcidjam: do c = d + 1, nvirt1
b_aibjcidjam: do b = nvirt0, nvirt1
if (b == c .or. b == d) cycle b_aibjcidjam
j_aibjcidjam: do j = nocc0, nocc1
if (j == m) cycle j_aibjcidjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidjam: do a = b + 1, d - 1
if (a == c) cycle a_aibjcidjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjcidjam: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcidjam
if (i > j .and. j > m) exit i_aibjcidjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcidjam(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d, m)
end do i_aibjcidjam
end do a_aibjcidjam
end do j_aibjcidjam
end do b_aibjcidjam
end do c_aibjcidjam
end do d_aibjcidjam
end do m_aibjcidjam
!
! Elementary loop 33
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, k
! Equalities: e == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjckdibj: do d = nvirt0, nvirt1
c_aibjckdibj: do c = d + 1, nvirt1
k_aibjckdibj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdibj: do b = nvirt0, d - 1
if (b == c) cycle b_aibjckdibj
j_aibjckdibj: do j = nocc0, nocc1
if (j == k) cycle j_aibjckdibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdibj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjckdibj
i_aibjckdibj: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjckdibj
if (k > i .and. i > j) cycle i_aibjckdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjckdibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k, d)
end do i_aibjckdibj
end do a_aibjckdibj
end do j_aibjckdibj
end do b_aibjckdibj
end do k_aibjckdibj
end do c_aibjckdibj
end do d_aibjckdibj
!
! Elementary loop 34
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, m
! Equalities: e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjdibm: do m = nocc0, nocc1
d_aibjcjdibm: do d = nvirt0, nvirt1
c_aibjcjdibm: do c = d + 1, nvirt1
b_aibjcjdibm: do b = nvirt0, d - 1
if (b == c) cycle b_aibjcjdibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjdibm: do j = nocc0, nocc1
if (j == m) cycle j_aibjcjdibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjdibm: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcjdibm
i_aibjcjdibm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcjdibm
if (j > i .and. i > m) cycle i_aibjcjdibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcjdibm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d, m)
end do i_aibjcjdibm
end do a_aibjcjdibm
end do j_aibjcjdibm
end do b_aibjcjdibm
end do c_aibjcjdibm
end do d_aibjcjdibm
end do m_aibjcjdibm
!
! Elementary loop 35
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, m
! Equalities: e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcidjbm: do m = nocc0, nocc1
d_aibjcidjbm: do d = nvirt0, nvirt1
c_aibjcidjbm: do c = d + 1, nvirt1
b_aibjcidjbm: do b = nvirt0, d - 1
if (b == c) cycle b_aibjcidjbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcidjbm: do j = nocc0, nocc1
if (j == m) cycle j_aibjcidjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidjbm: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcidjbm
i_aibjcidjbm: do i = nocc0, nocc1
if (i == j .or. i == m) cycle i_aibjcidjbm
if (i > j .and. j > m) exit i_aibjcidjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcidjbm(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d, m)
end do i_aibjcidjbm
end do a_aibjcidjbm
end do j_aibjcidjbm
end do b_aibjcidjbm
end do c_aibjcidjbm
end do d_aibjcidjbm
end do m_aibjcidjbm
!
! Elementary loop 36
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, l
! Equalities: e == b, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjcidlbj: do d = nvirt0, nvirt1
l_aibjcidlbj: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aibjcidlbj: do c = d + 1, nvirt1
b_aibjcidlbj: do b = nvirt0, d - 1
if (b == c) cycle b_aibjcidlbj
j_aibjcidlbj: do j = nocc0, nocc1
if (j == l) cycle j_aibjcidlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidlbj: do a = b + 1, nvirt1
if (a == c .or. a == d) cycle a_aibjcidlbj
i_aibjcidlbj: do i = nocc0, nocc1
if (i == j .or. i == l) cycle i_aibjcidlbj
if (i > l .and. l > j) exit i_aibjcidlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjcidlbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c, d, l)
end do i_aibjcidlbj
end do a_aibjcidlbj
end do j_aibjcidlbj
end do b_aibjcidlbj
end do c_aibjcidlbj
end do l_aibjcidlbj
end do d_aibjcidlbj
!
! Elementary loop 37
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, k, m
! Equalities: b == a, c == a, d == a, l == i
! No equalities independent of the above can hold.
!
e_aiajakaiem: do e = nvirt0, nvirt1
m_aiajakaiem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aiajakaiem: do k = nocc0, nocc1
if (k == m) cycle k_aiajakaiem
j_aiajakaiem: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aiajakaiem
a_aiajakaiem: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k, m)
i_aiajakaiem: do i = j + 1, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajakaiem(eorb, t2, t1, nocc, nactive, a, i, j, k, e, m)
end do i_aiajakaiem
end do a_aiajakaiem
end do j_aiajakaiem
end do k_aiajakaiem
end do m_aiajakaiem
end do e_aiajakaiem
!
! Elementary loop 38
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, k, l
! Equalities: b == a, c == a, d == a, m == i
! No equalities independent of the above can hold.
!
e_aiajakalei: do e = nvirt0, nvirt1
l_aiajakalei: do l = nocc0, nocc1
k_aiajakalei: do k = l + 1, nocc1
j_aiajakalei: do j = nocc0, nocc1
if (j == k .or. j == l) cycle j_aiajakalei
a_aiajakalei: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j, l)
i_aiajakalei: do i = i0 + 1, nocc1
if (i == k) cycle i_aiajakalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajakalei(eorb, t2, t1, nocc, nactive, a, i, j, k, l, e)
end do i_aiajakalei
end do a_aiajakalei
end do j_aiajakalei
end do k_aiajakalei
end do l_aiajakalei
end do e_aiajakalei
!
! Elementary loop 39
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, l, m
! Equalities: b == a, c == a, d == a, k == i
! No equalities independent of the above can hold.
!
e_aiajaialem: do e = nvirt0, nvirt1
m_aiajaialem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l_aiajaialem: do l = nocc0, m - 1
j_aiajaialem: do j = nocc0, nocc1
if (j == l .or. j == m) cycle j_aiajaialem
a_aiajaialem: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j, l)
i_aiajaialem: do i = i0 + 1, nocc1
if (i == m) cycle i_aiajaialem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaialem(eorb, t2, t1, nocc, nactive, a, i, j, l, e, m)
end do i_aiajaialem
end do a_aiajaialem
end do j_aiajaialem
end do l_aiajaialem
end do m_aiajaialem
end do e_aiajaialem
!
! Elementary loop 40
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, k, m
! Equalities: b == a, c == a, d == a, l == j
! No equalities independent of the above can hold.
!
e_aiajakajem: do e = nvirt0, nvirt1
m_aiajakajem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aiajakajem: do k = nocc0, nocc1
if (k == m) cycle k_aiajakajem
j1 = min(k, m)
j_aiajakajem: do j = nocc0, j1 - 1
a_aiajakajem: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajakajem: do i = j + 1, nocc1
if (i == k .or. i == m) cycle i_aiajakajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajakajem(eorb, t2, t1, nocc, nactive, a, i, j, k, e, m)
end do i_aiajakajem
end do a_aiajakajem
end do j_aiajakajem
end do k_aiajakajem
end do m_aiajakajem
end do e_aiajakajem
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, k, l
! Equalities: b == a, c == a, d == a, m == j
! No equalities independent of the above can hold.
!
e_aiajakalej: do e = nvirt0, nvirt1
l_aiajakalej: do l = nocc0, nocc1
k_aiajakalej: do k = l + 1, nocc1
j_aiajakalej: do j = l + 1, nocc1
if (j == k) cycle j_aiajakalej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajakalej: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajakalej: do i = j + 1, nocc1
if (i == k .or. i == l) cycle i_aiajakalej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajakalej(eorb, t2, t1, nocc, nactive, a, i, j, k, l, e)
end do i_aiajakalej
end do a_aiajakalej
end do j_aiajakalej
end do k_aiajakalej
end do l_aiajakalej
end do e_aiajakalej
!
! Elementary loop 42
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, l, m
! Equalities: b == a, c == a, d == a, k == j
! No equalities independent of the above can hold.
!
e_aiajajalem: do e = nvirt0, nvirt1
m_aiajajalem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l_aiajajalem: do l = nocc0, m - 1
j_aiajajalem: do j = l + 1, nocc1
if (j == m) cycle j_aiajajalem
a_aiajajalem: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajajalem: do i = j + 1, nocc1
if (i == l .or. i == m) cycle i_aiajajalem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajajalem(eorb, t2, t1, nocc, nactive, a, i, j, l, e, m)
end do i_aiajajalem
end do a_aiajajalem
end do j_aiajajalem
end do l_aiajajalem
end do m_aiajajalem
end do e_aiajajalem
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, m
! Equalities: c == a, d == a, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjakaibm: do m = nocc0, nocc1
k_aibjakaibm: do k = nocc0, nocc1
if (k == m) cycle k_aibjakaibm
b_aibjakaibm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjakaibm: do j = nocc0, nocc1
if (j == k .or. j == m) cycle j_aibjakaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakaibm: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k, m)
i_aibjakaibm: do i = nocc0, i1 - 1
if (i == j) cycle i_aibjakaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakaibm(eorb, t2, t1, nocc, nactive, a, i, b, j, k, m)
end do i_aibjakaibm
end do a_aibjakaibm
end do j_aibjakaibm
end do b_aibjakaibm
end do k_aibjakaibm
end do m_aibjakaibm
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l, m
! Equalities: c == a, d == a, e == b, k == i
! No equalities independent of the above can hold.
!
m_aibjaialbm: do m = nocc0, nocc1
l_aibjaialbm: do l = nocc0, m - 1
b_aibjaialbm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaialbm: do j = nocc0, nocc1
if (j == l .or. j == m) cycle j_aibjaialbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaialbm: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjaialbm: do i = l + 1, nocc1
if (i == j .or. i == m) cycle i_aibjaialbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaialbm(eorb, t2, t1, nocc, nactive, a, i, b, j, l, m)
end do i_aibjaialbm
end do a_aibjaialbm
end do j_aibjaialbm
end do b_aibjaialbm
end do l_aibjaialbm
end do m_aibjaialbm
!
! Elementary loop 45
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, m
! Equalities: c == a, d == a, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjakajbm: do m = nocc0, nocc1
k_aibjakajbm: do k = nocc0, nocc1
if (k == m) cycle k_aibjakajbm
b_aibjakajbm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j1 = min(k, m)
j_aibjakajbm: do j = nocc0, j1 - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakajbm: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakajbm: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == m) cycle i_aibjakajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakajbm(eorb, t2, t1, nocc, nactive, a, i, b, j, k, m)
end do i_aibjakajbm
end do a_aibjakajbm
end do j_aibjakajbm
end do b_aibjakajbm
end do k_aibjakajbm
end do m_aibjakajbm
!
! Elementary loop 46
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, l
! Equalities: c == a, d == a, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjakalbj: do l = nocc0, nocc1
k_aibjakalbj: do k = l + 1, nocc1
b_aibjakalbj: do b = nvirt0, nvirt1
j_aibjakalbj: do j = l + 1, nocc1
if (j == k) cycle j_aibjakalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjakalbj: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjakalbj: do i = nocc0, nocc1
if (i == j .or. i == k .or. i == l) cycle i_aibjakalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakalbj(eorb, t2, t1, nocc, nactive, a, i, b, j, k, l)
end do i_aibjakalbj
end do a_aibjakalbj
end do j_aibjakalbj
end do b_aibjakalbj
end do k_aibjakalbj
end do l_aibjakalbj
!
! Elementary loop 47
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l, m
! Equalities: c == a, d == a, e == b, k == j
! No equalities independent of the above can hold.
!
m_aibjajalbm: do m = nocc0, nocc1
l_aibjajalbm: do l = nocc0, m - 1
b_aibjajalbm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjajalbm: do j = l + 1, nocc1
if (j == m) cycle j_aibjajalbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajalbm: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjajalbm: do i = nocc0, nocc1
if (i == j .or. i == l .or. i == m) cycle i_aibjajalbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajalbm(eorb, t2, t1, nocc, nactive, a, i, b, j, l, m)
end do i_aibjajalbm
end do a_aibjajalbm
end do j_aibjajalbm
end do b_aibjajalbm
end do l_aibjajalbm
end do m_aibjajalbm
!
! Elementary loop 48
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjakaiej: do e = nvirt0, nvirt1
k_aibjakaiej: do k = nocc0, nocc1
b_aibjakaiej: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjakaiej
j_aibjakaiej: do j = nocc0, nocc1
if (j == k) cycle j_aibjakaiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjakaiej: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k, j)
i_aibjakaiej: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakaiej(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjakaiej
end do a_aibjakaiej
end do j_aibjakaiej
end do b_aibjakaiej
end do k_aibjakaiej
end do e_aibjakaiej
!
! Elementary loop 49
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajaiem: do e = nvirt0, nvirt1
m_aibjajaiem: do m = nocc0, nocc1
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjajaiem: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjajaiem
j_aibjajaiem: do j = nocc0, nocc1
if (j == m) cycle j_aibjajaiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjajaiem: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j, m)
i_aibjajaiem: do i = nocc0, i1 - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajaiem(eorb, t2, t1, nocc, nactive, a, i, b, j, e, m)
end do i_aibjajaiem
end do a_aibjajaiem
end do j_aibjajaiem
end do b_aibjajaiem
end do m_aibjajaiem
end do e_aibjajaiem
!
! Elementary loop 50
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjakajei: do e = nvirt0, nvirt1
k_aibjakajei: do k = nocc0, nocc1
b_aibjakajei: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjakajei
j_aibjakajei: do j = nocc0, k - 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjakajei: do a = a0 + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakajei: do i = j + 1, nocc1
if (i == k) cycle i_aibjakajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjakajei(eorb, t2, t1, nocc, nactive, a, i, b, j, k, e)
end do i_aibjakajei
end do a_aibjakajei
end do j_aibjakajei
end do b_aibjakajei
end do k_aibjakajei
end do e_aibjakajei
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
end subroutine ccjac_23_part1
end module ccjac_block_23_part1
