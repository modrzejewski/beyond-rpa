module ccjac_block_32_part6
use eom_cc3_32_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:41:38 UTC.
!
contains
 
subroutine ccjac_32_part6(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: a0
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
! Free occupied indices: j, i, k
! Equalities: b == a, d == a, e == c, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajckajcj: do c = nvirt0, nvirt1
k_aiajckajcj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckajcj: do j = nocc0, k - 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckajcj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajcj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckajcj(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckajcj
end do a_aiajckajcj
end do j_aiajckajcj
end do k_aiajckajcj
end do c_aiajckajcj
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aiajckajck: do c = nvirt0, nvirt1
k_aiajckajck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckajck: do j = nocc0, k - 1
a_aiajckajck: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajck: do i = j + 1, nocc1
if (i == k) cycle i_aiajckajck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckajck(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckajck
end do a_aiajckajck
end do j_aiajckajck
end do k_aiajckajck
end do c_aiajckajck
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == a, e == c, k == j, m == j
! No equalities independent of the above can hold.
!
l_aiajcjalcj: do l = nocc0, nocc1
c_aiajcjalcj: do c = nvirt0, nvirt1
j_aiajcjalcj: do j = nocc0, nocc1
if (j == l) cycle j_aiajcjalcj
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjalcj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajcjalcj: do i = j + 1, nocc1
if (i == l) cycle i_aiajcjalcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjalcj(eorb, t2, t1, nocc, nactive, a, i, j, c, l)
end do i_aiajcjalcj
end do a_aiajcjalcj
end do j_aiajcjalcj
end do c_aiajcjalcj
end do l_aiajcjalcj
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == c, m == j, l == k
! No equalities independent of the above can hold.
!
c_aiajckakcj: do c = nvirt0, nvirt1
k_aiajckakcj: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckakcj: do j = nocc0, k - 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajckakcj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakcj: do i = j + 1, nocc1
if (i == k) cycle i_aiajckakcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckakcj(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckakcj
end do a_aiajckakcj
end do j_aiajckakcj
end do k_aiajckakcj
end do c_aiajckakcj
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j
! Equalities: b == a, d == a, e == c, l == k, m == k
! No equalities independent of the above can hold.
!
c_aiajckakck: do c = nvirt0, nvirt1
k_aiajckakck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckakck: do j = nocc0, k - 1
a_aiajckakck: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakck: do i = j + 1, nocc1
if (i == k) cycle i_aiajckakck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajckakck(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajckakck
end do a_aiajckakck
end do j_aiajckakck
end do k_aiajckakck
end do c_aiajckakck
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiajciaiei: do e = nvirt0, nvirt1
c_aiajciaiei: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajciaiei
j_aiajciaiei: do j = nocc0, nocc1
a0 = max(c, e)
a_aiajciaiei: do a = a0 + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciaiei: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciaiei(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajciaiei
end do a_aiajciaiei
end do j_aiajciaiei
end do c_aiajciaiei
end do e_aiajciaiei
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajciaiej: do e = nvirt0, nvirt1
c_aiajciaiej: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajciaiej
j_aiajciaiej: do j = nocc0, nocc1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c, e)
a_aiajciaiej: do a = a0 + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciaiej: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciaiej(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajciaiej
end do a_aiajciaiej
end do j_aiajciaiej
end do c_aiajciaiej
end do e_aiajciaiej
!
! Elementary loop 8
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
a0 = max(c, e)
a_aiajcjaiej: do a = a0 + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjaiej: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjaiej(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajcjaiej
end do a_aiajcjaiej
end do j_aiajcjaiej
end do c_aiajcjaiej
end do e_aiajcjaiej
!
! Elementary loop 9
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
a0 = max(c, e)
a_aiajciajei: do a = a0 + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciajei: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajciajei(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajciajei
end do a_aiajciajei
end do j_aiajciajei
end do c_aiajciajei
end do e_aiajciajei
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aiajcjajei: do e = nvirt0, nvirt1
c_aiajcjajei: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajcjajei
j_aiajcjajei: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c, e)
a_aiajcjajei: do a = a0 + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjajei: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjajei(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajcjajei
end do a_aiajcjajei
end do j_aiajcjajei
end do c_aiajcjajei
end do e_aiajcjajei
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, i
! Equalities: b == a, d == a, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
e_aiajcjajej: do e = nvirt0, nvirt1
c_aiajcjajej: do c = nvirt0, nvirt1
if (c == e) cycle c_aiajcjajej
j_aiajcjajej: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c, e)
a_aiajcjajej: do a = a0 + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjajej: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjajej(eorb, t2, t1, nocc, nactive, a, i, j, c, e)
end do i_aiajcjajej
end do a_aiajcjajej
end do j_aiajcjajej
end do c_aiajcjajej
end do e_aiajcjajej
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: d == a, c == b, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibibkaibm: do m = nocc0, nocc1
k_aibibkaibm: do k = nocc0, nocc1
if (k == m) cycle k_aibibkaibm
b_aibibkaibm: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a_aibibkaibm: do a = b + 1, nvirt1
i_aibibkaibm: do i = k + 1, nocc1
if (i == m) cycle i_aibibkaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkaibm(eorb, t2, t1, nocc, nactive, a, i, b, k, m)
end do i_aibibkaibm
end do a_aibibkaibm
end do b_aibibkaibm
end do k_aibibkaibm
end do m_aibibkaibm
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: d == a, c == b, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibibkalbi: do l = nocc0, nocc1
k_aibibkalbi: do k = nocc0, nocc1
if (k == l) cycle k_aibibkalbi
b_aibibkalbi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkalbi: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibibkalbi: do i = k + 1, nocc1
if (i == l) cycle i_aibibkalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkalbi(eorb, t2, t1, nocc, nactive, a, i, b, k, l)
end do i_aibibkalbi
end do a_aibibkalbi
end do b_aibibkalbi
end do k_aibibkalbi
end do l_aibibkalbi
!
! Elementary loop 14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: d == a, c == b, e == b, j == i, l == k
! No equalities independent of the above can hold.
!
m_aibibkakbm: do m = nocc0, nocc1
k_aibibkakbm: do k = nocc0, nocc1
if (k == m) cycle k_aibibkakbm
b_aibibkakbm: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a_aibibkakbm: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibibkakbm: do i = k + 1, nocc1
if (i == m) cycle i_aibibkakbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkakbm(eorb, t2, t1, nocc, nactive, a, i, b, k, m)
end do i_aibibkakbm
end do a_aibibkakbm
end do b_aibibkakbm
end do k_aibibkakbm
end do m_aibibkakbm
!
! Elementary loop 15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: d == a, c == b, e == b, j == i, m == k
! No equalities independent of the above can hold.
!
l_aibibkalbk: do l = nocc0, nocc1
k_aibibkalbk: do k = nocc0, nocc1
if (k == l) cycle k_aibibkalbk
b_aibibkalbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkalbk: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibibkalbk: do i = k + 1, nocc1
if (i == l) cycle i_aibibkalbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkalbk(eorb, t2, t1, nocc, nactive, a, i, b, k, l)
end do i_aibibkalbk
end do a_aibibkalbk
end do b_aibibkalbk
end do k_aibibkalbk
end do l_aibibkalbk
!
! Elementary loop 16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: d == a, c == b, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjbiaibm: do m = nocc0, nocc1
b_aibjbiaibm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbiaibm: do j = nocc0, nocc1
if (j == m) cycle j_aibjbiaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiaibm: do a = b + 1, nvirt1
i_aibjbiaibm: do i = nocc0, j - 1
if (i == m) cycle i_aibjbiaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbiaibm(eorb, t2, t1, nocc, nactive, a, i, b, j, m)
end do i_aibjbiaibm
end do a_aibjbiaibm
end do j_aibjbiaibm
end do b_aibjbiaibm
end do m_aibjbiaibm
!
! Elementary loop 17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, e == b, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibjbkaibi: do k = nocc0, nocc1
b_aibjbkaibi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaibi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkaibi: do a = b + 1, nvirt1
i_aibjbkaibi: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkaibi(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkaibi
end do a_aibjbkaibi
end do j_aibjbkaibi
end do b_aibjbkaibi
end do k_aibjbkaibi
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjbkaibj: do k = nocc0, nocc1
b_aibjbkaibj: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaibj: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkaibj: do a = b + 1, nvirt1
i_aibjbkaibj: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkaibj(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkaibj
end do a_aibjbkaibj
end do j_aibjbkaibj
end do b_aibjbkaibj
end do k_aibjbkaibj
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: d == a, c == b, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjbkaibk: do k = nocc0, nocc1
b_aibjbkaibk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaibk: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkaibk: do a = b + 1, nvirt1
i_aibjbkaibk: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkaibk(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkaibk
end do a_aibjbkaibk
end do j_aibjbkaibk
end do b_aibjbkaibk
end do k_aibjbkaibk
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjbialbi: do l = nocc0, nocc1
b_aibjbialbi: do b = nvirt0, nvirt1
j_aibjbialbi: do j = nocc0, nocc1
if (j == l) cycle j_aibjbialbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbialbi: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjbialbi: do i = nocc0, j - 1
if (i == l) cycle i_aibjbialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbialbi(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjbialbi
end do a_aibjbialbi
end do j_aibjbialbi
end do b_aibjbialbi
end do l_aibjbialbi
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: d == a, c == b, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjbiajbm: do m = nocc0, nocc1
b_aibjbiajbm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbiajbm: do j = nocc0, nocc1
if (j == m) cycle j_aibjbiajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiajbm: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbiajbm: do i = nocc0, j - 1
if (i == m) cycle i_aibjbiajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbiajbm(eorb, t2, t1, nocc, nactive, a, i, b, j, m)
end do i_aibjbiajbm
end do a_aibjbiajbm
end do j_aibjbiajbm
end do b_aibjbiajbm
end do m_aibjbiajbm
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjbialbj: do l = nocc0, nocc1
b_aibjbialbj: do b = nvirt0, nvirt1
j_aibjbialbj: do j = nocc0, nocc1
if (j == l) cycle j_aibjbialbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbialbj: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjbialbj: do i = nocc0, j - 1
if (i == l) cycle i_aibjbialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbialbj(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjbialbj
end do a_aibjbialbj
end do j_aibjbialbj
end do b_aibjbialbj
end do l_aibjbialbj
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjbkajbi: do k = nocc0, nocc1
b_aibjbkajbi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkajbi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkajbi: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbkajbi: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkajbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkajbi
end do a_aibjbkajbi
end do j_aibjbkajbi
end do b_aibjbkajbi
end do k_aibjbkajbi
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: d == a, c == b, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjbkakbi: do k = nocc0, nocc1
b_aibjbkakbi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkakbi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkakbi: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjbkakbi: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkakbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkakbi
end do a_aibjbkakbi
end do j_aibjbkakbi
end do b_aibjbkakbi
end do k_aibjbkakbi
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: d == a, c == b, e == b, l == j, m == j
! No equalities independent of the above can hold.
!
k_aibjbkajbj: do k = nocc0, nocc1
b_aibjbkajbj: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkajbj: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkajbj: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbkajbj: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkajbj(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkajbj
end do a_aibjbkajbj
end do j_aibjbkajbj
end do b_aibjbkajbj
end do k_aibjbkajbj
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: d == a, c == b, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjbkajbk: do k = nocc0, nocc1
b_aibjbkajbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkajbk: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkajbk: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbkajbk: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkajbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkajbk(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkajbk
end do a_aibjbkajbk
end do j_aibjbkajbk
end do b_aibjbkajbk
end do k_aibjbkajbk
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: d == a, c == b, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
k_aibjbkakbj: do k = nocc0, nocc1
b_aibjbkakbj: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkakbj: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkakbj: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjbkakbj: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkakbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkakbj(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkakbj
end do a_aibjbkakbj
end do j_aibjbkakbj
end do b_aibjbkakbj
end do k_aibjbkakbj
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j
! Equalities: d == a, c == b, e == b, l == k, m == k
! No equalities independent of the above can hold.
!
k_aibjbkakbk: do k = nocc0, nocc1
b_aibjbkakbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkakbk: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkakbk: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjbkakbk: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkakbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbkakbk(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkakbk
end do a_aibjbkakbk
end do j_aibjbkakbk
end do b_aibjbkakbk
end do k_aibjbkakbk
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjciaici: do c = nvirt0, nvirt1
b_aibjciaici: do b = c + 1, nvirt1
j_aibjciaici: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjciaici: do a = a0 + 1, nvirt1
i_aibjciaici: do i = nocc0, nocc1
if (i == j) cycle i_aibjciaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciaici(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciaici
end do a_aibjciaici
end do j_aibjciaici
end do b_aibjciaici
end do c_aibjciaici
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciaicj: do c = nvirt0, nvirt1
b_aibjciaicj: do b = c + 1, nvirt1
j_aibjciaicj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjciaicj: do a = a0 + 1, nvirt1
i_aibjciaicj: do i = nocc0, nocc1
if (i == j) cycle i_aibjciaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciaicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciaicj
end do a_aibjciaicj
end do j_aibjciaicj
end do b_aibjciaicj
end do c_aibjciaicj
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjaicj: do c = nvirt0, nvirt1
b_aibjcjaicj: do b = c + 1, nvirt1
j_aibjcjaicj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjcjaicj: do a = a0 + 1, nvirt1
i_aibjcjaicj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjaicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjaicj
end do a_aibjcjaicj
end do j_aibjcjaicj
end do b_aibjcjaicj
end do c_aibjcjaicj
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajci: do c = nvirt0, nvirt1
b_aibjciajci: do b = c + 1, nvirt1
j_aibjciajci: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjciajci: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajci: do i = nocc0, nocc1
if (i == j) cycle i_aibjciajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciajci(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciajci
end do a_aibjciajci
end do j_aibjciajci
end do b_aibjciajci
end do c_aibjciajci
!
! Elementary loop 33
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjciajcj: do c = nvirt0, nvirt1
b_aibjciajcj: do b = c + 1, nvirt1
j_aibjciajcj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjciajcj: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajcj: do i = nocc0, nocc1
if (i == j) cycle i_aibjciajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciajcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciajcj
end do a_aibjciajcj
end do j_aibjciajcj
end do b_aibjciajcj
end do c_aibjciajcj
!
! Elementary loop 34
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjajci: do c = nvirt0, nvirt1
b_aibjcjajci: do b = c + 1, nvirt1
j_aibjcjajci: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjcjajci: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajci: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjajci(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjajci
end do a_aibjcjajci
end do j_aibjcjajci
end do b_aibjcjajci
end do c_aibjcjajci
!
! Elementary loop 35
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i
! Equalities: d == a, e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjajcj: do c = nvirt0, nvirt1
b_aibjcjajcj: do b = c + 1, nvirt1
j_aibjcjajcj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, c)
a_aibjcjajcj: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajcj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjajcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjajcj
end do a_aibjcjajcj
end do j_aibjcjajcj
end do b_aibjcjajcj
end do c_aibjcjajcj
!
! Elementary loop 36
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibibkaiei: do e = nvirt0, nvirt1
k_aibibkaiei: do k = nocc0, nocc1
b_aibibkaiei: do b = nvirt0, nvirt1
if (b == e) cycle b_aibibkaiei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b, e)
a_aibibkaiei: do a = a0 + 1, nvirt1
i_aibibkaiei: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkaiei(eorb, t2, t1, nocc, nactive, a, i, b, k, e)
end do i_aibibkaiei
end do a_aibibkaiei
end do b_aibibkaiei
end do k_aibibkaiei
end do e_aibibkaiei
!
! Elementary loop 37
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibibkaiek: do e = nvirt0, nvirt1
k_aibibkaiek: do k = nocc0, nocc1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibibkaiek: do b = nvirt0, nvirt1
if (b == e) cycle b_aibibkaiek
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b, e)
a_aibibkaiek: do a = a0 + 1, nvirt1
i_aibibkaiek: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkaiek(eorb, t2, t1, nocc, nactive, a, i, b, k, e)
end do i_aibibkaiek
end do a_aibibkaiek
end do b_aibibkaiek
end do k_aibibkaiek
end do e_aibibkaiek
!
! Elementary loop 38
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibibkakei: do e = nvirt0, nvirt1
k_aibibkakei: do k = nocc0, nocc1
b_aibibkakei: do b = nvirt0, nvirt1
if (b == e) cycle b_aibibkakei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b, e)
a_aibibkakei: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibibkakei: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkakei(eorb, t2, t1, nocc, nactive, a, i, b, k, e)
end do i_aibibkakei
end do a_aibibkakei
end do b_aibibkakei
end do k_aibibkakei
end do e_aibibkakei
!
! Elementary loop 39
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjbiaiei: do e = nvirt0, nvirt1
b_aibjbiaiei: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjbiaiei
j_aibjbiaiei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjbiaiei: do a = a0 + 1, nvirt1
i_aibjbiaiei: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbiaiei(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjbiaiei
end do a_aibjbiaiei
end do j_aibjbiaiei
end do b_aibjbiaiei
end do e_aibjbiaiei
!
! Elementary loop 40
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbiaiej: do e = nvirt0, nvirt1
b_aibjbiaiej: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjbiaiej
j_aibjbiaiej: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjbiaiej: do a = a0 + 1, nvirt1
i_aibjbiaiej: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbiaiej(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjbiaiej
end do a_aibjbiaiej
end do j_aibjbiaiej
end do b_aibjbiaiej
end do e_aibjbiaiej
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbiajei: do e = nvirt0, nvirt1
b_aibjbiajei: do b = nvirt0, nvirt1
if (b == e) cycle b_aibjbiajei
j_aibjbiajei: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b, e)
a_aibjbiajei: do a = a0 + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbiajei: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbiajei(eorb, t2, t1, nocc, nactive, a, i, b, j, e)
end do i_aibjbiajei
end do a_aibjbiajei
end do j_aibjbiajei
end do b_aibjbiajei
end do e_aibjbiajei
!
! Elementary loop 42
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjciaibi: do c = nvirt0, nvirt1
b_aibjciaibi: do b = c + 1, nvirt1
j_aibjciaibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciaibi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciaibi
i_aibjciaibi: do i = nocc0, nocc1
if (i == j) cycle i_aibjciaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciaibi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciaibi
end do a_aibjciaibi
end do j_aibjciaibi
end do b_aibjciaibi
end do c_aibjciaibi
!
! Elementary loop 43
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciaibj: do c = nvirt0, nvirt1
b_aibjciaibj: do b = c + 1, nvirt1
j_aibjciaibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciaibj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciaibj
i_aibjciaibj: do i = nocc0, nocc1
if (i == j) cycle i_aibjciaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciaibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciaibj
end do a_aibjciaibj
end do j_aibjciaibj
end do b_aibjciaibj
end do c_aibjciaibj
!
! Elementary loop 44
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjaibi: do c = nvirt0, nvirt1
b_aibjcjaibi: do b = c + 1, nvirt1
j_aibjcjaibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjaibi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjaibi
i_aibjcjaibi: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjaibi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjaibi
end do a_aibjcjaibi
end do j_aibjcjaibi
end do b_aibjcjaibi
end do c_aibjcjaibi
!
! Elementary loop 45
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjaibj: do c = nvirt0, nvirt1
b_aibjcjaibj: do b = c + 1, nvirt1
j_aibjcjaibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjaibj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjaibj
i_aibjcjaibj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjaibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjaibj
end do a_aibjcjaibj
end do j_aibjcjaibj
end do b_aibjcjaibj
end do c_aibjcjaibj
!
! Elementary loop 46
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajbi: do c = nvirt0, nvirt1
b_aibjciajbi: do b = c + 1, nvirt1
j_aibjciajbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciajbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajbi: do i = nocc0, nocc1
if (i == j) cycle i_aibjciajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciajbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciajbi
end do a_aibjciajbi
end do j_aibjciajbi
end do b_aibjciajbi
end do c_aibjciajbi
!
! Elementary loop 47
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjciajbj: do c = nvirt0, nvirt1
b_aibjciajbj: do b = c + 1, nvirt1
j_aibjciajbj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciajbj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciajbj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajbj: do i = nocc0, nocc1
if (i == j) cycle i_aibjciajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjciajbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciajbj
end do a_aibjciajbj
end do j_aibjciajbj
end do b_aibjciajbj
end do c_aibjciajbj
!
! Elementary loop 48
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjajbi: do c = nvirt0, nvirt1
b_aibjcjajbi: do b = c + 1, nvirt1
j_aibjcjajbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjajbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajbi: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjajbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjajbi
end do a_aibjcjajbi
end do j_aibjcjajbi
end do b_aibjcjajbi
end do c_aibjcjajbi
!
! Elementary loop 49
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i
! Equalities: d == a, e == b, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjajbj: do c = nvirt0, nvirt1
b_aibjcjajbj: do b = c + 1, nvirt1
j_aibjcjajbj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjajbj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjajbj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajbj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibjcjajbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjajbj
end do a_aibjcjajbj
end do j_aibjcjajbj
end do b_aibjcjajbj
end do c_aibjcjajbj
!
! Elementary loop 50
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aiajcidiai: do d = nvirt0, nvirt1
c_aiajcidiai: do c = nvirt0, nvirt1
if (c == d) cycle c_aiajcidiai
j_aiajcidiai: do j = nocc0, nocc1
a_aiajcidiai: do a = c + 1, d - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcidiai: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajcidiai(eorb, t2, t1, nocc, nactive, a, i, j, c, d)
end do i_aiajcidiai
end do a_aiajcidiai
end do j_aiajcidiai
end do c_aiajcidiai
end do d_aiajcidiai
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
end subroutine ccjac_32_part6
end module ccjac_block_32_part6
