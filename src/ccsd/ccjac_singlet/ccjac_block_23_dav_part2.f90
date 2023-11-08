module ccjac_block_23_dav_part2
use eom_cc3_23_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2019-06-21 14:26:59 UTC.
!
contains
subroutine ccjac_23_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, &
 n1k, n0l, n1l, n0m, n1m, bra0, ket0, offset) 
procedure(dav_sigma_update_diag) :: sigup_diag
procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
real(F64), dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c, &
 n0d, n1d, n0e, n1e
integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m
integer, intent(in)                                 :: bra0, ket0
integer, intent(in)                                 :: offset
!
! Local variables
!
real(F64) :: jac_ibra_iket
integer :: a, b, c, d, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, b0, b1, c0, d0, i0, i1, j0, j1, k0, l1
integer :: n0abc, n0abcd, n0ac, n0acd, n0bd
integer :: n0bde, n0be, n0ik, n0il, n0im
integer :: n0jk, n0jl, n0jm
integer :: n1abc, n1abcd, n1ac, n1acd, n1bd
integer :: n1bde, n1be, n1ik, n1il, n1im
integer :: n1jk, n1jl, n1jm
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket, iket2, ibra2
integer :: braoffset, ketoffset
integer :: qbj, qbj2
integer :: qck, qck2
integer :: q00
integer :: nk, nc, nl, nd, nm, ne
integer :: mk, mc, ml, md, mm, me


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
nk = n1k - n0k + 1
nc = n1c - n0c + 1
nl = n1l - n0l + 1
nd = n1d - n0d + 1
nm = n1m - n0m + 1
ne = n1e - n0e + 1
mk = 1
mc = nk
ml = nk * nc
md = nk * nc * nl
mm = nk * nc * nl * nd
me = nk * nc * nl * nd * nm
n0abc = max(n0a, n0b, n0c)
n0abcd = max(n0a, n0b, n0c, n0d)
n0ac = max(n0a, n0c)
n0acd = max(n0a, n0c, n0d)
n0bd = max(n0b, n0d)
n0bde = max(n0b, n0d, n0e)
n0be = max(n0b, n0e)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n1abc = min(n1a, n1b, n1c)
n1abcd = min(n1a, n1b, n1c, n1d)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1bd = min(n1b, n1d)
n1bde = min(n1b, n1d, n1e)
n1be = min(n1b, n1e)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, l
! Equalities: d == b, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjciblej: do e = n0e, n1e
l_aibjciblej: do l = n0l, n1l
c_aibjciblej: do c = n0c, n1c
if (c == e) cycle c_aibjciblej
b0 = max(e + 1, n0bd)
b1 = min(c - 1, n1bd)
b_aibjciblej: do b = b0, b1
if (b == c .or. b == e) cycle b_aibjciblej
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjciblej: do j = n0jm, n1jm
if (j == l) cycle j_aibjciblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjciblej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjciblej
i_aibjciblej: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjciblej
if (i > l .and. l > j) exit i_aibjciblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, b, l, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjciblej(a, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciblej
end do a_aibjciblej
end do j_aibjciblej
end do b_aibjciblej
end do c_aibjciblej
end do l_aibjciblej
end do e_aibjciblej
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, l
! Equalities: d == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjblei: do e = n0e, n1e
l_aibjcjblei: do l = n0l, n1l
c_aibjcjblei: do c = n0c, n1c
if (c == e) cycle c_aibjcjblei
b0 = max(e + 1, n0bd)
b1 = min(c - 1, n1bd)
b_aibjcjblei: do b = b0, b1
if (b == c .or. b == e) cycle b_aibjcjblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcjblei: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjblei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjblei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcjblei
i_aibjcjblei: do i = n0im, n1im
if (i == j .or. i == l) cycle i_aibjcjblei
if (j > l .and. l > i) cycle i_aibjcjblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, b, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjblei(a, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjblei
end do a_aibjcjblei
end do j_aibjcjblei
end do b_aibjcjblei
end do c_aibjcjblei
end do l_aibjcjblei
end do e_aibjcjblei
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, m
! Equalities: e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjdibm: do m = n0m, n1m
d_aibjcjdibm: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcjdibm: do c = c0, n1c
if (c == d) cycle c_aibjcjdibm
b1 = min(d - 1, n1be)
b_aibjcjdibm: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcjdibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjdibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjdibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjdibm: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdibm
i_aibjcjdibm: do i = n0il, n1il
if (i == j .or. i == m) cycle i_aibjcjdibm
if (j > i .and. i > m) cycle i_aibjcjdibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, d, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcjdibm(a, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdibm
end do a_aibjcjdibm
end do j_aibjcjdibm
end do b_aibjcjdibm
end do c_aibjcjdibm
end do d_aibjcjdibm
end do m_aibjcjdibm
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, k
! Equalities: e == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjckdibj: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjckdibj: do c = c0, n1c
if (c == d) cycle c_aibjckdibj
k_aibjckdibj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjckdibj: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjckdibj
j_aibjckdibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckdibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdibj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdibj
i_aibjckdibj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckdibj
if (k > i .and. i > j) cycle i_aibjckdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, d, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjckdibj(a, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdibj
end do a_aibjckdibj
end do j_aibjckdibj
end do b_aibjckdibj
end do k_aibjckdibj
end do c_aibjckdibj
end do d_aibjckdibj
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, m
! Equalities: e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcidjbm: do m = n0m, n1m
d_aibjcidjbm: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcidjbm: do c = c0, n1c
if (c == d) cycle c_aibjcidjbm
b1 = min(d - 1, n1be)
b_aibjcidjbm: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcidjbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcidjbm: do j = n0jl, n1jl
if (j == m) cycle j_aibjcidjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidjbm: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjbm
i_aibjcidjbm: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjcidjbm
if (i > j .and. j > m) exit i_aibjcidjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, j, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcidjbm(a, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidjbm
end do a_aibjcidjbm
end do j_aibjcidjbm
end do b_aibjcidjbm
end do c_aibjcidjbm
end do d_aibjcidjbm
end do m_aibjcidjbm
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, l
! Equalities: e == b, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjcidlbj: do d = n0d, n1d
l_aibjcidlbj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c0 = max(d + 1, n0c)
c_aibjcidlbj: do c = c0, n1c
if (c == d) cycle c_aibjcidlbj
b1 = min(d - 1, n1be)
b_aibjcidlbj: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcidlbj
j_aibjcidlbj: do j = n0jm, n1jm
if (j == l) cycle j_aibjcidlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidlbj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidlbj
i_aibjcidlbj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcidlbj
if (i > l .and. l > j) exit i_aibjcidlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcidlbj(a, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidlbj
end do a_aibjcidlbj
end do j_aibjcidlbj
end do b_aibjcidlbj
end do c_aibjcidlbj
end do l_aibjcidlbj
end do d_aibjcidlbj
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, k, m
! Equalities: b == a, c == a, d == a, l == i
! No equalities independent of the above can hold.
!
e_aiajakaiem: do e = n0e, n1e
m_aiajakaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aiajakaiem: do k = n0k, n1k
if (k == m) cycle k_aiajakaiem
j_aiajakaiem: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aiajakaiem
a0 = max(e + 1, n0abcd)
a_aiajakaiem: do a = a0, n1abcd
if (a == e) cycle a_aiajakaiem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i1 = min(k - 1, m - 1, n1il)
i_aiajakaiem: do i = i0, i1
if (i == j .or. i == k .or. i == m) cycle i_aiajakaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, e, m)
jac_ibra_iket = eom_cc3_23_trans_aiajakaiem(j, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakaiem
end do a_aiajakaiem
end do j_aiajakaiem
end do k_aiajakaiem
end do m_aiajakaiem
end do e_aiajakaiem
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, k, m
! Equalities: b == a, c == a, d == a, l == j
! No equalities independent of the above can hold.
!
e_aiajakajem: do e = n0e, n1e
m_aiajakajem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aiajakajem: do k = n0k, n1k
if (k == m) cycle k_aiajakajem
j1 = min(k - 1, m - 1, n1jl)
j_aiajakajem: do j = n0jl, j1
if (j == k .or. j == m) cycle j_aiajakajem
a0 = max(e + 1, n0abcd)
a_aiajakajem: do a = a0, n1abcd
if (a == e) cycle a_aiajakajem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakajem: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aiajakajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, j, e, m)
jac_ibra_iket = eom_cc3_23_trans_aiajakajem(i, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakajem
end do a_aiajakajem
end do j_aiajakajem
end do k_aiajakajem
end do m_aiajakajem
end do e_aiajakajem
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, l, m
! Equalities: b == a, c == a, d == a, k == i
! No equalities independent of the above can hold.
!
e_aiajaialem: do e = n0e, n1e
m_aiajaialem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l1 = min(m - 1, n1l)
l_aiajaialem: do l = n0l, l1
if (l == m) cycle l_aiajaialem
j_aiajaialem: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aiajaialem
a0 = max(e + 1, n0abcd)
a_aiajaialem: do a = a0, n1abcd
if (a == e) cycle a_aiajaialem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, l + 1, n0ik)
i_aiajaialem: do i = i0, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aiajaialem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, e, m)
jac_ibra_iket = eom_cc3_23_trans_aiajaialem(j, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaialem
end do a_aiajaialem
end do j_aiajaialem
end do l_aiajaialem
end do m_aiajaialem
end do e_aiajaialem
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, k, l
! Equalities: b == a, c == a, d == a, m == i
! No equalities independent of the above can hold.
!
e_aiajakalei: do e = n0e, n1e
l_aiajakalei: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aiajakalei: do k = k0, n1k
if (k == l) cycle k_aiajakalei
j_aiajakalei: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aiajakalei
a0 = max(e + 1, n0abcd)
a_aiajakalei: do a = a0, n1abcd
if (a == e) cycle a_aiajakalei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, l + 1, n0im)
i_aiajakalei: do i = i0, n1im
if (i == j .or. i == k .or. i == l) cycle i_aiajakalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajakalei(j, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakalei
end do a_aiajakalei
end do j_aiajakalei
end do k_aiajakalei
end do l_aiajakalei
end do e_aiajakalei
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, l, m
! Equalities: b == a, c == a, d == a, k == j
! No equalities independent of the above can hold.
!
e_aiajajalem: do e = n0e, n1e
m_aiajajalem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l1 = min(m - 1, n1l)
l_aiajajalem: do l = n0l, l1
if (l == m) cycle l_aiajajalem
j0 = max(l + 1, n0jk)
j_aiajajalem: do j = j0, n1jk
if (j == l .or. j == m) cycle j_aiajajalem
a0 = max(e + 1, n0abcd)
a_aiajajalem: do a = a0, n1abcd
if (a == e) cycle a_aiajajalem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajajalem: do i = i0, n1i
if (i == j .or. i == l .or. i == m) cycle i_aiajajalem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, l, e, m)
jac_ibra_iket = eom_cc3_23_trans_aiajajalem(i, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajalem
end do a_aiajajalem
end do j_aiajajalem
end do l_aiajajalem
end do m_aiajajalem
end do e_aiajajalem
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, k, l
! Equalities: b == a, c == a, d == a, m == j
! No equalities independent of the above can hold.
!
e_aiajakalej: do e = n0e, n1e
l_aiajakalej: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aiajakalej: do k = k0, n1k
if (k == l) cycle k_aiajakalej
j0 = max(l + 1, n0jm)
j_aiajakalej: do j = j0, n1jm
if (j == k .or. j == l) cycle j_aiajakalej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiajakalej: do a = a0, n1abcd
if (a == e) cycle a_aiajakalej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakalej: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajakalej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, l, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajakalej(i, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakalej
end do a_aiajakalej
end do j_aiajakalej
end do k_aiajakalej
end do l_aiajakalej
end do e_aiajakalej
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, m
! Equalities: c == a, d == a, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjakaibm: do m = n0m, n1m
k_aibjakaibm: do k = n0k, n1k
if (k == m) cycle k_aibjakaibm
b_aibjakaibm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjakaibm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjakaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakaibm: do a = a0, n1acd
if (a == b) cycle a_aibjakaibm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, m - 1, n1il)
i_aibjakaibm: do i = n0il, i1
if (i == j .or. i == k .or. i == m) cycle i_aibjakaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjakaibm(a, j, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakaibm
end do a_aibjakaibm
end do j_aibjakaibm
end do b_aibjakaibm
end do k_aibjakaibm
end do m_aibjakaibm
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, m
! Equalities: c == a, d == a, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjakajbm: do m = n0m, n1m
k_aibjakajbm: do k = n0k, n1k
if (k == m) cycle k_aibjakajbm
b_aibjakajbm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j1 = min(k - 1, m - 1, n1jl)
j_aibjakajbm: do j = n0jl, j1
if (j == k .or. j == m) cycle j_aibjakajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakajbm: do a = a0, n1acd
if (a == b) cycle a_aibjakajbm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakajbm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, j, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjakajbm(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakajbm
end do a_aibjakajbm
end do j_aibjakajbm
end do b_aibjakajbm
end do k_aibjakajbm
end do m_aibjakajbm
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l, m
! Equalities: c == a, d == a, e == b, k == i
! No equalities independent of the above can hold.
!
m_aibjaialbm: do m = n0m, n1m
l1 = min(m - 1, n1l)
l_aibjaialbm: do l = n0l, l1
if (l == m) cycle l_aibjaialbm
b_aibjaialbm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaialbm: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aibjaialbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaialbm: do a = a0, n1acd
if (a == b) cycle a_aibjaialbm
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ik)
i_aibjaialbm: do i = i0, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aibjaialbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjaialbm(a, j, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaialbm
end do a_aibjaialbm
end do j_aibjaialbm
end do b_aibjaialbm
end do l_aibjaialbm
end do m_aibjaialbm
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l, m
! Equalities: c == a, d == a, e == b, k == j
! No equalities independent of the above can hold.
!
m_aibjajalbm: do m = n0m, n1m
l1 = min(m - 1, n1l)
l_aibjajalbm: do l = n0l, l1
if (l == m) cycle l_aibjajalbm
b_aibjajalbm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(l + 1, n0jk)
j_aibjajalbm: do j = j0, n1jk
if (j == l .or. j == m) cycle j_aibjajalbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajalbm: do a = a0, n1acd
if (a == b) cycle a_aibjajalbm
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjajalbm: do i = n0i, n1i
if (i == j .or. i == l .or. i == m) cycle i_aibjajalbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, l, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjajalbm(a, i, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajalbm
end do a_aibjajalbm
end do j_aibjajalbm
end do b_aibjajalbm
end do l_aibjajalbm
end do m_aibjajalbm
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, l
! Equalities: c == a, d == a, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjakalbj: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibjakalbj: do k = k0, n1k
if (k == l) cycle k_aibjakalbj
b_aibjakalbj: do b = n0be, n1be
j0 = max(l + 1, n0jm)
j_aibjakalbj: do j = j0, n1jm
if (j == k .or. j == l) cycle j_aibjakalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakalbj: do a = a0, n1acd
if (a == b) cycle a_aibjakalbj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjakalbj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakalbj(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakalbj
end do a_aibjakalbj
end do j_aibjakalbj
end do b_aibjakalbj
end do k_aibjakalbj
end do l_aibjakalbj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajaiem: do e = n0e, n1e
m_aibjajaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjajaiem: do b = n0b, n1b
if (b == e) cycle b_aibjajaiem
j_aibjajaiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjajaiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjajaiem: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjajaiem
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, m - 1, n1il)
i_aibjajaiem: do i = n0il, i1
if (i == j .or. i == m) cycle i_aibjajaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, i, e, m)
jac_ibra_iket = eom_cc3_23_trans_aibjajaiem(a, b, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajaiem
end do a_aibjajaiem
end do j_aibjajaiem
end do b_aibjajaiem
end do m_aibjajaiem
end do e_aibjajaiem
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjakaiej: do e = n0e, n1e
k_aibjakaiej: do k = n0k, n1k
b_aibjakaiej: do b = n0b, n1b
if (b == e) cycle b_aibjakaiej
j_aibjakaiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjakaiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjakaiej: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjakaiej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, j - 1, n1il)
i_aibjakaiej: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakaiej(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakaiej
end do a_aibjakaiej
end do j_aibjakaiej
end do b_aibjakaiej
end do k_aibjakaiej
end do e_aibjakaiej
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaiajem: do e = n0e, n1e
m_aibjaiajem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjaiajem: do b = n0b, n1b
if (b == e) cycle b_aibjaiajem
j1 = min(m - 1, n1jl)
j_aibjaiajem: do j = n0jl, j1
if (j == m) cycle j_aibjaiajem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjaiajem: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjaiajem
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjaiajem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjaiajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, j, e, m)
jac_ibra_iket = eom_cc3_23_trans_aibjaiajem(a, b, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiajem
end do a_aibjaiajem
end do j_aibjaiajem
end do b_aibjaiajem
end do m_aibjaiajem
end do e_aibjaiajem
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjakajei: do e = n0e, n1e
k_aibjakajei: do k = n0k, n1k
b_aibjakajei: do b = n0b, n1b
if (b == e) cycle b_aibjakajei
j1 = min(k - 1, n1jl)
j_aibjakajei: do j = n0jl, j1
if (j == k) cycle j_aibjakajei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjakajei: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjakajei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aibjakajei: do i = i0, n1im
if (i == j .or. i == k) cycle i_aibjakajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjakajei(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakajei
end do a_aibjakajei
end do j_aibjakajei
end do b_aibjakajei
end do k_aibjakajei
end do e_aibjakajei
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjaialej: do e = n0e, n1e
l_aibjaialej: do l = n0l, n1l
b_aibjaialej: do b = n0b, n1b
if (b == e) cycle b_aibjaialej
j0 = max(l + 1, n0jm)
j_aibjaialej: do j = j0, n1jm
if (j == l) cycle j_aibjaialej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjaialej: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjaialej
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ik)
i_aibjaialej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaialej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjaialej(a, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaialej
end do a_aibjaialej
end do j_aibjaialej
end do b_aibjaialej
end do l_aibjaialej
end do e_aibjaialej
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajalei: do e = n0e, n1e
l_aibjajalei: do l = n0l, n1l
b_aibjajalei: do b = n0b, n1b
if (b == e) cycle b_aibjajalei
j0 = max(l + 1, n0jk)
j_aibjajalei: do j = j0, n1jk
if (j == l) cycle j_aibjajalei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjajalei: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjajalei
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0im)
i_aibjajalei: do i = i0, n1im
if (i == j .or. i == l) cycle i_aibjajalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjajalei(a, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajalei
end do a_aibjajalei
end do j_aibjajalei
end do b_aibjajalei
end do l_aibjajalei
end do e_aibjajalei
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, l == i, k == j
! No equalities independent of the above can hold.
!
e_aiajajdiem: do e = n0e, n1e
m_aiajajdiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d0 = max(e + 1, n0d)
d_aiajajdiem: do d = d0, n1d
if (d == e) cycle d_aiajajdiem
j_aiajajdiem: do j = n0jk, n1jk
if (j == m) cycle j_aiajajdiem
a0 = max(d + 1, n0abc)
a_aiajajdiem: do a = a0, n1abc
if (a == d .or. a == e) cycle a_aiajajdiem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajajdiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajajdiem
if (j > i .and. i > m) cycle i_aiajajdiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, i, e, m)
jac_ibra_iket = eom_cc3_23_trans_aiajajdiem(a, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajdiem
end do a_aiajajdiem
end do j_aiajajdiem
end do d_aiajajdiem
end do m_aiajajdiem
end do e_aiajajdiem
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajakdiej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aiajakdiej: do d = d0, n1d
if (d == e) cycle d_aiajakdiej
k_aiajakdiej: do k = n0k, n1k
j_aiajakdiej: do j = n0jm, n1jm
if (j == k) cycle j_aiajakdiej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajakdiej: do a = a0, n1abc
if (a == d .or. a == e) cycle a_aiajakdiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajakdiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajakdiej
if (k > i .and. i > j) cycle i_aiajakdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajakdiej(a, k, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakdiej
end do a_aiajakdiej
end do j_aiajakdiej
end do k_aiajakdiej
end do d_aiajakdiej
end do e_aiajakdiej
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aiajaidjem: do e = n0e, n1e
m_aiajaidjem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d0 = max(e + 1, n0d)
d_aiajaidjem: do d = d0, n1d
if (d == e) cycle d_aiajaidjem
j_aiajaidjem: do j = n0jl, n1jl
if (j == m) cycle j_aiajaidjem
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajaidjem: do a = a0, n1abc
if (a == d .or. a == e) cycle a_aiajaidjem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaidjem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aiajaidjem
if (i > j .and. j > m) exit i_aiajaidjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, j, e, m)
jac_ibra_iket = eom_cc3_23_trans_aiajaidjem(a, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidjem
end do a_aiajaidjem
end do j_aiajaidjem
end do d_aiajaidjem
end do m_aiajaidjem
end do e_aiajaidjem
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajakdjei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aiajakdjei: do d = d0, n1d
if (d == e) cycle d_aiajakdjei
k_aiajakdjei: do k = n0k, n1k
j_aiajakdjei: do j = n0jl, n1jl
if (j == k) cycle j_aiajakdjei
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajakdjei: do a = a0, n1abc
if (a == d .or. a == e) cycle a_aiajakdjei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajakdjei: do i = i0, n1im
if (i == j .or. i == k) cycle i_aiajakdjei
if (k > j .and. j > i) cycle i_aiajakdjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajakdjei(a, k, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakdjei
end do a_aiajakdjei
end do j_aiajakdjei
end do k_aiajakdjei
end do d_aiajakdjei
end do e_aiajakdjei
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aiajaidlej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aiajaidlej: do d = d0, n1d
if (d == e) cycle d_aiajaidlej
l_aiajaidlej: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajaidlej: do j = n0jm, n1jm
if (j == l) cycle j_aiajaidlej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajaidlej: do a = a0, n1abc
if (a == d .or. a == e) cycle a_aiajaidlej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaidlej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaidlej
if (i > l .and. l > j) exit i_aiajaidlej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajaidlej(a, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidlej
end do a_aiajaidlej
end do j_aiajaidlej
end do l_aiajaidlej
end do d_aiajaidlej
end do e_aiajaidlej
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aiajajdlei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aiajajdlei: do d = d0, n1d
if (d == e) cycle d_aiajajdlei
l_aiajajdlei: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajajdlei: do j = n0jk, n1jk
if (j == l) cycle j_aiajajdlei
a0 = max(d + 1, n0abc)
a_aiajajdlei: do a = a0, n1abc
if (a == d .or. a == e) cycle a_aiajajdlei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajajdlei: do i = i0, n1im
if (i == j .or. i == l) cycle i_aiajajdlei
if (j > l .and. l > i) cycle i_aiajajdlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajajdlei(a, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajdlei
end do a_aiajajdlei
end do j_aiajajdlei
end do l_aiajajdlei
end do d_aiajajdlei
end do e_aiajajdlei
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, m
! Equalities: c == a, d == b, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjakbibm: do m = n0m, n1m
k_aibjakbibm: do k = n0k, n1k
if (k == m) cycle k_aibjakbibm
b_aibjakbibm: do b = n0bde, n1bde
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjakbibm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjakbibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbibm: do a = a0, n1ac
if (a == b) cycle a_aibjakbibm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(m + 1, k + 1, n0il)
i_aibjakbibm: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjakbibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjakbibm(b, j, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbibm
end do a_aibjakbibm
end do j_aibjakbibm
end do b_aibjakbibm
end do k_aibjakbibm
end do m_aibjakbibm
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
function mu3_mem(c, k, d, l, e, m)
integer :: mu3_mem
integer, intent(in) :: c, k, d, l, e, m
mu3_mem = offset + (k-n0k)+mk + (c-n0c)*mc + (l-n0l)*ml + (d-n0d)*md + (m-n0m)*mm  &
+ (e-n0e)*me
end function mu3_mem

end subroutine ccjac_23_dav_part2
end module ccjac_block_23_dav_part2
