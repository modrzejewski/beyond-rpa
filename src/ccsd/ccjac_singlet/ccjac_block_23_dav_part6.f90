module ccjac_block_23_dav_part6
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
subroutine ccjac_23_dav_part6(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, a1, b1, c0, i0, i1, j0, j1
integer :: n0abe, n0ad, n0ae, n0bde, n0be
integer :: n0ijk, n0ijkm, n0ijlm, n0ijm, n0ik
integer :: n0ikm, n0il, n0ilm, n0im, n0jk
integer :: n0jkm, n0jl, n0jlm, n0jm, n0lm
integer :: n1abe, n1ad, n1ae, n1bde, n1be
integer :: n1ijk, n1ijkm, n1ijlm, n1ijm, n1ik
integer :: n1ikm, n1il, n1ilm, n1im, n1jk
integer :: n1jkm, n1jl, n1jlm, n1jm, n1lm
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

n0abe = max(n0a, n0b, n0e)
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0bde = max(n0b, n0d, n0e)
n0be = max(n0b, n0e)
n0ijk = max(n0i, n0j, n0k)
n0ijkm = max(n0i, n0j, n0k, n0m)
n0ijlm = max(n0i, n0j, n0l, n0m)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0ilm = max(n0i, n0l, n0m)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jlm = max(n0j, n0l, n0m)
n0jm = max(n0j, n0m)
n0lm = max(n0l, n0m)
n1abe = min(n1a, n1b, n1e)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bde = min(n1b, n1d, n1e)
n1be = min(n1b, n1e)
n1ijk = min(n1i, n1j, n1k)
n1ijkm = min(n1i, n1j, n1k, n1m)
n1ijlm = min(n1i, n1j, n1l, n1m)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1ilm = min(n1i, n1l, n1m)
n1im = min(n1i, n1m)
n1jk = min(n1j, n1k)
n1jkm = min(n1j, n1k, n1m)
n1jl = min(n1j, n1l)
n1jlm = min(n1j, n1l, n1m)
n1jm = min(n1j, n1m)
n1lm = min(n1l, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l, j
! Equalities: d == a, e == b, k == i, m == l
! No equalities independent of the above can hold.
!
l_aibjcialbl: do l = n0lm, n1lm
c_aibjcialbl: do c = n0c, n1c
b_aibjcialbl: do b = n0be, n1be
if (b == c) cycle b_aibjcialbl
em = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcialbl: do j = n0j, n1j
if (j == l) cycle j_aibjcialbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcialbl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcialbl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialbl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcialbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, b, l)
jac_ibra_iket = eom_cc3_23_trans_aibjcialbl(j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcialbl
end do a_aibjcialbl
end do j_aibjcialbl
end do b_aibjcialbl
end do c_aibjcialbl
end do l_aibjcialbl
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, l, i
! Equalities: d == a, e == b, k == j, m == l
! No equalities independent of the above can hold.
!
l_aibjcjalbl: do l = n0lm, n1lm
c_aibjcjalbl: do c = n0c, n1c
b_aibjcjalbl: do b = n0be, n1be
if (b == c) cycle b_aibjcjalbl
em = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcjalbl: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjalbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjalbl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjalbl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalbl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjcjalbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, l, b, l)
jac_ibra_iket = eom_cc3_23_trans_aibjcjalbl(i, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjalbl
end do a_aibjcjalbl
end do j_aibjcjalbl
end do b_aibjcjalbl
end do c_aibjcjalbl
end do l_aibjcjalbl
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l, m
! Equalities: d == a, e == b, j == i, k == i
! No equalities independent of the above can hold.
!
m_aibicialbm: do m = n0m, n1m
l_aibicialbm: do l = n0l, n1l
if (l == m) cycle l_aibicialbm
c_aibicialbm: do c = n0c, n1c
b_aibicialbm: do b = n0be, n1be
if (b == c) cycle b_aibicialbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibicialbm: do a = a0, a1
if (a == b .or. a == c) cycle a_aibicialbm
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibicialbm: do i = n0ijk, n1ijk
if (i == l .or. i == m) cycle i_aibicialbm
if (i > l .and. l > m) exit i_aibicialbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibicialbm(i, c, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicialbm
end do a_aibicialbm
end do b_aibicialbm
end do c_aibicialbm
end do l_aibicialbm
end do m_aibicialbm
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjcialbi: do l = n0l, n1l
c_aibjcialbi: do c = n0c, n1c
b_aibjcialbi: do b = n0be, n1be
if (b == c) cycle b_aibjcialbi
j_aibjcialbi: do j = n0j, n1j
if (j == l) cycle j_aibjcialbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcialbi: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialbi: do i = n0ikm, n1ikm
if (i == j .or. i == l) cycle i_aibjcialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcialbi(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcialbi
end do a_aibjcialbi
end do j_aibjcialbi
end do b_aibjcialbi
end do c_aibjcialbi
end do l_aibjcialbi
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjcialbj: do l = n0l, n1l
c_aibjcialbj: do c = n0c, n1c
b_aibjcialbj: do b = n0be, n1be
if (b == c) cycle b_aibjcialbj
j_aibjcialbj: do j = n0jm, n1jm
if (j == l) cycle j_aibjcialbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcialbj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcialbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialbj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcialbj
if (i > l .and. l > j) exit i_aibjcialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcialbj(nocc, a, i, b, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcialbj
end do a_aibjcialbj
end do j_aibjcialbj
end do b_aibjcialbj
end do c_aibjcialbj
end do l_aibjcialbj
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, l
! Equalities: d == a, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibickalbi: do l = n0l, n1l
c_aibickalbi: do c = n0c, n1c
k_aibickalbi: do k = n0k, n1k
if (k == l) cycle k_aibickalbi
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickalbi: do b = n0be, n1be
if (b == c) cycle b_aibickalbi
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibickalbi: do a = a0, a1
if (a == b .or. a == c) cycle a_aibickalbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibickalbi: do i = n0ijm, n1ijm
if (i == k .or. i == l) cycle i_aibickalbi
if (k > l .and. l > i) cycle i_aibickalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibickalbi(i, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickalbi
end do a_aibickalbi
end do b_aibickalbi
end do k_aibickalbi
end do c_aibickalbi
end do l_aibickalbi
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjalbi: do l = n0l, n1l
c_aibjcjalbi: do c = n0c, n1c
b_aibjcjalbi: do b = n0be, n1be
if (b == c) cycle b_aibjcjalbi
j_aibjcjalbi: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjalbi: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjalbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalbi: do i = n0im, n1im
if (i == j .or. i == l) cycle i_aibjcjalbi
if (j > l .and. l > i) cycle i_aibjcjalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjalbi(i, b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjalbi
end do a_aibjcjalbi
end do j_aibjcjalbi
end do b_aibjcjalbi
end do c_aibjcjalbi
end do l_aibjcjalbi
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjcjalbj: do l = n0l, n1l
c_aibjcjalbj: do c = n0c, n1c
b_aibjcjalbj: do b = n0be, n1be
if (b == c) cycle b_aibjcjalbj
j_aibjcjalbj: do j = n0jkm, n1jkm
if (j == l) cycle j_aibjcjalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjalbj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjalbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalbj: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjcjalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcjalbj(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjalbj
end do a_aibjcjalbj
end do j_aibjcjalbj
end do b_aibjcjalbj
end do c_aibjcjalbj
end do l_aibjcjalbj
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibickaiei: do e = n0e, n1e
c_aibickaiei: do c = n0c, n1c
if (c == e) cycle c_aibickaiei
k_aibickaiei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickaiei: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibickaiei
a0 = max(b + 1, e + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibickaiei: do a = a0, a1
if (a == b .or. a == c .or. a == e) cycle a_aibickaiei
i_aibickaiei: do i = n0ijlm, n1ijlm
if (i == k) cycle i_aibickaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibickaiei(b, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickaiei
end do a_aibickaiei
end do b_aibickaiei
end do k_aibickaiei
end do c_aibickaiei
end do e_aibickaiei
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjaiei: do e = n0e, n1e
c_aibjcjaiei: do c = n0c, n1c
if (c == e) cycle c_aibjcjaiei
b_aibjcjaiei: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjcjaiei
j_aibjcjaiei: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjaiei: do a = a0, a1
if (a == b .or. a == c .or. a == e) cycle a_aibjcjaiei
i_aibjcjaiei: do i = n0ilm, n1ilm
if (i == j) cycle i_aibjcjaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjaiei(i, b, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaiei
end do a_aibjcjaiei
end do j_aibjcjaiei
end do b_aibjcjaiei
end do c_aibjcjaiei
end do e_aibjcjaiei
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjaiej: do e = n0e, n1e
c_aibjcjaiej: do c = n0c, n1c
if (c == e) cycle c_aibjcjaiej
b_aibjcjaiej: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjcjaiej
j_aibjcjaiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjaiej: do a = a0, a1
if (a == b .or. a == c .or. a == e) cycle a_aibjcjaiej
i_aibjcjaiej: do i = n0il, n1il
if (i == j) cycle i_aibjcjaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcjaiej(b, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaiej
end do a_aibjcjaiej
end do j_aibjcjaiej
end do b_aibjcjaiej
end do c_aibjcjaiej
end do e_aibjcjaiej
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjciajej: do e = n0e, n1e
c_aibjciajej: do c = n0c, n1c
if (c == e) cycle c_aibjciajej
b_aibjciajej: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjciajej
j_aibjciajej: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciajej: do a = a0, a1
if (a == b .or. a == c .or. a == e) cycle a_aibjciajej
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajej: do i = n0ik, n1ik
if (i == j) cycle i_aibjciajej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, j, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjciajej(b, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciajej
end do a_aibjciajej
end do j_aibjciajej
end do b_aibjciajej
end do c_aibjciajej
end do e_aibjciajej
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, l
! Equalities: d == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibicialei: do e = n0e, n1e
l_aibicialei: do l = n0l, n1l
c_aibicialei: do c = n0c, n1c
if (c == e) cycle c_aibicialei
b_aibicialei: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibicialei
a0 = max(b + 1, e + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibicialei: do a = a0, a1
if (a == b .or. a == c .or. a == e) cycle a_aibicialei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibicialei: do i = n0ijkm, n1ijkm
if (i == l) cycle i_aibicialei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibicialei(b, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicialei
end do a_aibicialei
end do b_aibicialei
end do c_aibicialei
end do l_aibicialei
end do e_aibicialei
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, m
! Equalities: b == a, e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aiajcjdiam: do m = n0m, n1m
d_aiajcjdiam: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcjdiam: do c = c0, n1c
if (c == d) cycle c_aiajcjdiam
j_aiajcjdiam: do j = n0jk, n1jk
if (j == m) cycle j_aiajcjdiam
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abe)
a_aiajcjdiam: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajcjdiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjdiam: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajcjdiam
if (j > i .and. i > m) cycle i_aiajcjdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, d, i, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiajcjdiam(a, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdiam
end do a_aiajcjdiam
end do j_aiajcjdiam
end do c_aiajcjdiam
end do d_aiajcjdiam
end do m_aiajcjdiam
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajckdiaj: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajckdiaj: do c = c0, n1c
if (c == d) cycle c_aiajckdiaj
k_aiajckdiaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdiaj: do j = n0jm, n1jm
if (j == k) cycle j_aiajckdiaj
a1 = min(d - 1, n1abe)
a_aiajckdiaj: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajckdiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckdiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckdiaj
if (k > i .and. i > j) cycle i_aiajckdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, d, i, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajckdiaj(a, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdiaj
end do a_aiajckdiaj
end do j_aiajckdiaj
end do k_aiajckdiaj
end do c_aiajckdiaj
end do d_aiajckdiaj
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, m
! Equalities: b == a, e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajcidjam: do m = n0m, n1m
d_aiajcidjam: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcidjam: do c = c0, n1c
if (c == d) cycle c_aiajcidjam
j_aiajcidjam: do j = n0jl, n1jl
if (j == m) cycle j_aiajcidjam
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abe)
a_aiajcidjam: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajcidjam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcidjam: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aiajcidjam
if (i > j .and. j > m) exit i_aiajcidjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, j, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiajcidjam(a, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidjam
end do a_aiajcidjam
end do j_aiajcidjam
end do c_aiajcidjam
end do d_aiajcidjam
end do m_aiajcidjam
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajckdjai: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajckdjai: do c = c0, n1c
if (c == d) cycle c_aiajckdjai
k_aiajckdjai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdjai: do j = n0jl, n1jl
if (j == k) cycle j_aiajckdjai
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abe)
a_aiajckdjai: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajckdjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajckdjai: do i = i0, n1im
if (i == j .or. i == k) cycle i_aiajckdjai
if (k > j .and. j > i) cycle i_aiajckdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, d, j, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiajckdjai(a, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdjai
end do a_aiajckdjai
end do j_aiajckdjai
end do k_aiajckdjai
end do c_aiajckdjai
end do d_aiajckdjai
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, l
! Equalities: b == a, e == a, k == i, m == j
! No equalities independent of the above can hold.
!
d_aiajcidlaj: do d = n0d, n1d
l_aiajcidlaj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c0 = max(d + 1, n0c)
c_aiajcidlaj: do c = c0, n1c
if (c == d) cycle c_aiajcidlaj
j_aiajcidlaj: do j = n0jm, n1jm
if (j == l) cycle j_aiajcidlaj
a1 = min(d - 1, n1abe)
a_aiajcidlaj: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajcidlaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcidlaj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcidlaj
if (i > l .and. l > j) exit i_aiajcidlaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, l, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajcidlaj(a, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidlaj
end do a_aiajcidlaj
end do j_aiajcidlaj
end do c_aiajcidlaj
end do l_aiajcidlaj
end do d_aiajcidlaj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, l
! Equalities: b == a, e == a, m == i, k == j
! No equalities independent of the above can hold.
!
d_aiajcjdlai: do d = n0d, n1d
l_aiajcjdlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c0 = max(d + 1, n0c)
c_aiajcjdlai: do c = c0, n1c
if (c == d) cycle c_aiajcjdlai
j_aiajcjdlai: do j = n0jk, n1jk
if (j == l) cycle j_aiajcjdlai
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abe)
a_aiajcjdlai: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajcjdlai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajcjdlai: do i = i0, n1im
if (i == j .or. i == l) cycle i_aiajcjdlai
if (j > l .and. l > i) cycle i_aiajcjdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, d, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiajcjdlai(a, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdlai
end do a_aiajcjdlai
end do j_aiajcjdlai
end do c_aiajcjdlai
end do l_aiajcjdlai
end do d_aiajcjdlai
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, k
! Equalities: e == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibickdiai: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibickdiai: do c = c0, n1c
if (c == d) cycle c_aibickdiai
k_aibickdiai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickdiai: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibickdiai
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibickdiai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibickdiai
i_aibickdiai: do i = n0ijlm, n1ijlm
if (i == k) cycle i_aibickdiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, d, i, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibickdiai(b, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickdiai
end do a_aibickdiai
end do b_aibickdiai
end do k_aibickdiai
end do c_aibickdiai
end do d_aibickdiai
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdiai: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcjdiai: do c = c0, n1c
if (c == d) cycle c_aibjcjdiai
b_aibjcjdiai: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdiai
j_aibjcjdiai: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcjdiai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdiai
i_aibjcjdiai: do i = n0ilm, n1ilm
if (i == j) cycle i_aibjcjdiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, d, i, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjdiai(i, b, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdiai
end do a_aibjcjdiai
end do j_aibjcjdiai
end do b_aibjcjdiai
end do c_aibjcjdiai
end do d_aibjcjdiai
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdiaj: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcjdiaj: do c = c0, n1c
if (c == d) cycle c_aibjcjdiaj
b_aibjcjdiaj: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdiaj
j_aibjcjdiaj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcjdiaj: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjdiaj: do i = n0il, n1il
if (i == j) cycle i_aibjcjdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, d, i, a, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcjdiaj(b, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdiaj
end do a_aibjcjdiaj
end do j_aibjcjdiaj
end do b_aibjcjdiaj
end do c_aibjcjdiaj
end do d_aibjcjdiaj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcidjaj: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcidjaj: do c = c0, n1c
if (c == d) cycle c_aibjcidjaj
b_aibjcidjaj: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcidjaj
j_aibjcidjaj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcidjaj: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcidjaj: do i = n0ik, n1ik
if (i == j) cycle i_aibjcidjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, j, a, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcidjaj(b, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidjaj
end do a_aibjcidjaj
end do j_aibjcidjaj
end do b_aibjcidjaj
end do c_aibjcidjaj
end do d_aibjcidjaj
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidjai: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcidjai: do c = c0, n1c
if (c == d) cycle c_aibjcidjai
b_aibjcidjai: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcidjai
j_aibjcidjai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcidjai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjai
i_aibjcidjai: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjcidjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, j, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcidjai(i, b, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidjai
end do a_aibjcidjai
end do j_aibjcidjai
end do b_aibjcidjai
end do c_aibjcidjai
end do d_aibjcidjai
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, l
! Equalities: e == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibicidlai: do d = n0d, n1d
l_aibicidlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c0 = max(d + 1, n0c)
c_aibicidlai: do c = c0, n1c
if (c == d) cycle c_aibicidlai
b_aibicidlai: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibicidlai
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibicidlai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibicidlai
i_aibicidlai: do i = n0ijkm, n1ijkm
if (i == l) cycle i_aibicidlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibicidlai(b, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicidlai
end do a_aibicidlai
end do b_aibicidlai
end do c_aibicidlai
end do l_aibicidlai
end do d_aibicidlai
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, m
! Equalities: d == b, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjbibm: do m = n0m, n1m
c_aibjcjbibm: do c = n0c, n1c
b1 = min(c - 1, n1bde)
b_aibjcjbibm: do b = n0bde, b1
if (b == c) cycle b_aibjcjbibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjbibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjbibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbibm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbibm
i0 = max(m + 1, j + 1, n0il)
i_aibjcjbibm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjcjbibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, b, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcjbibm(a, b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbibm
end do a_aibjcjbibm
end do j_aibjcjbibm
end do b_aibjcjbibm
end do c_aibjcjbibm
end do m_aibjcjbibm
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckbibj: do c = n0c, n1c
k_aibjckbibj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(c - 1, n1bde)
b_aibjckbibj: do b = n0bde, b1
if (b == c) cycle b_aibjckbibj
j_aibjckbibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckbibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbibj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbibj
i0 = max(j + 1, k + 1, n0il)
i_aibjckbibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckbibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, b, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjckbibj(a, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbibj
end do a_aibjckbibj
end do j_aibjckbibj
end do b_aibjckbibj
end do k_aibjckbibj
end do c_aibjckbibj
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, m
! Equalities: d == b, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcibjbm: do m = n0m, n1m
c_aibjcibjbm: do c = n0c, n1c
b1 = min(c - 1, n1bde)
b_aibjcibjbm: do b = n0bde, b1
if (b == c) cycle b_aibjcibjbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjcibjbm: do j = j0, n1jl
if (j == m) cycle j_aibjcibjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibjbm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibjbm
i1 = min(j - 1, n1ik)
i_aibjcibjbm: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aibjcibjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, b, j, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcibjbm(a, b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibjbm
end do a_aibjcibjbm
end do j_aibjcibjbm
end do b_aibjcibjbm
end do c_aibjcibjbm
end do m_aibjcibjbm
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckbjbi: do c = n0c, n1c
k_aibjckbjbi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(c - 1, n1bde)
b_aibjckbjbi: do b = n0bde, b1
if (b == c) cycle b_aibjckbjbi
j0 = max(k + 1, n0jl)
j_aibjckbjbi: do j = j0, n1jl
if (j == k) cycle j_aibjckbjbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbjbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbjbi
i1 = min(j - 1, n1im)
i_aibjckbjbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckbjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, b, j, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjckbjbi(a, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbjbi
end do a_aibjckbjbi
end do j_aibjckbjbi
end do b_aibjckbjbi
end do k_aibjckbjbi
end do c_aibjckbjbi
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjciblbj: do l = n0l, n1l
c_aibjciblbj: do c = n0c, n1c
b1 = min(c - 1, n1bde)
b_aibjciblbj: do b = n0bde, b1
if (b == c) cycle b_aibjciblbj
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjciblbj: do j = n0jm, j1
if (j == l) cycle j_aibjciblbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjciblbj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjciblbj
i1 = min(l - 1, n1ik)
i_aibjciblbj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjciblbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, b, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjciblbj(a, b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciblbj
end do a_aibjciblbj
end do j_aibjciblbj
end do b_aibjciblbj
end do c_aibjciblbj
end do l_aibjciblbj
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

end subroutine ccjac_23_dav_part6
end module ccjac_block_23_dav_part6
