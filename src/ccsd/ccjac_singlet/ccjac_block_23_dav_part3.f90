module ccjac_block_23_dav_part3
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
subroutine ccjac_23_dav_part3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a, b, d, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, b0, b1, i0, i1, j0, j1, k1, l0
integer :: n0ac, n0bd, n0bde, n0be, n0de
integer :: n0ijk, n0ijl, n0ijm, n0ik, n0ikm
integer :: n0il, n0ilm, n0im, n0jk, n0jkm
integer :: n0jl, n0jlm, n0jm, n0km, n0lm
integer :: n1ac, n1bd, n1bde, n1be, n1de
integer :: n1ijk, n1ijl, n1ijm, n1ik, n1ikm
integer :: n1il, n1ilm, n1im, n1jk, n1jkm
integer :: n1jl, n1jlm, n1jm, n1km, n1lm
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


n0ac = max(n0a, n0c)
n0bd = max(n0b, n0d)
n0bde = max(n0b, n0d, n0e)
n0be = max(n0b, n0e)
n0de = max(n0d, n0e)
n0ijk = max(n0i, n0j, n0k)
n0ijl = max(n0i, n0j, n0l)
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
n0km = max(n0k, n0m)
n0lm = max(n0l, n0m)
n1ac = min(n1a, n1c)
n1bd = min(n1b, n1d)
n1bde = min(n1b, n1d, n1e)
n1be = min(n1b, n1e)
n1de = min(n1d, n1e)
n1ijk = min(n1i, n1j, n1k)
n1ijl = min(n1i, n1j, n1l)
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
n1km = min(n1k, n1m)
n1lm = min(n1l, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, m
! Equalities: c == a, d == b, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjakbjbm: do m = n0m, n1m
k_aibjakbjbm: do k = n0k, n1k
if (k == m) cycle k_aibjakbjbm
b_aibjakbjbm: do b = n0bde, n1bde
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, k + 1, n0jl)
j_aibjakbjbm: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjakbjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbjbm: do a = a0, n1ac
if (a == b) cycle a_aibjakbjbm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjbm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakbjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, j, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjakbjbm(i, b, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbjbm
end do a_aibjakbjbm
end do j_aibjakbjbm
end do b_aibjakbjbm
end do k_aibjakbjbm
end do m_aibjakbjbm
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l, m
! Equalities: c == a, d == b, e == b, k == i
! No equalities independent of the above can hold.
!
m_aibjaiblbm: do m = n0m, n1m
l0 = max(m + 1, n0l)
l_aibjaiblbm: do l = l0, n1l
if (l == m) cycle l_aibjaiblbm
b_aibjaiblbm: do b = n0bde, n1bde
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaiblbm: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aibjaiblbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaiblbm: do a = a0, n1ac
if (a == b) cycle a_aibjaiblbm
i1 = min(l - 1, n1ik)
i_aibjaiblbm: do i = n0ik, i1
if (i == j .or. i == l .or. i == m) cycle i_aibjaiblbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, l, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjaiblbm(b, j, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiblbm
end do a_aibjaiblbm
end do j_aibjaiblbm
end do b_aibjaiblbm
end do l_aibjaiblbm
end do m_aibjaiblbm
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == b, e == b, m == i
! No equalities independent of the above can hold.
!
l_aibjakblbi: do l = n0l, n1l
k1 = min(l - 1, n1k)
k_aibjakblbi: do k = n0k, k1
if (k == l) cycle k_aibjakblbi
b_aibjakblbi: do b = n0bde, n1bde
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjakblbi: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjakblbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakblbi: do a = a0, n1ac
if (a == b) cycle a_aibjakblbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjakblbi: do i = n0im, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjakblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjakblbi(b, j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakblbi
end do a_aibjakblbi
end do j_aibjakblbi
end do b_aibjakblbi
end do k_aibjakblbi
end do l_aibjakblbi
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, l
! Equalities: c == a, d == b, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjakblbj: do l = n0l, n1l
k1 = min(l - 1, n1k)
k_aibjakblbj: do k = n0k, k1
if (k == l) cycle k_aibjakblbj
b_aibjakblbj: do b = n0bde, n1bde
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjakblbj: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aibjakblbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakblbj: do a = a0, n1ac
if (a == b) cycle a_aibjakblbj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakblbj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakblbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakblbj(i, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakblbj
end do a_aibjakblbj
end do j_aibjakblbj
end do b_aibjakblbj
end do k_aibjakblbj
end do l_aibjakblbj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k, m
! Equalities: c == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
e_aibiakbiem: do e = n0e, n1e
m_aibiakbiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aibiakbiem: do k = n0k, n1k
if (k == m) cycle k_aibiakbiem
b0 = max(e + 1, n0bd)
b_aibiakbiem: do b = b0, n1bd
if (b == e) cycle b_aibiakbiem
a0 = max(b + 1, n0ac)
a_aibiakbiem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibiakbiem
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakbiem: do i = n0ijl, n1ijl
if (i == k .or. i == m) cycle i_aibiakbiem
if (k > i .and. i > m) cycle i_aibiakbiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, i, e, m)
jac_ibra_iket = eom_cc3_23_trans_aibiakbiem(i, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakbiem
end do a_aibiakbiem
end do b_aibiakbiem
end do k_aibiakbiem
end do m_aibiakbiem
end do e_aibiakbiem
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjakbiei: do e = n0e, n1e
k_aibjakbiei: do k = n0k, n1k
b0 = max(e + 1, n0bd)
b_aibjakbiei: do b = b0, n1bd
if (b == e) cycle b_aibjakbiei
j_aibjakbiei: do j = n0j, n1j
if (j == k) cycle j_aibjakbiei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbiei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakbiei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbiei: do i = n0ilm, n1ilm
if (i == j .or. i == k) cycle i_aibjakbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjakbiei(i, j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbiei
end do a_aibjakbiei
end do j_aibjakbiei
end do b_aibjakbiei
end do k_aibjakbiei
end do e_aibjakbiei
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjakbiej: do e = n0e, n1e
k_aibjakbiej: do k = n0k, n1k
b0 = max(e + 1, n0bd)
b_aibjakbiej: do b = b0, n1bd
if (b == e) cycle b_aibjakbiej
j_aibjakbiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjakbiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbiej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakbiej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbiej: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakbiej
if (k > i .and. i > j) cycle i_aibjakbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakbiej(i, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbiej
end do a_aibjakbiej
end do j_aibjakbiej
end do b_aibjakbiej
end do k_aibjakbiej
end do e_aibjakbiej
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjakbjej: do e = n0e, n1e
k_aibjakbjej: do k = n0k, n1k
b0 = max(e + 1, n0bd)
b_aibjakbjej: do b = b0, n1bd
if (b == e) cycle b_aibjakbjej
j_aibjakbjej: do j = n0jlm, n1jlm
if (j == k) cycle j_aibjakbjej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbjej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakbjej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjej: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, j, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakbjej(i, j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbjej
end do a_aibjakbjej
end do j_aibjakbjej
end do b_aibjakbjej
end do k_aibjakbjej
end do e_aibjakbjej
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaibjem: do e = n0e, n1e
m_aibjaibjem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibjaibjem: do b = b0, n1bd
if (b == e) cycle b_aibjaibjem
j_aibjaibjem: do j = n0jl, n1jl
if (j == m) cycle j_aibjaibjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibjem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaibjem
i_aibjaibjem: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjaibjem
if (i > j .and. j > m) exit i_aibjaibjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, j, e, m)
jac_ibra_iket = eom_cc3_23_trans_aibjaibjem(nocc, a, i, b, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibjem
end do a_aibjaibjem
end do j_aibjaibjem
end do b_aibjaibjem
end do m_aibjaibjem
end do e_aibjaibjem
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjakbjei: do e = n0e, n1e
k_aibjakbjei: do k = n0k, n1k
b0 = max(e + 1, n0bd)
b_aibjakbjei: do b = b0, n1bd
if (b == e) cycle b_aibjakbjei
j_aibjakbjei: do j = n0jl, n1jl
if (j == k) cycle j_aibjakbjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbjei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakbjei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjakbjei
if (k > j .and. j > i) cycle i_aibjakbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjakbjei(nocc, a, i, b, j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbjei
end do a_aibjakbjei
end do j_aibjakbjei
end do b_aibjakbjei
end do k_aibjakbjei
end do e_aibjakbjei
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, k, i
! Equalities: c == a, d == b, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjakbjek: do e = n0e, n1e
k_aibjakbjek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibjakbjek: do b = b0, n1bd
if (b == e) cycle b_aibjakbjek
j_aibjakbjek: do j = n0jl, n1jl
if (j == k) cycle j_aibjakbjek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbjek: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakbjek
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjek: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, j, e, k)
jac_ibra_iket = eom_cc3_23_trans_aibjakbjek(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbjek
end do a_aibjakbjek
end do j_aibjakbjek
end do b_aibjakbjek
end do k_aibjakbjek
end do e_aibjakbjek
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l, j
! Equalities: c == a, d == b, k == i, m == l
! No equalities independent of the above can hold.
!
e_aibjaiblel: do e = n0e, n1e
l_aibjaiblel: do l = n0lm, n1lm
em = (e - nvirt0) * nocc + (l - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibjaiblel: do b = b0, n1bd
if (b == e) cycle b_aibjaiblel
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblel: do j = n0j, n1j
if (j == l) cycle j_aibjaiblel
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaiblel: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaiblel
i_aibjaiblel: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaiblel
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, l, e, l)
jac_ibra_iket = eom_cc3_23_trans_aibjaiblel(j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiblel
end do a_aibjaiblel
end do j_aibjaiblel
end do b_aibjaiblel
end do l_aibjaiblel
end do e_aibjaiblel
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l, m
! Equalities: c == a, d == b, j == i, k == i
! No equalities independent of the above can hold.
!
e_aibiaiblem: do e = n0e, n1e
m_aibiaiblem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l_aibiaiblem: do l = n0l, n1l
if (l == m) cycle l_aibiaiblem
b0 = max(e + 1, n0bd)
b_aibiaiblem: do b = b0, n1bd
if (b == e) cycle b_aibiaiblem
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiaiblem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibiaiblem
i_aibiaiblem: do i = n0ijk, n1ijk
if (i == l .or. i == m) cycle i_aibiaiblem
if (i > l .and. l > m) exit i_aibiaiblem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, l, e, m)
jac_ibra_iket = eom_cc3_23_trans_aibiaiblem(i, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaiblem
end do a_aibiaiblem
end do b_aibiaiblem
end do l_aibiaiblem
end do m_aibiaiblem
end do e_aibiaiblem
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibjaiblei: do e = n0e, n1e
l_aibjaiblei: do l = n0l, n1l
b0 = max(e + 1, n0bd)
b_aibjaiblei: do b = b0, n1bd
if (b == e) cycle b_aibjaiblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblei: do j = n0j, n1j
if (j == l) cycle j_aibjaiblei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaiblei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaiblei
i_aibjaiblei: do i = n0ikm, n1ikm
if (i == j .or. i == l) cycle i_aibjaiblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjaiblei(i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiblei
end do a_aibjaiblei
end do j_aibjaiblei
end do b_aibjaiblei
end do l_aibjaiblei
end do e_aibjaiblei
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjaiblej: do e = n0e, n1e
l_aibjaiblej: do l = n0l, n1l
b0 = max(e + 1, n0bd)
b_aibjaiblej: do b = b0, n1bd
if (b == e) cycle b_aibjaiblej
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblej: do j = n0jm, n1jm
if (j == l) cycle j_aibjaiblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaiblej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaiblej
i_aibjaiblej: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaiblej
if (i > l .and. l > j) exit i_aibjaiblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, l, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjaiblej(nocc, a, i, b, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiblej
end do a_aibjaiblej
end do j_aibjaiblej
end do b_aibjaiblej
end do l_aibjaiblej
end do e_aibjaiblej
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k, l
! Equalities: c == a, d == b, j == i, m == i
! No equalities independent of the above can hold.
!
e_aibiakblei: do e = n0e, n1e
l_aibiakblei: do l = n0l, n1l
k_aibiakblei: do k = n0k, n1k
if (k == l) cycle k_aibiakblei
b0 = max(e + 1, n0bd)
b_aibiakblei: do b = b0, n1bd
if (b == e) cycle b_aibiakblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiakblei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibiakblei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakblei: do i = n0ijm, n1ijm
if (i == k .or. i == l) cycle i_aibiakblei
if (k > l .and. l > i) cycle i_aibiakblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibiakblei(i, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakblei
end do a_aibiakblei
end do b_aibiakblei
end do k_aibiakblei
end do l_aibiakblei
end do e_aibiakblei
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajblei: do e = n0e, n1e
l_aibjajblei: do l = n0l, n1l
b0 = max(e + 1, n0bd)
b_aibjajblei: do b = b0, n1bd
if (b == e) cycle b_aibjajblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjajblei: do j = n0jk, n1jk
if (j == l) cycle j_aibjajblei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajblei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjajblei
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajblei: do i = n0im, n1im
if (i == j .or. i == l) cycle i_aibjajblei
if (j > l .and. l > i) cycle i_aibjajblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, b, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjajblei(a, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajblei
end do a_aibjajblei
end do j_aibjajblei
end do b_aibjajblei
end do l_aibjajblei
end do e_aibjajblei
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjajblej: do e = n0e, n1e
l_aibjajblej: do l = n0l, n1l
b0 = max(e + 1, n0bd)
b_aibjajblej: do b = b0, n1bd
if (b == e) cycle b_aibjajblej
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjajblej: do j = n0jkm, n1jkm
if (j == l) cycle j_aibjajblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajblej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjajblej
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajblej: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, b, l, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjajblej(i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajblej
end do a_aibjajblej
end do j_aibjajblej
end do b_aibjajblej
end do l_aibjajblej
end do e_aibjajblej
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, j, k
! Equalities: c == a, e == d, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjakdidj: do d = n0de, n1de
k_aibjakdidj: do k = n0k, n1k
b_aibjakdidj: do b = n0b, n1b
if (b == d) cycle b_aibjakdidj
j_aibjakdidj: do j = n0jm, n1jm
if (j == k) cycle j_aibjakdidj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdidj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdidj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, k + 1, n0il)
i_aibjakdidj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakdidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, i, d, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakdidj(b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdidj
end do a_aibjakdidj
end do j_aibjakdidj
end do b_aibjakdidj
end do k_aibjakdidj
end do d_aibjakdidj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, j, m
! Equalities: c == a, e == d, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaidjdm: do m = n0m, n1m
d_aibjaidjdm: do d = n0de, n1de
em = (d - nvirt0) * nocc + (m - nocc0) + 1
b_aibjaidjdm: do b = n0b, n1b
if (b == d) cycle b_aibjaidjdm
j0 = max(m + 1, n0jl)
j_aibjaidjdm: do j = j0, n1jl
if (j == m) cycle j_aibjaidjdm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidjdm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidjdm
i1 = min(j - 1, n1ik)
i_aibjaidjdm: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aibjaidjdm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, j, d, m)
jac_ibra_iket = eom_cc3_23_trans_aibjaidjdm(b, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidjdm
end do a_aibjaidjdm
end do j_aibjaidjdm
end do b_aibjaidjdm
end do d_aibjaidjdm
end do m_aibjaidjdm
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, j, k
! Equalities: c == a, e == d, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjakdjdi: do d = n0de, n1de
k_aibjakdjdi: do k = n0k, n1k
b_aibjakdjdi: do b = n0b, n1b
if (b == d) cycle b_aibjakdjdi
j0 = max(k + 1, n0jl)
j_aibjakdjdi: do j = j0, n1jl
if (j == k) cycle j_aibjakdjdi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdjdi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdjdi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjakdjdi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjakdjdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, j, d, i)
jac_ibra_iket = eom_cc3_23_trans_aibjakdjdi(b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdjdi
end do a_aibjakdjdi
end do j_aibjakdjdi
end do b_aibjakdjdi
end do k_aibjakdjdi
end do d_aibjakdjdi
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, j, l
! Equalities: c == a, e == d, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjaidldj: do d = n0de, n1de
l_aibjaidldj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaidldj: do b = n0b, n1b
if (b == d) cycle b_aibjaidldj
j1 = min(l - 1, n1jm)
j_aibjaidldj: do j = n0jm, j1
if (j == l) cycle j_aibjaidldj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidldj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidldj
i1 = min(l - 1, n1ik)
i_aibjaidldj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjaidldj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, d, j)
jac_ibra_iket = eom_cc3_23_trans_aibjaidldj(b, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidldj
end do a_aibjaidldj
end do j_aibjaidldj
end do b_aibjaidldj
end do l_aibjaidldj
end do d_aibjaidldj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, m
! Equalities: c == a, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibiakdibm: do m = n0m, n1m
d_aibiakdibm: do d = n0d, n1d
k_aibiakdibm: do k = n0k, n1k
if (k == m) cycle k_aibiakdibm
b1 = min(d - 1, n1be)
b_aibiakdibm: do b = n0be, b1
if (b == d) cycle b_aibiakdibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibiakdibm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdibm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdibm: do i = n0ijl, n1ijl
if (i == k .or. i == m) cycle i_aibiakdibm
if (k > i .and. i > m) cycle i_aibiakdibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibiakdibm(i, k, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakdibm
end do a_aibiakdibm
end do b_aibiakdibm
end do k_aibiakdibm
end do d_aibiakdibm
end do m_aibiakdibm
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, e == b, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjakdibi: do d = n0d, n1d
k_aibjakdibi: do k = n0k, n1k
b1 = min(d - 1, n1be)
b_aibjakdibi: do b = n0be, b1
if (b == d) cycle b_aibjakdibi
j_aibjakdibi: do j = n0j, n1j
if (j == k) cycle j_aibjakdibi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdibi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdibi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdibi: do i = n0ilm, n1ilm
if (i == j .or. i == k) cycle i_aibjakdibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, i, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjakdibi(i, j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdibi
end do a_aibjakdibi
end do j_aibjakdibi
end do b_aibjakdibi
end do k_aibjakdibi
end do d_aibjakdibi
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, m
! Equalities: c == a, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjajdibm: do m = n0m, n1m
d_aibjajdibm: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjajdibm: do b = n0be, b1
if (b == d) cycle b_aibjajdibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjajdibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjajdibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdibm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdibm
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdibm: do i = n0il, n1il
if (i == j .or. i == m) cycle i_aibjajdibm
if (j > i .and. i > m) cycle i_aibjajdibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjajdibm(a, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdibm
end do a_aibjajdibm
end do j_aibjajdibm
end do b_aibjajdibm
end do d_aibjajdibm
end do m_aibjajdibm
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, j
! Equalities: c == a, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjakdibk: do d = n0d, n1d
k_aibjakdibk: do k = n0km, n1km
b1 = min(d - 1, n1be)
b_aibjakdibk: do b = n0be, b1
if (b == d) cycle b_aibjakdibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjakdibk: do j = n0j, n1j
if (j == k) cycle j_aibjakdibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdibk: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdibk: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, i, b, k)
jac_ibra_iket = eom_cc3_23_trans_aibjakdibk(j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdibk
end do a_aibjakdibk
end do j_aibjakdibk
end do b_aibjakdibk
end do k_aibjakdibk
end do d_aibjakdibk
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjakdibj: do d = n0d, n1d
k_aibjakdibj: do k = n0k, n1k
b1 = min(d - 1, n1be)
b_aibjakdibj: do b = n0be, b1
if (b == d) cycle b_aibjakdibj
j_aibjakdibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjakdibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdibj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdibj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdibj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakdibj
if (k > i .and. i > j) cycle i_aibjakdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakdibj(nocc, a, i, b, j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdibj
end do a_aibjakdibj
end do j_aibjakdibj
end do b_aibjakdibj
end do k_aibjakdibj
end do d_aibjakdibj
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k
! Equalities: c == a, e == b, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjakdjbj: do d = n0d, n1d
k_aibjakdjbj: do k = n0k, n1k
b1 = min(d - 1, n1be)
b_aibjakdjbj: do b = n0be, b1
if (b == d) cycle b_aibjakdjbj
j_aibjakdjbj: do j = n0jlm, n1jlm
if (j == k) cycle j_aibjakdjbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdjbj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdjbj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakdjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, j, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakdjbj(i, j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdjbj
end do a_aibjakdjbj
end do j_aibjakdjbj
end do b_aibjakdjbj
end do k_aibjakdjbj
end do d_aibjakdjbj
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, m
! Equalities: c == a, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaidjbm: do m = n0m, n1m
d_aibjaidjbm: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjaidjbm: do b = n0be, b1
if (b == d) cycle b_aibjaidjbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaidjbm: do j = n0jl, n1jl
if (j == m) cycle j_aibjaidjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidjbm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidjbm
i_aibjaidjbm: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjaidjbm
if (i > j .and. j > m) exit i_aibjaidjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, j, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjaidjbm(nocc, a, i, b, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidjbm
end do a_aibjaidjbm
end do j_aibjaidjbm
end do b_aibjaidjbm
end do d_aibjaidjbm
end do m_aibjaidjbm
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjakdjbi: do d = n0d, n1d
k_aibjakdjbi: do k = n0k, n1k
b1 = min(d - 1, n1be)
b_aibjakdjbi: do b = n0be, b1
if (b == d) cycle b_aibjakdjbi
j_aibjakdjbi: do j = n0jl, n1jl
if (j == k) cycle j_aibjakdjbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdjbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdjbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjbi: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjakdjbi
if (k > j .and. j > i) cycle i_aibjakdjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, j, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjakdjbi(i, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdjbi
end do a_aibjakdjbi
end do j_aibjakdjbi
end do b_aibjakdjbi
end do k_aibjakdjbi
end do d_aibjakdjbi
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

end subroutine ccjac_23_dav_part3
end module ccjac_block_23_dav_part3
