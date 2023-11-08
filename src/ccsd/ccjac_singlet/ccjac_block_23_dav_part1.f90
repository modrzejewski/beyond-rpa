module ccjac_block_23_dav_part1
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
subroutine ccjac_23_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, a1, b0, b1, c0, d0
integer :: n0ac, n0ad, n0ae, n0bc, n0bd
integer :: n0be, n0ik, n0il, n0im, n0jk
integer :: n0jl, n0jm
integer :: n1ac, n1ad, n1ae, n1bc, n1bd
integer :: n1be, n1ik, n1il, n1im, n1jk
integer :: n1jl, n1jm
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
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0bc = max(n0b, n0c)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n1ac = min(n1a, n1c)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bc = min(n1b, n1c)
n1bd = min(n1b, n1d)
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
! Free virtual indices: a, b, e
! Free occupied indices: j, i, k, m
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
e_aibjakbjem: do e = n0e, n1e
m_aibjakbjem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aibjakbjem: do k = n0k, n1k
if (k == m) cycle k_aibjakbjem
b0 = max(e + 1, n0bd)
b_aibjakbjem: do b = b0, n1bd
if (b == e) cycle b_aibjakbjem
j_aibjakbjem: do j = n0jl, n1jl
if (j == k .or. j == m) cycle j_aibjakbjem
if (k > j .and. j > m) cycle j_aibjakbjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbjem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakbjem
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjem: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakbjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, j, e, m)
jac_ibra_iket = eom_cc3_23_trans_aibjakbjem(i, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbjem
end do a_aibjakbjem
end do j_aibjakbjem
end do b_aibjakbjem
end do k_aibjakbjem
end do m_aibjakbjem
end do e_aibjakbjem
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l, m
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
e_aibjaiblem: do e = n0e, n1e
m_aibjaiblem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l_aibjaiblem: do l = n0l, n1l
if (l == m) cycle l_aibjaiblem
b0 = max(e + 1, n0bd)
b_aibjaiblem: do b = b0, n1bd
if (b == e) cycle b_aibjaiblem
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblem: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aibjaiblem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaiblem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaiblem
i_aibjaiblem: do i = n0ik, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aibjaiblem
if (i > l .and. l > m) exit i_aibjaiblem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, l, e, m)
jac_ibra_iket = eom_cc3_23_trans_aibjaiblem(j, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiblem
end do a_aibjaiblem
end do j_aibjaiblem
end do b_aibjaiblem
end do l_aibjaiblem
end do m_aibjaiblem
end do e_aibjaiblem
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == b, m == i
! No equalities independent of the above can hold.
!
e_aibjakblei: do e = n0e, n1e
l_aibjakblei: do l = n0l, n1l
k_aibjakblei: do k = n0k, n1k
if (k == l) cycle k_aibjakblei
b0 = max(e + 1, n0bd)
b_aibjakblei: do b = b0, n1bd
if (b == e) cycle b_aibjakblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjakblei: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjakblei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakblei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakblei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakblei: do i = n0im, n1im
if (i == j .or. i == k .or. i == l) cycle i_aibjakblei
if (k > l .and. l > i) cycle i_aibjakblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjakblei(j, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakblei
end do a_aibjakblei
end do j_aibjakblei
end do b_aibjakblei
end do k_aibjakblei
end do l_aibjakblei
end do e_aibjakblei
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, k, l
! Equalities: c == a, d == b, m == j
! No equalities independent of the above can hold.
!
e_aibjakblej: do e = n0e, n1e
l_aibjakblej: do l = n0l, n1l
k_aibjakblej: do k = n0k, n1k
if (k == l) cycle k_aibjakblej
b0 = max(e + 1, n0bd)
b_aibjakblej: do b = b0, n1bd
if (b == e) cycle b_aibjakblej
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjakblej: do j = n0jm, n1jm
if (j == k .or. j == l) cycle j_aibjakblej
if (k > l .and. l > j) cycle j_aibjakblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakblej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakblej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakblej: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, l, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakblej(i, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakblej
end do a_aibjakblej
end do j_aibjakblej
end do b_aibjakblej
end do k_aibjakblej
end do l_aibjakblej
end do e_aibjakblej
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k, m
! Equalities: c == a, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjakdibm: do m = n0m, n1m
d_aibjakdibm: do d = n0d, n1d
k_aibjakdibm: do k = n0k, n1k
if (k == m) cycle k_aibjakdibm
b1 = min(d - 1, n1be)
b_aibjakdibm: do b = n0be, b1
if (b == d) cycle b_aibjakdibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjakdibm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjakdibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdibm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdibm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdibm: do i = n0il, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjakdibm
if (k > i .and. i > m) cycle i_aibjakdibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjakdibm(j, k, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdibm
end do a_aibjakdibm
end do j_aibjakdibm
end do b_aibjakdibm
end do k_aibjakdibm
end do d_aibjakdibm
end do m_aibjakdibm
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k, m
! Equalities: c == a, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjakdjbm: do m = n0m, n1m
d_aibjakdjbm: do d = n0d, n1d
k_aibjakdjbm: do k = n0k, n1k
if (k == m) cycle k_aibjakdjbm
b1 = min(d - 1, n1be)
b_aibjakdjbm: do b = n0be, b1
if (b == d) cycle b_aibjakdjbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjakdjbm: do j = n0jl, n1jl
if (j == k .or. j == m) cycle j_aibjakdjbm
if (k > j .and. j > m) cycle j_aibjakdjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdjbm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdjbm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjbm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakdjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, j, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjakdjbm(i, k, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdjbm
end do a_aibjakdjbm
end do j_aibjakdjbm
end do b_aibjakdjbm
end do k_aibjakdjbm
end do d_aibjakdjbm
end do m_aibjakdjbm
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l, m
! Equalities: c == a, e == b, k == i
! No equalities independent of the above can hold.
!
m_aibjaidlbm: do m = n0m, n1m
d_aibjaidlbm: do d = n0d, n1d
l_aibjaidlbm: do l = n0l, n1l
if (l == m) cycle l_aibjaidlbm
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjaidlbm: do b = n0be, b1
if (b == d) cycle b_aibjaidlbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaidlbm: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aibjaidlbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidlbm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidlbm
i_aibjaidlbm: do i = n0ik, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aibjaidlbm
if (i > l .and. l > m) exit i_aibjaidlbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjaidlbm(j, d, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidlbm
end do a_aibjaidlbm
end do j_aibjaidlbm
end do b_aibjaidlbm
end do l_aibjaidlbm
end do d_aibjaidlbm
end do m_aibjaidlbm
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k, l
! Equalities: c == a, e == b, m == j
! No equalities independent of the above can hold.
!
d_aibjakdlbj: do d = n0d, n1d
l_aibjakdlbj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aibjakdlbj: do k = n0k, n1k
if (k == l) cycle k_aibjakdlbj
b1 = min(d - 1, n1be)
b_aibjakdlbj: do b = n0be, b1
if (b == d) cycle b_aibjakdlbj
j_aibjakdlbj: do j = n0jm, n1jm
if (j == k .or. j == l) cycle j_aibjakdlbj
if (k > l .and. l > j) cycle j_aibjakdlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdlbj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdlbj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdlbj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakdlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakdlbj(i, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdlbj
end do a_aibjakdlbj
end do j_aibjakdlbj
end do b_aibjakdlbj
end do k_aibjakdlbj
end do l_aibjakdlbj
end do d_aibjakdlbj
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j, k
! Equalities: c == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjakdiej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjakdiej: do d = d0, n1d
if (d == e) cycle d_aibjakdiej
k_aibjakdiej: do k = n0k, n1k
b_aibjakdiej: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjakdiej
j_aibjakdiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjakdiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdiej: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjakdiej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdiej: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakdiej
if (k > i .and. i > j) cycle i_aibjakdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakdiej(b, k, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdiej
end do a_aibjakdiej
end do j_aibjakdiej
end do b_aibjakdiej
end do k_aibjakdiej
end do d_aibjakdiej
end do e_aibjakdiej
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j, m
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaidjem: do e = n0e, n1e
m_aibjaidjem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d0 = max(e + 1, n0d)
d_aibjaidjem: do d = d0, n1d
if (d == e) cycle d_aibjaidjem
b_aibjaidjem: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjaidjem
j_aibjaidjem: do j = n0jl, n1jl
if (j == m) cycle j_aibjaidjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidjem: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjaidjem
i_aibjaidjem: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjaidjem
if (i > j .and. j > m) exit i_aibjaidjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, j, e, m)
jac_ibra_iket = eom_cc3_23_trans_aibjaidjem(b, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidjem
end do a_aibjaidjem
end do j_aibjaidjem
end do b_aibjaidjem
end do d_aibjaidjem
end do m_aibjaidjem
end do e_aibjaidjem
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j, k
! Equalities: c == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjakdjei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjakdjei: do d = d0, n1d
if (d == e) cycle d_aibjakdjei
k_aibjakdjei: do k = n0k, n1k
b_aibjakdjei: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjakdjei
j_aibjakdjei: do j = n0jl, n1jl
if (j == k) cycle j_aibjakdjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdjei: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjakdjei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjakdjei
if (k > j .and. j > i) cycle i_aibjakdjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjakdjei(b, k, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdjei
end do a_aibjakdjei
end do j_aibjakdjei
end do b_aibjakdjei
end do k_aibjakdjei
end do d_aibjakdjei
end do e_aibjakdjei
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j, l
! Equalities: c == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjaidlej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjaidlej: do d = d0, n1d
if (d == e) cycle d_aibjaidlej
l_aibjaidlej: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaidlej: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjaidlej
j_aibjaidlej: do j = n0jm, n1jm
if (j == l) cycle j_aibjaidlej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidlej: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjaidlej
i_aibjaidlej: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaidlej
if (i > l .and. l > j) exit i_aibjaidlej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjaidlej(b, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidlej
end do a_aibjaidlej
end do j_aibjaidlej
end do b_aibjaidlej
end do l_aibjaidlej
end do d_aibjaidlej
end do e_aibjaidlej
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j, m
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjdiem: do e = n0e, n1e
m_aibjbjdiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d0 = max(e + 1, n0d)
d_aibjbjdiem: do d = d0, n1d
if (d == e) cycle d_aibjbjdiem
b0 = max(d + 1, n0bc)
b_aibjbjdiem: do b = b0, n1bc
if (b == d .or. b == e) cycle b_aibjbjdiem
j_aibjbjdiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjbjdiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdiem: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbjdiem
i_aibjbjdiem: do i = n0il, n1il
if (i == j .or. i == m) cycle i_aibjbjdiem
if (j > i .and. i > m) cycle i_aibjbjdiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, j, d, i, e, m)
jac_ibra_iket = eom_cc3_23_trans_aibjbjdiem(a, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdiem
end do a_aibjbjdiem
end do j_aibjbjdiem
end do b_aibjbjdiem
end do d_aibjbjdiem
end do m_aibjbjdiem
end do e_aibjbjdiem
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j, k
! Equalities: c == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbkdiej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjbkdiej: do d = d0, n1d
if (d == e) cycle d_aibjbkdiej
k_aibjbkdiej: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibjbkdiej: do b = b0, n1bc
if (b == d .or. b == e) cycle b_aibjbkdiej
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjbkdiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdiej: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbkdiej
i_aibjbkdiej: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjbkdiej
if (k > i .and. i > j) cycle i_aibjbkdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, k, d, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjbkdiej(a, k, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdiej
end do a_aibjbkdiej
end do j_aibjbkdiej
end do b_aibjbkdiej
end do k_aibjbkdiej
end do d_aibjbkdiej
end do e_aibjbkdiej
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j, k
! Equalities: c == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbkdjei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjbkdjei: do d = d0, n1d
if (d == e) cycle d_aibjbkdjei
k_aibjbkdjei: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibjbkdjei: do b = b0, n1bc
if (b == d .or. b == e) cycle b_aibjbkdjei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdjei: do j = n0jl, n1jl
if (j == k) cycle j_aibjbkdjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdjei: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbkdjei
i_aibjbkdjei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjbkdjei
if (k > j .and. j > i) cycle i_aibjbkdjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, k, d, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjbkdjei(a, k, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdjei
end do a_aibjbkdjei
end do j_aibjbkdjei
end do b_aibjbkdjei
end do k_aibjbkdjei
end do d_aibjbkdjei
end do e_aibjbkdjei
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j, l
! Equalities: c == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjdlei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjbjdlei: do d = d0, n1d
if (d == e) cycle d_aibjbjdlei
l_aibjbjdlei: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b0 = max(d + 1, n0bc)
b_aibjbjdlei: do b = b0, n1bc
if (b == d .or. b == e) cycle b_aibjbjdlei
j_aibjbjdlei: do j = n0jk, n1jk
if (j == l) cycle j_aibjbjdlei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdlei: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbjdlei
i_aibjbjdlei: do i = n0im, n1im
if (i == j .or. i == l) cycle i_aibjbjdlei
if (j > l .and. l > i) cycle i_aibjbjdlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, j, d, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjbjdlei(a, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdlei
end do a_aibjbjdlei
end do j_aibjbjdlei
end do b_aibjbjdlei
end do l_aibjbjdlei
end do d_aibjbjdlei
end do e_aibjbjdlei
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, m
! Equalities: d == a, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjckaibm: do m = n0m, n1m
c_aibjckaibm: do c = n0c, n1c
k_aibjckaibm: do k = n0k, n1k
if (k == m) cycle k_aibjckaibm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibm: do b = n0be, n1be
if (b == c) cycle b_aibjckaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjckaibm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjckaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckaibm: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckaibm
i_aibjckaibm: do i = n0il, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjckaibm
if (k > i .and. i > m) cycle i_aibjckaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjckaibm(j, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaibm
end do a_aibjckaibm
end do j_aibjckaibm
end do b_aibjckaibm
end do k_aibjckaibm
end do c_aibjckaibm
end do m_aibjckaibm
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l, m
! Equalities: d == a, e == b, k == i
! No equalities independent of the above can hold.
!
m_aibjcialbm: do m = n0m, n1m
l_aibjcialbm: do l = n0l, n1l
if (l == m) cycle l_aibjcialbm
c_aibjcialbm: do c = n0c, n1c
b_aibjcialbm: do b = n0be, n1be
if (b == c) cycle b_aibjcialbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcialbm: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aibjcialbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcialbm: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcialbm
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialbm: do i = n0ik, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aibjcialbm
if (i > l .and. l > m) exit i_aibjcialbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcialbm(j, c, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcialbm
end do a_aibjcialbm
end do j_aibjcialbm
end do b_aibjcialbm
end do c_aibjcialbm
end do l_aibjcialbm
end do m_aibjcialbm
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l, m
! Equalities: d == a, e == b, k == j
! No equalities independent of the above can hold.
!
m_aibjcjalbm: do m = n0m, n1m
l_aibjcjalbm: do l = n0l, n1l
if (l == m) cycle l_aibjcjalbm
c_aibjcjalbm: do c = n0c, n1c
b_aibjcjalbm: do b = n0be, n1be
if (b == c) cycle b_aibjcjalbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjalbm: do j = n0jk, n1jk
if (j == l .or. j == m) cycle j_aibjcjalbm
if (j > l .and. l > m) exit j_aibjcjalbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjalbm: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjalbm
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalbm: do i = n0i, n1i
if (i == j .or. i == l .or. i == m) cycle i_aibjcjalbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, l, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcjalbm(i, c, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjalbm
end do a_aibjcjalbm
end do j_aibjcjalbm
end do b_aibjcjalbm
end do c_aibjcjalbm
end do l_aibjcjalbm
end do m_aibjcjalbm
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k, l
! Equalities: d == a, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjckalbj: do l = n0l, n1l
c_aibjckalbj: do c = n0c, n1c
k_aibjckalbj: do k = n0k, n1k
if (k == l) cycle k_aibjckalbj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckalbj: do b = n0be, n1be
if (b == c) cycle b_aibjckalbj
j_aibjckalbj: do j = n0jm, n1jm
if (j == k .or. j == l) cycle j_aibjckalbj
if (k > l .and. l > j) cycle j_aibjckalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckalbj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckalbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalbj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjckalbj(i, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckalbj
end do a_aibjckalbj
end do j_aibjckalbj
end do b_aibjckalbj
end do k_aibjckalbj
end do c_aibjckalbj
end do l_aibjckalbj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, m
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjaiem: do e = n0e, n1e
m_aibjcjaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aibjcjaiem: do c = n0c, n1c
if (c == e) cycle c_aibjcjaiem
b_aibjcjaiem: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjcjaiem
j_aibjcjaiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjaiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjaiem: do a = a0, a1
if (a == b .or. a == c .or. a == e) cycle a_aibjcjaiem
i_aibjcjaiem: do i = n0il, n1il
if (i == j .or. i == m) cycle i_aibjcjaiem
if (j > i .and. i > m) cycle i_aibjcjaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, i, e, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcjaiem(b, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaiem
end do a_aibjcjaiem
end do j_aibjcjaiem
end do b_aibjcjaiem
end do c_aibjcjaiem
end do m_aibjcjaiem
end do e_aibjcjaiem
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, k
! Equalities: d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjckaiej: do e = n0e, n1e
c_aibjckaiej: do c = n0c, n1c
if (c == e) cycle c_aibjckaiej
k_aibjckaiej: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaiej: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjckaiej
j_aibjckaiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjckaiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckaiej: do a = a0, a1
if (a == b .or. a == c .or. a == e) cycle a_aibjckaiej
i_aibjckaiej: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaiej
if (k > i .and. i > j) cycle i_aibjckaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjckaiej(b, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaiej
end do a_aibjckaiej
end do j_aibjckaiej
end do b_aibjckaiej
end do k_aibjckaiej
end do c_aibjckaiej
end do e_aibjckaiej
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, l
! Equalities: d == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjcialej: do e = n0e, n1e
l_aibjcialej: do l = n0l, n1l
c_aibjcialej: do c = n0c, n1c
if (c == e) cycle c_aibjcialej
b_aibjcialej: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjcialej
j_aibjcialej: do j = n0jm, n1jm
if (j == l) cycle j_aibjcialej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcialej: do a = a0, a1
if (a == b .or. a == c .or. a == e) cycle a_aibjcialej
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialej: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcialej
if (i > l .and. l > j) exit i_aibjcialej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcialej(b, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcialej
end do a_aibjcialej
end do j_aibjcialej
end do b_aibjcialej
end do c_aibjcialej
end do l_aibjcialej
end do e_aibjcialej
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, l
! Equalities: d == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjalei: do e = n0e, n1e
l_aibjcjalei: do l = n0l, n1l
c_aibjcjalei: do c = n0c, n1c
if (c == e) cycle c_aibjcjalei
b_aibjcjalei: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjcjalei
j_aibjcjalei: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjalei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjalei: do a = a0, a1
if (a == b .or. a == c .or. a == e) cycle a_aibjcjalei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalei: do i = n0im, n1im
if (i == j .or. i == l) cycle i_aibjcjalei
if (j > l .and. l > i) cycle i_aibjcjalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjalei(b, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjalei
end do a_aibjcjalei
end do j_aibjcjalei
end do b_aibjcjalei
end do c_aibjcjalei
end do l_aibjcjalei
end do e_aibjcjalei
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, m
! Equalities: e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjdiam: do m = n0m, n1m
d_aibjcjdiam: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcjdiam: do c = c0, n1c
if (c == d) cycle c_aibjcjdiam
b_aibjcjdiam: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdiam
j_aibjcjdiam: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjdiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcjdiam: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjcjdiam: do i = n0il, n1il
if (i == j .or. i == m) cycle i_aibjcjdiam
if (j > i .and. i > m) cycle i_aibjcjdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, d, i, a, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcjdiam(b, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdiam
end do a_aibjcjdiam
end do j_aibjcjdiam
end do b_aibjcjdiam
end do c_aibjcjdiam
end do d_aibjcjdiam
end do m_aibjcjdiam
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, m
! Equalities: e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcidjam: do m = n0m, n1m
d_aibjcidjam: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcidjam: do c = c0, n1c
if (c == d) cycle c_aibjcidjam
b_aibjcidjam: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcidjam
j_aibjcidjam: do j = n0jl, n1jl
if (j == m) cycle j_aibjcidjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcidjam: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjcidjam: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjcidjam
if (i > j .and. j > m) exit i_aibjcidjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, j, a, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcidjam(b, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidjam
end do a_aibjcidjam
end do j_aibjcidjam
end do b_aibjcidjam
end do c_aibjcidjam
end do d_aibjcidjam
end do m_aibjcidjam
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, k
! Equalities: e == a, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjckdjai: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjckdjai: do c = c0, n1c
if (c == d) cycle c_aibjckdjai
k_aibjckdjai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdjai: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjckdjai
j_aibjckdjai: do j = n0jl, n1jl
if (j == k) cycle j_aibjckdjai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjckdjai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjckdjai
i_aibjckdjai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckdjai
if (k > j .and. j > i) cycle i_aibjckdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, d, j, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibjckdjai(b, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdjai
end do a_aibjckdjai
end do j_aibjckdjai
end do b_aibjckdjai
end do k_aibjckdjai
end do c_aibjckdjai
end do d_aibjckdjai
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, l
! Equalities: e == a, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdlai: do d = n0d, n1d
l_aibjcjdlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c0 = max(d + 1, n0c)
c_aibjcjdlai: do c = c0, n1c
if (c == d) cycle c_aibjcjdlai
b_aibjcjdlai: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdlai
j_aibjcjdlai: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjdlai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcjdlai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdlai
i_aibjcjdlai: do i = n0im, n1im
if (i == j .or. i == l) cycle i_aibjcjdlai
if (j > l .and. l > i) cycle i_aibjcjdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, d, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjdlai(b, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdlai
end do a_aibjcjdlai
end do j_aibjcjdlai
end do b_aibjcjdlai
end do c_aibjcjdlai
end do l_aibjcjdlai
end do d_aibjcjdlai
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, m
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjcibjem: do e = n0e, n1e
m_aibjcibjem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aibjcibjem: do c = n0c, n1c
if (c == e) cycle c_aibjcibjem
b0 = max(e + 1, n0bd)
b1 = min(c - 1, n1bd)
b_aibjcibjem: do b = b0, b1
if (b == c .or. b == e) cycle b_aibjcibjem
j_aibjcibjem: do j = n0jl, n1jl
if (j == m) cycle j_aibjcibjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibjem: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcibjem
i_aibjcibjem: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjcibjem
if (i > j .and. j > m) exit i_aibjcibjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, b, j, e, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcibjem(a, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibjem
end do a_aibjcibjem
end do j_aibjcibjem
end do b_aibjcibjem
end do c_aibjcibjem
end do m_aibjcibjem
end do e_aibjcibjem
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, k
! Equalities: d == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjckbjei: do e = n0e, n1e
c_aibjckbjei: do c = n0c, n1c
if (c == e) cycle c_aibjckbjei
k_aibjckbjei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(e + 1, n0bd)
b1 = min(c - 1, n1bd)
b_aibjckbjei: do b = b0, b1
if (b == c .or. b == e) cycle b_aibjckbjei
j_aibjckbjei: do j = n0jl, n1jl
if (j == k) cycle j_aibjckbjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbjei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckbjei
i_aibjckbjei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckbjei
if (k > j .and. j > i) cycle i_aibjckbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, b, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjckbjei(a, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbjei
end do a_aibjckbjei
end do j_aibjckbjei
end do b_aibjckbjei
end do k_aibjckbjei
end do c_aibjckbjei
end do e_aibjckbjei
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
end subroutine ccjac_23_dav_part1
end module ccjac_block_23_dav_part1
