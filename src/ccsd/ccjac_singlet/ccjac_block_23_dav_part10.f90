module ccjac_block_23_dav_part10
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
subroutine ccjac_23_dav_part10(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, a1, b0, b1, i0, i1, j0, j1
integer :: n0abde, n0abe, n0ac, n0ae, n0bc
integer :: n0bcd, n0be, n0cd, n0de, n0ijk
integer :: n0ijkl, n0ijkm, n0ijl, n0ijlm, n0ik
integer :: n0ikl, n0ikm, n0il, n0ilm, n0im
integer :: n0jk, n0jkm, n0jl, n0jlm, n0jm
integer :: n0lm
integer :: n1abde, n1abe, n1ac, n1ae, n1bc
integer :: n1bcd, n1be, n1cd, n1de, n1ijk
integer :: n1ijkl, n1ijkm, n1ijl, n1ijlm, n1ik
integer :: n1ikl, n1ikm, n1il, n1ilm, n1im
integer :: n1jk, n1jkm, n1jl, n1jlm, n1jm
integer :: n1lm
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


n0abde = max(n0a, n0b, n0d, n0e)
n0abe = max(n0a, n0b, n0e)
n0ac = max(n0a, n0c)
n0ae = max(n0a, n0e)
n0bc = max(n0b, n0c)
n0bcd = max(n0b, n0c, n0d)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
n0de = max(n0d, n0e)
n0ijk = max(n0i, n0j, n0k)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijkm = max(n0i, n0j, n0k, n0m)
n0ijl = max(n0i, n0j, n0l)
n0ijlm = max(n0i, n0j, n0l, n0m)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
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
n1abde = min(n1a, n1b, n1d, n1e)
n1abe = min(n1a, n1b, n1e)
n1ac = min(n1a, n1c)
n1ae = min(n1a, n1e)
n1bc = min(n1b, n1c)
n1bcd = min(n1b, n1c, n1d)
n1be = min(n1b, n1e)
n1cd = min(n1c, n1d)
n1de = min(n1d, n1e)
n1ijk = min(n1i, n1j, n1k)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1ijkm = min(n1i, n1j, n1k, n1m)
n1ijl = min(n1i, n1j, n1l)
n1ijlm = min(n1i, n1j, n1l, n1m)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
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
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjajdibj: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjajdibj: do b = n0be, b1
if (b == d) cycle b_aibjajdibj
j_aibjajdibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdibj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdibj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdibj: do i = n0il, n1il
if (i == j) cycle i_aibjajdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjajdibj(nocc, a, i, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdibj
end do a_aibjajdibj
end do j_aibjajdibj
end do b_aibjajdibj
end do d_aibjajdibj
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjaidjbj: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjaidjbj: do b = n0be, b1
if (b == d) cycle b_aibjaidjbj
j_aibjaidjbj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidjbj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidjbj
i_aibjaidjbj: do i = n0ik, n1ik
if (i == j) cycle i_aibjaidjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, j, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjaidjbj(nocc, a, i, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidjbj
end do a_aibjaidjbj
end do j_aibjaidjbj
end do b_aibjaidjbj
end do d_aibjaidjbj
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidjbi: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjaidjbi: do b = n0be, b1
if (b == d) cycle b_aibjaidjbi
j_aibjaidjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidjbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidjbi
i_aibjaidjbi: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjaidjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, j, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjaidjbi(nocc, a, i, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidjbi
end do a_aibjaidjbi
end do j_aibjaidjbi
end do b_aibjaidjbi
end do d_aibjaidjbi
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l
! Equalities: c == a, e == b, j == i, k == i, m == l
! No equalities independent of the above can hold.
!
d_aibiaidlbl: do d = n0d, n1d
l_aibiaidlbl: do l = n0lm, n1lm
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibiaidlbl: do b = n0be, b1
if (b == d) cycle b_aibiaidlbl
em = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidlbl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidlbl
i_aibiaidlbl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibiaidlbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, b, l)
jac_ibra_iket = eom_cc3_23_trans_aibiaidlbl(i, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaidlbl
end do a_aibiaidlbl
end do b_aibiaidlbl
end do l_aibiaidlbl
end do d_aibiaidlbl
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l
! Equalities: c == a, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibiaidlbi: do d = n0d, n1d
l_aibiaidlbi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibiaidlbi: do b = n0be, b1
if (b == d) cycle b_aibiaidlbi
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidlbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidlbi
i_aibiaidlbi: do i = n0ijkm, n1ijkm
if (i == l) cycle i_aibiaidlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibiaidlbi(nocc, a, i, b, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaidlbi
end do a_aibiaidlbi
end do b_aibiaidlbi
end do l_aibiaidlbi
end do d_aibiaidlbi
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, k
! Equalities: c == b, d == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibibkbiei: do e = n0e, n1e
k_aibibkbiei: do k = n0k, n1k
b0 = max(e + 1, n0bcd)
b_aibibkbiei: do b = b0, n1bcd
if (b == e) cycle b_aibibkbiei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkbiei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibibkbiei
i1 = min(k - 1, n1ijlm)
i_aibibkbiei: do i = n0ijlm, i1
if (i == k) cycle i_aibibkbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, k, b, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibibkbiei(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbiei
end do a_aibibkbiei
end do b_aibibkbiei
end do k_aibibkbiei
end do e_aibibkbiei
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjbiei: do e = n0e, n1e
b0 = max(e + 1, n0bcd)
b_aibjbjbiei: do b = b0, n1bcd
if (b == e) cycle b_aibjbjbiei
j_aibjbjbiei: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjbiei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbjbiei
i1 = min(j - 1, n1ilm)
i_aibjbjbiei: do i = n0ilm, i1
if (i == j) cycle i_aibjbjbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, j, b, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjbjbiei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjbiei
end do a_aibjbjbiei
end do j_aibjbjbiei
end do b_aibjbjbiei
end do e_aibjbjbiei
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjbjbiej: do e = n0e, n1e
b0 = max(e + 1, n0bcd)
b_aibjbjbiej: do b = b0, n1bcd
if (b == e) cycle b_aibjbjbiej
j_aibjbjbiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjbiej: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbjbiej
i1 = min(j - 1, n1il)
i_aibjbjbiej: do i = n0il, i1
if (i == j) cycle i_aibjbjbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, j, b, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjbjbiej(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjbiej
end do a_aibjbjbiej
end do j_aibjbjbiej
end do b_aibjbjbiej
end do e_aibjbjbiej
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjbibjej: do e = n0e, n1e
b0 = max(e + 1, n0bcd)
b_aibjbibjej: do b = b0, n1bcd
if (b == e) cycle b_aibjbibjej
j_aibjbibjej: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibjej: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbibjej
i0 = max(j + 1, n0ik)
i_aibjbibjej: do i = i0, n1ik
if (i == j) cycle i_aibjbibjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, i, b, j, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjbibjej(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibjej
end do a_aibjbibjej
end do j_aibjbibjej
end do b_aibjbibjej
end do e_aibjbibjej
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbibjei: do e = n0e, n1e
b0 = max(e + 1, n0bcd)
b_aibjbibjei: do b = b0, n1bcd
if (b == e) cycle b_aibjbibjei
j_aibjbibjei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibjei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbibjei
i0 = max(j + 1, n0ikm)
i_aibjbibjei: do i = i0, n1ikm
if (i == j) cycle i_aibjbibjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, i, b, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjbibjei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibjei
end do a_aibjbibjei
end do j_aibjbibjei
end do b_aibjbibjei
end do e_aibjbibjei
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, l
! Equalities: c == b, d == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibibiblei: do e = n0e, n1e
l_aibibiblei: do l = n0l, n1l
b0 = max(e + 1, n0bcd)
b_aibibiblei: do b = b0, n1bcd
if (b == e) cycle b_aibibiblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibiblei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibibiblei
i0 = max(l + 1, n0ijkm)
i_aibibiblei: do i = i0, n1ijkm
if (i == l) cycle i_aibibiblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, i, b, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibibiblei(a, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibiblei
end do a_aibibiblei
end do b_aibibiblei
end do l_aibibiblei
end do e_aibibiblei
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, d == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckciaj: do c = n0cd, n1cd
k_aiajckciaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckciaj: do j = n0jm, n1jm
if (j == k) cycle j_aiajckciaj
a1 = min(c - 1, n1abe)
a_aiajckciaj: do a = n0abe, a1
if (a == c) cycle a_aiajckciaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i1 = min(k - 1, j - 1, n1il)
i_aiajckciaj: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajckciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, c, i, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajckciaj(a, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckciaj
end do a_aiajckciaj
end do j_aiajckciaj
end do k_aiajckciaj
end do c_aiajckciaj
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, e == a, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajcicjam: do m = n0m, n1m
c_aiajcicjam: do c = n0cd, n1cd
j1 = min(m - 1, n1jl)
j_aiajcicjam: do j = n0jl, j1
if (j == m) cycle j_aiajcicjam
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abe)
a_aiajcicjam: do a = n0abe, a1
if (a == c) cycle a_aiajcicjam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcicjam: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aiajcicjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, c, j, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiajcicjam(a, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicjam
end do a_aiajcicjam
end do j_aiajcicjam
end do c_aiajcicjam
end do m_aiajcicjam
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, d == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajckcjai: do c = n0cd, n1cd
k_aiajckcjai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckcjai: do j = n0jl, j1
if (j == k) cycle j_aiajckcjai
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abe)
a_aiajckcjai: do a = n0abe, a1
if (a == c) cycle a_aiajckcjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajckcjai: do i = i0, n1im
if (i == j .or. i == k) cycle i_aiajckcjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, c, j, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiajckcjai(a, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckcjai
end do a_aiajckcjai
end do j_aiajckcjai
end do k_aiajckcjai
end do c_aiajckcjai
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, e == a, d == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aiajciclaj: do l = n0l, n1l
c_aiajciclaj: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(l + 1, n0jm)
j_aiajciclaj: do j = j0, n1jm
if (j == l) cycle j_aiajciclaj
a1 = min(c - 1, n1abe)
a_aiajciclaj: do a = n0abe, a1
if (a == c) cycle a_aiajciclaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, l + 1, n0ik)
i_aiajciclaj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajciclaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, c, l, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajciclaj(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciclaj
end do a_aiajciclaj
end do j_aiajciclaj
end do c_aiajciclaj
end do l_aiajciclaj
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, e == a, d == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aiajcjclai: do l = n0l, n1l
c_aiajcjclai: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(l + 1, n0jk)
j_aiajcjclai: do j = j0, n1jk
if (j == l) cycle j_aiajcjclai
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abe)
a_aiajcjclai: do a = n0abe, a1
if (a == c) cycle a_aiajcjclai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, l + 1, n0im)
i_aiajcjclai: do i = i0, n1im
if (i == j .or. i == l) cycle i_aiajcjclai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, c, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiajcjclai(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjclai
end do a_aiajcjclai
end do j_aiajcjclai
end do c_aiajcjclai
end do l_aiajcjclai
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k
! Equalities: e == a, d == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibickciai: do c = n0cd, n1cd
k_aibickciai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickciai: do b = n0b, n1b
if (b == c) cycle b_aibickciai
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibickciai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibickciai
i1 = min(k - 1, n1ijlm)
i_aibickciai: do i = n0ijlm, i1
if (i == k) cycle i_aibickciai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, c, i, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibickciai(b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickciai
end do a_aibickciai
end do b_aibickciai
end do k_aibickciai
end do c_aibickciai
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: e == a, d == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjciaj: do c = n0cd, n1cd
b_aibjcjciaj: do b = n0b, n1b
if (b == c) cycle b_aibjcjciaj
j_aibjcjciaj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibjcjciaj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjciaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1il)
i_aibjcjciaj: do i = n0il, i1
if (i == j) cycle i_aibjcjciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, c, i, a, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcjciaj(b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjciaj
end do a_aibjcjciaj
end do j_aibjcjciaj
end do b_aibjcjciaj
end do c_aibjcjciaj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: e == a, d == c, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcicjaj: do c = n0cd, n1cd
b_aibjcicjaj: do b = n0b, n1b
if (b == c) cycle b_aibjcicjaj
j_aibjcicjaj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibjcicjaj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcicjaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjcicjaj: do i = i0, n1ik
if (i == j) cycle i_aibjcicjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, c, j, a, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcicjaj(b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicjaj
end do a_aibjcicjaj
end do j_aibjcicjaj
end do b_aibjcicjaj
end do c_aibjcicjaj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, l
! Equalities: e == a, d == c, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiciclai: do l = n0l, n1l
c_aibiciclai: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b_aibiciclai: do b = n0b, n1b
if (b == c) cycle b_aibiciclai
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibiciclai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibiciclai
i0 = max(l + 1, n0ijkm)
i_aibiciclai: do i = i0, n1ijkm
if (i == l) cycle i_aibiciclai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, c, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibiciclai(b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiciclai
end do a_aibiciclai
end do b_aibiciclai
end do c_aibiciclai
end do l_aibiciclai
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k
! Equalities: e == b, d == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibickcibi: do c = n0cd, n1cd
k_aibickcibi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(c - 1, n1be)
b_aibickcibi: do b = n0be, b1
if (b == c) cycle b_aibickcibi
a0 = max(b + 1, n0a)
a_aibickcibi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibickcibi
i1 = min(k - 1, n1ijlm)
i_aibickcibi: do i = n0ijlm, i1
if (i == k) cycle i_aibickcibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, c, i, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibickcibi(a, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickcibi
end do a_aibickcibi
end do b_aibickcibi
end do k_aibickcibi
end do c_aibickcibi
!
! Elementary loop  22
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: e == b, d == c, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjcibi: do c = n0cd, n1cd
b1 = min(c - 1, n1be)
b_aibjcjcibi: do b = n0be, b1
if (b == c) cycle b_aibjcjcibi
j_aibjcjcibi: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjcibi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjcibi
i1 = min(j - 1, n1ilm)
i_aibjcjcibi: do i = n0ilm, i1
if (i == j) cycle i_aibjcjcibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, c, i, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjcibi(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjcibi
end do a_aibjcjcibi
end do j_aibjcjcibi
end do b_aibjcjcibi
end do c_aibjcjcibi
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: e == b, d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicjbi: do c = n0cd, n1cd
b1 = min(c - 1, n1be)
b_aibjcicjbi: do b = n0be, b1
if (b == c) cycle b_aibjcicjbi
j_aibjcicjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcicjbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicjbi
i0 = max(j + 1, n0ikm)
i_aibjcicjbi: do i = i0, n1ikm
if (i == j) cycle i_aibjcicjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, c, j, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcicjbi(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicjbi
end do a_aibjcicjbi
end do j_aibjcicjbi
end do b_aibjcicjbi
end do c_aibjcicjbi
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, l
! Equalities: e == b, d == c, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiciclbi: do l = n0l, n1l
c_aibiciclbi: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(c - 1, n1be)
b_aibiciclbi: do b = n0be, b1
if (b == c) cycle b_aibiciclbi
a0 = max(b + 1, n0a)
a_aibiciclbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibiciclbi
i0 = max(l + 1, n0ijkm)
i_aibiciclbi: do i = i0, n1ijkm
if (i == l) cycle i_aibiciclbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, c, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibiciclbi(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiciclbi
end do a_aibiciclbi
end do b_aibiciclbi
end do c_aibiciclbi
end do l_aibiciclbi
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, m
! Equalities: c == b, e == d, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibibididm: do m = n0m, n1m
d_aibibididm: do d = n0de, n1de
em = (d - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(d + 1, n0bc)
b_aibibididm: do b = b0, n1bc
if (b == d) cycle b_aibibididm
a0 = max(b + 1, n0a)
a_aibibididm: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibididm
i0 = max(m + 1, n0ijkl)
i_aibibididm: do i = i0, n1ijkl
if (i == m) cycle i_aibibididm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, i, d, i, d, m)
jac_ibra_iket = eom_cc3_23_trans_aibibididm(a, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibididm
end do a_aibibididm
end do b_aibibididm
end do d_aibibididm
end do m_aibibididm
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, j
! Equalities: c == b, e == d, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbididj: do d = n0de, n1de
b0 = max(d + 1, n0bc)
b_aibjbididj: do b = b0, n1bc
if (b == d) cycle b_aibjbididj
j_aibjbididj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbididj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbididj
i0 = max(j + 1, n0ikl)
i_aibjbididj: do i = i0, n1ikl
if (i == j) cycle i_aibjbididj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, i, d, i, d, j)
jac_ibra_iket = eom_cc3_23_trans_aibjbididj(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbididj
end do a_aibjbididj
end do j_aibjbididj
end do b_aibjbididj
end do d_aibjbididj
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, j
! Equalities: c == b, e == d, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidjdi: do d = n0de, n1de
b0 = max(d + 1, n0bc)
b_aibjbidjdi: do b = b0, n1bc
if (b == d) cycle b_aibjbidjdi
j_aibjbidjdi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidjdi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidjdi
i1 = min(j - 1, n1ikm)
i_aibjbidjdi: do i = n0ikm, i1
if (i == j) cycle i_aibjbidjdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, i, d, j, d, i)
jac_ibra_iket = eom_cc3_23_trans_aibjbidjdi(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidjdi
end do a_aibjbidjdi
end do j_aibjbidjdi
end do b_aibjbidjdi
end do d_aibjbidjdi
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, l
! Equalities: c == b, e == d, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibibidldi: do d = n0de, n1de
l_aibibidldi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b0 = max(d + 1, n0bc)
b_aibibidldi: do b = b0, n1bc
if (b == d) cycle b_aibibidldi
a0 = max(b + 1, n0a)
a_aibibidldi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibidldi
i1 = min(l - 1, n1ijkm)
i_aibibidldi: do i = n0ijkm, i1
if (i == l) cycle i_aibibidldi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, i, d, l, d, i)
jac_ibra_iket = eom_cc3_23_trans_aibibidldi(a, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibidldi
end do a_aibibidldi
end do b_aibibidldi
end do l_aibibidldi
end do d_aibibidldi
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == a, k == i, l == i
! No equalities independent of the above can hold.
!
m_aiajciaiam: do m = n0m, n1m
c_aiajciaiam: do c = n0c, n1c
j_aiajciaiam: do j = n0j, n1j
if (j == m) cycle j_aiajciaiam
a1 = min(c - 1, n1abde)
a_aiajciaiam: do a = n0abde, a1
if (a == c) cycle a_aiajciaiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, n0ikl)
i_aiajciaiam: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aiajciaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, i, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiajciaiam(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaiam
end do a_aiajciaiam
end do j_aiajciaiam
end do c_aiajciaiam
end do m_aiajciaiam
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, m
! Equalities: b == a, d == a, e == a, j == i, l == i
! No equalities independent of the above can hold.
!
m_aiaickaiam: do m = n0m, n1m
c_aiaickaiam: do c = n0c, n1c
k_aiaickaiam: do k = n0k, n1k
if (k == m) cycle k_aiaickaiam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(c - 1, n1abde)
a_aiaickaiam: do a = n0abde, a1
if (a == c) cycle a_aiaickaiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, k + 1, n0ijl)
i_aiaickaiam: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aiaickaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiaickaiam(i, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickaiam
end do a_aiaickaiam
end do k_aiaickaiam
end do c_aiaickaiam
end do m_aiaickaiam
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

end subroutine ccjac_23_dav_part10
end module ccjac_block_23_dav_part10
