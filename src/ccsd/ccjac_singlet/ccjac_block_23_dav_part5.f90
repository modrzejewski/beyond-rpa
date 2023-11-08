module ccjac_block_23_dav_part5
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
subroutine ccjac_23_dav_part5(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, a1, b0, d0, i0, i1, j0, j1, k1, l0
integer :: n0abd, n0abde, n0ad, n0ade, n0bc
integer :: n0be, n0ijkm, n0ijl, n0ijlm, n0ik
integer :: n0ikm, n0il, n0ilm, n0im, n0jk
integer :: n0jkm, n0jl, n0jlm, n0jm, n0km
integer :: n1abd, n1abde, n1ad, n1ade, n1bc
integer :: n1be, n1ijkm, n1ijl, n1ijlm, n1ik
integer :: n1ikm, n1il, n1ilm, n1im, n1jk
integer :: n1jkm, n1jl, n1jlm, n1jm, n1km
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


n0abd = max(n0a, n0b, n0d)
n0abde = max(n0a, n0b, n0d, n0e)
n0ad = max(n0a, n0d)
n0ade = max(n0a, n0d, n0e)
n0bc = max(n0b, n0c)
n0be = max(n0b, n0e)
n0ijkm = max(n0i, n0j, n0k, n0m)
n0ijl = max(n0i, n0j, n0l)
n0ijlm = max(n0i, n0j, n0l, n0m)
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
n1abd = min(n1a, n1b, n1d)
n1abde = min(n1a, n1b, n1d, n1e)
n1ad = min(n1a, n1d)
n1ade = min(n1a, n1d, n1e)
n1bc = min(n1b, n1c)
n1be = min(n1b, n1e)
n1ijkm = min(n1i, n1j, n1k, n1m)
n1ijl = min(n1i, n1j, n1l)
n1ijlm = min(n1i, n1j, n1l, n1m)
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
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, k
! Equalities: c == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibibkdiei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibibkdiei: do d = d0, n1d
if (d == e) cycle d_aibibkdiei
k_aibibkdiei: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibibkdiei: do b = b0, n1bc
if (b == d .or. b == e) cycle b_aibibkdiei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkdiei: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibibkdiei
i_aibibkdiei: do i = n0ijlm, n1ijlm
if (i == k) cycle i_aibibkdiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, k, d, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibibkdiei(a, k, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkdiei
end do a_aibibkdiei
end do b_aibibkdiei
end do k_aibibkdiei
end do d_aibibkdiei
end do e_aibibkdiei
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j
! Equalities: c == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjdiei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjbjdiei: do d = d0, n1d
if (d == e) cycle d_aibjbjdiei
b0 = max(d + 1, n0bc)
b_aibjbjdiei: do b = b0, n1bc
if (b == d .or. b == e) cycle b_aibjbjdiei
j_aibjbjdiei: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdiei: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbjdiei
i_aibjbjdiei: do i = n0ilm, n1ilm
if (i == j) cycle i_aibjbjdiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, j, d, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjbjdiei(a, i, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdiei
end do a_aibjbjdiei
end do j_aibjbjdiei
end do b_aibjbjdiei
end do d_aibjbjdiei
end do e_aibjbjdiei
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjbjdiej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjbjdiej: do d = d0, n1d
if (d == e) cycle d_aibjbjdiej
b0 = max(d + 1, n0bc)
b_aibjbjdiej: do b = b0, n1bc
if (b == d .or. b == e) cycle b_aibjbjdiej
j_aibjbjdiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdiej: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbjdiej
i_aibjbjdiej: do i = n0il, n1il
if (i == j) cycle i_aibjbjdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, j, d, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjbjdiej(a, j, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdiej
end do a_aibjbjdiej
end do j_aibjbjdiej
end do b_aibjbjdiej
end do d_aibjbjdiej
end do e_aibjbjdiej
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j
! Equalities: c == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbidjei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjbidjei: do d = d0, n1d
if (d == e) cycle d_aibjbidjei
b0 = max(d + 1, n0bc)
b_aibjbidjei: do b = b0, n1bc
if (b == d .or. b == e) cycle b_aibjbidjei
j_aibjbidjei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidjei: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbidjei
i_aibjbidjei: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjbidjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, i, d, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjbidjei(a, i, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidjei
end do a_aibjbidjei
end do j_aibjbidjei
end do b_aibjbidjei
end do d_aibjbidjei
end do e_aibjbidjei
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, l
! Equalities: c == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibibidlei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibibidlei: do d = d0, n1d
if (d == e) cycle d_aibibidlei
l_aibibidlei: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b0 = max(d + 1, n0bc)
b_aibibidlei: do b = b0, n1bc
if (b == d .or. b == e) cycle b_aibibidlei
a0 = max(b + 1, n0a)
a_aibibidlei: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibibidlei
i_aibibidlei: do i = n0ijkm, n1ijkm
if (i == l) cycle i_aibibidlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, i, d, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibibidlei(a, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibidlei
end do a_aibibidlei
end do b_aibibidlei
end do l_aibibidlei
end do d_aibibidlei
end do e_aibibidlei
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, m
! Equalities: b == a, d == a, e == a, l == i
! No equalities independent of the above can hold.
!
m_aiajckaiam: do m = n0m, n1m
c_aiajckaiam: do c = n0c, n1c
k_aiajckaiam: do k = n0k, n1k
if (k == m) cycle k_aiajckaiam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiam: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aiajckaiam
a1 = min(c - 1, n1abde)
a_aiajckaiam: do a = n0abde, a1
if (a == c) cycle a_aiajckaiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, k + 1, n0il)
i_aiajckaiam: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aiajckaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiajckaiam(j, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaiam
end do a_aiajckaiam
end do j_aiajckaiam
end do k_aiajckaiam
end do c_aiajckaiam
end do m_aiajckaiam
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, m
! Equalities: b == a, d == a, e == a, l == j
! No equalities independent of the above can hold.
!
m_aiajckajam: do m = n0m, n1m
c_aiajckajam: do c = n0c, n1c
k_aiajckajam: do k = n0k, n1k
if (k == m) cycle k_aiajckajam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(m + 1, k + 1, n0jl)
j_aiajckajam: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aiajckajam
a1 = min(c - 1, n1abde)
a_aiajckajam: do a = n0abde, a1
if (a == c) cycle a_aiajckajam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckajam: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aiajckajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, j, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiajckajam(i, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckajam
end do a_aiajckajam
end do j_aiajckajam
end do k_aiajckajam
end do c_aiajckajam
end do m_aiajckajam
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l, m
! Equalities: b == a, d == a, e == a, k == i
! No equalities independent of the above can hold.
!
m_aiajcialam: do m = n0m, n1m
l0 = max(m + 1, n0l)
l_aiajcialam: do l = l0, n1l
if (l == m) cycle l_aiajcialam
c_aiajcialam: do c = n0c, n1c
j_aiajcialam: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aiajcialam
a1 = min(c - 1, n1abde)
a_aiajcialam: do a = n0abde, a1
if (a == c) cycle a_aiajcialam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0ik)
i1 = min(l - 1, n1ik)
i_aiajcialam: do i = i0, i1
if (i == j .or. i == l .or. i == m) cycle i_aiajcialam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiajcialam(j, c, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcialam
end do a_aiajcialam
end do j_aiajcialam
end do c_aiajcialam
end do l_aiajcialam
end do m_aiajcialam
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, l
! Equalities: b == a, d == a, e == a, m == i
! No equalities independent of the above can hold.
!
l_aiajckalai: do l = n0l, n1l
c_aiajckalai: do c = n0c, n1c
k1 = min(l - 1, n1k)
k_aiajckalai: do k = n0k, k1
if (k == l) cycle k_aiajckalai
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckalai: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aiajckalai
a1 = min(c - 1, n1abde)
a_aiajckalai: do a = n0abde, a1
if (a == c) cycle a_aiajckalai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajckalai: do i = i0, i1
if (i == j .or. i == k .or. i == l) cycle i_aiajckalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiajckalai(j, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckalai
end do a_aiajckalai
end do j_aiajckalai
end do k_aiajckalai
end do c_aiajckalai
end do l_aiajckalai
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l, m
! Equalities: b == a, d == a, e == a, k == j
! No equalities independent of the above can hold.
!
m_aiajcjalam: do m = n0m, n1m
l0 = max(m + 1, n0l)
l_aiajcjalam: do l = l0, n1l
if (l == m) cycle l_aiajcjalam
c_aiajcjalam: do c = n0c, n1c
j1 = min(l - 1, n1jk)
j_aiajcjalam: do j = n0jk, j1
if (j == l .or. j == m) cycle j_aiajcjalam
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abde)
a_aiajcjalam: do a = n0abde, a1
if (a == c) cycle a_aiajcjalam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjalam: do i = i0, n1i
if (i == j .or. i == l .or. i == m) cycle i_aiajcjalam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, l, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiajcjalam(i, c, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjalam
end do a_aiajcjalam
end do j_aiajcjalam
end do c_aiajcjalam
end do l_aiajcjalam
end do m_aiajcjalam
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, l
! Equalities: b == a, d == a, e == a, m == j
! No equalities independent of the above can hold.
!
l_aiajckalaj: do l = n0l, n1l
c_aiajckalaj: do c = n0c, n1c
k1 = min(l - 1, n1k)
k_aiajckalaj: do k = n0k, k1
if (k == l) cycle k_aiajckalaj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aiajckalaj: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aiajckalaj
a1 = min(c - 1, n1abde)
a_aiajckalaj: do a = n0abde, a1
if (a == c) cycle a_aiajckalaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckalaj: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckalaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, l, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajckalaj(i, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckalaj
end do a_aiajckalaj
end do j_aiajckalaj
end do k_aiajckalaj
end do c_aiajckalaj
end do l_aiajckalaj
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
e_aiajcjaiem: do e = n0e, n1e
m_aiajcjaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aiajcjaiem: do c = n0c, n1c
if (c == e) cycle c_aiajcjaiem
j_aiajcjaiem: do j = n0jk, n1jk
if (j == m) cycle j_aiajcjaiem
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abd)
a1 = min(c - 1, n1abd)
a_aiajcjaiem: do a = a0, a1
if (a == c .or. a == e) cycle a_aiajcjaiem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjaiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajcjaiem
if (j > i .and. i > m) cycle i_aiajcjaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, i, e, m)
jac_ibra_iket = eom_cc3_23_trans_aiajcjaiem(a, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaiem
end do a_aiajcjaiem
end do j_aiajcjaiem
end do c_aiajcjaiem
end do m_aiajcjaiem
end do e_aiajcjaiem
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajckaiej: do e = n0e, n1e
c_aiajckaiej: do c = n0c, n1c
if (c == e) cycle c_aiajckaiej
k_aiajckaiej: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiej: do j = n0jm, n1jm
if (j == k) cycle j_aiajckaiej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abd)
a1 = min(c - 1, n1abd)
a_aiajckaiej: do a = a0, a1
if (a == c .or. a == e) cycle a_aiajckaiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckaiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckaiej
if (k > i .and. i > j) cycle i_aiajckaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajckaiej(a, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaiej
end do a_aiajckaiej
end do j_aiajckaiej
end do k_aiajckaiej
end do c_aiajckaiej
end do e_aiajckaiej
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aiajciajem: do e = n0e, n1e
m_aiajciajem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aiajciajem: do c = n0c, n1c
if (c == e) cycle c_aiajciajem
j_aiajciajem: do j = n0jl, n1jl
if (j == m) cycle j_aiajciajem
a0 = max(e + 1, n0abd)
a1 = min(c - 1, n1abd)
a_aiajciajem: do a = a0, a1
if (a == c .or. a == e) cycle a_aiajciajem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajciajem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aiajciajem
if (i > j .and. j > m) exit i_aiajciajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, j, e, m)
jac_ibra_iket = eom_cc3_23_trans_aiajciajem(a, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciajem
end do a_aiajciajem
end do j_aiajciajem
end do c_aiajciajem
end do m_aiajciajem
end do e_aiajciajem
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajckajei: do e = n0e, n1e
c_aiajckajei: do c = n0c, n1c
if (c == e) cycle c_aiajckajei
k_aiajckajei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckajei: do j = n0jl, n1jl
if (j == k) cycle j_aiajckajei
a0 = max(e + 1, n0abd)
a1 = min(c - 1, n1abd)
a_aiajckajei: do a = a0, a1
if (a == c .or. a == e) cycle a_aiajckajei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajckajei: do i = i0, n1im
if (i == j .or. i == k) cycle i_aiajckajei
if (k > j .and. j > i) cycle i_aiajckajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajckajei(a, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckajei
end do a_aiajckajei
end do j_aiajckajei
end do k_aiajckajei
end do c_aiajckajei
end do e_aiajckajei
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aiajcialej: do e = n0e, n1e
l_aiajcialej: do l = n0l, n1l
c_aiajcialej: do c = n0c, n1c
if (c == e) cycle c_aiajcialej
j_aiajcialej: do j = n0jm, n1jm
if (j == l) cycle j_aiajcialej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abd)
a1 = min(c - 1, n1abd)
a_aiajcialej: do a = a0, a1
if (a == c .or. a == e) cycle a_aiajcialej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcialej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcialej
if (i > l .and. l > j) exit i_aiajcialej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajcialej(a, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcialej
end do a_aiajcialej
end do j_aiajcialej
end do c_aiajcialej
end do l_aiajcialej
end do e_aiajcialej
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aiajcjalei: do e = n0e, n1e
l_aiajcjalei: do l = n0l, n1l
c_aiajcjalei: do c = n0c, n1c
if (c == e) cycle c_aiajcjalei
j_aiajcjalei: do j = n0jk, n1jk
if (j == l) cycle j_aiajcjalei
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abd)
a1 = min(c - 1, n1abd)
a_aiajcjalei: do a = a0, a1
if (a == c .or. a == e) cycle a_aiajcjalei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajcjalei: do i = i0, n1im
if (i == j .or. i == l) cycle i_aiajcjalei
if (j > l .and. l > i) cycle i_aiajcjalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajcjalei(a, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjalei
end do a_aiajcjalei
end do j_aiajcjalei
end do c_aiajcjalei
end do l_aiajcjalei
end do e_aiajcjalei
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjaiam: do m = n0m, n1m
c_aibjcjaiam: do c = n0c, n1c
b_aibjcjaiam: do b = n0b, n1b
if (b == c) cycle b_aibjcjaiam
j_aibjcjaiam: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjaiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a1 = min(c - 1, n1ade)
a_aibjcjaiam: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjaiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, j + 1, n0il)
i_aibjcjaiam: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjcjaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, i, a, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcjaiam(a, b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaiam
end do a_aibjcjaiam
end do j_aibjcjaiam
end do b_aibjcjaiam
end do c_aibjcjaiam
end do m_aibjcjaiam
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaiaj: do c = n0c, n1c
k_aibjckaiaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaiaj: do b = n0b, n1b
if (b == c) cycle b_aibjckaiaj
j_aibjckaiaj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckaiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a1 = min(c - 1, n1ade)
a_aibjckaiaj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckaiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, k + 1, n0il)
i_aibjckaiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, a, j)
jac_ibra_iket = eom_cc3_23_trans_aibjckaiaj(a, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaiaj
end do a_aibjckaiaj
end do j_aibjckaiaj
end do b_aibjckaiaj
end do k_aibjckaiaj
end do c_aibjckaiaj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjciajam: do m = n0m, n1m
c_aibjciajam: do c = n0c, n1c
b_aibjciajam: do b = n0b, n1b
if (b == c) cycle b_aibjciajam
j0 = max(m + 1, n0jl)
j_aibjciajam: do j = j0, n1jl
if (j == m) cycle j_aibjciajam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a1 = min(c - 1, n1ade)
a_aibjciajam: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciajam
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjciajam: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aibjciajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, j, a, m)
jac_ibra_iket = eom_cc3_23_trans_aibjciajam(a, b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciajam
end do a_aibjciajam
end do j_aibjciajam
end do b_aibjciajam
end do c_aibjciajam
end do m_aibjciajam
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == a, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckajai: do c = n0c, n1c
k_aibjckajai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajai: do b = n0b, n1b
if (b == c) cycle b_aibjckajai
j0 = max(k + 1, n0jl)
j_aibjckajai: do j = j0, n1jl
if (j == k) cycle j_aibjckajai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a1 = min(c - 1, n1ade)
a_aibjckajai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckajai
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjckajai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, j, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibjckajai(a, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajai
end do a_aibjckajai
end do j_aibjckajai
end do b_aibjckajai
end do k_aibjckajai
end do c_aibjckajai
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == a, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjcialaj: do l = n0l, n1l
c_aibjcialaj: do c = n0c, n1c
b_aibjcialaj: do b = n0b, n1b
if (b == c) cycle b_aibjcialaj
j1 = min(l - 1, n1jm)
j_aibjcialaj: do j = n0jm, j1
if (j == l) cycle j_aibjcialaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a1 = min(c - 1, n1ade)
a_aibjcialaj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcialaj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(l - 1, n1ik)
i_aibjcialaj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjcialaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, a, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcialaj(a, b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcialaj
end do a_aibjcialaj
end do j_aibjcialaj
end do b_aibjcialaj
end do c_aibjcialaj
end do l_aibjcialaj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == a, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjalai: do l = n0l, n1l
c_aibjcjalai: do c = n0c, n1c
b_aibjcjalai: do b = n0b, n1b
if (b == c) cycle b_aibjcjalai
j1 = min(l - 1, n1jk)
j_aibjcjalai: do j = n0jk, j1
if (j == l) cycle j_aibjcjalai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a1 = min(c - 1, n1ade)
a_aibjcjalai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjalai
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjcjalai: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjcjalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjalai(a, b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjalai
end do a_aibjcjalai
end do j_aibjcjalai
end do b_aibjcjalai
end do c_aibjcjalai
end do l_aibjcjalai
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, m
! Equalities: d == a, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibickaibm: do m = n0m, n1m
c_aibickaibm: do c = n0c, n1c
k_aibickaibm: do k = n0k, n1k
if (k == m) cycle k_aibickaibm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickaibm: do b = n0be, n1be
if (b == c) cycle b_aibickaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibickaibm: do a = a0, a1
if (a == b .or. a == c) cycle a_aibickaibm
i_aibickaibm: do i = n0ijl, n1ijl
if (i == k .or. i == m) cycle i_aibickaibm
if (k > i .and. i > m) cycle i_aibickaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibickaibm(i, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickaibm
end do a_aibickaibm
end do b_aibickaibm
end do k_aibickaibm
end do c_aibickaibm
end do m_aibickaibm
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == b, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjckaibi: do c = n0c, n1c
k_aibjckaibi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibi: do b = n0be, n1be
if (b == c) cycle b_aibjckaibi
j_aibjckaibi: do j = n0j, n1j
if (j == k) cycle j_aibjckaibi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckaibi: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckaibi
i_aibjckaibi: do i = n0ilm, n1ilm
if (i == j .or. i == k) cycle i_aibjckaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjckaibi(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaibi
end do a_aibjckaibi
end do j_aibjckaibi
end do b_aibjckaibi
end do k_aibjckaibi
end do c_aibjckaibi
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjaibm: do m = n0m, n1m
c_aibjcjaibm: do c = n0c, n1c
b_aibjcjaibm: do b = n0be, n1be
if (b == c) cycle b_aibjcjaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjaibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjaibm: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjaibm
i_aibjcjaibm: do i = n0il, n1il
if (i == j .or. i == m) cycle i_aibjcjaibm
if (j > i .and. i > m) cycle i_aibjcjaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcjaibm(nocc, a, i, b, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaibm
end do a_aibjcjaibm
end do j_aibjcjaibm
end do b_aibjcjaibm
end do c_aibjcjaibm
end do m_aibjcjaibm
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckaibk: do c = n0c, n1c
k_aibjckaibk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibk: do b = n0be, n1be
if (b == c) cycle b_aibjckaibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckaibk: do j = n0j, n1j
if (j == k) cycle j_aibjckaibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckaibk: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckaibk
i_aibjckaibk: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, b, k)
jac_ibra_iket = eom_cc3_23_trans_aibjckaibk(j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaibk
end do a_aibjckaibk
end do j_aibjckaibk
end do b_aibjckaibk
end do k_aibjckaibk
end do c_aibjckaibk
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaibj: do c = n0c, n1c
k_aibjckaibj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibj: do b = n0be, n1be
if (b == c) cycle b_aibjckaibj
j_aibjckaibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckaibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckaibj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckaibj
i_aibjckaibj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaibj
if (k > i .and. i > j) cycle i_aibjckaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjckaibj(nocc, a, i, b, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaibj
end do a_aibjckaibj
end do j_aibjckaibj
end do b_aibjckaibj
end do k_aibjckaibj
end do c_aibjckaibj
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: d == a, e == b, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjckajbj: do c = n0c, n1c
k_aibjckajbj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajbj: do b = n0be, n1be
if (b == c) cycle b_aibjckajbj
j_aibjckajbj: do j = n0jlm, n1jlm
if (j == k) cycle j_aibjckajbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckajbj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckajbj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, j, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjckajbj(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajbj
end do a_aibjckajbj
end do j_aibjckajbj
end do b_aibjckajbj
end do k_aibjckajbj
end do c_aibjckajbj
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjciajbm: do m = n0m, n1m
c_aibjciajbm: do c = n0c, n1c
b_aibjciajbm: do b = n0be, n1be
if (b == c) cycle b_aibjciajbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjciajbm: do j = n0jl, n1jl
if (j == m) cycle j_aibjciajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciajbm: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajbm: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjciajbm
if (i > j .and. j > m) exit i_aibjciajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, j, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjciajbm(a, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciajbm
end do a_aibjciajbm
end do j_aibjciajbm
end do b_aibjciajbm
end do c_aibjciajbm
end do m_aibjciajbm
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

end subroutine ccjac_23_dav_part5
end module ccjac_block_23_dav_part5
