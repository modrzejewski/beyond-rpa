module ccjac_block_23_dav_part12
use eom_cc3_23_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2019-06-21 14:27:00 UTC.
!
contains
subroutine ccjac_23_dav_part12(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, a1, b1, c0, i0, i1
integer :: n0abcd, n0abe, n0acd, n0ad, n0bde
integer :: n0be, n0ijk, n0ijkl, n0ijkm, n0ijl
integer :: n0ijlm, n0ik, n0ikl, n0ikm, n0il
integer :: n0ilm, n0im, n0jk, n0jkl, n0jkm
integer :: n0jl, n0jlm, n0jm, n0km, n0lm
integer :: n1abcd, n1abe, n1acd, n1ad, n1bde
integer :: n1be, n1ijk, n1ijkl, n1ijkm, n1ijl
integer :: n1ijlm, n1ik, n1ikl, n1ikm, n1il
integer :: n1ilm, n1im, n1jk, n1jkl, n1jkm
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

n0abcd = max(n0a, n0b, n0c, n0d)
n0abe = max(n0a, n0b, n0e)
n0acd = max(n0a, n0c, n0d)
n0ad = max(n0a, n0d)
n0bde = max(n0b, n0d, n0e)
n0be = max(n0b, n0e)
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
n0jkl = max(n0j, n0k, n0l)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jlm = max(n0j, n0l, n0m)
n0jm = max(n0j, n0m)
n0km = max(n0k, n0m)
n0lm = max(n0l, n0m)
n1abcd = min(n1a, n1b, n1c, n1d)
n1abe = min(n1a, n1b, n1e)
n1acd = min(n1a, n1c, n1d)
n1ad = min(n1a, n1d)
n1bde = min(n1b, n1d, n1e)
n1be = min(n1b, n1e)
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
n1jkl = min(n1j, n1k, n1l)
n1jkm = min(n1j, n1k, n1m)
n1jl = min(n1j, n1l)
n1jlm = min(n1j, n1l, n1m)
n1jm = min(n1j, n1m)
n1km = min(n1k, n1m)
n1lm = min(n1l, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjciajbj: do c = n0c, n1c
b_aibjciajbj: do b = n0be, n1be
if (b == c) cycle b_aibjciajbj
j_aibjciajbj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciajbj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciajbj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajbj: do i = n0ik, n1ik
if (i == j) cycle i_aibjciajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, j, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjciajbj(nocc, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciajbj
end do a_aibjciajbj
end do j_aibjciajbj
end do b_aibjciajbj
end do c_aibjciajbj
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajbi: do c = n0c, n1c
b_aibjciajbi: do b = n0be, n1be
if (b == c) cycle b_aibjciajbi
j_aibjciajbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciajbi: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajbi: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjciajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, j, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjciajbi(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciajbi
end do a_aibjciajbi
end do j_aibjciajbi
end do b_aibjciajbi
end do c_aibjciajbi
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l
! Equalities: d == a, e == b, j == i, k == i, m == l
! No equalities independent of the above can hold.
!
l_aibicialbl: do l = n0lm, n1lm
c_aibicialbl: do c = n0c, n1c
b_aibicialbl: do b = n0be, n1be
if (b == c) cycle b_aibicialbl
em = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibicialbl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibicialbl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibicialbl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibicialbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, b, l)
jac_ibra_iket = eom_cc3_23_trans_aibicialbl(i, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicialbl
end do a_aibicialbl
end do b_aibicialbl
end do c_aibicialbl
end do l_aibicialbl
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l
! Equalities: d == a, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibicialbi: do l = n0l, n1l
c_aibicialbi: do c = n0c, n1c
b_aibicialbi: do b = n0be, n1be
if (b == c) cycle b_aibicialbi
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibicialbi: do a = a0, a1
if (a == b .or. a == c) cycle a_aibicialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibicialbi: do i = n0ijkm, n1ijkm
if (i == l) cycle i_aibicialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibicialbi(nocc, a, i, b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicialbi
end do a_aibicialbi
end do b_aibicialbi
end do c_aibicialbi
end do l_aibicialbi
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, e == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aiaickdiai: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiaickdiai: do c = c0, n1c
if (c == d) cycle c_aiaickdiai
k_aiaickdiai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(d - 1, n1abe)
a_aiaickdiai: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiaickdiai
i_aiaickdiai: do i = n0ijlm, n1ijlm
if (i == k) cycle i_aiaickdiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, d, i, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiaickdiai(a, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickdiai
end do a_aiaickdiai
end do k_aiaickdiai
end do c_aiaickdiai
end do d_aiaickdiai
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
d_aiajcjdiai: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcjdiai: do c = c0, n1c
if (c == d) cycle c_aiajcjdiai
j_aiajcjdiai: do j = n0jk, n1jk
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abe)
a_aiajcjdiai: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajcjdiai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ilm)
i_aiajcjdiai: do i = i0, n1ilm
if (i == j) cycle i_aiajcjdiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, d, i, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiajcjdiai(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdiai
end do a_aiajcjdiai
end do j_aiajcjdiai
end do c_aiajcjdiai
end do d_aiajcjdiai
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aiajcjdiaj: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcjdiaj: do c = c0, n1c
if (c == d) cycle c_aiajcjdiaj
j_aiajcjdiaj: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abe)
a_aiajcjdiaj: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajcjdiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjdiaj: do i = i0, n1il
if (i == j) cycle i_aiajcjdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, d, i, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajcjdiaj(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdiaj
end do a_aiajcjdiaj
end do j_aiajcjdiaj
end do c_aiajcjdiaj
end do d_aiajcjdiaj
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
d_aiajcidjaj: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcidjaj: do c = c0, n1c
if (c == d) cycle c_aiajcidjaj
j_aiajcidjaj: do j = n0jlm, n1jlm
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abe)
a_aiajcidjaj: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajcidjaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcidjaj: do i = i0, n1ik
if (i == j) cycle i_aiajcidjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, j, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajcidjaj(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidjaj
end do a_aiajcidjaj
end do j_aiajcidjaj
end do c_aiajcidjaj
end do d_aiajcidjaj
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidjai: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcidjai: do c = c0, n1c
if (c == d) cycle c_aiajcidjai
j_aiajcidjai: do j = n0jl, n1jl
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abe)
a_aiajcidjai: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajcidjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikm)
i_aiajcidjai: do i = i0, n1ikm
if (i == j) cycle i_aiajcidjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, j, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiajcidjai(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidjai
end do a_aiajcidjai
end do j_aiajcidjai
end do c_aiajcidjai
end do d_aiajcidjai
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, l
! Equalities: b == a, e == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aiaicidlai: do d = n0d, n1d
l_aiaicidlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c0 = max(d + 1, n0c)
c_aiaicidlai: do c = c0, n1c
if (c == d) cycle c_aiaicidlai
a1 = min(d - 1, n1abe)
a_aiaicidlai: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiaicidlai
i_aiaicidlai: do i = n0ijkm, n1ijkm
if (i == l) cycle i_aiaicidlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiaicidlai(a, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaicidlai
end do a_aiaicidlai
end do c_aiaicidlai
end do l_aiaicidlai
end do d_aiaicidlai
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, m
! Equalities: d == b, e == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibicibibm: do m = n0m, n1m
c_aibicibibm: do c = n0c, n1c
b1 = min(c - 1, n1bde)
b_aibicibibm: do b = n0bde, b1
if (b == c) cycle b_aibicibibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibicibibm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibicibibm
i0 = max(m + 1, n0ijkl)
i_aibicibibm: do i = i0, n1ijkl
if (i == m) cycle i_aibicibibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, b, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibicibibm(a, b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicibibm
end do a_aibicibibm
end do b_aibicibibm
end do c_aibicibibm
end do m_aibicibibm
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjcibibj: do c = n0c, n1c
b1 = min(c - 1, n1bde)
b_aibjcibibj: do b = n0bde, b1
if (b == c) cycle b_aibjcibibj
j_aibjcibibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibibj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibibj
i0 = max(j + 1, n0ikl)
i_aibjcibibj: do i = i0, n1ikl
if (i == j) cycle i_aibjcibibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, b, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcibibj(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibibj
end do a_aibjcibibj
end do j_aibjcibibj
end do b_aibjcibibj
end do c_aibjcibibj
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjbibj: do c = n0c, n1c
b1 = min(c - 1, n1bde)
b_aibjcjbibj: do b = n0bde, b1
if (b == c) cycle b_aibjcjbibj
j_aibjcjbibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbibj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbibj
i0 = max(j + 1, n0il)
i_aibjcjbibj: do i = i0, n1il
if (i == j) cycle i_aibjcjbibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, b, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcjbibj(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbibj
end do a_aibjcjbibj
end do j_aibjcjbibj
end do b_aibjcjbibj
end do c_aibjcjbibj
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjbjbi: do c = n0c, n1c
b1 = min(c - 1, n1bde)
b_aibjcjbjbi: do b = n0bde, b1
if (b == c) cycle b_aibjcjbjbi
j_aibjcjbjbi: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbjbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbjbi
i1 = min(j - 1, n1im)
i_aibjcjbjbi: do i = n0im, i1
if (i == j) cycle i_aibjcjbjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, b, j, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjbjbi(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbjbi
end do a_aibjcjbjbi
end do j_aibjcjbjbi
end do b_aibjcjbjbi
end do c_aibjcjbjbi
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibjbi: do c = n0c, n1c
b1 = min(c - 1, n1bde)
b_aibjcibjbi: do b = n0bde, b1
if (b == c) cycle b_aibjcibjbi
j_aibjcibjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibjbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibjbi
i1 = min(j - 1, n1ikm)
i_aibjcibjbi: do i = n0ikm, i1
if (i == j) cycle i_aibjcibjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, b, j, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcibjbi(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibjbi
end do a_aibjcibjbi
end do j_aibjcibjbi
end do b_aibjcibjbi
end do c_aibjcibjbi
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, l
! Equalities: d == b, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiciblbi: do l = n0l, n1l
c_aibiciblbi: do c = n0c, n1c
b1 = min(c - 1, n1bde)
b_aibiciblbi: do b = n0bde, b1
if (b == c) cycle b_aibiciblbi
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibiciblbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibiciblbi
i1 = min(l - 1, n1ijkm)
i_aibiciblbi: do i = n0ijkm, i1
if (i == l) cycle i_aibiciblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, b, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibiciblbi(a, b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiciblbi
end do a_aibiciblbi
end do b_aibiciblbi
end do c_aibiciblbi
end do l_aibiciblbi
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, k
! Equalities: b == a, c == a, d == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiaiakaiei: do e = n0e, n1e
k_aiaiakaiei: do k = n0k, n1k
a0 = max(e + 1, n0abcd)
a_aiaiakaiei: do a = a0, n1abcd
if (a == e) cycle a_aiaiakaiei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijlm)
i_aiaiakaiei: do i = n0ijlm, i1
if (i == k) cycle i_aiaiakaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiaiakaiei(nocc, a, i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiakaiei
end do a_aiaiakaiei
end do k_aiaiakaiei
end do e_aiaiakaiei
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, k
! Equalities: b == a, c == a, d == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aiaiakaiek: do e = n0e, n1e
k_aiaiakaiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiaiakaiek: do a = a0, n1abcd
if (a == e) cycle a_aiaiakaiek
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijl)
i_aiaiakaiek: do i = n0ijl, i1
if (i == k) cycle i_aiaiakaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, e, k)
jac_ibra_iket = eom_cc3_23_trans_aiaiakaiek(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiakaiek
end do a_aiaiakaiek
end do k_aiaiakaiek
end do e_aiaiakaiek
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aiajaiajej: do e = n0e, n1e
j_aiajaiajej: do j = n0jlm, n1jlm
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiajaiajej: do a = a0, n1abcd
if (a == e) cycle a_aiajaiajej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaiajej: do i = i0, n1ik
if (i == j) cycle i_aiajaiajej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, j, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajaiajej(nocc, a, i, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaiajej
end do a_aiajaiajej
end do j_aiajaiajej
end do e_aiajaiajej
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajaiajei: do e = n0e, n1e
j_aiajaiajei: do j = n0jl, n1jl
a0 = max(e + 1, n0abcd)
a_aiajaiajei: do a = a0, n1abcd
if (a == e) cycle a_aiajaiajei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikm)
i_aiajaiajei: do i = i0, n1ikm
if (i == j) cycle i_aiajaiajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajaiajei(nocc, a, i, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaiajei
end do a_aiajaiajei
end do j_aiajaiajei
end do e_aiajaiajei
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, l
! Equalities: b == a, c == a, d == a, j == i, k == i, m == l
! No equalities independent of the above can hold.
!
e_aiaiaialel: do e = n0e, n1e
l_aiaiaialel: do l = n0lm, n1lm
em = (e - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiaiaialel: do a = a0, n1abcd
if (a == e) cycle a_aiaiaialel
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijk)
i_aiaiaialel: do i = i0, n1ijk
if (i == l) cycle i_aiaiaialel
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, e, l)
jac_ibra_iket = eom_cc3_23_trans_aiaiaialel(i, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiaialel
end do a_aiaiaialel
end do l_aiaiaialel
end do e_aiaiaialel
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, l
! Equalities: b == a, c == a, d == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aiaiaialei: do e = n0e, n1e
l_aiaiaialei: do l = n0l, n1l
a0 = max(e + 1, n0abcd)
a_aiaiaialei: do a = a0, n1abcd
if (a == e) cycle a_aiaiaialei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijkm)
i_aiaiaialei: do i = i0, n1ijkm
if (i == l) cycle i_aiaiaialei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiaiaialei(nocc, a, i, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiaialei
end do a_aiaiaialei
end do l_aiaiaialei
end do e_aiaiaialei
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == a, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibiakaibi: do k = n0k, n1k
b_aibiakaibi: do b = n0be, n1be
a0 = max(b + 1, n0acd)
a_aibiakaibi: do a = a0, n1acd
if (a == b) cycle a_aibiakaibi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijlm)
i_aibiakaibi: do i = n0ijlm, i1
if (i == k) cycle i_aibiakaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibiakaibi(nocc, a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakaibi
end do a_aibiakaibi
end do b_aibiakaibi
end do k_aibiakaibi
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == a, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibiakaibk: do k = n0km, n1km
b_aibiakaibk: do b = n0be, n1be
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibiakaibk: do a = a0, n1acd
if (a == b) cycle a_aibiakaibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijl)
i_aibiakaibk: do i = n0ijl, i1
if (i == k) cycle i_aibiakaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, b, k)
jac_ibra_iket = eom_cc3_23_trans_aibiakaibk(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakaibk
end do a_aibiakaibk
end do b_aibiakaibk
end do k_aibiakaibk
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, e == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajaibi: do b = n0be, n1be
j_aibjajaibi: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajaibi: do a = a0, n1acd
if (a == b) cycle a_aibjajaibi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ilm)
i_aibjajaibi: do i = n0ilm, i1
if (i == j) cycle i_aibjajaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, i, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjajaibi(nocc, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajaibi
end do a_aibjajaibi
end do j_aibjajaibi
end do b_aibjajaibi
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
b_aibjajaibj: do b = n0be, n1be
j_aibjajaibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajaibj: do a = a0, n1acd
if (a == b) cycle a_aibjajaibj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1il)
i_aibjajaibj: do i = n0il, i1
if (i == j) cycle i_aibjajaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjajaibj(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajaibj
end do a_aibjajaibj
end do j_aibjajaibj
end do b_aibjajaibj
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
b_aibjaiajbj: do b = n0be, n1be
j_aibjaiajbj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaiajbj: do a = a0, n1acd
if (a == b) cycle a_aibjaiajbj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjaiajbj: do i = i0, n1ik
if (i == j) cycle i_aibjaiajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, j, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjaiajbj(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiajbj
end do a_aibjaiajbj
end do j_aibjaiajbj
end do b_aibjaiajbj
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaiajbi: do b = n0be, n1be
j_aibjaiajbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaiajbi: do a = a0, n1acd
if (a == b) cycle a_aibjaiajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikm)
i_aibjaiajbi: do i = i0, n1ikm
if (i == j) cycle i_aibjaiajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, j, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjaiajbi(nocc, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiajbi
end do a_aibjaiajbi
end do j_aibjaiajbi
end do b_aibjaiajbi
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == a, e == b, j == i, k == i, m == l
! No equalities independent of the above can hold.
!
l_aibiaialbl: do l = n0lm, n1lm
b_aibiaialbl: do b = n0be, n1be
em = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibiaialbl: do a = a0, n1acd
if (a == b) cycle a_aibiaialbl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijk)
i_aibiaialbl: do i = i0, n1ijk
if (i == l) cycle i_aibiaialbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, b, l)
jac_ibra_iket = eom_cc3_23_trans_aibiaialbl(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaialbl
end do a_aibiaialbl
end do b_aibiaialbl
end do l_aibiaialbl
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == a, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiaialbi: do l = n0l, n1l
b_aibiaialbi: do b = n0be, n1be
a0 = max(b + 1, n0acd)
a_aibiaialbi: do a = a0, n1acd
if (a == b) cycle a_aibiaialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijkm)
i_aibiaialbi: do i = i0, n1ijkm
if (i == l) cycle i_aibiaialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibiaialbi(nocc, a, i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaialbi
end do a_aibiaialbi
end do b_aibiaialbi
end do l_aibiaialbi
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


end subroutine ccjac_23_dav_part12
end module ccjac_block_23_dav_part12
