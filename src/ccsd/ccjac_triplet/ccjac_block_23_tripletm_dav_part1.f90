module ccjac_block_23_tripletm_dav_part1
use eom_cc3_23_tripletm_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
use cc_gparams 
implicit none
!
! File generated automatically on 2014-04-05 22:51:08 UTC.
!
contains
subroutine ccjac_23_tripletm_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
 nvirt0, nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, &
 n1j, n0k, n1k, n0l, n1l, n0m, n1m, bra0, ket0) 
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
!
! Local variables
!
real(F64) :: jac_ibra_iket
integer :: a, b, c, d, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, a1, b0, b1, d0, i0, i1, j0, j1, l0
integer :: n0abcd, n0ac, n0ad, n0ae, n0bc
integer :: n0bd, n0be, n0ik, n0il, n0im
integer :: n0jk, n0jl, n0jm
integer :: n1abcd, n1ac, n1ad, n1ae, n1bc
integer :: n1bd, n1be, n1ik, n1il, n1im
integer :: n1jk, n1jl, n1jm
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
n0abcd = max(n0a, n0b, n0c, n0d)
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
n1abcd = min(n1a, n1b, n1c, n1d)
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
! Free occupied indices: i, j, k, l
! Equalities: d == a, c == b, m == i
! No equalities independent of the above can hold.
!
e_aibjbkalei: do e = n0e, n1e
l_aibjbkalei: do l = n0l, n1l
k_aibjbkalei: do k = n0k, n1k
if (k == l) cycle k_aibjbkalei
b_aibjbkalei: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkalei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkalei: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjbkalei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkalei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkalei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjbkalei: do i = n0im, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkalei(j, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkalei
end do a_aibjbkalei
end do j_aibjbkalei
end do b_aibjbkalei
end do k_aibjbkalei
end do l_aibjbkalei
end do e_aibjbkalei
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k, m
! Equalities: d == a, c == b, l == i
! No equalities independent of the above can hold.
!
e_aibjbkaiem: do e = n0e, n1e
m_aibjbkaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aibjbkaiem: do k = n0k, n1k
if (k == m) cycle k_aibjbkaiem
b_aibjbkaiem: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkaiem
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaiem: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjbkaiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkaiem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkaiem
i0 = max(m + 1, n0il)
i_aibjbkaiem: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjbkaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkaiem(j, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkaiem
end do a_aibjbkaiem
end do j_aibjbkaiem
end do b_aibjbkaiem
end do k_aibjbkaiem
end do m_aibjbkaiem
end do e_aibjbkaiem
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, l, m
! Equalities: d == a, c == b, k == j
! No equalities independent of the above can hold.
!
e_aibjbjalem: do e = n0e, n1e
m_aibjbjalem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l0 = max(m + 1, n0l)
l_aibjbjalem: do l = l0, n1l
if (l == m) cycle l_aibjbjalem
b_aibjbjalem: do b = n0bc, n1bc
if (b == e) cycle b_aibjbjalem
j_aibjbjalem: do j = n0jk, n1jk
if (j == l .or. j == m) cycle j_aibjbjalem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbjalem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbjalem
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjbjalem: do i = n0i, n1i
if (i == j .or. i == l .or. i == m) cycle i_aibjbjalem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjalem(i, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjalem
end do a_aibjbjalem
end do j_aibjbjalem
end do b_aibjbjalem
end do l_aibjbjalem
end do m_aibjbjalem
end do e_aibjbjalem
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k, l
! Equalities: e == a, c == b, m == i
! No equalities independent of the above can hold.
!
d_aibjbkdlai: do d = n0d, n1d
l_aibjbkdlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aibjbkdlai: do k = n0k, n1k
if (k == l) cycle k_aibjbkdlai
b_aibjbkdlai: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdlai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdlai: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjbkdlai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdlai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdlai
i1 = min(l - 1, n1im)
i_aibjbkdlai: do i = n0im, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkdlai(j, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkdlai
end do a_aibjbkdlai
end do j_aibjbkdlai
end do b_aibjbkdlai
end do k_aibjbkdlai
end do l_aibjbkdlai
end do d_aibjbkdlai
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k, m
! Equalities: e == a, c == b, l == i
! No equalities independent of the above can hold.
!
m_aibjbkdiam: do m = n0m, n1m
d_aibjbkdiam: do d = n0d, n1d
k_aibjbkdiam: do k = n0k, n1k
if (k == m) cycle k_aibjbkdiam
b_aibjbkdiam: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdiam
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdiam: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjbkdiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdiam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0il)
i_aibjbkdiam: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjbkdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkdiam(j, k, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkdiam
end do a_aibjbkdiam
end do j_aibjbkdiam
end do b_aibjbkdiam
end do k_aibjbkdiam
end do d_aibjbkdiam
end do m_aibjbkdiam
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, l, m
! Equalities: e == a, c == b, k == j
! No equalities independent of the above can hold.
!
m_aibjbjdlam: do m = n0m, n1m
d_aibjbjdlam: do d = n0d, n1d
l0 = max(m + 1, n0l)
l_aibjbjdlam: do l = l0, n1l
if (l == m) cycle l_aibjbjdlam
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjbjdlam: do b = n0bc, n1bc
if (b == d) cycle b_aibjbjdlam
j_aibjbjdlam: do j = n0jk, n1jk
if (j == l .or. j == m) cycle j_aibjbjdlam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbjdlam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbjdlam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjbjdlam: do i = n0i, n1i
if (i == j .or. i == l .or. i == m) cycle i_aibjbjdlam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdlam(i, d, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdlam
end do a_aibjbjdlam
end do j_aibjbjdlam
end do b_aibjbjdlam
end do l_aibjbjdlam
end do d_aibjbjdlam
end do m_aibjbjdlam
!
! Elementary loop  7
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
b_aibjbjdlei: do b = n0bc, n1bc
if (b == d .or. b == e) cycle b_aibjbjdlei
j_aibjbjdlei: do j = n0jk, n1jk
if (j == l) cycle j_aibjbjdlei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdlei: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbjdlei
i1 = min(l - 1, n1im)
i_aibjbjdlei: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjbjdlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdlei(a, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdlei
end do a_aibjbjdlei
end do j_aibjbjdlei
end do b_aibjbjdlei
end do l_aibjbjdlei
end do d_aibjbjdlei
end do e_aibjbjdlei
!
! Elementary loop  8
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
b_aibjbjdiem: do b = n0bc, n1bc
if (b == d .or. b == e) cycle b_aibjbjdiem
j_aibjbjdiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjbjdiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdiem: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbjdiem
i0 = max(m + 1, n0il)
i_aibjbjdiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjbjdiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdiem(a, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdiem
end do a_aibjbjdiem
end do j_aibjbjdiem
end do b_aibjbjdiem
end do d_aibjbjdiem
end do m_aibjbjdiem
end do e_aibjbjdiem
!
! Elementary loop  9
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
j1 = min(l - 1, n1jm)
j_aibjakblej: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aibjakblej
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakblej(i, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakblej
end do a_aibjakblej
end do j_aibjakblej
end do b_aibjakblej
end do k_aibjakblej
end do l_aibjakblej
end do e_aibjakblej
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l, m
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
e_aibjaiblem: do e = n0e, n1e
m_aibjaiblem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l0 = max(m + 1, n0l)
l_aibjaiblem: do l = l0, n1l
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
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaiblem(j, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaiblem
end do a_aibjaiblem
end do j_aibjaiblem
end do b_aibjaiblem
end do l_aibjaiblem
end do m_aibjaiblem
end do e_aibjaiblem
!
! Elementary loop  11
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
j0 = max(m + 1, n0jl)
j_aibjakbjem: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjakbjem
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakbjem(i, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakbjem
end do a_aibjakbjem
end do j_aibjakbjem
end do b_aibjakbjem
end do k_aibjakbjem
end do m_aibjakbjem
end do e_aibjakbjem
!
! Elementary loop  12
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
b_aibjciblej: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjciblej
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjciblej: do j = n0jm, j1
if (j == l) cycle j_aibjciblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjciblej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjciblej
i_aibjciblej: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjciblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjciblej(a, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjciblej
end do a_aibjciblej
end do j_aibjciblej
end do b_aibjciblej
end do c_aibjciblej
end do l_aibjciblej
end do e_aibjciblej
!
! Elementary loop  13
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
b_aibjcibjem: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcibjem
j0 = max(m + 1, n0jl)
j_aibjcibjem: do j = j0, n1jl
if (j == m) cycle j_aibjcibjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibjem: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcibjem
i_aibjcibjem: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjcibjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcibjem(a, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcibjem
end do a_aibjcibjem
end do j_aibjcibjem
end do b_aibjcibjem
end do c_aibjcibjem
end do m_aibjcibjem
end do e_aibjcibjem
!
! Elementary loop  14
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
j1 = min(l - 1, n1jm)
j_aibjakdlbj: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aibjakdlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakdlbj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdlbj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdlbj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakdlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakdlbj(i, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakdlbj
end do a_aibjakdlbj
end do j_aibjakdlbj
end do b_aibjakdlbj
end do k_aibjakdlbj
end do l_aibjakdlbj
end do d_aibjakdlbj
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l, m
! Equalities: c == a, e == b, k == i
! No equalities independent of the above can hold.
!
m_aibjaidlbm: do m = n0m, n1m
d_aibjaidlbm: do d = n0d, n1d
l0 = max(m + 1, n0l)
l_aibjaidlbm: do l = l0, n1l
if (l == m) cycle l_aibjaidlbm
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjaidlbm: do b = n0be, b1
if (b == d) cycle b_aibjaidlbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaidlbm: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aibjaidlbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidlbm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidlbm
i_aibjaidlbm: do i = n0ik, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aibjaidlbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidlbm(j, d, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidlbm
end do a_aibjaidlbm
end do j_aibjaidlbm
end do b_aibjaidlbm
end do l_aibjaidlbm
end do d_aibjaidlbm
end do m_aibjaidlbm
!
! Elementary loop  16
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
j0 = max(m + 1, n0jl)
j_aibjakdjbm: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjakdjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakdjbm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdjbm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjbm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakdjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakdjbm(i, k, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakdjbm
end do a_aibjakdjbm
end do j_aibjakdjbm
end do b_aibjakdjbm
end do k_aibjakdjbm
end do d_aibjakdjbm
end do m_aibjakdjbm
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, l
! Equalities: e == b, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjcidlbj: do d = n0d, n1d
l_aibjcidlbj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aibjcidlbj: do c = n0c, n1c
if (c == d) cycle c_aibjcidlbj
b1 = min(d - 1, n1be)
b_aibjcidlbj: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcidlbj
j1 = min(l - 1, n1jm)
j_aibjcidlbj: do j = n0jm, j1
if (j == l) cycle j_aibjcidlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidlbj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidlbj
i_aibjcidlbj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcidlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcidlbj(a, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcidlbj
end do a_aibjcidlbj
end do j_aibjcidlbj
end do b_aibjcidlbj
end do c_aibjcidlbj
end do l_aibjcidlbj
end do d_aibjcidlbj
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, m
! Equalities: e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcidjbm: do m = n0m, n1m
d_aibjcidjbm: do d = n0d, n1d
c_aibjcidjbm: do c = n0c, n1c
if (c == d) cycle c_aibjcidjbm
b1 = min(d - 1, n1be)
b_aibjcidjbm: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcidjbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjcidjbm: do j = j0, n1jl
if (j == m) cycle j_aibjcidjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidjbm: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjbm
i_aibjcidjbm: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjcidjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcidjbm(a, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcidjbm
end do a_aibjcidjbm
end do j_aibjcidjbm
end do b_aibjcidjbm
end do c_aibjcidjbm
end do d_aibjcidjbm
end do m_aibjcidjbm
!
! Elementary loop  19
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
j1 = min(l - 1, n1jm)
j_aibjaidlej: do j = n0jm, j1
if (j == l) cycle j_aibjaidlej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidlej: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjaidlej
i_aibjaidlej: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaidlej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidlej(b, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidlej
end do a_aibjaidlej
end do j_aibjaidlej
end do b_aibjaidlej
end do l_aibjaidlej
end do d_aibjaidlej
end do e_aibjaidlej
!
! Elementary loop  20
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
j0 = max(m + 1, n0jl)
j_aibjaidjem: do j = j0, n1jl
if (j == m) cycle j_aibjaidjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidjem: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjaidjem
i_aibjaidjem: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjaidjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidjem(b, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidjem
end do a_aibjaidjem
end do j_aibjaidjem
end do b_aibjaidjem
end do d_aibjaidjem
end do m_aibjaidjem
end do e_aibjaidjem
!
! Elementary loop  21
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
a_aibjcjalei: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjcjalei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjcjalei: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjcjalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjalei(b, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjalei
end do a_aibjcjalei
end do j_aibjcjalei
end do b_aibjcjalei
end do c_aibjcjalei
end do l_aibjcjalei
end do e_aibjcjalei
!
! Elementary loop  22
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
a_aibjcjaiem: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjcjaiem
i0 = max(m + 1, n0il)
i_aibjcjaiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjcjaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjaiem(b, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjaiem
end do a_aibjcjaiem
end do j_aibjcjaiem
end do b_aibjcjaiem
end do c_aibjcjaiem
end do m_aibjcjaiem
end do e_aibjcjaiem
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, l
! Equalities: e == a, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdlai: do d = n0d, n1d
l_aibjcjdlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aibjcjdlai: do c = n0c, n1c
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
i1 = min(l - 1, n1im)
i_aibjcjdlai: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjcjdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjdlai(b, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjdlai
end do a_aibjcjdlai
end do j_aibjcjdlai
end do b_aibjcjdlai
end do c_aibjcjdlai
end do l_aibjcjdlai
end do d_aibjcjdlai
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, m
! Equalities: e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjdiam: do m = n0m, n1m
d_aibjcjdiam: do d = n0d, n1d
c_aibjcjdiam: do c = n0c, n1c
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
i0 = max(m + 1, n0il)
i_aibjcjdiam: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjcjdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjdiam(b, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjdiam
end do a_aibjcjdiam
end do j_aibjcjdiam
end do b_aibjcjdiam
end do c_aibjcjdiam
end do d_aibjcjdiam
end do m_aibjcjdiam
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, k, l
! Equalities: b == a, c == a, d == a, m == i
! No equalities independent of the above can hold.
!
e_aiajakalei: do e = n0e, n1e
l_aiajakalei: do l = n0l, n1l
k_aiajakalei: do k = n0k, n1k
if (k == l) cycle k_aiajakalei
j_aiajakalei: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aiajakalei
a0 = max(e + 1, n0abcd)
a_aiajakalei: do a = a0, n1abcd
if (a == e) cycle a_aiajakalei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajakalei: do i = i0, i1
if (i == j .or. i == k .or. i == l) cycle i_aiajakalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakalei(j, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakalei
end do a_aiajakalei
end do j_aiajakalei
end do k_aiajakalei
end do l_aiajakalei
end do e_aiajakalei
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, k, l
! Equalities: b == a, c == a, d == a, m == j
! No equalities independent of the above can hold.
!
e_aiajakalej: do e = n0e, n1e
l_aiajakalej: do l = n0l, n1l
k_aiajakalej: do k = n0k, n1k
if (k == l) cycle k_aiajakalej
j1 = min(l - 1, n1jm)
j_aiajakalej: do j = n0jm, j1
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakalej(i, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakalej
end do a_aiajakalej
end do j_aiajakalej
end do k_aiajakalej
end do l_aiajakalej
end do e_aiajakalej
!
! Elementary loop  27
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
i0 = max(j + 1, m + 1, n0il)
i_aiajakaiem: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aiajakaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakaiem(j, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakaiem
end do a_aiajakaiem
end do j_aiajakaiem
end do k_aiajakaiem
end do m_aiajakaiem
end do e_aiajakaiem
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, l, m
! Equalities: b == a, c == a, d == a, k == i
! No equalities independent of the above can hold.
!
e_aiajaialem: do e = n0e, n1e
m_aiajaialem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l0 = max(m + 1, n0l)
l_aiajaialem: do l = l0, n1l
if (l == m) cycle l_aiajaialem
j_aiajaialem: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aiajaialem
a0 = max(e + 1, n0abcd)
a_aiajaialem: do a = a0, n1abcd
if (a == e) cycle a_aiajaialem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaialem: do i = i0, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aiajaialem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaialem(j, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaialem
end do a_aiajaialem
end do j_aiajaialem
end do l_aiajaialem
end do m_aiajaialem
end do e_aiajaialem
!
! Elementary loop  29
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
j0 = max(m + 1, n0jl)
j_aiajakajem: do j = j0, n1jl
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakajem(i, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakajem
end do a_aiajakajem
end do j_aiajakajem
end do k_aiajakajem
end do m_aiajakajem
end do e_aiajakajem
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, l, m
! Equalities: b == a, c == a, d == a, k == j
! No equalities independent of the above can hold.
!
e_aiajajalem: do e = n0e, n1e
m_aiajajalem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l0 = max(m + 1, n0l)
l_aiajajalem: do l = l0, n1l
if (l == m) cycle l_aiajajalem
j_aiajajalem: do j = n0jk, n1jk
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajalem(i, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajalem
end do a_aiajajalem
end do j_aiajajalem
end do l_aiajajalem
end do m_aiajajalem
end do e_aiajajalem
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
end subroutine ccjac_23_tripletm_dav_part1
end module ccjac_block_23_tripletm_dav_part1
