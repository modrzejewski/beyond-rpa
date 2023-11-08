module ccjac_block_23_tripletm_dav_part2
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
subroutine ccjac_23_tripletm_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a, b, d, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, a1, b0, b1, d0, i0, i1, j0, j1, l0
integer :: n0abc, n0abce, n0ad, n0bc, n0bcd
integer :: n0bce, n0ijl, n0ijm, n0ik, n0ikm
integer :: n0il, n0im, n0jk, n0jkm, n0jl
integer :: n0jm, n0kl, n0km
integer :: n1abc, n1abce, n1ad, n1bc, n1bcd
integer :: n1bce, n1ijl, n1ijm, n1ik, n1ikm
integer :: n1il, n1im, n1jk, n1jkm, n1jl
integer :: n1jm, n1kl, n1km
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
n0abc = max(n0a, n0b, n0c)
n0abce = max(n0a, n0b, n0c, n0e)
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0bcd = max(n0b, n0c, n0d)
n0bce = max(n0b, n0c, n0e)
n0ijl = max(n0i, n0j, n0l)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1abc = min(n1a, n1b, n1c)
n1abce = min(n1a, n1b, n1c, n1e)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1bcd = min(n1b, n1c, n1d)
n1bce = min(n1b, n1c, n1e)
n1ijl = min(n1i, n1j, n1l)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jk = min(n1j, n1k)
n1jkm = min(n1j, n1k, n1m)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjblei: do e = n0e, n1e
l_aibjbjblei: do l = n0l, n1l
b0 = max(e + 1, n0bcd)
b_aibjbjblei: do b = b0, n1bcd
if (b == e) cycle b_aibjbjblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjbjblei: do j = n0jk, n1jk
if (j == l) cycle j_aibjbjblei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjblei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbjblei
i1 = min(l - 1, n1im)
i_aibjbjblei: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjbjblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjblei(a, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjblei
end do a_aibjbjblei
end do j_aibjbjblei
end do b_aibjbjblei
end do l_aibjbjblei
end do e_aibjbjblei
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbiblej: do e = n0e, n1e
l_aibjbiblej: do l = n0l, n1l
b0 = max(e + 1, n0bcd)
b_aibjbiblej: do b = b0, n1bcd
if (b == e) cycle b_aibjbiblej
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjbiblej: do j = n0jm, j1
if (j == l) cycle j_aibjbiblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbiblej: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbiblej
i_aibjbiblej: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjbiblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbiblej(a, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbiblej
end do a_aibjbiblej
end do j_aibjbiblej
end do b_aibjbiblej
end do l_aibjbiblej
end do e_aibjbiblej
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, m
! Equalities: c == b, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjbiem: do e = n0e, n1e
m_aibjbjbiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(e + 1, n0bcd)
b_aibjbjbiem: do b = b0, n1bcd
if (b == e) cycle b_aibjbjbiem
j_aibjbjbiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjbjbiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjbiem: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbjbiem
i0 = max(m + 1, n0il)
i_aibjbjbiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjbjbiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjbiem(a, b, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjbiem
end do a_aibjbjbiem
end do j_aibjbjbiem
end do b_aibjbjbiem
end do m_aibjbjbiem
end do e_aibjbjbiem
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, m
! Equalities: c == b, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbibjem: do e = n0e, n1e
m_aibjbibjem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(e + 1, n0bcd)
b_aibjbibjem: do b = b0, n1bcd
if (b == e) cycle b_aibjbibjem
j0 = max(m + 1, n0jl)
j_aibjbibjem: do j = j0, n1jl
if (j == m) cycle j_aibjbibjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibjem: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbibjem
i_aibjbibjem: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjbibjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbibjem(a, b, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbibjem
end do a_aibjbibjem
end do j_aibjbibjem
end do b_aibjbibjem
end do m_aibjbibjem
end do e_aibjbibjem
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, k, l
! Equalities: b == a, c == a, e == a, m == i
! No equalities independent of the above can hold.
!
d_aiajakdlai: do d = n0d, n1d
l_aiajakdlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aiajakdlai: do k = n0k, n1k
if (k == l) cycle k_aiajakdlai
j_aiajakdlai: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aiajakdlai
a1 = min(d - 1, n1abce)
a_aiajakdlai: do a = n0abce, a1
if (a == d) cycle a_aiajakdlai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajakdlai: do i = i0, i1
if (i == j .or. i == k .or. i == l) cycle i_aiajakdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakdlai(j, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakdlai
end do a_aiajakdlai
end do j_aiajakdlai
end do k_aiajakdlai
end do l_aiajakdlai
end do d_aiajakdlai
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, i, k, l
! Equalities: b == a, c == a, e == a, m == j
! No equalities independent of the above can hold.
!
d_aiajakdlaj: do d = n0d, n1d
l_aiajakdlaj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aiajakdlaj: do k = n0k, n1k
if (k == l) cycle k_aiajakdlaj
j1 = min(l - 1, n1jm)
j_aiajakdlaj: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aiajakdlaj
a1 = min(d - 1, n1abce)
a_aiajakdlaj: do a = n0abce, a1
if (a == d) cycle a_aiajakdlaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakdlaj: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajakdlaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakdlaj(i, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakdlaj
end do a_aiajakdlaj
end do j_aiajakdlaj
end do k_aiajakdlaj
end do l_aiajakdlaj
end do d_aiajakdlaj
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, k, m
! Equalities: b == a, c == a, e == a, l == i
! No equalities independent of the above can hold.
!
m_aiajakdiam: do m = n0m, n1m
d_aiajakdiam: do d = n0d, n1d
k_aiajakdiam: do k = n0k, n1k
if (k == m) cycle k_aiajakdiam
j_aiajakdiam: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aiajakdiam
a1 = min(d - 1, n1abce)
a_aiajakdiam: do a = n0abce, a1
if (a == d) cycle a_aiajakdiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aiajakdiam: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aiajakdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakdiam(j, k, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakdiam
end do a_aiajakdiam
end do j_aiajakdiam
end do k_aiajakdiam
end do d_aiajakdiam
end do m_aiajakdiam
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, l, m
! Equalities: b == a, c == a, e == a, k == i
! No equalities independent of the above can hold.
!
m_aiajaidlam: do m = n0m, n1m
d_aiajaidlam: do d = n0d, n1d
l0 = max(m + 1, n0l)
l_aiajaidlam: do l = l0, n1l
if (l == m) cycle l_aiajaidlam
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajaidlam: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aiajaidlam
a1 = min(d - 1, n1abce)
a_aiajaidlam: do a = n0abce, a1
if (a == d) cycle a_aiajaidlam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaidlam: do i = i0, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aiajaidlam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaidlam(j, d, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaidlam
end do a_aiajaidlam
end do j_aiajaidlam
end do l_aiajaidlam
end do d_aiajaidlam
end do m_aiajaidlam
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, i, k, m
! Equalities: b == a, c == a, e == a, l == j
! No equalities independent of the above can hold.
!
m_aiajakdjam: do m = n0m, n1m
d_aiajakdjam: do d = n0d, n1d
k_aiajakdjam: do k = n0k, n1k
if (k == m) cycle k_aiajakdjam
j0 = max(m + 1, n0jl)
j_aiajakdjam: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aiajakdjam
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abce)
a_aiajakdjam: do a = n0abce, a1
if (a == d) cycle a_aiajakdjam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakdjam: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aiajakdjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakdjam(i, k, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakdjam
end do a_aiajakdjam
end do j_aiajakdjam
end do k_aiajakdjam
end do d_aiajakdjam
end do m_aiajakdjam
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, i, l, m
! Equalities: b == a, c == a, e == a, k == j
! No equalities independent of the above can hold.
!
m_aiajajdlam: do m = n0m, n1m
d_aiajajdlam: do d = n0d, n1d
l0 = max(m + 1, n0l)
l_aiajajdlam: do l = l0, n1l
if (l == m) cycle l_aiajajdlam
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajajdlam: do j = n0jk, n1jk
if (j == l .or. j == m) cycle j_aiajajdlam
a1 = min(d - 1, n1abce)
a_aiajajdlam: do a = n0abce, a1
if (a == d) cycle a_aiajajdlam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajajdlam: do i = i0, n1i
if (i == j .or. i == l .or. i == m) cycle i_aiajajdlam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajdlam(i, d, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajdlam
end do a_aiajajdlam
end do j_aiajajdlam
end do l_aiajajdlam
end do d_aiajajdlam
end do m_aiajajdlam
!
! Elementary loop  11
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
a_aiajajdlei: do a = n0abc, n1abc
if (a == d .or. a == e) cycle a_aiajajdlei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajajdlei: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajajdlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajdlei(a, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajdlei
end do a_aiajajdlei
end do j_aiajajdlei
end do l_aiajajdlei
end do d_aiajajdlei
end do e_aiajajdlei
!
! Elementary loop  12
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
j1 = min(l - 1, n1jm)
j_aiajaidlej: do j = n0jm, j1
if (j == l) cycle j_aiajaidlej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajaidlej: do a = n0abc, n1abc
if (a == d .or. a == e) cycle a_aiajaidlej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaidlej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaidlej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaidlej(a, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaidlej
end do a_aiajaidlej
end do j_aiajaidlej
end do l_aiajaidlej
end do d_aiajaidlej
end do e_aiajaidlej
!
! Elementary loop  13
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
a_aiajajdiem: do a = n0abc, n1abc
if (a == d .or. a == e) cycle a_aiajajdiem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aiajajdiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajajdiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajdiem(a, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajdiem
end do a_aiajajdiem
end do j_aiajajdiem
end do d_aiajajdiem
end do m_aiajajdiem
end do e_aiajajdiem
!
! Elementary loop  14
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
j0 = max(m + 1, n0jl)
j_aiajaidjem: do j = j0, n1jl
if (j == m) cycle j_aiajaidjem
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajaidjem: do a = n0abc, n1abc
if (a == d .or. a == e) cycle a_aiajaidjem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaidjem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aiajaidjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaidjem(a, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaidjem
end do a_aiajaidjem
end do j_aiajaidjem
end do d_aiajaidjem
end do m_aiajaidjem
end do e_aiajaidjem
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: d == a, c == b, e == b, m == i
! No equalities independent of the above can hold.
!
l_aibjbkalbi: do l = n0l, n1l
k_aibjbkalbi: do k = n0k, n1k
if (k == l) cycle k_aibjbkalbi
b_aibjbkalbi: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkalbi: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjbkalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkalbi: do a = a0, n1ad
if (a == b) cycle a_aibjbkalbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjbkalbi: do i = n0im, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkalbi(b, j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkalbi
end do a_aibjbkalbi
end do j_aibjbkalbi
end do b_aibjbkalbi
end do k_aibjbkalbi
end do l_aibjbkalbi
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, m
! Equalities: d == a, c == b, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjbkaibm: do m = n0m, n1m
k_aibjbkaibm: do k = n0k, n1k
if (k == m) cycle k_aibjbkaibm
b_aibjbkaibm: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbkaibm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjbkaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkaibm: do a = a0, n1ad
if (a == b) cycle a_aibjbkaibm
i0 = max(m + 1, n0il)
i_aibjbkaibm: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjbkaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkaibm(b, j, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkaibm
end do a_aibjbkaibm
end do j_aibjbkaibm
end do b_aibjbkaibm
end do k_aibjbkaibm
end do m_aibjbkaibm
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l, m
! Equalities: d == a, c == b, e == b, k == j
! No equalities independent of the above can hold.
!
m_aibjbjalbm: do m = n0m, n1m
l0 = max(m + 1, n0l)
l_aibjbjalbm: do l = l0, n1l
if (l == m) cycle l_aibjbjalbm
b_aibjbjalbm: do b = n0bce, n1bce
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbjalbm: do j = n0jk, n1jk
if (j == l .or. j == m) cycle j_aibjbjalbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbjalbm: do a = a0, n1ad
if (a == b) cycle a_aibjbjalbm
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjbjalbm: do i = n0i, n1i
if (i == j .or. i == l .or. i == m) cycle i_aibjbjalbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjalbm(i, b, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjalbm
end do a_aibjbjalbm
end do j_aibjbjalbm
end do b_aibjbjalbm
end do l_aibjbjalbm
end do m_aibjbjalbm
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, l
! Equalities: c == b, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdlbi: do d = n0d, n1d
l_aibjbjdlbi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1bce)
b_aibjbjdlbi: do b = n0bce, b1
if (b == d) cycle b_aibjbjdlbi
j_aibjbjdlbi: do j = n0jk, n1jk
if (j == l) cycle j_aibjbjdlbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdlbi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdlbi
i1 = min(l - 1, n1im)
i_aibjbjdlbi: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjbjdlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdlbi(a, b, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdlbi
end do a_aibjbjdlbi
end do j_aibjbjdlbi
end do b_aibjbjdlbi
end do l_aibjbjdlbi
end do d_aibjbjdlbi
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, l
! Equalities: c == b, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbidlbj: do d = n0d, n1d
l_aibjbidlbj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1bce)
b_aibjbidlbj: do b = n0bce, b1
if (b == d) cycle b_aibjbidlbj
j1 = min(l - 1, n1jm)
j_aibjbidlbj: do j = n0jm, j1
if (j == l) cycle j_aibjbidlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidlbj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidlbj
i_aibjbidlbj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjbidlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbidlbj(a, b, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbidlbj
end do a_aibjbidlbj
end do j_aibjbidlbj
end do b_aibjbidlbj
end do l_aibjbidlbj
end do d_aibjbidlbj
!
! Elementary loop  20
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, m
! Equalities: c == b, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjbjdibm: do m = n0m, n1m
d_aibjbjdibm: do d = n0d, n1d
b1 = min(d - 1, n1bce)
b_aibjbjdibm: do b = n0bce, b1
if (b == d) cycle b_aibjbjdibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbjdibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjbjdibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdibm: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdibm
i0 = max(m + 1, n0il)
i_aibjbjdibm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjbjdibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdibm(a, b, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdibm
end do a_aibjbjdibm
end do j_aibjbjdibm
end do b_aibjbjdibm
end do d_aibjbjdibm
end do m_aibjbjdibm
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, m
! Equalities: c == b, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjbidjbm: do m = n0m, n1m
d_aibjbidjbm: do d = n0d, n1d
b1 = min(d - 1, n1bce)
b_aibjbidjbm: do b = n0bce, b1
if (b == d) cycle b_aibjbidjbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjbidjbm: do j = j0, n1jl
if (j == m) cycle j_aibjbidjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidjbm: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidjbm
i_aibjbidjbm: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjbidjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbidjbm(a, b, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbidjbm
end do a_aibjbidjbm
end do j_aibjbidjbm
end do b_aibjbidjbm
end do d_aibjbidjbm
end do m_aibjbidjbm
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k, l
! Equalities: d == a, c == b, j == i, m == i
! No equalities independent of the above can hold.
!
e_aibibkalei: do e = n0e, n1e
l_aibibkalei: do l = n0l, n1l
k_aibibkalei: do k = n0k, n1k
if (k == l) cycle k_aibibkalei
b_aibibkalei: do b = n0bc, n1bc
if (b == e) cycle b_aibibkalei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibibkalei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibibkalei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ijm)
i_aibibkalei: do i = n0ijm, i1
if (i == k .or. i == l) cycle i_aibibkalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibkalei(i, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibkalei
end do a_aibibkalei
end do b_aibibkalei
end do k_aibibkalei
end do l_aibibkalei
end do e_aibibkalei
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibjbialei: do e = n0e, n1e
l_aibjbialei: do l = n0l, n1l
b_aibjbialei: do b = n0bc, n1bc
if (b == e) cycle b_aibjbialei
j_aibjbialei: do j = n0j, n1j
if (j == l) cycle j_aibjbialei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbialei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbialei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ikm)
i_aibjbialei: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjbialei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbialei(i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbialei
end do a_aibjbialei
end do j_aibjbialei
end do b_aibjbialei
end do l_aibjbialei
end do e_aibjbialei
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbkajei: do e = n0e, n1e
k_aibjbkajei: do k = n0k, n1k
b_aibjbkajei: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkajei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkajei: do j = n0jl, n1jl
if (j == k) cycle j_aibjbkajei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkajei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjbkajei: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkajei(j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkajei
end do a_aibjbkajei
end do j_aibjbkajei
end do b_aibjbkajei
end do k_aibjbkajei
end do e_aibjbkajei
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k, j
! Equalities: d == a, c == b, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibjbkakei: do e = n0e, n1e
k_aibjbkakei: do k = n0kl, n1kl
b_aibjbkakei: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkakei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkakei: do j = n0j, n1j
if (j == k) cycle j_aibjbkakei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkakei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkakei
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1im)
i_aibjbkakei: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkakei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (e - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkakei(j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkakei
end do a_aibjbkakei
end do j_aibjbkakei
end do b_aibjbkakei
end do k_aibjbkakei
end do e_aibjbkakei
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjalei: do e = n0e, n1e
l_aibjbjalei: do l = n0l, n1l
b_aibjbjalei: do b = n0bc, n1bc
if (b == e) cycle b_aibjbjalei
j_aibjbjalei: do j = n0jk, n1jk
if (j == l) cycle j_aibjbjalei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbjalei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbjalei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjbjalei: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjbjalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjalei(nocc, a, i, b, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjalei
end do a_aibjbjalei
end do j_aibjbjalei
end do b_aibjbjalei
end do l_aibjbjalei
end do e_aibjbjalei
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, l
! Equalities: d == a, c == b, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjbjalej: do e = n0e, n1e
l_aibjbjalej: do l = n0l, n1l
b_aibjbjalej: do b = n0bc, n1bc
if (b == e) cycle b_aibjbjalej
j1 = min(l - 1, n1jkm)
j_aibjbjalej: do j = n0jkm, j1
if (j == l) cycle j_aibjbjalej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbjalej: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbjalej
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjbjalej: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjbjalej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjalej(i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjalej
end do a_aibjbjalej
end do j_aibjbjalej
end do b_aibjbjalej
end do l_aibjbjalej
end do e_aibjbjalej
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbkaiej: do e = n0e, n1e
k_aibjbkaiej: do k = n0k, n1k
b_aibjbkaiej: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkaiej
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjbkaiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkaiej: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkaiej
i0 = max(j + 1, n0il)
i_aibjbkaiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkaiej(j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkaiej
end do a_aibjbkaiej
end do j_aibjbkaiej
end do b_aibjbkaiej
end do k_aibjbkaiej
end do e_aibjbkaiej
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k, j
! Equalities: d == a, c == b, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibjbkaiek: do e = n0e, n1e
k_aibjbkaiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkaiek: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkaiek
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaiek: do j = n0j, n1j
if (j == k) cycle j_aibjbkaiek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkaiek: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkaiek
i0 = max(k + 1, n0il)
i_aibjbkaiek: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkaiek(j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkaiek
end do a_aibjbkaiek
end do j_aibjbkaiek
end do b_aibjbkaiek
end do k_aibjbkaiek
end do e_aibjbkaiek
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k, m
! Equalities: d == a, c == b, j == i, l == i
! No equalities independent of the above can hold.
!
e_aibibkaiem: do e = n0e, n1e
m_aibibkaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aibibkaiem: do k = n0k, n1k
if (k == m) cycle k_aibibkaiem
b_aibibkaiem: do b = n0bc, n1bc
if (b == e) cycle b_aibibkaiem
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibibkaiem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibibkaiem
i0 = max(m + 1, n0ijl)
i_aibibkaiem: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibibkaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibkaiem(i, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibkaiem
end do a_aibibkaiem
end do b_aibibkaiem
end do k_aibibkaiem
end do m_aibibkaiem
end do e_aibibkaiem
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
end subroutine ccjac_23_tripletm_dav_part2
end module ccjac_block_23_tripletm_dav_part2
