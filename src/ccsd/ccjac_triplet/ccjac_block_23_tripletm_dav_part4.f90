module ccjac_block_23_tripletm_dav_part4
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
subroutine ccjac_23_tripletm_dav_part4(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, b1, i0, i1, j0, j1, l0
integer :: n0abe, n0ac, n0acd, n0bd, n0be
integer :: n0cd, n0ce, n0ijk, n0ijkl, n0ijkm
integer :: n0ijl, n0ijm, n0ik, n0ikl, n0ikm
integer :: n0il, n0im, n0jk, n0jkl, n0jkm
integer :: n0jl, n0jm, n0kl, n0km
integer :: n1abe, n1ac, n1acd, n1bd, n1be
integer :: n1cd, n1ce, n1ijk, n1ijkl, n1ijkm
integer :: n1ijl, n1ijm, n1ik, n1ikl, n1ikm
integer :: n1il, n1im, n1jk, n1jkl, n1jkm
integer :: n1jl, n1jm, n1kl, n1km
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
n0abe = max(n0a, n0b, n0e)
n0ac = max(n0a, n0c)
n0acd = max(n0a, n0c, n0d)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
n0ce = max(n0c, n0e)
n0ijk = max(n0i, n0j, n0k)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijkm = max(n0i, n0j, n0k, n0m)
n0ijl = max(n0i, n0j, n0l)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1abe = min(n1a, n1b, n1e)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
n1cd = min(n1c, n1d)
n1ce = min(n1c, n1e)
n1ijk = min(n1i, n1j, n1k)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1ijkm = min(n1i, n1j, n1k, n1m)
n1ijl = min(n1i, n1j, n1l)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jk = min(n1j, n1k)
n1jkl = min(n1j, n1k, n1l)
n1jkm = min(n1j, n1k, n1m)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1km = min(n1k, n1m)
!
! Elementary loop  1
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
j1 = min(l - 1, n1jm)
j_aibjaiblej: do j = n0jm, j1
if (j == l) cycle j_aibjaiblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaiblej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaiblej
i_aibjaiblej: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaiblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaiblej(nocc, a, i, b, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaiblej
end do a_aibjaiblej
end do j_aibjaiblej
end do b_aibjaiblej
end do l_aibjaiblej
end do e_aibjaiblej
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, k, i
! Equalities: c == a, d == b, m == j, l == k
! No equalities independent of the above can hold.
!
e_aibjakbkej: do e = n0e, n1e
k_aibjakbkej: do k = n0kl, n1kl
b0 = max(e + 1, n0bd)
b_aibjakbkej: do b = b0, n1bd
if (b == e) cycle b_aibjakbkej
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aibjakbkej: do j = n0jm, j1
if (j == k) cycle j_aibjakbkej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbkej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakbkej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbkej: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbkej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (e - nvirt0) * (k - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakbkej(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakbkej
end do a_aibjakbkej
end do j_aibjakbkej
end do b_aibjakbkej
end do k_aibjakbkej
end do e_aibjakbkej
!
! Elementary loop  3
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
j0 = max(k + 1, n0jl)
j_aibjakbjek: do j = j0, n1jl
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakbjek(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakbjek
end do a_aibjakbjek
end do j_aibjakbjek
end do b_aibjakbjek
end do k_aibjakbjek
end do e_aibjakbjek
!
! Elementary loop  4
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
i0 = max(m + 1, n0ijl)
i_aibiakbiem: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibiakbiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiakbiem(i, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiakbiem
end do a_aibiakbiem
end do b_aibiakbiem
end do k_aibiakbiem
end do m_aibiakbiem
end do e_aibiakbiem
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: c == a, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
e_aibjaibiem: do e = n0e, n1e
m_aibjaibiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibjaibiem: do b = b0, n1bd
if (b == e) cycle b_aibjaibiem
j_aibjaibiem: do j = n0j, n1j
if (j == m) cycle j_aibjaibiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibiem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaibiem
i0 = max(m + 1, n0ikl)
i_aibjaibiem: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aibjaibiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaibiem(i, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaibiem
end do a_aibjaibiem
end do j_aibjaibiem
end do b_aibjaibiem
end do m_aibjaibiem
end do e_aibjaibiem
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l, m
! Equalities: c == a, d == b, j == i, k == i
! No equalities independent of the above can hold.
!
e_aibiaiblem: do e = n0e, n1e
m_aibiaiblem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l0 = max(m + 1, n0l)
l_aibiaiblem: do l = l0, n1l
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
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaiblem(i, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaiblem
end do a_aibiaiblem
end do b_aibiaiblem
end do l_aibiaiblem
end do m_aibiaiblem
end do e_aibiaiblem
!
! Elementary loop  7
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
j0 = max(m + 1, n0jl)
j_aibjaibjem: do j = j0, n1jl
if (j == m) cycle j_aibjaibjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibjem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaibjem
i_aibjaibjem: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjaibjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaibjem(nocc, a, i, b, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaibjem
end do a_aibjaibjem
end do j_aibjaibjem
end do b_aibjaibjem
end do m_aibjaibjem
end do e_aibjaibjem
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, m
! Equalities: c == a, d == b, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjajbjem: do e = n0e, n1e
m_aibjajbjem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibjajbjem: do b = b0, n1bd
if (b == e) cycle b_aibjajbjem
j0 = max(m + 1, n0jkl)
j_aibjajbjem: do j = j0, n1jkl
if (j == m) cycle j_aibjajbjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbjem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjajbjem
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajbjem: do i = n0i, n1i
if (i == j .or. i == m) cycle i_aibjajbjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajbjem(i, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajbjem
end do a_aibjajbjem
end do j_aibjajbjem
end do b_aibjajbjem
end do m_aibjajbjem
end do e_aibjajbjem
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, l
! Equalities: d == b, e == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjciblcj: do l = n0l, n1l
c_aibjciblcj: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjciblcj: do b = b0, n1bd
if (b == c) cycle b_aibjciblcj
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjciblcj: do j = n0jm, j1
if (j == l) cycle j_aibjciblcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjciblcj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjciblcj
i_aibjciblcj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjciblcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjciblcj(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjciblcj
end do a_aibjciblcj
end do j_aibjciblcj
end do b_aibjciblcj
end do c_aibjciblcj
end do l_aibjciblcj
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, m
! Equalities: d == b, e == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcibjcm: do m = n0m, n1m
c_aibjcibjcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjcibjcm: do b = b0, n1bd
if (b == c) cycle b_aibjcibjcm
j0 = max(m + 1, n0jl)
j_aibjcibjcm: do j = j0, n1jl
if (j == m) cycle j_aibjcibjcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibjcm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibjcm
i_aibjcibjcm: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjcibjcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcibjcm(a, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcibjcm
end do a_aibjcibjcm
end do j_aibjcibjcm
end do b_aibjcibjcm
end do c_aibjcibjcm
end do m_aibjcibjcm
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, l
! Equalities: d == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibiciblei: do e = n0e, n1e
l_aibiciblei: do l = n0l, n1l
c_aibiciblei: do c = n0c, n1c
if (c == e) cycle c_aibiciblei
b0 = max(e + 1, n0bd)
b_aibiciblei: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibiciblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibiciblei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibiciblei
i1 = min(l - 1, n1ijkm)
i_aibiciblei: do i = n0ijkm, i1
if (i == l) cycle i_aibiciblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiciblei(a, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiciblei
end do a_aibiciblei
end do b_aibiciblei
end do c_aibiciblei
end do l_aibiciblei
end do e_aibiciblei
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjcibjei: do e = n0e, n1e
c_aibjcibjei: do c = n0c, n1c
if (c == e) cycle c_aibjcibjei
b0 = max(e + 1, n0bd)
b_aibjcibjei: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcibjei
j_aibjcibjei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibjei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcibjei
i1 = min(j - 1, n1ikm)
i_aibjcibjei: do i = n0ikm, i1
if (i == j) cycle i_aibjcibjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcibjei(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcibjei
end do a_aibjcibjei
end do j_aibjcibjei
end do b_aibjcibjei
end do c_aibjcibjei
end do e_aibjcibjei
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjcibiej: do e = n0e, n1e
c_aibjcibiej: do c = n0c, n1c
if (c == e) cycle c_aibjcibiej
b0 = max(e + 1, n0bd)
b_aibjcibiej: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcibiej
j_aibjcibiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibiej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcibiej
i0 = max(j + 1, n0ikl)
i_aibjcibiej: do i = i0, n1ikl
if (i == j) cycle i_aibjcibiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcibiej(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcibiej
end do a_aibjcibiej
end do j_aibjcibiej
end do b_aibjcibiej
end do c_aibjcibiej
end do e_aibjcibiej
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, m
! Equalities: d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
e_aibicibiem: do e = n0e, n1e
m_aibicibiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aibicibiem: do c = n0c, n1c
if (c == e) cycle c_aibicibiem
b0 = max(e + 1, n0bd)
b_aibicibiem: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibicibiem
a0 = max(b + 1, n0a)
a_aibicibiem: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibicibiem
i0 = max(m + 1, n0ijkl)
i_aibicibiem: do i = i0, n1ijkl
if (i == m) cycle i_aibicibiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibicibiem(a, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibicibiem
end do a_aibicibiem
end do b_aibicibiem
end do c_aibicibiem
end do m_aibicibiem
end do e_aibicibiem
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, l
! Equalities: b == a, e == a, m == i, k == j
! No equalities independent of the above can hold.
!
d_aiajcjdlai: do d = n0d, n1d
l_aiajcjdlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aiajcjdlai: do c = n0c, n1c
if (c == d) cycle c_aiajcjdlai
j_aiajcjdlai: do j = n0jk, n1jk
if (j == l) cycle j_aiajcjdlai
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abe)
a_aiajcjdlai: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajcjdlai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajcjdlai: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajcjdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcjdlai(a, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcjdlai
end do a_aiajcjdlai
end do j_aiajcjdlai
end do c_aiajcjdlai
end do l_aiajcjdlai
end do d_aiajcjdlai
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, l
! Equalities: b == a, e == a, k == i, m == j
! No equalities independent of the above can hold.
!
d_aiajcidlaj: do d = n0d, n1d
l_aiajcidlaj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aiajcidlaj: do c = n0c, n1c
if (c == d) cycle c_aiajcidlaj
j1 = min(l - 1, n1jm)
j_aiajcidlaj: do j = n0jm, j1
if (j == l) cycle j_aiajcidlaj
a1 = min(d - 1, n1abe)
a_aiajcidlaj: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajcidlaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcidlaj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcidlaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcidlaj(a, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcidlaj
end do a_aiajcidlaj
end do j_aiajcidlaj
end do c_aiajcidlaj
end do l_aiajcidlaj
end do d_aiajcidlaj
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, m
! Equalities: b == a, e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aiajcjdiam: do m = n0m, n1m
d_aiajcjdiam: do d = n0d, n1d
c_aiajcjdiam: do c = n0c, n1c
if (c == d) cycle c_aiajcjdiam
j_aiajcjdiam: do j = n0jk, n1jk
if (j == m) cycle j_aiajcjdiam
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abe)
a_aiajcjdiam: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajcjdiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aiajcjdiam: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajcjdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcjdiam(a, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcjdiam
end do a_aiajcjdiam
end do j_aiajcjdiam
end do c_aiajcjdiam
end do d_aiajcjdiam
end do m_aiajcjdiam
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, m
! Equalities: b == a, e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajcidjam: do m = n0m, n1m
d_aiajcidjam: do d = n0d, n1d
c_aiajcidjam: do c = n0c, n1c
if (c == d) cycle c_aiajcidjam
j0 = max(m + 1, n0jl)
j_aiajcidjam: do j = j0, n1jl
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
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcidjam(a, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcidjam
end do a_aiajcidjam
end do j_aiajcidjam
end do c_aiajcidjam
end do d_aiajcidjam
end do m_aiajcidjam
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, l
! Equalities: c == a, d == a, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjakalbj: do l = n0l, n1l
k_aibjakalbj: do k = n0k, n1k
if (k == l) cycle k_aibjakalbj
b_aibjakalbj: do b = n0be, n1be
j1 = min(l - 1, n1jm)
j_aibjakalbj: do j = n0jm, j1
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakalbj(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakalbj
end do a_aibjakalbj
end do j_aibjakalbj
end do b_aibjakalbj
end do k_aibjakalbj
end do l_aibjakalbj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l, m
! Equalities: c == a, d == a, e == b, k == i
! No equalities independent of the above can hold.
!
m_aibjaialbm: do m = n0m, n1m
l0 = max(m + 1, n0l)
l_aibjaialbm: do l = l0, n1l
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
i_aibjaialbm: do i = n0ik, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aibjaialbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaialbm(a, j, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaialbm
end do a_aibjaialbm
end do j_aibjaialbm
end do b_aibjaialbm
end do l_aibjaialbm
end do m_aibjaialbm
!
! Elementary loop  21
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
j0 = max(m + 1, n0jl)
j_aibjakajbm: do j = j0, n1jl
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakajbm(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakajbm
end do a_aibjakajbm
end do j_aibjakajbm
end do b_aibjakajbm
end do k_aibjakajbm
end do m_aibjakajbm
!
! Elementary loop  22
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, l
! Equalities: e == b, d == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjciclbj: do l = n0l, n1l
c_aibjciclbj: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(c - 1, n1be)
b_aibjciclbj: do b = n0be, b1
if (b == c) cycle b_aibjciclbj
j1 = min(l - 1, n1jm)
j_aibjciclbj: do j = n0jm, j1
if (j == l) cycle j_aibjciclbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjciclbj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjciclbj
i_aibjciclbj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjciclbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjciclbj(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjciclbj
end do a_aibjciclbj
end do j_aibjciclbj
end do b_aibjciclbj
end do c_aibjciclbj
end do l_aibjciclbj
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, m
! Equalities: e == b, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcicjbm: do m = n0m, n1m
c_aibjcicjbm: do c = n0cd, n1cd
b1 = min(c - 1, n1be)
b_aibjcicjbm: do b = n0be, b1
if (b == c) cycle b_aibjcicjbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjcicjbm: do j = j0, n1jl
if (j == m) cycle j_aibjcicjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcicjbm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicjbm
i_aibjcicjbm: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjcicjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcicjbm(a, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcicjbm
end do a_aibjcicjbm
end do j_aibjcicjbm
end do b_aibjcicjbm
end do c_aibjcicjbm
end do m_aibjcicjbm
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, l
! Equalities: c == a, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
d_aibiakdlbi: do d = n0d, n1d
l_aibiakdlbi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aibiakdlbi: do k = n0k, n1k
if (k == l) cycle k_aibiakdlbi
b1 = min(d - 1, n1be)
b_aibiakdlbi: do b = n0be, b1
if (b == d) cycle b_aibiakdlbi
a0 = max(b + 1, n0ac)
a_aibiakdlbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdlbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(l - 1, n1ijm)
i_aibiakdlbi: do i = n0ijm, i1
if (i == k .or. i == l) cycle i_aibiakdlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiakdlbi(i, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiakdlbi
end do a_aibiakdlbi
end do b_aibiakdlbi
end do k_aibiakdlbi
end do l_aibiakdlbi
end do d_aibiakdlbi
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibjaidlbi: do d = n0d, n1d
l_aibjaidlbi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjaidlbi: do b = n0be, b1
if (b == d) cycle b_aibjaidlbi
j_aibjaidlbi: do j = n0j, n1j
if (j == l) cycle j_aibjaidlbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidlbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidlbi
i1 = min(l - 1, n1ikm)
i_aibjaidlbi: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjaidlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidlbi(i, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidlbi
end do a_aibjaidlbi
end do j_aibjaidlbi
end do b_aibjaidlbi
end do l_aibjaidlbi
end do d_aibjaidlbi
!
! Elementary loop  26
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
a0 = max(b + 1, n0ac)
a_aibjakdjbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdjbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjakdjbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjakdjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakdjbi(i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakdjbi
end do a_aibjakdjbi
end do j_aibjakdjbi
end do b_aibjakdjbi
end do k_aibjakdjbi
end do d_aibjakdjbi
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, l
! Equalities: c == a, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjajdlbj: do d = n0d, n1d
l_aibjajdlbj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjajdlbj: do b = n0be, b1
if (b == d) cycle b_aibjajdlbj
j1 = min(l - 1, n1jkm)
j_aibjajdlbj: do j = n0jkm, j1
if (j == l) cycle j_aibjajdlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajdlbj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdlbj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdlbj: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajdlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajdlbj(i, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajdlbj
end do a_aibjajdlbj
end do j_aibjajdlbj
end do b_aibjajdlbj
end do l_aibjajdlbj
end do d_aibjajdlbj
!
! Elementary loop  28
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
a0 = max(b + 1, n0ac)
a_aibjakdibj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdibj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjakdibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakdibj(i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakdibj
end do a_aibjakdibj
end do j_aibjakdibj
end do b_aibjakdibj
end do k_aibjakdibj
end do d_aibjakdibj
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjaidlbj: do d = n0d, n1d
l_aibjaidlbj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjaidlbj: do b = n0be, b1
if (b == d) cycle b_aibjaidlbj
j1 = min(l - 1, n1jm)
j_aibjaidlbj: do j = n0jm, j1
if (j == l) cycle j_aibjaidlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidlbj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidlbj
i_aibjaidlbj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaidlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidlbj(nocc, a, i, b, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidlbj
end do a_aibjaidlbj
end do j_aibjaidlbj
end do b_aibjaidlbj
end do l_aibjaidlbj
end do d_aibjaidlbj
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, k, i
! Equalities: c == a, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
d_aibjakdkbj: do d = n0d, n1d
k_aibjakdkbj: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjakdkbj: do b = n0be, b1
if (b == d) cycle b_aibjakdkbj
j1 = min(k - 1, n1jm)
j_aibjakdkbj: do j = n0jm, j1
if (j == k) cycle j_aibjakdkbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakdkbj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdkbj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdkbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakdkbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakdkbj(i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakdkbj
end do a_aibjakdkbj
end do j_aibjakdkbj
end do b_aibjakdkbj
end do k_aibjakdkbj
end do d_aibjakdkbj
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
end subroutine ccjac_23_tripletm_dav_part4
end module ccjac_block_23_tripletm_dav_part4
