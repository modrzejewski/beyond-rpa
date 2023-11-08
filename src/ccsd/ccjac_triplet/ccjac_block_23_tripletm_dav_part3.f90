module ccjac_block_23_tripletm_dav_part3
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
subroutine ccjac_23_tripletm_dav_part3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, d0, i0, i1, j0, j1, l0
integer :: n0abd, n0ac, n0ad, n0ae, n0bc
integer :: n0bd, n0ijk, n0ijkl, n0ijkm, n0ijl
integer :: n0ijm, n0ik, n0ikl, n0ikm, n0il
integer :: n0im, n0jk, n0jkl, n0jkm, n0jl
integer :: n0jm, n0kl, n0km
integer :: n1abd, n1ac, n1ad, n1ae, n1bc
integer :: n1bd, n1ijk, n1ijkl, n1ijkm, n1ijl
integer :: n1ijm, n1ik, n1ikl, n1ikm, n1il
integer :: n1im, n1jk, n1jkl, n1jkm, n1jl
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
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0bc = max(n0b, n0c)
n0bd = max(n0b, n0d)
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
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bc = min(n1b, n1c)
n1bd = min(n1b, n1d)
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
! Free occupied indices: i, j, m
! Equalities: d == a, c == b, k == i, l == i
! No equalities independent of the above can hold.
!
e_aibjbiaiem: do e = n0e, n1e
m_aibjbiaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjbiaiem: do b = n0bc, n1bc
if (b == e) cycle b_aibjbiaiem
j_aibjbiaiem: do j = n0j, n1j
if (j == m) cycle j_aibjbiaiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbiaiem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbiaiem
i0 = max(m + 1, n0ikl)
i_aibjbiaiem: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aibjbiaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbiaiem(i, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbiaiem
end do a_aibjbiaiem
end do j_aibjbiaiem
end do b_aibjbiaiem
end do m_aibjbiaiem
end do e_aibjbiaiem
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: d == a, c == b, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjaiem: do e = n0e, n1e
m_aibjbjaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjbjaiem: do b = n0bc, n1bc
if (b == e) cycle b_aibjbjaiem
j_aibjbjaiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjbjaiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbjaiem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbjaiem
i0 = max(m + 1, n0il)
i_aibjbjaiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjbjaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjaiem(nocc, a, i, b, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjaiem
end do a_aibjbjaiem
end do j_aibjbjaiem
end do b_aibjbjaiem
end do m_aibjbjaiem
end do e_aibjbjaiem
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l, m
! Equalities: d == a, c == b, j == i, k == i
! No equalities independent of the above can hold.
!
e_aibibialem: do e = n0e, n1e
m_aibibialem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l0 = max(m + 1, n0l)
l_aibibialem: do l = l0, n1l
if (l == m) cycle l_aibibialem
b_aibibialem: do b = n0bc, n1bc
if (b == e) cycle b_aibibialem
a0 = max(b + 1, e + 1, n0ad)
a_aibibialem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibibialem
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibibialem: do i = n0ijk, n1ijk
if (i == l .or. i == m) cycle i_aibibialem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibialem(i, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibialem
end do a_aibibialem
end do b_aibibialem
end do l_aibibialem
end do m_aibibialem
end do e_aibibialem
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, m
! Equalities: d == a, c == b, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjbjajem: do e = n0e, n1e
m_aibjbjajem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjbjajem: do b = n0bc, n1bc
if (b == e) cycle b_aibjbjajem
j0 = max(m + 1, n0jkl)
j_aibjbjajem: do j = j0, n1jkl
if (j == m) cycle j_aibjbjajem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbjajem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbjajem
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbjajem: do i = n0i, n1i
if (i == j .or. i == m) cycle i_aibjbjajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjajem(i, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjajem
end do a_aibjbjajem
end do j_aibjbjajem
end do b_aibjbjajem
end do m_aibjbjajem
end do e_aibjbjajem
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, l
! Equalities: e == a, c == b, j == i, m == i
! No equalities independent of the above can hold.
!
d_aibibkdlai: do d = n0d, n1d
l_aibibkdlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aibibkdlai: do k = n0k, n1k
if (k == l) cycle k_aibibkdlai
b_aibibkdlai: do b = n0bc, n1bc
if (b == d) cycle b_aibibkdlai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibibkdlai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibibkdlai
i1 = min(l - 1, n1ijm)
i_aibibkdlai: do i = n0ijm, i1
if (i == k .or. i == l) cycle i_aibibkdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibkdlai(i, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibkdlai
end do a_aibibkdlai
end do b_aibibkdlai
end do k_aibibkdlai
end do l_aibibkdlai
end do d_aibibkdlai
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: e == a, c == b, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibjbidlai: do d = n0d, n1d
l_aibjbidlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjbidlai: do b = n0bc, n1bc
if (b == d) cycle b_aibjbidlai
j_aibjbidlai: do j = n0j, n1j
if (j == l) cycle j_aibjbidlai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbidlai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbidlai
i1 = min(l - 1, n1ikm)
i_aibjbidlai: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjbidlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbidlai(i, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbidlai
end do a_aibjbidlai
end do j_aibjbidlai
end do b_aibjbidlai
end do l_aibjbidlai
end do d_aibjbidlai
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: e == a, c == b, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdjai: do d = n0d, n1d
k_aibjbkdjai: do k = n0k, n1k
b_aibjbkdjai: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdjai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdjai: do j = n0jl, n1jl
if (j == k) cycle j_aibjbkdjai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdjai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdjai
i1 = min(j - 1, n1im)
i_aibjbkdjai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkdjai(j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkdjai
end do a_aibjbkdjai
end do j_aibjbkdjai
end do b_aibjbkdjai
end do k_aibjbkdjai
end do d_aibjbkdjai
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, j
! Equalities: e == a, c == b, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibjbkdkai: do d = n0d, n1d
k_aibjbkdkai: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkdkai: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdkai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdkai: do j = n0j, n1j
if (j == k) cycle j_aibjbkdkai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdkai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdkai
i1 = min(k - 1, n1im)
i_aibjbkdkai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkdkai(j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkdkai
end do a_aibjbkdkai
end do j_aibjbkdkai
end do b_aibjbkdkai
end do k_aibjbkdkai
end do d_aibjbkdkai
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: e == a, c == b, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdlai: do d = n0d, n1d
l_aibjbjdlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjbjdlai: do b = n0bc, n1bc
if (b == d) cycle b_aibjbjdlai
j_aibjbjdlai: do j = n0jk, n1jk
if (j == l) cycle j_aibjbjdlai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbjdlai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbjdlai
i1 = min(l - 1, n1im)
i_aibjbjdlai: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjbjdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdlai(nocc, a, i, b, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdlai
end do a_aibjbjdlai
end do j_aibjbjdlai
end do b_aibjbjdlai
end do l_aibjbjdlai
end do d_aibjbjdlai
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, l
! Equalities: e == a, c == b, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjbjdlaj: do d = n0d, n1d
l_aibjbjdlaj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjbjdlaj: do b = n0bc, n1bc
if (b == d) cycle b_aibjbjdlaj
j1 = min(l - 1, n1jkm)
j_aibjbjdlaj: do j = n0jkm, j1
if (j == l) cycle j_aibjbjdlaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbjdlaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbjdlaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbjdlaj: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjbjdlaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdlaj(i, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdlaj
end do a_aibjbjdlaj
end do j_aibjbjdlaj
end do b_aibjbjdlaj
end do l_aibjbjdlaj
end do d_aibjbjdlaj
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: e == a, c == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbkdiaj: do d = n0d, n1d
k_aibjbkdiaj: do k = n0k, n1k
b_aibjbkdiaj: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdiaj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdiaj: do j = n0jm, n1jm
if (j == k) cycle j_aibjbkdiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdiaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjbkdiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkdiaj(j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkdiaj
end do a_aibjbkdiaj
end do j_aibjbkdiaj
end do b_aibjbkdiaj
end do k_aibjbkdiaj
end do d_aibjbkdiaj
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, j
! Equalities: e == a, c == b, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjbkdiak: do d = n0d, n1d
k_aibjbkdiak: do k = n0km, n1km
b_aibjbkdiak: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdiak
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdiak: do j = n0j, n1j
if (j == k) cycle j_aibjbkdiak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdiak: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0il)
i_aibjbkdiak: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkdiak(j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkdiak
end do a_aibjbkdiak
end do j_aibjbkdiak
end do b_aibjbkdiak
end do k_aibjbkdiak
end do d_aibjbkdiak
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, m
! Equalities: e == a, c == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibibkdiam: do m = n0m, n1m
d_aibibkdiam: do d = n0d, n1d
k_aibibkdiam: do k = n0k, n1k
if (k == m) cycle k_aibibkdiam
b_aibibkdiam: do b = n0bc, n1bc
if (b == d) cycle b_aibibkdiam
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibibkdiam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibibkdiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0ijl)
i_aibibkdiam: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibibkdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibkdiam(i, k, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibkdiam
end do a_aibibkdiam
end do b_aibibkdiam
end do k_aibibkdiam
end do d_aibibkdiam
end do m_aibibkdiam
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, m
! Equalities: e == a, c == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjbidiam: do m = n0m, n1m
d_aibjbidiam: do d = n0d, n1d
b_aibjbidiam: do b = n0bc, n1bc
if (b == d) cycle b_aibjbidiam
j_aibjbidiam: do j = n0j, n1j
if (j == m) cycle j_aibjbidiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbidiam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbidiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0ikl)
i_aibjbidiam: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aibjbidiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbidiam(i, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbidiam
end do a_aibjbidiam
end do j_aibjbidiam
end do b_aibjbidiam
end do d_aibjbidiam
end do m_aibjbidiam
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, m
! Equalities: e == a, c == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjbjdiam: do m = n0m, n1m
d_aibjbjdiam: do d = n0d, n1d
b_aibjbjdiam: do b = n0bc, n1bc
if (b == d) cycle b_aibjbjdiam
j_aibjbjdiam: do j = n0jk, n1jk
if (j == m) cycle j_aibjbjdiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbjdiam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbjdiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0il)
i_aibjbjdiam: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjbjdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdiam(nocc, a, i, b, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdiam
end do a_aibjbjdiam
end do j_aibjbjdiam
end do b_aibjbjdiam
end do d_aibjbjdiam
end do m_aibjbjdiam
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l, m
! Equalities: e == a, c == b, j == i, k == i
! No equalities independent of the above can hold.
!
m_aibibidlam: do m = n0m, n1m
d_aibibidlam: do d = n0d, n1d
l0 = max(m + 1, n0l)
l_aibibidlam: do l = l0, n1l
if (l == m) cycle l_aibibidlam
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibibidlam: do b = n0bc, n1bc
if (b == d) cycle b_aibibidlam
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibibidlam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibibidlam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibibidlam: do i = n0ijk, n1ijk
if (i == l .or. i == m) cycle i_aibibidlam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibidlam(i, d, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibidlam
end do a_aibibidlam
end do b_aibibidlam
end do l_aibibidlam
end do d_aibibidlam
end do m_aibibidlam
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, m
! Equalities: e == a, c == b, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjbjdjam: do m = n0m, n1m
d_aibjbjdjam: do d = n0d, n1d
b_aibjbjdjam: do b = n0bc, n1bc
if (b == d) cycle b_aibjbjdjam
j0 = max(m + 1, n0jkl)
j_aibjbjdjam: do j = j0, n1jkl
if (j == m) cycle j_aibjbjdjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbjdjam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbjdjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjbjdjam: do i = n0i, n1i
if (i == j .or. i == m) cycle i_aibjbjdjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdjam(i, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdjam
end do a_aibjbjdjam
end do j_aibjbjdjam
end do b_aibjbjdjam
end do d_aibjbjdjam
end do m_aibjbjdjam
!
! Elementary loop  18
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
b_aibibidlei: do b = n0bc, n1bc
if (b == d .or. b == e) cycle b_aibibidlei
a0 = max(b + 1, n0a)
a_aibibidlei: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibibidlei
i1 = min(l - 1, n1ijkm)
i_aibibidlei: do i = n0ijkm, i1
if (i == l) cycle i_aibibidlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibidlei(a, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibidlei
end do a_aibibidlei
end do b_aibibidlei
end do l_aibibidlei
end do d_aibibidlei
end do e_aibibidlei
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j
! Equalities: c == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjbjdjei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjbjdjei: do d = d0, n1d
if (d == e) cycle d_aibjbjdjei
b_aibjbjdjei: do b = n0bc, n1bc
if (b == d .or. b == e) cycle b_aibjbjdjei
j_aibjbjdjei: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdjei: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbjdjei
i1 = min(j - 1, n1im)
i_aibjbjdjei: do i = n0im, i1
if (i == j) cycle i_aibjbjdjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdjei(a, j, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdjei
end do a_aibjbjdjei
end do j_aibjbjdjei
end do b_aibjbjdjei
end do d_aibjbjdjei
end do e_aibjbjdjei
!
! Elementary loop  20
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
b_aibjbjdiej: do b = n0bc, n1bc
if (b == d .or. b == e) cycle b_aibjbjdiej
j_aibjbjdiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdiej: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbjdiej
i0 = max(j + 1, n0il)
i_aibjbjdiej: do i = i0, n1il
if (i == j) cycle i_aibjbjdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdiej(a, j, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdiej
end do a_aibjbjdiej
end do j_aibjbjdiej
end do b_aibjbjdiej
end do d_aibjbjdiej
end do e_aibjbjdiej
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, m
! Equalities: c == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
e_aibibidiem: do e = n0e, n1e
m_aibibidiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d0 = max(e + 1, n0d)
d_aibibidiem: do d = d0, n1d
if (d == e) cycle d_aibibidiem
b_aibibidiem: do b = n0bc, n1bc
if (b == d .or. b == e) cycle b_aibibidiem
a0 = max(b + 1, n0a)
a_aibibidiem: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibibidiem
i0 = max(m + 1, n0ijkl)
i_aibibidiem: do i = i0, n1ijkl
if (i == m) cycle i_aibibidiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibidiem(a, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibidiem
end do a_aibibidiem
end do b_aibibidiem
end do d_aibibidiem
end do m_aibibidiem
end do e_aibibidiem
!
! Elementary loop  22
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
a_aiajcjalei: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajcjalei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajcjalei: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajcjalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcjalei(a, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcjalei
end do a_aiajcjalei
end do j_aiajcjalei
end do c_aiajcjalei
end do l_aiajcjalei
end do e_aiajcjalei
!
! Elementary loop  23
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
j1 = min(l - 1, n1jm)
j_aiajcialej: do j = n0jm, j1
if (j == l) cycle j_aiajcialej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abd)
a_aiajcialej: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajcialej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcialej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcialej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcialej(a, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcialej
end do a_aiajcialej
end do j_aiajcialej
end do c_aiajcialej
end do l_aiajcialej
end do e_aiajcialej
!
! Elementary loop  24
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
a_aiajcjaiem: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajcjaiem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aiajcjaiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajcjaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcjaiem(a, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcjaiem
end do a_aiajcjaiem
end do j_aiajcjaiem
end do c_aiajcjaiem
end do m_aiajcjaiem
end do e_aiajcjaiem
!
! Elementary loop  25
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
j0 = max(m + 1, n0jl)
j_aiajciajem: do j = j0, n1jl
if (j == m) cycle j_aiajciajem
a0 = max(e + 1, n0abd)
a_aiajciajem: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajciajem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajciajem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aiajciajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajciajem(a, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajciajem
end do a_aiajciajem
end do j_aiajciajem
end do c_aiajciajem
end do m_aiajciajem
end do e_aiajciajem
!
! Elementary loop  26
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
i1 = min(l - 1, n1ijm)
i_aibiakblei: do i = n0ijm, i1
if (i == k .or. i == l) cycle i_aibiakblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiakblei(i, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiakblei
end do a_aibiakblei
end do b_aibiakblei
end do k_aibiakblei
end do l_aibiakblei
end do e_aibiakblei
!
! Elementary loop  27
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
i1 = min(l - 1, n1ikm)
i_aibjaiblei: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjaiblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaiblei(i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaiblei
end do a_aibjaiblei
end do j_aibjaiblei
end do b_aibjaiblei
end do l_aibjaiblei
end do e_aibjaiblei
!
! Elementary loop  28
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
i1 = min(j - 1, n1im)
i_aibjakbjei: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjakbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakbjei(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakbjei
end do a_aibjakbjei
end do j_aibjakbjei
end do b_aibjakbjei
end do k_aibjakbjei
end do e_aibjakbjei
!
! Elementary loop  29
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
j1 = min(l - 1, n1jkm)
j_aibjajblej: do j = n0jkm, j1
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajblej(i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajblej
end do a_aibjajblej
end do j_aibjajblej
end do b_aibjajblej
end do l_aibjajblej
end do e_aibjajblej
!
! Elementary loop  30
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
i0 = max(j + 1, n0il)
i_aibjakbiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakbiej(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakbiej
end do a_aibjakbiej
end do j_aibjakbiej
end do b_aibjakbiej
end do k_aibjakbiej
end do e_aibjakbiej
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
end subroutine ccjac_23_tripletm_dav_part3
end module ccjac_block_23_tripletm_dav_part3
