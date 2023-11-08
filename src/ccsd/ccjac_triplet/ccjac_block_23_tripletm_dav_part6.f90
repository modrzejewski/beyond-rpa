module ccjac_block_23_tripletm_dav_part6
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
subroutine ccjac_23_tripletm_dav_part6(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, i0, i1, j0, j1
integer :: n0abcd, n0abce, n0ad, n0ae, n0bcd
integer :: n0ijkl, n0ijkm, n0ik, n0ikl, n0ikm
integer :: n0il, n0im, n0jk, n0jkl, n0jkm
integer :: n0jl, n0jm, n0kl, n0km
integer :: n1abcd, n1abce, n1ad, n1ae, n1bcd
integer :: n1ijkl, n1ijkm, n1ik, n1ikl, n1ikm
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
n0abcd = max(n0a, n0b, n0c, n0d)
n0abce = max(n0a, n0b, n0c, n0e)
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0bcd = max(n0b, n0c, n0d)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijkm = max(n0i, n0j, n0k, n0m)
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
n1abcd = min(n1a, n1b, n1c, n1d)
n1abce = min(n1a, n1b, n1c, n1e)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bcd = min(n1b, n1c, n1d)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1ijkm = min(n1i, n1j, n1k, n1m)
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
a_aibicialei: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibicialei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ijkm)
i_aibicialei: do i = n0ijkm, i1
if (i == l) cycle i_aibicialei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibicialei(b, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibicialei
end do a_aibicialei
end do b_aibicialei
end do c_aibicialei
end do l_aibicialei
end do e_aibicialei
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjcjajei: do e = n0e, n1e
c_aibjcjajei: do c = n0c, n1c
if (c == e) cycle c_aibjcjajei
b_aibjcjajei: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjcjajei
j_aibjcjajei: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjcjajei: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjcjajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjcjajei: do i = n0im, i1
if (i == j) cycle i_aibjcjajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjajei(b, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjajei
end do a_aibjcjajei
end do j_aibjcjajei
end do b_aibjcjajei
end do c_aibjcjajei
end do e_aibjcjajei
!
! Elementary loop  3
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
a_aibjcjaiej: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjcjaiej
i0 = max(j + 1, n0il)
i_aibjcjaiej: do i = i0, n1il
if (i == j) cycle i_aibjcjaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjaiej(b, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjaiej
end do a_aibjcjaiej
end do j_aibjcjaiej
end do b_aibjcjaiej
end do c_aibjcjaiej
end do e_aibjcjaiej
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, m
! Equalities: d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
e_aibiciaiem: do e = n0e, n1e
m_aibiciaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aibiciaiem: do c = n0c, n1c
if (c == e) cycle c_aibiciaiem
b_aibiciaiem: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibiciaiem
a0 = max(b + 1, e + 1, n0ad)
a_aibiciaiem: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibiciaiem
i0 = max(m + 1, n0ijkl)
i_aibiciaiem: do i = i0, n1ijkl
if (i == m) cycle i_aibiciaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiciaiem(b, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiciaiem
end do a_aibiciaiem
end do b_aibiciaiem
end do c_aibiciaiem
end do m_aibiciaiem
end do e_aibiciaiem
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, l
! Equalities: e == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibicidlai: do d = n0d, n1d
l_aibicidlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aibicidlai: do c = n0c, n1c
if (c == d) cycle c_aibicidlai
b_aibicidlai: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibicidlai
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibicidlai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibicidlai
i1 = min(l - 1, n1ijkm)
i_aibicidlai: do i = n0ijkm, i1
if (i == l) cycle i_aibicidlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibicidlai(b, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibicidlai
end do a_aibicidlai
end do b_aibicidlai
end do c_aibicidlai
end do l_aibicidlai
end do d_aibicidlai
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjcjdjai: do d = n0d, n1d
c_aibjcjdjai: do c = n0c, n1c
if (c == d) cycle c_aibjcjdjai
b_aibjcjdjai: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdjai
j_aibjcjdjai: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcjdjai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdjai
i1 = min(j - 1, n1im)
i_aibjcjdjai: do i = n0im, i1
if (i == j) cycle i_aibjcjdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjdjai(b, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjdjai
end do a_aibjcjdjai
end do j_aibjcjdjai
end do b_aibjcjdjai
end do c_aibjcjdjai
end do d_aibjcjdjai
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdiaj: do d = n0d, n1d
c_aibjcjdiaj: do c = n0c, n1c
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
i0 = max(j + 1, n0il)
i_aibjcjdiaj: do i = i0, n1il
if (i == j) cycle i_aibjcjdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjdiaj(b, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjdiaj
end do a_aibjcjdiaj
end do j_aibjcjdiaj
end do b_aibjcjdiaj
end do c_aibjcjdiaj
end do d_aibjcjdiaj
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, m
! Equalities: e == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibicidiam: do m = n0m, n1m
d_aibicidiam: do d = n0d, n1d
c_aibicidiam: do c = n0c, n1c
if (c == d) cycle c_aibicidiam
b_aibicidiam: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibicidiam
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibicidiam: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibicidiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0ijkl)
i_aibicidiam: do i = i0, n1ijkl
if (i == m) cycle i_aibicidiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibicidiam(b, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibicidiam
end do a_aibicidiam
end do b_aibicidiam
end do c_aibicidiam
end do d_aibicidiam
end do m_aibicidiam
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i, m == i
! No equalities independent of the above can hold.
!
e_aiajaialei: do e = n0e, n1e
l_aiajaialei: do l = n0l, n1l
j_aiajaialei: do j = n0j, n1j
if (j == l) cycle j_aiajaialei
a0 = max(e + 1, n0abcd)
a_aiajaialei: do a = a0, n1abcd
if (a == e) cycle a_aiajaialei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ikm)
i1 = min(l - 1, n1ikm)
i_aiajaialei: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajaialei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaialei(i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaialei
end do a_aiajaialei
end do j_aiajaialei
end do l_aiajaialei
end do e_aiajaialei
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, k, j
! Equalities: b == a, c == a, d == a, m == i, l == k
! No equalities independent of the above can hold.
!
e_aiajakakei: do e = n0e, n1e
k_aiajakakei: do k = n0kl, n1kl
j_aiajakakei: do j = n0j, n1j
if (j == k) cycle j_aiajakakei
a0 = max(e + 1, n0abcd)
a_aiajakakei: do a = a0, n1abcd
if (a == e) cycle a_aiajakakei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(k - 1, n1im)
i_aiajakakei: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajakakei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (e - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakakei(j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakakei
end do a_aiajakakei
end do j_aiajakakei
end do k_aiajakakei
end do e_aiajakakei
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aiajajalei: do e = n0e, n1e
l_aiajajalei: do l = n0l, n1l
j_aiajajalei: do j = n0jk, n1jk
if (j == l) cycle j_aiajajalei
a0 = max(e + 1, n0abcd)
a_aiajajalei: do a = a0, n1abcd
if (a == e) cycle a_aiajajalei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajajalei: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajajalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajalei(nocc, a, i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajalei
end do a_aiajajalei
end do j_aiajajalei
end do l_aiajajalei
end do e_aiajajalei
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, l
! Equalities: b == a, c == a, d == a, k == j, m == j
! No equalities independent of the above can hold.
!
e_aiajajalej: do e = n0e, n1e
l_aiajajalej: do l = n0l, n1l
j1 = min(l - 1, n1jkm)
j_aiajajalej: do j = n0jkm, j1
if (j == l) cycle j_aiajajalej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiajajalej: do a = a0, n1abcd
if (a == e) cycle a_aiajajalej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajajalej: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajajalej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajalej(i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajalej
end do a_aiajajalej
end do j_aiajajalej
end do l_aiajajalej
end do e_aiajajalej
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajakaiej: do e = n0e, n1e
k_aiajakaiej: do k = n0k, n1k
j_aiajakaiej: do j = n0jm, n1jm
if (j == k) cycle j_aiajakaiej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiajakaiej: do a = a0, n1abcd
if (a == e) cycle a_aiajakaiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajakaiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajakaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakaiej(i, j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakaiej
end do a_aiajakaiej
end do j_aiajakaiej
end do k_aiajakaiej
end do e_aiajakaiej
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aiajaialej: do e = n0e, n1e
l_aiajaialej: do l = n0l, n1l
j1 = min(l - 1, n1jm)
j_aiajaialej: do j = n0jm, j1
if (j == l) cycle j_aiajaialej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiajaialej: do a = a0, n1abcd
if (a == e) cycle a_aiajaialej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaialej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaialej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaialej(nocc, a, i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaialej
end do a_aiajaialej
end do j_aiajaialej
end do l_aiajaialej
end do e_aiajaialej
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, k, i
! Equalities: b == a, c == a, d == a, m == j, l == k
! No equalities independent of the above can hold.
!
e_aiajakakej: do e = n0e, n1e
k_aiajakakej: do k = n0kl, n1kl
j1 = min(k - 1, n1jm)
j_aiajakakej: do j = n0jm, j1
if (j == k) cycle j_aiajakakej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiajakakej: do a = a0, n1abcd
if (a == e) cycle a_aiajakakej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakakej: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajakakej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (e - nvirt0) * (k - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakakej(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakakej
end do a_aiajakakej
end do j_aiajakakej
end do k_aiajakakej
end do e_aiajakakej
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, k, j
! Equalities: b == a, c == a, d == a, l == i, m == k
! No equalities independent of the above can hold.
!
e_aiajakaiek: do e = n0e, n1e
k_aiajakaiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j_aiajakaiek: do j = n0j, n1j
if (j == k) cycle j_aiajakaiek
a0 = max(e + 1, n0abcd)
a_aiajakaiek: do a = a0, n1abcd
if (a == e) cycle a_aiajakaiek
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, k + 1, n0il)
i_aiajakaiek: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajakaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakaiek(j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakaiek
end do a_aiajakaiek
end do j_aiajakaiek
end do k_aiajakaiek
end do e_aiajakaiek
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, k, i
! Equalities: b == a, c == a, d == a, l == j, m == k
! No equalities independent of the above can hold.
!
e_aiajakajek: do e = n0e, n1e
k_aiajakajek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajakajek: do j = j0, n1jl
if (j == k) cycle j_aiajakajek
a0 = max(e + 1, n0abcd)
a_aiajakajek: do a = a0, n1abcd
if (a == e) cycle a_aiajakajek
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakajek: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajakajek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakajek(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakajek
end do a_aiajakajek
end do j_aiajakajek
end do k_aiajakajek
end do e_aiajakajek
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, d == a, k == i, l == i
! No equalities independent of the above can hold.
!
e_aiajaiaiem: do e = n0e, n1e
m_aiajaiaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
j_aiajaiaiem: do j = n0j, n1j
if (j == m) cycle j_aiajaiaiem
a0 = max(e + 1, n0abcd)
a_aiajaiaiem: do a = a0, n1abcd
if (a == e) cycle a_aiajaiaiem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, m + 1, n0ikl)
i_aiajaiaiem: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aiajaiaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaiaiem(i, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaiaiem
end do a_aiajaiaiem
end do j_aiajaiaiem
end do m_aiajaiaiem
end do e_aiajaiaiem
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
e_aiajajaiem: do e = n0e, n1e
m_aiajajaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
j_aiajajaiem: do j = n0jk, n1jk
if (j == m) cycle j_aiajajaiem
a0 = max(e + 1, n0abcd)
a_aiajajaiem: do a = a0, n1abcd
if (a == e) cycle a_aiajajaiem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aiajajaiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajajaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajaiem(nocc, a, i, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajaiem
end do a_aiajajaiem
end do j_aiajajaiem
end do m_aiajajaiem
end do e_aiajajaiem
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aiajaiajem: do e = n0e, n1e
m_aiajaiajem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aiajaiajem: do j = j0, n1jl
if (j == m) cycle j_aiajaiajem
a0 = max(e + 1, n0abcd)
a_aiajaiajem: do a = a0, n1abcd
if (a == e) cycle a_aiajaiajem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaiajem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aiajaiajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaiajem(nocc, a, i, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaiajem
end do a_aiajaiajem
end do j_aiajaiajem
end do m_aiajaiajem
end do e_aiajaiajem
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, m
! Equalities: b == a, c == a, d == a, k == j, l == j
! No equalities independent of the above can hold.
!
e_aiajajajem: do e = n0e, n1e
m_aiajajajem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jkl)
j_aiajajajem: do j = j0, n1jkl
if (j == m) cycle j_aiajajajem
a0 = max(e + 1, n0abcd)
a_aiajajajem: do a = a0, n1abcd
if (a == e) cycle a_aiajajajem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajajajem: do i = i0, n1i
if (i == j .or. i == m) cycle i_aiajajajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajajem(i, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajajem
end do a_aiajajajem
end do j_aiajajajem
end do m_aiajajajem
end do e_aiajajajem
!
! Elementary loop  22
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
i1 = min(l - 1, n1ijkm)
i_aibibiblei: do i = n0ijkm, i1
if (i == l) cycle i_aibibiblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibiblei(a, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibiblei
end do a_aibibiblei
end do b_aibibiblei
end do l_aibibiblei
end do e_aibibiblei
!
! Elementary loop  23
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
i1 = min(j - 1, n1ikm)
i_aibjbibjei: do i = n0ikm, i1
if (i == j) cycle i_aibjbibjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbibjei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbibjei
end do a_aibjbibjei
end do j_aibjbibjei
end do b_aibjbibjei
end do e_aibjbibjei
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjbjbjei: do e = n0e, n1e
b0 = max(e + 1, n0bcd)
b_aibjbjbjei: do b = b0, n1bcd
if (b == e) cycle b_aibjbjbjei
j_aibjbjbjei: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjbjei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbjbjei
i1 = min(j - 1, n1im)
i_aibjbjbjei: do i = n0im, i1
if (i == j) cycle i_aibjbjbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjbjei(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjbjei
end do a_aibjbjbjei
end do j_aibjbjbjei
end do b_aibjbjbjei
end do e_aibjbjbjei
!
! Elementary loop  25
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
i0 = max(j + 1, n0il)
i_aibjbjbiej: do i = i0, n1il
if (i == j) cycle i_aibjbjbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjbiej(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjbiej
end do a_aibjbjbiej
end do j_aibjbjbiej
end do b_aibjbjbiej
end do e_aibjbjbiej
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbibiej: do e = n0e, n1e
b0 = max(e + 1, n0bcd)
b_aibjbibiej: do b = b0, n1bcd
if (b == e) cycle b_aibjbibiej
j_aibjbibiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibiej: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbibiej
i0 = max(j + 1, n0ikl)
i_aibjbibiej: do i = i0, n1ikl
if (i == j) cycle i_aibjbibiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbibiej(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbibiej
end do a_aibjbibiej
end do j_aibjbibiej
end do b_aibjbibiej
end do e_aibjbibiej
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, m
! Equalities: c == b, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
e_aibibibiem: do e = n0e, n1e
m_aibibibiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(e + 1, n0bcd)
b_aibibibiem: do b = b0, n1bcd
if (b == e) cycle b_aibibibiem
a0 = max(b + 1, n0a)
a_aibibibiem: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibibibiem
i0 = max(m + 1, n0ijkl)
i_aibibibiem: do i = i0, n1ijkl
if (i == m) cycle i_aibibibiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibibiem(a, b, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibibiem
end do a_aibibibiem
end do b_aibibibiem
end do m_aibibibiem
end do e_aibibibiem
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, e == a, k == i, m == i
! No equalities independent of the above can hold.
!
d_aiajaidlai: do d = n0d, n1d
l_aiajaidlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajaidlai: do j = n0j, n1j
if (j == l) cycle j_aiajaidlai
a1 = min(d - 1, n1abce)
a_aiajaidlai: do a = n0abce, a1
if (a == d) cycle a_aiajaidlai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikm)
i1 = min(l - 1, n1ikm)
i_aiajaidlai: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajaidlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaidlai(i, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaidlai
end do a_aiajaidlai
end do j_aiajaidlai
end do l_aiajaidlai
end do d_aiajaidlai
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, k, j
! Equalities: b == a, c == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
d_aiajakdkai: do d = n0d, n1d
k_aiajakdkai: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
j_aiajakdkai: do j = n0j, n1j
if (j == k) cycle j_aiajakdkai
a1 = min(d - 1, n1abce)
a_aiajakdkai: do a = n0abce, a1
if (a == d) cycle a_aiajakdkai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(k - 1, n1im)
i_aiajakdkai: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajakdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakdkai(j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakdkai
end do a_aiajakdkai
end do j_aiajakdkai
end do k_aiajakdkai
end do d_aiajakdkai
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, e == a, m == i, k == j
! No equalities independent of the above can hold.
!
d_aiajajdlai: do d = n0d, n1d
l_aiajajdlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajajdlai: do j = n0jk, n1jk
if (j == l) cycle j_aiajajdlai
a1 = min(d - 1, n1abce)
a_aiajajdlai: do a = n0abce, a1
if (a == d) cycle a_aiajajdlai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajajdlai: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajajdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajdlai(nocc, a, i, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajdlai
end do a_aiajajdlai
end do j_aiajajdlai
end do l_aiajajdlai
end do d_aiajajdlai
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
end subroutine ccjac_23_tripletm_dav_part6
end module ccjac_block_23_tripletm_dav_part6
