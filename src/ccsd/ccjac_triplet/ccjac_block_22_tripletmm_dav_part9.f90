module ccjac_block_22_tripletmm_dav_part9
use eom_ccsd_22_tripletmm_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2018-12-06 14:17:58 UTC.
!
contains
subroutine ccjac_22_tripletmm_dav_part9(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
 nvirt0, nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, &
 n1k, n0l, n1l, bra0, ket0) 
procedure(dav_sigma_update_diag) :: sigup_diag
procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
real(F64), dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c, &
 n0d, n1d
integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l
integer, intent(in)                                 :: bra0, ket0
!
! Local variables
!
real(F64) :: jac_ibra_iket
integer :: a, b, c, d
integer :: i, j, k, l
integer :: ai, bj, ck, dl
integer :: nn0a, nn0c, nn0i, nn0j, nn1b, nn1i, nn1j
integer :: a0, b1, c0, i0, i1, j0, j1
integer :: n0abc, n0abcd, n0bd, n0ij, n0ijkl
integer :: n0ik, n0ikl, n0il, n0jk, n0jkl
integer :: n0jl, n0kl
integer :: n1abc, n1abcd, n1bd, n1ij, n1ijkl
integer :: n1ik, n1ikl, n1il, n1jk, n1jkl
integer :: n1jl, n1kl
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket
integer :: braoffset, ketoffset
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
n0abc = max(n0a, n0b, n0c)
n0abcd = max(n0a, n0b, n0c, n0d)
n0bd = max(n0b, n0d)
n0ij = max(n0i, n0j)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1abc = min(n1a, n1b, n1c)
n1abcd = min(n1a, n1b, n1c, n1d)
n1bd = min(n1b, n1d)
n1ij = min(n1i, n1j)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jkl = min(n1j, n1k, n1l)
n1jl = min(n1j, n1l)
n1kl = min(n1k, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k
! Equalities: d == b, j == i, l == k
! No equalities independent of the above can hold.
!
c_aibickbk_aick: do c = n0c, n1c
k_aibickbk_aick: do k = n0kl, n1kl
a_aibickbk_aick: do a = n0a, n1a
if (a == c) cycle a_aibickbk_aick
i_aibickbk_aick: do i = n0ij, n1ij
if (i == k) cycle i_aibickbk_aick
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibickbk_aick(t2, &
 nocc, nactive, a, i, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibickbk_aick: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibickbk_aick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibickbk_aick
end do i_aibickbk_aick
end do a_aibickbk_aick
end do k_aibickbk_aick
end do c_aibickbk_aick
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi_abc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcjbi_abc: do b = n0bd, b1
if (b == c) cycle b_aibjcjbi_abc
a0 = max(b + 1, n0a)
a_aibjcjbi_abc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbi_abc
if ((n1jk .ge. n0jk).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjbi_abc(t2, &
 nocc, nactive, a, b, c)
j_aibjcjbi_abc: do j = n0jk, n1jk
i_aibjcjbi_abc: do i = n0il, n1il
if (i == j) cycle i_aibjcjbi_abc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbi_abc
end do j_aibjcjbi_abc
end do a_aibjcjbi_abc
end do b_aibjcjbi_abc
end do c_aibjcjbi_abc
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi_abjc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcjbi_abjc: do b = n0bd, b1
if (b == c) cycle b_aibjcjbi_abjc
j_aibjcjbi_abjc: do j = n0jk, n1jk
a0 = max(b + 1, n0a)
a_aibjcjbi_abjc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbi_abjc
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjbi_abjc(t2, &
 nocc, nactive, a, b, j, c)
i_aibjcjbi_abjc: do i = n0il, n1il
if (i == j) cycle i_aibjcjbi_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbi_abjc
end do a_aibjcjbi_abjc
end do j_aibjcjbi_abjc
end do b_aibjcjbi_abjc
end do c_aibjcjbi_abjc
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi_aibc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcjbi_aibc: do b = n0bd, b1
if (b == c) cycle b_aibjcjbi_aibc
a0 = max(b + 1, n0a)
a_aibjcjbi_aibc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbi_aibc
i_aibjcjbi_aibc: do i = n0il, n1il
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjbi_aibc(t2, &
 nocc, nactive, a, i, b, c)
j_aibjcjbi_aibc: do j = n0jk, n1jk
if (j == i) cycle j_aibjcjbi_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjbi_aibc
end do i_aibjcjbi_aibc
end do a_aibjcjbi_aibc
end do b_aibjcjbi_aibc
end do c_aibjcjbi_aibc
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi_aijc: do c = n0c, n1c
j_aibjcjbi_aijc: do j = n0jk, n1jk
a_aibjcjbi_aijc: do a = n0a, n1a
if (a == c) cycle a_aibjcjbi_aijc
i_aibjcjbi_aijc: do i = n0il, n1il
if (i == j) cycle i_aibjcjbi_aijc
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjbi_aijc(t2, &
 nocc, nactive, a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbi_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbi_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcjbi_aijc
end do i_aibjcjbi_aijc
end do a_aibjcjbi_aijc
end do j_aibjcjbi_aijc
end do c_aibjcjbi_aijc
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i
! Equalities: d == b, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjbj_aijc: do c = n0c, n1c
j_aibjcjbj_aijc: do j = n0jkl, n1jkl
a_aibjcjbj_aijc: do a = n0a, n1a
if (a == c) cycle a_aibjcjbj_aijc
i_aibjcjbj_aijc: do i = n0i, n1i
if (i == j) cycle i_aibjcjbj_aijc
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjbj_aijc(t2, &
 nocc, nactive, a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbj_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbj_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcjbj_aijc
end do i_aibjcjbj_aijc
end do a_aibjcjbj_aijc
end do j_aibjcjbj_aijc
end do c_aibjcjbj_aijc
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i
! Equalities: d == b, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjbj_aibjc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcjbj_aibjc: do b = n0bd, b1
if (b == c) cycle b_aibjcjbj_aibjc
j_aibjcjbj_aibjc: do j = n0jkl, n1jkl
a0 = max(b + 1, n0a)
a_aibjcjbj_aibjc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbj_aibjc
i_aibjcjbj_aibjc: do i = n0i, n1i
if (i == j) cycle i_aibjcjbj_aibjc
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjbj_aibjc(t2, nocc, nactive, a, &
 i, b, j, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbj_aibjc
end do a_aibjcjbj_aibjc
end do j_aibjcjbj_aibjc
end do b_aibjcjbj_aibjc
end do c_aibjcjbj_aibjc
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i
! Equalities: j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibicidi_abcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibicidi_abcd: do c = c0, n1c
if (c == d) cycle c_aibicidi_abcd
b_aibicidi_abcd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibicidi_abcd
a0 = max(b + 1, n0a)
a_aibicidi_abcd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibicidi_abcd
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibicidi_abcd(t2, &
 nocc, nactive, a, b, c, d)
i_aibicidi_abcd: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicidi_abcd
end do a_aibicidi_abcd
end do b_aibicidi_abcd
end do c_aibicidi_abcd
end do d_aibicidi_abcd
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i
! Equalities: j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibicidi_aibcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibicidi_aibcd: do c = c0, n1c
if (c == d) cycle c_aibicidi_aibcd
b_aibicidi_aibcd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibicidi_aibcd
a0 = max(b + 1, n0a)
a_aibicidi_aibcd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibicidi_aibcd
i_aibicidi_aibcd: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibicidi_aibcd(t2, nocc, nactive, a, &
 i, b, c, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicidi_aibcd
end do a_aibicidi_aibcd
end do b_aibicidi_aibcd
end do c_aibicidi_aibcd
end do d_aibicidi_aibcd
!
! Elementary loop  10
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajaial_jl: do l = n0l, n1l
j_aiajaial_jl: do j = n0j, n1j
if (j == l) cycle j_aiajaial_jl
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i).and. (n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaial_jl(t2, &
 nocc, nactive, j, l)
a_aiajaial_jl: do a = n0abcd, n1abcd
i0 = max(j + 1, l + 1, n0ik)
i_aiajaial_jl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaial_jl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaial_jl
end do a_aiajaial_jl
end do j_aiajaial_jl
end do l_aiajaial_jl
!
! Elementary loop  11
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajaial_ajl: do l = n0l, n1l
j_aiajaial_ajl: do j = n0j, n1j
if (j == l) cycle j_aiajaial_ajl
a_aiajaial_ajl: do a = n0abcd, n1abcd
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaial_ajl(t2, &
 nocc, nactive, a, j, l)
i0 = max(j + 1, l + 1, n0ik)
i_aiajaial_ajl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaial_ajl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaial_ajl
end do a_aiajaial_ajl
end do j_aiajaial_ajl
end do l_aiajaial_ajl
!
! Elementary loop  12
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajaial_ijl: do l = n0l, n1l
j_aiajaial_ijl: do j = n0j, n1j
if (j == l) cycle j_aiajaial_ijl
i0 = max(j + 1, l + 1, n0ik)
i_aiajaial_ijl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaial_ijl
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaial_ijl(t2, &
 nocc, nactive, i, j, l)
a_aiajaial_ijl: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aiajaial_ijl
end do i_aiajaial_ijl
end do j_aiajaial_ijl
end do l_aiajaial_ijl
!
! Elementary loop  13
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajaial_aijl: do l = n0l, n1l
j_aiajaial_aijl: do j = n0j, n1j
if (j == l) cycle j_aiajaial_aijl
a_aiajaial_aijl: do a = n0abcd, n1abcd
i0 = max(j + 1, l + 1, n0ik)
i_aiajaial_aijl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaial_aijl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaial_aijl(t2, nocc, nactive, a, i, &
 j, l)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaial_aijl
end do a_aiajaial_aijl
end do j_aiajaial_aijl
end do l_aiajaial_aijl
!
! Elementary loop  14
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, d == a, l == i
! No equalities independent of the above can hold.
!
k_aiajakai_jk: do k = n0k, n1k
j_aiajakai_jk: do j = n0j, n1j
if (j == k) cycle j_aiajakai_jk
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i).and. (n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajakai_jk(t2, &
 nocc, nactive, j, k)
a_aiajakai_jk: do a = n0abcd, n1abcd
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aiajakai_jk: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajakai_jk
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakai_jk
end do a_aiajakai_jk
end do j_aiajakai_jk
end do k_aiajakai_jk
!
! Elementary loop  15
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, d == a, l == i
! No equalities independent of the above can hold.
!
k_aiajakai_ijk: do k = n0k, n1k
j_aiajakai_ijk: do j = n0j, n1j
if (j == k) cycle j_aiajakai_ijk
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aiajakai_ijk: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajakai_ijk
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajakai_ijk(t2, &
 nocc, nactive, i, j, k)
a_aiajakai_ijk: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aiajakai_ijk
end do i_aiajakai_ijk
end do j_aiajakai_ijk
end do k_aiajakai_ijk
!
! Elementary loop  16
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, d == a, l == i
! No equalities independent of the above can hold.
!
k_aiajakai_ajk: do k = n0k, n1k
j_aiajakai_ajk: do j = n0j, n1j
if (j == k) cycle j_aiajakai_ajk
a_aiajakai_ajk: do a = n0abcd, n1abcd
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajakai_ajk(t2, &
 nocc, nactive, a, j, k)
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aiajakai_ajk: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajakai_ajk
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakai_ajk
end do a_aiajakai_ajk
end do j_aiajakai_ajk
end do k_aiajakai_ajk
!
! Elementary loop  17
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, d == a, l == i
! No equalities independent of the above can hold.
!
k_aiajakai_aijk: do k = n0k, n1k
j_aiajakai_aijk: do j = n0j, n1j
if (j == k) cycle j_aiajakai_aijk
a_aiajakai_aijk: do a = n0abcd, n1abcd
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aiajakai_aijk: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajakai_aijk
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajakai_aijk(t2, nocc, nactive, a, i, &
 j, k)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakai_aijk
end do a_aiajakai_aijk
end do j_aiajakai_aijk
end do k_aiajakai_aijk
!
! Elementary loop  18
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i, l
! Equalities: b == a, c == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aiajajal_il: do l = n0l, n1l
i_aiajajal_il: do i = n0i, n1i
if (i == l) cycle i_aiajajal_il
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j).and. (n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajajal_il(t2, &
 nocc, nactive, i, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aiajajal_il: do j = j0, j1
if (j == i .or. j == l) cycle j_aiajajal_il
a_aiajajal_il: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aiajajal_il
end do j_aiajajal_il
end do i_aiajajal_il
end do l_aiajajal_il
!
! Elementary loop  19
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i, l
! Equalities: b == a, c == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aiajajal_ijl: do l = n0l, n1l
j0 = max(l + 1, n0jk)
j_aiajajal_ijl: do j = j0, n1jk
if (j == l) cycle j_aiajajal_ijl
i0 = max(j + 1, n0i)
i_aiajajal_ijl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajajal_ijl
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajajal_ijl(t2, &
 nocc, nactive, i, j, l)
a_aiajajal_ijl: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aiajajal_ijl
end do i_aiajajal_ijl
end do j_aiajajal_ijl
end do l_aiajajal_ijl
!
! Elementary loop  20
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i, l
! Equalities: b == a, c == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aiajajal_ail: do l = n0l, n1l
a_aiajajal_ail: do a = n0abcd, n1abcd
i_aiajajal_ail: do i = n0i, n1i
if (i == l) cycle i_aiajajal_ail
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajajal_ail(t2, &
 nocc, nactive, a, i, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aiajajal_ail: do j = j0, j1
if (j == i .or. j == l) cycle j_aiajajal_ail
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajajal_ail
end do i_aiajajal_ail
end do a_aiajajal_ail
end do l_aiajajal_ail
!
! Elementary loop  21
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i, l
! Equalities: b == a, c == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aiajajal_aijl: do l = n0l, n1l
j0 = max(l + 1, n0jk)
j_aiajajal_aijl: do j = j0, n1jk
if (j == l) cycle j_aiajajal_aijl
a_aiajajal_aijl: do a = n0abcd, n1abcd
i0 = max(j + 1, n0i)
i_aiajajal_aijl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajajal_aijl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajajal_aijl(t2, nocc, nactive, a, i, &
 j, l)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajal_aijl
end do a_aiajajal_aijl
end do j_aiajajal_aijl
end do l_aiajajal_aijl
!
! Elementary loop  22
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i, k
! Equalities: b == a, c == a, d == a, l == j
! No equalities independent of the above can hold.
!
k_aiajakaj_ik: do k = n0k, n1k
i_aiajakaj_ik: do i = n0i, n1i
if (i == k) cycle i_aiajakaj_ik
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl).and. (n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajakaj_ik(t2, &
 nocc, nactive, i, k)
j1 = min(i - 1, k - 1, n1jl)
j_aiajakaj_ik: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aiajakaj_ik
a_aiajakaj_ik: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aiajakaj_ik
end do j_aiajakaj_ik
end do i_aiajakaj_ik
end do k_aiajakaj_ik
!
! Elementary loop  23
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i, k
! Equalities: b == a, c == a, d == a, l == j
! No equalities independent of the above can hold.
!
k_aiajakaj_aik: do k = n0k, n1k
a_aiajakaj_aik: do a = n0abcd, n1abcd
i_aiajakaj_aik: do i = n0i, n1i
if (i == k) cycle i_aiajakaj_aik
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajakaj_aik(t2, &
 nocc, nactive, a, i, k)
j1 = min(i - 1, k - 1, n1jl)
j_aiajakaj_aik: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aiajakaj_aik
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajakaj_aik
end do i_aiajakaj_aik
end do a_aiajakaj_aik
end do k_aiajakaj_aik
!
! Elementary loop  24
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i, k
! Equalities: b == a, c == a, d == a, l == j
! No equalities independent of the above can hold.
!
k_aiajakaj_ijk: do k = n0k, n1k
j1 = min(k - 1, n1jl)
j_aiajakaj_ijk: do j = n0jl, j1
if (j == k) cycle j_aiajakaj_ijk
i0 = max(j + 1, n0i)
i_aiajakaj_ijk: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajakaj_ijk
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajakaj_ijk(t2, &
 nocc, nactive, i, j, k)
a_aiajakaj_ijk: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aiajakaj_ijk
end do i_aiajakaj_ijk
end do j_aiajakaj_ijk
end do k_aiajakaj_ijk
!
! Elementary loop  25
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i, k
! Equalities: b == a, c == a, d == a, l == j
! No equalities independent of the above can hold.
!
k_aiajakaj_aijk: do k = n0k, n1k
j1 = min(k - 1, n1jl)
j_aiajakaj_aijk: do j = n0jl, j1
if (j == k) cycle j_aiajakaj_aijk
a_aiajakaj_aijk: do a = n0abcd, n1abcd
i0 = max(j + 1, n0i)
i_aiajakaj_aijk: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajakaj_aijk
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajakaj_aijk(t2, nocc, nactive, a, i, &
 j, k)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakaj_aijk
end do a_aiajakaj_aijk
end do j_aiajakaj_aijk
end do k_aiajakaj_aijk
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aiajaidi_aijd: do d = n0d, n1d
j_aiajaidi_aijd: do j = n0j, n1j
a0 = max(d + 1, n0abc)
a_aiajaidi_aijd: do a = a0, n1abc
if (a == d) cycle a_aiajaidi_aijd
i0 = max(j + 1, n0ikl)
i_aiajaidi_aijd: do i = i0, n1ikl
if (i == j) cycle i_aiajaidi_aijd
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaidi_aijd(t2, nocc, nactive, a, i, &
 j, d)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidi_aijd
end do a_aiajaidi_aijd
end do j_aiajaidi_aijd
end do d_aiajaidi_aijd
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajaidj_ad: do d = n0d, n1d
a0 = max(d + 1, n0abc)
a_aiajaidj_ad: do a = a0, n1abc
if (a == d) cycle a_aiajaidj_ad
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaidj_ad(t2, &
 nocc, nactive, a, d)
j_aiajaidj_ad: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aiajaidj_ad: do i = i0, n1ik
if (i == j) cycle i_aiajaidj_ad
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidj_ad
end do j_aiajaidj_ad
end do a_aiajaidj_ad
end do d_aiajaidj_ad
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajaidj_aid: do d = n0d, n1d
a0 = max(d + 1, n0abc)
a_aiajaidj_aid: do a = a0, n1abc
if (a == d) cycle a_aiajaidj_aid
i_aiajaidj_aid: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaidj_aid(t2, &
 nocc, nactive, a, i, d)
j1 = min(i - 1, n1jl)
j_aiajaidj_aid: do j = n0jl, j1
if (j == i) cycle j_aiajaidj_aid
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajaidj_aid
end do i_aiajaidj_aid
end do a_aiajaidj_aid
end do d_aiajaidj_aid
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajaidj_ajd: do d = n0d, n1d
j_aiajaidj_ajd: do j = n0jl, n1jl
a0 = max(d + 1, n0abc)
a_aiajaidj_ajd: do a = a0, n1abc
if (a == d) cycle a_aiajaidj_ajd
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaidj_ajd(t2, &
 nocc, nactive, a, j, d)
i0 = max(j + 1, n0ik)
i_aiajaidj_ajd: do i = i0, n1ik
if (i == j) cycle i_aiajaidj_ajd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidj_ajd
end do a_aiajaidj_ajd
end do j_aiajaidj_ajd
end do d_aiajaidj_ajd
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajaidj_aijd: do d = n0d, n1d
j_aiajaidj_aijd: do j = n0jl, n1jl
a0 = max(d + 1, n0abc)
a_aiajaidj_aijd: do a = a0, n1abc
if (a == d) cycle a_aiajaidj_aijd
i0 = max(j + 1, n0ik)
i_aiajaidj_aijd: do i = i0, n1ik
if (i == j) cycle i_aiajaidj_aijd
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaidj_aijd(t2, nocc, nactive, a, i, &
 j, d)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidj_aijd
end do a_aiajaidj_aijd
end do j_aiajaidj_aijd
end do d_aiajaidj_aijd
end subroutine ccjac_22_tripletmm_dav_part9
end module ccjac_block_22_tripletmm_dav_part9
