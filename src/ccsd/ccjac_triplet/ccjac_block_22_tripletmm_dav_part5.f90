module ccjac_block_22_tripletmm_dav_part5
use eom_ccsd_22_tripletmm_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2018-12-06 14:17:57 UTC.
!
contains
subroutine ccjac_22_tripletmm_dav_part5(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: nn0a, nn0i, nn1a, nn1b
integer :: a0, a1, b1, i0
integer :: n0abd, n0ac, n0bd, n0ij, n0ijk
integer :: n0ijl, n0ik, n0ikl, n0il, n0jk
integer :: n0jkl, n0jl, n0kl
integer :: n1abd, n1ac, n1bd, n1ij, n1ijk
integer :: n1ijl, n1ik, n1ikl, n1il, n1jk
integer :: n1jkl, n1jl, n1kl
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
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
n0bd = max(n0b, n0d)
n0ij = max(n0i, n0j)
n0ijk = max(n0i, n0j, n0k)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
n1bd = min(n1b, n1d)
n1ij = min(n1i, n1j)
n1ijk = min(n1i, n1j, n1k)
n1ijl = min(n1i, n1j, n1l)
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
! Free virtual indices: a, b
! Free occupied indices: k, i, j
! Equalities: c == a, d == b, l == k
! No equalities independent of the above can hold.
!
k_aibjakbk_ibjk: do k = n0kl, n1kl
b_aibjakbk_ibjk: do b = n0bd, n1bd
j_aibjakbk_ibjk: do j = n0j, n1j
if (j == k) cycle j_aibjakbk_ibjk
i_aibjakbk_ibjk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbk_ibjk
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbk_ibjk(t2, &
 nocc, nactive, i, b, j, k)
a0 = max(b + 1, n0ac)
a_aibjakbk_ibjk: do a = a0, n1ac
if (a == b) cycle a_aibjakbk_ibjk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbk_ibjk
end do i_aibjakbk_ibjk
end do j_aibjakbk_ibjk
end do b_aibjakbk_ibjk
end do k_aibjakbk_ibjk
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j
! Equalities: c == a, d == b, l == k
! No equalities independent of the above can hold.
!
k_aibjakbk_aijk: do k = n0kl, n1kl
j_aibjakbk_aijk: do j = n0j, n1j
if (j == k) cycle j_aibjakbk_aijk
a_aibjakbk_aijk: do a = n0ac, n1ac
i_aibjakbk_aijk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbk_aijk
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbk_aijk(t2, &
 nocc, nactive, a, i, j, k)
b1 = min(a - 1, n1bd)
b_aibjakbk_aijk: do b = n0bd, b1
if (b == a) cycle b_aibjakbk_aijk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjakbk_aijk
end do i_aibjakbk_aijk
end do a_aibjakbk_aijk
end do j_aibjakbk_aijk
end do k_aibjakbk_aijk
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_ik: do k = n0k, n1k
i_aibjakbj_ik: do i = n0i, n1i
if (i == k) cycle i_aibjakbj_ik
nn0a = max(n0bd + 1, n0ac)
if ((n1jl .ge. n0jl).and. (n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbj_ik(t2, &
 nocc, nactive, i, k)
b_aibjakbj_ik: do b = n0bd, n1bd
j_aibjakbj_ik: do j = n0jl, n1jl
if (j == i .or. j == k) cycle j_aibjakbj_ik
a0 = max(b + 1, n0ac)
a_aibjakbj_ik: do a = a0, n1ac
if (a == b) cycle a_aibjakbj_ik
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbj_ik
end do j_aibjakbj_ik
end do b_aibjakbj_ik
end do i_aibjakbj_ik
end do k_aibjakbj_ik
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_aik: do k = n0k, n1k
a_aibjakbj_aik: do a = n0ac, n1ac
i_aibjakbj_aik: do i = n0i, n1i
if (i == k) cycle i_aibjakbj_aik
nn1b = min(a - 1, n1bd)
if ((n1jl .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbj_aik(t2, &
 nocc, nactive, a, i, k)
b1 = min(a - 1, n1bd)
b_aibjakbj_aik: do b = n0bd, b1
if (b == a) cycle b_aibjakbj_aik
j_aibjakbj_aik: do j = n0jl, n1jl
if (j == i .or. j == k) cycle j_aibjakbj_aik
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjakbj_aik
end do b_aibjakbj_aik
end do i_aibjakbj_aik
end do a_aibjakbj_aik
end do k_aibjakbj_aik
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_ibk: do k = n0k, n1k
b_aibjakbj_ibk: do b = n0bd, n1bd
i_aibjakbj_ibk: do i = n0i, n1i
if (i == k) cycle i_aibjakbj_ibk
nn0a = max(b + 1, n0ac)
if ((n1jl .ge. n0jl).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbj_ibk(t2, &
 nocc, nactive, i, b, k)
j_aibjakbj_ibk: do j = n0jl, n1jl
if (j == i .or. j == k) cycle j_aibjakbj_ibk
a0 = max(b + 1, n0ac)
a_aibjakbj_ibk: do a = a0, n1ac
if (a == b) cycle a_aibjakbj_ibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbj_ibk
end do j_aibjakbj_ibk
end do i_aibjakbj_ibk
end do b_aibjakbj_ibk
end do k_aibjakbj_ibk
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_ijk: do k = n0k, n1k
j_aibjakbj_ijk: do j = n0jl, n1jl
if (j == k) cycle j_aibjakbj_ijk
i_aibjakbj_ijk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbj_ijk
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbj_ijk(t2, &
 nocc, nactive, i, j, k)
b_aibjakbj_ijk: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjakbj_ijk: do a = a0, n1ac
if (a == b) cycle a_aibjakbj_ijk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbj_ijk
end do b_aibjakbj_ijk
end do i_aibjakbj_ijk
end do j_aibjakbj_ijk
end do k_aibjakbj_ijk
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_ibjk: do k = n0k, n1k
b_aibjakbj_ibjk: do b = n0bd, n1bd
j_aibjakbj_ibjk: do j = n0jl, n1jl
if (j == k) cycle j_aibjakbj_ibjk
i_aibjakbj_ibjk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbj_ibjk
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbj_ibjk(t2, &
 nocc, nactive, i, b, j, k)
a0 = max(b + 1, n0ac)
a_aibjakbj_ibjk: do a = a0, n1ac
if (a == b) cycle a_aibjakbj_ibjk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbj_ibjk
end do i_aibjakbj_ibjk
end do j_aibjakbj_ibjk
end do b_aibjakbj_ibjk
end do k_aibjakbj_ibjk
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_aijk: do k = n0k, n1k
j_aibjakbj_aijk: do j = n0jl, n1jl
if (j == k) cycle j_aibjakbj_aijk
a_aibjakbj_aijk: do a = n0ac, n1ac
i_aibjakbj_aijk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbj_aijk
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbj_aijk(t2, &
 nocc, nactive, a, i, j, k)
b1 = min(a - 1, n1bd)
b_aibjakbj_aijk: do b = n0bd, b1
if (b == a) cycle b_aibjakbj_aijk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjakbj_aijk
end do i_aibjakbj_aijk
end do a_aibjakbj_aijk
end do j_aibjakbj_aijk
end do k_aibjakbj_aijk
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_aibk: do k = n0k, n1k
b_aibjakbj_aibk: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjakbj_aibk: do a = a0, n1ac
if (a == b) cycle a_aibjakbj_aibk
i_aibjakbj_aibk: do i = n0i, n1i
if (i == k) cycle i_aibjakbj_aibk
if ((n1jl .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbj_aibk(t2, &
 nocc, nactive, a, i, b, k)
j_aibjakbj_aibk: do j = n0jl, n1jl
if (j == i .or. j == k) cycle j_aibjakbj_aibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjakbj_aibk
end do i_aibjakbj_aibk
end do a_aibjakbj_aibk
end do b_aibjakbj_aibk
end do k_aibjakbj_aibk
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l
! Equalities: c == a, j == i, k == i
! No equalities independent of the above can hold.
!
d_aibiaidl_ibdl: do d = n0d, n1d
l_aibiaidl_ibdl: do l = n0l, n1l
b_aibiaidl_ibdl: do b = n0b, n1b
if (b == d) cycle b_aibiaidl_ibdl
i_aibiaidl_ibdl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibiaidl_ibdl
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiaidl_ibdl(t2, &
 nocc, nactive, i, b, d, l)
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidl_ibdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidl_ibdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiaidl_ibdl
end do i_aibiaidl_ibdl
end do b_aibiaidl_ibdl
end do l_aibiaidl_ibdl
end do d_aibiaidl_ibdl
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l
! Equalities: c == a, j == i, k == i
! No equalities independent of the above can hold.
!
d_aibiaidl_aibdl: do d = n0d, n1d
l_aibiaidl_aibdl: do l = n0l, n1l
b_aibiaidl_aibdl: do b = n0b, n1b
if (b == d) cycle b_aibiaidl_aibdl
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidl_aibdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidl_aibdl
i_aibiaidl_aibdl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibiaidl_aibdl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiaidl_aibdl(t2, nocc, nactive, a, &
 i, b, d, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaidl_aibdl
end do a_aibiaidl_aibdl
end do b_aibiaidl_aibdl
end do l_aibiaidl_aibdl
end do d_aibiaidl_aibdl
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjaidi_ibjd: do d = n0d, n1d
b_aibjaidi_ibjd: do b = n0b, n1b
if (b == d) cycle b_aibjaidi_ibjd
j_aibjaidi_ibjd: do j = n0j, n1j
i_aibjaidi_ibjd: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjaidi_ibjd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaidi_ibjd(t2, &
 nocc, nactive, i, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidi_ibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidi_ibjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaidi_ibjd
end do i_aibjaidi_ibjd
end do j_aibjaidi_ibjd
end do b_aibjaidi_ibjd
end do d_aibjaidi_ibjd
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjaidi_aibjd: do d = n0d, n1d
b_aibjaidi_aibjd: do b = n0b, n1b
if (b == d) cycle b_aibjaidi_aibjd
j_aibjaidi_aibjd: do j = n0j, n1j
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidi_aibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidi_aibjd
i_aibjaidi_aibjd: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjaidi_aibjd
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaidi_aibjd(t2, nocc, nactive, a, &
 i, b, j, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidi_aibjd
end do a_aibjaidi_aibjd
end do j_aibjaidi_aibjd
end do b_aibjaidi_aibjd
end do d_aibjaidi_aibjd
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_bd: do d = n0d, n1d
b_aibjaidj_bd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_bd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1jl .ge. n0jl).and. (n1ik .ge. n0ik).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaidj_bd(t2, &
 nocc, nactive, b, d)
j_aibjaidj_bd: do j = n0jl, n1jl
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_bd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_bd
i_aibjaidj_bd: do i = n0ik, n1ik
if (i == j) cycle i_aibjaidj_bd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidj_bd
end do a_aibjaidj_bd
end do j_aibjaidj_bd
end do b_aibjaidj_bd
end do d_aibjaidj_bd
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_abd: do d = n0d, n1d
b_aibjaidj_abd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_abd
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_abd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_abd
if ((n1jl .ge. n0jl).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaidj_abd(t2, &
 nocc, nactive, a, b, d)
j_aibjaidj_abd: do j = n0jl, n1jl
i_aibjaidj_abd: do i = n0ik, n1ik
if (i == j) cycle i_aibjaidj_abd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidj_abd
end do j_aibjaidj_abd
end do a_aibjaidj_abd
end do b_aibjaidj_abd
end do d_aibjaidj_abd
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_ibd: do d = n0d, n1d
b_aibjaidj_ibd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_ibd
i_aibjaidj_ibd: do i = n0ik, n1ik
nn0a = max(b + 1, d + 1, n0ac)
if ((n1jl .ge. n0jl).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaidj_ibd(t2, &
 nocc, nactive, i, b, d)
j_aibjaidj_ibd: do j = n0jl, n1jl
if (j == i) cycle j_aibjaidj_ibd
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_ibd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_ibd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaidj_ibd
end do j_aibjaidj_ibd
end do i_aibjaidj_ibd
end do b_aibjaidj_ibd
end do d_aibjaidj_ibd
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_bjd: do d = n0d, n1d
b_aibjaidj_bjd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_bjd
j_aibjaidj_bjd: do j = n0jl, n1jl
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ik .ge. n0ik).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaidj_bjd(t2, &
 nocc, nactive, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_bjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_bjd
i_aibjaidj_bjd: do i = n0ik, n1ik
if (i == j) cycle i_aibjaidj_bjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidj_bjd
end do a_aibjaidj_bjd
end do j_aibjaidj_bjd
end do b_aibjaidj_bjd
end do d_aibjaidj_bjd
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_ibjd: do d = n0d, n1d
b_aibjaidj_ibjd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_ibjd
j_aibjaidj_ibjd: do j = n0jl, n1jl
i_aibjaidj_ibjd: do i = n0ik, n1ik
if (i == j) cycle i_aibjaidj_ibjd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaidj_ibjd(t2, &
 nocc, nactive, i, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_ibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_ibjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaidj_ibjd
end do i_aibjaidj_ibjd
end do j_aibjaidj_ibjd
end do b_aibjaidj_ibjd
end do d_aibjaidj_ibjd
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_abjd: do d = n0d, n1d
b_aibjaidj_abjd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_abjd
j_aibjaidj_abjd: do j = n0jl, n1jl
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_abjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_abjd
if ((n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaidj_abjd(t2, &
 nocc, nactive, a, b, j, d)
i_aibjaidj_abjd: do i = n0ik, n1ik
if (i == j) cycle i_aibjaidj_abjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidj_abjd
end do a_aibjaidj_abjd
end do j_aibjaidj_abjd
end do b_aibjaidj_abjd
end do d_aibjaidj_abjd
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_aibd: do d = n0d, n1d
b_aibjaidj_aibd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_aibd
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_aibd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_aibd
i_aibjaidj_aibd: do i = n0ik, n1ik
if ((n1jl .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaidj_aibd(t2, &
 nocc, nactive, a, i, b, d)
j_aibjaidj_aibd: do j = n0jl, n1jl
if (j == i) cycle j_aibjaidj_aibd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjaidj_aibd
end do i_aibjaidj_aibd
end do a_aibjaidj_aibd
end do b_aibjaidj_aibd
end do d_aibjaidj_aibd
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibiakdi_ibkd: do d = n0d, n1d
k_aibiakdi_ibkd: do k = n0k, n1k
b_aibiakdi_ibkd: do b = n0b, n1b
if (b == d) cycle b_aibiakdi_ibkd
i_aibiakdi_ibkd: do i = n0ijl, n1ijl
if (i == k) cycle i_aibiakdi_ibkd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakdi_ibkd(t2, &
 nocc, nactive, i, b, k, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibiakdi_ibkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdi_ibkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiakdi_ibkd
end do i_aibiakdi_ibkd
end do b_aibiakdi_ibkd
end do k_aibiakdi_ibkd
end do d_aibiakdi_ibkd
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibiakdi_aibkd: do d = n0d, n1d
k_aibiakdi_aibkd: do k = n0k, n1k
b_aibiakdi_aibkd: do b = n0b, n1b
if (b == d) cycle b_aibiakdi_aibkd
a0 = max(b + 1, d + 1, n0ac)
a_aibiakdi_aibkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdi_aibkd
i_aibiakdi_aibkd: do i = n0ijl, n1ijl
if (i == k) cycle i_aibiakdi_aibkd
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakdi_aibkd(t2, nocc, nactive, a, &
 i, b, k, d)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakdi_aibkd
end do a_aibiakdi_aibkd
end do b_aibiakdi_aibkd
end do k_aibiakdi_aibkd
end do d_aibiakdi_aibkd
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == k
! No equalities independent of the above can hold.
!
d_aibiakdk_ibkd: do d = n0d, n1d
k_aibiakdk_ibkd: do k = n0kl, n1kl
b_aibiakdk_ibkd: do b = n0b, n1b
if (b == d) cycle b_aibiakdk_ibkd
i_aibiakdk_ibkd: do i = n0ij, n1ij
if (i == k) cycle i_aibiakdk_ibkd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakdk_ibkd(t2, &
 nocc, nactive, i, b, k, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibiakdk_ibkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdk_ibkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiakdk_ibkd
end do i_aibiakdk_ibkd
end do b_aibiakdk_ibkd
end do k_aibiakdk_ibkd
end do d_aibiakdk_ibkd
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdi_abd: do d = n0d, n1d
b_aibjajdi_abd: do b = n0b, n1b
if (b == d) cycle b_aibjajdi_abd
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi_abd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi_abd
if ((n1jk .ge. n0jk).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajdi_abd(t2, &
 nocc, nactive, a, b, d)
j_aibjajdi_abd: do j = n0jk, n1jk
i_aibjajdi_abd: do i = n0il, n1il
if (i == j) cycle i_aibjajdi_abd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdi_abd
end do j_aibjajdi_abd
end do a_aibjajdi_abd
end do b_aibjajdi_abd
end do d_aibjajdi_abd
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdi_ibjd: do d = n0d, n1d
b_aibjajdi_ibjd: do b = n0b, n1b
if (b == d) cycle b_aibjajdi_ibjd
j_aibjajdi_ibjd: do j = n0jk, n1jk
i_aibjajdi_ibjd: do i = n0il, n1il
if (i == j) cycle i_aibjajdi_ibjd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajdi_ibjd(t2, &
 nocc, nactive, i, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi_ibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi_ibjd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajdi_ibjd
end do i_aibjajdi_ibjd
end do j_aibjajdi_ibjd
end do b_aibjajdi_ibjd
end do d_aibjajdi_ibjd
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdi_abjd: do d = n0d, n1d
b_aibjajdi_abjd: do b = n0b, n1b
if (b == d) cycle b_aibjajdi_abjd
j_aibjajdi_abjd: do j = n0jk, n1jk
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi_abjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi_abjd
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajdi_abjd(t2, &
 nocc, nactive, a, b, j, d)
i_aibjajdi_abjd: do i = n0il, n1il
if (i == j) cycle i_aibjajdi_abjd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdi_abjd
end do a_aibjajdi_abjd
end do j_aibjajdi_abjd
end do b_aibjajdi_abjd
end do d_aibjajdi_abjd
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdi_aibd: do d = n0d, n1d
b_aibjajdi_aibd: do b = n0b, n1b
if (b == d) cycle b_aibjajdi_aibd
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi_aibd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi_aibd
i_aibjajdi_aibd: do i = n0il, n1il
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajdi_aibd(t2, &
 nocc, nactive, a, i, b, d)
j_aibjajdi_aibd: do j = n0jk, n1jk
if (j == i) cycle j_aibjajdi_aibd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajdi_aibd
end do i_aibjajdi_aibd
end do a_aibjajdi_aibd
end do b_aibjajdi_aibd
end do d_aibjajdi_aibd
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i
! Equalities: c == a, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjajdj_ibjd: do d = n0d, n1d
b_aibjajdj_ibjd: do b = n0b, n1b
if (b == d) cycle b_aibjajdj_ibjd
j_aibjajdj_ibjd: do j = n0jkl, n1jkl
i_aibjajdj_ibjd: do i = n0i, n1i
if (i == j) cycle i_aibjajdj_ibjd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajdj_ibjd(t2, &
 nocc, nactive, i, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdj_ibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdj_ibjd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajdj_ibjd
end do i_aibjajdj_ibjd
end do j_aibjajdj_ibjd
end do b_aibjajdj_ibjd
end do d_aibjajdj_ibjd
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i
! Equalities: c == a, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjajdj_aibjd: do d = n0d, n1d
b_aibjajdj_aibjd: do b = n0b, n1b
if (b == d) cycle b_aibjajdj_aibjd
j_aibjajdj_aibjd: do j = n0jkl, n1jkl
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdj_aibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdj_aibjd
i_aibjajdj_aibjd: do i = n0i, n1i
if (i == j) cycle i_aibjajdj_aibjd
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajdj_aibjd(t2, nocc, nactive, a, &
 i, b, j, d)
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdj_aibjd
end do a_aibjajdj_aibjd
end do j_aibjajdj_aibjd
end do b_aibjajdj_aibjd
end do d_aibjajdj_aibjd
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajcial_ajcl: do l = n0l, n1l
c_aiajcial_ajcl: do c = n0c, n1c
j_aiajcial_ajcl: do j = n0j, n1j
if (j == l) cycle j_aiajcial_ajcl
a1 = min(c - 1, n1abd)
a_aiajcial_ajcl: do a = n0abd, a1
if (a == c) cycle a_aiajcial_ajcl
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajcial_ajcl(t2, &
 nocc, nactive, a, j, c, l)
i0 = max(j + 1, n0ik)
i_aiajcial_ajcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcial_ajcl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcial_ajcl
end do a_aiajcial_ajcl
end do j_aiajcial_ajcl
end do c_aiajcial_ajcl
end do l_aiajcial_ajcl
end subroutine ccjac_22_tripletmm_dav_part5
end module ccjac_block_22_tripletmm_dav_part5
