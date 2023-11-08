module ccjac_block_12_tripletp_dav
use eom_ccsd_12_tripletp_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
use cc_gparams
implicit none
!
! File generated automatically on 2014-04-07 22:26:13 UTC.
!
contains
subroutine ccjac_12_tripletp_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, bra0, ket0) 
procedure(dav_sigma_update_diag) :: sigup_diag
procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
real(F64), dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c
integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k
integer, intent(in)                                 :: bra0, ket0
!
! Local variables
!
real(F64) :: jac_ibra_iket
integer :: a, b, c
integer :: i, j, k
integer :: ai, bj, ck
integer :: a0, a1, b0, i0, i1, j0
integer :: n0ab, n0ac, n0ij, n0ik
integer :: n1ab, n1ac, n1ij, n1ik
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket
integer :: braoffset, ketoffset
!real(F64) ::sum
!sum = zero
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
n0ab = max(n0a, n0b)
n0ac = max(n0a, n0c)
n0ij = max(n0i, n0j)
n0ik = max(n0i, n0k)
n1ab = min(n1a, n1b)
n1ac = min(n1a, n1c)
n1ij = min(n1i, n1j)
n1ik = min(n1i, n1k)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a
! No equalities independent of the above can hold.
!
k_aibjak: do k = n0k, n1k
b_aibjak: do b = n0b, n1b
j0 = max(k + 1, n0j)
j_aibjak: do j = j0, n1j
if (j == k) cycle j_aibjak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjak: do a = n0ac, a1
if (a == b) cycle a_aibjak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
jac_ibra_iket = eom_ccsd_12_tripletp_trans_aibjak(i, b, j, k)
!sum = sum + jac_ibra_iket
 ! if (a.lt.a) print*, 'aibjak'
 ! if (a.lt.k) print*, 'aibjak'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aibjak
end do a_aibjak
end do j_aibjak
end do b_aibjak
end do k_aibjak
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a
! No equalities independent of the above can hold.
!
c_aiajck: do c = n0c, n1c
k_aiajck: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajck: do j = j0, n1j
if (j == k) cycle j_aiajck
a0 = max(c + 1, n0ab)
a_aiajck: do a = a0, n1ab
if (a == c) cycle a_aiajck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
jac_ibra_iket = eom_ccsd_12_tripletp_trans_aiajck(i, j, c, k)
!!sum = sum + jac_ibra_iket
 ! if (a.lt.c) print*, 'aiajck'
 ! if (a.lt.k) print*, 'aiajck'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aiajck
end do a_aiajck
end do j_aiajck
end do k_aiajck
end do c_aiajck
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: j == i
! No equalities independent of the above can hold.
!
c_aibick: do c = n0c, n1c
k_aibick: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibick: do b = b0, n1b
if (b == c) cycle b_aibick
a_aibick: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibick
i0 = max(k + 1, n0ij)
i_aibick: do i = i0, n1ij
if (i == k) cycle i_aibick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_ccsd_12_tripletp_trans_aibick(a, b, c, k)
!sum = sum + jac_ibra_iket
 ! if (a.lt.c) print*, 'aibick'
 ! if (a.lt.k) print*, 'aibick'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aibick
end do a_aibick
end do b_aibick
end do k_aibick
end do c_aibick
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: k == i
! No equalities independent of the above can hold.
!
c_aibjci: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjci: do b = b0, n1b
if (b == c) cycle b_aibjci
j_aibjci: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjci
i1 = min(j - 1, n1ik)
i_aibjci: do i = n0ik, i1
if (i == j) cycle i_aibjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_ccsd_12_tripletp_trans_aibjci(a, b, j, c)
!sum = sum + jac_ibra_iket
 ! if (a.lt.c) print*, 'aibjci'
 ! if (a.lt.i) print*, 'aibjci'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aibjci
end do a_aibjci
end do j_aibjci
end do b_aibjci
end do c_aibjci
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, j == i
! No equalities independent of the above can hold.
!
k_aibiak: do k = n0k, n1k
b_aibiak: do b = n0b, n1b
a1 = min(b - 1, n1ac)
a_aibiak: do a = n0ac, a1
if (a == b) cycle a_aibiak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibiak: do i = i0, n1ij
if (i == k) cycle i_aibiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_ccsd_12_tripletp_trans_aibiak(nocc, a, i, b, k)
!sum = sum + jac_ibra_iket
 ! if (a.lt.a) print*, 'aibiak'
 ! if (a.lt.k) print*, 'aibiak'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aibiak
end do a_aibiak
end do b_aibiak
end do k_aibiak
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, k == i
! No equalities independent of the above can hold.
!
b_aibjai: do b = n0b, n1b
j_aibjai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjai: do a = n0ac, a1
if (a == b) cycle a_aibjai
i1 = min(j - 1, n1ik)
i_aibjai: do i = n0ik, i1
if (i == j) cycle i_aibjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_ccsd_12_tripletp_trans_aibjai(nocc, a, i, b, j)
!sum = sum + jac_ibra_iket
 ! if (a.lt.a) print*, 'aibjai'
 ! if (a.lt.i) print*, 'aibjai'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aibjai
end do a_aibjai
end do j_aibjai
end do b_aibjai
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, j == i
! No equalities independent of the above can hold.
!
c_aiaick: do c = n0c, n1c
k_aiaick: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaick: do a = a0, n1ab
if (a == c) cycle a_aiaick
i0 = max(k + 1, n0ij)
i_aiaick: do i = i0, n1ij
if (i == k) cycle i_aiaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_ccsd_12_tripletp_trans_aiaick(nocc, a, i, c, k)
!sum = sum + jac_ibra_iket
 ! if (a.lt.c) print*, 'aiaick'
 ! if (a.lt.k) print*, 'aiaick'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aiaick
end do a_aiaick
end do k_aiaick
end do c_aiaick
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, k == i
! No equalities independent of the above can hold.
!
c_aiajci: do c = n0c, n1c
j_aiajci: do j = n0j, n1j
a0 = max(c + 1, n0ab)
a_aiajci: do a = a0, n1ab
if (a == c) cycle a_aiajci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aiajci: do i = n0ik, i1
if (i == j) cycle i_aiajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_ccsd_12_tripletp_trans_aiajci(nocc, a, i, j, c)
!sum = sum + jac_ibra_iket
 ! if (a.lt.c) print*, 'aiajci'
 ! if (a.lt.i) print*, 'aiajci'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aiajci
end do a_aiajci
end do j_aiajci
end do c_aiajci
!  ! if(abs(sum).gt.one)then
! print*, '12p', sum
!  ! end if
end subroutine ccjac_12_tripletp_dav
end module ccjac_block_12_tripletp_dav
