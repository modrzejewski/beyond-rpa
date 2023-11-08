module ccjac_block_21_tripletp_dav
use eom_ccsd_21_tripletp_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
use cc_gparams
implicit none
!
! File generated automatically on 2014-04-07 22:27:10 UTC.
!
contains
subroutine ccjac_21_tripletp_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, i0
integer :: n0ac, n0bc, n0ik, n0jk
integer :: n1ac, n1bc, n1ik, n1jk
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket
integer :: braoffset, ketoffset
real(F64) :: sum
sum = zero
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
n0ac = max(n0a, n0c)
n0bc = max(n0b, n0c)
n0ik = max(n0i, n0k)
n0jk = max(n0j, n0k)
n1ac = min(n1a, n1c)
n1bc = min(n1b, n1c)
n1ik = min(n1i, n1k)
n1jk = min(n1j, n1k)
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
j_aibjak: do j = n0j, n1j
if (j == k) cycle j_aibjak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjak: do a = a0, n1ac
if (a == b) cycle a_aibjak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjak: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + ck
jac_ibra_iket = eom_ccsd_21_tripletp_trans_aibjak(t2, nocc, nactive, a, i, b, j, &
 k)
sum = sum + jac_ibra_iket
 ! if (a.lt.b) print*, 'aibjak'
 ! if (a.lt.j) print*, 'aibjak'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aibjak
end do a_aibjak
end do j_aibjak
end do b_aibjak
end do k_aibjak
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b
! No equalities independent of the above can hold.
!
k_aibjbk: do k = n0k, n1k
b_aibjbk: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbk: do j = n0j, n1j
if (j == k) cycle j_aibjbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbk: do a = a0, n1a
if (a == b) cycle a_aibjbk
i0 = max(j + 1, n0i)
i_aibjbk: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + ck
jac_ibra_iket = eom_ccsd_21_tripletp_trans_aibjbk(t2, nocc, nactive, a, i, b, j, &
 k)
sum = sum + jac_ibra_iket
 ! if (a.lt.b) print*, 'aibjbk'
 ! if (a.lt.j) print*, 'aibjbk'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aibjbk
end do a_aibjbk
end do j_aibjbk
end do b_aibjbk
end do k_aibjbk
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i
! Equalities: k == j
! No equalities independent of the above can hold.
!
c_aibjcj: do c = n0c, n1c
b_aibjcj: do b = n0b, n1b
if (b == c) cycle b_aibjcj
j_aibjcj: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcj
i0 = max(j + 1, n0i)
i_aibjcj: do i = i0, n1i
if (i == j) cycle i_aibjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + ck
jac_ibra_iket = eom_ccsd_21_tripletp_trans_aibjcj(t2, nocc, nactive, a, i, b, j, &
 c)
sum = sum + jac_ibra_iket
 ! if (a.lt.b) print*, 'aibjcj'
 ! if (a.lt.j) print*, 'aibjcj'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aibjcj
end do a_aibjcj
end do j_aibjcj
end do b_aibjcj
end do c_aibjcj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: k == i
! No equalities independent of the above can hold.
!
c_aibjci: do c = n0c, n1c
b_aibjci: do b = n0b, n1b
if (b == c) cycle b_aibjci
j_aibjci: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjci
i0 = max(j + 1, n0ik)
i_aibjci: do i = i0, n1ik
if (i == j) cycle i_aibjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + ck
jac_ibra_iket = eom_ccsd_21_tripletp_trans_aibjci(t2, nocc, nactive, a, i, b, j, &
 c)
sum = sum + jac_ibra_iket
 ! if (a.lt.b) print*, 'aibjci'
 ! if (a.lt.j) print*, 'aibjci'
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
! Free occupied indices: j, i
! Equalities: c == a, k == j
! No equalities independent of the above can hold.
!
b_aibjaj: do b = n0b, n1b
j_aibjaj: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaj: do a = a0, n1ac
if (a == b) cycle a_aibjaj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjaj: do i = i0, n1i
if (i == j) cycle i_aibjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + ck
jac_ibra_iket = eom_ccsd_21_tripletp_trans_aibjaj(t2, nocc, nactive, a, i, b, j)
sum = sum + jac_ibra_iket
 ! if (a.lt.b) print*, 'aibjaj'
 ! if (a.lt.j) print*, 'aibjaj'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aibjaj
end do a_aibjaj
end do j_aibjaj
end do b_aibjaj
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
a0 = max(b + 1, n0ac)
a_aibjai: do a = a0, n1ac
if (a == b) cycle a_aibjai
i0 = max(j + 1, n0ik)
i_aibjai: do i = i0, n1ik
if (i == j) cycle i_aibjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + ck
jac_ibra_iket = eom_ccsd_21_tripletp_trans_aibjai(t2, nocc, nactive, a, i, b, j)
sum = sum + jac_ibra_iket
 ! if (a.lt.b) print*, 'aibjai'
 ! if (a.lt.j) print*, 'aibjai'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aibjai
end do a_aibjai
end do j_aibjai
end do b_aibjai
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i
! Equalities: c == b, k == j
! No equalities independent of the above can hold.
!
b_aibjbj: do b = n0bc, n1bc
j_aibjbj: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbj: do a = a0, n1a
if (a == b) cycle a_aibjbj
i0 = max(j + 1, n0i)
i_aibjbj: do i = i0, n1i
if (i == j) cycle i_aibjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + ck
jac_ibra_iket = eom_ccsd_21_tripletp_trans_aibjbj(t2, nocc, nactive, a, i, b, j)
sum = sum + jac_ibra_iket
 ! if (a.lt.b) print*, 'aibjbj'
 ! if (a.lt.j) print*, 'aibjbj'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aibjbj
end do a_aibjbj
end do j_aibjbj
end do b_aibjbj
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, k == i
! No equalities independent of the above can hold.
!
b_aibjbi: do b = n0bc, n1bc
j_aibjbi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbi: do a = a0, n1a
if (a == b) cycle a_aibjbi
i0 = max(j + 1, n0ik)
i_aibjbi: do i = i0, n1ik
if (i == j) cycle i_aibjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + ck
jac_ibra_iket = eom_ccsd_21_tripletp_trans_aibjbi(t2, nocc, nactive, a, i, b, j)
sum = sum + jac_ibra_iket
 ! if (a.lt.b) print*, 'aibjbi'
 ! if (a.lt.j) print*, 'aibjbi'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aibjbi
end do a_aibjbi
end do j_aibjbi
end do b_aibjbi
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: none
! No equalities independent of the above can hold.
!
c_aibjck: do c = n0c, n1c
k_aibjck: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjck: do b = n0b, n1b
if (b == c) cycle b_aibjck
j_aibjck: do j = n0j, n1j
if (j == k) cycle j_aibjck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjck
i0 = max(j + 1, n0i)
i_aibjck: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + ck
jac_ibra_iket = eom_ccsd_21_tripletp_trans_aibjck(t2, nocc, nactive, a, i, b, j, &
 c, k)
sum = sum + jac_ibra_iket
 ! if (a.lt.b) print*, 'aibjck'
 ! if (a.lt.j) print*, 'aibjck'
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
 

end do i_aibjck
end do a_aibjck
end do j_aibjck
end do b_aibjck
end do k_aibjck
end do c_aibjck

end subroutine ccjac_21_tripletp_dav
end module ccjac_block_21_tripletp_dav
