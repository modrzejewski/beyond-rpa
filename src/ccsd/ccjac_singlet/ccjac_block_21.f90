module ccjac_block_21
use eom_ccsd_21_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-15 11:06:51 UTC.
!
contains
 
subroutine ccjac_21(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
double precision, dimension(:,:), intent(out)       :: jac
double precision, dimension(:), intent(in)          :: eorb
double precision, dimension(:, :), intent(in)       :: t1
double precision, dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: bra0, ket0
!
! Local variables
!
integer :: a, b, c
integer :: i, j, k
integer :: ai, bj, ck
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
!
! Elementary loop 1
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a
! No equalities independent of the above can hold.
!
k_aibjak: do k = nocc0, nocc1
b_aibjak: do b = nvirt0, nvirt1
j_aibjak: do j = nocc0, nocc1
if (j == k) cycle j_aibjak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjak: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjak: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibjak(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjak
end do a_aibjak
end do j_aibjak
end do b_aibjak
end do k_aibjak
!
! Elementary loop 2
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b
! No equalities independent of the above can hold.
!
k_aibjbk: do k = nocc0, nocc1
b_aibjbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbk: do j = nocc0, nocc1
if (j == k) cycle j_aibjbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbk: do a = b + 1, nvirt1
i_aibjbk: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibjbk(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbk
end do a_aibjbk
end do j_aibjbk
end do b_aibjbk
end do k_aibjbk
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a
! No equalities independent of the above can hold.
!
c_aiajck: do c = nvirt0, nvirt1
k_aiajck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajck: do j = nocc0, nocc1
if (j == k) cycle j_aiajck
a_aiajck: do a = nvirt0, nvirt1
if (a == c) cycle a_aiajck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajck: do i = j + 1, nocc1
if (i == k) cycle i_aiajck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aiajck(eorb, t2, t1, nocc, nactive, a, i, j, c, k)
end do i_aiajck
end do a_aiajck
end do j_aiajck
end do k_aiajck
end do c_aiajck
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: j == i
! No equalities independent of the above can hold.
!
c_aibick: do c = nvirt0, nvirt1
k_aibick: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibick: do b = nvirt0, nvirt1
if (b == c) cycle b_aibick
a_aibick: do a = b + 1, nvirt1
if (a == c) cycle a_aibick
i_aibick: do i = nocc0, nocc1
if (i == k) cycle i_aibick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibick(eorb, t2, t1, nocc, nactive, a, i, b, c, k)
end do i_aibick
end do a_aibick
end do b_aibick
end do k_aibick
end do c_aibick
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: k == i
! No equalities independent of the above can hold.
!
c_aibjci: do c = nvirt0, nvirt1
b_aibjci: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjci
j_aibjci: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjci
i_aibjci: do i = nocc0, nocc1
if (i == j) cycle i_aibjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjci
end do a_aibjci
end do j_aibjci
end do b_aibjci
end do c_aibjci
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i
! Equalities: k == j
! No equalities independent of the above can hold.
!
c_aibjcj: do c = nvirt0, nvirt1
b_aibjcj: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjcj
j_aibjcj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcj
i_aibjcj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibjcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcj
end do a_aibjcj
end do j_aibjcj
end do b_aibjcj
end do c_aibjcj
!
! Elementary loop 7
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, k
! Equalities: b == a, c == a
! No equalities independent of the above can hold.
!
k_aiajak: do k = nocc0, nocc1
j_aiajak: do j = nocc0, nocc1
if (j == k) cycle j_aiajak
a_aiajak: do a = nvirt0, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajak: do i = j + 1, nocc1
if (i == k) cycle i_aiajak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aiajak(eorb, t2, t1, nocc, nactive, a, i, j, k)
end do i_aiajak
end do a_aiajak
end do j_aiajak
end do k_aiajak
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, j == i
! No equalities independent of the above can hold.
!
k_aibiak: do k = nocc0, nocc1
b_aibiak: do b = nvirt0, nvirt1
a_aibiak: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiak: do i = nocc0, nocc1
if (i == k) cycle i_aibiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibiak(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibiak
end do a_aibiak
end do b_aibiak
end do k_aibiak
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, k == i
! No equalities independent of the above can hold.
!
b_aibjai: do b = nvirt0, nvirt1
j_aibjai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjai: do a = b + 1, nvirt1
i_aibjai: do i = nocc0, nocc1
if (i == j) cycle i_aibjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibjai(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjai
end do a_aibjai
end do j_aibjai
end do b_aibjai
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i
! Equalities: c == a, k == j
! No equalities independent of the above can hold.
!
b_aibjaj: do b = nvirt0, nvirt1
j_aibjaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaj: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjaj: do i = nocc0, nocc1
if (i == j) cycle i_aibjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibjaj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjaj
end do a_aibjaj
end do j_aibjaj
end do b_aibjaj
!
! Elementary loop 11
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, j == i
! No equalities independent of the above can hold.
!
k_aibibk: do k = nocc0, nocc1
b_aibibk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibk: do a = b + 1, nvirt1
i_aibibk: do i = nocc0, nocc1
if (i == k) cycle i_aibibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibibk(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibk
end do a_aibibk
end do b_aibibk
end do k_aibibk
!
! Elementary loop 12
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, k == i
! No equalities independent of the above can hold.
!
b_aibjbi: do b = nvirt0, nvirt1
j_aibjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbi: do a = b + 1, nvirt1
i_aibjbi: do i = nocc0, nocc1
if (i == j) cycle i_aibjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibjbi(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbi
end do a_aibjbi
end do j_aibjbi
end do b_aibjbi
!
! Elementary loop 13
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i
! Equalities: c == b, k == j
! No equalities independent of the above can hold.
!
b_aibjbj: do b = nvirt0, nvirt1
j_aibjbj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbj: do a = b + 1, nvirt1
i_aibjbj: do i = nocc0, nocc1
if (i == j) cycle i_aibjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibjbj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbj
end do a_aibjbj
end do j_aibjbj
end do b_aibjbj
!
! Elementary loop 14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, j == i
! No equalities independent of the above can hold.
!
c_aiaick: do c = nvirt0, nvirt1
k_aiaick: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a_aiaick: do a = nvirt0, nvirt1
if (a == c) cycle a_aiaick
i_aiaick: do i = nocc0, nocc1
if (i == k) cycle i_aiaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aiaick(eorb, t2, t1, nocc, nactive, a, i, c, k)
end do i_aiaick
end do a_aiaick
end do k_aiaick
end do c_aiaick
!
! Elementary loop 15
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, k == i
! No equalities independent of the above can hold.
!
c_aiajci: do c = nvirt0, nvirt1
j_aiajci: do j = nocc0, nocc1
a_aiajci: do a = nvirt0, nvirt1
if (a == c) cycle a_aiajci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajci: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aiajci(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajci
end do a_aiajci
end do j_aiajci
end do c_aiajci
!
! Elementary loop 16
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, k == j
! No equalities independent of the above can hold.
!
c_aiajcj: do c = nvirt0, nvirt1
j_aiajcj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcj: do a = nvirt0, nvirt1
if (a == c) cycle a_aiajcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aiajcj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcj
end do a_aiajcj
end do j_aiajcj
end do c_aiajcj
!
! Elementary loop 17
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i
! Equalities: j == i, k == i
! No equalities independent of the above can hold.
!
c_aibici: do c = nvirt0, nvirt1
b_aibici: do b = nvirt0, nvirt1
if (b == c) cycle b_aibici
a_aibici: do a = b + 1, nvirt1
if (a == c) cycle a_aibici
i_aibici: do i = nocc0, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibici(eorb, t2, t1, nocc, nactive, a, i, b, c)
end do i_aibici
end do a_aibici
end do b_aibici
end do c_aibici
!
! Elementary loop 18
! --------------------
! Free virtual indices: a
! Free occupied indices: i, k
! Equalities: b == a, c == a, j == i
! No equalities independent of the above can hold.
!
k_aiaiak: do k = nocc0, nocc1
a_aiaiak: do a = nvirt0, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiaiak: do i = nocc0, nocc1
if (i == k) cycle i_aiaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aiaiak(eorb, t2, t1, nocc, nactive, a, i, k)
end do i_aiaiak
end do a_aiaiak
end do k_aiaiak
!
! Elementary loop 19
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i
! No equalities independent of the above can hold.
!
j_aiajai: do j = nocc0, nocc1
a_aiajai: do a = nvirt0, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajai: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aiajai(eorb, t2, t1, nocc, nactive, a, i, j)
end do i_aiajai
end do a_aiajai
end do j_aiajai
!
! Elementary loop 20
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i
! Equalities: b == a, c == a, k == j
! No equalities independent of the above can hold.
!
j_aiajaj: do j = nocc0, nocc1
a_aiajaj: do a = nvirt0, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aiajaj(eorb, t2, t1, nocc, nactive, a, i, j)
end do i_aiajaj
end do a_aiajaj
end do j_aiajaj
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, j == i, k == i
! No equalities independent of the above can hold.
!
b_aibiai: do b = nvirt0, nvirt1
a_aibiai: do a = b + 1, nvirt1
i_aibiai: do i = nocc0, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibiai(eorb, t2, t1, nocc, nactive, a, i, b)
end do i_aibiai
end do a_aibiai
end do b_aibiai
!
! Elementary loop 22
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i
! Equalities: c == b, j == i, k == i
! No equalities independent of the above can hold.
!
b_aibibi: do b = nvirt0, nvirt1
a_aibibi: do a = b + 1, nvirt1
i_aibibi: do i = nocc0, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibibi(eorb, t2, t1, nocc, nactive, a, i, b)
end do i_aibibi
end do a_aibibi
end do b_aibibi
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i
! Equalities: b == a, j == i, k == i
! No equalities independent of the above can hold.
!
c_aiaici: do c = nvirt0, nvirt1
a_aiaici: do a = nvirt0, nvirt1
if (a == c) cycle a_aiaici
i_aiaici: do i = nocc0, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aiaici(eorb, t2, t1, nocc, nactive, a, i, c)
end do i_aiaici
end do a_aiaici
end do c_aiaici
!
! Elementary loop 24
! --------------------
! Free virtual indices: a
! Free occupied indices: i
! Equalities: b == a, c == a, j == i, k == i
! No equalities independent of the above can hold.
!
a_aiaiai: do a = nvirt0, nvirt1
i_aiaiai: do i = nocc0, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aiaiai(eorb, t2, t1, nocc, nactive, a, i)
end do i_aiaiai
end do a_aiaiai
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: none
! No equalities independent of the above can hold.
!
c_aibjck: do c = nvirt0, nvirt1
k_aibjck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjck: do b = nvirt0, nvirt1
if (b == c) cycle b_aibjck
j_aibjck: do j = nocc0, nocc1
if (j == k) cycle j_aibjck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjck: do a = b + 1, nvirt1
if (a == c) cycle a_aibjck
i_aibjck: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i_aibjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
jac(ibra, iket) = eom_ccsd_21_trans_aibjck(eorb, t2, t1, nocc, nactive, a, i, b, j, c, k)
end do i_aibjck
end do a_aibjck
end do j_aibjck
end do b_aibjck
end do k_aibjck
end do c_aibjck
end subroutine ccjac_21
end module ccjac_block_21
