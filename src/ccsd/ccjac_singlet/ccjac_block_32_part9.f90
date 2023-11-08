module ccjac_block_32_part9
use eom_cc3_32_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:41:38 UTC.
!
contains
 
subroutine ccjac_32_part9(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: ai, bj, ck, dl, em
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
!
! Elementary loop 1
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajcicicj: do c = nvirt0, nvirt1
j_aiajcicicj: do j = nocc0, nocc1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcicicj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcicicj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aiajcicicj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcicicj
end do a_aiajcicicj
end do j_aiajcicicj
end do c_aiajcicicj
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjcicj: do c = nvirt0, nvirt1
j_aiajcjcicj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjcicj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjcicj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjcicj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjcicj
end do a_aiajcjcicj
end do j_aiajcjcicj
end do c_aiajcjcicj
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, d == c, e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjcjcj: do c = nvirt0, nvirt1
j_aiajcjcjcj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjcjcj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjcjcj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aiajcjcjcj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjcjcj
end do a_aiajcjcjcj
end do j_aiajcjcjcj
end do c_aiajcjcjcj
!
! Elementary loop 4
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, d == b, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibibkbibi: do k = nocc0, nocc1
b_aibibkbibi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkbibi: do a = b + 1, nvirt1
i_aibibkbibi: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkbibi(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkbibi
end do a_aibibkbibi
end do b_aibibkbibi
end do k_aibibkbibi
!
! Elementary loop 5
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, d == b, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibibkbibk: do k = nocc0, nocc1
b_aibibkbibk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkbibk: do a = b + 1, nvirt1
i_aibibkbibk: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkbibk(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkbibk
end do a_aibibkbibk
end do b_aibibkbibk
end do k_aibibkbibk
!
! Elementary loop 6
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, d == b, e == b, j == i, l == k, m == k
! No equalities independent of the above can hold.
!
k_aibibkbkbk: do k = nocc0, nocc1
b_aibibkbkbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkbkbk: do a = b + 1, nvirt1
i_aibibkbkbk: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v0_eom_cc3_32_trans_aibibkbkbk(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkbkbk
end do a_aibibkbkbk
end do b_aibibkbkbk
end do k_aibibkbkbk
!
! Elementary loop 7
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
b_aibjbibibi: do b = nvirt0, nvirt1
j_aibjbibibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbibibi: do a = b + 1, nvirt1
i_aibjbibibi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbibibi(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbibibi
end do a_aibjbibibi
end do j_aibjbibibi
end do b_aibjbibibi
!
! Elementary loop 8
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbibjbi: do b = nvirt0, nvirt1
j_aibjbibjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbibjbi: do a = b + 1, nvirt1
i_aibjbibjbi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbibjbi(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbibjbi
end do a_aibjbibjbi
end do j_aibjbibjbi
end do b_aibjbibjbi
!
! Elementary loop 9
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
b_aibjbibjbj: do b = nvirt0, nvirt1
j_aibjbibjbj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbibjbj: do a = b + 1, nvirt1
i_aibjbibjbj: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac(ibra, iket) = v6_eom_cc3_32_trans_aibjbibjbj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbibjbj
end do a_aibjbibjbj
end do j_aibjbibjbj
end do b_aibjbibjbj
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
end subroutine ccjac_32_part9
end module ccjac_block_32_part9
