module ccjac_block_31_part2
use eom_cc3_31_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:36:33 UTC.
!
contains
 
subroutine ccjac_31_part2(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: a, b, c, d
integer :: i, j, k, l
integer :: ai, bj, ck, dl
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
! Free virtual indices: b, a
! Free occupied indices: k, i, j
! Equalities: c == b, d == b, l == k
! No equalities independent of the above can hold.
!
k_aibjbkbk: do k = nocc0, nocc1
b_aibjbkbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbk: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbk: do a = b + 1, nvirt1
i_aibjbkbk: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbkbk(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkbk
end do a_aibjbkbk
end do j_aibjbkbk
end do b_aibjbkbk
end do k_aibjbkbk
!
! Elementary loop 2
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, l
! Equalities: c == b, d == b, j == i
! No equalities independent of the above can hold.
!
l_aibibkbl: do l = nocc0, nocc1
k_aibibkbl: do k = nocc0, nocc1
if (k == l) cycle k_aibibkbl
b_aibibkbl: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibibkbl: do a = b + 1, nvirt1
i_aibibkbl: do i = k + 1, nocc1
if (i == l) cycle i_aibibkbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibibkbl(eorb, t2, t1, nocc, nactive, a, i, b, k, l)
end do i_aibibkbl
end do a_aibibkbl
end do b_aibibkbl
end do k_aibibkbl
end do l_aibibkbl
!
! Elementary loop 3
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, k
! Equalities: c == b, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjbkbj: do k = nocc0, nocc1
b_aibjbkbj: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbj: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbj: do a = b + 1, nvirt1
i_aibjbkbj: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbkbj(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkbj
end do a_aibjbkbj
end do j_aibjbkbj
end do b_aibjbkbj
end do k_aibjbkbj
!
! Elementary loop 4
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjbkbi: do k = nocc0, nocc1
b_aibjbkbi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbi: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkbi: do a = b + 1, nvirt1
i_aibjbkbi: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbkbi(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkbi
end do a_aibjbkbi
end do j_aibjbkbi
end do b_aibjbkbi
end do k_aibjbkbi
!
! Elementary loop 5
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i
! Equalities: d == b, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjbj: do c = nvirt0, nvirt1
b_aibjcjbj: do b = c + 1, nvirt1
j_aibjcjbj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbj
i_aibjcjbj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcjbj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjbj
end do a_aibjcjbj
end do j_aibjcjbj
end do b_aibjcjbj
end do c_aibjcjbj
!
! Elementary loop 6
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi: do c = nvirt0, nvirt1
b_aibjcjbi: do b = c + 1, nvirt1
j_aibjcjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjbi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjbi
i_aibjcjbi: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcjbi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjbi
end do a_aibjcjbi
end do j_aibjcjbi
end do b_aibjcjbi
end do c_aibjcjbi
!
! Elementary loop 7
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjcibi: do c = nvirt0, nvirt1
b_aibjcibi: do b = c + 1, nvirt1
j_aibjcibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibi: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibi
i_aibjcibi: do i = nocc0, nocc1
if (i == j) cycle i_aibjcibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcibi(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcibi
end do a_aibjcibi
end do j_aibjcibi
end do b_aibjcibi
end do c_aibjcibi
!
! Elementary loop 8
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibj: do c = nvirt0, nvirt1
b_aibjcibj: do b = c + 1, nvirt1
j_aibjcibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcibj
i_aibjcibj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcibj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcibj
end do a_aibjcibj
end do j_aibjcibj
end do b_aibjcibj
end do c_aibjcibj
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, k == i
! No equalities independent of the above can hold.
!
l_aibjbial: do l = nocc0, nocc1
b_aibjbial: do b = nvirt0, nvirt1
j_aibjbial: do j = nocc0, nocc1
if (j == l) cycle j_aibjbial
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbial: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjbial: do i = nocc0, j - 1
if (i == l) cycle i_aibjbial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbial(eorb, t2, t1, nocc, nactive, a, i, b, j, l)
end do i_aibjbial
end do a_aibjbial
end do j_aibjbial
end do b_aibjbial
end do l_aibjbial
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j
! Equalities: d == a, c == b, l == k
! No equalities independent of the above can hold.
!
k_aibjbkak: do k = nocc0, nocc1
b_aibjbkak: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkak: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkak: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjbkak: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbkak(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkak
end do a_aibjbkak
end do j_aibjbkak
end do b_aibjbkak
end do k_aibjbkak
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: d == a, c == b, j == i
! No equalities independent of the above can hold.
!
l_aibibkal: do l = nocc0, nocc1
k_aibibkal: do k = nocc0, nocc1
if (k == l) cycle k_aibibkal
b_aibibkal: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkal: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibibkal: do i = k + 1, nocc1
if (i == l) cycle i_aibibkal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibibkal(eorb, t2, t1, nocc, nactive, a, i, b, k, l)
end do i_aibibkal
end do a_aibibkal
end do b_aibibkal
end do k_aibibkal
end do l_aibibkal
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: d == a, c == b, l == j
! No equalities independent of the above can hold.
!
k_aibjbkaj: do k = nocc0, nocc1
b_aibjbkaj: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaj: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkaj: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbkaj: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbkaj(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkaj
end do a_aibjbkaj
end do j_aibjbkaj
end do b_aibjbkaj
end do k_aibjbkaj
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, l == i
! No equalities independent of the above can hold.
!
k_aibjbkai: do k = nocc0, nocc1
b_aibjbkai: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkai: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbkai: do a = b + 1, nvirt1
i_aibjbkai: do i = nocc0, j - 1
if (i == k) cycle i_aibjbkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbkai(eorb, t2, t1, nocc, nactive, a, i, b, j, k)
end do i_aibjbkai
end do a_aibjbkai
end do j_aibjbkai
end do b_aibjbkai
end do k_aibjbkai
!
! Elementary loop 14
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjbidi: do d = nvirt0, nvirt1
b_aibjbidi: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjbidi
j_aibjbidi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbidi: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbidi
i_aibjbidi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbidi(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjbidi
end do a_aibjbidi
end do j_aibjbidi
end do b_aibjbidi
end do d_aibjbidi
!
! Elementary loop 15
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj: do d = nvirt0, nvirt1
b_aibjbidj: do b = nvirt0, nvirt1
if (b == d) cycle b_aibjbidj
j_aibjbidj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbidj: do a = b + 1, nvirt1
if (a == d) cycle a_aibjbidj
i_aibjbidj: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbidj(eorb, t2, t1, nocc, nactive, a, i, b, j, d)
end do i_aibjbidj
end do a_aibjbidj
end do j_aibjbidj
end do b_aibjbidj
end do d_aibjbidj
!
! Elementary loop 16
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, j == i, l == k
! No equalities independent of the above can hold.
!
d_aibibkdk: do d = nvirt0, nvirt1
k_aibibkdk: do k = nocc0, nocc1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibibkdk: do b = nvirt0, nvirt1
if (b == d) cycle b_aibibkdk
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkdk: do a = b + 1, nvirt1
if (a == d) cycle a_aibibkdk
i_aibibkdk: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibibkdk(eorb, t2, t1, nocc, nactive, a, i, b, k, d)
end do i_aibibkdk
end do a_aibibkdk
end do b_aibibkdk
end do k_aibibkdk
end do d_aibibkdk
!
! Elementary loop 17
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibibkdi: do d = nvirt0, nvirt1
k_aibibkdi: do k = nocc0, nocc1
b_aibibkdi: do b = nvirt0, nvirt1
if (b == d) cycle b_aibibkdi
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkdi: do a = b + 1, nvirt1
if (a == d) cycle a_aibibkdi
i_aibibkdi: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibibkdi(eorb, t2, t1, nocc, nactive, a, i, b, k, d)
end do i_aibibkdi
end do a_aibibkdi
end do b_aibibkdi
end do k_aibibkdi
end do d_aibibkdi
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i
! Equalities: d == a, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjaj: do c = nvirt0, nvirt1
b_aibjcjaj: do b = c + 1, nvirt1
j_aibjcjaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjaj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjaj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcjaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjaj
end do a_aibjcjaj
end do j_aibjcjaj
end do b_aibjcjaj
end do c_aibjcjaj
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai: do c = nvirt0, nvirt1
b_aibjcjai: do b = c + 1, nvirt1
j_aibjcjai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjai: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjai
i_aibjcjai: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcjai(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjai
end do a_aibjcjai
end do j_aibjcjai
end do b_aibjcjai
end do c_aibjcjai
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjciai: do c = nvirt0, nvirt1
b_aibjciai: do b = c + 1, nvirt1
j_aibjciai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciai: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciai
i_aibjciai: do i = nocc0, nocc1
if (i == j) cycle i_aibjciai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjciai(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciai
end do a_aibjciai
end do j_aibjciai
end do b_aibjciai
end do c_aibjciai
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciaj: do c = nvirt0, nvirt1
b_aibjciaj: do b = c + 1, nvirt1
j_aibjciaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciaj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjciaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciaj: do i = nocc0, nocc1
if (i == j) cycle i_aibjciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjciaj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjciaj
end do a_aibjciaj
end do j_aibjciaj
end do b_aibjciaj
end do c_aibjciaj
!
! Elementary loop 22
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, i
! Equalities: d == c, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjcj: do c = nvirt0, nvirt1
b_aibjcjcj: do b = c + 1, nvirt1
j_aibjcjcj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjcj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjcj
i_aibjcjcj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcjcj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjcj
end do a_aibjcjcj
end do j_aibjcjcj
end do b_aibjcjcj
end do c_aibjcjcj
!
! Elementary loop 23
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjci: do c = nvirt0, nvirt1
b_aibjcjci: do b = c + 1, nvirt1
j_aibjcjci: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcjci: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcjci
i_aibjcjci: do i = nocc0, nocc1
if (i == j) cycle i_aibjcjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcjci(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcjci
end do a_aibjcjci
end do j_aibjcjci
end do b_aibjcjci
end do c_aibjcjci
!
! Elementary loop 24
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjcici: do c = nvirt0, nvirt1
b_aibjcici: do b = c + 1, nvirt1
j_aibjcici: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcici: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcici
i_aibjcici: do i = nocc0, nocc1
if (i == j) cycle i_aibjcici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcici(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcici
end do a_aibjcici
end do j_aibjcici
end do b_aibjcici
end do c_aibjcici
!
! Elementary loop 25
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicj: do c = nvirt0, nvirt1
b_aibjcicj: do b = c + 1, nvirt1
j_aibjcicj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicj: do a = b + 1, nvirt1
if (a == c) cycle a_aibjcicj
i_aibjcicj: do i = nocc0, nocc1
if (i == j) cycle i_aibjcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibjcicj(eorb, t2, t1, nocc, nactive, a, i, b, j, c)
end do i_aibjcicj
end do a_aibjcicj
end do j_aibjcicj
end do b_aibjcicj
end do c_aibjcicj
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, d == a, k == j, l == j
! No equalities independent of the above can hold.
!
c_aiajcjaj: do c = nvirt0, nvirt1
j_aiajcjaj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjaj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aiajcjaj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjaj
end do a_aiajcjaj
end do j_aiajcjaj
end do c_aiajcjaj
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjai: do c = nvirt0, nvirt1
j_aiajcjai: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjai: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjai: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aiajcjai(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjai
end do a_aiajcjai
end do j_aiajcjai
end do c_aiajcjai
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == i
! No equalities independent of the above can hold.
!
c_aiajciai: do c = nvirt0, nvirt1
j_aiajciai: do j = nocc0, nocc1
a_aiajciai: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciai: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajciai(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajciai
end do a_aiajciai
end do j_aiajciai
end do c_aiajciai
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajciaj: do c = nvirt0, nvirt1
j_aiajciaj: do j = nocc0, nocc1
a_aiajciaj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajciaj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajciaj
end do a_aiajciaj
end do j_aiajciaj
end do c_aiajciaj
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, d == c, k == j, l == j
! No equalities independent of the above can hold.
!
c_aiajcjcj: do c = nvirt0, nvirt1
j_aiajcjcj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjcj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjcj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aiajcjcj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjcj
end do a_aiajcjcj
end do j_aiajcjcj
end do c_aiajcjcj
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, l == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjci: do c = nvirt0, nvirt1
j_aiajcjci: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjci: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjci: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aiajcjci(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjci
end do a_aiajcjci
end do j_aiajcjci
end do c_aiajcjci
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == i
! No equalities independent of the above can hold.
!
c_aiajcici: do c = nvirt0, nvirt1
j_aiajcici: do j = nocc0, nocc1
a_aiajcici: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcici: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajcici(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcici
end do a_aiajcici
end do j_aiajcici
end do c_aiajcici
!
! Elementary loop 33
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajcicj: do c = nvirt0, nvirt1
j_aiajcicj: do j = nocc0, nocc1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcicj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcicj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aiajcicj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcicj
end do a_aiajcicj
end do j_aiajcicj
end do c_aiajcicj
!
! Elementary loop 34
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjbibi: do b = nvirt0, nvirt1
j_aibjbibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbibi: do a = b + 1, nvirt1
i_aibjbibi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbibi(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbibi
end do a_aibjbibi
end do j_aibjbibi
end do b_aibjbibi
!
! Elementary loop 35
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbibj: do b = nvirt0, nvirt1
j_aibjbibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbibj: do a = b + 1, nvirt1
i_aibjbibj: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbibj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbibj
end do a_aibjbibj
end do j_aibjbibj
end do b_aibjbibj
!
! Elementary loop 36
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, d == b, j == i, l == k
! No equalities independent of the above can hold.
!
k_aibibkbk: do k = nocc0, nocc1
b_aibibkbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkbk: do a = b + 1, nvirt1
i_aibibkbk: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibibkbk(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkbk
end do a_aibibkbk
end do b_aibibkbk
end do k_aibibkbk
!
! Elementary loop 37
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibibkbi: do k = nocc0, nocc1
b_aibibkbi: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkbi: do a = b + 1, nvirt1
i_aibibkbi: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibibkbi(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkbi
end do a_aibibkbi
end do b_aibibkbi
end do k_aibibkbi
!
! Elementary loop 38
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjbiai: do b = nvirt0, nvirt1
j_aibjbiai: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiai: do a = b + 1, nvirt1
i_aibjbiai: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbiai(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbiai
end do a_aibjbiai
end do j_aibjbiai
end do b_aibjbiai
!
! Elementary loop 39
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbiaj: do b = nvirt0, nvirt1
j_aibjbiaj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbiaj: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbiaj: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v6_eom_cc3_31_trans_aibjbiaj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjbiaj
end do a_aibjbiaj
end do j_aibjbiaj
end do b_aibjbiaj
!
! Elementary loop 40
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == k
! No equalities independent of the above can hold.
!
k_aibibkak: do k = nocc0, nocc1
b_aibibkak: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkak: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibibkak: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibibkak(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkak
end do a_aibibkak
end do b_aibibkak
end do k_aibibkak
!
! Elementary loop 41
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibibkai: do k = nocc0, nocc1
b_aibibkai: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibkai: do a = b + 1, nvirt1
i_aibibkai: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + mu3(ai, bj, ck)
iket = ketoffset + dl
jac(ibra, iket) = v0_eom_cc3_31_trans_aibibkai(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibibkai
end do a_aibibkai
end do b_aibibkai
end do k_aibibkai
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
end subroutine ccjac_31_part2
end module ccjac_block_31_part2
