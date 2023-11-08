module ccjac_block_23_part8
use eom_cc3_23_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:35:25 UTC.
!
contains
 
subroutine ccjac_23_part8(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
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
integer :: a, b, c, d, e
integer :: i, j, k, l, m
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
! Free virtual indices: a, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajaiajei: do e = nvirt0, nvirt1
j_aiajaiajei: do j = nocc0, nocc1
a_aiajaiajei: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajaiajei: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaiajei(eorb, t2, t1, nocc, nactive, a, i, j, e)
end do i_aiajaiajei
end do a_aiajaiajei
end do j_aiajaiajei
end do e_aiajaiajei
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aiajaiajej: do e = nvirt0, nvirt1
j_aiajaiajej: do j = nocc0, nocc1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajaiajej: do a = e + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajaiajej: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaiajej(eorb, t2, t1, nocc, nactive, a, i, j, e)
end do i_aiajaiajej
end do a_aiajaiajej
end do j_aiajaiajej
end do e_aiajaiajej
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == a, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibiakaibi: do k = nocc0, nocc1
b_aibiakaibi: do b = nvirt0, nvirt1
a_aibiakaibi: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakaibi: do i = nocc0, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakaibi(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibiakaibi
end do a_aibiakaibi
end do b_aibiakaibi
end do k_aibiakaibi
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == a, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibiakaibk: do k = nocc0, nocc1
b_aibiakaibk: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibiakaibk: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakaibk: do i = nocc0, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakaibk(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibiakaibk
end do a_aibiakaibk
end do b_aibiakaibk
end do k_aibiakaibk
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, e == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajaibi: do b = nvirt0, nvirt1
j_aibjajaibi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajaibi: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajaibi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajaibi(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjajaibi
end do a_aibjajaibi
end do j_aibjajaibi
end do b_aibjajaibi
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
b_aibjajaibj: do b = nvirt0, nvirt1
j_aibjajaibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajaibj: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajaibj: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajaibj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjajaibj
end do a_aibjajaibj
end do j_aibjajaibj
end do b_aibjajaibj
!
! Elementary loop 7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == a, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiaialbi: do l = nocc0, nocc1
b_aibiaialbi: do b = nvirt0, nvirt1
a_aibiaialbi: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibiaialbi: do i = l + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaialbi(eorb, t2, t1, nocc, nactive, a, i, b, l)
end do i_aibiaialbi
end do a_aibiaialbi
end do b_aibiaialbi
end do l_aibiaialbi
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == a, e == b, j == i, k == i, m == l
! No equalities independent of the above can hold.
!
l_aibiaialbl: do l = nocc0, nocc1
b_aibiaialbl: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibiaialbl: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibiaialbl: do i = l + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaialbl(eorb, t2, t1, nocc, nactive, a, i, b, l)
end do i_aibiaialbl
end do a_aibiaialbl
end do b_aibiaialbl
end do l_aibiaialbl
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaiajbi: do b = nvirt0, nvirt1
j_aibjaiajbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaiajbi: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjaiajbi: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaiajbi(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjaiajbi
end do a_aibjaiajbi
end do j_aibjaiajbi
end do b_aibjaiajbi
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
b_aibjaiajbj: do b = nvirt0, nvirt1
j_aibjaiajbj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaiajbj: do a = b + 1, nvirt1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjaiajbj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaiajbj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjaiajbj
end do a_aibjaiajbj
end do j_aibjaiajbj
end do b_aibjaiajbj
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, e == a, d == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiaickciai: do c = nvirt0, nvirt1
k_aiaickciai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a_aiaickciai: do a = nvirt0, c - 1
i_aiaickciai: do i = nocc0, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaickciai(eorb, t2, t1, nocc, nactive, a, i, c, k)
end do i_aiaickciai
end do a_aiaickciai
end do k_aiaickciai
end do c_aiaickciai
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, l
! Equalities: b == a, e == a, d == c, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aiaiciclai: do l = nocc0, nocc1
c_aiaiciclai: do c = nvirt0, nvirt1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
a_aiaiciclai: do a = nvirt0, c - 1
i_aiaiciclai: do i = l + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaiciclai(eorb, t2, t1, nocc, nactive, a, i, c, l)
end do i_aiaiciclai
end do a_aiaiciclai
end do c_aiaiciclai
end do l_aiaiciclai
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, e == a, d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajcicjai: do c = nvirt0, nvirt1
j_aiajcicjai: do j = nocc0, nocc1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcicjai: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcicjai: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcicjai(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcicjai
end do a_aiajcicjai
end do j_aiajcicjai
end do c_aiajcicjai
!
! Elementary loop 14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, e == a, d == c, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcicjaj: do c = nvirt0, nvirt1
j_aiajcicjaj: do j = nocc0, nocc1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcicjaj: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcicjaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcicjaj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcicjaj
end do a_aiajcicjaj
end do j_aiajcicjaj
end do c_aiajcicjaj
!
! Elementary loop 15
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, m
! Equalities: b == a, c == a, e == d, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aiaiaididm: do m = nocc0, nocc1
d_aiaiaididm: do d = nvirt0, nvirt1
em = (d - nvirt0) * nocc + (m - nocc0) + 1
a_aiaiaididm: do a = d + 1, nvirt1
i_aiaiaididm: do i = m + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaiaididm(eorb, t2, t1, nocc, nactive, a, i, d, m)
end do i_aiaiaididm
end do a_aiaiaididm
end do d_aiaiaididm
end do m_aiaiaididm
!
! Elementary loop 16
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, e == d, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajaididj: do d = nvirt0, nvirt1
j_aiajaididj: do j = nocc0, nocc1
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajaididj: do a = d + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajaididj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajaididj(eorb, t2, t1, nocc, nactive, a, i, j, d)
end do i_aiajaididj
end do a_aiajaididj
end do j_aiajaididj
end do d_aiajaididj
!
! Elementary loop 17
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, e == d, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aiajajdidj: do d = nvirt0, nvirt1
j_aiajajdidj: do j = nocc0, nocc1
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajajdidj: do a = d + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajajdidj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajajdidj(eorb, t2, t1, nocc, nactive, a, i, j, d)
end do i_aiajajdidj
end do a_aiajajdidj
end do j_aiajajdidj
end do d_aiajajdidj
!
! Elementary loop 18
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, l
! Equalities: b == a, c == a, e == d, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aiaiaidldi: do d = nvirt0, nvirt1
l_aiaiaidldi: do l = nocc0, nocc1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
a_aiaiaidldi: do a = d + 1, nvirt1
i_aiaiaidldi: do i = nocc0, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaiaidldi(eorb, t2, t1, nocc, nactive, a, i, d, l)
end do i_aiaiaidldi
end do a_aiaiaidldi
end do l_aiaiaidldi
end do d_aiaiaidldi
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, m
! Equalities: c == a, d == b, e == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibiaibibm: do m = nocc0, nocc1
b_aibiaibibm: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a_aibiaibibm: do a = b + 1, nvirt1
i_aibiaibibm: do i = m + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaibibm(eorb, t2, t1, nocc, nactive, a, i, b, m)
end do i_aibiaibibm
end do a_aibiaibibm
end do b_aibiaibibm
end do m_aibiaibibm
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibiakbibk: do k = nocc0, nocc1
b_aibiakbibk: do b = nvirt0, nvirt1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibiakbibk: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakbibk: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakbibk(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibiakbibk
end do a_aibiakbibk
end do b_aibiakbibk
end do k_aibiakbibk
!
! Elementary loop 21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
b_aibjaibibj: do b = nvirt0, nvirt1
j_aibjaibibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaibibj: do a = b + 1, nvirt1
i_aibjaibibj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaibibj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjaibibj
end do a_aibjaibibj
end do j_aibjaibibj
end do b_aibjaibibj
!
! Elementary loop 22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
b_aibjajbibj: do b = nvirt0, nvirt1
j_aibjajbibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajbibj: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajbibj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajbibj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjajbibj
end do a_aibjajbibj
end do j_aibjajbibj
end do b_aibjajbibj
!
! Elementary loop 23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == b, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiaiblbi: do l = nocc0, nocc1
b_aibiaiblbi: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibiaiblbi: do a = b + 1, nvirt1
i_aibiaiblbi: do i = nocc0, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiaiblbi(eorb, t2, t1, nocc, nactive, a, i, b, l)
end do i_aibiaiblbi
end do a_aibiaiblbi
end do b_aibiaiblbi
end do l_aibiaiblbi
!
! Elementary loop 24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, e == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibiakbkbi: do k = nocc0, nocc1
b_aibiakbkbi: do b = nvirt0, nvirt1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibiakbkbi: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakbkbi: do i = nocc0, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibiakbkbi(eorb, t2, t1, nocc, nactive, a, i, b, k)
end do i_aibiakbkbi
end do a_aibiakbkbi
end do b_aibiakbkbi
end do k_aibiakbkbi
!
! Elementary loop 25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibjbi: do b = nvirt0, nvirt1
j_aibjaibjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjaibjbi: do a = b + 1, nvirt1
i_aibjaibjbi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjaibjbi(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjaibjbi
end do a_aibjaibjbi
end do j_aibjaibjbi
end do b_aibjaibjbi
!
! Elementary loop 26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, e == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
b_aibjajbjbi: do b = nvirt0, nvirt1
j_aibjajbjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjajbjbi: do a = b + 1, nvirt1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajbjbi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aibjajbjbi(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibjajbjbi
end do a_aibjajbjbi
end do j_aibjajbjbi
end do b_aibjajbjbi
!
! Elementary loop 27
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, m
! Equalities: b == a, d == a, e == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aiaiciaiam: do m = nocc0, nocc1
c_aiaiciaiam: do c = nvirt0, nvirt1
a_aiaiciaiam: do a = nvirt0, c - 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiaiciaiam: do i = m + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaiciaiam(eorb, t2, t1, nocc, nactive, a, i, c, m)
end do i_aiaiciaiam
end do a_aiaiciaiam
end do c_aiaiciaiam
end do m_aiaiciaiam
!
! Elementary loop 28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, e == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiaickaiak: do c = nvirt0, nvirt1
k_aiaickaiak: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a_aiaickaiak: do a = nvirt0, c - 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiaickaiak: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaickaiak(eorb, t2, t1, nocc, nactive, a, i, c, k)
end do i_aiaickaiak
end do a_aiaickaiak
end do k_aiaickaiak
end do c_aiaickaiak
!
! Elementary loop 29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajciaiaj: do c = nvirt0, nvirt1
j_aiajciaiaj: do j = nocc0, nocc1
a_aiajciaiaj: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajciaiaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajciaiaj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajciaiaj
end do a_aiajciaiaj
end do j_aiajciaiaj
end do c_aiajciaiaj
!
! Elementary loop 30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjaiaj: do c = nvirt0, nvirt1
j_aiajcjaiaj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjaiaj: do a = nvirt0, c - 1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcjaiaj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiajcjaiaj(eorb, t2, t1, nocc, nactive, a, i, j, c)
end do i_aiajcjaiaj
end do a_aiajcjaiaj
end do j_aiajcjaiaj
end do c_aiajcjaiaj
!
! Elementary loop 31
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, l
! Equalities: b == a, d == a, e == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aiaicialai: do l = nocc0, nocc1
c_aiaicialai: do c = nvirt0, nvirt1
a_aiaicialai: do a = nvirt0, c - 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiaicialai: do i = nocc0, l - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaicialai(eorb, t2, t1, nocc, nactive, a, i, c, l)
end do i_aiaicialai
end do a_aiaicialai
end do c_aiaicialai
end do l_aiaicialai
!
! Elementary loop 32
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, e == a, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiaickakai: do c = nvirt0, nvirt1
k_aiaickakai: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a_aiaickakai: do a = nvirt0, c - 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiaickakai: do i = nocc0, k - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + mu3(ck, dl, em)
jac(ibra, iket) = eom_cc3_23_trans_aiaickakai(eorb, t2, t1, nocc, nactive, a, i, c, k)
end do i_aiaickakai
end do a_aiaickakai
end do k_aiaickakai
end do c_aiaickakai
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
end subroutine ccjac_23_part8
end module ccjac_block_23_part8
