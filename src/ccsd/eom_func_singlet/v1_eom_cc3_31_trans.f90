module v1_eom_cc3_31_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2013-02-27 23:39:38 UTC.
    !
    contains
    
    function v1_eom_cc3_31_trans_aibjckbl(t2, nocc, nactive, a, i, j, c, k, l) 
    double precision :: v1_eom_cc3_31_trans_aibjckbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,k) * tvoov(a, i, l, e)
term(1) = term(1) + t2(c,e,k,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,k) * tvoov(c, j, l, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(c, k, l, e)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(c, e, l, k)
term(5) = term(5) + t2(a,e,i,k) * tvvoo(c, e, l, j)
term(6) = term(6) + t2(c,e,j,i) * tvvoo(a, e, l, k)
term(7) = term(7) + t2(c,e,k,i) * tvvoo(a, e, l, j)
end do 

term(1) = -term(1) 
term(3) = -term(3) 
term(5) = -term(5) 
term(7) = -term(7) 

do m = 1, nocc 
term(8) = term(8) + t2(a,c,i,m) * toooo(m, j, l, k)
term(9) = term(9) + t2(a,c,i,m) * toooo(m, k, l, j)
term(10) = term(10) + t2(a,c,m,j) * toooo(m, i, l, k)
term(11) = term(11) + t2(a,c,m,k) * toooo(m, i, l, j)
end do 

term(8) = -term(8) 
term(10) = -term(10) 


    v1_eom_cc3_31_trans_aibjckbl = 0.d+0
    do s = 0, 11
    v1_eom_cc3_31_trans_aibjckbl = v1_eom_cc3_31_trans_aibjckbl + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckbl
    function v1_eom_cc3_31_trans_aibjckal(t2, nocc, nactive, i, b, j, c, k, l) 
    double precision :: v1_eom_cc3_31_trans_aibjckal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * tvoov(b, k, l, e)
term(1) = term(1) + t2(c,e,k,i) * tvoov(b, j, l, e)
term(2) = term(2) + t2(b,e,k,i) * tvoov(c, j, l, e)
term(3) = term(3) + t2(b,e,j,i) * tvoov(c, k, l, e)
term(4) = term(4) + t2(b,e,k,j) * tvvoo(c, e, l, i)
term(5) = term(5) + t2(b,e,j,k) * tvvoo(c, e, l, i)
term(6) = term(6) + t2(c,e,j,k) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(c,e,k,j) * tvvoo(b, e, l, i)
end do 

term(1) = -term(1) 
term(3) = -term(3) 
term(5) = -term(5) 
term(7) = -term(7) 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,k,m) * toooo(m, j, l, i)
term(9) = term(9) + t2(b,c,j,m) * toooo(m, k, l, i)
term(10) = term(10) + t2(b,c,m,j) * toooo(m, k, l, i)
term(11) = term(11) + t2(b,c,m,k) * toooo(m, j, l, i)
end do 

term(8) = -term(8) 
term(10) = -term(10) 


    v1_eom_cc3_31_trans_aibjckal = 0.d+0
    do s = 0, 11
    v1_eom_cc3_31_trans_aibjckal = v1_eom_cc3_31_trans_aibjckal + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckal
    function v1_eom_cc3_31_trans_aibjckcl(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: v1_eom_cc3_31_trans_aibjckcl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * tvoov(a, i, l, e)
term(1) = term(1) + t2(b,e,j,k) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(b, k, l, e)
term(3) = term(3) + t2(a,e,i,k) * tvoov(b, j, l, e)
term(4) = term(4) + t2(a,e,i,k) * tvvoo(b, e, l, j)
term(5) = term(5) + t2(a,e,i,j) * tvvoo(b, e, l, k)
term(6) = term(6) + t2(b,e,k,i) * tvvoo(a, e, l, j)
term(7) = term(7) + t2(b,e,j,i) * tvvoo(a, e, l, k)
end do 

term(1) = -term(1) 
term(3) = -term(3) 
term(5) = -term(5) 
term(7) = -term(7) 

do m = 1, nocc 
term(8) = term(8) + t2(a,b,i,m) * toooo(m, k, l, j)
term(9) = term(9) + t2(a,b,i,m) * toooo(m, j, l, k)
term(10) = term(10) + t2(a,b,m,k) * toooo(m, i, l, j)
term(11) = term(11) + t2(a,b,m,j) * toooo(m, i, l, k)
end do 

term(8) = -term(8) 
term(10) = -term(10) 


    v1_eom_cc3_31_trans_aibjckcl = 0.d+0
    do s = 0, 11
    v1_eom_cc3_31_trans_aibjckcl = v1_eom_cc3_31_trans_aibjckcl + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckcl
    function v1_eom_cc3_31_trans_aibjckdk(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: v1_eom_cc3_31_trans_aibjckdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, d)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, d, b, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, d, a, e)
end do 

term(0) = -term(0) 
term(2) = -term(2) 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,j,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(b,c,m,j) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvoov(b, j, m, d)
term(7) = term(7) + t2(a,b,i,m) * tvoov(c, j, m, d)
term(8) = term(8) + t2(a,c,i,m) * tvvoo(b, d, m, j)
term(9) = term(9) + t2(a,b,i,m) * tvvoo(c, d, m, j)
term(10) = term(10) + t2(a,c,m,j) * tvvoo(b, d, m, i)
term(11) = term(11) + t2(a,b,m,j) * tvvoo(c, d, m, i)
end do 

term(4) = -term(4) 
term(6) = -term(6) 
term(9) = -term(9) 
term(11) = -term(11) 


    v1_eom_cc3_31_trans_aibjckdk = 0.d+0
    do s = 0, 11
    v1_eom_cc3_31_trans_aibjckdk = v1_eom_cc3_31_trans_aibjckdk + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckdk
    function v1_eom_cc3_31_trans_aibjckdj(t2, nocc, nactive, a, i, b, c, k, d) 
    double precision :: v1_eom_cc3_31_trans_aibjckdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, b, d)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(c, d, b, e)
term(2) = term(2) + t2(c,e,k,i) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(b,e,k,i) * read_ftvvvv(c, d, a, e)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,k,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(b,c,m,k) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvoov(b, k, m, d)
term(7) = term(7) + t2(a,b,i,m) * tvoov(c, k, m, d)
term(8) = term(8) + t2(a,c,i,m) * tvvoo(b, d, m, k)
term(9) = term(9) + t2(a,b,i,m) * tvvoo(c, d, m, k)
term(10) = term(10) + t2(a,c,m,k) * tvvoo(b, d, m, i)
term(11) = term(11) + t2(a,b,m,k) * tvvoo(c, d, m, i)
end do 

term(5) = -term(5) 
term(7) = -term(7) 
term(8) = -term(8) 
term(10) = -term(10) 


    v1_eom_cc3_31_trans_aibjckdj = 0.d+0
    do s = 0, 11
    v1_eom_cc3_31_trans_aibjckdj = v1_eom_cc3_31_trans_aibjckdj + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckdj


    function v1el_eom_cc3_31_trans_aibjckdj_aibckd(t2, nocc, nactive, a, i, b, c, k, d)
    real(F64) :: v1el_eom_cc3_31_trans_aibjckdj_aibckd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0
    do e = nocc + 1, nactive
term(0) = term(0) + t2(c,e,i,k) * read_ftvvvv(b, e, a, d)
term(1) = term(1) + t2(c,e,k,i) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(b,e,k,i) * read_ftvvvv(c, e, a, d)
term(3) = term(3) + t2(b,e,i,k) * read_ftvvvv(c, e, a, d)
! term(4) = term(4) + t2(c,e,i,k) * read_ftvvvv(b, d, a, e)
! term(5) = term(5) + t2(c,e,k,i) * read_ftvvvv(b, d, a, e)
! term(6) = term(6) + t2(b,e,i,k) * read_ftvvvv(c, d, a, e)
! term(7) = term(7) + t2(b,e,k,i) * read_ftvvvv(c, d, a, e)
! term(8) = term(8) + t2(a,e,k,i) * read_ftvvvv(c, e, b, d)
! term(9) = term(9) + t2(a,e,k,i) * read_ftvvvv(c, d, b, e)
! term(10) = term(10) + t2(a,e,i,k) * read_ftvvvv(c, e, b, d)
! term(11) = term(11) + t2(a,e,i,k) * read_ftvvvv(c, d, b, e)
end do


term(0) = term(0) * (2.0d+0)
term(1) = term(1) * (-4.0d+0)
term(2) = term(2) * (2.0d+0)
term(3) = term(3) * (-4.0d+0)
! term(4) = term(4) * (-4.0d+0)
! term(5) = term(5) * (8.0d+0)
! term(6) = term(6) * (2.0d+0)
! term(7) = term(7) * (-4.0d+0)
! term(8) = term(8) * (-4.0d+0)
! term(9) = term(9) * (2.0d+0)
! term(10) = term(10) * (8.0d+0)
! term(11) = term(11) * (-4.0d+0)

do m = 1, nocc
term(12) = term(12) + t2(b,c,m,i) * tvoov(a, k, m, d)
term(13) = term(13) + t2(b,c,m,k) * tvoov(a, i, m, d)
term(14) = term(14) + t2(b,c,i,m) * tvoov(a, k, m, d)
term(15) = term(15) + t2(b,c,k,m) * tvoov(a, i, m, d)
term(16) = term(16) + t2(a,c,m,k) * tvoov(b, i, m, d)
term(17) = term(17) + t2(a,b,m,k) * tvoov(c, i, m, d)
term(18) = term(18) + t2(a,b,k,m) * tvoov(c, i, m, d)
term(19) = term(19) + t2(a,c,k,m) * tvoov(b, i, m, d)
! term(20) = term(20) + t2(a,c,m,i) * tvoov(b, k, m, d)
! term(21) = term(21) + t2(a,b,m,i) * tvoov(c, k, m, d)
! term(22) = term(22) + t2(a,c,i,m) * tvoov(b, k, m, d)
! term(23) = term(23) + t2(a,b,i,m) * tvoov(c, k, m, d)
! term(24) = term(24) + t2(b,c,m,i) * tvvoo(a, d, m, k)
! term(25) = term(25) + t2(b,c,m,k) * tvvoo(a, d, m, i)
! term(26) = term(26) + t2(b,c,k,m) * tvvoo(a, d, m, i)
! term(27) = term(27) + t2(b,c,i,m) * tvvoo(a, d, m, k)
! term(28) = term(28) + t2(a,c,m,i) * tvvoo(b, d, m, k)
! term(29) = term(29) + t2(a,c,m,k) * tvvoo(b, d, m, i)
! term(30) = term(30) + t2(a,b,m,i) * tvvoo(c, d, m, k)
! term(31) = term(31) + t2(a,b,m,k) * tvvoo(c, d, m, i)
! term(32) = term(32) + t2(a,c,k,m) * tvvoo(b, d, m, i)
! term(33) = term(33) + t2(a,b,k,m) * tvvoo(c, d, m, i)
! term(34) = term(34) + t2(a,c,i,m) * tvvoo(b, d, m, k)
! term(35) = term(35) + t2(a,b,i,m) * tvvoo(c, d, m, k)
end do
term(12) = term(12) * (4.0d+0)
term(13) = term(13) * (-8.0d+0)
term(14) = term(14) * (-2.0d+0)
term(15) = term(15) * (4.0d+0)
term(16) = term(16) * (4.0d+0)
term(17) = term(17) * (-2.0d+0)
term(18) = term(18) * (4.0d+0)
term(19) = term(19) * (-2.0d+0)
! term(20) = term(20) * (-2.0d+0)
! term(21) = term(21) * (4.0d+0)
! term(22) = term(22) * (4.0d+0)
! term(23) = term(23) * (-8.0d+0)
! term(24) = term(24) * (-2.0d+0)
! term(25) = term(25) * (4.0d+0)
! term(26) = term(26) * (-2.0d+0)
! term(27) = term(27) * (4.0d+0)
! term(28) = term(28) * (4.0d+0)
! term(29) = term(29) * (-8.0d+0)
! term(30) = term(30) * (-2.0d+0)
! term(31) = term(31) * (4.0d+0)
! term(32) = term(32) * (4.0d+0)
! term(33) = term(33) * (-2.0d+0)
! term(34) = term(34) * (-8.0d+0)
! term(35) = term(35) * (4.0d+0)


    v1el_eom_cc3_31_trans_aibjckdj_aibckd = 0.d+0
    do s = 0, 35
    v1el_eom_cc3_31_trans_aibjckdj_aibckd = v1el_eom_cc3_31_trans_aibjckdj_aibckd + term(s)
    end do
end function v1el_eom_cc3_31_trans_aibjckdj_aibckd

    function v1_eom_cc3_31_trans_aibjckdi(t2, nocc, nactive, a, b, j, c, k, d) 
    double precision :: v1_eom_cc3_31_trans_aibjckdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(c, e, a, d)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(b, e, a, d)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(b, e, a, d)
end do 

term(0) = -term(0) 
term(2) = -term(2) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,m,j) * tvoov(b, k, m, d)
term(5) = term(5) + t2(a,c,m,k) * tvoov(b, j, m, d)
term(6) = term(6) + t2(a,b,m,k) * tvoov(c, j, m, d)
term(7) = term(7) + t2(a,b,m,j) * tvoov(c, k, m, d)
term(8) = term(8) + t2(b,c,k,m) * tvvoo(a, d, m, j)
term(9) = term(9) + t2(b,c,j,m) * tvvoo(a, d, m, k)
term(10) = term(10) + t2(b,c,m,j) * tvvoo(a, d, m, k)
term(11) = term(11) + t2(b,c,m,k) * tvvoo(a, d, m, j)
end do 

term(5) = -term(5) 
term(7) = -term(7) 
term(9) = -term(9) 
term(11) = -term(11) 


    v1_eom_cc3_31_trans_aibjckdi = 0.d+0
    do s = 0, 11
    v1_eom_cc3_31_trans_aibjckdi = v1_eom_cc3_31_trans_aibjckdi + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckdi
    function v1_eom_cc3_31_trans_aibjckbk(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_31_trans_aibjckbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, b)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, b, b, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, b, a, e)
term(4) = term(4) + t2(c,e,j,k) * tvoov(a, i, k, e)
term(5) = term(5) + t2(c,e,k,j) * tvoov(a, i, k, e)
term(6) = term(6) + t2(a,e,i,k) * tvoov(c, j, k, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(c, k, k, e)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(c, e, k, k)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(c, e, k, j)
term(10) = term(10) + t2(c,e,j,i) * tvvoo(a, e, k, k)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(a, e, k, j)
end do 

term(0) = -term(0) 
term(2) = -term(2) 
term(5) = -term(5) 
term(7) = -term(7) 
term(9) = -term(9) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,j,m) * tvoov(a, i, m, b)
term(13) = term(13) + t2(b,c,m,j) * tvoov(a, i, m, b)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, j, m, b)
term(15) = term(15) + t2(a,b,i,m) * tvoov(c, j, m, b)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(b, b, m, j)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(c, b, m, j)
term(18) = term(18) + t2(a,c,m,j) * tvvoo(b, b, m, i)
term(19) = term(19) + t2(a,b,m,j) * tvvoo(c, b, m, i)
term(20) = term(20) + t2(a,c,i,m) * toooo(m, j, k, k)
term(21) = term(21) + t2(a,c,i,m) * toooo(m, k, k, j)
term(22) = term(22) + t2(a,c,m,j) * toooo(m, i, k, k)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, i, k, j)
end do 

term(12) = -term(12) 
term(14) = -term(14) 
term(17) = -term(17) 
term(19) = -term(19) 
term(20) = -term(20) 
term(22) = -term(22) 


    v1_eom_cc3_31_trans_aibjckbk = 0.d+0
    do s = 0, 23
    v1_eom_cc3_31_trans_aibjckbk = v1_eom_cc3_31_trans_aibjckbk + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckbk
    function v1_eom_cc3_31_trans_aibjckbj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_31_trans_aibjckbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, b, b)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(c, b, b, e)
term(2) = term(2) + t2(c,e,k,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(b,e,k,i) * read_ftvvvv(c, b, a, e)
term(4) = term(4) + t2(c,e,j,k) * tvoov(a, i, j, e)
term(5) = term(5) + t2(c,e,k,j) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,i,k) * tvoov(c, j, j, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(c, k, j, e)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(c, e, j, k)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(c, e, j, j)
term(10) = term(10) + t2(c,e,j,i) * tvvoo(a, e, j, k)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(a, e, j, j)
end do 

term(1) = -term(1) 
term(3) = -term(3) 
term(5) = -term(5) 
term(7) = -term(7) 
term(9) = -term(9) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,k,m) * tvoov(a, i, m, b)
term(13) = term(13) + t2(b,c,m,k) * tvoov(a, i, m, b)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, k, m, b)
term(15) = term(15) + t2(a,b,i,m) * tvoov(c, k, m, b)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(b, b, m, k)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(c, b, m, k)
term(18) = term(18) + t2(a,c,m,k) * tvvoo(b, b, m, i)
term(19) = term(19) + t2(a,b,m,k) * tvvoo(c, b, m, i)
term(20) = term(20) + t2(a,c,i,m) * toooo(m, j, j, k)
term(21) = term(21) + t2(a,c,i,m) * toooo(m, k, j, j)
term(22) = term(22) + t2(a,c,m,j) * toooo(m, i, j, k)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, i, j, j)
end do 

term(13) = -term(13) 
term(15) = -term(15) 
term(16) = -term(16) 
term(18) = -term(18) 
term(20) = -term(20) 
term(22) = -term(22) 


    v1_eom_cc3_31_trans_aibjckbj = 0.d+0
    do s = 0, 23
    v1_eom_cc3_31_trans_aibjckbj = v1_eom_cc3_31_trans_aibjckbj + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckbj
    function v1_eom_cc3_31_trans_aibjckbi(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_31_trans_aibjckbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(c, e, a, b)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(c, e, a, b)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(b, e, a, b)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(b, e, a, b)
term(4) = term(4) + t2(c,e,j,k) * tvoov(a, i, i, e)
term(5) = term(5) + t2(c,e,k,j) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,i,k) * tvoov(c, j, i, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(c, k, i, e)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(c, e, i, k)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(c, e, i, j)
term(10) = term(10) + t2(c,e,j,i) * tvvoo(a, e, i, k)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(a, e, i, j)
end do 

term(0) = -term(0) 
term(2) = -term(2) 
term(5) = -term(5) 
term(7) = -term(7) 
term(9) = -term(9) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,m,j) * tvoov(b, k, m, b)
term(13) = term(13) + t2(a,c,m,k) * tvoov(b, j, m, b)
term(14) = term(14) + t2(a,b,m,k) * tvoov(c, j, m, b)
term(15) = term(15) + t2(a,b,m,j) * tvoov(c, k, m, b)
term(16) = term(16) + t2(b,c,k,m) * tvvoo(a, b, m, j)
term(17) = term(17) + t2(b,c,j,m) * tvvoo(a, b, m, k)
term(18) = term(18) + t2(b,c,m,j) * tvvoo(a, b, m, k)
term(19) = term(19) + t2(b,c,m,k) * tvvoo(a, b, m, j)
term(20) = term(20) + t2(a,c,i,m) * toooo(m, j, i, k)
term(21) = term(21) + t2(a,c,i,m) * toooo(m, k, i, j)
term(22) = term(22) + t2(a,c,m,j) * toooo(m, i, i, k)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, i, i, j)
end do 

term(13) = -term(13) 
term(15) = -term(15) 
term(17) = -term(17) 
term(19) = -term(19) 
term(20) = -term(20) 
term(22) = -term(22) 


    v1_eom_cc3_31_trans_aibjckbi = 0.d+0
    do s = 0, 23
    v1_eom_cc3_31_trans_aibjckbi = v1_eom_cc3_31_trans_aibjckbi + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckbi
    function v1_eom_cc3_31_trans_aibjckak(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_31_trans_aibjckak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, a)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, a, b, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, a, a, e)
term(4) = term(4) + t2(c,e,j,i) * tvoov(b, k, k, e)
term(5) = term(5) + t2(c,e,k,i) * tvoov(b, j, k, e)
term(6) = term(6) + t2(b,e,k,i) * tvoov(c, j, k, e)
term(7) = term(7) + t2(b,e,j,i) * tvoov(c, k, k, e)
term(8) = term(8) + t2(b,e,k,j) * tvvoo(c, e, k, i)
term(9) = term(9) + t2(b,e,j,k) * tvvoo(c, e, k, i)
term(10) = term(10) + t2(c,e,j,k) * tvvoo(b, e, k, i)
term(11) = term(11) + t2(c,e,k,j) * tvvoo(b, e, k, i)
end do 

term(0) = -term(0) 
term(2) = -term(2) 
term(5) = -term(5) 
term(7) = -term(7) 
term(9) = -term(9) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,j,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(b,c,m,j) * tvoov(a, i, m, a)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, j, m, a)
term(15) = term(15) + t2(a,b,i,m) * tvoov(c, j, m, a)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(b, a, m, j)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(c, a, m, j)
term(18) = term(18) + t2(a,c,m,j) * tvvoo(b, a, m, i)
term(19) = term(19) + t2(a,b,m,j) * tvvoo(c, a, m, i)
term(20) = term(20) + t2(b,c,k,m) * toooo(m, j, k, i)
term(21) = term(21) + t2(b,c,j,m) * toooo(m, k, k, i)
term(22) = term(22) + t2(b,c,m,j) * toooo(m, k, k, i)
term(23) = term(23) + t2(b,c,m,k) * toooo(m, j, k, i)
end do 

term(12) = -term(12) 
term(14) = -term(14) 
term(17) = -term(17) 
term(19) = -term(19) 
term(20) = -term(20) 
term(22) = -term(22) 


    v1_eom_cc3_31_trans_aibjckak = 0.d+0
    do s = 0, 23
    v1_eom_cc3_31_trans_aibjckak = v1_eom_cc3_31_trans_aibjckak + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckak
    function v1_eom_cc3_31_trans_aibjckaj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_31_trans_aibjckaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, b, a)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(c, a, b, e)
term(2) = term(2) + t2(c,e,k,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(b,e,k,i) * read_ftvvvv(c, a, a, e)
term(4) = term(4) + t2(c,e,j,i) * tvoov(b, k, j, e)
term(5) = term(5) + t2(c,e,k,i) * tvoov(b, j, j, e)
term(6) = term(6) + t2(b,e,k,i) * tvoov(c, j, j, e)
term(7) = term(7) + t2(b,e,j,i) * tvoov(c, k, j, e)
term(8) = term(8) + t2(b,e,k,j) * tvvoo(c, e, j, i)
term(9) = term(9) + t2(b,e,j,k) * tvvoo(c, e, j, i)
term(10) = term(10) + t2(c,e,j,k) * tvvoo(b, e, j, i)
term(11) = term(11) + t2(c,e,k,j) * tvvoo(b, e, j, i)
end do 

term(1) = -term(1) 
term(3) = -term(3) 
term(5) = -term(5) 
term(7) = -term(7) 
term(9) = -term(9) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,k,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(b,c,m,k) * tvoov(a, i, m, a)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, k, m, a)
term(15) = term(15) + t2(a,b,i,m) * tvoov(c, k, m, a)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(b, a, m, k)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(c, a, m, k)
term(18) = term(18) + t2(a,c,m,k) * tvvoo(b, a, m, i)
term(19) = term(19) + t2(a,b,m,k) * tvvoo(c, a, m, i)
term(20) = term(20) + t2(b,c,k,m) * toooo(m, j, j, i)
term(21) = term(21) + t2(b,c,j,m) * toooo(m, k, j, i)
term(22) = term(22) + t2(b,c,m,j) * toooo(m, k, j, i)
term(23) = term(23) + t2(b,c,m,k) * toooo(m, j, j, i)
end do 

term(13) = -term(13) 
term(15) = -term(15) 
term(16) = -term(16) 
term(18) = -term(18) 
term(20) = -term(20) 
term(22) = -term(22) 


    v1_eom_cc3_31_trans_aibjckaj = 0.d+0
    do s = 0, 23
    v1_eom_cc3_31_trans_aibjckaj = v1_eom_cc3_31_trans_aibjckaj + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckaj
    function v1_eom_cc3_31_trans_aibjckai(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_31_trans_aibjckai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(c, e, a, a)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(b, e, a, a)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(b, e, a, a)
term(4) = term(4) + t2(c,e,j,i) * tvoov(b, k, i, e)
term(5) = term(5) + t2(c,e,k,i) * tvoov(b, j, i, e)
term(6) = term(6) + t2(b,e,k,i) * tvoov(c, j, i, e)
term(7) = term(7) + t2(b,e,j,i) * tvoov(c, k, i, e)
term(8) = term(8) + t2(b,e,k,j) * tvvoo(c, e, i, i)
term(9) = term(9) + t2(b,e,j,k) * tvvoo(c, e, i, i)
term(10) = term(10) + t2(c,e,j,k) * tvvoo(b, e, i, i)
term(11) = term(11) + t2(c,e,k,j) * tvvoo(b, e, i, i)
end do 

term(0) = -term(0) 
term(2) = -term(2) 
term(5) = -term(5) 
term(7) = -term(7) 
term(9) = -term(9) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,m,j) * tvoov(b, k, m, a)
term(13) = term(13) + t2(a,c,m,k) * tvoov(b, j, m, a)
term(14) = term(14) + t2(a,b,m,k) * tvoov(c, j, m, a)
term(15) = term(15) + t2(a,b,m,j) * tvoov(c, k, m, a)
term(16) = term(16) + t2(b,c,k,m) * tvvoo(a, a, m, j)
term(17) = term(17) + t2(b,c,j,m) * tvvoo(a, a, m, k)
term(18) = term(18) + t2(b,c,m,j) * tvvoo(a, a, m, k)
term(19) = term(19) + t2(b,c,m,k) * tvvoo(a, a, m, j)
term(20) = term(20) + t2(b,c,k,m) * toooo(m, j, i, i)
term(21) = term(21) + t2(b,c,j,m) * toooo(m, k, i, i)
term(22) = term(22) + t2(b,c,m,j) * toooo(m, k, i, i)
term(23) = term(23) + t2(b,c,m,k) * toooo(m, j, i, i)
end do 

term(13) = -term(13) 
term(15) = -term(15) 
term(17) = -term(17) 
term(19) = -term(19) 
term(20) = -term(20) 
term(22) = -term(22) 


    v1_eom_cc3_31_trans_aibjckai = 0.d+0
    do s = 0, 23
    v1_eom_cc3_31_trans_aibjckai = v1_eom_cc3_31_trans_aibjckai + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckai
    function v1_eom_cc3_31_trans_aibjckck(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_31_trans_aibjckck 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, c)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, c, b, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, c, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, c, a, e)
term(4) = term(4) + t2(b,e,k,j) * tvoov(a, i, k, e)
term(5) = term(5) + t2(b,e,j,k) * tvoov(a, i, k, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(b, k, k, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(b, j, k, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(b, e, k, j)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(b, e, k, k)
term(10) = term(10) + t2(b,e,k,i) * tvvoo(a, e, k, j)
term(11) = term(11) + t2(b,e,j,i) * tvvoo(a, e, k, k)
end do 

term(0) = -term(0) 
term(2) = -term(2) 
term(5) = -term(5) 
term(7) = -term(7) 
term(9) = -term(9) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,j,m) * tvoov(a, i, m, c)
term(13) = term(13) + t2(b,c,m,j) * tvoov(a, i, m, c)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, j, m, c)
term(15) = term(15) + t2(a,b,i,m) * tvoov(c, j, m, c)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(b, c, m, j)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(c, c, m, j)
term(18) = term(18) + t2(a,c,m,j) * tvvoo(b, c, m, i)
term(19) = term(19) + t2(a,b,m,j) * tvvoo(c, c, m, i)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, k, k, j)
term(21) = term(21) + t2(a,b,i,m) * toooo(m, j, k, k)
term(22) = term(22) + t2(a,b,m,k) * toooo(m, i, k, j)
term(23) = term(23) + t2(a,b,m,j) * toooo(m, i, k, k)
end do 

term(12) = -term(12) 
term(14) = -term(14) 
term(17) = -term(17) 
term(19) = -term(19) 
term(20) = -term(20) 
term(22) = -term(22) 


    v1_eom_cc3_31_trans_aibjckck = 0.d+0
    do s = 0, 23
    v1_eom_cc3_31_trans_aibjckck = v1_eom_cc3_31_trans_aibjckck + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckck
    function v1_eom_cc3_31_trans_aibjckcj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_31_trans_aibjckcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, b, c)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(c, c, b, e)
term(2) = term(2) + t2(c,e,k,i) * read_ftvvvv(b, c, a, e)
term(3) = term(3) + t2(b,e,k,i) * read_ftvvvv(c, c, a, e)
term(4) = term(4) + t2(b,e,k,j) * tvoov(a, i, j, e)
term(5) = term(5) + t2(b,e,j,k) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(b, k, j, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(b, j, j, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(b, e, j, j)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(b, e, j, k)
term(10) = term(10) + t2(b,e,k,i) * tvvoo(a, e, j, j)
term(11) = term(11) + t2(b,e,j,i) * tvvoo(a, e, j, k)
end do 

term(1) = -term(1) 
term(3) = -term(3) 
term(5) = -term(5) 
term(7) = -term(7) 
term(9) = -term(9) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,k,m) * tvoov(a, i, m, c)
term(13) = term(13) + t2(b,c,m,k) * tvoov(a, i, m, c)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, k, m, c)
term(15) = term(15) + t2(a,b,i,m) * tvoov(c, k, m, c)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(b, c, m, k)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(c, c, m, k)
term(18) = term(18) + t2(a,c,m,k) * tvvoo(b, c, m, i)
term(19) = term(19) + t2(a,b,m,k) * tvvoo(c, c, m, i)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, k, j, j)
term(21) = term(21) + t2(a,b,i,m) * toooo(m, j, j, k)
term(22) = term(22) + t2(a,b,m,k) * toooo(m, i, j, j)
term(23) = term(23) + t2(a,b,m,j) * toooo(m, i, j, k)
end do 

term(13) = -term(13) 
term(15) = -term(15) 
term(16) = -term(16) 
term(18) = -term(18) 
term(20) = -term(20) 
term(22) = -term(22) 


    v1_eom_cc3_31_trans_aibjckcj = 0.d+0
    do s = 0, 23
    v1_eom_cc3_31_trans_aibjckcj = v1_eom_cc3_31_trans_aibjckcj + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckcj
    function v1_eom_cc3_31_trans_aibjckci(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_31_trans_aibjckci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(c, e, a, c)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(b, e, a, c)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(b, e, a, c)
term(4) = term(4) + t2(b,e,k,j) * tvoov(a, i, i, e)
term(5) = term(5) + t2(b,e,j,k) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(b, k, i, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(b, j, i, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(b, e, i, j)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(b, e, i, k)
term(10) = term(10) + t2(b,e,k,i) * tvvoo(a, e, i, j)
term(11) = term(11) + t2(b,e,j,i) * tvvoo(a, e, i, k)
end do 

term(0) = -term(0) 
term(2) = -term(2) 
term(5) = -term(5) 
term(7) = -term(7) 
term(9) = -term(9) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,m,j) * tvoov(b, k, m, c)
term(13) = term(13) + t2(a,c,m,k) * tvoov(b, j, m, c)
term(14) = term(14) + t2(a,b,m,k) * tvoov(c, j, m, c)
term(15) = term(15) + t2(a,b,m,j) * tvoov(c, k, m, c)
term(16) = term(16) + t2(b,c,k,m) * tvvoo(a, c, m, j)
term(17) = term(17) + t2(b,c,j,m) * tvvoo(a, c, m, k)
term(18) = term(18) + t2(b,c,m,j) * tvvoo(a, c, m, k)
term(19) = term(19) + t2(b,c,m,k) * tvvoo(a, c, m, j)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, k, i, j)
term(21) = term(21) + t2(a,b,i,m) * toooo(m, j, i, k)
term(22) = term(22) + t2(a,b,m,k) * toooo(m, i, i, j)
term(23) = term(23) + t2(a,b,m,j) * toooo(m, i, i, k)
end do 

term(13) = -term(13) 
term(15) = -term(15) 
term(17) = -term(17) 
term(19) = -term(19) 
term(20) = -term(20) 
term(22) = -term(22) 


    v1_eom_cc3_31_trans_aibjckci = 0.d+0
    do s = 0, 23
    v1_eom_cc3_31_trans_aibjckci = v1_eom_cc3_31_trans_aibjckci + term(s)
    end do

    end function v1_eom_cc3_31_trans_aibjckci
    end module v1_eom_cc3_31_trans
    
