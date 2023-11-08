module v6_eom_cc3_31_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2013-02-27 23:39:38 UTC.
    !
    contains
    
    function v6_eom_cc3_31_trans_aiajckal(t2, nocc, nactive, a, i, j, c, k, l) 
    double precision :: v6_eom_cc3_31_trans_aiajckal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,k,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(c,e,j,k) * tvoov(a, i, l, e)
term(2) = term(2) + t2(c,e,k,j) * tvoov(a, i, l, e)
term(3) = term(3) + t2(a,e,i,k) * tvoov(c, j, l, e)
term(4) = term(4) + t2(a,e,j,i) * tvoov(c, k, l, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(c, k, l, e)
term(6) = term(6) + t2(c,e,j,i) * tvoov(a, k, l, e)
term(7) = term(7) + t2(a,e,k,i) * tvoov(c, j, l, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(c, e, l, j)
term(9) = term(9) + t2(c,e,k,i) * tvvoo(a, e, l, j)
term(10) = term(10) + t2(a,e,k,j) * tvvoo(c, e, l, i)
term(11) = term(11) + t2(a,e,j,k) * tvvoo(c, e, l, i)
term(12) = term(12) + t2(c,e,j,k) * tvvoo(a, e, l, i)
term(13) = term(13) + t2(c,e,k,j) * tvvoo(a, e, l, i)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(c, e, l, k)
term(15) = term(15) + t2(c,e,j,i) * tvvoo(a, e, l, k)
end do 

term(0) = -term(0) 
term(2) = -term(2) 
term(4) = -term(4) 
term(5) = -term(5) 
term(8) = -term(8) 
term(9) = -term(9) 
term(11) = -term(11) 
term(13) = -term(13) 

do m = 1, nocc 
term(16) = term(16) + t2(a,c,i,m) * toooo(m, k, l, j)
term(17) = term(17) + t2(a,c,m,k) * toooo(m, i, l, j)
term(18) = term(18) + t2(a,c,k,m) * toooo(m, j, l, i)
term(19) = term(19) + t2(a,c,j,m) * toooo(m, k, l, i)
term(20) = term(20) + t2(a,c,m,j) * toooo(m, k, l, i)
term(21) = term(21) + t2(a,c,m,k) * toooo(m, j, l, i)
term(22) = term(22) + t2(a,c,i,m) * toooo(m, j, l, k)
term(23) = term(23) + t2(a,c,m,j) * toooo(m, i, l, k)
end do 

term(18) = -term(18) 
term(20) = -term(20) 
term(22) = -term(22) 
term(23) = -term(23) 


    v6_eom_cc3_31_trans_aiajckal = 0.d+0
    do s = 0, 23
    v6_eom_cc3_31_trans_aiajckal = v6_eom_cc3_31_trans_aiajckal + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajckal
    function v6_eom_cc3_31_trans_aiajckcl(t2, nocc, nactive, a, i, j, k, l) 
    double precision :: v6_eom_cc3_31_trans_aiajckcl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * tvoov(a, j, l, e)
term(1) = term(1) + t2(a,e,k,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,j,k) * tvoov(a, i, l, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(a, k, l, e)
term(4) = term(4) + t2(a,e,i,k) * tvvoo(a, e, l, j)
term(5) = term(5) + t2(a,e,j,i) * tvvoo(a, e, l, k)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(a, e, l, k)
term(7) = term(7) + t2(a,e,k,i) * tvvoo(a, e, l, j)
end do 

term(0) = -term(0) 
term(2) = -term(2) 
term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
term(8) = term(8) + t2(a,a,i,m) * toooo(m, k, l, j)
term(9) = term(9) + t2(a,a,j,m) * toooo(m, i, l, k)
term(10) = term(10) + t2(a,a,i,m) * toooo(m, j, l, k)
term(11) = term(11) + t2(a,a,k,m) * toooo(m, i, l, j)
end do 

term(8) = -term(8) 
term(11) = -term(11) 


    v6_eom_cc3_31_trans_aiajckcl = 0.d+0
    do s = 0, 11
    v6_eom_cc3_31_trans_aiajckcl = v6_eom_cc3_31_trans_aiajckcl + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajckcl
    function v6_eom_cc3_31_trans_aiajckdk(t2, nocc, nactive, a, i, j, c, d) 
    double precision :: v6_eom_cc3_31_trans_aiajckdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, d, a, e)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, d, a, e)
term(3) = term(3) + t2(c,e,j,i) * read_ftvvvv(a, e, a, d)
end do 

term(0) = -term(0) 
term(3) = -term(3) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,i,m) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,c,j,m) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,c,m,j) * tvoov(a, i, m, d)
term(7) = term(7) + t2(a,a,i,m) * tvoov(c, j, m, d)
term(8) = term(8) + t2(a,c,i,m) * tvvoo(a, d, m, j)
term(9) = term(9) + t2(a,a,j,m) * tvvoo(c, d, m, i)
term(10) = term(10) + t2(a,a,i,m) * tvvoo(c, d, m, j)
term(11) = term(11) + t2(a,c,m,j) * tvvoo(a, d, m, i)
end do 

term(4) = -term(4) 
term(5) = -term(5) 
term(9) = -term(9) 
term(10) = -term(10) 


    v6_eom_cc3_31_trans_aiajckdk = 0.d+0
    do s = 0, 11
    v6_eom_cc3_31_trans_aiajckdk = v6_eom_cc3_31_trans_aiajckdk + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajckdk
    function v6_eom_cc3_31_trans_aiajckdj(t2, nocc, nactive, a, i, c, k, d) 
    double precision :: v6_eom_cc3_31_trans_aiajckdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(c,e,k,i) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, d, a, e)
term(3) = term(3) + t2(a,e,k,i) * read_ftvvvv(c, d, a, e)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,k,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,c,m,k) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvoov(a, k, m, d)
term(7) = term(7) + t2(a,a,i,m) * tvoov(c, k, m, d)
term(8) = term(8) + t2(a,c,i,m) * tvvoo(a, d, m, k)
term(9) = term(9) + t2(a,c,m,k) * tvvoo(a, d, m, i)
term(10) = term(10) + t2(a,a,i,m) * tvvoo(c, d, m, k)
term(11) = term(11) + t2(a,a,k,m) * tvvoo(c, d, m, i)
end do 

term(5) = -term(5) 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = -term(9) 


    v6_eom_cc3_31_trans_aiajckdj = 0.d+0
    do s = 0, 11
    v6_eom_cc3_31_trans_aiajckdj = v6_eom_cc3_31_trans_aiajckdj + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajckdj
    function v6_eom_cc3_31_trans_aiajckdi(t2, nocc, nactive, a, j, c, k, d) 
    double precision :: v6_eom_cc3_31_trans_aiajckdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,j) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(a,e,j,k) * read_ftvvvv(c, e, a, d)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(a, e, a, d)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(a, e, a, d)
end do 

term(0) = -term(0) 
term(2) = -term(2) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,m,k) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,a,j,m) * tvoov(c, k, m, d)
term(6) = term(6) + t2(a,c,m,j) * tvoov(a, k, m, d)
term(7) = term(7) + t2(a,a,k,m) * tvoov(c, j, m, d)
term(8) = term(8) + t2(a,c,k,m) * tvvoo(a, d, m, j)
term(9) = term(9) + t2(a,c,j,m) * tvvoo(a, d, m, k)
term(10) = term(10) + t2(a,c,m,j) * tvvoo(a, d, m, k)
term(11) = term(11) + t2(a,c,m,k) * tvvoo(a, d, m, j)
end do 

term(4) = -term(4) 
term(5) = -term(5) 
term(9) = -term(9) 
term(11) = -term(11) 


    v6_eom_cc3_31_trans_aiajckdi = 0.d+0
    do s = 0, 11
    v6_eom_cc3_31_trans_aiajckdi = v6_eom_cc3_31_trans_aiajckdi + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajckdi
    function v6_eom_cc3_31_trans_aibjbkbl(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: v6_eom_cc3_31_trans_aibjbkbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,k) * tvoov(a, j, l, e)
term(1) = term(1) + t2(b,e,k,i) * tvoov(a, j, l, e)
term(2) = term(2) + t2(b,e,k,j) * tvoov(a, i, l, e)
term(3) = term(3) + t2(b,e,j,k) * tvoov(a, i, l, e)
term(4) = term(4) + t2(a,e,j,k) * tvoov(b, i, l, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(b, k, l, e)
term(6) = term(6) + t2(a,e,i,k) * tvoov(b, j, l, e)
term(7) = term(7) + t2(a,e,j,i) * tvoov(b, k, l, e)
term(8) = term(8) + t2(a,e,j,k) * tvvoo(b, e, l, i)
term(9) = term(9) + t2(b,e,k,j) * tvvoo(a, e, l, i)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(b, e, l, k)
term(11) = term(11) + t2(a,e,i,k) * tvvoo(b, e, l, j)
term(12) = term(12) + t2(a,e,j,i) * tvvoo(b, e, l, k)
term(13) = term(13) + t2(b,e,i,j) * tvvoo(a, e, l, k)
term(14) = term(14) + t2(b,e,j,i) * tvvoo(a, e, l, k)
term(15) = term(15) + t2(b,e,k,i) * tvvoo(a, e, l, j)
end do 

term(2) = -term(2) 
term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 
term(10) = -term(10) 
term(11) = -term(11) 
term(14) = -term(14) 
term(15) = -term(15) 

do m = 1, nocc 
term(16) = term(16) + t2(a,b,j,m) * toooo(m, k, l, i)
term(17) = term(17) + t2(a,b,m,k) * toooo(m, j, l, i)
term(18) = term(18) + t2(a,b,i,m) * toooo(m, j, l, k)
term(19) = term(19) + t2(a,b,i,m) * toooo(m, k, l, j)
term(20) = term(20) + t2(a,b,j,m) * toooo(m, i, l, k)
term(21) = term(21) + t2(a,b,m,i) * toooo(m, j, l, k)
term(22) = term(22) + t2(a,b,m,j) * toooo(m, i, l, k)
term(23) = term(23) + t2(a,b,m,k) * toooo(m, i, l, j)
end do 

term(16) = -term(16) 
term(17) = -term(17) 
term(20) = -term(20) 
term(21) = -term(21) 


    v6_eom_cc3_31_trans_aibjbkbl = 0.d+0
    do s = 0, 23
    v6_eom_cc3_31_trans_aibjbkbl = v6_eom_cc3_31_trans_aibjbkbl + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbkbl
    function v6_eom_cc3_31_trans_aibjbkal(t2, nocc, nactive, i, b, j, k, l) 
    double precision :: v6_eom_cc3_31_trans_aibjbkal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * tvoov(b, i, l, e)
term(1) = term(1) + t2(b,e,i,j) * tvoov(b, k, l, e)
term(2) = term(2) + t2(b,e,j,i) * tvoov(b, k, l, e)
term(3) = term(3) + t2(b,e,k,i) * tvoov(b, j, l, e)
term(4) = term(4) + t2(b,e,i,k) * tvvoo(b, e, l, j)
term(5) = term(5) + t2(b,e,k,i) * tvvoo(b, e, l, j)
term(6) = term(6) + t2(b,e,k,j) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(b,e,j,k) * tvvoo(b, e, l, i)
end do 

term(2) = -term(2) 
term(3) = -term(3) 
term(6) = -term(6) 
term(7) = -term(7) 

do m = 1, nocc 
term(8) = term(8) + t2(b,b,i,m) * toooo(m, k, l, j)
term(9) = term(9) + t2(b,b,k,m) * toooo(m, i, l, j)
term(10) = term(10) + t2(b,b,k,m) * toooo(m, j, l, i)
term(11) = term(11) + t2(b,b,j,m) * toooo(m, k, l, i)
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    v6_eom_cc3_31_trans_aibjbkal = 0.d+0
    do s = 0, 11
    v6_eom_cc3_31_trans_aibjbkal = v6_eom_cc3_31_trans_aibjbkal + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbkal
    function v6_eom_cc3_31_trans_aibjbkdk(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: v6_eom_cc3_31_trans_aibjbkdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(b, e, b, d)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(b, e, b, d)
term(2) = term(2) + t2(b,e,i,j) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(b, d, a, e)
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do m = 1, nocc 
term(4) = term(4) + t2(b,b,i,m) * tvoov(a, j, m, d)
term(5) = term(5) + t2(b,b,j,m) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,b,j,m) * tvoov(b, i, m, d)
term(7) = term(7) + t2(a,b,i,m) * tvoov(b, j, m, d)
term(8) = term(8) + t2(a,b,i,m) * tvvoo(b, d, m, j)
term(9) = term(9) + t2(a,b,j,m) * tvvoo(b, d, m, i)
term(10) = term(10) + t2(a,b,m,i) * tvvoo(b, d, m, j)
term(11) = term(11) + t2(a,b,m,j) * tvvoo(b, d, m, i)
end do 

term(5) = -term(5) 
term(7) = -term(7) 
term(8) = -term(8) 
term(11) = -term(11) 


    v6_eom_cc3_31_trans_aibjbkdk = 0.d+0
    do s = 0, 11
    v6_eom_cc3_31_trans_aibjbkdk = v6_eom_cc3_31_trans_aibjbkdk + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbkdk
    function v6_eom_cc3_31_trans_aibjbkdj(t2, nocc, nactive, a, i, b, k, d) 
    double precision :: v6_eom_cc3_31_trans_aibjbkdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,k) * read_ftvvvv(b, e, a, d)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(b, e, b, d)
term(3) = term(3) + t2(b,e,k,i) * read_ftvvvv(b, d, a, e)
end do 

term(0) = -term(0) 
term(1) = -term(1) 

do m = 1, nocc 
term(4) = term(4) + t2(b,b,k,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,b,m,k) * tvoov(b, i, m, d)
term(6) = term(6) + t2(a,b,i,m) * tvoov(b, k, m, d)
term(7) = term(7) + t2(a,b,m,i) * tvoov(b, k, m, d)
term(8) = term(8) + t2(b,b,i,m) * tvvoo(a, d, m, k)
term(9) = term(9) + t2(b,b,k,m) * tvvoo(a, d, m, i)
term(10) = term(10) + t2(a,b,i,m) * tvvoo(b, d, m, k)
term(11) = term(11) + t2(a,b,m,k) * tvvoo(b, d, m, i)
end do 

term(4) = -term(4) 
term(6) = -term(6) 
term(10) = -term(10) 
term(11) = -term(11) 


    v6_eom_cc3_31_trans_aibjbkdj = 0.d+0
    do s = 0, 11
    v6_eom_cc3_31_trans_aibjbkdj = v6_eom_cc3_31_trans_aibjbkdj + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbkdj
    function v6_eom_cc3_31_trans_aibjbkdi(t2, nocc, nactive, a, b, j, k, d) 
    double precision :: v6_eom_cc3_31_trans_aibjbkdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(b, e, a, d)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,j,k) * read_ftvvvv(b, e, b, d)
term(3) = term(3) + t2(b,e,k,j) * read_ftvvvv(b, d, a, e)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do m = 1, nocc 
term(4) = term(4) + t2(b,b,k,m) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,b,j,m) * tvoov(b, k, m, d)
term(6) = term(6) + t2(a,b,m,j) * tvoov(b, k, m, d)
term(7) = term(7) + t2(a,b,m,k) * tvoov(b, j, m, d)
term(8) = term(8) + t2(b,b,k,m) * tvvoo(a, d, m, j)
term(9) = term(9) + t2(b,b,j,m) * tvvoo(a, d, m, k)
term(10) = term(10) + t2(a,b,j,m) * tvvoo(b, d, m, k)
term(11) = term(11) + t2(a,b,m,k) * tvvoo(b, d, m, j)
end do 

term(6) = -term(6) 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = -term(9) 


    v6_eom_cc3_31_trans_aibjbkdi = 0.d+0
    do s = 0, 11
    v6_eom_cc3_31_trans_aibjbkdi = v6_eom_cc3_31_trans_aibjbkdi + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbkdi
    function v6_eom_cc3_31_trans_aiajcial(t2, nocc, nactive, a, i, j, c, l) 
    double precision :: v6_eom_cc3_31_trans_aiajcial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(c,e,j,i) * tvoov(a, i, l, e)
term(2) = term(2) + t2(c,e,i,j) * tvoov(a, i, l, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(c, j, l, e)
term(4) = term(4) + t2(a,e,j,i) * tvoov(c, i, l, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(c, i, l, e)
term(6) = term(6) + t2(a,e,i,i) * tvvoo(c, e, l, j)
term(7) = term(7) + t2(c,e,i,i) * tvvoo(a, e, l, j)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(c, e, l, i)
term(9) = term(9) + t2(a,e,j,i) * tvvoo(c, e, l, i)
term(10) = term(10) + t2(c,e,j,i) * tvvoo(a, e, l, i)
term(11) = term(11) + t2(c,e,i,j) * tvvoo(a, e, l, i)
end do 

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = -term(7) 
term(8) = term(8) * 2.0d+0 
term(9) = -term(9) 
term(10) = term(10) * 2.0d+0 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,i,m) * toooo(m, i, l, j)
term(13) = term(13) + t2(a,c,m,i) * toooo(m, i, l, j)
term(14) = term(14) + t2(a,c,i,m) * toooo(m, j, l, i)
term(15) = term(15) + t2(a,c,j,m) * toooo(m, i, l, i)
term(16) = term(16) + t2(a,c,m,j) * toooo(m, i, l, i)
term(17) = term(17) + t2(a,c,m,i) * toooo(m, j, l, i)
end do 

term(14) = term(14) * (-2.0d+0) 
term(16) = term(16) * (-2.0d+0) 


    v6_eom_cc3_31_trans_aiajcial = 0.d+0
    do s = 0, 17
    v6_eom_cc3_31_trans_aiajcial = v6_eom_cc3_31_trans_aiajcial + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajcial
    function v6_eom_cc3_31_trans_aiajckak(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: v6_eom_cc3_31_trans_aiajckak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, a, a, e)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, a, a, e)
term(3) = term(3) + t2(c,e,j,i) * read_ftvvvv(a, e, a, a)
term(4) = term(4) + t2(c,e,k,i) * tvoov(a, j, k, e)
term(5) = term(5) + t2(c,e,j,k) * tvoov(a, i, k, e)
term(6) = term(6) + t2(c,e,k,j) * tvoov(a, i, k, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(c, j, k, e)
term(8) = term(8) + t2(a,e,j,i) * tvoov(c, k, k, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(c, k, k, e)
term(10) = term(10) + t2(c,e,j,i) * tvoov(a, k, k, e)
term(11) = term(11) + t2(a,e,k,i) * tvoov(c, j, k, e)
term(12) = term(12) + t2(a,e,i,k) * tvvoo(c, e, k, j)
term(13) = term(13) + t2(c,e,k,i) * tvvoo(a, e, k, j)
term(14) = term(14) + t2(a,e,k,j) * tvvoo(c, e, k, i)
term(15) = term(15) + t2(a,e,j,k) * tvvoo(c, e, k, i)
term(16) = term(16) + t2(c,e,j,k) * tvvoo(a, e, k, i)
term(17) = term(17) + t2(c,e,k,j) * tvvoo(a, e, k, i)
term(18) = term(18) + t2(a,e,i,j) * tvvoo(c, e, k, k)
term(19) = term(19) + t2(c,e,j,i) * tvvoo(a, e, k, k)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 
term(8) = -term(8) 
term(9) = -term(9) 
term(12) = -term(12) 
term(13) = -term(13) 
term(15) = -term(15) 
term(17) = -term(17) 

do m = 1, nocc 
term(20) = term(20) + t2(a,c,i,m) * tvoov(a, j, m, a)
term(21) = term(21) + t2(a,c,j,m) * tvoov(a, i, m, a)
term(22) = term(22) + t2(a,c,m,j) * tvoov(a, i, m, a)
term(23) = term(23) + t2(a,a,i,m) * tvoov(c, j, m, a)
term(24) = term(24) + t2(a,c,i,m) * tvvoo(a, a, m, j)
term(25) = term(25) + t2(a,a,j,m) * tvvoo(c, a, m, i)
term(26) = term(26) + t2(a,a,i,m) * tvvoo(c, a, m, j)
term(27) = term(27) + t2(a,c,m,j) * tvvoo(a, a, m, i)
term(28) = term(28) + t2(a,c,i,m) * toooo(m, k, k, j)
term(29) = term(29) + t2(a,c,m,k) * toooo(m, i, k, j)
term(30) = term(30) + t2(a,c,k,m) * toooo(m, j, k, i)
term(31) = term(31) + t2(a,c,j,m) * toooo(m, k, k, i)
term(32) = term(32) + t2(a,c,m,j) * toooo(m, k, k, i)
term(33) = term(33) + t2(a,c,m,k) * toooo(m, j, k, i)
term(34) = term(34) + t2(a,c,i,m) * toooo(m, j, k, k)
term(35) = term(35) + t2(a,c,m,j) * toooo(m, i, k, k)
end do 

term(20) = -term(20) 
term(21) = -term(21) 
term(25) = -term(25) 
term(26) = -term(26) 
term(30) = -term(30) 
term(32) = -term(32) 
term(34) = -term(34) 
term(35) = -term(35) 


    v6_eom_cc3_31_trans_aiajckak = 0.d+0
    do s = 0, 35
    v6_eom_cc3_31_trans_aiajckak = v6_eom_cc3_31_trans_aiajckak + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajckak
    function v6_eom_cc3_31_trans_aiajckaj(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: v6_eom_cc3_31_trans_aiajckaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(c,e,k,i) * read_ftvvvv(a, e, a, a)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, a, a, e)
term(3) = term(3) + t2(a,e,k,i) * read_ftvvvv(c, a, a, e)
term(4) = term(4) + t2(c,e,k,i) * tvoov(a, j, j, e)
term(5) = term(5) + t2(c,e,j,k) * tvoov(a, i, j, e)
term(6) = term(6) + t2(c,e,k,j) * tvoov(a, i, j, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(c, j, j, e)
term(8) = term(8) + t2(a,e,j,i) * tvoov(c, k, j, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(c, k, j, e)
term(10) = term(10) + t2(c,e,j,i) * tvoov(a, k, j, e)
term(11) = term(11) + t2(a,e,k,i) * tvoov(c, j, j, e)
term(12) = term(12) + t2(a,e,i,k) * tvvoo(c, e, j, j)
term(13) = term(13) + t2(c,e,k,i) * tvvoo(a, e, j, j)
term(14) = term(14) + t2(a,e,k,j) * tvvoo(c, e, j, i)
term(15) = term(15) + t2(a,e,j,k) * tvvoo(c, e, j, i)
term(16) = term(16) + t2(c,e,j,k) * tvvoo(a, e, j, i)
term(17) = term(17) + t2(c,e,k,j) * tvvoo(a, e, j, i)
term(18) = term(18) + t2(a,e,i,j) * tvvoo(c, e, j, k)
term(19) = term(19) + t2(c,e,j,i) * tvvoo(a, e, j, k)
end do 

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 
term(8) = -term(8) 
term(9) = -term(9) 
term(12) = -term(12) 
term(13) = -term(13) 
term(15) = -term(15) 
term(17) = -term(17) 

do m = 1, nocc 
term(20) = term(20) + t2(a,c,k,m) * tvoov(a, i, m, a)
term(21) = term(21) + t2(a,c,m,k) * tvoov(a, i, m, a)
term(22) = term(22) + t2(a,c,i,m) * tvoov(a, k, m, a)
term(23) = term(23) + t2(a,a,i,m) * tvoov(c, k, m, a)
term(24) = term(24) + t2(a,c,i,m) * tvvoo(a, a, m, k)
term(25) = term(25) + t2(a,c,m,k) * tvvoo(a, a, m, i)
term(26) = term(26) + t2(a,a,i,m) * tvvoo(c, a, m, k)
term(27) = term(27) + t2(a,a,k,m) * tvvoo(c, a, m, i)
term(28) = term(28) + t2(a,c,i,m) * toooo(m, k, j, j)
term(29) = term(29) + t2(a,c,m,k) * toooo(m, i, j, j)
term(30) = term(30) + t2(a,c,k,m) * toooo(m, j, j, i)
term(31) = term(31) + t2(a,c,j,m) * toooo(m, k, j, i)
term(32) = term(32) + t2(a,c,m,j) * toooo(m, k, j, i)
term(33) = term(33) + t2(a,c,m,k) * toooo(m, j, j, i)
term(34) = term(34) + t2(a,c,i,m) * toooo(m, j, j, k)
term(35) = term(35) + t2(a,c,m,j) * toooo(m, i, j, k)
end do 

term(21) = -term(21) 
term(23) = -term(23) 
term(24) = -term(24) 
term(25) = -term(25) 
term(30) = -term(30) 
term(32) = -term(32) 
term(34) = -term(34) 
term(35) = -term(35) 


    v6_eom_cc3_31_trans_aiajckaj = 0.d+0
    do s = 0, 35
    v6_eom_cc3_31_trans_aiajckaj = v6_eom_cc3_31_trans_aiajckaj + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajckaj
    function v6_eom_cc3_31_trans_aiajckai(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: v6_eom_cc3_31_trans_aiajckai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,j) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(a,e,j,k) * read_ftvvvv(c, e, a, a)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(a, e, a, a)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(a, e, a, a)
term(4) = term(4) + t2(c,e,k,i) * tvoov(a, j, i, e)
term(5) = term(5) + t2(c,e,j,k) * tvoov(a, i, i, e)
term(6) = term(6) + t2(c,e,k,j) * tvoov(a, i, i, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(c, j, i, e)
term(8) = term(8) + t2(a,e,j,i) * tvoov(c, k, i, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(c, k, i, e)
term(10) = term(10) + t2(c,e,j,i) * tvoov(a, k, i, e)
term(11) = term(11) + t2(a,e,k,i) * tvoov(c, j, i, e)
term(12) = term(12) + t2(a,e,i,k) * tvvoo(c, e, i, j)
term(13) = term(13) + t2(c,e,k,i) * tvvoo(a, e, i, j)
term(14) = term(14) + t2(a,e,k,j) * tvvoo(c, e, i, i)
term(15) = term(15) + t2(a,e,j,k) * tvvoo(c, e, i, i)
term(16) = term(16) + t2(c,e,j,k) * tvvoo(a, e, i, i)
term(17) = term(17) + t2(c,e,k,j) * tvvoo(a, e, i, i)
term(18) = term(18) + t2(a,e,i,j) * tvvoo(c, e, i, k)
term(19) = term(19) + t2(c,e,j,i) * tvvoo(a, e, i, k)
end do 

term(0) = -term(0) 
term(2) = -term(2) 
term(4) = -term(4) 
term(6) = -term(6) 
term(8) = -term(8) 
term(9) = -term(9) 
term(12) = -term(12) 
term(13) = -term(13) 
term(15) = -term(15) 
term(17) = -term(17) 

do m = 1, nocc 
term(20) = term(20) + t2(a,c,m,k) * tvoov(a, j, m, a)
term(21) = term(21) + t2(a,a,j,m) * tvoov(c, k, m, a)
term(22) = term(22) + t2(a,c,m,j) * tvoov(a, k, m, a)
term(23) = term(23) + t2(a,a,k,m) * tvoov(c, j, m, a)
term(24) = term(24) + t2(a,c,k,m) * tvvoo(a, a, m, j)
term(25) = term(25) + t2(a,c,j,m) * tvvoo(a, a, m, k)
term(26) = term(26) + t2(a,c,m,j) * tvvoo(a, a, m, k)
term(27) = term(27) + t2(a,c,m,k) * tvvoo(a, a, m, j)
term(28) = term(28) + t2(a,c,i,m) * toooo(m, k, i, j)
term(29) = term(29) + t2(a,c,m,k) * toooo(m, i, i, j)
term(30) = term(30) + t2(a,c,k,m) * toooo(m, j, i, i)
term(31) = term(31) + t2(a,c,j,m) * toooo(m, k, i, i)
term(32) = term(32) + t2(a,c,m,j) * toooo(m, k, i, i)
term(33) = term(33) + t2(a,c,m,k) * toooo(m, j, i, i)
term(34) = term(34) + t2(a,c,i,m) * toooo(m, j, i, k)
term(35) = term(35) + t2(a,c,m,j) * toooo(m, i, i, k)
end do 

term(20) = -term(20) 
term(21) = -term(21) 
term(25) = -term(25) 
term(27) = -term(27) 
term(30) = -term(30) 
term(32) = -term(32) 
term(34) = -term(34) 
term(35) = -term(35) 


    v6_eom_cc3_31_trans_aiajckai = 0.d+0
    do s = 0, 35
    v6_eom_cc3_31_trans_aiajckai = v6_eom_cc3_31_trans_aiajckai + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajckai
    function v6_eom_cc3_31_trans_aiajcicl(t2, nocc, nactive, a, i, j, l) 
    double precision :: v6_eom_cc3_31_trans_aiajcicl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(a,e,i,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,j,i) * tvoov(a, i, l, e)
term(3) = term(3) + t2(a,e,i,i) * tvvoo(a, e, l, j)
term(4) = term(4) + t2(a,e,j,i) * tvvoo(a, e, l, i)
term(5) = term(5) + t2(a,e,i,j) * tvvoo(a, e, l, i)
end do 

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
term(6) = term(6) + t2(a,a,i,m) * toooo(m, i, l, j)
term(7) = term(7) + t2(a,a,j,m) * toooo(m, i, l, i)
term(8) = term(8) + t2(a,a,i,m) * toooo(m, j, l, i)
end do 

term(6) = term(6) * (-2.0d+0) 


    v6_eom_cc3_31_trans_aiajcicl = 0.d+0
    do s = 0, 8
    v6_eom_cc3_31_trans_aiajcicl = v6_eom_cc3_31_trans_aiajcicl + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajcicl
    function v6_eom_cc3_31_trans_aiajckck(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: v6_eom_cc3_31_trans_aiajckck 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, c, a, e)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, c, a, e)
term(3) = term(3) + t2(c,e,j,i) * read_ftvvvv(a, e, a, c)
term(4) = term(4) + t2(a,e,i,k) * tvoov(a, j, k, e)
term(5) = term(5) + t2(a,e,k,j) * tvoov(a, i, k, e)
term(6) = term(6) + t2(a,e,j,k) * tvoov(a, i, k, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(a, k, k, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(a, e, k, j)
term(9) = term(9) + t2(a,e,j,i) * tvvoo(a, e, k, k)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(a, e, k, k)
term(11) = term(11) + t2(a,e,k,i) * tvvoo(a, e, k, j)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 
term(9) = -term(9) 
term(10) = -term(10) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,i,m) * tvoov(a, j, m, c)
term(13) = term(13) + t2(a,c,j,m) * tvoov(a, i, m, c)
term(14) = term(14) + t2(a,c,m,j) * tvoov(a, i, m, c)
term(15) = term(15) + t2(a,a,i,m) * tvoov(c, j, m, c)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(a, c, m, j)
term(17) = term(17) + t2(a,a,j,m) * tvvoo(c, c, m, i)
term(18) = term(18) + t2(a,a,i,m) * tvvoo(c, c, m, j)
term(19) = term(19) + t2(a,c,m,j) * tvvoo(a, c, m, i)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, k, k, j)
term(21) = term(21) + t2(a,a,j,m) * toooo(m, i, k, k)
term(22) = term(22) + t2(a,a,i,m) * toooo(m, j, k, k)
term(23) = term(23) + t2(a,a,k,m) * toooo(m, i, k, j)
end do 

term(12) = -term(12) 
term(13) = -term(13) 
term(17) = -term(17) 
term(18) = -term(18) 
term(20) = -term(20) 
term(23) = -term(23) 


    v6_eom_cc3_31_trans_aiajckck = 0.d+0
    do s = 0, 23
    v6_eom_cc3_31_trans_aiajckck = v6_eom_cc3_31_trans_aiajckck + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajckck
    function v6_eom_cc3_31_trans_aiajckcj(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: v6_eom_cc3_31_trans_aiajckcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,k,i) * read_ftvvvv(a, e, a, c)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, c, a, e)
term(3) = term(3) + t2(a,e,k,i) * read_ftvvvv(c, c, a, e)
term(4) = term(4) + t2(a,e,i,k) * tvoov(a, j, j, e)
term(5) = term(5) + t2(a,e,k,j) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,j,k) * tvoov(a, i, j, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(a, k, j, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(a, e, j, j)
term(9) = term(9) + t2(a,e,j,i) * tvvoo(a, e, j, k)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(a, e, j, k)
term(11) = term(11) + t2(a,e,k,i) * tvvoo(a, e, j, j)
end do 

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 
term(9) = -term(9) 
term(10) = -term(10) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,k,m) * tvoov(a, i, m, c)
term(13) = term(13) + t2(a,c,m,k) * tvoov(a, i, m, c)
term(14) = term(14) + t2(a,c,i,m) * tvoov(a, k, m, c)
term(15) = term(15) + t2(a,a,i,m) * tvoov(c, k, m, c)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(a, c, m, k)
term(17) = term(17) + t2(a,c,m,k) * tvvoo(a, c, m, i)
term(18) = term(18) + t2(a,a,i,m) * tvvoo(c, c, m, k)
term(19) = term(19) + t2(a,a,k,m) * tvvoo(c, c, m, i)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, k, j, j)
term(21) = term(21) + t2(a,a,j,m) * toooo(m, i, j, k)
term(22) = term(22) + t2(a,a,i,m) * toooo(m, j, j, k)
term(23) = term(23) + t2(a,a,k,m) * toooo(m, i, j, j)
end do 

term(13) = -term(13) 
term(15) = -term(15) 
term(16) = -term(16) 
term(17) = -term(17) 
term(20) = -term(20) 
term(23) = -term(23) 


    v6_eom_cc3_31_trans_aiajckcj = 0.d+0
    do s = 0, 23
    v6_eom_cc3_31_trans_aiajckcj = v6_eom_cc3_31_trans_aiajckcj + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajckcj
    function v6_eom_cc3_31_trans_aiajckci(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: v6_eom_cc3_31_trans_aiajckci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,j) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(a,e,j,k) * read_ftvvvv(c, e, a, c)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(a, e, a, c)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(a, e, a, c)
term(4) = term(4) + t2(a,e,i,k) * tvoov(a, j, i, e)
term(5) = term(5) + t2(a,e,k,j) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,j,k) * tvoov(a, i, i, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(a, k, i, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(a, e, i, j)
term(9) = term(9) + t2(a,e,j,i) * tvvoo(a, e, i, k)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(a, e, i, k)
term(11) = term(11) + t2(a,e,k,i) * tvvoo(a, e, i, j)
end do 

term(0) = -term(0) 
term(2) = -term(2) 
term(4) = -term(4) 
term(6) = -term(6) 
term(9) = -term(9) 
term(10) = -term(10) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,m,k) * tvoov(a, j, m, c)
term(13) = term(13) + t2(a,a,j,m) * tvoov(c, k, m, c)
term(14) = term(14) + t2(a,c,m,j) * tvoov(a, k, m, c)
term(15) = term(15) + t2(a,a,k,m) * tvoov(c, j, m, c)
term(16) = term(16) + t2(a,c,k,m) * tvvoo(a, c, m, j)
term(17) = term(17) + t2(a,c,j,m) * tvvoo(a, c, m, k)
term(18) = term(18) + t2(a,c,m,j) * tvvoo(a, c, m, k)
term(19) = term(19) + t2(a,c,m,k) * tvvoo(a, c, m, j)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, k, i, j)
term(21) = term(21) + t2(a,a,j,m) * toooo(m, i, i, k)
term(22) = term(22) + t2(a,a,i,m) * toooo(m, j, i, k)
term(23) = term(23) + t2(a,a,k,m) * toooo(m, i, i, j)
end do 

term(12) = -term(12) 
term(13) = -term(13) 
term(17) = -term(17) 
term(19) = -term(19) 
term(20) = -term(20) 
term(23) = -term(23) 


    v6_eom_cc3_31_trans_aiajckci = 0.d+0
    do s = 0, 23
    v6_eom_cc3_31_trans_aiajckci = v6_eom_cc3_31_trans_aiajckci + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajckci
    function v6_eom_cc3_31_trans_aiajcidi(t2, nocc, nactive, a, i, j, c, d) 
    double precision :: v6_eom_cc3_31_trans_aiajcidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, e, a, d)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(a, e, a, d)
term(3) = term(3) + t2(c,e,i,j) * read_ftvvvv(a, e, a, d)
term(4) = term(4) + t2(a,e,j,i) * read_ftvvvv(c, d, a, e)
term(5) = term(5) + t2(a,e,i,j) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (-2.0d+0) 
term(2) = term(2) * (-2.0d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,c,i,m) * tvoov(a, j, m, d)
term(7) = term(7) + t2(a,c,m,i) * tvoov(a, j, m, d)
term(8) = term(8) + t2(a,c,j,m) * tvoov(a, i, m, d)
term(9) = term(9) + t2(a,c,m,j) * tvoov(a, i, m, d)
term(10) = term(10) + t2(a,a,i,m) * tvoov(c, j, m, d)
term(11) = term(11) + t2(a,a,j,m) * tvoov(c, i, m, d)
term(12) = term(12) + t2(a,c,i,m) * tvvoo(a, d, m, j)
term(13) = term(13) + t2(a,c,j,m) * tvvoo(a, d, m, i)
term(14) = term(14) + t2(a,c,m,j) * tvvoo(a, d, m, i)
term(15) = term(15) + t2(a,c,m,i) * tvvoo(a, d, m, j)
term(16) = term(16) + t2(a,a,j,m) * tvvoo(c, d, m, i)
term(17) = term(17) + t2(a,a,i,m) * tvvoo(c, d, m, j)
end do 

term(6) = -term(6) 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = term(9) * 2.0d+0 
term(10) = term(10) * 2.0d+0 
term(11) = -term(11) 
term(12) = term(12) * 2.0d+0 
term(13) = -term(13) 
term(14) = term(14) * 2.0d+0 
term(15) = -term(15) 
term(16) = -term(16) 
term(17) = -term(17) 


    v6_eom_cc3_31_trans_aiajcidi = 0.d+0
    do s = 0, 17
    v6_eom_cc3_31_trans_aiajcidi = v6_eom_cc3_31_trans_aiajcidi + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajcidi
    function v6_eom_cc3_31_trans_aiajcidj(t2, nocc, nactive, a, i, c, d) 
    double precision :: v6_eom_cc3_31_trans_aiajcidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(c,e,i,i) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, d, a, e)
end do 

term(2) = term(2) * (-2.0d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,c,i,m) * tvoov(a, i, m, d)
term(4) = term(4) + t2(a,c,m,i) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,a,i,m) * tvoov(c, i, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvvoo(a, d, m, i)
term(7) = term(7) + t2(a,c,m,i) * tvvoo(a, d, m, i)
term(8) = term(8) + t2(a,a,i,m) * tvvoo(c, d, m, i)
end do 

term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = -term(7) 
term(8) = term(8) * 2.0d+0 


    v6_eom_cc3_31_trans_aiajcidj = 0.d+0
    do s = 0, 8
    v6_eom_cc3_31_trans_aiajcidj = v6_eom_cc3_31_trans_aiajcidj + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajcidj
    function v6_eom_cc3_31_trans_aibjbibl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: v6_eom_cc3_31_trans_aibjbibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(b,e,i,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(b,e,j,i) * tvoov(a, i, l, e)
term(3) = term(3) + t2(a,e,j,i) * tvoov(b, i, l, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(b, i, l, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(b, j, l, e)
term(6) = term(6) + t2(a,e,j,i) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(b,e,i,j) * tvvoo(a, e, l, i)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(b, e, l, i)
term(9) = term(9) + t2(a,e,i,i) * tvvoo(b, e, l, j)
term(10) = term(10) + t2(b,e,j,i) * tvvoo(a, e, l, i)
term(11) = term(11) + t2(b,e,i,i) * tvvoo(a, e, l, j)
end do 

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = term(6) * 2.0d+0 
term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 
term(9) = -term(9) 
term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,j,m) * toooo(m, i, l, i)
term(13) = term(13) + t2(a,b,m,i) * toooo(m, j, l, i)
term(14) = term(14) + t2(a,b,i,m) * toooo(m, j, l, i)
term(15) = term(15) + t2(a,b,i,m) * toooo(m, i, l, j)
term(16) = term(16) + t2(a,b,m,j) * toooo(m, i, l, i)
term(17) = term(17) + t2(a,b,m,i) * toooo(m, i, l, j)
end do 

term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (-2.0d+0) 


    v6_eom_cc3_31_trans_aibjbibl = 0.d+0
    do s = 0, 17
    v6_eom_cc3_31_trans_aibjbibl = v6_eom_cc3_31_trans_aibjbibl + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbibl
    function v6_eom_cc3_31_trans_aibjbkbk(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: v6_eom_cc3_31_trans_aibjbkbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(b, e, b, b)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(b, e, b, b)
term(2) = term(2) + t2(b,e,i,j) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(b, b, a, e)
term(4) = term(4) + t2(b,e,i,k) * tvoov(a, j, k, e)
term(5) = term(5) + t2(b,e,k,i) * tvoov(a, j, k, e)
term(6) = term(6) + t2(b,e,k,j) * tvoov(a, i, k, e)
term(7) = term(7) + t2(b,e,j,k) * tvoov(a, i, k, e)
term(8) = term(8) + t2(a,e,j,k) * tvoov(b, i, k, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(b, k, k, e)
term(10) = term(10) + t2(a,e,i,k) * tvoov(b, j, k, e)
term(11) = term(11) + t2(a,e,j,i) * tvoov(b, k, k, e)
term(12) = term(12) + t2(a,e,j,k) * tvvoo(b, e, k, i)
term(13) = term(13) + t2(b,e,k,j) * tvvoo(a, e, k, i)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(b, e, k, k)
term(15) = term(15) + t2(a,e,i,k) * tvvoo(b, e, k, j)
term(16) = term(16) + t2(a,e,j,i) * tvvoo(b, e, k, k)
term(17) = term(17) + t2(b,e,i,j) * tvvoo(a, e, k, k)
term(18) = term(18) + t2(b,e,j,i) * tvvoo(a, e, k, k)
term(19) = term(19) + t2(b,e,k,i) * tvvoo(a, e, k, j)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(6) = -term(6) 
term(7) = -term(7) 
term(9) = -term(9) 
term(10) = -term(10) 
term(14) = -term(14) 
term(15) = -term(15) 
term(18) = -term(18) 
term(19) = -term(19) 

do m = 1, nocc 
term(20) = term(20) + t2(b,b,i,m) * tvoov(a, j, m, b)
term(21) = term(21) + t2(b,b,j,m) * tvoov(a, i, m, b)
term(22) = term(22) + t2(a,b,j,m) * tvoov(b, i, m, b)
term(23) = term(23) + t2(a,b,i,m) * tvoov(b, j, m, b)
term(24) = term(24) + t2(a,b,i,m) * tvvoo(b, b, m, j)
term(25) = term(25) + t2(a,b,j,m) * tvvoo(b, b, m, i)
term(26) = term(26) + t2(a,b,m,i) * tvvoo(b, b, m, j)
term(27) = term(27) + t2(a,b,m,j) * tvvoo(b, b, m, i)
term(28) = term(28) + t2(a,b,j,m) * toooo(m, k, k, i)
term(29) = term(29) + t2(a,b,m,k) * toooo(m, j, k, i)
term(30) = term(30) + t2(a,b,i,m) * toooo(m, j, k, k)
term(31) = term(31) + t2(a,b,i,m) * toooo(m, k, k, j)
term(32) = term(32) + t2(a,b,j,m) * toooo(m, i, k, k)
term(33) = term(33) + t2(a,b,m,i) * toooo(m, j, k, k)
term(34) = term(34) + t2(a,b,m,j) * toooo(m, i, k, k)
term(35) = term(35) + t2(a,b,m,k) * toooo(m, i, k, j)
end do 

term(21) = -term(21) 
term(23) = -term(23) 
term(24) = -term(24) 
term(27) = -term(27) 
term(28) = -term(28) 
term(29) = -term(29) 
term(32) = -term(32) 
term(33) = -term(33) 


    v6_eom_cc3_31_trans_aibjbkbk = 0.d+0
    do s = 0, 35
    v6_eom_cc3_31_trans_aibjbkbk = v6_eom_cc3_31_trans_aibjbkbk + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbkbk
    function v6_eom_cc3_31_trans_aibjbkbj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: v6_eom_cc3_31_trans_aibjbkbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,k) * read_ftvvvv(b, e, a, b)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(b, e, b, b)
term(3) = term(3) + t2(b,e,k,i) * read_ftvvvv(b, b, a, e)
term(4) = term(4) + t2(b,e,i,k) * tvoov(a, j, j, e)
term(5) = term(5) + t2(b,e,k,i) * tvoov(a, j, j, e)
term(6) = term(6) + t2(b,e,k,j) * tvoov(a, i, j, e)
term(7) = term(7) + t2(b,e,j,k) * tvoov(a, i, j, e)
term(8) = term(8) + t2(a,e,j,k) * tvoov(b, i, j, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(b, k, j, e)
term(10) = term(10) + t2(a,e,i,k) * tvoov(b, j, j, e)
term(11) = term(11) + t2(a,e,j,i) * tvoov(b, k, j, e)
term(12) = term(12) + t2(a,e,j,k) * tvvoo(b, e, j, i)
term(13) = term(13) + t2(b,e,k,j) * tvvoo(a, e, j, i)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(b, e, j, k)
term(15) = term(15) + t2(a,e,i,k) * tvvoo(b, e, j, j)
term(16) = term(16) + t2(a,e,j,i) * tvvoo(b, e, j, k)
term(17) = term(17) + t2(b,e,i,j) * tvvoo(a, e, j, k)
term(18) = term(18) + t2(b,e,j,i) * tvvoo(a, e, j, k)
term(19) = term(19) + t2(b,e,k,i) * tvvoo(a, e, j, j)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(6) = -term(6) 
term(7) = -term(7) 
term(9) = -term(9) 
term(10) = -term(10) 
term(14) = -term(14) 
term(15) = -term(15) 
term(18) = -term(18) 
term(19) = -term(19) 

do m = 1, nocc 
term(20) = term(20) + t2(b,b,k,m) * tvoov(a, i, m, b)
term(21) = term(21) + t2(a,b,m,k) * tvoov(b, i, m, b)
term(22) = term(22) + t2(a,b,i,m) * tvoov(b, k, m, b)
term(23) = term(23) + t2(a,b,m,i) * tvoov(b, k, m, b)
term(24) = term(24) + t2(b,b,i,m) * tvvoo(a, b, m, k)
term(25) = term(25) + t2(b,b,k,m) * tvvoo(a, b, m, i)
term(26) = term(26) + t2(a,b,i,m) * tvvoo(b, b, m, k)
term(27) = term(27) + t2(a,b,m,k) * tvvoo(b, b, m, i)
term(28) = term(28) + t2(a,b,j,m) * toooo(m, k, j, i)
term(29) = term(29) + t2(a,b,m,k) * toooo(m, j, j, i)
term(30) = term(30) + t2(a,b,i,m) * toooo(m, j, j, k)
term(31) = term(31) + t2(a,b,i,m) * toooo(m, k, j, j)
term(32) = term(32) + t2(a,b,j,m) * toooo(m, i, j, k)
term(33) = term(33) + t2(a,b,m,i) * toooo(m, j, j, k)
term(34) = term(34) + t2(a,b,m,j) * toooo(m, i, j, k)
term(35) = term(35) + t2(a,b,m,k) * toooo(m, i, j, j)
end do 

term(20) = -term(20) 
term(22) = -term(22) 
term(26) = -term(26) 
term(27) = -term(27) 
term(28) = -term(28) 
term(29) = -term(29) 
term(32) = -term(32) 
term(33) = -term(33) 


    v6_eom_cc3_31_trans_aibjbkbj = 0.d+0
    do s = 0, 35
    v6_eom_cc3_31_trans_aibjbkbj = v6_eom_cc3_31_trans_aibjbkbj + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbkbj
    function v6_eom_cc3_31_trans_aibjbkbi(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: v6_eom_cc3_31_trans_aibjbkbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(b, e, a, b)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(a,e,j,k) * read_ftvvvv(b, e, b, b)
term(3) = term(3) + t2(b,e,k,j) * read_ftvvvv(b, b, a, e)
term(4) = term(4) + t2(b,e,i,k) * tvoov(a, j, i, e)
term(5) = term(5) + t2(b,e,k,i) * tvoov(a, j, i, e)
term(6) = term(6) + t2(b,e,k,j) * tvoov(a, i, i, e)
term(7) = term(7) + t2(b,e,j,k) * tvoov(a, i, i, e)
term(8) = term(8) + t2(a,e,j,k) * tvoov(b, i, i, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(b, k, i, e)
term(10) = term(10) + t2(a,e,i,k) * tvoov(b, j, i, e)
term(11) = term(11) + t2(a,e,j,i) * tvoov(b, k, i, e)
term(12) = term(12) + t2(a,e,j,k) * tvvoo(b, e, i, i)
term(13) = term(13) + t2(b,e,k,j) * tvvoo(a, e, i, i)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(b, e, i, k)
term(15) = term(15) + t2(a,e,i,k) * tvvoo(b, e, i, j)
term(16) = term(16) + t2(a,e,j,i) * tvvoo(b, e, i, k)
term(17) = term(17) + t2(b,e,i,j) * tvvoo(a, e, i, k)
term(18) = term(18) + t2(b,e,j,i) * tvvoo(a, e, i, k)
term(19) = term(19) + t2(b,e,k,i) * tvvoo(a, e, i, j)
end do 

term(2) = -term(2) 
term(3) = -term(3) 
term(6) = -term(6) 
term(7) = -term(7) 
term(9) = -term(9) 
term(10) = -term(10) 
term(14) = -term(14) 
term(15) = -term(15) 
term(18) = -term(18) 
term(19) = -term(19) 

do m = 1, nocc 
term(20) = term(20) + t2(b,b,k,m) * tvoov(a, j, m, b)
term(21) = term(21) + t2(a,b,j,m) * tvoov(b, k, m, b)
term(22) = term(22) + t2(a,b,m,j) * tvoov(b, k, m, b)
term(23) = term(23) + t2(a,b,m,k) * tvoov(b, j, m, b)
term(24) = term(24) + t2(b,b,k,m) * tvvoo(a, b, m, j)
term(25) = term(25) + t2(b,b,j,m) * tvvoo(a, b, m, k)
term(26) = term(26) + t2(a,b,j,m) * tvvoo(b, b, m, k)
term(27) = term(27) + t2(a,b,m,k) * tvvoo(b, b, m, j)
term(28) = term(28) + t2(a,b,j,m) * toooo(m, k, i, i)
term(29) = term(29) + t2(a,b,m,k) * toooo(m, j, i, i)
term(30) = term(30) + t2(a,b,i,m) * toooo(m, j, i, k)
term(31) = term(31) + t2(a,b,i,m) * toooo(m, k, i, j)
term(32) = term(32) + t2(a,b,j,m) * toooo(m, i, i, k)
term(33) = term(33) + t2(a,b,m,i) * toooo(m, j, i, k)
term(34) = term(34) + t2(a,b,m,j) * toooo(m, i, i, k)
term(35) = term(35) + t2(a,b,m,k) * toooo(m, i, i, j)
end do 

term(22) = -term(22) 
term(23) = -term(23) 
term(24) = -term(24) 
term(25) = -term(25) 
term(28) = -term(28) 
term(29) = -term(29) 
term(32) = -term(32) 
term(33) = -term(33) 


    v6_eom_cc3_31_trans_aibjbkbi = 0.d+0
    do s = 0, 35
    v6_eom_cc3_31_trans_aibjbkbi = v6_eom_cc3_31_trans_aibjbkbi + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbkbi
    function v6_eom_cc3_31_trans_aibjbial(t2, nocc, nactive, i, b, j, l) 
    double precision :: v6_eom_cc3_31_trans_aibjbial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tvoov(b, i, l, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(b, i, l, e)
term(2) = term(2) + t2(b,e,i,i) * tvoov(b, j, l, e)
term(3) = term(3) + t2(b,e,i,i) * tvvoo(b, e, l, j)
term(4) = term(4) + t2(b,e,i,j) * tvvoo(b, e, l, i)
term(5) = term(5) + t2(b,e,j,i) * tvvoo(b, e, l, i)
end do 

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
term(6) = term(6) + t2(b,b,i,m) * toooo(m, i, l, j)
term(7) = term(7) + t2(b,b,i,m) * toooo(m, j, l, i)
term(8) = term(8) + t2(b,b,j,m) * toooo(m, i, l, i)
end do 

term(6) = term(6) * (-2.0d+0) 


    v6_eom_cc3_31_trans_aibjbial = 0.d+0
    do s = 0, 8
    v6_eom_cc3_31_trans_aibjbial = v6_eom_cc3_31_trans_aibjbial + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbial
    function v6_eom_cc3_31_trans_aibjbkak(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: v6_eom_cc3_31_trans_aibjbkak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(b, e, b, a)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(b, e, b, a)
term(2) = term(2) + t2(b,e,i,j) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(b,e,k,j) * tvoov(b, i, k, e)
term(5) = term(5) + t2(b,e,i,j) * tvoov(b, k, k, e)
term(6) = term(6) + t2(b,e,j,i) * tvoov(b, k, k, e)
term(7) = term(7) + t2(b,e,k,i) * tvoov(b, j, k, e)
term(8) = term(8) + t2(b,e,i,k) * tvvoo(b, e, k, j)
term(9) = term(9) + t2(b,e,k,i) * tvvoo(b, e, k, j)
term(10) = term(10) + t2(b,e,k,j) * tvvoo(b, e, k, i)
term(11) = term(11) + t2(b,e,j,k) * tvvoo(b, e, k, i)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(6) = -term(6) 
term(7) = -term(7) 
term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(b,b,i,m) * tvoov(a, j, m, a)
term(13) = term(13) + t2(b,b,j,m) * tvoov(a, i, m, a)
term(14) = term(14) + t2(a,b,j,m) * tvoov(b, i, m, a)
term(15) = term(15) + t2(a,b,i,m) * tvoov(b, j, m, a)
term(16) = term(16) + t2(a,b,i,m) * tvvoo(b, a, m, j)
term(17) = term(17) + t2(a,b,j,m) * tvvoo(b, a, m, i)
term(18) = term(18) + t2(a,b,m,i) * tvvoo(b, a, m, j)
term(19) = term(19) + t2(a,b,m,j) * tvvoo(b, a, m, i)
term(20) = term(20) + t2(b,b,i,m) * toooo(m, k, k, j)
term(21) = term(21) + t2(b,b,k,m) * toooo(m, i, k, j)
term(22) = term(22) + t2(b,b,k,m) * toooo(m, j, k, i)
term(23) = term(23) + t2(b,b,j,m) * toooo(m, k, k, i)
end do 

term(13) = -term(13) 
term(15) = -term(15) 
term(16) = -term(16) 
term(19) = -term(19) 
term(20) = -term(20) 
term(21) = -term(21) 


    v6_eom_cc3_31_trans_aibjbkak = 0.d+0
    do s = 0, 23
    v6_eom_cc3_31_trans_aibjbkak = v6_eom_cc3_31_trans_aibjbkak + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbkak
    function v6_eom_cc3_31_trans_aibjbkaj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: v6_eom_cc3_31_trans_aibjbkaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,k) * read_ftvvvv(b, e, a, a)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(b, e, b, a)
term(3) = term(3) + t2(b,e,k,i) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(b,e,k,j) * tvoov(b, i, j, e)
term(5) = term(5) + t2(b,e,i,j) * tvoov(b, k, j, e)
term(6) = term(6) + t2(b,e,j,i) * tvoov(b, k, j, e)
term(7) = term(7) + t2(b,e,k,i) * tvoov(b, j, j, e)
term(8) = term(8) + t2(b,e,i,k) * tvvoo(b, e, j, j)
term(9) = term(9) + t2(b,e,k,i) * tvvoo(b, e, j, j)
term(10) = term(10) + t2(b,e,k,j) * tvvoo(b, e, j, i)
term(11) = term(11) + t2(b,e,j,k) * tvvoo(b, e, j, i)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(6) = -term(6) 
term(7) = -term(7) 
term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(b,b,k,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,b,m,k) * tvoov(b, i, m, a)
term(14) = term(14) + t2(a,b,i,m) * tvoov(b, k, m, a)
term(15) = term(15) + t2(a,b,m,i) * tvoov(b, k, m, a)
term(16) = term(16) + t2(b,b,i,m) * tvvoo(a, a, m, k)
term(17) = term(17) + t2(b,b,k,m) * tvvoo(a, a, m, i)
term(18) = term(18) + t2(a,b,i,m) * tvvoo(b, a, m, k)
term(19) = term(19) + t2(a,b,m,k) * tvvoo(b, a, m, i)
term(20) = term(20) + t2(b,b,i,m) * toooo(m, k, j, j)
term(21) = term(21) + t2(b,b,k,m) * toooo(m, i, j, j)
term(22) = term(22) + t2(b,b,k,m) * toooo(m, j, j, i)
term(23) = term(23) + t2(b,b,j,m) * toooo(m, k, j, i)
end do 

term(12) = -term(12) 
term(14) = -term(14) 
term(18) = -term(18) 
term(19) = -term(19) 
term(20) = -term(20) 
term(21) = -term(21) 


    v6_eom_cc3_31_trans_aibjbkaj = 0.d+0
    do s = 0, 23
    v6_eom_cc3_31_trans_aibjbkaj = v6_eom_cc3_31_trans_aibjbkaj + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbkaj
    function v6_eom_cc3_31_trans_aibjbkai(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: v6_eom_cc3_31_trans_aibjbkai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(b, e, a, a)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(a,e,j,k) * read_ftvvvv(b, e, b, a)
term(3) = term(3) + t2(b,e,k,j) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(b,e,k,j) * tvoov(b, i, i, e)
term(5) = term(5) + t2(b,e,i,j) * tvoov(b, k, i, e)
term(6) = term(6) + t2(b,e,j,i) * tvoov(b, k, i, e)
term(7) = term(7) + t2(b,e,k,i) * tvoov(b, j, i, e)
term(8) = term(8) + t2(b,e,i,k) * tvvoo(b, e, i, j)
term(9) = term(9) + t2(b,e,k,i) * tvvoo(b, e, i, j)
term(10) = term(10) + t2(b,e,k,j) * tvvoo(b, e, i, i)
term(11) = term(11) + t2(b,e,j,k) * tvvoo(b, e, i, i)
end do 

term(2) = -term(2) 
term(3) = -term(3) 
term(6) = -term(6) 
term(7) = -term(7) 
term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(b,b,k,m) * tvoov(a, j, m, a)
term(13) = term(13) + t2(a,b,j,m) * tvoov(b, k, m, a)
term(14) = term(14) + t2(a,b,m,j) * tvoov(b, k, m, a)
term(15) = term(15) + t2(a,b,m,k) * tvoov(b, j, m, a)
term(16) = term(16) + t2(b,b,k,m) * tvvoo(a, a, m, j)
term(17) = term(17) + t2(b,b,j,m) * tvvoo(a, a, m, k)
term(18) = term(18) + t2(a,b,j,m) * tvvoo(b, a, m, k)
term(19) = term(19) + t2(a,b,m,k) * tvvoo(b, a, m, j)
term(20) = term(20) + t2(b,b,i,m) * toooo(m, k, i, j)
term(21) = term(21) + t2(b,b,k,m) * toooo(m, i, i, j)
term(22) = term(22) + t2(b,b,k,m) * toooo(m, j, i, i)
term(23) = term(23) + t2(b,b,j,m) * toooo(m, k, i, i)
end do 

term(14) = -term(14) 
term(15) = -term(15) 
term(16) = -term(16) 
term(17) = -term(17) 
term(20) = -term(20) 
term(21) = -term(21) 


    v6_eom_cc3_31_trans_aibjbkai = 0.d+0
    do s = 0, 23
    v6_eom_cc3_31_trans_aibjbkai = v6_eom_cc3_31_trans_aibjbkai + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbkai
    function v6_eom_cc3_31_trans_aibjbidi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: v6_eom_cc3_31_trans_aibjbidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * read_ftvvvv(b, e, a, d)
term(1) = term(1) + t2(b,e,j,i) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(b, e, b, d)
term(3) = term(3) + t2(b,e,i,j) * read_ftvvvv(b, d, a, e)
term(4) = term(4) + t2(a,e,i,j) * read_ftvvvv(b, e, b, d)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(b, d, a, e)
end do 

term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(b,b,i,m) * tvoov(a, j, m, d)
term(7) = term(7) + t2(b,b,j,m) * tvoov(a, i, m, d)
term(8) = term(8) + t2(a,b,j,m) * tvoov(b, i, m, d)
term(9) = term(9) + t2(a,b,i,m) * tvoov(b, j, m, d)
term(10) = term(10) + t2(a,b,m,j) * tvoov(b, i, m, d)
term(11) = term(11) + t2(a,b,m,i) * tvoov(b, j, m, d)
term(12) = term(12) + t2(b,b,i,m) * tvvoo(a, d, m, j)
term(13) = term(13) + t2(b,b,j,m) * tvvoo(a, d, m, i)
term(14) = term(14) + t2(a,b,j,m) * tvvoo(b, d, m, i)
term(15) = term(15) + t2(a,b,m,i) * tvvoo(b, d, m, j)
term(16) = term(16) + t2(a,b,i,m) * tvvoo(b, d, m, j)
term(17) = term(17) + t2(a,b,m,j) * tvvoo(b, d, m, i)
end do 

term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 
term(8) = term(8) * 2.0d+0 
term(9) = -term(9) 
term(10) = -term(10) 
term(11) = -term(11) 
term(12) = -term(12) 
term(13) = -term(13) 
term(14) = term(14) * 2.0d+0 
term(15) = term(15) * 2.0d+0 
term(16) = -term(16) 
term(17) = -term(17) 


    v6_eom_cc3_31_trans_aibjbidi = 0.d+0
    do s = 0, 17
    v6_eom_cc3_31_trans_aibjbidi = v6_eom_cc3_31_trans_aibjbidi + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbidi
    function v6_eom_cc3_31_trans_aibjbidj(t2, nocc, nactive, a, i, b, d) 
    double precision :: v6_eom_cc3_31_trans_aibjbidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * read_ftvvvv(b, e, a, d)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(b, e, b, d)
term(2) = term(2) + t2(b,e,i,i) * read_ftvvvv(b, d, a, e)
end do 

term(0) = term(0) * (-2.0d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(b,b,i,m) * tvoov(a, i, m, d)
term(4) = term(4) + t2(a,b,m,i) * tvoov(b, i, m, d)
term(5) = term(5) + t2(a,b,i,m) * tvoov(b, i, m, d)
term(6) = term(6) + t2(b,b,i,m) * tvvoo(a, d, m, i)
term(7) = term(7) + t2(a,b,i,m) * tvvoo(b, d, m, i)
term(8) = term(8) + t2(a,b,m,i) * tvvoo(b, d, m, i)
end do 

term(3) = -term(3) 
term(4) = term(4) * 2.0d+0 
term(5) = -term(5) 
term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 
term(8) = -term(8) 


    v6_eom_cc3_31_trans_aibjbidj = 0.d+0
    do s = 0, 8
    v6_eom_cc3_31_trans_aibjbidj = v6_eom_cc3_31_trans_aibjbidj + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbidj
    function v6_eom_cc3_31_trans_aiajciai(t2, nocc, nactive, a, i, j, c) 
    double precision :: v6_eom_cc3_31_trans_aiajciai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,i) * tvoov(a, j, i, e)
term(1) = term(1) + t2(c,e,j,i) * tvoov(a, i, i, e)
term(2) = term(2) + t2(c,e,i,j) * tvoov(a, i, i, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(c, j, i, e)
term(4) = term(4) + t2(a,e,j,i) * tvoov(c, i, i, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(c, i, i, e)
term(6) = term(6) + t2(a,e,i,j) * read_ftvvvv(c, e, a, a)
term(7) = term(7) + t2(a,e,j,i) * read_ftvvvv(c, e, a, a)
term(8) = term(8) + t2(c,e,j,i) * read_ftvvvv(a, e, a, a)
term(9) = term(9) + t2(c,e,i,j) * read_ftvvvv(a, e, a, a)
term(10) = term(10) + t2(a,e,j,i) * read_ftvvvv(c, a, a, e)
term(11) = term(11) + t2(a,e,i,j) * read_ftvvvv(c, a, a, e)
term(12) = term(12) + t2(a,e,i,i) * tvvoo(c, e, i, j)
term(13) = term(13) + t2(c,e,i,i) * tvvoo(a, e, i, j)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(c, e, i, i)
term(15) = term(15) + t2(a,e,j,i) * tvvoo(c, e, i, i)
term(16) = term(16) + t2(c,e,j,i) * tvvoo(a, e, i, i)
term(17) = term(17) + t2(c,e,i,j) * tvvoo(a, e, i, i)
end do 

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = term(6) * (-2.0d+0) 
term(8) = term(8) * (-2.0d+0) 
term(12) = -term(12) 
term(13) = -term(13) 
term(14) = term(14) * 2.0d+0 
term(15) = -term(15) 
term(16) = term(16) * 2.0d+0 
term(17) = -term(17) 

do m = 1, nocc 
term(18) = term(18) + t2(a,c,i,m) * tvoov(a, j, m, a)
term(19) = term(19) + t2(a,c,m,i) * tvoov(a, j, m, a)
term(20) = term(20) + t2(a,c,j,m) * tvoov(a, i, m, a)
term(21) = term(21) + t2(a,c,m,j) * tvoov(a, i, m, a)
term(22) = term(22) + t2(a,a,i,m) * tvoov(c, j, m, a)
term(23) = term(23) + t2(a,a,j,m) * tvoov(c, i, m, a)
term(24) = term(24) + t2(a,c,i,m) * tvvoo(a, a, m, j)
term(25) = term(25) + t2(a,c,j,m) * tvvoo(a, a, m, i)
term(26) = term(26) + t2(a,c,m,j) * tvvoo(a, a, m, i)
term(27) = term(27) + t2(a,c,m,i) * tvvoo(a, a, m, j)
term(28) = term(28) + t2(a,a,j,m) * tvvoo(c, a, m, i)
term(29) = term(29) + t2(a,a,i,m) * tvvoo(c, a, m, j)
term(30) = term(30) + t2(a,c,i,m) * toooo(m, i, i, j)
term(31) = term(31) + t2(a,c,m,i) * toooo(m, i, i, j)
term(32) = term(32) + t2(a,c,i,m) * toooo(m, j, i, i)
term(33) = term(33) + t2(a,c,j,m) * toooo(m, i, i, i)
term(34) = term(34) + t2(a,c,m,j) * toooo(m, i, i, i)
term(35) = term(35) + t2(a,c,m,i) * toooo(m, j, i, i)
end do 

term(18) = -term(18) 
term(19) = -term(19) 
term(20) = -term(20) 
term(21) = term(21) * 2.0d+0 
term(22) = term(22) * 2.0d+0 
term(23) = -term(23) 
term(24) = term(24) * 2.0d+0 
term(25) = -term(25) 
term(26) = term(26) * 2.0d+0 
term(27) = -term(27) 
term(28) = -term(28) 
term(29) = -term(29) 
term(32) = term(32) * (-2.0d+0) 
term(34) = term(34) * (-2.0d+0) 


    v6_eom_cc3_31_trans_aiajciai = 0.d+0
    do s = 0, 35
    v6_eom_cc3_31_trans_aiajciai = v6_eom_cc3_31_trans_aiajciai + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajciai
    function v6_eom_cc3_31_trans_aiajciaj(t2, nocc, nactive, a, i, j, c) 
    double precision :: v6_eom_cc3_31_trans_aiajciaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:26) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(c,e,i,i) * read_ftvvvv(a, e, a, a)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, a, a, e)
term(3) = term(3) + t2(c,e,i,i) * tvoov(a, j, j, e)
term(4) = term(4) + t2(c,e,j,i) * tvoov(a, i, j, e)
term(5) = term(5) + t2(c,e,i,j) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,i,i) * tvoov(c, j, j, e)
term(7) = term(7) + t2(a,e,j,i) * tvoov(c, i, j, e)
term(8) = term(8) + t2(a,e,i,j) * tvoov(c, i, j, e)
term(9) = term(9) + t2(a,e,i,i) * tvvoo(c, e, j, j)
term(10) = term(10) + t2(c,e,i,i) * tvvoo(a, e, j, j)
term(11) = term(11) + t2(a,e,i,j) * tvvoo(c, e, j, i)
term(12) = term(12) + t2(a,e,j,i) * tvvoo(c, e, j, i)
term(13) = term(13) + t2(c,e,j,i) * tvvoo(a, e, j, i)
term(14) = term(14) + t2(c,e,i,j) * tvvoo(a, e, j, i)
end do 

term(2) = term(2) * (-2.0d+0) 
term(3) = -term(3) 
term(4) = term(4) * 2.0d+0 
term(5) = -term(5) 
term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = -term(9) 
term(10) = -term(10) 
term(11) = term(11) * 2.0d+0 
term(12) = -term(12) 
term(13) = term(13) * 2.0d+0 
term(14) = -term(14) 

do m = 1, nocc 
term(15) = term(15) + t2(a,c,i,m) * tvoov(a, i, m, a)
term(16) = term(16) + t2(a,c,m,i) * tvoov(a, i, m, a)
term(17) = term(17) + t2(a,a,i,m) * tvoov(c, i, m, a)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(a, a, m, i)
term(19) = term(19) + t2(a,c,m,i) * tvvoo(a, a, m, i)
term(20) = term(20) + t2(a,a,i,m) * tvvoo(c, a, m, i)
term(21) = term(21) + t2(a,c,i,m) * toooo(m, i, j, j)
term(22) = term(22) + t2(a,c,m,i) * toooo(m, i, j, j)
term(23) = term(23) + t2(a,c,i,m) * toooo(m, j, j, i)
term(24) = term(24) + t2(a,c,j,m) * toooo(m, i, j, i)
term(25) = term(25) + t2(a,c,m,j) * toooo(m, i, j, i)
term(26) = term(26) + t2(a,c,m,i) * toooo(m, j, j, i)
end do 

term(15) = term(15) * 2.0d+0 
term(16) = -term(16) 
term(17) = -term(17) 
term(18) = -term(18) 
term(19) = -term(19) 
term(20) = term(20) * 2.0d+0 
term(23) = term(23) * (-2.0d+0) 
term(25) = term(25) * (-2.0d+0) 


    v6_eom_cc3_31_trans_aiajciaj = 0.d+0
    do s = 0, 26
    v6_eom_cc3_31_trans_aiajciaj = v6_eom_cc3_31_trans_aiajciaj + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajciaj
    function v6_eom_cc3_31_trans_aiajcici(t2, nocc, nactive, a, i, j, c) 
    double precision :: v6_eom_cc3_31_trans_aiajcici 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:26) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, e, a, c)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(a, e, a, c)
term(3) = term(3) + t2(c,e,i,j) * read_ftvvvv(a, e, a, c)
term(4) = term(4) + t2(a,e,j,i) * read_ftvvvv(c, c, a, e)
term(5) = term(5) + t2(a,e,i,j) * read_ftvvvv(c, c, a, e)
term(6) = term(6) + t2(a,e,i,i) * tvoov(a, j, i, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(a, i, i, e)
term(8) = term(8) + t2(a,e,j,i) * tvoov(a, i, i, e)
term(9) = term(9) + t2(a,e,i,i) * tvvoo(a, e, i, j)
term(10) = term(10) + t2(a,e,j,i) * tvvoo(a, e, i, i)
term(11) = term(11) + t2(a,e,i,j) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * (-2.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(6) = -term(6) 
term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 
term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,i,m) * tvoov(a, j, m, c)
term(13) = term(13) + t2(a,c,m,i) * tvoov(a, j, m, c)
term(14) = term(14) + t2(a,c,j,m) * tvoov(a, i, m, c)
term(15) = term(15) + t2(a,c,m,j) * tvoov(a, i, m, c)
term(16) = term(16) + t2(a,a,i,m) * tvoov(c, j, m, c)
term(17) = term(17) + t2(a,a,j,m) * tvoov(c, i, m, c)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(a, c, m, j)
term(19) = term(19) + t2(a,c,j,m) * tvvoo(a, c, m, i)
term(20) = term(20) + t2(a,c,m,j) * tvvoo(a, c, m, i)
term(21) = term(21) + t2(a,c,m,i) * tvvoo(a, c, m, j)
term(22) = term(22) + t2(a,a,j,m) * tvvoo(c, c, m, i)
term(23) = term(23) + t2(a,a,i,m) * tvvoo(c, c, m, j)
term(24) = term(24) + t2(a,a,i,m) * toooo(m, i, i, j)
term(25) = term(25) + t2(a,a,j,m) * toooo(m, i, i, i)
term(26) = term(26) + t2(a,a,i,m) * toooo(m, j, i, i)
end do 

term(12) = -term(12) 
term(13) = -term(13) 
term(14) = -term(14) 
term(15) = term(15) * 2.0d+0 
term(16) = term(16) * 2.0d+0 
term(17) = -term(17) 
term(18) = term(18) * 2.0d+0 
term(19) = -term(19) 
term(20) = term(20) * 2.0d+0 
term(21) = -term(21) 
term(22) = -term(22) 
term(23) = -term(23) 
term(24) = term(24) * (-2.0d+0) 


    v6_eom_cc3_31_trans_aiajcici = 0.d+0
    do s = 0, 26
    v6_eom_cc3_31_trans_aiajcici = v6_eom_cc3_31_trans_aiajcici + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajcici
    function v6_eom_cc3_31_trans_aiajcicj(t2, nocc, nactive, a, i, j, c) 
    double precision :: v6_eom_cc3_31_trans_aiajcicj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,i,i) * read_ftvvvv(a, e, a, c)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, c, a, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(a, j, j, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(a, i, j, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,i,i) * tvvoo(a, e, j, j)
term(7) = term(7) + t2(a,e,j,i) * tvvoo(a, e, j, i)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(a, e, j, i)
end do 

term(2) = term(2) * (-2.0d+0) 
term(3) = -term(3) 
term(4) = term(4) * 2.0d+0 
term(5) = -term(5) 
term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 
term(8) = -term(8) 

do m = 1, nocc 
term(9) = term(9) + t2(a,c,i,m) * tvoov(a, i, m, c)
term(10) = term(10) + t2(a,c,m,i) * tvoov(a, i, m, c)
term(11) = term(11) + t2(a,a,i,m) * tvoov(c, i, m, c)
term(12) = term(12) + t2(a,c,i,m) * tvvoo(a, c, m, i)
term(13) = term(13) + t2(a,c,m,i) * tvvoo(a, c, m, i)
term(14) = term(14) + t2(a,a,i,m) * tvvoo(c, c, m, i)
term(15) = term(15) + t2(a,a,i,m) * toooo(m, i, j, j)
term(16) = term(16) + t2(a,a,j,m) * toooo(m, i, j, i)
term(17) = term(17) + t2(a,a,i,m) * toooo(m, j, j, i)
end do 

term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 
term(11) = -term(11) 
term(12) = -term(12) 
term(13) = -term(13) 
term(14) = term(14) * 2.0d+0 
term(15) = term(15) * (-2.0d+0) 


    v6_eom_cc3_31_trans_aiajcicj = 0.d+0
    do s = 0, 17
    v6_eom_cc3_31_trans_aiajcicj = v6_eom_cc3_31_trans_aiajcicj + term(s)
    end do

    end function v6_eom_cc3_31_trans_aiajcicj
    function v6_eom_cc3_31_trans_aibjbibi(t2, nocc, nactive, a, i, b, j) 
    double precision :: v6_eom_cc3_31_trans_aibjbibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tvoov(a, j, i, e)
term(1) = term(1) + t2(b,e,i,j) * tvoov(a, i, i, e)
term(2) = term(2) + t2(b,e,j,i) * tvoov(a, i, i, e)
term(3) = term(3) + t2(a,e,j,i) * tvoov(b, i, i, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(b, i, i, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(b, j, i, e)
term(6) = term(6) + t2(b,e,i,j) * read_ftvvvv(b, e, a, b)
term(7) = term(7) + t2(b,e,j,i) * read_ftvvvv(b, e, a, b)
term(8) = term(8) + t2(a,e,j,i) * read_ftvvvv(b, e, b, b)
term(9) = term(9) + t2(b,e,i,j) * read_ftvvvv(b, b, a, e)
term(10) = term(10) + t2(a,e,i,j) * read_ftvvvv(b, e, b, b)
term(11) = term(11) + t2(b,e,j,i) * read_ftvvvv(b, b, a, e)
term(12) = term(12) + t2(a,e,j,i) * tvvoo(b, e, i, i)
term(13) = term(13) + t2(b,e,i,j) * tvvoo(a, e, i, i)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(b, e, i, i)
term(15) = term(15) + t2(a,e,i,i) * tvvoo(b, e, i, j)
term(16) = term(16) + t2(b,e,j,i) * tvvoo(a, e, i, i)
term(17) = term(17) + t2(b,e,i,i) * tvvoo(a, e, i, j)
end do 

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * (-2.0d+0) 
term(12) = term(12) * 2.0d+0 
term(13) = term(13) * 2.0d+0 
term(14) = -term(14) 
term(15) = -term(15) 
term(16) = -term(16) 
term(17) = -term(17) 

do m = 1, nocc 
term(18) = term(18) + t2(b,b,i,m) * tvoov(a, j, m, b)
term(19) = term(19) + t2(b,b,j,m) * tvoov(a, i, m, b)
term(20) = term(20) + t2(a,b,j,m) * tvoov(b, i, m, b)
term(21) = term(21) + t2(a,b,i,m) * tvoov(b, j, m, b)
term(22) = term(22) + t2(a,b,m,j) * tvoov(b, i, m, b)
term(23) = term(23) + t2(a,b,m,i) * tvoov(b, j, m, b)
term(24) = term(24) + t2(b,b,i,m) * tvvoo(a, b, m, j)
term(25) = term(25) + t2(b,b,j,m) * tvvoo(a, b, m, i)
term(26) = term(26) + t2(a,b,j,m) * tvvoo(b, b, m, i)
term(27) = term(27) + t2(a,b,m,i) * tvvoo(b, b, m, j)
term(28) = term(28) + t2(a,b,i,m) * tvvoo(b, b, m, j)
term(29) = term(29) + t2(a,b,m,j) * tvvoo(b, b, m, i)
term(30) = term(30) + t2(a,b,j,m) * toooo(m, i, i, i)
term(31) = term(31) + t2(a,b,m,i) * toooo(m, j, i, i)
term(32) = term(32) + t2(a,b,i,m) * toooo(m, j, i, i)
term(33) = term(33) + t2(a,b,i,m) * toooo(m, i, i, j)
term(34) = term(34) + t2(a,b,m,j) * toooo(m, i, i, i)
term(35) = term(35) + t2(a,b,m,i) * toooo(m, i, i, j)
end do 

term(18) = term(18) * 2.0d+0 
term(19) = -term(19) 
term(20) = term(20) * 2.0d+0 
term(21) = -term(21) 
term(22) = -term(22) 
term(23) = -term(23) 
term(24) = -term(24) 
term(25) = -term(25) 
term(26) = term(26) * 2.0d+0 
term(27) = term(27) * 2.0d+0 
term(28) = -term(28) 
term(29) = -term(29) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (-2.0d+0) 


    v6_eom_cc3_31_trans_aibjbibi = 0.d+0
    do s = 0, 35
    v6_eom_cc3_31_trans_aibjbibi = v6_eom_cc3_31_trans_aibjbibi + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbibi
    function v6_eom_cc3_31_trans_aibjbibj(t2, nocc, nactive, a, i, b, j) 
    double precision :: v6_eom_cc3_31_trans_aibjbibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    double precision, dimension(0:26) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * read_ftvvvv(b, e, a, b)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(b, e, b, b)
term(2) = term(2) + t2(b,e,i,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(b,e,i,i) * tvoov(a, j, j, e)
term(4) = term(4) + t2(b,e,i,j) * tvoov(a, i, j, e)
term(5) = term(5) + t2(b,e,j,i) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,j,i) * tvoov(b, i, j, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(b, i, j, e)
term(8) = term(8) + t2(a,e,i,i) * tvoov(b, j, j, e)
term(9) = term(9) + t2(a,e,j,i) * tvvoo(b, e, j, i)
term(10) = term(10) + t2(b,e,i,j) * tvvoo(a, e, j, i)
term(11) = term(11) + t2(a,e,i,j) * tvvoo(b, e, j, i)
term(12) = term(12) + t2(a,e,i,i) * tvvoo(b, e, j, j)
term(13) = term(13) + t2(b,e,j,i) * tvvoo(a, e, j, i)
term(14) = term(14) + t2(b,e,i,i) * tvvoo(a, e, j, j)
end do 

term(0) = term(0) * (-2.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = term(9) * 2.0d+0 
term(10) = term(10) * 2.0d+0 
term(11) = -term(11) 
term(12) = -term(12) 
term(13) = -term(13) 
term(14) = -term(14) 

do m = 1, nocc 
term(15) = term(15) + t2(b,b,i,m) * tvoov(a, i, m, b)
term(16) = term(16) + t2(a,b,m,i) * tvoov(b, i, m, b)
term(17) = term(17) + t2(a,b,i,m) * tvoov(b, i, m, b)
term(18) = term(18) + t2(b,b,i,m) * tvvoo(a, b, m, i)
term(19) = term(19) + t2(a,b,i,m) * tvvoo(b, b, m, i)
term(20) = term(20) + t2(a,b,m,i) * tvvoo(b, b, m, i)
term(21) = term(21) + t2(a,b,j,m) * toooo(m, i, j, i)
term(22) = term(22) + t2(a,b,m,i) * toooo(m, j, j, i)
term(23) = term(23) + t2(a,b,i,m) * toooo(m, j, j, i)
term(24) = term(24) + t2(a,b,i,m) * toooo(m, i, j, j)
term(25) = term(25) + t2(a,b,m,j) * toooo(m, i, j, i)
term(26) = term(26) + t2(a,b,m,i) * toooo(m, i, j, j)
end do 

term(15) = -term(15) 
term(16) = term(16) * 2.0d+0 
term(17) = -term(17) 
term(18) = term(18) * 2.0d+0 
term(19) = -term(19) 
term(20) = -term(20) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (-2.0d+0) 


    v6_eom_cc3_31_trans_aibjbibj = 0.d+0
    do s = 0, 26
    v6_eom_cc3_31_trans_aibjbibj = v6_eom_cc3_31_trans_aibjbibj + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbibj
    function v6_eom_cc3_31_trans_aibjbiai(t2, nocc, nactive, a, i, b, j) 
    double precision :: v6_eom_cc3_31_trans_aibjbiai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    double precision, dimension(0:26) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * read_ftvvvv(b, e, a, a)
term(1) = term(1) + t2(b,e,j,i) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(b, e, b, a)
term(3) = term(3) + t2(b,e,i,j) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(a,e,i,j) * read_ftvvvv(b, e, b, a)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(b, a, a, e)
term(6) = term(6) + t2(b,e,i,j) * tvoov(b, i, i, e)
term(7) = term(7) + t2(b,e,j,i) * tvoov(b, i, i, e)
term(8) = term(8) + t2(b,e,i,i) * tvoov(b, j, i, e)
term(9) = term(9) + t2(b,e,i,i) * tvvoo(b, e, i, j)
term(10) = term(10) + t2(b,e,i,j) * tvvoo(b, e, i, i)
term(11) = term(11) + t2(b,e,j,i) * tvvoo(b, e, i, i)
end do 

term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(b,b,i,m) * tvoov(a, j, m, a)
term(13) = term(13) + t2(b,b,j,m) * tvoov(a, i, m, a)
term(14) = term(14) + t2(a,b,j,m) * tvoov(b, i, m, a)
term(15) = term(15) + t2(a,b,i,m) * tvoov(b, j, m, a)
term(16) = term(16) + t2(a,b,m,j) * tvoov(b, i, m, a)
term(17) = term(17) + t2(a,b,m,i) * tvoov(b, j, m, a)
term(18) = term(18) + t2(b,b,i,m) * tvvoo(a, a, m, j)
term(19) = term(19) + t2(b,b,j,m) * tvvoo(a, a, m, i)
term(20) = term(20) + t2(a,b,j,m) * tvvoo(b, a, m, i)
term(21) = term(21) + t2(a,b,m,i) * tvvoo(b, a, m, j)
term(22) = term(22) + t2(a,b,i,m) * tvvoo(b, a, m, j)
term(23) = term(23) + t2(a,b,m,j) * tvvoo(b, a, m, i)
term(24) = term(24) + t2(b,b,i,m) * toooo(m, i, i, j)
term(25) = term(25) + t2(b,b,i,m) * toooo(m, j, i, i)
term(26) = term(26) + t2(b,b,j,m) * toooo(m, i, i, i)
end do 

term(12) = term(12) * 2.0d+0 
term(13) = -term(13) 
term(14) = term(14) * 2.0d+0 
term(15) = -term(15) 
term(16) = -term(16) 
term(17) = -term(17) 
term(18) = -term(18) 
term(19) = -term(19) 
term(20) = term(20) * 2.0d+0 
term(21) = term(21) * 2.0d+0 
term(22) = -term(22) 
term(23) = -term(23) 
term(24) = term(24) * (-2.0d+0) 


    v6_eom_cc3_31_trans_aibjbiai = 0.d+0
    do s = 0, 26
    v6_eom_cc3_31_trans_aibjbiai = v6_eom_cc3_31_trans_aibjbiai + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbiai
    function v6_eom_cc3_31_trans_aibjbiaj(t2, nocc, nactive, a, i, b, j) 
    double precision :: v6_eom_cc3_31_trans_aibjbiaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * read_ftvvvv(b, e, a, a)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(b, e, b, a)
term(2) = term(2) + t2(b,e,i,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(b,e,i,j) * tvoov(b, i, j, e)
term(4) = term(4) + t2(b,e,j,i) * tvoov(b, i, j, e)
term(5) = term(5) + t2(b,e,i,i) * tvoov(b, j, j, e)
term(6) = term(6) + t2(b,e,i,i) * tvvoo(b, e, j, j)
term(7) = term(7) + t2(b,e,i,j) * tvvoo(b, e, j, i)
term(8) = term(8) + t2(b,e,j,i) * tvvoo(b, e, j, i)
end do 

term(0) = term(0) * (-2.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 
term(8) = -term(8) 

do m = 1, nocc 
term(9) = term(9) + t2(b,b,i,m) * tvoov(a, i, m, a)
term(10) = term(10) + t2(a,b,m,i) * tvoov(b, i, m, a)
term(11) = term(11) + t2(a,b,i,m) * tvoov(b, i, m, a)
term(12) = term(12) + t2(b,b,i,m) * tvvoo(a, a, m, i)
term(13) = term(13) + t2(a,b,i,m) * tvvoo(b, a, m, i)
term(14) = term(14) + t2(a,b,m,i) * tvvoo(b, a, m, i)
term(15) = term(15) + t2(b,b,i,m) * toooo(m, i, j, j)
term(16) = term(16) + t2(b,b,i,m) * toooo(m, j, j, i)
term(17) = term(17) + t2(b,b,j,m) * toooo(m, i, j, i)
end do 

term(9) = -term(9) 
term(10) = term(10) * 2.0d+0 
term(11) = -term(11) 
term(12) = term(12) * 2.0d+0 
term(13) = -term(13) 
term(14) = -term(14) 
term(15) = term(15) * (-2.0d+0) 


    v6_eom_cc3_31_trans_aibjbiaj = 0.d+0
    do s = 0, 17
    v6_eom_cc3_31_trans_aibjbiaj = v6_eom_cc3_31_trans_aibjbiaj + term(s)
    end do

    end function v6_eom_cc3_31_trans_aibjbiaj
    end module v6_eom_cc3_31_trans
    
