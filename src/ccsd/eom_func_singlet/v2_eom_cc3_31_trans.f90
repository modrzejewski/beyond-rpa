module v2_eom_cc3_31_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2013-02-27 23:39:38 UTC.
    !
    contains
    
    function v2_eom_cc3_31_trans_aibjckbl(t2, nocc, nactive, a, i, j, c, k, l) 
    double precision :: v2_eom_cc3_31_trans_aibjckbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,k,j) * tvoov(a, i, l, e)
term(1) = term(1) + t2(c,e,k,i) * tvoov(a, j, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(c, k, l, e)
term(3) = term(3) + t2(a,e,j,i) * tvoov(c, k, l, e)
term(4) = term(4) + t2(a,e,i,k) * tvvoo(c, e, l, j)
term(5) = term(5) + t2(c,e,k,i) * tvvoo(a, e, l, j)
term(6) = term(6) + t2(a,e,j,k) * tvvoo(c, e, l, i)
term(7) = term(7) + t2(c,e,k,j) * tvvoo(a, e, l, i)
end do 

term(0) = -term(0) 
term(2) = -term(2) 
term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
term(8) = term(8) + t2(a,c,i,m) * toooo(m, k, l, j)
term(9) = term(9) + t2(a,c,m,k) * toooo(m, i, l, j)
term(10) = term(10) + t2(a,c,j,m) * toooo(m, k, l, i)
term(11) = term(11) + t2(a,c,m,k) * toooo(m, j, l, i)
end do 

term(10) = -term(10) 
term(11) = -term(11) 


    v2_eom_cc3_31_trans_aibjckbl = 0.d+0
    do s = 0, 11
    v2_eom_cc3_31_trans_aibjckbl = v2_eom_cc3_31_trans_aibjckbl + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckbl
    function v2_eom_cc3_31_trans_aibjckal(t2, nocc, nactive, i, b, j, c, k, l) 
    double precision :: v2_eom_cc3_31_trans_aibjckal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,k,i) * tvoov(b, j, l, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(c, k, l, e)
term(2) = term(2) + t2(c,e,k,j) * tvoov(b, i, l, e)
term(3) = term(3) + t2(b,e,i,j) * tvoov(c, k, l, e)
term(4) = term(4) + t2(b,e,j,k) * tvvoo(c, e, l, i)
term(5) = term(5) + t2(b,e,i,k) * tvvoo(c, e, l, j)
term(6) = term(6) + t2(c,e,k,j) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(c,e,k,i) * tvvoo(b, e, l, j)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(4) = -term(4) 
term(6) = -term(6) 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,j,m) * toooo(m, k, l, i)
term(9) = term(9) + t2(b,c,i,m) * toooo(m, k, l, j)
term(10) = term(10) + t2(b,c,m,k) * toooo(m, j, l, i)
term(11) = term(11) + t2(b,c,m,k) * toooo(m, i, l, j)
end do 

term(9) = -term(9) 
term(11) = -term(11) 


    v2_eom_cc3_31_trans_aibjckal = 0.d+0
    do s = 0, 11
    v2_eom_cc3_31_trans_aibjckal = v2_eom_cc3_31_trans_aibjckal + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckal
    function v2_eom_cc3_31_trans_aibjckcl(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: v2_eom_cc3_31_trans_aibjckcl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,k) * tvoov(a, i, l, e)
term(1) = term(1) + t2(b,e,i,k) * tvoov(a, j, l, e)
term(2) = term(2) + t2(a,e,i,k) * tvoov(b, j, l, e)
term(3) = term(3) + t2(a,e,j,k) * tvoov(b, i, l, e)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(b, e, l, k)
term(5) = term(5) + t2(a,e,j,i) * tvvoo(b, e, l, k)
term(6) = term(6) + t2(b,e,j,i) * tvvoo(a, e, l, k)
term(7) = term(7) + t2(b,e,i,j) * tvvoo(a, e, l, k)
end do 

term(0) = -term(0) 
term(2) = -term(2) 
term(4) = -term(4) 
term(6) = -term(6) 

do m = 1, nocc 
term(8) = term(8) + t2(a,b,i,m) * toooo(m, j, l, k)
term(9) = term(9) + t2(a,b,j,m) * toooo(m, i, l, k)
term(10) = term(10) + t2(a,b,m,j) * toooo(m, i, l, k)
term(11) = term(11) + t2(a,b,m,i) * toooo(m, j, l, k)
end do 

term(9) = -term(9) 
term(11) = -term(11) 


    v2_eom_cc3_31_trans_aibjckcl = 0.d+0
    do s = 0, 11
    v2_eom_cc3_31_trans_aibjckcl = v2_eom_cc3_31_trans_aibjckcl + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckcl
    function v2_eom_cc3_31_trans_aibjckdk(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: v2_eom_cc3_31_trans_aibjckdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, d, b, e)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, d, b, e)
term(2) = term(2) + t2(b,e,j,i) * read_ftvvvv(c, d, a, e)
term(3) = term(3) + t2(b,e,i,j) * read_ftvvvv(c, d, a, e)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,j,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(b,c,i,m) * tvoov(a, j, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvoov(b, j, m, d)
term(7) = term(7) + t2(a,c,j,m) * tvoov(b, i, m, d)
term(8) = term(8) + t2(a,b,i,m) * tvvoo(c, d, m, j)
term(9) = term(9) + t2(a,b,j,m) * tvvoo(c, d, m, i)
term(10) = term(10) + t2(a,b,m,j) * tvvoo(c, d, m, i)
term(11) = term(11) + t2(a,b,m,i) * tvvoo(c, d, m, j)
end do 

term(4) = -term(4) 
term(6) = -term(6) 
term(8) = -term(8) 
term(10) = -term(10) 


    v2_eom_cc3_31_trans_aibjckdk = 0.d+0
    do s = 0, 11
    v2_eom_cc3_31_trans_aibjckdk = v2_eom_cc3_31_trans_aibjckdk + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckdk
    function v2_eom_cc3_31_trans_aibjckdj(t2, nocc, nactive, a, i, b, c, k, d) 
    double precision :: v2_eom_cc3_31_trans_aibjckdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,k) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(c,e,k,i) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, b, d)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, d, a, e)
end do 

term(0) = -term(0) 
term(1) = -term(1) 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,m,k) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,b,i,m) * tvoov(c, k, m, d)
term(6) = term(6) + t2(a,c,m,k) * tvoov(b, i, m, d)
term(7) = term(7) + t2(a,b,m,i) * tvoov(c, k, m, d)
term(8) = term(8) + t2(b,c,i,m) * tvvoo(a, d, m, k)
term(9) = term(9) + t2(b,c,m,k) * tvvoo(a, d, m, i)
term(10) = term(10) + t2(a,c,i,m) * tvvoo(b, d, m, k)
term(11) = term(11) + t2(a,c,m,k) * tvvoo(b, d, m, i)
end do 

term(4) = -term(4) 
term(5) = -term(5) 
term(10) = -term(10) 
term(11) = -term(11) 


    v2_eom_cc3_31_trans_aibjckdj = 0.d+0
    do s = 0, 11
    v2_eom_cc3_31_trans_aibjckdj = v2_eom_cc3_31_trans_aibjckdj + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckdj
    function v2_eom_cc3_31_trans_aibjckdi(t2, nocc, nactive, a, b, j, c, k, d) 
    double precision :: v2_eom_cc3_31_trans_aibjckdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,k) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(c,e,k,j) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,j,k) * read_ftvvvv(c, e, b, d)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(b, d, a, e)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,m,k) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,c,m,k) * tvoov(b, j, m, d)
term(6) = term(6) + t2(a,b,j,m) * tvoov(c, k, m, d)
term(7) = term(7) + t2(a,b,m,j) * tvoov(c, k, m, d)
term(8) = term(8) + t2(b,c,j,m) * tvvoo(a, d, m, k)
term(9) = term(9) + t2(b,c,m,k) * tvvoo(a, d, m, j)
term(10) = term(10) + t2(a,c,j,m) * tvvoo(b, d, m, k)
term(11) = term(11) + t2(a,c,m,k) * tvvoo(b, d, m, j)
end do 

term(5) = -term(5) 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = -term(9) 


    v2_eom_cc3_31_trans_aibjckdi = 0.d+0
    do s = 0, 11
    v2_eom_cc3_31_trans_aibjckdi = v2_eom_cc3_31_trans_aibjckdi + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckdi
    function v2_eom_cc3_31_trans_aibjckbk(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_31_trans_aibjckbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, b, b, e)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, b, b, e)
term(2) = term(2) + t2(b,e,j,i) * read_ftvvvv(c, b, a, e)
term(3) = term(3) + t2(b,e,i,j) * read_ftvvvv(c, b, a, e)
term(4) = term(4) + t2(c,e,k,j) * tvoov(a, i, k, e)
term(5) = term(5) + t2(c,e,k,i) * tvoov(a, j, k, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(c, k, k, e)
term(7) = term(7) + t2(a,e,j,i) * tvoov(c, k, k, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(c, e, k, j)
term(9) = term(9) + t2(c,e,k,i) * tvvoo(a, e, k, j)
term(10) = term(10) + t2(a,e,j,k) * tvvoo(c, e, k, i)
term(11) = term(11) + t2(c,e,k,j) * tvvoo(a, e, k, i)
end do 

term(1) = -term(1) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 
term(8) = -term(8) 
term(9) = -term(9) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,j,m) * tvoov(a, i, m, b)
term(13) = term(13) + t2(b,c,i,m) * tvoov(a, j, m, b)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, j, m, b)
term(15) = term(15) + t2(a,c,j,m) * tvoov(b, i, m, b)
term(16) = term(16) + t2(a,b,i,m) * tvvoo(c, b, m, j)
term(17) = term(17) + t2(a,b,j,m) * tvvoo(c, b, m, i)
term(18) = term(18) + t2(a,b,m,j) * tvvoo(c, b, m, i)
term(19) = term(19) + t2(a,b,m,i) * tvvoo(c, b, m, j)
term(20) = term(20) + t2(a,c,i,m) * toooo(m, k, k, j)
term(21) = term(21) + t2(a,c,m,k) * toooo(m, i, k, j)
term(22) = term(22) + t2(a,c,j,m) * toooo(m, k, k, i)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, j, k, i)
end do 

term(12) = -term(12) 
term(14) = -term(14) 
term(16) = -term(16) 
term(18) = -term(18) 
term(22) = -term(22) 
term(23) = -term(23) 


    v2_eom_cc3_31_trans_aibjckbk = 0.d+0
    do s = 0, 23
    v2_eom_cc3_31_trans_aibjckbk = v2_eom_cc3_31_trans_aibjckbk + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckbk
    function v2_eom_cc3_31_trans_aibjckbj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_31_trans_aibjckbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,k) * read_ftvvvv(c, e, a, b)
term(1) = term(1) + t2(c,e,k,i) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, b, b)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, b, a, e)
term(4) = term(4) + t2(c,e,k,j) * tvoov(a, i, j, e)
term(5) = term(5) + t2(c,e,k,i) * tvoov(a, j, j, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(c, k, j, e)
term(7) = term(7) + t2(a,e,j,i) * tvoov(c, k, j, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(c, e, j, j)
term(9) = term(9) + t2(c,e,k,i) * tvvoo(a, e, j, j)
term(10) = term(10) + t2(a,e,j,k) * tvvoo(c, e, j, i)
term(11) = term(11) + t2(c,e,k,j) * tvvoo(a, e, j, i)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(4) = -term(4) 
term(6) = -term(6) 
term(8) = -term(8) 
term(9) = -term(9) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,m,k) * tvoov(a, i, m, b)
term(13) = term(13) + t2(a,b,i,m) * tvoov(c, k, m, b)
term(14) = term(14) + t2(a,c,m,k) * tvoov(b, i, m, b)
term(15) = term(15) + t2(a,b,m,i) * tvoov(c, k, m, b)
term(16) = term(16) + t2(b,c,i,m) * tvvoo(a, b, m, k)
term(17) = term(17) + t2(b,c,m,k) * tvvoo(a, b, m, i)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(b, b, m, k)
term(19) = term(19) + t2(a,c,m,k) * tvvoo(b, b, m, i)
term(20) = term(20) + t2(a,c,i,m) * toooo(m, k, j, j)
term(21) = term(21) + t2(a,c,m,k) * toooo(m, i, j, j)
term(22) = term(22) + t2(a,c,j,m) * toooo(m, k, j, i)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, j, j, i)
end do 

term(12) = -term(12) 
term(13) = -term(13) 
term(18) = -term(18) 
term(19) = -term(19) 
term(22) = -term(22) 
term(23) = -term(23) 


    v2_eom_cc3_31_trans_aibjckbj = 0.d+0
    do s = 0, 23
    v2_eom_cc3_31_trans_aibjckbj = v2_eom_cc3_31_trans_aibjckbj + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckbj
    function v2_eom_cc3_31_trans_aibjckbi(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_31_trans_aibjckbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,k) * read_ftvvvv(c, e, a, b)
term(1) = term(1) + t2(c,e,k,j) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(a,e,j,k) * read_ftvvvv(c, e, b, b)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(b, b, a, e)
term(4) = term(4) + t2(c,e,k,j) * tvoov(a, i, i, e)
term(5) = term(5) + t2(c,e,k,i) * tvoov(a, j, i, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(c, k, i, e)
term(7) = term(7) + t2(a,e,j,i) * tvoov(c, k, i, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(c, e, i, j)
term(9) = term(9) + t2(c,e,k,i) * tvvoo(a, e, i, j)
term(10) = term(10) + t2(a,e,j,k) * tvvoo(c, e, i, i)
term(11) = term(11) + t2(c,e,k,j) * tvvoo(a, e, i, i)
end do 

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 
term(8) = -term(8) 
term(9) = -term(9) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,m,k) * tvoov(a, j, m, b)
term(13) = term(13) + t2(a,c,m,k) * tvoov(b, j, m, b)
term(14) = term(14) + t2(a,b,j,m) * tvoov(c, k, m, b)
term(15) = term(15) + t2(a,b,m,j) * tvoov(c, k, m, b)
term(16) = term(16) + t2(b,c,j,m) * tvvoo(a, b, m, k)
term(17) = term(17) + t2(b,c,m,k) * tvvoo(a, b, m, j)
term(18) = term(18) + t2(a,c,j,m) * tvvoo(b, b, m, k)
term(19) = term(19) + t2(a,c,m,k) * tvvoo(b, b, m, j)
term(20) = term(20) + t2(a,c,i,m) * toooo(m, k, i, j)
term(21) = term(21) + t2(a,c,m,k) * toooo(m, i, i, j)
term(22) = term(22) + t2(a,c,j,m) * toooo(m, k, i, i)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, j, i, i)
end do 

term(13) = -term(13) 
term(15) = -term(15) 
term(16) = -term(16) 
term(17) = -term(17) 
term(22) = -term(22) 
term(23) = -term(23) 


    v2_eom_cc3_31_trans_aibjckbi = 0.d+0
    do s = 0, 23
    v2_eom_cc3_31_trans_aibjckbi = v2_eom_cc3_31_trans_aibjckbi + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckbi
    function v2_eom_cc3_31_trans_aibjckak(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_31_trans_aibjckak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, a, b, e)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, a, b, e)
term(2) = term(2) + t2(b,e,j,i) * read_ftvvvv(c, a, a, e)
term(3) = term(3) + t2(b,e,i,j) * read_ftvvvv(c, a, a, e)
term(4) = term(4) + t2(c,e,k,i) * tvoov(b, j, k, e)
term(5) = term(5) + t2(b,e,j,i) * tvoov(c, k, k, e)
term(6) = term(6) + t2(c,e,k,j) * tvoov(b, i, k, e)
term(7) = term(7) + t2(b,e,i,j) * tvoov(c, k, k, e)
term(8) = term(8) + t2(b,e,j,k) * tvvoo(c, e, k, i)
term(9) = term(9) + t2(b,e,i,k) * tvvoo(c, e, k, j)
term(10) = term(10) + t2(c,e,k,j) * tvvoo(b, e, k, i)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(b, e, k, j)
end do 

term(1) = -term(1) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 
term(8) = -term(8) 
term(10) = -term(10) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,j,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(b,c,i,m) * tvoov(a, j, m, a)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, j, m, a)
term(15) = term(15) + t2(a,c,j,m) * tvoov(b, i, m, a)
term(16) = term(16) + t2(a,b,i,m) * tvvoo(c, a, m, j)
term(17) = term(17) + t2(a,b,j,m) * tvvoo(c, a, m, i)
term(18) = term(18) + t2(a,b,m,j) * tvvoo(c, a, m, i)
term(19) = term(19) + t2(a,b,m,i) * tvvoo(c, a, m, j)
term(20) = term(20) + t2(b,c,j,m) * toooo(m, k, k, i)
term(21) = term(21) + t2(b,c,i,m) * toooo(m, k, k, j)
term(22) = term(22) + t2(b,c,m,k) * toooo(m, j, k, i)
term(23) = term(23) + t2(b,c,m,k) * toooo(m, i, k, j)
end do 

term(12) = -term(12) 
term(14) = -term(14) 
term(16) = -term(16) 
term(18) = -term(18) 
term(21) = -term(21) 
term(23) = -term(23) 


    v2_eom_cc3_31_trans_aibjckak = 0.d+0
    do s = 0, 23
    v2_eom_cc3_31_trans_aibjckak = v2_eom_cc3_31_trans_aibjckak + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckak
    function v2_eom_cc3_31_trans_aibjckaj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_31_trans_aibjckaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,k) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(c,e,k,i) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, b, a)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(c,e,k,i) * tvoov(b, j, j, e)
term(5) = term(5) + t2(b,e,j,i) * tvoov(c, k, j, e)
term(6) = term(6) + t2(c,e,k,j) * tvoov(b, i, j, e)
term(7) = term(7) + t2(b,e,i,j) * tvoov(c, k, j, e)
term(8) = term(8) + t2(b,e,j,k) * tvvoo(c, e, j, i)
term(9) = term(9) + t2(b,e,i,k) * tvvoo(c, e, j, j)
term(10) = term(10) + t2(c,e,k,j) * tvvoo(b, e, j, i)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(b, e, j, j)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(4) = -term(4) 
term(5) = -term(5) 
term(8) = -term(8) 
term(10) = -term(10) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,m,k) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,b,i,m) * tvoov(c, k, m, a)
term(14) = term(14) + t2(a,c,m,k) * tvoov(b, i, m, a)
term(15) = term(15) + t2(a,b,m,i) * tvoov(c, k, m, a)
term(16) = term(16) + t2(b,c,i,m) * tvvoo(a, a, m, k)
term(17) = term(17) + t2(b,c,m,k) * tvvoo(a, a, m, i)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(b, a, m, k)
term(19) = term(19) + t2(a,c,m,k) * tvvoo(b, a, m, i)
term(20) = term(20) + t2(b,c,j,m) * toooo(m, k, j, i)
term(21) = term(21) + t2(b,c,i,m) * toooo(m, k, j, j)
term(22) = term(22) + t2(b,c,m,k) * toooo(m, j, j, i)
term(23) = term(23) + t2(b,c,m,k) * toooo(m, i, j, j)
end do 

term(12) = -term(12) 
term(13) = -term(13) 
term(18) = -term(18) 
term(19) = -term(19) 
term(21) = -term(21) 
term(23) = -term(23) 


    v2_eom_cc3_31_trans_aibjckaj = 0.d+0
    do s = 0, 23
    v2_eom_cc3_31_trans_aibjckaj = v2_eom_cc3_31_trans_aibjckaj + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckaj
    function v2_eom_cc3_31_trans_aibjckai(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_31_trans_aibjckai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,k) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(c,e,k,j) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(a,e,j,k) * read_ftvvvv(c, e, b, a)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(c,e,k,i) * tvoov(b, j, i, e)
term(5) = term(5) + t2(b,e,j,i) * tvoov(c, k, i, e)
term(6) = term(6) + t2(c,e,k,j) * tvoov(b, i, i, e)
term(7) = term(7) + t2(b,e,i,j) * tvoov(c, k, i, e)
term(8) = term(8) + t2(b,e,j,k) * tvvoo(c, e, i, i)
term(9) = term(9) + t2(b,e,i,k) * tvvoo(c, e, i, j)
term(10) = term(10) + t2(c,e,k,j) * tvvoo(b, e, i, i)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(b, e, i, j)
end do 

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 
term(8) = -term(8) 
term(10) = -term(10) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,m,k) * tvoov(a, j, m, a)
term(13) = term(13) + t2(a,c,m,k) * tvoov(b, j, m, a)
term(14) = term(14) + t2(a,b,j,m) * tvoov(c, k, m, a)
term(15) = term(15) + t2(a,b,m,j) * tvoov(c, k, m, a)
term(16) = term(16) + t2(b,c,j,m) * tvvoo(a, a, m, k)
term(17) = term(17) + t2(b,c,m,k) * tvvoo(a, a, m, j)
term(18) = term(18) + t2(a,c,j,m) * tvvoo(b, a, m, k)
term(19) = term(19) + t2(a,c,m,k) * tvvoo(b, a, m, j)
term(20) = term(20) + t2(b,c,j,m) * toooo(m, k, i, i)
term(21) = term(21) + t2(b,c,i,m) * toooo(m, k, i, j)
term(22) = term(22) + t2(b,c,m,k) * toooo(m, j, i, i)
term(23) = term(23) + t2(b,c,m,k) * toooo(m, i, i, j)
end do 

term(13) = -term(13) 
term(15) = -term(15) 
term(16) = -term(16) 
term(17) = -term(17) 
term(21) = -term(21) 
term(23) = -term(23) 


    v2_eom_cc3_31_trans_aibjckai = 0.d+0
    do s = 0, 23
    v2_eom_cc3_31_trans_aibjckai = v2_eom_cc3_31_trans_aibjckai + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckai
    function v2_eom_cc3_31_trans_aibjckck(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_31_trans_aibjckck 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, c, b, e)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, c, b, e)
term(2) = term(2) + t2(b,e,j,i) * read_ftvvvv(c, c, a, e)
term(3) = term(3) + t2(b,e,i,j) * read_ftvvvv(c, c, a, e)
term(4) = term(4) + t2(b,e,j,k) * tvoov(a, i, k, e)
term(5) = term(5) + t2(b,e,i,k) * tvoov(a, j, k, e)
term(6) = term(6) + t2(a,e,i,k) * tvoov(b, j, k, e)
term(7) = term(7) + t2(a,e,j,k) * tvoov(b, i, k, e)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(b, e, k, k)
term(9) = term(9) + t2(a,e,j,i) * tvvoo(b, e, k, k)
term(10) = term(10) + t2(b,e,j,i) * tvvoo(a, e, k, k)
term(11) = term(11) + t2(b,e,i,j) * tvvoo(a, e, k, k)
end do 

term(1) = -term(1) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 
term(8) = -term(8) 
term(10) = -term(10) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,j,m) * tvoov(a, i, m, c)
term(13) = term(13) + t2(b,c,i,m) * tvoov(a, j, m, c)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, j, m, c)
term(15) = term(15) + t2(a,c,j,m) * tvoov(b, i, m, c)
term(16) = term(16) + t2(a,b,i,m) * tvvoo(c, c, m, j)
term(17) = term(17) + t2(a,b,j,m) * tvvoo(c, c, m, i)
term(18) = term(18) + t2(a,b,m,j) * tvvoo(c, c, m, i)
term(19) = term(19) + t2(a,b,m,i) * tvvoo(c, c, m, j)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, j, k, k)
term(21) = term(21) + t2(a,b,j,m) * toooo(m, i, k, k)
term(22) = term(22) + t2(a,b,m,j) * toooo(m, i, k, k)
term(23) = term(23) + t2(a,b,m,i) * toooo(m, j, k, k)
end do 

term(12) = -term(12) 
term(14) = -term(14) 
term(16) = -term(16) 
term(18) = -term(18) 
term(21) = -term(21) 
term(23) = -term(23) 


    v2_eom_cc3_31_trans_aibjckck = 0.d+0
    do s = 0, 23
    v2_eom_cc3_31_trans_aibjckck = v2_eom_cc3_31_trans_aibjckck + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckck
    function v2_eom_cc3_31_trans_aibjckcj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_31_trans_aibjckcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,k) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,k,i) * read_ftvvvv(b, e, a, c)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, b, c)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, c, a, e)
term(4) = term(4) + t2(b,e,j,k) * tvoov(a, i, j, e)
term(5) = term(5) + t2(b,e,i,k) * tvoov(a, j, j, e)
term(6) = term(6) + t2(a,e,i,k) * tvoov(b, j, j, e)
term(7) = term(7) + t2(a,e,j,k) * tvoov(b, i, j, e)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(b, e, j, k)
term(9) = term(9) + t2(a,e,j,i) * tvvoo(b, e, j, k)
term(10) = term(10) + t2(b,e,j,i) * tvvoo(a, e, j, k)
term(11) = term(11) + t2(b,e,i,j) * tvvoo(a, e, j, k)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(4) = -term(4) 
term(6) = -term(6) 
term(8) = -term(8) 
term(10) = -term(10) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,m,k) * tvoov(a, i, m, c)
term(13) = term(13) + t2(a,b,i,m) * tvoov(c, k, m, c)
term(14) = term(14) + t2(a,c,m,k) * tvoov(b, i, m, c)
term(15) = term(15) + t2(a,b,m,i) * tvoov(c, k, m, c)
term(16) = term(16) + t2(b,c,i,m) * tvvoo(a, c, m, k)
term(17) = term(17) + t2(b,c,m,k) * tvvoo(a, c, m, i)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(b, c, m, k)
term(19) = term(19) + t2(a,c,m,k) * tvvoo(b, c, m, i)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, j, j, k)
term(21) = term(21) + t2(a,b,j,m) * toooo(m, i, j, k)
term(22) = term(22) + t2(a,b,m,j) * toooo(m, i, j, k)
term(23) = term(23) + t2(a,b,m,i) * toooo(m, j, j, k)
end do 

term(12) = -term(12) 
term(13) = -term(13) 
term(18) = -term(18) 
term(19) = -term(19) 
term(21) = -term(21) 
term(23) = -term(23) 


    v2_eom_cc3_31_trans_aibjckcj = 0.d+0
    do s = 0, 23
    v2_eom_cc3_31_trans_aibjckcj = v2_eom_cc3_31_trans_aibjckcj + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckcj
    function v2_eom_cc3_31_trans_aibjckci(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_31_trans_aibjckci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,k) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,k,j) * read_ftvvvv(b, e, a, c)
term(2) = term(2) + t2(a,e,j,k) * read_ftvvvv(c, e, b, c)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(b, c, a, e)
term(4) = term(4) + t2(b,e,j,k) * tvoov(a, i, i, e)
term(5) = term(5) + t2(b,e,i,k) * tvoov(a, j, i, e)
term(6) = term(6) + t2(a,e,i,k) * tvoov(b, j, i, e)
term(7) = term(7) + t2(a,e,j,k) * tvoov(b, i, i, e)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(b, e, i, k)
term(9) = term(9) + t2(a,e,j,i) * tvvoo(b, e, i, k)
term(10) = term(10) + t2(b,e,j,i) * tvvoo(a, e, i, k)
term(11) = term(11) + t2(b,e,i,j) * tvvoo(a, e, i, k)
end do 

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 
term(8) = -term(8) 
term(10) = -term(10) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,m,k) * tvoov(a, j, m, c)
term(13) = term(13) + t2(a,c,m,k) * tvoov(b, j, m, c)
term(14) = term(14) + t2(a,b,j,m) * tvoov(c, k, m, c)
term(15) = term(15) + t2(a,b,m,j) * tvoov(c, k, m, c)
term(16) = term(16) + t2(b,c,j,m) * tvvoo(a, c, m, k)
term(17) = term(17) + t2(b,c,m,k) * tvvoo(a, c, m, j)
term(18) = term(18) + t2(a,c,j,m) * tvvoo(b, c, m, k)
term(19) = term(19) + t2(a,c,m,k) * tvvoo(b, c, m, j)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, j, i, k)
term(21) = term(21) + t2(a,b,j,m) * toooo(m, i, i, k)
term(22) = term(22) + t2(a,b,m,j) * toooo(m, i, i, k)
term(23) = term(23) + t2(a,b,m,i) * toooo(m, j, i, k)
end do 

term(13) = -term(13) 
term(15) = -term(15) 
term(16) = -term(16) 
term(17) = -term(17) 
term(21) = -term(21) 
term(23) = -term(23) 


    v2_eom_cc3_31_trans_aibjckci = 0.d+0
    do s = 0, 23
    v2_eom_cc3_31_trans_aibjckci = v2_eom_cc3_31_trans_aibjckci + term(s)
    end do

    end function v2_eom_cc3_31_trans_aibjckci
    end module v2_eom_cc3_31_trans
    
