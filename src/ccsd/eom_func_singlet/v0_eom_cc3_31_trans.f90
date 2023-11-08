module v0_eom_cc3_31_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2013-02-27 23:39:38 UTC.
    !
    contains
    
    function v0_eom_cc3_31_trans_aibjcjbl(t2, nocc, nactive, a, i, j, c, l) 
    double precision :: v0_eom_cc3_31_trans_aibjcjbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,j) * tvoov(a, i, l, e)
term(1) = term(1) + t2(c,e,i,j) * tvoov(a, j, l, e)
term(2) = term(2) + t2(a,e,j,j) * tvoov(c, i, l, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(c, j, l, e)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(c, e, l, j)
term(5) = term(5) + t2(a,e,j,i) * tvvoo(c, e, l, j)
term(6) = term(6) + t2(c,e,i,j) * tvvoo(a, e, l, j)
term(7) = term(7) + t2(c,e,j,i) * tvvoo(a, e, l, j)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 
term(7) = -term(7) 

do m = 1, nocc 
term(8) = term(8) + t2(a,c,i,m) * toooo(m, j, l, j)
term(9) = term(9) + t2(a,c,j,m) * toooo(m, i, l, j)
term(10) = term(10) + t2(a,c,m,i) * toooo(m, j, l, j)
term(11) = term(11) + t2(a,c,m,j) * toooo(m, i, l, j)
end do 

term(9) = -term(9) 
term(10) = -term(10) 


    v0_eom_cc3_31_trans_aibjcjbl = 0.d+0
    do s = 0, 11
    v0_eom_cc3_31_trans_aibjcjbl = v0_eom_cc3_31_trans_aibjcjbl + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcjbl
    function v0_eom_cc3_31_trans_aibjcibl(t2, nocc, nactive, a, i, j, c, l) 
    double precision :: v0_eom_cc3_31_trans_aibjcibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * tvoov(a, i, l, e)
term(1) = term(1) + t2(c,e,i,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(c, i, l, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(c, j, l, e)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(c, e, l, i)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(c, e, l, j)
term(6) = term(6) + t2(c,e,i,i) * tvvoo(a, e, l, j)
term(7) = term(7) + t2(c,e,j,i) * tvvoo(a, e, l, i)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
term(8) = term(8) + t2(a,c,i,m) * toooo(m, j, l, i)
term(9) = term(9) + t2(a,c,i,m) * toooo(m, i, l, j)
term(10) = term(10) + t2(a,c,m,i) * toooo(m, i, l, j)
term(11) = term(11) + t2(a,c,m,j) * toooo(m, i, l, i)
end do 

term(8) = -term(8) 
term(11) = -term(11) 


    v0_eom_cc3_31_trans_aibjcibl = 0.d+0
    do s = 0, 11
    v0_eom_cc3_31_trans_aibjcibl = v0_eom_cc3_31_trans_aibjcibl + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcibl
    function v0_eom_cc3_31_trans_aibjcjal(t2, nocc, nactive, i, b, j, c, l) 
    double precision :: v0_eom_cc3_31_trans_aibjcjal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,j) * tvoov(c, i, l, e)
term(1) = term(1) + t2(c,e,i,j) * tvoov(b, j, l, e)
term(2) = term(2) + t2(c,e,j,i) * tvoov(b, j, l, e)
term(3) = term(3) + t2(b,e,j,i) * tvoov(c, j, l, e)
term(4) = term(4) + t2(b,e,j,i) * tvvoo(c, e, l, j)
term(5) = term(5) + t2(b,e,j,j) * tvvoo(c, e, l, i)
term(6) = term(6) + t2(c,e,j,j) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(c,e,i,j) * tvvoo(b, e, l, j)
end do 

term(2) = -term(2) 
term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,j,m) * toooo(m, i, l, j)
term(9) = term(9) + t2(b,c,j,m) * toooo(m, j, l, i)
term(10) = term(10) + t2(b,c,m,j) * toooo(m, j, l, i)
term(11) = term(11) + t2(b,c,m,i) * toooo(m, j, l, j)
end do 

term(8) = -term(8) 
term(11) = -term(11) 


    v0_eom_cc3_31_trans_aibjcjal = 0.d+0
    do s = 0, 11
    v0_eom_cc3_31_trans_aibjcjal = v0_eom_cc3_31_trans_aibjcjal + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcjal
    function v0_eom_cc3_31_trans_aibjcial(t2, nocc, nactive, i, b, j, c, l) 
    double precision :: v0_eom_cc3_31_trans_aibjcial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tvoov(c, i, l, e)
term(1) = term(1) + t2(c,e,i,i) * tvoov(b, j, l, e)
term(2) = term(2) + t2(c,e,j,i) * tvoov(b, i, l, e)
term(3) = term(3) + t2(b,e,i,i) * tvoov(c, j, l, e)
term(4) = term(4) + t2(b,e,j,i) * tvvoo(c, e, l, i)
term(5) = term(5) + t2(b,e,i,j) * tvvoo(c, e, l, i)
term(6) = term(6) + t2(c,e,j,i) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(c,e,i,j) * tvvoo(b, e, l, i)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(4) = -term(4) 
term(7) = -term(7) 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,j,m) * toooo(m, i, l, i)
term(9) = term(9) + t2(b,c,i,m) * toooo(m, j, l, i)
term(10) = term(10) + t2(b,c,m,j) * toooo(m, i, l, i)
term(11) = term(11) + t2(b,c,m,i) * toooo(m, j, l, i)
end do 

term(9) = -term(9) 
term(10) = -term(10) 


    v0_eom_cc3_31_trans_aibjcial = 0.d+0
    do s = 0, 11
    v0_eom_cc3_31_trans_aibjcial = v0_eom_cc3_31_trans_aibjcial + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcial
    function v0_eom_cc3_31_trans_aibjcjcl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: v0_eom_cc3_31_trans_aibjcjcl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(b,e,j,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(b, j, l, e)
term(3) = term(3) + t2(a,e,j,i) * tvoov(b, j, l, e)
term(4) = term(4) + t2(a,e,j,j) * tvvoo(b, e, l, i)
term(5) = term(5) + t2(b,e,j,j) * tvvoo(a, e, l, i)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(b, e, l, j)
term(7) = term(7) + t2(b,e,j,i) * tvvoo(a, e, l, j)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(6) = -term(6) 
term(7) = -term(7) 

do m = 1, nocc 
term(8) = term(8) + t2(a,b,j,m) * toooo(m, j, l, i)
term(9) = term(9) + t2(a,b,m,j) * toooo(m, j, l, i)
term(10) = term(10) + t2(a,b,i,m) * toooo(m, j, l, j)
term(11) = term(11) + t2(a,b,m,j) * toooo(m, i, l, j)
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    v0_eom_cc3_31_trans_aibjcjcl = 0.d+0
    do s = 0, 11
    v0_eom_cc3_31_trans_aibjcjcl = v0_eom_cc3_31_trans_aibjcjcl + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcjcl
    function v0_eom_cc3_31_trans_aibjcicl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: v0_eom_cc3_31_trans_aibjcicl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tvoov(a, i, l, e)
term(1) = term(1) + t2(b,e,i,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(b, i, l, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(b, j, l, e)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(b, e, l, i)
term(5) = term(5) + t2(b,e,j,i) * tvvoo(a, e, l, i)
term(6) = term(6) + t2(a,e,i,i) * tvvoo(b, e, l, j)
term(7) = term(7) + t2(b,e,i,i) * tvvoo(a, e, l, j)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
term(8) = term(8) + t2(a,b,i,m) * toooo(m, j, l, i)
term(9) = term(9) + t2(a,b,m,j) * toooo(m, i, l, i)
term(10) = term(10) + t2(a,b,i,m) * toooo(m, i, l, j)
term(11) = term(11) + t2(a,b,m,i) * toooo(m, i, l, j)
end do 

term(10) = -term(10) 
term(11) = -term(11) 


    v0_eom_cc3_31_trans_aibjcicl = 0.d+0
    do s = 0, 11
    v0_eom_cc3_31_trans_aibjcicl = v0_eom_cc3_31_trans_aibjcicl + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcicl
    function v0_eom_cc3_31_trans_aibjcjdj(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: v0_eom_cc3_31_trans_aibjcjdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term
    type(tclock) :: time1, time2
    term = 0.d+0 
    do e = nocc + 1, nactive
       term(0) = term(0) + t2(b,e,j,i) * read_ftvvvv(c, e, a, d)

term(1) = term(1) + t2(c,e,i,j) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, e, b, d)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(c, e, b, d)
term(4) = term(4) + t2(a,e,i,j) * read_ftvvvv(c, d, b, e)
term(5) = term(5) + t2(c,e,i,j) * read_ftvvvv(b, d, a, e)
term(6) = term(6) + t2(c,e,j,i) * read_ftvvvv(b, d, a, e)
term(7) = term(7) + t2(b,e,j,i) * read_ftvvvv(c, d, a, e)

end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(3) = -term(3) 
term(5) = -term(5) 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,j,m) * tvoov(a, i, m, d)
term(9) = term(9) + t2(b,c,m,j) * tvoov(a, i, m, d)
term(10) = term(10) + t2(b,c,m,i) * tvoov(a, j, m, d)
term(11) = term(11) + t2(a,b,j,m) * tvoov(c, i, m, d)
term(12) = term(12) + t2(a,b,m,j) * tvoov(c, i, m, d)
term(13) = term(13) + t2(a,c,i,m) * tvoov(b, j, m, d)
term(14) = term(14) + t2(a,b,i,m) * tvoov(c, j, m, d)
term(15) = term(15) + t2(a,c,m,i) * tvoov(b, j, m, d)
term(16) = term(16) + t2(b,c,j,m) * tvvoo(a, d, m, i)
term(17) = term(17) + t2(b,c,m,i) * tvvoo(a, d, m, j)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(b, d, m, j)
term(19) = term(19) + t2(a,c,j,m) * tvvoo(b, d, m, i)
term(20) = term(20) + t2(a,b,i,m) * tvvoo(c, d, m, j)
term(21) = term(21) + t2(a,c,m,i) * tvvoo(b, d, m, j)
term(22) = term(22) + t2(a,c,m,j) * tvvoo(b, d, m, i)
term(23) = term(23) + t2(a,b,m,j) * tvvoo(c, d, m, i)
end do 

term(8) = -term(8) 
term(9) = -term(9) 
term(13) = -term(13) 
term(14) = -term(14) 
term(18) = -term(18) 
term(20) = -term(20) 
term(22) = -term(22) 
term(23) = -term(23) 


    v0_eom_cc3_31_trans_aibjcjdj = 0.d+0
    do s = 0, 23
    v0_eom_cc3_31_trans_aibjcjdj = v0_eom_cc3_31_trans_aibjcjdj + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcjdj
    function v0_eom_cc3_31_trans_aibjcjdi(t2, nocc, nactive, a, b, j, c, d) 
    double precision :: v0_eom_cc3_31_trans_aibjcjdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,j) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(c,e,j,j) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,j,j) * read_ftvvvv(c, d, b, e)
term(3) = term(3) + t2(b,e,j,j) * read_ftvvvv(c, d, a, e)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,j,m) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,c,j,m) * tvoov(b, j, m, d)
term(6) = term(6) + t2(a,c,m,j) * tvoov(b, j, m, d)
term(7) = term(7) + t2(a,b,m,j) * tvoov(c, j, m, d)
term(8) = term(8) + t2(b,c,j,m) * tvvoo(a, d, m, j)
term(9) = term(9) + t2(b,c,m,j) * tvvoo(a, d, m, j)
term(10) = term(10) + t2(a,b,j,m) * tvvoo(c, d, m, j)
term(11) = term(11) + t2(a,b,m,j) * tvvoo(c, d, m, j)
end do 

term(6) = -term(6) 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = -term(9) 


    v0_eom_cc3_31_trans_aibjcjdi = 0.d+0
    do s = 0, 11
    v0_eom_cc3_31_trans_aibjcjdi = v0_eom_cc3_31_trans_aibjcjdi + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcjdi
    function v0_eom_cc3_31_trans_aibjcidi(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: v0_eom_cc3_31_trans_aibjcidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(b,e,i,j) * read_ftvvvv(c, e, a, d)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, e, a, d)
term(3) = term(3) + t2(c,e,i,j) * read_ftvvvv(b, e, a, d)
term(4) = term(4) + t2(a,e,i,j) * read_ftvvvv(c, d, b, e)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(c, d, a, e)
term(6) = term(6) + t2(a,e,i,j) * read_ftvvvv(c, e, b, d)
term(7) = term(7) + t2(c,e,j,i) * read_ftvvvv(b, d, a, e)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(6) = -term(6) 
term(7) = -term(7) 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,j,m) * tvoov(a, i, m, d)
term(9) = term(9) + t2(b,c,m,j) * tvoov(a, i, m, d)
term(10) = term(10) + t2(a,b,m,j) * tvoov(c, i, m, d)
term(11) = term(11) + t2(a,c,i,m) * tvoov(b, j, m, d)
term(12) = term(12) + t2(a,b,i,m) * tvoov(c, j, m, d)
term(13) = term(13) + t2(a,c,m,i) * tvoov(b, j, m, d)
term(14) = term(14) + t2(a,c,m,j) * tvoov(b, i, m, d)
term(15) = term(15) + t2(a,b,m,i) * tvoov(c, j, m, d)
term(16) = term(16) + t2(b,c,j,m) * tvvoo(a, d, m, i)
term(17) = term(17) + t2(b,c,i,m) * tvvoo(a, d, m, j)
term(18) = term(18) + t2(b,c,m,j) * tvvoo(a, d, m, i)
term(19) = term(19) + t2(b,c,m,i) * tvvoo(a, d, m, j)
term(20) = term(20) + t2(a,b,i,m) * tvvoo(c, d, m, j)
term(21) = term(21) + t2(a,b,m,j) * tvvoo(c, d, m, i)
term(22) = term(22) + t2(a,c,i,m) * tvvoo(b, d, m, j)
term(23) = term(23) + t2(a,c,m,j) * tvvoo(b, d, m, i)
end do 

term(8) = -term(8) 
term(10) = -term(10) 
term(11) = -term(11) 
term(13) = -term(13) 
term(16) = -term(16) 
term(19) = -term(19) 
term(20) = -term(20) 
term(21) = -term(21) 


    v0_eom_cc3_31_trans_aibjcidi = 0.d+0
    do s = 0, 23
    v0_eom_cc3_31_trans_aibjcidi = v0_eom_cc3_31_trans_aibjcidi + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcidi
    function v0_eom_cc3_31_trans_aibjcidj(t2, nocc, nactive, a, i, b, c, d) 
    double precision :: v0_eom_cc3_31_trans_aibjcidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, b, d)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(c, d, b, e)
term(2) = term(2) + t2(c,e,i,i) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(b,e,i,i) * read_ftvvvv(c, d, a, e)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,i,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(b,c,m,i) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,b,i,m) * tvoov(c, i, m, d)
term(7) = term(7) + t2(a,c,i,m) * tvoov(b, i, m, d)
term(8) = term(8) + t2(a,c,i,m) * tvvoo(b, d, m, i)
term(9) = term(9) + t2(a,b,i,m) * tvvoo(c, d, m, i)
term(10) = term(10) + t2(a,c,m,i) * tvvoo(b, d, m, i)
term(11) = term(11) + t2(a,b,m,i) * tvvoo(c, d, m, i)
end do 

term(5) = -term(5) 
term(6) = -term(6) 
term(8) = -term(8) 
term(10) = -term(10) 


    v0_eom_cc3_31_trans_aibjcidj = 0.d+0
    do s = 0, 11
    v0_eom_cc3_31_trans_aibjcidj = v0_eom_cc3_31_trans_aibjcidj + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcidj
    function v0_eom_cc3_31_trans_aiajcjal(t2, nocc, nactive, a, i, j, c, l) 
    double precision :: v0_eom_cc3_31_trans_aiajcjal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,j) * tvoov(a, i, l, e)
term(1) = term(1) + t2(c,e,i,j) * tvoov(a, j, l, e)
term(2) = term(2) + t2(a,e,j,j) * tvoov(c, i, l, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(c, j, l, e)
term(4) = term(4) + t2(c,e,j,i) * tvoov(a, j, l, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(c, j, l, e)
term(6) = term(6) + t2(a,e,j,i) * tvvoo(c, e, l, j)
term(7) = term(7) + t2(a,e,j,j) * tvvoo(c, e, l, i)
term(8) = term(8) + t2(c,e,j,j) * tvvoo(a, e, l, i)
term(9) = term(9) + t2(c,e,i,j) * tvvoo(a, e, l, j)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(c, e, l, j)
term(11) = term(11) + t2(c,e,j,i) * tvvoo(a, e, l, j)
end do 

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,j,m) * toooo(m, i, l, j)
term(13) = term(13) + t2(a,c,j,m) * toooo(m, j, l, i)
term(14) = term(14) + t2(a,c,m,j) * toooo(m, j, l, i)
term(15) = term(15) + t2(a,c,m,i) * toooo(m, j, l, j)
term(16) = term(16) + t2(a,c,i,m) * toooo(m, j, l, j)
term(17) = term(17) + t2(a,c,m,j) * toooo(m, i, l, j)
end do 

term(12) = term(12) * (-2.0d+0) 
term(15) = term(15) * (-2.0d+0) 


    v0_eom_cc3_31_trans_aiajcjal = 0.d+0
    do s = 0, 17
    v0_eom_cc3_31_trans_aiajcjal = v0_eom_cc3_31_trans_aiajcjal + term(s)
    end do

    end function v0_eom_cc3_31_trans_aiajcjal
    function v0_eom_cc3_31_trans_aiajcjcl(t2, nocc, nactive, a, i, j, l) 
    double precision :: v0_eom_cc3_31_trans_aiajcjcl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(a,e,j,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(a, j, l, e)
term(3) = term(3) + t2(a,e,j,j) * tvvoo(a, e, l, i)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(a, e, l, j)
term(5) = term(5) + t2(a,e,j,i) * tvvoo(a, e, l, j)
end do 

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
term(6) = term(6) + t2(a,a,j,m) * toooo(m, j, l, i)
term(7) = term(7) + t2(a,a,i,m) * toooo(m, j, l, j)
term(8) = term(8) + t2(a,a,j,m) * toooo(m, i, l, j)
end do 

term(6) = term(6) * (-2.0d+0) 


    v0_eom_cc3_31_trans_aiajcjcl = 0.d+0
    do s = 0, 8
    v0_eom_cc3_31_trans_aiajcjcl = v0_eom_cc3_31_trans_aiajcjcl + term(s)
    end do

    end function v0_eom_cc3_31_trans_aiajcjcl
    function v0_eom_cc3_31_trans_aiajcjdj(t2, nocc, nactive, a, i, j, c, d) 
    double precision :: v0_eom_cc3_31_trans_aiajcjdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(c,e,i,j) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, e, a, d)
term(3) = term(3) + t2(a,e,i,j) * read_ftvvvv(c, d, a, e)
term(4) = term(4) + t2(c,e,j,i) * read_ftvvvv(a, e, a, d)
term(5) = term(5) + t2(a,e,j,i) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (-2.0d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,c,j,m) * tvoov(a, i, m, d)
term(7) = term(7) + t2(a,c,m,j) * tvoov(a, i, m, d)
term(8) = term(8) + t2(a,c,m,i) * tvoov(a, j, m, d)
term(9) = term(9) + t2(a,a,j,m) * tvoov(c, i, m, d)
term(10) = term(10) + t2(a,c,i,m) * tvoov(a, j, m, d)
term(11) = term(11) + t2(a,a,i,m) * tvoov(c, j, m, d)
term(12) = term(12) + t2(a,c,j,m) * tvvoo(a, d, m, i)
term(13) = term(13) + t2(a,c,m,i) * tvvoo(a, d, m, j)
term(14) = term(14) + t2(a,c,i,m) * tvvoo(a, d, m, j)
term(15) = term(15) + t2(a,a,i,m) * tvvoo(c, d, m, j)
term(16) = term(16) + t2(a,c,m,j) * tvvoo(a, d, m, i)
term(17) = term(17) + t2(a,a,j,m) * tvvoo(c, d, m, i)
end do 

term(6) = -term(6) 
term(7) = -term(7) 
term(8) = term(8) * 2.0d+0 
term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 
term(11) = -term(11) 
term(12) = term(12) * 2.0d+0 
term(13) = term(13) * 2.0d+0 
term(14) = -term(14) 
term(15) = -term(15) 
term(16) = -term(16) 
term(17) = -term(17) 


    v0_eom_cc3_31_trans_aiajcjdj = 0.d+0
    do s = 0, 17
    v0_eom_cc3_31_trans_aiajcjdj = v0_eom_cc3_31_trans_aiajcjdj + term(s)
    end do

    end function v0_eom_cc3_31_trans_aiajcjdj
    function v0_eom_cc3_31_trans_aiajcjdi(t2, nocc, nactive, a, j, c, d) 
    double precision :: v0_eom_cc3_31_trans_aiajcjdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,j) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(c,e,j,j) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,j,j) * read_ftvvvv(c, d, a, e)
end do 

term(2) = term(2) * (-2.0d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,c,j,m) * tvoov(a, j, m, d)
term(4) = term(4) + t2(a,c,m,j) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,a,j,m) * tvoov(c, j, m, d)
term(6) = term(6) + t2(a,c,j,m) * tvvoo(a, d, m, j)
term(7) = term(7) + t2(a,c,m,j) * tvvoo(a, d, m, j)
term(8) = term(8) + t2(a,a,j,m) * tvvoo(c, d, m, j)
end do 

term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = -term(7) 
term(8) = term(8) * 2.0d+0 


    v0_eom_cc3_31_trans_aiajcjdi = 0.d+0
    do s = 0, 8
    v0_eom_cc3_31_trans_aiajcjdi = v0_eom_cc3_31_trans_aiajcjdi + term(s)
    end do

    end function v0_eom_cc3_31_trans_aiajcjdi
    function v0_eom_cc3_31_trans_aibibkbl(t2, nocc, nactive, a, i, b, k, l) 
    double precision :: v0_eom_cc3_31_trans_aibibkbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tvoov(a, k, l, e)
term(1) = term(1) + t2(b,e,k,i) * tvoov(a, i, l, e)
term(2) = term(2) + t2(b,e,i,k) * tvoov(a, i, l, e)
term(3) = term(3) + t2(a,e,k,i) * tvoov(b, i, l, e)
term(4) = term(4) + t2(a,e,i,i) * tvoov(b, k, l, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(b, i, l, e)
term(6) = term(6) + t2(a,e,k,i) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(b,e,i,k) * tvvoo(a, e, l, i)
term(8) = term(8) + t2(a,e,i,i) * tvvoo(b, e, l, k)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(b, e, l, i)
term(10) = term(10) + t2(b,e,i,i) * tvvoo(a, e, l, k)
term(11) = term(11) + t2(b,e,k,i) * tvvoo(a, e, l, i)
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
term(12) = term(12) + t2(a,b,k,m) * toooo(m, i, l, i)
term(13) = term(13) + t2(a,b,m,i) * toooo(m, k, l, i)
term(14) = term(14) + t2(a,b,i,m) * toooo(m, i, l, k)
term(15) = term(15) + t2(a,b,i,m) * toooo(m, k, l, i)
term(16) = term(16) + t2(a,b,m,i) * toooo(m, i, l, k)
term(17) = term(17) + t2(a,b,m,k) * toooo(m, i, l, i)
end do 

term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (-2.0d+0) 


    v0_eom_cc3_31_trans_aibibkbl = 0.d+0
    do s = 0, 17
    v0_eom_cc3_31_trans_aibibkbl = v0_eom_cc3_31_trans_aibibkbl + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibibkbl
    function v0_eom_cc3_31_trans_aibjcjbj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: v0_eom_cc3_31_trans_aibjcjbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * read_ftvvvv(c, e, a, b)
term(1) = term(1) + t2(c,e,i,j) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, e, b, b)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(c, e, b, b)
term(4) = term(4) + t2(a,e,i,j) * read_ftvvvv(c, b, b, e)
term(5) = term(5) + t2(c,e,i,j) * read_ftvvvv(b, b, a, e)
term(6) = term(6) + t2(c,e,j,i) * read_ftvvvv(b, b, a, e)
term(7) = term(7) + t2(b,e,j,i) * read_ftvvvv(c, b, a, e)
term(8) = term(8) + t2(c,e,j,j) * tvoov(a, i, j, e)
term(9) = term(9) + t2(c,e,i,j) * tvoov(a, j, j, e)
term(10) = term(10) + t2(a,e,j,j) * tvoov(c, i, j, e)
term(11) = term(11) + t2(a,e,i,j) * tvoov(c, j, j, e)
term(12) = term(12) + t2(a,e,i,j) * tvvoo(c, e, j, j)
term(13) = term(13) + t2(a,e,j,i) * tvvoo(c, e, j, j)
term(14) = term(14) + t2(c,e,i,j) * tvvoo(a, e, j, j)
term(15) = term(15) + t2(c,e,j,i) * tvvoo(a, e, j, j)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(3) = -term(3) 
term(5) = -term(5) 
term(8) = -term(8) 
term(11) = -term(11) 
term(12) = -term(12) 
term(15) = -term(15) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, i, m, b)
term(17) = term(17) + t2(b,c,m,j) * tvoov(a, i, m, b)
term(18) = term(18) + t2(b,c,m,i) * tvoov(a, j, m, b)
term(19) = term(19) + t2(a,b,j,m) * tvoov(c, i, m, b)
term(20) = term(20) + t2(a,b,m,j) * tvoov(c, i, m, b)
term(21) = term(21) + t2(a,c,i,m) * tvoov(b, j, m, b)
term(22) = term(22) + t2(a,b,i,m) * tvoov(c, j, m, b)
term(23) = term(23) + t2(a,c,m,i) * tvoov(b, j, m, b)
term(24) = term(24) + t2(b,c,j,m) * tvvoo(a, b, m, i)
term(25) = term(25) + t2(b,c,m,i) * tvvoo(a, b, m, j)
term(26) = term(26) + t2(a,c,i,m) * tvvoo(b, b, m, j)
term(27) = term(27) + t2(a,c,j,m) * tvvoo(b, b, m, i)
term(28) = term(28) + t2(a,b,i,m) * tvvoo(c, b, m, j)
term(29) = term(29) + t2(a,c,m,i) * tvvoo(b, b, m, j)
term(30) = term(30) + t2(a,c,m,j) * tvvoo(b, b, m, i)
term(31) = term(31) + t2(a,b,m,j) * tvvoo(c, b, m, i)
term(32) = term(32) + t2(a,c,i,m) * toooo(m, j, j, j)
term(33) = term(33) + t2(a,c,j,m) * toooo(m, i, j, j)
term(34) = term(34) + t2(a,c,m,i) * toooo(m, j, j, j)
term(35) = term(35) + t2(a,c,m,j) * toooo(m, i, j, j)
end do 

term(16) = -term(16) 
term(17) = -term(17) 
term(21) = -term(21) 
term(22) = -term(22) 
term(26) = -term(26) 
term(28) = -term(28) 
term(30) = -term(30) 
term(31) = -term(31) 
term(33) = -term(33) 
term(34) = -term(34) 


    v0_eom_cc3_31_trans_aibjcjbj = 0.d+0
    do s = 0, 35
    v0_eom_cc3_31_trans_aibjcjbj = v0_eom_cc3_31_trans_aibjcjbj + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcjbj
    function v0_eom_cc3_31_trans_aibjcjbi(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: v0_eom_cc3_31_trans_aibjcjbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,j) * tvoov(a, i, i, e)
term(1) = term(1) + t2(c,e,i,j) * tvoov(a, j, i, e)
term(2) = term(2) + t2(a,e,j,j) * tvoov(c, i, i, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(c, j, i, e)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(c, e, i, j)
term(5) = term(5) + t2(a,e,j,i) * tvvoo(c, e, i, j)
term(6) = term(6) + t2(c,e,i,j) * tvvoo(a, e, i, j)
term(7) = term(7) + t2(c,e,j,i) * tvvoo(a, e, i, j)
term(8) = term(8) + t2(b,e,j,j) * read_ftvvvv(c, e, a, b)
term(9) = term(9) + t2(c,e,j,j) * read_ftvvvv(b, e, a, b)
term(10) = term(10) + t2(a,e,j,j) * read_ftvvvv(c, b, b, e)
term(11) = term(11) + t2(b,e,j,j) * read_ftvvvv(c, b, a, e)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 
term(7) = -term(7) 
term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,i,m) * toooo(m, j, i, j)
term(13) = term(13) + t2(a,c,j,m) * toooo(m, i, i, j)
term(14) = term(14) + t2(a,c,m,i) * toooo(m, j, i, j)
term(15) = term(15) + t2(a,c,m,j) * toooo(m, i, i, j)
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, j, m, b)
term(17) = term(17) + t2(a,c,j,m) * tvoov(b, j, m, b)
term(18) = term(18) + t2(a,c,m,j) * tvoov(b, j, m, b)
term(19) = term(19) + t2(a,b,m,j) * tvoov(c, j, m, b)
term(20) = term(20) + t2(b,c,j,m) * tvvoo(a, b, m, j)
term(21) = term(21) + t2(b,c,m,j) * tvvoo(a, b, m, j)
term(22) = term(22) + t2(a,b,j,m) * tvvoo(c, b, m, j)
term(23) = term(23) + t2(a,b,m,j) * tvvoo(c, b, m, j)
end do 

term(13) = -term(13) 
term(14) = -term(14) 
term(18) = -term(18) 
term(19) = -term(19) 
term(20) = -term(20) 
term(21) = -term(21) 


    v0_eom_cc3_31_trans_aibjcjbi = 0.d+0
    do s = 0, 23
    v0_eom_cc3_31_trans_aibjcjbi = v0_eom_cc3_31_trans_aibjcjbi + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcjbi
    function v0_eom_cc3_31_trans_aibjcibi(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: v0_eom_cc3_31_trans_aibjcibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * read_ftvvvv(c, e, a, b)
term(1) = term(1) + t2(b,e,i,j) * read_ftvvvv(c, e, a, b)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, e, a, b)
term(3) = term(3) + t2(c,e,i,j) * read_ftvvvv(b, e, a, b)
term(4) = term(4) + t2(a,e,i,j) * read_ftvvvv(c, b, b, e)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(c, b, a, e)
term(6) = term(6) + t2(a,e,i,j) * read_ftvvvv(c, e, b, b)
term(7) = term(7) + t2(c,e,j,i) * read_ftvvvv(b, b, a, e)
term(8) = term(8) + t2(c,e,j,i) * tvoov(a, i, i, e)
term(9) = term(9) + t2(c,e,i,j) * tvoov(a, i, i, e)
term(10) = term(10) + t2(a,e,i,j) * tvoov(c, i, i, e)
term(11) = term(11) + t2(a,e,i,i) * tvoov(c, j, i, e)
term(12) = term(12) + t2(a,e,i,j) * tvvoo(c, e, i, i)
term(13) = term(13) + t2(a,e,i,i) * tvvoo(c, e, i, j)
term(14) = term(14) + t2(c,e,i,i) * tvvoo(a, e, i, j)
term(15) = term(15) + t2(c,e,j,i) * tvvoo(a, e, i, i)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(6) = -term(6) 
term(7) = -term(7) 
term(9) = -term(9) 
term(10) = -term(10) 
term(13) = -term(13) 
term(14) = -term(14) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, i, m, b)
term(17) = term(17) + t2(b,c,m,j) * tvoov(a, i, m, b)
term(18) = term(18) + t2(a,b,m,j) * tvoov(c, i, m, b)
term(19) = term(19) + t2(a,c,i,m) * tvoov(b, j, m, b)
term(20) = term(20) + t2(a,b,i,m) * tvoov(c, j, m, b)
term(21) = term(21) + t2(a,c,m,i) * tvoov(b, j, m, b)
term(22) = term(22) + t2(a,c,m,j) * tvoov(b, i, m, b)
term(23) = term(23) + t2(a,b,m,i) * tvoov(c, j, m, b)
term(24) = term(24) + t2(b,c,j,m) * tvvoo(a, b, m, i)
term(25) = term(25) + t2(b,c,i,m) * tvvoo(a, b, m, j)
term(26) = term(26) + t2(b,c,m,j) * tvvoo(a, b, m, i)
term(27) = term(27) + t2(b,c,m,i) * tvvoo(a, b, m, j)
term(28) = term(28) + t2(a,b,i,m) * tvvoo(c, b, m, j)
term(29) = term(29) + t2(a,b,m,j) * tvvoo(c, b, m, i)
term(30) = term(30) + t2(a,c,i,m) * tvvoo(b, b, m, j)
term(31) = term(31) + t2(a,c,m,j) * tvvoo(b, b, m, i)
term(32) = term(32) + t2(a,c,i,m) * toooo(m, j, i, i)
term(33) = term(33) + t2(a,c,i,m) * toooo(m, i, i, j)
term(34) = term(34) + t2(a,c,m,i) * toooo(m, i, i, j)
term(35) = term(35) + t2(a,c,m,j) * toooo(m, i, i, i)
end do 

term(16) = -term(16) 
term(18) = -term(18) 
term(19) = -term(19) 
term(21) = -term(21) 
term(24) = -term(24) 
term(27) = -term(27) 
term(28) = -term(28) 
term(29) = -term(29) 
term(32) = -term(32) 
term(35) = -term(35) 


    v0_eom_cc3_31_trans_aibjcibi = 0.d+0
    do s = 0, 35
    v0_eom_cc3_31_trans_aibjcibi = v0_eom_cc3_31_trans_aibjcibi + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcibi
    function v0_eom_cc3_31_trans_aibjcibj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: v0_eom_cc3_31_trans_aibjcibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * tvoov(a, i, j, e)
term(1) = term(1) + t2(c,e,i,j) * tvoov(a, i, j, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(c, i, j, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(c, j, j, e)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(c, e, j, i)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(c, e, j, j)
term(6) = term(6) + t2(c,e,i,i) * tvvoo(a, e, j, j)
term(7) = term(7) + t2(c,e,j,i) * tvvoo(a, e, j, i)
term(8) = term(8) + t2(a,e,i,i) * read_ftvvvv(c, e, b, b)
term(9) = term(9) + t2(a,e,i,i) * read_ftvvvv(c, b, b, e)
term(10) = term(10) + t2(c,e,i,i) * read_ftvvvv(b, b, a, e)
term(11) = term(11) + t2(b,e,i,i) * read_ftvvvv(c, b, a, e)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(5) = -term(5) 
term(6) = -term(6) 
term(9) = -term(9) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,i,m) * toooo(m, j, j, i)
term(13) = term(13) + t2(a,c,i,m) * toooo(m, i, j, j)
term(14) = term(14) + t2(a,c,m,i) * toooo(m, i, j, j)
term(15) = term(15) + t2(a,c,m,j) * toooo(m, i, j, i)
term(16) = term(16) + t2(b,c,i,m) * tvoov(a, i, m, b)
term(17) = term(17) + t2(b,c,m,i) * tvoov(a, i, m, b)
term(18) = term(18) + t2(a,b,i,m) * tvoov(c, i, m, b)
term(19) = term(19) + t2(a,c,i,m) * tvoov(b, i, m, b)
term(20) = term(20) + t2(a,c,i,m) * tvvoo(b, b, m, i)
term(21) = term(21) + t2(a,b,i,m) * tvvoo(c, b, m, i)
term(22) = term(22) + t2(a,c,m,i) * tvvoo(b, b, m, i)
term(23) = term(23) + t2(a,b,m,i) * tvvoo(c, b, m, i)
end do 

term(12) = -term(12) 
term(15) = -term(15) 
term(17) = -term(17) 
term(18) = -term(18) 
term(20) = -term(20) 
term(22) = -term(22) 


    v0_eom_cc3_31_trans_aibjcibj = 0.d+0
    do s = 0, 23
    v0_eom_cc3_31_trans_aibjcibj = v0_eom_cc3_31_trans_aibjcibj + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcibj
    function v0_eom_cc3_31_trans_aibibkal(t2, nocc, nactive, i, b, k, l) 
    double precision :: v0_eom_cc3_31_trans_aibibkal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,k) * tvoov(b, i, l, e)
term(1) = term(1) + t2(b,e,i,i) * tvoov(b, k, l, e)
term(2) = term(2) + t2(b,e,k,i) * tvoov(b, i, l, e)
term(3) = term(3) + t2(b,e,i,i) * tvvoo(b, e, l, k)
term(4) = term(4) + t2(b,e,k,i) * tvvoo(b, e, l, i)
term(5) = term(5) + t2(b,e,i,k) * tvvoo(b, e, l, i)
end do 

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
term(6) = term(6) + t2(b,b,i,m) * toooo(m, i, l, k)
term(7) = term(7) + t2(b,b,k,m) * toooo(m, i, l, i)
term(8) = term(8) + t2(b,b,i,m) * toooo(m, k, l, i)
end do 

term(6) = term(6) * (-2.0d+0) 


    v0_eom_cc3_31_trans_aibibkal = 0.d+0
    do s = 0, 8
    v0_eom_cc3_31_trans_aibibkal = v0_eom_cc3_31_trans_aibibkal + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibibkal
    function v0_eom_cc3_31_trans_aibibkdk(t2, nocc, nactive, a, i, b, d) 
    double precision :: v0_eom_cc3_31_trans_aibibkdk 
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


    v0_eom_cc3_31_trans_aibibkdk = 0.d+0
    do s = 0, 8
    v0_eom_cc3_31_trans_aibibkdk = v0_eom_cc3_31_trans_aibibkdk + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibibkdk
    function v0_eom_cc3_31_trans_aibibkdi(t2, nocc, nactive, a, i, b, k, d) 
    double precision :: v0_eom_cc3_31_trans_aibibkdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * read_ftvvvv(b, e, a, d)
term(1) = term(1) + t2(b,e,i,k) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(b, e, b, d)
term(3) = term(3) + t2(b,e,i,k) * read_ftvvvv(b, d, a, e)
term(4) = term(4) + t2(a,e,i,k) * read_ftvvvv(b, e, b, d)
term(5) = term(5) + t2(b,e,k,i) * read_ftvvvv(b, d, a, e)
end do 

term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(b,b,i,m) * tvoov(a, k, m, d)
term(7) = term(7) + t2(b,b,k,m) * tvoov(a, i, m, d)
term(8) = term(8) + t2(a,b,k,m) * tvoov(b, i, m, d)
term(9) = term(9) + t2(a,b,i,m) * tvoov(b, k, m, d)
term(10) = term(10) + t2(a,b,m,i) * tvoov(b, k, m, d)
term(11) = term(11) + t2(a,b,m,k) * tvoov(b, i, m, d)
term(12) = term(12) + t2(b,b,k,m) * tvvoo(a, d, m, i)
term(13) = term(13) + t2(b,b,i,m) * tvvoo(a, d, m, k)
term(14) = term(14) + t2(a,b,k,m) * tvvoo(b, d, m, i)
term(15) = term(15) + t2(a,b,m,i) * tvvoo(b, d, m, k)
term(16) = term(16) + t2(a,b,i,m) * tvvoo(b, d, m, k)
term(17) = term(17) + t2(a,b,m,k) * tvvoo(b, d, m, i)
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


    v0_eom_cc3_31_trans_aibibkdi = 0.d+0
    do s = 0, 17
    v0_eom_cc3_31_trans_aibibkdi = v0_eom_cc3_31_trans_aibibkdi + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibibkdi
    function v0_eom_cc3_31_trans_aibjcjaj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: v0_eom_cc3_31_trans_aibjcjaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(c,e,i,j) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, e, b, a)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(c, e, b, a)
term(4) = term(4) + t2(a,e,i,j) * read_ftvvvv(c, a, b, e)
term(5) = term(5) + t2(c,e,i,j) * read_ftvvvv(b, a, a, e)
term(6) = term(6) + t2(c,e,j,i) * read_ftvvvv(b, a, a, e)
term(7) = term(7) + t2(b,e,j,i) * read_ftvvvv(c, a, a, e)
term(8) = term(8) + t2(b,e,j,j) * tvoov(c, i, j, e)
term(9) = term(9) + t2(c,e,i,j) * tvoov(b, j, j, e)
term(10) = term(10) + t2(c,e,j,i) * tvoov(b, j, j, e)
term(11) = term(11) + t2(b,e,j,i) * tvoov(c, j, j, e)
term(12) = term(12) + t2(b,e,j,i) * tvvoo(c, e, j, j)
term(13) = term(13) + t2(b,e,j,j) * tvvoo(c, e, j, i)
term(14) = term(14) + t2(c,e,j,j) * tvvoo(b, e, j, i)
term(15) = term(15) + t2(c,e,i,j) * tvvoo(b, e, j, j)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(3) = -term(3) 
term(5) = -term(5) 
term(10) = -term(10) 
term(11) = -term(11) 
term(13) = -term(13) 
term(14) = -term(14) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, i, m, a)
term(17) = term(17) + t2(b,c,m,j) * tvoov(a, i, m, a)
term(18) = term(18) + t2(b,c,m,i) * tvoov(a, j, m, a)
term(19) = term(19) + t2(a,b,j,m) * tvoov(c, i, m, a)
term(20) = term(20) + t2(a,b,m,j) * tvoov(c, i, m, a)
term(21) = term(21) + t2(a,c,i,m) * tvoov(b, j, m, a)
term(22) = term(22) + t2(a,b,i,m) * tvoov(c, j, m, a)
term(23) = term(23) + t2(a,c,m,i) * tvoov(b, j, m, a)
term(24) = term(24) + t2(b,c,j,m) * tvvoo(a, a, m, i)
term(25) = term(25) + t2(b,c,m,i) * tvvoo(a, a, m, j)
term(26) = term(26) + t2(a,c,i,m) * tvvoo(b, a, m, j)
term(27) = term(27) + t2(a,c,j,m) * tvvoo(b, a, m, i)
term(28) = term(28) + t2(a,b,i,m) * tvvoo(c, a, m, j)
term(29) = term(29) + t2(a,c,m,i) * tvvoo(b, a, m, j)
term(30) = term(30) + t2(a,c,m,j) * tvvoo(b, a, m, i)
term(31) = term(31) + t2(a,b,m,j) * tvvoo(c, a, m, i)
term(32) = term(32) + t2(b,c,j,m) * toooo(m, i, j, j)
term(33) = term(33) + t2(b,c,j,m) * toooo(m, j, j, i)
term(34) = term(34) + t2(b,c,m,j) * toooo(m, j, j, i)
term(35) = term(35) + t2(b,c,m,i) * toooo(m, j, j, j)
end do 

term(16) = -term(16) 
term(17) = -term(17) 
term(21) = -term(21) 
term(22) = -term(22) 
term(26) = -term(26) 
term(28) = -term(28) 
term(30) = -term(30) 
term(31) = -term(31) 
term(32) = -term(32) 
term(35) = -term(35) 


    v0_eom_cc3_31_trans_aibjcjaj = 0.d+0
    do s = 0, 35
    v0_eom_cc3_31_trans_aibjcjaj = v0_eom_cc3_31_trans_aibjcjaj + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcjaj
    function v0_eom_cc3_31_trans_aibjcjai(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: v0_eom_cc3_31_trans_aibjcjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,j) * tvoov(c, i, i, e)
term(1) = term(1) + t2(c,e,i,j) * tvoov(b, j, i, e)
term(2) = term(2) + t2(c,e,j,i) * tvoov(b, j, i, e)
term(3) = term(3) + t2(b,e,j,i) * tvoov(c, j, i, e)
term(4) = term(4) + t2(b,e,j,i) * tvvoo(c, e, i, j)
term(5) = term(5) + t2(b,e,j,j) * tvvoo(c, e, i, i)
term(6) = term(6) + t2(c,e,j,j) * tvvoo(b, e, i, i)
term(7) = term(7) + t2(c,e,i,j) * tvvoo(b, e, i, j)
term(8) = term(8) + t2(b,e,j,j) * read_ftvvvv(c, e, a, a)
term(9) = term(9) + t2(c,e,j,j) * read_ftvvvv(b, e, a, a)
term(10) = term(10) + t2(a,e,j,j) * read_ftvvvv(c, a, b, e)
term(11) = term(11) + t2(b,e,j,j) * read_ftvvvv(c, a, a, e)
end do 

term(2) = -term(2) 
term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 
term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,j,m) * toooo(m, i, i, j)
term(13) = term(13) + t2(b,c,j,m) * toooo(m, j, i, i)
term(14) = term(14) + t2(b,c,m,j) * toooo(m, j, i, i)
term(15) = term(15) + t2(b,c,m,i) * toooo(m, j, i, j)
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, j, m, a)
term(17) = term(17) + t2(a,c,j,m) * tvoov(b, j, m, a)
term(18) = term(18) + t2(a,c,m,j) * tvoov(b, j, m, a)
term(19) = term(19) + t2(a,b,m,j) * tvoov(c, j, m, a)
term(20) = term(20) + t2(b,c,j,m) * tvvoo(a, a, m, j)
term(21) = term(21) + t2(b,c,m,j) * tvvoo(a, a, m, j)
term(22) = term(22) + t2(a,b,j,m) * tvvoo(c, a, m, j)
term(23) = term(23) + t2(a,b,m,j) * tvvoo(c, a, m, j)
end do 

term(12) = -term(12) 
term(15) = -term(15) 
term(18) = -term(18) 
term(19) = -term(19) 
term(20) = -term(20) 
term(21) = -term(21) 


    v0_eom_cc3_31_trans_aibjcjai = 0.d+0
    do s = 0, 23
    v0_eom_cc3_31_trans_aibjcjai = v0_eom_cc3_31_trans_aibjcjai + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcjai
    function v0_eom_cc3_31_trans_aibjciai(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: v0_eom_cc3_31_trans_aibjciai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(b,e,i,j) * read_ftvvvv(c, e, a, a)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, e, a, a)
term(3) = term(3) + t2(c,e,i,j) * read_ftvvvv(b, e, a, a)
term(4) = term(4) + t2(a,e,i,j) * read_ftvvvv(c, a, b, e)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(c, a, a, e)
term(6) = term(6) + t2(a,e,i,j) * read_ftvvvv(c, e, b, a)
term(7) = term(7) + t2(c,e,j,i) * read_ftvvvv(b, a, a, e)
term(8) = term(8) + t2(b,e,j,i) * tvoov(c, i, i, e)
term(9) = term(9) + t2(c,e,i,i) * tvoov(b, j, i, e)
term(10) = term(10) + t2(c,e,j,i) * tvoov(b, i, i, e)
term(11) = term(11) + t2(b,e,i,i) * tvoov(c, j, i, e)
term(12) = term(12) + t2(b,e,j,i) * tvvoo(c, e, i, i)
term(13) = term(13) + t2(b,e,i,j) * tvvoo(c, e, i, i)
term(14) = term(14) + t2(c,e,j,i) * tvvoo(b, e, i, i)
term(15) = term(15) + t2(c,e,i,j) * tvvoo(b, e, i, i)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(6) = -term(6) 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = -term(9) 
term(12) = -term(12) 
term(15) = -term(15) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, i, m, a)
term(17) = term(17) + t2(b,c,m,j) * tvoov(a, i, m, a)
term(18) = term(18) + t2(a,b,m,j) * tvoov(c, i, m, a)
term(19) = term(19) + t2(a,c,i,m) * tvoov(b, j, m, a)
term(20) = term(20) + t2(a,b,i,m) * tvoov(c, j, m, a)
term(21) = term(21) + t2(a,c,m,i) * tvoov(b, j, m, a)
term(22) = term(22) + t2(a,c,m,j) * tvoov(b, i, m, a)
term(23) = term(23) + t2(a,b,m,i) * tvoov(c, j, m, a)
term(24) = term(24) + t2(b,c,j,m) * tvvoo(a, a, m, i)
term(25) = term(25) + t2(b,c,i,m) * tvvoo(a, a, m, j)
term(26) = term(26) + t2(b,c,m,j) * tvvoo(a, a, m, i)
term(27) = term(27) + t2(b,c,m,i) * tvvoo(a, a, m, j)
term(28) = term(28) + t2(a,b,i,m) * tvvoo(c, a, m, j)
term(29) = term(29) + t2(a,b,m,j) * tvvoo(c, a, m, i)
term(30) = term(30) + t2(a,c,i,m) * tvvoo(b, a, m, j)
term(31) = term(31) + t2(a,c,m,j) * tvvoo(b, a, m, i)
term(32) = term(32) + t2(b,c,j,m) * toooo(m, i, i, i)
term(33) = term(33) + t2(b,c,i,m) * toooo(m, j, i, i)
term(34) = term(34) + t2(b,c,m,j) * toooo(m, i, i, i)
term(35) = term(35) + t2(b,c,m,i) * toooo(m, j, i, i)
end do 

term(16) = -term(16) 
term(18) = -term(18) 
term(19) = -term(19) 
term(21) = -term(21) 
term(24) = -term(24) 
term(27) = -term(27) 
term(28) = -term(28) 
term(29) = -term(29) 
term(33) = -term(33) 
term(34) = -term(34) 


    v0_eom_cc3_31_trans_aibjciai = 0.d+0
    do s = 0, 35
    v0_eom_cc3_31_trans_aibjciai = v0_eom_cc3_31_trans_aibjciai + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjciai
    function v0_eom_cc3_31_trans_aibjciaj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: v0_eom_cc3_31_trans_aibjciaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tvoov(c, i, j, e)
term(1) = term(1) + t2(c,e,i,i) * tvoov(b, j, j, e)
term(2) = term(2) + t2(c,e,j,i) * tvoov(b, i, j, e)
term(3) = term(3) + t2(b,e,i,i) * tvoov(c, j, j, e)
term(4) = term(4) + t2(b,e,j,i) * tvvoo(c, e, j, i)
term(5) = term(5) + t2(b,e,i,j) * tvvoo(c, e, j, i)
term(6) = term(6) + t2(c,e,j,i) * tvvoo(b, e, j, i)
term(7) = term(7) + t2(c,e,i,j) * tvvoo(b, e, j, i)
term(8) = term(8) + t2(a,e,i,i) * read_ftvvvv(c, e, b, a)
term(9) = term(9) + t2(a,e,i,i) * read_ftvvvv(c, a, b, e)
term(10) = term(10) + t2(c,e,i,i) * read_ftvvvv(b, a, a, e)
term(11) = term(11) + t2(b,e,i,i) * read_ftvvvv(c, a, a, e)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(4) = -term(4) 
term(7) = -term(7) 
term(9) = -term(9) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,j,m) * toooo(m, i, j, i)
term(13) = term(13) + t2(b,c,i,m) * toooo(m, j, j, i)
term(14) = term(14) + t2(b,c,m,j) * toooo(m, i, j, i)
term(15) = term(15) + t2(b,c,m,i) * toooo(m, j, j, i)
term(16) = term(16) + t2(b,c,i,m) * tvoov(a, i, m, a)
term(17) = term(17) + t2(b,c,m,i) * tvoov(a, i, m, a)
term(18) = term(18) + t2(a,b,i,m) * tvoov(c, i, m, a)
term(19) = term(19) + t2(a,c,i,m) * tvoov(b, i, m, a)
term(20) = term(20) + t2(a,c,i,m) * tvvoo(b, a, m, i)
term(21) = term(21) + t2(a,b,i,m) * tvvoo(c, a, m, i)
term(22) = term(22) + t2(a,c,m,i) * tvvoo(b, a, m, i)
term(23) = term(23) + t2(a,b,m,i) * tvvoo(c, a, m, i)
end do 

term(13) = -term(13) 
term(14) = -term(14) 
term(17) = -term(17) 
term(18) = -term(18) 
term(20) = -term(20) 
term(22) = -term(22) 


    v0_eom_cc3_31_trans_aibjciaj = 0.d+0
    do s = 0, 23
    v0_eom_cc3_31_trans_aibjciaj = v0_eom_cc3_31_trans_aibjciaj + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjciaj
    function v0_eom_cc3_31_trans_aibjcjcj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: v0_eom_cc3_31_trans_aibjcjcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,i,j) * read_ftvvvv(b, e, a, c)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, e, b, c)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(c, e, b, c)
term(4) = term(4) + t2(a,e,i,j) * read_ftvvvv(c, c, b, e)
term(5) = term(5) + t2(c,e,i,j) * read_ftvvvv(b, c, a, e)
term(6) = term(6) + t2(c,e,j,i) * read_ftvvvv(b, c, a, e)
term(7) = term(7) + t2(b,e,j,i) * read_ftvvvv(c, c, a, e)
term(8) = term(8) + t2(b,e,j,i) * tvoov(a, j, j, e)
term(9) = term(9) + t2(b,e,j,j) * tvoov(a, i, j, e)
term(10) = term(10) + t2(a,e,i,j) * tvoov(b, j, j, e)
term(11) = term(11) + t2(a,e,j,i) * tvoov(b, j, j, e)
term(12) = term(12) + t2(a,e,j,j) * tvvoo(b, e, j, i)
term(13) = term(13) + t2(b,e,j,j) * tvvoo(a, e, j, i)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(b, e, j, j)
term(15) = term(15) + t2(b,e,j,i) * tvvoo(a, e, j, j)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(3) = -term(3) 
term(5) = -term(5) 
term(9) = -term(9) 
term(10) = -term(10) 
term(14) = -term(14) 
term(15) = -term(15) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, i, m, c)
term(17) = term(17) + t2(b,c,m,j) * tvoov(a, i, m, c)
term(18) = term(18) + t2(b,c,m,i) * tvoov(a, j, m, c)
term(19) = term(19) + t2(a,b,j,m) * tvoov(c, i, m, c)
term(20) = term(20) + t2(a,b,m,j) * tvoov(c, i, m, c)
term(21) = term(21) + t2(a,c,i,m) * tvoov(b, j, m, c)
term(22) = term(22) + t2(a,b,i,m) * tvoov(c, j, m, c)
term(23) = term(23) + t2(a,c,m,i) * tvoov(b, j, m, c)
term(24) = term(24) + t2(b,c,j,m) * tvvoo(a, c, m, i)
term(25) = term(25) + t2(b,c,m,i) * tvvoo(a, c, m, j)
term(26) = term(26) + t2(a,c,i,m) * tvvoo(b, c, m, j)
term(27) = term(27) + t2(a,c,j,m) * tvvoo(b, c, m, i)
term(28) = term(28) + t2(a,b,i,m) * tvvoo(c, c, m, j)
term(29) = term(29) + t2(a,c,m,i) * tvvoo(b, c, m, j)
term(30) = term(30) + t2(a,c,m,j) * tvvoo(b, c, m, i)
term(31) = term(31) + t2(a,b,m,j) * tvvoo(c, c, m, i)
term(32) = term(32) + t2(a,b,j,m) * toooo(m, j, j, i)
term(33) = term(33) + t2(a,b,m,j) * toooo(m, j, j, i)
term(34) = term(34) + t2(a,b,i,m) * toooo(m, j, j, j)
term(35) = term(35) + t2(a,b,m,j) * toooo(m, i, j, j)
end do 

term(16) = -term(16) 
term(17) = -term(17) 
term(21) = -term(21) 
term(22) = -term(22) 
term(26) = -term(26) 
term(28) = -term(28) 
term(30) = -term(30) 
term(31) = -term(31) 
term(32) = -term(32) 
term(33) = -term(33) 


    v0_eom_cc3_31_trans_aibjcjcj = 0.d+0
    do s = 0, 35
    v0_eom_cc3_31_trans_aibjcjcj = v0_eom_cc3_31_trans_aibjcjcj + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcjcj
    function v0_eom_cc3_31_trans_aibjcjci(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: v0_eom_cc3_31_trans_aibjcjci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tvoov(a, j, i, e)
term(1) = term(1) + t2(b,e,j,j) * tvoov(a, i, i, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(b, j, i, e)
term(3) = term(3) + t2(a,e,j,i) * tvoov(b, j, i, e)
term(4) = term(4) + t2(a,e,j,j) * tvvoo(b, e, i, i)
term(5) = term(5) + t2(b,e,j,j) * tvvoo(a, e, i, i)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(b, e, i, j)
term(7) = term(7) + t2(b,e,j,i) * tvvoo(a, e, i, j)
term(8) = term(8) + t2(b,e,j,j) * read_ftvvvv(c, e, a, c)
term(9) = term(9) + t2(c,e,j,j) * read_ftvvvv(b, e, a, c)
term(10) = term(10) + t2(a,e,j,j) * read_ftvvvv(c, c, b, e)
term(11) = term(11) + t2(b,e,j,j) * read_ftvvvv(c, c, a, e)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(6) = -term(6) 
term(7) = -term(7) 
term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,j,m) * toooo(m, j, i, i)
term(13) = term(13) + t2(a,b,m,j) * toooo(m, j, i, i)
term(14) = term(14) + t2(a,b,i,m) * toooo(m, j, i, j)
term(15) = term(15) + t2(a,b,m,j) * toooo(m, i, i, j)
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, j, m, c)
term(17) = term(17) + t2(a,c,j,m) * tvoov(b, j, m, c)
term(18) = term(18) + t2(a,c,m,j) * tvoov(b, j, m, c)
term(19) = term(19) + t2(a,b,m,j) * tvoov(c, j, m, c)
term(20) = term(20) + t2(b,c,j,m) * tvvoo(a, c, m, j)
term(21) = term(21) + t2(b,c,m,j) * tvvoo(a, c, m, j)
term(22) = term(22) + t2(a,b,j,m) * tvvoo(c, c, m, j)
term(23) = term(23) + t2(a,b,m,j) * tvvoo(c, c, m, j)
end do 

term(12) = -term(12) 
term(13) = -term(13) 
term(18) = -term(18) 
term(19) = -term(19) 
term(20) = -term(20) 
term(21) = -term(21) 


    v0_eom_cc3_31_trans_aibjcjci = 0.d+0
    do s = 0, 23
    v0_eom_cc3_31_trans_aibjcjci = v0_eom_cc3_31_trans_aibjcjci + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcjci
    function v0_eom_cc3_31_trans_aibjcici(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: v0_eom_cc3_31_trans_aibjcici 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(b,e,i,j) * read_ftvvvv(c, e, a, c)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, e, a, c)
term(3) = term(3) + t2(c,e,i,j) * read_ftvvvv(b, e, a, c)
term(4) = term(4) + t2(a,e,i,j) * read_ftvvvv(c, c, b, e)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(c, c, a, e)
term(6) = term(6) + t2(a,e,i,j) * read_ftvvvv(c, e, b, c)
term(7) = term(7) + t2(c,e,j,i) * read_ftvvvv(b, c, a, e)
term(8) = term(8) + t2(b,e,j,i) * tvoov(a, i, i, e)
term(9) = term(9) + t2(b,e,i,j) * tvoov(a, i, i, e)
term(10) = term(10) + t2(a,e,i,j) * tvoov(b, i, i, e)
term(11) = term(11) + t2(a,e,i,i) * tvoov(b, j, i, e)
term(12) = term(12) + t2(a,e,i,j) * tvvoo(b, e, i, i)
term(13) = term(13) + t2(b,e,j,i) * tvvoo(a, e, i, i)
term(14) = term(14) + t2(a,e,i,i) * tvvoo(b, e, i, j)
term(15) = term(15) + t2(b,e,i,i) * tvvoo(a, e, i, j)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(6) = -term(6) 
term(7) = -term(7) 
term(8) = -term(8) 
term(11) = -term(11) 
term(12) = -term(12) 
term(13) = -term(13) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, i, m, c)
term(17) = term(17) + t2(b,c,m,j) * tvoov(a, i, m, c)
term(18) = term(18) + t2(a,b,m,j) * tvoov(c, i, m, c)
term(19) = term(19) + t2(a,c,i,m) * tvoov(b, j, m, c)
term(20) = term(20) + t2(a,b,i,m) * tvoov(c, j, m, c)
term(21) = term(21) + t2(a,c,m,i) * tvoov(b, j, m, c)
term(22) = term(22) + t2(a,c,m,j) * tvoov(b, i, m, c)
term(23) = term(23) + t2(a,b,m,i) * tvoov(c, j, m, c)
term(24) = term(24) + t2(b,c,j,m) * tvvoo(a, c, m, i)
term(25) = term(25) + t2(b,c,i,m) * tvvoo(a, c, m, j)
term(26) = term(26) + t2(b,c,m,j) * tvvoo(a, c, m, i)
term(27) = term(27) + t2(b,c,m,i) * tvvoo(a, c, m, j)
term(28) = term(28) + t2(a,b,i,m) * tvvoo(c, c, m, j)
term(29) = term(29) + t2(a,b,m,j) * tvvoo(c, c, m, i)
term(30) = term(30) + t2(a,c,i,m) * tvvoo(b, c, m, j)
term(31) = term(31) + t2(a,c,m,j) * tvvoo(b, c, m, i)
term(32) = term(32) + t2(a,b,i,m) * toooo(m, j, i, i)
term(33) = term(33) + t2(a,b,m,j) * toooo(m, i, i, i)
term(34) = term(34) + t2(a,b,i,m) * toooo(m, i, i, j)
term(35) = term(35) + t2(a,b,m,i) * toooo(m, i, i, j)
end do 

term(16) = -term(16) 
term(18) = -term(18) 
term(19) = -term(19) 
term(21) = -term(21) 
term(24) = -term(24) 
term(27) = -term(27) 
term(28) = -term(28) 
term(29) = -term(29) 
term(34) = -term(34) 
term(35) = -term(35) 


    v0_eom_cc3_31_trans_aibjcici = 0.d+0
    do s = 0, 35
    v0_eom_cc3_31_trans_aibjcici = v0_eom_cc3_31_trans_aibjcici + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcici
    function v0_eom_cc3_31_trans_aibjcicj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: v0_eom_cc3_31_trans_aibjcicj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tvoov(a, i, j, e)
term(1) = term(1) + t2(b,e,i,j) * tvoov(a, i, j, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(b, i, j, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(b, j, j, e)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(b, e, j, i)
term(5) = term(5) + t2(b,e,j,i) * tvvoo(a, e, j, i)
term(6) = term(6) + t2(a,e,i,i) * tvvoo(b, e, j, j)
term(7) = term(7) + t2(b,e,i,i) * tvvoo(a, e, j, j)
term(8) = term(8) + t2(a,e,i,i) * read_ftvvvv(c, e, b, c)
term(9) = term(9) + t2(a,e,i,i) * read_ftvvvv(c, c, b, e)
term(10) = term(10) + t2(c,e,i,i) * read_ftvvvv(b, c, a, e)
term(11) = term(11) + t2(b,e,i,i) * read_ftvvvv(c, c, a, e)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 
term(9) = -term(9) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,i,m) * toooo(m, j, j, i)
term(13) = term(13) + t2(a,b,m,j) * toooo(m, i, j, i)
term(14) = term(14) + t2(a,b,i,m) * toooo(m, i, j, j)
term(15) = term(15) + t2(a,b,m,i) * toooo(m, i, j, j)
term(16) = term(16) + t2(b,c,i,m) * tvoov(a, i, m, c)
term(17) = term(17) + t2(b,c,m,i) * tvoov(a, i, m, c)
term(18) = term(18) + t2(a,b,i,m) * tvoov(c, i, m, c)
term(19) = term(19) + t2(a,c,i,m) * tvoov(b, i, m, c)
term(20) = term(20) + t2(a,c,i,m) * tvvoo(b, c, m, i)
term(21) = term(21) + t2(a,b,i,m) * tvvoo(c, c, m, i)
term(22) = term(22) + t2(a,c,m,i) * tvvoo(b, c, m, i)
term(23) = term(23) + t2(a,b,m,i) * tvvoo(c, c, m, i)
end do 

term(14) = -term(14) 
term(15) = -term(15) 
term(17) = -term(17) 
term(18) = -term(18) 
term(20) = -term(20) 
term(22) = -term(22) 


    v0_eom_cc3_31_trans_aibjcicj = 0.d+0
    do s = 0, 23
    v0_eom_cc3_31_trans_aibjcicj = v0_eom_cc3_31_trans_aibjcicj + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibjcicj
    function v0_eom_cc3_31_trans_aiajcjaj(t2, nocc, nactive, a, i, j, c) 
    double precision :: v0_eom_cc3_31_trans_aiajcjaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,j) * tvoov(a, i, j, e)
term(1) = term(1) + t2(c,e,i,j) * tvoov(a, j, j, e)
term(2) = term(2) + t2(a,e,j,j) * tvoov(c, i, j, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(c, j, j, e)
term(4) = term(4) + t2(c,e,j,i) * tvoov(a, j, j, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(c, j, j, e)
term(6) = term(6) + t2(a,e,j,i) * read_ftvvvv(c, e, a, a)
term(7) = term(7) + t2(c,e,i,j) * read_ftvvvv(a, e, a, a)
term(8) = term(8) + t2(a,e,i,j) * read_ftvvvv(c, e, a, a)
term(9) = term(9) + t2(a,e,i,j) * read_ftvvvv(c, a, a, e)
term(10) = term(10) + t2(c,e,j,i) * read_ftvvvv(a, e, a, a)
term(11) = term(11) + t2(a,e,j,i) * read_ftvvvv(c, a, a, e)
term(12) = term(12) + t2(a,e,j,i) * tvvoo(c, e, j, j)
term(13) = term(13) + t2(a,e,j,j) * tvvoo(c, e, j, i)
term(14) = term(14) + t2(c,e,j,j) * tvvoo(a, e, j, i)
term(15) = term(15) + t2(c,e,i,j) * tvvoo(a, e, j, j)
term(16) = term(16) + t2(a,e,i,j) * tvvoo(c, e, j, j)
term(17) = term(17) + t2(c,e,j,i) * tvvoo(a, e, j, j)
end do 

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 
term(12) = term(12) * 2.0d+0 
term(13) = -term(13) 
term(14) = -term(14) 
term(15) = term(15) * 2.0d+0 
term(16) = -term(16) 
term(17) = -term(17) 

do m = 1, nocc 
term(18) = term(18) + t2(a,c,j,m) * tvoov(a, i, m, a)
term(19) = term(19) + t2(a,c,m,j) * tvoov(a, i, m, a)
term(20) = term(20) + t2(a,c,m,i) * tvoov(a, j, m, a)
term(21) = term(21) + t2(a,a,j,m) * tvoov(c, i, m, a)
term(22) = term(22) + t2(a,c,i,m) * tvoov(a, j, m, a)
term(23) = term(23) + t2(a,a,i,m) * tvoov(c, j, m, a)
term(24) = term(24) + t2(a,c,j,m) * tvvoo(a, a, m, i)
term(25) = term(25) + t2(a,c,m,i) * tvvoo(a, a, m, j)
term(26) = term(26) + t2(a,c,i,m) * tvvoo(a, a, m, j)
term(27) = term(27) + t2(a,a,i,m) * tvvoo(c, a, m, j)
term(28) = term(28) + t2(a,c,m,j) * tvvoo(a, a, m, i)
term(29) = term(29) + t2(a,a,j,m) * tvvoo(c, a, m, i)
term(30) = term(30) + t2(a,c,j,m) * toooo(m, i, j, j)
term(31) = term(31) + t2(a,c,j,m) * toooo(m, j, j, i)
term(32) = term(32) + t2(a,c,m,j) * toooo(m, j, j, i)
term(33) = term(33) + t2(a,c,m,i) * toooo(m, j, j, j)
term(34) = term(34) + t2(a,c,i,m) * toooo(m, j, j, j)
term(35) = term(35) + t2(a,c,m,j) * toooo(m, i, j, j)
end do 

term(18) = -term(18) 
term(19) = -term(19) 
term(20) = term(20) * 2.0d+0 
term(21) = term(21) * 2.0d+0 
term(22) = -term(22) 
term(23) = -term(23) 
term(24) = term(24) * 2.0d+0 
term(25) = term(25) * 2.0d+0 
term(26) = -term(26) 
term(27) = -term(27) 
term(28) = -term(28) 
term(29) = -term(29) 
term(30) = term(30) * (-2.0d+0) 
term(33) = term(33) * (-2.0d+0) 


    v0_eom_cc3_31_trans_aiajcjaj = 0.d+0
    do s = 0, 35
    v0_eom_cc3_31_trans_aiajcjaj = v0_eom_cc3_31_trans_aiajcjaj + term(s)
    end do

    end function v0_eom_cc3_31_trans_aiajcjaj
    function v0_eom_cc3_31_trans_aiajcjai(t2, nocc, nactive, a, i, j, c) 
    double precision :: v0_eom_cc3_31_trans_aiajcjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:26) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,j) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(c,e,j,j) * read_ftvvvv(a, e, a, a)
term(2) = term(2) + t2(a,e,j,j) * read_ftvvvv(c, a, a, e)
term(3) = term(3) + t2(c,e,j,j) * tvoov(a, i, i, e)
term(4) = term(4) + t2(c,e,i,j) * tvoov(a, j, i, e)
term(5) = term(5) + t2(a,e,j,j) * tvoov(c, i, i, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(c, j, i, e)
term(7) = term(7) + t2(c,e,j,i) * tvoov(a, j, i, e)
term(8) = term(8) + t2(a,e,j,i) * tvoov(c, j, i, e)
term(9) = term(9) + t2(a,e,j,i) * tvvoo(c, e, i, j)
term(10) = term(10) + t2(a,e,j,j) * tvvoo(c, e, i, i)
term(11) = term(11) + t2(c,e,j,j) * tvvoo(a, e, i, i)
term(12) = term(12) + t2(c,e,i,j) * tvvoo(a, e, i, j)
term(13) = term(13) + t2(a,e,i,j) * tvvoo(c, e, i, j)
term(14) = term(14) + t2(c,e,j,i) * tvvoo(a, e, i, j)
end do 

term(2) = term(2) * (-2.0d+0) 
term(3) = -term(3) 
term(4) = term(4) * 2.0d+0 
term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 
term(11) = -term(11) 
term(12) = term(12) * 2.0d+0 
term(13) = -term(13) 
term(14) = -term(14) 

do m = 1, nocc 
term(15) = term(15) + t2(a,c,j,m) * tvoov(a, j, m, a)
term(16) = term(16) + t2(a,c,m,j) * tvoov(a, j, m, a)
term(17) = term(17) + t2(a,a,j,m) * tvoov(c, j, m, a)
term(18) = term(18) + t2(a,c,j,m) * tvvoo(a, a, m, j)
term(19) = term(19) + t2(a,c,m,j) * tvvoo(a, a, m, j)
term(20) = term(20) + t2(a,a,j,m) * tvvoo(c, a, m, j)
term(21) = term(21) + t2(a,c,j,m) * toooo(m, i, i, j)
term(22) = term(22) + t2(a,c,j,m) * toooo(m, j, i, i)
term(23) = term(23) + t2(a,c,m,j) * toooo(m, j, i, i)
term(24) = term(24) + t2(a,c,m,i) * toooo(m, j, i, j)
term(25) = term(25) + t2(a,c,i,m) * toooo(m, j, i, j)
term(26) = term(26) + t2(a,c,m,j) * toooo(m, i, i, j)
end do 

term(15) = term(15) * 2.0d+0 
term(16) = -term(16) 
term(17) = -term(17) 
term(18) = -term(18) 
term(19) = -term(19) 
term(20) = term(20) * 2.0d+0 
term(21) = term(21) * (-2.0d+0) 
term(24) = term(24) * (-2.0d+0) 


    v0_eom_cc3_31_trans_aiajcjai = 0.d+0
    do s = 0, 26
    v0_eom_cc3_31_trans_aiajcjai = v0_eom_cc3_31_trans_aiajcjai + term(s)
    end do

    end function v0_eom_cc3_31_trans_aiajcjai
    function v0_eom_cc3_31_trans_aiajcjcj(t2, nocc, nactive, a, i, j, c) 
    double precision :: v0_eom_cc3_31_trans_aiajcjcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:26) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,i,j) * read_ftvvvv(a, e, a, c)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, e, a, c)
term(3) = term(3) + t2(a,e,i,j) * read_ftvvvv(c, c, a, e)
term(4) = term(4) + t2(c,e,j,i) * read_ftvvvv(a, e, a, c)
term(5) = term(5) + t2(a,e,j,i) * read_ftvvvv(c, c, a, e)
term(6) = term(6) + t2(a,e,j,i) * tvoov(a, j, j, e)
term(7) = term(7) + t2(a,e,j,j) * tvoov(a, i, j, e)
term(8) = term(8) + t2(a,e,i,j) * tvoov(a, j, j, e)
term(9) = term(9) + t2(a,e,j,j) * tvvoo(a, e, j, i)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(a, e, j, j)
term(11) = term(11) + t2(a,e,j,i) * tvvoo(a, e, j, j)
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (-2.0d+0) 
term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,j,m) * tvoov(a, i, m, c)
term(13) = term(13) + t2(a,c,m,j) * tvoov(a, i, m, c)
term(14) = term(14) + t2(a,c,m,i) * tvoov(a, j, m, c)
term(15) = term(15) + t2(a,a,j,m) * tvoov(c, i, m, c)
term(16) = term(16) + t2(a,c,i,m) * tvoov(a, j, m, c)
term(17) = term(17) + t2(a,a,i,m) * tvoov(c, j, m, c)
term(18) = term(18) + t2(a,c,j,m) * tvvoo(a, c, m, i)
term(19) = term(19) + t2(a,c,m,i) * tvvoo(a, c, m, j)
term(20) = term(20) + t2(a,c,i,m) * tvvoo(a, c, m, j)
term(21) = term(21) + t2(a,a,i,m) * tvvoo(c, c, m, j)
term(22) = term(22) + t2(a,c,m,j) * tvvoo(a, c, m, i)
term(23) = term(23) + t2(a,a,j,m) * tvvoo(c, c, m, i)
term(24) = term(24) + t2(a,a,j,m) * toooo(m, j, j, i)
term(25) = term(25) + t2(a,a,i,m) * toooo(m, j, j, j)
term(26) = term(26) + t2(a,a,j,m) * toooo(m, i, j, j)
end do 

term(12) = -term(12) 
term(13) = -term(13) 
term(14) = term(14) * 2.0d+0 
term(15) = term(15) * 2.0d+0 
term(16) = -term(16) 
term(17) = -term(17) 
term(18) = term(18) * 2.0d+0 
term(19) = term(19) * 2.0d+0 
term(20) = -term(20) 
term(21) = -term(21) 
term(22) = -term(22) 
term(23) = -term(23) 
term(24) = term(24) * (-2.0d+0) 


    v0_eom_cc3_31_trans_aiajcjcj = 0.d+0
    do s = 0, 26
    v0_eom_cc3_31_trans_aiajcjcj = v0_eom_cc3_31_trans_aiajcjcj + term(s)
    end do

    end function v0_eom_cc3_31_trans_aiajcjcj
    function v0_eom_cc3_31_trans_aiajcjci(t2, nocc, nactive, a, i, j, c) 
    double precision :: v0_eom_cc3_31_trans_aiajcjci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,j) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,j,j) * read_ftvvvv(a, e, a, c)
term(2) = term(2) + t2(a,e,j,j) * read_ftvvvv(c, c, a, e)
term(3) = term(3) + t2(a,e,j,i) * tvoov(a, j, i, e)
term(4) = term(4) + t2(a,e,j,j) * tvoov(a, i, i, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(a, j, i, e)
term(6) = term(6) + t2(a,e,j,j) * tvvoo(a, e, i, i)
term(7) = term(7) + t2(a,e,i,j) * tvvoo(a, e, i, j)
term(8) = term(8) + t2(a,e,j,i) * tvvoo(a, e, i, j)
end do 

term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 
term(8) = -term(8) 

do m = 1, nocc 
term(9) = term(9) + t2(a,c,j,m) * tvoov(a, j, m, c)
term(10) = term(10) + t2(a,c,m,j) * tvoov(a, j, m, c)
term(11) = term(11) + t2(a,a,j,m) * tvoov(c, j, m, c)
term(12) = term(12) + t2(a,c,j,m) * tvvoo(a, c, m, j)
term(13) = term(13) + t2(a,c,m,j) * tvvoo(a, c, m, j)
term(14) = term(14) + t2(a,a,j,m) * tvvoo(c, c, m, j)
term(15) = term(15) + t2(a,a,j,m) * toooo(m, j, i, i)
term(16) = term(16) + t2(a,a,i,m) * toooo(m, j, i, j)
term(17) = term(17) + t2(a,a,j,m) * toooo(m, i, i, j)
end do 

term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 
term(11) = -term(11) 
term(12) = -term(12) 
term(13) = -term(13) 
term(14) = term(14) * 2.0d+0 
term(15) = term(15) * (-2.0d+0) 


    v0_eom_cc3_31_trans_aiajcjci = 0.d+0
    do s = 0, 17
    v0_eom_cc3_31_trans_aiajcjci = v0_eom_cc3_31_trans_aiajcjci + term(s)
    end do

    end function v0_eom_cc3_31_trans_aiajcjci
    function v0_eom_cc3_31_trans_aibibkbk(t2, nocc, nactive, a, i, b, k) 
    double precision :: v0_eom_cc3_31_trans_aibibkbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    double precision, dimension(0:26) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * read_ftvvvv(b, e, a, b)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(b, e, b, b)
term(2) = term(2) + t2(b,e,i,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(b,e,i,i) * tvoov(a, k, k, e)
term(4) = term(4) + t2(b,e,k,i) * tvoov(a, i, k, e)
term(5) = term(5) + t2(b,e,i,k) * tvoov(a, i, k, e)
term(6) = term(6) + t2(a,e,k,i) * tvoov(b, i, k, e)
term(7) = term(7) + t2(a,e,i,i) * tvoov(b, k, k, e)
term(8) = term(8) + t2(a,e,i,k) * tvoov(b, i, k, e)
term(9) = term(9) + t2(a,e,k,i) * tvvoo(b, e, k, i)
term(10) = term(10) + t2(b,e,i,k) * tvvoo(a, e, k, i)
term(11) = term(11) + t2(a,e,i,i) * tvvoo(b, e, k, k)
term(12) = term(12) + t2(a,e,i,k) * tvvoo(b, e, k, i)
term(13) = term(13) + t2(b,e,i,i) * tvvoo(a, e, k, k)
term(14) = term(14) + t2(b,e,k,i) * tvvoo(a, e, k, i)
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
term(21) = term(21) + t2(a,b,k,m) * toooo(m, i, k, i)
term(22) = term(22) + t2(a,b,m,i) * toooo(m, k, k, i)
term(23) = term(23) + t2(a,b,i,m) * toooo(m, i, k, k)
term(24) = term(24) + t2(a,b,i,m) * toooo(m, k, k, i)
term(25) = term(25) + t2(a,b,m,i) * toooo(m, i, k, k)
term(26) = term(26) + t2(a,b,m,k) * toooo(m, i, k, i)
end do 

term(15) = -term(15) 
term(16) = term(16) * 2.0d+0 
term(17) = -term(17) 
term(18) = term(18) * 2.0d+0 
term(19) = -term(19) 
term(20) = -term(20) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (-2.0d+0) 


    v0_eom_cc3_31_trans_aibibkbk = 0.d+0
    do s = 0, 26
    v0_eom_cc3_31_trans_aibibkbk = v0_eom_cc3_31_trans_aibibkbk + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibibkbk
    function v0_eom_cc3_31_trans_aibibkbi(t2, nocc, nactive, a, i, b, k) 
    double precision :: v0_eom_cc3_31_trans_aibibkbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tvoov(a, k, i, e)
term(1) = term(1) + t2(b,e,k,i) * tvoov(a, i, i, e)
term(2) = term(2) + t2(b,e,i,k) * tvoov(a, i, i, e)
term(3) = term(3) + t2(a,e,k,i) * tvoov(b, i, i, e)
term(4) = term(4) + t2(a,e,i,i) * tvoov(b, k, i, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(b, i, i, e)
term(6) = term(6) + t2(b,e,k,i) * read_ftvvvv(b, e, a, b)
term(7) = term(7) + t2(b,e,i,k) * read_ftvvvv(b, e, a, b)
term(8) = term(8) + t2(a,e,k,i) * read_ftvvvv(b, e, b, b)
term(9) = term(9) + t2(b,e,i,k) * read_ftvvvv(b, b, a, e)
term(10) = term(10) + t2(a,e,i,k) * read_ftvvvv(b, e, b, b)
term(11) = term(11) + t2(b,e,k,i) * read_ftvvvv(b, b, a, e)
term(12) = term(12) + t2(a,e,k,i) * tvvoo(b, e, i, i)
term(13) = term(13) + t2(b,e,i,k) * tvvoo(a, e, i, i)
term(14) = term(14) + t2(a,e,i,i) * tvvoo(b, e, i, k)
term(15) = term(15) + t2(a,e,i,k) * tvvoo(b, e, i, i)
term(16) = term(16) + t2(b,e,i,i) * tvvoo(a, e, i, k)
term(17) = term(17) + t2(b,e,k,i) * tvvoo(a, e, i, i)
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
term(18) = term(18) + t2(b,b,i,m) * tvoov(a, k, m, b)
term(19) = term(19) + t2(b,b,k,m) * tvoov(a, i, m, b)
term(20) = term(20) + t2(a,b,k,m) * tvoov(b, i, m, b)
term(21) = term(21) + t2(a,b,i,m) * tvoov(b, k, m, b)
term(22) = term(22) + t2(a,b,m,i) * tvoov(b, k, m, b)
term(23) = term(23) + t2(a,b,m,k) * tvoov(b, i, m, b)
term(24) = term(24) + t2(b,b,k,m) * tvvoo(a, b, m, i)
term(25) = term(25) + t2(b,b,i,m) * tvvoo(a, b, m, k)
term(26) = term(26) + t2(a,b,k,m) * tvvoo(b, b, m, i)
term(27) = term(27) + t2(a,b,m,i) * tvvoo(b, b, m, k)
term(28) = term(28) + t2(a,b,i,m) * tvvoo(b, b, m, k)
term(29) = term(29) + t2(a,b,m,k) * tvvoo(b, b, m, i)
term(30) = term(30) + t2(a,b,k,m) * toooo(m, i, i, i)
term(31) = term(31) + t2(a,b,m,i) * toooo(m, k, i, i)
term(32) = term(32) + t2(a,b,i,m) * toooo(m, i, i, k)
term(33) = term(33) + t2(a,b,i,m) * toooo(m, k, i, i)
term(34) = term(34) + t2(a,b,m,i) * toooo(m, i, i, k)
term(35) = term(35) + t2(a,b,m,k) * toooo(m, i, i, i)
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


    v0_eom_cc3_31_trans_aibibkbi = 0.d+0
    do s = 0, 35
    v0_eom_cc3_31_trans_aibibkbi = v0_eom_cc3_31_trans_aibibkbi + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibibkbi
    function v0_eom_cc3_31_trans_aibibkak(t2, nocc, nactive, a, i, b, k) 
    double precision :: v0_eom_cc3_31_trans_aibibkak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * read_ftvvvv(b, e, a, a)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(b, e, b, a)
term(2) = term(2) + t2(b,e,i,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(b,e,i,k) * tvoov(b, i, k, e)
term(4) = term(4) + t2(b,e,i,i) * tvoov(b, k, k, e)
term(5) = term(5) + t2(b,e,k,i) * tvoov(b, i, k, e)
term(6) = term(6) + t2(b,e,i,i) * tvvoo(b, e, k, k)
term(7) = term(7) + t2(b,e,k,i) * tvvoo(b, e, k, i)
term(8) = term(8) + t2(b,e,i,k) * tvvoo(b, e, k, i)
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
term(15) = term(15) + t2(b,b,i,m) * toooo(m, i, k, k)
term(16) = term(16) + t2(b,b,k,m) * toooo(m, i, k, i)
term(17) = term(17) + t2(b,b,i,m) * toooo(m, k, k, i)
end do 

term(9) = -term(9) 
term(10) = term(10) * 2.0d+0 
term(11) = -term(11) 
term(12) = term(12) * 2.0d+0 
term(13) = -term(13) 
term(14) = -term(14) 
term(15) = term(15) * (-2.0d+0) 


    v0_eom_cc3_31_trans_aibibkak = 0.d+0
    do s = 0, 17
    v0_eom_cc3_31_trans_aibibkak = v0_eom_cc3_31_trans_aibibkak + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibibkak
    function v0_eom_cc3_31_trans_aibibkai(t2, nocc, nactive, a, i, b, k) 
    double precision :: v0_eom_cc3_31_trans_aibibkai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    double precision, dimension(0:26) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * read_ftvvvv(b, e, a, a)
term(1) = term(1) + t2(b,e,i,k) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(b, e, b, a)
term(3) = term(3) + t2(b,e,i,k) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(a,e,i,k) * read_ftvvvv(b, e, b, a)
term(5) = term(5) + t2(b,e,k,i) * read_ftvvvv(b, a, a, e)
term(6) = term(6) + t2(b,e,i,k) * tvoov(b, i, i, e)
term(7) = term(7) + t2(b,e,i,i) * tvoov(b, k, i, e)
term(8) = term(8) + t2(b,e,k,i) * tvoov(b, i, i, e)
term(9) = term(9) + t2(b,e,i,i) * tvvoo(b, e, i, k)
term(10) = term(10) + t2(b,e,k,i) * tvvoo(b, e, i, i)
term(11) = term(11) + t2(b,e,i,k) * tvvoo(b, e, i, i)
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
term(12) = term(12) + t2(b,b,i,m) * tvoov(a, k, m, a)
term(13) = term(13) + t2(b,b,k,m) * tvoov(a, i, m, a)
term(14) = term(14) + t2(a,b,k,m) * tvoov(b, i, m, a)
term(15) = term(15) + t2(a,b,i,m) * tvoov(b, k, m, a)
term(16) = term(16) + t2(a,b,m,i) * tvoov(b, k, m, a)
term(17) = term(17) + t2(a,b,m,k) * tvoov(b, i, m, a)
term(18) = term(18) + t2(b,b,k,m) * tvvoo(a, a, m, i)
term(19) = term(19) + t2(b,b,i,m) * tvvoo(a, a, m, k)
term(20) = term(20) + t2(a,b,k,m) * tvvoo(b, a, m, i)
term(21) = term(21) + t2(a,b,m,i) * tvvoo(b, a, m, k)
term(22) = term(22) + t2(a,b,i,m) * tvvoo(b, a, m, k)
term(23) = term(23) + t2(a,b,m,k) * tvvoo(b, a, m, i)
term(24) = term(24) + t2(b,b,i,m) * toooo(m, i, i, k)
term(25) = term(25) + t2(b,b,k,m) * toooo(m, i, i, i)
term(26) = term(26) + t2(b,b,i,m) * toooo(m, k, i, i)
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


    v0_eom_cc3_31_trans_aibibkai = 0.d+0
    do s = 0, 26
    v0_eom_cc3_31_trans_aibibkai = v0_eom_cc3_31_trans_aibibkai + term(s)
    end do

    end function v0_eom_cc3_31_trans_aibibkai
    end module v0_eom_cc3_31_trans
    
