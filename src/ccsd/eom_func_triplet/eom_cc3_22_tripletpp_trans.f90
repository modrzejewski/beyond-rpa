module eom_cc3_22_tripletpp_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_cc3_22_tripletpp_trans_aibjckal(t2, nocc, nactive, i, b, j, c, k, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjckal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k, l 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, c, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, c, k, e)
term(2) = term(2) + t2(b,e,i,j) * tovov(l, e, k, c)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, k, c)
end do 

term(0) = -term(0) 
term(3) = -term(3) 


    eom_cc3_22_tripletpp_trans_aibjckal = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpp_trans_aibjckal = eom_cc3_22_tripletpp_trans_aibjckal + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjckal
    function eom_cc3_22_tripletpp_trans_aibjakdl(t2, nocc, nactive, i, b, j, k, d, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjakdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, d, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, d, k, e)
term(2) = term(2) + t2(b,e,i,j) * tovov(l, e, k, d)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, k, d)
end do 

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletpp_trans_aibjakdl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpp_trans_aibjakdl = eom_cc3_22_tripletpp_trans_aibjakdl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjakdl
    function eom_cc3_22_tripletpp_trans_aibjckbl(t2, nocc, nactive, a, i, j, c, k, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjckbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, c, k, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, k, c)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, c, k, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, k, c)
end do 

term(0) = -term(0) 
term(3) = -term(3) 


    eom_cc3_22_tripletpp_trans_aibjckbl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpp_trans_aibjckbl = eom_cc3_22_tripletpp_trans_aibjckbl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjckbl
    function eom_cc3_22_tripletpp_trans_aibjbkdl(t2, nocc, nactive, a, i, j, k, d, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbkdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, d, k, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, k, d)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, d, k, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, k, d)
end do 

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletpp_trans_aibjbkdl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpp_trans_aibjbkdl = eom_cc3_22_tripletpp_trans_aibjbkdl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbkdl
    function eom_cc3_22_tripletpp_trans_aibjcjdl(t2, nocc, nactive, a, i, b, c, d, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjcjdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d, l 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, l, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, l, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, l, c)
end do 

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletpp_trans_aibjcjdl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpp_trans_aibjcjdl = eom_cc3_22_tripletpp_trans_aibjcjdl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjcjdl
    function eom_cc3_22_tripletpp_trans_aibjckdj(t2, nocc, nactive, a, i, b, c, k, d) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjckdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, k, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, k, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, k, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, k, c)
end do 

term(0) = -term(0) 
term(3) = -term(3) 


    eom_cc3_22_tripletpp_trans_aibjckdj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpp_trans_aibjckdj = eom_cc3_22_tripletpp_trans_aibjckdj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjckdj
    function eom_cc3_22_tripletpp_trans_aibjcidl(t2, nocc, nactive, a, b, j, c, d, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjcidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d, l 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, l, d)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, d, l, c)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, d, l, c)
end do 

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletpp_trans_aibjcidl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpp_trans_aibjcidl = eom_cc3_22_tripletpp_trans_aibjcidl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjcidl
    function eom_cc3_22_tripletpp_trans_aibjckdi(t2, nocc, nactive, a, b, j, c, k, d) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjckdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k, d 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, k, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, k, d)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, d, k, c)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, d, k, c)
end do 

term(0) = -term(0) 
term(3) = -term(3) 


    eom_cc3_22_tripletpp_trans_aibjckdi = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpp_trans_aibjckdi = eom_cc3_22_tripletpp_trans_aibjckdi + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjckdi
    function eom_cc3_22_tripletpp_trans_aibjbkal(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbkal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e,f 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, j, k, i)
term(1) = term(1) + toooo(l, i, k, j)

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(l, a, k, e)
term(3) = term(3) + t2(b,e,i,j) * tovov(l, b, k, e)
term(4) = term(4) + t2(b,e,j,i) * tovov(l, b, k, e)
term(5) = term(5) + t2(a,e,j,i) * tovov(l, e, k, a)
term(6) = term(6) + t2(b,e,i,j) * tovov(l, e, k, b)
term(7) = term(7) + t2(b,e,j,i) * tovov(l, e, k, b)
term(8) = term(8) + t2(a,e,i,j) * tovov(l, a, k, e)
term(9) = term(9) + t2(a,e,i,j) * tovov(l, e, k, a)
end do 

term(3) = -term(3) 
term(5) = -term(5) 
term(7) = -term(7) 
term(8) = -term(8) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(e,f,i,j) * tovov(l, e, k, f)
end do 
end do 


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(11) = term(11) + t2(e,f,i,j) * tovov(l, f, k, e)
end do 
end do 

term(11) = -term(11) 


    eom_cc3_22_tripletpp_trans_aibjbkal = 0.d+0
    do s = 0, 11
    eom_cc3_22_tripletpp_trans_aibjbkal = eom_cc3_22_tripletpp_trans_aibjbkal + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbkal
    function eom_cc3_22_tripletpp_trans_aibjcjal(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjcjal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, c)
term(1) = term(1) + tvvoo(b, c, l, i)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, c, l, a)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, a, l, c)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, c, l, a)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, a, l, c)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(6) = term(6) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, c, l, e)
term(8) = term(8) + t2(b,e,m,i) * tovov(m, e, l, c)
term(9) = term(9) + t2(b,e,m,i) * tovov(m, c, l, e)
end do 
end do 

term(7) = -term(7) 
term(8) = -term(8) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,j) * tovov(l, c, j, e)
term(11) = term(11) + t2(b,e,j,i) * tovov(l, c, j, e)
term(12) = term(12) + t2(b,e,i,j) * tovov(l, e, j, c)
term(13) = term(13) + t2(b,e,j,i) * tovov(l, e, j, c)
end do 

term(10) = -term(10) 
term(13) = -term(13) 


    eom_cc3_22_tripletpp_trans_aibjcjal = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjcjal = eom_cc3_22_tripletpp_trans_aibjcjal + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjcjal
    function eom_cc3_22_tripletpp_trans_aibjckaj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjckaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, k, i)
term(1) = term(1) + tvoov(b, i, k, c)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, c, k, a)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, a, k, c)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, c, k, a)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, a, k, c)
end do 

term(2) = -term(2) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(6) = term(6) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,m,i) * tovov(m, c, k, e)
term(8) = term(8) + t2(b,e,i,m) * tovov(m, c, k, e)
term(9) = term(9) + t2(b,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(7) = -term(7) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,j) * tovov(k, e, j, c)
term(11) = term(11) + t2(b,e,j,i) * tovov(k, e, j, c)
term(12) = term(12) + t2(b,e,i,j) * tovov(k, c, j, e)
term(13) = term(13) + t2(b,e,j,i) * tovov(k, c, j, e)
end do 

term(10) = -term(10) 
term(13) = -term(13) 


    eom_cc3_22_tripletpp_trans_aibjckaj = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjckaj = eom_cc3_22_tripletpp_trans_aibjckaj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjckaj
    function eom_cc3_22_tripletpp_trans_aibjcial(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjcial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, l, j)
term(1) = term(1) + tvoov(b, j, l, c)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,j,m) * tovov(m, c, l, a)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, c, l, a)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, a, l, c)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, a, l, c)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,m) * tovov(m, e, l, c)
end do 
end do 

term(6) = term(6) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,m,j) * tovov(m, c, l, e)
term(8) = term(8) + t2(b,e,j,m) * tovov(m, c, l, e)
term(9) = term(9) + t2(b,e,m,j) * tovov(m, e, l, c)
end do 
end do 

term(7) = -term(7) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,j) * tovov(l, c, i, e)
term(11) = term(11) + t2(b,e,j,i) * tovov(l, c, i, e)
term(12) = term(12) + t2(b,e,i,j) * tovov(l, e, i, c)
term(13) = term(13) + t2(b,e,j,i) * tovov(l, e, i, c)
end do 

term(10) = -term(10) 
term(13) = -term(13) 


    eom_cc3_22_tripletpp_trans_aibjcial = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjcial = eom_cc3_22_tripletpp_trans_aibjcial + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjcial
    function eom_cc3_22_tripletpp_trans_aibjckai(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjckai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, k, j)
term(1) = term(1) + tvoov(b, j, k, c)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,j,m) * tovov(m, c, k, a)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, c, k, a)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, a, k, c)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, a, k, c)
end do 

term(2) = -term(2) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(6) = term(6) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,m,j) * tovov(m, c, k, e)
term(8) = term(8) + t2(b,e,j,m) * tovov(m, c, k, e)
term(9) = term(9) + t2(b,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,j) * tovov(k, e, i, c)
term(11) = term(11) + t2(b,e,j,i) * tovov(k, e, i, c)
term(12) = term(12) + t2(b,e,i,j) * tovov(k, c, i, e)
term(13) = term(13) + t2(b,e,j,i) * tovov(k, c, i, e)
end do 

term(10) = -term(10) 
term(13) = -term(13) 


    eom_cc3_22_tripletpp_trans_aibjckai = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjckai = eom_cc3_22_tripletpp_trans_aibjckai + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjckai
    function eom_cc3_22_tripletpp_trans_aibjakbl(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjakbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e,f 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, i, k, j)
term(1) = term(1) + toooo(l, j, k, i)

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,j) * tovov(l, b, k, e)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, b, k, e)
term(4) = term(4) + t2(a,e,j,i) * tovov(l, a, k, e)
term(5) = term(5) + t2(b,e,i,j) * tovov(l, e, k, b)
term(6) = term(6) + t2(b,e,j,i) * tovov(l, e, k, b)
term(7) = term(7) + t2(a,e,j,i) * tovov(l, e, k, a)
term(8) = term(8) + t2(a,e,i,j) * tovov(l, a, k, e)
term(9) = term(9) + t2(a,e,i,j) * tovov(l, e, k, a)
end do 

term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 
term(9) = -term(9) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(10) = term(10) + t2(e,f,i,j) * tovov(l, f, k, e)
end do 
end do 


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(11) = term(11) + t2(e,f,i,j) * tovov(l, e, k, f)
end do 
end do 

term(11) = -term(11) 


    eom_cc3_22_tripletpp_trans_aibjakbl = 0.d+0
    do s = 0, 11
    eom_cc3_22_tripletpp_trans_aibjakbl = eom_cc3_22_tripletpp_trans_aibjakbl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjakbl
    function eom_cc3_22_tripletpp_trans_aibjajdl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjajdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, l, i)
term(1) = term(1) + tvoov(b, i, l, d)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, l, d)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, d, l, a)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, l, d)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, d, l, a)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(6) = term(6) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,m,i) * tovov(m, d, l, e)
term(8) = term(8) + t2(b,e,i,m) * tovov(m, d, l, e)
term(9) = term(9) + t2(b,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(7) = -term(7) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,j) * tovov(l, d, j, e)
term(11) = term(11) + t2(b,e,j,i) * tovov(l, d, j, e)
term(12) = term(12) + t2(b,e,i,j) * tovov(l, e, j, d)
term(13) = term(13) + t2(b,e,j,i) * tovov(l, e, j, d)
end do 

term(11) = -term(11) 
term(12) = -term(12) 


    eom_cc3_22_tripletpp_trans_aibjajdl = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjajdl = eom_cc3_22_tripletpp_trans_aibjajdl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjajdl
    function eom_cc3_22_tripletpp_trans_aibjakdj(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjakdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, k, i)
term(1) = term(1) + tvoov(b, i, k, d)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, k, d)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, d, k, a)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, k, d)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, d, k, a)
end do 

term(2) = -term(2) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(6) = term(6) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,m,i) * tovov(m, d, k, e)
term(8) = term(8) + t2(b,e,i,m) * tovov(m, d, k, e)
term(9) = term(9) + t2(b,e,m,i) * tovov(m, e, k, d)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,j) * tovov(k, e, j, d)
term(11) = term(11) + t2(b,e,j,i) * tovov(k, e, j, d)
term(12) = term(12) + t2(b,e,i,j) * tovov(k, d, j, e)
term(13) = term(13) + t2(b,e,j,i) * tovov(k, d, j, e)
end do 

term(11) = -term(11) 
term(12) = -term(12) 


    eom_cc3_22_tripletpp_trans_aibjakdj = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjakdj = eom_cc3_22_tripletpp_trans_aibjakdj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjakdj
    function eom_cc3_22_tripletpp_trans_aibjaidl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjaidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, l, j)
term(1) = term(1) + tvoov(b, j, l, d)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,j,m) * tovov(m, a, l, d)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, a, l, d)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, d, l, a)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, d, l, a)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(6) = term(6) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,m,j) * tovov(m, d, l, e)
term(8) = term(8) + t2(b,e,j,m) * tovov(m, d, l, e)
term(9) = term(9) + t2(b,e,m,j) * tovov(m, e, l, d)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,j) * tovov(l, d, i, e)
term(11) = term(11) + t2(b,e,j,i) * tovov(l, d, i, e)
term(12) = term(12) + t2(b,e,i,j) * tovov(l, e, i, d)
term(13) = term(13) + t2(b,e,j,i) * tovov(l, e, i, d)
end do 

term(11) = -term(11) 
term(12) = -term(12) 


    eom_cc3_22_tripletpp_trans_aibjaidl = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjaidl = eom_cc3_22_tripletpp_trans_aibjaidl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjaidl
    function eom_cc3_22_tripletpp_trans_aibjakdi(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjakdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, k, j)
term(1) = term(1) + tvoov(b, j, k, d)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,j,m) * tovov(m, a, k, d)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, a, k, d)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, d, k, a)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, d, k, a)
end do 

term(2) = -term(2) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,m) * tovov(m, e, k, d)
end do 
end do 

term(6) = term(6) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,m,j) * tovov(m, d, k, e)
term(8) = term(8) + t2(b,e,j,m) * tovov(m, d, k, e)
term(9) = term(9) + t2(b,e,m,j) * tovov(m, e, k, d)
end do 
end do 

term(7) = -term(7) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,j) * tovov(k, e, i, d)
term(11) = term(11) + t2(b,e,j,i) * tovov(k, e, i, d)
term(12) = term(12) + t2(b,e,i,j) * tovov(k, d, i, e)
term(13) = term(13) + t2(b,e,j,i) * tovov(k, d, i, e)
end do 

term(11) = -term(11) 
term(12) = -term(12) 


    eom_cc3_22_tripletpp_trans_aibjakdi = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjakdi = eom_cc3_22_tripletpp_trans_aibjakdi + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjakdi
    function eom_cc3_22_tripletpp_trans_aibjcjbl(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjcjbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, l, i)
term(1) = term(1) + tvoov(a, i, l, c)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, c, l, b)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, b, l, c)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, c, l, b)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, b, l, c)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(6) = term(6) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,i) * tovov(m, c, l, e)
term(8) = term(8) + t2(a,e,i,m) * tovov(m, c, l, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, l, c)
end do 
end do 

term(7) = -term(7) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(a,e,j,i) * tovov(l, c, j, e)
term(11) = term(11) + t2(a,e,j,i) * tovov(l, e, j, c)
term(12) = term(12) + t2(a,e,i,j) * tovov(l, c, j, e)
term(13) = term(13) + t2(a,e,i,j) * tovov(l, e, j, c)
end do 

term(10) = -term(10) 
term(13) = -term(13) 


    eom_cc3_22_tripletpp_trans_aibjcjbl = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjcjbl = eom_cc3_22_tripletpp_trans_aibjcjbl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjcjbl
    function eom_cc3_22_tripletpp_trans_aibjckbj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjckbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, k, i)
term(1) = term(1) + tvoov(a, i, k, c)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, c, k, b)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, b, k, c)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, c, k, b)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, b, k, c)
end do 

term(2) = -term(2) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(6) = term(6) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,i) * tovov(m, c, k, e)
term(8) = term(8) + t2(a,e,i,m) * tovov(m, c, k, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(a,e,j,i) * tovov(k, e, j, c)
term(11) = term(11) + t2(a,e,j,i) * tovov(k, c, j, e)
term(12) = term(12) + t2(a,e,i,j) * tovov(k, e, j, c)
term(13) = term(13) + t2(a,e,i,j) * tovov(k, c, j, e)
end do 

term(10) = -term(10) 
term(13) = -term(13) 


    eom_cc3_22_tripletpp_trans_aibjckbj = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjckbj = eom_cc3_22_tripletpp_trans_aibjckbj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjckbj
    function eom_cc3_22_tripletpp_trans_aibjcibl(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjcibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, l, j)
term(1) = term(1) + tvoov(a, j, l, c)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,j,m) * tovov(m, c, l, b)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, c, l, b)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, b, l, c)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, b, l, c)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,j,m) * tovov(m, e, l, c)
end do 
end do 

term(6) = term(6) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,j) * tovov(m, c, l, e)
term(8) = term(8) + t2(a,e,j,m) * tovov(m, c, l, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, e, l, c)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(a,e,j,i) * tovov(l, c, i, e)
term(11) = term(11) + t2(a,e,j,i) * tovov(l, e, i, c)
term(12) = term(12) + t2(a,e,i,j) * tovov(l, c, i, e)
term(13) = term(13) + t2(a,e,i,j) * tovov(l, e, i, c)
end do 

term(10) = -term(10) 
term(13) = -term(13) 


    eom_cc3_22_tripletpp_trans_aibjcibl = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjcibl = eom_cc3_22_tripletpp_trans_aibjcibl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjcibl
    function eom_cc3_22_tripletpp_trans_aibjckbi(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjckbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, k, j)
term(1) = term(1) + tvoov(a, j, k, c)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,j,m) * tovov(m, c, k, b)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, c, k, b)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, b, k, c)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, b, k, c)
end do 

term(2) = -term(2) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(6) = term(6) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,j) * tovov(m, c, k, e)
term(8) = term(8) + t2(a,e,j,m) * tovov(m, c, k, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(7) = -term(7) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(a,e,j,i) * tovov(k, e, i, c)
term(11) = term(11) + t2(a,e,j,i) * tovov(k, c, i, e)
term(12) = term(12) + t2(a,e,i,j) * tovov(k, e, i, c)
term(13) = term(13) + t2(a,e,i,j) * tovov(k, c, i, e)
end do 

term(10) = -term(10) 
term(13) = -term(13) 


    eom_cc3_22_tripletpp_trans_aibjckbi = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjckbi = eom_cc3_22_tripletpp_trans_aibjckbi + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjckbi
    function eom_cc3_22_tripletpp_trans_aibjbjdl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbjdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, l, i)
term(1) = term(1) + tvoov(a, i, l, d)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, l, d)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, d, l, b)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, b, l, d)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, d, l, b)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(6) = term(6) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,i) * tovov(m, d, l, e)
term(8) = term(8) + t2(a,e,i,m) * tovov(m, d, l, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(a,e,j,i) * tovov(l, d, j, e)
term(11) = term(11) + t2(a,e,j,i) * tovov(l, e, j, d)
term(12) = term(12) + t2(a,e,i,j) * tovov(l, d, j, e)
term(13) = term(13) + t2(a,e,i,j) * tovov(l, e, j, d)
end do 

term(11) = -term(11) 
term(12) = -term(12) 


    eom_cc3_22_tripletpp_trans_aibjbjdl = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjbjdl = eom_cc3_22_tripletpp_trans_aibjbjdl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbjdl
    function eom_cc3_22_tripletpp_trans_aibjbkdj(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbkdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, k, i)
term(1) = term(1) + tvoov(a, i, k, d)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, k, d)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, d, k, b)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, b, k, d)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, d, k, b)
end do 

term(2) = -term(2) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(6) = term(6) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,i) * tovov(m, d, k, e)
term(8) = term(8) + t2(a,e,i,m) * tovov(m, d, k, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, k, d)
end do 
end do 

term(7) = -term(7) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(a,e,j,i) * tovov(k, e, j, d)
term(11) = term(11) + t2(a,e,j,i) * tovov(k, d, j, e)
term(12) = term(12) + t2(a,e,i,j) * tovov(k, e, j, d)
term(13) = term(13) + t2(a,e,i,j) * tovov(k, d, j, e)
end do 

term(11) = -term(11) 
term(12) = -term(12) 


    eom_cc3_22_tripletpp_trans_aibjbkdj = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjbkdj = eom_cc3_22_tripletpp_trans_aibjbkdj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbkdj
    function eom_cc3_22_tripletpp_trans_aibjbidl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, l, j)
term(1) = term(1) + tvoov(a, j, l, d)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,j,m) * tovov(m, b, l, d)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, b, l, d)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, d, l, b)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, d, l, b)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(6) = term(6) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,j) * tovov(m, d, l, e)
term(8) = term(8) + t2(a,e,j,m) * tovov(m, d, l, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, e, l, d)
end do 
end do 

term(7) = -term(7) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(a,e,j,i) * tovov(l, d, i, e)
term(11) = term(11) + t2(a,e,j,i) * tovov(l, e, i, d)
term(12) = term(12) + t2(a,e,i,j) * tovov(l, d, i, e)
term(13) = term(13) + t2(a,e,i,j) * tovov(l, e, i, d)
end do 

term(11) = -term(11) 
term(12) = -term(12) 


    eom_cc3_22_tripletpp_trans_aibjbidl = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjbidl = eom_cc3_22_tripletpp_trans_aibjbidl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbidl
    function eom_cc3_22_tripletpp_trans_aibjbkdi(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbkdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, k, d)
term(1) = term(1) + tvvoo(a, d, k, j)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,j,m) * tovov(m, b, k, d)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, b, k, d)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, d, k, b)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, d, k, b)
end do 

term(2) = -term(2) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,j,m) * tovov(m, e, k, d)
end do 
end do 

term(6) = term(6) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, d, k, e)
term(8) = term(8) + t2(a,e,m,j) * tovov(m, e, k, d)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, d, k, e)
end do 
end do 

term(7) = -term(7) 
term(8) = -term(8) 

do e = nocc + 1, nactive 
term(10) = term(10) + t2(a,e,j,i) * tovov(k, e, i, d)
term(11) = term(11) + t2(a,e,j,i) * tovov(k, d, i, e)
term(12) = term(12) + t2(a,e,i,j) * tovov(k, e, i, d)
term(13) = term(13) + t2(a,e,i,j) * tovov(k, d, i, e)
end do 

term(11) = -term(11) 
term(12) = -term(12) 


    eom_cc3_22_tripletpp_trans_aibjbkdi = 0.d+0
    do s = 0, 13
    eom_cc3_22_tripletpp_trans_aibjbkdi = eom_cc3_22_tripletpp_trans_aibjbkdi + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbkdi
    function eom_cc3_22_tripletpp_trans_aibjcjdi(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjcjdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m,n 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, d, a, c)
term(1) = term(1) + read_ftvvvv(b, c, a, d)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,j,m) * tovov(m, c, j, d)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, c, i, d)
term(4) = term(4) + t2(a,b,m,j) * tovov(m, c, j, d)
term(5) = term(5) + t2(a,b,m,i) * tovov(m, d, i, c)
term(6) = term(6) + t2(a,b,j,m) * tovov(m, d, j, c)
term(7) = term(7) + t2(a,b,m,j) * tovov(m, d, j, c)
term(8) = term(8) + t2(a,b,i,m) * tovov(m, c, i, d)
term(9) = term(9) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(2) = -term(2) 
term(5) = -term(5) 
term(7) = -term(7) 
term(8) = -term(8) 

do n = 1, nocc 
do m = 1, nocc 
term(10) = term(10) + t2(a,b,m,n) * tovov(n, d, m, c)
term(11) = term(11) + t2(a,b,m,n) * tovov(n, c, m, d)
end do 
end do 

term(10) = -term(10) 


    eom_cc3_22_tripletpp_trans_aibjcjdi = 0.d+0
    do s = 0, 11
    eom_cc3_22_tripletpp_trans_aibjcjdi = eom_cc3_22_tripletpp_trans_aibjcjdi + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjcjdi
    function eom_cc3_22_tripletpp_trans_aibjcidj(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjcidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m,n 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, c, a, d)
term(1) = term(1) + read_ftvvvv(b, d, a, c)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,j,m) * tovov(m, c, j, d)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, c, i, d)
term(4) = term(4) + t2(a,b,m,j) * tovov(m, c, j, d)
term(5) = term(5) + t2(a,b,j,m) * tovov(m, d, j, c)
term(6) = term(6) + t2(a,b,m,j) * tovov(m, d, j, c)
term(7) = term(7) + t2(a,b,m,i) * tovov(m, d, i, c)
term(8) = term(8) + t2(a,b,i,m) * tovov(m, c, i, d)
term(9) = term(9) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 
term(9) = -term(9) 

do n = 1, nocc 
do m = 1, nocc 
term(10) = term(10) + t2(a,b,m,n) * tovov(n, c, m, d)
term(11) = term(11) + t2(a,b,m,n) * tovov(n, d, m, c)
end do 
end do 

term(10) = -term(10) 


    eom_cc3_22_tripletpp_trans_aibjcidj = 0.d+0
    do s = 0, 11
    eom_cc3_22_tripletpp_trans_aibjcidj = eom_cc3_22_tripletpp_trans_aibjcidj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjcidj
    function eom_cc3_22_tripletpp_trans_aibjbjal(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbjal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,f,m,e 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,i,m) * tovov(m, e, l, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,m) * tovov(m, f, l, e)
end do 
end do 
end do 

term(1) = term(1) * (-2.0d+0) 

term(2) = term(2) + tvoov(b, i, l, b)
term(3) = term(3) + tvvoo(a, a, l, i)
term(4) = term(4) + tvvoo(b, b, l, i)
term(5) = term(5) + tvoov(a, i, l, a)
term(6) = term(6) + toooo(l, j, j, i)
term(7) = term(7) + toooo(l, i, j, j)
term(8) = term(8) + too(l, i)

term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 
term(8) = -term(8) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(9) = term(9) + t2(b,e,i,m) * tovov(m, b, l, e)
term(10) = term(10) + t2(b,e,m,i) * tovov(m, e, l, b)
term(11) = term(11) + t2(b,e,m,i) * tovov(m, b, l, e)
term(12) = term(12) + t2(a,e,m,i) * tovov(m, a, l, e)
term(13) = term(13) + t2(a,e,i,m) * tovov(m, a, l, e)
term(14) = term(14) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 

term(9) = -term(9) 
term(10) = -term(10) 
term(13) = -term(13) 
term(14) = -term(14) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(15) = term(15) + t2(b,e,i,m) * tovov(m, e, l, b)
term(16) = term(16) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(15) = term(15) * 2.0d+0 
term(16) = term(16) * 2.0d+0 

do m = 1, nocc 
term(17) = term(17) + toooo(m, i, l, m)
term(18) = term(18) + toooo(m, m, l, i)
term(19) = term(19) + t2(a,b,m,i) * tovov(m, b, l, a)
term(20) = term(20) + t2(a,b,m,i) * tovov(m, a, l, b)
term(21) = term(21) + t2(a,b,i,m) * tovov(m, b, l, a)
term(22) = term(22) + t2(a,b,i,m) * tovov(m, a, l, b)
end do 

term(18) = term(18) * (-2.0d+0) 
term(20) = -term(20) 
term(21) = -term(21) 

do e = nocc + 1, nactive 
term(23) = term(23) + t2(a,e,j,i) * tovov(l, a, j, e)
term(24) = term(24) + t2(b,e,i,j) * tovov(l, b, j, e)
term(25) = term(25) + t2(b,e,j,i) * tovov(l, b, j, e)
term(26) = term(26) + t2(a,e,j,i) * tovov(l, e, j, a)
term(27) = term(27) + t2(b,e,i,j) * tovov(l, e, j, b)
term(28) = term(28) + t2(b,e,j,i) * tovov(l, e, j, b)
term(29) = term(29) + t2(a,e,i,j) * tovov(l, a, j, e)
term(30) = term(30) + t2(a,e,i,j) * tovov(l, e, j, a)
end do 

term(24) = -term(24) 
term(26) = -term(26) 
term(28) = -term(28) 
term(29) = -term(29) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(31) = term(31) + t2(e,f,i,j) * tovov(l, f, j, e)
end do 
end do 

term(31) = -term(31) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(32) = term(32) + t2(e,f,i,j) * tovov(l, e, j, f)
end do 
end do 



    eom_cc3_22_tripletpp_trans_aibjbjal = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjbjal = eom_cc3_22_tripletpp_trans_aibjbjal + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbjal
    function eom_cc3_22_tripletpp_trans_aibjbkaj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbkaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,f,m,e 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,i,m) * tovov(m, e, k, f)
end do 
end do 
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,m) * tovov(m, f, k, e)
end do 
end do 
end do 

term(1) = term(1) * 2.0d+0 

term(2) = term(2) + tvvoo(b, b, k, i)
term(3) = term(3) + tvvoo(a, a, k, i)
term(4) = term(4) + tvoov(b, i, k, b)
term(5) = term(5) + tvoov(a, i, k, a)
term(6) = term(6) + toooo(k, i, j, j)
term(7) = term(7) + toooo(k, j, j, i)
term(8) = term(8) + too(k, i)

term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(9) = term(9) + t2(a,e,m,i) * tovov(m, a, k, e)
term(10) = term(10) + t2(b,e,m,i) * tovov(m, b, k, e)
term(11) = term(11) + t2(b,e,i,m) * tovov(m, b, k, e)
term(12) = term(12) + t2(b,e,m,i) * tovov(m, e, k, b)
term(13) = term(13) + t2(a,e,i,m) * tovov(m, a, k, e)
term(14) = term(14) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(9) = -term(9) 
term(10) = -term(10) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(15) = term(15) + t2(b,e,i,m) * tovov(m, e, k, b)
term(16) = term(16) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(15) = term(15) * (-2.0d+0) 
term(16) = term(16) * (-2.0d+0) 

do m = 1, nocc 
term(17) = term(17) + toooo(m, i, k, m)
term(18) = term(18) + toooo(m, m, k, i)
term(19) = term(19) + t2(a,b,m,i) * tovov(m, b, k, a)
term(20) = term(20) + t2(a,b,m,i) * tovov(m, a, k, b)
term(21) = term(21) + t2(a,b,i,m) * tovov(m, b, k, a)
term(22) = term(22) + t2(a,b,i,m) * tovov(m, a, k, b)
end do 

term(17) = -term(17) 
term(18) = term(18) * 2.0d+0 
term(19) = -term(19) 
term(22) = -term(22) 

do e = nocc + 1, nactive 
term(23) = term(23) + t2(a,e,j,i) * tovov(k, e, j, a)
term(24) = term(24) + t2(b,e,i,j) * tovov(k, e, j, b)
term(25) = term(25) + t2(b,e,j,i) * tovov(k, e, j, b)
term(26) = term(26) + t2(a,e,j,i) * tovov(k, a, j, e)
term(27) = term(27) + t2(b,e,i,j) * tovov(k, b, j, e)
term(28) = term(28) + t2(b,e,j,i) * tovov(k, b, j, e)
term(29) = term(29) + t2(a,e,i,j) * tovov(k, e, j, a)
term(30) = term(30) + t2(a,e,i,j) * tovov(k, a, j, e)
end do 

term(24) = -term(24) 
term(26) = -term(26) 
term(28) = -term(28) 
term(29) = -term(29) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(31) = term(31) + t2(e,f,i,j) * tovov(k, e, j, f)
end do 
end do 

term(31) = -term(31) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(32) = term(32) + t2(e,f,i,j) * tovov(k, f, j, e)
end do 
end do 



    eom_cc3_22_tripletpp_trans_aibjbkaj = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjbkaj = eom_cc3_22_tripletpp_trans_aibjbkaj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbkaj
    function eom_cc3_22_tripletpp_trans_aibjbial(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,f,m,e 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,j,m) * tovov(m, e, l, f)
end do 
end do 
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,j,m) * tovov(m, f, l, e)
end do 
end do 
end do 

term(1) = term(1) * 2.0d+0 

term(2) = term(2) + tvvoo(b, b, l, j)
term(3) = term(3) + tvvoo(a, a, l, j)
term(4) = term(4) + tvoov(a, j, l, a)
term(5) = term(5) + tvoov(b, j, l, b)
term(6) = term(6) + toooo(l, j, i, i)
term(7) = term(7) + toooo(l, i, i, j)
term(8) = term(8) + too(l, j)

term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(9) = term(9) + t2(a,e,m,j) * tovov(m, a, l, e)
term(10) = term(10) + t2(b,e,m,j) * tovov(m, b, l, e)
term(11) = term(11) + t2(a,e,j,m) * tovov(m, a, l, e)
term(12) = term(12) + t2(b,e,j,m) * tovov(m, b, l, e)
term(13) = term(13) + t2(a,e,m,j) * tovov(m, e, l, a)
term(14) = term(14) + t2(b,e,m,j) * tovov(m, e, l, b)
end do 
end do 

term(9) = -term(9) 
term(10) = -term(10) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(15) = term(15) + t2(a,e,j,m) * tovov(m, e, l, a)
term(16) = term(16) + t2(b,e,j,m) * tovov(m, e, l, b)
end do 
end do 

term(15) = term(15) * (-2.0d+0) 
term(16) = term(16) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(17) = term(17) + t2(a,e,j,i) * tovov(l, a, i, e)
term(18) = term(18) + t2(b,e,i,j) * tovov(l, b, i, e)
term(19) = term(19) + t2(b,e,j,i) * tovov(l, b, i, e)
term(20) = term(20) + t2(a,e,j,i) * tovov(l, e, i, a)
term(21) = term(21) + t2(b,e,i,j) * tovov(l, e, i, b)
term(22) = term(22) + t2(b,e,j,i) * tovov(l, e, i, b)
term(23) = term(23) + t2(a,e,i,j) * tovov(l, a, i, e)
term(24) = term(24) + t2(a,e,i,j) * tovov(l, e, i, a)
end do 

term(18) = -term(18) 
term(20) = -term(20) 
term(22) = -term(22) 
term(23) = -term(23) 

do m = 1, nocc 
term(25) = term(25) + toooo(m, j, l, m)
term(26) = term(26) + toooo(m, m, l, j)
term(27) = term(27) + t2(a,b,j,m) * tovov(m, b, l, a)
term(28) = term(28) + t2(a,b,m,j) * tovov(m, b, l, a)
term(29) = term(29) + t2(a,b,j,m) * tovov(m, a, l, b)
term(30) = term(30) + t2(a,b,m,j) * tovov(m, a, l, b)
end do 

term(25) = -term(25) 
term(26) = term(26) * 2.0d+0 
term(28) = -term(28) 
term(29) = -term(29) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(31) = term(31) + t2(e,f,i,j) * tovov(l, f, i, e)
end do 
end do 

term(31) = -term(31) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(32) = term(32) + t2(e,f,i,j) * tovov(l, e, i, f)
end do 
end do 



    eom_cc3_22_tripletpp_trans_aibjbial = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjbial = eom_cc3_22_tripletpp_trans_aibjbial + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbial
    function eom_cc3_22_tripletpp_trans_aibjbkai(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbkai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,f,m,e 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,j,m) * tovov(m, e, k, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,j,m) * tovov(m, f, k, e)
end do 
end do 
end do 

term(1) = term(1) * (-2.0d+0) 

term(2) = term(2) + tvoov(a, j, k, a)
term(3) = term(3) + tvvoo(a, a, k, j)
term(4) = term(4) + tvvoo(b, b, k, j)
term(5) = term(5) + tvoov(b, j, k, b)
term(6) = term(6) + toooo(k, i, i, j)
term(7) = term(7) + toooo(k, j, i, i)
term(8) = term(8) + too(k, j)

term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 
term(8) = -term(8) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(9) = term(9) + t2(a,e,j,m) * tovov(m, a, k, e)
term(10) = term(10) + t2(a,e,m,j) * tovov(m, e, k, a)
term(11) = term(11) + t2(b,e,m,j) * tovov(m, b, k, e)
term(12) = term(12) + t2(a,e,m,j) * tovov(m, a, k, e)
term(13) = term(13) + t2(b,e,j,m) * tovov(m, b, k, e)
term(14) = term(14) + t2(b,e,m,j) * tovov(m, e, k, b)
end do 
end do 

term(9) = -term(9) 
term(10) = -term(10) 
term(13) = -term(13) 
term(14) = -term(14) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(15) = term(15) + t2(a,e,j,m) * tovov(m, e, k, a)
term(16) = term(16) + t2(b,e,j,m) * tovov(m, e, k, b)
end do 
end do 

term(15) = term(15) * 2.0d+0 
term(16) = term(16) * 2.0d+0 

do m = 1, nocc 
term(17) = term(17) + toooo(m, j, k, m)
term(18) = term(18) + toooo(m, m, k, j)
term(19) = term(19) + t2(a,b,j,m) * tovov(m, b, k, a)
term(20) = term(20) + t2(a,b,m,j) * tovov(m, b, k, a)
term(21) = term(21) + t2(a,b,j,m) * tovov(m, a, k, b)
term(22) = term(22) + t2(a,b,m,j) * tovov(m, a, k, b)
end do 

term(18) = term(18) * (-2.0d+0) 
term(19) = -term(19) 
term(22) = -term(22) 

do e = nocc + 1, nactive 
term(23) = term(23) + t2(a,e,j,i) * tovov(k, e, i, a)
term(24) = term(24) + t2(b,e,i,j) * tovov(k, e, i, b)
term(25) = term(25) + t2(b,e,j,i) * tovov(k, e, i, b)
term(26) = term(26) + t2(a,e,j,i) * tovov(k, a, i, e)
term(27) = term(27) + t2(b,e,i,j) * tovov(k, b, i, e)
term(28) = term(28) + t2(b,e,j,i) * tovov(k, b, i, e)
term(29) = term(29) + t2(a,e,i,j) * tovov(k, e, i, a)
term(30) = term(30) + t2(a,e,i,j) * tovov(k, a, i, e)
end do 

term(24) = -term(24) 
term(26) = -term(26) 
term(28) = -term(28) 
term(29) = -term(29) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(31) = term(31) + t2(e,f,i,j) * tovov(k, e, i, f)
end do 
end do 

term(31) = -term(31) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(32) = term(32) + t2(e,f,i,j) * tovov(k, f, i, e)
end do 
end do 



    eom_cc3_22_tripletpp_trans_aibjbkai = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjbkai = eom_cc3_22_tripletpp_trans_aibjbkai + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbkai
    function eom_cc3_22_tripletpp_trans_aibjcjai(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjcjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e,n 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, i, a)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, c, j, a)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, a, i, c)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, a, j, c)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, a, j, c)
term(6) = term(6) + t2(a,b,i,m) * tovov(m, c, i, a)
term(7) = term(7) + t2(a,b,i,m) * tovov(m, a, i, c)
term(8) = term(8) + tvvoo(b, c, m, m)
term(9) = term(9) + tvoov(b, m, m, c)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 
term(8) = term(8) * 2.0d+0 
term(9) = -term(9) 

term(10) = term(10) + read_ftvvvv(b, a, a, c)
term(11) = term(11) + read_ftvvvv(b, c, a, a)
term(12) = term(12) + tvv(b, c)
term(13) = term(13) + tvoov(b, i, i, c)
term(14) = term(14) + tvvoo(b, c, i, i)
term(15) = term(15) + tvvoo(b, c, j, j)
term(16) = term(16) + tvoov(b, j, j, c)

term(10) = -term(10) 
term(14) = -term(14) 
term(15) = -term(15) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(17) = term(17) + t2(b,e,i,m) * tovov(m, e, i, c)
term(18) = term(18) + t2(b,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(17) = term(17) * 2.0d+0 
term(18) = term(18) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(19) = term(19) + t2(b,e,i,m) * tovov(m, c, i, e)
term(20) = term(20) + t2(b,e,m,i) * tovov(m, e, i, c)
term(21) = term(21) + t2(b,e,m,i) * tovov(m, c, i, e)
term(22) = term(22) + t2(b,e,m,j) * tovov(m, c, j, e)
term(23) = term(23) + t2(b,e,j,m) * tovov(m, c, j, e)
term(24) = term(24) + t2(b,e,m,j) * tovov(m, e, j, c)
end do 
end do 

term(19) = -term(19) 
term(20) = -term(20) 
term(23) = -term(23) 
term(24) = -term(24) 

do e = nocc + 1, nactive 
term(25) = term(25) + t2(b,e,i,j) * tovov(j, e, i, c)
term(26) = term(26) + t2(b,e,j,i) * tovov(j, e, i, c)
term(27) = term(27) + t2(b,e,i,j) * tovov(j, c, i, e)
term(28) = term(28) + t2(b,e,j,i) * tovov(j, c, i, e)
end do 

term(25) = -term(25) 
term(28) = -term(28) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(29) = term(29) + t2(b,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(29) = term(29) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(30) = term(30) + t2(b,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do m = 1, nocc 
term(31) = term(31) + t2(a,b,m,n) * tovov(n, a, m, c)
term(32) = term(32) + t2(a,b,m,n) * tovov(n, c, m, a)
end do 
end do 

term(31) = -term(31) 


    eom_cc3_22_tripletpp_trans_aibjcjai = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjcjai = eom_cc3_22_tripletpp_trans_aibjcjai + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjcjai
    function eom_cc3_22_tripletpp_trans_aibjciaj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjciaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e,n 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, i, a)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, c, j, a)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, a, j, c)
term(4) = term(4) + t2(a,b,m,j) * tovov(m, a, j, c)
term(5) = term(5) + t2(a,b,m,i) * tovov(m, a, i, c)
term(6) = term(6) + t2(a,b,i,m) * tovov(m, c, i, a)
term(7) = term(7) + t2(a,b,i,m) * tovov(m, a, i, c)
term(8) = term(8) + tvvoo(b, c, m, m)
term(9) = term(9) + tvoov(b, m, m, c)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 
term(7) = -term(7) 
term(8) = term(8) * (-2.0d+0) 

term(10) = term(10) + read_ftvvvv(b, c, a, a)
term(11) = term(11) + read_ftvvvv(b, a, a, c)
term(12) = term(12) + tvv(b, c)
term(13) = term(13) + tvvoo(b, c, i, i)
term(14) = term(14) + tvvoo(b, c, j, j)
term(15) = term(15) + tvoov(b, j, j, c)
term(16) = term(16) + tvoov(b, i, i, c)

term(10) = -term(10) 
term(12) = -term(12) 
term(15) = -term(15) 
term(16) = -term(16) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(b,e,m,i) * tovov(m, c, i, e)
term(18) = term(18) + t2(b,e,m,j) * tovov(m, c, j, e)
term(19) = term(19) + t2(b,e,j,m) * tovov(m, c, j, e)
term(20) = term(20) + t2(b,e,m,j) * tovov(m, e, j, c)
term(21) = term(21) + t2(b,e,i,m) * tovov(m, c, i, e)
term(22) = term(22) + t2(b,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(17) = -term(17) 
term(18) = -term(18) 

do e = nocc + 1, nactive 
term(23) = term(23) + t2(b,e,i,j) * tovov(j, c, i, e)
term(24) = term(24) + t2(b,e,j,i) * tovov(j, c, i, e)
term(25) = term(25) + t2(b,e,i,j) * tovov(j, e, i, c)
term(26) = term(26) + t2(b,e,j,i) * tovov(j, e, i, c)
end do 

term(23) = -term(23) 
term(26) = -term(26) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(27) = term(27) + t2(b,e,j,m) * tovov(m, e, j, c)
term(28) = term(28) + t2(b,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (-2.0d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(29) = term(29) + t2(b,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(29) = term(29) * 2.0d+0 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(30) = term(30) + t2(b,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 

term(30) = -term(30) 

do n = 1, nocc 
do m = 1, nocc 
term(31) = term(31) + t2(a,b,m,n) * tovov(n, c, m, a)
term(32) = term(32) + t2(a,b,m,n) * tovov(n, a, m, c)
end do 
end do 

term(31) = -term(31) 


    eom_cc3_22_tripletpp_trans_aibjciaj = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjciaj = eom_cc3_22_tripletpp_trans_aibjciaj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjciaj
    function eom_cc3_22_tripletpp_trans_aibjajbl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjajbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,f,m,e 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,i,m) * tovov(m, e, l, f)
end do 
end do 
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,m) * tovov(m, f, l, e)
end do 
end do 
end do 

term(1) = term(1) * 2.0d+0 

term(2) = term(2) + tvvoo(a, a, l, i)
term(3) = term(3) + tvvoo(b, b, l, i)
term(4) = term(4) + tvoov(b, i, l, b)
term(5) = term(5) + tvoov(a, i, l, a)
term(6) = term(6) + toooo(l, i, j, j)
term(7) = term(7) + toooo(l, j, j, i)
term(8) = term(8) + too(l, i)

term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(9) = term(9) + t2(b,e,m,i) * tovov(m, b, l, e)
term(10) = term(10) + t2(a,e,m,i) * tovov(m, a, l, e)
term(11) = term(11) + t2(b,e,i,m) * tovov(m, b, l, e)
term(12) = term(12) + t2(b,e,m,i) * tovov(m, e, l, b)
term(13) = term(13) + t2(a,e,i,m) * tovov(m, a, l, e)
term(14) = term(14) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 

term(9) = -term(9) 
term(10) = -term(10) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(15) = term(15) + t2(b,e,i,m) * tovov(m, e, l, b)
term(16) = term(16) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(15) = term(15) * (-2.0d+0) 
term(16) = term(16) * (-2.0d+0) 

do m = 1, nocc 
term(17) = term(17) + toooo(m, i, l, m)
term(18) = term(18) + toooo(m, m, l, i)
term(19) = term(19) + t2(a,b,m,i) * tovov(m, a, l, b)
term(20) = term(20) + t2(a,b,m,i) * tovov(m, b, l, a)
term(21) = term(21) + t2(a,b,i,m) * tovov(m, a, l, b)
term(22) = term(22) + t2(a,b,i,m) * tovov(m, b, l, a)
end do 

term(17) = -term(17) 
term(18) = term(18) * 2.0d+0 
term(20) = -term(20) 
term(21) = -term(21) 

do e = nocc + 1, nactive 
term(23) = term(23) + t2(b,e,i,j) * tovov(l, b, j, e)
term(24) = term(24) + t2(b,e,j,i) * tovov(l, b, j, e)
term(25) = term(25) + t2(a,e,j,i) * tovov(l, a, j, e)
term(26) = term(26) + t2(b,e,i,j) * tovov(l, e, j, b)
term(27) = term(27) + t2(b,e,j,i) * tovov(l, e, j, b)
term(28) = term(28) + t2(a,e,j,i) * tovov(l, e, j, a)
term(29) = term(29) + t2(a,e,i,j) * tovov(l, a, j, e)
term(30) = term(30) + t2(a,e,i,j) * tovov(l, e, j, a)
end do 

term(24) = -term(24) 
term(25) = -term(25) 
term(26) = -term(26) 
term(30) = -term(30) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(31) = term(31) + t2(e,f,i,j) * tovov(l, e, j, f)
end do 
end do 

term(31) = -term(31) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(32) = term(32) + t2(e,f,i,j) * tovov(l, f, j, e)
end do 
end do 



    eom_cc3_22_tripletpp_trans_aibjajbl = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjajbl = eom_cc3_22_tripletpp_trans_aibjajbl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjajbl
    function eom_cc3_22_tripletpp_trans_aibjakbj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjakbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,f,m,e 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,i,m) * tovov(m, e, k, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,m) * tovov(m, f, k, e)
end do 
end do 
end do 

term(1) = term(1) * (-2.0d+0) 

term(2) = term(2) + tvvoo(a, a, k, i)
term(3) = term(3) + tvvoo(b, b, k, i)
term(4) = term(4) + tvoov(b, i, k, b)
term(5) = term(5) + tvoov(a, i, k, a)
term(6) = term(6) + toooo(k, j, j, i)
term(7) = term(7) + toooo(k, i, j, j)
term(8) = term(8) + too(k, i)

term(2) = -term(2) 
term(3) = -term(3) 
term(6) = -term(6) 
term(8) = -term(8) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(9) = term(9) + t2(b,e,m,i) * tovov(m, b, k, e)
term(10) = term(10) + t2(a,e,m,i) * tovov(m, a, k, e)
term(11) = term(11) + t2(b,e,i,m) * tovov(m, b, k, e)
term(12) = term(12) + t2(b,e,m,i) * tovov(m, e, k, b)
term(13) = term(13) + t2(a,e,i,m) * tovov(m, a, k, e)
term(14) = term(14) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(11) = -term(11) 
term(12) = -term(12) 
term(13) = -term(13) 
term(14) = -term(14) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(15) = term(15) + t2(b,e,i,m) * tovov(m, e, k, b)
term(16) = term(16) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(15) = term(15) * 2.0d+0 
term(16) = term(16) * 2.0d+0 

do m = 1, nocc 
term(17) = term(17) + toooo(m, i, k, m)
term(18) = term(18) + toooo(m, m, k, i)
term(19) = term(19) + t2(a,b,m,i) * tovov(m, a, k, b)
term(20) = term(20) + t2(a,b,m,i) * tovov(m, b, k, a)
term(21) = term(21) + t2(a,b,i,m) * tovov(m, a, k, b)
term(22) = term(22) + t2(a,b,i,m) * tovov(m, b, k, a)
end do 

term(18) = term(18) * (-2.0d+0) 
term(19) = -term(19) 
term(22) = -term(22) 

do e = nocc + 1, nactive 
term(23) = term(23) + t2(b,e,i,j) * tovov(k, e, j, b)
term(24) = term(24) + t2(b,e,j,i) * tovov(k, e, j, b)
term(25) = term(25) + t2(a,e,j,i) * tovov(k, e, j, a)
term(26) = term(26) + t2(b,e,i,j) * tovov(k, b, j, e)
term(27) = term(27) + t2(b,e,j,i) * tovov(k, b, j, e)
term(28) = term(28) + t2(a,e,j,i) * tovov(k, a, j, e)
term(29) = term(29) + t2(a,e,i,j) * tovov(k, e, j, a)
term(30) = term(30) + t2(a,e,i,j) * tovov(k, a, j, e)
end do 

term(24) = -term(24) 
term(25) = -term(25) 
term(26) = -term(26) 
term(30) = -term(30) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(31) = term(31) + t2(e,f,i,j) * tovov(k, f, j, e)
end do 
end do 

term(31) = -term(31) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(32) = term(32) + t2(e,f,i,j) * tovov(k, e, j, f)
end do 
end do 



    eom_cc3_22_tripletpp_trans_aibjakbj = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjakbj = eom_cc3_22_tripletpp_trans_aibjakbj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjakbj
    function eom_cc3_22_tripletpp_trans_aibjaibl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjaibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,f,m,e 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,j,m) * tovov(m, e, l, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,j,m) * tovov(m, f, l, e)
end do 
end do 
end do 

term(1) = term(1) * (-2.0d+0) 

term(2) = term(2) + tvvoo(a, a, l, j)
term(3) = term(3) + tvvoo(b, b, l, j)
term(4) = term(4) + tvoov(b, j, l, b)
term(5) = term(5) + tvoov(a, j, l, a)
term(6) = term(6) + toooo(l, i, i, j)
term(7) = term(7) + toooo(l, j, i, i)
term(8) = term(8) + too(l, j)

term(2) = -term(2) 
term(3) = -term(3) 
term(6) = -term(6) 
term(8) = -term(8) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(9) = term(9) + t2(b,e,m,j) * tovov(m, b, l, e)
term(10) = term(10) + t2(a,e,m,j) * tovov(m, a, l, e)
term(11) = term(11) + t2(b,e,j,m) * tovov(m, b, l, e)
term(12) = term(12) + t2(a,e,j,m) * tovov(m, a, l, e)
term(13) = term(13) + t2(b,e,m,j) * tovov(m, e, l, b)
term(14) = term(14) + t2(a,e,m,j) * tovov(m, e, l, a)
end do 
end do 

term(11) = -term(11) 
term(12) = -term(12) 
term(13) = -term(13) 
term(14) = -term(14) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(15) = term(15) + t2(b,e,j,m) * tovov(m, e, l, b)
term(16) = term(16) + t2(a,e,j,m) * tovov(m, e, l, a)
end do 
end do 

term(15) = term(15) * 2.0d+0 
term(16) = term(16) * 2.0d+0 

do e = nocc + 1, nactive 
term(17) = term(17) + t2(b,e,i,j) * tovov(l, b, i, e)
term(18) = term(18) + t2(b,e,j,i) * tovov(l, b, i, e)
term(19) = term(19) + t2(a,e,j,i) * tovov(l, a, i, e)
term(20) = term(20) + t2(b,e,i,j) * tovov(l, e, i, b)
term(21) = term(21) + t2(b,e,j,i) * tovov(l, e, i, b)
term(22) = term(22) + t2(a,e,j,i) * tovov(l, e, i, a)
term(23) = term(23) + t2(a,e,i,j) * tovov(l, a, i, e)
term(24) = term(24) + t2(a,e,i,j) * tovov(l, e, i, a)
end do 

term(18) = -term(18) 
term(19) = -term(19) 
term(20) = -term(20) 
term(24) = -term(24) 

do m = 1, nocc 
term(25) = term(25) + toooo(m, j, l, m)
term(26) = term(26) + toooo(m, m, l, j)
term(27) = term(27) + t2(a,b,j,m) * tovov(m, a, l, b)
term(28) = term(28) + t2(a,b,m,j) * tovov(m, a, l, b)
term(29) = term(29) + t2(a,b,j,m) * tovov(m, b, l, a)
term(30) = term(30) + t2(a,b,m,j) * tovov(m, b, l, a)
end do 

term(26) = term(26) * (-2.0d+0) 
term(28) = -term(28) 
term(29) = -term(29) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(31) = term(31) + t2(e,f,i,j) * tovov(l, e, i, f)
end do 
end do 

term(31) = -term(31) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(32) = term(32) + t2(e,f,i,j) * tovov(l, f, i, e)
end do 
end do 



    eom_cc3_22_tripletpp_trans_aibjaibl = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjaibl = eom_cc3_22_tripletpp_trans_aibjaibl + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjaibl
    function eom_cc3_22_tripletpp_trans_aibjakbi(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjakbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,f,m,e 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,j,m) * tovov(m, e, k, f)
end do 
end do 
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,j,m) * tovov(m, f, k, e)
end do 
end do 
end do 

term(1) = term(1) * 2.0d+0 

term(2) = term(2) + tvvoo(a, a, k, j)
term(3) = term(3) + tvvoo(b, b, k, j)
term(4) = term(4) + tvoov(b, j, k, b)
term(5) = term(5) + tvoov(a, j, k, a)
term(6) = term(6) + toooo(k, j, i, i)
term(7) = term(7) + toooo(k, i, i, j)
term(8) = term(8) + too(k, j)

term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(9) = term(9) + t2(b,e,m,j) * tovov(m, b, k, e)
term(10) = term(10) + t2(a,e,m,j) * tovov(m, a, k, e)
term(11) = term(11) + t2(b,e,j,m) * tovov(m, b, k, e)
term(12) = term(12) + t2(b,e,m,j) * tovov(m, e, k, b)
term(13) = term(13) + t2(a,e,j,m) * tovov(m, a, k, e)
term(14) = term(14) + t2(a,e,m,j) * tovov(m, e, k, a)
end do 
end do 

term(9) = -term(9) 
term(10) = -term(10) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(15) = term(15) + t2(b,e,j,m) * tovov(m, e, k, b)
term(16) = term(16) + t2(a,e,j,m) * tovov(m, e, k, a)
end do 
end do 

term(15) = term(15) * (-2.0d+0) 
term(16) = term(16) * (-2.0d+0) 

do m = 1, nocc 
term(17) = term(17) + toooo(m, j, k, m)
term(18) = term(18) + toooo(m, m, k, j)
term(19) = term(19) + t2(a,b,j,m) * tovov(m, a, k, b)
term(20) = term(20) + t2(a,b,m,j) * tovov(m, a, k, b)
term(21) = term(21) + t2(a,b,j,m) * tovov(m, b, k, a)
term(22) = term(22) + t2(a,b,m,j) * tovov(m, b, k, a)
end do 

term(17) = -term(17) 
term(18) = term(18) * 2.0d+0 
term(19) = -term(19) 
term(22) = -term(22) 

do e = nocc + 1, nactive 
term(23) = term(23) + t2(b,e,i,j) * tovov(k, e, i, b)
term(24) = term(24) + t2(b,e,j,i) * tovov(k, e, i, b)
term(25) = term(25) + t2(a,e,j,i) * tovov(k, e, i, a)
term(26) = term(26) + t2(b,e,i,j) * tovov(k, b, i, e)
term(27) = term(27) + t2(b,e,j,i) * tovov(k, b, i, e)
term(28) = term(28) + t2(a,e,j,i) * tovov(k, a, i, e)
term(29) = term(29) + t2(a,e,i,j) * tovov(k, e, i, a)
term(30) = term(30) + t2(a,e,i,j) * tovov(k, a, i, e)
end do 

term(24) = -term(24) 
term(25) = -term(25) 
term(26) = -term(26) 
term(30) = -term(30) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(31) = term(31) + t2(e,f,i,j) * tovov(k, f, i, e)
end do 
end do 

term(31) = -term(31) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(32) = term(32) + t2(e,f,i,j) * tovov(k, e, i, f)
end do 
end do 



    eom_cc3_22_tripletpp_trans_aibjakbi = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjakbi = eom_cc3_22_tripletpp_trans_aibjakbi + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjakbi
    function eom_cc3_22_tripletpp_trans_aibjajdi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjajdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e,n 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, i, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, j, d)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, d, i, a)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, d, j, a)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, d, j, a)
term(6) = term(6) + t2(a,b,i,m) * tovov(m, a, i, d)
term(7) = term(7) + t2(a,b,i,m) * tovov(m, d, i, a)
term(8) = term(8) + tvvoo(b, d, m, m)
term(9) = term(9) + tvoov(b, m, m, d)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 
term(8) = term(8) * (-2.0d+0) 

term(10) = term(10) + read_ftvvvv(b, d, a, a)
term(11) = term(11) + read_ftvvvv(b, a, a, d)
term(12) = term(12) + tvv(b, d)
term(13) = term(13) + tvvoo(b, d, j, j)
term(14) = term(14) + tvvoo(b, d, i, i)
term(15) = term(15) + tvoov(b, i, i, d)
term(16) = term(16) + tvoov(b, j, j, d)

term(10) = -term(10) 
term(12) = -term(12) 
term(15) = -term(15) 
term(16) = -term(16) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(b,e,m,j) * tovov(m, d, j, e)
term(18) = term(18) + t2(b,e,j,m) * tovov(m, d, j, e)
term(19) = term(19) + t2(b,e,m,i) * tovov(m, d, i, e)
term(20) = term(20) + t2(b,e,i,m) * tovov(m, d, i, e)
term(21) = term(21) + t2(b,e,m,j) * tovov(m, e, j, d)
term(22) = term(22) + t2(b,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(17) = -term(17) 
term(19) = -term(19) 

do e = nocc + 1, nactive 
term(23) = term(23) + t2(b,e,i,j) * tovov(j, e, i, d)
term(24) = term(24) + t2(b,e,j,i) * tovov(j, e, i, d)
term(25) = term(25) + t2(b,e,i,j) * tovov(j, d, i, e)
term(26) = term(26) + t2(b,e,j,i) * tovov(j, d, i, e)
end do 

term(24) = -term(24) 
term(25) = -term(25) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(27) = term(27) + t2(b,e,j,m) * tovov(m, e, j, d)
term(28) = term(28) + t2(b,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (-2.0d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(29) = term(29) + t2(b,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(29) = term(29) * 2.0d+0 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(30) = term(30) + t2(b,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 

term(30) = -term(30) 

do n = 1, nocc 
do m = 1, nocc 
term(31) = term(31) + t2(a,b,m,n) * tovov(n, d, m, a)
term(32) = term(32) + t2(a,b,m,n) * tovov(n, a, m, d)
end do 
end do 

term(31) = -term(31) 


    eom_cc3_22_tripletpp_trans_aibjajdi = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjajdi = eom_cc3_22_tripletpp_trans_aibjajdi + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjajdi
    function eom_cc3_22_tripletpp_trans_aibjaidj(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjaidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e,n 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, i, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, j, d)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, d, j, a)
term(4) = term(4) + t2(a,b,m,j) * tovov(m, d, j, a)
term(5) = term(5) + t2(a,b,m,i) * tovov(m, d, i, a)
term(6) = term(6) + t2(a,b,i,m) * tovov(m, a, i, d)
term(7) = term(7) + t2(a,b,i,m) * tovov(m, d, i, a)
term(8) = term(8) + tvvoo(b, d, m, m)
term(9) = term(9) + tvoov(b, m, m, d)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 
term(7) = -term(7) 
term(8) = term(8) * 2.0d+0 
term(9) = -term(9) 

term(10) = term(10) + read_ftvvvv(b, a, a, d)
term(11) = term(11) + read_ftvvvv(b, d, a, a)
term(12) = term(12) + tvv(b, d)
term(13) = term(13) + tvvoo(b, d, i, i)
term(14) = term(14) + tvvoo(b, d, j, j)
term(15) = term(15) + tvoov(b, i, i, d)
term(16) = term(16) + tvoov(b, j, j, d)

term(10) = -term(10) 
term(13) = -term(13) 
term(14) = -term(14) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(b,e,m,i) * tovov(m, d, i, e)
term(18) = term(18) + t2(b,e,i,m) * tovov(m, d, i, e)
term(19) = term(19) + t2(b,e,m,j) * tovov(m, d, j, e)
term(20) = term(20) + t2(b,e,j,m) * tovov(m, d, j, e)
term(21) = term(21) + t2(b,e,m,i) * tovov(m, e, i, d)
term(22) = term(22) + t2(b,e,m,j) * tovov(m, e, j, d)
end do 
end do 

term(18) = -term(18) 
term(20) = -term(20) 
term(21) = -term(21) 
term(22) = -term(22) 

do e = nocc + 1, nactive 
term(23) = term(23) + t2(b,e,i,j) * tovov(j, d, i, e)
term(24) = term(24) + t2(b,e,j,i) * tovov(j, d, i, e)
term(25) = term(25) + t2(b,e,i,j) * tovov(j, e, i, d)
term(26) = term(26) + t2(b,e,j,i) * tovov(j, e, i, d)
end do 

term(24) = -term(24) 
term(25) = -term(25) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(27) = term(27) + t2(b,e,i,m) * tovov(m, e, i, d)
term(28) = term(28) + t2(b,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(27) = term(27) * 2.0d+0 
term(28) = term(28) * 2.0d+0 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(29) = term(29) + t2(b,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(29) = term(29) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(30) = term(30) + t2(b,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do m = 1, nocc 
term(31) = term(31) + t2(a,b,m,n) * tovov(n, a, m, d)
term(32) = term(32) + t2(a,b,m,n) * tovov(n, d, m, a)
end do 
end do 

term(31) = -term(31) 


    eom_cc3_22_tripletpp_trans_aibjaidj = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjaidj = eom_cc3_22_tripletpp_trans_aibjaidj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjaidj
    function eom_cc3_22_tripletpp_trans_aibjcjbi(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjcjbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e,n 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, i, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, c, j, b)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, b, i, c)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, b, j, c)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, b, j, c)
term(6) = term(6) + t2(a,b,i,m) * tovov(m, c, i, b)
term(7) = term(7) + t2(a,b,i,m) * tovov(m, b, i, c)
term(8) = term(8) + tvvoo(a, c, m, m)
term(9) = term(9) + tvoov(a, m, m, c)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 
term(8) = term(8) * (-2.0d+0) 

term(10) = term(10) + read_ftvvvv(b, b, a, c)
term(11) = term(11) + read_ftvvvv(b, c, a, b)
term(12) = term(12) + tvv(a, c)
term(13) = term(13) + tvvoo(a, c, i, i)
term(14) = term(14) + tvvoo(a, c, j, j)
term(15) = term(15) + tvoov(a, j, j, c)
term(16) = term(16) + tvoov(a, i, i, c)

term(10) = -term(10) 
term(12) = -term(12) 
term(15) = -term(15) 
term(16) = -term(16) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(a,e,m,j) * tovov(m, c, j, e)
term(18) = term(18) + t2(a,e,m,i) * tovov(m, c, i, e)
term(19) = term(19) + t2(a,e,j,m) * tovov(m, c, j, e)
term(20) = term(20) + t2(a,e,m,j) * tovov(m, e, j, c)
term(21) = term(21) + t2(a,e,i,m) * tovov(m, c, i, e)
term(22) = term(22) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(17) = -term(17) 
term(18) = -term(18) 

do e = nocc + 1, nactive 
term(23) = term(23) + t2(a,e,j,i) * tovov(j, e, i, c)
term(24) = term(24) + t2(a,e,j,i) * tovov(j, c, i, e)
term(25) = term(25) + t2(a,e,i,j) * tovov(j, e, i, c)
term(26) = term(26) + t2(a,e,i,j) * tovov(j, c, i, e)
end do 

term(23) = -term(23) 
term(26) = -term(26) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(27) = term(27) + t2(a,e,j,m) * tovov(m, e, j, c)
term(28) = term(28) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (-2.0d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(29) = term(29) + t2(a,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(29) = term(29) * 2.0d+0 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(30) = term(30) + t2(a,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 

term(30) = -term(30) 

do n = 1, nocc 
do m = 1, nocc 
term(31) = term(31) + t2(a,b,m,n) * tovov(n, b, m, c)
term(32) = term(32) + t2(a,b,m,n) * tovov(n, c, m, b)
end do 
end do 

term(31) = -term(31) 


    eom_cc3_22_tripletpp_trans_aibjcjbi = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjcjbi = eom_cc3_22_tripletpp_trans_aibjcjbi + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjcjbi
    function eom_cc3_22_tripletpp_trans_aibjcibj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjcibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e,n 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, i, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, c, j, b)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, b, j, c)
term(4) = term(4) + t2(a,b,m,j) * tovov(m, b, j, c)
term(5) = term(5) + t2(a,b,m,i) * tovov(m, b, i, c)
term(6) = term(6) + t2(a,b,i,m) * tovov(m, c, i, b)
term(7) = term(7) + t2(a,b,i,m) * tovov(m, b, i, c)
term(8) = term(8) + tvvoo(a, c, m, m)
term(9) = term(9) + tvoov(a, m, m, c)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 
term(7) = -term(7) 
term(8) = term(8) * 2.0d+0 
term(9) = -term(9) 

term(10) = term(10) + read_ftvvvv(b, c, a, b)
term(11) = term(11) + read_ftvvvv(b, b, a, c)
term(12) = term(12) + tvv(a, c)
term(13) = term(13) + tvvoo(a, c, i, i)
term(14) = term(14) + tvvoo(a, c, j, j)
term(15) = term(15) + tvoov(a, j, j, c)
term(16) = term(16) + tvoov(a, i, i, c)

term(10) = -term(10) 
term(13) = -term(13) 
term(14) = -term(14) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(a,e,m,i) * tovov(m, c, i, e)
term(18) = term(18) + t2(a,e,m,j) * tovov(m, c, j, e)
term(19) = term(19) + t2(a,e,j,m) * tovov(m, c, j, e)
term(20) = term(20) + t2(a,e,m,j) * tovov(m, e, j, c)
term(21) = term(21) + t2(a,e,i,m) * tovov(m, c, i, e)
term(22) = term(22) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(19) = -term(19) 
term(20) = -term(20) 
term(21) = -term(21) 
term(22) = -term(22) 

do e = nocc + 1, nactive 
term(23) = term(23) + t2(a,e,j,i) * tovov(j, c, i, e)
term(24) = term(24) + t2(a,e,j,i) * tovov(j, e, i, c)
term(25) = term(25) + t2(a,e,i,j) * tovov(j, c, i, e)
term(26) = term(26) + t2(a,e,i,j) * tovov(j, e, i, c)
end do 

term(23) = -term(23) 
term(26) = -term(26) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(27) = term(27) + t2(a,e,j,m) * tovov(m, e, j, c)
term(28) = term(28) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(27) = term(27) * 2.0d+0 
term(28) = term(28) * 2.0d+0 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(29) = term(29) + t2(a,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(29) = term(29) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(30) = term(30) + t2(a,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do m = 1, nocc 
term(31) = term(31) + t2(a,b,m,n) * tovov(n, c, m, b)
term(32) = term(32) + t2(a,b,m,n) * tovov(n, b, m, c)
end do 
end do 

term(31) = -term(31) 


    eom_cc3_22_tripletpp_trans_aibjcibj = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjcibj = eom_cc3_22_tripletpp_trans_aibjcibj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjcibj
    function eom_cc3_22_tripletpp_trans_aibjbjdi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbjdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e,n 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, b, j, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, i, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, j, d)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, d, i, b)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, d, j, b)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, d, j, b)
term(6) = term(6) + t2(a,b,i,m) * tovov(m, b, i, d)
term(7) = term(7) + t2(a,b,i,m) * tovov(m, d, i, b)
term(8) = term(8) + tvvoo(a, d, m, m)
term(9) = term(9) + tvoov(a, m, m, d)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 
term(8) = term(8) * 2.0d+0 
term(9) = -term(9) 

term(10) = term(10) + read_ftvvvv(b, d, a, b)
term(11) = term(11) + read_ftvvvv(b, b, a, d)
term(12) = term(12) + tvv(a, d)
term(13) = term(13) + tvoov(a, j, j, d)
term(14) = term(14) + tvvoo(a, d, i, i)
term(15) = term(15) + tvvoo(a, d, j, j)
term(16) = term(16) + tvoov(a, i, i, d)

term(10) = -term(10) 
term(14) = -term(14) 
term(15) = -term(15) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(17) = term(17) + t2(a,e,j,m) * tovov(m, e, j, d)
term(18) = term(18) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(17) = term(17) * 2.0d+0 
term(18) = term(18) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(19) = term(19) + t2(a,e,j,m) * tovov(m, d, j, e)
term(20) = term(20) + t2(a,e,m,j) * tovov(m, e, j, d)
term(21) = term(21) + t2(a,e,m,i) * tovov(m, d, i, e)
term(22) = term(22) + t2(a,e,m,j) * tovov(m, d, j, e)
term(23) = term(23) + t2(a,e,i,m) * tovov(m, d, i, e)
term(24) = term(24) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(19) = -term(19) 
term(20) = -term(20) 
term(23) = -term(23) 
term(24) = -term(24) 

do e = nocc + 1, nactive 
term(25) = term(25) + t2(a,e,j,i) * tovov(j, e, i, d)
term(26) = term(26) + t2(a,e,j,i) * tovov(j, d, i, e)
term(27) = term(27) + t2(a,e,i,j) * tovov(j, e, i, d)
term(28) = term(28) + t2(a,e,i,j) * tovov(j, d, i, e)
end do 

term(26) = -term(26) 
term(27) = -term(27) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(29) = term(29) + t2(a,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(29) = term(29) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(30) = term(30) + t2(a,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do m = 1, nocc 
term(31) = term(31) + t2(a,b,m,n) * tovov(n, d, m, b)
term(32) = term(32) + t2(a,b,m,n) * tovov(n, b, m, d)
end do 
end do 

term(31) = -term(31) 


    eom_cc3_22_tripletpp_trans_aibjbjdi = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjbjdi = eom_cc3_22_tripletpp_trans_aibjbjdi + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbjdi
    function eom_cc3_22_tripletpp_trans_aibjbidj(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e,n 
    double precision, dimension(0:32) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, b, j, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, i, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, j, d)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, d, j, b)
term(4) = term(4) + t2(a,b,m,j) * tovov(m, d, j, b)
term(5) = term(5) + t2(a,b,m,i) * tovov(m, d, i, b)
term(6) = term(6) + t2(a,b,i,m) * tovov(m, b, i, d)
term(7) = term(7) + t2(a,b,i,m) * tovov(m, d, i, b)
term(8) = term(8) + tvvoo(a, d, m, m)
term(9) = term(9) + tvoov(a, m, m, d)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 
term(7) = -term(7) 
term(8) = term(8) * (-2.0d+0) 

term(10) = term(10) + read_ftvvvv(b, b, a, d)
term(11) = term(11) + read_ftvvvv(b, d, a, b)
term(12) = term(12) + tvv(a, d)
term(13) = term(13) + tvvoo(a, d, i, i)
term(14) = term(14) + tvvoo(a, d, j, j)
term(15) = term(15) + tvoov(a, j, j, d)
term(16) = term(16) + tvoov(a, i, i, d)

term(10) = -term(10) 
term(12) = -term(12) 
term(15) = -term(15) 
term(16) = -term(16) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(a,e,m,i) * tovov(m, d, i, e)
term(18) = term(18) + t2(a,e,m,j) * tovov(m, d, j, e)
term(19) = term(19) + t2(a,e,j,m) * tovov(m, d, j, e)
term(20) = term(20) + t2(a,e,m,j) * tovov(m, e, j, d)
term(21) = term(21) + t2(a,e,i,m) * tovov(m, d, i, e)
term(22) = term(22) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(17) = -term(17) 
term(18) = -term(18) 

do e = nocc + 1, nactive 
term(23) = term(23) + t2(a,e,j,i) * tovov(j, d, i, e)
term(24) = term(24) + t2(a,e,j,i) * tovov(j, e, i, d)
term(25) = term(25) + t2(a,e,i,j) * tovov(j, d, i, e)
term(26) = term(26) + t2(a,e,i,j) * tovov(j, e, i, d)
end do 

term(24) = -term(24) 
term(25) = -term(25) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(27) = term(27) + t2(a,e,j,m) * tovov(m, e, j, d)
term(28) = term(28) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (-2.0d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(29) = term(29) + t2(a,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(29) = term(29) * 2.0d+0 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(30) = term(30) + t2(a,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 

term(30) = -term(30) 

do n = 1, nocc 
do m = 1, nocc 
term(31) = term(31) + t2(a,b,m,n) * tovov(n, b, m, d)
term(32) = term(32) + t2(a,b,m,n) * tovov(n, d, m, b)
end do 
end do 

term(31) = -term(31) 


    eom_cc3_22_tripletpp_trans_aibjbidj = 0.d+0
    do s = 0, 32
    eom_cc3_22_tripletpp_trans_aibjbidj = eom_cc3_22_tripletpp_trans_aibjbidj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbidj
    function eom_cc3_22_tripletpp_trans_aibjbjai(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e,f,n 
    double precision, dimension(0:67) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, b, j, a)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, i, a)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, j, a)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, a, i, b)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, a, j, b)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, a, j, b)
term(6) = term(6) + t2(a,b,i,m) * tovov(m, b, i, a)
term(7) = term(7) + t2(a,b,i,m) * tovov(m, a, i, b)
term(8) = term(8) + toooo(m, i, i, m)
term(9) = term(9) + toooo(m, m, i, i)
term(10) = term(10) + toooo(m, j, j, m)
term(11) = term(11) + toooo(m, m, j, j)
term(12) = term(12) + tvvoo(a, a, m, m)
term(13) = term(13) + tvvoo(b, b, m, m)
term(14) = term(14) + tvoov(a, m, m, a)
term(15) = term(15) + tvoov(b, m, m, b)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 
term(9) = term(9) * (-2.0d+0) 
term(11) = term(11) * (-2.0d+0) 
term(12) = term(12) * 2.0d+0 
term(13) = term(13) * 2.0d+0 
term(14) = -term(14) 
term(15) = -term(15) 

do e = nocc + 1, nactive 
term(16) = term(16) + t2(a,e,j,i) * tovov(j, e, i, a)
term(17) = term(17) + t2(b,e,i,j) * tovov(j, e, i, b)
term(18) = term(18) + t2(b,e,j,i) * tovov(j, e, i, b)
term(19) = term(19) + t2(a,e,j,i) * tovov(j, a, i, e)
term(20) = term(20) + t2(b,e,i,j) * tovov(j, b, i, e)
term(21) = term(21) + t2(b,e,j,i) * tovov(j, b, i, e)
term(22) = term(22) + t2(a,e,i,j) * tovov(j, e, i, a)
term(23) = term(23) + t2(a,e,i,j) * tovov(j, a, i, e)
end do 

term(17) = -term(17) 
term(19) = -term(19) 
term(21) = -term(21) 
term(22) = -term(22) 

do n = 1, nocc 
do m = 1, nocc 
term(24) = term(24) + t2(a,b,m,n) * tovov(n, a, m, b)
term(25) = term(25) + t2(a,b,m,n) * tovov(n, b, m, a)
end do 
end do 

term(24) = -term(24) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(26) = term(26) + t2(e,f,i,m) * tovov(m, e, i, f)
term(27) = term(27) + t2(e,f,j,m) * tovov(m, e, j, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(28) = term(28) + t2(e,f,i,m) * tovov(m, f, i, e)
term(29) = term(29) + t2(e,f,j,m) * tovov(m, f, j, e)
end do 
end do 
end do 

term(28) = term(28) * (-2.0d+0) 
term(29) = term(29) * (-2.0d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(30) = term(30) + t2(a,e,m,n) * tovov(n, e, m, a)
term(31) = term(31) + t2(b,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (-2.0d+0) 

term(32) = term(32) + tvv(a, a)
term(33) = term(33) + tvv(b, b)
term(34) = term(34) + too(i, i)
term(35) = term(35) + too(j, j)
term(36) = term(36) + read_ftvvvv(b, a, a, b)
term(37) = term(37) + read_ftvvvv(b, b, a, a)
term(38) = term(38) + toooo(j, i, i, j)
term(39) = term(39) + toooo(j, j, i, i)
term(40) = term(40) + tvoov(a, j, j, a)
term(41) = term(41) + tvoov(b, i, i, b)
term(42) = term(42) + tvvoo(a, a, i, i)
term(43) = term(43) + tvvoo(a, a, j, j)
term(44) = term(44) + tvvoo(b, b, i, i)
term(45) = term(45) + tvvoo(b, b, j, j)
term(46) = term(46) + tvoov(b, j, j, b)
term(47) = term(47) + tvoov(a, i, i, a)

term(34) = -term(34) 
term(35) = -term(35) 
term(36) = -term(36) 
term(38) = -term(38) 
term(42) = -term(42) 
term(43) = -term(43) 
term(44) = -term(44) 
term(45) = -term(45) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(48) = term(48) + t2(a,e,m,n) * tovov(n, a, m, e)
term(49) = term(49) + t2(b,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(50) = term(50) + t2(e,f,i,j) * tovov(j, e, i, f)
end do 
end do 

term(50) = -term(50) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(51) = term(51) + t2(e,f,i,j) * tovov(j, f, i, e)
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(52) = term(52) + t2(a,e,j,m) * tovov(m, a, j, e)
term(53) = term(53) + t2(b,e,i,m) * tovov(m, b, i, e)
term(54) = term(54) + t2(a,e,m,j) * tovov(m, e, j, a)
term(55) = term(55) + t2(b,e,m,i) * tovov(m, e, i, b)
term(56) = term(56) + t2(b,e,m,i) * tovov(m, b, i, e)
term(57) = term(57) + t2(a,e,m,i) * tovov(m, a, i, e)
term(58) = term(58) + t2(b,e,m,j) * tovov(m, b, j, e)
term(59) = term(59) + t2(a,e,m,j) * tovov(m, a, j, e)
term(60) = term(60) + t2(b,e,j,m) * tovov(m, b, j, e)
term(61) = term(61) + t2(b,e,m,j) * tovov(m, e, j, b)
term(62) = term(62) + t2(a,e,i,m) * tovov(m, a, i, e)
term(63) = term(63) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(52) = -term(52) 
term(53) = -term(53) 
term(54) = -term(54) 
term(55) = -term(55) 
term(60) = -term(60) 
term(61) = -term(61) 
term(62) = -term(62) 
term(63) = -term(63) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(64) = term(64) + t2(a,e,j,m) * tovov(m, e, j, a)
term(65) = term(65) + t2(b,e,i,m) * tovov(m, e, i, b)
term(66) = term(66) + t2(b,e,j,m) * tovov(m, e, j, b)
term(67) = term(67) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(64) = term(64) * 2.0d+0 
term(65) = term(65) * 2.0d+0 
term(66) = term(66) * 2.0d+0 
term(67) = term(67) * 2.0d+0 


    eom_cc3_22_tripletpp_trans_aibjbjai = 0.d+0
    do s = 0, 67
    eom_cc3_22_tripletpp_trans_aibjbjai = eom_cc3_22_tripletpp_trans_aibjbjai + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbjai
    function eom_cc3_22_tripletpp_trans_aibjbiaj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjbiaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e,f,n 
    double precision, dimension(0:67) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, b, j, a)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, i, a)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, j, a)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, a, j, b)
term(4) = term(4) + t2(a,b,m,j) * tovov(m, a, j, b)
term(5) = term(5) + t2(a,b,m,i) * tovov(m, a, i, b)
term(6) = term(6) + t2(a,b,i,m) * tovov(m, b, i, a)
term(7) = term(7) + t2(a,b,i,m) * tovov(m, a, i, b)
term(8) = term(8) + toooo(m, i, i, m)
term(9) = term(9) + toooo(m, m, i, i)
term(10) = term(10) + toooo(m, j, j, m)
term(11) = term(11) + toooo(m, m, j, j)
term(12) = term(12) + tvvoo(b, b, m, m)
term(13) = term(13) + tvvoo(a, a, m, m)
term(14) = term(14) + tvoov(b, m, m, b)
term(15) = term(15) + tvoov(a, m, m, a)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 
term(11) = term(11) * 2.0d+0 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (-2.0d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(16) = term(16) + t2(a,b,m,n) * tovov(n, b, m, a)
term(17) = term(17) + t2(a,b,m,n) * tovov(n, a, m, b)
end do 
end do 

term(16) = -term(16) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(18) = term(18) + t2(e,f,i,m) * tovov(m, e, i, f)
term(19) = term(19) + t2(e,f,j,m) * tovov(m, e, j, f)
end do 
end do 
end do 

term(18) = -term(18) 
term(19) = -term(19) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(20) = term(20) + t2(e,f,i,m) * tovov(m, f, i, e)
term(21) = term(21) + t2(e,f,j,m) * tovov(m, f, j, e)
end do 
end do 
end do 

term(20) = term(20) * 2.0d+0 
term(21) = term(21) * 2.0d+0 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(22) = term(22) + t2(b,e,m,n) * tovov(n, e, m, b)
term(23) = term(23) + t2(a,e,m,n) * tovov(n, e, m, a)
end do 
end do 
end do 

term(22) = term(22) * 2.0d+0 
term(23) = term(23) * 2.0d+0 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(24) = term(24) + t2(e,f,i,j) * tovov(j, f, i, e)
end do 
end do 

term(24) = -term(24) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(25) = term(25) + t2(b,e,m,n) * tovov(n, b, m, e)
term(26) = term(26) + t2(a,e,m,n) * tovov(n, a, m, e)
end do 
end do 
end do 

term(25) = -term(25) 
term(26) = -term(26) 

term(27) = term(27) + tvv(b, b)
term(28) = term(28) + tvv(a, a)
term(29) = term(29) + too(i, i)
term(30) = term(30) + too(j, j)
term(31) = term(31) + read_ftvvvv(b, b, a, a)
term(32) = term(32) + read_ftvvvv(b, a, a, b)
term(33) = term(33) + tvvoo(b, b, i, i)
term(34) = term(34) + tvvoo(b, b, j, j)
term(35) = term(35) + tvvoo(a, a, i, i)
term(36) = term(36) + tvvoo(a, a, j, j)
term(37) = term(37) + tvoov(a, j, j, a)
term(38) = term(38) + tvoov(b, j, j, b)
term(39) = term(39) + tvoov(b, i, i, b)
term(40) = term(40) + tvoov(a, i, i, a)
term(41) = term(41) + toooo(j, j, i, i)
term(42) = term(42) + toooo(j, i, i, j)

term(27) = -term(27) 
term(28) = -term(28) 
term(31) = -term(31) 
term(37) = -term(37) 
term(38) = -term(38) 
term(39) = -term(39) 
term(40) = -term(40) 
term(41) = -term(41) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(43) = term(43) + t2(e,f,i,j) * tovov(j, e, i, f)
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(44) = term(44) + t2(a,e,m,i) * tovov(m, a, i, e)
term(45) = term(45) + t2(b,e,m,i) * tovov(m, b, i, e)
term(46) = term(46) + t2(a,e,m,j) * tovov(m, a, j, e)
term(47) = term(47) + t2(b,e,m,j) * tovov(m, b, j, e)
term(48) = term(48) + t2(a,e,j,m) * tovov(m, a, j, e)
term(49) = term(49) + t2(b,e,j,m) * tovov(m, b, j, e)
term(50) = term(50) + t2(a,e,m,j) * tovov(m, e, j, a)
term(51) = term(51) + t2(b,e,m,j) * tovov(m, e, j, b)
term(52) = term(52) + t2(b,e,i,m) * tovov(m, b, i, e)
term(53) = term(53) + t2(b,e,m,i) * tovov(m, e, i, b)
term(54) = term(54) + t2(a,e,i,m) * tovov(m, a, i, e)
term(55) = term(55) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(44) = -term(44) 
term(45) = -term(45) 
term(46) = -term(46) 
term(47) = -term(47) 

do e = nocc + 1, nactive 
term(56) = term(56) + t2(a,e,j,i) * tovov(j, a, i, e)
term(57) = term(57) + t2(b,e,i,j) * tovov(j, b, i, e)
term(58) = term(58) + t2(b,e,j,i) * tovov(j, b, i, e)
term(59) = term(59) + t2(a,e,j,i) * tovov(j, e, i, a)
term(60) = term(60) + t2(b,e,i,j) * tovov(j, e, i, b)
term(61) = term(61) + t2(b,e,j,i) * tovov(j, e, i, b)
term(62) = term(62) + t2(a,e,i,j) * tovov(j, a, i, e)
term(63) = term(63) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 

term(57) = -term(57) 
term(59) = -term(59) 
term(61) = -term(61) 
term(62) = -term(62) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(64) = term(64) + t2(a,e,j,m) * tovov(m, e, j, a)
term(65) = term(65) + t2(b,e,j,m) * tovov(m, e, j, b)
term(66) = term(66) + t2(b,e,i,m) * tovov(m, e, i, b)
term(67) = term(67) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * (-2.0d+0) 
term(66) = term(66) * (-2.0d+0) 
term(67) = term(67) * (-2.0d+0) 


    eom_cc3_22_tripletpp_trans_aibjbiaj = 0.d+0
    do s = 0, 67
    eom_cc3_22_tripletpp_trans_aibjbiaj = eom_cc3_22_tripletpp_trans_aibjbiaj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjbiaj
    function eom_cc3_22_tripletpp_trans_aibjajbi(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjajbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e,f,n 
    double precision, dimension(0:67) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, i, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, j, b)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, b, i, a)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, b, j, a)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, b, j, a)
term(6) = term(6) + t2(a,b,i,m) * tovov(m, a, i, b)
term(7) = term(7) + t2(a,b,i,m) * tovov(m, b, i, a)
term(8) = term(8) + toooo(m, j, j, m)
term(9) = term(9) + toooo(m, m, j, j)
term(10) = term(10) + toooo(m, i, i, m)
term(11) = term(11) + toooo(m, m, i, i)
term(12) = term(12) + tvvoo(a, a, m, m)
term(13) = term(13) + tvvoo(b, b, m, m)
term(14) = term(14) + tvoov(a, m, m, a)
term(15) = term(15) + tvoov(b, m, m, b)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 
term(8) = -term(8) 
term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 
term(11) = term(11) * 2.0d+0 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (-2.0d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(16) = term(16) + t2(a,b,m,n) * tovov(n, b, m, a)
term(17) = term(17) + t2(a,b,m,n) * tovov(n, a, m, b)
end do 
end do 

term(16) = -term(16) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(18) = term(18) + t2(e,f,j,m) * tovov(m, e, j, f)
term(19) = term(19) + t2(e,f,i,m) * tovov(m, e, i, f)
end do 
end do 
end do 

term(18) = -term(18) 
term(19) = -term(19) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(20) = term(20) + t2(e,f,j,m) * tovov(m, f, j, e)
term(21) = term(21) + t2(e,f,i,m) * tovov(m, f, i, e)
end do 
end do 
end do 

term(20) = term(20) * 2.0d+0 
term(21) = term(21) * 2.0d+0 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(22) = term(22) + t2(a,e,m,n) * tovov(n, e, m, a)
term(23) = term(23) + t2(b,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(22) = term(22) * 2.0d+0 
term(23) = term(23) * 2.0d+0 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(24) = term(24) + t2(e,f,i,j) * tovov(j, f, i, e)
end do 
end do 

term(24) = -term(24) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(25) = term(25) + t2(a,e,m,n) * tovov(n, a, m, e)
term(26) = term(26) + t2(b,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 

term(25) = -term(25) 
term(26) = -term(26) 

term(27) = term(27) + tvv(a, a)
term(28) = term(28) + tvv(b, b)
term(29) = term(29) + too(j, j)
term(30) = term(30) + too(i, i)
term(31) = term(31) + read_ftvvvv(b, b, a, a)
term(32) = term(32) + read_ftvvvv(b, a, a, b)
term(33) = term(33) + tvvoo(a, a, i, i)
term(34) = term(34) + tvvoo(a, a, j, j)
term(35) = term(35) + tvvoo(b, b, j, j)
term(36) = term(36) + tvvoo(b, b, i, i)
term(37) = term(37) + tvoov(b, i, i, b)
term(38) = term(38) + tvoov(b, j, j, b)
term(39) = term(39) + tvoov(a, j, j, a)
term(40) = term(40) + tvoov(a, i, i, a)
term(41) = term(41) + toooo(j, j, i, i)
term(42) = term(42) + toooo(j, i, i, j)

term(27) = -term(27) 
term(28) = -term(28) 
term(31) = -term(31) 
term(37) = -term(37) 
term(38) = -term(38) 
term(39) = -term(39) 
term(40) = -term(40) 
term(41) = -term(41) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(43) = term(43) + t2(e,f,i,j) * tovov(j, e, i, f)
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(44) = term(44) + t2(b,e,m,j) * tovov(m, b, j, e)
term(45) = term(45) + t2(a,e,m,j) * tovov(m, a, j, e)
term(46) = term(46) + t2(b,e,j,m) * tovov(m, b, j, e)
term(47) = term(47) + t2(b,e,m,i) * tovov(m, b, i, e)
term(48) = term(48) + t2(a,e,m,i) * tovov(m, a, i, e)
term(49) = term(49) + t2(b,e,i,m) * tovov(m, b, i, e)
term(50) = term(50) + t2(b,e,m,j) * tovov(m, e, j, b)
term(51) = term(51) + t2(b,e,m,i) * tovov(m, e, i, b)
term(52) = term(52) + t2(a,e,j,m) * tovov(m, a, j, e)
term(53) = term(53) + t2(a,e,m,j) * tovov(m, e, j, a)
term(54) = term(54) + t2(a,e,i,m) * tovov(m, a, i, e)
term(55) = term(55) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(44) = -term(44) 
term(45) = -term(45) 
term(47) = -term(47) 
term(48) = -term(48) 

do e = nocc + 1, nactive 
term(56) = term(56) + t2(b,e,i,j) * tovov(j, e, i, b)
term(57) = term(57) + t2(b,e,j,i) * tovov(j, e, i, b)
term(58) = term(58) + t2(a,e,j,i) * tovov(j, e, i, a)
term(59) = term(59) + t2(b,e,i,j) * tovov(j, b, i, e)
term(60) = term(60) + t2(b,e,j,i) * tovov(j, b, i, e)
term(61) = term(61) + t2(a,e,j,i) * tovov(j, a, i, e)
term(62) = term(62) + t2(a,e,i,j) * tovov(j, e, i, a)
term(63) = term(63) + t2(a,e,i,j) * tovov(j, a, i, e)
end do 

term(57) = -term(57) 
term(58) = -term(58) 
term(59) = -term(59) 
term(63) = -term(63) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(64) = term(64) + t2(b,e,j,m) * tovov(m, e, j, b)
term(65) = term(65) + t2(b,e,i,m) * tovov(m, e, i, b)
term(66) = term(66) + t2(a,e,j,m) * tovov(m, e, j, a)
term(67) = term(67) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * (-2.0d+0) 
term(66) = term(66) * (-2.0d+0) 
term(67) = term(67) * (-2.0d+0) 


    eom_cc3_22_tripletpp_trans_aibjajbi = 0.d+0
    do s = 0, 67
    eom_cc3_22_tripletpp_trans_aibjajbi = eom_cc3_22_tripletpp_trans_aibjajbi + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjajbi
    function eom_cc3_22_tripletpp_trans_aibjaibj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpp_trans_aibjaibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e,f,n 
    double precision, dimension(0:67) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, i, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, j, b)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, b, j, a)
term(4) = term(4) + t2(a,b,m,j) * tovov(m, b, j, a)
term(5) = term(5) + t2(a,b,m,i) * tovov(m, b, i, a)
term(6) = term(6) + t2(a,b,i,m) * tovov(m, a, i, b)
term(7) = term(7) + t2(a,b,i,m) * tovov(m, b, i, a)
term(8) = term(8) + toooo(m, i, i, m)
term(9) = term(9) + toooo(m, m, i, i)
term(10) = term(10) + toooo(m, j, j, m)
term(11) = term(11) + toooo(m, m, j, j)
term(12) = term(12) + tvvoo(a, a, m, m)
term(13) = term(13) + tvvoo(b, b, m, m)
term(14) = term(14) + tvoov(a, m, m, a)
term(15) = term(15) + tvoov(b, m, m, b)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 
term(7) = -term(7) 
term(9) = term(9) * (-2.0d+0) 
term(11) = term(11) * (-2.0d+0) 
term(12) = term(12) * 2.0d+0 
term(13) = term(13) * 2.0d+0 
term(14) = -term(14) 
term(15) = -term(15) 

do n = 1, nocc 
do m = 1, nocc 
term(16) = term(16) + t2(a,b,m,n) * tovov(n, a, m, b)
term(17) = term(17) + t2(a,b,m,n) * tovov(n, b, m, a)
end do 
end do 

term(16) = -term(16) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(18) = term(18) + t2(e,f,i,m) * tovov(m, e, i, f)
term(19) = term(19) + t2(e,f,j,m) * tovov(m, e, j, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(20) = term(20) + t2(e,f,i,m) * tovov(m, f, i, e)
term(21) = term(21) + t2(e,f,j,m) * tovov(m, f, j, e)
end do 
end do 
end do 

term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (-2.0d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(22) = term(22) + t2(a,e,m,n) * tovov(n, e, m, a)
term(23) = term(23) + t2(b,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * (-2.0d+0) 

term(24) = term(24) + tvv(a, a)
term(25) = term(25) + tvv(b, b)
term(26) = term(26) + too(i, i)
term(27) = term(27) + too(j, j)
term(28) = term(28) + read_ftvvvv(b, a, a, b)
term(29) = term(29) + read_ftvvvv(b, b, a, a)
term(30) = term(30) + tvvoo(a, a, i, i)
term(31) = term(31) + tvvoo(a, a, j, j)
term(32) = term(32) + tvvoo(b, b, i, i)
term(33) = term(33) + tvvoo(b, b, j, j)
term(34) = term(34) + tvoov(b, i, i, b)
term(35) = term(35) + tvoov(b, j, j, b)
term(36) = term(36) + tvoov(a, j, j, a)
term(37) = term(37) + tvoov(a, i, i, a)
term(38) = term(38) + toooo(j, i, i, j)
term(39) = term(39) + toooo(j, j, i, i)

term(26) = -term(26) 
term(27) = -term(27) 
term(28) = -term(28) 
term(30) = -term(30) 
term(31) = -term(31) 
term(32) = -term(32) 
term(33) = -term(33) 
term(38) = -term(38) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(40) = term(40) + t2(a,e,m,n) * tovov(n, a, m, e)
term(41) = term(41) + t2(b,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(42) = term(42) + t2(e,f,i,j) * tovov(j, e, i, f)
end do 
end do 

term(42) = -term(42) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(43) = term(43) + t2(e,f,i,j) * tovov(j, f, i, e)
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(44) = term(44) + t2(b,e,m,i) * tovov(m, b, i, e)
term(45) = term(45) + t2(a,e,m,i) * tovov(m, a, i, e)
term(46) = term(46) + t2(b,e,i,m) * tovov(m, b, i, e)
term(47) = term(47) + t2(b,e,m,j) * tovov(m, b, j, e)
term(48) = term(48) + t2(a,e,m,j) * tovov(m, a, j, e)
term(49) = term(49) + t2(b,e,j,m) * tovov(m, b, j, e)
term(50) = term(50) + t2(a,e,j,m) * tovov(m, a, j, e)
term(51) = term(51) + t2(b,e,m,i) * tovov(m, e, i, b)
term(52) = term(52) + t2(b,e,m,j) * tovov(m, e, j, b)
term(53) = term(53) + t2(a,e,m,j) * tovov(m, e, j, a)
term(54) = term(54) + t2(a,e,i,m) * tovov(m, a, i, e)
term(55) = term(55) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(46) = -term(46) 
term(49) = -term(49) 
term(50) = -term(50) 
term(51) = -term(51) 
term(52) = -term(52) 
term(53) = -term(53) 
term(54) = -term(54) 
term(55) = -term(55) 

do e = nocc + 1, nactive 
term(56) = term(56) + t2(b,e,i,j) * tovov(j, b, i, e)
term(57) = term(57) + t2(b,e,j,i) * tovov(j, b, i, e)
term(58) = term(58) + t2(a,e,j,i) * tovov(j, a, i, e)
term(59) = term(59) + t2(b,e,i,j) * tovov(j, e, i, b)
term(60) = term(60) + t2(b,e,j,i) * tovov(j, e, i, b)
term(61) = term(61) + t2(a,e,j,i) * tovov(j, e, i, a)
term(62) = term(62) + t2(a,e,i,j) * tovov(j, a, i, e)
term(63) = term(63) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 

term(57) = -term(57) 
term(58) = -term(58) 
term(59) = -term(59) 
term(63) = -term(63) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(64) = term(64) + t2(b,e,i,m) * tovov(m, e, i, b)
term(65) = term(65) + t2(b,e,j,m) * tovov(m, e, j, b)
term(66) = term(66) + t2(a,e,j,m) * tovov(m, e, j, a)
term(67) = term(67) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(64) = term(64) * 2.0d+0 
term(65) = term(65) * 2.0d+0 
term(66) = term(66) * 2.0d+0 
term(67) = term(67) * 2.0d+0 


    eom_cc3_22_tripletpp_trans_aibjaibj = 0.d+0
    do s = 0, 67
    eom_cc3_22_tripletpp_trans_aibjaibj = eom_cc3_22_tripletpp_trans_aibjaibj + term(s)
    end do

    end function eom_cc3_22_tripletpp_trans_aibjaibj
    end module eom_cc3_22_tripletpp_trans
    
