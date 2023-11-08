module eom_cc3_22_tripletmm_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_cc3_22_tripletmm_trans_aibjckbl(t2, nocc, nactive, a, i, j, c, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjckbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, e, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibjckbl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjckbl = eom_cc3_22_tripletmm_trans_aibjckbl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjckbl
    function eom_cc3_22_tripletmm_trans_aibjbkdl(t2, nocc, nactive, a, i, j, k, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbkdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, d, k, e)
end do 



    eom_cc3_22_tripletmm_trans_aibjbkdl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjbkdl = eom_cc3_22_tripletmm_trans_aibjbkdl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbkdl
    function eom_cc3_22_tripletmm_trans_aibjckal(t2, nocc, nactive, i, b, j, c, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjckal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k, l 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, e, k, c)
end do 



    eom_cc3_22_tripletmm_trans_aibjckal = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjckal = eom_cc3_22_tripletmm_trans_aibjckal + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjckal
    function eom_cc3_22_tripletmm_trans_aibjakdl(t2, nocc, nactive, i, b, j, k, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjakdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, d, k, e)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibjakdl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjakdl = eom_cc3_22_tripletmm_trans_aibjakdl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjakdl
    function eom_cc3_22_tripletmm_trans_aibjcjdl(t2, nocc, nactive, a, i, b, c, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcjdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d, l 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, l, d)
end do 



    eom_cc3_22_tripletmm_trans_aibjcjdl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjcjdl = eom_cc3_22_tripletmm_trans_aibjcjdl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcjdl
    function eom_cc3_22_tripletmm_trans_aibjckdj(t2, nocc, nactive, a, i, b, c, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjckdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, d, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibjckdj = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjckdj = eom_cc3_22_tripletmm_trans_aibjckdj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjckdj
    function eom_cc3_22_tripletmm_trans_aibjcidl(t2, nocc, nactive, a, b, j, c, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d, l 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, d)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibjcidl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjcidl = eom_cc3_22_tripletmm_trans_aibjcidl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcidl
    function eom_cc3_22_tripletmm_trans_aibjckdi(t2, nocc, nactive, a, b, j, c, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjckdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k, d 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, d, k, c)
end do 



    eom_cc3_22_tripletmm_trans_aibjckdi = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjckdi = eom_cc3_22_tripletmm_trans_aibjckdi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjckdi
    function eom_cc3_22_tripletmm_trans_aibjbkbl(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbkbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, b, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, k, b)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmm_trans_aibjbkbl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibjbkbl = eom_cc3_22_tripletmm_trans_aibjbkbl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbkbl
    function eom_cc3_22_tripletmm_trans_aiajckal(t2, nocc, nactive, a, i, j, c, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajckal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, e, k, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, k, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmm_trans_aiajckal = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aiajckal = eom_cc3_22_tripletmm_trans_aiajckal + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajckal
    function eom_cc3_22_tripletmm_trans_aibjakbl(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjakbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e,f 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, j, k, i)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,j,i) * tovov(l, b, k, e)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, e, k, a)
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(l, f, k, e)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibjakbl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibjakbl = eom_cc3_22_tripletmm_trans_aibjakbl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjakbl
    function eom_cc3_22_tripletmm_trans_aibickbl(t2, nocc, nactive, a, i, c, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibickbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, l 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(l, e, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibickbl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibickbl = eom_cc3_22_tripletmm_trans_aibickbl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibickbl
    function eom_cc3_22_tripletmm_trans_aibjcjbl(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcjbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, l, b)
end do 


do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, j, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmm_trans_aibjcjbl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibjcjbl = eom_cc3_22_tripletmm_trans_aibjcjbl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcjbl
    function eom_cc3_22_tripletmm_trans_aibjckbj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjckbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, k, i)
term(1) = term(1) + tvoov(a, i, k, c)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, k, c)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,j) * tovov(k, c, j, e)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, c, k, e)
term(5) = term(5) + t2(a,e,i,m) * tovov(m, c, k, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibjckbj = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aibjckbj = eom_cc3_22_tripletmm_trans_aibjckbj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjckbj
    function eom_cc3_22_tripletmm_trans_aibjcibl(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, l, j)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, l, b)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(l, e, i, c)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,j) * tovov(m, c, l, e)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibjcibl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibjcibl = eom_cc3_22_tripletmm_trans_aibjcibl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcibl
    function eom_cc3_22_tripletmm_trans_aibjckbi(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjckbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, k, c)
end do 


do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,j) * tovov(k, c, i, e)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmm_trans_aibjckbi = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibjckbi = eom_cc3_22_tripletmm_trans_aibjckbi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjckbi
    function eom_cc3_22_tripletmm_trans_aibjckbk(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjckbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibjckbk = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjckbk = eom_cc3_22_tripletmm_trans_aibjckbk + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjckbk
    function eom_cc3_22_tripletmm_trans_aiajakdl(t2, nocc, nactive, a, i, j, k, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajakdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, d, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, d, k, e)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aiajakdl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aiajakdl = eom_cc3_22_tripletmm_trans_aiajakdl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajakdl
    function eom_cc3_22_tripletmm_trans_aibjbkal(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbkal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e,f 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, i, k, j)

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, b)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, a, k, e)
end do 


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(l, e, k, f)
end do 
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletmm_trans_aibjbkal = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibjbkal = eom_cc3_22_tripletmm_trans_aibjbkal + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbkal
    function eom_cc3_22_tripletmm_trans_aibibkdl(t2, nocc, nactive, a, i, k, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibibkdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(l, d, k, e)
end do 



    eom_cc3_22_tripletmm_trans_aibibkdl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibibkdl = eom_cc3_22_tripletmm_trans_aibibkdl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibibkdl
    function eom_cc3_22_tripletmm_trans_aibjbjdl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbjdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, l, i)
term(1) = term(1) + tvoov(a, i, l, d)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, l, d)
end do 


do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,j) * tovov(l, d, j, e)
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, d, l, e)
term(5) = term(5) + t2(a,e,i,m) * tovov(m, d, l, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibjbjdl = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aibjbjdl = eom_cc3_22_tripletmm_trans_aibjbjdl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbjdl
    function eom_cc3_22_tripletmm_trans_aibjbkdj(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbkdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, d, k, b)
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, j, d)
end do 



    eom_cc3_22_tripletmm_trans_aibjbkdj = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibjbkdj = eom_cc3_22_tripletmm_trans_aibjbkdj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbkdj
    function eom_cc3_22_tripletmm_trans_aibjbidl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, l, d)
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,j) * tovov(l, d, i, e)
end do 



    eom_cc3_22_tripletmm_trans_aibjbidl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibjbidl = eom_cc3_22_tripletmm_trans_aibjbidl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbidl
    function eom_cc3_22_tripletmm_trans_aibjbkdi(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbkdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, k, j)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, k, b)
end do 


do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, i, d)
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,j) * tovov(m, d, k, e)
end do 
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletmm_trans_aibjbkdi = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibjbkdi = eom_cc3_22_tripletmm_trans_aibjbkdi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbkdi
    function eom_cc3_22_tripletmm_trans_aibjbkdk(t2, nocc, nactive, a, i, j, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbkdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, k, d)
end do 



    eom_cc3_22_tripletmm_trans_aibjbkdk = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjbkdk = eom_cc3_22_tripletmm_trans_aibjbkdk + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbkdk
    function eom_cc3_22_tripletmm_trans_aiajcjdl(t2, nocc, nactive, a, i, c, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcjdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, d, l 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, l, d)
end do 



    eom_cc3_22_tripletmm_trans_aiajcjdl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aiajcjdl = eom_cc3_22_tripletmm_trans_aiajcjdl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcjdl
    function eom_cc3_22_tripletmm_trans_aiajckdj(t2, nocc, nactive, a, i, c, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajckdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, d 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, d, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aiajckdj = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aiajckdj = eom_cc3_22_tripletmm_trans_aiajckdj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajckdj
    function eom_cc3_22_tripletmm_trans_aiajcidl(t2, nocc, nactive, a, j, c, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, d, l 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, l, d)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aiajcidl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aiajcidl = eom_cc3_22_tripletmm_trans_aiajcidl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcidl
    function eom_cc3_22_tripletmm_trans_aiajckdi(t2, nocc, nactive, a, j, c, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajckdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k, d 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, d, k, c)
end do 



    eom_cc3_22_tripletmm_trans_aiajckdi = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aiajckdi = eom_cc3_22_tripletmm_trans_aiajckdi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajckdi
    function eom_cc3_22_tripletmm_trans_aibjakal(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjakal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, a, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, a)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibjakal = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibjakal = eom_cc3_22_tripletmm_trans_aibjakal + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjakal
    function eom_cc3_22_tripletmm_trans_aibjcjcl(t2, nocc, nactive, a, i, b, c, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcjcl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, l, c)
end do 



    eom_cc3_22_tripletmm_trans_aibjcjcl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjcjcl = eom_cc3_22_tripletmm_trans_aibjcjcl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcjcl
    function eom_cc3_22_tripletmm_trans_aibjckcj(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjckcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibjckcj = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjckcj = eom_cc3_22_tripletmm_trans_aibjckcj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjckcj
    function eom_cc3_22_tripletmm_trans_aibjcicl(t2, nocc, nactive, a, b, j, c, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcicl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, l 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibjcicl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjcicl = eom_cc3_22_tripletmm_trans_aibjcicl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcicl
    function eom_cc3_22_tripletmm_trans_aibjckci(t2, nocc, nactive, a, b, j, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjckci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, k, c)
end do 



    eom_cc3_22_tripletmm_trans_aibjckci = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjckci = eom_cc3_22_tripletmm_trans_aibjckci + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjckci
    function eom_cc3_22_tripletmm_trans_aibickal(t2, nocc, nactive, i, b, c, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibickal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, k, l 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(l, e, k, c)
end do 



    eom_cc3_22_tripletmm_trans_aibickal = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibickal = eom_cc3_22_tripletmm_trans_aibickal + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibickal
    function eom_cc3_22_tripletmm_trans_aibjcjal(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcjal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, l, i)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, a)
end do 


do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(l, e, j, c)
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,i) * tovov(m, c, l, e)
end do 
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletmm_trans_aibjcjal = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibjcjal = eom_cc3_22_tripletmm_trans_aibjcjal + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcjal
    function eom_cc3_22_tripletmm_trans_aibjckaj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjckaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, k, c)
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,j,i) * tovov(k, c, j, e)
end do 



    eom_cc3_22_tripletmm_trans_aibjckaj = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibjckaj = eom_cc3_22_tripletmm_trans_aibjckaj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjckaj
    function eom_cc3_22_tripletmm_trans_aibjcial(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, a)
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, i, c)
end do 



    eom_cc3_22_tripletmm_trans_aibjcial = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibjcial = eom_cc3_22_tripletmm_trans_aibjcial + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcial
    function eom_cc3_22_tripletmm_trans_aibjckai(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjckai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, k, j)
term(1) = term(1) + tvoov(b, j, k, c)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, k, c)
end do 


do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,i) * tovov(k, c, i, e)
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,j) * tovov(m, c, k, e)
term(5) = term(5) + t2(b,e,j,m) * tovov(m, c, k, e)
term(6) = term(6) + t2(b,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibjckai = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aibjckai = eom_cc3_22_tripletmm_trans_aibjckai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjckai
    function eom_cc3_22_tripletmm_trans_aibjckak(t2, nocc, nactive, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjckak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, k, c)
end do 



    eom_cc3_22_tripletmm_trans_aibjckak = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjckak = eom_cc3_22_tripletmm_trans_aibjckak + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjckak
    function eom_cc3_22_tripletmm_trans_aibiakdl(t2, nocc, nactive, i, b, k, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiakdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(l, d, k, e)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibiakdl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibiakdl = eom_cc3_22_tripletmm_trans_aibiakdl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiakdl
    function eom_cc3_22_tripletmm_trans_aibjajdl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjajdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, l, d)
end do 


do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,j,i) * tovov(l, d, j, e)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmm_trans_aibjajdl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibjajdl = eom_cc3_22_tripletmm_trans_aibjajdl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjajdl
    function eom_cc3_22_tripletmm_trans_aibjakdj(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjakdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, k, i)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, k, a)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(k, e, j, d)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,i) * tovov(m, d, k, e)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibjakdj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibjakdj = eom_cc3_22_tripletmm_trans_aibjakdj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjakdj
    function eom_cc3_22_tripletmm_trans_aibjaidl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjaidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, l, j)
term(1) = term(1) + tvoov(b, j, l, d)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, l, d)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,i) * tovov(l, d, i, e)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,j) * tovov(m, d, l, e)
term(5) = term(5) + t2(b,e,j,m) * tovov(m, d, l, e)
term(6) = term(6) + t2(b,e,m,j) * tovov(m, e, l, d)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibjaidl = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aibjaidl = eom_cc3_22_tripletmm_trans_aibjaidl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjaidl
    function eom_cc3_22_tripletmm_trans_aibjakdi(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjakdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, d, k, a)
end do 


do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, i, d)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmm_trans_aibjakdi = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibjakdi = eom_cc3_22_tripletmm_trans_aibjakdi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjakdi
    function eom_cc3_22_tripletmm_trans_aibjakdk(t2, nocc, nactive, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjakdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, k, d)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibjakdk = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibjakdk = eom_cc3_22_tripletmm_trans_aibjakdk + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjakdk
    function eom_cc3_22_tripletmm_trans_aibicidl(t2, nocc, nactive, a, i, b, c, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibicidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d, l 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, d)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibicidl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibicidl = eom_cc3_22_tripletmm_trans_aibicidl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibicidl
    function eom_cc3_22_tripletmm_trans_aibickdi(t2, nocc, nactive, a, i, b, c, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibickdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, d, k, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, k, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmm_trans_aibickdi = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibickdi = eom_cc3_22_tripletmm_trans_aibickdi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibickdi
    function eom_cc3_22_tripletmm_trans_aibjcjdj(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcjdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, j, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmm_trans_aibjcjdj = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibjcjdj = eom_cc3_22_tripletmm_trans_aibjcjdj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcjdj
    function eom_cc3_22_tripletmm_trans_aibjcjdi(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcjdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m,n 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, c, a, d)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, i, d)
end do 


do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,n) * tovov(n, c, m, d)
end do 
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletmm_trans_aibjcjdi = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibjcjdi = eom_cc3_22_tripletmm_trans_aibjcjdi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcjdi
    function eom_cc3_22_tripletmm_trans_aibjcidj(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m,n 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, d, a, c)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, j, d)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,n) * tovov(n, d, m, c)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibjcidj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibjcidj = eom_cc3_22_tripletmm_trans_aibjcidj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcidj
    function eom_cc3_22_tripletmm_trans_aibjcidi(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, i, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibjcidi = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibjcidi = eom_cc3_22_tripletmm_trans_aibjcidi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcidi
    function eom_cc3_22_tripletmm_trans_aiajakal(t2, nocc, nactive, a, i, j, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajakal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, l 
    integer :: s ,e,f 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, j, k, i)
term(1) = term(1) + toooo(l, i, k, j)

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(l, a, k, e)
term(3) = term(3) + t2(a,e,j,i) * tovov(l, e, k, a)
term(4) = term(4) + t2(a,e,i,j) * tovov(l, a, k, e)
term(5) = term(5) + t2(a,e,i,j) * tovov(l, e, k, a)
end do 

term(2) = -term(2) 
term(5) = -term(5) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(6) = term(6) + t2(e,f,i,j) * tovov(l, f, k, e)
end do 
end do 


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(e,f,i,j) * tovov(l, e, k, f)
end do 
end do 

term(7) = -term(7) 


    eom_cc3_22_tripletmm_trans_aiajakal = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aiajakal = eom_cc3_22_tripletmm_trans_aiajakal + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajakal
    function eom_cc3_22_tripletmm_trans_aibibkbl(t2, nocc, nactive, a, i, b, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibibkbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(l, b, k, e)
term(1) = term(1) + t2(a,e,i,i) * tovov(l, e, k, b)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmm_trans_aibibkbl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibibkbl = eom_cc3_22_tripletmm_trans_aibibkbl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibibkbl
    function eom_cc3_22_tripletmm_trans_aibjbjbl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbjbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, l, i)
term(1) = term(1) + tvoov(a, i, l, b)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, l, b)
end do 


do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,j) * tovov(l, b, j, e)
term(4) = term(4) + t2(a,e,i,j) * tovov(l, e, j, b)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, b, l, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, b, l, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, l, b)
end do 
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, l, b)
end do 
end do 

term(8) = term(8) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibjbjbl = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aibjbjbl = eom_cc3_22_tripletmm_trans_aibjbjbl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbjbl
    function eom_cc3_22_tripletmm_trans_aibjbkbj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbkbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, k, i)
term(1) = term(1) + tvoov(a, i, k, b)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, k, b)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,j) * tovov(k, e, j, b)
term(4) = term(4) + t2(a,e,i,j) * tovov(k, b, j, e)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, b, k, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, b, k, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, k, b)
end do 
end do 

term(6) = -term(6) 
term(7) = -term(7) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, k, b)
end do 
end do 

term(8) = term(8) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibjbkbj = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aibjbkbj = eom_cc3_22_tripletmm_trans_aibjbkbj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbkbj
    function eom_cc3_22_tripletmm_trans_aibjbibl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, l, j)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, l, b)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(l, b, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, i, b)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,j) * tovov(m, b, l, e)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibjbibl = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjbibl = eom_cc3_22_tripletmm_trans_aibjbibl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbibl
    function eom_cc3_22_tripletmm_trans_aibjbkbi(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbkbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, k, j)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, k, b)
end do 


do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, i, b)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, b, i, e)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,j) * tovov(m, b, k, e)
end do 
end do 

term(4) = -term(4) 


    eom_cc3_22_tripletmm_trans_aibjbkbi = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjbkbi = eom_cc3_22_tripletmm_trans_aibjbkbi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbkbi
    function eom_cc3_22_tripletmm_trans_aiajcjal(t2, nocc, nactive, a, i, j, c, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcjal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, l, i)


do m = 1, nocc 
term(1) = term(1) + t2(a,a,i,m) * tovov(m, c, l, a)
end do 


do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, j, c)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, j, c)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, c, l, e)
end do 
end do 

term(4) = -term(4) 


    eom_cc3_22_tripletmm_trans_aiajcjal = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aiajcjal = eom_cc3_22_tripletmm_trans_aiajcjal + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcjal
    function eom_cc3_22_tripletmm_trans_aiajckaj(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajckaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, k, i)
term(1) = term(1) + tvoov(a, i, k, c)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, k, c)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(k, c, j, e)
term(4) = term(4) + t2(a,e,i,j) * tovov(k, c, j, e)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, c, k, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, c, k, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(6) = -term(6) 
term(7) = -term(7) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(8) = term(8) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aiajckaj = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aiajckaj = eom_cc3_22_tripletmm_trans_aiajckaj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajckaj
    function eom_cc3_22_tripletmm_trans_aiajcial(t2, nocc, nactive, a, i, j, c, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, l, j)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,a,j,m) * tovov(m, c, l, a)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, i, c)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, i, c)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,j) * tovov(m, c, l, e)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aiajcial = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aiajcial = eom_cc3_22_tripletmm_trans_aiajcial + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcial
    function eom_cc3_22_tripletmm_trans_aiajckai(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajckai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, k, j)
term(1) = term(1) + tvoov(a, j, k, c)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, k, c)
end do 


do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(k, c, i, e)
term(4) = term(4) + t2(a,e,i,j) * tovov(k, c, i, e)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,j) * tovov(m, c, k, e)
term(6) = term(6) + t2(a,e,j,m) * tovov(m, c, k, e)
term(7) = term(7) + t2(a,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(8) = term(8) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aiajckai = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aiajckai = eom_cc3_22_tripletmm_trans_aiajckai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajckai
    function eom_cc3_22_tripletmm_trans_aiajckak(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajckak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, k, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, k, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmm_trans_aiajckak = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aiajckak = eom_cc3_22_tripletmm_trans_aiajckak + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajckak
    function eom_cc3_22_tripletmm_trans_aibiakbl(t2, nocc, nactive, a, i, b, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiakbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, l 
    integer :: s ,e,f 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,i,i) * tovov(l, f, k, e)
end do 
end do 


do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,i) * tovov(l, b, k, e)
term(2) = term(2) + t2(a,e,i,i) * tovov(l, e, k, a)
end do 

term(1) = -term(1) 
term(2) = -term(2) 

term(3) = term(3) + toooo(l, i, k, i)



    eom_cc3_22_tripletmm_trans_aibiakbl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibiakbl = eom_cc3_22_tripletmm_trans_aibiakbl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiakbl
    function eom_cc3_22_tripletmm_trans_aibjajbl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjajbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,m,e,f 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, l, b)
end do 


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,j) * tovov(l, f, j, e)
end do 
end do 


do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(l, b, j, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, j, a)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

term(4) = term(4) + toooo(l, j, j, i)



    eom_cc3_22_tripletmm_trans_aibjajbl = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjajbl = eom_cc3_22_tripletmm_trans_aibjajbl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjajbl
    function eom_cc3_22_tripletmm_trans_aibjakbj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjakbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,f,m,e 
    double precision, dimension(0:17) :: term 
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
term(4) = term(4) + tvoov(a, i, k, a)
term(5) = term(5) + toooo(k, i, j, j)
term(6) = term(6) + too(k, i)

term(2) = -term(2) 
term(3) = -term(3) 
term(6) = -term(6) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,m,i) * tovov(m, b, k, e)
term(8) = term(8) + t2(a,e,m,i) * tovov(m, a, k, e)
term(9) = term(9) + t2(a,e,i,m) * tovov(m, a, k, e)
term(10) = term(10) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(9) = -term(9) 
term(10) = -term(10) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(11) = term(11) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(11) = term(11) * 2.0d+0 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(e,f,i,j) * tovov(k, e, j, f)
end do 
end do 


do e = nocc + 1, nactive 
term(13) = term(13) + t2(b,e,j,i) * tovov(k, e, j, b)
term(14) = term(14) + t2(a,e,i,j) * tovov(k, a, j, e)
end do 

term(13) = -term(13) 
term(14) = -term(14) 

do m = 1, nocc 
term(15) = term(15) + toooo(m, i, k, m)
term(16) = term(16) + toooo(m, m, k, i)
term(17) = term(17) + t2(a,b,i,m) * tovov(m, b, k, a)
end do 

term(16) = term(16) * (-2.0d+0) 
term(17) = -term(17) 


    eom_cc3_22_tripletmm_trans_aibjakbj = 0.d+0
    do s = 0, 17
    eom_cc3_22_tripletmm_trans_aibjakbj = eom_cc3_22_tripletmm_trans_aibjakbj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjakbj
    function eom_cc3_22_tripletmm_trans_aibjaibl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjaibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,f,m,e 
    double precision, dimension(0:17) :: term 
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
term(5) = term(5) + toooo(l, j, i, i)
term(6) = term(6) + too(l, j)

term(2) = -term(2) 
term(3) = -term(3) 
term(6) = -term(6) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,m,j) * tovov(m, b, l, e)
term(8) = term(8) + t2(a,e,m,j) * tovov(m, a, l, e)
term(9) = term(9) + t2(b,e,j,m) * tovov(m, b, l, e)
term(10) = term(10) + t2(b,e,m,j) * tovov(m, e, l, b)
end do 
end do 

term(9) = -term(9) 
term(10) = -term(10) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(11) = term(11) + t2(b,e,j,m) * tovov(m, e, l, b)
end do 
end do 

term(11) = term(11) * 2.0d+0 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(12) = term(12) + t2(e,f,i,j) * tovov(l, f, i, e)
end do 
end do 


do e = nocc + 1, nactive 
term(13) = term(13) + t2(b,e,j,i) * tovov(l, b, i, e)
term(14) = term(14) + t2(a,e,i,j) * tovov(l, e, i, a)
end do 

term(13) = -term(13) 
term(14) = -term(14) 

do m = 1, nocc 
term(15) = term(15) + toooo(m, j, l, m)
term(16) = term(16) + toooo(m, m, l, j)
term(17) = term(17) + t2(a,b,m,j) * tovov(m, a, l, b)
end do 

term(16) = term(16) * (-2.0d+0) 
term(17) = -term(17) 


    eom_cc3_22_tripletmm_trans_aibjaibl = 0.d+0
    do s = 0, 17
    eom_cc3_22_tripletmm_trans_aibjaibl = eom_cc3_22_tripletmm_trans_aibjaibl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjaibl
    function eom_cc3_22_tripletmm_trans_aibjakbi(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjakbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,m,f,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, k, a)
end do 


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,j) * tovov(k, e, i, f)
end do 
end do 


do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(k, e, i, b)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, a, i, e)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

term(4) = term(4) + toooo(k, i, i, j)



    eom_cc3_22_tripletmm_trans_aibjakbi = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjakbi = eom_cc3_22_tripletmm_trans_aibjakbi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjakbi
    function eom_cc3_22_tripletmm_trans_aibjakbk(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjakbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,f 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,i,j) * tovov(k, f, k, e)
end do 
end do 


do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, k, b)
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, k, a)
end do 

term(1) = -term(1) 
term(2) = -term(2) 

term(3) = term(3) + toooo(k, j, k, i)



    eom_cc3_22_tripletmm_trans_aibjakbk = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibjakbk = eom_cc3_22_tripletmm_trans_aibjakbk + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjakbk
    function eom_cc3_22_tripletmm_trans_aibicibl(t2, nocc, nactive, a, i, b, c, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibicibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, l, i)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, l, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, l, b)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,i) * tovov(l, e, i, c)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, c, l, e)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibicibl = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibicibl = eom_cc3_22_tripletmm_trans_aibicibl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibicibl
    function eom_cc3_22_tripletmm_trans_aibickbi(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibickbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, k, i)
term(1) = term(1) + tvoov(a, i, k, c)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, k, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, k, c)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(a,e,i,i) * tovov(k, c, i, e)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, c, k, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, c, k, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(6) = -term(6) 
term(7) = -term(7) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(8) = term(8) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibickbi = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aibickbi = eom_cc3_22_tripletmm_trans_aibickbi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibickbi
    function eom_cc3_22_tripletmm_trans_aibickbk(t2, nocc, nactive, a, i, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibickbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(k, e, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibickbk = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibickbk = eom_cc3_22_tripletmm_trans_aibickbk + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibickbk
    function eom_cc3_22_tripletmm_trans_aibjcjbj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcjbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, j, i)
term(1) = term(1) + tvoov(a, i, j, c)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, j, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, j, c)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(a,e,i,j) * tovov(j, e, j, c)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, c, j, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, c, j, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, j, c)
end do 
end do 

term(6) = -term(6) 
term(7) = -term(7) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, j, c)
end do 
end do 

term(8) = term(8) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibjcjbj = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aibjcjbj = eom_cc3_22_tripletmm_trans_aibjcjbj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcjbj
    function eom_cc3_22_tripletmm_trans_aibjcjbi(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcjbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e,n 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, i, b)
end do 


do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(j, c, i, e)
end do 

term(2) = -term(2) 

do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,n) * tovov(n, c, m, b)
end do 
end do 

term(3) = -term(3) 

term(4) = term(4) + read_ftvvvv(b, c, a, b)

term(4) = -term(4) 


    eom_cc3_22_tripletmm_trans_aibjcjbi = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjcjbi = eom_cc3_22_tripletmm_trans_aibjcjbi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcjbi
    function eom_cc3_22_tripletmm_trans_aibjcibj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e,n 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, c)
term(2) = term(2) + tvvoo(a, c, m, m)
term(3) = term(3) + tvoov(a, m, m, c)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, c, i, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, c, j, e)
term(7) = term(7) + t2(a,e,i,m) * tovov(m, c, i, e)
term(8) = term(8) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(7) = -term(7) 
term(8) = -term(8) 

do n = 1, nocc 
do m = 1, nocc 
term(9) = term(9) + t2(a,b,m,n) * tovov(n, b, m, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(10) = term(10) * 2.0d+0 

term(11) = term(11) + read_ftvvvv(b, b, a, c)
term(12) = term(12) + tvv(a, c)
term(13) = term(13) + tvvoo(a, c, i, i)
term(14) = term(14) + tvvoo(a, c, j, j)
term(15) = term(15) + tvoov(a, i, i, c)

term(13) = -term(13) 
term(14) = -term(14) 

do e = nocc + 1, nactive 
term(16) = term(16) + t2(a,e,i,j) * tovov(j, e, i, c)
end do 

term(16) = -term(16) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(a,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(17) = term(17) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibjcibj = 0.d+0
    do s = 0, 17
    eom_cc3_22_tripletmm_trans_aibjcibj = eom_cc3_22_tripletmm_trans_aibjcibj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcibj
    function eom_cc3_22_tripletmm_trans_aibjcibi(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, i, j)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, i, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, i, c)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,j) * tovov(i, e, i, c)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,j) * tovov(m, c, i, e)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibjcibi = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjcibi = eom_cc3_22_tripletmm_trans_aibjcibi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcibi
    function eom_cc3_22_tripletmm_trans_aiajajdl(t2, nocc, nactive, a, i, j, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajajdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, l, i)
term(1) = term(1) + tvoov(a, i, l, d)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, l, d)
end do 


do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(l, d, j, e)
term(4) = term(4) + t2(a,e,i,j) * tovov(l, d, j, e)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, d, l, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, d, l, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(8) = term(8) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aiajajdl = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aiajajdl = eom_cc3_22_tripletmm_trans_aiajajdl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajajdl
    function eom_cc3_22_tripletmm_trans_aiajakdj(t2, nocc, nactive, a, i, j, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajakdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, k, i)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,a,i,m) * tovov(m, d, k, a)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(k, e, j, d)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, e, j, d)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, d, k, e)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aiajakdj = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aiajakdj = eom_cc3_22_tripletmm_trans_aiajakdj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajakdj
    function eom_cc3_22_tripletmm_trans_aiajaidl(t2, nocc, nactive, a, i, j, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajaidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, l, j)
term(1) = term(1) + tvoov(a, j, l, d)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, l, d)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(l, d, i, e)
term(4) = term(4) + t2(a,e,i,j) * tovov(l, d, i, e)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,j) * tovov(m, d, l, e)
term(6) = term(6) + t2(a,e,j,m) * tovov(m, d, l, e)
term(7) = term(7) + t2(a,e,m,j) * tovov(m, e, l, d)
end do 
end do 

term(6) = -term(6) 
term(7) = -term(7) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(8) = term(8) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aiajaidl = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aiajaidl = eom_cc3_22_tripletmm_trans_aiajaidl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajaidl
    function eom_cc3_22_tripletmm_trans_aiajakdi(t2, nocc, nactive, a, i, j, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajakdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, k, j)


do m = 1, nocc 
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, k, a)
end do 


do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(k, e, i, d)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, e, i, d)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,j) * tovov(m, d, k, e)
end do 
end do 

term(4) = -term(4) 


    eom_cc3_22_tripletmm_trans_aiajakdi = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aiajakdi = eom_cc3_22_tripletmm_trans_aiajakdi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajakdi
    function eom_cc3_22_tripletmm_trans_aiajakdk(t2, nocc, nactive, a, i, j, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajakdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, k, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, k, d)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aiajakdk = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aiajakdk = eom_cc3_22_tripletmm_trans_aiajakdk + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajakdk
    function eom_cc3_22_tripletmm_trans_aibibkal(t2, nocc, nactive, a, i, b, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibibkal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, l 
    integer :: s ,f,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,i,i) * tovov(l, e, k, f)
end do 
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,i) * tovov(l, e, k, b)
term(2) = term(2) + t2(a,e,i,i) * tovov(l, a, k, e)
end do 


term(3) = term(3) + toooo(l, i, k, i)

term(3) = -term(3) 


    eom_cc3_22_tripletmm_trans_aibibkal = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibibkal = eom_cc3_22_tripletmm_trans_aibibkal + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibibkal
    function eom_cc3_22_tripletmm_trans_aibjbjal(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbjal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,f,m,e 
    double precision, dimension(0:17) :: term 
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
term(4) = term(4) + tvoov(a, i, l, a)
term(5) = term(5) + toooo(l, i, j, j)
term(6) = term(6) + too(l, i)

term(4) = -term(4) 
term(5) = -term(5) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,m,i) * tovov(m, b, l, e)
term(8) = term(8) + t2(a,e,m,i) * tovov(m, a, l, e)
term(9) = term(9) + t2(a,e,i,m) * tovov(m, a, l, e)
term(10) = term(10) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 

term(7) = -term(7) 
term(8) = -term(8) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(11) = term(11) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(11) = term(11) * (-2.0d+0) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(e,f,i,j) * tovov(l, e, j, f)
end do 
end do 

term(12) = -term(12) 

do e = nocc + 1, nactive 
term(13) = term(13) + t2(b,e,j,i) * tovov(l, e, j, b)
term(14) = term(14) + t2(a,e,i,j) * tovov(l, a, j, e)
end do 


do m = 1, nocc 
term(15) = term(15) + toooo(m, i, l, m)
term(16) = term(16) + toooo(m, m, l, i)
term(17) = term(17) + t2(a,b,i,m) * tovov(m, b, l, a)
end do 

term(15) = -term(15) 
term(16) = term(16) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibjbjal = 0.d+0
    do s = 0, 17
    eom_cc3_22_tripletmm_trans_aibjbjal = eom_cc3_22_tripletmm_trans_aibjbjal + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbjal
    function eom_cc3_22_tripletmm_trans_aibjbkaj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbkaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,m,e,f 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, k, b)
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,j) * tovov(k, f, j, e)
end do 
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(k, b, j, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, e, j, a)
end do 


term(4) = term(4) + toooo(k, j, j, i)

term(4) = -term(4) 


    eom_cc3_22_tripletmm_trans_aibjbkaj = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjbkaj = eom_cc3_22_tripletmm_trans_aibjbkaj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbkaj
    function eom_cc3_22_tripletmm_trans_aibjbial(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,m,f,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, l, a)
end do 

term(0) = -term(0) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,j) * tovov(l, e, i, f)
end do 
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(l, e, i, b)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, a, i, e)
end do 


term(4) = term(4) + toooo(l, i, i, j)

term(4) = -term(4) 


    eom_cc3_22_tripletmm_trans_aibjbial = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjbial = eom_cc3_22_tripletmm_trans_aibjbial + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbial
    function eom_cc3_22_tripletmm_trans_aibjbkai(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbkai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,f,m,e 
    double precision, dimension(0:17) :: term 
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
term(5) = term(5) + toooo(k, j, i, i)
term(6) = term(6) + too(k, j)

term(4) = -term(4) 
term(5) = -term(5) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,m,j) * tovov(m, b, k, e)
term(8) = term(8) + t2(a,e,m,j) * tovov(m, a, k, e)
term(9) = term(9) + t2(b,e,j,m) * tovov(m, b, k, e)
term(10) = term(10) + t2(b,e,m,j) * tovov(m, e, k, b)
end do 
end do 

term(7) = -term(7) 
term(8) = -term(8) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(11) = term(11) + t2(b,e,j,m) * tovov(m, e, k, b)
end do 
end do 

term(11) = term(11) * (-2.0d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(12) = term(12) + t2(e,f,i,j) * tovov(k, f, i, e)
end do 
end do 

term(12) = -term(12) 

do e = nocc + 1, nactive 
term(13) = term(13) + t2(b,e,j,i) * tovov(k, b, i, e)
term(14) = term(14) + t2(a,e,i,j) * tovov(k, e, i, a)
end do 


do m = 1, nocc 
term(15) = term(15) + toooo(m, j, k, m)
term(16) = term(16) + toooo(m, m, k, j)
term(17) = term(17) + t2(a,b,m,j) * tovov(m, a, k, b)
end do 

term(15) = -term(15) 
term(16) = term(16) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibjbkai = 0.d+0
    do s = 0, 17
    eom_cc3_22_tripletmm_trans_aibjbkai = eom_cc3_22_tripletmm_trans_aibjbkai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbkai
    function eom_cc3_22_tripletmm_trans_aibjbkak(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbkak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,f 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,i,j) * tovov(k, f, k, e)
end do 
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, k, b)
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, k, a)
end do 


term(3) = term(3) + toooo(k, j, k, i)

term(3) = -term(3) 


    eom_cc3_22_tripletmm_trans_aibjbkak = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibjbkak = eom_cc3_22_tripletmm_trans_aibjbkak + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbkak
    function eom_cc3_22_tripletmm_trans_aibibidl(t2, nocc, nactive, a, i, b, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibibidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, l, i)
term(1) = term(1) + tvoov(a, i, l, d)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, l, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, l, d)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(a,e,i,i) * tovov(l, d, i, e)
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, d, l, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, d, l, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(8) = term(8) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibibidl = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aibibidl = eom_cc3_22_tripletmm_trans_aibibidl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibibidl
    function eom_cc3_22_tripletmm_trans_aibibkdi(t2, nocc, nactive, a, i, b, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibibkdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, k, i)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, k, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, d, k, b)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,i) * tovov(k, e, i, d)
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, d, k, e)
end do 
end do 

term(4) = -term(4) 


    eom_cc3_22_tripletmm_trans_aibibkdi = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibibkdi = eom_cc3_22_tripletmm_trans_aibibkdi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibibkdi
    function eom_cc3_22_tripletmm_trans_aibibkdk(t2, nocc, nactive, a, i, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibibkdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(k, e, k, d)
end do 



    eom_cc3_22_tripletmm_trans_aibibkdk = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibibkdk = eom_cc3_22_tripletmm_trans_aibibkdk + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibibkdk
    function eom_cc3_22_tripletmm_trans_aibjbjdj(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbjdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, j, i)
term(1) = term(1) + tvoov(a, i, j, d)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, j, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, j, b)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(a,e,i,j) * tovov(j, e, j, d)
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, d, j, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, d, j, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, j, d)
end do 
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, j, d)
end do 
end do 

term(8) = term(8) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibjbjdj = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aibjbjdj = eom_cc3_22_tripletmm_trans_aibjbjdj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbjdj
    function eom_cc3_22_tripletmm_trans_aibjbjdi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbjdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e,n 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, d, j, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, d)
term(2) = term(2) + tvvoo(a, d, m, m)
term(3) = term(3) + tvoov(a, m, m, d)
end do 

term(2) = term(2) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, d, i, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, d, j, e)
term(7) = term(7) + t2(a,e,i,m) * tovov(m, d, i, e)
term(8) = term(8) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do n = 1, nocc 
do m = 1, nocc 
term(9) = term(9) + t2(a,b,m,n) * tovov(n, b, m, d)
end do 
end do 

term(9) = -term(9) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(10) = term(10) * (-2.0d+0) 

term(11) = term(11) + read_ftvvvv(b, b, a, d)
term(12) = term(12) + tvv(a, d)
term(13) = term(13) + tvvoo(a, d, i, i)
term(14) = term(14) + tvvoo(a, d, j, j)
term(15) = term(15) + tvoov(a, i, i, d)

term(11) = -term(11) 
term(12) = -term(12) 
term(15) = -term(15) 

do e = nocc + 1, nactive 
term(16) = term(16) + t2(a,e,i,j) * tovov(j, e, i, d)
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(a,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(17) = term(17) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibjbjdi = 0.d+0
    do s = 0, 17
    eom_cc3_22_tripletmm_trans_aibjbjdi = eom_cc3_22_tripletmm_trans_aibjbjdi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbjdi
    function eom_cc3_22_tripletmm_trans_aibjbidj(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e,n 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, b)
end do 

term(0) = -term(0) 
term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(j, d, i, e)
end do 


do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,n) * tovov(n, d, m, b)
end do 
end do 


term(4) = term(4) + read_ftvvvv(b, d, a, b)



    eom_cc3_22_tripletmm_trans_aibjbidj = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjbidj = eom_cc3_22_tripletmm_trans_aibjbidj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbidj
    function eom_cc3_22_tripletmm_trans_aibjbidi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, i, j)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, i, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, d, i, b)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,j) * tovov(i, e, i, d)
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,j) * tovov(m, d, i, e)
end do 
end do 

term(4) = -term(4) 


    eom_cc3_22_tripletmm_trans_aibjbidi = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjbidi = eom_cc3_22_tripletmm_trans_aibjbidi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbidi
    function eom_cc3_22_tripletmm_trans_aiajcjcl(t2, nocc, nactive, a, i, c, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcjcl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, l 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, l, c)
end do 



    eom_cc3_22_tripletmm_trans_aiajcjcl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aiajcjcl = eom_cc3_22_tripletmm_trans_aiajcjcl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcjcl
    function eom_cc3_22_tripletmm_trans_aiajckcj(t2, nocc, nactive, a, i, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajckcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aiajckcj = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aiajckcj = eom_cc3_22_tripletmm_trans_aiajckcj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajckcj
    function eom_cc3_22_tripletmm_trans_aiajcicl(t2, nocc, nactive, a, j, c, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcicl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, l 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, l, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aiajcicl = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aiajcicl = eom_cc3_22_tripletmm_trans_aiajcicl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcicl
    function eom_cc3_22_tripletmm_trans_aiajckci(t2, nocc, nactive, a, j, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajckci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k 
    integer :: s ,m 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, k, c)
end do 



    eom_cc3_22_tripletmm_trans_aiajckci = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aiajckci = eom_cc3_22_tripletmm_trans_aiajckci + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajckci
    function eom_cc3_22_tripletmm_trans_aiajcjdj(t2, nocc, nactive, a, i, j, c, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcjdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, d, j, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmm_trans_aiajcjdj = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aiajcjdj = eom_cc3_22_tripletmm_trans_aiajcjdj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcjdj
    function eom_cc3_22_tripletmm_trans_aiajcjdi(t2, nocc, nactive, a, i, j, c, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcjdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,m,n 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, d, j, c)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, c, i, d)
end do 


term(2) = term(2) + read_ftvvvv(a, d, a, c)

term(2) = -term(2) 

do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,a,m,n) * tovov(n, c, m, d)
end do 
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletmm_trans_aiajcjdi = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aiajcjdi = eom_cc3_22_tripletmm_trans_aiajcjdi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcjdi
    function eom_cc3_22_tripletmm_trans_aiajcidj(t2, nocc, nactive, a, i, j, c, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,m,n 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, d, i, c)
end do 

term(0) = -term(0) 
term(1) = -term(1) 

term(2) = term(2) + read_ftvvvv(a, d, a, c)


do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,a,m,n) * tovov(n, d, m, c)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aiajcidj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aiajcidj = eom_cc3_22_tripletmm_trans_aiajcidj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcidj
    function eom_cc3_22_tripletmm_trans_aiajcidi(t2, nocc, nactive, a, i, j, c, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, i, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aiajcidi = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aiajcidi = eom_cc3_22_tripletmm_trans_aiajcidi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcidi
    function eom_cc3_22_tripletmm_trans_aibiakal(t2, nocc, nactive, a, i, b, k, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiakal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(l, a, k, e)
term(1) = term(1) + t2(b,e,i,i) * tovov(l, e, k, a)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibiakal = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibiakal = eom_cc3_22_tripletmm_trans_aibiakal + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiakal
    function eom_cc3_22_tripletmm_trans_aibjajal(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjajal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, l, i)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, l, a)
end do 


do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(l, a, j, e)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, j, a)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,i) * tovov(m, a, l, e)
end do 
end do 

term(4) = -term(4) 


    eom_cc3_22_tripletmm_trans_aibjajal = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjajal = eom_cc3_22_tripletmm_trans_aibjajal + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjajal
    function eom_cc3_22_tripletmm_trans_aibjakaj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjakaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, k, i)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, k, a)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(k, e, j, a)
term(3) = term(3) + t2(b,e,j,i) * tovov(k, a, j, e)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,i) * tovov(m, a, k, e)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibjakaj = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjakaj = eom_cc3_22_tripletmm_trans_aibjakaj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjakaj
    function eom_cc3_22_tripletmm_trans_aibjaial(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjaial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, l, j)
term(1) = term(1) + tvoov(b, j, l, a)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, l, a)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,i) * tovov(l, a, i, e)
term(4) = term(4) + t2(b,e,j,i) * tovov(l, e, i, a)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,m,j) * tovov(m, a, l, e)
term(6) = term(6) + t2(b,e,j,m) * tovov(m, a, l, e)
term(7) = term(7) + t2(b,e,m,j) * tovov(m, e, l, a)
end do 
end do 

term(6) = -term(6) 
term(7) = -term(7) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,j,m) * tovov(m, e, l, a)
end do 
end do 

term(8) = term(8) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibjaial = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aibjaial = eom_cc3_22_tripletmm_trans_aibjaial + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjaial
    function eom_cc3_22_tripletmm_trans_aibjakai(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjakai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, k, j)
term(1) = term(1) + tvoov(b, j, k, a)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, k, a)
end do 


do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,i) * tovov(k, e, i, a)
term(4) = term(4) + t2(b,e,j,i) * tovov(k, a, i, e)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,m,j) * tovov(m, a, k, e)
term(6) = term(6) + t2(b,e,j,m) * tovov(m, a, k, e)
term(7) = term(7) + t2(b,e,m,j) * tovov(m, e, k, a)
end do 
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,j,m) * tovov(m, e, k, a)
end do 
end do 

term(8) = term(8) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibjakai = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aibjakai = eom_cc3_22_tripletmm_trans_aibjakai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjakai
    function eom_cc3_22_tripletmm_trans_aibicicl(t2, nocc, nactive, a, i, b, c, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibicicl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibicicl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibicicl = eom_cc3_22_tripletmm_trans_aibicicl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibicicl
    function eom_cc3_22_tripletmm_trans_aibickci(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibickci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, k, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, k, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmm_trans_aibickci = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmm_trans_aibickci = eom_cc3_22_tripletmm_trans_aibickci + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibickci
    function eom_cc3_22_tripletmm_trans_aibjcjci(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcjci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,n 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, i, c)
end do 


term(2) = term(2) + read_ftvvvv(b, c, a, c)

term(2) = -term(2) 

do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,n) * tovov(n, c, m, c)
end do 
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletmm_trans_aibjcjci = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibjcjci = eom_cc3_22_tripletmm_trans_aibjcjci + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcjci
    function eom_cc3_22_tripletmm_trans_aibjcicj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcicj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,n 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, i, c)
end do 

term(0) = -term(0) 
term(1) = -term(1) 

term(2) = term(2) + read_ftvvvv(b, c, a, c)


do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,n) * tovov(n, c, m, c)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibjcicj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibjcicj = eom_cc3_22_tripletmm_trans_aibjcicj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcicj
    function eom_cc3_22_tripletmm_trans_aibicial(t2, nocc, nactive, a, i, b, c, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibicial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, l, i)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, l, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, l, a)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,i) * tovov(l, e, i, c)
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,i) * tovov(m, c, l, e)
end do 
end do 

term(4) = -term(4) 


    eom_cc3_22_tripletmm_trans_aibicial = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibicial = eom_cc3_22_tripletmm_trans_aibicial + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibicial
    function eom_cc3_22_tripletmm_trans_aibickai(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibickai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, k, i)
term(1) = term(1) + tvoov(b, i, k, c)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, k, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, k, c)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(b,e,i,i) * tovov(k, c, i, e)
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,m,i) * tovov(m, c, k, e)
term(6) = term(6) + t2(b,e,i,m) * tovov(m, c, k, e)
term(7) = term(7) + t2(b,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(8) = term(8) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibickai = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aibickai = eom_cc3_22_tripletmm_trans_aibickai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibickai
    function eom_cc3_22_tripletmm_trans_aibickak(t2, nocc, nactive, i, b, c, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibickak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, k 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(k, e, k, c)
end do 



    eom_cc3_22_tripletmm_trans_aibickak = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibickak = eom_cc3_22_tripletmm_trans_aibickak + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibickak
    function eom_cc3_22_tripletmm_trans_aibjcjaj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcjaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, j, i)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, j, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, j, c)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,i) * tovov(j, e, j, c)
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,i) * tovov(m, c, j, e)
end do 
end do 

term(4) = -term(4) 


    eom_cc3_22_tripletmm_trans_aibjcjaj = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjcjaj = eom_cc3_22_tripletmm_trans_aibjcjaj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcjaj
    function eom_cc3_22_tripletmm_trans_aibjcjai(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjcjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e,n 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, i, a)
term(2) = term(2) + tvvoo(b, c, m, m)
term(3) = term(3) + tvoov(b, m, m, c)
end do 

term(2) = term(2) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,m,i) * tovov(m, c, i, e)
term(6) = term(6) + t2(b,e,m,j) * tovov(m, c, j, e)
term(7) = term(7) + t2(b,e,j,m) * tovov(m, c, j, e)
term(8) = term(8) + t2(b,e,m,j) * tovov(m, e, j, c)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do n = 1, nocc 
do m = 1, nocc 
term(9) = term(9) + t2(a,b,m,n) * tovov(n, c, m, a)
end do 
end do 

term(9) = -term(9) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(10) = term(10) * (-2.0d+0) 

term(11) = term(11) + read_ftvvvv(b, c, a, a)
term(12) = term(12) + tvv(b, c)
term(13) = term(13) + tvvoo(b, c, i, i)
term(14) = term(14) + tvvoo(b, c, j, j)
term(15) = term(15) + tvoov(b, j, j, c)

term(11) = -term(11) 
term(12) = -term(12) 
term(15) = -term(15) 

do e = nocc + 1, nactive 
term(16) = term(16) + t2(b,e,j,i) * tovov(j, c, i, e)
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(b,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(17) = term(17) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibjcjai = 0.d+0
    do s = 0, 17
    eom_cc3_22_tripletmm_trans_aibjcjai = eom_cc3_22_tripletmm_trans_aibjcjai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjcjai
    function eom_cc3_22_tripletmm_trans_aibjciaj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjciaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e,n 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, c)
end do 

term(0) = -term(0) 
term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(j, e, i, c)
end do 


do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,n) * tovov(n, a, m, c)
end do 
end do 


term(4) = term(4) + read_ftvvvv(b, a, a, c)



    eom_cc3_22_tripletmm_trans_aibjciaj = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjciaj = eom_cc3_22_tripletmm_trans_aibjciaj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjciaj
    function eom_cc3_22_tripletmm_trans_aibjciai(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjciai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, i, j)
term(1) = term(1) + tvoov(b, j, i, c)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,j) * tovov(m, c, i, a)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, a, i, c)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(b,e,j,i) * tovov(i, e, i, c)
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,m,j) * tovov(m, c, i, e)
term(6) = term(6) + t2(b,e,j,m) * tovov(m, c, i, e)
term(7) = term(7) + t2(b,e,m,j) * tovov(m, e, i, c)
end do 
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,j,m) * tovov(m, e, i, c)
end do 
end do 

term(8) = term(8) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibjciai = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aibjciai = eom_cc3_22_tripletmm_trans_aibjciai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjciai
    function eom_cc3_22_tripletmm_trans_aibiaidl(t2, nocc, nactive, a, i, b, d, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiaidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, l, i)
term(1) = term(1) + tvoov(b, i, l, d)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, l, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, l, d)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(b,e,i,i) * tovov(l, d, i, e)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,m,i) * tovov(m, d, l, e)
term(6) = term(6) + t2(b,e,i,m) * tovov(m, d, l, e)
term(7) = term(7) + t2(b,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(6) = -term(6) 
term(7) = -term(7) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(8) = term(8) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibiaidl = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aibiaidl = eom_cc3_22_tripletmm_trans_aibiaidl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiaidl
    function eom_cc3_22_tripletmm_trans_aibiakdi(t2, nocc, nactive, a, i, b, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiakdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, k, i)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, k, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, d, k, a)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,i) * tovov(k, e, i, d)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,i) * tovov(m, d, k, e)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibiakdi = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibiakdi = eom_cc3_22_tripletmm_trans_aibiakdi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiakdi
    function eom_cc3_22_tripletmm_trans_aibiakdk(t2, nocc, nactive, i, b, k, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiakdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k, d 
    integer :: s ,e 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(k, e, k, d)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmm_trans_aibiakdk = 0.d+0
    do s = 0, 0
    eom_cc3_22_tripletmm_trans_aibiakdk = eom_cc3_22_tripletmm_trans_aibiakdk + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiakdk
    function eom_cc3_22_tripletmm_trans_aibjajdj(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjajdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, j, i)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, j, d)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, d, j, a)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,i) * tovov(j, e, j, d)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,i) * tovov(m, d, j, e)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibjajdj = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjajdj = eom_cc3_22_tripletmm_trans_aibjajdj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjajdj
    function eom_cc3_22_tripletmm_trans_aibjajdi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjajdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m,n 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, e, i, d)
end do 

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, i, d)
end do 


do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,n) * tovov(n, a, m, d)
end do 
end do 

term(3) = -term(3) 

term(4) = term(4) + read_ftvvvv(b, a, a, d)

term(4) = -term(4) 


    eom_cc3_22_tripletmm_trans_aibjajdi = 0.d+0
    do s = 0, 4
    eom_cc3_22_tripletmm_trans_aibjajdi = eom_cc3_22_tripletmm_trans_aibjajdi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjajdi
    function eom_cc3_22_tripletmm_trans_aibjaidj(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjaidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e,n 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, a)
term(2) = term(2) + tvvoo(b, d, m, m)
term(3) = term(3) + tvoov(b, m, m, d)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,m,i) * tovov(m, d, i, e)
term(6) = term(6) + t2(b,e,m,j) * tovov(m, d, j, e)
term(7) = term(7) + t2(b,e,j,m) * tovov(m, d, j, e)
term(8) = term(8) + t2(b,e,m,j) * tovov(m, e, j, d)
end do 
end do 

term(7) = -term(7) 
term(8) = -term(8) 

do n = 1, nocc 
do m = 1, nocc 
term(9) = term(9) + t2(a,b,m,n) * tovov(n, d, m, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(10) = term(10) * 2.0d+0 

term(11) = term(11) + read_ftvvvv(b, d, a, a)
term(12) = term(12) + tvv(b, d)
term(13) = term(13) + tvvoo(b, d, i, i)
term(14) = term(14) + tvvoo(b, d, j, j)
term(15) = term(15) + tvoov(b, j, j, d)

term(13) = -term(13) 
term(14) = -term(14) 

do e = nocc + 1, nactive 
term(16) = term(16) + t2(b,e,j,i) * tovov(j, d, i, e)
end do 

term(16) = -term(16) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(b,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(17) = term(17) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibjaidj = 0.d+0
    do s = 0, 17
    eom_cc3_22_tripletmm_trans_aibjaidj = eom_cc3_22_tripletmm_trans_aibjaidj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjaidj
    function eom_cc3_22_tripletmm_trans_aibjaidi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjaidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, i, j)
term(1) = term(1) + tvoov(b, j, i, d)

term(0) = -term(0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, i, d)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, d, i, a)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(b,e,j,i) * tovov(i, e, i, d)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,m,j) * tovov(m, d, i, e)
term(6) = term(6) + t2(b,e,j,m) * tovov(m, d, i, e)
term(7) = term(7) + t2(b,e,m,j) * tovov(m, e, i, d)
end do 
end do 

term(6) = -term(6) 
term(7) = -term(7) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,j,m) * tovov(m, e, i, d)
end do 
end do 

term(8) = term(8) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibjaidi = 0.d+0
    do s = 0, 8
    eom_cc3_22_tripletmm_trans_aibjaidi = eom_cc3_22_tripletmm_trans_aibjaidi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjaidi
    function eom_cc3_22_tripletmm_trans_aibicidi(t2, nocc, nactive, a, i, b, c, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibicidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,m,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, d, a, c)
term(1) = term(1) + read_ftvvvv(b, c, a, d)

term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, c, i, d)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, d, i, c)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, c, i, d)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(2) = -term(2) 
term(5) = -term(5) 

do n = 1, nocc 
do m = 1, nocc 
term(6) = term(6) + t2(a,b,m,n) * tovov(n, d, m, c)
term(7) = term(7) + t2(a,b,m,n) * tovov(n, c, m, d)
end do 
end do 

term(7) = -term(7) 


    eom_cc3_22_tripletmm_trans_aibicidi = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aibicidi = eom_cc3_22_tripletmm_trans_aibicidi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibicidi
    function eom_cc3_22_tripletmm_trans_aiajajal(t2, nocc, nactive, a, i, j, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajajal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,f,m,e 
    double precision, dimension(0:19) :: term 
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
term(3) = term(3) + tvoov(a, i, l, a)
term(4) = term(4) + too(l, i)
term(5) = term(5) + toooo(l, j, j, i)
term(6) = term(6) + toooo(l, i, j, j)

term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 
term(6) = -term(6) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(7) = term(7) + t2(e,f,i,j) * tovov(l, f, j, e)
end do 
end do 


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(e,f,i,j) * tovov(l, e, j, f)
end do 
end do 

term(8) = -term(8) 

do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,j,i) * tovov(l, a, j, e)
term(10) = term(10) + t2(a,e,j,i) * tovov(l, e, j, a)
term(11) = term(11) + t2(a,e,i,j) * tovov(l, a, j, e)
term(12) = term(12) + t2(a,e,i,j) * tovov(l, e, j, a)
end do 

term(9) = -term(9) 
term(12) = -term(12) 

do m = 1, nocc 
term(13) = term(13) + t2(a,a,i,m) * tovov(m, a, l, a)
term(14) = term(14) + toooo(m, i, l, m)
term(15) = term(15) + toooo(m, m, l, i)
end do 

term(14) = -term(14) 
term(15) = term(15) * 2.0d+0 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(16) = term(16) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(16) = term(16) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(a,e,m,i) * tovov(m, a, l, e)
term(18) = term(18) + t2(a,e,i,m) * tovov(m, a, l, e)
term(19) = term(19) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 

term(17) = term(17) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aiajajal = 0.d+0
    do s = 0, 19
    eom_cc3_22_tripletmm_trans_aiajajal = eom_cc3_22_tripletmm_trans_aiajajal + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajajal
    function eom_cc3_22_tripletmm_trans_aiajakaj(t2, nocc, nactive, a, i, j, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajakaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,f,m,e 
    double precision, dimension(0:19) :: term 
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
term(3) = term(3) + tvoov(a, i, k, a)
term(4) = term(4) + too(k, i)
term(5) = term(5) + toooo(k, i, j, j)
term(6) = term(6) + toooo(k, j, j, i)

term(2) = term(2) * (-2.0d+0) 
term(4) = -term(4) 
term(6) = -term(6) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(e,f,i,j) * tovov(k, e, j, f)
end do 
end do 


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(8) = term(8) + t2(e,f,i,j) * tovov(k, f, j, e)
end do 
end do 

term(8) = -term(8) 

do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,j,i) * tovov(k, e, j, a)
term(10) = term(10) + t2(a,e,j,i) * tovov(k, a, j, e)
term(11) = term(11) + t2(a,e,i,j) * tovov(k, e, j, a)
term(12) = term(12) + t2(a,e,i,j) * tovov(k, a, j, e)
end do 

term(9) = -term(9) 
term(12) = -term(12) 

do m = 1, nocc 
term(13) = term(13) + t2(a,a,i,m) * tovov(m, a, k, a)
term(14) = term(14) + toooo(m, i, k, m)
term(15) = term(15) + toooo(m, m, k, i)
end do 

term(13) = -term(13) 
term(15) = term(15) * (-2.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(16) = term(16) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(16) = term(16) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(a,e,m,i) * tovov(m, a, k, e)
term(18) = term(18) + t2(a,e,i,m) * tovov(m, a, k, e)
term(19) = term(19) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(17) = term(17) * 2.0d+0 
term(18) = -term(18) 
term(19) = -term(19) 


    eom_cc3_22_tripletmm_trans_aiajakaj = 0.d+0
    do s = 0, 19
    eom_cc3_22_tripletmm_trans_aiajakaj = eom_cc3_22_tripletmm_trans_aiajakaj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajakaj
    function eom_cc3_22_tripletmm_trans_aiajaial(t2, nocc, nactive, a, i, j, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajaial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,f,m,e 
    double precision, dimension(0:19) :: term 
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
term(3) = term(3) + tvoov(a, j, l, a)
term(4) = term(4) + too(l, j)
term(5) = term(5) + toooo(l, j, i, i)
term(6) = term(6) + toooo(l, i, i, j)

term(2) = term(2) * (-2.0d+0) 
term(4) = -term(4) 
term(6) = -term(6) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(7) = term(7) + t2(e,f,i,j) * tovov(l, f, i, e)
end do 
end do 


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(e,f,i,j) * tovov(l, e, i, f)
end do 
end do 

term(8) = -term(8) 

do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,j,i) * tovov(l, a, i, e)
term(10) = term(10) + t2(a,e,j,i) * tovov(l, e, i, a)
term(11) = term(11) + t2(a,e,i,j) * tovov(l, a, i, e)
term(12) = term(12) + t2(a,e,i,j) * tovov(l, e, i, a)
end do 

term(9) = -term(9) 
term(12) = -term(12) 

do m = 1, nocc 
term(13) = term(13) + t2(a,a,j,m) * tovov(m, a, l, a)
term(14) = term(14) + toooo(m, j, l, m)
term(15) = term(15) + toooo(m, m, l, j)
end do 

term(13) = -term(13) 
term(15) = term(15) * (-2.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(16) = term(16) + t2(a,e,j,m) * tovov(m, e, l, a)
end do 
end do 

term(16) = term(16) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(a,e,m,j) * tovov(m, a, l, e)
term(18) = term(18) + t2(a,e,j,m) * tovov(m, a, l, e)
term(19) = term(19) + t2(a,e,m,j) * tovov(m, e, l, a)
end do 
end do 

term(17) = term(17) * 2.0d+0 
term(18) = -term(18) 
term(19) = -term(19) 


    eom_cc3_22_tripletmm_trans_aiajaial = 0.d+0
    do s = 0, 19
    eom_cc3_22_tripletmm_trans_aiajaial = eom_cc3_22_tripletmm_trans_aiajaial + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajaial
    function eom_cc3_22_tripletmm_trans_aiajakai(t2, nocc, nactive, a, i, j, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajakai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,f,m,e 
    double precision, dimension(0:19) :: term 
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
term(3) = term(3) + tvoov(a, j, k, a)
term(4) = term(4) + too(k, j)
term(5) = term(5) + toooo(k, i, i, j)
term(6) = term(6) + toooo(k, j, i, i)

term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 
term(6) = -term(6) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(e,f,i,j) * tovov(k, e, i, f)
end do 
end do 


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(8) = term(8) + t2(e,f,i,j) * tovov(k, f, i, e)
end do 
end do 

term(8) = -term(8) 

do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,j,i) * tovov(k, e, i, a)
term(10) = term(10) + t2(a,e,j,i) * tovov(k, a, i, e)
term(11) = term(11) + t2(a,e,i,j) * tovov(k, e, i, a)
term(12) = term(12) + t2(a,e,i,j) * tovov(k, a, i, e)
end do 

term(9) = -term(9) 
term(12) = -term(12) 

do m = 1, nocc 
term(13) = term(13) + t2(a,a,j,m) * tovov(m, a, k, a)
term(14) = term(14) + toooo(m, j, k, m)
term(15) = term(15) + toooo(m, m, k, j)
end do 

term(14) = -term(14) 
term(15) = term(15) * 2.0d+0 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(16) = term(16) + t2(a,e,j,m) * tovov(m, e, k, a)
end do 
end do 

term(16) = term(16) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(17) = term(17) + t2(a,e,m,j) * tovov(m, a, k, e)
term(18) = term(18) + t2(a,e,j,m) * tovov(m, a, k, e)
term(19) = term(19) + t2(a,e,m,j) * tovov(m, e, k, a)
end do 
end do 

term(17) = term(17) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aiajakai = 0.d+0
    do s = 0, 19
    eom_cc3_22_tripletmm_trans_aiajakai = eom_cc3_22_tripletmm_trans_aiajakai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajakai
    function eom_cc3_22_tripletmm_trans_aibibibl(t2, nocc, nactive, a, i, b, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibibibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, b)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, l, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, l, b)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,i) * tovov(l, b, i, e)
term(4) = term(4) + t2(a,e,i,i) * tovov(l, e, i, b)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, b, l, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, l, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, b)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibibibl = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aibibibl = eom_cc3_22_tripletmm_trans_aibibibl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibibibl
    function eom_cc3_22_tripletmm_trans_aibibkbi(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibibkbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, b)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, k, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, k, b)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,i) * tovov(k, e, i, b)
term(4) = term(4) + t2(a,e,i,i) * tovov(k, b, i, e)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, b, k, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, k, b)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, b)
end do 
end do 

term(7) = term(7) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibibkbi = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aibibkbi = eom_cc3_22_tripletmm_trans_aibibkbi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibibkbi
    function eom_cc3_22_tripletmm_trans_aibjbjbi(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbjbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,n,e 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, b)
term(2) = term(2) + tvvoo(a, b, m, m)
term(3) = term(3) + tvoov(a, m, m, b)
end do 

term(2) = term(2) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(a,e,i,j) * tovov(j, e, i, b)
term(5) = term(5) + t2(a,e,i,j) * tovov(j, b, i, e)
end do 

term(5) = -term(5) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(6) = term(6) * 2.0d+0 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 

term(7) = -term(7) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,m,i) * tovov(m, b, i, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, b, j, e)
term(10) = term(10) + t2(a,e,i,m) * tovov(m, b, i, e)
term(11) = term(11) + t2(a,e,m,i) * tovov(m, e, i, b)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(a,e,i,m) * tovov(m, e, i, b)
end do 
end do 

term(12) = term(12) * (-2.0d+0) 

term(13) = term(13) + tvv(a, b)
term(14) = term(14) + read_ftvvvv(b, b, a, b)
term(15) = term(15) + tvvoo(a, b, i, i)
term(16) = term(16) + tvvoo(a, b, j, j)
term(17) = term(17) + tvoov(a, i, i, b)

term(13) = -term(13) 
term(14) = -term(14) 
term(17) = -term(17) 

do n = 1, nocc 
do m = 1, nocc 
term(18) = term(18) + t2(a,b,m,n) * tovov(n, b, m, b)
end do 
end do 

term(18) = -term(18) 


    eom_cc3_22_tripletmm_trans_aibjbjbi = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aibjbjbi = eom_cc3_22_tripletmm_trans_aibjbjbi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbjbi
    function eom_cc3_22_tripletmm_trans_aibjbibj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,n,e 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, b)
term(2) = term(2) + tvvoo(a, b, m, m)
term(3) = term(3) + tvoov(a, m, m, b)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(a,e,i,j) * tovov(j, b, i, e)
term(5) = term(5) + t2(a,e,i,j) * tovov(j, e, i, b)
end do 

term(5) = -term(5) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(6) = term(6) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,m,i) * tovov(m, b, i, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, b, j, e)
term(10) = term(10) + t2(a,e,i,m) * tovov(m, b, i, e)
term(11) = term(11) + t2(a,e,m,i) * tovov(m, e, i, b)
end do 
end do 

term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(a,e,i,m) * tovov(m, e, i, b)
end do 
end do 

term(12) = term(12) * 2.0d+0 

term(13) = term(13) + tvv(a, b)
term(14) = term(14) + read_ftvvvv(b, b, a, b)
term(15) = term(15) + tvvoo(a, b, i, i)
term(16) = term(16) + tvvoo(a, b, j, j)
term(17) = term(17) + tvoov(a, i, i, b)

term(15) = -term(15) 
term(16) = -term(16) 

do n = 1, nocc 
do m = 1, nocc 
term(18) = term(18) + t2(a,b,m,n) * tovov(n, b, m, b)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibjbibj = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aibjbibj = eom_cc3_22_tripletmm_trans_aibjbibj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbibj
    function eom_cc3_22_tripletmm_trans_aiajcjaj(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcjaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, j, c)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,j,i) * tovov(j, e, j, c)
term(2) = term(2) + t2(a,e,i,j) * tovov(j, e, j, c)
end do 

term(2) = -term(2) 

do m = 1, nocc 
term(3) = term(3) + t2(a,a,i,m) * tovov(m, c, j, a)
term(4) = term(4) + t2(a,a,i,m) * tovov(m, a, j, c)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, c, j, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, j, c)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, j, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aiajcjaj = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aiajcjaj = eom_cc3_22_tripletmm_trans_aiajcjaj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcjaj
    function eom_cc3_22_tripletmm_trans_aiajcjai(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,n,e 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, a, j, c)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, c, i, a)
term(2) = term(2) + tvvoo(a, c, m, m)
term(3) = term(3) + tvoov(a, m, m, c)
end do 

term(2) = term(2) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(a,e,j,i) * tovov(j, c, i, e)
term(5) = term(5) + t2(a,e,i,j) * tovov(j, c, i, e)
end do 

term(5) = -term(5) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(6) = term(6) * 2.0d+0 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 

term(7) = -term(7) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,m,i) * tovov(m, c, i, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, c, j, e)
term(10) = term(10) + t2(a,e,j,m) * tovov(m, c, j, e)
term(11) = term(11) + t2(a,e,m,j) * tovov(m, e, j, c)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(a,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(12) = term(12) * (-2.0d+0) 

term(13) = term(13) + tvv(a, c)
term(14) = term(14) + read_ftvvvv(a, c, a, a)
term(15) = term(15) + tvvoo(a, c, i, i)
term(16) = term(16) + tvvoo(a, c, j, j)
term(17) = term(17) + tvoov(a, j, j, c)

term(13) = -term(13) 
term(14) = -term(14) 
term(17) = -term(17) 

do n = 1, nocc 
do m = 1, nocc 
term(18) = term(18) + t2(a,a,m,n) * tovov(n, c, m, a)
end do 
end do 

term(18) = -term(18) 


    eom_cc3_22_tripletmm_trans_aiajcjai = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aiajcjai = eom_cc3_22_tripletmm_trans_aiajcjai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcjai
    function eom_cc3_22_tripletmm_trans_aiajciaj(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajciaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,n,e 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, a, i, c)
term(2) = term(2) + tvvoo(a, c, m, m)
term(3) = term(3) + tvoov(a, m, m, c)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(a,e,j,i) * tovov(j, e, i, c)
term(5) = term(5) + t2(a,e,i,j) * tovov(j, e, i, c)
end do 

term(5) = -term(5) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(6) = term(6) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,m,i) * tovov(m, c, i, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, c, j, e)
term(10) = term(10) + t2(a,e,i,m) * tovov(m, c, i, e)
term(11) = term(11) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(12) = term(12) * 2.0d+0 

term(13) = term(13) + tvv(a, c)
term(14) = term(14) + read_ftvvvv(a, c, a, a)
term(15) = term(15) + tvvoo(a, c, i, i)
term(16) = term(16) + tvvoo(a, c, j, j)
term(17) = term(17) + tvoov(a, i, i, c)

term(15) = -term(15) 
term(16) = -term(16) 

do n = 1, nocc 
do m = 1, nocc 
term(18) = term(18) + t2(a,a,m,n) * tovov(n, a, m, c)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aiajciaj = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aiajciaj = eom_cc3_22_tripletmm_trans_aiajciaj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajciaj
    function eom_cc3_22_tripletmm_trans_aiajciai(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajciai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, i, c)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,a,j,m) * tovov(m, c, i, a)
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, i, c)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(i, e, i, c)
term(4) = term(4) + t2(a,e,i,j) * tovov(i, e, i, c)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, c, i, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, e, i, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, i, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aiajciai = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aiajciai = eom_cc3_22_tripletmm_trans_aiajciai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajciai
    function eom_cc3_22_tripletmm_trans_aibiaibl(t2, nocc, nactive, a, i, b, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiaibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,f,m,e 
    double precision, dimension(0:18) :: term 
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

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,i) * tovov(l, f, i, e)
end do 
end do 


term(3) = term(3) + too(l, i)
term(4) = term(4) + toooo(l, i, i, i)
term(5) = term(5) + tvvoo(a, a, l, i)
term(6) = term(6) + tvvoo(b, b, l, i)
term(7) = term(7) + tvoov(b, i, l, b)

term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,i,m) * tovov(m, e, l, b)
end do 
end do 

term(8) = term(8) * 2.0d+0 

do e = nocc + 1, nactive 
term(9) = term(9) + t2(b,e,i,i) * tovov(l, b, i, e)
term(10) = term(10) + t2(a,e,i,i) * tovov(l, e, i, a)
end do 

term(9) = -term(9) 
term(10) = -term(10) 

do m = 1, nocc 
term(11) = term(11) + toooo(m, i, l, m)
term(12) = term(12) + toooo(m, m, l, i)
term(13) = term(13) + t2(a,b,m,i) * tovov(m, a, l, b)
term(14) = term(14) + t2(a,b,i,m) * tovov(m, a, l, b)
end do 

term(12) = term(12) * (-2.0d+0) 
term(13) = -term(13) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(15) = term(15) + t2(b,e,m,i) * tovov(m, b, l, e)
term(16) = term(16) + t2(a,e,m,i) * tovov(m, a, l, e)
term(17) = term(17) + t2(b,e,i,m) * tovov(m, b, l, e)
term(18) = term(18) + t2(b,e,m,i) * tovov(m, e, l, b)
end do 
end do 

term(17) = -term(17) 
term(18) = -term(18) 


    eom_cc3_22_tripletmm_trans_aibiaibl = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aibiaibl = eom_cc3_22_tripletmm_trans_aibiaibl + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiaibl
    function eom_cc3_22_tripletmm_trans_aibiakbi(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiakbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,f,m,e 
    double precision, dimension(0:18) :: term 
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

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,i) * tovov(k, e, i, f)
end do 
end do 


term(3) = term(3) + too(k, i)
term(4) = term(4) + toooo(k, i, i, i)
term(5) = term(5) + tvvoo(a, a, k, i)
term(6) = term(6) + tvvoo(b, b, k, i)
term(7) = term(7) + tvoov(a, i, k, a)

term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(8) = term(8) * 2.0d+0 

do e = nocc + 1, nactive 
term(9) = term(9) + t2(b,e,i,i) * tovov(k, e, i, b)
term(10) = term(10) + t2(a,e,i,i) * tovov(k, a, i, e)
end do 

term(9) = -term(9) 
term(10) = -term(10) 

do m = 1, nocc 
term(11) = term(11) + toooo(m, i, k, m)
term(12) = term(12) + toooo(m, m, k, i)
term(13) = term(13) + t2(a,b,m,i) * tovov(m, b, k, a)
term(14) = term(14) + t2(a,b,i,m) * tovov(m, b, k, a)
end do 

term(12) = term(12) * (-2.0d+0) 
term(14) = -term(14) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(15) = term(15) + t2(b,e,m,i) * tovov(m, b, k, e)
term(16) = term(16) + t2(a,e,m,i) * tovov(m, a, k, e)
term(17) = term(17) + t2(a,e,i,m) * tovov(m, a, k, e)
term(18) = term(18) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(17) = -term(17) 
term(18) = -term(18) 


    eom_cc3_22_tripletmm_trans_aibiakbi = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aibiakbi = eom_cc3_22_tripletmm_trans_aibiakbi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiakbi
    function eom_cc3_22_tripletmm_trans_aibiakbk(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiakbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,f 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,i,i) * tovov(k, f, k, e)
end do 
end do 


term(1) = term(1) + toooo(k, i, k, i)


do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(k, e, k, b)
term(3) = term(3) + t2(a,e,i,i) * tovov(k, e, k, a)
end do 

term(2) = -term(2) 
term(3) = -term(3) 


    eom_cc3_22_tripletmm_trans_aibiakbk = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibiakbk = eom_cc3_22_tripletmm_trans_aibiakbk + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiakbk
    function eom_cc3_22_tripletmm_trans_aibjajbj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjajbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,f,m,e 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,i,m) * tovov(m, e, j, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,m) * tovov(m, f, j, e)
end do 
end do 
end do 

term(1) = term(1) * (-2.0d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(j, f, j, e)
end do 
end do 


term(3) = term(3) + too(j, i)
term(4) = term(4) + toooo(j, j, j, i)
term(5) = term(5) + tvvoo(a, a, j, i)
term(6) = term(6) + tvvoo(b, b, j, i)
term(7) = term(7) + tvoov(a, i, j, a)

term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, j, a)
end do 
end do 

term(8) = term(8) * 2.0d+0 

do e = nocc + 1, nactive 
term(9) = term(9) + t2(b,e,j,i) * tovov(j, e, j, b)
term(10) = term(10) + t2(a,e,i,j) * tovov(j, e, j, a)
end do 

term(9) = -term(9) 
term(10) = -term(10) 

do m = 1, nocc 
term(11) = term(11) + toooo(m, i, j, m)
term(12) = term(12) + toooo(m, m, j, i)
term(13) = term(13) + t2(a,b,i,m) * tovov(m, a, j, b)
term(14) = term(14) + t2(a,b,i,m) * tovov(m, b, j, a)
end do 

term(12) = term(12) * (-2.0d+0) 
term(14) = -term(14) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(15) = term(15) + t2(b,e,m,i) * tovov(m, b, j, e)
term(16) = term(16) + t2(a,e,m,i) * tovov(m, a, j, e)
term(17) = term(17) + t2(a,e,i,m) * tovov(m, a, j, e)
term(18) = term(18) + t2(a,e,m,i) * tovov(m, e, j, a)
end do 
end do 

term(17) = -term(17) 
term(18) = -term(18) 


    eom_cc3_22_tripletmm_trans_aibjajbj = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aibjajbj = eom_cc3_22_tripletmm_trans_aibjajbj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjajbj
    function eom_cc3_22_tripletmm_trans_aibjajbi(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjajbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,f,e,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, b)
end do 


term(2) = term(2) + toooo(j, i, i, j)
term(3) = term(3) + read_ftvvvv(b, a, a, b)

term(3) = -term(3) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,j) * tovov(j, e, i, f)
end do 
end do 


do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,i) * tovov(j, e, i, b)
term(6) = term(6) + t2(a,e,i,j) * tovov(j, a, i, e)
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(a,b,m,n) * tovov(n, a, m, b)
end do 
end do 

term(7) = -term(7) 


    eom_cc3_22_tripletmm_trans_aibjajbi = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aibjajbi = eom_cc3_22_tripletmm_trans_aibjajbi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjajbi
    function eom_cc3_22_tripletmm_trans_aibjaibj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjaibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e,f,n 
    double precision, dimension(0:43) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, a)
term(2) = term(2) + toooo(m, i, i, m)
term(3) = term(3) + toooo(m, m, i, i)
term(4) = term(4) + toooo(m, j, j, m)
term(5) = term(5) + toooo(m, m, j, j)
term(6) = term(6) + tvvoo(a, a, m, m)
term(7) = term(7) + tvvoo(b, b, m, m)
term(8) = term(8) + tvoov(a, m, m, a)
term(9) = term(9) + tvoov(b, m, m, b)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(3) = term(3) * (-2.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * 2.0d+0 
term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 
term(9) = -term(9) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(e,f,i,m) * tovov(m, e, i, f)
term(11) = term(11) + t2(e,f,j,m) * tovov(m, e, j, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
term(12) = term(12) + t2(b,e,j,i) * tovov(j, b, i, e)
term(13) = term(13) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 

term(12) = -term(12) 
term(13) = -term(13) 

term(14) = term(14) + tvv(a, a)
term(15) = term(15) + tvv(b, b)
term(16) = term(16) + too(i, i)
term(17) = term(17) + too(j, j)
term(18) = term(18) + read_ftvvvv(b, b, a, a)
term(19) = term(19) + tvvoo(a, a, i, i)
term(20) = term(20) + tvvoo(a, a, j, j)
term(21) = term(21) + tvvoo(b, b, i, i)
term(22) = term(22) + tvvoo(b, b, j, j)
term(23) = term(23) + tvoov(b, j, j, b)
term(24) = term(24) + tvoov(a, i, i, a)
term(25) = term(25) + toooo(j, j, i, i)

term(16) = -term(16) 
term(17) = -term(17) 
term(19) = -term(19) 
term(20) = -term(20) 
term(21) = -term(21) 
term(22) = -term(22) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(26) = term(26) + t2(e,f,i,j) * tovov(j, f, i, e)
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(27) = term(27) + t2(b,e,m,i) * tovov(m, b, i, e)
term(28) = term(28) + t2(a,e,m,i) * tovov(m, a, i, e)
term(29) = term(29) + t2(b,e,m,j) * tovov(m, b, j, e)
term(30) = term(30) + t2(a,e,m,j) * tovov(m, a, j, e)
term(31) = term(31) + t2(b,e,j,m) * tovov(m, b, j, e)
term(32) = term(32) + t2(b,e,m,j) * tovov(m, e, j, b)
term(33) = term(33) + t2(a,e,i,m) * tovov(m, a, i, e)
term(34) = term(34) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(31) = -term(31) 
term(32) = -term(32) 
term(33) = -term(33) 
term(34) = -term(34) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(35) = term(35) + t2(e,f,i,m) * tovov(m, f, i, e)
term(36) = term(36) + t2(e,f,j,m) * tovov(m, f, j, e)
end do 
end do 
end do 

term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (-2.0d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(37) = term(37) + t2(a,b,m,n) * tovov(n, b, m, a)
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(38) = term(38) + t2(a,e,m,n) * tovov(n, e, m, a)
term(39) = term(39) + t2(b,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * (-2.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(40) = term(40) + t2(b,e,j,m) * tovov(m, e, j, b)
term(41) = term(41) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(40) = term(40) * 2.0d+0 
term(41) = term(41) * 2.0d+0 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(42) = term(42) + t2(a,e,m,n) * tovov(n, a, m, e)
term(43) = term(43) + t2(b,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibjaibj = 0.d+0
    do s = 0, 43
    eom_cc3_22_tripletmm_trans_aibjaibj = eom_cc3_22_tripletmm_trans_aibjaibj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjaibj
    function eom_cc3_22_tripletmm_trans_aibjaibi(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjaibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,f,m,e 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,j,m) * tovov(m, e, i, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,j,m) * tovov(m, f, i, e)
end do 
end do 
end do 

term(1) = term(1) * (-2.0d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(i, f, i, e)
end do 
end do 


term(3) = term(3) + too(i, j)
term(4) = term(4) + toooo(i, j, i, i)
term(5) = term(5) + tvvoo(a, a, i, j)
term(6) = term(6) + tvvoo(b, b, i, j)
term(7) = term(7) + tvoov(b, j, i, b)

term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,j,m) * tovov(m, e, i, b)
end do 
end do 

term(8) = term(8) * 2.0d+0 

do e = nocc + 1, nactive 
term(9) = term(9) + t2(b,e,j,i) * tovov(i, e, i, b)
term(10) = term(10) + t2(a,e,i,j) * tovov(i, e, i, a)
end do 

term(9) = -term(9) 
term(10) = -term(10) 

do m = 1, nocc 
term(11) = term(11) + toooo(m, j, i, m)
term(12) = term(12) + toooo(m, m, i, j)
term(13) = term(13) + t2(a,b,m,j) * tovov(m, a, i, b)
term(14) = term(14) + t2(a,b,m,j) * tovov(m, b, i, a)
end do 

term(12) = term(12) * (-2.0d+0) 
term(13) = -term(13) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(15) = term(15) + t2(b,e,m,j) * tovov(m, b, i, e)
term(16) = term(16) + t2(a,e,m,j) * tovov(m, a, i, e)
term(17) = term(17) + t2(b,e,j,m) * tovov(m, b, i, e)
term(18) = term(18) + t2(b,e,m,j) * tovov(m, e, i, b)
end do 
end do 

term(17) = -term(17) 
term(18) = -term(18) 


    eom_cc3_22_tripletmm_trans_aibjaibi = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aibjaibi = eom_cc3_22_tripletmm_trans_aibjaibi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjaibi
    function eom_cc3_22_tripletmm_trans_aibicibi(t2, nocc, nactive, a, i, b, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aibicibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,e,m,n 
    double precision, dimension(0:19) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(i, e, i, c)
end do 

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, i, b)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, i, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, c, i, b)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, b, i, c)
term(5) = term(5) + tvvoo(a, c, m, m)
term(6) = term(6) + tvoov(a, m, m, c)
end do 

term(1) = -term(1) 
term(4) = -term(4) 
term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 

do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(a,b,m,n) * tovov(n, b, m, c)
term(8) = term(8) + t2(a,b,m,n) * tovov(n, c, m, b)
end do 
end do 

term(8) = -term(8) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(9) = term(9) + t2(a,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(a,e,m,i) * tovov(m, c, i, e)
term(11) = term(11) + t2(a,e,i,m) * tovov(m, c, i, e)
term(12) = term(12) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(10) = term(10) * 2.0d+0 
term(11) = -term(11) 
term(12) = -term(12) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(13) = term(13) + t2(a,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(13) = term(13) * (-2.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(14) = term(14) * 2.0d+0 

term(15) = term(15) + read_ftvvvv(b, b, a, c)
term(16) = term(16) + read_ftvvvv(b, c, a, b)
term(17) = term(17) + tvv(a, c)
term(18) = term(18) + tvvoo(a, c, i, i)
term(19) = term(19) + tvoov(a, i, i, c)

term(16) = -term(16) 
term(18) = term(18) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibicibi = 0.d+0
    do s = 0, 19
    eom_cc3_22_tripletmm_trans_aibicibi = eom_cc3_22_tripletmm_trans_aibicibi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibicibi
    function eom_cc3_22_tripletmm_trans_aiajajdj(t2, nocc, nactive, a, i, j, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajajdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,e,m 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, j, d)

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,j,i) * tovov(j, e, j, d)
term(2) = term(2) + t2(a,e,i,j) * tovov(j, e, j, d)
end do 

term(1) = -term(1) 

do m = 1, nocc 
term(3) = term(3) + t2(a,a,i,m) * tovov(m, a, j, d)
term(4) = term(4) + t2(a,a,i,m) * tovov(m, d, j, a)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, d, j, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, j, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, j, d)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aiajajdj = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aiajajdj = eom_cc3_22_tripletmm_trans_aiajajdj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajajdj
    function eom_cc3_22_tripletmm_trans_aiajajdi(t2, nocc, nactive, a, i, j, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajajdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,m,n,e 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, d, j, a)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, a, i, d)
term(2) = term(2) + tvvoo(a, d, m, m)
term(3) = term(3) + tvoov(a, m, m, d)
end do 

term(2) = term(2) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(a,e,j,i) * tovov(j, e, i, d)
term(5) = term(5) + t2(a,e,i,j) * tovov(j, e, i, d)
end do 

term(4) = -term(4) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(6) = term(6) * 2.0d+0 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 

term(7) = -term(7) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,m,i) * tovov(m, d, i, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, d, j, e)
term(10) = term(10) + t2(a,e,i,m) * tovov(m, d, i, e)
term(11) = term(11) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(12) = term(12) * (-2.0d+0) 

term(13) = term(13) + tvv(a, d)
term(14) = term(14) + read_ftvvvv(a, d, a, a)
term(15) = term(15) + tvvoo(a, d, i, i)
term(16) = term(16) + tvvoo(a, d, j, j)
term(17) = term(17) + tvoov(a, i, i, d)

term(13) = -term(13) 
term(14) = -term(14) 
term(17) = -term(17) 

do n = 1, nocc 
do m = 1, nocc 
term(18) = term(18) + t2(a,a,m,n) * tovov(n, a, m, d)
end do 
end do 

term(18) = -term(18) 


    eom_cc3_22_tripletmm_trans_aiajajdi = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aiajajdi = eom_cc3_22_tripletmm_trans_aiajajdi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajajdi
    function eom_cc3_22_tripletmm_trans_aiajaidj(t2, nocc, nactive, a, i, j, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajaidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,m,n,e 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, d, i, a)
term(2) = term(2) + tvvoo(a, d, m, m)
term(3) = term(3) + tvoov(a, m, m, d)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(a,e,j,i) * tovov(j, d, i, e)
term(5) = term(5) + t2(a,e,i,j) * tovov(j, d, i, e)
end do 

term(4) = -term(4) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(6) = term(6) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,m,i) * tovov(m, d, i, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, d, j, e)
term(10) = term(10) + t2(a,e,j,m) * tovov(m, d, j, e)
term(11) = term(11) + t2(a,e,m,j) * tovov(m, e, j, d)
end do 
end do 

term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(a,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(12) = term(12) * 2.0d+0 

term(13) = term(13) + tvv(a, d)
term(14) = term(14) + read_ftvvvv(a, d, a, a)
term(15) = term(15) + tvvoo(a, d, i, i)
term(16) = term(16) + tvvoo(a, d, j, j)
term(17) = term(17) + tvoov(a, j, j, d)

term(15) = -term(15) 
term(16) = -term(16) 

do n = 1, nocc 
do m = 1, nocc 
term(18) = term(18) + t2(a,a,m,n) * tovov(n, d, m, a)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aiajaidj = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aiajaidj = eom_cc3_22_tripletmm_trans_aiajaidj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajaidj
    function eom_cc3_22_tripletmm_trans_aiajaidi(t2, nocc, nactive, a, i, j, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajaidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,e,m 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, i, d)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,j,i) * tovov(i, e, i, d)
term(2) = term(2) + t2(a,e,i,j) * tovov(i, e, i, d)
end do 

term(1) = -term(1) 

do m = 1, nocc 
term(3) = term(3) + t2(a,a,j,m) * tovov(m, a, i, d)
term(4) = term(4) + t2(a,a,j,m) * tovov(m, d, i, a)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, d, i, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, e, i, d)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, i, d)
end do 
end do 

term(7) = term(7) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aiajaidi = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aiajaidi = eom_cc3_22_tripletmm_trans_aiajaidi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajaidi
    function eom_cc3_22_tripletmm_trans_aibibial(t2, nocc, nactive, a, i, b, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibibial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,f,m,e 
    double precision, dimension(0:18) :: term 
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

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,i) * tovov(l, e, i, f)
end do 
end do 

term(2) = -term(2) 

term(3) = term(3) + too(l, i)
term(4) = term(4) + toooo(l, i, i, i)
term(5) = term(5) + tvvoo(a, a, l, i)
term(6) = term(6) + tvvoo(b, b, l, i)
term(7) = term(7) + tvoov(a, i, l, a)

term(4) = -term(4) 
term(7) = -term(7) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(8) = term(8) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(9) = term(9) + t2(b,e,i,i) * tovov(l, e, i, b)
term(10) = term(10) + t2(a,e,i,i) * tovov(l, a, i, e)
end do 


do m = 1, nocc 
term(11) = term(11) + toooo(m, i, l, m)
term(12) = term(12) + toooo(m, m, l, i)
term(13) = term(13) + t2(a,b,m,i) * tovov(m, b, l, a)
term(14) = term(14) + t2(a,b,i,m) * tovov(m, b, l, a)
end do 

term(11) = -term(11) 
term(12) = term(12) * 2.0d+0 
term(13) = -term(13) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(15) = term(15) + t2(b,e,m,i) * tovov(m, b, l, e)
term(16) = term(16) + t2(a,e,m,i) * tovov(m, a, l, e)
term(17) = term(17) + t2(a,e,i,m) * tovov(m, a, l, e)
term(18) = term(18) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 

term(15) = -term(15) 
term(16) = -term(16) 


    eom_cc3_22_tripletmm_trans_aibibial = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aibibial = eom_cc3_22_tripletmm_trans_aibibial + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibibial
    function eom_cc3_22_tripletmm_trans_aibibkai(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibibkai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,f,m,e 
    double precision, dimension(0:18) :: term 
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

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,i) * tovov(k, f, i, e)
end do 
end do 

term(2) = -term(2) 

term(3) = term(3) + too(k, i)
term(4) = term(4) + toooo(k, i, i, i)
term(5) = term(5) + tvvoo(a, a, k, i)
term(6) = term(6) + tvvoo(b, b, k, i)
term(7) = term(7) + tvoov(b, i, k, b)

term(4) = -term(4) 
term(7) = -term(7) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,i,m) * tovov(m, e, k, b)
end do 
end do 

term(8) = term(8) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(9) = term(9) + t2(b,e,i,i) * tovov(k, b, i, e)
term(10) = term(10) + t2(a,e,i,i) * tovov(k, e, i, a)
end do 


do m = 1, nocc 
term(11) = term(11) + toooo(m, i, k, m)
term(12) = term(12) + toooo(m, m, k, i)
term(13) = term(13) + t2(a,b,m,i) * tovov(m, a, k, b)
term(14) = term(14) + t2(a,b,i,m) * tovov(m, a, k, b)
end do 

term(11) = -term(11) 
term(12) = term(12) * 2.0d+0 
term(14) = -term(14) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(15) = term(15) + t2(b,e,m,i) * tovov(m, b, k, e)
term(16) = term(16) + t2(a,e,m,i) * tovov(m, a, k, e)
term(17) = term(17) + t2(b,e,i,m) * tovov(m, b, k, e)
term(18) = term(18) + t2(b,e,m,i) * tovov(m, e, k, b)
end do 
end do 

term(15) = -term(15) 
term(16) = -term(16) 


    eom_cc3_22_tripletmm_trans_aibibkai = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aibibkai = eom_cc3_22_tripletmm_trans_aibibkai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibibkai
    function eom_cc3_22_tripletmm_trans_aibibkak(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibibkak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,f 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,i,i) * tovov(k, f, k, e)
end do 
end do 

term(0) = -term(0) 

term(1) = term(1) + toooo(k, i, k, i)

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(k, e, k, b)
term(3) = term(3) + t2(a,e,i,i) * tovov(k, e, k, a)
end do 



    eom_cc3_22_tripletmm_trans_aibibkak = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aibibkak = eom_cc3_22_tripletmm_trans_aibibkak + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibibkak
    function eom_cc3_22_tripletmm_trans_aibjbjaj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbjaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,f,m,e 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,i,m) * tovov(m, e, j, f)
end do 
end do 
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,m) * tovov(m, f, j, e)
end do 
end do 
end do 

term(1) = term(1) * 2.0d+0 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(j, f, j, e)
end do 
end do 

term(2) = -term(2) 

term(3) = term(3) + too(j, i)
term(4) = term(4) + toooo(j, j, j, i)
term(5) = term(5) + tvvoo(a, a, j, i)
term(6) = term(6) + tvvoo(b, b, j, i)
term(7) = term(7) + tvoov(a, i, j, a)

term(4) = -term(4) 
term(7) = -term(7) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, j, a)
end do 
end do 

term(8) = term(8) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(9) = term(9) + t2(b,e,j,i) * tovov(j, e, j, b)
term(10) = term(10) + t2(a,e,i,j) * tovov(j, e, j, a)
end do 


do m = 1, nocc 
term(11) = term(11) + toooo(m, i, j, m)
term(12) = term(12) + toooo(m, m, j, i)
term(13) = term(13) + t2(a,b,i,m) * tovov(m, b, j, a)
term(14) = term(14) + t2(a,b,i,m) * tovov(m, a, j, b)
end do 

term(11) = -term(11) 
term(12) = term(12) * 2.0d+0 
term(14) = -term(14) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(15) = term(15) + t2(b,e,m,i) * tovov(m, b, j, e)
term(16) = term(16) + t2(a,e,m,i) * tovov(m, a, j, e)
term(17) = term(17) + t2(a,e,i,m) * tovov(m, a, j, e)
term(18) = term(18) + t2(a,e,m,i) * tovov(m, e, j, a)
end do 
end do 

term(15) = -term(15) 
term(16) = -term(16) 


    eom_cc3_22_tripletmm_trans_aibjbjaj = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aibjbjaj = eom_cc3_22_tripletmm_trans_aibjbjaj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbjaj
    function eom_cc3_22_tripletmm_trans_aibjbjai(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e,f,n 
    double precision, dimension(0:43) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, a)
term(2) = term(2) + toooo(m, i, i, m)
term(3) = term(3) + toooo(m, m, i, i)
term(4) = term(4) + toooo(m, j, j, m)
term(5) = term(5) + toooo(m, m, j, j)
term(6) = term(6) + tvvoo(a, a, m, m)
term(7) = term(7) + tvvoo(b, b, m, m)
term(8) = term(8) + tvoov(a, m, m, a)
term(9) = term(9) + tvoov(b, m, m, b)
end do 

term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = term(5) * 2.0d+0 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(e,f,i,m) * tovov(m, e, i, f)
term(11) = term(11) + t2(e,f,j,m) * tovov(m, e, j, f)
end do 
end do 
end do 

term(10) = -term(10) 
term(11) = -term(11) 

do e = nocc + 1, nactive 
term(12) = term(12) + t2(b,e,j,i) * tovov(j, b, i, e)
term(13) = term(13) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 


term(14) = term(14) + tvv(a, a)
term(15) = term(15) + tvv(b, b)
term(16) = term(16) + too(i, i)
term(17) = term(17) + too(j, j)
term(18) = term(18) + read_ftvvvv(b, b, a, a)
term(19) = term(19) + tvvoo(a, a, i, i)
term(20) = term(20) + tvvoo(a, a, j, j)
term(21) = term(21) + tvvoo(b, b, i, i)
term(22) = term(22) + tvvoo(b, b, j, j)
term(23) = term(23) + tvoov(b, j, j, b)
term(24) = term(24) + tvoov(a, i, i, a)
term(25) = term(25) + toooo(j, j, i, i)

term(14) = -term(14) 
term(15) = -term(15) 
term(18) = -term(18) 
term(23) = -term(23) 
term(24) = -term(24) 
term(25) = -term(25) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(26) = term(26) + t2(e,f,i,j) * tovov(j, f, i, e)
end do 
end do 

term(26) = -term(26) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(27) = term(27) + t2(b,e,m,i) * tovov(m, b, i, e)
term(28) = term(28) + t2(a,e,m,i) * tovov(m, a, i, e)
term(29) = term(29) + t2(b,e,m,j) * tovov(m, b, j, e)
term(30) = term(30) + t2(a,e,m,j) * tovov(m, a, j, e)
term(31) = term(31) + t2(b,e,j,m) * tovov(m, b, j, e)
term(32) = term(32) + t2(b,e,m,j) * tovov(m, e, j, b)
term(33) = term(33) + t2(a,e,i,m) * tovov(m, a, i, e)
term(34) = term(34) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(27) = -term(27) 
term(28) = -term(28) 
term(29) = -term(29) 
term(30) = -term(30) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(35) = term(35) + t2(e,f,i,m) * tovov(m, f, i, e)
term(36) = term(36) + t2(e,f,j,m) * tovov(m, f, j, e)
end do 
end do 
end do 

term(35) = term(35) * 2.0d+0 
term(36) = term(36) * 2.0d+0 

do n = 1, nocc 
do m = 1, nocc 
term(37) = term(37) + t2(a,b,m,n) * tovov(n, b, m, a)
end do 
end do 

term(37) = -term(37) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(38) = term(38) + t2(a,e,m,n) * tovov(n, e, m, a)
term(39) = term(39) + t2(b,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(38) = term(38) * 2.0d+0 
term(39) = term(39) * 2.0d+0 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(40) = term(40) + t2(b,e,j,m) * tovov(m, e, j, b)
term(41) = term(41) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(42) = term(42) + t2(a,e,m,n) * tovov(n, a, m, e)
term(43) = term(43) + t2(b,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 

term(42) = -term(42) 
term(43) = -term(43) 


    eom_cc3_22_tripletmm_trans_aibjbjai = 0.d+0
    do s = 0, 43
    eom_cc3_22_tripletmm_trans_aibjbjai = eom_cc3_22_tripletmm_trans_aibjbjai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbjai
    function eom_cc3_22_tripletmm_trans_aibjbiaj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbiaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,f,e,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, b)
end do 

term(0) = -term(0) 
term(1) = -term(1) 

term(2) = term(2) + toooo(j, i, i, j)
term(3) = term(3) + read_ftvvvv(b, a, a, b)

term(2) = -term(2) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,j) * tovov(j, e, i, f)
end do 
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,i) * tovov(j, e, i, b)
term(6) = term(6) + t2(a,e,i,j) * tovov(j, a, i, e)
end do 


do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(a,b,m,n) * tovov(n, a, m, b)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibjbiaj = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aibjbiaj = eom_cc3_22_tripletmm_trans_aibjbiaj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbiaj
    function eom_cc3_22_tripletmm_trans_aibjbiai(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjbiai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,f,m,e 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(0) = term(0) + t2(e,f,j,m) * tovov(m, e, i, f)
end do 
end do 
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,j,m) * tovov(m, f, i, e)
end do 
end do 
end do 

term(1) = term(1) * 2.0d+0 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(i, f, i, e)
end do 
end do 

term(2) = -term(2) 

term(3) = term(3) + too(i, j)
term(4) = term(4) + toooo(i, j, i, i)
term(5) = term(5) + tvvoo(a, a, i, j)
term(6) = term(6) + tvvoo(b, b, i, j)
term(7) = term(7) + tvoov(b, j, i, b)

term(4) = -term(4) 
term(7) = -term(7) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,j,m) * tovov(m, e, i, b)
end do 
end do 

term(8) = term(8) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(9) = term(9) + t2(b,e,j,i) * tovov(i, e, i, b)
term(10) = term(10) + t2(a,e,i,j) * tovov(i, e, i, a)
end do 


do m = 1, nocc 
term(11) = term(11) + toooo(m, j, i, m)
term(12) = term(12) + toooo(m, m, i, j)
term(13) = term(13) + t2(a,b,m,j) * tovov(m, b, i, a)
term(14) = term(14) + t2(a,b,m,j) * tovov(m, a, i, b)
end do 

term(11) = -term(11) 
term(12) = term(12) * 2.0d+0 
term(13) = -term(13) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(15) = term(15) + t2(b,e,m,j) * tovov(m, b, i, e)
term(16) = term(16) + t2(a,e,m,j) * tovov(m, a, i, e)
term(17) = term(17) + t2(b,e,j,m) * tovov(m, b, i, e)
term(18) = term(18) + t2(b,e,m,j) * tovov(m, e, i, b)
end do 
end do 

term(15) = -term(15) 
term(16) = -term(16) 


    eom_cc3_22_tripletmm_trans_aibjbiai = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aibjbiai = eom_cc3_22_tripletmm_trans_aibjbiai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjbiai
    function eom_cc3_22_tripletmm_trans_aibibidi(t2, nocc, nactive, a, i, b, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibibidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m,n 
    double precision, dimension(0:19) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(i, e, i, d)
end do 


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, i, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, i, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, d)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, d, i, b)
term(5) = term(5) + tvvoo(a, d, m, m)
term(6) = term(6) + tvoov(a, m, m, d)
end do 

term(1) = -term(1) 
term(4) = -term(4) 
term(5) = term(5) * (-2.0d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(a,b,m,n) * tovov(n, d, m, b)
term(8) = term(8) + t2(a,b,m,n) * tovov(n, b, m, d)
end do 
end do 

term(8) = -term(8) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(9) = term(9) + t2(a,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 

term(9) = -term(9) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(a,e,m,i) * tovov(m, d, i, e)
term(11) = term(11) + t2(a,e,i,m) * tovov(m, d, i, e)
term(12) = term(12) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(10) = term(10) * (-2.0d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(13) = term(13) + t2(a,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(13) = term(13) * 2.0d+0 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(14) = term(14) * (-2.0d+0) 

term(15) = term(15) + read_ftvvvv(b, d, a, b)
term(16) = term(16) + read_ftvvvv(b, b, a, d)
term(17) = term(17) + tvv(a, d)
term(18) = term(18) + tvvoo(a, d, i, i)
term(19) = term(19) + tvoov(a, i, i, d)

term(16) = -term(16) 
term(17) = -term(17) 
term(18) = term(18) * 2.0d+0 
term(19) = -term(19) 


    eom_cc3_22_tripletmm_trans_aibibidi = 0.d+0
    do s = 0, 19
    eom_cc3_22_tripletmm_trans_aibibidi = eom_cc3_22_tripletmm_trans_aibibidi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibibidi
    function eom_cc3_22_tripletmm_trans_aiajcjci(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcjci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,n 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, j, c)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, c, i, c)
end do 


do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,a,m,n) * tovov(n, c, m, c)
end do 
end do 

term(2) = -term(2) 

term(3) = term(3) + read_ftvvvv(a, c, a, c)

term(3) = -term(3) 


    eom_cc3_22_tripletmm_trans_aiajcjci = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aiajcjci = eom_cc3_22_tripletmm_trans_aiajcjci + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcjci
    function eom_cc3_22_tripletmm_trans_aiajcicj(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajcicj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,n 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, j, c)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, c, i, c)
end do 

term(0) = -term(0) 
term(1) = -term(1) 

do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,a,m,n) * tovov(n, c, m, c)
end do 
end do 


term(3) = term(3) + read_ftvvvv(a, c, a, c)



    eom_cc3_22_tripletmm_trans_aiajcicj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmm_trans_aiajcicj = eom_cc3_22_tripletmm_trans_aiajcicj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajcicj
    function eom_cc3_22_tripletmm_trans_aibiaial(t2, nocc, nactive, a, i, b, l) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiaial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,e,m 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, a)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,i) * tovov(l, a, i, e)
term(2) = term(2) + t2(b,e,i,i) * tovov(l, e, i, a)
end do 

term(1) = -term(1) 

do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,i) * tovov(m, a, l, a)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, l, a)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, a, l, e)
term(6) = term(6) + t2(b,e,m,i) * tovov(m, e, l, a)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(7) = term(7) * 2.0d+0 


    eom_cc3_22_tripletmm_trans_aibiaial = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aibiaial = eom_cc3_22_tripletmm_trans_aibiaial + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiaial
    function eom_cc3_22_tripletmm_trans_aibiakai(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiakai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, a)

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,i) * tovov(k, e, i, a)
term(2) = term(2) + t2(b,e,i,i) * tovov(k, a, i, e)
end do 

term(1) = -term(1) 

do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,i) * tovov(m, a, k, a)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, k, a)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, a, k, e)
term(6) = term(6) + t2(b,e,m,i) * tovov(m, e, k, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibiakai = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmm_trans_aibiakai = eom_cc3_22_tripletmm_trans_aibiakai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiakai
    function eom_cc3_22_tripletmm_trans_aibjajai(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjajai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,n,e 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, a)
term(2) = term(2) + tvvoo(b, a, m, m)
term(3) = term(3) + tvoov(b, m, m, a)
end do 

term(2) = term(2) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(b,e,j,i) * tovov(j, e, i, a)
term(5) = term(5) + t2(b,e,j,i) * tovov(j, a, i, e)
end do 

term(4) = -term(4) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,m,n) * tovov(n, e, m, a)
end do 
end do 
end do 

term(6) = term(6) * 2.0d+0 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,m,n) * tovov(n, a, m, e)
end do 
end do 
end do 

term(7) = -term(7) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,m,i) * tovov(m, a, i, e)
term(9) = term(9) + t2(b,e,m,j) * tovov(m, a, j, e)
term(10) = term(10) + t2(b,e,j,m) * tovov(m, a, j, e)
term(11) = term(11) + t2(b,e,m,j) * tovov(m, e, j, a)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(b,e,j,m) * tovov(m, e, j, a)
end do 
end do 

term(12) = term(12) * (-2.0d+0) 

term(13) = term(13) + tvv(b, a)
term(14) = term(14) + read_ftvvvv(b, a, a, a)
term(15) = term(15) + tvvoo(b, a, i, i)
term(16) = term(16) + tvvoo(b, a, j, j)
term(17) = term(17) + tvoov(b, j, j, a)

term(13) = -term(13) 
term(14) = -term(14) 
term(17) = -term(17) 

do n = 1, nocc 
do m = 1, nocc 
term(18) = term(18) + t2(a,b,m,n) * tovov(n, a, m, a)
end do 
end do 

term(18) = -term(18) 


    eom_cc3_22_tripletmm_trans_aibjajai = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aibjajai = eom_cc3_22_tripletmm_trans_aibjajai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjajai
    function eom_cc3_22_tripletmm_trans_aibjaiaj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aibjaiaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,n,e 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, a)
term(2) = term(2) + tvvoo(b, a, m, m)
term(3) = term(3) + tvoov(b, m, m, a)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(b,e,j,i) * tovov(j, a, i, e)
term(5) = term(5) + t2(b,e,j,i) * tovov(j, e, i, a)
end do 

term(4) = -term(4) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,m,n) * tovov(n, e, m, a)
end do 
end do 
end do 

term(6) = term(6) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(b,e,m,n) * tovov(n, a, m, e)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,m,i) * tovov(m, a, i, e)
term(9) = term(9) + t2(b,e,m,j) * tovov(m, a, j, e)
term(10) = term(10) + t2(b,e,j,m) * tovov(m, a, j, e)
term(11) = term(11) + t2(b,e,m,j) * tovov(m, e, j, a)
end do 
end do 

term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(b,e,j,m) * tovov(m, e, j, a)
end do 
end do 

term(12) = term(12) * 2.0d+0 

term(13) = term(13) + tvv(b, a)
term(14) = term(14) + read_ftvvvv(b, a, a, a)
term(15) = term(15) + tvvoo(b, a, i, i)
term(16) = term(16) + tvvoo(b, a, j, j)
term(17) = term(17) + tvoov(b, j, j, a)

term(15) = -term(15) 
term(16) = -term(16) 

do n = 1, nocc 
do m = 1, nocc 
term(18) = term(18) + t2(a,b,m,n) * tovov(n, a, m, a)
end do 
end do 



    eom_cc3_22_tripletmm_trans_aibjaiaj = 0.d+0
    do s = 0, 18
    eom_cc3_22_tripletmm_trans_aibjaiaj = eom_cc3_22_tripletmm_trans_aibjaiaj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibjaiaj
    function eom_cc3_22_tripletmm_trans_aibiciai(t2, nocc, nactive, a, i, b, c) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiciai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,e,m,n 
    double precision, dimension(0:19) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(i, e, i, c)
end do 


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, i, a)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, i, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, c, i, a)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, i, c)
term(5) = term(5) + tvvoo(b, c, m, m)
term(6) = term(6) + tvoov(b, m, m, c)
end do 

term(1) = -term(1) 
term(4) = -term(4) 
term(5) = term(5) * (-2.0d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(a,b,m,n) * tovov(n, a, m, c)
term(8) = term(8) + t2(a,b,m,n) * tovov(n, c, m, a)
end do 
end do 

term(8) = -term(8) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(9) = term(9) + t2(b,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 

term(9) = -term(9) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(b,e,m,i) * tovov(m, c, i, e)
term(11) = term(11) + t2(b,e,i,m) * tovov(m, c, i, e)
term(12) = term(12) + t2(b,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(10) = term(10) * (-2.0d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(13) = term(13) + t2(b,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(13) = term(13) * 2.0d+0 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(b,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(14) = term(14) * (-2.0d+0) 

term(15) = term(15) + read_ftvvvv(b, a, a, c)
term(16) = term(16) + read_ftvvvv(b, c, a, a)
term(17) = term(17) + tvv(b, c)
term(18) = term(18) + tvvoo(b, c, i, i)
term(19) = term(19) + tvoov(b, i, i, c)

term(16) = -term(16) 
term(17) = -term(17) 
term(18) = term(18) * 2.0d+0 
term(19) = -term(19) 


    eom_cc3_22_tripletmm_trans_aibiciai = 0.d+0
    do s = 0, 19
    eom_cc3_22_tripletmm_trans_aibiciai = eom_cc3_22_tripletmm_trans_aibiciai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiciai
    function eom_cc3_22_tripletmm_trans_aibiaidi(t2, nocc, nactive, a, i, b, d) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiaidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m,n 
    double precision, dimension(0:19) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(i, e, i, d)
end do 

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, i, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, i, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, i, d)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, d, i, a)
term(5) = term(5) + tvvoo(b, d, m, m)
term(6) = term(6) + tvoov(b, m, m, d)
end do 

term(1) = -term(1) 
term(4) = -term(4) 
term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 

do n = 1, nocc 
do m = 1, nocc 
term(7) = term(7) + t2(a,b,m,n) * tovov(n, d, m, a)
term(8) = term(8) + t2(a,b,m,n) * tovov(n, a, m, d)
end do 
end do 

term(8) = -term(8) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(9) = term(9) + t2(b,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(b,e,m,i) * tovov(m, d, i, e)
term(11) = term(11) + t2(b,e,i,m) * tovov(m, d, i, e)
term(12) = term(12) + t2(b,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(10) = term(10) * 2.0d+0 
term(11) = -term(11) 
term(12) = -term(12) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(13) = term(13) + t2(b,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(13) = term(13) * (-2.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(b,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(14) = term(14) * 2.0d+0 

term(15) = term(15) + read_ftvvvv(b, d, a, a)
term(16) = term(16) + read_ftvvvv(b, a, a, d)
term(17) = term(17) + tvv(b, d)
term(18) = term(18) + tvvoo(b, d, i, i)
term(19) = term(19) + tvoov(b, i, i, d)

term(16) = -term(16) 
term(18) = term(18) * (-2.0d+0) 


    eom_cc3_22_tripletmm_trans_aibiaidi = 0.d+0
    do s = 0, 19
    eom_cc3_22_tripletmm_trans_aibiaidi = eom_cc3_22_tripletmm_trans_aibiaidi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiaidi
    function eom_cc3_22_tripletmm_trans_aiajajai(t2, nocc, nactive, a, i, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajajai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,m,e,n,f 
    double precision, dimension(0:38) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + toooo(m, i, i, m)
term(1) = term(1) + toooo(m, m, i, i)
term(2) = term(2) + toooo(m, j, j, m)
term(3) = term(3) + toooo(m, m, j, j)
term(4) = term(4) + tvvoo(a, a, m, m)
term(5) = term(5) + tvoov(a, m, m, a)
term(6) = term(6) + t2(a,a,j,m) * tovov(m, a, j, a)
term(7) = term(7) + t2(a,a,i,m) * tovov(m, a, i, a)
end do 

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * 2.0d+0 

term(8) = term(8) + tvv(a, a)
term(9) = term(9) + read_ftvvvv(a, a, a, a)
term(10) = term(10) + too(i, i)
term(11) = term(11) + too(j, j)
term(12) = term(12) + toooo(j, i, i, j)
term(13) = term(13) + toooo(j, j, i, i)
term(14) = term(14) + tvvoo(a, a, i, i)
term(15) = term(15) + tvvoo(a, a, j, j)
term(16) = term(16) + tvoov(a, j, j, a)
term(17) = term(17) + tvoov(a, i, i, a)

term(8) = term(8) * (-2.0d+0) 
term(9) = -term(9) 
term(13) = -term(13) 
term(14) = term(14) * 2.0d+0 
term(15) = term(15) * 2.0d+0 
term(16) = -term(16) 
term(17) = -term(17) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(18) = term(18) + t2(a,e,m,i) * tovov(m, a, i, e)
term(19) = term(19) + t2(a,e,m,j) * tovov(m, a, j, e)
term(20) = term(20) + t2(a,e,j,m) * tovov(m, a, j, e)
term(21) = term(21) + t2(a,e,m,j) * tovov(m, e, j, a)
term(22) = term(22) + t2(a,e,i,m) * tovov(m, a, i, e)
term(23) = term(23) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(18) = term(18) * (-2.0d+0) 
term(19) = term(19) * (-2.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(24) = term(24) + t2(a,e,j,m) * tovov(m, e, j, a)
term(25) = term(25) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (-2.0d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(26) = term(26) + t2(a,a,m,n) * tovov(n, a, m, a)
end do 
end do 

term(26) = -term(26) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(27) = term(27) + t2(e,f,i,m) * tovov(m, e, i, f)
term(28) = term(28) + t2(e,f,j,m) * tovov(m, e, j, f)
end do 
end do 
end do 

term(27) = -term(27) 
term(28) = -term(28) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(29) = term(29) + t2(e,f,i,m) * tovov(m, f, i, e)
term(30) = term(30) + t2(e,f,j,m) * tovov(m, f, j, e)
end do 
end do 
end do 

term(29) = term(29) * 2.0d+0 
term(30) = term(30) * 2.0d+0 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(31) = term(31) + t2(a,e,m,n) * tovov(n, e, m, a)
end do 
end do 
end do 

term(31) = term(31) * 4.0d+0 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(32) = term(32) + t2(a,e,m,n) * tovov(n, a, m, e)
end do 
end do 
end do 

term(32) = term(32) * (-2.0d+0) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(33) = term(33) + t2(e,f,i,j) * tovov(j, e, i, f)
end do 
end do 


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(34) = term(34) + t2(e,f,i,j) * tovov(j, f, i, e)
end do 
end do 

term(34) = -term(34) 

do e = nocc + 1, nactive 
term(35) = term(35) + t2(a,e,j,i) * tovov(j, e, i, a)
term(36) = term(36) + t2(a,e,j,i) * tovov(j, a, i, e)
term(37) = term(37) + t2(a,e,i,j) * tovov(j, e, i, a)
term(38) = term(38) + t2(a,e,i,j) * tovov(j, a, i, e)
end do 

term(35) = -term(35) 
term(38) = -term(38) 


    eom_cc3_22_tripletmm_trans_aiajajai = 0.d+0
    do s = 0, 38
    eom_cc3_22_tripletmm_trans_aiajajai = eom_cc3_22_tripletmm_trans_aiajajai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajajai
    function eom_cc3_22_tripletmm_trans_aiajaiaj(t2, nocc, nactive, a, i, j) 
    double precision :: eom_cc3_22_tripletmm_trans_aiajaiaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,m,e,n,f 
    double precision, dimension(0:38) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + toooo(m, i, i, m)
term(1) = term(1) + toooo(m, m, i, i)
term(2) = term(2) + toooo(m, j, j, m)
term(3) = term(3) + toooo(m, m, j, j)
term(4) = term(4) + tvvoo(a, a, m, m)
term(5) = term(5) + tvoov(a, m, m, a)
term(6) = term(6) + t2(a,a,j,m) * tovov(m, a, j, a)
term(7) = term(7) + t2(a,a,i,m) * tovov(m, a, i, a)
end do 

term(1) = term(1) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * 4.0d+0 
term(5) = term(5) * (-2.0d+0) 
term(6) = -term(6) 
term(7) = -term(7) 

term(8) = term(8) + tvv(a, a)
term(9) = term(9) + read_ftvvvv(a, a, a, a)
term(10) = term(10) + too(i, i)
term(11) = term(11) + too(j, j)
term(12) = term(12) + tvvoo(a, a, i, i)
term(13) = term(13) + tvvoo(a, a, j, j)
term(14) = term(14) + tvoov(a, j, j, a)
term(15) = term(15) + tvoov(a, i, i, a)
term(16) = term(16) + toooo(j, j, i, i)
term(17) = term(17) + toooo(j, i, i, j)

term(8) = term(8) * 2.0d+0 
term(10) = -term(10) 
term(11) = -term(11) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (-2.0d+0) 
term(17) = -term(17) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(18) = term(18) + t2(a,e,m,i) * tovov(m, a, i, e)
term(19) = term(19) + t2(a,e,m,j) * tovov(m, a, j, e)
term(20) = term(20) + t2(a,e,j,m) * tovov(m, a, j, e)
term(21) = term(21) + t2(a,e,m,j) * tovov(m, e, j, a)
term(22) = term(22) + t2(a,e,i,m) * tovov(m, a, i, e)
term(23) = term(23) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(18) = term(18) * 2.0d+0 
term(19) = term(19) * 2.0d+0 
term(20) = -term(20) 
term(21) = -term(21) 
term(22) = -term(22) 
term(23) = -term(23) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(24) = term(24) + t2(a,e,j,m) * tovov(m, e, j, a)
term(25) = term(25) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(24) = term(24) * 2.0d+0 
term(25) = term(25) * 2.0d+0 

do n = 1, nocc 
do m = 1, nocc 
term(26) = term(26) + t2(a,a,m,n) * tovov(n, a, m, a)
end do 
end do 


do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(27) = term(27) + t2(e,f,i,m) * tovov(m, e, i, f)
term(28) = term(28) + t2(e,f,j,m) * tovov(m, e, j, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(29) = term(29) + t2(e,f,i,m) * tovov(m, f, i, e)
term(30) = term(30) + t2(e,f,j,m) * tovov(m, f, j, e)
end do 
end do 
end do 

term(29) = term(29) * (-2.0d+0) 
term(30) = term(30) * (-2.0d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(31) = term(31) + t2(a,e,m,n) * tovov(n, e, m, a)
end do 
end do 
end do 

term(31) = term(31) * (-4.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(32) = term(32) + t2(a,e,m,n) * tovov(n, a, m, e)
end do 
end do 
end do 

term(32) = term(32) * 2.0d+0 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(33) = term(33) + t2(e,f,i,j) * tovov(j, f, i, e)
end do 
end do 


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(34) = term(34) + t2(e,f,i,j) * tovov(j, e, i, f)
end do 
end do 

term(34) = -term(34) 

do e = nocc + 1, nactive 
term(35) = term(35) + t2(a,e,j,i) * tovov(j, a, i, e)
term(36) = term(36) + t2(a,e,j,i) * tovov(j, e, i, a)
term(37) = term(37) + t2(a,e,i,j) * tovov(j, a, i, e)
term(38) = term(38) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 

term(35) = -term(35) 
term(38) = -term(38) 


    eom_cc3_22_tripletmm_trans_aiajaiaj = 0.d+0
    do s = 0, 38
    eom_cc3_22_tripletmm_trans_aiajaiaj = eom_cc3_22_tripletmm_trans_aiajaiaj + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aiajaiaj
    function eom_cc3_22_tripletmm_trans_aibiaibi(t2, nocc, nactive, a, i, b) 
    double precision :: eom_cc3_22_tripletmm_trans_aibiaibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,m,e,f,n 
    double precision, dimension(0:38) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, i, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, i, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, i, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, a)
term(4) = term(4) + toooo(m, i, i, m)
term(5) = term(5) + toooo(m, m, i, i)
term(6) = term(6) + tvvoo(a, a, m, m)
term(7) = term(7) + tvvoo(b, b, m, m)
term(8) = term(8) + tvoov(a, m, m, a)
term(9) = term(9) + tvoov(b, m, m, b)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = term(4) * 2.0d+0 
term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * 2.0d+0 
term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 
term(9) = -term(9) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(a,e,m,n) * tovov(n, e, m, a)
term(11) = term(11) + t2(b,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (-2.0d+0) 

term(12) = term(12) + tvv(a, a)
term(13) = term(13) + tvv(b, b)
term(14) = term(14) + too(i, i)
term(15) = term(15) + toooo(i, i, i, i)
term(16) = term(16) + tvvoo(a, a, i, i)
term(17) = term(17) + tvvoo(b, b, i, i)
term(18) = term(18) + tvoov(b, i, i, b)
term(19) = term(19) + tvoov(a, i, i, a)
term(20) = term(20) + read_ftvvvv(b, b, a, a)
term(21) = term(21) + read_ftvvvv(b, a, a, b)

term(14) = term(14) * (-2.0d+0) 
term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * (-2.0d+0) 
term(21) = -term(21) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(22) = term(22) + t2(e,f,i,m) * tovov(m, e, i, f)
end do 
end do 
end do 

term(22) = term(22) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(23) = term(23) + t2(e,f,i,m) * tovov(m, f, i, e)
end do 
end do 
end do 

term(23) = term(23) * (-4.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(24) = term(24) + t2(b,e,m,i) * tovov(m, b, i, e)
term(25) = term(25) + t2(a,e,m,i) * tovov(m, a, i, e)
term(26) = term(26) + t2(b,e,i,m) * tovov(m, b, i, e)
term(27) = term(27) + t2(b,e,m,i) * tovov(m, e, i, b)
term(28) = term(28) + t2(a,e,i,m) * tovov(m, a, i, e)
term(29) = term(29) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(24) = term(24) * 2.0d+0 
term(25) = term(25) * 2.0d+0 
term(26) = -term(26) 
term(27) = -term(27) 
term(28) = -term(28) 
term(29) = -term(29) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(30) = term(30) + t2(a,e,m,n) * tovov(n, a, m, e)
term(31) = term(31) + t2(b,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(32) = term(32) + t2(b,e,i,m) * tovov(m, e, i, b)
term(33) = term(33) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(32) = term(32) * 2.0d+0 
term(33) = term(33) * 2.0d+0 

do e = nocc + 1, nactive 
term(34) = term(34) + t2(b,e,i,i) * tovov(i, e, i, b)
term(35) = term(35) + t2(a,e,i,i) * tovov(i, e, i, a)
end do 

term(34) = -term(34) 
term(35) = -term(35) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(36) = term(36) + t2(e,f,i,i) * tovov(i, f, i, e)
end do 
end do 


do n = 1, nocc 
do m = 1, nocc 
term(37) = term(37) + t2(a,b,m,n) * tovov(n, b, m, a)
term(38) = term(38) + t2(a,b,m,n) * tovov(n, a, m, b)
end do 
end do 

term(38) = -term(38) 


    eom_cc3_22_tripletmm_trans_aibiaibi = 0.d+0
    do s = 0, 38
    eom_cc3_22_tripletmm_trans_aibiaibi = eom_cc3_22_tripletmm_trans_aibiaibi + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibiaibi
    function eom_cc3_22_tripletmm_trans_aibibiai(t2, nocc, nactive, a, i, b) 
    double precision :: eom_cc3_22_tripletmm_trans_aibibiai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,m,e,f,n 
    double precision, dimension(0:38) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, i, a)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, i, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, i, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, i, b)
term(4) = term(4) + toooo(m, i, i, m)
term(5) = term(5) + toooo(m, m, i, i)
term(6) = term(6) + tvvoo(a, a, m, m)
term(7) = term(7) + tvvoo(b, b, m, m)
term(8) = term(8) + tvoov(a, m, m, a)
term(9) = term(9) + tvoov(b, m, m, b)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(a,e,m,n) * tovov(n, e, m, a)
term(11) = term(11) + t2(b,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(10) = term(10) * 2.0d+0 
term(11) = term(11) * 2.0d+0 

term(12) = term(12) + tvv(a, a)
term(13) = term(13) + tvv(b, b)
term(14) = term(14) + too(i, i)
term(15) = term(15) + toooo(i, i, i, i)
term(16) = term(16) + tvvoo(a, a, i, i)
term(17) = term(17) + tvvoo(b, b, i, i)
term(18) = term(18) + tvoov(b, i, i, b)
term(19) = term(19) + tvoov(a, i, i, a)
term(20) = term(20) + read_ftvvvv(b, a, a, b)
term(21) = term(21) + read_ftvvvv(b, b, a, a)

term(12) = -term(12) 
term(13) = -term(13) 
term(14) = term(14) * 2.0d+0 
term(15) = -term(15) 
term(16) = term(16) * 2.0d+0 
term(17) = term(17) * 2.0d+0 
term(18) = -term(18) 
term(19) = -term(19) 
term(21) = -term(21) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(22) = term(22) + t2(e,f,i,m) * tovov(m, e, i, f)
end do 
end do 
end do 

term(22) = term(22) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(23) = term(23) + t2(e,f,i,m) * tovov(m, f, i, e)
end do 
end do 
end do 

term(23) = term(23) * 4.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(24) = term(24) + t2(b,e,m,i) * tovov(m, b, i, e)
term(25) = term(25) + t2(a,e,m,i) * tovov(m, a, i, e)
term(26) = term(26) + t2(b,e,i,m) * tovov(m, b, i, e)
term(27) = term(27) + t2(b,e,m,i) * tovov(m, e, i, b)
term(28) = term(28) + t2(a,e,i,m) * tovov(m, a, i, e)
term(29) = term(29) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(30) = term(30) + t2(a,e,m,n) * tovov(n, a, m, e)
term(31) = term(31) + t2(b,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 

term(30) = -term(30) 
term(31) = -term(31) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(32) = term(32) + t2(b,e,i,m) * tovov(m, e, i, b)
term(33) = term(33) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(34) = term(34) + t2(b,e,i,i) * tovov(i, e, i, b)
term(35) = term(35) + t2(a,e,i,i) * tovov(i, e, i, a)
end do 


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(36) = term(36) + t2(e,f,i,i) * tovov(i, f, i, e)
end do 
end do 

term(36) = -term(36) 

do n = 1, nocc 
do m = 1, nocc 
term(37) = term(37) + t2(a,b,m,n) * tovov(n, a, m, b)
term(38) = term(38) + t2(a,b,m,n) * tovov(n, b, m, a)
end do 
end do 

term(38) = -term(38) 


    eom_cc3_22_tripletmm_trans_aibibiai = 0.d+0
    do s = 0, 38
    eom_cc3_22_tripletmm_trans_aibibiai = eom_cc3_22_tripletmm_trans_aibibiai + term(s)
    end do

    end function eom_cc3_22_tripletmm_trans_aibibiai
    end module eom_cc3_22_tripletmm_trans
    
