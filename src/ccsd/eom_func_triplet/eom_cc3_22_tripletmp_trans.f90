module eom_cc3_22_tripletmp_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_cc3_22_tripletmp_trans_aibjckbl(t2, nocc, nactive, a, i, j, c, k, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjckbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, c, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmp_trans_aibjckbl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aibjckbl = eom_cc3_22_tripletmp_trans_aibjckbl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjckbl
    function eom_cc3_22_tripletmp_trans_aibjbkdl(t2, nocc, nactive, a, i, j, k, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbkdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, d, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, k, d)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmp_trans_aibjbkdl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aibjbkdl = eom_cc3_22_tripletmp_trans_aibjbkdl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbkdl
    function eom_cc3_22_tripletmp_trans_aibjckal(t2, nocc, nactive, i, b, j, c, k, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjckal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, c, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmp_trans_aibjckal = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aibjckal = eom_cc3_22_tripletmp_trans_aibjckal + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjckal
    function eom_cc3_22_tripletmp_trans_aibjakdl(t2, nocc, nactive, i, b, j, k, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjakdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, d, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, d)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmp_trans_aibjakdl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aibjakdl = eom_cc3_22_tripletmp_trans_aibjakdl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjakdl
    function eom_cc3_22_tripletmp_trans_aibjcjdl(t2, nocc, nactive, a, i, b, c, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjcjdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d, l 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, l, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmp_trans_aibjcjdl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aibjcjdl = eom_cc3_22_tripletmp_trans_aibjcjdl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjcjdl
    function eom_cc3_22_tripletmp_trans_aibjckdj(t2, nocc, nactive, a, i, b, c, k, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjckdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, k, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmp_trans_aibjckdj = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aibjckdj = eom_cc3_22_tripletmp_trans_aibjckdj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjckdj
    function eom_cc3_22_tripletmp_trans_aibjcidl(t2, nocc, nactive, a, b, j, c, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjcidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d, l 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, l, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmp_trans_aibjcidl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aibjcidl = eom_cc3_22_tripletmp_trans_aibjcidl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjcidl
    function eom_cc3_22_tripletmp_trans_aibjckdi(t2, nocc, nactive, a, b, j, c, k, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjckdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k, d 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, k, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, k, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmp_trans_aibjckdi = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aibjckdi = eom_cc3_22_tripletmp_trans_aibjckdi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjckdi
    function eom_cc3_22_tripletmp_trans_aiajckal(t2, nocc, nactive, a, i, j, c, k, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajckal 
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

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletmp_trans_aiajckal = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aiajckal = eom_cc3_22_tripletmp_trans_aiajckal + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajckal
    function eom_cc3_22_tripletmp_trans_aibjakbl(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjakbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, b, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, b)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, a, k, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, k, a)
end do 

term(0) = -term(0) 
term(2) = -term(2) 


    eom_cc3_22_tripletmp_trans_aibjakbl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibjakbl = eom_cc3_22_tripletmp_trans_aibjakbl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjakbl
    function eom_cc3_22_tripletmp_trans_aibjcjbl(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjcjbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, c)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, l, c)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,j) * tovov(l, c, j, e)
term(4) = term(4) + t2(a,e,i,j) * tovov(l, e, j, c)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, c, l, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, l, c)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 


    eom_cc3_22_tripletmp_trans_aibjcjbl = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmp_trans_aibjcjbl = eom_cc3_22_tripletmp_trans_aibjcjbl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjcjbl
    function eom_cc3_22_tripletmp_trans_aibickbl(t2, nocc, nactive, a, i, c, k, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibickbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(l, c, k, e)
term(1) = term(1) + t2(a,e,i,i) * tovov(l, e, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmp_trans_aibickbl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aibickbl = eom_cc3_22_tripletmp_trans_aibickbl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibickbl
    function eom_cc3_22_tripletmp_trans_aibjckbj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjckbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, c)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, k, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, k, c)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,j) * tovov(k, e, j, c)
term(4) = term(4) + t2(a,e,i,j) * tovov(k, c, j, e)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, c, k, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletmp_trans_aibjckbj = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmp_trans_aibjckbj = eom_cc3_22_tripletmp_trans_aibjckbj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjckbj
    function eom_cc3_22_tripletmp_trans_aibjcibl(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjcibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, l, c)
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(l, c, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, i, c)
end do 

term(2) = -term(2) 


    eom_cc3_22_tripletmp_trans_aibjcibl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibjcibl = eom_cc3_22_tripletmp_trans_aibjcibl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjcibl
    function eom_cc3_22_tripletmp_trans_aibjckbi(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjckbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, k, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, k, c)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, i, c)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, c, i, e)
end do 

term(2) = -term(2) 


    eom_cc3_22_tripletmp_trans_aibjckbi = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibjckbi = eom_cc3_22_tripletmp_trans_aibjckbi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjckbi
    function eom_cc3_22_tripletmp_trans_aiajakdl(t2, nocc, nactive, a, i, j, k, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajakdl 
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

term(0) = -term(0) 
term(3) = -term(3) 


    eom_cc3_22_tripletmp_trans_aiajakdl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aiajakdl = eom_cc3_22_tripletmp_trans_aiajakdl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajakdl
    function eom_cc3_22_tripletmp_trans_aibjbkal(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbkal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, b, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, b)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, a, k, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, k, a)
end do 

term(1) = -term(1) 
term(3) = -term(3) 


    eom_cc3_22_tripletmp_trans_aibjbkal = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibjbkal = eom_cc3_22_tripletmp_trans_aibjbkal + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbkal
    function eom_cc3_22_tripletmp_trans_aibjbjdl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbjdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, d)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, l, d)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, d, l, b)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,j) * tovov(l, d, j, e)
term(4) = term(4) + t2(a,e,i,j) * tovov(l, e, j, d)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, d, l, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletmp_trans_aibjbjdl = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmp_trans_aibjbjdl = eom_cc3_22_tripletmp_trans_aibjbjdl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbjdl
    function eom_cc3_22_tripletmp_trans_aibibkdl(t2, nocc, nactive, a, i, k, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibibkdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(l, d, k, e)
term(1) = term(1) + t2(a,e,i,i) * tovov(l, e, k, d)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmp_trans_aibibkdl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aibibkdl = eom_cc3_22_tripletmp_trans_aibibkdl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibibkdl
    function eom_cc3_22_tripletmp_trans_aibjbkdj(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbkdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, k, d)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, d, k, b)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,j) * tovov(k, e, j, d)
term(4) = term(4) + t2(a,e,i,j) * tovov(k, d, j, e)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, d, k, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, k, d)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(7) = term(7) * 2.0d+0 


    eom_cc3_22_tripletmp_trans_aibjbkdj = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmp_trans_aibjbkdj = eom_cc3_22_tripletmp_trans_aibjbkdj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbkdj
    function eom_cc3_22_tripletmp_trans_aibjbidl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, l, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, l, b)
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(l, d, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, i, d)
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletmp_trans_aibjbidl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibjbidl = eom_cc3_22_tripletmp_trans_aibjbidl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbidl
    function eom_cc3_22_tripletmp_trans_aibjbkdi(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbkdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, k, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, k, b)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, i, d)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, d, i, e)
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletmp_trans_aibjbkdi = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibjbkdi = eom_cc3_22_tripletmp_trans_aibjbkdi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbkdi
    function eom_cc3_22_tripletmp_trans_aiajcjdl(t2, nocc, nactive, a, i, c, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajcjdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, d, l 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, d, l, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmp_trans_aiajcjdl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aiajcjdl = eom_cc3_22_tripletmp_trans_aiajcjdl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajcjdl
    function eom_cc3_22_tripletmp_trans_aiajckdj(t2, nocc, nactive, a, i, c, k, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajckdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, d 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, k, d)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, d, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmp_trans_aiajckdj = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aiajckdj = eom_cc3_22_tripletmp_trans_aiajckdj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajckdj
    function eom_cc3_22_tripletmp_trans_aiajcidl(t2, nocc, nactive, a, j, c, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajcidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, d, l 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, l, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmp_trans_aiajcidl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aiajcidl = eom_cc3_22_tripletmp_trans_aiajcidl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajcidl
    function eom_cc3_22_tripletmp_trans_aiajckdi(t2, nocc, nactive, a, j, c, k, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajckdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k, d 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, k, d)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, k, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmp_trans_aiajckdi = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aiajckdi = eom_cc3_22_tripletmp_trans_aiajckdi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajckdi
    function eom_cc3_22_tripletmp_trans_aibjcjal(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjcjal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, l, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, l, c)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(l, c, j, e)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, j, c)
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletmp_trans_aibjcjal = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibjcjal = eom_cc3_22_tripletmp_trans_aibjcjal + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjcjal
    function eom_cc3_22_tripletmp_trans_aibickal(t2, nocc, nactive, i, b, c, k, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibickal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, k, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(l, c, k, e)
term(1) = term(1) + t2(b,e,i,i) * tovov(l, e, k, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletmp_trans_aibickal = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aibickal = eom_cc3_22_tripletmp_trans_aibickal + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibickal
    function eom_cc3_22_tripletmp_trans_aibjckaj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjckaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, k, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, k, c)
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(k, e, j, c)
term(3) = term(3) + t2(b,e,j,i) * tovov(k, c, j, e)
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletmp_trans_aibjckaj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibjckaj = eom_cc3_22_tripletmp_trans_aibjckaj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjckaj
    function eom_cc3_22_tripletmp_trans_aibjcial(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjcial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, l, c)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, l, a)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, l, c)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,i) * tovov(l, c, i, e)
term(4) = term(4) + t2(b,e,j,i) * tovov(l, e, i, c)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, c, l, e)
term(6) = term(6) + t2(b,e,m,j) * tovov(m, e, l, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,j,m) * tovov(m, e, l, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletmp_trans_aibjcial = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmp_trans_aibjcial = eom_cc3_22_tripletmp_trans_aibjcial + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjcial
    function eom_cc3_22_tripletmp_trans_aibjckai(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjckai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, c)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, k, a)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, k, c)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,i) * tovov(k, e, i, c)
term(4) = term(4) + t2(b,e,j,i) * tovov(k, c, i, e)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, c, k, e)
term(6) = term(6) + t2(b,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 


    eom_cc3_22_tripletmp_trans_aibjckai = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmp_trans_aibjckai = eom_cc3_22_tripletmp_trans_aibjckai + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjckai
    function eom_cc3_22_tripletmp_trans_aibjajdl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjajdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, l, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, l, a)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(l, d, j, e)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, j, d)
end do 

term(2) = -term(2) 


    eom_cc3_22_tripletmp_trans_aibjajdl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibjajdl = eom_cc3_22_tripletmp_trans_aibjajdl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjajdl
    function eom_cc3_22_tripletmp_trans_aibiakdl(t2, nocc, nactive, i, b, k, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibiakdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(l, d, k, e)
term(1) = term(1) + t2(b,e,i,i) * tovov(l, e, k, d)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletmp_trans_aibiakdl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletmp_trans_aibiakdl = eom_cc3_22_tripletmp_trans_aibiakdl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibiakdl
    function eom_cc3_22_tripletmp_trans_aibjakdj(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjakdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, k, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, k, a)
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(k, e, j, d)
term(3) = term(3) + t2(b,e,j,i) * tovov(k, d, j, e)
end do 

term(2) = -term(2) 


    eom_cc3_22_tripletmp_trans_aibjakdj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibjakdj = eom_cc3_22_tripletmp_trans_aibjakdj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjakdj
    function eom_cc3_22_tripletmp_trans_aibjaidl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjaidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, l, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, l, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, d, l, a)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,i) * tovov(l, d, i, e)
term(4) = term(4) + t2(b,e,j,i) * tovov(l, e, i, d)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
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


    eom_cc3_22_tripletmp_trans_aibjaidl = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmp_trans_aibjaidl = eom_cc3_22_tripletmp_trans_aibjaidl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjaidl
    function eom_cc3_22_tripletmp_trans_aibjakdi(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjakdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, d)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, k, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, d, k, a)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,i) * tovov(k, e, i, d)
term(4) = term(4) + t2(b,e,j,i) * tovov(k, d, i, e)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, d, k, e)
term(6) = term(6) + t2(b,e,m,j) * tovov(m, e, k, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,j,m) * tovov(m, e, k, d)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletmp_trans_aibjakdi = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletmp_trans_aibjakdi = eom_cc3_22_tripletmp_trans_aibjakdi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjakdi
    function eom_cc3_22_tripletmp_trans_aibicidl(t2, nocc, nactive, a, i, b, c, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibicidl 
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

term(0) = -term(0) 
term(3) = -term(3) 


    eom_cc3_22_tripletmp_trans_aibicidl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibicidl = eom_cc3_22_tripletmp_trans_aibicidl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibicidl
    function eom_cc3_22_tripletmp_trans_aibjcjdi(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjcjdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, i, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(1) = -term(1) 
term(3) = -term(3) 


    eom_cc3_22_tripletmp_trans_aibjcjdi = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibjcjdi = eom_cc3_22_tripletmp_trans_aibjcjdi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjcjdi
    function eom_cc3_22_tripletmp_trans_aibickdi(t2, nocc, nactive, a, i, b, c, k, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibickdi 
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

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletmp_trans_aibickdi = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibickdi = eom_cc3_22_tripletmp_trans_aibickdi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibickdi
    function eom_cc3_22_tripletmp_trans_aibjcidj(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjcidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, i, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(0) = -term(0) 
term(2) = -term(2) 


    eom_cc3_22_tripletmp_trans_aibjcidj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibjcidj = eom_cc3_22_tripletmp_trans_aibjcidj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjcidj
    function eom_cc3_22_tripletmp_trans_aiajcjal(t2, nocc, nactive, a, i, j, c, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajcjal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, c)


do m = 1, nocc 
term(1) = term(1) + t2(a,a,i,m) * tovov(m, c, l, a)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, l, c)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(l, c, j, e)
term(4) = term(4) + t2(a,e,j,i) * tovov(l, e, j, c)
term(5) = term(5) + t2(a,e,i,j) * tovov(l, c, j, e)
term(6) = term(6) + t2(a,e,i,j) * tovov(l, e, j, c)
end do 

term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, c, l, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, l, c)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aiajcjal = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aiajcjal = eom_cc3_22_tripletmp_trans_aiajcjal + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajcjal
    function eom_cc3_22_tripletmp_trans_aiajckaj(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajckaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, c)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,a,i,m) * tovov(m, c, k, a)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, k, c)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(k, e, j, c)
term(4) = term(4) + t2(a,e,j,i) * tovov(k, c, j, e)
term(5) = term(5) + t2(a,e,i,j) * tovov(k, e, j, c)
term(6) = term(6) + t2(a,e,i,j) * tovov(k, c, j, e)
end do 

term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, c, k, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 



    eom_cc3_22_tripletmp_trans_aiajckaj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aiajckaj = eom_cc3_22_tripletmp_trans_aiajckaj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajckaj
    function eom_cc3_22_tripletmp_trans_aiajcial(t2, nocc, nactive, a, i, j, c, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajcial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, l, c)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,a,j,m) * tovov(m, c, l, a)
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, l, c)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(l, c, i, e)
term(4) = term(4) + t2(a,e,j,i) * tovov(l, e, i, c)
term(5) = term(5) + t2(a,e,i,j) * tovov(l, c, i, e)
term(6) = term(6) + t2(a,e,i,j) * tovov(l, e, i, c)
end do 

term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, l, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,j,m) * tovov(m, c, l, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, e, l, c)
end do 
end do 



    eom_cc3_22_tripletmp_trans_aiajcial = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aiajcial = eom_cc3_22_tripletmp_trans_aiajcial + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajcial
    function eom_cc3_22_tripletmp_trans_aiajckai(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajckai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, k, c)


do m = 1, nocc 
term(1) = term(1) + t2(a,a,j,m) * tovov(m, c, k, a)
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, k, c)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(k, e, i, c)
term(4) = term(4) + t2(a,e,j,i) * tovov(k, c, i, e)
term(5) = term(5) + t2(a,e,i,j) * tovov(k, e, i, c)
term(6) = term(6) + t2(a,e,i,j) * tovov(k, c, i, e)
end do 

term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,j,m) * tovov(m, c, k, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aiajckai = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aiajckai = eom_cc3_22_tripletmp_trans_aiajckai + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajckai
    function eom_cc3_22_tripletmp_trans_aibjajbl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjajbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, a)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, a, l, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(3) = term(3) * 2.0d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, l, b)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, b, l, a)
end do 

term(5) = -term(5) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,i) * tovov(l, b, j, e)
term(7) = term(7) + t2(b,e,j,i) * tovov(l, e, j, b)
term(8) = term(8) + t2(a,e,i,j) * tovov(l, a, j, e)
term(9) = term(9) + t2(a,e,i,j) * tovov(l, e, j, a)
end do 

term(6) = -term(6) 
term(8) = -term(8) 


    eom_cc3_22_tripletmp_trans_aibjajbl = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjajbl = eom_cc3_22_tripletmp_trans_aibjajbl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjajbl
    function eom_cc3_22_tripletmp_trans_aibiakbl(t2, nocc, nactive, a, i, b, k, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibiakbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, l 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(l, b, k, e)
term(1) = term(1) + t2(b,e,i,i) * tovov(l, e, k, b)
term(2) = term(2) + t2(a,e,i,i) * tovov(l, a, k, e)
term(3) = term(3) + t2(a,e,i,i) * tovov(l, e, k, a)
end do 

term(0) = -term(0) 
term(2) = -term(2) 


    eom_cc3_22_tripletmp_trans_aibiakbl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibiakbl = eom_cc3_22_tripletmp_trans_aibiakbl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibiakbl
    function eom_cc3_22_tripletmp_trans_aibjakbj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjakbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, a)

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, a, k, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, k, b)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, b, k, a)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,i) * tovov(k, e, j, b)
term(7) = term(7) + t2(b,e,j,i) * tovov(k, b, j, e)
term(8) = term(8) + t2(a,e,i,j) * tovov(k, e, j, a)
term(9) = term(9) + t2(a,e,i,j) * tovov(k, a, j, e)
end do 

term(6) = -term(6) 
term(8) = -term(8) 


    eom_cc3_22_tripletmp_trans_aibjakbj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjakbj = eom_cc3_22_tripletmp_trans_aibjakbj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjakbj
    function eom_cc3_22_tripletmp_trans_aibjaibl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjaibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, l, b)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, b, l, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, l, b)
end do 
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, l, b)
end do 
end do 

term(3) = term(3) * 2.0d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,j) * tovov(m, a, l, b)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, b, l, a)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,i) * tovov(l, b, i, e)
term(7) = term(7) + t2(b,e,j,i) * tovov(l, e, i, b)
term(8) = term(8) + t2(a,e,i,j) * tovov(l, a, i, e)
term(9) = term(9) + t2(a,e,i,j) * tovov(l, e, i, a)
end do 

term(6) = -term(6) 
term(8) = -term(8) 


    eom_cc3_22_tripletmp_trans_aibjaibl = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjaibl = eom_cc3_22_tripletmp_trans_aibjaibl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjaibl
    function eom_cc3_22_tripletmp_trans_aibjakbi(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjakbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, b)

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, b, k, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, k, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, k, b)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,j) * tovov(m, a, k, b)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, b, k, a)
end do 

term(5) = -term(5) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,i) * tovov(k, e, i, b)
term(7) = term(7) + t2(b,e,j,i) * tovov(k, b, i, e)
term(8) = term(8) + t2(a,e,i,j) * tovov(k, e, i, a)
term(9) = term(9) + t2(a,e,i,j) * tovov(k, a, i, e)
end do 

term(6) = -term(6) 
term(8) = -term(8) 


    eom_cc3_22_tripletmp_trans_aibjakbi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjakbi = eom_cc3_22_tripletmp_trans_aibjakbi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjakbi
    function eom_cc3_22_tripletmp_trans_aibicibl(t2, nocc, nactive, a, i, b, c, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibicibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, c)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, l, b)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, l, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, c, l, b)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, b, l, c)
end do 

term(1) = -term(1) 
term(4) = -term(4) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,i) * tovov(l, c, i, e)
term(6) = term(6) + t2(a,e,i,i) * tovov(l, e, i, c)
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, c, l, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, l, c)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aibicibl = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibicibl = eom_cc3_22_tripletmp_trans_aibicibl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibicibl
    function eom_cc3_22_tripletmp_trans_aibjcjbi(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjcjbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, i, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, c)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

term(4) = term(4) + tvoov(a, i, i, c)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, c, i, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,j) * tovov(j, e, i, c)
term(9) = term(9) + t2(a,e,i,j) * tovov(j, c, i, e)
end do 

term(8) = -term(8) 


    eom_cc3_22_tripletmp_trans_aibjcjbi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjcjbi = eom_cc3_22_tripletmp_trans_aibjcjbi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjcjbi
    function eom_cc3_22_tripletmp_trans_aibickbi(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aibickbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, c)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, k, b)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, k, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, c, k, b)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, b, k, c)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,i) * tovov(k, e, i, c)
term(6) = term(6) + t2(a,e,i,i) * tovov(k, c, i, e)
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, c, k, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 



    eom_cc3_22_tripletmp_trans_aibickbi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibickbi = eom_cc3_22_tripletmp_trans_aibickbi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibickbi
    function eom_cc3_22_tripletmp_trans_aibjcibj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjcibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, i, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, c)
end do 

term(0) = -term(0) 
term(2) = -term(2) 

term(4) = term(4) + tvoov(a, i, i, c)

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, c, i, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,j) * tovov(j, c, i, e)
term(9) = term(9) + t2(a,e,i,j) * tovov(j, e, i, c)
end do 

term(8) = -term(8) 


    eom_cc3_22_tripletmp_trans_aibjcibj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjcibj = eom_cc3_22_tripletmp_trans_aibjcibj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjcibj
    function eom_cc3_22_tripletmp_trans_aiajajdl(t2, nocc, nactive, a, i, j, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajajdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, d)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,a,i,m) * tovov(m, a, l, d)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, d, l, a)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(l, d, j, e)
term(4) = term(4) + t2(a,e,j,i) * tovov(l, e, j, d)
term(5) = term(5) + t2(a,e,i,j) * tovov(l, d, j, e)
term(6) = term(6) + t2(a,e,i,j) * tovov(l, e, j, d)
end do 

term(3) = -term(3) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, d, l, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 



    eom_cc3_22_tripletmp_trans_aiajajdl = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aiajajdl = eom_cc3_22_tripletmp_trans_aiajajdl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajajdl
    function eom_cc3_22_tripletmp_trans_aiajakdj(t2, nocc, nactive, a, i, j, k, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajakdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,a,i,m) * tovov(m, a, k, d)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, d, k, a)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(k, e, j, d)
term(4) = term(4) + t2(a,e,j,i) * tovov(k, d, j, e)
term(5) = term(5) + t2(a,e,i,j) * tovov(k, e, j, d)
term(6) = term(6) + t2(a,e,i,j) * tovov(k, d, j, e)
end do 

term(3) = -term(3) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, d, k, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, k, d)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aiajakdj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aiajakdj = eom_cc3_22_tripletmp_trans_aiajakdj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajakdj
    function eom_cc3_22_tripletmp_trans_aiajaidl(t2, nocc, nactive, a, i, j, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajaidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, l, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,a,j,m) * tovov(m, a, l, d)
term(2) = term(2) + t2(a,a,j,m) * tovov(m, d, l, a)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(l, d, i, e)
term(4) = term(4) + t2(a,e,j,i) * tovov(l, e, i, d)
term(5) = term(5) + t2(a,e,i,j) * tovov(l, d, i, e)
term(6) = term(6) + t2(a,e,i,j) * tovov(l, e, i, d)
end do 

term(3) = -term(3) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,j,m) * tovov(m, d, l, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, e, l, d)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aiajaidl = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aiajaidl = eom_cc3_22_tripletmp_trans_aiajaidl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajaidl
    function eom_cc3_22_tripletmp_trans_aiajakdi(t2, nocc, nactive, a, i, j, k, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajakdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, k, d)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,a,j,m) * tovov(m, a, k, d)
term(2) = term(2) + t2(a,a,j,m) * tovov(m, d, k, a)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(k, e, i, d)
term(4) = term(4) + t2(a,e,j,i) * tovov(k, d, i, e)
term(5) = term(5) + t2(a,e,i,j) * tovov(k, e, i, d)
term(6) = term(6) + t2(a,e,i,j) * tovov(k, d, i, e)
end do 

term(3) = -term(3) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, k, d)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,j,m) * tovov(m, d, k, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, e, k, d)
end do 
end do 



    eom_cc3_22_tripletmp_trans_aiajakdi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aiajakdi = eom_cc3_22_tripletmp_trans_aiajakdi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajakdi
    function eom_cc3_22_tripletmp_trans_aibjbjal(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbjal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, a)

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, a, l, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,i,m) * tovov(m, b, l, a)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, a, l, b)
end do 

term(5) = -term(5) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,i) * tovov(l, b, j, e)
term(7) = term(7) + t2(b,e,j,i) * tovov(l, e, j, b)
term(8) = term(8) + t2(a,e,i,j) * tovov(l, a, j, e)
term(9) = term(9) + t2(a,e,i,j) * tovov(l, e, j, a)
end do 

term(7) = -term(7) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aibjbjal = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjbjal = eom_cc3_22_tripletmp_trans_aibjbjal + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbjal
    function eom_cc3_22_tripletmp_trans_aibibkal(t2, nocc, nactive, a, i, b, k, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibibkal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, l 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(l, b, k, e)
term(1) = term(1) + t2(b,e,i,i) * tovov(l, e, k, b)
term(2) = term(2) + t2(a,e,i,i) * tovov(l, a, k, e)
term(3) = term(3) + t2(a,e,i,i) * tovov(l, e, k, a)
end do 

term(1) = -term(1) 
term(3) = -term(3) 


    eom_cc3_22_tripletmp_trans_aibibkal = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aibibkal = eom_cc3_22_tripletmp_trans_aibibkal + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibibkal
    function eom_cc3_22_tripletmp_trans_aibjbkaj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbkaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, a)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, a, k, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(3) = term(3) * 2.0d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,i,m) * tovov(m, b, k, a)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, a, k, b)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,i) * tovov(k, e, j, b)
term(7) = term(7) + t2(b,e,j,i) * tovov(k, b, j, e)
term(8) = term(8) + t2(a,e,i,j) * tovov(k, e, j, a)
term(9) = term(9) + t2(a,e,i,j) * tovov(k, a, j, e)
end do 

term(7) = -term(7) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aibjbkaj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjbkaj = eom_cc3_22_tripletmp_trans_aibjbkaj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbkaj
    function eom_cc3_22_tripletmp_trans_aibjbial(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, l, b)

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, b, l, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, l, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, l, b)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,j) * tovov(m, b, l, a)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, a, l, b)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,i) * tovov(l, b, i, e)
term(7) = term(7) + t2(b,e,j,i) * tovov(l, e, i, b)
term(8) = term(8) + t2(a,e,i,j) * tovov(l, a, i, e)
term(9) = term(9) + t2(a,e,i,j) * tovov(l, e, i, a)
end do 

term(7) = -term(7) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aibjbial = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjbial = eom_cc3_22_tripletmp_trans_aibjbial + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbial
    function eom_cc3_22_tripletmp_trans_aibjbkai(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbkai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, b)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, b, k, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, k, b)
end do 
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, k, b)
end do 
end do 

term(3) = term(3) * 2.0d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,j) * tovov(m, b, k, a)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, a, k, b)
end do 

term(5) = -term(5) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,i) * tovov(k, e, i, b)
term(7) = term(7) + t2(b,e,j,i) * tovov(k, b, i, e)
term(8) = term(8) + t2(a,e,i,j) * tovov(k, e, i, a)
term(9) = term(9) + t2(a,e,i,j) * tovov(k, a, i, e)
end do 

term(7) = -term(7) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aibjbkai = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjbkai = eom_cc3_22_tripletmp_trans_aibjbkai + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbkai
    function eom_cc3_22_tripletmp_trans_aibibidl(t2, nocc, nactive, a, i, b, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibibidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, d)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, l, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, l, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, l, d)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, d, l, b)
end do 

term(1) = -term(1) 
term(4) = -term(4) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,i) * tovov(l, d, i, e)
term(6) = term(6) + t2(a,e,i,i) * tovov(l, e, i, d)
end do 

term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, d, l, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 



    eom_cc3_22_tripletmp_trans_aibibidl = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibibidl = eom_cc3_22_tripletmp_trans_aibibidl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibibidl
    function eom_cc3_22_tripletmp_trans_aibjbjdi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbjdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, i, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, i, b)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

term(4) = term(4) + tvoov(a, i, i, d)

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, d, i, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,j) * tovov(j, e, i, d)
term(9) = term(9) + t2(a,e,i,j) * tovov(j, d, i, e)
end do 

term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aibjbjdi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjbjdi = eom_cc3_22_tripletmp_trans_aibjbjdi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbjdi
    function eom_cc3_22_tripletmp_trans_aibibkdi(t2, nocc, nactive, a, i, b, k, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibibkdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, k, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, k, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, k, d)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, d, k, b)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,i) * tovov(k, e, i, d)
term(6) = term(6) + t2(a,e,i,i) * tovov(k, d, i, e)
end do 

term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, d, k, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, k, d)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aibibkdi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibibkdi = eom_cc3_22_tripletmp_trans_aibibkdi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibibkdi
    function eom_cc3_22_tripletmp_trans_aibjbidj(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, i, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, i, b)
end do 

term(0) = -term(0) 
term(2) = -term(2) 

term(4) = term(4) + tvoov(a, i, i, d)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, d, i, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,j) * tovov(j, d, i, e)
term(9) = term(9) + t2(a,e,i,j) * tovov(j, e, i, d)
end do 

term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aibjbidj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjbidj = eom_cc3_22_tripletmp_trans_aibjbidj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbidj
    function eom_cc3_22_tripletmp_trans_aiajcjdi(t2, nocc, nactive, a, i, j, c, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajcjdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, j, c)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, c, i, d)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, d, i, c)
end do 

term(1) = -term(1) 
term(3) = -term(3) 


    eom_cc3_22_tripletmp_trans_aiajcjdi = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aiajcjdi = eom_cc3_22_tripletmp_trans_aiajcjdi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajcjdi
    function eom_cc3_22_tripletmp_trans_aiajcidj(t2, nocc, nactive, a, i, j, c, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajcidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, j, c)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, c, i, d)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, d, i, c)
end do 

term(0) = -term(0) 
term(2) = -term(2) 


    eom_cc3_22_tripletmp_trans_aiajcidj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletmp_trans_aiajcidj = eom_cc3_22_tripletmp_trans_aiajcidj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajcidj
    function eom_cc3_22_tripletmp_trans_aibicial(t2, nocc, nactive, a, i, b, c, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibicial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, c)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, l, a)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, l, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, c, l, a)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, l, c)
end do 

term(1) = -term(1) 
term(4) = -term(4) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,i) * tovov(l, c, i, e)
term(6) = term(6) + t2(b,e,i,i) * tovov(l, e, i, c)
end do 

term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,i,m) * tovov(m, c, l, e)
term(9) = term(9) + t2(b,e,m,i) * tovov(m, e, l, c)
end do 
end do 



    eom_cc3_22_tripletmp_trans_aibicial = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibicial = eom_cc3_22_tripletmp_trans_aibicial + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibicial
    function eom_cc3_22_tripletmp_trans_aibjcjai(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjcjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, e, i, c)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, c, i, e)
end do 

term(1) = -term(1) 

term(2) = term(2) + tvoov(b, j, j, c)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, c, j, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, j, c)
end do 
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(5) = term(5) * 2.0d+0 

do m = 1, nocc 
term(6) = term(6) + t2(a,b,m,j) * tovov(m, c, j, a)
term(7) = term(7) + t2(a,b,m,j) * tovov(m, a, j, c)
term(8) = term(8) + t2(a,b,i,m) * tovov(m, c, i, a)
term(9) = term(9) + t2(a,b,i,m) * tovov(m, a, i, c)
end do 

term(7) = -term(7) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aibjcjai = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjcjai = eom_cc3_22_tripletmp_trans_aibjcjai + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjcjai
    function eom_cc3_22_tripletmp_trans_aibickai(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aibickai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, c)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, k, a)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, k, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, c, k, a)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, k, c)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,i) * tovov(k, e, i, c)
term(6) = term(6) + t2(b,e,i,i) * tovov(k, c, i, e)
end do 

term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,i,m) * tovov(m, c, k, e)
term(9) = term(9) + t2(b,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aibickai = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibickai = eom_cc3_22_tripletmp_trans_aibickai + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibickai
    function eom_cc3_22_tripletmp_trans_aibjciaj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjciaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, c, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, i, c)
end do 

term(1) = -term(1) 

term(2) = term(2) + tvoov(b, j, j, c)

term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, c, j, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, j, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,b,m,j) * tovov(m, c, j, a)
term(7) = term(7) + t2(a,b,m,j) * tovov(m, a, j, c)
term(8) = term(8) + t2(a,b,i,m) * tovov(m, c, i, a)
term(9) = term(9) + t2(a,b,i,m) * tovov(m, a, i, c)
end do 

term(6) = -term(6) 
term(8) = -term(8) 


    eom_cc3_22_tripletmp_trans_aibjciaj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjciaj = eom_cc3_22_tripletmp_trans_aibjciaj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjciaj
    function eom_cc3_22_tripletmp_trans_aibiaidl(t2, nocc, nactive, a, i, b, d, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibiaidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, l, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, l, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, l, d)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, d, l, a)
end do 

term(1) = -term(1) 
term(4) = -term(4) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,i) * tovov(l, d, i, e)
term(6) = term(6) + t2(b,e,i,i) * tovov(l, e, i, d)
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,i,m) * tovov(m, d, l, e)
term(9) = term(9) + t2(b,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aibiaidl = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibiaidl = eom_cc3_22_tripletmp_trans_aibiaidl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibiaidl
    function eom_cc3_22_tripletmp_trans_aibjajdi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjajdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, e, i, d)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, d, i, e)
end do 

term(0) = -term(0) 

term(2) = term(2) + tvoov(b, j, j, d)

term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, d, j, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, j, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,b,m,j) * tovov(m, a, j, d)
term(7) = term(7) + t2(a,b,m,j) * tovov(m, d, j, a)
term(8) = term(8) + t2(a,b,i,m) * tovov(m, a, i, d)
term(9) = term(9) + t2(a,b,i,m) * tovov(m, d, i, a)
end do 

term(7) = -term(7) 
term(9) = -term(9) 


    eom_cc3_22_tripletmp_trans_aibjajdi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjajdi = eom_cc3_22_tripletmp_trans_aibjajdi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjajdi
    function eom_cc3_22_tripletmp_trans_aibiakdi(t2, nocc, nactive, a, i, b, k, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibiakdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, d)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, k, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, k, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, k, d)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, d, k, a)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,i) * tovov(k, e, i, d)
term(6) = term(6) + t2(b,e,i,i) * tovov(k, d, i, e)
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,i,m) * tovov(m, d, k, e)
term(9) = term(9) + t2(b,e,m,i) * tovov(m, e, k, d)
end do 
end do 



    eom_cc3_22_tripletmp_trans_aibiakdi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibiakdi = eom_cc3_22_tripletmp_trans_aibiakdi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibiakdi
    function eom_cc3_22_tripletmp_trans_aibjaidj(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjaidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, d, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, i, d)
end do 

term(0) = -term(0) 

term(2) = term(2) + tvoov(b, j, j, d)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, d, j, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, j, d)
end do 
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(5) = term(5) * 2.0d+0 

do m = 1, nocc 
term(6) = term(6) + t2(a,b,m,j) * tovov(m, a, j, d)
term(7) = term(7) + t2(a,b,m,j) * tovov(m, d, j, a)
term(8) = term(8) + t2(a,b,i,m) * tovov(m, a, i, d)
term(9) = term(9) + t2(a,b,i,m) * tovov(m, d, i, a)
end do 

term(6) = -term(6) 
term(8) = -term(8) 


    eom_cc3_22_tripletmp_trans_aibjaidj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletmp_trans_aibjaidj = eom_cc3_22_tripletmp_trans_aibjaidj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjaidj
    function eom_cc3_22_tripletmp_trans_aiajcjai(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajcjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, a, j, c)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, c, i, a)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, a, i, c)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,j,m) * tovov(m, c, j, e)
term(5) = term(5) + t2(a,e,m,j) * tovov(m, e, j, c)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, c, i, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = -term(7) 

do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,j,i) * tovov(j, e, i, c)
term(9) = term(9) + t2(a,e,j,i) * tovov(j, c, i, e)
term(10) = term(10) + t2(a,e,i,j) * tovov(j, e, i, c)
term(11) = term(11) + t2(a,e,i,j) * tovov(j, c, i, e)
end do 

term(9) = -term(9) 
term(10) = -term(10) 

term(12) = term(12) + tvoov(a, j, j, c)
term(13) = term(13) + tvoov(a, i, i, c)


do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(a,e,j,m) * tovov(m, e, j, c)
term(15) = term(15) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(14) = term(14) * 2.0d+0 
term(15) = term(15) * 2.0d+0 


    eom_cc3_22_tripletmp_trans_aiajcjai = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletmp_trans_aiajcjai = eom_cc3_22_tripletmp_trans_aiajcjai + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajcjai
    function eom_cc3_22_tripletmp_trans_aiajciaj(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajciaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, a, j, c)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, c, i, a)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, a, i, c)
end do 

term(0) = -term(0) 
term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,j,m) * tovov(m, c, j, e)
term(5) = term(5) + t2(a,e,m,j) * tovov(m, e, j, c)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, c, i, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 


do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,j,i) * tovov(j, c, i, e)
term(9) = term(9) + t2(a,e,j,i) * tovov(j, e, i, c)
term(10) = term(10) + t2(a,e,i,j) * tovov(j, c, i, e)
term(11) = term(11) + t2(a,e,i,j) * tovov(j, e, i, c)
end do 

term(9) = -term(9) 
term(10) = -term(10) 

term(12) = term(12) + tvoov(a, j, j, c)
term(13) = term(13) + tvoov(a, i, i, c)

term(12) = -term(12) 
term(13) = -term(13) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(a,e,j,m) * tovov(m, e, j, c)
term(15) = term(15) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (-2.0d+0) 


    eom_cc3_22_tripletmp_trans_aiajciaj = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletmp_trans_aiajciaj = eom_cc3_22_tripletmp_trans_aiajciaj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajciaj
    function eom_cc3_22_tripletmp_trans_aibiaibl(t2, nocc, nactive, a, i, b, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibiaibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,e,m 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, b)
term(1) = term(1) + tvoov(a, i, l, a)


do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, l, b)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, b, l, a)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, l, b)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, b, l, a)
end do 

term(2) = -term(2) 
term(5) = -term(5) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, b, l, e)
term(7) = term(7) + t2(b,e,m,i) * tovov(m, e, l, b)
term(8) = term(8) + t2(a,e,i,m) * tovov(m, a, l, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 

term(6) = -term(6) 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = -term(9) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,m) * tovov(m, e, l, b)
term(11) = term(11) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(10) = term(10) * 2.0d+0 
term(11) = term(11) * 2.0d+0 

do e = nocc + 1, nactive 
term(12) = term(12) + t2(b,e,i,i) * tovov(l, b, i, e)
term(13) = term(13) + t2(b,e,i,i) * tovov(l, e, i, b)
term(14) = term(14) + t2(a,e,i,i) * tovov(l, a, i, e)
term(15) = term(15) + t2(a,e,i,i) * tovov(l, e, i, a)
end do 

term(12) = -term(12) 
term(14) = -term(14) 


    eom_cc3_22_tripletmp_trans_aibiaibl = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletmp_trans_aibiaibl = eom_cc3_22_tripletmp_trans_aibiaibl + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibiaibl
    function eom_cc3_22_tripletmp_trans_aibjajbi(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjajbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, i, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, a)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(b,e,j,i) * tovov(j, e, i, b)
term(5) = term(5) + t2(b,e,j,i) * tovov(j, b, i, e)
term(6) = term(6) + t2(a,e,i,j) * tovov(j, e, i, a)
term(7) = term(7) + t2(a,e,i,j) * tovov(j, a, i, e)
end do 

term(4) = -term(4) 
term(6) = -term(6) 

term(8) = term(8) + tvoov(b, j, j, b)
term(9) = term(9) + tvoov(a, i, i, a)

term(8) = -term(8) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(b,e,j,m) * tovov(m, b, j, e)
term(11) = term(11) + t2(b,e,m,j) * tovov(m, e, j, b)
term(12) = term(12) + t2(a,e,i,m) * tovov(m, a, i, e)
term(13) = term(13) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(12) = -term(12) 
term(13) = -term(13) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(b,e,j,m) * tovov(m, e, j, b)
term(15) = term(15) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * 2.0d+0 


    eom_cc3_22_tripletmp_trans_aibjajbi = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletmp_trans_aibjajbi = eom_cc3_22_tripletmp_trans_aibjajbi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjajbi
    function eom_cc3_22_tripletmp_trans_aibiakbi(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aibiakbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, b)
term(1) = term(1) + tvoov(a, i, k, a)

term(0) = -term(0) 
term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, k, b)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, b, k, a)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, k, b)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, b, k, a)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, b, k, e)
term(7) = term(7) + t2(b,e,m,i) * tovov(m, e, k, b)
term(8) = term(8) + t2(a,e,i,m) * tovov(m, a, k, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,m) * tovov(m, e, k, b)
term(11) = term(11) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(12) = term(12) + t2(b,e,i,i) * tovov(k, e, i, b)
term(13) = term(13) + t2(b,e,i,i) * tovov(k, b, i, e)
term(14) = term(14) + t2(a,e,i,i) * tovov(k, e, i, a)
term(15) = term(15) + t2(a,e,i,i) * tovov(k, a, i, e)
end do 

term(12) = -term(12) 
term(14) = -term(14) 


    eom_cc3_22_tripletmp_trans_aibiakbi = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletmp_trans_aibiakbi = eom_cc3_22_tripletmp_trans_aibiakbi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibiakbi
    function eom_cc3_22_tripletmp_trans_aibjaibj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjaibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, i, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, a)
end do 

term(0) = -term(0) 
term(2) = -term(2) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(b,e,j,i) * tovov(j, b, i, e)
term(5) = term(5) + t2(b,e,j,i) * tovov(j, e, i, b)
term(6) = term(6) + t2(a,e,i,j) * tovov(j, a, i, e)
term(7) = term(7) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 

term(4) = -term(4) 
term(6) = -term(6) 

term(8) = term(8) + tvoov(b, j, j, b)
term(9) = term(9) + tvoov(a, i, i, a)

term(9) = -term(9) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(b,e,j,m) * tovov(m, b, j, e)
term(11) = term(11) + t2(b,e,m,j) * tovov(m, e, j, b)
term(12) = term(12) + t2(a,e,i,m) * tovov(m, a, i, e)
term(13) = term(13) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(b,e,j,m) * tovov(m, e, j, b)
term(15) = term(15) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(14) = term(14) * 2.0d+0 
term(15) = term(15) * (-2.0d+0) 


    eom_cc3_22_tripletmp_trans_aibjaibj = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletmp_trans_aibjaibj = eom_cc3_22_tripletmp_trans_aibjaibj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjaibj
    function eom_cc3_22_tripletmp_trans_aiajajdi(t2, nocc, nactive, a, i, j, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajajdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, j, a)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, i, d)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, d, i, a)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,j,m) * tovov(m, d, j, e)
term(5) = term(5) + t2(a,e,m,j) * tovov(m, e, j, d)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, d, i, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 


do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,j,i) * tovov(j, e, i, d)
term(9) = term(9) + t2(a,e,j,i) * tovov(j, d, i, e)
term(10) = term(10) + t2(a,e,i,j) * tovov(j, e, i, d)
term(11) = term(11) + t2(a,e,i,j) * tovov(j, d, i, e)
end do 

term(8) = -term(8) 
term(11) = -term(11) 

term(12) = term(12) + tvoov(a, j, j, d)
term(13) = term(13) + tvoov(a, i, i, d)

term(12) = -term(12) 
term(13) = -term(13) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(a,e,j,m) * tovov(m, e, j, d)
term(15) = term(15) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (-2.0d+0) 


    eom_cc3_22_tripletmp_trans_aiajajdi = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletmp_trans_aiajajdi = eom_cc3_22_tripletmp_trans_aiajajdi + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajajdi
    function eom_cc3_22_tripletmp_trans_aiajaidj(t2, nocc, nactive, a, i, j, d) 
    double precision :: eom_cc3_22_tripletmp_trans_aiajaidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, j, a)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, i, d)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, d, i, a)
end do 

term(0) = -term(0) 
term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,j,m) * tovov(m, d, j, e)
term(5) = term(5) + t2(a,e,m,j) * tovov(m, e, j, d)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, d, i, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = -term(7) 

do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,j,i) * tovov(j, d, i, e)
term(9) = term(9) + t2(a,e,j,i) * tovov(j, e, i, d)
term(10) = term(10) + t2(a,e,i,j) * tovov(j, d, i, e)
term(11) = term(11) + t2(a,e,i,j) * tovov(j, e, i, d)
end do 

term(8) = -term(8) 
term(11) = -term(11) 

term(12) = term(12) + tvoov(a, j, j, d)
term(13) = term(13) + tvoov(a, i, i, d)


do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(a,e,j,m) * tovov(m, e, j, d)
term(15) = term(15) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(14) = term(14) * 2.0d+0 
term(15) = term(15) * 2.0d+0 


    eom_cc3_22_tripletmp_trans_aiajaidj = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletmp_trans_aiajaidj = eom_cc3_22_tripletmp_trans_aiajaidj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aiajaidj
    function eom_cc3_22_tripletmp_trans_aibibial(t2, nocc, nactive, a, i, b, l) 
    double precision :: eom_cc3_22_tripletmp_trans_aibibial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,e,m 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, b)
term(1) = term(1) + tvoov(a, i, l, a)

term(0) = -term(0) 
term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, l, a)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, a, l, b)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, b, l, a)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, a, l, b)
end do 

term(2) = -term(2) 
term(5) = -term(5) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, b, l, e)
term(7) = term(7) + t2(b,e,m,i) * tovov(m, e, l, b)
term(8) = term(8) + t2(a,e,i,m) * tovov(m, a, l, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,m) * tovov(m, e, l, b)
term(11) = term(11) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(12) = term(12) + t2(b,e,i,i) * tovov(l, b, i, e)
term(13) = term(13) + t2(b,e,i,i) * tovov(l, e, i, b)
term(14) = term(14) + t2(a,e,i,i) * tovov(l, a, i, e)
term(15) = term(15) + t2(a,e,i,i) * tovov(l, e, i, a)
end do 

term(13) = -term(13) 
term(15) = -term(15) 


    eom_cc3_22_tripletmp_trans_aibibial = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletmp_trans_aibibial = eom_cc3_22_tripletmp_trans_aibibial + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibibial
    function eom_cc3_22_tripletmp_trans_aibjbjai(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, j, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, i, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, i, b)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(b,e,j,i) * tovov(j, e, i, b)
term(5) = term(5) + t2(b,e,j,i) * tovov(j, b, i, e)
term(6) = term(6) + t2(a,e,i,j) * tovov(j, e, i, a)
term(7) = term(7) + t2(a,e,i,j) * tovov(j, a, i, e)
end do 

term(5) = -term(5) 
term(7) = -term(7) 

term(8) = term(8) + tvoov(b, j, j, b)
term(9) = term(9) + tvoov(a, i, i, a)

term(9) = -term(9) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(b,e,j,m) * tovov(m, b, j, e)
term(11) = term(11) + t2(b,e,m,j) * tovov(m, e, j, b)
term(12) = term(12) + t2(a,e,i,m) * tovov(m, a, i, e)
term(13) = term(13) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(b,e,j,m) * tovov(m, e, j, b)
term(15) = term(15) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(14) = term(14) * 2.0d+0 
term(15) = term(15) * (-2.0d+0) 


    eom_cc3_22_tripletmp_trans_aibjbjai = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletmp_trans_aibjbjai = eom_cc3_22_tripletmp_trans_aibjbjai + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbjai
    function eom_cc3_22_tripletmp_trans_aibibkai(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_22_tripletmp_trans_aibibkai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, b)
term(1) = term(1) + tvoov(a, i, k, a)


do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, k, a)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, a, k, b)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, b, k, a)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, a, k, b)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, b, k, e)
term(7) = term(7) + t2(b,e,m,i) * tovov(m, e, k, b)
term(8) = term(8) + t2(a,e,i,m) * tovov(m, a, k, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(6) = -term(6) 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = -term(9) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,m) * tovov(m, e, k, b)
term(11) = term(11) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(10) = term(10) * 2.0d+0 
term(11) = term(11) * 2.0d+0 

do e = nocc + 1, nactive 
term(12) = term(12) + t2(b,e,i,i) * tovov(k, e, i, b)
term(13) = term(13) + t2(b,e,i,i) * tovov(k, b, i, e)
term(14) = term(14) + t2(a,e,i,i) * tovov(k, e, i, a)
term(15) = term(15) + t2(a,e,i,i) * tovov(k, a, i, e)
end do 

term(13) = -term(13) 
term(15) = -term(15) 


    eom_cc3_22_tripletmp_trans_aibibkai = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletmp_trans_aibibkai = eom_cc3_22_tripletmp_trans_aibibkai + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibibkai
    function eom_cc3_22_tripletmp_trans_aibjbiaj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletmp_trans_aibjbiaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, j, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, i, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, i, b)
end do 

term(0) = -term(0) 
term(2) = -term(2) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(b,e,j,i) * tovov(j, b, i, e)
term(5) = term(5) + t2(b,e,j,i) * tovov(j, e, i, b)
term(6) = term(6) + t2(a,e,i,j) * tovov(j, a, i, e)
term(7) = term(7) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 

term(5) = -term(5) 
term(7) = -term(7) 

term(8) = term(8) + tvoov(b, j, j, b)
term(9) = term(9) + tvoov(a, i, i, a)

term(8) = -term(8) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(b,e,j,m) * tovov(m, b, j, e)
term(11) = term(11) + t2(b,e,m,j) * tovov(m, e, j, b)
term(12) = term(12) + t2(a,e,i,m) * tovov(m, a, i, e)
term(13) = term(13) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(12) = -term(12) 
term(13) = -term(13) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(b,e,j,m) * tovov(m, e, j, b)
term(15) = term(15) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * 2.0d+0 


    eom_cc3_22_tripletmp_trans_aibjbiaj = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletmp_trans_aibjbiaj = eom_cc3_22_tripletmp_trans_aibjbiaj + term(s)
    end do

    end function eom_cc3_22_tripletmp_trans_aibjbiaj
    end module eom_cc3_22_tripletmp_trans
    
