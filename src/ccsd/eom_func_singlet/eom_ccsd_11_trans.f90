module eom_ccsd_11_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_ccsd_11_trans_aiaj(t2, nocc, nactive, a, i, j) 
    double precision :: eom_ccsd_11_trans_aiaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,d,k,c 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, j, i)
term(1) = term(1) + tvoov(a, i, j, a)
term(2) = term(2) + too(j, i)

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = -term(2) 

do d = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
term(3) = term(3) + t2(c,d,i,k) * tovov(k, c, j, d)
end do 
end do 
end do 


do c = nocc + 1, nactive 
do k = 1, nocc 
do d = nocc + 1, nactive 
term(4) = term(4) + t2(c,d,i,k) * tovov(k, d, j, c)
end do 
end do 
end do 

term(4) = term(4) * (-2.0d+0) 

do c = nocc + 1, nactive 
do k = 1, nocc 
term(5) = term(5) + t2(a,c,k,i) * tovov(k, a, j, c)
term(6) = term(6) + t2(a,c,i,k) * tovov(k, a, j, c)
term(7) = term(7) + t2(a,c,k,i) * tovov(k, c, j, a)
end do 
end do 

term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 

do k = 1, nocc 
do c = nocc + 1, nactive 
term(8) = term(8) + t2(a,c,i,k) * tovov(k, c, j, a)
end do 
end do 

term(8) = term(8) * 4.0d+0 

do k = 1, nocc 
term(9) = term(9) + toooo(k, i, j, k)
term(10) = term(10) + toooo(k, k, j, i)
end do 

term(10) = term(10) * (-2.0d+0) 


    eom_ccsd_11_trans_aiaj = 0.d+0
    do s = 0, 10
    eom_ccsd_11_trans_aiaj = eom_ccsd_11_trans_aiaj + term(s)
    end do

    end function eom_ccsd_11_trans_aiaj
    function eom_ccsd_11_trans_aibi(t2, nocc, nactive, a, i, b) 
    double precision :: eom_ccsd_11_trans_aibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,k,l,c 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    do k = 1, nocc 
term(0) = term(0) + tvvoo(a, b, k, k)
term(1) = term(1) + tvoov(a, k, k, b)
end do 

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 

do l = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
term(2) = term(2) + t2(a,c,k,l) * tovov(l, c, k, b)
end do 
end do 
end do 

term(2) = term(2) * (-2.0d+0) 

do c = nocc + 1, nactive 
do l = 1, nocc 
do k = 1, nocc 
term(3) = term(3) + t2(a,c,k,l) * tovov(l, b, k, c)
end do 
end do 
end do 


do c = nocc + 1, nactive 
do k = 1, nocc 
term(4) = term(4) + t2(a,c,k,i) * tovov(k, b, i, c)
term(5) = term(5) + t2(a,c,i,k) * tovov(k, b, i, c)
term(6) = term(6) + t2(a,c,k,i) * tovov(k, c, i, b)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (-2.0d+0) 

do k = 1, nocc 
do c = nocc + 1, nactive 
term(7) = term(7) + t2(a,c,i,k) * tovov(k, c, i, b)
end do 
end do 

term(7) = term(7) * 4.0d+0 

term(8) = term(8) + tvv(a, b)
term(9) = term(9) + tvvoo(a, b, i, i)
term(10) = term(10) + tvoov(a, i, i, b)

term(9) = -term(9) 
term(10) = term(10) * 2.0d+0 


    eom_ccsd_11_trans_aibi = 0.d+0
    do s = 0, 10
    eom_ccsd_11_trans_aibi = eom_ccsd_11_trans_aibi + term(s)
    end do

    end function eom_ccsd_11_trans_aibi
    function eom_ccsd_11_trans_aiai(t2, nocc, nactive, a, i) 
    double precision :: eom_ccsd_11_trans_aiai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i 
    integer :: s ,k,c,d,l 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do k = 1, nocc 
term(0) = term(0) + toooo(k, i, i, k)
term(1) = term(1) + toooo(k, k, i, i)
term(2) = term(2) + tvvoo(a, a, k, k)
term(3) = term(3) + tvoov(a, k, k, a)
end do 

term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 

term(4) = term(4) + tvv(a, a)
term(5) = term(5) + too(i, i)
term(6) = term(6) + tvvoo(a, a, i, i)
term(7) = term(7) + tvoov(a, i, i, a)

term(5) = -term(5) 
term(6) = -term(6) 
term(7) = term(7) * 2.0d+0 

do c = nocc + 1, nactive 
do k = 1, nocc 
term(8) = term(8) + t2(a,c,k,i) * tovov(k, a, i, c)
term(9) = term(9) + t2(a,c,i,k) * tovov(k, a, i, c)
term(10) = term(10) + t2(a,c,k,i) * tovov(k, c, i, a)
end do 
end do 

term(9) = term(9) * (-2.0d+0) 
term(10) = term(10) * (-2.0d+0) 

do k = 1, nocc 
do c = nocc + 1, nactive 
term(11) = term(11) + t2(a,c,i,k) * tovov(k, c, i, a)
end do 
end do 

term(11) = term(11) * 4.0d+0 

do d = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
term(12) = term(12) + t2(c,d,i,k) * tovov(k, c, i, d)
end do 
end do 
end do 


do c = nocc + 1, nactive 
do k = 1, nocc 
do d = nocc + 1, nactive 
term(13) = term(13) + t2(c,d,i,k) * tovov(k, d, i, c)
end do 
end do 
end do 

term(13) = term(13) * (-2.0d+0) 

do l = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
term(14) = term(14) + t2(a,c,k,l) * tovov(l, c, k, a)
end do 
end do 
end do 

term(14) = term(14) * (-2.0d+0) 

do c = nocc + 1, nactive 
do l = 1, nocc 
do k = 1, nocc 
term(15) = term(15) + t2(a,c,k,l) * tovov(l, a, k, c)
end do 
end do 
end do 



    eom_ccsd_11_trans_aiai = 0.d+0
    do s = 0, 15
    eom_ccsd_11_trans_aiai = eom_ccsd_11_trans_aiai + term(s)
    end do

    end function eom_ccsd_11_trans_aiai
    function eom_ccsd_11_trans_aibj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_ccsd_11_trans_aibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,c,k 
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, j, i)
term(1) = term(1) + tvoov(a, i, j, b)

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 

do c = nocc + 1, nactive 
do k = 1, nocc 
term(2) = term(2) + t2(a,c,k,i) * tovov(k, b, j, c)
term(3) = term(3) + t2(a,c,i,k) * tovov(k, b, j, c)
term(4) = term(4) + t2(a,c,k,i) * tovov(k, c, j, b)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-2.0d+0) 

do k = 1, nocc 
do c = nocc + 1, nactive 
term(5) = term(5) + t2(a,c,i,k) * tovov(k, c, j, b)
end do 
end do 

term(5) = term(5) * 4.0d+0 


    eom_ccsd_11_trans_aibj = 0.d+0
    do s = 0, 5
    eom_ccsd_11_trans_aibj = eom_ccsd_11_trans_aibj + term(s)
    end do

    end function eom_ccsd_11_trans_aibj
    end module eom_ccsd_11_trans
    
