module eom_cc3_21_tripletm_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_cc3_21_tripletm_trans_aibjak(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_21_tripletm_trans_aibjak  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j, k 
    integer :: s ,l,e,d 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(0) = term(0) + t3(nocc, nactive, b,d,e,j,i,l) * tovov(l, d, k, e)
end do 
end do 
end do 

term(0) = term(0) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(1) = term(1) + t3(nocc, nactive, b,d,e,l,i,j) * tovov(l, e, k, d)
term(2) = term(2) + t3(nocc, nactive, b,d,e,j,i,l) * tovov(l, e, k, d)
end do 
end do 
end do 

term(1) = term(1) * 0.5d+0 
term(2) = -term(2) 

do d = nocc + 1, nactive 
term(3) = term(3) + t2(b,d,j,i) * tvvov(a, a, k, d)
term(4) = term(4) + t2(a,d,i,j) * tvvov(b, a, k, d)
term(5) = term(5) + tov(k, d) * t2(b,d,j,i)
end do 

term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5d+0) 

do l = 1, nocc 
term(6) = term(6) + t2(a,b,l,j) * tovoo(l, a, k, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(l, a, k, j)
end do 

term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(8) = term(8) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, a, k, d)
term(9) = term(9) + t2(b,d,j,l) * tovoo(k, d, l, i)
term(10) = term(10) + t2(b,d,j,i) * tovoo(k, d, l, l)
term(11) = term(11) + t2(b,d,j,l) * tovoo(l, d, k, i)
term(12) = term(12) + t2(b,d,j,i) * tovoo(l, d, k, l)
end do 
end do 

term(8) = term(8) * 0.5d+0 
term(9) = term(9) * 0.5d+0 
term(10) = -term(10) 
term(11) = -term(11) 
term(12) = term(12) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(13) = term(13) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, a, k, d)
term(14) = term(14) + t2(b,d,l,i) * tovoo(k, d, l, j)
term(15) = term(15) + t2(b,d,l,j) * tovoo(l, d, k, i)
end do 
end do 

term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5d+0 
term(15) = term(15) * 0.5d+0 

term(16) = term(16) + tvooo(b, j, k, i)

term(16) = term(16) * (-0.5d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(17) = term(17) + t2(d,e,i,j) * tvvov(b, e, k, d)
end do 
end do 

term(17) = term(17) * (-0.5d+0) 


    eom_cc3_21_tripletm_trans_aibjak = triple_w1 + triple_w2
    do s = 0, 17
    eom_cc3_21_tripletm_trans_aibjak = eom_cc3_21_tripletm_trans_aibjak + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibjak
    
    function eom_cc3_21_tripletm_trans_aibjbk(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_21_tripletm_trans_aibjbk  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j, k 
    integer :: s ,l,e,d 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(0) = term(0) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, d, k, e)
term(1) = term(1) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, d, k, e)
end do 
end do 
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(2) = term(2) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, e, k, d)
end do 
end do 
end do 


do d = nocc + 1, nactive 
term(3) = term(3) + t2(b,d,j,i) * tvvov(a, b, k, d)
term(4) = term(4) + t2(a,d,i,j) * tvvov(b, b, k, d)
term(5) = term(5) + tov(k, d) * t2(a,d,i,j)
end do 

term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * 0.5d+0 
term(5) = term(5) * 0.5d+0 

do l = 1, nocc 
term(6) = term(6) + t2(a,b,l,j) * tovoo(l, b, k, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(l, b, k, j)
end do 

term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(8) = term(8) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, b, k, d)
term(9) = term(9) + t2(a,d,i,l) * tovoo(k, d, l, j)
term(10) = term(10) + t2(a,d,i,j) * tovoo(k, d, l, l)
term(11) = term(11) + t2(a,d,i,l) * tovoo(l, d, k, j)
term(12) = term(12) + t2(a,d,i,j) * tovoo(l, d, k, l)
end do 
end do 

term(8) = term(8) * 0.5d+0 
term(9) = term(9) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(13) = term(13) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, b, k, d)
term(14) = term(14) + t2(a,d,l,j) * tovoo(k, d, l, i)
term(15) = term(15) + t2(a,d,l,i) * tovoo(l, d, k, j)
end do 
end do 

term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 

term(16) = term(16) + tvooo(a, i, k, j)

term(16) = term(16) * 0.5d+0 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(17) = term(17) + t2(d,e,i,j) * tvvov(a, d, k, e)
end do 
end do 

term(17) = term(17) * 0.5d+0 


    eom_cc3_21_tripletm_trans_aibjbk = triple_w1 + triple_w2
    do s = 0, 17
    eom_cc3_21_tripletm_trans_aibjbk = eom_cc3_21_tripletm_trans_aibjbk + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibjbk
    
    function eom_cc3_21_tripletm_trans_aiajck(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_21_tripletm_trans_aiajck  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, j, c, k 
    integer :: s ,d,l 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,j,i) * tvvov(a, c, k, d)
term(1) = term(1) + t2(a,d,i,j) * tvvov(a, c, k, d)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * 0.5d+0 

do l = 1, nocc 
term(2) = term(2) + t2(a,a,j,l) * tovoo(l, c, k, i)
term(3) = term(3) + t2(a,a,i,l) * tovoo(l, c, k, j)
end do 

term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(4) = term(4) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, c, k, d)
term(5) = term(5) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, c, k, d)
end do 
end do 

term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5d+0) 


    eom_cc3_21_tripletm_trans_aiajck = triple_w1 + triple_w2
    do s = 0, 5
    eom_cc3_21_tripletm_trans_aiajck = eom_cc3_21_tripletm_trans_aiajck + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aiajck
    
    function eom_cc3_21_tripletm_trans_aibick(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_21_tripletm_trans_aibick  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, c, k 
    integer :: s ,l,d 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    do l = 1, nocc 
do d = nocc + 1, nactive 
term(0) = term(0) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, c, k, d)
end do 
end do 

term(0) = term(0) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(1) = term(1) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, c, k, d)
end do 
end do 

term(1) = term(1) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(2) = term(2) + t2(b,d,i,i) * tvvov(a, c, k, d)
term(3) = term(3) + t2(a,d,i,i) * tvvov(b, c, k, d)
end do 

term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * 0.5d+0 

do l = 1, nocc 
term(4) = term(4) + t2(a,b,l,i) * tovoo(l, c, k, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(l, c, k, i)
end do 

term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5d+0) 


    eom_cc3_21_tripletm_trans_aibick = triple_w1 + triple_w2
    do s = 0, 5
    eom_cc3_21_tripletm_trans_aibick = eom_cc3_21_tripletm_trans_aibick + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibick
    
    function eom_cc3_21_tripletm_trans_aibjci(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_21_tripletm_trans_aibjci  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j, c 
    integer :: s ,l,d,m 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    do l = 1, nocc 
do d = nocc + 1, nactive 
term(0) = term(0) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, c, i, d)
term(1) = term(1) + t2(b,d,j,l) * tvvov(a, d, l, c)
end do 
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(2) = term(2) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, c, i, d)
term(3) = term(3) + t2(a,d,l,j) * tvvov(b, d, l, c)
term(4) = term(4) + t2(b,d,j,l) * tvvov(a, c, l, d)
term(5) = term(5) + t2(b,d,l,j) * tvvov(a, c, l, d)
end do 
end do 

term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 

do l = 1, nocc 
term(6) = term(6) + tov(l, c) * t2(a,b,l,j)
term(7) = term(7) + t2(a,b,l,j) * tovoo(l, c, i, i)
term(8) = term(8) + t2(a,b,i,l) * tovoo(l, c, i, j)
end do 

term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * 0.5d+0 
term(8) = term(8) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(9) = term(9) + t2(a,b,l,m) * tovoo(l, c, m, j)
term(10) = term(10) + t2(a,b,l,j) * tovoo(l, c, m, m)
term(11) = term(11) + t2(a,b,l,j) * tovoo(m, c, l, m)
end do 
end do 

term(9) = term(9) * 0.5d+0 
term(10) = -term(10) 
term(11) = term(11) * 0.5d+0 

term(12) = term(12) + tvvvo(a, c, b, j)

term(12) = term(12) * 0.5d+0 

do d = nocc + 1, nactive 
term(13) = term(13) + t2(b,d,j,i) * tvvov(a, c, i, d)
term(14) = term(14) + t2(a,d,i,j) * tvvov(b, c, i, d)
end do 

term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5d+0 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(15) = term(15) + t3(nocc, nactive, a,b,d,l,m,j) * tovov(m, d, l, c)
end do 
end do 
end do 

term(15) = term(15) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(16) = term(16) + t3(nocc, nactive, a,b,d,l,j,m) * tovov(m, d, l, c)
term(17) = term(17) + t3(nocc, nactive, a,b,d,l,j,m) * tovov(m, c, l, d)
end do 
end do 
end do 

term(16) = -term(16) 
term(17) = term(17) * 0.5d+0 


    eom_cc3_21_tripletm_trans_aibjci = triple_w1 + triple_w2
    do s = 0, 17
    eom_cc3_21_tripletm_trans_aibjci = eom_cc3_21_tripletm_trans_aibjci + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibjci
    
    function eom_cc3_21_tripletm_trans_aibjcj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_21_tripletm_trans_aibjcj  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j, c 
    integer :: s ,l,d,m 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    do l = 1, nocc 
do d = nocc + 1, nactive 
term(0) = term(0) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, c, j, d)
term(1) = term(1) + t2(a,d,i,l) * tvvov(b, d, l, c)
end do 
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(2) = term(2) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, c, j, d)
term(3) = term(3) + t2(b,d,l,i) * tvvov(a, d, l, c)
term(4) = term(4) + t2(a,d,i,l) * tvvov(b, c, l, d)
term(5) = term(5) + t2(a,d,l,i) * tvvov(b, c, l, d)
end do 
end do 

term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * 0.5d+0 
term(4) = -term(4) 
term(5) = term(5) * 0.5d+0 

do l = 1, nocc 
term(6) = term(6) + tov(l, c) * t2(a,b,i,l)
term(7) = term(7) + t2(a,b,l,j) * tovoo(l, c, j, i)
term(8) = term(8) + t2(a,b,i,l) * tovoo(l, c, j, j)
end do 

term(6) = term(6) * 0.5d+0 
term(7) = term(7) * 0.5d+0 
term(8) = term(8) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(9) = term(9) + t2(a,b,l,m) * tovoo(m, c, l, i)
term(10) = term(10) + t2(a,b,i,l) * tovoo(l, c, m, m)
term(11) = term(11) + t2(a,b,i,l) * tovoo(m, c, l, m)
end do 
end do 

term(9) = term(9) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(12) = term(12) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, c, l, d)
end do 
end do 
end do 

term(12) = term(12) * (-0.5d+0) 

term(13) = term(13) + tvvvo(b, c, a, i)

term(13) = term(13) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(14) = term(14) + t2(b,d,j,i) * tvvov(a, c, j, d)
term(15) = term(15) + t2(a,d,i,j) * tvvov(b, c, j, d)
end do 

term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(16) = term(16) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, c, l, d)
end do 
end do 
end do 

term(16) = term(16) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(17) = term(17) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, d, l, c)
end do 
end do 
end do 



    eom_cc3_21_tripletm_trans_aibjcj = triple_w1 + triple_w2
    do s = 0, 17
    eom_cc3_21_tripletm_trans_aibjcj = eom_cc3_21_tripletm_trans_aibjcj + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibjcj
    
    function eom_cc3_21_tripletm_trans_aiajak(t2, nocc, nactive, a, i, j, k) 
    double precision :: eom_cc3_21_tripletm_trans_aiajak  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, j, k 
    integer :: s ,l,e,d 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:29) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    term(0) = term(0) + tvooo(a, j, k, i)
term(1) = term(1) + tvooo(a, i, k, j)

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * 0.5d+0 

do d = nocc + 1, nactive 
term(2) = term(2) + t2(a,d,j,i) * tvvov(a, a, k, d)
term(3) = term(3) + t2(a,d,i,j) * tvvov(a, a, k, d)
term(4) = term(4) + tov(k, d) * t2(a,d,j,i)
term(5) = term(5) + tov(k, d) * t2(a,d,i,j)
end do 

term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * 0.5d+0 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * 0.5d+0 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(6) = term(6) + t3(nocc, nactive, a,d,e,j,i,l) * tovov(l, d, k, e)
term(7) = term(7) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, d, k, e)
term(8) = term(8) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, d, k, e)
end do 
end do 
end do 

term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(9) = term(9) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, e, k, d)
term(10) = term(10) + t3(nocc, nactive, a,d,e,j,i,l) * tovov(l, e, k, d)
term(11) = term(11) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, e, k, d)
end do 
end do 
end do 

term(9) = term(9) * 0.5d+0 
term(10) = -term(10) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(12) = term(12) + t2(d,e,i,j) * tvvov(a, d, k, e)
end do 
end do 

term(12) = term(12) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(13) = term(13) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, a, k, d)
term(14) = term(14) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, a, k, d)
term(15) = term(15) + t2(a,d,l,i) * tovoo(k, d, l, j)
term(16) = term(16) + t2(a,d,l,j) * tovoo(l, d, k, i)
term(17) = term(17) + t2(a,d,l,j) * tovoo(k, d, l, i)
term(18) = term(18) + t2(a,d,l,i) * tovoo(l, d, k, j)
end do 
end do 

term(13) = term(13) * 0.5d+0 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * 0.5d+0 
term(16) = term(16) * 0.5d+0 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 

do l = 1, nocc 
term(19) = term(19) + t2(a,a,j,l) * tovoo(l, a, k, i)
term(20) = term(20) + t2(a,a,i,l) * tovoo(l, a, k, j)
end do 

term(19) = term(19) * 0.5d+0 
term(20) = term(20) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(21) = term(21) + t2(a,d,j,l) * tovoo(k, d, l, i)
term(22) = term(22) + t2(a,d,j,i) * tovoo(k, d, l, l)
term(23) = term(23) + t2(a,d,j,l) * tovoo(l, d, k, i)
term(24) = term(24) + t2(a,d,j,i) * tovoo(l, d, k, l)
term(25) = term(25) + t2(a,d,i,l) * tovoo(k, d, l, j)
term(26) = term(26) + t2(a,d,i,j) * tovoo(k, d, l, l)
term(27) = term(27) + t2(a,d,i,l) * tovoo(l, d, k, j)
term(28) = term(28) + t2(a,d,i,j) * tovoo(l, d, k, l)
end do 
end do 

term(21) = term(21) * 0.5d+0 
term(22) = -term(22) 
term(23) = -term(23) 
term(24) = term(24) * 0.5d+0 
term(25) = term(25) * (-0.5d+0) 
term(28) = term(28) * (-0.5d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(29) = term(29) + t2(d,e,i,j) * tvvov(a, e, k, d)
end do 
end do 

term(29) = term(29) * (-0.5d+0) 


    eom_cc3_21_tripletm_trans_aiajak = triple_w1 + triple_w2
    do s = 0, 29
    eom_cc3_21_tripletm_trans_aiajak = eom_cc3_21_tripletm_trans_aiajak + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aiajak
    
    function eom_cc3_21_tripletm_trans_aibiak(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_21_tripletm_trans_aibiak  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, k 
    integer :: s ,l,d,e 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    term(0) = term(0) + tvooo(b, i, k, i)

term(0) = term(0) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(1) = term(1) + t2(b,d,i,l) * tovoo(k, d, l, i)
term(2) = term(2) + t2(b,d,i,i) * tovoo(k, d, l, l)
term(3) = term(3) + t2(b,d,i,l) * tovoo(l, d, k, i)
term(4) = term(4) + t2(b,d,i,i) * tovoo(l, d, k, l)
term(5) = term(5) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, a, k, d)
end do 
end do 

term(1) = term(1) * 0.5d+0 
term(2) = -term(2) 
term(3) = -term(3) 
term(4) = term(4) * 0.5d+0 
term(5) = term(5) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(6) = term(6) + t2(b,d,l,i) * tovoo(k, d, l, i)
term(7) = term(7) + t2(b,d,l,i) * tovoo(l, d, k, i)
term(8) = term(8) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, a, k, d)
end do 
end do 

term(6) = term(6) * 0.5d+0 
term(7) = term(7) * 0.5d+0 
term(8) = term(8) * (-0.5d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(9) = term(9) + t2(d,e,i,i) * tvvov(b, e, k, d)
end do 
end do 

term(9) = term(9) * (-0.5d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(10) = term(10) + t3(nocc, nactive, b,d,e,i,i,l) * tovov(l, d, k, e)
end do 
end do 
end do 

term(10) = term(10) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(11) = term(11) + t3(nocc, nactive, b,d,e,l,i,i) * tovov(l, e, k, d)
term(12) = term(12) + t3(nocc, nactive, b,d,e,i,i,l) * tovov(l, e, k, d)
end do 
end do 
end do 

term(11) = term(11) * 0.5d+0 
term(12) = -term(12) 

do d = nocc + 1, nactive 
term(13) = term(13) + tov(k, d) * t2(b,d,i,i)
term(14) = term(14) + t2(b,d,i,i) * tvvov(a, a, k, d)
term(15) = term(15) + t2(a,d,i,i) * tvvov(b, a, k, d)
end do 

term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * 0.5d+0 

do l = 1, nocc 
term(16) = term(16) + t2(a,b,l,i) * tovoo(l, a, k, i)
term(17) = term(17) + t2(a,b,i,l) * tovoo(l, a, k, i)
end do 

term(16) = term(16) * 0.5d+0 
term(17) = term(17) * (-0.5d+0) 


    eom_cc3_21_tripletm_trans_aibiak = triple_w1 + triple_w2
    do s = 0, 17
    eom_cc3_21_tripletm_trans_aibiak = eom_cc3_21_tripletm_trans_aibiak + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibiak
    
    function eom_cc3_21_tripletm_trans_aibjai(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_21_tripletm_trans_aibjai  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j 
    integer :: s ,l,d,e,m 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:29) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    term(0) = term(0) + tvooo(b, j, i, i)
term(1) = term(1) + tvvvo(a, a, b, j)

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(2) = term(2) + t2(b,d,j,l) * tovoo(i, d, l, i)
term(3) = term(3) + t2(b,d,j,i) * tovoo(i, d, l, l)
term(4) = term(4) + t2(b,d,j,l) * tovoo(l, d, i, i)
term(5) = term(5) + t2(b,d,j,i) * tovoo(l, d, i, l)
term(6) = term(6) + t2(b,d,j,l) * tvvov(a, d, l, a)
term(7) = term(7) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, a, i, d)
end do 
end do 

term(2) = term(2) * 0.5d+0 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = term(5) * 0.5d+0 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(8) = term(8) + t2(b,d,l,i) * tovoo(i, d, l, j)
term(9) = term(9) + t2(b,d,l,j) * tovoo(l, d, i, i)
term(10) = term(10) + t2(a,d,l,j) * tvvov(b, d, l, a)
term(11) = term(11) + t2(b,d,j,l) * tvvov(a, a, l, d)
term(12) = term(12) + t2(b,d,l,j) * tvvov(a, a, l, d)
term(13) = term(13) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, a, i, d)
end do 
end do 

term(8) = term(8) * 0.5d+0 
term(9) = term(9) * 0.5d+0 
term(10) = term(10) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(14) = term(14) + t3(nocc, nactive, b,d,e,l,i,j) * tovov(l, e, i, d)
term(15) = term(15) + t3(nocc, nactive, b,d,e,j,i,l) * tovov(l, e, i, d)
end do 
end do 
end do 

term(14) = term(14) * 0.5d+0 
term(15) = -term(15) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(16) = term(16) + t2(d,e,i,j) * tvvov(b, e, i, d)
end do 
end do 

term(16) = term(16) * (-0.5d+0) 

do l = 1, nocc 
term(17) = term(17) + tov(l, a) * t2(a,b,l,j)
term(18) = term(18) + t2(a,b,l,j) * tovoo(l, a, i, i)
term(19) = term(19) + t2(a,b,i,l) * tovoo(l, a, i, j)
end do 

term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * 0.5d+0 
term(19) = term(19) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(20) = term(20) + t2(b,d,j,i) * tvvov(a, a, i, d)
term(21) = term(21) + t2(a,d,i,j) * tvvov(b, a, i, d)
term(22) = term(22) + tov(i, d) * t2(b,d,j,i)
end do 

term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * 0.5d+0 
term(22) = term(22) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(23) = term(23) + t3(nocc, nactive, a,b,d,l,j,m) * tovov(m, d, l, a)
term(24) = term(24) + t3(nocc, nactive, a,b,d,l,j,m) * tovov(m, a, l, d)
end do 
end do 
end do 

term(23) = -term(23) 
term(24) = term(24) * 0.5d+0 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(25) = term(25) + t3(nocc, nactive, a,b,d,l,m,j) * tovov(m, d, l, a)
end do 
end do 
end do 

term(25) = term(25) * 0.5d+0 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(26) = term(26) + t3(nocc, nactive, b,d,e,j,i,l) * tovov(l, d, i, e)
end do 
end do 
end do 

term(26) = term(26) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
term(27) = term(27) + t2(a,b,l,m) * tovoo(l, a, m, j)
term(28) = term(28) + t2(a,b,l,j) * tovoo(l, a, m, m)
term(29) = term(29) + t2(a,b,l,j) * tovoo(m, a, l, m)
end do 
end do 

term(27) = term(27) * 0.5d+0 
term(28) = -term(28) 
term(29) = term(29) * 0.5d+0 


    eom_cc3_21_tripletm_trans_aibjai = triple_w1 + triple_w2
    do s = 0, 29
    eom_cc3_21_tripletm_trans_aibjai = eom_cc3_21_tripletm_trans_aibjai + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibjai
    
    function eom_cc3_21_tripletm_trans_aibjaj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_21_tripletm_trans_aibjaj  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j 
    integer :: s ,l,d,e,m 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:29) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    term(0) = term(0) + tvooo(b, j, j, i)
term(1) = term(1) + tvvvo(b, a, a, i)

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(2) = term(2) + t2(b,d,j,l) * tovoo(j, d, l, i)
term(3) = term(3) + t2(b,d,j,i) * tovoo(j, d, l, l)
term(4) = term(4) + t2(b,d,j,l) * tovoo(l, d, j, i)
term(5) = term(5) + t2(b,d,j,i) * tovoo(l, d, j, l)
term(6) = term(6) + t2(a,d,i,l) * tvvov(b, d, l, a)
term(7) = term(7) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, a, j, d)
end do 
end do 

term(2) = term(2) * 0.5d+0 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = term(5) * 0.5d+0 
term(6) = term(6) * 0.5d+0 
term(7) = term(7) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(8) = term(8) + t2(b,d,l,i) * tovoo(j, d, l, j)
term(9) = term(9) + t2(b,d,l,j) * tovoo(l, d, j, i)
term(10) = term(10) + t2(b,d,l,i) * tvvov(a, d, l, a)
term(11) = term(11) + t2(a,d,i,l) * tvvov(b, a, l, d)
term(12) = term(12) + t2(a,d,l,i) * tvvov(b, a, l, d)
term(13) = term(13) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, a, j, d)
end do 
end do 

term(8) = term(8) * 0.5d+0 
term(9) = term(9) * 0.5d+0 
term(10) = term(10) * 0.5d+0 
term(11) = -term(11) 
term(12) = term(12) * 0.5d+0 
term(13) = term(13) * (-0.5d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(d,e,i,j) * tvvov(b, e, j, d)
end do 
end do 

term(14) = term(14) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(15) = term(15) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, d, l, a)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(16) = term(16) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, a, l, d)
end do 
end do 
end do 

term(16) = term(16) * (-0.5d+0) 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(17) = term(17) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, a, l, d)
end do 
end do 
end do 

term(17) = term(17) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(18) = term(18) + t2(a,b,l,m) * tovoo(m, a, l, i)
term(19) = term(19) + t2(a,b,i,m) * tovoo(m, a, l, l)
term(20) = term(20) + t2(a,b,i,m) * tovoo(l, a, m, l)
end do 
end do 

term(18) = term(18) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(21) = term(21) + t3(nocc, nactive, b,d,e,j,i,l) * tovov(l, d, j, e)
end do 
end do 
end do 

term(21) = term(21) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(22) = term(22) + t3(nocc, nactive, b,d,e,l,i,j) * tovov(l, e, j, d)
term(23) = term(23) + t3(nocc, nactive, b,d,e,j,i,l) * tovov(l, e, j, d)
end do 
end do 
end do 

term(22) = term(22) * 0.5d+0 
term(23) = -term(23) 

do l = 1, nocc 
term(24) = term(24) + tov(l, a) * t2(a,b,i,l)
term(25) = term(25) + t2(a,b,l,j) * tovoo(l, a, j, i)
term(26) = term(26) + t2(a,b,i,l) * tovoo(l, a, j, j)
end do 

term(24) = term(24) * 0.5d+0 
term(25) = term(25) * 0.5d+0 
term(26) = term(26) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(27) = term(27) + t2(b,d,j,i) * tvvov(a, a, j, d)
term(28) = term(28) + t2(a,d,i,j) * tvvov(b, a, j, d)
term(29) = term(29) + tov(j, d) * t2(b,d,j,i)
end do 

term(27) = term(27) * (-0.5d+0) 
term(28) = term(28) * 0.5d+0 
term(29) = term(29) * (-0.5d+0) 


    eom_cc3_21_tripletm_trans_aibjaj = triple_w1 + triple_w2
    do s = 0, 29
    eom_cc3_21_tripletm_trans_aibjaj = eom_cc3_21_tripletm_trans_aibjaj + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibjaj
    
    function eom_cc3_21_tripletm_trans_aibibk(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_21_tripletm_trans_aibibk  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, k 
    integer :: s ,l,d,e 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    term(0) = term(0) + tvooo(a, i, k, i)

term(0) = term(0) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(1) = term(1) + t2(a,d,i,l) * tovoo(k, d, l, i)
term(2) = term(2) + t2(a,d,i,i) * tovoo(k, d, l, l)
term(3) = term(3) + t2(a,d,i,l) * tovoo(l, d, k, i)
term(4) = term(4) + t2(a,d,i,i) * tovoo(l, d, k, l)
term(5) = term(5) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, b, k, d)
end do 
end do 

term(1) = term(1) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(6) = term(6) + t2(a,d,l,i) * tovoo(k, d, l, i)
term(7) = term(7) + t2(a,d,l,i) * tovoo(l, d, k, i)
term(8) = term(8) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, b, k, d)
end do 
end do 

term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(9) = term(9) + t2(d,e,i,i) * tvvov(a, d, k, e)
end do 
end do 

term(9) = term(9) * 0.5d+0 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(10) = term(10) + t3(nocc, nactive, a,d,e,i,i,l) * tovov(l, d, k, e)
term(11) = term(11) + t3(nocc, nactive, a,d,e,l,i,i) * tovov(l, d, k, e)
end do 
end do 
end do 

term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(12) = term(12) + t3(nocc, nactive, a,d,e,i,i,l) * tovov(l, e, k, d)
end do 
end do 
end do 


do d = nocc + 1, nactive 
term(13) = term(13) + tov(k, d) * t2(a,d,i,i)
term(14) = term(14) + t2(b,d,i,i) * tvvov(a, b, k, d)
term(15) = term(15) + t2(a,d,i,i) * tvvov(b, b, k, d)
end do 

term(13) = term(13) * 0.5d+0 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * 0.5d+0 

do l = 1, nocc 
term(16) = term(16) + t2(a,b,l,i) * tovoo(l, b, k, i)
term(17) = term(17) + t2(a,b,i,l) * tovoo(l, b, k, i)
end do 

term(16) = term(16) * 0.5d+0 
term(17) = term(17) * (-0.5d+0) 


    eom_cc3_21_tripletm_trans_aibibk = triple_w1 + triple_w2
    do s = 0, 17
    eom_cc3_21_tripletm_trans_aibibk = eom_cc3_21_tripletm_trans_aibibk + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibibk
    
    function eom_cc3_21_tripletm_trans_aibjbi(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_21_tripletm_trans_aibjbi  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j 
    integer :: s ,l,d,e,m 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:29) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    term(0) = term(0) + tvooo(a, i, i, j)
term(1) = term(1) + tvvvo(a, b, b, j)

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(2) = term(2) + t2(a,d,i,l) * tovoo(i, d, l, j)
term(3) = term(3) + t2(a,d,i,j) * tovoo(i, d, l, l)
term(4) = term(4) + t2(a,d,i,l) * tovoo(l, d, i, j)
term(5) = term(5) + t2(a,d,i,j) * tovoo(l, d, i, l)
term(6) = term(6) + t2(b,d,j,l) * tvvov(a, d, l, b)
term(7) = term(7) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, b, i, d)
end do 
end do 

term(2) = term(2) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(8) = term(8) + t2(a,d,l,j) * tovoo(i, d, l, i)
term(9) = term(9) + t2(a,d,l,i) * tovoo(l, d, i, j)
term(10) = term(10) + t2(a,d,l,j) * tvvov(b, d, l, b)
term(11) = term(11) + t2(b,d,j,l) * tvvov(a, b, l, d)
term(12) = term(12) + t2(b,d,l,j) * tvvov(a, b, l, d)
term(13) = term(13) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, b, i, d)
end do 
end do 

term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(14) = term(14) + t2(d,e,i,j) * tvvov(a, d, i, e)
end do 
end do 

term(14) = term(14) * 0.5d+0 

do d = nocc + 1, nactive 
term(15) = term(15) + t2(b,d,j,i) * tvvov(a, b, i, d)
term(16) = term(16) + t2(a,d,i,j) * tvvov(b, b, i, d)
term(17) = term(17) + tov(i, d) * t2(a,d,i,j)
end do 

term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * 0.5d+0 
term(17) = term(17) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(18) = term(18) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, e, i, d)
end do 
end do 
end do 


do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(19) = term(19) + t3(nocc, nactive, a,b,d,l,j,m) * tovov(m, d, l, b)
term(20) = term(20) + t3(nocc, nactive, a,b,d,l,j,m) * tovov(m, b, l, d)
end do 
end do 
end do 

term(19) = -term(19) 
term(20) = term(20) * 0.5d+0 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(21) = term(21) + t3(nocc, nactive, a,b,d,l,m,j) * tovov(m, d, l, b)
end do 
end do 
end do 

term(21) = term(21) * 0.5d+0 

do l = 1, nocc 
term(22) = term(22) + t2(a,b,l,j) * tovoo(l, b, i, i)
term(23) = term(23) + t2(a,b,i,l) * tovoo(l, b, i, j)
term(24) = term(24) + tov(l, b) * t2(a,b,l,j)
end do 

term(22) = term(22) * 0.5d+0 
term(23) = term(23) * (-0.5d+0) 
term(24) = term(24) * (-0.5d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(25) = term(25) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, d, i, e)
term(26) = term(26) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, d, i, e)
end do 
end do 
end do 

term(25) = term(25) * (-0.5d+0) 
term(26) = term(26) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(27) = term(27) + t2(a,b,l,m) * tovoo(l, b, m, j)
term(28) = term(28) + t2(a,b,l,j) * tovoo(l, b, m, m)
term(29) = term(29) + t2(a,b,l,j) * tovoo(m, b, l, m)
end do 
end do 

term(27) = term(27) * 0.5d+0 
term(28) = -term(28) 
term(29) = term(29) * 0.5d+0 


    eom_cc3_21_tripletm_trans_aibjbi = triple_w1 + triple_w2
    do s = 0, 29
    eom_cc3_21_tripletm_trans_aibjbi = eom_cc3_21_tripletm_trans_aibjbi + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibjbi
    
    function eom_cc3_21_tripletm_trans_aibjbj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_21_tripletm_trans_aibjbj  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j 
    integer :: s ,l,d,e,m 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:29) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvvvo(b, b, a, i)

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(2) = term(2) + t2(a,d,i,l) * tovoo(j, d, l, j)
term(3) = term(3) + t2(a,d,i,j) * tovoo(j, d, l, l)
term(4) = term(4) + t2(a,d,i,l) * tovoo(l, d, j, j)
term(5) = term(5) + t2(a,d,i,j) * tovoo(l, d, j, l)
term(6) = term(6) + t2(a,d,i,l) * tvvov(b, d, l, b)
term(7) = term(7) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, b, j, d)
end do 
end do 

term(2) = term(2) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * 0.5d+0 
term(7) = term(7) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(8) = term(8) + t2(a,d,l,j) * tovoo(j, d, l, i)
term(9) = term(9) + t2(a,d,l,i) * tovoo(l, d, j, j)
term(10) = term(10) + t2(b,d,l,i) * tvvov(a, d, l, b)
term(11) = term(11) + t2(a,d,i,l) * tvvov(b, b, l, d)
term(12) = term(12) + t2(a,d,l,i) * tvvov(b, b, l, d)
term(13) = term(13) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, b, j, d)
end do 
end do 

term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * 0.5d+0 
term(11) = -term(11) 
term(12) = term(12) * 0.5d+0 
term(13) = term(13) * (-0.5d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(14) = term(14) + t2(d,e,i,j) * tvvov(a, d, j, e)
end do 
end do 

term(14) = term(14) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(15) = term(15) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, d, l, b)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(16) = term(16) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, b, l, d)
end do 
end do 
end do 

term(16) = term(16) * (-0.5d+0) 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(17) = term(17) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, b, l, d)
end do 
end do 
end do 

term(17) = term(17) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(18) = term(18) + t2(a,b,l,m) * tovoo(m, b, l, i)
term(19) = term(19) + t2(a,b,i,l) * tovoo(l, b, m, m)
term(20) = term(20) + t2(a,b,i,l) * tovoo(m, b, l, m)
end do 
end do 

term(18) = term(18) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(21) = term(21) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, d, j, e)
term(22) = term(22) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, d, j, e)
end do 
end do 
end do 

term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(23) = term(23) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, e, j, d)
end do 
end do 
end do 


do l = 1, nocc 
term(24) = term(24) + tov(l, b) * t2(a,b,i,l)
term(25) = term(25) + t2(a,b,l,j) * tovoo(l, b, j, i)
term(26) = term(26) + t2(a,b,i,l) * tovoo(l, b, j, j)
end do 

term(24) = term(24) * 0.5d+0 
term(25) = term(25) * 0.5d+0 
term(26) = term(26) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(27) = term(27) + t2(b,d,j,i) * tvvov(a, b, j, d)
term(28) = term(28) + t2(a,d,i,j) * tvvov(b, b, j, d)
term(29) = term(29) + tov(j, d) * t2(a,d,i,j)
end do 

term(27) = term(27) * (-0.5d+0) 
term(28) = term(28) * 0.5d+0 
term(29) = term(29) * 0.5d+0 


    eom_cc3_21_tripletm_trans_aibjbj = triple_w1 + triple_w2
    do s = 0, 29
    eom_cc3_21_tripletm_trans_aibjbj = eom_cc3_21_tripletm_trans_aibjbj + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibjbj
    
    function eom_cc3_21_tripletm_trans_aiajci(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_21_tripletm_trans_aiajci  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, j, c 
    integer :: s ,l,d,m 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    do l = 1, nocc 
term(0) = term(0) + tov(l, c) * t2(a,a,j,l)
term(1) = term(1) + t2(a,a,j,l) * tovoo(l, c, i, i)
term(2) = term(2) + t2(a,a,i,l) * tovoo(l, c, i, j)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * 0.5d+0 
term(2) = term(2) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(3) = term(3) + t2(a,d,l,j) * tvvov(a, d, l, c)
term(4) = term(4) + t2(a,d,j,l) * tvvov(a, c, l, d)
term(5) = term(5) + t2(a,d,l,j) * tvvov(a, c, l, d)
term(6) = term(6) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, c, i, d)
term(7) = term(7) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, c, i, d)
end do 
end do 

term(3) = term(3) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(8) = term(8) + t2(a,d,j,l) * tvvov(a, d, l, c)
end do 
end do 

term(8) = term(8) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(9) = term(9) + t2(a,a,l,m) * tovoo(l, c, m, j)
term(10) = term(10) + t2(a,a,j,l) * tovoo(l, c, m, m)
term(11) = term(11) + t2(a,a,j,l) * tovoo(m, c, l, m)
end do 
end do 

term(9) = term(9) * 0.5d+0 
term(10) = -term(10) 
term(11) = term(11) * 0.5d+0 

term(12) = term(12) + tvvvo(a, c, a, j)

term(12) = term(12) * 0.5d+0 

do d = nocc + 1, nactive 
term(13) = term(13) + t2(a,d,j,i) * tvvov(a, c, i, d)
term(14) = term(14) + t2(a,d,i,j) * tvvov(a, c, i, d)
end do 

term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(15) = term(15) + t3(nocc, nactive, a,a,d,j,l,m) * tovov(m, d, l, c)
end do 
end do 
end do 

term(15) = -term(15) 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(16) = term(16) + t3(nocc, nactive, a,a,d,l,m,j) * tovov(m, d, l, c)
end do 
end do 
end do 

term(16) = term(16) * 0.5d+0 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(17) = term(17) + t3(nocc, nactive, a,a,d,j,l,m) * tovov(m, c, l, d)
end do 
end do 
end do 

term(17) = term(17) * 0.5d+0 


    eom_cc3_21_tripletm_trans_aiajci = triple_w1 + triple_w2
    do s = 0, 17
    eom_cc3_21_tripletm_trans_aiajci = eom_cc3_21_tripletm_trans_aiajci + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aiajci
    
    function eom_cc3_21_tripletm_trans_aiajcj(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_21_tripletm_trans_aiajcj  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, j, c 
    integer :: s ,l,d,m 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    do l = 1, nocc 
term(0) = term(0) + tov(l, c) * t2(a,a,i,l)
term(1) = term(1) + t2(a,a,j,l) * tovoo(l, c, j, i)
term(2) = term(2) + t2(a,a,i,l) * tovoo(l, c, j, j)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * 0.5d+0 
term(2) = term(2) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(3) = term(3) + t2(a,d,l,i) * tvvov(a, d, l, c)
term(4) = term(4) + t2(a,d,i,l) * tvvov(a, c, l, d)
term(5) = term(5) + t2(a,d,l,i) * tvvov(a, c, l, d)
term(6) = term(6) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, c, j, d)
term(7) = term(7) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, c, j, d)
end do 
end do 

term(3) = term(3) * 0.5d+0 
term(4) = -term(4) 
term(5) = term(5) * 0.5d+0 
term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(8) = term(8) + t2(a,d,i,l) * tvvov(a, d, l, c)
end do 
end do 

term(8) = term(8) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
term(9) = term(9) + t2(a,a,l,m) * tovoo(m, c, l, i)
term(10) = term(10) + t2(a,a,i,l) * tovoo(l, c, m, m)
term(11) = term(11) + t2(a,a,i,l) * tovoo(m, c, l, m)
end do 
end do 

term(9) = term(9) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

term(12) = term(12) + tvvvo(a, c, a, i)

term(12) = term(12) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(13) = term(13) + t2(a,d,j,i) * tvvov(a, c, j, d)
term(14) = term(14) + t2(a,d,i,j) * tvvov(a, c, j, d)
end do 

term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(15) = term(15) + t3(nocc, nactive, a,a,d,i,l,m) * tovov(m, d, l, c)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(16) = term(16) + t3(nocc, nactive, a,a,d,l,m,i) * tovov(m, c, l, d)
end do 
end do 
end do 

term(16) = term(16) * (-0.5d+0) 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(17) = term(17) + t3(nocc, nactive, a,a,d,i,l,m) * tovov(m, c, l, d)
end do 
end do 
end do 

term(17) = term(17) * (-0.5d+0) 


    eom_cc3_21_tripletm_trans_aiajcj = triple_w1 + triple_w2
    do s = 0, 17
    eom_cc3_21_tripletm_trans_aiajcj = eom_cc3_21_tripletm_trans_aiajcj + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aiajcj
    
    function eom_cc3_21_tripletm_trans_aibici(t2, nocc, nactive, a, i, b, c) 
    double precision :: eom_cc3_21_tripletm_trans_aibici  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, c 
    integer :: s ,d,l,m 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:29) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    term(0) = term(0) + tvvvo(a, c, b, i)
term(1) = term(1) + tvvvo(b, c, a, i)

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(2) = term(2) + t3(nocc, nactive, a,b,d,l,i,m) * tovov(m, d, l, c)
term(3) = term(3) + t3(nocc, nactive, a,b,d,l,i,m) * tovov(m, c, l, d)
term(4) = term(4) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, d, l, c)
end do 
end do 
end do 

term(2) = -term(2) 
term(3) = term(3) * 0.5d+0 

do d = nocc + 1, nactive 
term(5) = term(5) + t2(b,d,i,i) * tvvov(a, c, i, d)
term(6) = term(6) + t2(a,d,i,i) * tvvov(b, c, i, d)
end do 

term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * 0.5d+0 

do l = 1, nocc 
term(7) = term(7) + t2(a,b,l,i) * tovoo(l, c, i, i)
term(8) = term(8) + t2(a,b,i,l) * tovoo(l, c, i, i)
term(9) = term(9) + tov(l, c) * t2(a,b,l,i)
term(10) = term(10) + tov(l, c) * t2(a,b,i,l)
end do 

term(7) = term(7) * 0.5d+0 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * 0.5d+0 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(11) = term(11) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, d, l, c)
end do 
end do 
end do 

term(11) = term(11) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(12) = term(12) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, c, i, d)
term(13) = term(13) + t2(b,d,i,l) * tvvov(a, d, l, c)
term(14) = term(14) + t2(a,d,i,l) * tvvov(b, d, l, c)
end do 
end do 

term(12) = term(12) * 0.5d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(15) = term(15) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, c, i, d)
term(16) = term(16) + t2(a,d,l,i) * tvvov(b, d, l, c)
term(17) = term(17) + t2(b,d,i,l) * tvvov(a, c, l, d)
term(18) = term(18) + t2(b,d,l,i) * tvvov(a, c, l, d)
term(19) = term(19) + t2(b,d,l,i) * tvvov(a, d, l, c)
term(20) = term(20) + t2(a,d,i,l) * tvvov(b, c, l, d)
term(21) = term(21) + t2(a,d,l,i) * tvvov(b, c, l, d)
end do 
end do 

term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * 0.5d+0 
term(20) = -term(20) 
term(21) = term(21) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(22) = term(22) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, c, l, d)
end do 
end do 
end do 

term(22) = term(22) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(23) = term(23) + t2(a,b,l,m) * tovoo(l, c, m, i)
term(24) = term(24) + t2(a,b,l,i) * tovoo(l, c, m, m)
term(25) = term(25) + t2(a,b,l,i) * tovoo(m, c, l, m)
term(26) = term(26) + t2(a,b,l,m) * tovoo(m, c, l, i)
term(27) = term(27) + t2(a,b,i,l) * tovoo(l, c, m, m)
term(28) = term(28) + t2(a,b,i,l) * tovoo(m, c, l, m)
end do 
end do 

term(23) = term(23) * 0.5d+0 
term(24) = -term(24) 
term(25) = term(25) * 0.5d+0 
term(26) = term(26) * (-0.5d+0) 
term(28) = term(28) * (-0.5d+0) 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(29) = term(29) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, c, l, d)
end do 
end do 
end do 

term(29) = term(29) * (-0.5d+0) 


    eom_cc3_21_tripletm_trans_aibici = triple_w1 + triple_w2
    do s = 0, 29
    eom_cc3_21_tripletm_trans_aibici = eom_cc3_21_tripletm_trans_aibici + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibici
    
    function eom_cc3_21_tripletm_trans_aiajai(t2, nocc, nactive, a, i, j) 
    double precision :: eom_cc3_21_tripletm_trans_aiajai  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, j 
    integer :: s ,l,d,e,m 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:41) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    do l = 1, nocc 
term(0) = term(0) + tov(l, a) * t2(a,a,j,l)
term(1) = term(1) + t2(a,a,j,l) * tovoo(l, a, i, i)
term(2) = term(2) + t2(a,a,i,l) * tovoo(l, a, i, j)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * 0.5d+0 
term(2) = term(2) * (-0.5d+0) 

term(3) = term(3) + tvooo(a, j, i, i)
term(4) = term(4) + tvooo(a, i, i, j)
term(5) = term(5) + tvvvo(a, a, a, j)

term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * 0.5d+0 
term(5) = term(5) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(6) = term(6) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, a, i, d)
term(7) = term(7) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, a, i, d)
term(8) = term(8) + t2(a,d,l,i) * tovoo(i, d, l, j)
term(9) = term(9) + t2(a,d,l,j) * tovoo(l, d, i, i)
term(10) = term(10) + t2(a,d,l,j) * tovoo(i, d, l, i)
term(11) = term(11) + t2(a,d,l,i) * tovoo(l, d, i, j)
term(12) = term(12) + t2(a,d,l,j) * tvvov(a, d, l, a)
term(13) = term(13) + t2(a,d,j,l) * tvvov(a, a, l, d)
term(14) = term(14) + t2(a,d,l,j) * tvvov(a, a, l, d)
end do 
end do 

term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * 0.5d+0 
term(9) = term(9) * 0.5d+0 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(15) = term(15) + t2(a,d,j,l) * tovoo(i, d, l, i)
term(16) = term(16) + t2(a,d,j,i) * tovoo(i, d, l, l)
term(17) = term(17) + t2(a,d,j,l) * tovoo(l, d, i, i)
term(18) = term(18) + t2(a,d,j,i) * tovoo(l, d, i, l)
term(19) = term(19) + t2(a,d,i,l) * tovoo(i, d, l, j)
term(20) = term(20) + t2(a,d,i,j) * tovoo(i, d, l, l)
term(21) = term(21) + t2(a,d,i,l) * tovoo(l, d, i, j)
term(22) = term(22) + t2(a,d,i,j) * tovoo(l, d, i, l)
term(23) = term(23) + t2(a,d,j,l) * tvvov(a, d, l, a)
end do 
end do 

term(15) = term(15) * 0.5d+0 
term(16) = -term(16) 
term(17) = -term(17) 
term(18) = term(18) * 0.5d+0 
term(19) = term(19) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(24) = term(24) + t2(a,d,j,i) * tvvov(a, a, i, d)
term(25) = term(25) + t2(a,d,i,j) * tvvov(a, a, i, d)
term(26) = term(26) + tov(i, d) * t2(a,d,j,i)
term(27) = term(27) + tov(i, d) * t2(a,d,i,j)
end do 

term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * 0.5d+0 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * 0.5d+0 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(28) = term(28) + t2(d,e,i,j) * tvvov(a, e, i, d)
end do 
end do 

term(28) = term(28) * (-0.5d+0) 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(29) = term(29) + t3(nocc, nactive, a,a,d,j,l,m) * tovov(m, a, l, d)
end do 
end do 
end do 

term(29) = term(29) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(30) = term(30) + t3(nocc, nactive, a,a,d,j,l,m) * tovov(m, d, l, a)
end do 
end do 
end do 

term(30) = -term(30) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(31) = term(31) + t2(d,e,i,j) * tvvov(a, d, i, e)
end do 
end do 

term(31) = term(31) * 0.5d+0 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(32) = term(32) + t3(nocc, nactive, a,a,d,l,m,j) * tovov(m, d, l, a)
end do 
end do 
end do 

term(32) = term(32) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
term(33) = term(33) + t2(a,a,l,m) * tovoo(l, a, m, j)
term(34) = term(34) + t2(a,a,j,l) * tovoo(l, a, m, m)
term(35) = term(35) + t2(a,a,j,l) * tovoo(m, a, l, m)
end do 
end do 

term(33) = term(33) * 0.5d+0 
term(34) = -term(34) 
term(35) = term(35) * 0.5d+0 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(36) = term(36) + t3(nocc, nactive, a,d,e,j,i,l) * tovov(l, d, i, e)
term(37) = term(37) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, d, i, e)
term(38) = term(38) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, d, i, e)
end do 
end do 
end do 

term(36) = term(36) * 0.5d+0 
term(37) = term(37) * (-0.5d+0) 
term(38) = term(38) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(39) = term(39) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, e, i, d)
term(40) = term(40) + t3(nocc, nactive, a,d,e,j,i,l) * tovov(l, e, i, d)
term(41) = term(41) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, e, i, d)
end do 
end do 
end do 

term(39) = term(39) * 0.5d+0 
term(40) = -term(40) 


    eom_cc3_21_tripletm_trans_aiajai = triple_w1 + triple_w2
    do s = 0, 41
    eom_cc3_21_tripletm_trans_aiajai = eom_cc3_21_tripletm_trans_aiajai + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aiajai
    
    function eom_cc3_21_tripletm_trans_aiajaj(t2, nocc, nactive, a, i, j) 
    double precision :: eom_cc3_21_tripletm_trans_aiajaj  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, j 
    integer :: s ,l,d,e,m 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:41) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    do l = 1, nocc 
term(0) = term(0) + tov(l, a) * t2(a,a,i,l)
term(1) = term(1) + t2(a,a,j,l) * tovoo(l, a, j, i)
term(2) = term(2) + t2(a,a,i,l) * tovoo(l, a, j, j)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * 0.5d+0 
term(2) = term(2) * (-0.5d+0) 

term(3) = term(3) + tvooo(a, j, j, i)
term(4) = term(4) + tvooo(a, i, j, j)
term(5) = term(5) + tvvvo(a, a, a, i)

term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(6) = term(6) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, a, j, d)
term(7) = term(7) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, a, j, d)
term(8) = term(8) + t2(a,d,l,i) * tovoo(j, d, l, j)
term(9) = term(9) + t2(a,d,l,j) * tovoo(l, d, j, i)
term(10) = term(10) + t2(a,d,l,j) * tovoo(j, d, l, i)
term(11) = term(11) + t2(a,d,l,i) * tovoo(l, d, j, j)
term(12) = term(12) + t2(a,d,l,i) * tvvov(a, d, l, a)
term(13) = term(13) + t2(a,d,i,l) * tvvov(a, a, l, d)
term(14) = term(14) + t2(a,d,l,i) * tvvov(a, a, l, d)
end do 
end do 

term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * 0.5d+0 
term(9) = term(9) * 0.5d+0 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * 0.5d+0 
term(13) = -term(13) 
term(14) = term(14) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(15) = term(15) + t2(a,d,j,l) * tovoo(j, d, l, i)
term(16) = term(16) + t2(a,d,j,i) * tovoo(j, d, l, l)
term(17) = term(17) + t2(a,d,j,l) * tovoo(l, d, j, i)
term(18) = term(18) + t2(a,d,j,i) * tovoo(l, d, j, l)
term(19) = term(19) + t2(a,d,i,l) * tovoo(j, d, l, j)
term(20) = term(20) + t2(a,d,i,j) * tovoo(j, d, l, l)
term(21) = term(21) + t2(a,d,i,l) * tovoo(l, d, j, j)
term(22) = term(22) + t2(a,d,i,j) * tovoo(l, d, j, l)
term(23) = term(23) + t2(a,d,i,l) * tvvov(a, d, l, a)
end do 
end do 

term(15) = term(15) * 0.5d+0 
term(16) = -term(16) 
term(17) = -term(17) 
term(18) = term(18) * 0.5d+0 
term(19) = term(19) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * 0.5d+0 

do d = nocc + 1, nactive 
term(24) = term(24) + t2(a,d,j,i) * tvvov(a, a, j, d)
term(25) = term(25) + t2(a,d,i,j) * tvvov(a, a, j, d)
term(26) = term(26) + tov(j, d) * t2(a,d,j,i)
term(27) = term(27) + tov(j, d) * t2(a,d,i,j)
end do 

term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * 0.5d+0 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * 0.5d+0 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(28) = term(28) + t2(d,e,i,j) * tvvov(a, e, j, d)
end do 
end do 

term(28) = term(28) * (-0.5d+0) 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(29) = term(29) + t3(nocc, nactive, a,a,d,i,l,m) * tovov(m, a, l, d)
end do 
end do 
end do 

term(29) = term(29) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(30) = term(30) + t3(nocc, nactive, a,a,d,i,l,m) * tovov(m, d, l, a)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(31) = term(31) + t2(d,e,i,j) * tvvov(a, d, j, e)
end do 
end do 

term(31) = term(31) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(32) = term(32) + t3(nocc, nactive, a,a,d,l,m,i) * tovov(m, a, l, d)
end do 
end do 
end do 

term(32) = term(32) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(33) = term(33) + t2(a,a,l,m) * tovoo(m, a, l, i)
term(34) = term(34) + t2(a,a,i,l) * tovoo(l, a, m, m)
term(35) = term(35) + t2(a,a,i,l) * tovoo(m, a, l, m)
end do 
end do 

term(33) = term(33) * (-0.5d+0) 
term(35) = term(35) * (-0.5d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(36) = term(36) + t3(nocc, nactive, a,d,e,j,i,l) * tovov(l, d, j, e)
term(37) = term(37) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, d, j, e)
term(38) = term(38) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, d, j, e)
end do 
end do 
end do 

term(36) = term(36) * 0.5d+0 
term(37) = term(37) * (-0.5d+0) 
term(38) = term(38) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(39) = term(39) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, e, j, d)
term(40) = term(40) + t3(nocc, nactive, a,d,e,j,i,l) * tovov(l, e, j, d)
term(41) = term(41) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, e, j, d)
end do 
end do 
end do 

term(39) = term(39) * 0.5d+0 
term(40) = -term(40) 


    eom_cc3_21_tripletm_trans_aiajaj = triple_w1 + triple_w2
    do s = 0, 41
    eom_cc3_21_tripletm_trans_aiajaj = eom_cc3_21_tripletm_trans_aiajaj + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aiajaj
    
    function eom_cc3_21_tripletm_trans_aibiai(t2, nocc, nactive, a, i, b) 
    double precision :: eom_cc3_21_tripletm_trans_aibiai  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b 
    integer :: s ,d,l,m,e 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:41) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    do d = nocc + 1, nactive 
term(0) = term(0) + tov(i, d) * t2(b,d,i,i)
term(1) = term(1) + t2(b,d,i,i) * tvvov(a, a, i, d)
term(2) = term(2) + t2(a,d,i,i) * tvvov(b, a, i, d)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(3) = term(3) + t3(nocc, nactive, a,b,d,l,i,m) * tovov(m, d, l, a)
term(4) = term(4) + t3(nocc, nactive, a,b,d,l,i,m) * tovov(m, a, l, d)
term(5) = term(5) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, d, l, a)
end do 
end do 
end do 

term(3) = -term(3) 
term(4) = term(4) * 0.5d+0 

do l = 1, nocc 
term(6) = term(6) + t2(a,b,l,i) * tovoo(l, a, i, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(l, a, i, i)
term(8) = term(8) + tov(l, a) * t2(a,b,l,i)
term(9) = term(9) + tov(l, a) * t2(a,b,i,l)
end do 

term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, a, l, d)
end do 
end do 
end do 

term(10) = term(10) * (-0.5d+0) 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(11) = term(11) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, a, l, d)
end do 
end do 
end do 

term(11) = term(11) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(12) = term(12) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, a, i, d)
term(13) = term(13) + t2(b,d,i,l) * tvvov(a, d, l, a)
term(14) = term(14) + t2(a,d,i,l) * tvvov(b, d, l, a)
term(15) = term(15) + t2(b,d,i,l) * tovoo(i, d, l, i)
term(16) = term(16) + t2(b,d,i,i) * tovoo(i, d, l, l)
term(17) = term(17) + t2(b,d,i,l) * tovoo(l, d, i, i)
term(18) = term(18) + t2(b,d,i,i) * tovoo(l, d, i, l)
end do 
end do 

term(12) = term(12) * 0.5d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5d+0 
term(15) = term(15) * 0.5d+0 
term(16) = -term(16) 
term(17) = -term(17) 
term(18) = term(18) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(19) = term(19) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, a, i, d)
term(20) = term(20) + t2(a,d,l,i) * tvvov(b, d, l, a)
term(21) = term(21) + t2(b,d,i,l) * tvvov(a, a, l, d)
term(22) = term(22) + t2(b,d,l,i) * tvvov(a, a, l, d)
term(23) = term(23) + t2(b,d,l,i) * tvvov(a, d, l, a)
term(24) = term(24) + t2(a,d,i,l) * tvvov(b, a, l, d)
term(25) = term(25) + t2(a,d,l,i) * tvvov(b, a, l, d)
term(26) = term(26) + t2(b,d,l,i) * tovoo(i, d, l, i)
term(27) = term(27) + t2(b,d,l,i) * tovoo(l, d, i, i)
end do 
end do 

term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * 0.5d+0 
term(24) = -term(24) 
term(25) = term(25) * 0.5d+0 
term(26) = term(26) * 0.5d+0 
term(27) = term(27) * 0.5d+0 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(28) = term(28) + t3(nocc, nactive, b,d,e,i,i,l) * tovov(l, d, i, e)
end do 
end do 
end do 

term(28) = term(28) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
term(29) = term(29) + t2(a,b,l,m) * tovoo(l, a, m, i)
term(30) = term(30) + t2(a,b,l,i) * tovoo(l, a, m, m)
term(31) = term(31) + t2(a,b,l,i) * tovoo(m, a, l, m)
term(32) = term(32) + t2(a,b,l,m) * tovoo(m, a, l, i)
term(33) = term(33) + t2(a,b,i,m) * tovoo(m, a, l, l)
term(34) = term(34) + t2(a,b,i,m) * tovoo(l, a, m, l)
end do 
end do 

term(29) = term(29) * 0.5d+0 
term(30) = -term(30) 
term(31) = term(31) * 0.5d+0 
term(32) = term(32) * (-0.5d+0) 
term(34) = term(34) * (-0.5d+0) 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(35) = term(35) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, d, l, a)
end do 
end do 
end do 

term(35) = term(35) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(36) = term(36) + t3(nocc, nactive, b,d,e,l,i,i) * tovov(l, e, i, d)
term(37) = term(37) + t3(nocc, nactive, b,d,e,i,i,l) * tovov(l, e, i, d)
end do 
end do 
end do 

term(36) = term(36) * 0.5d+0 
term(37) = -term(37) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(38) = term(38) + t2(d,e,i,i) * tvvov(b, e, i, d)
end do 
end do 

term(38) = term(38) * (-0.5d+0) 

term(39) = term(39) + tvvvo(a, a, b, i)
term(40) = term(40) + tvvvo(b, a, a, i)
term(41) = term(41) + tvooo(b, i, i, i)

term(39) = term(39) * 0.5d+0 
term(40) = term(40) * (-0.5d+0) 
term(41) = term(41) * (-0.5d+0) 


    eom_cc3_21_tripletm_trans_aibiai = triple_w1 + triple_w2
    do s = 0, 41
    eom_cc3_21_tripletm_trans_aibiai = eom_cc3_21_tripletm_trans_aibiai + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibiai
    
    function eom_cc3_21_tripletm_trans_aibibi(t2, nocc, nactive, a, i, b) 
    double precision :: eom_cc3_21_tripletm_trans_aibibi  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b 
    integer :: s ,d,l,m,e 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:41) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    do d = nocc + 1, nactive 
term(0) = term(0) + tov(i, d) * t2(a,d,i,i)
term(1) = term(1) + t2(b,d,i,i) * tvvov(a, b, i, d)
term(2) = term(2) + t2(a,d,i,i) * tvvov(b, b, i, d)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(3) = term(3) + t3(nocc, nactive, a,b,d,l,i,m) * tovov(m, d, l, b)
term(4) = term(4) + t3(nocc, nactive, a,b,d,l,i,m) * tovov(m, b, l, d)
term(5) = term(5) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, d, l, b)
end do 
end do 
end do 

term(3) = -term(3) 
term(4) = term(4) * 0.5d+0 

do l = 1, nocc 
term(6) = term(6) + t2(a,b,l,i) * tovoo(l, b, i, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(l, b, i, i)
term(8) = term(8) + tov(l, b) * t2(a,b,l,i)
term(9) = term(9) + tov(l, b) * t2(a,b,i,l)
end do 

term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, b, l, d)
end do 
end do 
end do 

term(10) = term(10) * (-0.5d+0) 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(11) = term(11) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, b, l, d)
end do 
end do 
end do 

term(11) = term(11) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(12) = term(12) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, b, i, d)
term(13) = term(13) + t2(b,d,i,l) * tvvov(a, d, l, b)
term(14) = term(14) + t2(a,d,i,l) * tvvov(b, d, l, b)
term(15) = term(15) + t2(a,d,i,l) * tovoo(i, d, l, i)
term(16) = term(16) + t2(a,d,i,i) * tovoo(i, d, l, l)
term(17) = term(17) + t2(a,d,i,l) * tovoo(l, d, i, i)
term(18) = term(18) + t2(a,d,i,i) * tovoo(l, d, i, l)
end do 
end do 

term(12) = term(12) * 0.5d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5d+0 
term(15) = term(15) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(19) = term(19) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, b, i, d)
term(20) = term(20) + t2(a,d,l,i) * tvvov(b, d, l, b)
term(21) = term(21) + t2(b,d,i,l) * tvvov(a, b, l, d)
term(22) = term(22) + t2(b,d,l,i) * tvvov(a, b, l, d)
term(23) = term(23) + t2(b,d,l,i) * tvvov(a, d, l, b)
term(24) = term(24) + t2(a,d,i,l) * tvvov(b, b, l, d)
term(25) = term(25) + t2(a,d,l,i) * tvvov(b, b, l, d)
term(26) = term(26) + t2(a,d,l,i) * tovoo(i, d, l, i)
term(27) = term(27) + t2(a,d,l,i) * tovoo(l, d, i, i)
end do 
end do 

term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * 0.5d+0 
term(24) = -term(24) 
term(25) = term(25) * 0.5d+0 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(28) = term(28) + t3(nocc, nactive, a,d,e,i,i,l) * tovov(l, e, i, d)
end do 
end do 
end do 


do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(29) = term(29) + t3(nocc, nactive, a,d,e,i,i,l) * tovov(l, d, i, e)
term(30) = term(30) + t3(nocc, nactive, a,d,e,l,i,i) * tovov(l, d, i, e)
end do 
end do 
end do 

term(29) = term(29) * (-0.5d+0) 
term(30) = term(30) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(31) = term(31) + t2(a,b,l,m) * tovoo(l, b, m, i)
term(32) = term(32) + t2(a,b,l,i) * tovoo(l, b, m, m)
term(33) = term(33) + t2(a,b,l,i) * tovoo(m, b, l, m)
term(34) = term(34) + t2(a,b,l,m) * tovoo(m, b, l, i)
term(35) = term(35) + t2(a,b,i,l) * tovoo(l, b, m, m)
term(36) = term(36) + t2(a,b,i,l) * tovoo(m, b, l, m)
end do 
end do 

term(31) = term(31) * 0.5d+0 
term(32) = -term(32) 
term(33) = term(33) * 0.5d+0 
term(34) = term(34) * (-0.5d+0) 
term(36) = term(36) * (-0.5d+0) 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(37) = term(37) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, d, l, b)
end do 
end do 
end do 

term(37) = term(37) * 0.5d+0 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(38) = term(38) + t2(d,e,i,i) * tvvov(a, d, i, e)
end do 
end do 

term(38) = term(38) * 0.5d+0 

term(39) = term(39) + tvvvo(a, b, b, i)
term(40) = term(40) + tvvvo(b, b, a, i)
term(41) = term(41) + tvooo(a, i, i, i)

term(39) = term(39) * 0.5d+0 
term(40) = term(40) * (-0.5d+0) 
term(41) = term(41) * 0.5d+0 


    eom_cc3_21_tripletm_trans_aibibi = triple_w1 + triple_w2
    do s = 0, 41
    eom_cc3_21_tripletm_trans_aibibi = eom_cc3_21_tripletm_trans_aibibi + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibibi
    
    function eom_cc3_21_tripletm_trans_aibjck(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_21_tripletm_trans_aibjck  
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j, c, k 
    integer :: s ,d,l 
    double precision :: triple_w1
    double precision :: triple_w2
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    triple_w1 = 0.d+0
    triple_w2 = 0.d+0
    
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(b,d,j,i) * tvvov(a, c, k, d)
term(1) = term(1) + t2(a,d,i,j) * tvvov(b, c, k, d)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * 0.5d+0 

do l = 1, nocc 
term(2) = term(2) + t2(a,b,l,j) * tovoo(l, c, k, i)
term(3) = term(3) + t2(a,b,i,l) * tovoo(l, c, k, j)
end do 

term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(4) = term(4) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, c, k, d)
end do 
end do 

term(4) = term(4) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(5) = term(5) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, c, k, d)
end do 
end do 

term(5) = term(5) * (-0.5d+0) 


    eom_cc3_21_tripletm_trans_aibjck = triple_w1 + triple_w2
    do s = 0, 5
    eom_cc3_21_tripletm_trans_aibjck = eom_cc3_21_tripletm_trans_aibjck + term(s)
    end do

    end function eom_cc3_21_tripletm_trans_aibjck
    
    end module eom_cc3_21_tripletm_trans
    
