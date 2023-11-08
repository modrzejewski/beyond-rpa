module eom_cc3_21_trans

    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none
    !               
    ! File generated automatically on 2019-06-13 16:51:46  
    !  
    contains
    
    function eom_cc3_21_trans_aiajck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_cc3_21_trans_aiajck  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, j, c, k 
    integer :: s ,d,l 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:13) :: term 
    term = 0.d+0 
    !!triple_w1 = 0.d+0
    !!triple_w2 = 0.d+0
    
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,j,i) * tvvov(a, c, k, d)
term(1) = term(1) + t2(a,d,i,j) * tvvov(a, c, k, d)
term(2) = term(2) + t2(a,d,j,i) * tvvov(a, d, k, c)
term(3) = term(3) + t2(a,d,i,j) * tvvov(a, d, k, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (2.0000000000000004d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 

do l = 1, nocc 
term(4) = term(4) + t2(a,a,j,l) * tovoo(l, c, k, i)
term(5) = term(5) + t2(a,a,i,l) * tovoo(l, c, k, j)
term(6) = term(6) + t2(a,a,j,l) * tovoo(k, c, l, i)
term(7) = term(7) + t2(a,a,i,l) * tovoo(k, c, l, j)
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 
term(7) = term(7) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(8) = term(8) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, c, k, d)
term(9) = term(9) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, c, k, d)
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
term(10) = term(10) + t3(nocc, nactive, a,a,d,i,j,l) * tovov(l, c, k, d)
term(11) = term(11) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, d, k, c)
term(12) = term(12) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, d, k, c)
term(13) = term(13) + t3(nocc, nactive, a,a,d,i,j,l) * tovov(l, d, k, c)
end do 
end do 

term(10) = term(10) * (-2.000000000000003d+0) 
term(11) = term(11) * (-2.000000000000003d+0) 
term(12) = term(12) * (-2.000000000000003d+0) 
term(13) = term(13) * (4.000000000000006d+0) 


    eom_cc3_21_trans_aiajck = 0 !!triple_w1 + !triple_w2
    do s = 0, 13
    eom_cc3_21_trans_aiajck = eom_cc3_21_trans_aiajck + term(s)
    end do

    end function eom_cc3_21_trans_aiajck
    
    function eom_cc3_21_trans_aibjak(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: eom_cc3_21_trans_aibjak  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j, k 
    integer :: s ,d,l,e 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:25) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(b, j, i, k)
    
    term(0) = term(0) + tvooo(b, j, k, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do d = nocc + 1, nactive 
term(1) = term(1) + tov(k, d) * t2(b,d,j,i)
term(2) = term(2) + t2(b,d,j,i) * tvvov(a, a, k, d)
term(3) = term(3) + t2(a,d,i,j) * tvvov(b, a, k, d)
term(4) = term(4) + t2(b,d,j,i) * tvvov(a, d, k, a)
term(5) = term(5) + t2(a,d,i,j) * tvvov(b, d, k, a)
end do 

term(1) = term(1) * (-0.9999999999999999d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(b,d,j,l) * tovoo(k, d, l, i)
term(7) = term(7) + t2(b,d,j,i) * tovoo(k, d, l, l)
term(8) = term(8) + t2(b,d,j,l) * tovoo(l, d, k, i)
term(9) = term(9) + t2(b,d,j,i) * tovoo(l, d, k, l)
term(10) = term(10) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, a, k, d)
term(11) = term(11) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, a, k, d)
term(12) = term(12) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, d, k, a)
term(13) = term(13) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, d, k, a)
term(14) = term(14) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, d, k, a)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 
term(8) = term(8) * (-2.0000000000000004d+0) 
term(11) = term(11) * (-2.000000000000003d+0) 
term(12) = term(12) * (-2.000000000000003d+0) 
term(13) = term(13) * (-2.000000000000003d+0) 
term(14) = term(14) * (4.000000000000006d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(15) = term(15) + t2(b,d,l,i) * tovoo(k, d, l, j)
term(16) = term(16) + t2(b,d,l,j) * tovoo(l, d, k, i)
term(17) = term(17) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, a, k, d)
end do 
end do 


do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(18) = term(18) + t2(d,e,i,j) * tvvov(b, e, k, d)
end do 
end do 

term(18) = term(18) * (-1.0000000000000002d+0) 

do l = 1, nocc 
term(19) = term(19) + t2(a,b,l,j) * tovoo(l, a, k, i)
term(20) = term(20) + t2(a,b,i,l) * tovoo(l, a, k, j)
term(21) = term(21) + t2(a,b,l,j) * tovoo(k, a, l, i)
term(22) = term(22) + t2(a,b,i,l) * tovoo(k, a, l, j)
end do 

term(21) = term(21) * (-2.0000000000000004d+0) 
term(22) = term(22) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(23) = term(23) + t3(nocc, nactive, b,d,e,j,i,l) * tovov(l, d, k, e)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(24) = term(24) + t3(nocc, nactive, b,d,e,l,i,j) * tovov(l, e, k, d)
term(25) = term(25) + t3(nocc, nactive, b,d,e,j,i,l) * tovov(l, e, k, d)
end do 
end do 
end do 

term(25) = term(25) * (-2.000000000000003d+0) 


    eom_cc3_21_trans_aibjak = 0 !!triple_w1 + !triple_w2
    do s = 0, 25
    eom_cc3_21_trans_aibjak = eom_cc3_21_trans_aibjak + term(s)
    end do

    end function eom_cc3_21_trans_aibjak
    
    function eom_cc3_21_trans_aibjbk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: eom_cc3_21_trans_aibjbk  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j, k 
    integer :: s ,d,l,e 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:25) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(a, i, j, k)
    
    term(0) = term(0) + tvooo(a, i, k, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do d = nocc + 1, nactive 
term(1) = term(1) + tov(k, d) * t2(a,d,i,j)
term(2) = term(2) + t2(b,d,j,i) * tvvov(a, b, k, d)
term(3) = term(3) + t2(a,d,i,j) * tvvov(b, b, k, d)
term(4) = term(4) + t2(b,d,j,i) * tvvov(a, d, k, b)
term(5) = term(5) + t2(a,d,i,j) * tvvov(b, d, k, b)
end do 

term(1) = term(1) * (-0.9999999999999999d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(6) = term(6) + t2(a,d,l,j) * tovoo(k, d, l, i)
term(7) = term(7) + t2(a,d,l,i) * tovoo(l, d, k, j)
term(8) = term(8) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, b, k, d)
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
term(9) = term(9) + t2(a,d,i,l) * tovoo(k, d, l, j)
term(10) = term(10) + t2(a,d,i,j) * tovoo(k, d, l, l)
term(11) = term(11) + t2(a,d,i,l) * tovoo(l, d, k, j)
term(12) = term(12) + t2(a,d,i,j) * tovoo(l, d, k, l)
term(13) = term(13) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, b, k, d)
term(14) = term(14) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, b, k, d)
term(15) = term(15) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, d, k, b)
term(16) = term(16) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, d, k, b)
term(17) = term(17) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, d, k, b)
end do 
end do 

term(10) = term(10) * (-2.0000000000000004d+0) 
term(11) = term(11) * (-2.0000000000000004d+0) 
term(14) = term(14) * (-2.000000000000003d+0) 
term(15) = term(15) * (-2.000000000000003d+0) 
term(16) = term(16) * (-2.000000000000003d+0) 
term(17) = term(17) * (4.000000000000006d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(18) = term(18) + t2(d,e,i,j) * tvvov(a, d, k, e)
end do 
end do 

term(18) = term(18) * (-1.0000000000000002d+0) 

do l = 1, nocc 
term(19) = term(19) + t2(a,b,l,j) * tovoo(l, b, k, i)
term(20) = term(20) + t2(a,b,i,l) * tovoo(l, b, k, j)
term(21) = term(21) + t2(a,b,l,j) * tovoo(k, b, l, i)
term(22) = term(22) + t2(a,b,i,l) * tovoo(k, b, l, j)
end do 

term(21) = term(21) * (-2.0000000000000004d+0) 
term(22) = term(22) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(23) = term(23) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, d, k, e)
term(24) = term(24) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, d, k, e)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(25) = term(25) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, e, k, d)
end do 
end do 
end do 

term(25) = term(25) * (-2.000000000000003d+0) 


    eom_cc3_21_trans_aibjbk = 0 !!triple_w1 + !triple_w2
    do s = 0, 25
    eom_cc3_21_trans_aibjbk = eom_cc3_21_trans_aibjbk + term(s)
    end do

    end function eom_cc3_21_trans_aibjbk
    
    function eom_cc3_21_trans_aibick(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_cc3_21_trans_aibick  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, c, k 
    integer :: s ,d,l 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:13) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(b,d,i,i) * tvvov(a, c, k, d)
term(1) = term(1) + t2(a,d,i,i) * tvvov(b, c, k, d)
term(2) = term(2) + t2(b,d,i,i) * tvvov(a, d, k, c)
term(3) = term(3) + t2(a,d,i,i) * tvvov(b, d, k, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (2.0000000000000004d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 

do l = 1, nocc 
term(4) = term(4) + t2(a,b,l,i) * tovoo(l, c, k, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(l, c, k, i)
term(6) = term(6) + t2(a,b,l,i) * tovoo(k, c, l, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(k, c, l, i)
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 
term(7) = term(7) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(8) = term(8) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, c, k, d)
term(9) = term(9) + t3(nocc, nactive, a,b,d,i,i,l) * tovov(l, c, k, d)
term(10) = term(10) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, d, k, c)
term(11) = term(11) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, d, k, c)
term(12) = term(12) + t3(nocc, nactive, a,b,d,i,i,l) * tovov(l, d, k, c)
end do 
end do 

term(9) = term(9) * (-2.000000000000003d+0) 
term(10) = term(10) * (-2.000000000000003d+0) 
term(11) = term(11) * (-2.000000000000003d+0) 
term(12) = term(12) * (4.000000000000006d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(13) = term(13) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, c, k, d)
end do 
end do 



    eom_cc3_21_trans_aibick = 0 !!triple_w1 + !triple_w2
    do s = 0, 13
    eom_cc3_21_trans_aibick = eom_cc3_21_trans_aibick + term(s)
    end do

    end function eom_cc3_21_trans_aibick
    
    function eom_cc3_21_trans_aibjci(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: eom_cc3_21_trans_aibjci  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j, c 
    integer :: s ,l,d,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:25) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(a, b, c, j)
    
    term(0) = term(0) + tvvvo(a, c, b, j)


do l = 1, nocc 
term(1) = term(1) + tov(l, c) * t2(a,b,l,j)
term(2) = term(2) + t2(a,b,l,j) * tovoo(l, c, i, i)
term(3) = term(3) + t2(a,b,i,l) * tovoo(l, c, i, j)
term(4) = term(4) + t2(a,b,l,j) * tovoo(i, c, l, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(i, c, l, j)
end do 

term(1) = term(1) * (-0.9999999999999999d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 
term(5) = term(5) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(b,d,j,l) * tvvov(a, d, l, c)
term(7) = term(7) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, c, i, d)
term(8) = term(8) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, c, i, d)
term(9) = term(9) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, d, i, c)
term(10) = term(10) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, d, i, c)
term(11) = term(11) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, d, i, c)
end do 
end do 

term(6) = term(6) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-2.000000000000003d+0) 
term(9) = term(9) * (-2.000000000000003d+0) 
term(10) = term(10) * (-2.000000000000003d+0) 
term(11) = term(11) * (4.000000000000006d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(12) = term(12) + t2(a,d,l,j) * tvvov(b, d, l, c)
term(13) = term(13) + t2(b,d,l,j) * tvvov(a, c, l, d)
term(14) = term(14) + t2(b,d,j,l) * tvvov(a, c, l, d)
term(15) = term(15) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, c, i, d)
end do 
end do 

term(12) = term(12) * (-1.0000000000000002d+0) 
term(13) = term(13) * (-1.0000000000000002d+0) 
term(14) = term(14) * (2.0000000000000004d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(16) = term(16) + t2(a,b,l,m) * tovoo(l, c, m, j)
term(17) = term(17) + t2(a,b,l,j) * tovoo(l, c, m, m)
term(18) = term(18) + t2(a,b,l,j) * tovoo(m, c, l, m)
end do 
end do 

term(17) = term(17) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
term(19) = term(19) + t2(b,d,j,i) * tvvov(a, c, i, d)
term(20) = term(20) + t2(a,d,i,j) * tvvov(b, c, i, d)
term(21) = term(21) + t2(b,d,j,i) * tvvov(a, d, i, c)
term(22) = term(22) + t2(a,d,i,j) * tvvov(b, d, i, c)
end do 

term(19) = term(19) * (-1.0000000000000002d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(21) = term(21) * (2.0000000000000004d+0) 
term(22) = term(22) * (2.0000000000000004d+0) 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(23) = term(23) + t3(nocc, nactive, a,b,d,l,j,m) * tovov(m, c, l, d)
term(24) = term(24) + t3(nocc, nactive, a,b,d,l,j,m) * tovov(m, d, l, c)
end do 
end do 
end do 

term(24) = term(24) * (-2.000000000000003d+0) 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(25) = term(25) + t3(nocc, nactive, a,b,d,l,m,j) * tovov(m, d, l, c)
end do 
end do 
end do 



    eom_cc3_21_trans_aibjci = 0 !!triple_w1 + !triple_w2
    do s = 0, 25
    eom_cc3_21_trans_aibjci = eom_cc3_21_trans_aibjci + term(s)
    end do

    end function eom_cc3_21_trans_aibjci
    
    function eom_cc3_21_trans_aibjcj(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: eom_cc3_21_trans_aibjcj  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j, c 
    integer :: s ,l,d,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:25) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(b, a, c, i)
    
    term(0) = term(0) + tvvvo(b, c, a, i)


do l = 1, nocc 
term(1) = term(1) + tov(l, c) * t2(a,b,i,l)
term(2) = term(2) + t2(a,b,l,j) * tovoo(l, c, j, i)
term(3) = term(3) + t2(a,b,i,l) * tovoo(l, c, j, j)
term(4) = term(4) + t2(a,b,l,j) * tovoo(j, c, l, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(j, c, l, j)
end do 

term(1) = term(1) * (-0.9999999999999999d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 
term(5) = term(5) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(6) = term(6) + t2(b,d,l,i) * tvvov(a, d, l, c)
term(7) = term(7) + t2(a,d,l,i) * tvvov(b, c, l, d)
term(8) = term(8) + t2(a,d,i,l) * tvvov(b, c, l, d)
term(9) = term(9) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, c, j, d)
end do 
end do 

term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(10) = term(10) + t2(a,d,i,l) * tvvov(b, d, l, c)
term(11) = term(11) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, c, j, d)
term(12) = term(12) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, c, j, d)
term(13) = term(13) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, d, j, c)
term(14) = term(14) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, d, j, c)
term(15) = term(15) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, d, j, c)
end do 
end do 

term(10) = term(10) * (-1.0000000000000002d+0) 
term(12) = term(12) * (-2.000000000000003d+0) 
term(13) = term(13) * (-2.000000000000003d+0) 
term(14) = term(14) * (-2.000000000000003d+0) 
term(15) = term(15) * (4.000000000000006d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(16) = term(16) + t2(a,b,l,m) * tovoo(m, c, l, i)
term(17) = term(17) + t2(a,b,i,l) * tovoo(l, c, m, m)
term(18) = term(18) + t2(a,b,i,l) * tovoo(m, c, l, m)
end do 
end do 

term(17) = term(17) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
term(19) = term(19) + t2(b,d,j,i) * tvvov(a, c, j, d)
term(20) = term(20) + t2(a,d,i,j) * tvvov(b, c, j, d)
term(21) = term(21) + t2(b,d,j,i) * tvvov(a, d, j, c)
term(22) = term(22) + t2(a,d,i,j) * tvvov(b, d, j, c)
end do 

term(19) = term(19) * (-1.0000000000000002d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(21) = term(21) * (2.0000000000000004d+0) 
term(22) = term(22) * (2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(23) = term(23) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, c, l, d)
end do 
end do 
end do 


do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(24) = term(24) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, c, l, d)
end do 
end do 
end do 


do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(25) = term(25) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, d, l, c)
end do 
end do 
end do 

term(25) = term(25) * (-2.000000000000003d+0) 


    eom_cc3_21_trans_aibjcj = 0 !!triple_w1 + !triple_w2
    do s = 0, 25
    eom_cc3_21_trans_aibjcj = eom_cc3_21_trans_aibjcj + term(s)
    end do

    end function eom_cc3_21_trans_aibjcj
    
    function eom_cc3_21_trans_aiajak(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_cc3_21_trans_aiajak  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, j, k 
    integer :: s ,d,l,e 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:37) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(a, j, i, k)
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(a, i, j, k)
    
    term(0) = term(0) + tvooo(a, i, k, j)
term(1) = term(1) + tvooo(a, j, k, i)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do d = nocc + 1, nactive 
term(2) = term(2) + tov(k, d) * t2(a,d,j,i)
term(3) = term(3) + tov(k, d) * t2(a,d,i,j)
term(4) = term(4) + t2(a,d,j,i) * tvvov(a, a, k, d)
term(5) = term(5) + t2(a,d,i,j) * tvvov(a, a, k, d)
term(6) = term(6) + t2(a,d,j,i) * tvvov(a, d, k, a)
term(7) = term(7) + t2(a,d,i,j) * tvvov(a, d, k, a)
end do 

term(2) = term(2) * (-0.9999999999999999d+0) 
term(3) = term(3) * (-0.9999999999999999d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (2.0000000000000004d+0) 

do l = 1, nocc 
term(8) = term(8) + t2(a,a,j,l) * tovoo(l, a, k, i)
term(9) = term(9) + t2(a,a,i,l) * tovoo(l, a, k, j)
term(10) = term(10) + t2(a,a,j,l) * tovoo(k, a, l, i)
term(11) = term(11) + t2(a,a,i,l) * tovoo(k, a, l, j)
end do 

term(10) = term(10) * (-2.0000000000000004d+0) 
term(11) = term(11) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(12) = term(12) + t2(a,d,j,l) * tovoo(k, d, l, i)
term(13) = term(13) + t2(a,d,j,i) * tovoo(k, d, l, l)
term(14) = term(14) + t2(a,d,i,l) * tovoo(k, d, l, j)
term(15) = term(15) + t2(a,d,i,j) * tovoo(k, d, l, l)
term(16) = term(16) + t2(a,d,j,l) * tovoo(l, d, k, i)
term(17) = term(17) + t2(a,d,i,l) * tovoo(l, d, k, j)
term(18) = term(18) + t2(a,d,j,i) * tovoo(l, d, k, l)
term(19) = term(19) + t2(a,d,i,j) * tovoo(l, d, k, l)
term(20) = term(20) + t3(nocc, nactive, a,a,d,i,j,l) * tovov(l, a, k, d)
term(21) = term(21) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, d, k, a)
term(22) = term(22) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, d, k, a)
term(23) = term(23) + t3(nocc, nactive, a,a,d,i,j,l) * tovov(l, d, k, a)
end do 
end do 

term(13) = term(13) * (-2.0000000000000004d+0) 
term(15) = term(15) * (-2.0000000000000004d+0) 
term(16) = term(16) * (-2.0000000000000004d+0) 
term(17) = term(17) * (-2.0000000000000004d+0) 
term(20) = term(20) * (-2.000000000000003d+0) 
term(21) = term(21) * (-2.000000000000003d+0) 
term(22) = term(22) * (-2.000000000000003d+0) 
term(23) = term(23) * (4.000000000000006d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(24) = term(24) + t2(a,d,l,j) * tovoo(k, d, l, i)
term(25) = term(25) + t2(a,d,l,i) * tovoo(k, d, l, j)
term(26) = term(26) + t2(a,d,l,j) * tovoo(l, d, k, i)
term(27) = term(27) + t2(a,d,l,i) * tovoo(l, d, k, j)
term(28) = term(28) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, a, k, d)
term(29) = term(29) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, a, k, d)
end do 
end do 


do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(30) = term(30) + t2(d,e,i,j) * tvvov(a, e, k, d)
end do 
end do 

term(30) = term(30) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(31) = term(31) + t2(d,e,i,j) * tvvov(a, d, k, e)
end do 
end do 

term(31) = term(31) * (-1.0000000000000002d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(32) = term(32) + t3(nocc, nactive, a,d,e,j,i,l) * tovov(l, d, k, e)
term(33) = term(33) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, d, k, e)
term(34) = term(34) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, d, k, e)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(35) = term(35) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, e, k, d)
term(36) = term(36) + t3(nocc, nactive, a,d,e,j,i,l) * tovov(l, e, k, d)
term(37) = term(37) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, e, k, d)
end do 
end do 
end do 

term(36) = term(36) * (-2.000000000000003d+0) 
term(37) = term(37) * (-2.000000000000003d+0) 


    eom_cc3_21_trans_aiajak = 0 !!triple_w1 + !triple_w2
    do s = 0, 37
    eom_cc3_21_trans_aiajak = eom_cc3_21_trans_aiajak + term(s)
    end do

    end function eom_cc3_21_trans_aiajak
    
    function eom_cc3_21_trans_aiaick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_cc3_21_trans_aiaick  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, c, k 
    integer :: s ,d,l 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,i,i) * tvvov(a, c, k, d)
term(1) = term(1) + t2(a,d,i,i) * tvvov(a, d, k, c)
end do 

term(0) = term(0) * (-1.0000000000000004d+0) 
term(1) = term(1) * (2.000000000000001d+0) 

do l = 1, nocc 
term(2) = term(2) + t2(a,a,i,l) * tovoo(l, c, k, i)
term(3) = term(3) + t2(a,a,i,l) * tovoo(k, c, l, i)
end do 

term(3) = term(3) * (-2.000000000000001d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(4) = term(4) + t3(nocc, nactive, a,a,d,i,l,i) * tovov(l, c, k, d)
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
term(5) = term(5) + t3(nocc, nactive, a,a,d,i,i,l) * tovov(l, c, k, d)
term(6) = term(6) + t3(nocc, nactive, a,a,d,i,l,i) * tovov(l, d, k, c)
term(7) = term(7) + t3(nocc, nactive, a,a,d,i,i,l) * tovov(l, d, k, c)
end do 
end do 

term(5) = term(5) * (-1.0000000000000016d+0) 
term(6) = term(6) * (-2.0000000000000027d+0) 
term(7) = term(7) * (2.000000000000003d+0) 


    eom_cc3_21_trans_aiaick = 0 !!triple_w1 + !triple_w2
    do s = 0, 7
    eom_cc3_21_trans_aiaick = eom_cc3_21_trans_aiaick + term(s)
    end do

    end function eom_cc3_21_trans_aiaick
    
    function eom_cc3_21_trans_aiajci(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_cc3_21_trans_aiajci  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, j, c 
    integer :: s ,l,d,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:25) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(a, a, c, j)
    
    term(0) = term(0) + tvvvo(a, c, a, j)


do l = 1, nocc 
term(1) = term(1) + tov(l, c) * t2(a,a,j,l)
term(2) = term(2) + t2(a,a,j,l) * tovoo(l, c, i, i)
term(3) = term(3) + t2(a,a,i,l) * tovoo(l, c, i, j)
term(4) = term(4) + t2(a,a,j,l) * tovoo(i, c, l, i)
term(5) = term(5) + t2(a,a,i,l) * tovoo(i, c, l, j)
end do 

term(1) = term(1) * (-0.9999999999999999d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 
term(5) = term(5) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(a,d,j,l) * tvvov(a, d, l, c)
term(7) = term(7) + t3(nocc, nactive, a,a,d,i,j,l) * tovov(l, c, i, d)
term(8) = term(8) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, d, i, c)
term(9) = term(9) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, d, i, c)
term(10) = term(10) + t3(nocc, nactive, a,a,d,i,j,l) * tovov(l, d, i, c)
end do 
end do 

term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-2.000000000000003d+0) 
term(8) = term(8) * (-2.000000000000003d+0) 
term(9) = term(9) * (-2.000000000000003d+0) 
term(10) = term(10) * (4.000000000000006d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(11) = term(11) + t2(a,d,l,j) * tvvov(a, d, l, c)
term(12) = term(12) + t2(a,d,l,j) * tvvov(a, c, l, d)
term(13) = term(13) + t2(a,d,j,l) * tvvov(a, c, l, d)
term(14) = term(14) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, c, i, d)
term(15) = term(15) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, c, i, d)
end do 
end do 

term(11) = term(11) * (-1.0000000000000002d+0) 
term(12) = term(12) * (-1.0000000000000002d+0) 
term(13) = term(13) * (2.0000000000000004d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(16) = term(16) + t2(a,a,l,m) * tovoo(l, c, m, j)
term(17) = term(17) + t2(a,a,j,l) * tovoo(l, c, m, m)
term(18) = term(18) + t2(a,a,j,l) * tovoo(m, c, l, m)
end do 
end do 

term(17) = term(17) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
term(19) = term(19) + t2(a,d,j,i) * tvvov(a, c, i, d)
term(20) = term(20) + t2(a,d,i,j) * tvvov(a, c, i, d)
term(21) = term(21) + t2(a,d,j,i) * tvvov(a, d, i, c)
term(22) = term(22) + t2(a,d,i,j) * tvvov(a, d, i, c)
end do 

term(19) = term(19) * (-1.0000000000000002d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(21) = term(21) * (2.0000000000000004d+0) 
term(22) = term(22) * (2.0000000000000004d+0) 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(23) = term(23) + t3(nocc, nactive, a,a,d,j,l,m) * tovov(m, c, l, d)
end do 
end do 
end do 


do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(24) = term(24) + t3(nocc, nactive, a,a,d,l,m,j) * tovov(m, d, l, c)
end do 
end do 
end do 


do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(25) = term(25) + t3(nocc, nactive, a,a,d,j,l,m) * tovov(m, d, l, c)
end do 
end do 
end do 

term(25) = term(25) * (-2.000000000000003d+0) 


    eom_cc3_21_trans_aiajci = 0 !!triple_w1 + !triple_w2
    do s = 0, 25
    eom_cc3_21_trans_aiajci = eom_cc3_21_trans_aiajci + term(s)
    end do

    end function eom_cc3_21_trans_aiajci
    
    function eom_cc3_21_trans_aiajcj(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_cc3_21_trans_aiajcj  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, j, c 
    integer :: s ,l,d,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:25) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(a, a, c, i)
    
    term(0) = term(0) + tvvvo(a, c, a, i)


do l = 1, nocc 
term(1) = term(1) + tov(l, c) * t2(a,a,i,l)
term(2) = term(2) + t2(a,a,j,l) * tovoo(l, c, j, i)
term(3) = term(3) + t2(a,a,i,l) * tovoo(l, c, j, j)
term(4) = term(4) + t2(a,a,j,l) * tovoo(j, c, l, i)
term(5) = term(5) + t2(a,a,i,l) * tovoo(j, c, l, j)
end do 

term(1) = term(1) * (-0.9999999999999999d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 
term(5) = term(5) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(6) = term(6) + t2(a,d,l,i) * tvvov(a, d, l, c)
term(7) = term(7) + t2(a,d,l,i) * tvvov(a, c, l, d)
term(8) = term(8) + t2(a,d,i,l) * tvvov(a, c, l, d)
term(9) = term(9) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, c, j, d)
term(10) = term(10) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, c, j, d)
end do 
end do 

term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(11) = term(11) + t2(a,d,i,l) * tvvov(a, d, l, c)
term(12) = term(12) + t3(nocc, nactive, a,a,d,i,j,l) * tovov(l, c, j, d)
term(13) = term(13) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, d, j, c)
term(14) = term(14) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, d, j, c)
term(15) = term(15) + t3(nocc, nactive, a,a,d,i,j,l) * tovov(l, d, j, c)
end do 
end do 

term(11) = term(11) * (-1.0000000000000002d+0) 
term(12) = term(12) * (-2.000000000000003d+0) 
term(13) = term(13) * (-2.000000000000003d+0) 
term(14) = term(14) * (-2.000000000000003d+0) 
term(15) = term(15) * (4.000000000000006d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(16) = term(16) + t2(a,a,l,m) * tovoo(m, c, l, i)
term(17) = term(17) + t2(a,a,i,l) * tovoo(l, c, m, m)
term(18) = term(18) + t2(a,a,i,l) * tovoo(m, c, l, m)
end do 
end do 

term(17) = term(17) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
term(19) = term(19) + t2(a,d,j,i) * tvvov(a, c, j, d)
term(20) = term(20) + t2(a,d,i,j) * tvvov(a, c, j, d)
term(21) = term(21) + t2(a,d,j,i) * tvvov(a, d, j, c)
term(22) = term(22) + t2(a,d,i,j) * tvvov(a, d, j, c)
end do 

term(19) = term(19) * (-1.0000000000000002d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(21) = term(21) * (2.0000000000000004d+0) 
term(22) = term(22) * (2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(23) = term(23) + t3(nocc, nactive, a,a,d,l,m,i) * tovov(m, c, l, d)
end do 
end do 
end do 


do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(24) = term(24) + t3(nocc, nactive, a,a,d,i,l,m) * tovov(m, c, l, d)
end do 
end do 
end do 


do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(25) = term(25) + t3(nocc, nactive, a,a,d,i,l,m) * tovov(m, d, l, c)
end do 
end do 
end do 

term(25) = term(25) * (-2.000000000000003d+0) 


    eom_cc3_21_trans_aiajcj = 0 !!triple_w1 + !triple_w2
    do s = 0, 25
    eom_cc3_21_trans_aiajcj = eom_cc3_21_trans_aiajcj + term(s)
    end do

    end function eom_cc3_21_trans_aiajcj
    
    function eom_cc3_21_trans_aibiak(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_cc3_21_trans_aibiak  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, k 
    integer :: s ,d,l,e 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:25) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(b, i, i, k)
    
    term(0) = term(0) + tvooo(b, i, k, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do d = nocc + 1, nactive 
term(1) = term(1) + tov(k, d) * t2(b,d,i,i)
term(2) = term(2) + t2(b,d,i,i) * tvvov(a, a, k, d)
term(3) = term(3) + t2(a,d,i,i) * tvvov(b, a, k, d)
term(4) = term(4) + t2(b,d,i,i) * tvvov(a, d, k, a)
term(5) = term(5) + t2(a,d,i,i) * tvvov(b, d, k, a)
end do 

term(1) = term(1) * (-0.9999999999999999d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(b,d,i,l) * tovoo(k, d, l, i)
term(7) = term(7) + t2(b,d,i,i) * tovoo(k, d, l, l)
term(8) = term(8) + t2(b,d,i,l) * tovoo(l, d, k, i)
term(9) = term(9) + t2(b,d,i,i) * tovoo(l, d, k, l)
term(10) = term(10) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, a, k, d)
term(11) = term(11) + t3(nocc, nactive, a,b,d,i,i,l) * tovov(l, a, k, d)
term(12) = term(12) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, d, k, a)
term(13) = term(13) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, d, k, a)
term(14) = term(14) + t3(nocc, nactive, a,b,d,i,i,l) * tovov(l, d, k, a)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 
term(8) = term(8) * (-2.0000000000000004d+0) 
term(11) = term(11) * (-2.000000000000003d+0) 
term(12) = term(12) * (-2.000000000000003d+0) 
term(13) = term(13) * (-2.000000000000003d+0) 
term(14) = term(14) * (4.000000000000006d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(15) = term(15) + t2(b,d,l,i) * tovoo(k, d, l, i)
term(16) = term(16) + t2(b,d,l,i) * tovoo(l, d, k, i)
term(17) = term(17) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, a, k, d)
end do 
end do 


do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(18) = term(18) + t2(d,e,i,i) * tvvov(b, e, k, d)
end do 
end do 

term(18) = term(18) * (-1.0000000000000002d+0) 

do l = 1, nocc 
term(19) = term(19) + t2(a,b,l,i) * tovoo(l, a, k, i)
term(20) = term(20) + t2(a,b,i,l) * tovoo(l, a, k, i)
term(21) = term(21) + t2(a,b,l,i) * tovoo(k, a, l, i)
term(22) = term(22) + t2(a,b,i,l) * tovoo(k, a, l, i)
end do 

term(21) = term(21) * (-2.0000000000000004d+0) 
term(22) = term(22) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(23) = term(23) + t3(nocc, nactive, b,d,e,i,i,l) * tovov(l, d, k, e)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(24) = term(24) + t3(nocc, nactive, b,d,e,l,i,i) * tovov(l, e, k, d)
term(25) = term(25) + t3(nocc, nactive, b,d,e,i,i,l) * tovov(l, e, k, d)
end do 
end do 
end do 

term(25) = term(25) * (-2.000000000000003d+0) 


    eom_cc3_21_trans_aibiak = 0 !!triple_w1 + !triple_w2
    do s = 0, 25
    eom_cc3_21_trans_aibiak = eom_cc3_21_trans_aibiak + term(s)
    end do

    end function eom_cc3_21_trans_aibiak
    
    function eom_cc3_21_trans_aibjai(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_cc3_21_trans_aibjai  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j 
    integer :: s ,d,l,e,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:37) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(b, j, i, i)
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(a, b, a, j)
    
    term(0) = term(0) + tvvvo(a, a, b, j)
term(1) = term(1) + tvooo(b, j, i, i)

term(1) = term(1) * (-0.9999999999999999d+0) 

do d = nocc + 1, nactive 
term(2) = term(2) + tov(i, d) * t2(b,d,j,i)
term(3) = term(3) + t2(b,d,j,i) * tvvov(a, a, i, d)
term(4) = term(4) + t2(a,d,i,j) * tvvov(b, a, i, d)
term(5) = term(5) + t2(b,d,j,i) * tvvov(a, d, i, a)
term(6) = term(6) + t2(a,d,i,j) * tvvov(b, d, i, a)
end do 

term(2) = term(2) * (-0.9999999999999999d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (2.0000000000000004d+0) 
term(6) = term(6) * (2.0000000000000004d+0) 

do l = 1, nocc 
term(7) = term(7) + tov(l, a) * t2(a,b,l,j)
term(8) = term(8) + t2(a,b,l,j) * tovoo(l, a, i, i)
term(9) = term(9) + t2(a,b,i,l) * tovoo(l, a, i, j)
term(10) = term(10) + t2(a,b,l,j) * tovoo(i, a, l, i)
term(11) = term(11) + t2(a,b,i,l) * tovoo(i, a, l, j)
end do 

term(7) = term(7) * (-0.9999999999999999d+0) 
term(10) = term(10) * (-2.0000000000000004d+0) 
term(11) = term(11) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(12) = term(12) + t2(b,d,j,l) * tovoo(i, d, l, i)
term(13) = term(13) + t2(b,d,j,i) * tovoo(i, d, l, l)
term(14) = term(14) + t2(b,d,j,l) * tovoo(l, d, i, i)
term(15) = term(15) + t2(b,d,j,i) * tovoo(l, d, i, l)
term(16) = term(16) + t2(b,d,j,l) * tvvov(a, d, l, a)
term(17) = term(17) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, a, i, d)
term(18) = term(18) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, a, i, d)
term(19) = term(19) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, d, i, a)
term(20) = term(20) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, d, i, a)
term(21) = term(21) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, d, i, a)
end do 
end do 

term(13) = term(13) * (-2.0000000000000004d+0) 
term(14) = term(14) * (-2.0000000000000004d+0) 
term(16) = term(16) * (-1.0000000000000002d+0) 
term(18) = term(18) * (-2.000000000000003d+0) 
term(19) = term(19) * (-2.000000000000003d+0) 
term(20) = term(20) * (-2.000000000000003d+0) 
term(21) = term(21) * (4.000000000000006d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(22) = term(22) + t2(b,d,l,i) * tovoo(i, d, l, j)
term(23) = term(23) + t2(b,d,l,j) * tovoo(l, d, i, i)
term(24) = term(24) + t2(a,d,l,j) * tvvov(b, d, l, a)
term(25) = term(25) + t2(b,d,l,j) * tvvov(a, a, l, d)
term(26) = term(26) + t2(b,d,j,l) * tvvov(a, a, l, d)
term(27) = term(27) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, a, i, d)
end do 
end do 

term(24) = term(24) * (-1.0000000000000002d+0) 
term(25) = term(25) * (-1.0000000000000002d+0) 
term(26) = term(26) * (2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(28) = term(28) + t2(d,e,i,j) * tvvov(b, e, i, d)
end do 
end do 

term(28) = term(28) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(29) = term(29) + t2(a,b,l,m) * tovoo(l, a, m, j)
term(30) = term(30) + t2(a,b,l,j) * tovoo(l, a, m, m)
term(31) = term(31) + t2(a,b,l,j) * tovoo(m, a, l, m)
end do 
end do 

term(30) = term(30) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(32) = term(32) + t3(nocc, nactive, b,d,e,j,i,l) * tovov(l, d, i, e)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(33) = term(33) + t3(nocc, nactive, b,d,e,l,i,j) * tovov(l, e, i, d)
term(34) = term(34) + t3(nocc, nactive, b,d,e,j,i,l) * tovov(l, e, i, d)
end do 
end do 
end do 

term(34) = term(34) * (-2.000000000000003d+0) 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(35) = term(35) + t3(nocc, nactive, a,b,d,l,j,m) * tovov(m, a, l, d)
term(36) = term(36) + t3(nocc, nactive, a,b,d,l,j,m) * tovov(m, d, l, a)
end do 
end do 
end do 

term(36) = term(36) * (-2.000000000000003d+0) 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(37) = term(37) + t3(nocc, nactive, a,b,d,l,m,j) * tovov(m, d, l, a)
end do 
end do 
end do 



    eom_cc3_21_trans_aibjai = 0 !!triple_w1 + !triple_w2
    do s = 0, 37
    eom_cc3_21_trans_aibjai = eom_cc3_21_trans_aibjai + term(s)
    end do

    end function eom_cc3_21_trans_aibjai
    
    function eom_cc3_21_trans_aibjaj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_cc3_21_trans_aibjaj  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j 
    integer :: s ,d,l,e,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:37) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(b, j, i, j)
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(b, a, a, i)
    
    term(0) = term(0) + tvvvo(b, a, a, i)
term(1) = term(1) + tvooo(b, j, j, i)

term(1) = term(1) * (-0.9999999999999999d+0) 

do d = nocc + 1, nactive 
term(2) = term(2) + tov(j, d) * t2(b,d,j,i)
term(3) = term(3) + t2(b,d,j,i) * tvvov(a, a, j, d)
term(4) = term(4) + t2(a,d,i,j) * tvvov(b, a, j, d)
term(5) = term(5) + t2(b,d,j,i) * tvvov(a, d, j, a)
term(6) = term(6) + t2(a,d,i,j) * tvvov(b, d, j, a)
end do 

term(2) = term(2) * (-0.9999999999999999d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (2.0000000000000004d+0) 
term(6) = term(6) * (2.0000000000000004d+0) 

do l = 1, nocc 
term(7) = term(7) + tov(l, a) * t2(a,b,i,l)
term(8) = term(8) + t2(a,b,l,j) * tovoo(l, a, j, i)
term(9) = term(9) + t2(a,b,i,l) * tovoo(l, a, j, j)
term(10) = term(10) + t2(a,b,l,j) * tovoo(j, a, l, i)
term(11) = term(11) + t2(a,b,i,l) * tovoo(j, a, l, j)
end do 

term(7) = term(7) * (-0.9999999999999999d+0) 
term(10) = term(10) * (-2.0000000000000004d+0) 
term(11) = term(11) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(12) = term(12) + t2(b,d,j,l) * tovoo(j, d, l, i)
term(13) = term(13) + t2(b,d,j,i) * tovoo(j, d, l, l)
term(14) = term(14) + t2(b,d,j,l) * tovoo(l, d, j, i)
term(15) = term(15) + t2(b,d,j,i) * tovoo(l, d, j, l)
term(16) = term(16) + t2(a,d,i,l) * tvvov(b, d, l, a)
term(17) = term(17) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, a, j, d)
term(18) = term(18) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, a, j, d)
term(19) = term(19) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, d, j, a)
term(20) = term(20) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, d, j, a)
term(21) = term(21) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, d, j, a)
end do 
end do 

term(13) = term(13) * (-2.0000000000000004d+0) 
term(14) = term(14) * (-2.0000000000000004d+0) 
term(16) = term(16) * (-1.0000000000000002d+0) 
term(18) = term(18) * (-2.000000000000003d+0) 
term(19) = term(19) * (-2.000000000000003d+0) 
term(20) = term(20) * (-2.000000000000003d+0) 
term(21) = term(21) * (4.000000000000006d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(22) = term(22) + t2(b,d,l,i) * tovoo(j, d, l, j)
term(23) = term(23) + t2(b,d,l,j) * tovoo(l, d, j, i)
term(24) = term(24) + t2(b,d,l,i) * tvvov(a, d, l, a)
term(25) = term(25) + t2(a,d,l,i) * tvvov(b, a, l, d)
term(26) = term(26) + t2(a,d,i,l) * tvvov(b, a, l, d)
term(27) = term(27) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, a, j, d)
end do 
end do 

term(24) = term(24) * (-1.0000000000000002d+0) 
term(25) = term(25) * (-1.0000000000000002d+0) 
term(26) = term(26) * (2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(28) = term(28) + t2(d,e,i,j) * tvvov(b, e, j, d)
end do 
end do 

term(28) = term(28) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(29) = term(29) + t2(a,b,l,m) * tovoo(m, a, l, i)
term(30) = term(30) + t2(a,b,i,m) * tovoo(m, a, l, l)
term(31) = term(31) + t2(a,b,i,m) * tovoo(l, a, m, l)
end do 
end do 

term(30) = term(30) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(32) = term(32) + t3(nocc, nactive, b,d,e,j,i,l) * tovov(l, d, j, e)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(33) = term(33) + t3(nocc, nactive, b,d,e,l,i,j) * tovov(l, e, j, d)
term(34) = term(34) + t3(nocc, nactive, b,d,e,j,i,l) * tovov(l, e, j, d)
end do 
end do 
end do 

term(34) = term(34) * (-2.000000000000003d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(35) = term(35) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, a, l, d)
end do 
end do 
end do 


do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(36) = term(36) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, a, l, d)
end do 
end do 
end do 


do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(37) = term(37) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, d, l, a)
end do 
end do 
end do 

term(37) = term(37) * (-2.000000000000003d+0) 


    eom_cc3_21_trans_aibjaj = 0 !!triple_w1 + !triple_w2
    do s = 0, 37
    eom_cc3_21_trans_aibjaj = eom_cc3_21_trans_aibjaj + term(s)
    end do

    end function eom_cc3_21_trans_aibjaj
    
    function eom_cc3_21_trans_aibibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_cc3_21_trans_aibibk  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, k 
    integer :: s ,d,l,e 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:25) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(a, i, i, k)
    
    term(0) = term(0) + tvooo(a, i, k, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do d = nocc + 1, nactive 
term(1) = term(1) + tov(k, d) * t2(a,d,i,i)
term(2) = term(2) + t2(b,d,i,i) * tvvov(a, b, k, d)
term(3) = term(3) + t2(a,d,i,i) * tvvov(b, b, k, d)
term(4) = term(4) + t2(b,d,i,i) * tvvov(a, d, k, b)
term(5) = term(5) + t2(a,d,i,i) * tvvov(b, d, k, b)
end do 

term(1) = term(1) * (-0.9999999999999999d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(6) = term(6) + t2(a,d,l,i) * tovoo(k, d, l, i)
term(7) = term(7) + t2(a,d,l,i) * tovoo(l, d, k, i)
term(8) = term(8) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, b, k, d)
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
term(9) = term(9) + t2(a,d,i,l) * tovoo(k, d, l, i)
term(10) = term(10) + t2(a,d,i,i) * tovoo(k, d, l, l)
term(11) = term(11) + t2(a,d,i,l) * tovoo(l, d, k, i)
term(12) = term(12) + t2(a,d,i,i) * tovoo(l, d, k, l)
term(13) = term(13) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, b, k, d)
term(14) = term(14) + t3(nocc, nactive, a,b,d,i,i,l) * tovov(l, b, k, d)
term(15) = term(15) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, d, k, b)
term(16) = term(16) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, d, k, b)
term(17) = term(17) + t3(nocc, nactive, a,b,d,i,i,l) * tovov(l, d, k, b)
end do 
end do 

term(10) = term(10) * (-2.0000000000000004d+0) 
term(11) = term(11) * (-2.0000000000000004d+0) 
term(14) = term(14) * (-2.000000000000003d+0) 
term(15) = term(15) * (-2.000000000000003d+0) 
term(16) = term(16) * (-2.000000000000003d+0) 
term(17) = term(17) * (4.000000000000006d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(18) = term(18) + t2(d,e,i,i) * tvvov(a, d, k, e)
end do 
end do 

term(18) = term(18) * (-1.0000000000000002d+0) 

do l = 1, nocc 
term(19) = term(19) + t2(a,b,l,i) * tovoo(l, b, k, i)
term(20) = term(20) + t2(a,b,i,l) * tovoo(l, b, k, i)
term(21) = term(21) + t2(a,b,l,i) * tovoo(k, b, l, i)
term(22) = term(22) + t2(a,b,i,l) * tovoo(k, b, l, i)
end do 

term(21) = term(21) * (-2.0000000000000004d+0) 
term(22) = term(22) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(23) = term(23) + t3(nocc, nactive, a,d,e,i,i,l) * tovov(l, d, k, e)
term(24) = term(24) + t3(nocc, nactive, a,d,e,l,i,i) * tovov(l, d, k, e)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(25) = term(25) + t3(nocc, nactive, a,d,e,i,i,l) * tovov(l, e, k, d)
end do 
end do 
end do 

term(25) = term(25) * (-2.000000000000003d+0) 


    eom_cc3_21_trans_aibibk = 0 !!triple_w1 + !triple_w2
    do s = 0, 25
    eom_cc3_21_trans_aibibk = eom_cc3_21_trans_aibibk + term(s)
    end do

    end function eom_cc3_21_trans_aibibk
    
    function eom_cc3_21_trans_aibjbi(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_cc3_21_trans_aibjbi  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j 
    integer :: s ,d,l,e,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:37) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(a, i, j, i)
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(a, b, b, j)
    
    term(0) = term(0) + tvvvo(a, b, b, j)
term(1) = term(1) + tvooo(a, i, i, j)

term(1) = term(1) * (-0.9999999999999999d+0) 

do d = nocc + 1, nactive 
term(2) = term(2) + tov(i, d) * t2(a,d,i,j)
term(3) = term(3) + t2(b,d,j,i) * tvvov(a, b, i, d)
term(4) = term(4) + t2(a,d,i,j) * tvvov(b, b, i, d)
term(5) = term(5) + t2(b,d,j,i) * tvvov(a, d, i, b)
term(6) = term(6) + t2(a,d,i,j) * tvvov(b, d, i, b)
end do 

term(2) = term(2) * (-0.9999999999999999d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (2.0000000000000004d+0) 
term(6) = term(6) * (2.0000000000000004d+0) 

do l = 1, nocc 
term(7) = term(7) + tov(l, b) * t2(a,b,l,j)
term(8) = term(8) + t2(a,b,l,j) * tovoo(l, b, i, i)
term(9) = term(9) + t2(a,b,i,l) * tovoo(l, b, i, j)
term(10) = term(10) + t2(a,b,l,j) * tovoo(i, b, l, i)
term(11) = term(11) + t2(a,b,i,l) * tovoo(i, b, l, j)
end do 

term(7) = term(7) * (-0.9999999999999999d+0) 
term(10) = term(10) * (-2.0000000000000004d+0) 
term(11) = term(11) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(12) = term(12) + t2(a,d,l,j) * tovoo(i, d, l, i)
term(13) = term(13) + t2(a,d,l,i) * tovoo(l, d, i, j)
term(14) = term(14) + t2(a,d,l,j) * tvvov(b, d, l, b)
term(15) = term(15) + t2(b,d,l,j) * tvvov(a, b, l, d)
term(16) = term(16) + t2(b,d,j,l) * tvvov(a, b, l, d)
term(17) = term(17) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, b, i, d)
end do 
end do 

term(14) = term(14) * (-1.0000000000000002d+0) 
term(15) = term(15) * (-1.0000000000000002d+0) 
term(16) = term(16) * (2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(18) = term(18) + t2(a,d,i,l) * tovoo(i, d, l, j)
term(19) = term(19) + t2(a,d,i,j) * tovoo(i, d, l, l)
term(20) = term(20) + t2(a,d,i,l) * tovoo(l, d, i, j)
term(21) = term(21) + t2(a,d,i,j) * tovoo(l, d, i, l)
term(22) = term(22) + t2(b,d,j,l) * tvvov(a, d, l, b)
term(23) = term(23) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, b, i, d)
term(24) = term(24) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, b, i, d)
term(25) = term(25) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, d, i, b)
term(26) = term(26) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, d, i, b)
term(27) = term(27) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, d, i, b)
end do 
end do 

term(19) = term(19) * (-2.0000000000000004d+0) 
term(20) = term(20) * (-2.0000000000000004d+0) 
term(22) = term(22) * (-1.0000000000000002d+0) 
term(24) = term(24) * (-2.000000000000003d+0) 
term(25) = term(25) * (-2.000000000000003d+0) 
term(26) = term(26) * (-2.000000000000003d+0) 
term(27) = term(27) * (4.000000000000006d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(28) = term(28) + t2(d,e,i,j) * tvvov(a, d, i, e)
end do 
end do 

term(28) = term(28) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(29) = term(29) + t2(a,b,l,m) * tovoo(l, b, m, j)
term(30) = term(30) + t2(a,b,l,j) * tovoo(l, b, m, m)
term(31) = term(31) + t2(a,b,l,j) * tovoo(m, b, l, m)
end do 
end do 

term(30) = term(30) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(32) = term(32) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, d, i, e)
term(33) = term(33) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, d, i, e)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(34) = term(34) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, e, i, d)
end do 
end do 
end do 

term(34) = term(34) * (-2.000000000000003d+0) 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(35) = term(35) + t3(nocc, nactive, a,b,d,l,j,m) * tovov(m, b, l, d)
term(36) = term(36) + t3(nocc, nactive, a,b,d,l,j,m) * tovov(m, d, l, b)
end do 
end do 
end do 

term(36) = term(36) * (-2.000000000000003d+0) 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(37) = term(37) + t3(nocc, nactive, a,b,d,l,m,j) * tovov(m, d, l, b)
end do 
end do 
end do 



    eom_cc3_21_trans_aibjbi = 0 !!triple_w1 + !triple_w2
    do s = 0, 37
    eom_cc3_21_trans_aibjbi = eom_cc3_21_trans_aibjbi + term(s)
    end do

    end function eom_cc3_21_trans_aibjbi
    
    function eom_cc3_21_trans_aibjbj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_cc3_21_trans_aibjbj  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j 
    integer :: s ,d,l,e,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:37) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(a, i, j, j)
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(b, a, b, i)
    
    term(0) = term(0) + tvvvo(b, b, a, i)
term(1) = term(1) + tvooo(a, i, j, j)

term(1) = term(1) * (-0.9999999999999999d+0) 

do d = nocc + 1, nactive 
term(2) = term(2) + tov(j, d) * t2(a,d,i,j)
term(3) = term(3) + t2(b,d,j,i) * tvvov(a, b, j, d)
term(4) = term(4) + t2(a,d,i,j) * tvvov(b, b, j, d)
term(5) = term(5) + t2(b,d,j,i) * tvvov(a, d, j, b)
term(6) = term(6) + t2(a,d,i,j) * tvvov(b, d, j, b)
end do 

term(2) = term(2) * (-0.9999999999999999d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (2.0000000000000004d+0) 
term(6) = term(6) * (2.0000000000000004d+0) 

do l = 1, nocc 
term(7) = term(7) + tov(l, b) * t2(a,b,i,l)
term(8) = term(8) + t2(a,b,l,j) * tovoo(l, b, j, i)
term(9) = term(9) + t2(a,b,i,l) * tovoo(l, b, j, j)
term(10) = term(10) + t2(a,b,l,j) * tovoo(j, b, l, i)
term(11) = term(11) + t2(a,b,i,l) * tovoo(j, b, l, j)
end do 

term(7) = term(7) * (-0.9999999999999999d+0) 
term(10) = term(10) * (-2.0000000000000004d+0) 
term(11) = term(11) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(12) = term(12) + t2(a,d,l,j) * tovoo(j, d, l, i)
term(13) = term(13) + t2(a,d,l,i) * tovoo(l, d, j, j)
term(14) = term(14) + t2(b,d,l,i) * tvvov(a, d, l, b)
term(15) = term(15) + t2(a,d,l,i) * tvvov(b, b, l, d)
term(16) = term(16) + t2(a,d,i,l) * tvvov(b, b, l, d)
term(17) = term(17) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, b, j, d)
end do 
end do 

term(14) = term(14) * (-1.0000000000000002d+0) 
term(15) = term(15) * (-1.0000000000000002d+0) 
term(16) = term(16) * (2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(18) = term(18) + t2(a,d,i,l) * tovoo(j, d, l, j)
term(19) = term(19) + t2(a,d,i,j) * tovoo(j, d, l, l)
term(20) = term(20) + t2(a,d,i,l) * tovoo(l, d, j, j)
term(21) = term(21) + t2(a,d,i,j) * tovoo(l, d, j, l)
term(22) = term(22) + t2(a,d,i,l) * tvvov(b, d, l, b)
term(23) = term(23) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, b, j, d)
term(24) = term(24) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, b, j, d)
term(25) = term(25) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, d, j, b)
term(26) = term(26) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, d, j, b)
term(27) = term(27) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, d, j, b)
end do 
end do 

term(19) = term(19) * (-2.0000000000000004d+0) 
term(20) = term(20) * (-2.0000000000000004d+0) 
term(22) = term(22) * (-1.0000000000000002d+0) 
term(24) = term(24) * (-2.000000000000003d+0) 
term(25) = term(25) * (-2.000000000000003d+0) 
term(26) = term(26) * (-2.000000000000003d+0) 
term(27) = term(27) * (4.000000000000006d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(28) = term(28) + t2(d,e,i,j) * tvvov(a, d, j, e)
end do 
end do 

term(28) = term(28) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(29) = term(29) + t2(a,b,l,m) * tovoo(m, b, l, i)
term(30) = term(30) + t2(a,b,i,l) * tovoo(l, b, m, m)
term(31) = term(31) + t2(a,b,i,l) * tovoo(m, b, l, m)
end do 
end do 

term(30) = term(30) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(32) = term(32) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, d, j, e)
term(33) = term(33) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, d, j, e)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(34) = term(34) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, e, j, d)
end do 
end do 
end do 

term(34) = term(34) * (-2.000000000000003d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(35) = term(35) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, b, l, d)
end do 
end do 
end do 


do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(36) = term(36) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, b, l, d)
end do 
end do 
end do 


do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(37) = term(37) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, d, l, b)
end do 
end do 
end do 

term(37) = term(37) * (-2.000000000000003d+0) 


    eom_cc3_21_trans_aibjbj = 0 !!triple_w1 + !triple_w2
    do s = 0, 37
    eom_cc3_21_trans_aibjbj = eom_cc3_21_trans_aibjbj + term(s)
    end do

    end function eom_cc3_21_trans_aibjbj
    
    function eom_cc3_21_trans_aibici(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_cc3_21_trans_aibici  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, c 
    integer :: s ,l,d,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:37) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(a, b, c, i)
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(b, a, c, i)
    
    term(0) = term(0) + tvvvo(b, c, a, i)
term(1) = term(1) + tvvvo(a, c, b, i)


do l = 1, nocc 
term(2) = term(2) + tov(l, c) * t2(a,b,l,i)
term(3) = term(3) + tov(l, c) * t2(a,b,i,l)
term(4) = term(4) + t2(a,b,l,i) * tovoo(l, c, i, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(l, c, i, i)
term(6) = term(6) + t2(a,b,l,i) * tovoo(i, c, l, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(i, c, l, i)
end do 

term(2) = term(2) * (-0.9999999999999999d+0) 
term(3) = term(3) * (-0.9999999999999999d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 
term(7) = term(7) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
term(8) = term(8) + t2(b,d,i,i) * tvvov(a, c, i, d)
term(9) = term(9) + t2(a,d,i,i) * tvvov(b, c, i, d)
term(10) = term(10) + t2(b,d,i,i) * tvvov(a, d, i, c)
term(11) = term(11) + t2(a,d,i,i) * tvvov(b, d, i, c)
end do 

term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(10) = term(10) * (2.0000000000000004d+0) 
term(11) = term(11) * (2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(12) = term(12) + t2(b,d,i,l) * tvvov(a, d, l, c)
term(13) = term(13) + t2(a,d,i,l) * tvvov(b, d, l, c)
term(14) = term(14) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, c, i, d)
term(15) = term(15) + t3(nocc, nactive, a,b,d,i,i,l) * tovov(l, c, i, d)
term(16) = term(16) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, d, i, c)
term(17) = term(17) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, d, i, c)
term(18) = term(18) + t3(nocc, nactive, a,b,d,i,i,l) * tovov(l, d, i, c)
end do 
end do 

term(12) = term(12) * (-1.0000000000000002d+0) 
term(13) = term(13) * (-1.0000000000000002d+0) 
term(15) = term(15) * (-2.000000000000003d+0) 
term(16) = term(16) * (-2.000000000000003d+0) 
term(17) = term(17) * (-2.000000000000003d+0) 
term(18) = term(18) * (4.000000000000006d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(19) = term(19) + t2(a,d,l,i) * tvvov(b, d, l, c)
term(20) = term(20) + t2(b,d,l,i) * tvvov(a, d, l, c)
term(21) = term(21) + t2(b,d,l,i) * tvvov(a, c, l, d)
term(22) = term(22) + t2(b,d,i,l) * tvvov(a, c, l, d)
term(23) = term(23) + t2(a,d,l,i) * tvvov(b, c, l, d)
term(24) = term(24) + t2(a,d,i,l) * tvvov(b, c, l, d)
term(25) = term(25) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, c, i, d)
end do 
end do 

term(19) = term(19) * (-1.0000000000000002d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(21) = term(21) * (-1.0000000000000002d+0) 
term(22) = term(22) * (2.0000000000000004d+0) 
term(23) = term(23) * (-1.0000000000000002d+0) 
term(24) = term(24) * (2.0000000000000004d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(26) = term(26) + t2(a,b,l,m) * tovoo(m, c, l, i)
term(27) = term(27) + t2(a,b,l,m) * tovoo(l, c, m, i)
term(28) = term(28) + t2(a,b,l,i) * tovoo(l, c, m, m)
term(29) = term(29) + t2(a,b,i,l) * tovoo(l, c, m, m)
term(30) = term(30) + t2(a,b,l,i) * tovoo(m, c, l, m)
term(31) = term(31) + t2(a,b,i,l) * tovoo(m, c, l, m)
end do 
end do 

term(28) = term(28) * (-2.0000000000000004d+0) 
term(29) = term(29) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(32) = term(32) + t3(nocc, nactive, a,b,d,l,i,m) * tovov(m, c, l, d)
term(33) = term(33) + t3(nocc, nactive, a,b,d,l,i,m) * tovov(m, d, l, c)
term(34) = term(34) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, d, l, c)
end do 
end do 
end do 

term(33) = term(33) * (-2.000000000000003d+0) 
term(34) = term(34) * (-2.000000000000003d+0) 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(35) = term(35) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, d, l, c)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(36) = term(36) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, c, l, d)
end do 
end do 
end do 


do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(37) = term(37) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, c, l, d)
end do 
end do 
end do 



    eom_cc3_21_trans_aibici = 0 !!triple_w1 + !triple_w2
    do s = 0, 37
    eom_cc3_21_trans_aibici = eom_cc3_21_trans_aibici + term(s)
    end do

    end function eom_cc3_21_trans_aibici
    
    function eom_cc3_21_trans_aiaiak(t2, nocc, nactive, a, i, k) 
    real(F64) :: eom_cc3_21_trans_aiaiak  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, k 
    integer :: s ,d,l,e 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:21) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(a, i, i, k)
    
    term(0) = term(0) + tvooo(a, i, k, i)

term(0) = term(0) * (-0.9999999999999998d+0) 

do d = nocc + 1, nactive 
term(1) = term(1) + tov(k, d) * t2(a,d,i,i)
term(2) = term(2) + t2(a,d,i,i) * tvvov(a, a, k, d)
term(3) = term(3) + t2(a,d,i,i) * tvvov(a, d, k, a)
end do 

term(1) = term(1) * (-0.9999999999999998d+0) 
term(2) = term(2) * (-1.0000000000000004d+0) 
term(3) = term(3) * (2.000000000000001d+0) 

do l = 1, nocc 
term(4) = term(4) + t2(a,a,i,l) * tovoo(l, a, k, i)
term(5) = term(5) + t2(a,a,i,l) * tovoo(k, a, l, i)
end do 

term(5) = term(5) * (-2.000000000000001d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(a,d,i,l) * tovoo(k, d, l, i)
term(7) = term(7) + t2(a,d,i,i) * tovoo(k, d, l, l)
term(8) = term(8) + t2(a,d,i,l) * tovoo(l, d, k, i)
term(9) = term(9) + t2(a,d,i,i) * tovoo(l, d, k, l)
term(10) = term(10) + t3(nocc, nactive, a,a,d,i,i,l) * tovov(l, a, k, d)
term(11) = term(11) + t3(nocc, nactive, a,a,d,i,l,i) * tovov(l, d, k, a)
term(12) = term(12) + t3(nocc, nactive, a,a,d,i,i,l) * tovov(l, d, k, a)
end do 
end do 

term(7) = term(7) * (-2.000000000000001d+0) 
term(8) = term(8) * (-2.000000000000001d+0) 
term(10) = term(10) * (-1.0000000000000016d+0) 
term(11) = term(11) * (-2.0000000000000027d+0) 
term(12) = term(12) * (2.000000000000003d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(13) = term(13) + t2(a,d,l,i) * tovoo(k, d, l, i)
term(14) = term(14) + t2(a,d,l,i) * tovoo(l, d, k, i)
term(15) = term(15) + t3(nocc, nactive, a,a,d,i,l,i) * tovov(l, a, k, d)
end do 
end do 


do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(16) = term(16) + t2(d,e,i,i) * tvvov(a, e, k, d)
end do 
end do 

term(16) = term(16) * (-0.5000000000000001d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(17) = term(17) + t2(d,e,i,i) * tvvov(a, d, k, e)
end do 
end do 

term(17) = term(17) * (-0.5000000000000001d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(18) = term(18) + t3(nocc, nactive, a,d,e,i,i,l) * tovov(l, d, k, e)
term(19) = term(19) + t3(nocc, nactive, a,d,e,l,i,i) * tovov(l, d, k, e)
end do 
end do 
end do 

term(19) = term(19) * (0.5000000000000008d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(20) = term(20) + t3(nocc, nactive, a,d,e,l,i,i) * tovov(l, e, k, d)
term(21) = term(21) + t3(nocc, nactive, a,d,e,i,i,l) * tovov(l, e, k, d)
end do 
end do 
end do 

term(20) = term(20) * (0.5000000000000008d+0) 
term(21) = term(21) * (-2.0000000000000027d+0) 


    eom_cc3_21_trans_aiaiak = 0 !!triple_w1 + !triple_w2
    do s = 0, 21
    eom_cc3_21_trans_aiaiak = eom_cc3_21_trans_aiaiak + term(s)
    end do

    end function eom_cc3_21_trans_aiaiak
    
    function eom_cc3_21_trans_aiajai(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_cc3_21_trans_aiajai  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, j 
    integer :: s ,d,l,e,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:49) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(a, j, i, i)
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(a, i, j, i)
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(a, a, a, j)
    
    term(0) = term(0) + tvvvo(a, a, a, j)
term(1) = term(1) + tvooo(a, i, i, j)
term(2) = term(2) + tvooo(a, j, i, i)

term(1) = term(1) * (-0.9999999999999999d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do d = nocc + 1, nactive 
term(3) = term(3) + tov(i, d) * t2(a,d,j,i)
term(4) = term(4) + tov(i, d) * t2(a,d,i,j)
term(5) = term(5) + t2(a,d,j,i) * tvvov(a, a, i, d)
term(6) = term(6) + t2(a,d,i,j) * tvvov(a, a, i, d)
term(7) = term(7) + t2(a,d,j,i) * tvvov(a, d, i, a)
term(8) = term(8) + t2(a,d,i,j) * tvvov(a, d, i, a)
end do 

term(3) = term(3) * (-0.9999999999999999d+0) 
term(4) = term(4) * (-0.9999999999999999d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (2.0000000000000004d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 

do l = 1, nocc 
term(9) = term(9) + t2(a,a,j,l) * tovoo(l, a, i, i)
term(10) = term(10) + t2(a,a,i,l) * tovoo(l, a, i, j)
term(11) = term(11) + t2(a,a,j,l) * tovoo(i, a, l, i)
term(12) = term(12) + t2(a,a,i,l) * tovoo(i, a, l, j)
term(13) = term(13) + tov(l, a) * t2(a,a,j,l)
end do 

term(11) = term(11) * (-2.0000000000000004d+0) 
term(12) = term(12) * (-2.0000000000000004d+0) 
term(13) = term(13) * (-0.9999999999999999d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(14) = term(14) + t2(a,d,j,l) * tovoo(i, d, l, i)
term(15) = term(15) + t2(a,d,j,i) * tovoo(i, d, l, l)
term(16) = term(16) + t2(a,d,i,l) * tovoo(i, d, l, j)
term(17) = term(17) + t2(a,d,i,j) * tovoo(i, d, l, l)
term(18) = term(18) + t2(a,d,j,l) * tovoo(l, d, i, i)
term(19) = term(19) + t2(a,d,i,l) * tovoo(l, d, i, j)
term(20) = term(20) + t2(a,d,j,i) * tovoo(l, d, i, l)
term(21) = term(21) + t2(a,d,i,j) * tovoo(l, d, i, l)
term(22) = term(22) + t3(nocc, nactive, a,a,d,i,j,l) * tovov(l, a, i, d)
term(23) = term(23) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, d, i, a)
term(24) = term(24) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, d, i, a)
term(25) = term(25) + t3(nocc, nactive, a,a,d,i,j,l) * tovov(l, d, i, a)
term(26) = term(26) + t2(a,d,j,l) * tvvov(a, d, l, a)
end do 
end do 

term(15) = term(15) * (-2.0000000000000004d+0) 
term(17) = term(17) * (-2.0000000000000004d+0) 
term(18) = term(18) * (-2.0000000000000004d+0) 
term(19) = term(19) * (-2.0000000000000004d+0) 
term(22) = term(22) * (-2.000000000000003d+0) 
term(23) = term(23) * (-2.000000000000003d+0) 
term(24) = term(24) * (-2.000000000000003d+0) 
term(25) = term(25) * (4.000000000000006d+0) 
term(26) = term(26) * (-1.0000000000000002d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(27) = term(27) + t2(a,d,l,j) * tovoo(i, d, l, i)
term(28) = term(28) + t2(a,d,l,i) * tovoo(i, d, l, j)
term(29) = term(29) + t2(a,d,l,j) * tovoo(l, d, i, i)
term(30) = term(30) + t2(a,d,l,i) * tovoo(l, d, i, j)
term(31) = term(31) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, a, i, d)
term(32) = term(32) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, a, i, d)
term(33) = term(33) + t2(a,d,l,j) * tvvov(a, d, l, a)
term(34) = term(34) + t2(a,d,l,j) * tvvov(a, a, l, d)
term(35) = term(35) + t2(a,d,j,l) * tvvov(a, a, l, d)
end do 
end do 

term(33) = term(33) * (-1.0000000000000002d+0) 
term(34) = term(34) * (-1.0000000000000002d+0) 
term(35) = term(35) * (2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(36) = term(36) + t2(d,e,i,j) * tvvov(a, e, i, d)
end do 
end do 

term(36) = term(36) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(37) = term(37) + t2(d,e,i,j) * tvvov(a, d, i, e)
end do 
end do 

term(37) = term(37) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(38) = term(38) + t2(a,a,l,m) * tovoo(l, a, m, j)
term(39) = term(39) + t2(a,a,j,l) * tovoo(l, a, m, m)
term(40) = term(40) + t2(a,a,j,l) * tovoo(m, a, l, m)
end do 
end do 

term(39) = term(39) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(41) = term(41) + t3(nocc, nactive, a,d,e,j,i,l) * tovov(l, d, i, e)
term(42) = term(42) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, d, i, e)
term(43) = term(43) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, d, i, e)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(44) = term(44) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, e, i, d)
term(45) = term(45) + t3(nocc, nactive, a,d,e,j,i,l) * tovov(l, e, i, d)
term(46) = term(46) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, e, i, d)
end do 
end do 
end do 

term(45) = term(45) * (-2.000000000000003d+0) 
term(46) = term(46) * (-2.000000000000003d+0) 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(47) = term(47) + t3(nocc, nactive, a,a,d,j,l,m) * tovov(m, a, l, d)
end do 
end do 
end do 


do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(48) = term(48) + t3(nocc, nactive, a,a,d,l,m,j) * tovov(m, d, l, a)
end do 
end do 
end do 


do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(49) = term(49) + t3(nocc, nactive, a,a,d,j,l,m) * tovov(m, d, l, a)
end do 
end do 
end do 

term(49) = term(49) * (-2.000000000000003d+0) 


    eom_cc3_21_trans_aiajai = 0 !!triple_w1 + !triple_w2
    do s = 0, 49
    eom_cc3_21_trans_aiajai = eom_cc3_21_trans_aiajai + term(s)
    end do

    end function eom_cc3_21_trans_aiajai
    
    function eom_cc3_21_trans_aiajaj(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_cc3_21_trans_aiajaj  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, j 
    integer :: s ,d,l,e,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:49) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(a, j, i, j)
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(a, i, j, j)
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(a, a, a, i)
    
    term(0) = term(0) + tvvvo(a, a, a, i)
term(1) = term(1) + tvooo(a, i, j, j)
term(2) = term(2) + tvooo(a, j, j, i)

term(1) = term(1) * (-0.9999999999999999d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do d = nocc + 1, nactive 
term(3) = term(3) + tov(j, d) * t2(a,d,j,i)
term(4) = term(4) + tov(j, d) * t2(a,d,i,j)
term(5) = term(5) + t2(a,d,j,i) * tvvov(a, a, j, d)
term(6) = term(6) + t2(a,d,i,j) * tvvov(a, a, j, d)
term(7) = term(7) + t2(a,d,j,i) * tvvov(a, d, j, a)
term(8) = term(8) + t2(a,d,i,j) * tvvov(a, d, j, a)
end do 

term(3) = term(3) * (-0.9999999999999999d+0) 
term(4) = term(4) * (-0.9999999999999999d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (2.0000000000000004d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 

do l = 1, nocc 
term(9) = term(9) + t2(a,a,j,l) * tovoo(l, a, j, i)
term(10) = term(10) + t2(a,a,i,l) * tovoo(l, a, j, j)
term(11) = term(11) + t2(a,a,j,l) * tovoo(j, a, l, i)
term(12) = term(12) + t2(a,a,i,l) * tovoo(j, a, l, j)
term(13) = term(13) + tov(l, a) * t2(a,a,i,l)
end do 

term(11) = term(11) * (-2.0000000000000004d+0) 
term(12) = term(12) * (-2.0000000000000004d+0) 
term(13) = term(13) * (-0.9999999999999999d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(14) = term(14) + t2(a,d,j,l) * tovoo(j, d, l, i)
term(15) = term(15) + t2(a,d,j,i) * tovoo(j, d, l, l)
term(16) = term(16) + t2(a,d,i,l) * tovoo(j, d, l, j)
term(17) = term(17) + t2(a,d,i,j) * tovoo(j, d, l, l)
term(18) = term(18) + t2(a,d,j,l) * tovoo(l, d, j, i)
term(19) = term(19) + t2(a,d,i,l) * tovoo(l, d, j, j)
term(20) = term(20) + t2(a,d,j,i) * tovoo(l, d, j, l)
term(21) = term(21) + t2(a,d,i,j) * tovoo(l, d, j, l)
term(22) = term(22) + t3(nocc, nactive, a,a,d,i,j,l) * tovov(l, a, j, d)
term(23) = term(23) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, d, j, a)
term(24) = term(24) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, d, j, a)
term(25) = term(25) + t3(nocc, nactive, a,a,d,i,j,l) * tovov(l, d, j, a)
term(26) = term(26) + t2(a,d,i,l) * tvvov(a, d, l, a)
end do 
end do 

term(15) = term(15) * (-2.0000000000000004d+0) 
term(17) = term(17) * (-2.0000000000000004d+0) 
term(18) = term(18) * (-2.0000000000000004d+0) 
term(19) = term(19) * (-2.0000000000000004d+0) 
term(22) = term(22) * (-2.000000000000003d+0) 
term(23) = term(23) * (-2.000000000000003d+0) 
term(24) = term(24) * (-2.000000000000003d+0) 
term(25) = term(25) * (4.000000000000006d+0) 
term(26) = term(26) * (-1.0000000000000002d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(27) = term(27) + t2(a,d,l,j) * tovoo(j, d, l, i)
term(28) = term(28) + t2(a,d,l,i) * tovoo(j, d, l, j)
term(29) = term(29) + t2(a,d,l,j) * tovoo(l, d, j, i)
term(30) = term(30) + t2(a,d,l,i) * tovoo(l, d, j, j)
term(31) = term(31) + t3(nocc, nactive, a,a,d,j,l,i) * tovov(l, a, j, d)
term(32) = term(32) + t3(nocc, nactive, a,a,d,i,l,j) * tovov(l, a, j, d)
term(33) = term(33) + t2(a,d,l,i) * tvvov(a, d, l, a)
term(34) = term(34) + t2(a,d,l,i) * tvvov(a, a, l, d)
term(35) = term(35) + t2(a,d,i,l) * tvvov(a, a, l, d)
end do 
end do 

term(33) = term(33) * (-1.0000000000000002d+0) 
term(34) = term(34) * (-1.0000000000000002d+0) 
term(35) = term(35) * (2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(36) = term(36) + t2(d,e,i,j) * tvvov(a, e, j, d)
end do 
end do 

term(36) = term(36) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(37) = term(37) + t2(d,e,i,j) * tvvov(a, d, j, e)
end do 
end do 

term(37) = term(37) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(38) = term(38) + t2(a,a,l,m) * tovoo(m, a, l, i)
term(39) = term(39) + t2(a,a,i,l) * tovoo(l, a, m, m)
term(40) = term(40) + t2(a,a,i,l) * tovoo(m, a, l, m)
end do 
end do 

term(39) = term(39) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(41) = term(41) + t3(nocc, nactive, a,d,e,j,i,l) * tovov(l, d, j, e)
term(42) = term(42) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, d, j, e)
term(43) = term(43) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, d, j, e)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(44) = term(44) + t3(nocc, nactive, a,d,e,l,i,j) * tovov(l, e, j, d)
term(45) = term(45) + t3(nocc, nactive, a,d,e,j,i,l) * tovov(l, e, j, d)
term(46) = term(46) + t3(nocc, nactive, a,d,e,i,j,l) * tovov(l, e, j, d)
end do 
end do 
end do 

term(45) = term(45) * (-2.000000000000003d+0) 
term(46) = term(46) * (-2.000000000000003d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(47) = term(47) + t3(nocc, nactive, a,a,d,l,m,i) * tovov(m, a, l, d)
end do 
end do 
end do 


do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(48) = term(48) + t3(nocc, nactive, a,a,d,i,l,m) * tovov(m, a, l, d)
end do 
end do 
end do 


do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(49) = term(49) + t3(nocc, nactive, a,a,d,i,l,m) * tovov(m, d, l, a)
end do 
end do 
end do 

term(49) = term(49) * (-2.000000000000003d+0) 


    eom_cc3_21_trans_aiajaj = 0 !!triple_w1 + !triple_w2
    do s = 0, 49
    eom_cc3_21_trans_aiajaj = eom_cc3_21_trans_aiajaj + term(s)
    end do

    end function eom_cc3_21_trans_aiajaj
    
    function eom_cc3_21_trans_aiaici(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_cc3_21_trans_aiaici  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, c 
    integer :: s ,l,d,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:21) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(a, a, c, i)
    
    term(0) = term(0) + tvvvo(a, c, a, i)


do l = 1, nocc 
term(1) = term(1) + tov(l, c) * t2(a,a,i,l)
term(2) = term(2) + t2(a,a,i,l) * tovoo(l, c, i, i)
term(3) = term(3) + t2(a,a,i,l) * tovoo(i, c, l, i)
end do 

term(1) = term(1) * (-0.9999999999999998d+0) 
term(3) = term(3) * (-2.000000000000001d+0) 

do d = nocc + 1, nactive 
term(4) = term(4) + t2(a,d,i,i) * tvvov(a, c, i, d)
term(5) = term(5) + t2(a,d,i,i) * tvvov(a, d, i, c)
end do 

term(4) = term(4) * (-1.0000000000000004d+0) 
term(5) = term(5) * (2.000000000000001d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(a,d,i,l) * tvvov(a, d, l, c)
term(7) = term(7) + t3(nocc, nactive, a,a,d,i,i,l) * tovov(l, c, i, d)
term(8) = term(8) + t3(nocc, nactive, a,a,d,i,l,i) * tovov(l, d, i, c)
term(9) = term(9) + t3(nocc, nactive, a,a,d,i,i,l) * tovov(l, d, i, c)
end do 
end do 

term(6) = term(6) * (-1.0000000000000004d+0) 
term(7) = term(7) * (-1.0000000000000016d+0) 
term(8) = term(8) * (-2.0000000000000027d+0) 
term(9) = term(9) * (2.000000000000003d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(10) = term(10) + t2(a,d,l,i) * tvvov(a, d, l, c)
term(11) = term(11) + t2(a,d,l,i) * tvvov(a, c, l, d)
term(12) = term(12) + t2(a,d,i,l) * tvvov(a, c, l, d)
term(13) = term(13) + t3(nocc, nactive, a,a,d,i,l,i) * tovov(l, c, i, d)
end do 
end do 

term(10) = term(10) * (-1.0000000000000004d+0) 
term(11) = term(11) * (-1.0000000000000004d+0) 
term(12) = term(12) * (2.000000000000001d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(14) = term(14) + t2(a,a,l,m) * tovoo(m, c, l, i)
term(15) = term(15) + t2(a,a,l,m) * tovoo(l, c, m, i)
term(16) = term(16) + t2(a,a,i,l) * tovoo(l, c, m, m)
term(17) = term(17) + t2(a,a,i,l) * tovoo(m, c, l, m)
end do 
end do 

term(14) = term(14) * (0.5000000000000001d+0) 
term(15) = term(15) * (0.5000000000000001d+0) 
term(16) = term(16) * (-2.000000000000001d+0) 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(18) = term(18) + t3(nocc, nactive, a,a,d,i,l,m) * tovov(m, c, l, d)
end do 
end do 
end do 


do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(19) = term(19) + t3(nocc, nactive, a,a,d,l,m,i) * tovov(m, d, l, c)
end do 
end do 
end do 

term(19) = term(19) * (0.5000000000000008d+0) 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(20) = term(20) + t3(nocc, nactive, a,a,d,i,l,m) * tovov(m, d, l, c)
end do 
end do 
end do 

term(20) = term(20) * (-2.0000000000000027d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(21) = term(21) + t3(nocc, nactive, a,a,d,l,m,i) * tovov(m, c, l, d)
end do 
end do 
end do 

term(21) = term(21) * (0.5000000000000008d+0) 


    eom_cc3_21_trans_aiaici = 0 !!triple_w1 + !triple_w2
    do s = 0, 21
    eom_cc3_21_trans_aiaici = eom_cc3_21_trans_aiaici + term(s)
    end do

    end function eom_cc3_21_trans_aiaici
    
    function eom_cc3_21_trans_aibiai(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_cc3_21_trans_aibiai  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b 
    integer :: s ,d,l,e,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:49) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(b, i, i, i)
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(a, b, a, i)
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(b, a, a, i)
    
    term(0) = term(0) + tvvvo(b, a, a, i)
term(1) = term(1) + tvvvo(a, a, b, i)
term(2) = term(2) + tvooo(b, i, i, i)

term(2) = term(2) * (-0.9999999999999999d+0) 

do d = nocc + 1, nactive 
term(3) = term(3) + tov(i, d) * t2(b,d,i,i)
term(4) = term(4) + t2(b,d,i,i) * tvvov(a, a, i, d)
term(5) = term(5) + t2(a,d,i,i) * tvvov(b, a, i, d)
term(6) = term(6) + t2(b,d,i,i) * tvvov(a, d, i, a)
term(7) = term(7) + t2(a,d,i,i) * tvvov(b, d, i, a)
end do 

term(3) = term(3) * (-0.9999999999999999d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (2.0000000000000004d+0) 

do l = 1, nocc 
term(8) = term(8) + tov(l, a) * t2(a,b,l,i)
term(9) = term(9) + tov(l, a) * t2(a,b,i,l)
term(10) = term(10) + t2(a,b,l,i) * tovoo(l, a, i, i)
term(11) = term(11) + t2(a,b,i,l) * tovoo(l, a, i, i)
term(12) = term(12) + t2(a,b,l,i) * tovoo(i, a, l, i)
term(13) = term(13) + t2(a,b,i,l) * tovoo(i, a, l, i)
end do 

term(8) = term(8) * (-0.9999999999999999d+0) 
term(9) = term(9) * (-0.9999999999999999d+0) 
term(12) = term(12) * (-2.0000000000000004d+0) 
term(13) = term(13) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(14) = term(14) + t2(b,d,i,l) * tovoo(i, d, l, i)
term(15) = term(15) + t2(b,d,i,i) * tovoo(i, d, l, l)
term(16) = term(16) + t2(b,d,i,l) * tovoo(l, d, i, i)
term(17) = term(17) + t2(b,d,i,i) * tovoo(l, d, i, l)
term(18) = term(18) + t2(b,d,i,l) * tvvov(a, d, l, a)
term(19) = term(19) + t2(a,d,i,l) * tvvov(b, d, l, a)
term(20) = term(20) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, a, i, d)
term(21) = term(21) + t3(nocc, nactive, a,b,d,i,i,l) * tovov(l, a, i, d)
term(22) = term(22) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, d, i, a)
term(23) = term(23) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, d, i, a)
term(24) = term(24) + t3(nocc, nactive, a,b,d,i,i,l) * tovov(l, d, i, a)
end do 
end do 

term(15) = term(15) * (-2.0000000000000004d+0) 
term(16) = term(16) * (-2.0000000000000004d+0) 
term(18) = term(18) * (-1.0000000000000002d+0) 
term(19) = term(19) * (-1.0000000000000002d+0) 
term(21) = term(21) * (-2.000000000000003d+0) 
term(22) = term(22) * (-2.000000000000003d+0) 
term(23) = term(23) * (-2.000000000000003d+0) 
term(24) = term(24) * (4.000000000000006d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(25) = term(25) + t2(b,d,l,i) * tovoo(i, d, l, i)
term(26) = term(26) + t2(b,d,l,i) * tovoo(l, d, i, i)
term(27) = term(27) + t2(a,d,l,i) * tvvov(b, d, l, a)
term(28) = term(28) + t2(b,d,l,i) * tvvov(a, d, l, a)
term(29) = term(29) + t2(b,d,l,i) * tvvov(a, a, l, d)
term(30) = term(30) + t2(b,d,i,l) * tvvov(a, a, l, d)
term(31) = term(31) + t2(a,d,l,i) * tvvov(b, a, l, d)
term(32) = term(32) + t2(a,d,i,l) * tvvov(b, a, l, d)
term(33) = term(33) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, a, i, d)
end do 
end do 

term(27) = term(27) * (-1.0000000000000002d+0) 
term(28) = term(28) * (-1.0000000000000002d+0) 
term(29) = term(29) * (-1.0000000000000002d+0) 
term(30) = term(30) * (2.0000000000000004d+0) 
term(31) = term(31) * (-1.0000000000000002d+0) 
term(32) = term(32) * (2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(34) = term(34) + t2(d,e,i,i) * tvvov(b, e, i, d)
end do 
end do 

term(34) = term(34) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(35) = term(35) + t2(a,b,l,m) * tovoo(m, a, l, i)
term(36) = term(36) + t2(a,b,l,m) * tovoo(l, a, m, i)
term(37) = term(37) + t2(a,b,l,i) * tovoo(l, a, m, m)
term(38) = term(38) + t2(a,b,i,m) * tovoo(m, a, l, l)
term(39) = term(39) + t2(a,b,l,i) * tovoo(m, a, l, m)
term(40) = term(40) + t2(a,b,i,m) * tovoo(l, a, m, l)
end do 
end do 

term(37) = term(37) * (-2.0000000000000004d+0) 
term(38) = term(38) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(41) = term(41) + t3(nocc, nactive, b,d,e,i,i,l) * tovov(l, d, i, e)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(42) = term(42) + t3(nocc, nactive, b,d,e,l,i,i) * tovov(l, e, i, d)
term(43) = term(43) + t3(nocc, nactive, b,d,e,i,i,l) * tovov(l, e, i, d)
end do 
end do 
end do 

term(43) = term(43) * (-2.000000000000003d+0) 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(44) = term(44) + t3(nocc, nactive, a,b,d,l,i,m) * tovov(m, a, l, d)
term(45) = term(45) + t3(nocc, nactive, a,b,d,l,i,m) * tovov(m, d, l, a)
term(46) = term(46) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, d, l, a)
end do 
end do 
end do 

term(45) = term(45) * (-2.000000000000003d+0) 
term(46) = term(46) * (-2.000000000000003d+0) 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(47) = term(47) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, d, l, a)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(48) = term(48) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, a, l, d)
end do 
end do 
end do 


do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(49) = term(49) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, a, l, d)
end do 
end do 
end do 



    eom_cc3_21_trans_aibiai = 0 !!triple_w1 + !triple_w2
    do s = 0, 49
    eom_cc3_21_trans_aibiai = eom_cc3_21_trans_aibiai + term(s)
    end do

    end function eom_cc3_21_trans_aibiai
    
    function eom_cc3_21_trans_aibibi(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_cc3_21_trans_aibibi  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b 
    integer :: s ,d,l,e,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:49) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(a, i, i, i)
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(a, b, b, i)
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(b, a, b, i)
    
    term(0) = term(0) + tvvvo(b, b, a, i)
term(1) = term(1) + tvvvo(a, b, b, i)
term(2) = term(2) + tvooo(a, i, i, i)

term(2) = term(2) * (-0.9999999999999999d+0) 

do d = nocc + 1, nactive 
term(3) = term(3) + tov(i, d) * t2(a,d,i,i)
term(4) = term(4) + t2(b,d,i,i) * tvvov(a, b, i, d)
term(5) = term(5) + t2(a,d,i,i) * tvvov(b, b, i, d)
term(6) = term(6) + t2(b,d,i,i) * tvvov(a, d, i, b)
term(7) = term(7) + t2(a,d,i,i) * tvvov(b, d, i, b)
end do 

term(3) = term(3) * (-0.9999999999999999d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (2.0000000000000004d+0) 

do l = 1, nocc 
term(8) = term(8) + tov(l, b) * t2(a,b,l,i)
term(9) = term(9) + tov(l, b) * t2(a,b,i,l)
term(10) = term(10) + t2(a,b,l,i) * tovoo(l, b, i, i)
term(11) = term(11) + t2(a,b,i,l) * tovoo(l, b, i, i)
term(12) = term(12) + t2(a,b,l,i) * tovoo(i, b, l, i)
term(13) = term(13) + t2(a,b,i,l) * tovoo(i, b, l, i)
end do 

term(8) = term(8) * (-0.9999999999999999d+0) 
term(9) = term(9) * (-0.9999999999999999d+0) 
term(12) = term(12) * (-2.0000000000000004d+0) 
term(13) = term(13) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(14) = term(14) + t2(a,d,l,i) * tovoo(i, d, l, i)
term(15) = term(15) + t2(a,d,l,i) * tovoo(l, d, i, i)
term(16) = term(16) + t2(a,d,l,i) * tvvov(b, d, l, b)
term(17) = term(17) + t2(b,d,l,i) * tvvov(a, d, l, b)
term(18) = term(18) + t2(b,d,l,i) * tvvov(a, b, l, d)
term(19) = term(19) + t2(b,d,i,l) * tvvov(a, b, l, d)
term(20) = term(20) + t2(a,d,l,i) * tvvov(b, b, l, d)
term(21) = term(21) + t2(a,d,i,l) * tvvov(b, b, l, d)
term(22) = term(22) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, b, i, d)
end do 
end do 

term(16) = term(16) * (-1.0000000000000002d+0) 
term(17) = term(17) * (-1.0000000000000002d+0) 
term(18) = term(18) * (-1.0000000000000002d+0) 
term(19) = term(19) * (2.0000000000000004d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(21) = term(21) * (2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(23) = term(23) + t2(a,d,i,l) * tovoo(i, d, l, i)
term(24) = term(24) + t2(a,d,i,i) * tovoo(i, d, l, l)
term(25) = term(25) + t2(a,d,i,l) * tovoo(l, d, i, i)
term(26) = term(26) + t2(a,d,i,i) * tovoo(l, d, i, l)
term(27) = term(27) + t2(b,d,i,l) * tvvov(a, d, l, b)
term(28) = term(28) + t2(a,d,i,l) * tvvov(b, d, l, b)
term(29) = term(29) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, b, i, d)
term(30) = term(30) + t3(nocc, nactive, a,b,d,i,i,l) * tovov(l, b, i, d)
term(31) = term(31) + t3(nocc, nactive, a,b,d,l,i,i) * tovov(l, d, i, b)
term(32) = term(32) + t3(nocc, nactive, a,b,d,i,l,i) * tovov(l, d, i, b)
term(33) = term(33) + t3(nocc, nactive, a,b,d,i,i,l) * tovov(l, d, i, b)
end do 
end do 

term(24) = term(24) * (-2.0000000000000004d+0) 
term(25) = term(25) * (-2.0000000000000004d+0) 
term(27) = term(27) * (-1.0000000000000002d+0) 
term(28) = term(28) * (-1.0000000000000002d+0) 
term(30) = term(30) * (-2.000000000000003d+0) 
term(31) = term(31) * (-2.000000000000003d+0) 
term(32) = term(32) * (-2.000000000000003d+0) 
term(33) = term(33) * (4.000000000000006d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(34) = term(34) + t2(d,e,i,i) * tvvov(a, d, i, e)
end do 
end do 

term(34) = term(34) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(35) = term(35) + t2(a,b,l,m) * tovoo(m, b, l, i)
term(36) = term(36) + t2(a,b,l,m) * tovoo(l, b, m, i)
term(37) = term(37) + t2(a,b,l,i) * tovoo(l, b, m, m)
term(38) = term(38) + t2(a,b,i,l) * tovoo(l, b, m, m)
term(39) = term(39) + t2(a,b,l,i) * tovoo(m, b, l, m)
term(40) = term(40) + t2(a,b,i,l) * tovoo(m, b, l, m)
end do 
end do 

term(37) = term(37) * (-2.0000000000000004d+0) 
term(38) = term(38) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(41) = term(41) + t3(nocc, nactive, a,d,e,i,i,l) * tovov(l, d, i, e)
term(42) = term(42) + t3(nocc, nactive, a,d,e,l,i,i) * tovov(l, d, i, e)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(43) = term(43) + t3(nocc, nactive, a,d,e,i,i,l) * tovov(l, e, i, d)
end do 
end do 
end do 

term(43) = term(43) * (-2.000000000000003d+0) 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(44) = term(44) + t3(nocc, nactive, a,b,d,l,i,m) * tovov(m, b, l, d)
term(45) = term(45) + t3(nocc, nactive, a,b,d,l,i,m) * tovov(m, d, l, b)
term(46) = term(46) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, d, l, b)
end do 
end do 
end do 

term(45) = term(45) * (-2.000000000000003d+0) 
term(46) = term(46) * (-2.000000000000003d+0) 

do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(47) = term(47) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, d, l, b)
end do 
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(48) = term(48) + t3(nocc, nactive, a,b,d,l,m,i) * tovov(m, b, l, d)
end do 
end do 
end do 


do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(49) = term(49) + t3(nocc, nactive, a,b,d,i,l,m) * tovov(m, b, l, d)
end do 
end do 
end do 



    eom_cc3_21_trans_aibibi = 0 !!triple_w1 + !triple_w2
    do s = 0, 49
    eom_cc3_21_trans_aibibi = eom_cc3_21_trans_aibibi + term(s)
    end do

    end function eom_cc3_21_trans_aibibi
    
    function eom_cc3_21_trans_aiaiai(t2, nocc, nactive, a, i) 
    real(F64) :: eom_cc3_21_trans_aiaiai  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i 
    integer :: s ,d,l,e,m 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    !triple_w1 = !triple_w1 + 2.d+0 * w1(a, i, i, i)
    
    !triple_w2 = !triple_w2 + 2.d+0 * w2(a, a, a, i)
    
    term(0) = term(0) + tvvvo(a, a, a, i)
term(1) = term(1) + tvooo(a, i, i, i)

term(1) = term(1) * (-0.9999999999999998d+0) 

do d = nocc + 1, nactive 
term(2) = term(2) + tov(i, d) * t2(a,d,i,i)
term(3) = term(3) + t2(a,d,i,i) * tvvov(a, a, i, d)
term(4) = term(4) + t2(a,d,i,i) * tvvov(a, d, i, a)
end do 

term(2) = term(2) * (-0.9999999999999998d+0) 
term(3) = term(3) * (-1.0000000000000004d+0) 
term(4) = term(4) * (2.000000000000001d+0) 

do l = 1, nocc 
term(5) = term(5) + tov(l, a) * t2(a,a,i,l)
term(6) = term(6) + t2(a,a,i,l) * tovoo(l, a, i, i)
term(7) = term(7) + t2(a,a,i,l) * tovoo(i, a, l, i)
end do 

term(5) = term(5) * (-0.9999999999999998d+0) 
term(7) = term(7) * (-2.000000000000001d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(8) = term(8) + t2(a,d,i,l) * tovoo(i, d, l, i)
term(9) = term(9) + t2(a,d,i,i) * tovoo(i, d, l, l)
term(10) = term(10) + t2(a,d,i,l) * tvvov(a, d, l, a)
term(11) = term(11) + t2(a,d,i,l) * tovoo(l, d, i, i)
term(12) = term(12) + t2(a,d,i,i) * tovoo(l, d, i, l)
term(13) = term(13) + t3(nocc, nactive, a,a,d,i,i,l) * tovov(l, a, i, d)
term(14) = term(14) + t3(nocc, nactive, a,a,d,i,l,i) * tovov(l, d, i, a)
term(15) = term(15) + t3(nocc, nactive, a,a,d,i,i,l) * tovov(l, d, i, a)
end do 
end do 

term(9) = term(9) * (-2.000000000000001d+0) 
term(10) = term(10) * (-1.0000000000000004d+0) 
term(11) = term(11) * (-2.000000000000001d+0) 
term(13) = term(13) * (-1.0000000000000016d+0) 
term(14) = term(14) * (-2.0000000000000027d+0) 
term(15) = term(15) * (2.000000000000003d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(16) = term(16) + t2(a,d,l,i) * tovoo(i, d, l, i)
term(17) = term(17) + t2(a,d,l,i) * tvvov(a, d, l, a)
term(18) = term(18) + t2(a,d,l,i) * tvvov(a, a, l, d)
term(19) = term(19) + t2(a,d,i,l) * tvvov(a, a, l, d)
term(20) = term(20) + t2(a,d,l,i) * tovoo(l, d, i, i)
term(21) = term(21) + t3(nocc, nactive, a,a,d,i,l,i) * tovov(l, a, i, d)
end do 
end do 

term(17) = term(17) * (-1.0000000000000004d+0) 
term(18) = term(18) * (-1.0000000000000004d+0) 
term(19) = term(19) * (2.000000000000001d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(22) = term(22) + t2(d,e,i,i) * tvvov(a, e, i, d)
end do 
end do 

term(22) = term(22) * (-0.5000000000000001d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(23) = term(23) + t2(d,e,i,i) * tvvov(a, d, i, e)
end do 
end do 

term(23) = term(23) * (-0.5000000000000001d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(24) = term(24) + t2(a,a,l,m) * tovoo(m, a, l, i)
term(25) = term(25) + t2(a,a,l,m) * tovoo(l, a, m, i)
term(26) = term(26) + t2(a,a,i,l) * tovoo(l, a, m, m)
term(27) = term(27) + t2(a,a,i,l) * tovoo(m, a, l, m)
end do 
end do 

term(24) = term(24) * (0.5000000000000001d+0) 
term(25) = term(25) * (0.5000000000000001d+0) 
term(26) = term(26) * (-2.000000000000001d+0) 

do l = 1, nocc 
do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(28) = term(28) + t3(nocc, nactive, a,d,e,i,i,l) * tovov(l, d, i, e)
term(29) = term(29) + t3(nocc, nactive, a,d,e,l,i,i) * tovov(l, d, i, e)
end do 
end do 
end do 

term(29) = term(29) * (0.5000000000000008d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(30) = term(30) + t3(nocc, nactive, a,d,e,l,i,i) * tovov(l, e, i, d)
term(31) = term(31) + t3(nocc, nactive, a,d,e,i,i,l) * tovov(l, e, i, d)
end do 
end do 
end do 

term(30) = term(30) * (0.5000000000000008d+0) 
term(31) = term(31) * (-2.0000000000000027d+0) 

do m = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
term(32) = term(32) + t3(nocc, nactive, a,a,d,i,l,m) * tovov(m, a, l, d)
end do 
end do 
end do 


do l = 1, nocc 
do m = 1, nocc 
do d = nocc + 1, nactive 
term(33) = term(33) + t3(nocc, nactive, a,a,d,l,m,i) * tovov(m, d, l, a)
end do 
end do 
end do 

term(33) = term(33) * (0.5000000000000008d+0) 

do m = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
term(34) = term(34) + t3(nocc, nactive, a,a,d,i,l,m) * tovov(m, d, l, a)
end do 
end do 
end do 

term(34) = term(34) * (-2.0000000000000027d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do m = 1, nocc 
term(35) = term(35) + t3(nocc, nactive, a,a,d,l,m,i) * tovov(m, a, l, d)
end do 
end do 
end do 

term(35) = term(35) * (0.5000000000000008d+0) 


    eom_cc3_21_trans_aiaiai = 0 !!triple_w1 + !triple_w2
    do s = 0, 35
    eom_cc3_21_trans_aiaiai = eom_cc3_21_trans_aiaiai + term(s)
    end do

    end function eom_cc3_21_trans_aiaiai
    
    function eom_cc3_21_trans_aibjck(t2, nocc, nactive, a, i, b, j, c, k) 
    real(F64) :: eom_cc3_21_trans_aibjck  
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) ::  a, i, b, j, c, k 
    integer :: s ,d,l 
    real(F64) :: triple_w1
    real(F64) :: triple_w2
    real(F64), dimension(0:13) :: term 
    term = 0.d+0 
    !triple_w1 = 0.d+0
    !triple_w2 = 0.d+0
    
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(b,d,j,i) * tvvov(a, c, k, d)
term(1) = term(1) + t2(a,d,i,j) * tvvov(b, c, k, d)
term(2) = term(2) + t2(b,d,j,i) * tvvov(a, d, k, c)
term(3) = term(3) + t2(a,d,i,j) * tvvov(b, d, k, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (2.0000000000000004d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 

do l = 1, nocc 
term(4) = term(4) + t2(a,b,l,j) * tovoo(l, c, k, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(l, c, k, j)
term(6) = term(6) + t2(a,b,l,j) * tovoo(k, c, l, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(k, c, l, j)
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 
term(7) = term(7) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(8) = term(8) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, c, k, d)
term(9) = term(9) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, c, k, d)
term(10) = term(10) + t3(nocc, nactive, a,b,d,l,j,i) * tovov(l, d, k, c)
term(11) = term(11) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, d, k, c)
term(12) = term(12) + t3(nocc, nactive, a,b,d,i,j,l) * tovov(l, d, k, c)
end do 
end do 

term(9) = term(9) * (-2.000000000000003d+0) 
term(10) = term(10) * (-2.000000000000003d+0) 
term(11) = term(11) * (-2.000000000000003d+0) 
term(12) = term(12) * (4.000000000000006d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(13) = term(13) + t3(nocc, nactive, a,b,d,i,l,j) * tovov(l, c, k, d)
end do 
end do 



    eom_cc3_21_trans_aibjck = 0 !!triple_w1 + !triple_w2
    do s = 0, 13
    eom_cc3_21_trans_aibjck = eom_cc3_21_trans_aibjck + term(s)
    end do

    end function eom_cc3_21_trans_aibjck
    
    end module eom_cc3_21_trans
    
