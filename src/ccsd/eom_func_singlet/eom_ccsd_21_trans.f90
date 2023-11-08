module eom_ccsd_21_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_ccsd_21_trans_aiajck(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_ccsd_21_trans_aiajck 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,d,l 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,i,j) * tvvov(a, c, k, d)
term(1) = term(1) + t2(a,d,j,i) * tvvov(a, c, k, d)
term(2) = term(2) + t2(a,d,i,j) * tvvov(a, d, k, c)
term(3) = term(3) + t2(a,d,j,i) * tvvov(a, d, k, c)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = term(2) * 2.0000000000000004d+0 
term(3) = term(3) * 2.0000000000000004d+0 

do l = 1, nocc 
term(4) = term(4) + t2(a,a,j,l) * tovoo(l, c, k, i)
term(5) = term(5) + t2(a,a,i,l) * tovoo(l, c, k, j)
term(6) = term(6) + t2(a,a,i,l) * tovoo(k, c, l, j)
term(7) = term(7) + t2(a,a,j,l) * tovoo(k, c, l, i)
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 
term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_21_trans_aiajck = 0.d+0
    do s = 0, 7
    eom_ccsd_21_trans_aiajck = eom_ccsd_21_trans_aiajck + term(s)
    end do

    end function eom_ccsd_21_trans_aiajck
    function eom_ccsd_21_trans_aibjak(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_ccsd_21_trans_aibjak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,d,l,e 
    double precision, dimension(0:16) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, k, i)

term(0) = -term(0) 

do d = nocc + 1, nactive 
term(1) = term(1) + t2(a,d,i,j) * tvvov(b, a, k, d)
term(2) = term(2) + t2(b,d,j,i) * tvvov(a, a, k, d)
term(3) = term(3) + t2(a,d,i,j) * tvvov(b, d, k, a)
term(4) = term(4) + t2(b,d,j,i) * tvvov(a, d, k, a)
term(5) = term(5) + tov(k, d) * t2(b,d,j,i)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0000000000000004d+0 
term(4) = term(4) * 2.0000000000000004d+0 
term(5) = -term(5) 

do l = 1, nocc 
term(6) = term(6) + t2(a,b,l,j) * tovoo(l, a, k, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(l, a, k, j)
term(8) = term(8) + t2(a,b,i,l) * tovoo(k, a, l, j)
term(9) = term(9) + t2(a,b,l,j) * tovoo(k, a, l, i)
end do 

term(8) = term(8) * (-2.0000000000000004d+0) 
term(9) = term(9) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(10) = term(10) + t2(b,d,j,l) * tovoo(k, d, l, i)
term(11) = term(11) + t2(b,d,j,i) * tovoo(k, d, l, l)
term(12) = term(12) + t2(b,d,j,l) * tovoo(l, d, k, i)
term(13) = term(13) + t2(b,d,j,i) * tovoo(l, d, k, l)
end do 
end do 

term(11) = term(11) * (-2.0000000000000004d+0) 
term(12) = term(12) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(14) = term(14) + t2(b,d,l,i) * tovoo(k, d, l, j)
term(15) = term(15) + t2(b,d,l,j) * tovoo(l, d, k, i)
end do 
end do 


do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(16) = term(16) + t2(d,e,j,i) * tvvov(b, d, k, e)
end do 
end do 

term(16) = -term(16) 


    eom_ccsd_21_trans_aibjak = 0.d+0
    do s = 0, 16
    eom_ccsd_21_trans_aibjak = eom_ccsd_21_trans_aibjak + term(s)
    end do

    end function eom_ccsd_21_trans_aibjak
    function eom_ccsd_21_trans_aibjbk(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_ccsd_21_trans_aibjbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,d,l,e 
    double precision, dimension(0:16) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, j)

term(0) = -term(0) 

do d = nocc + 1, nactive 
term(1) = term(1) + t2(a,d,i,j) * tvvov(b, b, k, d)
term(2) = term(2) + t2(b,d,j,i) * tvvov(a, b, k, d)
term(3) = term(3) + t2(a,d,i,j) * tvvov(b, d, k, b)
term(4) = term(4) + t2(b,d,j,i) * tvvov(a, d, k, b)
term(5) = term(5) + tov(k, d) * t2(a,d,i,j)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0000000000000004d+0 
term(4) = term(4) * 2.0000000000000004d+0 
term(5) = -term(5) 

do l = 1, nocc 
term(6) = term(6) + t2(a,b,l,j) * tovoo(l, b, k, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(l, b, k, j)
term(8) = term(8) + t2(a,b,i,l) * tovoo(k, b, l, j)
term(9) = term(9) + t2(a,b,l,j) * tovoo(k, b, l, i)
end do 

term(8) = term(8) * (-2.0000000000000004d+0) 
term(9) = term(9) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(10) = term(10) + t2(a,d,l,j) * tovoo(k, d, l, i)
term(11) = term(11) + t2(a,d,l,i) * tovoo(l, d, k, j)
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
term(12) = term(12) + t2(a,d,i,l) * tovoo(k, d, l, j)
term(13) = term(13) + t2(a,d,i,j) * tovoo(k, d, l, l)
term(14) = term(14) + t2(a,d,i,l) * tovoo(l, d, k, j)
term(15) = term(15) + t2(a,d,i,j) * tovoo(l, d, k, l)
end do 
end do 

term(13) = term(13) * (-2.0000000000000004d+0) 
term(14) = term(14) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(16) = term(16) + t2(d,e,j,i) * tvvov(a, e, k, d)
end do 
end do 

term(16) = -term(16) 


    eom_ccsd_21_trans_aibjbk = 0.d+0
    do s = 0, 16
    eom_ccsd_21_trans_aibjbk = eom_ccsd_21_trans_aibjbk + term(s)
    end do

    end function eom_ccsd_21_trans_aibjbk
    function eom_ccsd_21_trans_aibick(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_ccsd_21_trans_aibick 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,d,l 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,i,i) * tvvov(b, c, k, d)
term(1) = term(1) + t2(b,d,i,i) * tvvov(a, c, k, d)
term(2) = term(2) + t2(a,d,i,i) * tvvov(b, d, k, c)
term(3) = term(3) + t2(b,d,i,i) * tvvov(a, d, k, c)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = term(2) * 2.0000000000000004d+0 
term(3) = term(3) * 2.0000000000000004d+0 

do l = 1, nocc 
term(4) = term(4) + t2(a,b,l,i) * tovoo(l, c, k, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(l, c, k, i)
term(6) = term(6) + t2(a,b,i,l) * tovoo(k, c, l, i)
term(7) = term(7) + t2(a,b,l,i) * tovoo(k, c, l, i)
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 
term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_21_trans_aibick = 0.d+0
    do s = 0, 7
    eom_ccsd_21_trans_aibick = eom_ccsd_21_trans_aibick + term(s)
    end do

    end function eom_ccsd_21_trans_aibick
    function eom_ccsd_21_trans_aibjci(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_ccsd_21_trans_aibjci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,d,l,m 
    double precision, dimension(0:16) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, j)


do d = nocc + 1, nactive 
term(1) = term(1) + t2(a,d,i,j) * tvvov(b, c, i, d)
term(2) = term(2) + t2(b,d,j,i) * tvvov(a, c, i, d)
term(3) = term(3) + t2(a,d,i,j) * tvvov(b, d, i, c)
term(4) = term(4) + t2(b,d,j,i) * tvvov(a, d, i, c)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0000000000000004d+0 
term(4) = term(4) * 2.0000000000000004d+0 

do l = 1, nocc 
term(5) = term(5) + t2(a,b,l,j) * tovoo(l, c, i, i)
term(6) = term(6) + t2(a,b,i,l) * tovoo(l, c, i, j)
term(7) = term(7) + t2(a,b,i,l) * tovoo(i, c, l, j)
term(8) = term(8) + t2(a,b,l,j) * tovoo(i, c, l, i)
term(9) = term(9) + tov(l, c) * t2(a,b,l,j)
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 
term(8) = term(8) * (-2.0000000000000004d+0) 
term(9) = -term(9) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(10) = term(10) + t2(a,d,l,j) * tvvov(b, d, l, c)
term(11) = term(11) + t2(b,d,j,l) * tvvov(a, c, l, d)
term(12) = term(12) + t2(b,d,l,j) * tvvov(a, c, l, d)
end do 
end do 

term(10) = -term(10) 
term(11) = term(11) * 2.0000000000000004d+0 
term(12) = -term(12) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(13) = term(13) + t2(b,d,j,l) * tvvov(a, d, l, c)
end do 
end do 

term(13) = -term(13) 

do l = 1, nocc 
do m = 1, nocc 
term(14) = term(14) + t2(a,b,m,l) * tovoo(m, c, l, j)
term(15) = term(15) + t2(a,b,m,j) * tovoo(m, c, l, l)
term(16) = term(16) + t2(a,b,m,j) * tovoo(l, c, m, l)
end do 
end do 

term(15) = term(15) * (-2.0000000000000004d+0) 


    eom_ccsd_21_trans_aibjci = 0.d+0
    do s = 0, 16
    eom_ccsd_21_trans_aibjci = eom_ccsd_21_trans_aibjci + term(s)
    end do

    end function eom_ccsd_21_trans_aibjci
    function eom_ccsd_21_trans_aibjcj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_ccsd_21_trans_aibjcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,d,l,m 
    double precision, dimension(0:16) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, i)


do d = nocc + 1, nactive 
term(1) = term(1) + t2(a,d,i,j) * tvvov(b, c, j, d)
term(2) = term(2) + t2(b,d,j,i) * tvvov(a, c, j, d)
term(3) = term(3) + t2(a,d,i,j) * tvvov(b, d, j, c)
term(4) = term(4) + t2(b,d,j,i) * tvvov(a, d, j, c)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0000000000000004d+0 
term(4) = term(4) * 2.0000000000000004d+0 

do l = 1, nocc 
term(5) = term(5) + t2(a,b,l,j) * tovoo(l, c, j, i)
term(6) = term(6) + t2(a,b,i,l) * tovoo(l, c, j, j)
term(7) = term(7) + t2(a,b,i,l) * tovoo(j, c, l, j)
term(8) = term(8) + t2(a,b,l,j) * tovoo(j, c, l, i)
term(9) = term(9) + tov(l, c) * t2(a,b,i,l)
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 
term(8) = term(8) * (-2.0000000000000004d+0) 
term(9) = -term(9) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(10) = term(10) + t2(a,d,i,l) * tvvov(b, d, l, c)
end do 
end do 

term(10) = -term(10) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(11) = term(11) + t2(b,d,l,i) * tvvov(a, d, l, c)
term(12) = term(12) + t2(a,d,i,l) * tvvov(b, c, l, d)
term(13) = term(13) + t2(a,d,l,i) * tvvov(b, c, l, d)
end do 
end do 

term(11) = -term(11) 
term(12) = term(12) * 2.0000000000000004d+0 
term(13) = -term(13) 

do l = 1, nocc 
do m = 1, nocc 
term(14) = term(14) + t2(a,b,m,l) * tovoo(l, c, m, i)
end do 
end do 


do m = 1, nocc 
do l = 1, nocc 
term(15) = term(15) + t2(a,b,i,m) * tovoo(m, c, l, l)
term(16) = term(16) + t2(a,b,i,m) * tovoo(l, c, m, l)
end do 
end do 

term(15) = term(15) * (-2.0000000000000004d+0) 


    eom_ccsd_21_trans_aibjcj = 0.d+0
    do s = 0, 16
    eom_ccsd_21_trans_aibjcj = eom_ccsd_21_trans_aibjcj + term(s)
    end do

    end function eom_ccsd_21_trans_aibjcj
    function eom_ccsd_21_trans_aiajak(t2, nocc, nactive, a, i, j, k) 
    double precision :: eom_ccsd_21_trans_aiajak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,d,l,e 
    double precision, dimension(0:25) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, j)
term(1) = term(1) + tvooo(a, j, k, i)

term(0) = -term(0) 
term(1) = -term(1) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(2) = term(2) + t2(a,d,l,j) * tovoo(k, d, l, i)
term(3) = term(3) + t2(a,d,l,i) * tovoo(k, d, l, j)
term(4) = term(4) + t2(a,d,l,j) * tovoo(l, d, k, i)
term(5) = term(5) + t2(a,d,l,i) * tovoo(l, d, k, j)
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(a,d,j,l) * tovoo(k, d, l, i)
term(7) = term(7) + t2(a,d,i,l) * tovoo(k, d, l, j)
term(8) = term(8) + t2(a,d,i,j) * tovoo(k, d, l, l)
term(9) = term(9) + t2(a,d,j,i) * tovoo(k, d, l, l)
term(10) = term(10) + t2(a,d,j,l) * tovoo(l, d, k, i)
term(11) = term(11) + t2(a,d,i,l) * tovoo(l, d, k, j)
term(12) = term(12) + t2(a,d,i,j) * tovoo(l, d, k, l)
term(13) = term(13) + t2(a,d,j,i) * tovoo(l, d, k, l)
end do 
end do 

term(8) = term(8) * (-2.0000000000000004d+0) 
term(9) = term(9) * (-2.0000000000000004d+0) 
term(10) = term(10) * (-2.0000000000000004d+0) 
term(11) = term(11) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(d,e,j,i) * tvvov(a, e, k, d)
end do 
end do 

term(14) = -term(14) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(15) = term(15) + t2(d,e,j,i) * tvvov(a, d, k, e)
end do 
end do 

term(15) = -term(15) 

do d = nocc + 1, nactive 
term(16) = term(16) + t2(a,d,i,j) * tvvov(a, a, k, d)
term(17) = term(17) + t2(a,d,j,i) * tvvov(a, a, k, d)
term(18) = term(18) + t2(a,d,i,j) * tvvov(a, d, k, a)
term(19) = term(19) + t2(a,d,j,i) * tvvov(a, d, k, a)
term(20) = term(20) + tov(k, d) * t2(a,d,i,j)
term(21) = term(21) + tov(k, d) * t2(a,d,j,i)
end do 

term(16) = -term(16) 
term(17) = -term(17) 
term(18) = term(18) * 2.0000000000000004d+0 
term(19) = term(19) * 2.0000000000000004d+0 
term(20) = -term(20) 
term(21) = -term(21) 

do l = 1, nocc 
term(22) = term(22) + t2(a,a,j,l) * tovoo(l, a, k, i)
term(23) = term(23) + t2(a,a,i,l) * tovoo(l, a, k, j)
term(24) = term(24) + t2(a,a,i,l) * tovoo(k, a, l, j)
term(25) = term(25) + t2(a,a,j,l) * tovoo(k, a, l, i)
end do 

term(24) = term(24) * (-2.0000000000000004d+0) 
term(25) = term(25) * (-2.0000000000000004d+0) 


    eom_ccsd_21_trans_aiajak = 0.d+0
    do s = 0, 25
    eom_ccsd_21_trans_aiajak = eom_ccsd_21_trans_aiajak + term(s)
    end do

    end function eom_ccsd_21_trans_aiajak
    function eom_ccsd_21_trans_aiaick(t2, nocc, nactive, a, i, c, k) 
    double precision :: eom_ccsd_21_trans_aiaick 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,d,l 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,i,i) * tvvov(a, c, k, d)
term(1) = term(1) + t2(a,d,i,i) * tvvov(a, d, k, c)
end do 

term(0) = -term(0) 
term(1) = term(1) * 2.000000000000001d+0 

do l = 1, nocc 
term(2) = term(2) + t2(a,a,i,l) * tovoo(l, c, k, i)
term(3) = term(3) + t2(a,a,i,l) * tovoo(k, c, l, i)
end do 

term(3) = term(3) * (-2.000000000000001d+0) 


    eom_ccsd_21_trans_aiaick = 0.d+0
    do s = 0, 3
    eom_ccsd_21_trans_aiaick = eom_ccsd_21_trans_aiaick + term(s)
    end do

    end function eom_ccsd_21_trans_aiaick
    function eom_ccsd_21_trans_aiajci(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_ccsd_21_trans_aiajci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,l,d,m 
    double precision, dimension(0:16) :: term 
    term = 0.d+0 
    do l = 1, nocc 
term(0) = term(0) + tov(l, c) * t2(a,a,j,l)
term(1) = term(1) + t2(a,a,j,l) * tovoo(l, c, i, i)
term(2) = term(2) + t2(a,a,i,l) * tovoo(l, c, i, j)
term(3) = term(3) + t2(a,a,i,l) * tovoo(i, c, l, j)
term(4) = term(4) + t2(a,a,j,l) * tovoo(i, c, l, i)
end do 

term(0) = -term(0) 
term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
term(5) = term(5) + t2(a,d,i,j) * tvvov(a, c, i, d)
term(6) = term(6) + t2(a,d,j,i) * tvvov(a, c, i, d)
term(7) = term(7) + t2(a,d,i,j) * tvvov(a, d, i, c)
term(8) = term(8) + t2(a,d,j,i) * tvvov(a, d, i, c)
end do 

term(5) = -term(5) 
term(6) = -term(6) 
term(7) = term(7) * 2.0000000000000004d+0 
term(8) = term(8) * 2.0000000000000004d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(9) = term(9) + t2(a,d,l,j) * tvvov(a, d, l, c)
term(10) = term(10) + t2(a,d,j,l) * tvvov(a, c, l, d)
term(11) = term(11) + t2(a,d,l,j) * tvvov(a, c, l, d)
end do 
end do 

term(9) = -term(9) 
term(10) = term(10) * 2.0000000000000004d+0 
term(11) = -term(11) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(12) = term(12) + t2(a,d,j,l) * tvvov(a, d, l, c)
end do 
end do 

term(12) = -term(12) 

do m = 1, nocc 
do l = 1, nocc 
term(13) = term(13) + t2(a,a,l,m) * tovoo(l, c, m, j)
term(14) = term(14) + t2(a,a,j,m) * tovoo(m, c, l, l)
term(15) = term(15) + t2(a,a,j,m) * tovoo(l, c, m, l)
end do 
end do 

term(14) = term(14) * (-2.0000000000000004d+0) 

term(16) = term(16) + tvvvo(a, c, a, j)



    eom_ccsd_21_trans_aiajci = 0.d+0
    do s = 0, 16
    eom_ccsd_21_trans_aiajci = eom_ccsd_21_trans_aiajci + term(s)
    end do

    end function eom_ccsd_21_trans_aiajci
    function eom_ccsd_21_trans_aiajcj(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_ccsd_21_trans_aiajcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,l,d,m 
    double precision, dimension(0:16) :: term 
    term = 0.d+0 
    do l = 1, nocc 
term(0) = term(0) + tov(l, c) * t2(a,a,i,l)
term(1) = term(1) + t2(a,a,j,l) * tovoo(l, c, j, i)
term(2) = term(2) + t2(a,a,i,l) * tovoo(l, c, j, j)
term(3) = term(3) + t2(a,a,i,l) * tovoo(j, c, l, j)
term(4) = term(4) + t2(a,a,j,l) * tovoo(j, c, l, i)
end do 

term(0) = -term(0) 
term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
term(5) = term(5) + t2(a,d,i,j) * tvvov(a, c, j, d)
term(6) = term(6) + t2(a,d,j,i) * tvvov(a, c, j, d)
term(7) = term(7) + t2(a,d,i,j) * tvvov(a, d, j, c)
term(8) = term(8) + t2(a,d,j,i) * tvvov(a, d, j, c)
end do 

term(5) = -term(5) 
term(6) = -term(6) 
term(7) = term(7) * 2.0000000000000004d+0 
term(8) = term(8) * 2.0000000000000004d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(9) = term(9) + t2(a,d,i,l) * tvvov(a, d, l, c)
end do 
end do 

term(9) = -term(9) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(10) = term(10) + t2(a,d,l,i) * tvvov(a, d, l, c)
term(11) = term(11) + t2(a,d,i,l) * tvvov(a, c, l, d)
term(12) = term(12) + t2(a,d,l,i) * tvvov(a, c, l, d)
end do 
end do 

term(10) = -term(10) 
term(11) = term(11) * 2.0000000000000004d+0 
term(12) = -term(12) 

do m = 1, nocc 
do l = 1, nocc 
term(13) = term(13) + t2(a,a,l,m) * tovoo(m, c, l, i)
term(14) = term(14) + t2(a,a,i,m) * tovoo(m, c, l, l)
term(15) = term(15) + t2(a,a,i,m) * tovoo(l, c, m, l)
end do 
end do 

term(14) = term(14) * (-2.0000000000000004d+0) 

term(16) = term(16) + tvvvo(a, c, a, i)



    eom_ccsd_21_trans_aiajcj = 0.d+0
    do s = 0, 16
    eom_ccsd_21_trans_aiajcj = eom_ccsd_21_trans_aiajcj + term(s)
    end do

    end function eom_ccsd_21_trans_aiajcj
    function eom_ccsd_21_trans_aibiak(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_ccsd_21_trans_aibiak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,d,l,e 
    double precision, dimension(0:16) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, k, i)

term(0) = -term(0) 

do d = nocc + 1, nactive 
term(1) = term(1) + t2(a,d,i,i) * tvvov(b, a, k, d)
term(2) = term(2) + t2(b,d,i,i) * tvvov(a, a, k, d)
term(3) = term(3) + t2(a,d,i,i) * tvvov(b, d, k, a)
term(4) = term(4) + t2(b,d,i,i) * tvvov(a, d, k, a)
term(5) = term(5) + tov(k, d) * t2(b,d,i,i)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0000000000000004d+0 
term(4) = term(4) * 2.0000000000000004d+0 
term(5) = -term(5) 

do l = 1, nocc 
term(6) = term(6) + t2(a,b,l,i) * tovoo(l, a, k, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(l, a, k, i)
term(8) = term(8) + t2(a,b,i,l) * tovoo(k, a, l, i)
term(9) = term(9) + t2(a,b,l,i) * tovoo(k, a, l, i)
end do 

term(8) = term(8) * (-2.0000000000000004d+0) 
term(9) = term(9) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(10) = term(10) + t2(b,d,i,l) * tovoo(k, d, l, i)
term(11) = term(11) + t2(b,d,i,i) * tovoo(k, d, l, l)
term(12) = term(12) + t2(b,d,i,l) * tovoo(l, d, k, i)
term(13) = term(13) + t2(b,d,i,i) * tovoo(l, d, k, l)
end do 
end do 

term(11) = term(11) * (-2.0000000000000004d+0) 
term(12) = term(12) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(14) = term(14) + t2(b,d,l,i) * tovoo(k, d, l, i)
term(15) = term(15) + t2(b,d,l,i) * tovoo(l, d, k, i)
end do 
end do 


do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(16) = term(16) + t2(d,e,i,i) * tvvov(b, e, k, d)
end do 
end do 

term(16) = -term(16) 


    eom_ccsd_21_trans_aibiak = 0.d+0
    do s = 0, 16
    eom_ccsd_21_trans_aibiak = eom_ccsd_21_trans_aibiak + term(s)
    end do

    end function eom_ccsd_21_trans_aibiak
    function eom_ccsd_21_trans_aibjai(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_ccsd_21_trans_aibjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,l,d,e,m 
    double precision, dimension(0:25) :: term 
    term = 0.d+0 
    do l = 1, nocc 
term(0) = term(0) + tov(l, a) * t2(a,b,l,j)
term(1) = term(1) + t2(a,b,l,j) * tovoo(l, a, i, i)
term(2) = term(2) + t2(a,b,i,l) * tovoo(l, a, i, j)
term(3) = term(3) + t2(a,b,i,l) * tovoo(i, a, l, j)
term(4) = term(4) + t2(a,b,l,j) * tovoo(i, a, l, i)
end do 

term(0) = -term(0) 
term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(5) = term(5) + t2(b,d,j,l) * tovoo(i, d, l, i)
term(6) = term(6) + t2(b,d,j,i) * tovoo(i, d, l, l)
term(7) = term(7) + t2(b,d,j,l) * tovoo(l, d, i, i)
term(8) = term(8) + t2(b,d,j,i) * tovoo(l, d, i, l)
term(9) = term(9) + t2(b,d,j,l) * tvvov(a, d, l, a)
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 
term(7) = term(7) * (-2.0000000000000004d+0) 
term(9) = -term(9) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(10) = term(10) + t2(b,d,l,i) * tovoo(i, d, l, j)
term(11) = term(11) + t2(b,d,l,j) * tovoo(l, d, i, i)
term(12) = term(12) + t2(a,d,l,j) * tvvov(b, d, l, a)
term(13) = term(13) + t2(b,d,j,l) * tvvov(a, a, l, d)
term(14) = term(14) + t2(b,d,l,j) * tvvov(a, a, l, d)
end do 
end do 

term(12) = -term(12) 
term(13) = term(13) * 2.0000000000000004d+0 
term(14) = -term(14) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(15) = term(15) + t2(d,e,j,i) * tvvov(b, d, i, e)
end do 
end do 

term(15) = -term(15) 

term(16) = term(16) + tvvvo(a, a, b, j)
term(17) = term(17) + tvooo(b, j, i, i)

term(17) = -term(17) 

do d = nocc + 1, nactive 
term(18) = term(18) + t2(a,d,i,j) * tvvov(b, a, i, d)
term(19) = term(19) + t2(b,d,j,i) * tvvov(a, a, i, d)
term(20) = term(20) + t2(a,d,i,j) * tvvov(b, d, i, a)
term(21) = term(21) + t2(b,d,j,i) * tvvov(a, d, i, a)
term(22) = term(22) + tov(i, d) * t2(b,d,j,i)
end do 

term(18) = -term(18) 
term(19) = -term(19) 
term(20) = term(20) * 2.0000000000000004d+0 
term(21) = term(21) * 2.0000000000000004d+0 
term(22) = -term(22) 

do l = 1, nocc 
do m = 1, nocc 
term(23) = term(23) + t2(a,b,m,l) * tovoo(m, a, l, j)
term(24) = term(24) + t2(a,b,m,j) * tovoo(m, a, l, l)
term(25) = term(25) + t2(a,b,m,j) * tovoo(l, a, m, l)
end do 
end do 

term(24) = term(24) * (-2.0000000000000004d+0) 


    eom_ccsd_21_trans_aibjai = 0.d+0
    do s = 0, 25
    eom_ccsd_21_trans_aibjai = eom_ccsd_21_trans_aibjai + term(s)
    end do

    end function eom_ccsd_21_trans_aibjai
    function eom_ccsd_21_trans_aibjaj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_ccsd_21_trans_aibjaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,l,d,e,m 
    double precision, dimension(0:25) :: term 
    term = 0.d+0 
    do l = 1, nocc 
term(0) = term(0) + tov(l, a) * t2(a,b,i,l)
term(1) = term(1) + t2(a,b,l,j) * tovoo(l, a, j, i)
term(2) = term(2) + t2(a,b,i,l) * tovoo(l, a, j, j)
term(3) = term(3) + t2(a,b,i,l) * tovoo(j, a, l, j)
term(4) = term(4) + t2(a,b,l,j) * tovoo(j, a, l, i)
end do 

term(0) = -term(0) 
term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(5) = term(5) + t2(b,d,j,l) * tovoo(j, d, l, i)
term(6) = term(6) + t2(b,d,j,i) * tovoo(j, d, l, l)
term(7) = term(7) + t2(b,d,j,l) * tovoo(l, d, j, i)
term(8) = term(8) + t2(b,d,j,i) * tovoo(l, d, j, l)
term(9) = term(9) + t2(a,d,i,l) * tvvov(b, d, l, a)
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 
term(7) = term(7) * (-2.0000000000000004d+0) 
term(9) = -term(9) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(10) = term(10) + t2(b,d,l,i) * tovoo(j, d, l, j)
term(11) = term(11) + t2(b,d,l,j) * tovoo(l, d, j, i)
term(12) = term(12) + t2(b,d,l,i) * tvvov(a, d, l, a)
term(13) = term(13) + t2(a,d,i,l) * tvvov(b, a, l, d)
term(14) = term(14) + t2(a,d,l,i) * tvvov(b, a, l, d)
end do 
end do 

term(12) = -term(12) 
term(13) = term(13) * 2.0000000000000004d+0 
term(14) = -term(14) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(15) = term(15) + t2(d,e,j,i) * tvvov(b, d, j, e)
end do 
end do 

term(15) = -term(15) 

term(16) = term(16) + tvvvo(b, a, a, i)
term(17) = term(17) + tvooo(b, j, j, i)

term(17) = -term(17) 

do d = nocc + 1, nactive 
term(18) = term(18) + t2(a,d,i,j) * tvvov(b, a, j, d)
term(19) = term(19) + t2(b,d,j,i) * tvvov(a, a, j, d)
term(20) = term(20) + t2(a,d,i,j) * tvvov(b, d, j, a)
term(21) = term(21) + t2(b,d,j,i) * tvvov(a, d, j, a)
term(22) = term(22) + tov(j, d) * t2(b,d,j,i)
end do 

term(18) = -term(18) 
term(19) = -term(19) 
term(20) = term(20) * 2.0000000000000004d+0 
term(21) = term(21) * 2.0000000000000004d+0 
term(22) = -term(22) 

do l = 1, nocc 
do m = 1, nocc 
term(23) = term(23) + t2(a,b,m,l) * tovoo(l, a, m, i)
end do 
end do 


do m = 1, nocc 
do l = 1, nocc 
term(24) = term(24) + t2(a,b,i,l) * tovoo(l, a, m, m)
term(25) = term(25) + t2(a,b,i,l) * tovoo(m, a, l, m)
end do 
end do 

term(24) = term(24) * (-2.0000000000000004d+0) 


    eom_ccsd_21_trans_aibjaj = 0.d+0
    do s = 0, 25
    eom_ccsd_21_trans_aibjaj = eom_ccsd_21_trans_aibjaj + term(s)
    end do

    end function eom_ccsd_21_trans_aibjaj
    function eom_ccsd_21_trans_aibibk(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_ccsd_21_trans_aibibk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,d,l,e 
    double precision, dimension(0:16) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, i)

term(0) = -term(0) 

do d = nocc + 1, nactive 
term(1) = term(1) + t2(a,d,i,i) * tvvov(b, b, k, d)
term(2) = term(2) + t2(b,d,i,i) * tvvov(a, b, k, d)
term(3) = term(3) + t2(a,d,i,i) * tvvov(b, d, k, b)
term(4) = term(4) + t2(b,d,i,i) * tvvov(a, d, k, b)
term(5) = term(5) + tov(k, d) * t2(a,d,i,i)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0000000000000004d+0 
term(4) = term(4) * 2.0000000000000004d+0 
term(5) = -term(5) 

do l = 1, nocc 
term(6) = term(6) + t2(a,b,l,i) * tovoo(l, b, k, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(l, b, k, i)
term(8) = term(8) + t2(a,b,i,l) * tovoo(k, b, l, i)
term(9) = term(9) + t2(a,b,l,i) * tovoo(k, b, l, i)
end do 

term(8) = term(8) * (-2.0000000000000004d+0) 
term(9) = term(9) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(10) = term(10) + t2(a,d,l,i) * tovoo(k, d, l, i)
term(11) = term(11) + t2(a,d,l,i) * tovoo(l, d, k, i)
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
term(12) = term(12) + t2(a,d,i,l) * tovoo(k, d, l, i)
term(13) = term(13) + t2(a,d,i,i) * tovoo(k, d, l, l)
term(14) = term(14) + t2(a,d,i,l) * tovoo(l, d, k, i)
term(15) = term(15) + t2(a,d,i,i) * tovoo(l, d, k, l)
end do 
end do 

term(13) = term(13) * (-2.0000000000000004d+0) 
term(14) = term(14) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(16) = term(16) + t2(d,e,i,i) * tvvov(a, d, k, e)
end do 
end do 

term(16) = -term(16) 


    eom_ccsd_21_trans_aibibk = 0.d+0
    do s = 0, 16
    eom_ccsd_21_trans_aibibk = eom_ccsd_21_trans_aibibk + term(s)
    end do

    end function eom_ccsd_21_trans_aibibk
    function eom_ccsd_21_trans_aibjbi(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_ccsd_21_trans_aibjbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,l,d,e,m 
    double precision, dimension(0:25) :: term 
    term = 0.d+0 
    do l = 1, nocc 
term(0) = term(0) + tov(l, b) * t2(a,b,l,j)
term(1) = term(1) + t2(a,b,l,j) * tovoo(l, b, i, i)
term(2) = term(2) + t2(a,b,i,l) * tovoo(l, b, i, j)
term(3) = term(3) + t2(a,b,i,l) * tovoo(i, b, l, j)
term(4) = term(4) + t2(a,b,l,j) * tovoo(i, b, l, i)
end do 

term(0) = -term(0) 
term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(5) = term(5) + t2(a,d,l,j) * tovoo(i, d, l, i)
term(6) = term(6) + t2(a,d,l,i) * tovoo(l, d, i, j)
term(7) = term(7) + t2(a,d,l,j) * tvvov(b, d, l, b)
term(8) = term(8) + t2(b,d,j,l) * tvvov(a, b, l, d)
term(9) = term(9) + t2(b,d,l,j) * tvvov(a, b, l, d)
end do 
end do 

term(7) = -term(7) 
term(8) = term(8) * 2.0000000000000004d+0 
term(9) = -term(9) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(10) = term(10) + t2(a,d,i,l) * tovoo(i, d, l, j)
term(11) = term(11) + t2(a,d,i,j) * tovoo(i, d, l, l)
term(12) = term(12) + t2(a,d,i,l) * tovoo(l, d, i, j)
term(13) = term(13) + t2(a,d,i,j) * tovoo(l, d, i, l)
term(14) = term(14) + t2(b,d,j,l) * tvvov(a, d, l, b)
end do 
end do 

term(11) = term(11) * (-2.0000000000000004d+0) 
term(12) = term(12) * (-2.0000000000000004d+0) 
term(14) = -term(14) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(15) = term(15) + t2(d,e,j,i) * tvvov(a, e, i, d)
end do 
end do 

term(15) = -term(15) 

term(16) = term(16) + tvvvo(a, b, b, j)
term(17) = term(17) + tvooo(a, i, i, j)

term(17) = -term(17) 

do d = nocc + 1, nactive 
term(18) = term(18) + t2(a,d,i,j) * tvvov(b, b, i, d)
term(19) = term(19) + t2(b,d,j,i) * tvvov(a, b, i, d)
term(20) = term(20) + t2(a,d,i,j) * tvvov(b, d, i, b)
term(21) = term(21) + t2(b,d,j,i) * tvvov(a, d, i, b)
term(22) = term(22) + tov(i, d) * t2(a,d,i,j)
end do 

term(18) = -term(18) 
term(19) = -term(19) 
term(20) = term(20) * 2.0000000000000004d+0 
term(21) = term(21) * 2.0000000000000004d+0 
term(22) = -term(22) 

do l = 1, nocc 
do m = 1, nocc 
term(23) = term(23) + t2(a,b,m,l) * tovoo(m, b, l, j)
term(24) = term(24) + t2(a,b,m,j) * tovoo(m, b, l, l)
term(25) = term(25) + t2(a,b,m,j) * tovoo(l, b, m, l)
end do 
end do 

term(24) = term(24) * (-2.0000000000000004d+0) 


    eom_ccsd_21_trans_aibjbi = 0.d+0
    do s = 0, 25
    eom_ccsd_21_trans_aibjbi = eom_ccsd_21_trans_aibjbi + term(s)
    end do

    end function eom_ccsd_21_trans_aibjbi
    function eom_ccsd_21_trans_aibjbj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_ccsd_21_trans_aibjbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,l,d,e,m 
    double precision, dimension(0:25) :: term 
    term = 0.d+0 
    do l = 1, nocc 
term(0) = term(0) + tov(l, b) * t2(a,b,i,l)
term(1) = term(1) + t2(a,b,l,j) * tovoo(l, b, j, i)
term(2) = term(2) + t2(a,b,i,l) * tovoo(l, b, j, j)
term(3) = term(3) + t2(a,b,i,l) * tovoo(j, b, l, j)
term(4) = term(4) + t2(a,b,l,j) * tovoo(j, b, l, i)
end do 

term(0) = -term(0) 
term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(5) = term(5) + t2(a,d,l,j) * tovoo(j, d, l, i)
term(6) = term(6) + t2(a,d,l,i) * tovoo(l, d, j, j)
term(7) = term(7) + t2(b,d,l,i) * tvvov(a, d, l, b)
term(8) = term(8) + t2(a,d,i,l) * tvvov(b, b, l, d)
term(9) = term(9) + t2(a,d,l,i) * tvvov(b, b, l, d)
end do 
end do 

term(7) = -term(7) 
term(8) = term(8) * 2.0000000000000004d+0 
term(9) = -term(9) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(10) = term(10) + t2(a,d,i,l) * tovoo(j, d, l, j)
term(11) = term(11) + t2(a,d,i,j) * tovoo(j, d, l, l)
term(12) = term(12) + t2(a,d,i,l) * tovoo(l, d, j, j)
term(13) = term(13) + t2(a,d,i,j) * tovoo(l, d, j, l)
term(14) = term(14) + t2(a,d,i,l) * tvvov(b, d, l, b)
end do 
end do 

term(11) = term(11) * (-2.0000000000000004d+0) 
term(12) = term(12) * (-2.0000000000000004d+0) 
term(14) = -term(14) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(15) = term(15) + t2(d,e,j,i) * tvvov(a, e, j, d)
end do 
end do 

term(15) = -term(15) 

term(16) = term(16) + tvvvo(b, b, a, i)
term(17) = term(17) + tvooo(a, i, j, j)

term(17) = -term(17) 

do d = nocc + 1, nactive 
term(18) = term(18) + t2(a,d,i,j) * tvvov(b, b, j, d)
term(19) = term(19) + t2(b,d,j,i) * tvvov(a, b, j, d)
term(20) = term(20) + t2(a,d,i,j) * tvvov(b, d, j, b)
term(21) = term(21) + t2(b,d,j,i) * tvvov(a, d, j, b)
term(22) = term(22) + tov(j, d) * t2(a,d,i,j)
end do 

term(18) = -term(18) 
term(19) = -term(19) 
term(20) = term(20) * 2.0000000000000004d+0 
term(21) = term(21) * 2.0000000000000004d+0 
term(22) = -term(22) 

do l = 1, nocc 
do m = 1, nocc 
term(23) = term(23) + t2(a,b,m,l) * tovoo(l, b, m, i)
end do 
end do 


do m = 1, nocc 
do l = 1, nocc 
term(24) = term(24) + t2(a,b,i,m) * tovoo(m, b, l, l)
term(25) = term(25) + t2(a,b,i,m) * tovoo(l, b, m, l)
end do 
end do 

term(24) = term(24) * (-2.0000000000000004d+0) 


    eom_ccsd_21_trans_aibjbj = 0.d+0
    do s = 0, 25
    eom_ccsd_21_trans_aibjbj = eom_ccsd_21_trans_aibjbj + term(s)
    end do

    end function eom_ccsd_21_trans_aibjbj
    function eom_ccsd_21_trans_aibici(t2, nocc, nactive, a, i, b, c) 
    double precision :: eom_ccsd_21_trans_aibici 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,d,l,m 
    double precision, dimension(0:25) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, i)
term(1) = term(1) + tvvvo(a, c, b, i)


do d = nocc + 1, nactive 
term(2) = term(2) + t2(a,d,i,i) * tvvov(b, c, i, d)
term(3) = term(3) + t2(b,d,i,i) * tvvov(a, c, i, d)
term(4) = term(4) + t2(a,d,i,i) * tvvov(b, d, i, c)
term(5) = term(5) + t2(b,d,i,i) * tvvov(a, d, i, c)
end do 

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = term(4) * 2.0000000000000004d+0 
term(5) = term(5) * 2.0000000000000004d+0 

do l = 1, nocc 
term(6) = term(6) + t2(a,b,l,i) * tovoo(l, c, i, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(l, c, i, i)
term(8) = term(8) + t2(a,b,i,l) * tovoo(i, c, l, i)
term(9) = term(9) + t2(a,b,l,i) * tovoo(i, c, l, i)
term(10) = term(10) + tov(l, c) * t2(a,b,i,l)
term(11) = term(11) + tov(l, c) * t2(a,b,l,i)
end do 

term(8) = term(8) * (-2.0000000000000004d+0) 
term(9) = term(9) * (-2.0000000000000004d+0) 
term(10) = -term(10) 
term(11) = -term(11) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(12) = term(12) + t2(a,d,l,i) * tvvov(b, d, l, c)
term(13) = term(13) + t2(b,d,l,i) * tvvov(a, d, l, c)
term(14) = term(14) + t2(b,d,i,l) * tvvov(a, c, l, d)
term(15) = term(15) + t2(a,d,i,l) * tvvov(b, c, l, d)
term(16) = term(16) + t2(b,d,l,i) * tvvov(a, c, l, d)
term(17) = term(17) + t2(a,d,l,i) * tvvov(b, c, l, d)
end do 
end do 

term(12) = -term(12) 
term(13) = -term(13) 
term(14) = term(14) * 2.0000000000000004d+0 
term(15) = term(15) * 2.0000000000000004d+0 
term(16) = -term(16) 
term(17) = -term(17) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(18) = term(18) + t2(b,d,i,l) * tvvov(a, d, l, c)
term(19) = term(19) + t2(a,d,i,l) * tvvov(b, d, l, c)
end do 
end do 

term(18) = -term(18) 
term(19) = -term(19) 

do l = 1, nocc 
do m = 1, nocc 
term(20) = term(20) + t2(a,b,m,l) * tovoo(l, c, m, i)
term(21) = term(21) + t2(a,b,m,l) * tovoo(m, c, l, i)
term(22) = term(22) + t2(a,b,m,i) * tovoo(m, c, l, l)
term(23) = term(23) + t2(a,b,m,i) * tovoo(l, c, m, l)
end do 
end do 

term(22) = term(22) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(24) = term(24) + t2(a,b,i,m) * tovoo(m, c, l, l)
term(25) = term(25) + t2(a,b,i,m) * tovoo(l, c, m, l)
end do 
end do 

term(24) = term(24) * (-2.0000000000000004d+0) 


    eom_ccsd_21_trans_aibici = 0.d+0
    do s = 0, 25
    eom_ccsd_21_trans_aibici = eom_ccsd_21_trans_aibici + term(s)
    end do

    end function eom_ccsd_21_trans_aibici
    function eom_ccsd_21_trans_aiaiak(t2, nocc, nactive, a, i, k) 
    double precision :: eom_ccsd_21_trans_aiaiak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k 
    integer :: s ,d,l,e 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, i)

term(0) = -term(0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(1) = term(1) + t2(a,d,l,i) * tovoo(k, d, l, i)
term(2) = term(2) + t2(a,d,l,i) * tovoo(l, d, k, i)
end do 
end do 


do l = 1, nocc 
do d = nocc + 1, nactive 
term(3) = term(3) + t2(a,d,i,l) * tovoo(k, d, l, i)
term(4) = term(4) + t2(a,d,i,i) * tovoo(k, d, l, l)
term(5) = term(5) + t2(a,d,i,l) * tovoo(l, d, k, i)
term(6) = term(6) + t2(a,d,i,i) * tovoo(l, d, k, l)
end do 
end do 

term(4) = term(4) * (-2.000000000000001d+0) 
term(5) = term(5) * (-2.000000000000001d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(7) = term(7) + t2(d,e,i,i) * tvvov(a, d, k, e)
end do 
end do 

term(7) = term(7) * (-0.5000000000000001d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(d,e,i,i) * tvvov(a, e, k, d)
end do 
end do 

term(8) = term(8) * (-0.5000000000000001d+0) 

do d = nocc + 1, nactive 
term(9) = term(9) + t2(a,d,i,i) * tvvov(a, a, k, d)
term(10) = term(10) + t2(a,d,i,i) * tvvov(a, d, k, a)
term(11) = term(11) + tov(k, d) * t2(a,d,i,i)
end do 

term(9) = -term(9) 
term(10) = term(10) * 2.000000000000001d+0 
term(11) = -term(11) 

do l = 1, nocc 
term(12) = term(12) + t2(a,a,i,l) * tovoo(l, a, k, i)
term(13) = term(13) + t2(a,a,i,l) * tovoo(k, a, l, i)
end do 

term(13) = term(13) * (-2.000000000000001d+0) 


    eom_ccsd_21_trans_aiaiak = 0.d+0
    do s = 0, 13
    eom_ccsd_21_trans_aiaiak = eom_ccsd_21_trans_aiaiak + term(s)
    end do

    end function eom_ccsd_21_trans_aiaiak
    function eom_ccsd_21_trans_aiajai(t2, nocc, nactive, a, i, j) 
    double precision :: eom_ccsd_21_trans_aiajai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,d,l,e,m 
    double precision, dimension(0:34) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
do l = 1, nocc 
term(0) = term(0) + t2(a,d,l,j) * tovoo(i, d, l, i)
term(1) = term(1) + t2(a,d,l,i) * tovoo(i, d, l, j)
term(2) = term(2) + t2(a,d,l,j) * tovoo(l, d, i, i)
term(3) = term(3) + t2(a,d,l,i) * tovoo(l, d, i, j)
term(4) = term(4) + t2(a,d,l,j) * tvvov(a, d, l, a)
term(5) = term(5) + t2(a,d,j,l) * tvvov(a, a, l, d)
term(6) = term(6) + t2(a,d,l,j) * tvvov(a, a, l, d)
end do 
end do 

term(4) = -term(4) 
term(5) = term(5) * 2.0000000000000004d+0 
term(6) = -term(6) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(7) = term(7) + t2(a,d,j,l) * tovoo(i, d, l, i)
term(8) = term(8) + t2(a,d,i,l) * tovoo(i, d, l, j)
term(9) = term(9) + t2(a,d,i,j) * tovoo(i, d, l, l)
term(10) = term(10) + t2(a,d,j,i) * tovoo(i, d, l, l)
term(11) = term(11) + t2(a,d,j,l) * tovoo(l, d, i, i)
term(12) = term(12) + t2(a,d,i,l) * tovoo(l, d, i, j)
term(13) = term(13) + t2(a,d,i,j) * tovoo(l, d, i, l)
term(14) = term(14) + t2(a,d,j,i) * tovoo(l, d, i, l)
term(15) = term(15) + t2(a,d,j,l) * tvvov(a, d, l, a)
end do 
end do 

term(9) = term(9) * (-2.0000000000000004d+0) 
term(10) = term(10) * (-2.0000000000000004d+0) 
term(11) = term(11) * (-2.0000000000000004d+0) 
term(12) = term(12) * (-2.0000000000000004d+0) 
term(15) = -term(15) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(16) = term(16) + t2(d,e,j,i) * tvvov(a, e, i, d)
end do 
end do 

term(16) = -term(16) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(17) = term(17) + t2(d,e,j,i) * tvvov(a, d, i, e)
end do 
end do 

term(17) = -term(17) 

do m = 1, nocc 
do l = 1, nocc 
term(18) = term(18) + t2(a,a,l,m) * tovoo(l, a, m, j)
term(19) = term(19) + t2(a,a,j,m) * tovoo(m, a, l, l)
term(20) = term(20) + t2(a,a,j,m) * tovoo(l, a, m, l)
end do 
end do 

term(19) = term(19) * (-2.0000000000000004d+0) 

do l = 1, nocc 
term(21) = term(21) + tov(l, a) * t2(a,a,j,l)
term(22) = term(22) + t2(a,a,j,l) * tovoo(l, a, i, i)
term(23) = term(23) + t2(a,a,i,l) * tovoo(l, a, i, j)
term(24) = term(24) + t2(a,a,i,l) * tovoo(i, a, l, j)
term(25) = term(25) + t2(a,a,j,l) * tovoo(i, a, l, i)
end do 

term(21) = -term(21) 
term(24) = term(24) * (-2.0000000000000004d+0) 
term(25) = term(25) * (-2.0000000000000004d+0) 

term(26) = term(26) + tvooo(a, i, i, j)
term(27) = term(27) + tvooo(a, j, i, i)
term(28) = term(28) + tvvvo(a, a, a, j)

term(26) = -term(26) 
term(27) = -term(27) 

do d = nocc + 1, nactive 
term(29) = term(29) + t2(a,d,i,j) * tvvov(a, a, i, d)
term(30) = term(30) + t2(a,d,j,i) * tvvov(a, a, i, d)
term(31) = term(31) + t2(a,d,i,j) * tvvov(a, d, i, a)
term(32) = term(32) + t2(a,d,j,i) * tvvov(a, d, i, a)
term(33) = term(33) + tov(i, d) * t2(a,d,i,j)
term(34) = term(34) + tov(i, d) * t2(a,d,j,i)
end do 

term(29) = -term(29) 
term(30) = -term(30) 
term(31) = term(31) * 2.0000000000000004d+0 
term(32) = term(32) * 2.0000000000000004d+0 
term(33) = -term(33) 
term(34) = -term(34) 


    eom_ccsd_21_trans_aiajai = 0.d+0
    do s = 0, 34
    eom_ccsd_21_trans_aiajai = eom_ccsd_21_trans_aiajai + term(s)
    end do

    end function eom_ccsd_21_trans_aiajai
    function eom_ccsd_21_trans_aiajaj(t2, nocc, nactive, a, i, j) 
    double precision :: eom_ccsd_21_trans_aiajaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,d,l,e,m 
    double precision, dimension(0:34) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
do l = 1, nocc 
term(0) = term(0) + t2(a,d,l,j) * tovoo(j, d, l, i)
term(1) = term(1) + t2(a,d,l,i) * tovoo(j, d, l, j)
term(2) = term(2) + t2(a,d,l,j) * tovoo(l, d, j, i)
term(3) = term(3) + t2(a,d,l,i) * tovoo(l, d, j, j)
term(4) = term(4) + t2(a,d,l,i) * tvvov(a, d, l, a)
term(5) = term(5) + t2(a,d,i,l) * tvvov(a, a, l, d)
term(6) = term(6) + t2(a,d,l,i) * tvvov(a, a, l, d)
end do 
end do 

term(4) = -term(4) 
term(5) = term(5) * 2.0000000000000004d+0 
term(6) = -term(6) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(7) = term(7) + t2(a,d,j,l) * tovoo(j, d, l, i)
term(8) = term(8) + t2(a,d,i,l) * tovoo(j, d, l, j)
term(9) = term(9) + t2(a,d,i,j) * tovoo(j, d, l, l)
term(10) = term(10) + t2(a,d,j,i) * tovoo(j, d, l, l)
term(11) = term(11) + t2(a,d,j,l) * tovoo(l, d, j, i)
term(12) = term(12) + t2(a,d,i,l) * tovoo(l, d, j, j)
term(13) = term(13) + t2(a,d,i,j) * tovoo(l, d, j, l)
term(14) = term(14) + t2(a,d,j,i) * tovoo(l, d, j, l)
term(15) = term(15) + t2(a,d,i,l) * tvvov(a, d, l, a)
end do 
end do 

term(9) = term(9) * (-2.0000000000000004d+0) 
term(10) = term(10) * (-2.0000000000000004d+0) 
term(11) = term(11) * (-2.0000000000000004d+0) 
term(12) = term(12) * (-2.0000000000000004d+0) 
term(15) = -term(15) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(16) = term(16) + t2(d,e,j,i) * tvvov(a, e, j, d)
end do 
end do 

term(16) = -term(16) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(17) = term(17) + t2(d,e,j,i) * tvvov(a, d, j, e)
end do 
end do 

term(17) = -term(17) 

do m = 1, nocc 
do l = 1, nocc 
term(18) = term(18) + t2(a,a,l,m) * tovoo(m, a, l, i)
term(19) = term(19) + t2(a,a,i,m) * tovoo(m, a, l, l)
term(20) = term(20) + t2(a,a,i,m) * tovoo(l, a, m, l)
end do 
end do 

term(19) = term(19) * (-2.0000000000000004d+0) 

do l = 1, nocc 
term(21) = term(21) + tov(l, a) * t2(a,a,i,l)
term(22) = term(22) + t2(a,a,j,l) * tovoo(l, a, j, i)
term(23) = term(23) + t2(a,a,i,l) * tovoo(l, a, j, j)
term(24) = term(24) + t2(a,a,i,l) * tovoo(j, a, l, j)
term(25) = term(25) + t2(a,a,j,l) * tovoo(j, a, l, i)
end do 

term(21) = -term(21) 
term(24) = term(24) * (-2.0000000000000004d+0) 
term(25) = term(25) * (-2.0000000000000004d+0) 

term(26) = term(26) + tvooo(a, i, j, j)
term(27) = term(27) + tvooo(a, j, j, i)
term(28) = term(28) + tvvvo(a, a, a, i)

term(26) = -term(26) 
term(27) = -term(27) 

do d = nocc + 1, nactive 
term(29) = term(29) + t2(a,d,i,j) * tvvov(a, a, j, d)
term(30) = term(30) + t2(a,d,j,i) * tvvov(a, a, j, d)
term(31) = term(31) + t2(a,d,i,j) * tvvov(a, d, j, a)
term(32) = term(32) + t2(a,d,j,i) * tvvov(a, d, j, a)
term(33) = term(33) + tov(j, d) * t2(a,d,i,j)
term(34) = term(34) + tov(j, d) * t2(a,d,j,i)
end do 

term(29) = -term(29) 
term(30) = -term(30) 
term(31) = term(31) * 2.0000000000000004d+0 
term(32) = term(32) * 2.0000000000000004d+0 
term(33) = -term(33) 
term(34) = -term(34) 


    eom_ccsd_21_trans_aiajaj = 0.d+0
    do s = 0, 34
    eom_ccsd_21_trans_aiajaj = eom_ccsd_21_trans_aiajaj + term(s)
    end do

    end function eom_ccsd_21_trans_aiajaj
    function eom_ccsd_21_trans_aiaici(t2, nocc, nactive, a, i, c) 
    double precision :: eom_ccsd_21_trans_aiaici 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,d,l,m 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,i,i) * tvvov(a, c, i, d)
term(1) = term(1) + t2(a,d,i,i) * tvvov(a, d, i, c)
end do 

term(0) = -term(0) 
term(1) = term(1) * 2.000000000000001d+0 

do l = 1, nocc 
term(2) = term(2) + t2(a,a,i,l) * tovoo(l, c, i, i)
term(3) = term(3) + t2(a,a,i,l) * tovoo(i, c, l, i)
term(4) = term(4) + tov(l, c) * t2(a,a,i,l)
end do 

term(3) = term(3) * (-2.000000000000001d+0) 
term(4) = -term(4) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(5) = term(5) + t2(a,d,l,i) * tvvov(a, d, l, c)
term(6) = term(6) + t2(a,d,i,l) * tvvov(a, c, l, d)
term(7) = term(7) + t2(a,d,l,i) * tvvov(a, c, l, d)
end do 
end do 

term(5) = -term(5) 
term(6) = term(6) * 2.000000000000001d+0 
term(7) = -term(7) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(8) = term(8) + t2(a,d,i,l) * tvvov(a, d, l, c)
end do 
end do 

term(8) = -term(8) 

do m = 1, nocc 
do l = 1, nocc 
term(9) = term(9) + t2(a,a,l,m) * tovoo(m, c, l, i)
term(10) = term(10) + t2(a,a,i,m) * tovoo(m, c, l, l)
term(11) = term(11) + t2(a,a,l,m) * tovoo(l, c, m, i)
term(12) = term(12) + t2(a,a,i,m) * tovoo(l, c, m, l)
end do 
end do 

term(9) = term(9) * 0.5000000000000001d+0 
term(10) = term(10) * (-2.000000000000001d+0) 
term(11) = term(11) * 0.5000000000000001d+0 

term(13) = term(13) + tvvvo(a, c, a, i)



    eom_ccsd_21_trans_aiaici = 0.d+0
    do s = 0, 13
    eom_ccsd_21_trans_aiaici = eom_ccsd_21_trans_aiaici + term(s)
    end do

    end function eom_ccsd_21_trans_aiaici
    function eom_ccsd_21_trans_aibiai(t2, nocc, nactive, a, i, b) 
    double precision :: eom_ccsd_21_trans_aibiai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,d,l,e,m 
    double precision, dimension(0:34) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,i,i) * tvvov(b, a, i, d)
term(1) = term(1) + t2(b,d,i,i) * tvvov(a, a, i, d)
term(2) = term(2) + t2(a,d,i,i) * tvvov(b, d, i, a)
term(3) = term(3) + t2(b,d,i,i) * tvvov(a, d, i, a)
term(4) = term(4) + tov(i, d) * t2(b,d,i,i)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = term(2) * 2.0000000000000004d+0 
term(3) = term(3) * 2.0000000000000004d+0 
term(4) = -term(4) 

do l = 1, nocc 
term(5) = term(5) + t2(a,b,l,i) * tovoo(l, a, i, i)
term(6) = term(6) + t2(a,b,i,l) * tovoo(l, a, i, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(i, a, l, i)
term(8) = term(8) + t2(a,b,l,i) * tovoo(i, a, l, i)
term(9) = term(9) + tov(l, a) * t2(a,b,i,l)
term(10) = term(10) + tov(l, a) * t2(a,b,l,i)
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 
term(8) = term(8) * (-2.0000000000000004d+0) 
term(9) = -term(9) 
term(10) = -term(10) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(11) = term(11) + t2(b,d,i,l) * tovoo(i, d, l, i)
term(12) = term(12) + t2(b,d,i,i) * tovoo(i, d, l, l)
term(13) = term(13) + t2(b,d,i,l) * tovoo(l, d, i, i)
term(14) = term(14) + t2(b,d,i,i) * tovoo(l, d, i, l)
term(15) = term(15) + t2(b,d,i,l) * tvvov(a, d, l, a)
term(16) = term(16) + t2(a,d,i,l) * tvvov(b, d, l, a)
end do 
end do 

term(12) = term(12) * (-2.0000000000000004d+0) 
term(13) = term(13) * (-2.0000000000000004d+0) 
term(15) = -term(15) 
term(16) = -term(16) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(17) = term(17) + t2(b,d,l,i) * tovoo(i, d, l, i)
term(18) = term(18) + t2(b,d,l,i) * tovoo(l, d, i, i)
term(19) = term(19) + t2(a,d,l,i) * tvvov(b, d, l, a)
term(20) = term(20) + t2(b,d,l,i) * tvvov(a, d, l, a)
term(21) = term(21) + t2(b,d,i,l) * tvvov(a, a, l, d)
term(22) = term(22) + t2(a,d,i,l) * tvvov(b, a, l, d)
term(23) = term(23) + t2(b,d,l,i) * tvvov(a, a, l, d)
term(24) = term(24) + t2(a,d,l,i) * tvvov(b, a, l, d)
end do 
end do 

term(19) = -term(19) 
term(20) = -term(20) 
term(21) = term(21) * 2.0000000000000004d+0 
term(22) = term(22) * 2.0000000000000004d+0 
term(23) = -term(23) 
term(24) = -term(24) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(25) = term(25) + t2(d,e,i,i) * tvvov(b, e, i, d)
end do 
end do 

term(25) = -term(25) 

term(26) = term(26) + tvvvo(b, a, a, i)
term(27) = term(27) + tvvvo(a, a, b, i)
term(28) = term(28) + tvooo(b, i, i, i)

term(28) = -term(28) 

do l = 1, nocc 
do m = 1, nocc 
term(29) = term(29) + t2(a,b,m,l) * tovoo(l, a, m, i)
term(30) = term(30) + t2(a,b,m,l) * tovoo(m, a, l, i)
term(31) = term(31) + t2(a,b,m,i) * tovoo(m, a, l, l)
term(32) = term(32) + t2(a,b,m,i) * tovoo(l, a, m, l)
end do 
end do 

term(31) = term(31) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(33) = term(33) + t2(a,b,i,l) * tovoo(l, a, m, m)
term(34) = term(34) + t2(a,b,i,l) * tovoo(m, a, l, m)
end do 
end do 

term(33) = term(33) * (-2.0000000000000004d+0) 


    eom_ccsd_21_trans_aibiai = 0.d+0
    do s = 0, 34
    eom_ccsd_21_trans_aibiai = eom_ccsd_21_trans_aibiai + term(s)
    end do

    end function eom_ccsd_21_trans_aibiai
    function eom_ccsd_21_trans_aibibi(t2, nocc, nactive, a, i, b) 
    double precision :: eom_ccsd_21_trans_aibibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,d,l,e,m 
    double precision, dimension(0:34) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,i,i) * tvvov(b, b, i, d)
term(1) = term(1) + t2(b,d,i,i) * tvvov(a, b, i, d)
term(2) = term(2) + t2(a,d,i,i) * tvvov(b, d, i, b)
term(3) = term(3) + t2(b,d,i,i) * tvvov(a, d, i, b)
term(4) = term(4) + tov(i, d) * t2(a,d,i,i)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = term(2) * 2.0000000000000004d+0 
term(3) = term(3) * 2.0000000000000004d+0 
term(4) = -term(4) 

do l = 1, nocc 
term(5) = term(5) + t2(a,b,l,i) * tovoo(l, b, i, i)
term(6) = term(6) + t2(a,b,i,l) * tovoo(l, b, i, i)
term(7) = term(7) + t2(a,b,i,l) * tovoo(i, b, l, i)
term(8) = term(8) + t2(a,b,l,i) * tovoo(i, b, l, i)
term(9) = term(9) + tov(l, b) * t2(a,b,i,l)
term(10) = term(10) + tov(l, b) * t2(a,b,l,i)
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 
term(8) = term(8) * (-2.0000000000000004d+0) 
term(9) = -term(9) 
term(10) = -term(10) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(11) = term(11) + t2(a,d,l,i) * tovoo(i, d, l, i)
term(12) = term(12) + t2(a,d,l,i) * tovoo(l, d, i, i)
term(13) = term(13) + t2(a,d,l,i) * tvvov(b, d, l, b)
term(14) = term(14) + t2(b,d,l,i) * tvvov(a, d, l, b)
term(15) = term(15) + t2(b,d,i,l) * tvvov(a, b, l, d)
term(16) = term(16) + t2(a,d,i,l) * tvvov(b, b, l, d)
term(17) = term(17) + t2(b,d,l,i) * tvvov(a, b, l, d)
term(18) = term(18) + t2(a,d,l,i) * tvvov(b, b, l, d)
end do 
end do 

term(13) = -term(13) 
term(14) = -term(14) 
term(15) = term(15) * 2.0000000000000004d+0 
term(16) = term(16) * 2.0000000000000004d+0 
term(17) = -term(17) 
term(18) = -term(18) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(19) = term(19) + t2(a,d,i,l) * tovoo(i, d, l, i)
term(20) = term(20) + t2(a,d,i,i) * tovoo(i, d, l, l)
term(21) = term(21) + t2(a,d,i,l) * tovoo(l, d, i, i)
term(22) = term(22) + t2(a,d,i,i) * tovoo(l, d, i, l)
term(23) = term(23) + t2(b,d,i,l) * tvvov(a, d, l, b)
term(24) = term(24) + t2(a,d,i,l) * tvvov(b, d, l, b)
end do 
end do 

term(20) = term(20) * (-2.0000000000000004d+0) 
term(21) = term(21) * (-2.0000000000000004d+0) 
term(23) = -term(23) 
term(24) = -term(24) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(25) = term(25) + t2(d,e,i,i) * tvvov(a, d, i, e)
end do 
end do 

term(25) = -term(25) 

term(26) = term(26) + tvvvo(b, b, a, i)
term(27) = term(27) + tvvvo(a, b, b, i)
term(28) = term(28) + tvooo(a, i, i, i)

term(28) = -term(28) 

do l = 1, nocc 
do m = 1, nocc 
term(29) = term(29) + t2(a,b,m,l) * tovoo(l, b, m, i)
term(30) = term(30) + t2(a,b,m,l) * tovoo(m, b, l, i)
term(31) = term(31) + t2(a,b,m,i) * tovoo(m, b, l, l)
term(32) = term(32) + t2(a,b,m,i) * tovoo(l, b, m, l)
end do 
end do 

term(31) = term(31) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(33) = term(33) + t2(a,b,i,m) * tovoo(m, b, l, l)
term(34) = term(34) + t2(a,b,i,m) * tovoo(l, b, m, l)
end do 
end do 

term(33) = term(33) * (-2.0000000000000004d+0) 


    eom_ccsd_21_trans_aibibi = 0.d+0
    do s = 0, 34
    eom_ccsd_21_trans_aibibi = eom_ccsd_21_trans_aibibi + term(s)
    end do

    end function eom_ccsd_21_trans_aibibi
    function eom_ccsd_21_trans_aiaiai(t2, nocc, nactive, a, i) 
    double precision :: eom_ccsd_21_trans_aiaiai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i 
    integer :: s ,d,l,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,i,i) * tvvov(a, a, i, d)
term(1) = term(1) + t2(a,d,i,i) * tvvov(a, d, i, a)
term(2) = term(2) + tov(i, d) * t2(a,d,i,i)
end do 

term(0) = -term(0) 
term(1) = term(1) * 2.000000000000001d+0 
term(2) = -term(2) 

do l = 1, nocc 
term(3) = term(3) + t2(a,a,i,l) * tovoo(l, a, i, i)
term(4) = term(4) + t2(a,a,i,l) * tovoo(i, a, l, i)
term(5) = term(5) + tov(l, a) * t2(a,a,i,l)
end do 

term(4) = term(4) * (-2.000000000000001d+0) 
term(5) = -term(5) 

term(6) = term(6) + tvvvo(a, a, a, i)
term(7) = term(7) + tvooo(a, i, i, i)

term(7) = -term(7) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(8) = term(8) + t2(a,d,l,i) * tovoo(i, d, l, i)
term(9) = term(9) + t2(a,d,l,i) * tvvov(a, d, l, a)
term(10) = term(10) + t2(a,d,i,l) * tvvov(a, a, l, d)
term(11) = term(11) + t2(a,d,l,i) * tvvov(a, a, l, d)
term(12) = term(12) + t2(a,d,l,i) * tovoo(l, d, i, i)
end do 
end do 

term(9) = -term(9) 
term(10) = term(10) * 2.000000000000001d+0 
term(11) = -term(11) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(13) = term(13) + t2(a,d,i,l) * tovoo(i, d, l, i)
term(14) = term(14) + t2(a,d,i,i) * tovoo(i, d, l, l)
term(15) = term(15) + t2(a,d,i,l) * tvvov(a, d, l, a)
term(16) = term(16) + t2(a,d,i,l) * tovoo(l, d, i, i)
term(17) = term(17) + t2(a,d,i,i) * tovoo(l, d, i, l)
end do 
end do 

term(14) = term(14) * (-2.000000000000001d+0) 
term(15) = -term(15) 
term(16) = term(16) * (-2.000000000000001d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(18) = term(18) + t2(d,e,i,i) * tvvov(a, d, i, e)
end do 
end do 

term(18) = term(18) * (-0.5000000000000001d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(19) = term(19) + t2(d,e,i,i) * tvvov(a, e, i, d)
end do 
end do 

term(19) = term(19) * (-0.5000000000000001d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(20) = term(20) + t2(a,a,l,m) * tovoo(m, a, l, i)
term(21) = term(21) + t2(a,a,i,m) * tovoo(m, a, l, l)
term(22) = term(22) + t2(a,a,l,m) * tovoo(l, a, m, i)
term(23) = term(23) + t2(a,a,i,m) * tovoo(l, a, m, l)
end do 
end do 

term(20) = term(20) * 0.5000000000000001d+0 
term(21) = term(21) * (-2.000000000000001d+0) 
term(22) = term(22) * 0.5000000000000001d+0 


    eom_ccsd_21_trans_aiaiai = 0.d+0
    do s = 0, 23
    eom_ccsd_21_trans_aiaiai = eom_ccsd_21_trans_aiaiai + term(s)
    end do

    end function eom_ccsd_21_trans_aiaiai
    function eom_ccsd_21_trans_aibjck(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_ccsd_21_trans_aibjck 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,d,l 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,i,j) * tvvov(b, c, k, d)
term(1) = term(1) + t2(b,d,j,i) * tvvov(a, c, k, d)
term(2) = term(2) + t2(a,d,i,j) * tvvov(b, d, k, c)
term(3) = term(3) + t2(b,d,j,i) * tvvov(a, d, k, c)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = term(2) * 2.0000000000000004d+0 
term(3) = term(3) * 2.0000000000000004d+0 

do l = 1, nocc 
term(4) = term(4) + t2(a,b,l,j) * tovoo(l, c, k, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(l, c, k, j)
term(6) = term(6) + t2(a,b,i,l) * tovoo(k, c, l, j)
term(7) = term(7) + t2(a,b,l,j) * tovoo(k, c, l, i)
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 
term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_21_trans_aibjck = 0.d+0
    do s = 0, 7
    eom_ccsd_21_trans_aibjck = eom_ccsd_21_trans_aibjck + term(s)
    end do

    end function eom_ccsd_21_trans_aibjck
    end module eom_ccsd_21_trans
    
