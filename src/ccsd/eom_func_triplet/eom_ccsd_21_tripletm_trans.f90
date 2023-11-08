module eom_ccsd_21_tripletm_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_ccsd_21_tripletm_trans_aibjak(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_ccsd_21_tripletm_trans_aibjak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,d,l,e 
    double precision, dimension(0:12) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, k, i)

term(0) = term(0) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(1) = term(1) + t2(b,d,j,i) * tvvov(a, a, k, d)
term(2) = term(2) + t2(a,d,i,j) * tvvov(b, a, k, d)
term(3) = term(3) + tov(k, d) * t2(b,d,j,i)
end do 

term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5d+0) 

do l = 1, nocc 
term(4) = term(4) + t2(a,b,l,j) * tovoo(l, a, k, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(l, a, k, j)
end do 

term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(b,d,j,l) * tovoo(k, d, l, i)
term(7) = term(7) + t2(b,d,j,i) * tovoo(k, d, l, l)
term(8) = term(8) + t2(b,d,j,l) * tovoo(l, d, k, i)
term(9) = term(9) + t2(b,d,j,i) * tovoo(l, d, k, l)
end do 
end do 

term(6) = term(6) * 0.5d+0 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = term(9) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(10) = term(10) + t2(b,d,l,i) * tovoo(k, d, l, j)
term(11) = term(11) + t2(b,d,l,j) * tovoo(l, d, k, i)
end do 
end do 

term(10) = term(10) * 0.5d+0 
term(11) = term(11) * 0.5d+0 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(d,e,i,j) * tvvov(b, e, k, d)
end do 
end do 

term(12) = term(12) * (-0.5d+0) 


    eom_ccsd_21_tripletm_trans_aibjak = 0.d+0
    do s = 0, 12
    eom_ccsd_21_tripletm_trans_aibjak = eom_ccsd_21_tripletm_trans_aibjak + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibjak
    function eom_ccsd_21_tripletm_trans_aibjbk(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_ccsd_21_tripletm_trans_aibjbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,d,l,e 
    double precision, dimension(0:12) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, j)

term(0) = term(0) * 0.5d+0 

do d = nocc + 1, nactive 
term(1) = term(1) + t2(b,d,j,i) * tvvov(a, b, k, d)
term(2) = term(2) + t2(a,d,i,j) * tvvov(b, b, k, d)
term(3) = term(3) + tov(k, d) * t2(a,d,i,j)
end do 

term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * 0.5d+0 

do l = 1, nocc 
term(4) = term(4) + t2(a,b,l,j) * tovoo(l, b, k, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(l, b, k, j)
end do 

term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(a,d,i,l) * tovoo(k, d, l, j)
term(7) = term(7) + t2(a,d,i,j) * tovoo(k, d, l, l)
term(8) = term(8) + t2(a,d,i,l) * tovoo(l, d, k, j)
term(9) = term(9) + t2(a,d,i,j) * tovoo(l, d, k, l)
end do 
end do 

term(6) = term(6) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(10) = term(10) + t2(a,d,l,j) * tovoo(k, d, l, i)
term(11) = term(11) + t2(a,d,l,i) * tovoo(l, d, k, j)
end do 
end do 

term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(12) = term(12) + t2(d,e,i,j) * tvvov(a, d, k, e)
end do 
end do 

term(12) = term(12) * 0.5d+0 


    eom_ccsd_21_tripletm_trans_aibjbk = 0.d+0
    do s = 0, 12
    eom_ccsd_21_tripletm_trans_aibjbk = eom_ccsd_21_tripletm_trans_aibjbk + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibjbk
    function eom_ccsd_21_tripletm_trans_aiajck(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_ccsd_21_tripletm_trans_aiajck 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,d,l 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
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


    eom_ccsd_21_tripletm_trans_aiajck = 0.d+0
    do s = 0, 3
    eom_ccsd_21_tripletm_trans_aiajck = eom_ccsd_21_tripletm_trans_aiajck + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aiajck
    function eom_ccsd_21_tripletm_trans_aibick(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_ccsd_21_tripletm_trans_aibick 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,d,l 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(b,d,i,i) * tvvov(a, c, k, d)
term(1) = term(1) + t2(a,d,i,i) * tvvov(b, c, k, d)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * 0.5d+0 

do l = 1, nocc 
term(2) = term(2) + t2(a,b,l,i) * tovoo(l, c, k, i)
term(3) = term(3) + t2(a,b,i,l) * tovoo(l, c, k, i)
end do 

term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5d+0) 


    eom_ccsd_21_tripletm_trans_aibick = 0.d+0
    do s = 0, 3
    eom_ccsd_21_tripletm_trans_aibick = eom_ccsd_21_tripletm_trans_aibick + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibick
    function eom_ccsd_21_tripletm_trans_aibjci(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_ccsd_21_tripletm_trans_aibjci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,d,l,m 
    double precision, dimension(0:12) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, j)

term(0) = term(0) * 0.5d+0 

do d = nocc + 1, nactive 
term(1) = term(1) + t2(b,d,j,i) * tvvov(a, c, i, d)
term(2) = term(2) + t2(a,d,i,j) * tvvov(b, c, i, d)
end do 

term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5d+0 

do l = 1, nocc 
term(3) = term(3) + t2(a,b,l,j) * tovoo(l, c, i, i)
term(4) = term(4) + t2(a,b,i,l) * tovoo(l, c, i, j)
term(5) = term(5) + tov(l, c) * t2(a,b,l,j)
end do 

term(3) = term(3) * 0.5d+0 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(6) = term(6) + t2(a,d,l,j) * tvvov(b, d, l, c)
term(7) = term(7) + t2(b,d,j,l) * tvvov(a, c, l, d)
term(8) = term(8) + t2(b,d,l,j) * tvvov(a, c, l, d)
end do 
end do 

term(6) = term(6) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(9) = term(9) + t2(b,d,j,l) * tvvov(a, d, l, c)
end do 
end do 

term(9) = term(9) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(10) = term(10) + t2(a,b,l,m) * tovoo(l, c, m, j)
term(11) = term(11) + t2(a,b,l,j) * tovoo(l, c, m, m)
term(12) = term(12) + t2(a,b,l,j) * tovoo(m, c, l, m)
end do 
end do 

term(10) = term(10) * 0.5d+0 
term(11) = -term(11) 
term(12) = term(12) * 0.5d+0 


    eom_ccsd_21_tripletm_trans_aibjci = 0.d+0
    do s = 0, 12
    eom_ccsd_21_tripletm_trans_aibjci = eom_ccsd_21_tripletm_trans_aibjci + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibjci
    function eom_ccsd_21_tripletm_trans_aibjcj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_ccsd_21_tripletm_trans_aibjcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,d,l,m 
    double precision, dimension(0:12) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, i)

term(0) = term(0) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(1) = term(1) + t2(b,d,j,i) * tvvov(a, c, j, d)
term(2) = term(2) + t2(a,d,i,j) * tvvov(b, c, j, d)
end do 

term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5d+0 

do l = 1, nocc 
term(3) = term(3) + t2(a,b,l,j) * tovoo(l, c, j, i)
term(4) = term(4) + t2(a,b,i,l) * tovoo(l, c, j, j)
term(5) = term(5) + tov(l, c) * t2(a,b,i,l)
end do 

term(3) = term(3) * 0.5d+0 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(6) = term(6) + t2(b,d,l,i) * tvvov(a, d, l, c)
term(7) = term(7) + t2(a,d,i,l) * tvvov(b, c, l, d)
term(8) = term(8) + t2(a,d,l,i) * tvvov(b, c, l, d)
end do 
end do 

term(6) = term(6) * 0.5d+0 
term(7) = -term(7) 
term(8) = term(8) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(9) = term(9) + t2(a,d,i,l) * tvvov(b, d, l, c)
end do 
end do 

term(9) = term(9) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
term(10) = term(10) + t2(a,b,l,m) * tovoo(m, c, l, i)
term(11) = term(11) + t2(a,b,i,l) * tovoo(l, c, m, m)
term(12) = term(12) + t2(a,b,i,l) * tovoo(m, c, l, m)
end do 
end do 

term(10) = term(10) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 


    eom_ccsd_21_tripletm_trans_aibjcj = 0.d+0
    do s = 0, 12
    eom_ccsd_21_tripletm_trans_aibjcj = eom_ccsd_21_tripletm_trans_aibjcj + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibjcj
    function eom_ccsd_21_tripletm_trans_aiajak(t2, nocc, nactive, a, i, j, k) 
    double precision :: eom_ccsd_21_tripletm_trans_aiajak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,l,d,e 
    double precision, dimension(0:21) :: term 
    term = 0.d+0 
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
do d = nocc + 1, nactive 
term(6) = term(6) + t2(a,d,j,l) * tovoo(k, d, l, i)
term(7) = term(7) + t2(a,d,j,i) * tovoo(k, d, l, l)
term(8) = term(8) + t2(a,d,j,l) * tovoo(l, d, k, i)
term(9) = term(9) + t2(a,d,j,i) * tovoo(l, d, k, l)
term(10) = term(10) + t2(a,d,i,l) * tovoo(k, d, l, j)
term(11) = term(11) + t2(a,d,i,j) * tovoo(k, d, l, l)
term(12) = term(12) + t2(a,d,i,l) * tovoo(l, d, k, j)
term(13) = term(13) + t2(a,d,i,j) * tovoo(l, d, k, l)
end do 
end do 

term(6) = term(6) * 0.5d+0 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = term(9) * 0.5d+0 
term(10) = term(10) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(14) = term(14) + t2(a,d,l,i) * tovoo(k, d, l, j)
term(15) = term(15) + t2(a,d,l,j) * tovoo(l, d, k, i)
term(16) = term(16) + t2(a,d,l,j) * tovoo(k, d, l, i)
term(17) = term(17) + t2(a,d,l,i) * tovoo(l, d, k, j)
end do 
end do 

term(14) = term(14) * 0.5d+0 
term(15) = term(15) * 0.5d+0 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(18) = term(18) + t2(d,e,i,j) * tvvov(a, e, k, d)
end do 
end do 

term(18) = term(18) * (-0.5d+0) 

do l = 1, nocc 
term(19) = term(19) + t2(a,a,j,l) * tovoo(l, a, k, i)
term(20) = term(20) + t2(a,a,i,l) * tovoo(l, a, k, j)
end do 

term(19) = term(19) * 0.5d+0 
term(20) = term(20) * (-0.5d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(21) = term(21) + t2(d,e,i,j) * tvvov(a, d, k, e)
end do 
end do 

term(21) = term(21) * 0.5d+0 


    eom_ccsd_21_tripletm_trans_aiajak = 0.d+0
    do s = 0, 21
    eom_ccsd_21_tripletm_trans_aiajak = eom_ccsd_21_tripletm_trans_aiajak + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aiajak
    function eom_ccsd_21_tripletm_trans_aibiak(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_ccsd_21_tripletm_trans_aibiak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,d,l,e 
    double precision, dimension(0:12) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, k, i)

term(0) = term(0) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(1) = term(1) + t2(b,d,i,i) * tvvov(a, a, k, d)
term(2) = term(2) + t2(a,d,i,i) * tvvov(b, a, k, d)
term(3) = term(3) + tov(k, d) * t2(b,d,i,i)
end do 

term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5d+0) 

do l = 1, nocc 
term(4) = term(4) + t2(a,b,l,i) * tovoo(l, a, k, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(l, a, k, i)
end do 

term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(b,d,i,l) * tovoo(k, d, l, i)
term(7) = term(7) + t2(b,d,i,i) * tovoo(k, d, l, l)
term(8) = term(8) + t2(b,d,i,l) * tovoo(l, d, k, i)
term(9) = term(9) + t2(b,d,i,i) * tovoo(l, d, k, l)
end do 
end do 

term(6) = term(6) * 0.5d+0 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = term(9) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(10) = term(10) + t2(b,d,l,i) * tovoo(k, d, l, i)
term(11) = term(11) + t2(b,d,l,i) * tovoo(l, d, k, i)
end do 
end do 

term(10) = term(10) * 0.5d+0 
term(11) = term(11) * 0.5d+0 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(d,e,i,i) * tvvov(b, e, k, d)
end do 
end do 

term(12) = term(12) * (-0.5d+0) 


    eom_ccsd_21_tripletm_trans_aibiak = 0.d+0
    do s = 0, 12
    eom_ccsd_21_tripletm_trans_aibiak = eom_ccsd_21_tripletm_trans_aibiak + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibiak
    function eom_ccsd_21_tripletm_trans_aibjai(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_ccsd_21_tripletm_trans_aibjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,l,d,e,m 
    double precision, dimension(0:21) :: term 
    term = 0.d+0 
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
end do 
end do 

term(2) = term(2) * 0.5d+0 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = term(5) * 0.5d+0 
term(6) = term(6) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(7) = term(7) + t2(b,d,l,i) * tovoo(i, d, l, j)
term(8) = term(8) + t2(b,d,l,j) * tovoo(l, d, i, i)
term(9) = term(9) + t2(a,d,l,j) * tvvov(b, d, l, a)
term(10) = term(10) + t2(b,d,j,l) * tvvov(a, a, l, d)
term(11) = term(11) + t2(b,d,l,j) * tvvov(a, a, l, d)
end do 
end do 

term(7) = term(7) * 0.5d+0 
term(8) = term(8) * 0.5d+0 
term(9) = term(9) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(d,e,i,j) * tvvov(b, e, i, d)
end do 
end do 

term(12) = term(12) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(13) = term(13) + t2(b,d,j,i) * tvvov(a, a, i, d)
term(14) = term(14) + t2(a,d,i,j) * tvvov(b, a, i, d)
term(15) = term(15) + tov(i, d) * t2(b,d,j,i)
end do 

term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5d+0 
term(15) = term(15) * (-0.5d+0) 

do l = 1, nocc 
term(16) = term(16) + t2(a,b,l,j) * tovoo(l, a, i, i)
term(17) = term(17) + t2(a,b,i,l) * tovoo(l, a, i, j)
term(18) = term(18) + tov(l, a) * t2(a,b,l,j)
end do 

term(16) = term(16) * 0.5d+0 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(19) = term(19) + t2(a,b,l,m) * tovoo(l, a, m, j)
term(20) = term(20) + t2(a,b,l,j) * tovoo(l, a, m, m)
term(21) = term(21) + t2(a,b,l,j) * tovoo(m, a, l, m)
end do 
end do 

term(19) = term(19) * 0.5d+0 
term(20) = -term(20) 
term(21) = term(21) * 0.5d+0 


    eom_ccsd_21_tripletm_trans_aibjai = 0.d+0
    do s = 0, 21
    eom_ccsd_21_tripletm_trans_aibjai = eom_ccsd_21_tripletm_trans_aibjai + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibjai
    function eom_ccsd_21_tripletm_trans_aibjaj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_ccsd_21_tripletm_trans_aibjaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,l,d,e,m 
    double precision, dimension(0:21) :: term 
    term = 0.d+0 
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
end do 
end do 

term(2) = term(2) * 0.5d+0 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = term(5) * 0.5d+0 
term(6) = term(6) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(7) = term(7) + t2(b,d,l,i) * tovoo(j, d, l, j)
term(8) = term(8) + t2(b,d,l,j) * tovoo(l, d, j, i)
term(9) = term(9) + t2(b,d,l,i) * tvvov(a, d, l, a)
term(10) = term(10) + t2(a,d,i,l) * tvvov(b, a, l, d)
term(11) = term(11) + t2(a,d,l,i) * tvvov(b, a, l, d)
end do 
end do 

term(7) = term(7) * 0.5d+0 
term(8) = term(8) * 0.5d+0 
term(9) = term(9) * 0.5d+0 
term(10) = -term(10) 
term(11) = term(11) * 0.5d+0 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(12) = term(12) + t2(d,e,i,j) * tvvov(b, e, j, d)
end do 
end do 

term(12) = term(12) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(13) = term(13) + t2(b,d,j,i) * tvvov(a, a, j, d)
term(14) = term(14) + t2(a,d,i,j) * tvvov(b, a, j, d)
term(15) = term(15) + tov(j, d) * t2(b,d,j,i)
end do 

term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5d+0 
term(15) = term(15) * (-0.5d+0) 

do l = 1, nocc 
term(16) = term(16) + t2(a,b,l,j) * tovoo(l, a, j, i)
term(17) = term(17) + t2(a,b,i,l) * tovoo(l, a, j, j)
term(18) = term(18) + tov(l, a) * t2(a,b,i,l)
end do 

term(16) = term(16) * 0.5d+0 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
term(19) = term(19) + t2(a,b,l,m) * tovoo(m, a, l, i)
term(20) = term(20) + t2(a,b,i,m) * tovoo(m, a, l, l)
term(21) = term(21) + t2(a,b,i,m) * tovoo(l, a, m, l)
end do 
end do 

term(19) = term(19) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 


    eom_ccsd_21_tripletm_trans_aibjaj = 0.d+0
    do s = 0, 21
    eom_ccsd_21_tripletm_trans_aibjaj = eom_ccsd_21_tripletm_trans_aibjaj + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibjaj
    function eom_ccsd_21_tripletm_trans_aibibk(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_ccsd_21_tripletm_trans_aibibk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,d,l,e 
    double precision, dimension(0:12) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, i)

term(0) = term(0) * 0.5d+0 

do d = nocc + 1, nactive 
term(1) = term(1) + t2(b,d,i,i) * tvvov(a, b, k, d)
term(2) = term(2) + t2(a,d,i,i) * tvvov(b, b, k, d)
term(3) = term(3) + tov(k, d) * t2(a,d,i,i)
end do 

term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * 0.5d+0 

do l = 1, nocc 
term(4) = term(4) + t2(a,b,l,i) * tovoo(l, b, k, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(l, b, k, i)
end do 

term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(a,d,i,l) * tovoo(k, d, l, i)
term(7) = term(7) + t2(a,d,i,i) * tovoo(k, d, l, l)
term(8) = term(8) + t2(a,d,i,l) * tovoo(l, d, k, i)
term(9) = term(9) + t2(a,d,i,i) * tovoo(l, d, k, l)
end do 
end do 

term(6) = term(6) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(10) = term(10) + t2(a,d,l,i) * tovoo(k, d, l, i)
term(11) = term(11) + t2(a,d,l,i) * tovoo(l, d, k, i)
end do 
end do 

term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(12) = term(12) + t2(d,e,i,i) * tvvov(a, d, k, e)
end do 
end do 

term(12) = term(12) * 0.5d+0 


    eom_ccsd_21_tripletm_trans_aibibk = 0.d+0
    do s = 0, 12
    eom_ccsd_21_tripletm_trans_aibibk = eom_ccsd_21_tripletm_trans_aibibk + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibibk
    function eom_ccsd_21_tripletm_trans_aibjbi(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_ccsd_21_tripletm_trans_aibjbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,l,d,e,m 
    double precision, dimension(0:21) :: term 
    term = 0.d+0 
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
end do 
end do 

term(2) = term(2) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(7) = term(7) + t2(a,d,l,j) * tovoo(i, d, l, i)
term(8) = term(8) + t2(a,d,l,i) * tovoo(l, d, i, j)
term(9) = term(9) + t2(a,d,l,j) * tvvov(b, d, l, b)
term(10) = term(10) + t2(b,d,j,l) * tvvov(a, b, l, d)
term(11) = term(11) + t2(b,d,l,j) * tvvov(a, b, l, d)
end do 
end do 

term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(12) = term(12) + t2(d,e,i,j) * tvvov(a, d, i, e)
end do 
end do 

term(12) = term(12) * 0.5d+0 

do d = nocc + 1, nactive 
term(13) = term(13) + t2(b,d,j,i) * tvvov(a, b, i, d)
term(14) = term(14) + t2(a,d,i,j) * tvvov(b, b, i, d)
term(15) = term(15) + tov(i, d) * t2(a,d,i,j)
end do 

term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5d+0 
term(15) = term(15) * 0.5d+0 

do l = 1, nocc 
term(16) = term(16) + t2(a,b,l,j) * tovoo(l, b, i, i)
term(17) = term(17) + t2(a,b,i,l) * tovoo(l, b, i, j)
term(18) = term(18) + tov(l, b) * t2(a,b,l,j)
end do 

term(16) = term(16) * 0.5d+0 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(19) = term(19) + t2(a,b,l,m) * tovoo(l, b, m, j)
term(20) = term(20) + t2(a,b,l,j) * tovoo(l, b, m, m)
term(21) = term(21) + t2(a,b,l,j) * tovoo(m, b, l, m)
end do 
end do 

term(19) = term(19) * 0.5d+0 
term(20) = -term(20) 
term(21) = term(21) * 0.5d+0 


    eom_ccsd_21_tripletm_trans_aibjbi = 0.d+0
    do s = 0, 21
    eom_ccsd_21_tripletm_trans_aibjbi = eom_ccsd_21_tripletm_trans_aibjbi + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibjbi
    function eom_ccsd_21_tripletm_trans_aibjbj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_ccsd_21_tripletm_trans_aibjbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,l,d,e,m 
    double precision, dimension(0:21) :: term 
    term = 0.d+0 
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
end do 
end do 

term(2) = term(2) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(7) = term(7) + t2(a,d,l,j) * tovoo(j, d, l, i)
term(8) = term(8) + t2(a,d,l,i) * tovoo(l, d, j, j)
term(9) = term(9) + t2(b,d,l,i) * tvvov(a, d, l, b)
term(10) = term(10) + t2(a,d,i,l) * tvvov(b, b, l, d)
term(11) = term(11) + t2(a,d,l,i) * tvvov(b, b, l, d)
end do 
end do 

term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * 0.5d+0 
term(10) = -term(10) 
term(11) = term(11) * 0.5d+0 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(12) = term(12) + t2(d,e,i,j) * tvvov(a, d, j, e)
end do 
end do 

term(12) = term(12) * 0.5d+0 

do d = nocc + 1, nactive 
term(13) = term(13) + t2(b,d,j,i) * tvvov(a, b, j, d)
term(14) = term(14) + t2(a,d,i,j) * tvvov(b, b, j, d)
term(15) = term(15) + tov(j, d) * t2(a,d,i,j)
end do 

term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5d+0 
term(15) = term(15) * 0.5d+0 

do l = 1, nocc 
term(16) = term(16) + t2(a,b,l,j) * tovoo(l, b, j, i)
term(17) = term(17) + t2(a,b,i,l) * tovoo(l, b, j, j)
term(18) = term(18) + tov(l, b) * t2(a,b,i,l)
end do 

term(16) = term(16) * 0.5d+0 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
term(19) = term(19) + t2(a,b,l,m) * tovoo(m, b, l, i)
term(20) = term(20) + t2(a,b,i,l) * tovoo(l, b, m, m)
term(21) = term(21) + t2(a,b,i,l) * tovoo(m, b, l, m)
end do 
end do 

term(19) = term(19) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 


    eom_ccsd_21_tripletm_trans_aibjbj = 0.d+0
    do s = 0, 21
    eom_ccsd_21_tripletm_trans_aibjbj = eom_ccsd_21_tripletm_trans_aibjbj + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibjbj
    function eom_ccsd_21_tripletm_trans_aiajci(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_ccsd_21_tripletm_trans_aiajci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,l,d,m 
    double precision, dimension(0:12) :: term 
    term = 0.d+0 
    do l = 1, nocc 
term(0) = term(0) + tov(l, c) * t2(a,a,j,l)
term(1) = term(1) + t2(a,a,j,l) * tovoo(l, c, i, i)
term(2) = term(2) + t2(a,a,i,l) * tovoo(l, c, i, j)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * 0.5d+0 
term(2) = term(2) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(3) = term(3) + t2(a,d,j,i) * tvvov(a, c, i, d)
term(4) = term(4) + t2(a,d,i,j) * tvvov(a, c, i, d)
end do 

term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(5) = term(5) + t2(a,d,l,j) * tvvov(a, d, l, c)
term(6) = term(6) + t2(a,d,j,l) * tvvov(a, c, l, d)
term(7) = term(7) + t2(a,d,l,j) * tvvov(a, c, l, d)
end do 
end do 

term(5) = term(5) * (-0.5d+0) 
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


    eom_ccsd_21_tripletm_trans_aiajci = 0.d+0
    do s = 0, 12
    eom_ccsd_21_tripletm_trans_aiajci = eom_ccsd_21_tripletm_trans_aiajci + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aiajci
    function eom_ccsd_21_tripletm_trans_aiajcj(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_ccsd_21_tripletm_trans_aiajcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,l,d,m 
    double precision, dimension(0:12) :: term 
    term = 0.d+0 
    do l = 1, nocc 
term(0) = term(0) + tov(l, c) * t2(a,a,i,l)
term(1) = term(1) + t2(a,a,j,l) * tovoo(l, c, j, i)
term(2) = term(2) + t2(a,a,i,l) * tovoo(l, c, j, j)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * 0.5d+0 
term(2) = term(2) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(3) = term(3) + t2(a,d,j,i) * tvvov(a, c, j, d)
term(4) = term(4) + t2(a,d,i,j) * tvvov(a, c, j, d)
end do 

term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(5) = term(5) + t2(a,d,l,i) * tvvov(a, d, l, c)
term(6) = term(6) + t2(a,d,i,l) * tvvov(a, c, l, d)
term(7) = term(7) + t2(a,d,l,i) * tvvov(a, c, l, d)
end do 
end do 

term(5) = term(5) * 0.5d+0 
term(6) = -term(6) 
term(7) = term(7) * 0.5d+0 

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


    eom_ccsd_21_tripletm_trans_aiajcj = 0.d+0
    do s = 0, 12
    eom_ccsd_21_tripletm_trans_aiajcj = eom_ccsd_21_tripletm_trans_aiajcj + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aiajcj
    function eom_ccsd_21_tripletm_trans_aibici(t2, nocc, nactive, a, i, b, c) 
    double precision :: eom_ccsd_21_tripletm_trans_aibici 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,d,l,m 
    double precision, dimension(0:21) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, i)
term(1) = term(1) + tvvvo(b, c, a, i)

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(2) = term(2) + t2(b,d,i,i) * tvvov(a, c, i, d)
term(3) = term(3) + t2(a,d,i,i) * tvvov(b, c, i, d)
end do 

term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * 0.5d+0 

do l = 1, nocc 
term(4) = term(4) + t2(a,b,l,i) * tovoo(l, c, i, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(l, c, i, i)
term(6) = term(6) + tov(l, c) * t2(a,b,l,i)
term(7) = term(7) + tov(l, c) * t2(a,b,i,l)
end do 

term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(8) = term(8) + t2(a,d,l,i) * tvvov(b, d, l, c)
term(9) = term(9) + t2(b,d,i,l) * tvvov(a, c, l, d)
term(10) = term(10) + t2(b,d,l,i) * tvvov(a, c, l, d)
term(11) = term(11) + t2(b,d,l,i) * tvvov(a, d, l, c)
term(12) = term(12) + t2(a,d,i,l) * tvvov(b, c, l, d)
term(13) = term(13) + t2(a,d,l,i) * tvvov(b, c, l, d)
end do 
end do 

term(8) = term(8) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * 0.5d+0 
term(12) = -term(12) 
term(13) = term(13) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(14) = term(14) + t2(b,d,i,l) * tvvov(a, d, l, c)
term(15) = term(15) + t2(a,d,i,l) * tvvov(b, d, l, c)
end do 
end do 

term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
term(16) = term(16) + t2(a,b,l,m) * tovoo(l, c, m, i)
term(17) = term(17) + t2(a,b,l,i) * tovoo(l, c, m, m)
term(18) = term(18) + t2(a,b,l,i) * tovoo(m, c, l, m)
term(19) = term(19) + t2(a,b,l,m) * tovoo(m, c, l, i)
term(20) = term(20) + t2(a,b,i,l) * tovoo(l, c, m, m)
term(21) = term(21) + t2(a,b,i,l) * tovoo(m, c, l, m)
end do 
end do 

term(16) = term(16) * 0.5d+0 
term(17) = -term(17) 
term(18) = term(18) * 0.5d+0 
term(19) = term(19) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 


    eom_ccsd_21_tripletm_trans_aibici = 0.d+0
    do s = 0, 21
    eom_ccsd_21_tripletm_trans_aibici = eom_ccsd_21_tripletm_trans_aibici + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibici
    function eom_ccsd_21_tripletm_trans_aiajai(t2, nocc, nactive, a, i, j) 
    double precision :: eom_ccsd_21_tripletm_trans_aiajai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,l,d,e,m 
    double precision, dimension(0:30) :: term 
    term = 0.d+0 
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

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(a,d,j,l) * tovoo(i, d, l, i)
term(7) = term(7) + t2(a,d,j,i) * tovoo(i, d, l, l)
term(8) = term(8) + t2(a,d,j,l) * tovoo(l, d, i, i)
term(9) = term(9) + t2(a,d,j,i) * tovoo(l, d, i, l)
term(10) = term(10) + t2(a,d,i,l) * tovoo(i, d, l, j)
term(11) = term(11) + t2(a,d,i,j) * tovoo(i, d, l, l)
term(12) = term(12) + t2(a,d,i,l) * tovoo(l, d, i, j)
term(13) = term(13) + t2(a,d,i,j) * tovoo(l, d, i, l)
term(14) = term(14) + t2(a,d,j,l) * tvvov(a, d, l, a)
end do 
end do 

term(6) = term(6) * 0.5d+0 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = term(9) * 0.5d+0 
term(10) = term(10) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(15) = term(15) + t2(a,d,l,i) * tovoo(i, d, l, j)
term(16) = term(16) + t2(a,d,l,j) * tovoo(l, d, i, i)
term(17) = term(17) + t2(a,d,l,j) * tovoo(i, d, l, i)
term(18) = term(18) + t2(a,d,l,i) * tovoo(l, d, i, j)
term(19) = term(19) + t2(a,d,l,j) * tvvov(a, d, l, a)
term(20) = term(20) + t2(a,d,j,l) * tvvov(a, a, l, d)
term(21) = term(21) + t2(a,d,l,j) * tvvov(a, a, l, d)
end do 
end do 

term(15) = term(15) * 0.5d+0 
term(16) = term(16) * 0.5d+0 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(22) = term(22) + t2(d,e,i,j) * tvvov(a, e, i, d)
end do 
end do 

term(22) = term(22) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(23) = term(23) + t2(a,d,j,i) * tvvov(a, a, i, d)
term(24) = term(24) + t2(a,d,i,j) * tvvov(a, a, i, d)
term(25) = term(25) + tov(i, d) * t2(a,d,j,i)
term(26) = term(26) + tov(i, d) * t2(a,d,i,j)
end do 

term(23) = term(23) * (-0.5d+0) 
term(24) = term(24) * 0.5d+0 
term(25) = term(25) * (-0.5d+0) 
term(26) = term(26) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
term(27) = term(27) + t2(a,a,l,m) * tovoo(l, a, m, j)
term(28) = term(28) + t2(a,a,j,l) * tovoo(l, a, m, m)
term(29) = term(29) + t2(a,a,j,l) * tovoo(m, a, l, m)
end do 
end do 

term(27) = term(27) * 0.5d+0 
term(28) = -term(28) 
term(29) = term(29) * 0.5d+0 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(30) = term(30) + t2(d,e,i,j) * tvvov(a, d, i, e)
end do 
end do 

term(30) = term(30) * 0.5d+0 


    eom_ccsd_21_tripletm_trans_aiajai = 0.d+0
    do s = 0, 30
    eom_ccsd_21_tripletm_trans_aiajai = eom_ccsd_21_tripletm_trans_aiajai + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aiajai
    function eom_ccsd_21_tripletm_trans_aiajaj(t2, nocc, nactive, a, i, j) 
    double precision :: eom_ccsd_21_tripletm_trans_aiajaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,l,d,e,m 
    double precision, dimension(0:30) :: term 
    term = 0.d+0 
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

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(a,d,j,l) * tovoo(j, d, l, i)
term(7) = term(7) + t2(a,d,j,i) * tovoo(j, d, l, l)
term(8) = term(8) + t2(a,d,j,l) * tovoo(l, d, j, i)
term(9) = term(9) + t2(a,d,j,i) * tovoo(l, d, j, l)
term(10) = term(10) + t2(a,d,i,l) * tovoo(j, d, l, j)
term(11) = term(11) + t2(a,d,i,j) * tovoo(j, d, l, l)
term(12) = term(12) + t2(a,d,i,l) * tovoo(l, d, j, j)
term(13) = term(13) + t2(a,d,i,j) * tovoo(l, d, j, l)
term(14) = term(14) + t2(a,d,i,l) * tvvov(a, d, l, a)
end do 
end do 

term(6) = term(6) * 0.5d+0 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = term(9) * 0.5d+0 
term(10) = term(10) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(15) = term(15) + t2(a,d,l,i) * tovoo(j, d, l, j)
term(16) = term(16) + t2(a,d,l,j) * tovoo(l, d, j, i)
term(17) = term(17) + t2(a,d,l,j) * tovoo(j, d, l, i)
term(18) = term(18) + t2(a,d,l,i) * tovoo(l, d, j, j)
term(19) = term(19) + t2(a,d,l,i) * tvvov(a, d, l, a)
term(20) = term(20) + t2(a,d,i,l) * tvvov(a, a, l, d)
term(21) = term(21) + t2(a,d,l,i) * tvvov(a, a, l, d)
end do 
end do 

term(15) = term(15) * 0.5d+0 
term(16) = term(16) * 0.5d+0 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * 0.5d+0 
term(20) = -term(20) 
term(21) = term(21) * 0.5d+0 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(22) = term(22) + t2(d,e,i,j) * tvvov(a, e, j, d)
end do 
end do 

term(22) = term(22) * (-0.5d+0) 

do d = nocc + 1, nactive 
term(23) = term(23) + t2(a,d,j,i) * tvvov(a, a, j, d)
term(24) = term(24) + t2(a,d,i,j) * tvvov(a, a, j, d)
term(25) = term(25) + tov(j, d) * t2(a,d,j,i)
term(26) = term(26) + tov(j, d) * t2(a,d,i,j)
end do 

term(23) = term(23) * (-0.5d+0) 
term(24) = term(24) * 0.5d+0 
term(25) = term(25) * (-0.5d+0) 
term(26) = term(26) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
term(27) = term(27) + t2(a,a,l,m) * tovoo(m, a, l, i)
term(28) = term(28) + t2(a,a,i,l) * tovoo(l, a, m, m)
term(29) = term(29) + t2(a,a,i,l) * tovoo(m, a, l, m)
end do 
end do 

term(27) = term(27) * (-0.5d+0) 
term(29) = term(29) * (-0.5d+0) 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(30) = term(30) + t2(d,e,i,j) * tvvov(a, d, j, e)
end do 
end do 

term(30) = term(30) * 0.5d+0 


    eom_ccsd_21_tripletm_trans_aiajaj = 0.d+0
    do s = 0, 30
    eom_ccsd_21_tripletm_trans_aiajaj = eom_ccsd_21_tripletm_trans_aiajaj + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aiajaj
    function eom_ccsd_21_tripletm_trans_aibiai(t2, nocc, nactive, a, i, b) 
    double precision :: eom_ccsd_21_tripletm_trans_aibiai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,d,l,m,e 
    double precision, dimension(0:30) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + tov(i, d) * t2(b,d,i,i)
term(1) = term(1) + t2(b,d,i,i) * tvvov(a, a, i, d)
term(2) = term(2) + t2(a,d,i,i) * tvvov(b, a, i, d)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5d+0 

do d = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(d,e,i,i) * tvvov(b, e, i, d)
end do 
end do 

term(3) = term(3) * (-0.5d+0) 

do l = 1, nocc 
term(4) = term(4) + t2(a,b,l,i) * tovoo(l, a, i, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(l, a, i, i)
term(6) = term(6) + tov(l, a) * t2(a,b,l,i)
term(7) = term(7) + tov(l, a) * t2(a,b,i,l)
end do 

term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(8) = term(8) + t2(a,d,l,i) * tvvov(b, d, l, a)
term(9) = term(9) + t2(b,d,i,l) * tvvov(a, a, l, d)
term(10) = term(10) + t2(b,d,l,i) * tvvov(a, a, l, d)
term(11) = term(11) + t2(b,d,l,i) * tvvov(a, d, l, a)
term(12) = term(12) + t2(a,d,i,l) * tvvov(b, a, l, d)
term(13) = term(13) + t2(a,d,l,i) * tvvov(b, a, l, d)
term(14) = term(14) + t2(b,d,l,i) * tovoo(i, d, l, i)
term(15) = term(15) + t2(b,d,l,i) * tovoo(l, d, i, i)
end do 
end do 

term(8) = term(8) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * 0.5d+0 
term(12) = -term(12) 
term(13) = term(13) * 0.5d+0 
term(14) = term(14) * 0.5d+0 
term(15) = term(15) * 0.5d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(16) = term(16) + t2(b,d,i,l) * tvvov(a, d, l, a)
term(17) = term(17) + t2(a,d,i,l) * tvvov(b, d, l, a)
term(18) = term(18) + t2(b,d,i,l) * tovoo(i, d, l, i)
term(19) = term(19) + t2(b,d,i,i) * tovoo(i, d, l, l)
term(20) = term(20) + t2(b,d,i,l) * tovoo(l, d, i, i)
term(21) = term(21) + t2(b,d,i,i) * tovoo(l, d, i, l)
end do 
end do 

term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * 0.5d+0 
term(18) = term(18) * 0.5d+0 
term(19) = -term(19) 
term(20) = -term(20) 
term(21) = term(21) * 0.5d+0 

do m = 1, nocc 
do l = 1, nocc 
term(22) = term(22) + t2(a,b,l,m) * tovoo(l, a, m, i)
term(23) = term(23) + t2(a,b,l,i) * tovoo(l, a, m, m)
term(24) = term(24) + t2(a,b,l,i) * tovoo(m, a, l, m)
term(25) = term(25) + t2(a,b,l,m) * tovoo(m, a, l, i)
term(26) = term(26) + t2(a,b,i,m) * tovoo(m, a, l, l)
term(27) = term(27) + t2(a,b,i,m) * tovoo(l, a, m, l)
end do 
end do 

term(22) = term(22) * 0.5d+0 
term(23) = -term(23) 
term(24) = term(24) * 0.5d+0 
term(25) = term(25) * (-0.5d+0) 
term(27) = term(27) * (-0.5d+0) 

term(28) = term(28) + tvvvo(a, a, b, i)
term(29) = term(29) + tvvvo(b, a, a, i)
term(30) = term(30) + tvooo(b, i, i, i)

term(28) = term(28) * 0.5d+0 
term(29) = term(29) * (-0.5d+0) 
term(30) = term(30) * (-0.5d+0) 


    eom_ccsd_21_tripletm_trans_aibiai = 0.d+0
    do s = 0, 30
    eom_ccsd_21_tripletm_trans_aibiai = eom_ccsd_21_tripletm_trans_aibiai + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibiai
    function eom_ccsd_21_tripletm_trans_aibibi(t2, nocc, nactive, a, i, b) 
    double precision :: eom_ccsd_21_tripletm_trans_aibibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,d,l,m,e 
    double precision, dimension(0:30) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + tov(i, d) * t2(a,d,i,i)
term(1) = term(1) + t2(b,d,i,i) * tvvov(a, b, i, d)
term(2) = term(2) + t2(a,d,i,i) * tvvov(b, b, i, d)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5d+0 

do e = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(3) = term(3) + t2(d,e,i,i) * tvvov(a, d, i, e)
end do 
end do 

term(3) = term(3) * 0.5d+0 

do l = 1, nocc 
term(4) = term(4) + t2(a,b,l,i) * tovoo(l, b, i, i)
term(5) = term(5) + t2(a,b,i,l) * tovoo(l, b, i, i)
term(6) = term(6) + tov(l, b) * t2(a,b,l,i)
term(7) = term(7) + tov(l, b) * t2(a,b,i,l)
end do 

term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * 0.5d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(8) = term(8) + t2(a,d,l,i) * tvvov(b, d, l, b)
term(9) = term(9) + t2(b,d,i,l) * tvvov(a, b, l, d)
term(10) = term(10) + t2(b,d,l,i) * tvvov(a, b, l, d)
term(11) = term(11) + t2(b,d,l,i) * tvvov(a, d, l, b)
term(12) = term(12) + t2(a,d,i,l) * tvvov(b, b, l, d)
term(13) = term(13) + t2(a,d,l,i) * tvvov(b, b, l, d)
term(14) = term(14) + t2(a,d,l,i) * tovoo(i, d, l, i)
term(15) = term(15) + t2(a,d,l,i) * tovoo(l, d, i, i)
end do 
end do 

term(8) = term(8) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * 0.5d+0 
term(12) = -term(12) 
term(13) = term(13) * 0.5d+0 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(16) = term(16) + t2(b,d,i,l) * tvvov(a, d, l, b)
term(17) = term(17) + t2(a,d,i,l) * tvvov(b, d, l, b)
term(18) = term(18) + t2(a,d,i,l) * tovoo(i, d, l, i)
term(19) = term(19) + t2(a,d,i,i) * tovoo(i, d, l, l)
term(20) = term(20) + t2(a,d,i,l) * tovoo(l, d, i, i)
term(21) = term(21) + t2(a,d,i,i) * tovoo(l, d, i, l)
end do 
end do 

term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * 0.5d+0 
term(18) = term(18) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 

do m = 1, nocc 
do l = 1, nocc 
term(22) = term(22) + t2(a,b,l,m) * tovoo(l, b, m, i)
term(23) = term(23) + t2(a,b,l,i) * tovoo(l, b, m, m)
term(24) = term(24) + t2(a,b,l,i) * tovoo(m, b, l, m)
term(25) = term(25) + t2(a,b,l,m) * tovoo(m, b, l, i)
term(26) = term(26) + t2(a,b,i,l) * tovoo(l, b, m, m)
term(27) = term(27) + t2(a,b,i,l) * tovoo(m, b, l, m)
end do 
end do 

term(22) = term(22) * 0.5d+0 
term(23) = -term(23) 
term(24) = term(24) * 0.5d+0 
term(25) = term(25) * (-0.5d+0) 
term(27) = term(27) * (-0.5d+0) 

term(28) = term(28) + tvvvo(a, b, b, i)
term(29) = term(29) + tvvvo(b, b, a, i)
term(30) = term(30) + tvooo(a, i, i, i)

term(28) = term(28) * 0.5d+0 
term(29) = term(29) * (-0.5d+0) 
term(30) = term(30) * 0.5d+0 


    eom_ccsd_21_tripletm_trans_aibibi = 0.d+0
    do s = 0, 30
    eom_ccsd_21_tripletm_trans_aibibi = eom_ccsd_21_tripletm_trans_aibibi + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibibi
    function eom_ccsd_21_tripletm_trans_aibjck(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_ccsd_21_tripletm_trans_aibjck 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,d,l 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
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


    eom_ccsd_21_tripletm_trans_aibjck = 0.d+0
    do s = 0, 3
    eom_ccsd_21_tripletm_trans_aibjck = eom_ccsd_21_tripletm_trans_aibjck + term(s)
    end do

    end function eom_ccsd_21_tripletm_trans_aibjck
    end module eom_ccsd_21_tripletm_trans
    
