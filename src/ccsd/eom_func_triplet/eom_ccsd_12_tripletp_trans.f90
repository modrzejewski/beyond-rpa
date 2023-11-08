module eom_ccsd_12_tripletp_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_ccsd_12_tripletp_trans_aibjak(i, b, j, k) 
    double precision :: eom_ccsd_12_tripletp_trans_aibjak   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, j, i)
term(1) = term(1) + tovoo(j, b, k, i)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-2.0d+0) 


    eom_ccsd_12_tripletp_trans_aibjak = 0.d+0
    do s = 0, 1
    eom_ccsd_12_tripletp_trans_aibjak = eom_ccsd_12_tripletp_trans_aibjak + term(s)
    end do

    end function eom_ccsd_12_tripletp_trans_aibjak
    function eom_ccsd_12_tripletp_trans_aiajck(i, j, c, k) 
    double precision :: eom_ccsd_12_tripletp_trans_aiajck   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, j, i)
term(1) = term(1) + tovoo(j, c, k, i)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 2.0d+0 


    eom_ccsd_12_tripletp_trans_aiajck = 0.d+0
    do s = 0, 1
    eom_ccsd_12_tripletp_trans_aiajck = eom_ccsd_12_tripletp_trans_aiajck + term(s)
    end do

    end function eom_ccsd_12_tripletp_trans_aiajck
    function eom_ccsd_12_tripletp_trans_aibjci(a, b, j, c) 
    double precision :: eom_ccsd_12_tripletp_trans_aibjci   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, j, c)
term(1) = term(1) + tvvov(a, c, j, b)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 2.0d+0 


    eom_ccsd_12_tripletp_trans_aibjci = 0.d+0
    do s = 0, 1
    eom_ccsd_12_tripletp_trans_aibjci = eom_ccsd_12_tripletp_trans_aibjci + term(s)
    end do

    end function eom_ccsd_12_tripletp_trans_aibjci
    function eom_ccsd_12_tripletp_trans_aibick(a, b, c, k) 
    double precision :: eom_ccsd_12_tripletp_trans_aibick   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, k, c)
term(1) = term(1) + tvvov(a, c, k, b)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-2.0d+0) 


    eom_ccsd_12_tripletp_trans_aibick = 0.d+0
    do s = 0, 1
    eom_ccsd_12_tripletp_trans_aibick = eom_ccsd_12_tripletp_trans_aibick + term(s)
    end do

    end function eom_ccsd_12_tripletp_trans_aibick
    function eom_ccsd_12_tripletp_trans_aibjai(nocc, a, i, b, j) 
    double precision :: eom_ccsd_12_tripletp_trans_aibjai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,l 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, b, j, i)
term(1) = term(1) + tovoo(j, b, i, i)
term(2) = term(2) + tov(j, b)
term(3) = term(3) + tvvov(a, b, j, a)
term(4) = term(4) + tvvov(a, a, j, b)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * 2.0d+0 

do l = 1, nocc 
term(5) = term(5) + tovoo(j, b, l, l)
term(6) = term(6) + tovoo(l, b, j, l)
end do 

term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-2.0d+0) 


    eom_ccsd_12_tripletp_trans_aibjai = 0.d+0
    do s = 0, 6
    eom_ccsd_12_tripletp_trans_aibjai = eom_ccsd_12_tripletp_trans_aibjai + term(s)
    end do

    end function eom_ccsd_12_tripletp_trans_aibjai
    function eom_ccsd_12_tripletp_trans_aibiak(nocc, a, i, b, k) 
    double precision :: eom_ccsd_12_tripletp_trans_aibiak 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,l 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, i, i)
term(1) = term(1) + tovoo(i, b, k, i)
term(2) = term(2) + tov(k, b)
term(3) = term(3) + tvvov(a, b, k, a)
term(4) = term(4) + tvvov(a, a, k, b)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * (-2.0d+0) 

do l = 1, nocc 
term(5) = term(5) + tovoo(k, b, l, l)
term(6) = term(6) + tovoo(l, b, k, l)
end do 

term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * 2.0d+0 


    eom_ccsd_12_tripletp_trans_aibiak = 0.d+0
    do s = 0, 6
    eom_ccsd_12_tripletp_trans_aibiak = eom_ccsd_12_tripletp_trans_aibiak + term(s)
    end do

    end function eom_ccsd_12_tripletp_trans_aibiak
    function eom_ccsd_12_tripletp_trans_aiajci(nocc, a, i, j, c) 
    double precision :: eom_ccsd_12_tripletp_trans_aiajci 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,l 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, c, j, i)
term(1) = term(1) + tovoo(j, c, i, i)
term(2) = term(2) + tov(j, c)
term(3) = term(3) + tvvov(a, a, j, c)
term(4) = term(4) + tvvov(a, c, j, a)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * 2.0d+0 

do l = 1, nocc 
term(5) = term(5) + tovoo(l, c, j, l)
term(6) = term(6) + tovoo(j, c, l, l)
end do 

term(5) = term(5) * 2.0d+0 
term(6) = term(6) * (-4.0d+0) 


    eom_ccsd_12_tripletp_trans_aiajci = 0.d+0
    do s = 0, 6
    eom_ccsd_12_tripletp_trans_aiajci = eom_ccsd_12_tripletp_trans_aiajci + term(s)
    end do

    end function eom_ccsd_12_tripletp_trans_aiajci
    function eom_ccsd_12_tripletp_trans_aiaick(nocc, a, i, c, k) 
    double precision :: eom_ccsd_12_tripletp_trans_aiaick 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,l 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, i, i)
term(1) = term(1) + tovoo(i, c, k, i)
term(2) = term(2) + tov(k, c)
term(3) = term(3) + tvvov(a, a, k, c)
term(4) = term(4) + tvvov(a, c, k, a)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * 2.0d+0 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * (-2.0d+0) 

do l = 1, nocc 
term(5) = term(5) + tovoo(k, c, l, l)
term(6) = term(6) + tovoo(l, c, k, l)
end do 

term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-2.0d+0) 


    eom_ccsd_12_tripletp_trans_aiaick = 0.d+0
    do s = 0, 6
    eom_ccsd_12_tripletp_trans_aiaick = eom_ccsd_12_tripletp_trans_aiaick + term(s)
    end do

    end function eom_ccsd_12_tripletp_trans_aiaick
    end module eom_ccsd_12_tripletp_trans
    
