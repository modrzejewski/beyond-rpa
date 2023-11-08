module eom_ccsd_12_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_ccsd_12_trans_aiajck(i, j, c, k) 
    double precision :: eom_ccsd_12_trans_aiajck   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, j, i)
term(1) = term(1) + tovoo(j, c, k, i)

term(0) = term(0) * (-2.0d+0) 


    eom_ccsd_12_trans_aiajck = 0.d+0
    do s = 0, 1
    eom_ccsd_12_trans_aiajck = eom_ccsd_12_trans_aiajck + term(s)
    end do

    end function eom_ccsd_12_trans_aiajck
    function eom_ccsd_12_trans_aibjak(i, b, j, k) 
    double precision :: eom_ccsd_12_trans_aibjak   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, k, i)
term(1) = term(1) + tovoo(k, b, j, i)

term(0) = term(0) * (-2.0d+0) 


    eom_ccsd_12_trans_aibjak = 0.d+0
    do s = 0, 1
    eom_ccsd_12_trans_aibjak = eom_ccsd_12_trans_aibjak + term(s)
    end do

    end function eom_ccsd_12_trans_aibjak
    function eom_ccsd_12_trans_aibick(a, b, c, k) 
    double precision :: eom_ccsd_12_trans_aibick   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, k, c)
term(1) = term(1) + tvvov(a, c, k, b)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    eom_ccsd_12_trans_aibick = 0.d+0
    do s = 0, 1
    eom_ccsd_12_trans_aibick = eom_ccsd_12_trans_aibick + term(s)
    end do

    end function eom_ccsd_12_trans_aibick
    function eom_ccsd_12_trans_aibjci(a, b, j, c) 
    double precision :: eom_ccsd_12_trans_aibjci   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, j, c)
term(1) = term(1) + tvvov(a, c, j, b)

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 


    eom_ccsd_12_trans_aibjci = 0.d+0
    do s = 0, 1
    eom_ccsd_12_trans_aibjci = eom_ccsd_12_trans_aibjci + term(s)
    end do

    end function eom_ccsd_12_trans_aibjci
    function eom_ccsd_12_trans_aiajak(a, i, j, k) 
    double precision :: eom_ccsd_12_trans_aiajak   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, a, k, i)
term(1) = term(1) + tovoo(k, a, j, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_ccsd_12_trans_aiajak = 0.d+0
    do s = 0, 1
    eom_ccsd_12_trans_aiajak = eom_ccsd_12_trans_aiajak + term(s)
    end do

    end function eom_ccsd_12_trans_aiajak
    function eom_ccsd_12_trans_aibibk(a, b, k) 
    double precision :: eom_ccsd_12_trans_aibibk   
    integer, intent(in) :: a, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, k, b)



    eom_ccsd_12_trans_aibibk = 0.d+0
    do s = 0, 0
    eom_ccsd_12_trans_aibibk = eom_ccsd_12_trans_aibibk + term(s)
    end do

    end function eom_ccsd_12_trans_aibibk
    function eom_ccsd_12_trans_aibjbi(a, b, j) 
    double precision :: eom_ccsd_12_trans_aibjbi   
    integer, intent(in) :: a, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, j, b)



    eom_ccsd_12_trans_aibjbi = 0.d+0
    do s = 0, 0
    eom_ccsd_12_trans_aibjbi = eom_ccsd_12_trans_aibjbi + term(s)
    end do

    end function eom_ccsd_12_trans_aibjbi
    function eom_ccsd_12_trans_aiaick(nocc, a, i, c, k) 
    double precision :: eom_ccsd_12_trans_aiaick 
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
term(2) = term(2) * 2.0d+0 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 

do l = 1, nocc 
term(5) = term(5) + tovoo(k, c, l, l)
term(6) = term(6) + tovoo(l, c, k, l)
end do 

term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-2.0d+0) 


    eom_ccsd_12_trans_aiaick = 0.d+0
    do s = 0, 6
    eom_ccsd_12_trans_aiaick = eom_ccsd_12_trans_aiaick + term(s)
    end do

    end function eom_ccsd_12_trans_aiaick
    function eom_ccsd_12_trans_aiajcj(i, j, c) 
    double precision :: eom_ccsd_12_trans_aiajcj   
    integer, intent(in) :: i, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, c, j, i)

term(0) = -term(0) 


    eom_ccsd_12_trans_aiajcj = 0.d+0
    do s = 0, 0
    eom_ccsd_12_trans_aiajcj = eom_ccsd_12_trans_aiajcj + term(s)
    end do

    end function eom_ccsd_12_trans_aiajcj
    function eom_ccsd_12_trans_aiajci(nocc, a, i, j, c) 
    double precision :: eom_ccsd_12_trans_aiajci 
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
term(2) = -term(2) 
term(3) = -term(3) 
term(4) = term(4) * 2.0d+0 

do l = 1, nocc 
term(5) = term(5) + tovoo(l, c, j, l)
term(6) = term(6) + tovoo(j, c, l, l)
end do 

term(6) = term(6) * (-2.0d+0) 


    eom_ccsd_12_trans_aiajci = 0.d+0
    do s = 0, 6
    eom_ccsd_12_trans_aiajci = eom_ccsd_12_trans_aiajci + term(s)
    end do

    end function eom_ccsd_12_trans_aiajci
    function eom_ccsd_12_trans_aibiak(nocc, a, i, b, k) 
    double precision :: eom_ccsd_12_trans_aibiak 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,l 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, b, k, i)
term(1) = term(1) + tovoo(k, b, i, i)
term(2) = term(2) + tov(k, b)
term(3) = term(3) + tvvov(a, b, k, a)
term(4) = term(4) + tvvov(a, a, k, b)

term(0) = term(0) * (-2.0d+0) 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 

do l = 1, nocc 
term(5) = term(5) + tovoo(k, b, l, l)
term(6) = term(6) + tovoo(l, b, k, l)
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_12_trans_aibiak = 0.d+0
    do s = 0, 6
    eom_ccsd_12_trans_aibiak = eom_ccsd_12_trans_aibiak + term(s)
    end do

    end function eom_ccsd_12_trans_aibiak
    function eom_ccsd_12_trans_aibjaj(i, b, j) 
    double precision :: eom_ccsd_12_trans_aibjaj   
    integer, intent(in) :: i, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, j, i)

term(0) = -term(0) 


    eom_ccsd_12_trans_aibjaj = 0.d+0
    do s = 0, 0
    eom_ccsd_12_trans_aibjaj = eom_ccsd_12_trans_aibjaj + term(s)
    end do

    end function eom_ccsd_12_trans_aibjaj
    function eom_ccsd_12_trans_aibjai(nocc, a, i, b, j) 
    double precision :: eom_ccsd_12_trans_aibjai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,l 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, i, i)
term(1) = term(1) + tovoo(i, b, j, i)
term(2) = term(2) + tov(j, b)
term(3) = term(3) + tvvov(a, b, j, a)
term(4) = term(4) + tvvov(a, a, j, b)

term(0) = term(0) * (-2.0d+0) 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 
term(4) = term(4) * 2.0d+0 

do l = 1, nocc 
term(5) = term(5) + tovoo(j, b, l, l)
term(6) = term(6) + tovoo(l, b, j, l)
end do 

term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-2.0d+0) 


    eom_ccsd_12_trans_aibjai = 0.d+0
    do s = 0, 6
    eom_ccsd_12_trans_aibjai = eom_ccsd_12_trans_aibjai + term(s)
    end do

    end function eom_ccsd_12_trans_aibjai
    function eom_ccsd_12_trans_aibici(a, i, b, c) 
    double precision :: eom_ccsd_12_trans_aibici   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, i, c)
term(1) = term(1) + tvvov(a, c, i, b)



    eom_ccsd_12_trans_aibici = 0.d+0
    do s = 0, 1
    eom_ccsd_12_trans_aibici = eom_ccsd_12_trans_aibici + term(s)
    end do

    end function eom_ccsd_12_trans_aibici
    function eom_ccsd_12_trans_aiaiak(nocc, a, i, k) 
    double precision :: eom_ccsd_12_trans_aiaiak 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, k 
    integer :: s ,l 
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, k, i)
term(1) = term(1) + tovoo(k, a, i, i)
term(2) = term(2) + tov(k, a)
term(3) = term(3) + tvvov(a, a, k, a)

term(0) = -term(0) 
term(1) = -term(1) 

do l = 1, nocc 
term(4) = term(4) + tovoo(k, a, l, l)
term(5) = term(5) + tovoo(l, a, k, l)
end do 

term(4) = term(4) * 2.0d+0 
term(5) = -term(5) 


    eom_ccsd_12_trans_aiaiak = 0.d+0
    do s = 0, 5
    eom_ccsd_12_trans_aiaiak = eom_ccsd_12_trans_aiaiak + term(s)
    end do

    end function eom_ccsd_12_trans_aiaiak
    function eom_ccsd_12_trans_aiajaj(a, i, j) 
    double precision :: eom_ccsd_12_trans_aiajaj   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, a, j, i)

term(0) = term(0) * (-2.0d+0) 


    eom_ccsd_12_trans_aiajaj = 0.d+0
    do s = 0, 0
    eom_ccsd_12_trans_aiajaj = eom_ccsd_12_trans_aiajaj + term(s)
    end do

    end function eom_ccsd_12_trans_aiajaj
    function eom_ccsd_12_trans_aiajai(nocc, a, i, j) 
    double precision :: eom_ccsd_12_trans_aiajai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j 
    integer :: s ,l 
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, a, i, i)
term(1) = term(1) + tovoo(i, a, j, i)
term(2) = term(2) + tov(j, a)
term(3) = term(3) + tvvov(a, a, j, a)

term(0) = -term(0) 
term(1) = -term(1) 

do l = 1, nocc 
term(4) = term(4) + tovoo(j, a, l, l)
term(5) = term(5) + tovoo(l, a, j, l)
end do 

term(4) = term(4) * 2.0d+0 
term(5) = -term(5) 


    eom_ccsd_12_trans_aiajai = 0.d+0
    do s = 0, 5
    eom_ccsd_12_trans_aiajai = eom_ccsd_12_trans_aiajai + term(s)
    end do

    end function eom_ccsd_12_trans_aiajai
    function eom_ccsd_12_trans_aibibi(a, i, b) 
    double precision :: eom_ccsd_12_trans_aibibi   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, i, b)

term(0) = term(0) * 2.0d+0 


    eom_ccsd_12_trans_aibibi = 0.d+0
    do s = 0, 0
    eom_ccsd_12_trans_aibibi = eom_ccsd_12_trans_aibibi + term(s)
    end do

    end function eom_ccsd_12_trans_aibibi
    function eom_ccsd_12_trans_aiaici(nocc, a, i, c) 
    double precision :: eom_ccsd_12_trans_aiaici 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, c 
    integer :: s ,l 
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    do l = 1, nocc 
term(0) = term(0) + tovoo(l, c, i, l)
term(1) = term(1) + tovoo(i, c, l, l)
end do 

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 

term(2) = term(2) + tov(i, c)
term(3) = term(3) + tovoo(i, c, i, i)
term(4) = term(4) + tvvov(a, a, i, c)
term(5) = term(5) + tvvov(a, c, i, a)

term(3) = -term(3) 


    eom_ccsd_12_trans_aiaici = 0.d+0
    do s = 0, 5
    eom_ccsd_12_trans_aiaici = eom_ccsd_12_trans_aiaici + term(s)
    end do

    end function eom_ccsd_12_trans_aiaici
    function eom_ccsd_12_trans_aibiai(nocc, a, i, b) 
    double precision :: eom_ccsd_12_trans_aibiai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b 
    integer :: s ,l 
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    do l = 1, nocc 
term(0) = term(0) + tovoo(i, b, l, l)
term(1) = term(1) + tovoo(l, b, i, l)
end do 

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 

term(2) = term(2) + tov(i, b)
term(3) = term(3) + tovoo(i, b, i, i)
term(4) = term(4) + tvvov(a, b, i, a)
term(5) = term(5) + tvvov(a, a, i, b)

term(3) = -term(3) 


    eom_ccsd_12_trans_aibiai = 0.d+0
    do s = 0, 5
    eom_ccsd_12_trans_aibiai = eom_ccsd_12_trans_aibiai + term(s)
    end do

    end function eom_ccsd_12_trans_aibiai
    function eom_ccsd_12_trans_aiaiai(nocc, a, i) 
    double precision :: eom_ccsd_12_trans_aiaiai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i 
    integer :: s ,l 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    do l = 1, nocc 
term(0) = term(0) + tovoo(i, a, l, l)
term(1) = term(1) + tovoo(l, a, i, l)
end do 

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-2.0d+0) 

term(2) = term(2) + tov(i, a)
term(3) = term(3) + tovoo(i, a, i, i)
term(4) = term(4) + tvvov(a, a, i, a)

term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * 2.0d+0 


    eom_ccsd_12_trans_aiaiai = 0.d+0
    do s = 0, 4
    eom_ccsd_12_trans_aiaiai = eom_ccsd_12_trans_aiaiai + term(s)
    end do

    end function eom_ccsd_12_trans_aiaiai
    end module eom_ccsd_12_trans
    
