module cisd21

    use ccsd_transformed_integrals
    use basis
    
    implicit none

    contains
    
    function cisd21_aibjak(i, b, j, k) 
    double precision :: cisd21_aibjak   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(b, j, i, k)

term(0) = -term(0) 


    cisd21_aibjak = 0.d+0
    do s = 0, 0
    cisd21_aibjak = cisd21_aibjak + term(s)
    end do

    end function cisd21_aibjak
    function cisd21_aibjbk(a, i, j, k) 
    double precision :: cisd21_aibjbk   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, i, j, k)

term(0) = -term(0) 


    cisd21_aibjbk = 0.d+0
    do s = 0, 0
    cisd21_aibjbk = cisd21_aibjbk + term(s)
    end do

    end function cisd21_aibjbk
    function cisd21_aibjcj(a, i, b, c) 
    double precision :: cisd21_aibjcj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvvo(b, c, a, i)



    cisd21_aibjcj = 0.d+0
    do s = 0, 0
    cisd21_aibjcj = cisd21_aibjcj + term(s)
    end do

    end function cisd21_aibjcj
    function cisd21_aibjci(a, b, j, c) 
    double precision :: cisd21_aibjci   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvvo(a, c, b, j)



    cisd21_aibjci = 0.d+0
    do s = 0, 0
    cisd21_aibjci = cisd21_aibjci + term(s)
    end do

    end function cisd21_aibjci
    function cisd21_aiajak(a, i, j, k) 
    double precision :: cisd21_aiajak   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, j, i, k)
term(1) = term(1) + vooo(a, i, j, k)

term(0) = -term(0) 
term(1) = -term(1) 


    cisd21_aiajak = 0.d+0
    do s = 0, 1
    cisd21_aiajak = cisd21_aiajak + term(s)
    end do

    end function cisd21_aiajak
    function cisd21_aibiak(i, b, k) 
    double precision :: cisd21_aibiak   
    integer, intent(in) :: i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(b, i, i, k)

term(0) = -term(0) 


    cisd21_aibiak = 0.d+0
    do s = 0, 0
    cisd21_aibiak = cisd21_aibiak + term(s)
    end do

    end function cisd21_aibiak
    function cisd21_aibjaj(a, i, b, j) 
    double precision :: cisd21_aibjaj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(b, j, i, j)
term(1) = term(1) + vvvo(a, b, a, i)

term(0) = -term(0) 


    cisd21_aibjaj = 0.d+0
    do s = 0, 1
    cisd21_aibjaj = cisd21_aibjaj + term(s)
    end do

    end function cisd21_aibjaj
    function cisd21_aibjai(a, i, b, j) 
    double precision :: cisd21_aibjai   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(b, j, i, i)
term(1) = term(1) + vvvo(a, a, b, j)

term(0) = -term(0) 


    cisd21_aibjai = 0.d+0
    do s = 0, 1
    cisd21_aibjai = cisd21_aibjai + term(s)
    end do

    end function cisd21_aibjai
    function cisd21_aiajcj(a, i, c) 
    double precision :: cisd21_aiajcj   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvvo(a, c, a, i)



    cisd21_aiajcj = 0.d+0
    do s = 0, 0
    cisd21_aiajcj = cisd21_aiajcj + term(s)
    end do

    end function cisd21_aiajcj
    function cisd21_aiajci(a, j, c) 
    double precision :: cisd21_aiajci   
    integer, intent(in) :: a, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvvo(a, c, a, j)



    cisd21_aiajci = 0.d+0
    do s = 0, 0
    cisd21_aiajci = cisd21_aiajci + term(s)
    end do

    end function cisd21_aiajci
    function cisd21_aibibk(a, i, k) 
    double precision :: cisd21_aibibk   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, i, i, k)

term(0) = -term(0) 


    cisd21_aibibk = 0.d+0
    do s = 0, 0
    cisd21_aibibk = cisd21_aibibk + term(s)
    end do

    end function cisd21_aibibk
    function cisd21_aibjbj(a, i, b, j) 
    double precision :: cisd21_aibjbj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, i, j, j)
term(1) = term(1) + vvvo(b, b, a, i)

term(0) = -term(0) 


    cisd21_aibjbj = 0.d+0
    do s = 0, 1
    cisd21_aibjbj = cisd21_aibjbj + term(s)
    end do

    end function cisd21_aibjbj
    function cisd21_aibjbi(a, i, b, j) 
    double precision :: cisd21_aibjbi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, i, i, j)
term(1) = term(1) + vvvo(a, b, b, j)

term(0) = -term(0) 


    cisd21_aibjbi = 0.d+0
    do s = 0, 1
    cisd21_aibjbi = cisd21_aibjbi + term(s)
    end do

    end function cisd21_aibjbi
    function cisd21_aibici(a, i, b, c) 
    double precision :: cisd21_aibici   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvvo(b, c, a, i)
term(1) = term(1) + vvvo(a, c, b, i)



    cisd21_aibici = 0.d+0
    do s = 0, 1
    cisd21_aibici = cisd21_aibici + term(s)
    end do

    end function cisd21_aibici
    function cisd21_aiaiak(a, i, k) 
    double precision :: cisd21_aiaiak   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, i, i, k)

term(0) = -term(0) 


    cisd21_aiaiak = 0.d+0
    do s = 0, 0
    cisd21_aiaiak = cisd21_aiaiak + term(s)
    end do

    end function cisd21_aiaiak
    function cisd21_aiajaj(a, i, j) 
    double precision :: cisd21_aiajaj   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, j, i, j)
term(1) = term(1) + vooo(a, i, j, j)
term(2) = term(2) + vvvo(a, a, a, i)

term(0) = -term(0) 
term(1) = -term(1) 


    cisd21_aiajaj = 0.d+0
    do s = 0, 2
    cisd21_aiajaj = cisd21_aiajaj + term(s)
    end do

    end function cisd21_aiajaj
    function cisd21_aiajai(a, i, j) 
    double precision :: cisd21_aiajai   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, j, i, i)
term(1) = term(1) + vooo(a, i, i, j)
term(2) = term(2) + vvvo(a, a, a, j)

term(0) = -term(0) 
term(1) = -term(1) 


    cisd21_aiajai = 0.d+0
    do s = 0, 2
    cisd21_aiajai = cisd21_aiajai + term(s)
    end do

    end function cisd21_aiajai
    function cisd21_aibiai(a, i, b) 
    double precision :: cisd21_aibiai   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(b, i, i, i)
term(1) = term(1) + vvvo(a, b, a, i)
term(2) = term(2) + vvvo(a, a, b, i)

term(0) = -term(0) 


    cisd21_aibiai = 0.d+0
    do s = 0, 2
    cisd21_aibiai = cisd21_aibiai + term(s)
    end do

    end function cisd21_aibiai
    function cisd21_aiaici(a, i, c) 
    double precision :: cisd21_aiaici   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvvo(a, c, a, i)



    cisd21_aiaici = 0.d+0
    do s = 0, 0
    cisd21_aiaici = cisd21_aiaici + term(s)
    end do

    end function cisd21_aiaici
    function cisd21_aibibi(a, i, b) 
    double precision :: cisd21_aibibi   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, i, i, i)
term(1) = term(1) + vvvo(b, b, a, i)
term(2) = term(2) + vvvo(a, b, b, i)

term(0) = -term(0) 


    cisd21_aibibi = 0.d+0
    do s = 0, 2
    cisd21_aibibi = cisd21_aibibi + term(s)
    end do

    end function cisd21_aibibi
    function cisd21_aiaiai(a, i) 
    double precision :: cisd21_aiaiai   
    integer, intent(in) :: a, i 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, i, i, i)
term(1) = term(1) + vvvo(a, a, a, i)

term(0) = -term(0) 


    cisd21_aiaiai = 0.d+0
    do s = 0, 1
    cisd21_aiaiai = cisd21_aiaiai + term(s)
    end do

    end function cisd21_aiaiai
    end module cisd21
    