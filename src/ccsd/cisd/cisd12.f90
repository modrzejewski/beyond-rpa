module cisd12

    use ccsd_transformed_integrals
    use basis
    
    implicit none

    contains
    
    function cisd12_aiajck(i, j, c, k) 
    double precision :: cisd12_aiajck   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(c, j, i, k)
term(1) = term(1) + vooo(c, k, i, j)

term(1) = term(1) * (-2.0d+0) 


    cisd12_aiajck = 0.d+0
    do s = 0, 1
    cisd12_aiajck = cisd12_aiajck + term(s)
    end do

    end function cisd12_aiajck
    function cisd12_aibjak(i, b, j, k) 
    double precision :: cisd12_aibjak   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(b, k, i, j)
term(1) = term(1) + vooo(b, j, i, k)

term(1) = term(1) * (-2.0d+0) 


    cisd12_aibjak = 0.d+0
    do s = 0, 1
    cisd12_aibjak = cisd12_aibjak + term(s)
    end do

    end function cisd12_aibjak
    function cisd12_aibjci(a, b, j, c) 
    double precision :: cisd12_aibjci   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvvo(a, b, c, j)
term(1) = term(1) + vvvo(a, c, b, j)

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 


    cisd12_aibjci = 0.d+0
    do s = 0, 1
    cisd12_aibjci = cisd12_aibjci + term(s)
    end do

    end function cisd12_aibjci
    function cisd12_aibick(a, b, c, k) 
    double precision :: cisd12_aibick   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvvo(a, b, c, k)
term(1) = term(1) + vvvo(a, c, b, k)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    cisd12_aibick = 0.d+0
    do s = 0, 1
    cisd12_aibick = cisd12_aibick + term(s)
    end do

    end function cisd12_aibick
    function cisd12_aiajak(a, i, j, k) 
    double precision :: cisd12_aiajak   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, k, i, j)
term(1) = term(1) + vooo(a, j, i, k)

term(0) = -term(0) 
term(1) = -term(1) 


    cisd12_aiajak = 0.d+0
    do s = 0, 1
    cisd12_aiajak = cisd12_aiajak + term(s)
    end do

    end function cisd12_aiajak
    function cisd12_aibjbi(a, b, j) 
    double precision :: cisd12_aibjbi   
    integer, intent(in) :: a, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvvo(a, b, b, j)



    cisd12_aibjbi = 0.d+0
    do s = 0, 0
    cisd12_aibjbi = cisd12_aibjbi + term(s)
    end do

    end function cisd12_aibjbi
    function cisd12_aibibk(a, b, k) 
    double precision :: cisd12_aibibk   
    integer, intent(in) :: a, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvvo(a, b, b, k)



    cisd12_aibibk = 0.d+0
    do s = 0, 0
    cisd12_aibibk = cisd12_aibibk + term(s)
    end do

    end function cisd12_aibibk
    function cisd12_aiajci(a, i, j, c) 
    double precision :: cisd12_aiajci   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(c, j, i, i)
term(1) = term(1) + vooo(c, i, i, j)
term(2) = term(2) + vvvo(a, a, c, j)
term(3) = term(3) + vvvo(a, c, a, j)

term(1) = term(1) * (-2.0d+0) 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 


    cisd12_aiajci = 0.d+0
    do s = 0, 3
    cisd12_aiajci = cisd12_aiajci + term(s)
    end do

    end function cisd12_aiajci
    function cisd12_aiajcj(i, j, c) 
    double precision :: cisd12_aiajcj   
    integer, intent(in) :: i, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(c, j, i, j)

term(0) = -term(0) 


    cisd12_aiajcj = 0.d+0
    do s = 0, 0
    cisd12_aiajcj = cisd12_aiajcj + term(s)
    end do

    end function cisd12_aiajcj
    function cisd12_aiaick(a, i, c, k) 
    double precision :: cisd12_aiaick   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(c, i, i, k)
term(1) = term(1) + vooo(c, k, i, i)
term(2) = term(2) + vvvo(a, a, c, k)
term(3) = term(3) + vvvo(a, c, a, k)

term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 


    cisd12_aiaick = 0.d+0
    do s = 0, 3
    cisd12_aiaick = cisd12_aiaick + term(s)
    end do

    end function cisd12_aiaick
    function cisd12_aibjai(a, i, b, j) 
    double precision :: cisd12_aibjai   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(b, i, i, j)
term(1) = term(1) + vooo(b, j, i, i)
term(2) = term(2) + vvvo(a, b, a, j)
term(3) = term(3) + vvvo(a, a, b, j)

term(1) = term(1) * (-2.0d+0) 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 


    cisd12_aibjai = 0.d+0
    do s = 0, 3
    cisd12_aibjai = cisd12_aibjai + term(s)
    end do

    end function cisd12_aibjai
    function cisd12_aibjaj(i, b, j) 
    double precision :: cisd12_aibjaj   
    integer, intent(in) :: i, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(b, j, i, j)

term(0) = -term(0) 


    cisd12_aibjaj = 0.d+0
    do s = 0, 0
    cisd12_aibjaj = cisd12_aibjaj + term(s)
    end do

    end function cisd12_aibjaj
    function cisd12_aibiak(a, i, b, k) 
    double precision :: cisd12_aibiak   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(b, k, i, i)
term(1) = term(1) + vooo(b, i, i, k)
term(2) = term(2) + vvvo(a, b, a, k)
term(3) = term(3) + vvvo(a, a, b, k)

term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 


    cisd12_aibiak = 0.d+0
    do s = 0, 3
    cisd12_aibiak = cisd12_aibiak + term(s)
    end do

    end function cisd12_aibiak
    function cisd12_aibici(a, i, b, c) 
    double precision :: cisd12_aibici   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvvo(a, b, c, i)
term(1) = term(1) + vvvo(a, c, b, i)



    cisd12_aibici = 0.d+0
    do s = 0, 1
    cisd12_aibici = cisd12_aibici + term(s)
    end do

    end function cisd12_aibici
    function cisd12_aiajai(a, i, j) 
    double precision :: cisd12_aiajai   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, i, i, j)
term(1) = term(1) + vooo(a, j, i, i)
term(2) = term(2) + vvvo(a, a, a, j)

term(0) = -term(0) 
term(1) = -term(1) 


    cisd12_aiajai = 0.d+0
    do s = 0, 2
    cisd12_aiajai = cisd12_aiajai + term(s)
    end do

    end function cisd12_aiajai
    function cisd12_aiajaj(a, i, j) 
    double precision :: cisd12_aiajaj   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, j, i, j)

term(0) = term(0) * (-2.0d+0) 


    cisd12_aiajaj = 0.d+0
    do s = 0, 0
    cisd12_aiajaj = cisd12_aiajaj + term(s)
    end do

    end function cisd12_aiajaj
    function cisd12_aiaiak(a, i, k) 
    double precision :: cisd12_aiaiak   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, k, i, i)
term(1) = term(1) + vooo(a, i, i, k)
term(2) = term(2) + vvvo(a, a, a, k)

term(0) = -term(0) 
term(1) = -term(1) 


    cisd12_aiaiak = 0.d+0
    do s = 0, 2
    cisd12_aiaiak = cisd12_aiaiak + term(s)
    end do

    end function cisd12_aiaiak
    function cisd12_aibibi(a, i, b) 
    double precision :: cisd12_aibibi   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vvvo(a, b, b, i)

term(0) = term(0) * 2.0d+0 


    cisd12_aibibi = 0.d+0
    do s = 0, 0
    cisd12_aibibi = cisd12_aibibi + term(s)
    end do

    end function cisd12_aibibi
    function cisd12_aiaici(a, i, c) 
    double precision :: cisd12_aiaici   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(c, i, i, i)
term(1) = term(1) + vvvo(a, a, c, i)
term(2) = term(2) + vvvo(a, c, a, i)

term(0) = -term(0) 


    cisd12_aiaici = 0.d+0
    do s = 0, 2
    cisd12_aiaici = cisd12_aiaici + term(s)
    end do

    end function cisd12_aiaici
    function cisd12_aibiai(a, i, b) 
    double precision :: cisd12_aibiai   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(b, i, i, i)
term(1) = term(1) + vvvo(a, b, a, i)
term(2) = term(2) + vvvo(a, a, b, i)

term(0) = -term(0) 


    cisd12_aibiai = 0.d+0
    do s = 0, 2
    cisd12_aibiai = cisd12_aibiai + term(s)
    end do

    end function cisd12_aibiai
    function cisd12_aiaiai(a, i) 
    double precision :: cisd12_aiaiai   
    integer, intent(in) :: a, i 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vooo(a, i, i, i)
term(1) = term(1) + vvvo(a, a, a, i)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 2.0d+0 


    cisd12_aiaiai = 0.d+0
    do s = 0, 1
    cisd12_aiaiai = cisd12_aiaiai + term(s)
    end do

    end function cisd12_aiaiai
    end module cisd12
    