module cisd02

    use ccsd_transformed_integrals
    use basis
    
    implicit none

    contains
    
    function cisd02_aiaj(a, i, j) 
    double precision :: cisd02_aiaj   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(a, j, a, i)

term(0) = term(0) * 2.0d+0 


    cisd02_aiaj = 0.d+0
    do s = 0, 0
    cisd02_aiaj = cisd02_aiaj + term(s)
    end do

    end function cisd02_aiaj
    function cisd02_aibi(a, i, b) 
    double precision :: cisd02_aibi   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, i, a, i)

term(0) = term(0) * 2.0d+0 


    cisd02_aibi = 0.d+0
    do s = 0, 0
    cisd02_aibi = cisd02_aibi + term(s)
    end do

    end function cisd02_aibi
    function cisd02_aiai(a, i) 
    double precision :: cisd02_aiai   
    integer, intent(in) :: a, i 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(a, i, a, i)

term(0) = term(0) * 2.0d+0 


    cisd02_aiai = 0.d+0
    do s = 0, 0
    cisd02_aiai = cisd02_aiai + term(s)
    end do

    end function cisd02_aiai
    function cisd02_aibj(a, i, b, j) 
    double precision :: cisd02_aibj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, i, a, j)
term(1) = term(1) + vovo(b, j, a, i)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 4.0d+0 


    cisd02_aibj = 0.d+0
    do s = 0, 1
    cisd02_aibj = cisd02_aibj + term(s)
    end do

    end function cisd02_aibj
    end module cisd02
    