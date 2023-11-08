module cisd20

    use ccsd_transformed_integrals
    use basis
    
    implicit none

    contains
    
    function cisd20_aiaj(a, i, j) 
    double precision :: cisd20_aiaj   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(a, j, a, i)



    cisd20_aiaj = 0.d+0
    do s = 0, 0
    cisd20_aiaj = cisd20_aiaj + term(s)
    end do

    end function cisd20_aiaj
    function cisd20_aibi(a, i, b) 
    double precision :: cisd20_aibi   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, i, a, i)



    cisd20_aibi = 0.d+0
    do s = 0, 0
    cisd20_aibi = cisd20_aibi + term(s)
    end do

    end function cisd20_aibi
    function cisd20_aiai(a, i) 
    double precision :: cisd20_aiai   
    integer, intent(in) :: a, i 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(a, i, a, i)

term(0) = term(0) * 0.49999999999999994d+0 


    cisd20_aiai = 0.d+0
    do s = 0, 0
    cisd20_aiai = cisd20_aiai + term(s)
    end do

    end function cisd20_aiai
    function cisd20_aibj(a, i, b, j) 
    double precision :: cisd20_aibj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, j, a, i)



    cisd20_aibj = 0.d+0
    do s = 0, 0
    cisd20_aibj = cisd20_aibj + term(s)
    end do

    end function cisd20_aibj
    end module cisd20
    