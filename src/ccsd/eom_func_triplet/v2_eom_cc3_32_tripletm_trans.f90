module v2_eom_cc3_32_tripletm_trans

    use ccsd_transformed_integrals                                                                                                                                   
    use t1_transformed_int                                                                                                                   
                                                                                                                                   
    use basis                                                                                                                           
    use arithmetic                                                                                                                           
                                                                        
    use cc3_intermediates 
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2017-01-20 15:07:29 UTC.
    !
    contains
    
    function v2_eom_cc3_32_tripletm_trans_aibjakbkam(a, i, j, m) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbkam   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, i)



    v2_eom_cc3_32_tripletm_trans_aibjakbkam = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbkam = v2_eom_cc3_32_tripletm_trans_aibjakbkam + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbkam
    function v2_eom_cc3_32_tripletm_trans_aibjakblai(a, j, k, l) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakblai   
    integer, intent(in) :: a, j, k, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, k)
term(1) = term(1) + tvooo(a, k, l, j)

term(1) = term(1) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakblai = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_tripletm_trans_aibjakblai = v2_eom_cc3_32_tripletm_trans_aibjakblai + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakblai
    function v2_eom_cc3_32_tripletm_trans_aibjakbjam(a, i, k, m) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbjam   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, m, i)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakbjam = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbjam = v2_eom_cc3_32_tripletm_trans_aibjakbjam + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbjam
    function v2_eom_cc3_32_tripletm_trans_aibjakbkei(a, j, e) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbkei   
    integer, intent(in) :: a, j, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, j)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakbkei = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbkei = v2_eom_cc3_32_tripletm_trans_aibjakbkei + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbkei
    function v2_eom_cc3_32_tripletm_trans_aibjakbjei(a, k, e) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbjei   
    integer, intent(in) :: a, k, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, k)



    v2_eom_cc3_32_tripletm_trans_aibjakbjei = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbjei = v2_eom_cc3_32_tripletm_trans_aibjakbjei + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbjei
    function v2_eom_cc3_32_tripletm_trans_aibjakdibk(a, j, d) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakdibk   
    integer, intent(in) :: a, j, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, j)



    v2_eom_cc3_32_tripletm_trans_aibjakdibk = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakdibk = v2_eom_cc3_32_tripletm_trans_aibjakdibk + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakdibk
    function v2_eom_cc3_32_tripletm_trans_aibjakdibj(a, k, d) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakdibj   
    integer, intent(in) :: a, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, k)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakdibj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakdibj = v2_eom_cc3_32_tripletm_trans_aibjakdibj + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakdibj
    function v2_eom_cc3_32_tripletm_trans_aibjakalak(i, b, j, l) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakalak   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, i)



    v2_eom_cc3_32_tripletm_trans_aibjakalak = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakalak = v2_eom_cc3_32_tripletm_trans_aibjakalak + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakalak
    function v2_eom_cc3_32_tripletm_trans_aibjakakam(i, b, j, m) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakakam   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, m, i)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakakam = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakakam = v2_eom_cc3_32_tripletm_trans_aibjakakam + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakakam
    function v2_eom_cc3_32_tripletm_trans_aibjakalai(b, j, k, l) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakalai   
    integer, intent(in) :: b, j, k, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, j)
term(1) = term(1) + tvooo(b, j, l, k)

term(1) = term(1) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakalai = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_tripletm_trans_aibjakalai = v2_eom_cc3_32_tripletm_trans_aibjakalai + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakalai
    function v2_eom_cc3_32_tripletm_trans_aibjakaiam(b, j, k, m) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakaiam   
    integer, intent(in) :: b, j, k, m 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, m, j)
term(1) = term(1) + tvooo(b, j, m, k)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakaiam = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_tripletm_trans_aibjakaiam = v2_eom_cc3_32_tripletm_trans_aibjakaiam + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakaiam
    function v2_eom_cc3_32_tripletm_trans_aibjakalaj(i, b, k, l) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakalaj   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, i)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakalaj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakalaj = v2_eom_cc3_32_tripletm_trans_aibjakalaj + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakalaj
    function v2_eom_cc3_32_tripletm_trans_aibjakajam(i, b, k, m) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakajam   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, m, i)



    v2_eom_cc3_32_tripletm_trans_aibjakajam = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakajam = v2_eom_cc3_32_tripletm_trans_aibjakajam + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakajam
    function v2_eom_cc3_32_tripletm_trans_aibjakaiek(a, b, j, e) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakaiek   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, j)
term(1) = term(1) + tvvvo(a, e, b, j)

term(1) = term(1) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakaiek = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_tripletm_trans_aibjakaiek = v2_eom_cc3_32_tripletm_trans_aibjakaiek + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakaiek
    function v2_eom_cc3_32_tripletm_trans_aibjakakei(a, b, j, e) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakakei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, j)



    v2_eom_cc3_32_tripletm_trans_aibjakakei = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakakei = v2_eom_cc3_32_tripletm_trans_aibjakakei + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakakei
    function v2_eom_cc3_32_tripletm_trans_aibjakajei(a, b, k, e) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakajei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, k)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakajei = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakajei = v2_eom_cc3_32_tripletm_trans_aibjakajei + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakajei
    function v2_eom_cc3_32_tripletm_trans_aibjakaiej(a, b, k, e) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakaiej   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, k)
term(1) = term(1) + tvvvo(b, e, a, k)

term(1) = term(1) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakaiej = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_tripletm_trans_aibjakaiej = v2_eom_cc3_32_tripletm_trans_aibjakaiej + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakaiej
    function v2_eom_cc3_32_tripletm_trans_aibjakdiak(a, b, j, d) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakdiak   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, j)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakdiak = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakdiak = v2_eom_cc3_32_tripletm_trans_aibjakdiak + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakdiak
    function v2_eom_cc3_32_tripletm_trans_aibjakdkai(a, b, j, d) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakdkai   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, j)
term(1) = term(1) + tvvvo(a, d, b, j)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakdkai = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_tripletm_trans_aibjakdkai = v2_eom_cc3_32_tripletm_trans_aibjakdkai + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakdkai
    function v2_eom_cc3_32_tripletm_trans_aibjakdjai(a, b, k, d) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakdjai   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, k)
term(1) = term(1) + tvvvo(a, d, b, k)

term(1) = term(1) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakdjai = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_tripletm_trans_aibjakdjai = v2_eom_cc3_32_tripletm_trans_aibjakdjai + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakdjai
    function v2_eom_cc3_32_tripletm_trans_aibjakdiaj(a, b, k, d) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakdiaj   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, k)



    v2_eom_cc3_32_tripletm_trans_aibjakdiaj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakdiaj = v2_eom_cc3_32_tripletm_trans_aibjakdiaj + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakdiaj
    function v2_eom_cc3_32_tripletm_trans_aibjakbibk(a, b, j) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbibk   
    integer, intent(in) :: a, b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, a, j)



    v2_eom_cc3_32_tripletm_trans_aibjakbibk = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbibk = v2_eom_cc3_32_tripletm_trans_aibjakbibk + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbibk
    function v2_eom_cc3_32_tripletm_trans_aibjakbkbi(a, b, j) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbkbi   
    integer, intent(in) :: a, b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, a, j)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakbkbi = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbkbi = v2_eom_cc3_32_tripletm_trans_aibjakbkbi + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbkbi
    function v2_eom_cc3_32_tripletm_trans_aibjakbjbi(a, b, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbjbi   
    integer, intent(in) :: a, b, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, a, k)



    v2_eom_cc3_32_tripletm_trans_aibjakbjbi = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbjbi = v2_eom_cc3_32_tripletm_trans_aibjakbjbi + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbjbi
    function v2_eom_cc3_32_tripletm_trans_aibjakbibj(a, b, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbibj   
    integer, intent(in) :: a, b, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, a, k)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakbibj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbibj = v2_eom_cc3_32_tripletm_trans_aibjakbibj + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbibj
    function v2_eom_cc3_32_tripletm_trans_aibjakbkak(a, i, j, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbkak   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, k, i)



    v2_eom_cc3_32_tripletm_trans_aibjakbkak = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbkak = v2_eom_cc3_32_tripletm_trans_aibjakbkak + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbkak
    function v2_eom_cc3_32_tripletm_trans_aibjakbiak(a, b, j) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbiak   
    integer, intent(in) :: a, b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, j)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakbiak = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbiak = v2_eom_cc3_32_tripletm_trans_aibjakbiak + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbiak
    function v2_eom_cc3_32_tripletm_trans_aibjakbjak(a, i, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbjak   
    integer, intent(in) :: a, i, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, k, i)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakbjak = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbjak = v2_eom_cc3_32_tripletm_trans_aibjakbjak + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbjak
    function v2_eom_cc3_32_tripletm_trans_aibjakbkai(a, i, b, j, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbkai   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, k, k)
term(1) = term(1) + tvooo(a, k, k, j)
term(2) = term(2) + tvooo(a, j, i, i)
term(3) = term(3) + tvvvo(a, a, a, j)
term(4) = term(4) + tvvvo(b, b, a, j)
term(5) = term(5) + tvvvo(a, b, b, j)

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakbkai = 0.d+0
    do s = 0, 5
    v2_eom_cc3_32_tripletm_trans_aibjakbkai = v2_eom_cc3_32_tripletm_trans_aibjakbkai + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbkai
    function v2_eom_cc3_32_tripletm_trans_aibjakbkaj(a, i, j) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbkaj   
    integer, intent(in) :: a, i, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)



    v2_eom_cc3_32_tripletm_trans_aibjakbkaj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbkaj = v2_eom_cc3_32_tripletm_trans_aibjakbkaj + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbkaj
    function v2_eom_cc3_32_tripletm_trans_aibjakbiai(a, i, j, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbiai   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, k)
term(1) = term(1) + tvooo(a, k, i, j)

term(1) = term(1) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakbiai = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_tripletm_trans_aibjakbiai = v2_eom_cc3_32_tripletm_trans_aibjakbiai + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbiai
    function v2_eom_cc3_32_tripletm_trans_aibjakbjai(a, i, b, j, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbjai   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, k)
term(1) = term(1) + tvooo(a, k, j, j)
term(2) = term(2) + tvooo(a, k, i, i)
term(3) = term(3) + tvvvo(a, a, a, k)
term(4) = term(4) + tvvvo(b, b, a, k)
term(5) = term(5) + tvvvo(a, b, b, k)

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakbjai = 0.d+0
    do s = 0, 5
    v2_eom_cc3_32_tripletm_trans_aibjakbjai = v2_eom_cc3_32_tripletm_trans_aibjakbjai + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbjai
    function v2_eom_cc3_32_tripletm_trans_aibjakbiaj(a, b, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbiaj   
    integer, intent(in) :: a, b, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, k)



    v2_eom_cc3_32_tripletm_trans_aibjakbiaj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbiaj = v2_eom_cc3_32_tripletm_trans_aibjakbiaj + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbiaj
    function v2_eom_cc3_32_tripletm_trans_aibjakbjaj(a, i, j, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakbjaj   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, j, i)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakbjaj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletm_trans_aibjakbjaj = v2_eom_cc3_32_tripletm_trans_aibjakbjaj + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakbjaj
    function v2_eom_cc3_32_tripletm_trans_aibjakaiak(a, i, b, j, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakaiak   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, j)
term(1) = term(1) + tvooo(b, j, i, i)
term(2) = term(2) + tvooo(b, j, k, k)
term(3) = term(3) + tvvvo(b, a, a, j)
term(4) = term(4) + tvvvo(a, a, b, j)

term(0) = term(0) * (-1.0d+0) 
term(4) = term(4) * (-2.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakaiak = 0.d+0
    do s = 0, 4
    v2_eom_cc3_32_tripletm_trans_aibjakaiak = v2_eom_cc3_32_tripletm_trans_aibjakaiak + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakaiak
    function v2_eom_cc3_32_tripletm_trans_aibjakajak(i, b, j, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakajak   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)
term(1) = term(1) + tvooo(b, j, j, i)



    v2_eom_cc3_32_tripletm_trans_aibjakajak = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_tripletm_trans_aibjakajak = v2_eom_cc3_32_tripletm_trans_aibjakajak + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakajak
    function v2_eom_cc3_32_tripletm_trans_aibjakakai(a, i, b, j, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakakai   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, j)
term(1) = term(1) + tvooo(b, j, k, k)
term(2) = term(2) + tvooo(b, j, i, i)
term(3) = term(3) + tvvvo(b, a, a, j)
term(4) = term(4) + tvvvo(a, a, b, j)

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (2.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakakai = 0.d+0
    do s = 0, 4
    v2_eom_cc3_32_tripletm_trans_aibjakakai = v2_eom_cc3_32_tripletm_trans_aibjakakai + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakakai
    function v2_eom_cc3_32_tripletm_trans_aibjakajai(a, i, b, j, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakajai   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, j, j)
term(1) = term(1) + tvooo(b, k, i, i)
term(2) = term(2) + tvooo(b, j, j, k)
term(3) = term(3) + tvvvo(b, a, a, k)
term(4) = term(4) + tvvvo(a, a, b, k)

term(2) = term(2) * (-1.0d+0) 
term(4) = term(4) * (-2.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakajai = 0.d+0
    do s = 0, 4
    v2_eom_cc3_32_tripletm_trans_aibjakajai = v2_eom_cc3_32_tripletm_trans_aibjakajai + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakajai
    function v2_eom_cc3_32_tripletm_trans_aibjakaiaj(a, i, b, j, k) 
    real(F64) :: v2_eom_cc3_32_tripletm_trans_aibjakaiaj   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, i, i)
term(1) = term(1) + tvooo(b, k, j, j)
term(2) = term(2) + tvooo(b, j, j, k)
term(3) = term(3) + tvvvo(a, a, b, k)
term(4) = term(4) + tvvvo(b, a, a, k)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v2_eom_cc3_32_tripletm_trans_aibjakaiaj = 0.d+0
    do s = 0, 4
    v2_eom_cc3_32_tripletm_trans_aibjakaiaj = v2_eom_cc3_32_tripletm_trans_aibjakaiaj + term(s)
    end do

    end function v2_eom_cc3_32_tripletm_trans_aibjakaiaj
    end module v2_eom_cc3_32_tripletm_trans
    