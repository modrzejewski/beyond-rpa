module v7_eom_cc3_32_tripletm_trans

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
    
    function v7_eom_cc3_32_tripletm_trans_aibiakbkam(a, i, m) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakbkam   
    integer, intent(in) :: a, i, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)



    v7_eom_cc3_32_tripletm_trans_aibiakbkam = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakbkam = v7_eom_cc3_32_tripletm_trans_aibiakbkam + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakbkam
    function v7_eom_cc3_32_tripletm_trans_aibiakblai(a, i, k, l) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakblai   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)
term(1) = term(1) + tvooo(a, k, l, i)

term(1) = term(1) * (-1.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakblai = 0.d+0
    do s = 0, 1
    v7_eom_cc3_32_tripletm_trans_aibiakblai = v7_eom_cc3_32_tripletm_trans_aibiakblai + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakblai
    function v7_eom_cc3_32_tripletm_trans_aibiakbiam(a, i, k, m) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakbiam   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, m, i)

term(0) = term(0) * (-1.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakbiam = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakbiam = v7_eom_cc3_32_tripletm_trans_aibiakbiam + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakbiam
    function v7_eom_cc3_32_tripletm_trans_aibiakbkei(a, i, e) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakbkei   
    integer, intent(in) :: a, i, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)

term(0) = term(0) * (-1.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakbkei = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakbkei = v7_eom_cc3_32_tripletm_trans_aibiakbkei + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakbkei
    function v7_eom_cc3_32_tripletm_trans_aibiakbiei(a, k, e) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakbiei   
    integer, intent(in) :: a, k, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, k)



    v7_eom_cc3_32_tripletm_trans_aibiakbiei = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakbiei = v7_eom_cc3_32_tripletm_trans_aibiakbiei + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakbiei
    function v7_eom_cc3_32_tripletm_trans_aibiakdibk(a, i, d) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakdibk   
    integer, intent(in) :: a, i, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)



    v7_eom_cc3_32_tripletm_trans_aibiakdibk = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakdibk = v7_eom_cc3_32_tripletm_trans_aibiakdibk + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakdibk
    function v7_eom_cc3_32_tripletm_trans_aibiakdibi(a, k, d) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakdibi   
    integer, intent(in) :: a, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, k)

term(0) = term(0) * (-1.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakdibi = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakdibi = v7_eom_cc3_32_tripletm_trans_aibiakdibi + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakdibi
    function v7_eom_cc3_32_tripletm_trans_aibiakalak(i, b, l) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakalak   
    integer, intent(in) :: i, b, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, i)



    v7_eom_cc3_32_tripletm_trans_aibiakalak = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakalak = v7_eom_cc3_32_tripletm_trans_aibiakalak + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakalak
    function v7_eom_cc3_32_tripletm_trans_aibiakakam(i, b, m) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakakam   
    integer, intent(in) :: i, b, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, i)

term(0) = term(0) * (-1.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakakam = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakakam = v7_eom_cc3_32_tripletm_trans_aibiakakam + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakakam
    function v7_eom_cc3_32_tripletm_trans_aibiakalai(i, b, k, l) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakalai   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, k)

term(0) = term(0) * (-1.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakalai = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakalai = v7_eom_cc3_32_tripletm_trans_aibiakalai + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakalai
    function v7_eom_cc3_32_tripletm_trans_aibiakaiam(i, b, k, m) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakaiam   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, k)



    v7_eom_cc3_32_tripletm_trans_aibiakaiam = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakaiam = v7_eom_cc3_32_tripletm_trans_aibiakaiam + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakaiam
    function v7_eom_cc3_32_tripletm_trans_aibiakaiek(a, i, b, e) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakaiek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)
term(1) = term(1) + tvvvo(a, e, b, i)

term(1) = term(1) * (-1.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakaiek = 0.d+0
    do s = 0, 1
    v7_eom_cc3_32_tripletm_trans_aibiakaiek = v7_eom_cc3_32_tripletm_trans_aibiakaiek + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakaiek
    function v7_eom_cc3_32_tripletm_trans_aibiakakei(a, i, b, e) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakakei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, i)



    v7_eom_cc3_32_tripletm_trans_aibiakakei = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakakei = v7_eom_cc3_32_tripletm_trans_aibiakakei + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakakei
    function v7_eom_cc3_32_tripletm_trans_aibiakaiei(a, b, k, e) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakaiei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, k)

term(0) = term(0) * (-1.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakaiei = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakaiei = v7_eom_cc3_32_tripletm_trans_aibiakaiei + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakaiei
    function v7_eom_cc3_32_tripletm_trans_aibiakdiak(a, i, b, d) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakdiak   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, i)

term(0) = term(0) * (-1.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakdiak = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakdiak = v7_eom_cc3_32_tripletm_trans_aibiakdiak + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakdiak
    function v7_eom_cc3_32_tripletm_trans_aibiakdkai(a, i, b, d) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakdkai   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)
term(1) = term(1) + tvvvo(a, d, b, i)

term(0) = term(0) * (-1.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakdkai = 0.d+0
    do s = 0, 1
    v7_eom_cc3_32_tripletm_trans_aibiakdkai = v7_eom_cc3_32_tripletm_trans_aibiakdkai + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakdkai
    function v7_eom_cc3_32_tripletm_trans_aibiakdiai(a, b, k, d) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakdiai   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, k)



    v7_eom_cc3_32_tripletm_trans_aibiakdiai = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakdiai = v7_eom_cc3_32_tripletm_trans_aibiakdiai + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakdiai
    function v7_eom_cc3_32_tripletm_trans_aibiakbibk(a, i, b) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakbibk   
    integer, intent(in) :: a, i, b 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, a, i)



    v7_eom_cc3_32_tripletm_trans_aibiakbibk = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakbibk = v7_eom_cc3_32_tripletm_trans_aibiakbibk + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakbibk
    function v7_eom_cc3_32_tripletm_trans_aibiakbkak(a, i, k) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakbkak   
    integer, intent(in) :: a, i, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, i)



    v7_eom_cc3_32_tripletm_trans_aibiakbkak = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletm_trans_aibiakbkak = v7_eom_cc3_32_tripletm_trans_aibiakbkak + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakbkak
    function v7_eom_cc3_32_tripletm_trans_aibiakbiak(a, i, b, k) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakbiak   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, k, i)
term(1) = term(1) + tvvvo(a, b, b, i)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakbiak = 0.d+0
    do s = 0, 1
    v7_eom_cc3_32_tripletm_trans_aibiakbiak = v7_eom_cc3_32_tripletm_trans_aibiakbiak + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakbiak
    function v7_eom_cc3_32_tripletm_trans_aibiakbkai(a, i, b, k) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakbkai   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvooo(a, k, k, i)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvooo(a, i, i, i)
term(4) = term(4) + tvvvo(b, b, a, i)
term(5) = term(5) + tvvvo(a, b, b, i)

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakbkai = 0.d+0
    do s = 0, 5
    v7_eom_cc3_32_tripletm_trans_aibiakbkai = v7_eom_cc3_32_tripletm_trans_aibiakbkai + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakbkai
    function v7_eom_cc3_32_tripletm_trans_aibiakbiai(a, i, b, k) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakbiai   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)
term(1) = term(1) + tvooo(a, k, i, i)
term(2) = term(2) + tvvvo(a, a, a, k)
term(3) = term(3) + tvvvo(b, b, a, k)

term(1) = term(1) * (-2.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakbiai = 0.d+0
    do s = 0, 3
    v7_eom_cc3_32_tripletm_trans_aibiakbiai = v7_eom_cc3_32_tripletm_trans_aibiakbiai + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakbiai
    function v7_eom_cc3_32_tripletm_trans_aibiakaiak(a, i, b, k) 
    real(F64) :: v7_eom_cc3_32_tripletm_trans_aibiakaiak   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, k, k)
term(1) = term(1) + tvooo(b, i, i, i)
term(2) = term(2) + tvvvo(a, a, b, i)
term(3) = term(3) + tvvvo(b, a, a, i)

term(2) = term(2) * (-2.0d+0) 


    v7_eom_cc3_32_tripletm_trans_aibiakaiak = 0.d+0
    do s = 0, 3
    v7_eom_cc3_32_tripletm_trans_aibiakaiak = v7_eom_cc3_32_tripletm_trans_aibiakaiak + term(s)
    end do

    end function v7_eom_cc3_32_tripletm_trans_aibiakaiak
    end module v7_eom_cc3_32_tripletm_trans
    