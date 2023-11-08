module v7_eom_cc3_32_tripletp_trans

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
    ! File generated automatically on 2017-01-20 15:12:39 UTC.
    !
    contains
    
    function v7_eom_cc3_32_tripletp_trans_aibiakaiek(a, i, b, e) 
    real(F64) :: v7_eom_cc3_32_tripletp_trans_aibiakaiek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)

term(0) = term(0) * (-1.0d+0) 


    v7_eom_cc3_32_tripletp_trans_aibiakaiek = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletp_trans_aibiakaiek = v7_eom_cc3_32_tripletp_trans_aibiakaiek + term(s)
    end do

    end function v7_eom_cc3_32_tripletp_trans_aibiakaiek
    function v7_eom_cc3_32_tripletp_trans_aibiakbiam(a, i, k, m) 
    real(F64) :: v7_eom_cc3_32_tripletp_trans_aibiakbiam   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = term(0) * (-1.0d+0) 


    v7_eom_cc3_32_tripletp_trans_aibiakbiam = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletp_trans_aibiakbiam = v7_eom_cc3_32_tripletp_trans_aibiakbiam + term(s)
    end do

    end function v7_eom_cc3_32_tripletp_trans_aibiakbiam
    function v7_eom_cc3_32_tripletp_trans_aibiakblai(a, i, k, l) 
    real(F64) :: v7_eom_cc3_32_tripletp_trans_aibiakblai   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)



    v7_eom_cc3_32_tripletp_trans_aibiakblai = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletp_trans_aibiakblai = v7_eom_cc3_32_tripletp_trans_aibiakblai + term(s)
    end do

    end function v7_eom_cc3_32_tripletp_trans_aibiakblai
    function v7_eom_cc3_32_tripletp_trans_aibiakbkam(a, i, m) 
    real(F64) :: v7_eom_cc3_32_tripletp_trans_aibiakbkam   
    integer, intent(in) :: a, i, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)



    v7_eom_cc3_32_tripletp_trans_aibiakbkam = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletp_trans_aibiakbkam = v7_eom_cc3_32_tripletp_trans_aibiakbkam + term(s)
    end do

    end function v7_eom_cc3_32_tripletp_trans_aibiakbkam
    function v7_eom_cc3_32_tripletp_trans_aibiakblak(a, i, l) 
    real(F64) :: v7_eom_cc3_32_tripletp_trans_aibiakblak   
    integer, intent(in) :: a, i, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)

term(0) = term(0) * (-1.0d+0) 


    v7_eom_cc3_32_tripletp_trans_aibiakblak = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletp_trans_aibiakblak = v7_eom_cc3_32_tripletp_trans_aibiakblak + term(s)
    end do

    end function v7_eom_cc3_32_tripletp_trans_aibiakblak
    function v7_eom_cc3_32_tripletp_trans_aibiakdiak(a, i, b, d) 
    real(F64) :: v7_eom_cc3_32_tripletp_trans_aibiakdiak   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    v7_eom_cc3_32_tripletp_trans_aibiakdiak = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletp_trans_aibiakdiak = v7_eom_cc3_32_tripletp_trans_aibiakdiak + term(s)
    end do

    end function v7_eom_cc3_32_tripletp_trans_aibiakdiak
    function v7_eom_cc3_32_tripletp_trans_aibiakbiek(a, i, e) 
    real(F64) :: v7_eom_cc3_32_tripletp_trans_aibiakbiek   
    integer, intent(in) :: a, i, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)



    v7_eom_cc3_32_tripletp_trans_aibiakbiek = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletp_trans_aibiakbiek = v7_eom_cc3_32_tripletp_trans_aibiakbiek + term(s)
    end do

    end function v7_eom_cc3_32_tripletp_trans_aibiakbiek
    function v7_eom_cc3_32_tripletp_trans_aibiakdibk(a, i, d) 
    real(F64) :: v7_eom_cc3_32_tripletp_trans_aibiakdibk   
    integer, intent(in) :: a, i, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)

term(0) = term(0) * (-1.0d+0) 


    v7_eom_cc3_32_tripletp_trans_aibiakdibk = 0.d+0
    do s = 0, 0
    v7_eom_cc3_32_tripletp_trans_aibiakdibk = v7_eom_cc3_32_tripletp_trans_aibiakdibk + term(s)
    end do

    end function v7_eom_cc3_32_tripletp_trans_aibiakdibk
    function v7_eom_cc3_32_tripletp_trans_aibiakbiak(a, i, b, k) 
    real(F64) :: v7_eom_cc3_32_tripletp_trans_aibiakbiak   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvooo(a, i, i, i)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvvvo(b, b, a, i)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 


    v7_eom_cc3_32_tripletp_trans_aibiakbiak = 0.d+0
    do s = 0, 3
    v7_eom_cc3_32_tripletp_trans_aibiakbiak = v7_eom_cc3_32_tripletp_trans_aibiakbiak + term(s)
    end do

    end function v7_eom_cc3_32_tripletp_trans_aibiakbiak
    end module v7_eom_cc3_32_tripletp_trans
    