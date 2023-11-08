module v3_eom_cc3_32_tripletp_trans

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
    
    function v3_eom_cc3_32_tripletp_trans_aibickciek(a, i, b, e) 
    real(F64) :: v3_eom_cc3_32_tripletp_trans_aibickciek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletp_trans_aibickciek = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletp_trans_aibickciek = v3_eom_cc3_32_tripletp_trans_aibickciek + term(s)
    end do

    end function v3_eom_cc3_32_tripletp_trans_aibickciek
    function v3_eom_cc3_32_tripletp_trans_aibickbicm(a, i, k, m) 
    real(F64) :: v3_eom_cc3_32_tripletp_trans_aibickbicm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletp_trans_aibickbicm = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletp_trans_aibickbicm = v3_eom_cc3_32_tripletp_trans_aibickbicm + term(s)
    end do

    end function v3_eom_cc3_32_tripletp_trans_aibickbicm
    function v3_eom_cc3_32_tripletp_trans_aibickblci(a, i, k, l) 
    real(F64) :: v3_eom_cc3_32_tripletp_trans_aibickblci   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)



    v3_eom_cc3_32_tripletp_trans_aibickblci = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletp_trans_aibickblci = v3_eom_cc3_32_tripletp_trans_aibickblci + term(s)
    end do

    end function v3_eom_cc3_32_tripletp_trans_aibickblci
    function v3_eom_cc3_32_tripletp_trans_aibickbkcm(a, i, m) 
    real(F64) :: v3_eom_cc3_32_tripletp_trans_aibickbkcm   
    integer, intent(in) :: a, i, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)



    v3_eom_cc3_32_tripletp_trans_aibickbkcm = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletp_trans_aibickbkcm = v3_eom_cc3_32_tripletp_trans_aibickbkcm + term(s)
    end do

    end function v3_eom_cc3_32_tripletp_trans_aibickbkcm
    function v3_eom_cc3_32_tripletp_trans_aibickblck(a, i, l) 
    real(F64) :: v3_eom_cc3_32_tripletp_trans_aibickblck   
    integer, intent(in) :: a, i, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletp_trans_aibickblck = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletp_trans_aibickblck = v3_eom_cc3_32_tripletp_trans_aibickblck + term(s)
    end do

    end function v3_eom_cc3_32_tripletp_trans_aibickblck
    function v3_eom_cc3_32_tripletp_trans_aibickdick(a, i, b, d) 
    real(F64) :: v3_eom_cc3_32_tripletp_trans_aibickdick   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    v3_eom_cc3_32_tripletp_trans_aibickdick = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletp_trans_aibickdick = v3_eom_cc3_32_tripletp_trans_aibickdick + term(s)
    end do

    end function v3_eom_cc3_32_tripletp_trans_aibickdick
    function v3_eom_cc3_32_tripletp_trans_aibickbiek(a, i, c, e) 
    real(F64) :: v3_eom_cc3_32_tripletp_trans_aibickbiek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    v3_eom_cc3_32_tripletp_trans_aibickbiek = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletp_trans_aibickbiek = v3_eom_cc3_32_tripletp_trans_aibickbiek + term(s)
    end do

    end function v3_eom_cc3_32_tripletp_trans_aibickbiek
    function v3_eom_cc3_32_tripletp_trans_aibickdibk(a, i, c, d) 
    real(F64) :: v3_eom_cc3_32_tripletp_trans_aibickdibk   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletp_trans_aibickdibk = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletp_trans_aibickdibk = v3_eom_cc3_32_tripletp_trans_aibickdibk + term(s)
    end do

    end function v3_eom_cc3_32_tripletp_trans_aibickdibk
    function v3_eom_cc3_32_tripletp_trans_aibickaick(a, i, b) 
    real(F64) :: v3_eom_cc3_32_tripletp_trans_aibickaick   
    integer, intent(in) :: a, i, b 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, a, i)



    v3_eom_cc3_32_tripletp_trans_aibickaick = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletp_trans_aibickaick = v3_eom_cc3_32_tripletp_trans_aibickaick + term(s)
    end do

    end function v3_eom_cc3_32_tripletp_trans_aibickaick
    function v3_eom_cc3_32_tripletp_trans_aibickaibk(a, i, c) 
    real(F64) :: v3_eom_cc3_32_tripletp_trans_aibickaibk   
    integer, intent(in) :: a, i, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, a, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletp_trans_aibickaibk = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletp_trans_aibickaibk = v3_eom_cc3_32_tripletp_trans_aibickaibk + term(s)
    end do

    end function v3_eom_cc3_32_tripletp_trans_aibickaibk
    function v3_eom_cc3_32_tripletp_trans_aibickciak(a, i, b) 
    real(F64) :: v3_eom_cc3_32_tripletp_trans_aibickciak   
    integer, intent(in) :: a, i, b 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, a, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletp_trans_aibickciak = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletp_trans_aibickciak = v3_eom_cc3_32_tripletp_trans_aibickciak + term(s)
    end do

    end function v3_eom_cc3_32_tripletp_trans_aibickciak
    function v3_eom_cc3_32_tripletp_trans_aibickbiak(a, i, c) 
    real(F64) :: v3_eom_cc3_32_tripletp_trans_aibickbiak   
    integer, intent(in) :: a, i, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, a, i)



    v3_eom_cc3_32_tripletp_trans_aibickbiak = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletp_trans_aibickbiak = v3_eom_cc3_32_tripletp_trans_aibickbiak + term(s)
    end do

    end function v3_eom_cc3_32_tripletp_trans_aibickbiak
    function v3_eom_cc3_32_tripletp_trans_aibickbick(a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletp_trans_aibickbick   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvooo(a, i, i, i)
term(2) = term(2) + tvvvo(b, b, a, i)
term(3) = term(3) + tvvvo(c, c, a, i)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 


    v3_eom_cc3_32_tripletp_trans_aibickbick = 0.d+0
    do s = 0, 3
    v3_eom_cc3_32_tripletp_trans_aibickbick = v3_eom_cc3_32_tripletp_trans_aibickbick + term(s)
    end do

    end function v3_eom_cc3_32_tripletp_trans_aibickbick
    end module v3_eom_cc3_32_tripletp_trans
    