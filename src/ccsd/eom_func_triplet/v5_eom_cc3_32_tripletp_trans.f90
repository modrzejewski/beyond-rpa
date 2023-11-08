module v5_eom_cc3_32_tripletp_trans

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
    
    function v5_eom_cc3_32_tripletp_trans_aiaickaicm(a, i, k, m) 
    real(F64) :: v5_eom_cc3_32_tripletp_trans_aiaickaicm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletp_trans_aiaickaicm = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletp_trans_aiaickaicm = v5_eom_cc3_32_tripletp_trans_aiaickaicm + term(s)
    end do

    end function v5_eom_cc3_32_tripletp_trans_aiaickaicm
    function v5_eom_cc3_32_tripletp_trans_aiaickalci(a, i, k, l) 
    real(F64) :: v5_eom_cc3_32_tripletp_trans_aiaickalci   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)



    v5_eom_cc3_32_tripletp_trans_aiaickalci = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletp_trans_aiaickalci = v5_eom_cc3_32_tripletp_trans_aiaickalci + term(s)
    end do

    end function v5_eom_cc3_32_tripletp_trans_aiaickalci
    function v5_eom_cc3_32_tripletp_trans_aiaickakcm(a, i, m) 
    real(F64) :: v5_eom_cc3_32_tripletp_trans_aiaickakcm   
    integer, intent(in) :: a, i, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)



    v5_eom_cc3_32_tripletp_trans_aiaickakcm = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletp_trans_aiaickakcm = v5_eom_cc3_32_tripletp_trans_aiaickakcm + term(s)
    end do

    end function v5_eom_cc3_32_tripletp_trans_aiaickakcm
    function v5_eom_cc3_32_tripletp_trans_aiaickalck(a, i, l) 
    real(F64) :: v5_eom_cc3_32_tripletp_trans_aiaickalck   
    integer, intent(in) :: a, i, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletp_trans_aiaickalck = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletp_trans_aiaickalck = v5_eom_cc3_32_tripletp_trans_aiaickalck + term(s)
    end do

    end function v5_eom_cc3_32_tripletp_trans_aiaickalck
    function v5_eom_cc3_32_tripletp_trans_aiaickaiek(a, i, c, e) 
    real(F64) :: v5_eom_cc3_32_tripletp_trans_aiaickaiek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    v5_eom_cc3_32_tripletp_trans_aiaickaiek = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletp_trans_aiaickaiek = v5_eom_cc3_32_tripletp_trans_aiaickaiek + term(s)
    end do

    end function v5_eom_cc3_32_tripletp_trans_aiaickaiek
    function v5_eom_cc3_32_tripletp_trans_aiaickdiak(a, i, c, d) 
    real(F64) :: v5_eom_cc3_32_tripletp_trans_aiaickdiak   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletp_trans_aiaickdiak = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletp_trans_aiaickdiak = v5_eom_cc3_32_tripletp_trans_aiaickdiak + term(s)
    end do

    end function v5_eom_cc3_32_tripletp_trans_aiaickdiak
    function v5_eom_cc3_32_tripletp_trans_aiaickciek(a, i, e) 
    real(F64) :: v5_eom_cc3_32_tripletp_trans_aiaickciek   
    integer, intent(in) :: a, i, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletp_trans_aiaickciek = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletp_trans_aiaickciek = v5_eom_cc3_32_tripletp_trans_aiaickciek + term(s)
    end do

    end function v5_eom_cc3_32_tripletp_trans_aiaickciek
    function v5_eom_cc3_32_tripletp_trans_aiaickdick(a, i, d) 
    real(F64) :: v5_eom_cc3_32_tripletp_trans_aiaickdick   
    integer, intent(in) :: a, i, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)



    v5_eom_cc3_32_tripletp_trans_aiaickdick = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletp_trans_aiaickdick = v5_eom_cc3_32_tripletp_trans_aiaickdick + term(s)
    end do

    end function v5_eom_cc3_32_tripletp_trans_aiaickdick
    function v5_eom_cc3_32_tripletp_trans_aiaickaick(a, i, c, k) 
    real(F64) :: v5_eom_cc3_32_tripletp_trans_aiaickaick   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvvvo(a, a, a, i)
term(2) = term(2) + tvooo(a, i, i, i)
term(3) = term(3) + tvvvo(c, c, a, i)

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    v5_eom_cc3_32_tripletp_trans_aiaickaick = 0.d+0
    do s = 0, 3
    v5_eom_cc3_32_tripletp_trans_aiaickaick = v5_eom_cc3_32_tripletp_trans_aiaickaick + term(s)
    end do

    end function v5_eom_cc3_32_tripletp_trans_aiaickaick
    end module v5_eom_cc3_32_tripletp_trans
    