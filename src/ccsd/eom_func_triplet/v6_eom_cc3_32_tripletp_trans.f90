module v6_eom_cc3_32_tripletp_trans

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
    
    function v6_eom_cc3_32_tripletp_trans_aiajciaicm(a, i, j, m) 
    real(F64) :: v6_eom_cc3_32_tripletp_trans_aiajciaicm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    v6_eom_cc3_32_tripletp_trans_aiajciaicm = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletp_trans_aiajciaicm = v6_eom_cc3_32_tripletp_trans_aiajciaicm + term(s)
    end do

    end function v6_eom_cc3_32_tripletp_trans_aiajciaicm
    function v6_eom_cc3_32_tripletp_trans_aiajcialci(a, i, j, l) 
    real(F64) :: v6_eom_cc3_32_tripletp_trans_aiajcialci   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletp_trans_aiajcialci = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletp_trans_aiajcialci = v6_eom_cc3_32_tripletp_trans_aiajcialci + term(s)
    end do

    end function v6_eom_cc3_32_tripletp_trans_aiajcialci
    function v6_eom_cc3_32_tripletp_trans_aiajciajcm(a, i, m) 
    real(F64) :: v6_eom_cc3_32_tripletp_trans_aiajciajcm   
    integer, intent(in) :: a, i, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletp_trans_aiajciajcm = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletp_trans_aiajciajcm = v6_eom_cc3_32_tripletp_trans_aiajciajcm + term(s)
    end do

    end function v6_eom_cc3_32_tripletp_trans_aiajciajcm
    function v6_eom_cc3_32_tripletp_trans_aiajcialcj(a, i, l) 
    real(F64) :: v6_eom_cc3_32_tripletp_trans_aiajcialcj   
    integer, intent(in) :: a, i, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)



    v6_eom_cc3_32_tripletp_trans_aiajcialcj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletp_trans_aiajcialcj = v6_eom_cc3_32_tripletp_trans_aiajcialcj + term(s)
    end do

    end function v6_eom_cc3_32_tripletp_trans_aiajcialcj
    function v6_eom_cc3_32_tripletp_trans_aiajciajei(a, i, c, e) 
    real(F64) :: v6_eom_cc3_32_tripletp_trans_aiajciajei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    v6_eom_cc3_32_tripletp_trans_aiajciajei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletp_trans_aiajciajei = v6_eom_cc3_32_tripletp_trans_aiajciajei + term(s)
    end do

    end function v6_eom_cc3_32_tripletp_trans_aiajciajei
    function v6_eom_cc3_32_tripletp_trans_aiajcidjai(a, i, c, d) 
    real(F64) :: v6_eom_cc3_32_tripletp_trans_aiajcidjai   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletp_trans_aiajcidjai = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletp_trans_aiajcidjai = v6_eom_cc3_32_tripletp_trans_aiajcidjai + term(s)
    end do

    end function v6_eom_cc3_32_tripletp_trans_aiajcidjai
    function v6_eom_cc3_32_tripletp_trans_aiajcicjei(a, i, e) 
    real(F64) :: v6_eom_cc3_32_tripletp_trans_aiajcicjei   
    integer, intent(in) :: a, i, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletp_trans_aiajcicjei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletp_trans_aiajcicjei = v6_eom_cc3_32_tripletp_trans_aiajcicjei + term(s)
    end do

    end function v6_eom_cc3_32_tripletp_trans_aiajcicjei
    function v6_eom_cc3_32_tripletp_trans_aiajcidjci(a, i, d) 
    real(F64) :: v6_eom_cc3_32_tripletp_trans_aiajcidjci   
    integer, intent(in) :: a, i, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)



    v6_eom_cc3_32_tripletp_trans_aiajcidjci = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletp_trans_aiajcidjci = v6_eom_cc3_32_tripletp_trans_aiajcidjci + term(s)
    end do

    end function v6_eom_cc3_32_tripletp_trans_aiajcidjci
    function v6_eom_cc3_32_tripletp_trans_aiajciajci(a, i, j, c) 
    real(F64) :: v6_eom_cc3_32_tripletp_trans_aiajciajci   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvvvo(a, a, a, i)
term(2) = term(2) + tvooo(a, i, i, i)
term(3) = term(3) + tvvvo(c, c, a, i)

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    v6_eom_cc3_32_tripletp_trans_aiajciajci = 0.d+0
    do s = 0, 3
    v6_eom_cc3_32_tripletp_trans_aiajciajci = v6_eom_cc3_32_tripletp_trans_aiajciajci + term(s)
    end do

    end function v6_eom_cc3_32_tripletp_trans_aiajciajci
    end module v6_eom_cc3_32_tripletp_trans
    