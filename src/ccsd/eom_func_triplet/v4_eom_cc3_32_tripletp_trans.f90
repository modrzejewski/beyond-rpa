module v4_eom_cc3_32_tripletp_trans

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
    
    function v4_eom_cc3_32_tripletp_trans_aibjcicjei(a, i, b, e) 
    real(F64) :: v4_eom_cc3_32_tripletp_trans_aibjcicjei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletp_trans_aibjcicjei = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletp_trans_aibjcicjei = v4_eom_cc3_32_tripletp_trans_aibjcicjei + term(s)
    end do

    end function v4_eom_cc3_32_tripletp_trans_aibjcicjei
    function v4_eom_cc3_32_tripletp_trans_aibjcibicm(a, i, j, m) 
    real(F64) :: v4_eom_cc3_32_tripletp_trans_aibjcibicm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    v4_eom_cc3_32_tripletp_trans_aibjcibicm = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletp_trans_aibjcibicm = v4_eom_cc3_32_tripletp_trans_aibjcibicm + term(s)
    end do

    end function v4_eom_cc3_32_tripletp_trans_aibjcibicm
    function v4_eom_cc3_32_tripletp_trans_aibjciblci(a, i, j, l) 
    real(F64) :: v4_eom_cc3_32_tripletp_trans_aibjciblci   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletp_trans_aibjciblci = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletp_trans_aibjciblci = v4_eom_cc3_32_tripletp_trans_aibjciblci + term(s)
    end do

    end function v4_eom_cc3_32_tripletp_trans_aibjciblci
    function v4_eom_cc3_32_tripletp_trans_aibjcibjcm(a, i, m) 
    real(F64) :: v4_eom_cc3_32_tripletp_trans_aibjcibjcm   
    integer, intent(in) :: a, i, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletp_trans_aibjcibjcm = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletp_trans_aibjcibjcm = v4_eom_cc3_32_tripletp_trans_aibjcibjcm + term(s)
    end do

    end function v4_eom_cc3_32_tripletp_trans_aibjcibjcm
    function v4_eom_cc3_32_tripletp_trans_aibjciblcj(a, i, l) 
    real(F64) :: v4_eom_cc3_32_tripletp_trans_aibjciblcj   
    integer, intent(in) :: a, i, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)



    v4_eom_cc3_32_tripletp_trans_aibjciblcj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletp_trans_aibjciblcj = v4_eom_cc3_32_tripletp_trans_aibjciblcj + term(s)
    end do

    end function v4_eom_cc3_32_tripletp_trans_aibjciblcj
    function v4_eom_cc3_32_tripletp_trans_aibjcidjci(a, i, b, d) 
    real(F64) :: v4_eom_cc3_32_tripletp_trans_aibjcidjci   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    v4_eom_cc3_32_tripletp_trans_aibjcidjci = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletp_trans_aibjcidjci = v4_eom_cc3_32_tripletp_trans_aibjcidjci + term(s)
    end do

    end function v4_eom_cc3_32_tripletp_trans_aibjcidjci
    function v4_eom_cc3_32_tripletp_trans_aibjcibjei(a, i, c, e) 
    real(F64) :: v4_eom_cc3_32_tripletp_trans_aibjcibjei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    v4_eom_cc3_32_tripletp_trans_aibjcibjei = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletp_trans_aibjcibjei = v4_eom_cc3_32_tripletp_trans_aibjcibjei + term(s)
    end do

    end function v4_eom_cc3_32_tripletp_trans_aibjcibjei
    function v4_eom_cc3_32_tripletp_trans_aibjcidjbi(a, i, c, d) 
    real(F64) :: v4_eom_cc3_32_tripletp_trans_aibjcidjbi   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletp_trans_aibjcidjbi = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletp_trans_aibjcidjbi = v4_eom_cc3_32_tripletp_trans_aibjcidjbi + term(s)
    end do

    end function v4_eom_cc3_32_tripletp_trans_aibjcidjbi
    function v4_eom_cc3_32_tripletp_trans_aibjciajci(a, i, b) 
    real(F64) :: v4_eom_cc3_32_tripletp_trans_aibjciajci   
    integer, intent(in) :: a, i, b 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, a, i)



    v4_eom_cc3_32_tripletp_trans_aibjciajci = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletp_trans_aibjciajci = v4_eom_cc3_32_tripletp_trans_aibjciajci + term(s)
    end do

    end function v4_eom_cc3_32_tripletp_trans_aibjciajci
    function v4_eom_cc3_32_tripletp_trans_aibjciajbi(a, i, c) 
    real(F64) :: v4_eom_cc3_32_tripletp_trans_aibjciajbi   
    integer, intent(in) :: a, i, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, a, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletp_trans_aibjciajbi = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletp_trans_aibjciajbi = v4_eom_cc3_32_tripletp_trans_aibjciajbi + term(s)
    end do

    end function v4_eom_cc3_32_tripletp_trans_aibjciajbi
    function v4_eom_cc3_32_tripletp_trans_aibjcicjai(a, i, b) 
    real(F64) :: v4_eom_cc3_32_tripletp_trans_aibjcicjai   
    integer, intent(in) :: a, i, b 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, a, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletp_trans_aibjcicjai = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletp_trans_aibjcicjai = v4_eom_cc3_32_tripletp_trans_aibjcicjai + term(s)
    end do

    end function v4_eom_cc3_32_tripletp_trans_aibjcicjai
    function v4_eom_cc3_32_tripletp_trans_aibjcibjai(a, i, c) 
    real(F64) :: v4_eom_cc3_32_tripletp_trans_aibjcibjai   
    integer, intent(in) :: a, i, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, a, i)



    v4_eom_cc3_32_tripletp_trans_aibjcibjai = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletp_trans_aibjcibjai = v4_eom_cc3_32_tripletp_trans_aibjcibjai + term(s)
    end do

    end function v4_eom_cc3_32_tripletp_trans_aibjcibjai
    function v4_eom_cc3_32_tripletp_trans_aibjcibjci(a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletp_trans_aibjcibjci   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, i, i, i)
term(2) = term(2) + tvvvo(b, b, a, i)
term(3) = term(3) + tvvvo(c, c, a, i)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 


    v4_eom_cc3_32_tripletp_trans_aibjcibjci = 0.d+0
    do s = 0, 3
    v4_eom_cc3_32_tripletp_trans_aibjcibjci = v4_eom_cc3_32_tripletp_trans_aibjcibjci + term(s)
    end do

    end function v4_eom_cc3_32_tripletp_trans_aibjcibjci
    end module v4_eom_cc3_32_tripletp_trans
    