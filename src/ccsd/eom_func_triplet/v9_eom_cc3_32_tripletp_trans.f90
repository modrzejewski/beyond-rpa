module v9_eom_cc3_32_tripletp_trans

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
    
    function v9_eom_cc3_32_tripletp_trans_aibjckcjek(a, i, b, e) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckcjek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)

term(0) = term(0) * (-0.9999999999999994d+0) 


    v9_eom_cc3_32_tripletp_trans_aibjckcjek = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckcjek = v9_eom_cc3_32_tripletp_trans_aibjckcjek + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckcjek
    function v9_eom_cc3_32_tripletp_trans_aibjckbjcm(a, i, k, m) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckbjcm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = term(0) * (-0.9999999999999994d+0) 


    v9_eom_cc3_32_tripletp_trans_aibjckbjcm = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckbjcm = v9_eom_cc3_32_tripletp_trans_aibjckbjcm + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckbjcm
    function v9_eom_cc3_32_tripletp_trans_aibjckblcj(a, i, k, l) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckblcj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)



    v9_eom_cc3_32_tripletp_trans_aibjckblcj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckblcj = v9_eom_cc3_32_tripletp_trans_aibjckblcj + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckblcj
    function v9_eom_cc3_32_tripletp_trans_aibjckbkcm(a, i, j, m) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckbkcm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    v9_eom_cc3_32_tripletp_trans_aibjckbkcm = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckbkcm = v9_eom_cc3_32_tripletp_trans_aibjckbkcm + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckbkcm
    function v9_eom_cc3_32_tripletp_trans_aibjckblck(a, i, j, l) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckblck   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = term(0) * (-0.9999999999999994d+0) 


    v9_eom_cc3_32_tripletp_trans_aibjckblck = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckblck = v9_eom_cc3_32_tripletp_trans_aibjckblck + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckblck
    function v9_eom_cc3_32_tripletp_trans_aibjckdjck(a, i, b, d) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckdjck   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    v9_eom_cc3_32_tripletp_trans_aibjckdjck = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckdjck = v9_eom_cc3_32_tripletp_trans_aibjckdjck + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckdjck
    function v9_eom_cc3_32_tripletp_trans_aibjckbjek(a, i, c, e) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckbjek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    v9_eom_cc3_32_tripletp_trans_aibjckbjek = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckbjek = v9_eom_cc3_32_tripletp_trans_aibjckbjek + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckbjek
    function v9_eom_cc3_32_tripletp_trans_aibjckdjbk(a, i, c, d) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckdjbk   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = term(0) * (-0.9999999999999994d+0) 


    v9_eom_cc3_32_tripletp_trans_aibjckdjbk = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckdjbk = v9_eom_cc3_32_tripletp_trans_aibjckdjbk + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckdjbk
    function v9_eom_cc3_32_tripletp_trans_aibjckajck(a, i, b) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckajck   
    integer, intent(in) :: a, i, b 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, a, i)



    v9_eom_cc3_32_tripletp_trans_aibjckajck = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckajck = v9_eom_cc3_32_tripletp_trans_aibjckajck + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckajck
    function v9_eom_cc3_32_tripletp_trans_aibjckajbk(a, i, c) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckajbk   
    integer, intent(in) :: a, i, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, a, i)

term(0) = term(0) * (-0.9999999999999994d+0) 


    v9_eom_cc3_32_tripletp_trans_aibjckajbk = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckajbk = v9_eom_cc3_32_tripletp_trans_aibjckajbk + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckajbk
    function v9_eom_cc3_32_tripletp_trans_aibjckcjak(a, i, b) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckcjak   
    integer, intent(in) :: a, i, b 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, a, i)

term(0) = term(0) * (-0.9999999999999994d+0) 


    v9_eom_cc3_32_tripletp_trans_aibjckcjak = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckcjak = v9_eom_cc3_32_tripletp_trans_aibjckcjak + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckcjak
    function v9_eom_cc3_32_tripletp_trans_aibjckbjak(a, i, c) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckbjak   
    integer, intent(in) :: a, i, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, a, i)



    v9_eom_cc3_32_tripletp_trans_aibjckbjak = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckbjak = v9_eom_cc3_32_tripletp_trans_aibjckbjak + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckbjak
    function v9_eom_cc3_32_tripletp_trans_aibjckbicj(a, i, k) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckbicj   
    integer, intent(in) :: a, i, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)



    v9_eom_cc3_32_tripletp_trans_aibjckbicj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckbicj = v9_eom_cc3_32_tripletp_trans_aibjckbicj + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckbicj
    function v9_eom_cc3_32_tripletp_trans_aibjckbick(a, i, j) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckbick   
    integer, intent(in) :: a, i, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)

term(0) = term(0) * (-0.9999999999999994d+0) 


    v9_eom_cc3_32_tripletp_trans_aibjckbick = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckbick = v9_eom_cc3_32_tripletp_trans_aibjckbick + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckbick
    function v9_eom_cc3_32_tripletp_trans_aibjckbjci(a, i, k) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckbjci   
    integer, intent(in) :: a, i, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)

term(0) = term(0) * (-0.9999999999999994d+0) 


    v9_eom_cc3_32_tripletp_trans_aibjckbjci = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckbjci = v9_eom_cc3_32_tripletp_trans_aibjckbjci + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckbjci
    function v9_eom_cc3_32_tripletp_trans_aibjckbkci(a, i, j) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckbkci   
    integer, intent(in) :: a, i, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)



    v9_eom_cc3_32_tripletp_trans_aibjckbkci = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletp_trans_aibjckbkci = v9_eom_cc3_32_tripletp_trans_aibjckbkci + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckbkci
    function v9_eom_cc3_32_tripletp_trans_aibjckbjck(a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletp_trans_aibjckbjck   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, i, k, k)
term(2) = term(2) + tvvvo(b, b, a, i)
term(3) = term(3) + tvvvo(c, c, a, i)

term(0) = term(0) * (-0.9999999999999994d+0) 
term(1) = term(1) * (-0.9999999999999994d+0) 


    v9_eom_cc3_32_tripletp_trans_aibjckbjck = 0.d+0
    do s = 0, 3
    v9_eom_cc3_32_tripletp_trans_aibjckbjck = v9_eom_cc3_32_tripletp_trans_aibjckbjck + term(s)
    end do

    end function v9_eom_cc3_32_tripletp_trans_aibjckbjck
    end module v9_eom_cc3_32_tripletp_trans
    