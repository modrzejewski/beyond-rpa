module v8_eom_cc3_32_tripletp_trans

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
    
    function v8_eom_cc3_32_tripletp_trans_aibjaiajei(a, i, b, e) 
    real(F64) :: v8_eom_cc3_32_tripletp_trans_aibjaiajei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)

term(0) = term(0) * (-1.0d+0) 


    v8_eom_cc3_32_tripletp_trans_aibjaiajei = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletp_trans_aibjaiajei = v8_eom_cc3_32_tripletp_trans_aibjaiajei + term(s)
    end do

    end function v8_eom_cc3_32_tripletp_trans_aibjaiajei
    function v8_eom_cc3_32_tripletp_trans_aibjaibiam(a, i, j, m) 
    real(F64) :: v8_eom_cc3_32_tripletp_trans_aibjaibiam   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    v8_eom_cc3_32_tripletp_trans_aibjaibiam = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletp_trans_aibjaibiam = v8_eom_cc3_32_tripletp_trans_aibjaibiam + term(s)
    end do

    end function v8_eom_cc3_32_tripletp_trans_aibjaibiam
    function v8_eom_cc3_32_tripletp_trans_aibjaiblai(a, i, j, l) 
    real(F64) :: v8_eom_cc3_32_tripletp_trans_aibjaiblai   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = term(0) * (-1.0d+0) 


    v8_eom_cc3_32_tripletp_trans_aibjaiblai = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletp_trans_aibjaiblai = v8_eom_cc3_32_tripletp_trans_aibjaiblai + term(s)
    end do

    end function v8_eom_cc3_32_tripletp_trans_aibjaiblai
    function v8_eom_cc3_32_tripletp_trans_aibjaibjam(a, i, m) 
    real(F64) :: v8_eom_cc3_32_tripletp_trans_aibjaibjam   
    integer, intent(in) :: a, i, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)

term(0) = term(0) * (-1.0d+0) 


    v8_eom_cc3_32_tripletp_trans_aibjaibjam = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletp_trans_aibjaibjam = v8_eom_cc3_32_tripletp_trans_aibjaibjam + term(s)
    end do

    end function v8_eom_cc3_32_tripletp_trans_aibjaibjam
    function v8_eom_cc3_32_tripletp_trans_aibjaiblaj(a, i, l) 
    real(F64) :: v8_eom_cc3_32_tripletp_trans_aibjaiblaj   
    integer, intent(in) :: a, i, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)



    v8_eom_cc3_32_tripletp_trans_aibjaiblaj = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletp_trans_aibjaiblaj = v8_eom_cc3_32_tripletp_trans_aibjaiblaj + term(s)
    end do

    end function v8_eom_cc3_32_tripletp_trans_aibjaiblaj
    function v8_eom_cc3_32_tripletp_trans_aibjaidjai(a, i, b, d) 
    real(F64) :: v8_eom_cc3_32_tripletp_trans_aibjaidjai   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    v8_eom_cc3_32_tripletp_trans_aibjaidjai = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletp_trans_aibjaidjai = v8_eom_cc3_32_tripletp_trans_aibjaidjai + term(s)
    end do

    end function v8_eom_cc3_32_tripletp_trans_aibjaidjai
    function v8_eom_cc3_32_tripletp_trans_aibjaibjei(a, i, e) 
    real(F64) :: v8_eom_cc3_32_tripletp_trans_aibjaibjei   
    integer, intent(in) :: a, i, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)



    v8_eom_cc3_32_tripletp_trans_aibjaibjei = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletp_trans_aibjaibjei = v8_eom_cc3_32_tripletp_trans_aibjaibjei + term(s)
    end do

    end function v8_eom_cc3_32_tripletp_trans_aibjaibjei
    function v8_eom_cc3_32_tripletp_trans_aibjaidjbi(a, i, d) 
    real(F64) :: v8_eom_cc3_32_tripletp_trans_aibjaidjbi   
    integer, intent(in) :: a, i, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)

term(0) = term(0) * (-1.0d+0) 


    v8_eom_cc3_32_tripletp_trans_aibjaidjbi = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletp_trans_aibjaidjbi = v8_eom_cc3_32_tripletp_trans_aibjaidjbi + term(s)
    end do

    end function v8_eom_cc3_32_tripletp_trans_aibjaidjbi
    function v8_eom_cc3_32_tripletp_trans_aibjaibjai(a, i, b, j) 
    real(F64) :: v8_eom_cc3_32_tripletp_trans_aibjaibjai   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvvvo(a, a, a, i)
term(2) = term(2) + tvooo(a, i, i, i)
term(3) = term(3) + tvvvo(b, b, a, i)

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    v8_eom_cc3_32_tripletp_trans_aibjaibjai = 0.d+0
    do s = 0, 3
    v8_eom_cc3_32_tripletp_trans_aibjaibjai = v8_eom_cc3_32_tripletp_trans_aibjaibjai + term(s)
    end do

    end function v8_eom_cc3_32_tripletp_trans_aibjaibjai
end module v8_eom_cc3_32_tripletp_trans
