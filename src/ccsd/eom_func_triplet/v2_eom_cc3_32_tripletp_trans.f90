module v2_eom_cc3_32_tripletp_trans

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
    
    function v2_eom_cc3_32_tripletp_trans_aibjakajek(a, i, b, e) 
    real(F64) :: v2_eom_cc3_32_tripletp_trans_aibjakajek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletp_trans_aibjakajek = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletp_trans_aibjakajek = v2_eom_cc3_32_tripletp_trans_aibjakajek + term(s)
    end do

    end function v2_eom_cc3_32_tripletp_trans_aibjakajek
    function v2_eom_cc3_32_tripletp_trans_aibjakbjam(a, i, k, m) 
    real(F64) :: v2_eom_cc3_32_tripletp_trans_aibjakbjam   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletp_trans_aibjakbjam = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletp_trans_aibjakbjam = v2_eom_cc3_32_tripletp_trans_aibjakbjam + term(s)
    end do

    end function v2_eom_cc3_32_tripletp_trans_aibjakbjam
    function v2_eom_cc3_32_tripletp_trans_aibjakblaj(a, i, k, l) 
    real(F64) :: v2_eom_cc3_32_tripletp_trans_aibjakblaj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)



    v2_eom_cc3_32_tripletp_trans_aibjakblaj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletp_trans_aibjakblaj = v2_eom_cc3_32_tripletp_trans_aibjakblaj + term(s)
    end do

    end function v2_eom_cc3_32_tripletp_trans_aibjakblaj
    function v2_eom_cc3_32_tripletp_trans_aibjakbkam(a, i, j, m) 
    real(F64) :: v2_eom_cc3_32_tripletp_trans_aibjakbkam   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    v2_eom_cc3_32_tripletp_trans_aibjakbkam = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletp_trans_aibjakbkam = v2_eom_cc3_32_tripletp_trans_aibjakbkam + term(s)
    end do

    end function v2_eom_cc3_32_tripletp_trans_aibjakbkam
    function v2_eom_cc3_32_tripletp_trans_aibjakblak(a, i, j, l) 
    real(F64) :: v2_eom_cc3_32_tripletp_trans_aibjakblak   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletp_trans_aibjakblak = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletp_trans_aibjakblak = v2_eom_cc3_32_tripletp_trans_aibjakblak + term(s)
    end do

    end function v2_eom_cc3_32_tripletp_trans_aibjakblak
    function v2_eom_cc3_32_tripletp_trans_aibjakdjak(a, i, b, d) 
    real(F64) :: v2_eom_cc3_32_tripletp_trans_aibjakdjak   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    v2_eom_cc3_32_tripletp_trans_aibjakdjak = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletp_trans_aibjakdjak = v2_eom_cc3_32_tripletp_trans_aibjakdjak + term(s)
    end do

    end function v2_eom_cc3_32_tripletp_trans_aibjakdjak
    function v2_eom_cc3_32_tripletp_trans_aibjakbjek(a, i, e) 
    real(F64) :: v2_eom_cc3_32_tripletp_trans_aibjakbjek   
    integer, intent(in) :: a, i, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)



    v2_eom_cc3_32_tripletp_trans_aibjakbjek = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletp_trans_aibjakbjek = v2_eom_cc3_32_tripletp_trans_aibjakbjek + term(s)
    end do

    end function v2_eom_cc3_32_tripletp_trans_aibjakbjek
    function v2_eom_cc3_32_tripletp_trans_aibjakdjbk(a, i, d) 
    real(F64) :: v2_eom_cc3_32_tripletp_trans_aibjakdjbk   
    integer, intent(in) :: a, i, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletp_trans_aibjakdjbk = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletp_trans_aibjakdjbk = v2_eom_cc3_32_tripletp_trans_aibjakdjbk + term(s)
    end do

    end function v2_eom_cc3_32_tripletp_trans_aibjakdjbk
    function v2_eom_cc3_32_tripletp_trans_aibjakbiaj(a, i, k) 
    real(F64) :: v2_eom_cc3_32_tripletp_trans_aibjakbiaj   
    integer, intent(in) :: a, i, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)



    v2_eom_cc3_32_tripletp_trans_aibjakbiaj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletp_trans_aibjakbiaj = v2_eom_cc3_32_tripletp_trans_aibjakbiaj + term(s)
    end do

    end function v2_eom_cc3_32_tripletp_trans_aibjakbiaj
    function v2_eom_cc3_32_tripletp_trans_aibjakbiak(a, i, j) 
    real(F64) :: v2_eom_cc3_32_tripletp_trans_aibjakbiak   
    integer, intent(in) :: a, i, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletp_trans_aibjakbiak = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletp_trans_aibjakbiak = v2_eom_cc3_32_tripletp_trans_aibjakbiak + term(s)
    end do

    end function v2_eom_cc3_32_tripletp_trans_aibjakbiak
    function v2_eom_cc3_32_tripletp_trans_aibjakbjai(a, i, k) 
    real(F64) :: v2_eom_cc3_32_tripletp_trans_aibjakbjai   
    integer, intent(in) :: a, i, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)

term(0) = term(0) * (-1.0d+0) 


    v2_eom_cc3_32_tripletp_trans_aibjakbjai = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletp_trans_aibjakbjai = v2_eom_cc3_32_tripletp_trans_aibjakbjai + term(s)
    end do

    end function v2_eom_cc3_32_tripletp_trans_aibjakbjai
    function v2_eom_cc3_32_tripletp_trans_aibjakbkai(a, i, j) 
    real(F64) :: v2_eom_cc3_32_tripletp_trans_aibjakbkai   
    integer, intent(in) :: a, i, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)



    v2_eom_cc3_32_tripletp_trans_aibjakbkai = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_tripletp_trans_aibjakbkai = v2_eom_cc3_32_tripletp_trans_aibjakbkai + term(s)
    end do

    end function v2_eom_cc3_32_tripletp_trans_aibjakbkai
    function v2_eom_cc3_32_tripletp_trans_aibjakbjak(a, i, b, j, k) 
    real(F64) :: v2_eom_cc3_32_tripletp_trans_aibjakbjak   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, i, k, k)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvvvo(b, b, a, i)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 


    v2_eom_cc3_32_tripletp_trans_aibjakbjak = 0.d+0
    do s = 0, 3
    v2_eom_cc3_32_tripletp_trans_aibjakbjak = v2_eom_cc3_32_tripletp_trans_aibjakbjak + term(s)
    end do

    end function v2_eom_cc3_32_tripletp_trans_aibjakbjak
    end module v2_eom_cc3_32_tripletp_trans
    