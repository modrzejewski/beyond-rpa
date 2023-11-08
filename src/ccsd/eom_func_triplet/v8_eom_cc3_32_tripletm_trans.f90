module v8_eom_cc3_32_tripletm_trans

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
    
    function v8_eom_cc3_32_tripletm_trans_aibjaiblai(a, i, j, l) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaiblai   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, i)
term(1) = term(1) + tvooo(a, i, l, j)

term(1) = term(1) * (-1.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaiblai = 0.d+0
    do s = 0, 1
    v8_eom_cc3_32_tripletm_trans_aibjaiblai = v8_eom_cc3_32_tripletm_trans_aibjaiblai + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaiblai
    function v8_eom_cc3_32_tripletm_trans_aibjaibiam(a, i, j, m) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaibiam   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, i)



    v8_eom_cc3_32_tripletm_trans_aibjaibiam = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaibiam = v8_eom_cc3_32_tripletm_trans_aibjaibiam + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaibiam
    function v8_eom_cc3_32_tripletm_trans_aibjaibjam(a, i, m) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaibjam   
    integer, intent(in) :: a, i, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)

term(0) = term(0) * (-1.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaibjam = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaibjam = v8_eom_cc3_32_tripletm_trans_aibjaibjam + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaibjam
    function v8_eom_cc3_32_tripletm_trans_aibjaibiei(a, j, e) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaibiei   
    integer, intent(in) :: a, j, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, j)

term(0) = term(0) * (-1.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaibiei = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaibiei = v8_eom_cc3_32_tripletm_trans_aibjaibiei + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaibiei
    function v8_eom_cc3_32_tripletm_trans_aibjaibjei(a, i, e) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaibjei   
    integer, intent(in) :: a, i, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)



    v8_eom_cc3_32_tripletm_trans_aibjaibjei = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaibjei = v8_eom_cc3_32_tripletm_trans_aibjaibjei + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaibjei
    function v8_eom_cc3_32_tripletm_trans_aibjaidibi(a, j, d) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaidibi   
    integer, intent(in) :: a, j, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, j)



    v8_eom_cc3_32_tripletm_trans_aibjaidibi = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaidibi = v8_eom_cc3_32_tripletm_trans_aibjaidibi + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaidibi
    function v8_eom_cc3_32_tripletm_trans_aibjaidibj(a, i, d) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaidibj   
    integer, intent(in) :: a, i, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)

term(0) = term(0) * (-1.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaidibj = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaidibj = v8_eom_cc3_32_tripletm_trans_aibjaidibj + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaidibj
    function v8_eom_cc3_32_tripletm_trans_aibjaialai(i, b, j, l) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaialai   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, j)



    v8_eom_cc3_32_tripletm_trans_aibjaialai = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaialai = v8_eom_cc3_32_tripletm_trans_aibjaialai + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaialai
    function v8_eom_cc3_32_tripletm_trans_aibjaiaiam(i, b, j, m) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaiaiam   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, j)

term(0) = term(0) * (-1.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaiaiam = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaiaiam = v8_eom_cc3_32_tripletm_trans_aibjaiaiam + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaiaiam
    function v8_eom_cc3_32_tripletm_trans_aibjaialaj(i, b, l) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaialaj   
    integer, intent(in) :: i, b, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, i)

term(0) = term(0) * (-1.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaialaj = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaialaj = v8_eom_cc3_32_tripletm_trans_aibjaialaj + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaialaj
    function v8_eom_cc3_32_tripletm_trans_aibjaiajam(i, b, m) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaiajam   
    integer, intent(in) :: i, b, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, i)



    v8_eom_cc3_32_tripletm_trans_aibjaiajam = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaiajam = v8_eom_cc3_32_tripletm_trans_aibjaiajam + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaiajam
    function v8_eom_cc3_32_tripletm_trans_aibjaiaiei(a, b, j, e) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaiaiei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, j)



    v8_eom_cc3_32_tripletm_trans_aibjaiaiei = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaiaiei = v8_eom_cc3_32_tripletm_trans_aibjaiaiei + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaiaiei
    function v8_eom_cc3_32_tripletm_trans_aibjaiajei(a, i, b, e) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaiajei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, i)

term(0) = term(0) * (-1.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaiajei = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaiajei = v8_eom_cc3_32_tripletm_trans_aibjaiajei + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaiajei
    function v8_eom_cc3_32_tripletm_trans_aibjaiaiej(a, i, b, e) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaiaiej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, i)
term(1) = term(1) + tvvvo(b, e, a, i)

term(1) = term(1) * (-1.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaiaiej = 0.d+0
    do s = 0, 1
    v8_eom_cc3_32_tripletm_trans_aibjaiaiej = v8_eom_cc3_32_tripletm_trans_aibjaiaiej + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaiaiej
    function v8_eom_cc3_32_tripletm_trans_aibjaidiai(a, b, j, d) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaidiai   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, j)

term(0) = term(0) * (-1.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaidiai = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaidiai = v8_eom_cc3_32_tripletm_trans_aibjaidiai + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaidiai
    function v8_eom_cc3_32_tripletm_trans_aibjaidjai(a, i, b, d) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaidjai   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)
term(1) = term(1) + tvvvo(a, d, b, i)

term(1) = term(1) * (-1.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaidjai = 0.d+0
    do s = 0, 1
    v8_eom_cc3_32_tripletm_trans_aibjaidjai = v8_eom_cc3_32_tripletm_trans_aibjaidjai + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaidjai
    function v8_eom_cc3_32_tripletm_trans_aibjaidiaj(a, i, b, d) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaidiaj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, i)



    v8_eom_cc3_32_tripletm_trans_aibjaidiaj = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaidiaj = v8_eom_cc3_32_tripletm_trans_aibjaidiaj + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaidiaj
    function v8_eom_cc3_32_tripletm_trans_aibjaibjbi(a, i, b) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaibjbi   
    integer, intent(in) :: a, i, b 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, a, i)



    v8_eom_cc3_32_tripletm_trans_aibjaibjbi = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaibjbi = v8_eom_cc3_32_tripletm_trans_aibjaibjbi + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaibjbi
    function v8_eom_cc3_32_tripletm_trans_aibjaibiai(a, i, b, j) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaibiai   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, i)
term(1) = term(1) + tvooo(a, i, i, j)
term(2) = term(2) + tvvvo(a, a, a, j)
term(3) = term(3) + tvvvo(b, b, a, j)

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaibiai = 0.d+0
    do s = 0, 3
    v8_eom_cc3_32_tripletm_trans_aibjaibiai = v8_eom_cc3_32_tripletm_trans_aibjaibiai + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaibiai
    function v8_eom_cc3_32_tripletm_trans_aibjaibjai(a, i, b, j) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaibjai   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)
term(1) = term(1) + tvooo(a, i, j, j)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvooo(a, i, i, i)
term(4) = term(4) + tvvvo(b, b, a, i)
term(5) = term(5) + tvvvo(a, b, b, i)

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaibjai = 0.d+0
    do s = 0, 5
    v8_eom_cc3_32_tripletm_trans_aibjaibjai = v8_eom_cc3_32_tripletm_trans_aibjaibjai + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaibjai
    function v8_eom_cc3_32_tripletm_trans_aibjaibiaj(a, i, b, j) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaibiaj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)
term(1) = term(1) + tvvvo(a, b, b, i)



    v8_eom_cc3_32_tripletm_trans_aibjaibiaj = 0.d+0
    do s = 0, 1
    v8_eom_cc3_32_tripletm_trans_aibjaibiaj = v8_eom_cc3_32_tripletm_trans_aibjaibiaj + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaibiaj
    function v8_eom_cc3_32_tripletm_trans_aibjaibjaj(a, i, j) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaibjaj   
    integer, intent(in) :: a, i, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, i)

term(0) = term(0) * (-1.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaibjaj = 0.d+0
    do s = 0, 0
    v8_eom_cc3_32_tripletm_trans_aibjaibjaj = v8_eom_cc3_32_tripletm_trans_aibjaibjaj + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaibjaj
    function v8_eom_cc3_32_tripletm_trans_aibjaiajai(a, i, b, j) 
    real(F64) :: v8_eom_cc3_32_tripletm_trans_aibjaiajai   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, j)
term(1) = term(1) + tvooo(b, i, i, i)
term(2) = term(2) + tvvvo(b, a, a, i)
term(3) = term(3) + tvvvo(a, a, b, i)

term(3) = term(3) * (-2.0d+0) 


    v8_eom_cc3_32_tripletm_trans_aibjaiajai = 0.d+0
    do s = 0, 3
    v8_eom_cc3_32_tripletm_trans_aibjaiajai = v8_eom_cc3_32_tripletm_trans_aibjaiajai + term(s)
    end do

    end function v8_eom_cc3_32_tripletm_trans_aibjaiajai
end module v8_eom_cc3_32_tripletm_trans
