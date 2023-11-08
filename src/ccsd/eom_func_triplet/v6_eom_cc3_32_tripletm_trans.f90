module v6_eom_cc3_32_tripletm_trans

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
    
    function v6_eom_cc3_32_tripletm_trans_aiajcialai(i, j, c, l) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajcialai   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, j)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajcialai = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajcialai = v6_eom_cc3_32_tripletm_trans_aiajcialai + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajcialai
    function v6_eom_cc3_32_tripletm_trans_aiajciaiam(i, j, c, m) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajciaiam   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, j)



    v6_eom_cc3_32_tripletm_trans_aiajciaiam = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajciaiam = v6_eom_cc3_32_tripletm_trans_aiajciaiam + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajciaiam
    function v6_eom_cc3_32_tripletm_trans_aiajcialaj(i, c, l) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajcialaj   
    integer, intent(in) :: i, c, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, i)



    v6_eom_cc3_32_tripletm_trans_aiajcialaj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajcialaj = v6_eom_cc3_32_tripletm_trans_aiajcialaj + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajcialaj
    function v6_eom_cc3_32_tripletm_trans_aiajciajam(i, c, m) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajciajam   
    integer, intent(in) :: i, c, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, i)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajciajam = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajciajam = v6_eom_cc3_32_tripletm_trans_aiajciajam + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajciajam
    function v6_eom_cc3_32_tripletm_trans_aiajcialci(a, i, j, l) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajcialci   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, i)



    v6_eom_cc3_32_tripletm_trans_aiajcialci = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajcialci = v6_eom_cc3_32_tripletm_trans_aiajcialci + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajcialci
    function v6_eom_cc3_32_tripletm_trans_aiajciaicm(a, i, j, m) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajciaicm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, i)
term(1) = term(1) + tvooo(a, i, m, j)

term(1) = term(1) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajciaicm = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_tripletm_trans_aiajciaicm = v6_eom_cc3_32_tripletm_trans_aiajciaicm + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajciaicm
    function v6_eom_cc3_32_tripletm_trans_aiajcialcj(a, i, l) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajcialcj   
    integer, intent(in) :: a, i, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajcialcj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajcialcj = v6_eom_cc3_32_tripletm_trans_aiajcialcj + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajcialcj
    function v6_eom_cc3_32_tripletm_trans_aiajciaiei(a, j, c, e) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajciaiei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, j)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajciaiei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajciaiei = v6_eom_cc3_32_tripletm_trans_aiajciaiei + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajciaiei
    function v6_eom_cc3_32_tripletm_trans_aiajciajei(a, i, c, e) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajciajei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, i)



    v6_eom_cc3_32_tripletm_trans_aiajciajei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajciajei = v6_eom_cc3_32_tripletm_trans_aiajciajei + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajciajei
    function v6_eom_cc3_32_tripletm_trans_aiajciaiej(a, i, c, e) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajciaiej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)
term(1) = term(1) + tvvvo(a, e, c, i)

term(1) = term(1) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajciaiej = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_tripletm_trans_aiajciaiej = v6_eom_cc3_32_tripletm_trans_aiajciaiej + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajciaiej
    function v6_eom_cc3_32_tripletm_trans_aiajcidiai(a, j, c, d) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajcidiai   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, j)



    v6_eom_cc3_32_tripletm_trans_aiajcidiai = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajcidiai = v6_eom_cc3_32_tripletm_trans_aiajcidiai + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajcidiai
    function v6_eom_cc3_32_tripletm_trans_aiajcidjai(a, i, c, d) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajcidjai   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)
term(1) = term(1) + tvvvo(a, d, c, i)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajcidjai = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_tripletm_trans_aiajcidjai = v6_eom_cc3_32_tripletm_trans_aiajcidjai + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajcidjai
    function v6_eom_cc3_32_tripletm_trans_aiajcidiaj(a, i, c, d) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajcidiaj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, i)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajcidiaj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajcidiaj = v6_eom_cc3_32_tripletm_trans_aiajcidiaj + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajcidiaj
    function v6_eom_cc3_32_tripletm_trans_aiajciciei(a, j, e) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajciciei   
    integer, intent(in) :: a, j, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, j)



    v6_eom_cc3_32_tripletm_trans_aiajciciei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajciciei = v6_eom_cc3_32_tripletm_trans_aiajciciei + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajciciei
    function v6_eom_cc3_32_tripletm_trans_aiajcicjei(a, i, e) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajcicjei   
    integer, intent(in) :: a, i, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajcicjei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajcicjei = v6_eom_cc3_32_tripletm_trans_aiajcicjei + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajcicjei
    function v6_eom_cc3_32_tripletm_trans_aiajcidici(a, j, d) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajcidici   
    integer, intent(in) :: a, j, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, j)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajcidici = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajcidici = v6_eom_cc3_32_tripletm_trans_aiajcidici + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajcidici
    function v6_eom_cc3_32_tripletm_trans_aiajcidicj(a, i, d) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajcidicj   
    integer, intent(in) :: a, i, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)



    v6_eom_cc3_32_tripletm_trans_aiajcidicj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajcidicj = v6_eom_cc3_32_tripletm_trans_aiajcidicj + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajcidicj
    function v6_eom_cc3_32_tripletm_trans_aiajciajai(a, i, j, c) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajciajai   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, j, j)
term(1) = term(1) + tvooo(c, i, i, i)
term(2) = term(2) + tvvvo(c, a, a, i)
term(3) = term(3) + tvvvo(a, a, c, i)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (2.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajciajai = 0.d+0
    do s = 0, 3
    v6_eom_cc3_32_tripletm_trans_aiajciajai = v6_eom_cc3_32_tripletm_trans_aiajciajai + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajciajai
    function v6_eom_cc3_32_tripletm_trans_aiajciaici(a, i, j, c) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajciaici   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, i)
term(1) = term(1) + tvooo(a, i, i, j)
term(2) = term(2) + tvvvo(a, a, a, j)
term(3) = term(3) + tvvvo(c, c, a, j)

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajciaici = 0.d+0
    do s = 0, 3
    v6_eom_cc3_32_tripletm_trans_aiajciaici = v6_eom_cc3_32_tripletm_trans_aiajciaici + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajciaici
    function v6_eom_cc3_32_tripletm_trans_aiajciajci(a, i, j, c) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajciajci   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)
term(1) = term(1) + tvvvo(a, c, c, i)



    v6_eom_cc3_32_tripletm_trans_aiajciajci = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_tripletm_trans_aiajciajci = v6_eom_cc3_32_tripletm_trans_aiajciajci + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajciajci
    function v6_eom_cc3_32_tripletm_trans_aiajciaicj(a, i, j, c) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajciaicj   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)
term(1) = term(1) + tvooo(a, i, j, j)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvooo(a, i, i, i)
term(4) = term(4) + tvvvo(c, c, a, i)
term(5) = term(5) + tvvvo(a, c, c, i)

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajciaicj = 0.d+0
    do s = 0, 5
    v6_eom_cc3_32_tripletm_trans_aiajciaicj = v6_eom_cc3_32_tripletm_trans_aiajciaicj + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajciaicj
    function v6_eom_cc3_32_tripletm_trans_aiajciajcj(a, i, j) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajciajcj   
    integer, intent(in) :: a, i, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, i)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajciajcj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajciajcj = v6_eom_cc3_32_tripletm_trans_aiajciajcj + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajciajcj
    function v6_eom_cc3_32_tripletm_trans_aiajcicjci(a, i, c) 
    real(F64) :: v6_eom_cc3_32_tripletm_trans_aiajcicjci   
    integer, intent(in) :: a, i, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, a, i)

term(0) = term(0) * (-1.0d+0) 


    v6_eom_cc3_32_tripletm_trans_aiajcicjci = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_tripletm_trans_aiajcicjci = v6_eom_cc3_32_tripletm_trans_aiajcicjci + term(s)
    end do

    end function v6_eom_cc3_32_tripletm_trans_aiajcicjci
    end module v6_eom_cc3_32_tripletm_trans
    