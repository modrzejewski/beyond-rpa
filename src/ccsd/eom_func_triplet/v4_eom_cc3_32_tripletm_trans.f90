module v4_eom_cc3_32_tripletm_trans

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
    
    function v4_eom_cc3_32_tripletm_trans_aibjciblai(i, j, c, l) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciblai   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, j)
term(1) = term(1) + tvooo(c, j, l, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciblai = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_tripletm_trans_aibjciblai = v4_eom_cc3_32_tripletm_trans_aibjciblai + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciblai
    function v4_eom_cc3_32_tripletm_trans_aibjcibiam(i, j, c, m) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcibiam   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, i)



    v4_eom_cc3_32_tripletm_trans_aibjcibiam = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcibiam = v4_eom_cc3_32_tripletm_trans_aibjcibiam + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcibiam
    function v4_eom_cc3_32_tripletm_trans_aibjcibjam(i, c, m) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcibjam   
    integer, intent(in) :: i, c, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcibjam = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcibjam = v4_eom_cc3_32_tripletm_trans_aibjcibjam + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcibjam
    function v4_eom_cc3_32_tripletm_trans_aibjcibiei(a, j, c, e) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcibiei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, j)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcibiei = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcibiei = v4_eom_cc3_32_tripletm_trans_aibjcibiei + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcibiei
    function v4_eom_cc3_32_tripletm_trans_aibjcibjei(a, i, c, e) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcibjei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, i)



    v4_eom_cc3_32_tripletm_trans_aibjcibjei = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcibjei = v4_eom_cc3_32_tripletm_trans_aibjcibjei + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcibjei
    function v4_eom_cc3_32_tripletm_trans_aibjcialbi(i, j, c, l) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcialbi   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcialbi = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcialbi = v4_eom_cc3_32_tripletm_trans_aibjcialbi + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcialbi
    function v4_eom_cc3_32_tripletm_trans_aibjciaibm(i, j, c, m) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciaibm   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, i)
term(1) = term(1) + tvooo(c, i, m, j)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciaibm = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_tripletm_trans_aibjciaibm = v4_eom_cc3_32_tripletm_trans_aibjciaibm + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciaibm
    function v4_eom_cc3_32_tripletm_trans_aibjcialbj(i, c, l) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcialbj   
    integer, intent(in) :: i, c, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, i)



    v4_eom_cc3_32_tripletm_trans_aibjcialbj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcialbj = v4_eom_cc3_32_tripletm_trans_aibjcialbj + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcialbj
    function v4_eom_cc3_32_tripletm_trans_aibjcidibi(a, j, c, d) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcidibi   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, j)



    v4_eom_cc3_32_tripletm_trans_aibjcidibi = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcidibi = v4_eom_cc3_32_tripletm_trans_aibjcidibi + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcidibi
    function v4_eom_cc3_32_tripletm_trans_aibjcidibj(a, i, c, d) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcidibj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcidibj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcidibj = v4_eom_cc3_32_tripletm_trans_aibjcidibj + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcidibj
    function v4_eom_cc3_32_tripletm_trans_aibjciclai(i, b, j, l) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciclai   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, j)
term(1) = term(1) + tvooo(b, j, l, i)

term(1) = term(1) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciclai = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_tripletm_trans_aibjciclai = v4_eom_cc3_32_tripletm_trans_aibjciclai + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciclai
    function v4_eom_cc3_32_tripletm_trans_aibjciciam(i, b, j, m) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciciam   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, m, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciciam = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjciciam = v4_eom_cc3_32_tripletm_trans_aibjciciam + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciciam
    function v4_eom_cc3_32_tripletm_trans_aibjcicjam(i, b, m) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcicjam   
    integer, intent(in) :: i, b, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, i)



    v4_eom_cc3_32_tripletm_trans_aibjcicjam = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcicjam = v4_eom_cc3_32_tripletm_trans_aibjcicjam + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcicjam
    function v4_eom_cc3_32_tripletm_trans_aibjciciei(a, b, j, e) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciciei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, j)



    v4_eom_cc3_32_tripletm_trans_aibjciciei = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjciciei = v4_eom_cc3_32_tripletm_trans_aibjciciei + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciciei
    function v4_eom_cc3_32_tripletm_trans_aibjcicjei(a, i, b, e) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcicjei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcicjei = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcicjei = v4_eom_cc3_32_tripletm_trans_aibjcicjei + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcicjei
    function v4_eom_cc3_32_tripletm_trans_aibjcialci(i, b, j, l) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcialci   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, i)



    v4_eom_cc3_32_tripletm_trans_aibjcialci = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcialci = v4_eom_cc3_32_tripletm_trans_aibjcialci + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcialci
    function v4_eom_cc3_32_tripletm_trans_aibjciaicm(i, b, j, m) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciaicm   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, j)
term(1) = term(1) + tvooo(b, j, m, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciaicm = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_tripletm_trans_aibjciaicm = v4_eom_cc3_32_tripletm_trans_aibjciaicm + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciaicm
    function v4_eom_cc3_32_tripletm_trans_aibjcialcj(i, b, l) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcialcj   
    integer, intent(in) :: i, b, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcialcj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcialcj = v4_eom_cc3_32_tripletm_trans_aibjcialcj + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcialcj
    function v4_eom_cc3_32_tripletm_trans_aibjcidici(a, b, j, d) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcidici   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, j)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcidici = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcidici = v4_eom_cc3_32_tripletm_trans_aibjcidici + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcidici
    function v4_eom_cc3_32_tripletm_trans_aibjcidicj(a, i, b, d) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcidicj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, i)



    v4_eom_cc3_32_tripletm_trans_aibjcidicj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcidicj = v4_eom_cc3_32_tripletm_trans_aibjcidicj + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcidicj
    function v4_eom_cc3_32_tripletm_trans_aibjciaiei(b, j, c, e) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciaiei   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, c, j)
term(1) = term(1) + tvvvo(c, e, b, j)

term(1) = term(1) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciaiei = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_tripletm_trans_aibjciaiei = v4_eom_cc3_32_tripletm_trans_aibjciaiei + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciaiei
    function v4_eom_cc3_32_tripletm_trans_aibjciaiej(i, b, c, e) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciaiej   
    integer, intent(in) :: i, b, c, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, i)
term(1) = term(1) + tvvvo(b, e, c, i)

term(1) = term(1) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciaiej = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_tripletm_trans_aibjciaiej = v4_eom_cc3_32_tripletm_trans_aibjciaiej + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciaiej
    function v4_eom_cc3_32_tripletm_trans_aibjcidiai(b, j, c, d) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcidiai   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, c, j)
term(1) = term(1) + tvvvo(c, d, b, j)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcidiai = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_tripletm_trans_aibjcidiai = v4_eom_cc3_32_tripletm_trans_aibjcidiai + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcidiai
    function v4_eom_cc3_32_tripletm_trans_aibjcidjai(i, b, c, d) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcidjai   
    integer, intent(in) :: i, b, c, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, i)
term(1) = term(1) + tvvvo(b, d, c, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcidjai = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_tripletm_trans_aibjcidjai = v4_eom_cc3_32_tripletm_trans_aibjcidjai + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcidjai
    function v4_eom_cc3_32_tripletm_trans_aibjcibjbi(a, i, b, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcibjbi   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, i)



    v4_eom_cc3_32_tripletm_trans_aibjcibjbi = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcibjbi = v4_eom_cc3_32_tripletm_trans_aibjcibjbi + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcibjbi
    function v4_eom_cc3_32_tripletm_trans_aibjcibici(a, b, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcibici   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, c, j)
term(1) = term(1) + tvvvo(a, b, b, j)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcibici = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_tripletm_trans_aibjcibici = v4_eom_cc3_32_tripletm_trans_aibjcibici + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcibici
    function v4_eom_cc3_32_tripletm_trans_aibjcibjci(a, i, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcibjci   
    integer, intent(in) :: a, i, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, c, i)



    v4_eom_cc3_32_tripletm_trans_aibjcibjci = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcibjci = v4_eom_cc3_32_tripletm_trans_aibjcibjci + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcibjci
    function v4_eom_cc3_32_tripletm_trans_aibjcibicj(a, i, b) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcibicj   
    integer, intent(in) :: a, i, b 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, i)



    v4_eom_cc3_32_tripletm_trans_aibjcibicj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcibicj = v4_eom_cc3_32_tripletm_trans_aibjcibicj + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcibicj
    function v4_eom_cc3_32_tripletm_trans_aibjcibiai(a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcibiai   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, i, j)
term(1) = term(1) + tvooo(c, j, i, i)
term(2) = term(2) + tvvvo(b, b, c, j)
term(3) = term(3) + tvvvo(a, a, c, j)
term(4) = term(4) + tvvvo(c, b, b, j)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcibiai = 0.d+0
    do s = 0, 4
    v4_eom_cc3_32_tripletm_trans_aibjcibiai = v4_eom_cc3_32_tripletm_trans_aibjcibiai + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcibiai
    function v4_eom_cc3_32_tripletm_trans_aibjcibjai(a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcibjai   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, j, j)
term(1) = term(1) + tvooo(c, j, j, i)
term(2) = term(2) + tvooo(c, i, i, i)
term(3) = term(3) + tvvvo(c, b, b, i)
term(4) = term(4) + tvvvo(b, b, c, i)
term(5) = term(5) + tvvvo(a, a, c, i)

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcibjai = 0.d+0
    do s = 0, 5
    v4_eom_cc3_32_tripletm_trans_aibjcibjai = v4_eom_cc3_32_tripletm_trans_aibjcibjai + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcibjai
    function v4_eom_cc3_32_tripletm_trans_aibjcibiaj(i, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcibiaj   
    integer, intent(in) :: i, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)



    v4_eom_cc3_32_tripletm_trans_aibjcibiaj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcibiaj = v4_eom_cc3_32_tripletm_trans_aibjcibiaj + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcibiaj
    function v4_eom_cc3_32_tripletm_trans_aibjcibjaj(i, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcibjaj   
    integer, intent(in) :: i, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, j, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcibjaj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcibjaj = v4_eom_cc3_32_tripletm_trans_aibjcibjaj + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcibjaj
    function v4_eom_cc3_32_tripletm_trans_aibjciaibi(a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciaibi   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, i, i)
term(1) = term(1) + tvooo(c, i, i, j)
term(2) = term(2) + tvvvo(a, a, c, j)
term(3) = term(3) + tvvvo(b, b, c, j)
term(4) = term(4) + tvvvo(c, b, b, j)

term(0) = term(0) * (-2.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciaibi = 0.d+0
    do s = 0, 4
    v4_eom_cc3_32_tripletm_trans_aibjciaibi = v4_eom_cc3_32_tripletm_trans_aibjciaibi + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciaibi
    function v4_eom_cc3_32_tripletm_trans_aibjciajbi(i, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciajbi   
    integer, intent(in) :: i, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciajbi = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjciajbi = v4_eom_cc3_32_tripletm_trans_aibjciajbi + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciajbi
    function v4_eom_cc3_32_tripletm_trans_aibjciaibj(a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciaibj   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)
term(1) = term(1) + tvooo(c, i, j, j)
term(2) = term(2) + tvooo(c, i, i, i)
term(3) = term(3) + tvvvo(c, b, b, i)
term(4) = term(4) + tvvvo(b, b, c, i)
term(5) = term(5) + tvvvo(a, a, c, i)

term(0) = term(0) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciaibj = 0.d+0
    do s = 0, 5
    v4_eom_cc3_32_tripletm_trans_aibjciaibj = v4_eom_cc3_32_tripletm_trans_aibjciaibj + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciaibj
    function v4_eom_cc3_32_tripletm_trans_aibjciajbj(i, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciajbj   
    integer, intent(in) :: i, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, j, i)



    v4_eom_cc3_32_tripletm_trans_aibjciajbj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjciajbj = v4_eom_cc3_32_tripletm_trans_aibjciajbj + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciajbj
    function v4_eom_cc3_32_tripletm_trans_aibjcicjci(a, i, b, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcicjci   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcicjci = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcicjci = v4_eom_cc3_32_tripletm_trans_aibjcicjci + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcicjci
    function v4_eom_cc3_32_tripletm_trans_aibjciciai(a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciciai   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, i, j)
term(1) = term(1) + tvooo(b, j, i, i)
term(2) = term(2) + tvvvo(b, c, c, j)
term(3) = term(3) + tvvvo(c, c, b, j)
term(4) = term(4) + tvvvo(a, a, b, j)

term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciciai = 0.d+0
    do s = 0, 4
    v4_eom_cc3_32_tripletm_trans_aibjciciai = v4_eom_cc3_32_tripletm_trans_aibjciciai + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciciai
    function v4_eom_cc3_32_tripletm_trans_aibjcicjai(a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcicjai   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, j)
term(1) = term(1) + tvooo(b, j, j, i)
term(2) = term(2) + tvooo(b, i, i, i)
term(3) = term(3) + tvvvo(c, c, b, i)
term(4) = term(4) + tvvvo(b, c, c, i)
term(5) = term(5) + tvvvo(a, a, b, i)

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjcicjai = 0.d+0
    do s = 0, 5
    v4_eom_cc3_32_tripletm_trans_aibjcicjai = v4_eom_cc3_32_tripletm_trans_aibjcicjai + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcicjai
    function v4_eom_cc3_32_tripletm_trans_aibjciciaj(i, b, j) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciciaj   
    integer, intent(in) :: i, b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciciaj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjciciaj = v4_eom_cc3_32_tripletm_trans_aibjciciaj + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciciaj
    function v4_eom_cc3_32_tripletm_trans_aibjcicjaj(i, b, j) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjcicjaj   
    integer, intent(in) :: i, b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, i)



    v4_eom_cc3_32_tripletm_trans_aibjcicjaj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjcicjaj = v4_eom_cc3_32_tripletm_trans_aibjcicjaj + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjcicjaj
    function v4_eom_cc3_32_tripletm_trans_aibjciaici(a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciaici   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, i, j)
term(1) = term(1) + tvooo(b, j, i, i)
term(2) = term(2) + tvvvo(b, c, c, j)
term(3) = term(3) + tvvvo(a, a, b, j)
term(4) = term(4) + tvvvo(c, c, b, j)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciaici = 0.d+0
    do s = 0, 4
    v4_eom_cc3_32_tripletm_trans_aibjciaici = v4_eom_cc3_32_tripletm_trans_aibjciaici + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciaici
    function v4_eom_cc3_32_tripletm_trans_aibjciajci(i, b, j) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciajci   
    integer, intent(in) :: i, b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)



    v4_eom_cc3_32_tripletm_trans_aibjciajci = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjciajci = v4_eom_cc3_32_tripletm_trans_aibjciajci + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciajci
    function v4_eom_cc3_32_tripletm_trans_aibjciaicj(a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciaicj   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, j)
term(1) = term(1) + tvooo(b, j, j, i)
term(2) = term(2) + tvooo(b, i, i, i)
term(3) = term(3) + tvvvo(a, a, b, i)
term(4) = term(4) + tvvvo(c, c, b, i)
term(5) = term(5) + tvvvo(b, c, c, i)

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciaicj = 0.d+0
    do s = 0, 5
    v4_eom_cc3_32_tripletm_trans_aibjciaicj = v4_eom_cc3_32_tripletm_trans_aibjciaicj + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciaicj
    function v4_eom_cc3_32_tripletm_trans_aibjciajcj(i, b, j) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciajcj   
    integer, intent(in) :: i, b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciajcj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_tripletm_trans_aibjciajcj = v4_eom_cc3_32_tripletm_trans_aibjciajcj + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciajcj
    function v4_eom_cc3_32_tripletm_trans_aibjciajai(a, i, b, c) 
    real(F64) :: v4_eom_cc3_32_tripletm_trans_aibjciajai   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, i)
term(1) = term(1) + tvvvo(b, a, c, i)

term(0) = term(0) * (-1.0d+0) 


    v4_eom_cc3_32_tripletm_trans_aibjciajai = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_tripletm_trans_aibjciajai = v4_eom_cc3_32_tripletm_trans_aibjciajai + term(s)
    end do

    end function v4_eom_cc3_32_tripletm_trans_aibjciajai
    end module v4_eom_cc3_32_tripletm_trans
    