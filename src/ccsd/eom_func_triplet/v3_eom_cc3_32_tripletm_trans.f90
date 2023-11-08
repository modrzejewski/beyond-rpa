module v3_eom_cc3_32_tripletm_trans

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
    
    function v3_eom_cc3_32_tripletm_trans_aibickbkam(i, c, m) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickbkam   
    integer, intent(in) :: i, c, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, i)



    v3_eom_cc3_32_tripletm_trans_aibickbkam = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickbkam = v3_eom_cc3_32_tripletm_trans_aibickbkam + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickbkam
    function v3_eom_cc3_32_tripletm_trans_aibickblai(i, c, k, l) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickblai   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, k)
term(1) = term(1) + tvooo(c, k, l, i)

term(1) = term(1) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickblai = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_tripletm_trans_aibickblai = v3_eom_cc3_32_tripletm_trans_aibickblai + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickblai
    function v3_eom_cc3_32_tripletm_trans_aibickbiam(i, c, k, m) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickbiam   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, m, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickbiam = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickbiam = v3_eom_cc3_32_tripletm_trans_aibickbiam + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickbiam
    function v3_eom_cc3_32_tripletm_trans_aibickbkei(a, i, c, e) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickbkei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickbkei = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickbkei = v3_eom_cc3_32_tripletm_trans_aibickbkei + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickbkei
    function v3_eom_cc3_32_tripletm_trans_aibickbiei(a, c, k, e) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickbiei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, k)



    v3_eom_cc3_32_tripletm_trans_aibickbiei = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickbiei = v3_eom_cc3_32_tripletm_trans_aibickbiei + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickbiei
    function v3_eom_cc3_32_tripletm_trans_aibickalbk(i, c, l) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickalbk   
    integer, intent(in) :: i, c, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickalbk = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickalbk = v3_eom_cc3_32_tripletm_trans_aibickalbk + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickalbk
    function v3_eom_cc3_32_tripletm_trans_aibickalbi(i, c, k, l) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickalbi   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, l, i)



    v3_eom_cc3_32_tripletm_trans_aibickalbi = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickalbi = v3_eom_cc3_32_tripletm_trans_aibickalbi + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickalbi
    function v3_eom_cc3_32_tripletm_trans_aibickaibm(i, c, k, m) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickaibm   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, k)
term(1) = term(1) + tvooo(c, k, m, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickaibm = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_tripletm_trans_aibickaibm = v3_eom_cc3_32_tripletm_trans_aibickaibm + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickaibm
    function v3_eom_cc3_32_tripletm_trans_aibickdibk(a, i, c, d) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickdibk   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, i)



    v3_eom_cc3_32_tripletm_trans_aibickdibk = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickdibk = v3_eom_cc3_32_tripletm_trans_aibickdibk + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickdibk
    function v3_eom_cc3_32_tripletm_trans_aibickdibi(a, c, k, d) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickdibi   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, k)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickdibi = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickdibi = v3_eom_cc3_32_tripletm_trans_aibickdibi + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickdibi
    function v3_eom_cc3_32_tripletm_trans_aibickckam(i, b, m) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickckam   
    integer, intent(in) :: i, b, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickckam = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickckam = v3_eom_cc3_32_tripletm_trans_aibickckam + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickckam
    function v3_eom_cc3_32_tripletm_trans_aibickclai(i, b, k, l) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickclai   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, i)
term(1) = term(1) + tvooo(b, i, l, k)

term(1) = term(1) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickclai = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_tripletm_trans_aibickclai = v3_eom_cc3_32_tripletm_trans_aibickclai + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickclai
    function v3_eom_cc3_32_tripletm_trans_aibickciam(i, b, k, m) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickciam   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, m, i)



    v3_eom_cc3_32_tripletm_trans_aibickciam = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickciam = v3_eom_cc3_32_tripletm_trans_aibickciam + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickciam
    function v3_eom_cc3_32_tripletm_trans_aibickckei(a, i, b, e) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickckei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, i)



    v3_eom_cc3_32_tripletm_trans_aibickckei = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickckei = v3_eom_cc3_32_tripletm_trans_aibickckei + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickckei
    function v3_eom_cc3_32_tripletm_trans_aibickciei(a, b, k, e) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickciei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, k)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickciei = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickciei = v3_eom_cc3_32_tripletm_trans_aibickciei + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickciei
    function v3_eom_cc3_32_tripletm_trans_aibickalck(i, b, l) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickalck   
    integer, intent(in) :: i, b, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, i)



    v3_eom_cc3_32_tripletm_trans_aibickalck = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickalck = v3_eom_cc3_32_tripletm_trans_aibickalck + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickalck
    function v3_eom_cc3_32_tripletm_trans_aibickalci(i, b, k, l) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickalci   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickalci = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickalci = v3_eom_cc3_32_tripletm_trans_aibickalci + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickalci
    function v3_eom_cc3_32_tripletm_trans_aibickaicm(i, b, k, m) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickaicm   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, k)
term(1) = term(1) + tvooo(b, k, m, i)

term(1) = term(1) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickaicm = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_tripletm_trans_aibickaicm = v3_eom_cc3_32_tripletm_trans_aibickaicm + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickaicm
    function v3_eom_cc3_32_tripletm_trans_aibickdick(a, i, b, d) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickdick   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickdick = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickdick = v3_eom_cc3_32_tripletm_trans_aibickdick + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickdick
    function v3_eom_cc3_32_tripletm_trans_aibickdici(a, b, k, d) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickdici   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, k)



    v3_eom_cc3_32_tripletm_trans_aibickdici = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickdici = v3_eom_cc3_32_tripletm_trans_aibickdici + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickdici
    function v3_eom_cc3_32_tripletm_trans_aibickaiek(i, b, c, e) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickaiek   
    integer, intent(in) :: i, b, c, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, c, i)
term(1) = term(1) + tvvvo(c, e, b, i)

term(1) = term(1) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickaiek = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_tripletm_trans_aibickaiek = v3_eom_cc3_32_tripletm_trans_aibickaiek + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickaiek
    function v3_eom_cc3_32_tripletm_trans_aibickaiei(b, c, k, e) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickaiei   
    integer, intent(in) :: b, c, k, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, k)
term(1) = term(1) + tvvvo(b, e, c, k)

term(1) = term(1) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickaiei = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_tripletm_trans_aibickaiei = v3_eom_cc3_32_tripletm_trans_aibickaiei + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickaiei
    function v3_eom_cc3_32_tripletm_trans_aibickdkai(i, b, c, d) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickdkai   
    integer, intent(in) :: i, b, c, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, c, i)
term(1) = term(1) + tvvvo(c, d, b, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickdkai = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_tripletm_trans_aibickdkai = v3_eom_cc3_32_tripletm_trans_aibickdkai + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickdkai
    function v3_eom_cc3_32_tripletm_trans_aibickdiai(b, c, k, d) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickdiai   
    integer, intent(in) :: b, c, k, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, k)
term(1) = term(1) + tvvvo(b, d, c, k)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickdiai = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_tripletm_trans_aibickdiai = v3_eom_cc3_32_tripletm_trans_aibickdiai + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickdiai
    function v3_eom_cc3_32_tripletm_trans_aibickbibk(a, i, b, c) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickbibk   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, i)



    v3_eom_cc3_32_tripletm_trans_aibickbibk = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickbibk = v3_eom_cc3_32_tripletm_trans_aibickbibk + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickbibk
    function v3_eom_cc3_32_tripletm_trans_aibickbick(a, i, b) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickbick   
    integer, intent(in) :: a, i, b 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickbick = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickbick = v3_eom_cc3_32_tripletm_trans_aibickbick + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickbick
    function v3_eom_cc3_32_tripletm_trans_aibickbkci(a, i, c) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickbkci   
    integer, intent(in) :: a, i, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, c, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickbkci = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickbkci = v3_eom_cc3_32_tripletm_trans_aibickbkci + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickbkci
    function v3_eom_cc3_32_tripletm_trans_aibickbici(a, b, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickbici   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, k)
term(1) = term(1) + tvvvo(a, c, c, k)



    v3_eom_cc3_32_tripletm_trans_aibickbici = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_tripletm_trans_aibickbici = v3_eom_cc3_32_tripletm_trans_aibickbici + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickbici
    function v3_eom_cc3_32_tripletm_trans_aibickbkak(i, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickbkak   
    integer, intent(in) :: i, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, k, i)



    v3_eom_cc3_32_tripletm_trans_aibickbkak = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickbkak = v3_eom_cc3_32_tripletm_trans_aibickbkak + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickbkak
    function v3_eom_cc3_32_tripletm_trans_aibickbiak(i, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickbiak   
    integer, intent(in) :: i, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickbiak = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickbiak = v3_eom_cc3_32_tripletm_trans_aibickbiak + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickbiak
    function v3_eom_cc3_32_tripletm_trans_aibickbkai(a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickbkai   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, k, k)
term(1) = term(1) + tvooo(c, k, k, i)
term(2) = term(2) + tvooo(c, i, i, i)
term(3) = term(3) + tvvvo(b, b, c, i)
term(4) = term(4) + tvvvo(a, a, c, i)
term(5) = term(5) + tvvvo(c, b, b, i)

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickbkai = 0.d+0
    do s = 0, 5
    v3_eom_cc3_32_tripletm_trans_aibickbkai = v3_eom_cc3_32_tripletm_trans_aibickbkai + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickbkai
    function v3_eom_cc3_32_tripletm_trans_aibickbiai(a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickbiai   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, i, k)
term(1) = term(1) + tvooo(c, k, i, i)
term(2) = term(2) + tvvvo(c, b, b, k)
term(3) = term(3) + tvvvo(b, b, c, k)
term(4) = term(4) + tvvvo(a, a, c, k)

term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickbiai = 0.d+0
    do s = 0, 4
    v3_eom_cc3_32_tripletm_trans_aibickbiai = v3_eom_cc3_32_tripletm_trans_aibickbiai + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickbiai
    function v3_eom_cc3_32_tripletm_trans_aibickakbk(i, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickakbk   
    integer, intent(in) :: i, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, k, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickakbk = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickakbk = v3_eom_cc3_32_tripletm_trans_aibickakbk + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickakbk
    function v3_eom_cc3_32_tripletm_trans_aibickaibk(a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickaibk   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, k, k)
term(1) = term(1) + tvooo(c, k, k, i)
term(2) = term(2) + tvooo(c, i, i, i)
term(3) = term(3) + tvvvo(a, a, c, i)
term(4) = term(4) + tvvvo(b, b, c, i)
term(5) = term(5) + tvvvo(c, b, b, i)

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickaibk = 0.d+0
    do s = 0, 5
    v3_eom_cc3_32_tripletm_trans_aibickaibk = v3_eom_cc3_32_tripletm_trans_aibickaibk + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickaibk
    function v3_eom_cc3_32_tripletm_trans_aibickakbi(i, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickakbi   
    integer, intent(in) :: i, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)



    v3_eom_cc3_32_tripletm_trans_aibickakbi = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickakbi = v3_eom_cc3_32_tripletm_trans_aibickakbi + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickakbi
    function v3_eom_cc3_32_tripletm_trans_aibickaibi(a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickaibi   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, i, i)
term(1) = term(1) + tvooo(c, i, i, k)
term(2) = term(2) + tvvvo(a, a, c, k)
term(3) = term(3) + tvvvo(c, b, b, k)
term(4) = term(4) + tvvvo(b, b, c, k)

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickaibi = 0.d+0
    do s = 0, 4
    v3_eom_cc3_32_tripletm_trans_aibickaibi = v3_eom_cc3_32_tripletm_trans_aibickaibi + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickaibi
    function v3_eom_cc3_32_tripletm_trans_aibickcick(a, i, b, c) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickcick   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickcick = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickcick = v3_eom_cc3_32_tripletm_trans_aibickcick + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickcick
    function v3_eom_cc3_32_tripletm_trans_aibickckak(i, b, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickckak   
    integer, intent(in) :: i, b, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, k, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickckak = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickckak = v3_eom_cc3_32_tripletm_trans_aibickckak + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickckak
    function v3_eom_cc3_32_tripletm_trans_aibickciak(i, b, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickciak   
    integer, intent(in) :: i, b, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)



    v3_eom_cc3_32_tripletm_trans_aibickciak = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickciak = v3_eom_cc3_32_tripletm_trans_aibickciak + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickciak
    function v3_eom_cc3_32_tripletm_trans_aibickckai(a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickckai   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)
term(1) = term(1) + tvooo(b, i, k, k)
term(2) = term(2) + tvooo(b, i, i, i)
term(3) = term(3) + tvvvo(b, c, c, i)
term(4) = term(4) + tvvvo(a, a, b, i)
term(5) = term(5) + tvvvo(c, c, b, i)

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickckai = 0.d+0
    do s = 0, 5
    v3_eom_cc3_32_tripletm_trans_aibickckai = v3_eom_cc3_32_tripletm_trans_aibickckai + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickckai
    function v3_eom_cc3_32_tripletm_trans_aibickciai(a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickciai   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, i, i)
term(1) = term(1) + tvooo(b, i, i, k)
term(2) = term(2) + tvvvo(c, c, b, k)
term(3) = term(3) + tvvvo(b, c, c, k)
term(4) = term(4) + tvvvo(a, a, b, k)

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickciai = 0.d+0
    do s = 0, 4
    v3_eom_cc3_32_tripletm_trans_aibickciai = v3_eom_cc3_32_tripletm_trans_aibickciai + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickciai
    function v3_eom_cc3_32_tripletm_trans_aibickakck(i, b, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickakck   
    integer, intent(in) :: i, b, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, k, i)



    v3_eom_cc3_32_tripletm_trans_aibickakck = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickakck = v3_eom_cc3_32_tripletm_trans_aibickakck + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickakck
    function v3_eom_cc3_32_tripletm_trans_aibickaick(a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickaick   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, k, k)
term(1) = term(1) + tvooo(b, k, k, i)
term(2) = term(2) + tvooo(b, i, i, i)
term(3) = term(3) + tvvvo(a, a, b, i)
term(4) = term(4) + tvvvo(b, c, c, i)
term(5) = term(5) + tvvvo(c, c, b, i)

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickaick = 0.d+0
    do s = 0, 5
    v3_eom_cc3_32_tripletm_trans_aibickaick = v3_eom_cc3_32_tripletm_trans_aibickaick + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickaick
    function v3_eom_cc3_32_tripletm_trans_aibickakci(i, b, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickakci   
    integer, intent(in) :: i, b, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)

term(0) = term(0) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickakci = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_tripletm_trans_aibickakci = v3_eom_cc3_32_tripletm_trans_aibickakci + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickakci
    function v3_eom_cc3_32_tripletm_trans_aibickaici(a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickaici   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, i, i)
term(1) = term(1) + tvooo(b, i, i, k)
term(2) = term(2) + tvvvo(a, a, b, k)
term(3) = term(3) + tvvvo(c, c, b, k)
term(4) = term(4) + tvvvo(b, c, c, k)

term(0) = term(0) * (-2.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickaici = 0.d+0
    do s = 0, 4
    v3_eom_cc3_32_tripletm_trans_aibickaici = v3_eom_cc3_32_tripletm_trans_aibickaici + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickaici
    function v3_eom_cc3_32_tripletm_trans_aibickaiak(a, i, b, c) 
    real(F64) :: v3_eom_cc3_32_tripletm_trans_aibickaiak   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, i)
term(1) = term(1) + tvvvo(c, a, b, i)

term(1) = term(1) * (-1.0d+0) 


    v3_eom_cc3_32_tripletm_trans_aibickaiak = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_tripletm_trans_aibickaiak = v3_eom_cc3_32_tripletm_trans_aibickaiak + term(s)
    end do

    end function v3_eom_cc3_32_tripletm_trans_aibickaiak
    end module v3_eom_cc3_32_tripletm_trans
    