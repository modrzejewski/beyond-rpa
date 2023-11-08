module v5_eom_cc3_32_tripletm_trans

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
    
    function v5_eom_cc3_32_tripletm_trans_aiaickalak(i, c, l) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickalak   
    integer, intent(in) :: i, c, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, i)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickalak = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickalak = v5_eom_cc3_32_tripletm_trans_aiaickalak + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickalak
    function v5_eom_cc3_32_tripletm_trans_aiaickakam(i, c, m) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickakam   
    integer, intent(in) :: i, c, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, i)



    v5_eom_cc3_32_tripletm_trans_aiaickakam = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickakam = v5_eom_cc3_32_tripletm_trans_aiaickakam + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickakam
    function v5_eom_cc3_32_tripletm_trans_aiaickalai(i, c, k, l) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickalai   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, k)



    v5_eom_cc3_32_tripletm_trans_aiaickalai = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickalai = v5_eom_cc3_32_tripletm_trans_aiaickalai + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickalai
    function v5_eom_cc3_32_tripletm_trans_aiaickaiam(i, c, k, m) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickaiam   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, k)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickaiam = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickaiam = v5_eom_cc3_32_tripletm_trans_aiaickaiam + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickaiam
    function v5_eom_cc3_32_tripletm_trans_aiaickalck(a, i, l) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickalck   
    integer, intent(in) :: a, i, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)



    v5_eom_cc3_32_tripletm_trans_aiaickalck = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickalck = v5_eom_cc3_32_tripletm_trans_aiaickalck + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickalck
    function v5_eom_cc3_32_tripletm_trans_aiaickalci(a, i, k, l) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickalci   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, l, i)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickalci = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickalci = v5_eom_cc3_32_tripletm_trans_aiaickalci + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickalci
    function v5_eom_cc3_32_tripletm_trans_aiaickaicm(a, i, k, m) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickaicm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)
term(1) = term(1) + tvooo(a, k, m, i)

term(1) = term(1) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickaicm = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_tripletm_trans_aiaickaicm = v5_eom_cc3_32_tripletm_trans_aiaickaicm + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickaicm
    function v5_eom_cc3_32_tripletm_trans_aiaickaiek(a, i, c, e) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickaiek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)
term(1) = term(1) + tvvvo(a, e, c, i)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickaiek = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_tripletm_trans_aiaickaiek = v5_eom_cc3_32_tripletm_trans_aiaickaiek + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickaiek
    function v5_eom_cc3_32_tripletm_trans_aiaickakei(a, i, c, e) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickakei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, i)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickakei = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickakei = v5_eom_cc3_32_tripletm_trans_aiaickakei + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickakei
    function v5_eom_cc3_32_tripletm_trans_aiaickaiei(a, c, k, e) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickaiei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, k)



    v5_eom_cc3_32_tripletm_trans_aiaickaiei = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickaiei = v5_eom_cc3_32_tripletm_trans_aiaickaiei + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickaiei
    function v5_eom_cc3_32_tripletm_trans_aiaickdiak(a, i, c, d) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickdiak   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, i)



    v5_eom_cc3_32_tripletm_trans_aiaickdiak = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickdiak = v5_eom_cc3_32_tripletm_trans_aiaickdiak + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickdiak
    function v5_eom_cc3_32_tripletm_trans_aiaickdkai(a, i, c, d) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickdkai   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, i)
term(1) = term(1) + tvvvo(c, d, a, i)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickdkai = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_tripletm_trans_aiaickdkai = v5_eom_cc3_32_tripletm_trans_aiaickdkai + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickdkai
    function v5_eom_cc3_32_tripletm_trans_aiaickdiai(a, c, k, d) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickdiai   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, k)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickdiai = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickdiai = v5_eom_cc3_32_tripletm_trans_aiaickdiai + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickdiai
    function v5_eom_cc3_32_tripletm_trans_aiaickckei(a, i, e) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickckei   
    integer, intent(in) :: a, i, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)



    v5_eom_cc3_32_tripletm_trans_aiaickckei = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickckei = v5_eom_cc3_32_tripletm_trans_aiaickckei + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickckei
    function v5_eom_cc3_32_tripletm_trans_aiaickciei(a, k, e) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickciei   
    integer, intent(in) :: a, k, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, k)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickciei = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickciei = v5_eom_cc3_32_tripletm_trans_aiaickciei + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickciei
    function v5_eom_cc3_32_tripletm_trans_aiaickdick(a, i, d) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickdick   
    integer, intent(in) :: a, i, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickdick = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickdick = v5_eom_cc3_32_tripletm_trans_aiaickdick + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickdick
    function v5_eom_cc3_32_tripletm_trans_aiaickdici(a, k, d) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickdici   
    integer, intent(in) :: a, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, k)



    v5_eom_cc3_32_tripletm_trans_aiaickdici = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickdici = v5_eom_cc3_32_tripletm_trans_aiaickdici + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickdici
    function v5_eom_cc3_32_tripletm_trans_aiaickaiak(a, i, c, k) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickaiak   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, k, k)
term(1) = term(1) + tvooo(c, i, i, i)
term(2) = term(2) + tvvvo(a, a, c, i)
term(3) = term(3) + tvvvo(c, a, a, i)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickaiak = 0.d+0
    do s = 0, 3
    v5_eom_cc3_32_tripletm_trans_aiaickaiak = v5_eom_cc3_32_tripletm_trans_aiaickaiak + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickaiak
    function v5_eom_cc3_32_tripletm_trans_aiaickakck(a, i, k) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickakck   
    integer, intent(in) :: a, i, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, i)



    v5_eom_cc3_32_tripletm_trans_aiaickakck = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickakck = v5_eom_cc3_32_tripletm_trans_aiaickakck + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickakck
    function v5_eom_cc3_32_tripletm_trans_aiaickaick(a, i, c, k) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickaick   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvooo(a, k, k, i)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvooo(a, i, i, i)
term(4) = term(4) + tvvvo(c, c, a, i)
term(5) = term(5) + tvvvo(a, c, c, i)

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickaick = 0.d+0
    do s = 0, 5
    v5_eom_cc3_32_tripletm_trans_aiaickaick = v5_eom_cc3_32_tripletm_trans_aiaickaick + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickaick
    function v5_eom_cc3_32_tripletm_trans_aiaickakci(a, i, c, k) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickakci   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, k, i)
term(1) = term(1) + tvvvo(a, c, c, i)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickakci = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_tripletm_trans_aiaickakci = v5_eom_cc3_32_tripletm_trans_aiaickakci + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickakci
    function v5_eom_cc3_32_tripletm_trans_aiaickaici(a, i, c, k) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickaici   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, i, i)
term(1) = term(1) + tvooo(a, i, i, k)
term(2) = term(2) + tvvvo(a, a, a, k)
term(3) = term(3) + tvvvo(c, c, a, k)

term(0) = term(0) * (-2.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickaici = 0.d+0
    do s = 0, 3
    v5_eom_cc3_32_tripletm_trans_aiaickaici = v5_eom_cc3_32_tripletm_trans_aiaickaici + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickaici
    function v5_eom_cc3_32_tripletm_trans_aiaickcick(a, i, c) 
    real(F64) :: v5_eom_cc3_32_tripletm_trans_aiaickcick   
    integer, intent(in) :: a, i, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, a, i)

term(0) = term(0) * (-1.0d+0) 


    v5_eom_cc3_32_tripletm_trans_aiaickcick = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_tripletm_trans_aiaickcick = v5_eom_cc3_32_tripletm_trans_aiaickcick + term(s)
    end do

    end function v5_eom_cc3_32_tripletm_trans_aiaickcick
    end module v5_eom_cc3_32_tripletm_trans
    