module v1_eom_cc3_32_tripletm_trans

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
    
    function v1_eom_cc3_32_tripletm_trans_aiajckalak(i, j, c, l) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckalak   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, i)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckalak = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckalak = v1_eom_cc3_32_tripletm_trans_aiajckalak + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckalak
    function v1_eom_cc3_32_tripletm_trans_aiajckakam(i, j, c, m) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckakam   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, i)



    v1_eom_cc3_32_tripletm_trans_aiajckakam = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckakam = v1_eom_cc3_32_tripletm_trans_aiajckakam + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckakam
    function v1_eom_cc3_32_tripletm_trans_aiajckalai(j, c, k, l) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckalai   
    integer, intent(in) :: j, c, k, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, k)
term(1) = term(1) + tvooo(c, k, l, j)

term(1) = term(1) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckalai = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_tripletm_trans_aiajckalai = v1_eom_cc3_32_tripletm_trans_aiajckalai + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckalai
    function v1_eom_cc3_32_tripletm_trans_aiajckaiam(j, c, k, m) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckaiam   
    integer, intent(in) :: j, c, k, m 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, k)
term(1) = term(1) + tvooo(c, k, m, j)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckaiam = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_tripletm_trans_aiajckaiam = v1_eom_cc3_32_tripletm_trans_aiajckaiam + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckaiam
    function v1_eom_cc3_32_tripletm_trans_aiajckalaj(i, c, k, l) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckalaj   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, l, i)



    v1_eom_cc3_32_tripletm_trans_aiajckalaj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckalaj = v1_eom_cc3_32_tripletm_trans_aiajckalaj + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckalaj
    function v1_eom_cc3_32_tripletm_trans_aiajckajam(i, c, k, m) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckajam   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, m, i)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckajam = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckajam = v1_eom_cc3_32_tripletm_trans_aiajckajam + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckajam
    function v1_eom_cc3_32_tripletm_trans_aiajckalck(a, i, j, l) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckalck   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, i)



    v1_eom_cc3_32_tripletm_trans_aiajckalck = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckalck = v1_eom_cc3_32_tripletm_trans_aiajckalck + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckalck
    function v1_eom_cc3_32_tripletm_trans_aiajckaicm(a, j, k, m) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckaicm   
    integer, intent(in) :: a, j, k, m 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, k)
term(1) = term(1) + tvooo(a, k, m, j)

term(1) = term(1) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckaicm = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_tripletm_trans_aiajckaicm = v1_eom_cc3_32_tripletm_trans_aiajckaicm + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckaicm
    function v1_eom_cc3_32_tripletm_trans_aiajckalcj(a, i, k, l) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckalcj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, l, i)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckalcj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckalcj = v1_eom_cc3_32_tripletm_trans_aiajckalcj + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckalcj
    function v1_eom_cc3_32_tripletm_trans_aiajckaiek(a, j, c, e) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckaiek   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, j)
term(1) = term(1) + tvvvo(a, e, c, j)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckaiek = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_tripletm_trans_aiajckaiek = v1_eom_cc3_32_tripletm_trans_aiajckaiek + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckaiek
    function v1_eom_cc3_32_tripletm_trans_aiajckakei(a, j, c, e) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckakei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, j)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckakei = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckakei = v1_eom_cc3_32_tripletm_trans_aiajckakei + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckakei
    function v1_eom_cc3_32_tripletm_trans_aiajckajei(a, c, k, e) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckajei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, k)



    v1_eom_cc3_32_tripletm_trans_aiajckajei = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckajei = v1_eom_cc3_32_tripletm_trans_aiajckajei + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckajei
    function v1_eom_cc3_32_tripletm_trans_aiajckaiej(a, c, k, e) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckaiej   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, k)
term(1) = term(1) + tvvvo(a, e, c, k)

term(1) = term(1) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckaiej = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_tripletm_trans_aiajckaiej = v1_eom_cc3_32_tripletm_trans_aiajckaiej + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckaiej
    function v1_eom_cc3_32_tripletm_trans_aiajckdiak(a, j, c, d) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckdiak   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, j)



    v1_eom_cc3_32_tripletm_trans_aiajckdiak = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckdiak = v1_eom_cc3_32_tripletm_trans_aiajckdiak + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckdiak
    function v1_eom_cc3_32_tripletm_trans_aiajckdkai(a, j, c, d) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckdkai   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, j)
term(1) = term(1) + tvvvo(c, d, a, j)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckdkai = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_tripletm_trans_aiajckdkai = v1_eom_cc3_32_tripletm_trans_aiajckdkai + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckdkai
    function v1_eom_cc3_32_tripletm_trans_aiajckdjai(a, c, k, d) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckdjai   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, k)
term(1) = term(1) + tvvvo(a, d, c, k)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckdjai = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_tripletm_trans_aiajckdjai = v1_eom_cc3_32_tripletm_trans_aiajckdjai + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckdjai
    function v1_eom_cc3_32_tripletm_trans_aiajckdiaj(a, c, k, d) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckdiaj   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, k)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckdiaj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckdiaj = v1_eom_cc3_32_tripletm_trans_aiajckdiaj + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckdiaj
    function v1_eom_cc3_32_tripletm_trans_aiajckckei(a, j, e) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckckei   
    integer, intent(in) :: a, j, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, j)



    v1_eom_cc3_32_tripletm_trans_aiajckckei = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckckei = v1_eom_cc3_32_tripletm_trans_aiajckckei + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckckei
    function v1_eom_cc3_32_tripletm_trans_aiajckcjei(a, k, e) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckcjei   
    integer, intent(in) :: a, k, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, k)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckcjei = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckcjei = v1_eom_cc3_32_tripletm_trans_aiajckcjei + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckcjei
    function v1_eom_cc3_32_tripletm_trans_aiajckdick(a, j, d) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckdick   
    integer, intent(in) :: a, j, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, j)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckdick = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckdick = v1_eom_cc3_32_tripletm_trans_aiajckdick + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckdick
    function v1_eom_cc3_32_tripletm_trans_aiajckdicj(a, k, d) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckdicj   
    integer, intent(in) :: a, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, k)



    v1_eom_cc3_32_tripletm_trans_aiajckdicj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckdicj = v1_eom_cc3_32_tripletm_trans_aiajckdicj + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckdicj
    function v1_eom_cc3_32_tripletm_trans_aiajckaiak(a, i, j, c, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckaiak   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, i, i)
term(1) = term(1) + tvooo(c, j, k, k)
term(2) = term(2) + tvooo(c, k, k, j)
term(3) = term(3) + tvvvo(a, a, c, j)
term(4) = term(4) + tvvvo(c, a, a, j)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckaiak = 0.d+0
    do s = 0, 4
    v1_eom_cc3_32_tripletm_trans_aiajckaiak = v1_eom_cc3_32_tripletm_trans_aiajckaiak + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckaiak
    function v1_eom_cc3_32_tripletm_trans_aiajckajak(i, j, c, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckajak   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)
term(1) = term(1) + tvooo(c, k, k, i)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckajak = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_tripletm_trans_aiajckajak = v1_eom_cc3_32_tripletm_trans_aiajckajak + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckajak
    function v1_eom_cc3_32_tripletm_trans_aiajckakai(a, i, j, c, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckakai   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, k, k)
term(1) = term(1) + tvooo(c, k, k, j)
term(2) = term(2) + tvooo(c, j, i, i)
term(3) = term(3) + tvvvo(a, a, c, j)
term(4) = term(4) + tvvvo(c, a, a, j)

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-2.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckakai = 0.d+0
    do s = 0, 4
    v1_eom_cc3_32_tripletm_trans_aiajckakai = v1_eom_cc3_32_tripletm_trans_aiajckakai + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckakai
    function v1_eom_cc3_32_tripletm_trans_aiajckajai(a, i, j, c, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckajai   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, k)
term(1) = term(1) + tvooo(c, k, j, j)
term(2) = term(2) + tvooo(c, k, i, i)
term(3) = term(3) + tvvvo(c, a, a, k)
term(4) = term(4) + tvvvo(a, a, c, k)

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (2.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckajai = 0.d+0
    do s = 0, 4
    v1_eom_cc3_32_tripletm_trans_aiajckajai = v1_eom_cc3_32_tripletm_trans_aiajckajai + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckajai
    function v1_eom_cc3_32_tripletm_trans_aiajckaiaj(a, i, j, c, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckaiaj   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, i, i)
term(1) = term(1) + tvooo(c, j, j, k)
term(2) = term(2) + tvooo(c, k, j, j)
term(3) = term(3) + tvvvo(a, a, c, k)
term(4) = term(4) + tvvvo(c, a, a, k)

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-2.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckaiaj = 0.d+0
    do s = 0, 4
    v1_eom_cc3_32_tripletm_trans_aiajckaiaj = v1_eom_cc3_32_tripletm_trans_aiajckaiaj + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckaiaj
    function v1_eom_cc3_32_tripletm_trans_aiajckakck(a, i, j, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckakck   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, k, i)



    v1_eom_cc3_32_tripletm_trans_aiajckakck = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckakck = v1_eom_cc3_32_tripletm_trans_aiajckakck + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckakck
    function v1_eom_cc3_32_tripletm_trans_aiajckaick(a, i, j, c, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckaick   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, i)
term(1) = term(1) + tvooo(a, j, k, k)
term(2) = term(2) + tvooo(a, k, k, j)
term(3) = term(3) + tvvvo(a, a, a, j)
term(4) = term(4) + tvvvo(c, c, a, j)
term(5) = term(5) + tvvvo(a, c, c, j)

term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckaick = 0.d+0
    do s = 0, 5
    v1_eom_cc3_32_tripletm_trans_aiajckaick = v1_eom_cc3_32_tripletm_trans_aiajckaick + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckaick
    function v1_eom_cc3_32_tripletm_trans_aiajckajck(a, i, j) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckajck   
    integer, intent(in) :: a, i, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)



    v1_eom_cc3_32_tripletm_trans_aiajckajck = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckajck = v1_eom_cc3_32_tripletm_trans_aiajckajck + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckajck
    function v1_eom_cc3_32_tripletm_trans_aiajckakci(a, j, c) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckakci   
    integer, intent(in) :: a, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, c, j)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckakci = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckakci = v1_eom_cc3_32_tripletm_trans_aiajckakci + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckakci
    function v1_eom_cc3_32_tripletm_trans_aiajckakcj(a, i, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckakcj   
    integer, intent(in) :: a, i, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, k, i)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckakcj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckakcj = v1_eom_cc3_32_tripletm_trans_aiajckakcj + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckakcj
    function v1_eom_cc3_32_tripletm_trans_aiajckaici(a, i, j, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckaici   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, k)
term(1) = term(1) + tvooo(a, k, i, j)

term(1) = term(1) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckaici = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_tripletm_trans_aiajckaici = v1_eom_cc3_32_tripletm_trans_aiajckaici + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckaici
    function v1_eom_cc3_32_tripletm_trans_aiajckajci(a, c, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckajci   
    integer, intent(in) :: a, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, c, k)



    v1_eom_cc3_32_tripletm_trans_aiajckajci = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckajci = v1_eom_cc3_32_tripletm_trans_aiajckajci + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckajci
    function v1_eom_cc3_32_tripletm_trans_aiajckaicj(a, i, j, c, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckaicj   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, i, i)
term(1) = term(1) + tvooo(a, j, j, k)
term(2) = term(2) + tvooo(a, k, j, j)
term(3) = term(3) + tvvvo(a, a, a, k)
term(4) = term(4) + tvvvo(c, c, a, k)
term(5) = term(5) + tvvvo(a, c, c, k)

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckaicj = 0.d+0
    do s = 0, 5
    v1_eom_cc3_32_tripletm_trans_aiajckaicj = v1_eom_cc3_32_tripletm_trans_aiajckaicj + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckaicj
    function v1_eom_cc3_32_tripletm_trans_aiajckajcj(a, i, j, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckajcj   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, j, i)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckajcj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckajcj = v1_eom_cc3_32_tripletm_trans_aiajckajcj + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckajcj
    function v1_eom_cc3_32_tripletm_trans_aiajckcick(a, j, c) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckcick   
    integer, intent(in) :: a, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, a, j)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckcick = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckcick = v1_eom_cc3_32_tripletm_trans_aiajckcick + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckcick
    function v1_eom_cc3_32_tripletm_trans_aiajckckci(a, j, c) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckckci   
    integer, intent(in) :: a, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, a, j)



    v1_eom_cc3_32_tripletm_trans_aiajckckci = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckckci = v1_eom_cc3_32_tripletm_trans_aiajckckci + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckckci
    function v1_eom_cc3_32_tripletm_trans_aiajckcjci(a, c, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckcjci   
    integer, intent(in) :: a, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, a, k)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletm_trans_aiajckcjci = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckcjci = v1_eom_cc3_32_tripletm_trans_aiajckcjci + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckcjci
    function v1_eom_cc3_32_tripletm_trans_aiajckcicj(a, c, k) 
    real(F64) :: v1_eom_cc3_32_tripletm_trans_aiajckcicj   
    integer, intent(in) :: a, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, a, k)



    v1_eom_cc3_32_tripletm_trans_aiajckcicj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletm_trans_aiajckcicj = v1_eom_cc3_32_tripletm_trans_aiajckcicj + term(s)
    end do

    end function v1_eom_cc3_32_tripletm_trans_aiajckcicj
    end module v1_eom_cc3_32_tripletm_trans
    