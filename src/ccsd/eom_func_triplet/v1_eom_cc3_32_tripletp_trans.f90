module v1_eom_cc3_32_tripletp_trans

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
    
    function v1_eom_cc3_32_tripletp_trans_aiajckajcm(a, i, k, m) 
    real(F64) :: v1_eom_cc3_32_tripletp_trans_aiajckajcm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletp_trans_aiajckajcm = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletp_trans_aiajckajcm = v1_eom_cc3_32_tripletp_trans_aiajckajcm + term(s)
    end do

    end function v1_eom_cc3_32_tripletp_trans_aiajckajcm
    function v1_eom_cc3_32_tripletp_trans_aiajckalcj(a, i, k, l) 
    real(F64) :: v1_eom_cc3_32_tripletp_trans_aiajckalcj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)



    v1_eom_cc3_32_tripletp_trans_aiajckalcj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletp_trans_aiajckalcj = v1_eom_cc3_32_tripletp_trans_aiajckalcj + term(s)
    end do

    end function v1_eom_cc3_32_tripletp_trans_aiajckalcj
    function v1_eom_cc3_32_tripletp_trans_aiajckakcm(a, i, j, m) 
    real(F64) :: v1_eom_cc3_32_tripletp_trans_aiajckakcm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    v1_eom_cc3_32_tripletp_trans_aiajckakcm = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletp_trans_aiajckakcm = v1_eom_cc3_32_tripletp_trans_aiajckakcm + term(s)
    end do

    end function v1_eom_cc3_32_tripletp_trans_aiajckakcm
    function v1_eom_cc3_32_tripletp_trans_aiajckalck(a, i, j, l) 
    real(F64) :: v1_eom_cc3_32_tripletp_trans_aiajckalck   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletp_trans_aiajckalck = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletp_trans_aiajckalck = v1_eom_cc3_32_tripletp_trans_aiajckalck + term(s)
    end do

    end function v1_eom_cc3_32_tripletp_trans_aiajckalck
    function v1_eom_cc3_32_tripletp_trans_aiajckajek(a, i, c, e) 
    real(F64) :: v1_eom_cc3_32_tripletp_trans_aiajckajek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    v1_eom_cc3_32_tripletp_trans_aiajckajek = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletp_trans_aiajckajek = v1_eom_cc3_32_tripletp_trans_aiajckajek + term(s)
    end do

    end function v1_eom_cc3_32_tripletp_trans_aiajckajek
    function v1_eom_cc3_32_tripletp_trans_aiajckdjak(a, i, c, d) 
    real(F64) :: v1_eom_cc3_32_tripletp_trans_aiajckdjak   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletp_trans_aiajckdjak = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletp_trans_aiajckdjak = v1_eom_cc3_32_tripletp_trans_aiajckdjak + term(s)
    end do

    end function v1_eom_cc3_32_tripletp_trans_aiajckdjak
    function v1_eom_cc3_32_tripletp_trans_aiajckcjek(a, i, e) 
    real(F64) :: v1_eom_cc3_32_tripletp_trans_aiajckcjek   
    integer, intent(in) :: a, i, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletp_trans_aiajckcjek = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletp_trans_aiajckcjek = v1_eom_cc3_32_tripletp_trans_aiajckcjek + term(s)
    end do

    end function v1_eom_cc3_32_tripletp_trans_aiajckcjek
    function v1_eom_cc3_32_tripletp_trans_aiajckdjck(a, i, d) 
    real(F64) :: v1_eom_cc3_32_tripletp_trans_aiajckdjck   
    integer, intent(in) :: a, i, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)



    v1_eom_cc3_32_tripletp_trans_aiajckdjck = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletp_trans_aiajckdjck = v1_eom_cc3_32_tripletp_trans_aiajckdjck + term(s)
    end do

    end function v1_eom_cc3_32_tripletp_trans_aiajckdjck
    function v1_eom_cc3_32_tripletp_trans_aiajckaicj(a, i, k) 
    real(F64) :: v1_eom_cc3_32_tripletp_trans_aiajckaicj   
    integer, intent(in) :: a, i, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)



    v1_eom_cc3_32_tripletp_trans_aiajckaicj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletp_trans_aiajckaicj = v1_eom_cc3_32_tripletp_trans_aiajckaicj + term(s)
    end do

    end function v1_eom_cc3_32_tripletp_trans_aiajckaicj
    function v1_eom_cc3_32_tripletp_trans_aiajckaick(a, i, j) 
    real(F64) :: v1_eom_cc3_32_tripletp_trans_aiajckaick   
    integer, intent(in) :: a, i, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletp_trans_aiajckaick = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletp_trans_aiajckaick = v1_eom_cc3_32_tripletp_trans_aiajckaick + term(s)
    end do

    end function v1_eom_cc3_32_tripletp_trans_aiajckaick
    function v1_eom_cc3_32_tripletp_trans_aiajckajci(a, i, k) 
    real(F64) :: v1_eom_cc3_32_tripletp_trans_aiajckajci   
    integer, intent(in) :: a, i, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)

term(0) = term(0) * (-1.0d+0) 


    v1_eom_cc3_32_tripletp_trans_aiajckajci = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletp_trans_aiajckajci = v1_eom_cc3_32_tripletp_trans_aiajckajci + term(s)
    end do

    end function v1_eom_cc3_32_tripletp_trans_aiajckajci
    function v1_eom_cc3_32_tripletp_trans_aiajckakci(a, i, j) 
    real(F64) :: v1_eom_cc3_32_tripletp_trans_aiajckakci   
    integer, intent(in) :: a, i, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)



    v1_eom_cc3_32_tripletp_trans_aiajckakci = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_tripletp_trans_aiajckakci = v1_eom_cc3_32_tripletp_trans_aiajckakci + term(s)
    end do

    end function v1_eom_cc3_32_tripletp_trans_aiajckakci
    function v1_eom_cc3_32_tripletp_trans_aiajckajck(a, i, j, c, k) 
    real(F64) :: v1_eom_cc3_32_tripletp_trans_aiajckajck   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, i, k, k)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvvvo(c, c, a, i)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 


    v1_eom_cc3_32_tripletp_trans_aiajckajck = 0.d+0
    do s = 0, 3
    v1_eom_cc3_32_tripletp_trans_aiajckajck = v1_eom_cc3_32_tripletp_trans_aiajckajck + term(s)
    end do

    end function v1_eom_cc3_32_tripletp_trans_aiajckajck
    end module v1_eom_cc3_32_tripletp_trans
    