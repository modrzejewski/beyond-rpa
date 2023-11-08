module v9_eom_cc3_32_tripletm_trans

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
    
    function v9_eom_cc3_32_tripletm_trans_aibjckbkam(i, j, c, m) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbkam   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, i)



    v9_eom_cc3_32_tripletm_trans_aibjckbkam = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbkam = v9_eom_cc3_32_tripletm_trans_aibjckbkam + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbkam
    function v9_eom_cc3_32_tripletm_trans_aibjckblai(j, c, k, l) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckblai   
    integer, intent(in) :: j, c, k, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, k)
term(1) = term(1) + tvooo(c, k, l, j)

term(1) = term(1) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckblai = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckblai = v9_eom_cc3_32_tripletm_trans_aibjckblai + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckblai
    function v9_eom_cc3_32_tripletm_trans_aibjckbjam(i, c, k, m) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbjam   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, m, i)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckbjam = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbjam = v9_eom_cc3_32_tripletm_trans_aibjckbjam + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbjam
    function v9_eom_cc3_32_tripletm_trans_aibjckbkei(a, j, c, e) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbkei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, j)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckbkei = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbkei = v9_eom_cc3_32_tripletm_trans_aibjckbkei + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbkei
    function v9_eom_cc3_32_tripletm_trans_aibjckbjei(a, c, k, e) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbjei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, k)



    v9_eom_cc3_32_tripletm_trans_aibjckbjei = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbjei = v9_eom_cc3_32_tripletm_trans_aibjckbjei + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbjei
    function v9_eom_cc3_32_tripletm_trans_aibjckalbk(i, j, c, l) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckalbk   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, i)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckalbk = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckalbk = v9_eom_cc3_32_tripletm_trans_aibjckalbk + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckalbk
    function v9_eom_cc3_32_tripletm_trans_aibjckaibm(j, c, k, m) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckaibm   
    integer, intent(in) :: j, c, k, m 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, k)
term(1) = term(1) + tvooo(c, k, m, j)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckaibm = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckaibm = v9_eom_cc3_32_tripletm_trans_aibjckaibm + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckaibm
    function v9_eom_cc3_32_tripletm_trans_aibjckalbj(i, c, k, l) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckalbj   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, l, i)



    v9_eom_cc3_32_tripletm_trans_aibjckalbj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckalbj = v9_eom_cc3_32_tripletm_trans_aibjckalbj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckalbj
    function v9_eom_cc3_32_tripletm_trans_aibjckdibk(a, j, c, d) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckdibk   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, j)



    v9_eom_cc3_32_tripletm_trans_aibjckdibk = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckdibk = v9_eom_cc3_32_tripletm_trans_aibjckdibk + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckdibk
    function v9_eom_cc3_32_tripletm_trans_aibjckdibj(a, c, k, d) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckdibj   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, k)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckdibj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckdibj = v9_eom_cc3_32_tripletm_trans_aibjckdibj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckdibj
    function v9_eom_cc3_32_tripletm_trans_aibjckckam(i, b, j, m) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckckam   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, m, i)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckckam = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckckam = v9_eom_cc3_32_tripletm_trans_aibjckckam + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckckam
    function v9_eom_cc3_32_tripletm_trans_aibjckclai(b, j, k, l) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckclai   
    integer, intent(in) :: b, j, k, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, j)
term(1) = term(1) + tvooo(b, j, l, k)

term(1) = term(1) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckclai = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckclai = v9_eom_cc3_32_tripletm_trans_aibjckclai + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckclai
    function v9_eom_cc3_32_tripletm_trans_aibjckcjam(i, b, k, m) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckcjam   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, m, i)



    v9_eom_cc3_32_tripletm_trans_aibjckcjam = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckcjam = v9_eom_cc3_32_tripletm_trans_aibjckcjam + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckcjam
    function v9_eom_cc3_32_tripletm_trans_aibjckckei(a, b, j, e) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckckei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, j)



    v9_eom_cc3_32_tripletm_trans_aibjckckei = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckckei = v9_eom_cc3_32_tripletm_trans_aibjckckei + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckckei
    function v9_eom_cc3_32_tripletm_trans_aibjckcjei(a, b, k, e) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckcjei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, k)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckcjei = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckcjei = v9_eom_cc3_32_tripletm_trans_aibjckcjei + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckcjei
    function v9_eom_cc3_32_tripletm_trans_aibjckalck(i, b, j, l) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckalck   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, i)



    v9_eom_cc3_32_tripletm_trans_aibjckalck = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckalck = v9_eom_cc3_32_tripletm_trans_aibjckalck + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckalck
    function v9_eom_cc3_32_tripletm_trans_aibjckaicm(b, j, k, m) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckaicm   
    integer, intent(in) :: b, j, k, m 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, m, j)
term(1) = term(1) + tvooo(b, j, m, k)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckaicm = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckaicm = v9_eom_cc3_32_tripletm_trans_aibjckaicm + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckaicm
    function v9_eom_cc3_32_tripletm_trans_aibjckalcj(i, b, k, l) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckalcj   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, i)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckalcj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckalcj = v9_eom_cc3_32_tripletm_trans_aibjckalcj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckalcj
    function v9_eom_cc3_32_tripletm_trans_aibjckdick(a, b, j, d) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckdick   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, j)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckdick = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckdick = v9_eom_cc3_32_tripletm_trans_aibjckdick + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckdick
    function v9_eom_cc3_32_tripletm_trans_aibjckdicj(a, b, k, d) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckdicj   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, k)



    v9_eom_cc3_32_tripletm_trans_aibjckdicj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckdicj = v9_eom_cc3_32_tripletm_trans_aibjckdicj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckdicj
    function v9_eom_cc3_32_tripletm_trans_aibjckaiek(b, j, c, e) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckaiek   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, c, j)
term(1) = term(1) + tvvvo(c, e, b, j)

term(1) = term(1) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckaiek = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckaiek = v9_eom_cc3_32_tripletm_trans_aibjckaiek + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckaiek
    function v9_eom_cc3_32_tripletm_trans_aibjckaiej(b, c, k, e) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckaiej   
    integer, intent(in) :: b, c, k, e 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, k)
term(1) = term(1) + tvvvo(b, e, c, k)

term(1) = term(1) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckaiej = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckaiej = v9_eom_cc3_32_tripletm_trans_aibjckaiej + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckaiej
    function v9_eom_cc3_32_tripletm_trans_aibjckdkai(b, j, c, d) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckdkai   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, c, j)
term(1) = term(1) + tvvvo(c, d, b, j)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckdkai = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckdkai = v9_eom_cc3_32_tripletm_trans_aibjckdkai + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckdkai
    function v9_eom_cc3_32_tripletm_trans_aibjckdjai(b, c, k, d) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckdjai   
    integer, intent(in) :: b, c, k, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, k)
term(1) = term(1) + tvvvo(b, d, c, k)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckdjai = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckdjai = v9_eom_cc3_32_tripletm_trans_aibjckdjai + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckdjai
    function v9_eom_cc3_32_tripletm_trans_aibjckbibk(a, b, j, c) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbibk   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, j)



    v9_eom_cc3_32_tripletm_trans_aibjckbibk = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbibk = v9_eom_cc3_32_tripletm_trans_aibjckbibk + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbibk
    function v9_eom_cc3_32_tripletm_trans_aibjckbkbi(a, b, j, c) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbkbi   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, j)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckbkbi = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbkbi = v9_eom_cc3_32_tripletm_trans_aibjckbkbi + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbkbi
    function v9_eom_cc3_32_tripletm_trans_aibjckbjbi(a, b, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbjbi   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, k)



    v9_eom_cc3_32_tripletm_trans_aibjckbjbi = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbjbi = v9_eom_cc3_32_tripletm_trans_aibjckbjbi + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbjbi
    function v9_eom_cc3_32_tripletm_trans_aibjckbibj(a, b, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbibj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, k)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckbibj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbibj = v9_eom_cc3_32_tripletm_trans_aibjckbibj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbibj
    function v9_eom_cc3_32_tripletm_trans_aibjckbick(a, b, j) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbick   
    integer, intent(in) :: a, b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, j)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckbick = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbick = v9_eom_cc3_32_tripletm_trans_aibjckbick + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbick
    function v9_eom_cc3_32_tripletm_trans_aibjckbkci(a, j, c) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbkci   
    integer, intent(in) :: a, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, c, j)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckbkci = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbkci = v9_eom_cc3_32_tripletm_trans_aibjckbkci + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbkci
    function v9_eom_cc3_32_tripletm_trans_aibjckbjci(a, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbjci   
    integer, intent(in) :: a, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, c, k)



    v9_eom_cc3_32_tripletm_trans_aibjckbjci = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbjci = v9_eom_cc3_32_tripletm_trans_aibjckbjci + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbjci
    function v9_eom_cc3_32_tripletm_trans_aibjckbicj(a, b, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbicj   
    integer, intent(in) :: a, b, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, k)



    v9_eom_cc3_32_tripletm_trans_aibjckbicj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbicj = v9_eom_cc3_32_tripletm_trans_aibjckbicj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbicj
    function v9_eom_cc3_32_tripletm_trans_aibjckbkak(i, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbkak   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, k, i)



    v9_eom_cc3_32_tripletm_trans_aibjckbkak = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbkak = v9_eom_cc3_32_tripletm_trans_aibjckbkak + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbkak
    function v9_eom_cc3_32_tripletm_trans_aibjckbjak(i, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbjak   
    integer, intent(in) :: i, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckbjak = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbjak = v9_eom_cc3_32_tripletm_trans_aibjckbjak + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbjak
    function v9_eom_cc3_32_tripletm_trans_aibjckbkai(a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbkai   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, k, k)
term(1) = term(1) + tvooo(c, k, k, j)
term(2) = term(2) + tvooo(c, j, i, i)
term(3) = term(3) + tvvvo(b, b, c, j)
term(4) = term(4) + tvvvo(a, a, c, j)
term(5) = term(5) + tvvvo(c, b, b, j)

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckbkai = 0.d+0
    do s = 0, 5
    v9_eom_cc3_32_tripletm_trans_aibjckbkai = v9_eom_cc3_32_tripletm_trans_aibjckbkai + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbkai
    function v9_eom_cc3_32_tripletm_trans_aibjckbkaj(i, j, c) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbkaj   
    integer, intent(in) :: i, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)



    v9_eom_cc3_32_tripletm_trans_aibjckbkaj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbkaj = v9_eom_cc3_32_tripletm_trans_aibjckbkaj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbkaj
    function v9_eom_cc3_32_tripletm_trans_aibjckbiai(i, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbiai   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, i, k)
term(1) = term(1) + tvooo(c, k, i, j)

term(1) = term(1) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckbiai = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckbiai = v9_eom_cc3_32_tripletm_trans_aibjckbiai + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbiai
    function v9_eom_cc3_32_tripletm_trans_aibjckbjai(a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbjai   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, k)
term(1) = term(1) + tvooo(c, k, j, j)
term(2) = term(2) + tvooo(c, k, i, i)
term(3) = term(3) + tvvvo(c, b, b, k)
term(4) = term(4) + tvvvo(b, b, c, k)
term(5) = term(5) + tvvvo(a, a, c, k)

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckbjai = 0.d+0
    do s = 0, 5
    v9_eom_cc3_32_tripletm_trans_aibjckbjai = v9_eom_cc3_32_tripletm_trans_aibjckbjai + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbjai
    function v9_eom_cc3_32_tripletm_trans_aibjckbjaj(i, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckbjaj   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, j, i)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckbjaj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckbjaj = v9_eom_cc3_32_tripletm_trans_aibjckbjaj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckbjaj
    function v9_eom_cc3_32_tripletm_trans_aibjckakbk(i, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckakbk   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, k, i)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckakbk = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckakbk = v9_eom_cc3_32_tripletm_trans_aibjckakbk + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckakbk
    function v9_eom_cc3_32_tripletm_trans_aibjckaibk(a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckaibk   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, k, k)
term(1) = term(1) + tvooo(c, k, k, j)
term(2) = term(2) + tvooo(c, j, i, i)
term(3) = term(3) + tvvvo(b, b, c, j)
term(4) = term(4) + tvvvo(a, a, c, j)
term(5) = term(5) + tvvvo(c, b, b, j)

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckaibk = 0.d+0
    do s = 0, 5
    v9_eom_cc3_32_tripletm_trans_aibjckaibk = v9_eom_cc3_32_tripletm_trans_aibjckaibk + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckaibk
    function v9_eom_cc3_32_tripletm_trans_aibjckajbk(i, j, c) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckajbk   
    integer, intent(in) :: i, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckajbk = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckajbk = v9_eom_cc3_32_tripletm_trans_aibjckajbk + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckajbk
    function v9_eom_cc3_32_tripletm_trans_aibjckakbj(i, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckakbj   
    integer, intent(in) :: i, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)



    v9_eom_cc3_32_tripletm_trans_aibjckakbj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckakbj = v9_eom_cc3_32_tripletm_trans_aibjckakbj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckakbj
    function v9_eom_cc3_32_tripletm_trans_aibjckaibi(i, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckaibi   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, i, k)
term(1) = term(1) + tvooo(c, k, i, j)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckaibi = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckaibi = v9_eom_cc3_32_tripletm_trans_aibjckaibi + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckaibi
    function v9_eom_cc3_32_tripletm_trans_aibjckaibj(a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckaibj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, k)
term(1) = term(1) + tvooo(c, k, j, j)
term(2) = term(2) + tvooo(c, k, i, i)
term(3) = term(3) + tvvvo(c, b, b, k)
term(4) = term(4) + tvvvo(b, b, c, k)
term(5) = term(5) + tvvvo(a, a, c, k)

term(0) = term(0) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckaibj = 0.d+0
    do s = 0, 5
    v9_eom_cc3_32_tripletm_trans_aibjckaibj = v9_eom_cc3_32_tripletm_trans_aibjckaibj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckaibj
    function v9_eom_cc3_32_tripletm_trans_aibjckajbj(i, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckajbj   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, j, i)



    v9_eom_cc3_32_tripletm_trans_aibjckajbj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckajbj = v9_eom_cc3_32_tripletm_trans_aibjckajbj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckajbj
    function v9_eom_cc3_32_tripletm_trans_aibjckcick(a, b, j, c) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckcick   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, j)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckcick = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckcick = v9_eom_cc3_32_tripletm_trans_aibjckcick + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckcick
    function v9_eom_cc3_32_tripletm_trans_aibjckckci(a, b, j, c) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckckci   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, j)



    v9_eom_cc3_32_tripletm_trans_aibjckckci = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckckci = v9_eom_cc3_32_tripletm_trans_aibjckckci + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckckci
    function v9_eom_cc3_32_tripletm_trans_aibjckcjci(a, b, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckcjci   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, k)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckcjci = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckcjci = v9_eom_cc3_32_tripletm_trans_aibjckcjci + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckcjci
    function v9_eom_cc3_32_tripletm_trans_aibjckcicj(a, b, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckcicj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, k)



    v9_eom_cc3_32_tripletm_trans_aibjckcicj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckcicj = v9_eom_cc3_32_tripletm_trans_aibjckcicj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckcicj
    function v9_eom_cc3_32_tripletm_trans_aibjckckak(i, b, j, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckckak   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, k, i)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckckak = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckckak = v9_eom_cc3_32_tripletm_trans_aibjckckak + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckckak
    function v9_eom_cc3_32_tripletm_trans_aibjckcjak(i, b, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckcjak   
    integer, intent(in) :: i, b, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)



    v9_eom_cc3_32_tripletm_trans_aibjckcjak = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckcjak = v9_eom_cc3_32_tripletm_trans_aibjckcjak + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckcjak
    function v9_eom_cc3_32_tripletm_trans_aibjckckai(a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckckai   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, j)
term(1) = term(1) + tvooo(b, j, k, k)
term(2) = term(2) + tvooo(b, j, i, i)
term(3) = term(3) + tvvvo(b, c, c, j)
term(4) = term(4) + tvvvo(c, c, b, j)
term(5) = term(5) + tvvvo(a, a, b, j)

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckckai = 0.d+0
    do s = 0, 5
    v9_eom_cc3_32_tripletm_trans_aibjckckai = v9_eom_cc3_32_tripletm_trans_aibjckckai + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckckai
    function v9_eom_cc3_32_tripletm_trans_aibjckckaj(i, b, j) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckckaj   
    integer, intent(in) :: i, b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckckaj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckckaj = v9_eom_cc3_32_tripletm_trans_aibjckckaj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckckaj
    function v9_eom_cc3_32_tripletm_trans_aibjckciai(i, b, j, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckciai   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, i, j)
term(1) = term(1) + tvooo(b, j, i, k)

term(1) = term(1) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckciai = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckciai = v9_eom_cc3_32_tripletm_trans_aibjckciai + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckciai
    function v9_eom_cc3_32_tripletm_trans_aibjckcjai(a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckcjai   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, j, j)
term(1) = term(1) + tvooo(b, k, i, i)
term(2) = term(2) + tvooo(b, j, j, k)
term(3) = term(3) + tvvvo(c, c, b, k)
term(4) = term(4) + tvvvo(b, c, c, k)
term(5) = term(5) + tvvvo(a, a, b, k)

term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckcjai = 0.d+0
    do s = 0, 5
    v9_eom_cc3_32_tripletm_trans_aibjckcjai = v9_eom_cc3_32_tripletm_trans_aibjckcjai + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckcjai
    function v9_eom_cc3_32_tripletm_trans_aibjckcjaj(i, b, j, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckcjaj   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, j, i)



    v9_eom_cc3_32_tripletm_trans_aibjckcjaj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckcjaj = v9_eom_cc3_32_tripletm_trans_aibjckcjaj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckcjaj
    function v9_eom_cc3_32_tripletm_trans_aibjckakck(i, b, j, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckakck   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, k, i)



    v9_eom_cc3_32_tripletm_trans_aibjckakck = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckakck = v9_eom_cc3_32_tripletm_trans_aibjckakck + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckakck
    function v9_eom_cc3_32_tripletm_trans_aibjckaick(a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckaick   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, j)
term(1) = term(1) + tvooo(b, j, k, k)
term(2) = term(2) + tvooo(b, j, i, i)
term(3) = term(3) + tvvvo(b, c, c, j)
term(4) = term(4) + tvvvo(c, c, b, j)
term(5) = term(5) + tvvvo(a, a, b, j)

term(0) = term(0) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckaick = 0.d+0
    do s = 0, 5
    v9_eom_cc3_32_tripletm_trans_aibjckaick = v9_eom_cc3_32_tripletm_trans_aibjckaick + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckaick
    function v9_eom_cc3_32_tripletm_trans_aibjckajck(i, b, j) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckajck   
    integer, intent(in) :: i, b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)



    v9_eom_cc3_32_tripletm_trans_aibjckajck = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckajck = v9_eom_cc3_32_tripletm_trans_aibjckajck + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckajck
    function v9_eom_cc3_32_tripletm_trans_aibjckakcj(i, b, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckakcj   
    integer, intent(in) :: i, b, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckakcj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckakcj = v9_eom_cc3_32_tripletm_trans_aibjckakcj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckakcj
    function v9_eom_cc3_32_tripletm_trans_aibjckaici(i, b, j, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckaici   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, i, j)
term(1) = term(1) + tvooo(b, j, i, k)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckaici = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckaici = v9_eom_cc3_32_tripletm_trans_aibjckaici + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckaici
    function v9_eom_cc3_32_tripletm_trans_aibjckaicj(a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckaicj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, j, j)
term(1) = term(1) + tvooo(b, k, i, i)
term(2) = term(2) + tvooo(b, j, j, k)
term(3) = term(3) + tvvvo(c, c, b, k)
term(4) = term(4) + tvvvo(b, c, c, k)
term(5) = term(5) + tvvvo(a, a, b, k)

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckaicj = 0.d+0
    do s = 0, 5
    v9_eom_cc3_32_tripletm_trans_aibjckaicj = v9_eom_cc3_32_tripletm_trans_aibjckaicj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckaicj
    function v9_eom_cc3_32_tripletm_trans_aibjckajcj(i, b, j, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckajcj   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, j, i)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckajcj = 0.d+0
    do s = 0, 0
    v9_eom_cc3_32_tripletm_trans_aibjckajcj = v9_eom_cc3_32_tripletm_trans_aibjckajcj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckajcj
    function v9_eom_cc3_32_tripletm_trans_aibjckaiak(a, b, j, c) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckaiak   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, j)
term(1) = term(1) + tvvvo(c, a, b, j)

term(1) = term(1) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckaiak = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckaiak = v9_eom_cc3_32_tripletm_trans_aibjckaiak + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckaiak
    function v9_eom_cc3_32_tripletm_trans_aibjckakai(a, b, j, c) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckakai   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, j)
term(1) = term(1) + tvvvo(c, a, b, j)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckakai = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckakai = v9_eom_cc3_32_tripletm_trans_aibjckakai + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckakai
    function v9_eom_cc3_32_tripletm_trans_aibjckajai(a, b, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckajai   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, k)
term(1) = term(1) + tvvvo(b, a, c, k)

term(0) = term(0) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckajai = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckajai = v9_eom_cc3_32_tripletm_trans_aibjckajai + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckajai
    function v9_eom_cc3_32_tripletm_trans_aibjckaiaj(a, b, c, k) 
    real(F64) :: v9_eom_cc3_32_tripletm_trans_aibjckaiaj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, k)
term(1) = term(1) + tvvvo(b, a, c, k)

term(1) = term(1) * (-1.0d+0) 


    v9_eom_cc3_32_tripletm_trans_aibjckaiaj = 0.d+0
    do s = 0, 1
    v9_eom_cc3_32_tripletm_trans_aibjckaiaj = v9_eom_cc3_32_tripletm_trans_aibjckaiaj + term(s)
    end do

    end function v9_eom_cc3_32_tripletm_trans_aibjckaiaj
    end module v9_eom_cc3_32_tripletm_trans
    