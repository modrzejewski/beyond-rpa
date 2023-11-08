module eom_cc3_32_tripletm_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_cc3_32_tripletm_trans_aibjckaibm(j, c, k, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckaibm   
    integer, intent(in) :: j, c, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, k)
term(1) = term(1) + tvooo(c, k, m, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckaibm = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjckaibm = eom_cc3_32_tripletm_trans_aibjckaibm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckaibm
    function eom_cc3_32_tripletm_trans_aibjckalbj(i, c, k, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckalbj   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, l, i)



    eom_cc3_32_tripletm_trans_aibjckalbj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckalbj = eom_cc3_32_tripletm_trans_aibjckalbj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckalbj
    function eom_cc3_32_tripletm_trans_aibjckalbk(i, j, c, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckalbk   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckalbk = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckalbk = eom_cc3_32_tripletm_trans_aibjckalbk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckalbk
    function eom_cc3_32_tripletm_trans_aibjckaicm(b, j, k, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckaicm   
    integer, intent(in) :: b, j, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, m, j)
term(1) = term(1) + tvooo(b, j, m, k)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckaicm = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjckaicm = eom_cc3_32_tripletm_trans_aibjckaicm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckaicm
    function eom_cc3_32_tripletm_trans_aibjckalcj(i, b, k, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckalcj   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckalcj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckalcj = eom_cc3_32_tripletm_trans_aibjckalcj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckalcj
    function eom_cc3_32_tripletm_trans_aibjckalck(i, b, j, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckalck   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, i)



    eom_cc3_32_tripletm_trans_aibjckalck = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckalck = eom_cc3_32_tripletm_trans_aibjckalck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckalck
    function eom_cc3_32_tripletm_trans_aibjckaiej(b, c, k, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckaiej   
    integer, intent(in) :: b, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, k)
term(1) = term(1) + tvvvo(b, e, c, k)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibjckaiej = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjckaiej = eom_cc3_32_tripletm_trans_aibjckaiej + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckaiej
    function eom_cc3_32_tripletm_trans_aibjckaiek(b, j, c, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckaiek   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, j)
term(1) = term(1) + tvvvo(b, e, c, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckaiek = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjckaiek = eom_cc3_32_tripletm_trans_aibjckaiek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckaiek
    function eom_cc3_32_tripletm_trans_aibjckblai(j, c, k, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckblai   
    integer, intent(in) :: j, c, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, k)
term(1) = term(1) + tvooo(c, k, l, j)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibjckblai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjckblai = eom_cc3_32_tripletm_trans_aibjckblai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckblai
    function eom_cc3_32_tripletm_trans_aibjckbjam(i, c, k, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckbjam   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckbjam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckbjam = eom_cc3_32_tripletm_trans_aibjckbjam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckbjam
    function eom_cc3_32_tripletm_trans_aibjckbkam(i, j, c, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckbkam   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, i)



    eom_cc3_32_tripletm_trans_aibjckbkam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckbkam = eom_cc3_32_tripletm_trans_aibjckbkam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckbkam
    function eom_cc3_32_tripletm_trans_aibjckbjei(a, c, k, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckbjei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, k)



    eom_cc3_32_tripletm_trans_aibjckbjei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckbjei = eom_cc3_32_tripletm_trans_aibjckbjei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckbjei
    function eom_cc3_32_tripletm_trans_aibjckbkei(a, j, c, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckbkei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckbkei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckbkei = eom_cc3_32_tripletm_trans_aibjckbkei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckbkei
    function eom_cc3_32_tripletm_trans_aibjckclai(b, j, k, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckclai   
    integer, intent(in) :: b, j, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, j)
term(1) = term(1) + tvooo(b, j, l, k)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibjckclai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjckclai = eom_cc3_32_tripletm_trans_aibjckclai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckclai
    function eom_cc3_32_tripletm_trans_aibjckcjam(i, b, k, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckcjam   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, m, i)



    eom_cc3_32_tripletm_trans_aibjckcjam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckcjam = eom_cc3_32_tripletm_trans_aibjckcjam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckcjam
    function eom_cc3_32_tripletm_trans_aibjckckam(i, b, j, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckckam   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckckam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckckam = eom_cc3_32_tripletm_trans_aibjckckam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckckam
    function eom_cc3_32_tripletm_trans_aibjckcjei(a, b, k, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckcjei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, k)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckcjei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckcjei = eom_cc3_32_tripletm_trans_aibjckcjei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckcjei
    function eom_cc3_32_tripletm_trans_aibjckckei(a, b, j, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckckei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, j)



    eom_cc3_32_tripletm_trans_aibjckckei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckckei = eom_cc3_32_tripletm_trans_aibjckckei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckckei
    function eom_cc3_32_tripletm_trans_aibjckdjai(b, c, k, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckdjai   
    integer, intent(in) :: b, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, k)
term(1) = term(1) + tvvvo(b, d, c, k)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckdjai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjckdjai = eom_cc3_32_tripletm_trans_aibjckdjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckdjai
    function eom_cc3_32_tripletm_trans_aibjckdkai(b, j, c, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckdkai   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, j)
term(1) = term(1) + tvvvo(b, d, c, j)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibjckdkai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjckdkai = eom_cc3_32_tripletm_trans_aibjckdkai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckdkai
    function eom_cc3_32_tripletm_trans_aibjckdibj(a, c, k, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckdibj   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, k)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckdibj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckdibj = eom_cc3_32_tripletm_trans_aibjckdibj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckdibj
    function eom_cc3_32_tripletm_trans_aibjckdibk(a, j, c, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckdibk   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, j)



    eom_cc3_32_tripletm_trans_aibjckdibk = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckdibk = eom_cc3_32_tripletm_trans_aibjckdibk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckdibk
    function eom_cc3_32_tripletm_trans_aibjckdicj(a, b, k, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckdicj   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, k)



    eom_cc3_32_tripletm_trans_aibjckdicj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckdicj = eom_cc3_32_tripletm_trans_aibjckdicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckdicj
    function eom_cc3_32_tripletm_trans_aibjckdick(a, b, j, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckdick   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckdick = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckdick = eom_cc3_32_tripletm_trans_aibjckdick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckdick
    function eom_cc3_32_tripletm_trans_aiajckaicm(a, j, k, m) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckaicm   
    integer, intent(in) :: a, j, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, m, j)
term(1) = term(1) + tvooo(a, j, m, k)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajckaicm = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aiajckaicm = eom_cc3_32_tripletm_trans_aiajckaicm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckaicm
    function eom_cc3_32_tripletm_trans_aiajckalcj(a, i, k, l) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckalcj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, l, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajckalcj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckalcj = eom_cc3_32_tripletm_trans_aiajckalcj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckalcj
    function eom_cc3_32_tripletm_trans_aiajckalck(a, i, j, l) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckalck   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, i)



    eom_cc3_32_tripletm_trans_aiajckalck = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckalck = eom_cc3_32_tripletm_trans_aiajckalck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckalck
    function eom_cc3_32_tripletm_trans_aiajckajei(a, c, k, e) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckajei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, k)



    eom_cc3_32_tripletm_trans_aiajckajei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckajei = eom_cc3_32_tripletm_trans_aiajckajei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckajei
    function eom_cc3_32_tripletm_trans_aiajckakei(a, j, c, e) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckakei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajckakei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckakei = eom_cc3_32_tripletm_trans_aiajckakei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckakei
    function eom_cc3_32_tripletm_trans_aiajckaiej(a, c, k, e) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckaiej   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, k)
term(1) = term(1) + tvvvo(a, e, c, k)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aiajckaiej = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aiajckaiej = eom_cc3_32_tripletm_trans_aiajckaiej + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckaiej
    function eom_cc3_32_tripletm_trans_aiajckaiek(a, j, c, e) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckaiek   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, j)
term(1) = term(1) + tvvvo(a, e, c, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajckaiek = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aiajckaiek = eom_cc3_32_tripletm_trans_aiajckaiek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckaiek
    function eom_cc3_32_tripletm_trans_aibjakajei(a, b, k, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakajei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, k)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjakajei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakajei = eom_cc3_32_tripletm_trans_aibjakajei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakajei
    function eom_cc3_32_tripletm_trans_aibjakakei(a, b, j, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakakei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, j)



    eom_cc3_32_tripletm_trans_aibjakakei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakakei = eom_cc3_32_tripletm_trans_aibjakakei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakakei
    function eom_cc3_32_tripletm_trans_aibjakaiej(a, b, k, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakaiej   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, k)
term(1) = term(1) + tvvvo(b, e, a, k)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibjakaiej = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjakaiej = eom_cc3_32_tripletm_trans_aibjakaiej + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakaiej
    function eom_cc3_32_tripletm_trans_aibjakaiek(a, b, j, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakaiek   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, j)
term(1) = term(1) + tvvvo(b, e, a, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjakaiek = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjakaiek = eom_cc3_32_tripletm_trans_aibjakaiek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakaiek
    function eom_cc3_32_tripletm_trans_aibickalbi(i, c, k, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibickalbi   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, l, i)



    eom_cc3_32_tripletm_trans_aibickalbi = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickalbi = eom_cc3_32_tripletm_trans_aibickalbi + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickalbi
    function eom_cc3_32_tripletm_trans_aibickaibm(i, c, k, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibickaibm   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, k)
term(1) = term(1) + tvooo(c, k, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibickaibm = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibickaibm = eom_cc3_32_tripletm_trans_aibickaibm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickaibm
    function eom_cc3_32_tripletm_trans_aibickalbk(i, c, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibickalbk   
    integer, intent(in) :: i, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibickalbk = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickalbk = eom_cc3_32_tripletm_trans_aibickalbk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickalbk
    function eom_cc3_32_tripletm_trans_aibjcialbi(i, j, c, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcialbi   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjcialbi = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcialbi = eom_cc3_32_tripletm_trans_aibjcialbi + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcialbi
    function eom_cc3_32_tripletm_trans_aibjciaibm(i, j, c, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjciaibm   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, i)
term(1) = term(1) + tvooo(c, i, m, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjciaibm = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjciaibm = eom_cc3_32_tripletm_trans_aibjciaibm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjciaibm
    function eom_cc3_32_tripletm_trans_aibjcialbj(i, c, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcialbj   
    integer, intent(in) :: i, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, i)



    eom_cc3_32_tripletm_trans_aibjcialbj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcialbj = eom_cc3_32_tripletm_trans_aibjcialbj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcialbj
    function eom_cc3_32_tripletm_trans_aibjckaibj(a, i, b, j, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckaibj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, k)
term(1) = term(1) + tvooo(c, k, j, j)
term(2) = term(2) + tvooo(c, k, i, i)
term(3) = term(3) + tvvvo(c, b, b, k)
term(4) = term(4) + tvvvo(b, b, c, k)
term(5) = term(5) + tvvvo(a, a, c, k)

term(0) = -term(0) 
term(4) = -term(4) 
term(5) = -term(5) 


    eom_cc3_32_tripletm_trans_aibjckaibj = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjckaibj = eom_cc3_32_tripletm_trans_aibjckaibj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckaibj
    function eom_cc3_32_tripletm_trans_aibjckaibk(a, i, b, j, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckaibk   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, k, k)
term(1) = term(1) + tvooo(c, j, i, i)
term(2) = term(2) + tvooo(c, k, k, j)
term(3) = term(3) + tvvvo(c, b, b, j)
term(4) = term(4) + tvvvo(b, b, c, j)
term(5) = term(5) + tvvvo(a, a, c, j)

term(0) = -term(0) 
term(1) = -term(1) 
term(3) = -term(3) 


    eom_cc3_32_tripletm_trans_aibjckaibk = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjckaibk = eom_cc3_32_tripletm_trans_aibjckaibk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckaibk
    function eom_cc3_32_tripletm_trans_aibjckakbj(i, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckakbj   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)



    eom_cc3_32_tripletm_trans_aibjckakbj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckakbj = eom_cc3_32_tripletm_trans_aibjckakbj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckakbj
    function eom_cc3_32_tripletm_trans_aibjckajbk(i, j, c) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckajbk   
    integer, intent(in) :: i, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckajbk = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckajbk = eom_cc3_32_tripletm_trans_aibjckajbk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckajbk
    function eom_cc3_32_tripletm_trans_aibickalci(i, b, k, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibickalci   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibickalci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickalci = eom_cc3_32_tripletm_trans_aibickalci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickalci
    function eom_cc3_32_tripletm_trans_aibickaicm(i, b, k, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibickaicm   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, m, i)
term(1) = term(1) + tvooo(b, i, m, k)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibickaicm = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibickaicm = eom_cc3_32_tripletm_trans_aibickaicm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickaicm
    function eom_cc3_32_tripletm_trans_aibickalck(i, b, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibickalck   
    integer, intent(in) :: i, b, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, i)



    eom_cc3_32_tripletm_trans_aibickalck = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickalck = eom_cc3_32_tripletm_trans_aibickalck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickalck
    function eom_cc3_32_tripletm_trans_aibjcialci(i, b, j, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcialci   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, i)



    eom_cc3_32_tripletm_trans_aibjcialci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcialci = eom_cc3_32_tripletm_trans_aibjcialci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcialci
    function eom_cc3_32_tripletm_trans_aibjciaicm(i, b, j, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjciaicm   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, j)
term(1) = term(1) + tvooo(b, j, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjciaicm = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjciaicm = eom_cc3_32_tripletm_trans_aibjciaicm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjciaicm
    function eom_cc3_32_tripletm_trans_aibjcialcj(i, b, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcialcj   
    integer, intent(in) :: i, b, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjcialcj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcialcj = eom_cc3_32_tripletm_trans_aibjcialcj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcialcj
    function eom_cc3_32_tripletm_trans_aibjckaicj(a, i, b, j, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckaicj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, j, j)
term(1) = term(1) + tvooo(b, j, j, k)
term(2) = term(2) + tvooo(b, k, i, i)
term(3) = term(3) + tvvvo(c, c, b, k)
term(4) = term(4) + tvvvo(a, a, b, k)
term(5) = term(5) + tvvvo(b, c, c, k)

term(0) = -term(0) 
term(2) = -term(2) 
term(5) = -term(5) 


    eom_cc3_32_tripletm_trans_aibjckaicj = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjckaicj = eom_cc3_32_tripletm_trans_aibjckaicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckaicj
    function eom_cc3_32_tripletm_trans_aibjckaick(a, i, b, j, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckaick   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, j)
term(1) = term(1) + tvooo(b, j, k, k)
term(2) = term(2) + tvooo(b, j, i, i)
term(3) = term(3) + tvvvo(c, c, b, j)
term(4) = term(4) + tvvvo(b, c, c, j)
term(5) = term(5) + tvvvo(a, a, b, j)

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = -term(5) 


    eom_cc3_32_tripletm_trans_aibjckaick = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjckaick = eom_cc3_32_tripletm_trans_aibjckaick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckaick
    function eom_cc3_32_tripletm_trans_aibjckakcj(i, b, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckakcj   
    integer, intent(in) :: i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckakcj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckakcj = eom_cc3_32_tripletm_trans_aibjckakcj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckakcj
    function eom_cc3_32_tripletm_trans_aibjckajck(i, b, j) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckajck   
    integer, intent(in) :: i, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)



    eom_cc3_32_tripletm_trans_aibjckajck = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckajck = eom_cc3_32_tripletm_trans_aibjckajck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckajck
    function eom_cc3_32_tripletm_trans_aibickaiek(i, b, c, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibickaiek   
    integer, intent(in) :: i, b, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, i)
term(1) = term(1) + tvvvo(b, e, c, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibickaiek = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibickaiek = eom_cc3_32_tripletm_trans_aibickaiek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickaiek
    function eom_cc3_32_tripletm_trans_aibjciaiej(i, b, c, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjciaiej   
    integer, intent(in) :: i, b, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, i)
term(1) = term(1) + tvvvo(b, e, c, i)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibjciaiej = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjciaiej = eom_cc3_32_tripletm_trans_aibjciaiej + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjciaiej
    function eom_cc3_32_tripletm_trans_aibjakblai(a, j, k, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakblai   
    integer, intent(in) :: a, j, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, k)
term(1) = term(1) + tvooo(a, k, l, j)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibjakblai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjakblai = eom_cc3_32_tripletm_trans_aibjakblai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakblai
    function eom_cc3_32_tripletm_trans_aibjakbjam(a, i, k, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakbjam   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjakbjam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakbjam = eom_cc3_32_tripletm_trans_aibjakbjam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakbjam
    function eom_cc3_32_tripletm_trans_aibjakbkam(a, i, j, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakbkam   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, i)



    eom_cc3_32_tripletm_trans_aibjakbkam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakbkam = eom_cc3_32_tripletm_trans_aibjakbkam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakbkam
    function eom_cc3_32_tripletm_trans_aibjakbjei(a, k, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakbjei   
    integer, intent(in) :: a, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, k)



    eom_cc3_32_tripletm_trans_aibjakbjei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakbjei = eom_cc3_32_tripletm_trans_aibjakbjei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakbjei
    function eom_cc3_32_tripletm_trans_aibjakbkei(a, j, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakbkei   
    integer, intent(in) :: a, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjakbkei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakbkei = eom_cc3_32_tripletm_trans_aibjakbkei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakbkei
    function eom_cc3_32_tripletm_trans_aibickblai(i, c, k, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibickblai   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, k)
term(1) = term(1) + tvooo(c, k, l, i)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibickblai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibickblai = eom_cc3_32_tripletm_trans_aibickblai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickblai
    function eom_cc3_32_tripletm_trans_aibickbiam(i, c, k, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibickbiam   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibickbiam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickbiam = eom_cc3_32_tripletm_trans_aibickbiam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickbiam
    function eom_cc3_32_tripletm_trans_aibickbkam(i, c, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibickbkam   
    integer, intent(in) :: i, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, i)



    eom_cc3_32_tripletm_trans_aibickbkam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickbkam = eom_cc3_32_tripletm_trans_aibickbkam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickbkam
    function eom_cc3_32_tripletm_trans_aibjciblai(i, j, c, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjciblai   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, i)
term(1) = term(1) + tvooo(c, i, l, j)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibjciblai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjciblai = eom_cc3_32_tripletm_trans_aibjciblai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjciblai
    function eom_cc3_32_tripletm_trans_aibjckbjai(a, i, b, j, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckbjai   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, k)
term(1) = term(1) + tvooo(c, k, j, j)
term(2) = term(2) + tvooo(c, k, i, i)
term(3) = term(3) + tvvvo(c, b, b, k)
term(4) = term(4) + tvvvo(b, b, c, k)
term(5) = term(5) + tvvvo(a, a, c, k)

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 


    eom_cc3_32_tripletm_trans_aibjckbjai = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjckbjai = eom_cc3_32_tripletm_trans_aibjckbjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckbjai
    function eom_cc3_32_tripletm_trans_aibjckbkai(a, i, b, j, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckbkai   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, k, k)
term(1) = term(1) + tvooo(c, j, i, i)
term(2) = term(2) + tvooo(c, k, k, j)
term(3) = term(3) + tvvvo(c, b, b, j)
term(4) = term(4) + tvvvo(b, b, c, j)
term(5) = term(5) + tvvvo(a, a, c, j)

term(2) = -term(2) 
term(4) = -term(4) 
term(5) = -term(5) 


    eom_cc3_32_tripletm_trans_aibjckbkai = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjckbkai = eom_cc3_32_tripletm_trans_aibjckbkai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckbkai
    function eom_cc3_32_tripletm_trans_aibjcibiam(i, j, c, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcibiam   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, i)



    eom_cc3_32_tripletm_trans_aibjcibiam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcibiam = eom_cc3_32_tripletm_trans_aibjcibiam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcibiam
    function eom_cc3_32_tripletm_trans_aibjcibjam(i, c, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcibjam   
    integer, intent(in) :: i, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjcibjam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcibjam = eom_cc3_32_tripletm_trans_aibjcibjam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcibjam
    function eom_cc3_32_tripletm_trans_aibjckbkaj(i, j, c) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckbkaj   
    integer, intent(in) :: i, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)



    eom_cc3_32_tripletm_trans_aibjckbkaj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckbkaj = eom_cc3_32_tripletm_trans_aibjckbkaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckbkaj
    function eom_cc3_32_tripletm_trans_aibjckbjak(i, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckbjak   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckbjak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckbjak = eom_cc3_32_tripletm_trans_aibjckbjak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckbjak
    function eom_cc3_32_tripletm_trans_aibjckbjci(a, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckbjci   
    integer, intent(in) :: a, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, c, k)



    eom_cc3_32_tripletm_trans_aibjckbjci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckbjci = eom_cc3_32_tripletm_trans_aibjckbjci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckbjci
    function eom_cc3_32_tripletm_trans_aibjckbkci(a, j, c) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckbkci   
    integer, intent(in) :: a, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, c, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckbkci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckbkci = eom_cc3_32_tripletm_trans_aibjckbkci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckbkci
    function eom_cc3_32_tripletm_trans_aibjckbicj(a, b, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckbicj   
    integer, intent(in) :: a, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, k)



    eom_cc3_32_tripletm_trans_aibjckbicj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckbicj = eom_cc3_32_tripletm_trans_aibjckbicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckbicj
    function eom_cc3_32_tripletm_trans_aibjckbick(a, b, j) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckbick   
    integer, intent(in) :: a, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckbick = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckbick = eom_cc3_32_tripletm_trans_aibjckbick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckbick
    function eom_cc3_32_tripletm_trans_aibickbkei(a, i, c, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibickbkei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibickbkei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickbkei = eom_cc3_32_tripletm_trans_aibickbkei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickbkei
    function eom_cc3_32_tripletm_trans_aibjcibjei(a, i, c, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcibjei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, i)



    eom_cc3_32_tripletm_trans_aibjcibjei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcibjei = eom_cc3_32_tripletm_trans_aibjcibjei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcibjei
    function eom_cc3_32_tripletm_trans_aiajckcjei(a, k, e) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckcjei   
    integer, intent(in) :: a, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, k)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajckcjei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckcjei = eom_cc3_32_tripletm_trans_aiajckcjei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckcjei
    function eom_cc3_32_tripletm_trans_aiajckckei(a, j, e) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckckei   
    integer, intent(in) :: a, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, j)



    eom_cc3_32_tripletm_trans_aiajckckei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckckei = eom_cc3_32_tripletm_trans_aiajckckei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckckei
    function eom_cc3_32_tripletm_trans_aibickclai(i, b, k, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibickclai   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, i)
term(1) = term(1) + tvooo(b, i, l, k)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibickclai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibickclai = eom_cc3_32_tripletm_trans_aibickclai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickclai
    function eom_cc3_32_tripletm_trans_aibickciam(i, b, k, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibickciam   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, m, i)



    eom_cc3_32_tripletm_trans_aibickciam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickciam = eom_cc3_32_tripletm_trans_aibickciam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickciam
    function eom_cc3_32_tripletm_trans_aibickckam(i, b, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibickckam   
    integer, intent(in) :: i, b, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibickckam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickckam = eom_cc3_32_tripletm_trans_aibickckam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickckam
    function eom_cc3_32_tripletm_trans_aibjciclai(i, b, j, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjciclai   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, j)
term(1) = term(1) + tvooo(b, j, l, i)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibjciclai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjciclai = eom_cc3_32_tripletm_trans_aibjciclai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjciclai
    function eom_cc3_32_tripletm_trans_aibjckcjai(a, i, b, j, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckcjai   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, j, j)
term(1) = term(1) + tvooo(b, j, j, k)
term(2) = term(2) + tvooo(b, k, i, i)
term(3) = term(3) + tvvvo(c, c, b, k)
term(4) = term(4) + tvvvo(a, a, b, k)
term(5) = term(5) + tvvvo(b, c, c, k)

term(1) = -term(1) 
term(3) = -term(3) 
term(4) = -term(4) 


    eom_cc3_32_tripletm_trans_aibjckcjai = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjckcjai = eom_cc3_32_tripletm_trans_aibjckcjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckcjai
    function eom_cc3_32_tripletm_trans_aibjckckai(a, i, b, j, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckckai   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, j)
term(1) = term(1) + tvooo(b, j, k, k)
term(2) = term(2) + tvooo(b, j, i, i)
term(3) = term(3) + tvvvo(c, c, b, j)
term(4) = term(4) + tvvvo(b, c, c, j)
term(5) = term(5) + tvvvo(a, a, b, j)

term(1) = -term(1) 
term(2) = -term(2) 
term(4) = -term(4) 


    eom_cc3_32_tripletm_trans_aibjckckai = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjckckai = eom_cc3_32_tripletm_trans_aibjckckai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckckai
    function eom_cc3_32_tripletm_trans_aibjciciam(i, b, j, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjciciam   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjciciam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjciciam = eom_cc3_32_tripletm_trans_aibjciciam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjciciam
    function eom_cc3_32_tripletm_trans_aibjcicjam(i, b, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcicjam   
    integer, intent(in) :: i, b, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, i)



    eom_cc3_32_tripletm_trans_aibjcicjam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcicjam = eom_cc3_32_tripletm_trans_aibjcicjam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcicjam
    function eom_cc3_32_tripletm_trans_aibjckckaj(i, b, j) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckckaj   
    integer, intent(in) :: i, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjckckaj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckckaj = eom_cc3_32_tripletm_trans_aibjckckaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckckaj
    function eom_cc3_32_tripletm_trans_aibjckcjak(i, b, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjckcjak   
    integer, intent(in) :: i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)



    eom_cc3_32_tripletm_trans_aibjckcjak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjckcjak = eom_cc3_32_tripletm_trans_aibjckcjak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjckcjak
    function eom_cc3_32_tripletm_trans_aibickckei(a, i, b, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibickckei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, i)



    eom_cc3_32_tripletm_trans_aibickckei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickckei = eom_cc3_32_tripletm_trans_aibickckei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickckei
    function eom_cc3_32_tripletm_trans_aibjcicjei(a, i, b, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcicjei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjcicjei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcicjei = eom_cc3_32_tripletm_trans_aibjcicjei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcicjei
    function eom_cc3_32_tripletm_trans_aiajckdjai(a, c, k, d) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckdjai   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, k)
term(1) = term(1) + tvvvo(a, d, c, k)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajckdjai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aiajckdjai = eom_cc3_32_tripletm_trans_aiajckdjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckdjai
    function eom_cc3_32_tripletm_trans_aiajckdkai(a, j, c, d) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckdkai   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, j)
term(1) = term(1) + tvvvo(a, d, c, j)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aiajckdkai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aiajckdkai = eom_cc3_32_tripletm_trans_aiajckdkai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckdkai
    function eom_cc3_32_tripletm_trans_aiajckdiaj(a, c, k, d) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckdiaj   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, k)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajckdiaj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckdiaj = eom_cc3_32_tripletm_trans_aiajckdiaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckdiaj
    function eom_cc3_32_tripletm_trans_aiajckdiak(a, j, c, d) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckdiak   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, j)



    eom_cc3_32_tripletm_trans_aiajckdiak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckdiak = eom_cc3_32_tripletm_trans_aiajckdiak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckdiak
    function eom_cc3_32_tripletm_trans_aiajckdicj(a, k, d) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckdicj   
    integer, intent(in) :: a, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, k)



    eom_cc3_32_tripletm_trans_aiajckdicj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckdicj = eom_cc3_32_tripletm_trans_aiajckdicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckdicj
    function eom_cc3_32_tripletm_trans_aiajckdick(a, j, d) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckdick   
    integer, intent(in) :: a, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajckdick = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckdick = eom_cc3_32_tripletm_trans_aiajckdick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckdick
    function eom_cc3_32_tripletm_trans_aibjakdjai(a, b, k, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakdjai   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, k)
term(1) = term(1) + tvvvo(b, d, a, k)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjakdjai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjakdjai = eom_cc3_32_tripletm_trans_aibjakdjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakdjai
    function eom_cc3_32_tripletm_trans_aibjakdkai(a, b, j, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakdkai   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, j)
term(1) = term(1) + tvvvo(b, d, a, j)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibjakdkai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjakdkai = eom_cc3_32_tripletm_trans_aibjakdkai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakdkai
    function eom_cc3_32_tripletm_trans_aibjakdiaj(a, b, k, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakdiaj   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, k)



    eom_cc3_32_tripletm_trans_aibjakdiaj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakdiaj = eom_cc3_32_tripletm_trans_aibjakdiaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakdiaj
    function eom_cc3_32_tripletm_trans_aibjakdiak(a, b, j, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakdiak   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjakdiak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakdiak = eom_cc3_32_tripletm_trans_aibjakdiak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakdiak
    function eom_cc3_32_tripletm_trans_aibjakdibj(a, k, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakdibj   
    integer, intent(in) :: a, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, k)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjakdibj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakdibj = eom_cc3_32_tripletm_trans_aibjakdibj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakdibj
    function eom_cc3_32_tripletm_trans_aibjakdibk(a, j, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakdibk   
    integer, intent(in) :: a, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, j)



    eom_cc3_32_tripletm_trans_aibjakdibk = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakdibk = eom_cc3_32_tripletm_trans_aibjakdibk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakdibk
    function eom_cc3_32_tripletm_trans_aibickdkai(i, b, c, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibickdkai   
    integer, intent(in) :: i, b, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, i)
term(1) = term(1) + tvvvo(b, d, c, i)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibickdkai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibickdkai = eom_cc3_32_tripletm_trans_aibickdkai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickdkai
    function eom_cc3_32_tripletm_trans_aibjcidjai(i, b, c, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcidjai   
    integer, intent(in) :: i, b, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, i)
term(1) = term(1) + tvvvo(b, d, c, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjcidjai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjcidjai = eom_cc3_32_tripletm_trans_aibjcidjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcidjai
    function eom_cc3_32_tripletm_trans_aibickdibk(a, i, c, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibickdibk   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, i)



    eom_cc3_32_tripletm_trans_aibickdibk = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickdibk = eom_cc3_32_tripletm_trans_aibickdibk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickdibk
    function eom_cc3_32_tripletm_trans_aibjcidibj(a, i, c, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcidibj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjcidibj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcidibj = eom_cc3_32_tripletm_trans_aibjcidibj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcidibj
    function eom_cc3_32_tripletm_trans_aibickdick(a, i, b, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibickdick   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibickdick = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickdick = eom_cc3_32_tripletm_trans_aibickdick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickdick
    function eom_cc3_32_tripletm_trans_aibjcidicj(a, i, b, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcidicj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, i)



    eom_cc3_32_tripletm_trans_aibjcidicj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcidicj = eom_cc3_32_tripletm_trans_aibjcidicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcidicj
    function eom_cc3_32_tripletm_trans_aiaickalci(a, i, k, l) 
    double precision :: eom_cc3_32_tripletm_trans_aiaickalci   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, l, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiaickalci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiaickalci = eom_cc3_32_tripletm_trans_aiaickalci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiaickalci
    function eom_cc3_32_tripletm_trans_aiaickaicm(a, i, k, m) 
    double precision :: eom_cc3_32_tripletm_trans_aiaickaicm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, m, i)
term(1) = term(1) + tvooo(a, i, m, k)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiaickaicm = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aiaickaicm = eom_cc3_32_tripletm_trans_aiaickaicm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiaickaicm
    function eom_cc3_32_tripletm_trans_aiaickalck(a, i, l) 
    double precision :: eom_cc3_32_tripletm_trans_aiaickalck   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)



    eom_cc3_32_tripletm_trans_aiaickalck = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiaickalck = eom_cc3_32_tripletm_trans_aiaickalck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiaickalck
    function eom_cc3_32_tripletm_trans_aiajcialci(a, i, j, l) 
    double precision :: eom_cc3_32_tripletm_trans_aiajcialci   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, i)



    eom_cc3_32_tripletm_trans_aiajcialci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajcialci = eom_cc3_32_tripletm_trans_aiajcialci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajcialci
    function eom_cc3_32_tripletm_trans_aiajckajci(a, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckajci   
    integer, intent(in) :: a, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, c, k)



    eom_cc3_32_tripletm_trans_aiajckajci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckajci = eom_cc3_32_tripletm_trans_aiajckajci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckajci
    function eom_cc3_32_tripletm_trans_aiajckakci(a, j, c) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckakci   
    integer, intent(in) :: a, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, c, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajckakci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckakci = eom_cc3_32_tripletm_trans_aiajckakci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckakci
    function eom_cc3_32_tripletm_trans_aiajciaicm(a, i, j, m) 
    double precision :: eom_cc3_32_tripletm_trans_aiajciaicm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)
term(1) = term(1) + tvooo(a, j, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajciaicm = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aiajciaicm = eom_cc3_32_tripletm_trans_aiajciaicm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajciaicm
    function eom_cc3_32_tripletm_trans_aiajcialcj(a, i, l) 
    double precision :: eom_cc3_32_tripletm_trans_aiajcialcj   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajcialcj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajcialcj = eom_cc3_32_tripletm_trans_aiajcialcj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajcialcj
    function eom_cc3_32_tripletm_trans_aiajckaicj(a, i, j, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckaicj   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, j, j)
term(1) = term(1) + tvooo(a, j, j, k)
term(2) = term(2) + tvooo(a, k, i, i)
term(3) = term(3) + tvvvo(a, a, a, k)
term(4) = term(4) + tvvvo(c, c, a, k)
term(5) = term(5) + tvvvo(a, c, c, k)

term(0) = -term(0) 
term(2) = -term(2) 
term(5) = -term(5) 


    eom_cc3_32_tripletm_trans_aiajckaicj = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aiajckaicj = eom_cc3_32_tripletm_trans_aiajckaicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckaicj
    function eom_cc3_32_tripletm_trans_aiajckaick(a, i, j, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckaick   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, k, j)
term(1) = term(1) + tvooo(a, j, k, k)
term(2) = term(2) + tvooo(a, j, i, i)
term(3) = term(3) + tvvvo(a, a, a, j)
term(4) = term(4) + tvvvo(c, c, a, j)
term(5) = term(5) + tvvvo(a, c, c, j)

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 


    eom_cc3_32_tripletm_trans_aiajckaick = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aiajckaick = eom_cc3_32_tripletm_trans_aiajckaick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckaick
    function eom_cc3_32_tripletm_trans_aiajckakcj(a, i, k) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckakcj   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, k, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajckakcj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckakcj = eom_cc3_32_tripletm_trans_aiajckakcj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckakcj
    function eom_cc3_32_tripletm_trans_aiajckajck(a, i, j) 
    double precision :: eom_cc3_32_tripletm_trans_aiajckajck   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)



    eom_cc3_32_tripletm_trans_aiajckajck = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajckajck = eom_cc3_32_tripletm_trans_aiajckajck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajckajck
    function eom_cc3_32_tripletm_trans_aiaickakei(a, i, c, e) 
    double precision :: eom_cc3_32_tripletm_trans_aiaickakei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiaickakei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiaickakei = eom_cc3_32_tripletm_trans_aiaickakei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiaickakei
    function eom_cc3_32_tripletm_trans_aiaickaiek(a, i, c, e) 
    double precision :: eom_cc3_32_tripletm_trans_aiaickaiek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)
term(1) = term(1) + tvvvo(a, e, c, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiaickaiek = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aiaickaiek = eom_cc3_32_tripletm_trans_aiaickaiek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiaickaiek
    function eom_cc3_32_tripletm_trans_aiajciajei(a, i, c, e) 
    double precision :: eom_cc3_32_tripletm_trans_aiajciajei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, i)



    eom_cc3_32_tripletm_trans_aiajciajei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajciajei = eom_cc3_32_tripletm_trans_aiajciajei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajciajei
    function eom_cc3_32_tripletm_trans_aiajciaiej(a, i, c, e) 
    double precision :: eom_cc3_32_tripletm_trans_aiajciaiej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)
term(1) = term(1) + tvvvo(a, e, c, i)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aiajciaiej = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aiajciaiej = eom_cc3_32_tripletm_trans_aiajciaiej + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajciaiej
    function eom_cc3_32_tripletm_trans_aibiakakei(a, i, b, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibiakakei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, i)



    eom_cc3_32_tripletm_trans_aibiakakei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibiakakei = eom_cc3_32_tripletm_trans_aibiakakei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibiakakei
    function eom_cc3_32_tripletm_trans_aibiakaiek(a, i, b, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibiakaiek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, i)
term(1) = term(1) + tvvvo(b, e, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibiakaiek = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibiakaiek = eom_cc3_32_tripletm_trans_aibiakaiek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibiakaiek
    function eom_cc3_32_tripletm_trans_aibjaiajei(a, i, b, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjaiajei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjaiajei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjaiajei = eom_cc3_32_tripletm_trans_aibjaiajei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjaiajei
    function eom_cc3_32_tripletm_trans_aibjaiaiej(a, i, b, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjaiaiej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, i)
term(1) = term(1) + tvvvo(b, e, a, i)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibjaiaiej = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjaiaiej = eom_cc3_32_tripletm_trans_aibjaiaiej + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjaiaiej
    function eom_cc3_32_tripletm_trans_aibickakbi(i, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibickakbi   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)



    eom_cc3_32_tripletm_trans_aibickakbi = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickakbi = eom_cc3_32_tripletm_trans_aibickakbi + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickakbi
    function eom_cc3_32_tripletm_trans_aibickaibk(a, i, b, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibickaibk   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, k, k)
term(1) = term(1) + tvooo(c, k, k, i)
term(2) = term(2) + tvooo(c, i, i, i)
term(3) = term(3) + tvvvo(c, b, b, i)
term(4) = term(4) + tvvvo(b, b, c, i)
term(5) = term(5) + tvvvo(a, a, c, i)

term(0) = -term(0) 
term(2) = -term(2) 
term(3) = -term(3) 


    eom_cc3_32_tripletm_trans_aibickaibk = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibickaibk = eom_cc3_32_tripletm_trans_aibickaibk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickaibk
    function eom_cc3_32_tripletm_trans_aibjciajbi(i, j, c) 
    double precision :: eom_cc3_32_tripletm_trans_aibjciajbi   
    integer, intent(in) :: i, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjciajbi = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjciajbi = eom_cc3_32_tripletm_trans_aibjciajbi + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjciajbi
    function eom_cc3_32_tripletm_trans_aibjciaibj(a, i, b, j, c) 
    double precision :: eom_cc3_32_tripletm_trans_aibjciaibj   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)
term(1) = term(1) + tvooo(c, i, j, j)
term(2) = term(2) + tvooo(c, i, i, i)
term(3) = term(3) + tvvvo(c, b, b, i)
term(4) = term(4) + tvvvo(b, b, c, i)
term(5) = term(5) + tvvvo(a, a, c, i)

term(0) = -term(0) 
term(4) = -term(4) 
term(5) = -term(5) 


    eom_cc3_32_tripletm_trans_aibjciaibj = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjciaibj = eom_cc3_32_tripletm_trans_aibjciaibj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjciaibj
    function eom_cc3_32_tripletm_trans_aibickakci(i, b, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibickakci   
    integer, intent(in) :: i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibickakci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickakci = eom_cc3_32_tripletm_trans_aibickakci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickakci
    function eom_cc3_32_tripletm_trans_aibickaick(a, i, b, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibickaick   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)
term(1) = term(1) + tvooo(b, i, k, k)
term(2) = term(2) + tvooo(b, i, i, i)
term(3) = term(3) + tvvvo(c, c, b, i)
term(4) = term(4) + tvvvo(b, c, c, i)
term(5) = term(5) + tvvvo(a, a, b, i)

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = -term(5) 


    eom_cc3_32_tripletm_trans_aibickaick = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibickaick = eom_cc3_32_tripletm_trans_aibickaick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickaick
    function eom_cc3_32_tripletm_trans_aibjciajci(i, b, j) 
    double precision :: eom_cc3_32_tripletm_trans_aibjciajci   
    integer, intent(in) :: i, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)



    eom_cc3_32_tripletm_trans_aibjciajci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjciajci = eom_cc3_32_tripletm_trans_aibjciajci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjciajci
    function eom_cc3_32_tripletm_trans_aibjciaicj(a, i, b, j, c) 
    double precision :: eom_cc3_32_tripletm_trans_aibjciaicj   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, j)
term(1) = term(1) + tvooo(b, j, j, i)
term(2) = term(2) + tvooo(b, i, i, i)
term(3) = term(3) + tvvvo(c, c, b, i)
term(4) = term(4) + tvvvo(a, a, b, i)
term(5) = term(5) + tvvvo(b, c, c, i)

term(0) = -term(0) 
term(2) = -term(2) 
term(5) = -term(5) 


    eom_cc3_32_tripletm_trans_aibjciaicj = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjciaicj = eom_cc3_32_tripletm_trans_aibjciaicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjciaicj
    function eom_cc3_32_tripletm_trans_aibiakblai(a, i, k, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibiakblai   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)
term(1) = term(1) + tvooo(a, k, l, i)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibiakblai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibiakblai = eom_cc3_32_tripletm_trans_aibiakblai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibiakblai
    function eom_cc3_32_tripletm_trans_aibiakbiam(a, i, k, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibiakbiam   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibiakbiam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibiakbiam = eom_cc3_32_tripletm_trans_aibiakbiam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibiakbiam
    function eom_cc3_32_tripletm_trans_aibiakbkam(a, i, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibiakbkam   
    integer, intent(in) :: a, i, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)



    eom_cc3_32_tripletm_trans_aibiakbkam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibiakbkam = eom_cc3_32_tripletm_trans_aibiakbkam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibiakbkam
    function eom_cc3_32_tripletm_trans_aibjaiblai(a, i, j, l) 
    double precision :: eom_cc3_32_tripletm_trans_aibjaiblai   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, i)
term(1) = term(1) + tvooo(a, i, l, j)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibjaiblai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjaiblai = eom_cc3_32_tripletm_trans_aibjaiblai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjaiblai
    function eom_cc3_32_tripletm_trans_aibjakbjai(a, i, b, j, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakbjai   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, k)
term(1) = term(1) + tvooo(a, k, j, j)
term(2) = term(2) + tvooo(a, k, i, i)
term(3) = term(3) + tvvvo(a, a, a, k)
term(4) = term(4) + tvvvo(a, b, b, k)
term(5) = term(5) + tvvvo(b, b, a, k)

term(1) = -term(1) 
term(2) = -term(2) 
term(4) = -term(4) 


    eom_cc3_32_tripletm_trans_aibjakbjai = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjakbjai = eom_cc3_32_tripletm_trans_aibjakbjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakbjai
    function eom_cc3_32_tripletm_trans_aibjakbkai(a, i, b, j, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakbkai   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, k, k)
term(1) = term(1) + tvooo(a, j, i, i)
term(2) = term(2) + tvooo(a, k, k, j)
term(3) = term(3) + tvvvo(a, a, a, j)
term(4) = term(4) + tvvvo(a, b, b, j)
term(5) = term(5) + tvvvo(b, b, a, j)

term(2) = -term(2) 
term(3) = -term(3) 
term(5) = -term(5) 


    eom_cc3_32_tripletm_trans_aibjakbkai = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjakbkai = eom_cc3_32_tripletm_trans_aibjakbkai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakbkai
    function eom_cc3_32_tripletm_trans_aibjaibiam(a, i, j, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjaibiam   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, i)



    eom_cc3_32_tripletm_trans_aibjaibiam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjaibiam = eom_cc3_32_tripletm_trans_aibjaibiam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjaibiam
    function eom_cc3_32_tripletm_trans_aibjaibjam(a, i, m) 
    double precision :: eom_cc3_32_tripletm_trans_aibjaibjam   
    integer, intent(in) :: a, i, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjaibjam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjaibjam = eom_cc3_32_tripletm_trans_aibjaibjam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjaibjam
    function eom_cc3_32_tripletm_trans_aibjakbiaj(a, b, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakbiaj   
    integer, intent(in) :: a, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, k)



    eom_cc3_32_tripletm_trans_aibjakbiaj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakbiaj = eom_cc3_32_tripletm_trans_aibjakbiaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakbiaj
    function eom_cc3_32_tripletm_trans_aibjakbiak(a, b, j) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakbiak   
    integer, intent(in) :: a, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, j)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjakbiak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakbiak = eom_cc3_32_tripletm_trans_aibjakbiak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakbiak
    function eom_cc3_32_tripletm_trans_aibjakbkaj(a, i, j) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakbkaj   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)



    eom_cc3_32_tripletm_trans_aibjakbkaj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakbkaj = eom_cc3_32_tripletm_trans_aibjakbkaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakbkaj
    function eom_cc3_32_tripletm_trans_aibjakbjak(a, i, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibjakbjak   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, k, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjakbjak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjakbjak = eom_cc3_32_tripletm_trans_aibjakbjak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjakbjak
    function eom_cc3_32_tripletm_trans_aibiakbkei(a, i, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibiakbkei   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibiakbkei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibiakbkei = eom_cc3_32_tripletm_trans_aibiakbkei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibiakbkei
    function eom_cc3_32_tripletm_trans_aibjaibjei(a, i, e) 
    double precision :: eom_cc3_32_tripletm_trans_aibjaibjei   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)



    eom_cc3_32_tripletm_trans_aibjaibjei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjaibjei = eom_cc3_32_tripletm_trans_aibjaibjei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjaibjei
    function eom_cc3_32_tripletm_trans_aibickbkai(a, i, b, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibickbkai   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, k, k)
term(1) = term(1) + tvooo(c, k, k, i)
term(2) = term(2) + tvooo(c, i, i, i)
term(3) = term(3) + tvvvo(c, b, b, i)
term(4) = term(4) + tvvvo(b, b, c, i)
term(5) = term(5) + tvvvo(a, a, c, i)

term(1) = -term(1) 
term(4) = -term(4) 
term(5) = -term(5) 


    eom_cc3_32_tripletm_trans_aibickbkai = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibickbkai = eom_cc3_32_tripletm_trans_aibickbkai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickbkai
    function eom_cc3_32_tripletm_trans_aibickbiak(i, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibickbiak   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibickbiak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickbiak = eom_cc3_32_tripletm_trans_aibickbiak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickbiak
    function eom_cc3_32_tripletm_trans_aibjcibjai(a, i, b, j, c) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcibjai   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)
term(1) = term(1) + tvooo(c, i, j, j)
term(2) = term(2) + tvooo(c, i, i, i)
term(3) = term(3) + tvvvo(c, b, b, i)
term(4) = term(4) + tvvvo(b, b, c, i)
term(5) = term(5) + tvvvo(a, a, c, i)

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 


    eom_cc3_32_tripletm_trans_aibjcibjai = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjcibjai = eom_cc3_32_tripletm_trans_aibjcibjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcibjai
    function eom_cc3_32_tripletm_trans_aibjcibiaj(i, j, c) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcibiaj   
    integer, intent(in) :: i, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)



    eom_cc3_32_tripletm_trans_aibjcibiaj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcibiaj = eom_cc3_32_tripletm_trans_aibjcibiaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcibiaj
    function eom_cc3_32_tripletm_trans_aibickbkci(a, i, c) 
    double precision :: eom_cc3_32_tripletm_trans_aibickbkci   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, c, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibickbkci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickbkci = eom_cc3_32_tripletm_trans_aibickbkci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickbkci
    function eom_cc3_32_tripletm_trans_aibickbick(a, i, b) 
    double precision :: eom_cc3_32_tripletm_trans_aibickbick   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibickbick = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickbick = eom_cc3_32_tripletm_trans_aibickbick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickbick
    function eom_cc3_32_tripletm_trans_aibjcibjci(a, i, c) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcibjci   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, c, i)



    eom_cc3_32_tripletm_trans_aibjcibjci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcibjci = eom_cc3_32_tripletm_trans_aibjcibjci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcibjci
    function eom_cc3_32_tripletm_trans_aibjcibicj(a, i, b) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcibicj   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, i)



    eom_cc3_32_tripletm_trans_aibjcibicj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjcibicj = eom_cc3_32_tripletm_trans_aibjcibicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcibicj
    function eom_cc3_32_tripletm_trans_aiaickckei(a, i, e) 
    double precision :: eom_cc3_32_tripletm_trans_aiaickckei   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)



    eom_cc3_32_tripletm_trans_aiaickckei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiaickckei = eom_cc3_32_tripletm_trans_aiaickckei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiaickckei
    function eom_cc3_32_tripletm_trans_aiajcicjei(a, i, e) 
    double precision :: eom_cc3_32_tripletm_trans_aiajcicjei   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajcicjei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajcicjei = eom_cc3_32_tripletm_trans_aiajcicjei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajcicjei
    function eom_cc3_32_tripletm_trans_aibickckai(a, i, b, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibickckai   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)
term(1) = term(1) + tvooo(b, i, k, k)
term(2) = term(2) + tvooo(b, i, i, i)
term(3) = term(3) + tvvvo(c, c, b, i)
term(4) = term(4) + tvvvo(b, c, c, i)
term(5) = term(5) + tvvvo(a, a, b, i)

term(1) = -term(1) 
term(2) = -term(2) 
term(4) = -term(4) 


    eom_cc3_32_tripletm_trans_aibickckai = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibickckai = eom_cc3_32_tripletm_trans_aibickckai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickckai
    function eom_cc3_32_tripletm_trans_aibickciak(i, b, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibickciak   
    integer, intent(in) :: i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)



    eom_cc3_32_tripletm_trans_aibickciak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibickciak = eom_cc3_32_tripletm_trans_aibickciak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibickciak
    function eom_cc3_32_tripletm_trans_aibjcicjai(a, i, b, j, c) 
    double precision :: eom_cc3_32_tripletm_trans_aibjcicjai   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, j)
term(1) = term(1) + tvooo(b, j, j, i)
term(2) = term(2) + tvooo(b, i, i, i)
term(3) = term(3) + tvvvo(c, c, b, i)
term(4) = term(4) + tvvvo(a, a, b, i)
term(5) = term(5) + tvvvo(b, c, c, i)

term(1) = -term(1) 
term(3) = -term(3) 
term(4) = -term(4) 


    eom_cc3_32_tripletm_trans_aibjcicjai = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjcicjai = eom_cc3_32_tripletm_trans_aibjcicjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjcicjai
    function eom_cc3_32_tripletm_trans_aibjciciaj(i, b, j) 
    double precision :: eom_cc3_32_tripletm_trans_aibjciciaj   
    integer, intent(in) :: i, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjciciaj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjciciaj = eom_cc3_32_tripletm_trans_aibjciciaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjciciaj
    function eom_cc3_32_tripletm_trans_aiaickdkai(a, i, c, d) 
    double precision :: eom_cc3_32_tripletm_trans_aiaickdkai   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)
term(1) = term(1) + tvvvo(a, d, c, i)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aiaickdkai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aiaickdkai = eom_cc3_32_tripletm_trans_aiaickdkai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiaickdkai
    function eom_cc3_32_tripletm_trans_aiaickdiak(a, i, c, d) 
    double precision :: eom_cc3_32_tripletm_trans_aiaickdiak   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, i)



    eom_cc3_32_tripletm_trans_aiaickdiak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiaickdiak = eom_cc3_32_tripletm_trans_aiaickdiak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiaickdiak
    function eom_cc3_32_tripletm_trans_aiajcidjai(a, i, c, d) 
    double precision :: eom_cc3_32_tripletm_trans_aiajcidjai   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)
term(1) = term(1) + tvvvo(a, d, c, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajcidjai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aiajcidjai = eom_cc3_32_tripletm_trans_aiajcidjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajcidjai
    function eom_cc3_32_tripletm_trans_aiajcidiaj(a, i, c, d) 
    double precision :: eom_cc3_32_tripletm_trans_aiajcidiaj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiajcidiaj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajcidiaj = eom_cc3_32_tripletm_trans_aiajcidiaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajcidiaj
    function eom_cc3_32_tripletm_trans_aiaickdick(a, i, d) 
    double precision :: eom_cc3_32_tripletm_trans_aiaickdick   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aiaickdick = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiaickdick = eom_cc3_32_tripletm_trans_aiaickdick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiaickdick
    function eom_cc3_32_tripletm_trans_aiajcidicj(a, i, d) 
    double precision :: eom_cc3_32_tripletm_trans_aiajcidicj   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)



    eom_cc3_32_tripletm_trans_aiajcidicj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aiajcidicj = eom_cc3_32_tripletm_trans_aiajcidicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajcidicj
    function eom_cc3_32_tripletm_trans_aibiakdkai(a, i, b, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibiakdkai   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, i)
term(1) = term(1) + tvvvo(b, d, a, i)

term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibiakdkai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibiakdkai = eom_cc3_32_tripletm_trans_aibiakdkai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibiakdkai
    function eom_cc3_32_tripletm_trans_aibiakdiak(a, i, b, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibiakdiak   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibiakdiak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibiakdiak = eom_cc3_32_tripletm_trans_aibiakdiak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibiakdiak
    function eom_cc3_32_tripletm_trans_aibjaidjai(a, i, b, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjaidjai   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, i)
term(1) = term(1) + tvvvo(b, d, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjaidjai = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjaidjai = eom_cc3_32_tripletm_trans_aibjaidjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjaidjai
    function eom_cc3_32_tripletm_trans_aibjaidiaj(a, i, b, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjaidiaj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, i)



    eom_cc3_32_tripletm_trans_aibjaidiaj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjaidiaj = eom_cc3_32_tripletm_trans_aibjaidiaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjaidiaj
    function eom_cc3_32_tripletm_trans_aibiakdibk(a, i, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibiakdibk   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)



    eom_cc3_32_tripletm_trans_aibiakdibk = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibiakdibk = eom_cc3_32_tripletm_trans_aibiakdibk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibiakdibk
    function eom_cc3_32_tripletm_trans_aibjaidibj(a, i, d) 
    double precision :: eom_cc3_32_tripletm_trans_aibjaidibj   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletm_trans_aibjaidibj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletm_trans_aibjaidibj = eom_cc3_32_tripletm_trans_aibjaidibj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjaidibj
    function eom_cc3_32_tripletm_trans_aiaickakci(a, i, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aiaickakci   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, k, i)
term(1) = term(1) + tvvvo(a, c, c, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aiaickakci = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aiaickakci = eom_cc3_32_tripletm_trans_aiaickakci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiaickakci
    function eom_cc3_32_tripletm_trans_aiaickaick(a, i, c, k) 
    double precision :: eom_cc3_32_tripletm_trans_aiaickaick   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, k, i)
term(1) = term(1) + tvooo(a, i, k, k)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvooo(a, i, i, i)
term(4) = term(4) + tvvvo(c, c, a, i)
term(5) = term(5) + tvvvo(a, c, c, i)

term(0) = -term(0) 
term(2) = -term(2) 
term(4) = -term(4) 


    eom_cc3_32_tripletm_trans_aiaickaick = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aiaickaick = eom_cc3_32_tripletm_trans_aiaickaick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiaickaick
    function eom_cc3_32_tripletm_trans_aiajciajci(a, i, j, c) 
    double precision :: eom_cc3_32_tripletm_trans_aiajciajci   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)
term(1) = term(1) + tvvvo(a, c, c, i)



    eom_cc3_32_tripletm_trans_aiajciajci = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aiajciajci = eom_cc3_32_tripletm_trans_aiajciajci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajciajci
    function eom_cc3_32_tripletm_trans_aiajciaicj(a, i, j, c) 
    double precision :: eom_cc3_32_tripletm_trans_aiajciaicj   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, j, j, i)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvooo(a, i, i, i)
term(4) = term(4) + tvvvo(c, c, a, i)
term(5) = term(5) + tvvvo(a, c, c, i)

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = -term(5) 


    eom_cc3_32_tripletm_trans_aiajciaicj = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aiajciaicj = eom_cc3_32_tripletm_trans_aiajciaicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aiajciaicj
    function eom_cc3_32_tripletm_trans_aibiakbkai(a, i, b, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibiakbkai   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvooo(a, k, k, i)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvooo(a, i, i, i)
term(4) = term(4) + tvvvo(a, b, b, i)
term(5) = term(5) + tvvvo(b, b, a, i)

term(1) = -term(1) 
term(2) = -term(2) 
term(5) = -term(5) 


    eom_cc3_32_tripletm_trans_aibiakbkai = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibiakbkai = eom_cc3_32_tripletm_trans_aibiakbkai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibiakbkai
    function eom_cc3_32_tripletm_trans_aibiakbiak(a, i, b, k) 
    double precision :: eom_cc3_32_tripletm_trans_aibiakbiak   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, k, i)
term(1) = term(1) + tvvvo(a, b, b, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_32_tripletm_trans_aibiakbiak = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibiakbiak = eom_cc3_32_tripletm_trans_aibiakbiak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibiakbiak
    function eom_cc3_32_tripletm_trans_aibjaibjai(a, i, b, j) 
    double precision :: eom_cc3_32_tripletm_trans_aibjaibjai   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)
term(1) = term(1) + tvooo(a, i, j, j)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvooo(a, i, i, i)
term(4) = term(4) + tvvvo(a, b, b, i)
term(5) = term(5) + tvvvo(b, b, a, i)

term(1) = -term(1) 
term(3) = -term(3) 
term(4) = -term(4) 


    eom_cc3_32_tripletm_trans_aibjaibjai = 0.d+0
    do s = 0, 5
    eom_cc3_32_tripletm_trans_aibjaibjai = eom_cc3_32_tripletm_trans_aibjaibjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjaibjai
    function eom_cc3_32_tripletm_trans_aibjaibiaj(a, i, b, j) 
    double precision :: eom_cc3_32_tripletm_trans_aibjaibiaj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)
term(1) = term(1) + tvvvo(a, b, b, i)



    eom_cc3_32_tripletm_trans_aibjaibiaj = 0.d+0
    do s = 0, 1
    eom_cc3_32_tripletm_trans_aibjaibiaj = eom_cc3_32_tripletm_trans_aibjaibiaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletm_trans_aibjaibiaj
    end module eom_cc3_32_tripletm_trans
    
