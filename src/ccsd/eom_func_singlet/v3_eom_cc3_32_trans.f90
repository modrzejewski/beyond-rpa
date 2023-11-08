module v3_eom_cc3_32_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2013-02-27 23:45:57 UTC.
    !
    contains
    
    function v3_eom_cc3_32_trans_aibjckaicm(b, j, k, m) 
    double precision :: v3_eom_cc3_32_trans_aibjckaicm   
    integer, intent(in) :: b, j, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, m, k)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckaicm = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckaicm = v3_eom_cc3_32_trans_aibjckaicm + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckaicm
    function v3_eom_cc3_32_trans_aibjckalcj(i, b, k, l) 
    double precision :: v3_eom_cc3_32_trans_aibjckalcj   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, k)



    v3_eom_cc3_32_trans_aibjckalcj = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckalcj = v3_eom_cc3_32_trans_aibjckalcj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckalcj
    function v3_eom_cc3_32_trans_aibjckakcm(i, b, j, m) 
    double precision :: v3_eom_cc3_32_trans_aibjckakcm   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, j)



    v3_eom_cc3_32_trans_aibjckakcm = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckakcm = v3_eom_cc3_32_trans_aibjckakcm + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckakcm
    function v3_eom_cc3_32_trans_aibjckalck(i, b, j, l) 
    double precision :: v3_eom_cc3_32_trans_aibjckalck   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, i)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckalck = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckalck = v3_eom_cc3_32_trans_aibjckalck + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckalck
    function v3_eom_cc3_32_trans_aibjckaibm(j, c, k, m) 
    double precision :: v3_eom_cc3_32_trans_aibjckaibm   
    integer, intent(in) :: j, c, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, m, j)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckaibm = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckaibm = v3_eom_cc3_32_trans_aibjckaibm + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckaibm
    function v3_eom_cc3_32_trans_aibjckalbi(j, c, k, l) 
    double precision :: v3_eom_cc3_32_trans_aibjckalbi   
    integer, intent(in) :: j, c, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, k)



    v3_eom_cc3_32_trans_aibjckalbi = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckalbi = v3_eom_cc3_32_trans_aibjckalbi + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckalbi
    function v3_eom_cc3_32_trans_aibjckalbj(i, c, k, l) 
    double precision :: v3_eom_cc3_32_trans_aibjckalbj   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, l, i)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckalbj = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckalbj = v3_eom_cc3_32_trans_aibjckalbj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckalbj
    function v3_eom_cc3_32_trans_aibjckakbm(i, j, c, m) 
    double precision :: v3_eom_cc3_32_trans_aibjckakbm   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, i)



    v3_eom_cc3_32_trans_aibjckakbm = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckakbm = v3_eom_cc3_32_trans_aibjckakbm + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckakbm
    function v3_eom_cc3_32_trans_aibjckaiej(b, c, k, e) 
    double precision :: v3_eom_cc3_32_trans_aibjckaiej   
    integer, intent(in) :: b, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, c, k)



    v3_eom_cc3_32_trans_aibjckaiej = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckaiej = v3_eom_cc3_32_trans_aibjckaiej + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckaiej
    function v3_eom_cc3_32_trans_aibjckaiek(b, j, c, e) 
    double precision :: v3_eom_cc3_32_trans_aibjckaiek   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, j)



    v3_eom_cc3_32_trans_aibjckaiek = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckaiek = v3_eom_cc3_32_trans_aibjckaiek + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckaiek
    function v3_eom_cc3_32_trans_aibjckakei(b, j, c, e) 
    double precision :: v3_eom_cc3_32_trans_aibjckakei   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, c, j)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckakei = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckakei = v3_eom_cc3_32_trans_aibjckakei + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckakei
    function v3_eom_cc3_32_trans_aibjckakej(i, b, c, e) 
    double precision :: v3_eom_cc3_32_trans_aibjckakej   
    integer, intent(in) :: i, b, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, i)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckakej = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckakej = v3_eom_cc3_32_trans_aibjckakej + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckakej
    function v3_eom_cc3_32_trans_aibjckdiak(b, j, c, d) 
    double precision :: v3_eom_cc3_32_trans_aibjckdiak   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, c, j)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckdiak = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckdiak = v3_eom_cc3_32_trans_aibjckdiak + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckdiak
    function v3_eom_cc3_32_trans_aibjckdjai(b, c, k, d) 
    double precision :: v3_eom_cc3_32_trans_aibjckdjai   
    integer, intent(in) :: b, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, c, k)



    v3_eom_cc3_32_trans_aibjckdjai = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckdjai = v3_eom_cc3_32_trans_aibjckdjai + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckdjai
    function v3_eom_cc3_32_trans_aibjckdkai(b, j, c, d) 
    double precision :: v3_eom_cc3_32_trans_aibjckdkai   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, j)



    v3_eom_cc3_32_trans_aibjckdkai = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckdkai = v3_eom_cc3_32_trans_aibjckdkai + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckdkai
    function v3_eom_cc3_32_trans_aibjckdjak(i, b, c, d) 
    double precision :: v3_eom_cc3_32_trans_aibjckdjak   
    integer, intent(in) :: i, b, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, i)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckdjak = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckdjak = v3_eom_cc3_32_trans_aibjckdjak + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckdjak
    function v3_eom_cc3_32_trans_aibjckcjei(a, b, k, e) 
    double precision :: v3_eom_cc3_32_trans_aibjckcjei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, k)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckcjei = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckcjei = v3_eom_cc3_32_trans_aibjckcjei + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckcjei
    function v3_eom_cc3_32_trans_aibjckckei(a, b, j, e) 
    double precision :: v3_eom_cc3_32_trans_aibjckckei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, j)



    v3_eom_cc3_32_trans_aibjckckei = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckckei = v3_eom_cc3_32_trans_aibjckckei + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckckei
    function v3_eom_cc3_32_trans_aibjckcjek(a, i, b, e) 
    double precision :: v3_eom_cc3_32_trans_aibjckcjek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, i)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckcjek = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckcjek = v3_eom_cc3_32_trans_aibjckcjek + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckcjek
    function v3_eom_cc3_32_trans_aibjckckej(a, i, b, e) 
    double precision :: v3_eom_cc3_32_trans_aibjckckej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)



    v3_eom_cc3_32_trans_aibjckckej = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckckej = v3_eom_cc3_32_trans_aibjckckej + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckckej
    function v3_eom_cc3_32_trans_aibjckbicm(a, j, k, m) 
    double precision :: v3_eom_cc3_32_trans_aibjckbicm   
    integer, intent(in) :: a, j, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, m, j)



    v3_eom_cc3_32_trans_aibjckbicm = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckbicm = v3_eom_cc3_32_trans_aibjckbicm + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbicm
    function v3_eom_cc3_32_trans_aibjckbjcm(a, i, k, m) 
    double precision :: v3_eom_cc3_32_trans_aibjckbjcm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckbjcm = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckbjcm = v3_eom_cc3_32_trans_aibjckbjcm + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbjcm
    function v3_eom_cc3_32_trans_aibjckblcj(a, i, k, l) 
    double precision :: v3_eom_cc3_32_trans_aibjckblcj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, l, i)



    v3_eom_cc3_32_trans_aibjckblcj = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckblcj = v3_eom_cc3_32_trans_aibjckblcj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckblcj
    function v3_eom_cc3_32_trans_aibjckblck(a, i, j, l) 
    double precision :: v3_eom_cc3_32_trans_aibjckblck   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckblck = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckblck = v3_eom_cc3_32_trans_aibjckblck + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckblck
    function v3_eom_cc3_32_trans_aibjckdicj(a, b, k, d) 
    double precision :: v3_eom_cc3_32_trans_aibjckdicj   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, k)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckdicj = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckdicj = v3_eom_cc3_32_trans_aibjckdicj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckdicj
    function v3_eom_cc3_32_trans_aibjckdick(a, b, j, d) 
    double precision :: v3_eom_cc3_32_trans_aibjckdick   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, j)



    v3_eom_cc3_32_trans_aibjckdick = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckdick = v3_eom_cc3_32_trans_aibjckdick + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckdick
    function v3_eom_cc3_32_trans_aibjckdjck(a, i, b, d) 
    double precision :: v3_eom_cc3_32_trans_aibjckdjck   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    v3_eom_cc3_32_trans_aibjckdjck = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckdjck = v3_eom_cc3_32_trans_aibjckdjck + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckdjck
    function v3_eom_cc3_32_trans_aibjckdkcj(a, i, b, d) 
    double precision :: v3_eom_cc3_32_trans_aibjckdkcj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, i)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckdkcj = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckdkcj = v3_eom_cc3_32_trans_aibjckdkcj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckdkcj
    function v3_eom_cc3_32_trans_aibjckbiej(a, c, k, e) 
    double precision :: v3_eom_cc3_32_trans_aibjckbiej   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, k)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckbiej = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckbiej = v3_eom_cc3_32_trans_aibjckbiej + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbiej
    function v3_eom_cc3_32_trans_aibjckbiek(a, j, c, e) 
    double precision :: v3_eom_cc3_32_trans_aibjckbiek   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, j)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckbiek = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckbiek = v3_eom_cc3_32_trans_aibjckbiek + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbiek
    function v3_eom_cc3_32_trans_aibjckbjei(a, c, k, e) 
    double precision :: v3_eom_cc3_32_trans_aibjckbjei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, k)



    v3_eom_cc3_32_trans_aibjckbjei = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckbjei = v3_eom_cc3_32_trans_aibjckbjei + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbjei
    function v3_eom_cc3_32_trans_aibjckbjek(a, i, c, e) 
    double precision :: v3_eom_cc3_32_trans_aibjckbjek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    v3_eom_cc3_32_trans_aibjckbjek = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckbjek = v3_eom_cc3_32_trans_aibjckbjek + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbjek
    function v3_eom_cc3_32_trans_aibjckdibj(a, c, k, d) 
    double precision :: v3_eom_cc3_32_trans_aibjckdibj   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, k)



    v3_eom_cc3_32_trans_aibjckdibj = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckdibj = v3_eom_cc3_32_trans_aibjckdibj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckdibj
    function v3_eom_cc3_32_trans_aibjckdjbi(a, c, k, d) 
    double precision :: v3_eom_cc3_32_trans_aibjckdjbi   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, k)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckdjbi = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckdjbi = v3_eom_cc3_32_trans_aibjckdjbi + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckdjbi
    function v3_eom_cc3_32_trans_aibjckdkbi(a, j, c, d) 
    double precision :: v3_eom_cc3_32_trans_aibjckdkbi   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, j)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckdkbi = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckdkbi = v3_eom_cc3_32_trans_aibjckdkbi + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckdkbi
    function v3_eom_cc3_32_trans_aibjckdkbj(a, i, c, d) 
    double precision :: v3_eom_cc3_32_trans_aibjckdkbj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)



    v3_eom_cc3_32_trans_aibjckdkbj = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckdkbj = v3_eom_cc3_32_trans_aibjckdkbj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckdkbj
    function v3_eom_cc3_32_trans_aibjckaiaj(a, b, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckaiaj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, k)



    v3_eom_cc3_32_trans_aibjckaiaj = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckaiaj = v3_eom_cc3_32_trans_aibjckaiaj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckaiaj
    function v3_eom_cc3_32_trans_aibjckaiak(a, b, j, c) 
    double precision :: v3_eom_cc3_32_trans_aibjckaiak   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, j)
term(1) = term(1) + tvvvo(c, a, b, j)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckaiak = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckaiak = v3_eom_cc3_32_trans_aibjckaiak + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckaiak
    function v3_eom_cc3_32_trans_aibjckajai(a, b, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckajai   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, k)



    v3_eom_cc3_32_trans_aibjckajai = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckajai = v3_eom_cc3_32_trans_aibjckajai + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckajai
    function v3_eom_cc3_32_trans_aibjckakai(a, b, j, c) 
    double precision :: v3_eom_cc3_32_trans_aibjckakai   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, j)
term(1) = term(1) + tvvvo(b, a, c, j)

term(1) = -term(1) 


    v3_eom_cc3_32_trans_aibjckakai = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckakai = v3_eom_cc3_32_trans_aibjckakai + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckakai
    function v3_eom_cc3_32_trans_aibjckajak(a, i, b, c) 
    double precision :: v3_eom_cc3_32_trans_aibjckajak   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, i)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckajak = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckajak = v3_eom_cc3_32_trans_aibjckajak + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckajak
    function v3_eom_cc3_32_trans_aibjckakaj(a, i, b, c) 
    double precision :: v3_eom_cc3_32_trans_aibjckakaj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, i)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckakaj = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckakaj = v3_eom_cc3_32_trans_aibjckakaj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckakaj
    function v3_eom_cc3_32_trans_aibjckaici(i, b, j, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckaici   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, i, k)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckaici = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckaici = v3_eom_cc3_32_trans_aibjckaici + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckaici
    function v3_eom_cc3_32_trans_aibjckaicj(a, i, b, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckaicj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, i, k)
term(1) = term(1) + tvooo(b, j, j, k)
term(2) = term(2) + tvvvo(b, a, a, k)
term(3) = term(3) + tvvvo(b, c, c, k)

term(1) = -term(1) 
term(2) = -term(2) 


    v3_eom_cc3_32_trans_aibjckaicj = 0.d+0
    do s = 0, 3
    v3_eom_cc3_32_trans_aibjckaicj = v3_eom_cc3_32_trans_aibjckaicj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckaicj
    function v3_eom_cc3_32_trans_aibjckaick(a, i, b, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckaick   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, i, i)
term(1) = term(1) + tvooo(b, j, k, k)
term(2) = term(2) + tvvvo(a, a, b, j)
term(3) = term(3) + tvvvo(c, c, b, j)

term(0) = -term(0) 
term(1) = -term(1) 


    v3_eom_cc3_32_trans_aibjckaick = 0.d+0
    do s = 0, 3
    v3_eom_cc3_32_trans_aibjckaick = v3_eom_cc3_32_trans_aibjckaick + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckaick
    function v3_eom_cc3_32_trans_aibjckakci(i, b, j, c) 
    double precision :: v3_eom_cc3_32_trans_aibjckakci   
    integer, intent(in) :: i, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, i, j)
term(1) = term(1) + tvvvo(b, c, c, j)

term(1) = -term(1) 


    v3_eom_cc3_32_trans_aibjckakci = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckakci = v3_eom_cc3_32_trans_aibjckakci + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckakci
    function v3_eom_cc3_32_trans_aibjckajcj(i, b, j, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckajcj   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, k)



    v3_eom_cc3_32_trans_aibjckajcj = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckajcj = v3_eom_cc3_32_trans_aibjckajcj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckajcj
    function v3_eom_cc3_32_trans_aibjckajck(a, i, b, j) 
    double precision :: v3_eom_cc3_32_trans_aibjckajck   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)
term(1) = term(1) + tvvvo(b, a, a, i)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckajck = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckajck = v3_eom_cc3_32_trans_aibjckajck + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckajck
    function v3_eom_cc3_32_trans_aibjckakcj(a, i, b, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckakcj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, k, k)
term(1) = term(1) + tvooo(b, i, j, j)
term(2) = term(2) + tvvvo(a, a, b, i)
term(3) = term(3) + tvvvo(c, c, b, i)

term(2) = -term(2) 
term(3) = -term(3) 


    v3_eom_cc3_32_trans_aibjckakcj = 0.d+0
    do s = 0, 3
    v3_eom_cc3_32_trans_aibjckakcj = v3_eom_cc3_32_trans_aibjckakcj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckakcj
    function v3_eom_cc3_32_trans_aibjckakck(i, b, j, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckakck   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, k, i)
term(1) = term(1) + tvooo(b, i, k, j)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckakck = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckakck = v3_eom_cc3_32_trans_aibjckakck + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckakck
    function v3_eom_cc3_32_trans_aibjckaibi(i, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckaibi   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, i, k)
term(1) = term(1) + tvooo(c, k, i, j)

term(1) = -term(1) 


    v3_eom_cc3_32_trans_aibjckaibi = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckaibi = v3_eom_cc3_32_trans_aibjckaibi + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckaibi
    function v3_eom_cc3_32_trans_aibjckaibj(a, i, b, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckaibj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, i, i)
term(1) = term(1) + tvooo(c, k, j, j)
term(2) = term(2) + tvvvo(a, a, c, k)
term(3) = term(3) + tvvvo(b, b, c, k)

term(0) = -term(0) 
term(1) = -term(1) 


    v3_eom_cc3_32_trans_aibjckaibj = 0.d+0
    do s = 0, 3
    v3_eom_cc3_32_trans_aibjckaibj = v3_eom_cc3_32_trans_aibjckaibj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckaibj
    function v3_eom_cc3_32_trans_aibjckaibk(b, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckaibk   
    integer, intent(in) :: b, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, j)
term(1) = term(1) + tvvvo(c, b, b, j)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckaibk = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckaibk = v3_eom_cc3_32_trans_aibjckaibk + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckaibk
    function v3_eom_cc3_32_trans_aibjckajbi(a, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckajbi   
    integer, intent(in) :: a, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, k)
term(1) = term(1) + tvvvo(c, a, a, k)

term(1) = -term(1) 


    v3_eom_cc3_32_trans_aibjckajbi = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckajbi = v3_eom_cc3_32_trans_aibjckajbi + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckajbi
    function v3_eom_cc3_32_trans_aibjckakbi(a, i, b, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckakbi   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, k, k)
term(1) = term(1) + tvooo(c, j, i, i)
term(2) = term(2) + tvvvo(a, a, c, j)
term(3) = term(3) + tvvvo(b, b, c, j)

term(2) = -term(2) 
term(3) = -term(3) 


    v3_eom_cc3_32_trans_aibjckakbi = 0.d+0
    do s = 0, 3
    v3_eom_cc3_32_trans_aibjckakbi = v3_eom_cc3_32_trans_aibjckakbi + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckakbi
    function v3_eom_cc3_32_trans_aibjckajbj(i, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckajbj   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, j, i)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckajbj = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckajbj = v3_eom_cc3_32_trans_aibjckajbj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckajbj
    function v3_eom_cc3_32_trans_aibjckakbj(a, i, b, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckakbj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)
term(1) = term(1) + tvooo(c, j, j, i)
term(2) = term(2) + tvvvo(c, a, a, i)
term(3) = term(3) + tvvvo(c, b, b, i)

term(0) = -term(0) 
term(3) = -term(3) 


    v3_eom_cc3_32_trans_aibjckakbj = 0.d+0
    do s = 0, 3
    v3_eom_cc3_32_trans_aibjckakbj = v3_eom_cc3_32_trans_aibjckakbj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckakbj
    function v3_eom_cc3_32_trans_aibjckakbk(i, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckakbk   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, k, i)



    v3_eom_cc3_32_trans_aibjckakbk = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckakbk = v3_eom_cc3_32_trans_aibjckakbk + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckakbk
    function v3_eom_cc3_32_trans_aibjckcicj(a, b, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckcicj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, k)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckcicj = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckcicj = v3_eom_cc3_32_trans_aibjckcicj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckcicj
    function v3_eom_cc3_32_trans_aibjckcick(a, b, j, c) 
    double precision :: v3_eom_cc3_32_trans_aibjckcick   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, j)



    v3_eom_cc3_32_trans_aibjckcick = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckcick = v3_eom_cc3_32_trans_aibjckcick + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckcick
    function v3_eom_cc3_32_trans_aibjckcjci(a, b, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckcjci   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, k)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckcjci = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckcjci = v3_eom_cc3_32_trans_aibjckcjci + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckcjci
    function v3_eom_cc3_32_trans_aibjckckci(a, b, j, c) 
    double precision :: v3_eom_cc3_32_trans_aibjckckci   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, j)



    v3_eom_cc3_32_trans_aibjckckci = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckckci = v3_eom_cc3_32_trans_aibjckckci + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckckci
    function v3_eom_cc3_32_trans_aibjckcjck(a, i, b, c) 
    double precision :: v3_eom_cc3_32_trans_aibjckcjck   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, i)
term(1) = term(1) + tvvvo(a, c, b, i)

term(1) = -term(1) 


    v3_eom_cc3_32_trans_aibjckcjck = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckcjck = v3_eom_cc3_32_trans_aibjckcjck + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckcjck
    function v3_eom_cc3_32_trans_aibjckckcj(a, i, b, c) 
    double precision :: v3_eom_cc3_32_trans_aibjckckcj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, i)
term(1) = term(1) + tvvvo(b, c, a, i)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckckcj = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckckcj = v3_eom_cc3_32_trans_aibjckckcj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckckcj
    function v3_eom_cc3_32_trans_aibjckbici(a, i, j, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckbici   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, i, j)



    v3_eom_cc3_32_trans_aibjckbici = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckbici = v3_eom_cc3_32_trans_aibjckbici + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbici
    function v3_eom_cc3_32_trans_aibjckbicj(a, i, b, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckbicj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, i, i)
term(1) = term(1) + tvooo(a, k, j, j)
term(2) = term(2) + tvvvo(b, b, a, k)
term(3) = term(3) + tvvvo(c, c, a, k)

term(2) = -term(2) 
term(3) = -term(3) 


    v3_eom_cc3_32_trans_aibjckbicj = 0.d+0
    do s = 0, 3
    v3_eom_cc3_32_trans_aibjckbicj = v3_eom_cc3_32_trans_aibjckbicj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbicj
    function v3_eom_cc3_32_trans_aibjckbick(a, i, b, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckbick   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)
term(1) = term(1) + tvooo(a, k, k, j)
term(2) = term(2) + tvvvo(a, b, b, j)
term(3) = term(3) + tvvvo(a, c, c, j)

term(0) = -term(0) 
term(3) = -term(3) 


    v3_eom_cc3_32_trans_aibjckbick = 0.d+0
    do s = 0, 3
    v3_eom_cc3_32_trans_aibjckbick = v3_eom_cc3_32_trans_aibjckbick + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbick
    function v3_eom_cc3_32_trans_aibjckbjci(a, i, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckbjci   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)
term(1) = term(1) + tvvvo(a, c, c, k)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckbjci = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckbjci = v3_eom_cc3_32_trans_aibjckbjci + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbjci
    function v3_eom_cc3_32_trans_aibjckbjcj(a, i, j, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckbjcj   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, j, i)
term(1) = term(1) + tvooo(a, i, j, k)

term(1) = -term(1) 


    v3_eom_cc3_32_trans_aibjckbjcj = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckbjcj = v3_eom_cc3_32_trans_aibjckbjcj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbjcj
    function v3_eom_cc3_32_trans_aibjckbjck(a, i, b, j, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckbjck   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, i, k, k)
term(2) = term(2) + tvvvo(b, b, a, i)
term(3) = term(3) + tvvvo(c, c, a, i)

term(0) = -term(0) 
term(1) = -term(1) 


    v3_eom_cc3_32_trans_aibjckbjck = 0.d+0
    do s = 0, 3
    v3_eom_cc3_32_trans_aibjckbjck = v3_eom_cc3_32_trans_aibjckbjck + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbjck
    function v3_eom_cc3_32_trans_aibjckbkcj(a, i, b, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckbkcj   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, k, i)
term(1) = term(1) + tvvvo(a, b, b, i)

term(1) = -term(1) 


    v3_eom_cc3_32_trans_aibjckbkcj = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckbkcj = v3_eom_cc3_32_trans_aibjckbkcj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbkcj
    function v3_eom_cc3_32_trans_aibjckbkck(a, i, j, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckbkck   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, j)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckbkck = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckbkck = v3_eom_cc3_32_trans_aibjckbkck + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbkck
    function v3_eom_cc3_32_trans_aibjckbibj(a, b, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckbibj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, k)
term(1) = term(1) + tvvvo(c, b, a, k)

term(1) = -term(1) 


    v3_eom_cc3_32_trans_aibjckbibj = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckbibj = v3_eom_cc3_32_trans_aibjckbibj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbibj
    function v3_eom_cc3_32_trans_aibjckbibk(a, b, j, c) 
    double precision :: v3_eom_cc3_32_trans_aibjckbibk   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, j)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckbibk = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckbibk = v3_eom_cc3_32_trans_aibjckbibk + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbibk
    function v3_eom_cc3_32_trans_aibjckbjbi(a, b, c, k) 
    double precision :: v3_eom_cc3_32_trans_aibjckbjbi   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, k)
term(1) = term(1) + tvvvo(a, b, c, k)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckbjbi = 0.d+0
    do s = 0, 1
    v3_eom_cc3_32_trans_aibjckbjbi = v3_eom_cc3_32_trans_aibjckbjbi + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbjbi
    function v3_eom_cc3_32_trans_aibjckbkbi(a, b, j, c) 
    double precision :: v3_eom_cc3_32_trans_aibjckbkbi   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, j)

term(0) = -term(0) 


    v3_eom_cc3_32_trans_aibjckbkbi = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckbkbi = v3_eom_cc3_32_trans_aibjckbkbi + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbkbi
    function v3_eom_cc3_32_trans_aibjckbjbk(a, i, b, c) 
    double precision :: v3_eom_cc3_32_trans_aibjckbjbk   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, i)



    v3_eom_cc3_32_trans_aibjckbjbk = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckbjbk = v3_eom_cc3_32_trans_aibjckbjbk + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbjbk
    function v3_eom_cc3_32_trans_aibjckbkbj(a, i, b, c) 
    double precision :: v3_eom_cc3_32_trans_aibjckbkbj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, i)



    v3_eom_cc3_32_trans_aibjckbkbj = 0.d+0
    do s = 0, 0
    v3_eom_cc3_32_trans_aibjckbkbj = v3_eom_cc3_32_trans_aibjckbkbj + term(s)
    end do

    end function v3_eom_cc3_32_trans_aibjckbkbj
    end module v3_eom_cc3_32_trans
    
