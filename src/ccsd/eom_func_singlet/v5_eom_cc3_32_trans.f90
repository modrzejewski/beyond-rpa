module v5_eom_cc3_32_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2013-02-27 23:45:57 UTC.
    !
    contains
    
    function v5_eom_cc3_32_trans_aibjckaicm(b, j, k, m) 
    double precision :: v5_eom_cc3_32_trans_aibjckaicm   
    integer, intent(in) :: b, j, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, m, k)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckaicm = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckaicm = v5_eom_cc3_32_trans_aibjckaicm + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckaicm
    function v5_eom_cc3_32_trans_aibjckalci(b, j, k, l) 
    double precision :: v5_eom_cc3_32_trans_aibjckalci   
    integer, intent(in) :: b, j, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, k)



    v5_eom_cc3_32_trans_aibjckalci = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckalci = v5_eom_cc3_32_trans_aibjckalci + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckalci
    function v5_eom_cc3_32_trans_aibjckakcm(i, b, j, m) 
    double precision :: v5_eom_cc3_32_trans_aibjckakcm   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, m, i)



    v5_eom_cc3_32_trans_aibjckakcm = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckakcm = v5_eom_cc3_32_trans_aibjckakcm + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckakcm
    function v5_eom_cc3_32_trans_aibjckalck(i, b, j, l) 
    double precision :: v5_eom_cc3_32_trans_aibjckalck   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, i)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckalck = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckalck = v5_eom_cc3_32_trans_aibjckalck + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckalck
    function v5_eom_cc3_32_trans_aibjckaibm(j, c, k, m) 
    double precision :: v5_eom_cc3_32_trans_aibjckaibm   
    integer, intent(in) :: j, c, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, m, j)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckaibm = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckaibm = v5_eom_cc3_32_trans_aibjckaibm + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckaibm
    function v5_eom_cc3_32_trans_aibjckalbj(i, c, k, l) 
    double precision :: v5_eom_cc3_32_trans_aibjckalbj   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, l, i)
term(1) = term(1) + tvooo(c, i, l, k)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckalbj = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckalbj = v5_eom_cc3_32_trans_aibjckalbj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckalbj
    function v5_eom_cc3_32_trans_aibjckakbm(i, j, c, m) 
    double precision :: v5_eom_cc3_32_trans_aibjckakbm   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, j)



    v5_eom_cc3_32_trans_aibjckakbm = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckakbm = v5_eom_cc3_32_trans_aibjckakbm + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckakbm
    function v5_eom_cc3_32_trans_aibjckaiej(b, c, k, e) 
    double precision :: v5_eom_cc3_32_trans_aibjckaiej   
    integer, intent(in) :: b, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, c, k)



    v5_eom_cc3_32_trans_aibjckaiej = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckaiej = v5_eom_cc3_32_trans_aibjckaiej + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckaiej
    function v5_eom_cc3_32_trans_aibjckaiek(b, j, c, e) 
    double precision :: v5_eom_cc3_32_trans_aibjckaiek   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, j)



    v5_eom_cc3_32_trans_aibjckaiek = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckaiek = v5_eom_cc3_32_trans_aibjckaiek + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckaiek
    function v5_eom_cc3_32_trans_aibjckakei(b, j, c, e) 
    double precision :: v5_eom_cc3_32_trans_aibjckakei   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, j)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckakei = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckakei = v5_eom_cc3_32_trans_aibjckakei + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckakei
    function v5_eom_cc3_32_trans_aibjckakej(i, b, c, e) 
    double precision :: v5_eom_cc3_32_trans_aibjckakej   
    integer, intent(in) :: i, b, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, c, i)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckakej = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckakej = v5_eom_cc3_32_trans_aibjckakej + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckakej
    function v5_eom_cc3_32_trans_aibjckdiak(b, j, c, d) 
    double precision :: v5_eom_cc3_32_trans_aibjckdiak   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, j)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckdiak = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckdiak = v5_eom_cc3_32_trans_aibjckdiak + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckdiak
    function v5_eom_cc3_32_trans_aibjckdjai(b, c, k, d) 
    double precision :: v5_eom_cc3_32_trans_aibjckdjai   
    integer, intent(in) :: b, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, c, k)



    v5_eom_cc3_32_trans_aibjckdjai = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckdjai = v5_eom_cc3_32_trans_aibjckdjai + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckdjai
    function v5_eom_cc3_32_trans_aibjckdkai(b, j, c, d) 
    double precision :: v5_eom_cc3_32_trans_aibjckdkai   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, j)



    v5_eom_cc3_32_trans_aibjckdkai = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckdkai = v5_eom_cc3_32_trans_aibjckdkai + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckdkai
    function v5_eom_cc3_32_trans_aibjckdjak(i, b, c, d) 
    double precision :: v5_eom_cc3_32_trans_aibjckdjak   
    integer, intent(in) :: i, b, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, c, i)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckdjak = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckdjak = v5_eom_cc3_32_trans_aibjckdjak + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckdjak
    function v5_eom_cc3_32_trans_aibjckciej(a, b, k, e) 
    double precision :: v5_eom_cc3_32_trans_aibjckciej   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, k)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckciej = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckciej = v5_eom_cc3_32_trans_aibjckciej + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckciej
    function v5_eom_cc3_32_trans_aibjckciek(a, b, j, e) 
    double precision :: v5_eom_cc3_32_trans_aibjckciek   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, j)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckciek = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckciek = v5_eom_cc3_32_trans_aibjckciek + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckciek
    function v5_eom_cc3_32_trans_aibjckckei(a, b, j, e) 
    double precision :: v5_eom_cc3_32_trans_aibjckckei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, j)



    v5_eom_cc3_32_trans_aibjckckei = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckckei = v5_eom_cc3_32_trans_aibjckckei + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckckei
    function v5_eom_cc3_32_trans_aibjckckej(a, i, b, e) 
    double precision :: v5_eom_cc3_32_trans_aibjckckej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)



    v5_eom_cc3_32_trans_aibjckckej = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckckej = v5_eom_cc3_32_trans_aibjckckej + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckckej
    function v5_eom_cc3_32_trans_aibjckblci(a, j, k, l) 
    double precision :: v5_eom_cc3_32_trans_aibjckblci   
    integer, intent(in) :: a, j, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, l, j)



    v5_eom_cc3_32_trans_aibjckblci = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckblci = v5_eom_cc3_32_trans_aibjckblci + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckblci
    function v5_eom_cc3_32_trans_aibjckbjcm(a, i, k, m) 
    double precision :: v5_eom_cc3_32_trans_aibjckbjcm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)
term(1) = term(1) + tvooo(a, k, m, i)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckbjcm = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckbjcm = v5_eom_cc3_32_trans_aibjckbjcm + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbjcm
    function v5_eom_cc3_32_trans_aibjckblck(a, i, j, l) 
    double precision :: v5_eom_cc3_32_trans_aibjckblck   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckblck = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckblck = v5_eom_cc3_32_trans_aibjckblck + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckblck
    function v5_eom_cc3_32_trans_aibjckdick(a, b, j, d) 
    double precision :: v5_eom_cc3_32_trans_aibjckdick   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, j)



    v5_eom_cc3_32_trans_aibjckdick = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckdick = v5_eom_cc3_32_trans_aibjckdick + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckdick
    function v5_eom_cc3_32_trans_aibjckdjci(a, b, k, d) 
    double precision :: v5_eom_cc3_32_trans_aibjckdjci   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, k)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckdjci = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckdjci = v5_eom_cc3_32_trans_aibjckdjci + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckdjci
    function v5_eom_cc3_32_trans_aibjckdkci(a, b, j, d) 
    double precision :: v5_eom_cc3_32_trans_aibjckdkci   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, j)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckdkci = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckdkci = v5_eom_cc3_32_trans_aibjckdkci + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckdkci
    function v5_eom_cc3_32_trans_aibjckdjck(a, i, b, d) 
    double precision :: v5_eom_cc3_32_trans_aibjckdjck   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    v5_eom_cc3_32_trans_aibjckdjck = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckdjck = v5_eom_cc3_32_trans_aibjckdjck + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckdjck
    function v5_eom_cc3_32_trans_aibjckbjei(a, c, k, e) 
    double precision :: v5_eom_cc3_32_trans_aibjckbjei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, k)
term(1) = term(1) + tvvvo(a, e, c, k)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckbjei = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckbjei = v5_eom_cc3_32_trans_aibjckbjei + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbjei
    function v5_eom_cc3_32_trans_aibjckbjek(a, i, c, e) 
    double precision :: v5_eom_cc3_32_trans_aibjckbjek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)
term(1) = term(1) + tvvvo(a, e, c, i)

term(1) = -term(1) 


    v5_eom_cc3_32_trans_aibjckbjek = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckbjek = v5_eom_cc3_32_trans_aibjckbjek + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbjek
    function v5_eom_cc3_32_trans_aibjckdibj(a, c, k, d) 
    double precision :: v5_eom_cc3_32_trans_aibjckdibj   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, k)
term(1) = term(1) + tvvvo(a, d, c, k)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckdibj = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckdibj = v5_eom_cc3_32_trans_aibjckdibj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckdibj
    function v5_eom_cc3_32_trans_aibjckdkbj(a, i, c, d) 
    double precision :: v5_eom_cc3_32_trans_aibjckdkbj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)
term(1) = term(1) + tvvvo(a, d, c, i)

term(1) = -term(1) 


    v5_eom_cc3_32_trans_aibjckdkbj = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckdkbj = v5_eom_cc3_32_trans_aibjckdkbj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckdkbj
    function v5_eom_cc3_32_trans_aibjckaiaj(a, b, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckaiaj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, k)



    v5_eom_cc3_32_trans_aibjckaiaj = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckaiaj = v5_eom_cc3_32_trans_aibjckaiaj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckaiaj
    function v5_eom_cc3_32_trans_aibjckajai(a, b, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckajai   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, k)



    v5_eom_cc3_32_trans_aibjckajai = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckajai = v5_eom_cc3_32_trans_aibjckajai + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckajai
    function v5_eom_cc3_32_trans_aibjckajak(a, i, b, c) 
    double precision :: v5_eom_cc3_32_trans_aibjckajak   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, i)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckajak = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckajak = v5_eom_cc3_32_trans_aibjckajak + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckajak
    function v5_eom_cc3_32_trans_aibjckakaj(a, i, b, c) 
    double precision :: v5_eom_cc3_32_trans_aibjckakaj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, i)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckakaj = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckakaj = v5_eom_cc3_32_trans_aibjckakaj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckakaj
    function v5_eom_cc3_32_trans_aibjckaicj(b, j, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckaicj   
    integer, intent(in) :: b, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, k)
term(1) = term(1) + tvvvo(b, c, c, k)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckaicj = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckaicj = v5_eom_cc3_32_trans_aibjckaicj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckaicj
    function v5_eom_cc3_32_trans_aibjckaick(a, i, b, j, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckaick   
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


    v5_eom_cc3_32_trans_aibjckaick = 0.d+0
    do s = 0, 3
    v5_eom_cc3_32_trans_aibjckaick = v5_eom_cc3_32_trans_aibjckaick + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckaick
    function v5_eom_cc3_32_trans_aibjckajci(a, b, j, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckajci   
    integer, intent(in) :: a, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, k)
term(1) = term(1) + tvvvo(b, a, a, k)

term(1) = -term(1) 


    v5_eom_cc3_32_trans_aibjckajci = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckajci = v5_eom_cc3_32_trans_aibjckajci + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckajci
    function v5_eom_cc3_32_trans_aibjckakci(a, i, b, j, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckakci   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, k, k)
term(1) = term(1) + tvooo(b, j, i, i)
term(2) = term(2) + tvvvo(a, a, b, j)
term(3) = term(3) + tvvvo(c, c, b, j)

term(2) = -term(2) 
term(3) = -term(3) 


    v5_eom_cc3_32_trans_aibjckakci = 0.d+0
    do s = 0, 3
    v5_eom_cc3_32_trans_aibjckakci = v5_eom_cc3_32_trans_aibjckakci + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckakci
    function v5_eom_cc3_32_trans_aibjckajck(a, i, b, j) 
    double precision :: v5_eom_cc3_32_trans_aibjckajck   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)
term(1) = term(1) + tvvvo(b, a, a, i)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckajck = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckajck = v5_eom_cc3_32_trans_aibjckajck + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckajck
    function v5_eom_cc3_32_trans_aibjckakcj(i, b, j, c) 
    double precision :: v5_eom_cc3_32_trans_aibjckakcj   
    integer, intent(in) :: i, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)
term(1) = term(1) + tvvvo(b, c, c, i)

term(1) = -term(1) 


    v5_eom_cc3_32_trans_aibjckakcj = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckakcj = v5_eom_cc3_32_trans_aibjckakcj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckakcj
    function v5_eom_cc3_32_trans_aibjckaibi(i, j, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckaibi   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, i, j)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckaibi = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckaibi = v5_eom_cc3_32_trans_aibjckaibi + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckaibi
    function v5_eom_cc3_32_trans_aibjckaibj(a, i, b, j, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckaibj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, i, i)
term(1) = term(1) + tvooo(c, i, i, k)
term(2) = term(2) + tvooo(c, k, j, j)
term(3) = term(3) + tvvvo(c, a, a, k)
term(4) = term(4) + tvvvo(a, a, c, k)
term(5) = term(5) + tvvvo(b, b, c, k)

term(0) = -term(0) 
term(2) = -term(2) 
term(3) = -term(3) 


    v5_eom_cc3_32_trans_aibjckaibj = 0.d+0
    do s = 0, 5
    v5_eom_cc3_32_trans_aibjckaibj = v5_eom_cc3_32_trans_aibjckaibj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckaibj
    function v5_eom_cc3_32_trans_aibjckaibk(b, j, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckaibk   
    integer, intent(in) :: b, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, j)
term(1) = term(1) + tvvvo(c, b, b, j)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckaibk = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckaibk = v5_eom_cc3_32_trans_aibjckaibk + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckaibk
    function v5_eom_cc3_32_trans_aibjckakbi(i, b, j, c) 
    double precision :: v5_eom_cc3_32_trans_aibjckakbi   
    integer, intent(in) :: i, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, i, j)
term(1) = term(1) + tvvvo(c, b, b, j)

term(1) = -term(1) 


    v5_eom_cc3_32_trans_aibjckakbi = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckakbi = v5_eom_cc3_32_trans_aibjckakbi + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckakbi
    function v5_eom_cc3_32_trans_aibjckajbj(i, j, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckajbj   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, j, i)
term(1) = term(1) + tvooo(c, i, j, k)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckajbj = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckajbj = v5_eom_cc3_32_trans_aibjckajbj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckajbj
    function v5_eom_cc3_32_trans_aibjckakbj(a, i, b, j, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckakbj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)
term(1) = term(1) + tvooo(c, i, k, k)
term(2) = term(2) + tvooo(c, i, j, j)
term(3) = term(3) + tvvvo(c, a, a, i)
term(4) = term(4) + tvvvo(a, a, c, i)
term(5) = term(5) + tvvvo(b, b, c, i)

term(0) = -term(0) 
term(4) = -term(4) 
term(5) = -term(5) 


    v5_eom_cc3_32_trans_aibjckakbj = 0.d+0
    do s = 0, 5
    v5_eom_cc3_32_trans_aibjckakbj = v5_eom_cc3_32_trans_aibjckakbj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckakbj
    function v5_eom_cc3_32_trans_aibjckakbk(i, j, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckakbk   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, k, j)



    v5_eom_cc3_32_trans_aibjckakbk = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckakbk = v5_eom_cc3_32_trans_aibjckakbk + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckakbk
    function v5_eom_cc3_32_trans_aibjckcicj(a, b, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckcicj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, k)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckcicj = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckcicj = v5_eom_cc3_32_trans_aibjckcicj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckcicj
    function v5_eom_cc3_32_trans_aibjckcjci(a, b, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckcjci   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, k)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckcjci = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckcjci = v5_eom_cc3_32_trans_aibjckcjci + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckcjci
    function v5_eom_cc3_32_trans_aibjckcjck(a, i, b, c) 
    double precision :: v5_eom_cc3_32_trans_aibjckcjck   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, i)



    v5_eom_cc3_32_trans_aibjckcjck = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckcjck = v5_eom_cc3_32_trans_aibjckcjck + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckcjck
    function v5_eom_cc3_32_trans_aibjckckcj(a, i, b, c) 
    double precision :: v5_eom_cc3_32_trans_aibjckckcj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, i)



    v5_eom_cc3_32_trans_aibjckckcj = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckckcj = v5_eom_cc3_32_trans_aibjckckcj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckckcj
    function v5_eom_cc3_32_trans_aibjckbici(a, i, j, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckbici   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, i, j)



    v5_eom_cc3_32_trans_aibjckbici = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckbici = v5_eom_cc3_32_trans_aibjckbici + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbici
    function v5_eom_cc3_32_trans_aibjckbick(a, i, b, j) 
    double precision :: v5_eom_cc3_32_trans_aibjckbick   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)
term(1) = term(1) + tvvvo(a, b, b, j)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckbick = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckbick = v5_eom_cc3_32_trans_aibjckbick + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbick
    function v5_eom_cc3_32_trans_aibjckbjci(a, i, b, j, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckbjci   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, j, j)
term(1) = term(1) + tvooo(a, i, i, k)
term(2) = term(2) + tvooo(a, k, i, i)
term(3) = term(3) + tvvvo(b, b, a, k)
term(4) = term(4) + tvvvo(c, c, a, k)
term(5) = term(5) + tvvvo(a, c, c, k)

term(1) = -term(1) 
term(3) = -term(3) 
term(4) = -term(4) 


    v5_eom_cc3_32_trans_aibjckbjci = 0.d+0
    do s = 0, 5
    v5_eom_cc3_32_trans_aibjckbjci = v5_eom_cc3_32_trans_aibjckbjci + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbjci
    function v5_eom_cc3_32_trans_aibjckbkci(a, b, j, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckbkci   
    integer, intent(in) :: a, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, k, j)
term(1) = term(1) + tvvvo(a, b, b, j)

term(1) = -term(1) 


    v5_eom_cc3_32_trans_aibjckbkci = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckbkci = v5_eom_cc3_32_trans_aibjckbkci + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbkci
    function v5_eom_cc3_32_trans_aibjckbjcj(a, i, j, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckbjcj   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, k)
term(1) = term(1) + tvooo(a, k, j, i)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckbjcj = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckbjcj = v5_eom_cc3_32_trans_aibjckbjcj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbjcj
    function v5_eom_cc3_32_trans_aibjckbjck(a, i, b, j, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckbjck   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, i, k, k)
term(2) = term(2) + tvooo(a, k, k, i)
term(3) = term(3) + tvvvo(b, b, a, i)
term(4) = term(4) + tvvvo(c, c, a, i)
term(5) = term(5) + tvvvo(a, c, c, i)

term(0) = -term(0) 
term(1) = -term(1) 
term(5) = -term(5) 


    v5_eom_cc3_32_trans_aibjckbjck = 0.d+0
    do s = 0, 5
    v5_eom_cc3_32_trans_aibjckbjck = v5_eom_cc3_32_trans_aibjckbjck + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbjck
    function v5_eom_cc3_32_trans_aibjckbkck(a, i, j, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckbkck   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, j)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckbkck = 0.d+0
    do s = 0, 0
    v5_eom_cc3_32_trans_aibjckbkck = v5_eom_cc3_32_trans_aibjckbkck + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbkck
    function v5_eom_cc3_32_trans_aibjckbibj(a, b, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckbibj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, k)
term(1) = term(1) + tvvvo(a, b, c, k)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckbibj = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckbibj = v5_eom_cc3_32_trans_aibjckbibj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbibj
    function v5_eom_cc3_32_trans_aibjckbjbi(a, b, c, k) 
    double precision :: v5_eom_cc3_32_trans_aibjckbjbi   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, k)
term(1) = term(1) + tvvvo(a, b, c, k)

term(0) = -term(0) 


    v5_eom_cc3_32_trans_aibjckbjbi = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckbjbi = v5_eom_cc3_32_trans_aibjckbjbi + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbjbi
    function v5_eom_cc3_32_trans_aibjckbjbk(a, i, b, c) 
    double precision :: v5_eom_cc3_32_trans_aibjckbjbk   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, i)
term(1) = term(1) + tvvvo(a, b, c, i)

term(1) = -term(1) 


    v5_eom_cc3_32_trans_aibjckbjbk = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckbjbk = v5_eom_cc3_32_trans_aibjckbjbk + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbjbk
    function v5_eom_cc3_32_trans_aibjckbkbj(a, i, b, c) 
    double precision :: v5_eom_cc3_32_trans_aibjckbkbj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, i)
term(1) = term(1) + tvvvo(a, b, c, i)

term(1) = -term(1) 


    v5_eom_cc3_32_trans_aibjckbkbj = 0.d+0
    do s = 0, 1
    v5_eom_cc3_32_trans_aibjckbkbj = v5_eom_cc3_32_trans_aibjckbkbj + term(s)
    end do

    end function v5_eom_cc3_32_trans_aibjckbkbj
    end module v5_eom_cc3_32_trans
    
