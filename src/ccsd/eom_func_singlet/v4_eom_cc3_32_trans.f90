module v4_eom_cc3_32_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2013-02-27 23:45:57 UTC.
    !
    contains
    
    function v4_eom_cc3_32_trans_aibjckaicm(b, j, k, m) 
    double precision :: v4_eom_cc3_32_trans_aibjckaicm   
    integer, intent(in) :: b, j, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, m, k)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckaicm = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckaicm = v4_eom_cc3_32_trans_aibjckaicm + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckaicm
    function v4_eom_cc3_32_trans_aibjckalci(b, j, k, l) 
    double precision :: v4_eom_cc3_32_trans_aibjckalci   
    integer, intent(in) :: b, j, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, j)



    v4_eom_cc3_32_trans_aibjckalci = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckalci = v4_eom_cc3_32_trans_aibjckalci + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckalci
    function v4_eom_cc3_32_trans_aibjckajcm(i, b, k, m) 
    double precision :: v4_eom_cc3_32_trans_aibjckajcm   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, m, i)



    v4_eom_cc3_32_trans_aibjckajcm = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckajcm = v4_eom_cc3_32_trans_aibjckajcm + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckajcm
    function v4_eom_cc3_32_trans_aibjckalck(i, b, j, l) 
    double precision :: v4_eom_cc3_32_trans_aibjckalck   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, i)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckalck = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckalck = v4_eom_cc3_32_trans_aibjckalck + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckalck
    function v4_eom_cc3_32_trans_aibjckaibm(j, c, k, m) 
    double precision :: v4_eom_cc3_32_trans_aibjckaibm   
    integer, intent(in) :: j, c, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, m, j)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckaibm = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckaibm = v4_eom_cc3_32_trans_aibjckaibm + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckaibm
    function v4_eom_cc3_32_trans_aibjckajbm(i, c, k, m) 
    double precision :: v4_eom_cc3_32_trans_aibjckajbm   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, k)



    v4_eom_cc3_32_trans_aibjckajbm = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckajbm = v4_eom_cc3_32_trans_aibjckajbm + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckajbm
    function v4_eom_cc3_32_trans_aibjckalbj(i, c, k, l) 
    double precision :: v4_eom_cc3_32_trans_aibjckalbj   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, l, i)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckalbj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckalbj = v4_eom_cc3_32_trans_aibjckalbj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckalbj
    function v4_eom_cc3_32_trans_aibjckalbk(i, j, c, l) 
    double precision :: v4_eom_cc3_32_trans_aibjckalbk   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, j)



    v4_eom_cc3_32_trans_aibjckalbk = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckalbk = v4_eom_cc3_32_trans_aibjckalbk + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckalbk
    function v4_eom_cc3_32_trans_aibjckaiej(b, c, k, e) 
    double precision :: v4_eom_cc3_32_trans_aibjckaiej   
    integer, intent(in) :: b, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, c, k)



    v4_eom_cc3_32_trans_aibjckaiej = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckaiej = v4_eom_cc3_32_trans_aibjckaiej + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckaiej
    function v4_eom_cc3_32_trans_aibjckaiek(b, j, c, e) 
    double precision :: v4_eom_cc3_32_trans_aibjckaiek   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, j)



    v4_eom_cc3_32_trans_aibjckaiek = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckaiek = v4_eom_cc3_32_trans_aibjckaiek + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckaiek
    function v4_eom_cc3_32_trans_aibjckajei(b, c, k, e) 
    double precision :: v4_eom_cc3_32_trans_aibjckajei   
    integer, intent(in) :: b, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, k)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckajei = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckajei = v4_eom_cc3_32_trans_aibjckajei + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckajei
    function v4_eom_cc3_32_trans_aibjckajek(i, b, c, e) 
    double precision :: v4_eom_cc3_32_trans_aibjckajek   
    integer, intent(in) :: i, b, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, c, i)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckajek = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckajek = v4_eom_cc3_32_trans_aibjckajek + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckajek
    function v4_eom_cc3_32_trans_aibjckdiaj(b, c, k, d) 
    double precision :: v4_eom_cc3_32_trans_aibjckdiaj   
    integer, intent(in) :: b, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, k)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckdiaj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckdiaj = v4_eom_cc3_32_trans_aibjckdiaj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckdiaj
    function v4_eom_cc3_32_trans_aibjckdjai(b, c, k, d) 
    double precision :: v4_eom_cc3_32_trans_aibjckdjai   
    integer, intent(in) :: b, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, c, k)



    v4_eom_cc3_32_trans_aibjckdjai = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckdjai = v4_eom_cc3_32_trans_aibjckdjai + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckdjai
    function v4_eom_cc3_32_trans_aibjckdkai(b, j, c, d) 
    double precision :: v4_eom_cc3_32_trans_aibjckdkai   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, j)



    v4_eom_cc3_32_trans_aibjckdkai = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckdkai = v4_eom_cc3_32_trans_aibjckdkai + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckdkai
    function v4_eom_cc3_32_trans_aibjckdkaj(i, b, c, d) 
    double precision :: v4_eom_cc3_32_trans_aibjckdkaj   
    integer, intent(in) :: i, b, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, c, i)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckdkaj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckdkaj = v4_eom_cc3_32_trans_aibjckdkaj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckdkaj
    function v4_eom_cc3_32_trans_aibjckciej(a, b, k, e) 
    double precision :: v4_eom_cc3_32_trans_aibjckciej   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, k)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckciej = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckciej = v4_eom_cc3_32_trans_aibjckciej + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckciej
    function v4_eom_cc3_32_trans_aibjckciek(a, b, j, e) 
    double precision :: v4_eom_cc3_32_trans_aibjckciek   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, j)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckciek = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckciek = v4_eom_cc3_32_trans_aibjckciek + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckciek
    function v4_eom_cc3_32_trans_aibjckckei(a, b, j, e) 
    double precision :: v4_eom_cc3_32_trans_aibjckckei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, j)



    v4_eom_cc3_32_trans_aibjckckei = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckckei = v4_eom_cc3_32_trans_aibjckckei + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckckei
    function v4_eom_cc3_32_trans_aibjckckej(a, i, b, e) 
    double precision :: v4_eom_cc3_32_trans_aibjckckej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)



    v4_eom_cc3_32_trans_aibjckckej = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckckej = v4_eom_cc3_32_trans_aibjckckej + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckckej
    function v4_eom_cc3_32_trans_aibjckblci(a, j, k, l) 
    double precision :: v4_eom_cc3_32_trans_aibjckblci   
    integer, intent(in) :: a, j, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, k)



    v4_eom_cc3_32_trans_aibjckblci = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckblci = v4_eom_cc3_32_trans_aibjckblci + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckblci
    function v4_eom_cc3_32_trans_aibjckbjcm(a, i, k, m) 
    double precision :: v4_eom_cc3_32_trans_aibjckbjcm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckbjcm = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckbjcm = v4_eom_cc3_32_trans_aibjckbjcm + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbjcm
    function v4_eom_cc3_32_trans_aibjckbkcm(a, i, j, m) 
    double precision :: v4_eom_cc3_32_trans_aibjckbkcm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, i)



    v4_eom_cc3_32_trans_aibjckbkcm = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckbkcm = v4_eom_cc3_32_trans_aibjckbkcm + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbkcm
    function v4_eom_cc3_32_trans_aibjckblck(a, i, j, l) 
    double precision :: v4_eom_cc3_32_trans_aibjckblck   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckblck = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckblck = v4_eom_cc3_32_trans_aibjckblck + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckblck
    function v4_eom_cc3_32_trans_aibjckdick(a, b, j, d) 
    double precision :: v4_eom_cc3_32_trans_aibjckdick   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, j)



    v4_eom_cc3_32_trans_aibjckdick = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckdick = v4_eom_cc3_32_trans_aibjckdick + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckdick
    function v4_eom_cc3_32_trans_aibjckdjci(a, b, k, d) 
    double precision :: v4_eom_cc3_32_trans_aibjckdjci   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, k)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckdjci = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckdjci = v4_eom_cc3_32_trans_aibjckdjci + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckdjci
    function v4_eom_cc3_32_trans_aibjckdkci(a, b, j, d) 
    double precision :: v4_eom_cc3_32_trans_aibjckdkci   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, j)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckdkci = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckdkci = v4_eom_cc3_32_trans_aibjckdkci + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckdkci
    function v4_eom_cc3_32_trans_aibjckdjck(a, i, b, d) 
    double precision :: v4_eom_cc3_32_trans_aibjckdjck   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    v4_eom_cc3_32_trans_aibjckdjck = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckdjck = v4_eom_cc3_32_trans_aibjckdjck + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckdjck
    function v4_eom_cc3_32_trans_aibjckbjei(a, c, k, e) 
    double precision :: v4_eom_cc3_32_trans_aibjckbjei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, k)



    v4_eom_cc3_32_trans_aibjckbjei = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckbjei = v4_eom_cc3_32_trans_aibjckbjei + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbjei
    function v4_eom_cc3_32_trans_aibjckbkei(a, j, c, e) 
    double precision :: v4_eom_cc3_32_trans_aibjckbkei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, j)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckbkei = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckbkei = v4_eom_cc3_32_trans_aibjckbkei + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbkei
    function v4_eom_cc3_32_trans_aibjckbjek(a, i, c, e) 
    double precision :: v4_eom_cc3_32_trans_aibjckbjek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    v4_eom_cc3_32_trans_aibjckbjek = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckbjek = v4_eom_cc3_32_trans_aibjckbjek + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbjek
    function v4_eom_cc3_32_trans_aibjckbkej(a, i, c, e) 
    double precision :: v4_eom_cc3_32_trans_aibjckbkej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, i)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckbkej = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckbkej = v4_eom_cc3_32_trans_aibjckbkej + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbkej
    function v4_eom_cc3_32_trans_aibjckdibj(a, c, k, d) 
    double precision :: v4_eom_cc3_32_trans_aibjckdibj   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, k)



    v4_eom_cc3_32_trans_aibjckdibj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckdibj = v4_eom_cc3_32_trans_aibjckdibj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckdibj
    function v4_eom_cc3_32_trans_aibjckdibk(a, j, c, d) 
    double precision :: v4_eom_cc3_32_trans_aibjckdibk   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, j)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckdibk = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckdibk = v4_eom_cc3_32_trans_aibjckdibk + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckdibk
    function v4_eom_cc3_32_trans_aibjckdjbk(a, i, c, d) 
    double precision :: v4_eom_cc3_32_trans_aibjckdjbk   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, i)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckdjbk = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckdjbk = v4_eom_cc3_32_trans_aibjckdjbk + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckdjbk
    function v4_eom_cc3_32_trans_aibjckdkbj(a, i, c, d) 
    double precision :: v4_eom_cc3_32_trans_aibjckdkbj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)



    v4_eom_cc3_32_trans_aibjckdkbj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckdkbj = v4_eom_cc3_32_trans_aibjckdkbj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckdkbj
    function v4_eom_cc3_32_trans_aibjckaiaj(a, b, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckaiaj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, k)
term(1) = term(1) + tvvvo(b, a, c, k)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckaiaj = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckaiaj = v4_eom_cc3_32_trans_aibjckaiaj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckaiaj
    function v4_eom_cc3_32_trans_aibjckaiak(a, b, j, c) 
    double precision :: v4_eom_cc3_32_trans_aibjckaiak   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, j)



    v4_eom_cc3_32_trans_aibjckaiak = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckaiak = v4_eom_cc3_32_trans_aibjckaiak + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckaiak
    function v4_eom_cc3_32_trans_aibjckajai(a, b, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckajai   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, k)
term(1) = term(1) + tvvvo(c, a, b, k)

term(1) = -term(1) 


    v4_eom_cc3_32_trans_aibjckajai = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckajai = v4_eom_cc3_32_trans_aibjckajai + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckajai
    function v4_eom_cc3_32_trans_aibjckakai(a, b, j, c) 
    double precision :: v4_eom_cc3_32_trans_aibjckakai   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, j)



    v4_eom_cc3_32_trans_aibjckakai = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckakai = v4_eom_cc3_32_trans_aibjckakai + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckakai
    function v4_eom_cc3_32_trans_aibjckajak(a, i, b, c) 
    double precision :: v4_eom_cc3_32_trans_aibjckajak   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, i)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckajak = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckajak = v4_eom_cc3_32_trans_aibjckajak + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckajak
    function v4_eom_cc3_32_trans_aibjckakaj(a, i, b, c) 
    double precision :: v4_eom_cc3_32_trans_aibjckakaj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, i)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckakaj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckakaj = v4_eom_cc3_32_trans_aibjckakaj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckakaj
    function v4_eom_cc3_32_trans_aibjckaici(i, b, j, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckaici   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, i, j)
term(1) = term(1) + tvooo(b, j, i, k)

term(1) = -term(1) 


    v4_eom_cc3_32_trans_aibjckaici = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckaici = v4_eom_cc3_32_trans_aibjckaici + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckaici
    function v4_eom_cc3_32_trans_aibjckaicj(b, j, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckaicj   
    integer, intent(in) :: b, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, k)
term(1) = term(1) + tvvvo(b, c, c, k)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckaicj = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckaicj = v4_eom_cc3_32_trans_aibjckaicj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckaicj
    function v4_eom_cc3_32_trans_aibjckaick(a, i, b, j, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckaick   
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


    v4_eom_cc3_32_trans_aibjckaick = 0.d+0
    do s = 0, 3
    v4_eom_cc3_32_trans_aibjckaick = v4_eom_cc3_32_trans_aibjckaick + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckaick
    function v4_eom_cc3_32_trans_aibjckajci(a, i, b, j, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckajci   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, j, j)
term(1) = term(1) + tvooo(b, k, i, i)
term(2) = term(2) + tvvvo(a, a, b, k)
term(3) = term(3) + tvvvo(c, c, b, k)

term(2) = -term(2) 
term(3) = -term(3) 


    v4_eom_cc3_32_trans_aibjckajci = 0.d+0
    do s = 0, 3
    v4_eom_cc3_32_trans_aibjckajci = v4_eom_cc3_32_trans_aibjckajci + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckajci
    function v4_eom_cc3_32_trans_aibjckakci(a, b, j, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckakci   
    integer, intent(in) :: a, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, j)
term(1) = term(1) + tvvvo(b, a, a, j)

term(1) = -term(1) 


    v4_eom_cc3_32_trans_aibjckakci = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckakci = v4_eom_cc3_32_trans_aibjckakci + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckakci
    function v4_eom_cc3_32_trans_aibjckajcj(i, b, j, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckajcj   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, j, i)



    v4_eom_cc3_32_trans_aibjckajcj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckajcj = v4_eom_cc3_32_trans_aibjckajcj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckajcj
    function v4_eom_cc3_32_trans_aibjckajck(a, i, b, j, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckajck   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)
term(1) = term(1) + tvooo(b, k, k, i)
term(2) = term(2) + tvvvo(b, a, a, i)
term(3) = term(3) + tvvvo(b, c, c, i)

term(0) = -term(0) 
term(3) = -term(3) 


    v4_eom_cc3_32_trans_aibjckajck = 0.d+0
    do s = 0, 3
    v4_eom_cc3_32_trans_aibjckajck = v4_eom_cc3_32_trans_aibjckajck + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckajck
    function v4_eom_cc3_32_trans_aibjckakck(i, b, j, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckakck   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, k, i)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckakck = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckakck = v4_eom_cc3_32_trans_aibjckakck + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckakck
    function v4_eom_cc3_32_trans_aibjckaibi(i, j, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckaibi   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, i, j)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckaibi = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckaibi = v4_eom_cc3_32_trans_aibjckaibi + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckaibi
    function v4_eom_cc3_32_trans_aibjckaibj(a, i, b, j, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckaibj   
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


    v4_eom_cc3_32_trans_aibjckaibj = 0.d+0
    do s = 0, 3
    v4_eom_cc3_32_trans_aibjckaibj = v4_eom_cc3_32_trans_aibjckaibj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckaibj
    function v4_eom_cc3_32_trans_aibjckaibk(a, i, b, j, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckaibk   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, i, j)
term(1) = term(1) + tvooo(c, k, k, j)
term(2) = term(2) + tvvvo(c, a, a, j)
term(3) = term(3) + tvvvo(c, b, b, j)

term(1) = -term(1) 
term(2) = -term(2) 


    v4_eom_cc3_32_trans_aibjckaibk = 0.d+0
    do s = 0, 3
    v4_eom_cc3_32_trans_aibjckaibk = v4_eom_cc3_32_trans_aibjckaibk + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckaibk
    function v4_eom_cc3_32_trans_aibjckajbi(i, b, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckajbi   
    integer, intent(in) :: i, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, i, k)
term(1) = term(1) + tvvvo(c, b, b, k)

term(1) = -term(1) 


    v4_eom_cc3_32_trans_aibjckajbi = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckajbi = v4_eom_cc3_32_trans_aibjckajbi + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckajbi
    function v4_eom_cc3_32_trans_aibjckajbj(i, j, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckajbj   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, j, i)
term(1) = term(1) + tvooo(c, i, j, k)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckajbj = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckajbj = v4_eom_cc3_32_trans_aibjckajbj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckajbj
    function v4_eom_cc3_32_trans_aibjckajbk(a, i, b, j, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckajbk   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, j, j)
term(1) = term(1) + tvooo(c, i, k, k)
term(2) = term(2) + tvvvo(a, a, c, i)
term(3) = term(3) + tvvvo(b, b, c, i)

term(2) = -term(2) 
term(3) = -term(3) 


    v4_eom_cc3_32_trans_aibjckajbk = 0.d+0
    do s = 0, 3
    v4_eom_cc3_32_trans_aibjckajbk = v4_eom_cc3_32_trans_aibjckajbk + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckajbk
    function v4_eom_cc3_32_trans_aibjckakbj(a, i, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckakbj   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)
term(1) = term(1) + tvvvo(c, a, a, i)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckakbj = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckakbj = v4_eom_cc3_32_trans_aibjckakbj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckakbj
    function v4_eom_cc3_32_trans_aibjckakbk(i, j, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckakbk   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, k, j)



    v4_eom_cc3_32_trans_aibjckakbk = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckakbk = v4_eom_cc3_32_trans_aibjckakbk + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckakbk
    function v4_eom_cc3_32_trans_aibjckcicj(a, b, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckcicj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, k)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckcicj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckcicj = v4_eom_cc3_32_trans_aibjckcicj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckcicj
    function v4_eom_cc3_32_trans_aibjckcick(a, b, j, c) 
    double precision :: v4_eom_cc3_32_trans_aibjckcick   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, j)
term(1) = term(1) + tvvvo(b, c, a, j)

term(1) = -term(1) 


    v4_eom_cc3_32_trans_aibjckcick = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckcick = v4_eom_cc3_32_trans_aibjckcick + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckcick
    function v4_eom_cc3_32_trans_aibjckcjci(a, b, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckcjci   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, k)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckcjci = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckcjci = v4_eom_cc3_32_trans_aibjckcjci + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckcjci
    function v4_eom_cc3_32_trans_aibjckckci(a, b, j, c) 
    double precision :: v4_eom_cc3_32_trans_aibjckckci   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, j)
term(1) = term(1) + tvvvo(a, c, b, j)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckckci = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckckci = v4_eom_cc3_32_trans_aibjckckci + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckckci
    function v4_eom_cc3_32_trans_aibjckcjck(a, i, b, c) 
    double precision :: v4_eom_cc3_32_trans_aibjckcjck   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, i)



    v4_eom_cc3_32_trans_aibjckcjck = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckcjck = v4_eom_cc3_32_trans_aibjckcjck + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckcjck
    function v4_eom_cc3_32_trans_aibjckckcj(a, i, b, c) 
    double precision :: v4_eom_cc3_32_trans_aibjckckcj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, i)



    v4_eom_cc3_32_trans_aibjckckcj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckckcj = v4_eom_cc3_32_trans_aibjckckcj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckckcj
    function v4_eom_cc3_32_trans_aibjckbici(a, i, j, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckbici   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, k)



    v4_eom_cc3_32_trans_aibjckbici = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckbici = v4_eom_cc3_32_trans_aibjckbici + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbici
    function v4_eom_cc3_32_trans_aibjckbick(a, i, b, j) 
    double precision :: v4_eom_cc3_32_trans_aibjckbick   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)
term(1) = term(1) + tvvvo(a, b, b, j)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckbick = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckbick = v4_eom_cc3_32_trans_aibjckbick + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbick
    function v4_eom_cc3_32_trans_aibjckbjci(a, i, b, j, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckbjci   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, k)
term(1) = term(1) + tvooo(a, i, i, k)
term(2) = term(2) + tvvvo(a, b, b, k)
term(3) = term(3) + tvvvo(a, c, c, k)

term(1) = -term(1) 
term(2) = -term(2) 


    v4_eom_cc3_32_trans_aibjckbjci = 0.d+0
    do s = 0, 3
    v4_eom_cc3_32_trans_aibjckbjci = v4_eom_cc3_32_trans_aibjckbjci + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbjci
    function v4_eom_cc3_32_trans_aibjckbkci(a, i, b, j, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckbkci   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, k, k)
term(1) = term(1) + tvooo(a, j, i, i)
term(2) = term(2) + tvvvo(b, b, a, j)
term(3) = term(3) + tvvvo(c, c, a, j)

term(2) = -term(2) 
term(3) = -term(3) 


    v4_eom_cc3_32_trans_aibjckbkci = 0.d+0
    do s = 0, 3
    v4_eom_cc3_32_trans_aibjckbkci = v4_eom_cc3_32_trans_aibjckbkci + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbkci
    function v4_eom_cc3_32_trans_aibjckbjcj(a, i, j, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckbjcj   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, k)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckbjcj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckbjcj = v4_eom_cc3_32_trans_aibjckbjcj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbjcj
    function v4_eom_cc3_32_trans_aibjckbjck(a, i, b, j, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckbjck   
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


    v4_eom_cc3_32_trans_aibjckbjck = 0.d+0
    do s = 0, 3
    v4_eom_cc3_32_trans_aibjckbjck = v4_eom_cc3_32_trans_aibjckbjck + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbjck
    function v4_eom_cc3_32_trans_aibjckbkcj(a, i, j, c) 
    double precision :: v4_eom_cc3_32_trans_aibjckbkcj   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)
term(1) = term(1) + tvvvo(a, c, c, i)

term(1) = -term(1) 


    v4_eom_cc3_32_trans_aibjckbkcj = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckbkcj = v4_eom_cc3_32_trans_aibjckbkcj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbkcj
    function v4_eom_cc3_32_trans_aibjckbkck(a, i, j, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckbkck   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, j)
term(1) = term(1) + tvooo(a, j, k, i)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckbkck = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckbkck = v4_eom_cc3_32_trans_aibjckbkck + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbkck
    function v4_eom_cc3_32_trans_aibjckbibj(a, b, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckbibj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, k)



    v4_eom_cc3_32_trans_aibjckbibj = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckbibj = v4_eom_cc3_32_trans_aibjckbibj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbibj
    function v4_eom_cc3_32_trans_aibjckbibk(a, b, j, c) 
    double precision :: v4_eom_cc3_32_trans_aibjckbibk   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, j)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckbibk = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckbibk = v4_eom_cc3_32_trans_aibjckbibk + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbibk
    function v4_eom_cc3_32_trans_aibjckbjbi(a, b, c, k) 
    double precision :: v4_eom_cc3_32_trans_aibjckbjbi   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, k)



    v4_eom_cc3_32_trans_aibjckbjbi = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckbjbi = v4_eom_cc3_32_trans_aibjckbjbi + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbjbi
    function v4_eom_cc3_32_trans_aibjckbkbi(a, b, j, c) 
    double precision :: v4_eom_cc3_32_trans_aibjckbkbi   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, j)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckbkbi = 0.d+0
    do s = 0, 0
    v4_eom_cc3_32_trans_aibjckbkbi = v4_eom_cc3_32_trans_aibjckbkbi + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbkbi
    function v4_eom_cc3_32_trans_aibjckbjbk(a, i, b, c) 
    double precision :: v4_eom_cc3_32_trans_aibjckbjbk   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, i)
term(1) = term(1) + tvvvo(c, b, a, i)

term(0) = -term(0) 


    v4_eom_cc3_32_trans_aibjckbjbk = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckbjbk = v4_eom_cc3_32_trans_aibjckbjbk + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbjbk
    function v4_eom_cc3_32_trans_aibjckbkbj(a, i, b, c) 
    double precision :: v4_eom_cc3_32_trans_aibjckbkbj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, i)
term(1) = term(1) + tvvvo(a, b, c, i)

term(1) = -term(1) 


    v4_eom_cc3_32_trans_aibjckbkbj = 0.d+0
    do s = 0, 1
    v4_eom_cc3_32_trans_aibjckbkbj = v4_eom_cc3_32_trans_aibjckbkbj + term(s)
    end do

    end function v4_eom_cc3_32_trans_aibjckbkbj
    end module v4_eom_cc3_32_trans
    
