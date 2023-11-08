module v2_eom_cc3_32_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2013-02-27 23:45:57 UTC.
    !
    contains
    
    function v2_eom_cc3_32_trans_aibjckaicm(b, j, k, m) 
    double precision :: v2_eom_cc3_32_trans_aibjckaicm   
    integer, intent(in) :: b, j, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, m, k)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckaicm = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckaicm = v2_eom_cc3_32_trans_aibjckaicm + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckaicm
    function v2_eom_cc3_32_trans_aibjckajcm(i, b, k, m) 
    double precision :: v2_eom_cc3_32_trans_aibjckajcm   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, k)



    v2_eom_cc3_32_trans_aibjckajcm = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckajcm = v2_eom_cc3_32_trans_aibjckajcm + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckajcm
    function v2_eom_cc3_32_trans_aibjckalck(i, b, j, l) 
    double precision :: v2_eom_cc3_32_trans_aibjckalck   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, i)
term(1) = term(1) + tvooo(b, i, l, j)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckalck = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckalck = v2_eom_cc3_32_trans_aibjckalck + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckalck
    function v2_eom_cc3_32_trans_aibjckaibm(j, c, k, m) 
    double precision :: v2_eom_cc3_32_trans_aibjckaibm   
    integer, intent(in) :: j, c, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, m, j)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckaibm = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckaibm = v2_eom_cc3_32_trans_aibjckaibm + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckaibm
    function v2_eom_cc3_32_trans_aibjckalbi(j, c, k, l) 
    double precision :: v2_eom_cc3_32_trans_aibjckalbi   
    integer, intent(in) :: j, c, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, l, j)



    v2_eom_cc3_32_trans_aibjckalbi = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckalbi = v2_eom_cc3_32_trans_aibjckalbi + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckalbi
    function v2_eom_cc3_32_trans_aibjckajbm(i, c, k, m) 
    double precision :: v2_eom_cc3_32_trans_aibjckajbm   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, m, i)



    v2_eom_cc3_32_trans_aibjckajbm = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckajbm = v2_eom_cc3_32_trans_aibjckajbm + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckajbm
    function v2_eom_cc3_32_trans_aibjckalbj(i, c, k, l) 
    double precision :: v2_eom_cc3_32_trans_aibjckalbj   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, l, i)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckalbj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckalbj = v2_eom_cc3_32_trans_aibjckalbj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckalbj
    function v2_eom_cc3_32_trans_aibjckaiej(b, c, k, e) 
    double precision :: v2_eom_cc3_32_trans_aibjckaiej   
    integer, intent(in) :: b, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, c, k)



    v2_eom_cc3_32_trans_aibjckaiej = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckaiej = v2_eom_cc3_32_trans_aibjckaiej + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckaiej
    function v2_eom_cc3_32_trans_aibjckaiek(b, j, c, e) 
    double precision :: v2_eom_cc3_32_trans_aibjckaiek   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, j)



    v2_eom_cc3_32_trans_aibjckaiek = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckaiek = v2_eom_cc3_32_trans_aibjckaiek + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckaiek
    function v2_eom_cc3_32_trans_aibjckajei(b, c, k, e) 
    double precision :: v2_eom_cc3_32_trans_aibjckajei   
    integer, intent(in) :: b, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, c, k)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckajei = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckajei = v2_eom_cc3_32_trans_aibjckajei + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckajei
    function v2_eom_cc3_32_trans_aibjckajek(i, b, c, e) 
    double precision :: v2_eom_cc3_32_trans_aibjckajek   
    integer, intent(in) :: i, b, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, i)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckajek = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckajek = v2_eom_cc3_32_trans_aibjckajek + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckajek
    function v2_eom_cc3_32_trans_aibjckdiaj(b, c, k, d) 
    double precision :: v2_eom_cc3_32_trans_aibjckdiaj   
    integer, intent(in) :: b, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, c, k)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckdiaj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckdiaj = v2_eom_cc3_32_trans_aibjckdiaj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckdiaj
    function v2_eom_cc3_32_trans_aibjckdjai(b, c, k, d) 
    double precision :: v2_eom_cc3_32_trans_aibjckdjai   
    integer, intent(in) :: b, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, c, k)



    v2_eom_cc3_32_trans_aibjckdjai = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckdjai = v2_eom_cc3_32_trans_aibjckdjai + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckdjai
    function v2_eom_cc3_32_trans_aibjckdkai(b, j, c, d) 
    double precision :: v2_eom_cc3_32_trans_aibjckdkai   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, j)



    v2_eom_cc3_32_trans_aibjckdkai = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckdkai = v2_eom_cc3_32_trans_aibjckdkai + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckdkai
    function v2_eom_cc3_32_trans_aibjckdkaj(i, b, c, d) 
    double precision :: v2_eom_cc3_32_trans_aibjckdkaj   
    integer, intent(in) :: i, b, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, i)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckdkaj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckdkaj = v2_eom_cc3_32_trans_aibjckdkaj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckdkaj
    function v2_eom_cc3_32_trans_aibjckckei(a, b, j, e) 
    double precision :: v2_eom_cc3_32_trans_aibjckckei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, j)
term(1) = term(1) + tvvvo(a, e, b, j)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckckei = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckckei = v2_eom_cc3_32_trans_aibjckckei + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckckei
    function v2_eom_cc3_32_trans_aibjckckej(a, i, b, e) 
    double precision :: v2_eom_cc3_32_trans_aibjckckej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)
term(1) = term(1) + tvvvo(a, e, b, i)

term(1) = -term(1) 


    v2_eom_cc3_32_trans_aibjckckej = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckckej = v2_eom_cc3_32_trans_aibjckckej + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckckej
    function v2_eom_cc3_32_trans_aibjckbicm(a, j, k, m) 
    double precision :: v2_eom_cc3_32_trans_aibjckbicm   
    integer, intent(in) :: a, j, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, k)



    v2_eom_cc3_32_trans_aibjckbicm = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckbicm = v2_eom_cc3_32_trans_aibjckbicm + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbicm
    function v2_eom_cc3_32_trans_aibjckbjcm(a, i, k, m) 
    double precision :: v2_eom_cc3_32_trans_aibjckbjcm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckbjcm = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckbjcm = v2_eom_cc3_32_trans_aibjckbjcm + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbjcm
    function v2_eom_cc3_32_trans_aibjckblck(a, i, j, l) 
    double precision :: v2_eom_cc3_32_trans_aibjckblck   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)
term(1) = term(1) + tvooo(a, j, l, i)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckblck = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckblck = v2_eom_cc3_32_trans_aibjckblck + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckblck
    function v2_eom_cc3_32_trans_aibjckdick(a, b, j, d) 
    double precision :: v2_eom_cc3_32_trans_aibjckdick   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, j)
term(1) = term(1) + tvvvo(a, d, b, j)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckdick = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckdick = v2_eom_cc3_32_trans_aibjckdick + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckdick
    function v2_eom_cc3_32_trans_aibjckdjck(a, i, b, d) 
    double precision :: v2_eom_cc3_32_trans_aibjckdjck   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)
term(1) = term(1) + tvvvo(a, d, b, i)

term(1) = -term(1) 


    v2_eom_cc3_32_trans_aibjckdjck = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckdjck = v2_eom_cc3_32_trans_aibjckdjck + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckdjck
    function v2_eom_cc3_32_trans_aibjckbiej(a, c, k, e) 
    double precision :: v2_eom_cc3_32_trans_aibjckbiej   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, k)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckbiej = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckbiej = v2_eom_cc3_32_trans_aibjckbiej + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbiej
    function v2_eom_cc3_32_trans_aibjckbiek(a, j, c, e) 
    double precision :: v2_eom_cc3_32_trans_aibjckbiek   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, j)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckbiek = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckbiek = v2_eom_cc3_32_trans_aibjckbiek + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbiek
    function v2_eom_cc3_32_trans_aibjckbjei(a, c, k, e) 
    double precision :: v2_eom_cc3_32_trans_aibjckbjei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, k)



    v2_eom_cc3_32_trans_aibjckbjei = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckbjei = v2_eom_cc3_32_trans_aibjckbjei + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbjei
    function v2_eom_cc3_32_trans_aibjckbjek(a, i, c, e) 
    double precision :: v2_eom_cc3_32_trans_aibjckbjek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    v2_eom_cc3_32_trans_aibjckbjek = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckbjek = v2_eom_cc3_32_trans_aibjckbjek + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbjek
    function v2_eom_cc3_32_trans_aibjckdibj(a, c, k, d) 
    double precision :: v2_eom_cc3_32_trans_aibjckdibj   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, k)



    v2_eom_cc3_32_trans_aibjckdibj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckdibj = v2_eom_cc3_32_trans_aibjckdibj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckdibj
    function v2_eom_cc3_32_trans_aibjckdjbi(a, c, k, d) 
    double precision :: v2_eom_cc3_32_trans_aibjckdjbi   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, k)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckdjbi = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckdjbi = v2_eom_cc3_32_trans_aibjckdjbi + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckdjbi
    function v2_eom_cc3_32_trans_aibjckdkbi(a, j, c, d) 
    double precision :: v2_eom_cc3_32_trans_aibjckdkbi   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, j)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckdkbi = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckdkbi = v2_eom_cc3_32_trans_aibjckdkbi + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckdkbi
    function v2_eom_cc3_32_trans_aibjckdkbj(a, i, c, d) 
    double precision :: v2_eom_cc3_32_trans_aibjckdkbj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)



    v2_eom_cc3_32_trans_aibjckdkbj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckdkbj = v2_eom_cc3_32_trans_aibjckdkbj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckdkbj
    function v2_eom_cc3_32_trans_aibjckaiak(a, b, j, c) 
    double precision :: v2_eom_cc3_32_trans_aibjckaiak   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, j)



    v2_eom_cc3_32_trans_aibjckaiak = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckaiak = v2_eom_cc3_32_trans_aibjckaiak + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckaiak
    function v2_eom_cc3_32_trans_aibjckakai(a, b, j, c) 
    double precision :: v2_eom_cc3_32_trans_aibjckakai   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, j)



    v2_eom_cc3_32_trans_aibjckakai = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckakai = v2_eom_cc3_32_trans_aibjckakai + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckakai
    function v2_eom_cc3_32_trans_aibjckajak(a, i, b, c) 
    double precision :: v2_eom_cc3_32_trans_aibjckajak   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, i)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckajak = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckajak = v2_eom_cc3_32_trans_aibjckajak + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckajak
    function v2_eom_cc3_32_trans_aibjckakaj(a, i, b, c) 
    double precision :: v2_eom_cc3_32_trans_aibjckakaj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, i)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckakaj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckakaj = v2_eom_cc3_32_trans_aibjckakaj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckakaj
    function v2_eom_cc3_32_trans_aibjckaici(i, b, j, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckaici   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, i, k)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckaici = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckaici = v2_eom_cc3_32_trans_aibjckaici + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckaici
    function v2_eom_cc3_32_trans_aibjckaicj(b, j, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckaicj   
    integer, intent(in) :: b, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, k)
term(1) = term(1) + tvvvo(b, c, c, k)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckaicj = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckaicj = v2_eom_cc3_32_trans_aibjckaicj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckaicj
    function v2_eom_cc3_32_trans_aibjckaick(a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckaick   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, i, i)
term(1) = term(1) + tvooo(b, i, i, j)
term(2) = term(2) + tvooo(b, j, k, k)
term(3) = term(3) + tvvvo(b, a, a, j)
term(4) = term(4) + tvvvo(a, a, b, j)
term(5) = term(5) + tvvvo(c, c, b, j)

term(0) = -term(0) 
term(2) = -term(2) 
term(3) = -term(3) 


    v2_eom_cc3_32_trans_aibjckaick = 0.d+0
    do s = 0, 5
    v2_eom_cc3_32_trans_aibjckaick = v2_eom_cc3_32_trans_aibjckaick + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckaick
    function v2_eom_cc3_32_trans_aibjckajci(i, b, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckajci   
    integer, intent(in) :: i, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, i, k)
term(1) = term(1) + tvvvo(b, c, c, k)

term(1) = -term(1) 


    v2_eom_cc3_32_trans_aibjckajci = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckajci = v2_eom_cc3_32_trans_aibjckajci + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckajci
    function v2_eom_cc3_32_trans_aibjckajcj(i, b, j, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckajcj   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, k)



    v2_eom_cc3_32_trans_aibjckajcj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckajcj = v2_eom_cc3_32_trans_aibjckajcj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckajcj
    function v2_eom_cc3_32_trans_aibjckajck(a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckajck   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)
term(1) = term(1) + tvooo(b, i, j, j)
term(2) = term(2) + tvooo(b, i, k, k)
term(3) = term(3) + tvvvo(b, a, a, i)
term(4) = term(4) + tvvvo(a, a, b, i)
term(5) = term(5) + tvvvo(c, c, b, i)

term(0) = -term(0) 
term(4) = -term(4) 
term(5) = -term(5) 


    v2_eom_cc3_32_trans_aibjckajck = 0.d+0
    do s = 0, 5
    v2_eom_cc3_32_trans_aibjckajck = v2_eom_cc3_32_trans_aibjckajck + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckajck
    function v2_eom_cc3_32_trans_aibjckakck(i, b, j, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckakck   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, k, i)
term(1) = term(1) + tvooo(b, i, k, j)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckakck = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckakck = v2_eom_cc3_32_trans_aibjckakck + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckakck
    function v2_eom_cc3_32_trans_aibjckaibj(a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckaibj   
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


    v2_eom_cc3_32_trans_aibjckaibj = 0.d+0
    do s = 0, 3
    v2_eom_cc3_32_trans_aibjckaibj = v2_eom_cc3_32_trans_aibjckaibj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckaibj
    function v2_eom_cc3_32_trans_aibjckaibk(b, j, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckaibk   
    integer, intent(in) :: b, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, j)
term(1) = term(1) + tvvvo(c, b, b, j)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckaibk = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckaibk = v2_eom_cc3_32_trans_aibjckaibk + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckaibk
    function v2_eom_cc3_32_trans_aibjckajbi(a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckajbi   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, j, j)
term(1) = term(1) + tvooo(c, k, i, i)
term(2) = term(2) + tvvvo(a, a, c, k)
term(3) = term(3) + tvvvo(b, b, c, k)

term(2) = -term(2) 
term(3) = -term(3) 


    v2_eom_cc3_32_trans_aibjckajbi = 0.d+0
    do s = 0, 3
    v2_eom_cc3_32_trans_aibjckajbi = v2_eom_cc3_32_trans_aibjckajbi + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckajbi
    function v2_eom_cc3_32_trans_aibjckakbi(a, j, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckakbi   
    integer, intent(in) :: a, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, j)
term(1) = term(1) + tvvvo(c, a, a, j)

term(1) = -term(1) 


    v2_eom_cc3_32_trans_aibjckakbi = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckakbi = v2_eom_cc3_32_trans_aibjckakbi + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckakbi
    function v2_eom_cc3_32_trans_aibjckajbk(i, b, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckajbk   
    integer, intent(in) :: i, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)
term(1) = term(1) + tvvvo(c, b, b, i)

term(1) = -term(1) 


    v2_eom_cc3_32_trans_aibjckajbk = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckajbk = v2_eom_cc3_32_trans_aibjckajbk + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckajbk
    function v2_eom_cc3_32_trans_aibjckakbj(a, i, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckakbj   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)
term(1) = term(1) + tvvvo(c, a, a, i)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckakbj = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckakbj = v2_eom_cc3_32_trans_aibjckakbj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckakbj
    function v2_eom_cc3_32_trans_aibjckcick(a, b, j, c) 
    double precision :: v2_eom_cc3_32_trans_aibjckcick   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, j)
term(1) = term(1) + tvvvo(a, c, b, j)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckcick = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckcick = v2_eom_cc3_32_trans_aibjckcick + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckcick
    function v2_eom_cc3_32_trans_aibjckckci(a, b, j, c) 
    double precision :: v2_eom_cc3_32_trans_aibjckckci   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, j)
term(1) = term(1) + tvvvo(a, c, b, j)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckckci = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckckci = v2_eom_cc3_32_trans_aibjckckci + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckckci
    function v2_eom_cc3_32_trans_aibjckcjck(a, i, b, c) 
    double precision :: v2_eom_cc3_32_trans_aibjckcjck   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, i)
term(1) = term(1) + tvvvo(a, c, b, i)

term(1) = -term(1) 


    v2_eom_cc3_32_trans_aibjckcjck = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckcjck = v2_eom_cc3_32_trans_aibjckcjck + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckcjck
    function v2_eom_cc3_32_trans_aibjckckcj(a, i, b, c) 
    double precision :: v2_eom_cc3_32_trans_aibjckckcj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, i)
term(1) = term(1) + tvvvo(a, c, b, i)

term(1) = -term(1) 


    v2_eom_cc3_32_trans_aibjckckcj = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckckcj = v2_eom_cc3_32_trans_aibjckckcj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckckcj
    function v2_eom_cc3_32_trans_aibjckbici(a, i, j, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckbici   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, k)



    v2_eom_cc3_32_trans_aibjckbici = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckbici = v2_eom_cc3_32_trans_aibjckbici + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbici
    function v2_eom_cc3_32_trans_aibjckbicj(a, j, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckbicj   
    integer, intent(in) :: a, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, k)
term(1) = term(1) + tvvvo(a, c, c, k)

term(1) = -term(1) 


    v2_eom_cc3_32_trans_aibjckbicj = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckbicj = v2_eom_cc3_32_trans_aibjckbicj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbicj
    function v2_eom_cc3_32_trans_aibjckbick(a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckbick   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)
term(1) = term(1) + tvooo(a, j, i, i)
term(2) = term(2) + tvooo(a, j, k, k)
term(3) = term(3) + tvvvo(b, b, a, j)
term(4) = term(4) + tvvvo(a, b, b, j)
term(5) = term(5) + tvvvo(c, c, a, j)

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = -term(5) 


    v2_eom_cc3_32_trans_aibjckbick = 0.d+0
    do s = 0, 5
    v2_eom_cc3_32_trans_aibjckbick = v2_eom_cc3_32_trans_aibjckbick + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbick
    function v2_eom_cc3_32_trans_aibjckbjci(a, i, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckbjci   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)
term(1) = term(1) + tvvvo(a, c, c, k)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckbjci = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckbjci = v2_eom_cc3_32_trans_aibjckbjci + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbjci
    function v2_eom_cc3_32_trans_aibjckbjcj(a, i, j, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckbjcj   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, k)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckbjcj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckbjcj = v2_eom_cc3_32_trans_aibjckbjcj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbjcj
    function v2_eom_cc3_32_trans_aibjckbjck(a, i, b, j, c, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckbjck   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, j, j, i)
term(2) = term(2) + tvooo(a, i, k, k)
term(3) = term(3) + tvvvo(b, b, a, i)
term(4) = term(4) + tvvvo(a, b, b, i)
term(5) = term(5) + tvvvo(c, c, a, i)

term(0) = -term(0) 
term(2) = -term(2) 
term(4) = -term(4) 


    v2_eom_cc3_32_trans_aibjckbjck = 0.d+0
    do s = 0, 5
    v2_eom_cc3_32_trans_aibjckbjck = v2_eom_cc3_32_trans_aibjckbjck + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbjck
    function v2_eom_cc3_32_trans_aibjckbkck(a, i, j, k) 
    double precision :: v2_eom_cc3_32_trans_aibjckbkck   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, j)
term(1) = term(1) + tvooo(a, j, k, i)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckbkck = 0.d+0
    do s = 0, 1
    v2_eom_cc3_32_trans_aibjckbkck = v2_eom_cc3_32_trans_aibjckbkck + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbkck
    function v2_eom_cc3_32_trans_aibjckbibk(a, b, j, c) 
    double precision :: v2_eom_cc3_32_trans_aibjckbibk   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, j)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckbibk = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckbibk = v2_eom_cc3_32_trans_aibjckbibk + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbibk
    function v2_eom_cc3_32_trans_aibjckbkbi(a, b, j, c) 
    double precision :: v2_eom_cc3_32_trans_aibjckbkbi   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, j)

term(0) = -term(0) 


    v2_eom_cc3_32_trans_aibjckbkbi = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckbkbi = v2_eom_cc3_32_trans_aibjckbkbi + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbkbi
    function v2_eom_cc3_32_trans_aibjckbjbk(a, i, b, c) 
    double precision :: v2_eom_cc3_32_trans_aibjckbjbk   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, i)



    v2_eom_cc3_32_trans_aibjckbjbk = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckbjbk = v2_eom_cc3_32_trans_aibjckbjbk + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbjbk
    function v2_eom_cc3_32_trans_aibjckbkbj(a, i, b, c) 
    double precision :: v2_eom_cc3_32_trans_aibjckbkbj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, i)



    v2_eom_cc3_32_trans_aibjckbkbj = 0.d+0
    do s = 0, 0
    v2_eom_cc3_32_trans_aibjckbkbj = v2_eom_cc3_32_trans_aibjckbkbj + term(s)
    end do

    end function v2_eom_cc3_32_trans_aibjckbkbj
    end module v2_eom_cc3_32_trans
    
