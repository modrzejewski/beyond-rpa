module v1_eom_cc3_32_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2013-02-27 23:45:57 UTC.
    !
    contains
    
    function v1_eom_cc3_32_trans_aibjckaicm(b, j, k, m) 
    double precision :: v1_eom_cc3_32_trans_aibjckaicm   
    integer, intent(in) :: b, j, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, m, j)
term(1) = term(1) + tvooo(b, j, m, k)

term(1) = -term(1) 


    v1_eom_cc3_32_trans_aibjckaicm = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckaicm = v1_eom_cc3_32_trans_aibjckaicm + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckaicm
    function v1_eom_cc3_32_trans_aibjckalcj(i, b, k, l) 
    double precision :: v1_eom_cc3_32_trans_aibjckalcj   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, i)



    v1_eom_cc3_32_trans_aibjckalcj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckalcj = v1_eom_cc3_32_trans_aibjckalcj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckalcj
    function v1_eom_cc3_32_trans_aibjckalck(i, b, j, l) 
    double precision :: v1_eom_cc3_32_trans_aibjckalck   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, i)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckalck = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckalck = v1_eom_cc3_32_trans_aibjckalck + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckalck
    function v1_eom_cc3_32_trans_aibjckaibm(j, c, k, m) 
    double precision :: v1_eom_cc3_32_trans_aibjckaibm   
    integer, intent(in) :: j, c, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, k)
term(1) = term(1) + tvooo(c, k, m, j)

term(1) = -term(1) 


    v1_eom_cc3_32_trans_aibjckaibm = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckaibm = v1_eom_cc3_32_trans_aibjckaibm + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckaibm
    function v1_eom_cc3_32_trans_aibjckalbj(i, c, k, l) 
    double precision :: v1_eom_cc3_32_trans_aibjckalbj   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, l, i)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckalbj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckalbj = v1_eom_cc3_32_trans_aibjckalbj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckalbj
    function v1_eom_cc3_32_trans_aibjckalbk(i, j, c, l) 
    double precision :: v1_eom_cc3_32_trans_aibjckalbk   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, i)



    v1_eom_cc3_32_trans_aibjckalbk = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckalbk = v1_eom_cc3_32_trans_aibjckalbk + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckalbk
    function v1_eom_cc3_32_trans_aibjckaiej(b, c, k, e) 
    double precision :: v1_eom_cc3_32_trans_aibjckaiej   
    integer, intent(in) :: b, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, k)
term(1) = term(1) + tvvvo(b, e, c, k)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckaiej = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckaiej = v1_eom_cc3_32_trans_aibjckaiej + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckaiej
    function v1_eom_cc3_32_trans_aibjckaiek(b, j, c, e) 
    double precision :: v1_eom_cc3_32_trans_aibjckaiek   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, j)
term(1) = term(1) + tvvvo(b, e, c, j)

term(1) = -term(1) 


    v1_eom_cc3_32_trans_aibjckaiek = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckaiek = v1_eom_cc3_32_trans_aibjckaiek + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckaiek
    function v1_eom_cc3_32_trans_aibjckdjai(b, c, k, d) 
    double precision :: v1_eom_cc3_32_trans_aibjckdjai   
    integer, intent(in) :: b, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, k)
term(1) = term(1) + tvvvo(b, d, c, k)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckdjai = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckdjai = v1_eom_cc3_32_trans_aibjckdjai + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckdjai
    function v1_eom_cc3_32_trans_aibjckdkai(b, j, c, d) 
    double precision :: v1_eom_cc3_32_trans_aibjckdkai   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, j)
term(1) = term(1) + tvvvo(b, d, c, j)

term(1) = -term(1) 


    v1_eom_cc3_32_trans_aibjckdkai = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckdkai = v1_eom_cc3_32_trans_aibjckdkai + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckdkai
    function v1_eom_cc3_32_trans_aibjckcjei(a, b, k, e) 
    double precision :: v1_eom_cc3_32_trans_aibjckcjei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, k)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckcjei = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckcjei = v1_eom_cc3_32_trans_aibjckcjei + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckcjei
    function v1_eom_cc3_32_trans_aibjckckei(a, b, j, e) 
    double precision :: v1_eom_cc3_32_trans_aibjckckei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, j)



    v1_eom_cc3_32_trans_aibjckckei = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckckei = v1_eom_cc3_32_trans_aibjckckei + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckckei
    function v1_eom_cc3_32_trans_aibjckcjek(a, i, b, e) 
    double precision :: v1_eom_cc3_32_trans_aibjckcjek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckcjek = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckcjek = v1_eom_cc3_32_trans_aibjckcjek + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckcjek
    function v1_eom_cc3_32_trans_aibjckckej(a, i, b, e) 
    double precision :: v1_eom_cc3_32_trans_aibjckckej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)



    v1_eom_cc3_32_trans_aibjckckej = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckckej = v1_eom_cc3_32_trans_aibjckckej + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckckej
    function v1_eom_cc3_32_trans_aibjckbjcm(a, i, k, m) 
    double precision :: v1_eom_cc3_32_trans_aibjckbjcm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckbjcm = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckbjcm = v1_eom_cc3_32_trans_aibjckbjcm + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbjcm
    function v1_eom_cc3_32_trans_aibjckblcj(a, i, k, l) 
    double precision :: v1_eom_cc3_32_trans_aibjckblcj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)



    v1_eom_cc3_32_trans_aibjckblcj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckblcj = v1_eom_cc3_32_trans_aibjckblcj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckblcj
    function v1_eom_cc3_32_trans_aibjckbkcm(a, i, j, m) 
    double precision :: v1_eom_cc3_32_trans_aibjckbkcm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    v1_eom_cc3_32_trans_aibjckbkcm = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckbkcm = v1_eom_cc3_32_trans_aibjckbkcm + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbkcm
    function v1_eom_cc3_32_trans_aibjckblck(a, i, j, l) 
    double precision :: v1_eom_cc3_32_trans_aibjckblck   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckblck = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckblck = v1_eom_cc3_32_trans_aibjckblck + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckblck
    function v1_eom_cc3_32_trans_aibjckdicj(a, b, k, d) 
    double precision :: v1_eom_cc3_32_trans_aibjckdicj   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, k)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckdicj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckdicj = v1_eom_cc3_32_trans_aibjckdicj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckdicj
    function v1_eom_cc3_32_trans_aibjckdick(a, b, j, d) 
    double precision :: v1_eom_cc3_32_trans_aibjckdick   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, j)



    v1_eom_cc3_32_trans_aibjckdick = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckdick = v1_eom_cc3_32_trans_aibjckdick + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckdick
    function v1_eom_cc3_32_trans_aibjckdjck(a, i, b, d) 
    double precision :: v1_eom_cc3_32_trans_aibjckdjck   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    v1_eom_cc3_32_trans_aibjckdjck = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckdjck = v1_eom_cc3_32_trans_aibjckdjck + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckdjck
    function v1_eom_cc3_32_trans_aibjckdkcj(a, i, b, d) 
    double precision :: v1_eom_cc3_32_trans_aibjckdkcj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckdkcj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckdkcj = v1_eom_cc3_32_trans_aibjckdkcj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckdkcj
    function v1_eom_cc3_32_trans_aibjckbjei(a, c, k, e) 
    double precision :: v1_eom_cc3_32_trans_aibjckbjei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, k)



    v1_eom_cc3_32_trans_aibjckbjei = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckbjei = v1_eom_cc3_32_trans_aibjckbjei + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbjei
    function v1_eom_cc3_32_trans_aibjckbkei(a, j, c, e) 
    double precision :: v1_eom_cc3_32_trans_aibjckbkei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, j)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckbkei = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckbkei = v1_eom_cc3_32_trans_aibjckbkei + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbkei
    function v1_eom_cc3_32_trans_aibjckbjek(a, i, c, e) 
    double precision :: v1_eom_cc3_32_trans_aibjckbjek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    v1_eom_cc3_32_trans_aibjckbjek = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckbjek = v1_eom_cc3_32_trans_aibjckbjek + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbjek
    function v1_eom_cc3_32_trans_aibjckbkej(a, i, c, e) 
    double precision :: v1_eom_cc3_32_trans_aibjckbkej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckbkej = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckbkej = v1_eom_cc3_32_trans_aibjckbkej + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbkej
    function v1_eom_cc3_32_trans_aibjckdibj(a, c, k, d) 
    double precision :: v1_eom_cc3_32_trans_aibjckdibj   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, k)



    v1_eom_cc3_32_trans_aibjckdibj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckdibj = v1_eom_cc3_32_trans_aibjckdibj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckdibj
    function v1_eom_cc3_32_trans_aibjckdibk(a, j, c, d) 
    double precision :: v1_eom_cc3_32_trans_aibjckdibk   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, j)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckdibk = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckdibk = v1_eom_cc3_32_trans_aibjckdibk + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckdibk
    function v1_eom_cc3_32_trans_aibjckdjbk(a, i, c, d) 
    double precision :: v1_eom_cc3_32_trans_aibjckdjbk   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckdjbk = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckdjbk = v1_eom_cc3_32_trans_aibjckdjbk + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckdjbk
    function v1_eom_cc3_32_trans_aibjckdkbj(a, i, c, d) 
    double precision :: v1_eom_cc3_32_trans_aibjckdkbj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)



    v1_eom_cc3_32_trans_aibjckdkbj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckdkbj = v1_eom_cc3_32_trans_aibjckdkbj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckdkbj
    function v1_eom_cc3_32_trans_aibjckaiaj(a, b, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckaiaj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, k)
term(1) = term(1) + tvvvo(b, a, c, k)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckaiaj = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckaiaj = v1_eom_cc3_32_trans_aibjckaiaj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckaiaj
    function v1_eom_cc3_32_trans_aibjckaiak(a, b, j, c) 
    double precision :: v1_eom_cc3_32_trans_aibjckaiak   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, j)
term(1) = term(1) + tvvvo(b, a, c, j)

term(1) = -term(1) 


    v1_eom_cc3_32_trans_aibjckaiak = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckaiak = v1_eom_cc3_32_trans_aibjckaiak + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckaiak
    function v1_eom_cc3_32_trans_aibjckajai(a, b, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckajai   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, k)
term(1) = term(1) + tvvvo(b, a, c, k)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckajai = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckajai = v1_eom_cc3_32_trans_aibjckajai + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckajai
    function v1_eom_cc3_32_trans_aibjckakai(a, b, j, c) 
    double precision :: v1_eom_cc3_32_trans_aibjckakai   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, j)
term(1) = term(1) + tvvvo(b, a, c, j)

term(1) = -term(1) 


    v1_eom_cc3_32_trans_aibjckakai = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckakai = v1_eom_cc3_32_trans_aibjckakai + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckakai
    function v1_eom_cc3_32_trans_aibjckaici(i, b, j, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckaici   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, i, j)
term(1) = term(1) + tvooo(b, j, i, k)

term(1) = -term(1) 


    v1_eom_cc3_32_trans_aibjckaici = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckaici = v1_eom_cc3_32_trans_aibjckaici + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckaici
    function v1_eom_cc3_32_trans_aibjckaicj(a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckaicj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, i, i)
term(1) = term(1) + tvooo(b, k, j, j)
term(2) = term(2) + tvooo(b, j, j, k)
term(3) = term(3) + tvvvo(a, a, b, k)
term(4) = term(4) + tvvvo(c, c, b, k)
term(5) = term(5) + tvvvo(b, c, c, k)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 


    v1_eom_cc3_32_trans_aibjckaicj = 0.d+0
    do s = 0, 5
    v1_eom_cc3_32_trans_aibjckaicj = v1_eom_cc3_32_trans_aibjckaicj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckaicj
    function v1_eom_cc3_32_trans_aibjckaick(a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckaick   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, i, i)
term(1) = term(1) + tvooo(b, k, k, j)
term(2) = term(2) + tvooo(b, j, k, k)
term(3) = term(3) + tvvvo(a, a, b, j)
term(4) = term(4) + tvvvo(c, c, b, j)
term(5) = term(5) + tvvvo(b, c, c, j)

term(0) = -term(0) 
term(2) = -term(2) 
term(5) = -term(5) 


    v1_eom_cc3_32_trans_aibjckaick = 0.d+0
    do s = 0, 5
    v1_eom_cc3_32_trans_aibjckaick = v1_eom_cc3_32_trans_aibjckaick + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckaick
    function v1_eom_cc3_32_trans_aibjckajcj(i, b, j, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckajcj   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, j, i)



    v1_eom_cc3_32_trans_aibjckajcj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckajcj = v1_eom_cc3_32_trans_aibjckajcj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckajcj
    function v1_eom_cc3_32_trans_aibjckajck(a, i, b, j) 
    double precision :: v1_eom_cc3_32_trans_aibjckajck   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)
term(1) = term(1) + tvvvo(b, a, a, i)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckajck = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckajck = v1_eom_cc3_32_trans_aibjckajck + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckajck
    function v1_eom_cc3_32_trans_aibjckakcj(a, i, b, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckakcj   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)
term(1) = term(1) + tvvvo(b, a, a, i)

term(1) = -term(1) 


    v1_eom_cc3_32_trans_aibjckakcj = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckakcj = v1_eom_cc3_32_trans_aibjckakcj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckakcj
    function v1_eom_cc3_32_trans_aibjckakck(i, b, j, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckakck   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, k, i)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckakck = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckakck = v1_eom_cc3_32_trans_aibjckakck + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckakck
    function v1_eom_cc3_32_trans_aibjckaibi(i, j, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckaibi   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, i, k)
term(1) = term(1) + tvooo(c, k, i, j)

term(1) = -term(1) 


    v1_eom_cc3_32_trans_aibjckaibi = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckaibi = v1_eom_cc3_32_trans_aibjckaibi + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckaibi
    function v1_eom_cc3_32_trans_aibjckaibj(a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckaibj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, i, i)
term(1) = term(1) + tvooo(c, j, j, k)
term(2) = term(2) + tvooo(c, k, j, j)
term(3) = term(3) + tvvvo(a, a, c, k)
term(4) = term(4) + tvvvo(c, b, b, k)
term(5) = term(5) + tvvvo(b, b, c, k)

term(0) = -term(0) 
term(2) = -term(2) 
term(4) = -term(4) 


    v1_eom_cc3_32_trans_aibjckaibj = 0.d+0
    do s = 0, 5
    v1_eom_cc3_32_trans_aibjckaibj = v1_eom_cc3_32_trans_aibjckaibj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckaibj
    function v1_eom_cc3_32_trans_aibjckaibk(a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckaibk   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, i, i)
term(1) = term(1) + tvooo(c, j, k, k)
term(2) = term(2) + tvooo(c, k, k, j)
term(3) = term(3) + tvvvo(a, a, c, j)
term(4) = term(4) + tvvvo(c, b, b, j)
term(5) = term(5) + tvvvo(b, b, c, j)

term(2) = -term(2) 
term(3) = -term(3) 
term(5) = -term(5) 


    v1_eom_cc3_32_trans_aibjckaibk = 0.d+0
    do s = 0, 5
    v1_eom_cc3_32_trans_aibjckaibk = v1_eom_cc3_32_trans_aibjckaibk + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckaibk
    function v1_eom_cc3_32_trans_aibjckajbj(i, j, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckajbj   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, j, i)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckajbj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckajbj = v1_eom_cc3_32_trans_aibjckajbj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckajbj
    function v1_eom_cc3_32_trans_aibjckajbk(a, i, j, c) 
    double precision :: v1_eom_cc3_32_trans_aibjckajbk   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)
term(1) = term(1) + tvvvo(c, a, a, i)

term(1) = -term(1) 


    v1_eom_cc3_32_trans_aibjckajbk = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckajbk = v1_eom_cc3_32_trans_aibjckajbk + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckajbk
    function v1_eom_cc3_32_trans_aibjckakbj(a, i, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckakbj   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)
term(1) = term(1) + tvvvo(c, a, a, i)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckakbj = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckakbj = v1_eom_cc3_32_trans_aibjckakbj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckakbj
    function v1_eom_cc3_32_trans_aibjckakbk(i, j, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckakbk   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, k, i)



    v1_eom_cc3_32_trans_aibjckakbk = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckakbk = v1_eom_cc3_32_trans_aibjckakbk + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckakbk
    function v1_eom_cc3_32_trans_aibjckcicj(a, b, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckcicj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, k)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckcicj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckcicj = v1_eom_cc3_32_trans_aibjckcicj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckcicj
    function v1_eom_cc3_32_trans_aibjckcick(a, b, j, c) 
    double precision :: v1_eom_cc3_32_trans_aibjckcick   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, j)



    v1_eom_cc3_32_trans_aibjckcick = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckcick = v1_eom_cc3_32_trans_aibjckcick + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckcick
    function v1_eom_cc3_32_trans_aibjckcjci(a, b, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckcjci   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, k)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckcjci = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckcjci = v1_eom_cc3_32_trans_aibjckcjci + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckcjci
    function v1_eom_cc3_32_trans_aibjckckci(a, b, j, c) 
    double precision :: v1_eom_cc3_32_trans_aibjckckci   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, j)



    v1_eom_cc3_32_trans_aibjckckci = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckckci = v1_eom_cc3_32_trans_aibjckckci + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckckci
    function v1_eom_cc3_32_trans_aibjckbicj(a, i, b, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckbicj   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)
term(1) = term(1) + tvvvo(a, b, b, k)

term(1) = -term(1) 


    v1_eom_cc3_32_trans_aibjckbicj = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckbicj = v1_eom_cc3_32_trans_aibjckbicj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbicj
    function v1_eom_cc3_32_trans_aibjckbick(a, i, b, j) 
    double precision :: v1_eom_cc3_32_trans_aibjckbick   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)
term(1) = term(1) + tvvvo(a, b, b, j)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckbick = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckbick = v1_eom_cc3_32_trans_aibjckbick + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbick
    function v1_eom_cc3_32_trans_aibjckbjci(a, i, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckbjci   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)
term(1) = term(1) + tvvvo(a, c, c, k)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckbjci = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckbjci = v1_eom_cc3_32_trans_aibjckbjci + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbjci
    function v1_eom_cc3_32_trans_aibjckbkci(a, i, j, c) 
    double precision :: v1_eom_cc3_32_trans_aibjckbkci   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)
term(1) = term(1) + tvvvo(a, c, c, j)

term(1) = -term(1) 


    v1_eom_cc3_32_trans_aibjckbkci = 0.d+0
    do s = 0, 1
    v1_eom_cc3_32_trans_aibjckbkci = v1_eom_cc3_32_trans_aibjckbkci + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbkci
    function v1_eom_cc3_32_trans_aibjckbjck(a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckbjck   
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


    v1_eom_cc3_32_trans_aibjckbjck = 0.d+0
    do s = 0, 3
    v1_eom_cc3_32_trans_aibjckbjck = v1_eom_cc3_32_trans_aibjckbjck + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbjck
    function v1_eom_cc3_32_trans_aibjckbkcj(a, i, b, j, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckbkcj   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvooo(a, i, j, j)
term(2) = term(2) + tvvvo(b, b, a, i)
term(3) = term(3) + tvvvo(c, c, a, i)

term(2) = -term(2) 
term(3) = -term(3) 


    v1_eom_cc3_32_trans_aibjckbkcj = 0.d+0
    do s = 0, 3
    v1_eom_cc3_32_trans_aibjckbkcj = v1_eom_cc3_32_trans_aibjckbkcj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbkcj
    function v1_eom_cc3_32_trans_aibjckbibj(a, b, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckbibj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, k)



    v1_eom_cc3_32_trans_aibjckbibj = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckbibj = v1_eom_cc3_32_trans_aibjckbibj + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbibj
    function v1_eom_cc3_32_trans_aibjckbibk(a, b, j, c) 
    double precision :: v1_eom_cc3_32_trans_aibjckbibk   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, j)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckbibk = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckbibk = v1_eom_cc3_32_trans_aibjckbibk + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbibk
    function v1_eom_cc3_32_trans_aibjckbjbi(a, b, c, k) 
    double precision :: v1_eom_cc3_32_trans_aibjckbjbi   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, k)



    v1_eom_cc3_32_trans_aibjckbjbi = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckbjbi = v1_eom_cc3_32_trans_aibjckbjbi + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbjbi
    function v1_eom_cc3_32_trans_aibjckbkbi(a, b, j, c) 
    double precision :: v1_eom_cc3_32_trans_aibjckbkbi   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, j)

term(0) = -term(0) 


    v1_eom_cc3_32_trans_aibjckbkbi = 0.d+0
    do s = 0, 0
    v1_eom_cc3_32_trans_aibjckbkbi = v1_eom_cc3_32_trans_aibjckbkbi + term(s)
    end do

    end function v1_eom_cc3_32_trans_aibjckbkbi
    end module v1_eom_cc3_32_trans
    
