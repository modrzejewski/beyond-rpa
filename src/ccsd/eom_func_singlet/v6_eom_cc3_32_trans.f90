module v6_eom_cc3_32_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2013-02-27 23:45:57 UTC.
    !
    contains
    
    function v6_eom_cc3_32_trans_aiajckaiam(j, c, k, m) 
    double precision :: v6_eom_cc3_32_trans_aiajckaiam   
    integer, intent(in) :: j, c, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, m, j)
term(1) = term(1) + tvooo(c, j, m, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckaiam = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckaiam = v6_eom_cc3_32_trans_aiajckaiam + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckaiam
    function v6_eom_cc3_32_trans_aiajckalai(j, c, k, l) 
    double precision :: v6_eom_cc3_32_trans_aiajckalai   
    integer, intent(in) :: j, c, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, k)
term(1) = term(1) + tvooo(c, k, l, j)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aiajckalai = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckalai = v6_eom_cc3_32_trans_aiajckalai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckalai
    function v6_eom_cc3_32_trans_aiajckajam(i, c, k, m) 
    double precision :: v6_eom_cc3_32_trans_aiajckajam   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, m, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckajam = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckajam = v6_eom_cc3_32_trans_aiajckajam + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckajam
    function v6_eom_cc3_32_trans_aiajckalaj(i, c, k, l) 
    double precision :: v6_eom_cc3_32_trans_aiajckalaj   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, l, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckalaj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckalaj = v6_eom_cc3_32_trans_aiajckalaj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckalaj
    function v6_eom_cc3_32_trans_aiajckakam(i, j, c, m) 
    double precision :: v6_eom_cc3_32_trans_aiajckakam   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, i)



    v6_eom_cc3_32_trans_aiajckakam = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckakam = v6_eom_cc3_32_trans_aiajckakam + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckakam
    function v6_eom_cc3_32_trans_aiajckalak(i, j, c, l) 
    double precision :: v6_eom_cc3_32_trans_aiajckalak   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, i)



    v6_eom_cc3_32_trans_aiajckalak = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckalak = v6_eom_cc3_32_trans_aiajckalak + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckalak
    function v6_eom_cc3_32_trans_aiajckaicm(a, j, k, m) 
    double precision :: v6_eom_cc3_32_trans_aiajckaicm   
    integer, intent(in) :: a, j, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, k)
term(1) = term(1) + tvooo(a, k, m, j)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckaicm = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckaicm = v6_eom_cc3_32_trans_aiajckaicm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckaicm
    function v6_eom_cc3_32_trans_aiajckajcm(a, i, k, m) 
    double precision :: v6_eom_cc3_32_trans_aiajckajcm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckajcm = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckajcm = v6_eom_cc3_32_trans_aiajckajcm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckajcm
    function v6_eom_cc3_32_trans_aiajckalcj(a, i, k, l) 
    double precision :: v6_eom_cc3_32_trans_aiajckalcj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)
term(1) = term(1) + tvooo(a, k, l, i)



    v6_eom_cc3_32_trans_aiajckalcj = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckalcj = v6_eom_cc3_32_trans_aiajckalcj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckalcj
    function v6_eom_cc3_32_trans_aiajckakcm(a, i, j, m) 
    double precision :: v6_eom_cc3_32_trans_aiajckakcm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    v6_eom_cc3_32_trans_aiajckakcm = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckakcm = v6_eom_cc3_32_trans_aiajckakcm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckakcm
    function v6_eom_cc3_32_trans_aiajckalck(a, i, j, l) 
    double precision :: v6_eom_cc3_32_trans_aiajckalck   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, i)
term(1) = term(1) + tvooo(a, i, l, j)

term(0) = -term(0) 
term(1) = -term(1) 


    v6_eom_cc3_32_trans_aiajckalck = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckalck = v6_eom_cc3_32_trans_aiajckalck + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckalck
    function v6_eom_cc3_32_trans_aiajckaiej(a, c, k, e) 
    double precision :: v6_eom_cc3_32_trans_aiajckaiej   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, k)
term(1) = term(1) + tvvvo(c, e, a, k)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aiajckaiej = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckaiej = v6_eom_cc3_32_trans_aiajckaiej + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckaiej
    function v6_eom_cc3_32_trans_aiajckaiek(a, j, c, e) 
    double precision :: v6_eom_cc3_32_trans_aiajckaiek   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, j)
term(1) = term(1) + tvvvo(a, e, c, j)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aiajckaiek = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckaiek = v6_eom_cc3_32_trans_aiajckaiek + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckaiek
    function v6_eom_cc3_32_trans_aiajckajei(a, c, k, e) 
    double precision :: v6_eom_cc3_32_trans_aiajckajei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, k)



    v6_eom_cc3_32_trans_aiajckajei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckajei = v6_eom_cc3_32_trans_aiajckajei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckajei
    function v6_eom_cc3_32_trans_aiajckakei(a, j, c, e) 
    double precision :: v6_eom_cc3_32_trans_aiajckakei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, j)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckakei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckakei = v6_eom_cc3_32_trans_aiajckakei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckakei
    function v6_eom_cc3_32_trans_aiajckajek(a, i, c, e) 
    double precision :: v6_eom_cc3_32_trans_aiajckajek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    v6_eom_cc3_32_trans_aiajckajek = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckajek = v6_eom_cc3_32_trans_aiajckajek + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckajek
    function v6_eom_cc3_32_trans_aiajckakej(a, i, c, e) 
    double precision :: v6_eom_cc3_32_trans_aiajckakej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckakej = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckakej = v6_eom_cc3_32_trans_aiajckakej + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckakej
    function v6_eom_cc3_32_trans_aibjbkaibm(b, j, k, m) 
    double precision :: v6_eom_cc3_32_trans_aibjbkaibm   
    integer, intent(in) :: b, j, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, m, j)
term(1) = term(1) + tvooo(b, j, m, k)

term(0) = -term(0) 
term(1) = -term(1) 


    v6_eom_cc3_32_trans_aibjbkaibm = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkaibm = v6_eom_cc3_32_trans_aibjbkaibm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkaibm
    function v6_eom_cc3_32_trans_aibjbkalbi(b, j, k, l) 
    double precision :: v6_eom_cc3_32_trans_aibjbkalbi   
    integer, intent(in) :: b, j, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, j)



    v6_eom_cc3_32_trans_aibjbkalbi = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkalbi = v6_eom_cc3_32_trans_aibjbkalbi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkalbi
    function v6_eom_cc3_32_trans_aibjbkajbm(i, b, k, m) 
    double precision :: v6_eom_cc3_32_trans_aibjbkajbm   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, k)
term(1) = term(1) + tvooo(b, k, m, i)



    v6_eom_cc3_32_trans_aibjbkajbm = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkajbm = v6_eom_cc3_32_trans_aibjbkajbm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkajbm
    function v6_eom_cc3_32_trans_aibjbkalbj(i, b, k, l) 
    double precision :: v6_eom_cc3_32_trans_aibjbkalbj   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, l, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkalbj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkalbj = v6_eom_cc3_32_trans_aibjbkalbj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkalbj
    function v6_eom_cc3_32_trans_aibjbkalbk(i, b, j, l) 
    double precision :: v6_eom_cc3_32_trans_aibjbkalbk   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, j)
term(1) = term(1) + tvooo(b, j, l, i)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aibjbkalbk = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkalbk = v6_eom_cc3_32_trans_aibjbkalbk + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkalbk
    function v6_eom_cc3_32_trans_aibjbkaiej(b, k, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbkaiej   
    integer, intent(in) :: b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, b, k)



    v6_eom_cc3_32_trans_aibjbkaiej = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkaiej = v6_eom_cc3_32_trans_aibjbkaiej + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkaiej
    function v6_eom_cc3_32_trans_aibjbkaiek(b, j, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbkaiek   
    integer, intent(in) :: b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, b, j)



    v6_eom_cc3_32_trans_aibjbkaiek = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkaiek = v6_eom_cc3_32_trans_aibjbkaiek + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkaiek
    function v6_eom_cc3_32_trans_aibjbkajei(b, k, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbkajei   
    integer, intent(in) :: b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, b, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkajei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkajei = v6_eom_cc3_32_trans_aibjbkajei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkajei
    function v6_eom_cc3_32_trans_aibjbkajek(i, b, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbkajek   
    integer, intent(in) :: i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, b, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkajek = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkajek = v6_eom_cc3_32_trans_aibjbkajek + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkajek
    function v6_eom_cc3_32_trans_aiajckdiaj(a, c, k, d) 
    double precision :: v6_eom_cc3_32_trans_aiajckdiaj   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, k)



    v6_eom_cc3_32_trans_aiajckdiaj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckdiaj = v6_eom_cc3_32_trans_aiajckdiaj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckdiaj
    function v6_eom_cc3_32_trans_aiajckdiak(a, j, c, d) 
    double precision :: v6_eom_cc3_32_trans_aiajckdiak   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, j)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckdiak = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckdiak = v6_eom_cc3_32_trans_aiajckdiak + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckdiak
    function v6_eom_cc3_32_trans_aiajckdjai(a, c, k, d) 
    double precision :: v6_eom_cc3_32_trans_aiajckdjai   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, k)
term(1) = term(1) + tvvvo(a, d, c, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckdjai = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckdjai = v6_eom_cc3_32_trans_aiajckdjai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckdjai
    function v6_eom_cc3_32_trans_aiajckdkai(a, j, c, d) 
    double precision :: v6_eom_cc3_32_trans_aiajckdkai   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, j)
term(1) = term(1) + tvvvo(a, d, c, j)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aiajckdkai = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckdkai = v6_eom_cc3_32_trans_aiajckdkai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckdkai
    function v6_eom_cc3_32_trans_aiajckdjak(a, i, c, d) 
    double precision :: v6_eom_cc3_32_trans_aiajckdjak   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckdjak = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckdjak = v6_eom_cc3_32_trans_aiajckdjak + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckdjak
    function v6_eom_cc3_32_trans_aiajckdkaj(a, i, c, d) 
    double precision :: v6_eom_cc3_32_trans_aiajckdkaj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)



    v6_eom_cc3_32_trans_aiajckdkaj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckdkaj = v6_eom_cc3_32_trans_aiajckdkaj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckdkaj
    function v6_eom_cc3_32_trans_aibjbkdiaj(b, k, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbkdiaj   
    integer, intent(in) :: b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, b, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkdiaj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkdiaj = v6_eom_cc3_32_trans_aibjbkdiaj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkdiaj
    function v6_eom_cc3_32_trans_aibjbkdjai(b, k, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbkdjai   
    integer, intent(in) :: b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, b, k)



    v6_eom_cc3_32_trans_aibjbkdjai = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkdjai = v6_eom_cc3_32_trans_aibjbkdjai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkdjai
    function v6_eom_cc3_32_trans_aibjbkdkai(b, j, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbkdkai   
    integer, intent(in) :: b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, b, j)



    v6_eom_cc3_32_trans_aibjbkdkai = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkdkai = v6_eom_cc3_32_trans_aibjbkdkai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkdkai
    function v6_eom_cc3_32_trans_aibjbkdkaj(i, b, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbkdkaj   
    integer, intent(in) :: i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, b, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkdkaj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkdkaj = v6_eom_cc3_32_trans_aibjbkdkaj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkdkaj
    function v6_eom_cc3_32_trans_aiajckcjei(a, k, e) 
    double precision :: v6_eom_cc3_32_trans_aiajckcjei   
    integer, intent(in) :: a, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckcjei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckcjei = v6_eom_cc3_32_trans_aiajckcjei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckcjei
    function v6_eom_cc3_32_trans_aiajckckei(a, j, e) 
    double precision :: v6_eom_cc3_32_trans_aiajckckei   
    integer, intent(in) :: a, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, j)



    v6_eom_cc3_32_trans_aiajckckei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckckei = v6_eom_cc3_32_trans_aiajckckei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckckei
    function v6_eom_cc3_32_trans_aiajckcjek(a, i, e) 
    double precision :: v6_eom_cc3_32_trans_aiajckcjek   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckcjek = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckcjek = v6_eom_cc3_32_trans_aiajckcjek + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckcjek
    function v6_eom_cc3_32_trans_aiajckckej(a, i, e) 
    double precision :: v6_eom_cc3_32_trans_aiajckckej   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)



    v6_eom_cc3_32_trans_aiajckckej = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckckej = v6_eom_cc3_32_trans_aiajckckej + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckckej
    function v6_eom_cc3_32_trans_aiajckdicj(a, k, d) 
    double precision :: v6_eom_cc3_32_trans_aiajckdicj   
    integer, intent(in) :: a, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckdicj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckdicj = v6_eom_cc3_32_trans_aiajckdicj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckdicj
    function v6_eom_cc3_32_trans_aiajckdick(a, j, d) 
    double precision :: v6_eom_cc3_32_trans_aiajckdick   
    integer, intent(in) :: a, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, j)



    v6_eom_cc3_32_trans_aiajckdick = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckdick = v6_eom_cc3_32_trans_aiajckdick + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckdick
    function v6_eom_cc3_32_trans_aiajckdjck(a, i, d) 
    double precision :: v6_eom_cc3_32_trans_aiajckdjck   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)



    v6_eom_cc3_32_trans_aiajckdjck = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckdjck = v6_eom_cc3_32_trans_aiajckdjck + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckdjck
    function v6_eom_cc3_32_trans_aiajckdkcj(a, i, d) 
    double precision :: v6_eom_cc3_32_trans_aiajckdkcj   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckdkcj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckdkcj = v6_eom_cc3_32_trans_aiajckdkcj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckdkcj
    function v6_eom_cc3_32_trans_aibjbkbibm(a, j, k, m) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbibm   
    integer, intent(in) :: a, j, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, k)



    v6_eom_cc3_32_trans_aibjbkbibm = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkbibm = v6_eom_cc3_32_trans_aibjbkbibm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbibm
    function v6_eom_cc3_32_trans_aibjbkblbi(a, j, k, l) 
    double precision :: v6_eom_cc3_32_trans_aibjbkblbi   
    integer, intent(in) :: a, j, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, k)



    v6_eom_cc3_32_trans_aibjbkblbi = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkblbi = v6_eom_cc3_32_trans_aibjbkblbi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkblbi
    function v6_eom_cc3_32_trans_aibjbkbjbm(a, i, k, m) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbjbm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkbjbm = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkbjbm = v6_eom_cc3_32_trans_aibjbkbjbm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbjbm
    function v6_eom_cc3_32_trans_aibjbkblbj(a, i, k, l) 
    double precision :: v6_eom_cc3_32_trans_aibjbkblbj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkblbj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkblbj = v6_eom_cc3_32_trans_aibjbkblbj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkblbj
    function v6_eom_cc3_32_trans_aibjbkbkbm(a, i, j, m) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbkbm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, i)
term(1) = term(1) + tvooo(a, i, m, j)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aibjbkbkbm = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkbkbm = v6_eom_cc3_32_trans_aibjbkbkbm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbkbm
    function v6_eom_cc3_32_trans_aibjbkblbk(a, i, j, l) 
    double precision :: v6_eom_cc3_32_trans_aibjbkblbk   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, i)
term(1) = term(1) + tvooo(a, i, l, j)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aibjbkblbk = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkblbk = v6_eom_cc3_32_trans_aibjbkblbk + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkblbk
    function v6_eom_cc3_32_trans_aibjbkbiej(a, b, k, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbiej   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkbiej = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkbiej = v6_eom_cc3_32_trans_aibjbkbiej + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbiej
    function v6_eom_cc3_32_trans_aibjbkbiek(a, b, j, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbiek   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, j)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkbiek = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkbiek = v6_eom_cc3_32_trans_aibjbkbiek + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbiek
    function v6_eom_cc3_32_trans_aibjbkbjei(a, b, k, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbjei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, k)



    v6_eom_cc3_32_trans_aibjbkbjei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkbjei = v6_eom_cc3_32_trans_aibjbkbjei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbjei
    function v6_eom_cc3_32_trans_aibjbkbkei(a, b, j, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbkei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, j)
term(1) = term(1) + tvvvo(a, e, b, j)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkbkei = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkbkei = v6_eom_cc3_32_trans_aibjbkbkei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbkei
    function v6_eom_cc3_32_trans_aibjbkbjek(a, i, b, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbjek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)



    v6_eom_cc3_32_trans_aibjbkbjek = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkbjek = v6_eom_cc3_32_trans_aibjbkbjek + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbjek
    function v6_eom_cc3_32_trans_aibjbkbkej(a, i, b, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbkej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)
term(1) = term(1) + tvvvo(a, e, b, i)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aibjbkbkej = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkbkej = v6_eom_cc3_32_trans_aibjbkbkej + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbkej
    function v6_eom_cc3_32_trans_aibjbkdibj(a, b, k, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbkdibj   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, k)



    v6_eom_cc3_32_trans_aibjbkdibj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkdibj = v6_eom_cc3_32_trans_aibjbkdibj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkdibj
    function v6_eom_cc3_32_trans_aibjbkdibk(a, b, j, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbkdibk   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, j)
term(1) = term(1) + tvvvo(a, d, b, j)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkdibk = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkdibk = v6_eom_cc3_32_trans_aibjbkdibk + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkdibk
    function v6_eom_cc3_32_trans_aibjbkdjbi(a, b, k, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbkdjbi   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkdjbi = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkdjbi = v6_eom_cc3_32_trans_aibjbkdjbi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkdjbi
    function v6_eom_cc3_32_trans_aibjbkdkbi(a, b, j, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbkdkbi   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, j)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkdkbi = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkdkbi = v6_eom_cc3_32_trans_aibjbkdkbi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkdkbi
    function v6_eom_cc3_32_trans_aibjbkdjbk(a, i, b, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbkdjbk   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)
term(1) = term(1) + tvvvo(a, d, b, i)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aibjbkdjbk = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkdjbk = v6_eom_cc3_32_trans_aibjbkdjbk + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkdjbk
    function v6_eom_cc3_32_trans_aibjbkdkbj(a, i, b, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbkdkbj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    v6_eom_cc3_32_trans_aibjbkdkbj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkdkbj = v6_eom_cc3_32_trans_aibjbkdkbj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkdkbj
    function v6_eom_cc3_32_trans_aiajciaiam(i, j, c, m) 
    double precision :: v6_eom_cc3_32_trans_aiajciaiam   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, i)
term(1) = term(1) + tvooo(c, i, m, j)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    v6_eom_cc3_32_trans_aiajciaiam = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajciaiam = v6_eom_cc3_32_trans_aiajciaiam + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciaiam
    function v6_eom_cc3_32_trans_aiajckaiai(i, j, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckaiai   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, i, k)
term(1) = term(1) + tvooo(c, k, i, j)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aiajckaiai = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckaiai = v6_eom_cc3_32_trans_aiajckaiai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckaiai
    function v6_eom_cc3_32_trans_aiajckaiaj(a, i, j, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckaiaj   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, i, i)
term(1) = term(1) + tvooo(c, k, j, j)
term(2) = term(2) + tvooo(c, j, j, k)
term(3) = term(3) + tvvvo(a, a, c, k)
term(4) = term(4) + tvvvo(c, a, a, k)

term(0) = -term(0) 
term(1) = -term(1) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 


    v6_eom_cc3_32_trans_aiajckaiaj = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aiajckaiaj = v6_eom_cc3_32_trans_aiajckaiaj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckaiaj
    function v6_eom_cc3_32_trans_aiajckaiak(a, i, j, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckaiak   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, i, i)
term(1) = term(1) + tvooo(c, k, k, j)
term(2) = term(2) + tvooo(c, j, k, k)
term(3) = term(3) + tvvvo(a, a, c, j)
term(4) = term(4) + tvvvo(c, a, a, j)

term(1) = -term(1) 
term(3) = term(3) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aiajckaiak = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aiajckaiak = v6_eom_cc3_32_trans_aiajckaiak + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckaiak
    function v6_eom_cc3_32_trans_aiajcialai(i, j, c, l) 
    double precision :: v6_eom_cc3_32_trans_aiajcialai   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, i)
term(1) = term(1) + tvooo(c, i, l, j)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    v6_eom_cc3_32_trans_aiajcialai = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajcialai = v6_eom_cc3_32_trans_aiajcialai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajcialai
    function v6_eom_cc3_32_trans_aiajciajam(i, c, m) 
    double precision :: v6_eom_cc3_32_trans_aiajciajam   
    integer, intent(in) :: i, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajciajam = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajciajam = v6_eom_cc3_32_trans_aiajciajam + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciajam
    function v6_eom_cc3_32_trans_aiajcialaj(i, c, l) 
    double precision :: v6_eom_cc3_32_trans_aiajcialaj   
    integer, intent(in) :: i, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajcialaj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajcialaj = v6_eom_cc3_32_trans_aiajcialaj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajcialaj
    function v6_eom_cc3_32_trans_aiajckakai(a, i, j, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckakai   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, k, k)
term(1) = term(1) + tvooo(c, k, k, j)
term(2) = term(2) + tvooo(c, j, i, i)
term(3) = term(3) + tvvvo(c, a, a, j)
term(4) = term(4) + tvvvo(a, a, c, j)

term(1) = -term(1) 
term(4) = term(4) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aiajckakai = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aiajckakai = v6_eom_cc3_32_trans_aiajckakai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckakai
    function v6_eom_cc3_32_trans_aiajckajaj(i, j, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckajaj   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, j, i)

term(0) = term(0) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aiajckajaj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckajaj = v6_eom_cc3_32_trans_aiajckajaj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckajaj
    function v6_eom_cc3_32_trans_aiajckajak(i, j, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckajak   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)
term(1) = term(1) + tvooo(c, k, k, i)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aiajckajak = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckajak = v6_eom_cc3_32_trans_aiajckajak + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckajak
    function v6_eom_cc3_32_trans_aiajckakaj(i, j, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckakaj   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, k, k, i)
term(1) = term(1) + tvooo(c, j, j, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckakaj = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckakaj = v6_eom_cc3_32_trans_aiajckakaj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckakaj
    function v6_eom_cc3_32_trans_aiajckakak(i, j, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckakak   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, k, i)

term(0) = term(0) * 2.0d+0 


    v6_eom_cc3_32_trans_aiajckakak = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckakak = v6_eom_cc3_32_trans_aiajckakak + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckakak
    function v6_eom_cc3_32_trans_aibjbkaiak(a, b, j) 
    double precision :: v6_eom_cc3_32_trans_aibjbkaiak   
    integer, intent(in) :: a, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, b, j)



    v6_eom_cc3_32_trans_aibjbkaiak = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkaiak = v6_eom_cc3_32_trans_aibjbkaiak + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkaiak
    function v6_eom_cc3_32_trans_aibjbkakai(a, b, j) 
    double precision :: v6_eom_cc3_32_trans_aibjbkakai   
    integer, intent(in) :: a, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, b, j)



    v6_eom_cc3_32_trans_aibjbkakai = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkakai = v6_eom_cc3_32_trans_aibjbkakai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkakai
    function v6_eom_cc3_32_trans_aibjbkajak(a, i, b) 
    double precision :: v6_eom_cc3_32_trans_aibjbkajak   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, b, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkajak = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkajak = v6_eom_cc3_32_trans_aibjbkajak + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkajak
    function v6_eom_cc3_32_trans_aiajciaicm(a, i, j, m) 
    double precision :: v6_eom_cc3_32_trans_aiajciaicm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, i)
term(1) = term(1) + tvooo(a, i, m, j)

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 


    v6_eom_cc3_32_trans_aiajciaicm = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajciaicm = v6_eom_cc3_32_trans_aiajciaicm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciaicm
    function v6_eom_cc3_32_trans_aiajckaici(a, i, j, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckaici   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, k)
term(1) = term(1) + tvooo(a, k, i, j)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckaici = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckaici = v6_eom_cc3_32_trans_aiajckaici + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckaici
    function v6_eom_cc3_32_trans_aiajckaicj(a, i, j, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckaicj   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)
term(1) = term(1) + tvooo(a, k, i, i)
term(2) = term(2) + tvooo(a, j, j, k)
term(3) = term(3) + tvooo(a, k, j, j)
term(4) = term(4) + tvvvo(a, a, a, k)
term(5) = term(5) + tvvvo(a, c, c, k)
term(6) = term(6) + tvvvo(c, c, a, k)

term(2) = -term(2) 
term(4) = -term(4) 
term(6) = -term(6) 


    v6_eom_cc3_32_trans_aiajckaicj = 0.d+0
    do s = 0, 6
    v6_eom_cc3_32_trans_aiajckaicj = v6_eom_cc3_32_trans_aiajckaicj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckaicj
    function v6_eom_cc3_32_trans_aiajckaick(a, i, j, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckaick   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, i)
term(1) = term(1) + tvooo(a, i, i, j)
term(2) = term(2) + tvooo(a, j, k, k)
term(3) = term(3) + tvooo(a, k, k, j)
term(4) = term(4) + tvvvo(a, a, a, j)
term(5) = term(5) + tvvvo(c, c, a, j)
term(6) = term(6) + tvvvo(a, c, c, j)

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = -term(2) 
term(6) = -term(6) 


    v6_eom_cc3_32_trans_aiajckaick = 0.d+0
    do s = 0, 6
    v6_eom_cc3_32_trans_aiajckaick = v6_eom_cc3_32_trans_aiajckaick + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckaick
    function v6_eom_cc3_32_trans_aiajcialci(a, i, j, l) 
    double precision :: v6_eom_cc3_32_trans_aiajcialci   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, i)
term(1) = term(1) + tvooo(a, i, l, j)

term(0) = -term(0) 
term(1) = -term(1) 


    v6_eom_cc3_32_trans_aiajcialci = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajcialci = v6_eom_cc3_32_trans_aiajcialci + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajcialci
    function v6_eom_cc3_32_trans_aiajciajcm(a, i, m) 
    double precision :: v6_eom_cc3_32_trans_aiajciajcm   
    integer, intent(in) :: a, i, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajciajcm = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajciajcm = v6_eom_cc3_32_trans_aiajciajcm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciajcm
    function v6_eom_cc3_32_trans_aiajcialcj(a, i, l) 
    double precision :: v6_eom_cc3_32_trans_aiajcialcj   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)

term(0) = term(0) * 2.0d+0 


    v6_eom_cc3_32_trans_aiajcialcj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajcialcj = v6_eom_cc3_32_trans_aiajcialcj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajcialcj
    function v6_eom_cc3_32_trans_aiajckajci(a, i, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckajci   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)
term(1) = term(1) + tvvvo(a, c, c, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckajci = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckajci = v6_eom_cc3_32_trans_aiajckajci + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckajci
    function v6_eom_cc3_32_trans_aiajckakci(a, i, j, c) 
    double precision :: v6_eom_cc3_32_trans_aiajckakci   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)
term(1) = term(1) + tvvvo(a, c, c, j)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aiajckakci = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajckakci = v6_eom_cc3_32_trans_aiajckakci + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckakci
    function v6_eom_cc3_32_trans_aiajckajcj(a, i, j, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckajcj   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, j, i)



    v6_eom_cc3_32_trans_aiajckajcj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckajcj = v6_eom_cc3_32_trans_aiajckajcj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckajcj
    function v6_eom_cc3_32_trans_aiajckajck(a, i, j, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckajck   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)
term(1) = term(1) + tvooo(a, i, j, j)
term(2) = term(2) + tvooo(a, i, k, k)
term(3) = term(3) + tvvvo(a, a, a, i)
term(4) = term(4) + tvvvo(c, c, a, i)

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = -term(2) 


    v6_eom_cc3_32_trans_aiajckajck = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aiajckajck = v6_eom_cc3_32_trans_aiajckajck + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckajck
    function v6_eom_cc3_32_trans_aiajckakcj(a, i, j, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckakcj   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvooo(a, k, k, i)
term(2) = term(2) + tvooo(a, i, j, j)
term(3) = term(3) + tvvvo(a, a, a, i)
term(4) = term(4) + tvvvo(c, c, a, i)

term(3) = -term(3) 
term(4) = -term(4) 


    v6_eom_cc3_32_trans_aiajckakcj = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aiajckakcj = v6_eom_cc3_32_trans_aiajckakcj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckakcj
    function v6_eom_cc3_32_trans_aiajckakck(a, i, j, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckakck   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, k, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckakck = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckakck = v6_eom_cc3_32_trans_aiajckakck + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckakck
    function v6_eom_cc3_32_trans_aiajciaiei(a, j, c, e) 
    double precision :: v6_eom_cc3_32_trans_aiajciaiei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, j)
term(1) = term(1) + tvvvo(a, e, c, j)

term(1) = term(1) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aiajciaiei = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajciaiei = v6_eom_cc3_32_trans_aiajciaiei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciaiei
    function v6_eom_cc3_32_trans_aiajciaiej(a, i, c, e) 
    double precision :: v6_eom_cc3_32_trans_aiajciaiej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)
term(1) = term(1) + tvvvo(a, e, c, i)

term(0) = term(0) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aiajciaiej = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajciaiej = v6_eom_cc3_32_trans_aiajciaiej + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciaiej
    function v6_eom_cc3_32_trans_aiajciajei(a, i, c, e) 
    double precision :: v6_eom_cc3_32_trans_aiajciajei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)
term(1) = term(1) + tvvvo(a, e, c, i)



    v6_eom_cc3_32_trans_aiajciajei = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajciajei = v6_eom_cc3_32_trans_aiajciajei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciajei
    function v6_eom_cc3_32_trans_aibjbiaibm(i, b, j, m) 
    double precision :: v6_eom_cc3_32_trans_aibjbiaibm   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, j)
term(1) = term(1) + tvooo(b, j, m, i)

term(0) = -term(0) 
term(1) = -term(1) 


    v6_eom_cc3_32_trans_aibjbiaibm = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbiaibm = v6_eom_cc3_32_trans_aibjbiaibm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbiaibm
    function v6_eom_cc3_32_trans_aibjbkaibi(i, b, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkaibi   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, i, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkaibi = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkaibi = v6_eom_cc3_32_trans_aibjbkaibi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkaibi
    function v6_eom_cc3_32_trans_aibjbkaibj(a, i, b, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkaibj   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, i, i)
term(1) = term(1) + tvooo(b, k, j, j)
term(2) = term(2) + tvooo(b, j, j, k)
term(3) = term(3) + tvvvo(b, b, b, k)
term(4) = term(4) + tvvvo(a, a, b, k)

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = -term(2) 


    v6_eom_cc3_32_trans_aibjbkaibj = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aibjbkaibj = v6_eom_cc3_32_trans_aibjbkaibj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkaibj
    function v6_eom_cc3_32_trans_aibjbkaibk(a, i, b, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkaibk   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, i, j)
term(1) = term(1) + tvooo(b, j, i, i)
term(2) = term(2) + tvooo(b, k, k, j)
term(3) = term(3) + tvooo(b, j, k, k)
term(4) = term(4) + tvvvo(b, b, b, j)
term(5) = term(5) + tvvvo(b, a, a, j)
term(6) = term(6) + tvvvo(a, a, b, j)

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 
term(5) = -term(5) 


    v6_eom_cc3_32_trans_aibjbkaibk = 0.d+0
    do s = 0, 6
    v6_eom_cc3_32_trans_aibjbkaibk = v6_eom_cc3_32_trans_aibjbkaibk + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkaibk
    function v6_eom_cc3_32_trans_aibjbialbi(i, b, j, l) 
    double precision :: v6_eom_cc3_32_trans_aibjbialbi   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, j)
term(1) = term(1) + tvooo(b, j, l, i)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    v6_eom_cc3_32_trans_aibjbialbi = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbialbi = v6_eom_cc3_32_trans_aibjbialbi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbialbi
    function v6_eom_cc3_32_trans_aibjbiajbm(i, b, m) 
    double precision :: v6_eom_cc3_32_trans_aibjbiajbm   
    integer, intent(in) :: i, b, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, i)

term(0) = term(0) * 2.0d+0 


    v6_eom_cc3_32_trans_aibjbiajbm = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbiajbm = v6_eom_cc3_32_trans_aibjbiajbm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbiajbm
    function v6_eom_cc3_32_trans_aibjbialbj(i, b, l) 
    double precision :: v6_eom_cc3_32_trans_aibjbialbj   
    integer, intent(in) :: i, b, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbialbj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbialbj = v6_eom_cc3_32_trans_aibjbialbj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbialbj
    function v6_eom_cc3_32_trans_aibjbkajbi(a, i, b, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkajbi   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, j, j)
term(1) = term(1) + tvooo(b, i, i, k)
term(2) = term(2) + tvooo(b, k, i, i)
term(3) = term(3) + tvvvo(b, b, b, k)
term(4) = term(4) + tvvvo(a, a, b, k)

term(3) = -term(3) 
term(4) = -term(4) 


    v6_eom_cc3_32_trans_aibjbkajbi = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aibjbkajbi = v6_eom_cc3_32_trans_aibjbkajbi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkajbi
    function v6_eom_cc3_32_trans_aibjbkakbi(a, b, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkakbi   
    integer, intent(in) :: a, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, j)
term(1) = term(1) + tvvvo(b, a, a, j)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aibjbkakbi = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkakbi = v6_eom_cc3_32_trans_aibjbkakbi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkakbi
    function v6_eom_cc3_32_trans_aibjbkajbj(i, b, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkajbj   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, k)



    v6_eom_cc3_32_trans_aibjbkajbj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkajbj = v6_eom_cc3_32_trans_aibjbkajbj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkajbj
    function v6_eom_cc3_32_trans_aibjbkajbk(a, i, b, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkajbk   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, j)
term(1) = term(1) + tvooo(b, j, j, i)
term(2) = term(2) + tvooo(b, i, k, k)
term(3) = term(3) + tvooo(b, k, k, i)
term(4) = term(4) + tvvvo(b, b, b, i)
term(5) = term(5) + tvvvo(b, a, a, i)
term(6) = term(6) + tvvvo(a, a, b, i)

term(1) = -term(1) 
term(4) = -term(4) 
term(6) = -term(6) 


    v6_eom_cc3_32_trans_aibjbkajbk = 0.d+0
    do s = 0, 6
    v6_eom_cc3_32_trans_aibjbkajbk = v6_eom_cc3_32_trans_aibjbkajbk + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkajbk
    function v6_eom_cc3_32_trans_aibjbkakbj(a, i, b, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkakbj   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)
term(1) = term(1) + tvvvo(b, a, a, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkakbj = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkakbj = v6_eom_cc3_32_trans_aibjbkakbj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkakbj
    function v6_eom_cc3_32_trans_aibjbkakbk(i, b, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkakbk   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, k, j)
term(1) = term(1) + tvooo(b, j, k, i)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aibjbkakbk = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkakbk = v6_eom_cc3_32_trans_aibjbkakbk + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkakbk
    function v6_eom_cc3_32_trans_aibjbiaiei(b, j, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbiaiei   
    integer, intent(in) :: b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, b, j)



    v6_eom_cc3_32_trans_aibjbiaiei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbiaiei = v6_eom_cc3_32_trans_aibjbiaiei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbiaiei
    function v6_eom_cc3_32_trans_aibjbiaiej(i, b, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbiaiej   
    integer, intent(in) :: i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, b, i)



    v6_eom_cc3_32_trans_aibjbiaiej = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbiaiej = v6_eom_cc3_32_trans_aibjbiaiej + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbiaiej
    function v6_eom_cc3_32_trans_aibjbiajei(i, b, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbiajei   
    integer, intent(in) :: i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, b, i)

term(0) = term(0) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aibjbiajei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbiajei = v6_eom_cc3_32_trans_aibjbiajei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbiajei
    function v6_eom_cc3_32_trans_aiajcidiai(a, j, c, d) 
    double precision :: v6_eom_cc3_32_trans_aiajcidiai   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, j)
term(1) = term(1) + tvvvo(a, d, c, j)

term(1) = term(1) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aiajcidiai = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajcidiai = v6_eom_cc3_32_trans_aiajcidiai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajcidiai
    function v6_eom_cc3_32_trans_aiajcidiaj(a, i, c, d) 
    double precision :: v6_eom_cc3_32_trans_aiajcidiaj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)
term(1) = term(1) + tvvvo(a, d, c, i)



    v6_eom_cc3_32_trans_aiajcidiaj = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajcidiaj = v6_eom_cc3_32_trans_aiajcidiaj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajcidiaj
    function v6_eom_cc3_32_trans_aiajcidjai(a, i, c, d) 
    double precision :: v6_eom_cc3_32_trans_aiajcidjai   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)
term(1) = term(1) + tvvvo(a, d, c, i)

term(0) = term(0) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aiajcidjai = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aiajcidjai = v6_eom_cc3_32_trans_aiajcidjai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajcidjai
    function v6_eom_cc3_32_trans_aibjbidiai(b, j, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbidiai   
    integer, intent(in) :: b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, b, j)



    v6_eom_cc3_32_trans_aibjbidiai = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbidiai = v6_eom_cc3_32_trans_aibjbidiai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbidiai
    function v6_eom_cc3_32_trans_aibjbidiaj(i, b, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbidiaj   
    integer, intent(in) :: i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, b, i)

term(0) = term(0) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aibjbidiaj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbidiaj = v6_eom_cc3_32_trans_aibjbidiaj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbidiaj
    function v6_eom_cc3_32_trans_aibjbidjai(i, b, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbidjai   
    integer, intent(in) :: i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, b, i)



    v6_eom_cc3_32_trans_aibjbidjai = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbidjai = v6_eom_cc3_32_trans_aibjbidjai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbidjai
    function v6_eom_cc3_32_trans_aiajckcicj(a, c, k) 
    double precision :: v6_eom_cc3_32_trans_aiajckcicj   
    integer, intent(in) :: a, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, a, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajckcicj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckcicj = v6_eom_cc3_32_trans_aiajckcicj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckcicj
    function v6_eom_cc3_32_trans_aiajckcick(a, j, c) 
    double precision :: v6_eom_cc3_32_trans_aiajckcick   
    integer, intent(in) :: a, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, a, j)



    v6_eom_cc3_32_trans_aiajckcick = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckcick = v6_eom_cc3_32_trans_aiajckcick + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckcick
    function v6_eom_cc3_32_trans_aiajckckci(a, j, c) 
    double precision :: v6_eom_cc3_32_trans_aiajckckci   
    integer, intent(in) :: a, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, a, j)



    v6_eom_cc3_32_trans_aiajckckci = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajckckci = v6_eom_cc3_32_trans_aiajckckci + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajckckci
    function v6_eom_cc3_32_trans_aiajciciei(a, j, e) 
    double precision :: v6_eom_cc3_32_trans_aiajciciei   
    integer, intent(in) :: a, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, j)



    v6_eom_cc3_32_trans_aiajciciei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajciciei = v6_eom_cc3_32_trans_aiajciciei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciciei
    function v6_eom_cc3_32_trans_aiajciciej(a, i, e) 
    double precision :: v6_eom_cc3_32_trans_aiajciciej   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)



    v6_eom_cc3_32_trans_aiajciciej = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajciciej = v6_eom_cc3_32_trans_aiajciciej + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciciej
    function v6_eom_cc3_32_trans_aiajcicjei(a, i, e) 
    double precision :: v6_eom_cc3_32_trans_aiajcicjei   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)

term(0) = term(0) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aiajcicjei = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajcicjei = v6_eom_cc3_32_trans_aiajcicjei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajcicjei
    function v6_eom_cc3_32_trans_aiajcidici(a, j, d) 
    double precision :: v6_eom_cc3_32_trans_aiajcidici   
    integer, intent(in) :: a, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, j)



    v6_eom_cc3_32_trans_aiajcidici = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajcidici = v6_eom_cc3_32_trans_aiajcidici + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajcidici
    function v6_eom_cc3_32_trans_aiajcidicj(a, i, d) 
    double precision :: v6_eom_cc3_32_trans_aiajcidicj   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)

term(0) = term(0) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aiajcidicj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajcidicj = v6_eom_cc3_32_trans_aiajcidicj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajcidicj
    function v6_eom_cc3_32_trans_aiajcidjci(a, i, d) 
    double precision :: v6_eom_cc3_32_trans_aiajcidjci   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)



    v6_eom_cc3_32_trans_aiajcidjci = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajcidjci = v6_eom_cc3_32_trans_aiajcidjci + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajcidjci
    function v6_eom_cc3_32_trans_aibjbibibm(a, i, j, m) 
    double precision :: v6_eom_cc3_32_trans_aibjbibibm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, i)
term(1) = term(1) + tvooo(a, i, m, j)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    v6_eom_cc3_32_trans_aibjbibibm = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbibibm = v6_eom_cc3_32_trans_aibjbibibm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbibibm
    function v6_eom_cc3_32_trans_aibjbkbibi(a, i, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbibi   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, k)

term(0) = term(0) * 2.0d+0 


    v6_eom_cc3_32_trans_aibjbkbibi = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkbibi = v6_eom_cc3_32_trans_aibjbkbibi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbibi
    function v6_eom_cc3_32_trans_aibjbkbibj(a, i, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbibj   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)
term(1) = term(1) + tvooo(a, j, j, k)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbkbibj = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkbibj = v6_eom_cc3_32_trans_aibjbkbibj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbibj
    function v6_eom_cc3_32_trans_aibjbkbibk(a, i, b, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbibk   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, i)
term(1) = term(1) + tvooo(a, i, i, j)
term(2) = term(2) + tvooo(a, j, k, k)
term(3) = term(3) + tvvvo(b, b, a, j)
term(4) = term(4) + tvvvo(a, b, b, j)

term(1) = -term(1) 
term(3) = term(3) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aibjbkbibk = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aibjbkbibk = v6_eom_cc3_32_trans_aibjbkbibk + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbibk
    function v6_eom_cc3_32_trans_aibjbiblbi(a, i, j, l) 
    double precision :: v6_eom_cc3_32_trans_aibjbiblbi   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, i)
term(1) = term(1) + tvooo(a, i, l, j)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    v6_eom_cc3_32_trans_aibjbiblbi = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbiblbi = v6_eom_cc3_32_trans_aibjbiblbi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbiblbi
    function v6_eom_cc3_32_trans_aibjbibjbm(a, i, m) 
    double precision :: v6_eom_cc3_32_trans_aibjbibjbm   
    integer, intent(in) :: a, i, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbibjbm = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbibjbm = v6_eom_cc3_32_trans_aibjbibjbm + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbibjbm
    function v6_eom_cc3_32_trans_aibjbiblbj(a, i, l) 
    double precision :: v6_eom_cc3_32_trans_aibjbiblbj   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbiblbj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbiblbj = v6_eom_cc3_32_trans_aibjbiblbj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbiblbj
    function v6_eom_cc3_32_trans_aibjbkbjbi(a, i, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbjbi   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, k)
term(1) = term(1) + tvooo(a, i, i, k)

term(1) = -term(1) 


    v6_eom_cc3_32_trans_aibjbkbjbi = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkbjbi = v6_eom_cc3_32_trans_aibjbkbjbi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbjbi
    function v6_eom_cc3_32_trans_aibjbkbkbi(a, i, b, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbkbi   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, k, k)
term(1) = term(1) + tvooo(a, j, i, i)
term(2) = term(2) + tvooo(a, i, i, j)
term(3) = term(3) + tvvvo(b, b, a, j)
term(4) = term(4) + tvvvo(a, b, b, j)

term(2) = -term(2) 
term(3) = term(3) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aibjbkbkbi = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aibjbkbkbi = v6_eom_cc3_32_trans_aibjbkbkbi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbkbi
    function v6_eom_cc3_32_trans_aibjbkbjbj(a, i, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbjbj   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, k)

term(0) = term(0) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aibjbkbjbj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbkbjbj = v6_eom_cc3_32_trans_aibjbkbjbj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbjbj
    function v6_eom_cc3_32_trans_aibjbkbjbk(a, i, b, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbjbk   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)
term(1) = term(1) + tvooo(a, i, j, j)
term(2) = term(2) + tvooo(a, i, k, k)
term(3) = term(3) + tvvvo(b, b, a, i)
term(4) = term(4) + tvvvo(a, b, b, i)

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 


    v6_eom_cc3_32_trans_aibjbkbjbk = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aibjbkbjbk = v6_eom_cc3_32_trans_aibjbkbjbk + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbjbk
    function v6_eom_cc3_32_trans_aibjbkbkbk(a, i, j, k) 
    double precision :: v6_eom_cc3_32_trans_aibjbkbkbk   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, k, i)
term(1) = term(1) + tvooo(a, i, k, j)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aibjbkbkbk = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbkbkbk = v6_eom_cc3_32_trans_aibjbkbkbk + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbkbkbk
    function v6_eom_cc3_32_trans_aibjbibiei(a, b, j, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbibiei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, j)
term(1) = term(1) + tvvvo(a, e, b, j)

term(0) = term(0) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aibjbibiei = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbibiei = v6_eom_cc3_32_trans_aibjbibiei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbibiei
    function v6_eom_cc3_32_trans_aibjbibiej(a, i, b, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbibiej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)
term(1) = term(1) + tvvvo(a, e, b, i)

term(1) = term(1) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aibjbibiej = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbibiej = v6_eom_cc3_32_trans_aibjbibiej + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbibiej
    function v6_eom_cc3_32_trans_aibjbibjei(a, i, b, e) 
    double precision :: v6_eom_cc3_32_trans_aibjbibjei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)
term(1) = term(1) + tvvvo(a, e, b, i)



    v6_eom_cc3_32_trans_aibjbibjei = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbibjei = v6_eom_cc3_32_trans_aibjbibjei + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbibjei
    function v6_eom_cc3_32_trans_aibjbidibi(a, b, j, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbidibi   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, j)
term(1) = term(1) + tvvvo(a, d, b, j)

term(0) = term(0) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aibjbidibi = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbidibi = v6_eom_cc3_32_trans_aibjbidibi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbidibi
    function v6_eom_cc3_32_trans_aibjbidibj(a, i, b, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbidibj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)
term(1) = term(1) + tvvvo(a, d, b, i)



    v6_eom_cc3_32_trans_aibjbidibj = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbidibj = v6_eom_cc3_32_trans_aibjbidibj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbidibj
    function v6_eom_cc3_32_trans_aibjbidjbi(a, i, b, d) 
    double precision :: v6_eom_cc3_32_trans_aibjbidjbi   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)
term(1) = term(1) + tvvvo(a, d, b, i)

term(1) = term(1) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aibjbidjbi = 0.d+0
    do s = 0, 1
    v6_eom_cc3_32_trans_aibjbidjbi = v6_eom_cc3_32_trans_aibjbidjbi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbidjbi
    function v6_eom_cc3_32_trans_aiajciaiai(a, i, j, c) 
    double precision :: v6_eom_cc3_32_trans_aiajciaiai   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, i, i)
term(1) = term(1) + tvooo(c, i, i, j)
term(2) = term(2) + tvvvo(c, a, a, j)
term(3) = term(3) + tvvvo(a, a, c, j)

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-4.0d+0) 


    v6_eom_cc3_32_trans_aiajciaiai = 0.d+0
    do s = 0, 3
    v6_eom_cc3_32_trans_aiajciaiai = v6_eom_cc3_32_trans_aiajciaiai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciaiai
    function v6_eom_cc3_32_trans_aiajciaiaj(a, i, j, c) 
    double precision :: v6_eom_cc3_32_trans_aiajciaiaj   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)
term(1) = term(1) + tvooo(c, i, j, j)
term(2) = term(2) + tvooo(c, i, i, i)
term(3) = term(3) + tvvvo(c, a, a, i)
term(4) = term(4) + tvvvo(a, a, c, i)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 
term(4) = term(4) * 2.0d+0 


    v6_eom_cc3_32_trans_aiajciaiaj = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aiajciaiaj = v6_eom_cc3_32_trans_aiajciaiaj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciaiaj
    function v6_eom_cc3_32_trans_aiajciajaj(i, j, c) 
    double precision :: v6_eom_cc3_32_trans_aiajciajaj   
    integer, intent(in) :: i, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, j, i)

term(0) = term(0) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aiajciajaj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajciajaj = v6_eom_cc3_32_trans_aiajciajaj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciajaj
    function v6_eom_cc3_32_trans_aibjbiaiai(a, b, j) 
    double precision :: v6_eom_cc3_32_trans_aibjbiaiai   
    integer, intent(in) :: a, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, b, j)

term(0) = term(0) * 2.0d+0 


    v6_eom_cc3_32_trans_aibjbiaiai = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbiaiai = v6_eom_cc3_32_trans_aibjbiaiai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbiaiai
    function v6_eom_cc3_32_trans_aibjbiajai(a, i, b) 
    double precision :: v6_eom_cc3_32_trans_aibjbiajai   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, b, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aibjbiajai = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbiajai = v6_eom_cc3_32_trans_aibjbiajai + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbiajai
    function v6_eom_cc3_32_trans_aiajciaici(a, i, j, c) 
    double precision :: v6_eom_cc3_32_trans_aiajciaici   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, i)
term(1) = term(1) + tvooo(a, i, i, j)
term(2) = term(2) + tvvvo(a, a, a, j)
term(3) = term(3) + tvvvo(c, c, a, j)
term(4) = term(4) + tvvvo(a, c, c, j)

term(0) = term(0) * (-2.0d+0) 
term(4) = term(4) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aiajciaici = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aiajciaici = v6_eom_cc3_32_trans_aiajciaici + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciaici
    function v6_eom_cc3_32_trans_aiajciaicj(a, i, j, c) 
    double precision :: v6_eom_cc3_32_trans_aiajciaicj   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)
term(1) = term(1) + tvooo(a, i, j, j)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvooo(a, i, i, i)
term(4) = term(4) + tvvvo(c, c, a, i)
term(5) = term(5) + tvvvo(a, c, c, i)

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aiajciaicj = 0.d+0
    do s = 0, 5
    v6_eom_cc3_32_trans_aiajciaicj = v6_eom_cc3_32_trans_aiajciaicj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciaicj
    function v6_eom_cc3_32_trans_aiajciajci(a, i, j, c) 
    double precision :: v6_eom_cc3_32_trans_aiajciajci   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)
term(1) = term(1) + tvooo(a, i, j, j)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvooo(a, i, i, i)
term(4) = term(4) + tvvvo(c, c, a, i)
term(5) = term(5) + tvvvo(a, c, c, i)

term(0) = -term(0) 
term(1) = -term(1) 
term(3) = -term(3) 


    v6_eom_cc3_32_trans_aiajciajci = 0.d+0
    do s = 0, 5
    v6_eom_cc3_32_trans_aiajciajci = v6_eom_cc3_32_trans_aiajciajci + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciajci
    function v6_eom_cc3_32_trans_aiajciajcj(a, i, j) 
    double precision :: v6_eom_cc3_32_trans_aiajciajcj   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, i)



    v6_eom_cc3_32_trans_aiajciajcj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajciajcj = v6_eom_cc3_32_trans_aiajciajcj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajciajcj
    function v6_eom_cc3_32_trans_aibjbiaibi(a, i, b, j) 
    double precision :: v6_eom_cc3_32_trans_aibjbiaibi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, i, j)
term(1) = term(1) + tvooo(b, j, i, i)
term(2) = term(2) + tvvvo(b, b, b, j)
term(3) = term(3) + tvvvo(b, a, a, j)
term(4) = term(4) + tvvvo(a, a, b, j)

term(1) = term(1) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aibjbiaibi = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aibjbiaibi = v6_eom_cc3_32_trans_aibjbiaibi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbiaibi
    function v6_eom_cc3_32_trans_aibjbiaibj(a, i, b, j) 
    double precision :: v6_eom_cc3_32_trans_aibjbiaibj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, j)
term(1) = term(1) + tvooo(b, j, j, i)
term(2) = term(2) + tvooo(b, i, i, i)
term(3) = term(3) + tvvvo(b, b, b, i)
term(4) = term(4) + tvvvo(b, a, a, i)
term(5) = term(5) + tvvvo(a, a, b, i)

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = -term(2) 


    v6_eom_cc3_32_trans_aibjbiaibj = 0.d+0
    do s = 0, 5
    v6_eom_cc3_32_trans_aibjbiaibj = v6_eom_cc3_32_trans_aibjbiaibj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbiaibj
    function v6_eom_cc3_32_trans_aibjbiajbi(a, i, b, j) 
    double precision :: v6_eom_cc3_32_trans_aibjbiajbi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, j)
term(1) = term(1) + tvooo(b, j, j, i)
term(2) = term(2) + tvvvo(b, b, b, i)
term(3) = term(3) + tvooo(b, i, i, i)
term(4) = term(4) + tvvvo(b, a, a, i)
term(5) = term(5) + tvvvo(a, a, b, i)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 2.0d+0 
term(5) = term(5) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aibjbiajbi = 0.d+0
    do s = 0, 5
    v6_eom_cc3_32_trans_aibjbiajbi = v6_eom_cc3_32_trans_aibjbiajbi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbiajbi
    function v6_eom_cc3_32_trans_aibjbiajbj(i, b, j) 
    double precision :: v6_eom_cc3_32_trans_aibjbiajbj   
    integer, intent(in) :: i, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, i)



    v6_eom_cc3_32_trans_aibjbiajbj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbiajbj = v6_eom_cc3_32_trans_aibjbiajbj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbiajbj
    function v6_eom_cc3_32_trans_aiajcicici(a, j, c) 
    double precision :: v6_eom_cc3_32_trans_aiajcicici   
    integer, intent(in) :: a, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, a, j)

term(0) = term(0) * 2.0d+0 


    v6_eom_cc3_32_trans_aiajcicici = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajcicici = v6_eom_cc3_32_trans_aiajcicici + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajcicici
    function v6_eom_cc3_32_trans_aiajcicicj(a, i, c) 
    double precision :: v6_eom_cc3_32_trans_aiajcicicj   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, a, i)

term(0) = -term(0) 


    v6_eom_cc3_32_trans_aiajcicicj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aiajcicicj = v6_eom_cc3_32_trans_aiajcicicj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aiajcicicj
    function v6_eom_cc3_32_trans_aibjbibibi(a, i, b, j) 
    double precision :: v6_eom_cc3_32_trans_aibjbibibi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, i)
term(1) = term(1) + tvooo(a, i, i, j)
term(2) = term(2) + tvvvo(b, b, a, j)
term(3) = term(3) + tvvvo(a, b, b, j)

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 2.0d+0 


    v6_eom_cc3_32_trans_aibjbibibi = 0.d+0
    do s = 0, 3
    v6_eom_cc3_32_trans_aibjbibibi = v6_eom_cc3_32_trans_aibjbibibi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbibibi
    function v6_eom_cc3_32_trans_aibjbibjbi(a, i, b, j) 
    double precision :: v6_eom_cc3_32_trans_aibjbibjbi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, j, i)
term(1) = term(1) + tvooo(a, i, j, j)
term(2) = term(2) + tvooo(a, i, i, i)
term(3) = term(3) + tvvvo(b, b, a, i)
term(4) = term(4) + tvvvo(a, b, b, i)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 


    v6_eom_cc3_32_trans_aibjbibjbi = 0.d+0
    do s = 0, 4
    v6_eom_cc3_32_trans_aibjbibjbi = v6_eom_cc3_32_trans_aibjbibjbi + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbibjbi
    function v6_eom_cc3_32_trans_aibjbibjbj(a, i, j) 
    double precision :: v6_eom_cc3_32_trans_aibjbibjbj   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, i)

term(0) = term(0) * (-2.0d+0) 


    v6_eom_cc3_32_trans_aibjbibjbj = 0.d+0
    do s = 0, 0
    v6_eom_cc3_32_trans_aibjbibjbj = v6_eom_cc3_32_trans_aibjbibjbj + term(s)
    end do

    end function v6_eom_cc3_32_trans_aibjbibjbj
    end module v6_eom_cc3_32_trans
    
