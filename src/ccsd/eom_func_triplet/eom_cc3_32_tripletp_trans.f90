module eom_cc3_32_tripletp_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_cc3_32_tripletp_trans_aibjckblcj(a, i, k, l) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckblcj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)



    eom_cc3_32_tripletp_trans_aibjckblcj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckblcj = eom_cc3_32_tripletp_trans_aibjckblcj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckblcj
    function eom_cc3_32_tripletp_trans_aibjckbjcm(a, i, k, m) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckbjcm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjckbjcm = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckbjcm = eom_cc3_32_tripletp_trans_aibjckbjcm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckbjcm
    function eom_cc3_32_tripletp_trans_aibjckblck(a, i, j, l) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckblck   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjckblck = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckblck = eom_cc3_32_tripletp_trans_aibjckblck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckblck
    function eom_cc3_32_tripletp_trans_aibjckbkcm(a, i, j, m) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckbkcm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    eom_cc3_32_tripletp_trans_aibjckbkcm = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckbkcm = eom_cc3_32_tripletp_trans_aibjckbkcm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckbkcm
    function eom_cc3_32_tripletp_trans_aibjckbjek(a, i, c, e) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckbjek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    eom_cc3_32_tripletp_trans_aibjckbjek = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckbjek = eom_cc3_32_tripletp_trans_aibjckbjek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckbjek
    function eom_cc3_32_tripletp_trans_aibjckcjek(a, i, b, e) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckcjek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjckcjek = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckcjek = eom_cc3_32_tripletp_trans_aibjckcjek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckcjek
    function eom_cc3_32_tripletp_trans_aibjckdjbk(a, i, c, d) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckdjbk   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjckdjbk = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckdjbk = eom_cc3_32_tripletp_trans_aibjckdjbk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckdjbk
    function eom_cc3_32_tripletp_trans_aibjckdjck(a, i, b, d) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckdjck   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    eom_cc3_32_tripletp_trans_aibjckdjck = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckdjck = eom_cc3_32_tripletp_trans_aibjckdjck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckdjck
    function eom_cc3_32_tripletp_trans_aiajckalcj(a, i, k, l) 
    double precision :: eom_cc3_32_tripletp_trans_aiajckalcj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)



    eom_cc3_32_tripletp_trans_aiajckalcj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajckalcj = eom_cc3_32_tripletp_trans_aiajckalcj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajckalcj
    function eom_cc3_32_tripletp_trans_aiajckajcm(a, i, k, m) 
    double precision :: eom_cc3_32_tripletp_trans_aiajckajcm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiajckajcm = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajckajcm = eom_cc3_32_tripletp_trans_aiajckajcm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajckajcm
    function eom_cc3_32_tripletp_trans_aiajckalck(a, i, j, l) 
    double precision :: eom_cc3_32_tripletp_trans_aiajckalck   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiajckalck = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajckalck = eom_cc3_32_tripletp_trans_aiajckalck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajckalck
    function eom_cc3_32_tripletp_trans_aiajckakcm(a, i, j, m) 
    double precision :: eom_cc3_32_tripletp_trans_aiajckakcm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    eom_cc3_32_tripletp_trans_aiajckakcm = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajckakcm = eom_cc3_32_tripletp_trans_aiajckakcm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajckakcm
    function eom_cc3_32_tripletp_trans_aiajckajek(a, i, c, e) 
    double precision :: eom_cc3_32_tripletp_trans_aiajckajek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    eom_cc3_32_tripletp_trans_aiajckajek = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajckajek = eom_cc3_32_tripletp_trans_aiajckajek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajckajek
    function eom_cc3_32_tripletp_trans_aibjakajek(a, i, b, e) 
    double precision :: eom_cc3_32_tripletp_trans_aibjakajek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjakajek = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjakajek = eom_cc3_32_tripletp_trans_aibjakajek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjakajek
    function eom_cc3_32_tripletp_trans_aibjckajbk(a, i, c) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckajbk   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjckajbk = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckajbk = eom_cc3_32_tripletp_trans_aibjckajbk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckajbk
    function eom_cc3_32_tripletp_trans_aibjckajck(a, i, b) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckajck   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, a, i)



    eom_cc3_32_tripletp_trans_aibjckajck = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckajck = eom_cc3_32_tripletp_trans_aibjckajck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckajck
    function eom_cc3_32_tripletp_trans_aibjakblaj(a, i, k, l) 
    double precision :: eom_cc3_32_tripletp_trans_aibjakblaj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)



    eom_cc3_32_tripletp_trans_aibjakblaj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjakblaj = eom_cc3_32_tripletp_trans_aibjakblaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjakblaj
    function eom_cc3_32_tripletp_trans_aibjakbjam(a, i, k, m) 
    double precision :: eom_cc3_32_tripletp_trans_aibjakbjam   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjakbjam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjakbjam = eom_cc3_32_tripletp_trans_aibjakbjam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjakbjam
    function eom_cc3_32_tripletp_trans_aibjakblak(a, i, j, l) 
    double precision :: eom_cc3_32_tripletp_trans_aibjakblak   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjakblak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjakblak = eom_cc3_32_tripletp_trans_aibjakblak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjakblak
    function eom_cc3_32_tripletp_trans_aibjakbkam(a, i, j, m) 
    double precision :: eom_cc3_32_tripletp_trans_aibjakbkam   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    eom_cc3_32_tripletp_trans_aibjakbkam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjakbkam = eom_cc3_32_tripletp_trans_aibjakbkam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjakbkam
    function eom_cc3_32_tripletp_trans_aibjakbjek(a, i, e) 
    double precision :: eom_cc3_32_tripletp_trans_aibjakbjek   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)



    eom_cc3_32_tripletp_trans_aibjakbjek = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjakbjek = eom_cc3_32_tripletp_trans_aibjakbjek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjakbjek
    function eom_cc3_32_tripletp_trans_aibjckbjak(a, i, c) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckbjak   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, a, i)



    eom_cc3_32_tripletp_trans_aibjckbjak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckbjak = eom_cc3_32_tripletp_trans_aibjckbjak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckbjak
    function eom_cc3_32_tripletp_trans_aibickblci(a, i, k, l) 
    double precision :: eom_cc3_32_tripletp_trans_aibickblci   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)



    eom_cc3_32_tripletp_trans_aibickblci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibickblci = eom_cc3_32_tripletp_trans_aibickblci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibickblci
    function eom_cc3_32_tripletp_trans_aibickbicm(a, i, k, m) 
    double precision :: eom_cc3_32_tripletp_trans_aibickbicm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibickbicm = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibickbicm = eom_cc3_32_tripletp_trans_aibickbicm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibickbicm
    function eom_cc3_32_tripletp_trans_aibickblck(a, i, l) 
    double precision :: eom_cc3_32_tripletp_trans_aibickblck   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibickblck = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibickblck = eom_cc3_32_tripletp_trans_aibickblck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibickblck
    function eom_cc3_32_tripletp_trans_aibickbkcm(a, i, m) 
    double precision :: eom_cc3_32_tripletp_trans_aibickbkcm   
    integer, intent(in) :: a, i, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)



    eom_cc3_32_tripletp_trans_aibickbkcm = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibickbkcm = eom_cc3_32_tripletp_trans_aibickbkcm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibickbkcm
    function eom_cc3_32_tripletp_trans_aibjciblci(a, i, j, l) 
    double precision :: eom_cc3_32_tripletp_trans_aibjciblci   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjciblci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjciblci = eom_cc3_32_tripletp_trans_aibjciblci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjciblci
    function eom_cc3_32_tripletp_trans_aibjckbjci(a, i, k) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckbjci   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjckbjci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckbjci = eom_cc3_32_tripletp_trans_aibjckbjci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckbjci
    function eom_cc3_32_tripletp_trans_aibjckbkci(a, i, j) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckbkci   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)



    eom_cc3_32_tripletp_trans_aibjckbkci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckbkci = eom_cc3_32_tripletp_trans_aibjckbkci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckbkci
    function eom_cc3_32_tripletp_trans_aibjcibicm(a, i, j, m) 
    double precision :: eom_cc3_32_tripletp_trans_aibjcibicm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    eom_cc3_32_tripletp_trans_aibjcibicm = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjcibicm = eom_cc3_32_tripletp_trans_aibjcibicm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjcibicm
    function eom_cc3_32_tripletp_trans_aibjciblcj(a, i, l) 
    double precision :: eom_cc3_32_tripletp_trans_aibjciblcj   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)



    eom_cc3_32_tripletp_trans_aibjciblcj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjciblcj = eom_cc3_32_tripletp_trans_aibjciblcj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjciblcj
    function eom_cc3_32_tripletp_trans_aibjcibjcm(a, i, m) 
    double precision :: eom_cc3_32_tripletp_trans_aibjcibjcm   
    integer, intent(in) :: a, i, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjcibjcm = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjcibjcm = eom_cc3_32_tripletp_trans_aibjcibjcm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjcibjcm
    function eom_cc3_32_tripletp_trans_aibjckbicj(a, i, k) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckbicj   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)



    eom_cc3_32_tripletp_trans_aibjckbicj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckbicj = eom_cc3_32_tripletp_trans_aibjckbicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckbicj
    function eom_cc3_32_tripletp_trans_aibjckbick(a, i, j) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckbick   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjckbick = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckbick = eom_cc3_32_tripletp_trans_aibjckbick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckbick
    function eom_cc3_32_tripletp_trans_aibjckbjck(a, i, b, j, c, k) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckbjck   
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvooo(a, i, j, j)
term(2) = term(2) + tvvvo(c, c, a, i)
term(3) = term(3) + tvvvo(b, b, a, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_32_tripletp_trans_aibjckbjck = 0.d+0
    do s = 0, 3
    eom_cc3_32_tripletp_trans_aibjckbjck = eom_cc3_32_tripletp_trans_aibjckbjck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckbjck
    function eom_cc3_32_tripletp_trans_aibickbiek(a, i, c, e) 
    double precision :: eom_cc3_32_tripletp_trans_aibickbiek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    eom_cc3_32_tripletp_trans_aibickbiek = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibickbiek = eom_cc3_32_tripletp_trans_aibickbiek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibickbiek
    function eom_cc3_32_tripletp_trans_aibjcibjei(a, i, c, e) 
    double precision :: eom_cc3_32_tripletp_trans_aibjcibjei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    eom_cc3_32_tripletp_trans_aibjcibjei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjcibjei = eom_cc3_32_tripletp_trans_aibjcibjei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjcibjei
    function eom_cc3_32_tripletp_trans_aiajckcjek(a, i, e) 
    double precision :: eom_cc3_32_tripletp_trans_aiajckcjek   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiajckcjek = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajckcjek = eom_cc3_32_tripletp_trans_aiajckcjek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajckcjek
    function eom_cc3_32_tripletp_trans_aibjckcjak(a, i, b) 
    double precision :: eom_cc3_32_tripletp_trans_aibjckcjak   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjckcjak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjckcjak = eom_cc3_32_tripletp_trans_aibjckcjak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjckcjak
    function eom_cc3_32_tripletp_trans_aibickciek(a, i, b, e) 
    double precision :: eom_cc3_32_tripletp_trans_aibickciek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibickciek = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibickciek = eom_cc3_32_tripletp_trans_aibickciek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibickciek
    function eom_cc3_32_tripletp_trans_aibjcicjei(a, i, b, e) 
    double precision :: eom_cc3_32_tripletp_trans_aibjcicjei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjcicjei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjcicjei = eom_cc3_32_tripletp_trans_aibjcicjei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjcicjei
    function eom_cc3_32_tripletp_trans_aiajckdjak(a, i, c, d) 
    double precision :: eom_cc3_32_tripletp_trans_aiajckdjak   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiajckdjak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajckdjak = eom_cc3_32_tripletp_trans_aiajckdjak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajckdjak
    function eom_cc3_32_tripletp_trans_aiajckdjck(a, i, d) 
    double precision :: eom_cc3_32_tripletp_trans_aiajckdjck   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)



    eom_cc3_32_tripletp_trans_aiajckdjck = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajckdjck = eom_cc3_32_tripletp_trans_aiajckdjck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajckdjck
    function eom_cc3_32_tripletp_trans_aibjakdjak(a, i, b, d) 
    double precision :: eom_cc3_32_tripletp_trans_aibjakdjak   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    eom_cc3_32_tripletp_trans_aibjakdjak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjakdjak = eom_cc3_32_tripletp_trans_aibjakdjak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjakdjak
    function eom_cc3_32_tripletp_trans_aibjakdjbk(a, i, d) 
    double precision :: eom_cc3_32_tripletp_trans_aibjakdjbk   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjakdjbk = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjakdjbk = eom_cc3_32_tripletp_trans_aibjakdjbk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjakdjbk
    function eom_cc3_32_tripletp_trans_aibickdibk(a, i, c, d) 
    double precision :: eom_cc3_32_tripletp_trans_aibickdibk   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibickdibk = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibickdibk = eom_cc3_32_tripletp_trans_aibickdibk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibickdibk
    function eom_cc3_32_tripletp_trans_aibjcidjbi(a, i, c, d) 
    double precision :: eom_cc3_32_tripletp_trans_aibjcidjbi   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjcidjbi = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjcidjbi = eom_cc3_32_tripletp_trans_aibjcidjbi + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjcidjbi
    function eom_cc3_32_tripletp_trans_aibickdick(a, i, b, d) 
    double precision :: eom_cc3_32_tripletp_trans_aibickdick   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    eom_cc3_32_tripletp_trans_aibickdick = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibickdick = eom_cc3_32_tripletp_trans_aibickdick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibickdick
    function eom_cc3_32_tripletp_trans_aibjcidjci(a, i, b, d) 
    double precision :: eom_cc3_32_tripletp_trans_aibjcidjci   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    eom_cc3_32_tripletp_trans_aibjcidjci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjcidjci = eom_cc3_32_tripletp_trans_aibjcidjci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjcidjci
    function eom_cc3_32_tripletp_trans_aiaickalci(a, i, k, l) 
    double precision :: eom_cc3_32_tripletp_trans_aiaickalci   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)



    eom_cc3_32_tripletp_trans_aiaickalci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiaickalci = eom_cc3_32_tripletp_trans_aiaickalci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiaickalci
    function eom_cc3_32_tripletp_trans_aiaickaicm(a, i, k, m) 
    double precision :: eom_cc3_32_tripletp_trans_aiaickaicm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiaickaicm = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiaickaicm = eom_cc3_32_tripletp_trans_aiaickaicm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiaickaicm
    function eom_cc3_32_tripletp_trans_aiaickalck(a, i, l) 
    double precision :: eom_cc3_32_tripletp_trans_aiaickalck   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiaickalck = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiaickalck = eom_cc3_32_tripletp_trans_aiaickalck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiaickalck
    function eom_cc3_32_tripletp_trans_aiaickakcm(a, i, m) 
    double precision :: eom_cc3_32_tripletp_trans_aiaickakcm   
    integer, intent(in) :: a, i, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)



    eom_cc3_32_tripletp_trans_aiaickakcm = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiaickakcm = eom_cc3_32_tripletp_trans_aiaickakcm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiaickakcm
    function eom_cc3_32_tripletp_trans_aiajcialci(a, i, j, l) 
    double precision :: eom_cc3_32_tripletp_trans_aiajcialci   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiajcialci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajcialci = eom_cc3_32_tripletp_trans_aiajcialci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajcialci
    function eom_cc3_32_tripletp_trans_aiajckajci(a, i, k) 
    double precision :: eom_cc3_32_tripletp_trans_aiajckajci   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiajckajci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajckajci = eom_cc3_32_tripletp_trans_aiajckajci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajckajci
    function eom_cc3_32_tripletp_trans_aiajckakci(a, i, j) 
    double precision :: eom_cc3_32_tripletp_trans_aiajckakci   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)



    eom_cc3_32_tripletp_trans_aiajckakci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajckakci = eom_cc3_32_tripletp_trans_aiajckakci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajckakci
    function eom_cc3_32_tripletp_trans_aiajciaicm(a, i, j, m) 
    double precision :: eom_cc3_32_tripletp_trans_aiajciaicm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    eom_cc3_32_tripletp_trans_aiajciaicm = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajciaicm = eom_cc3_32_tripletp_trans_aiajciaicm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajciaicm
    function eom_cc3_32_tripletp_trans_aiajcialcj(a, i, l) 
    double precision :: eom_cc3_32_tripletp_trans_aiajcialcj   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)



    eom_cc3_32_tripletp_trans_aiajcialcj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajcialcj = eom_cc3_32_tripletp_trans_aiajcialcj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajcialcj
    function eom_cc3_32_tripletp_trans_aiajciajcm(a, i, m) 
    double precision :: eom_cc3_32_tripletp_trans_aiajciajcm   
    integer, intent(in) :: a, i, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiajciajcm = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajciajcm = eom_cc3_32_tripletp_trans_aiajciajcm + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajciajcm
    function eom_cc3_32_tripletp_trans_aiajckaicj(a, i, k) 
    double precision :: eom_cc3_32_tripletp_trans_aiajckaicj   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)



    eom_cc3_32_tripletp_trans_aiajckaicj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajckaicj = eom_cc3_32_tripletp_trans_aiajckaicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajckaicj
    function eom_cc3_32_tripletp_trans_aiajckaick(a, i, j) 
    double precision :: eom_cc3_32_tripletp_trans_aiajckaick   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiajckaick = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajckaick = eom_cc3_32_tripletp_trans_aiajckaick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajckaick
    function eom_cc3_32_tripletp_trans_aiajckajck(a, i, j, c, k) 
    double precision :: eom_cc3_32_tripletp_trans_aiajckajck   
    integer, intent(in) :: a, i, j, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvooo(a, i, j, j)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvvvo(c, c, a, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_32_tripletp_trans_aiajckajck = 0.d+0
    do s = 0, 3
    eom_cc3_32_tripletp_trans_aiajckajck = eom_cc3_32_tripletp_trans_aiajckajck + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajckajck
    function eom_cc3_32_tripletp_trans_aiaickaiek(a, i, c, e) 
    double precision :: eom_cc3_32_tripletp_trans_aiaickaiek   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    eom_cc3_32_tripletp_trans_aiaickaiek = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiaickaiek = eom_cc3_32_tripletp_trans_aiaickaiek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiaickaiek
    function eom_cc3_32_tripletp_trans_aiajciajei(a, i, c, e) 
    double precision :: eom_cc3_32_tripletp_trans_aiajciajei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)



    eom_cc3_32_tripletp_trans_aiajciajei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajciajei = eom_cc3_32_tripletp_trans_aiajciajei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajciajei
    function eom_cc3_32_tripletp_trans_aibiakaiek(a, i, b, e) 
    double precision :: eom_cc3_32_tripletp_trans_aibiakaiek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibiakaiek = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibiakaiek = eom_cc3_32_tripletp_trans_aibiakaiek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibiakaiek
    function eom_cc3_32_tripletp_trans_aibjaiajei(a, i, b, e) 
    double precision :: eom_cc3_32_tripletp_trans_aibjaiajei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjaiajei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjaiajei = eom_cc3_32_tripletp_trans_aibjaiajei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjaiajei
    function eom_cc3_32_tripletp_trans_aibickaibk(a, i, c) 
    double precision :: eom_cc3_32_tripletp_trans_aibickaibk   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibickaibk = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibickaibk = eom_cc3_32_tripletp_trans_aibickaibk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibickaibk
    function eom_cc3_32_tripletp_trans_aibjciajbi(a, i, c) 
    double precision :: eom_cc3_32_tripletp_trans_aibjciajbi   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjciajbi = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjciajbi = eom_cc3_32_tripletp_trans_aibjciajbi + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjciajbi
    function eom_cc3_32_tripletp_trans_aibickaick(a, i, b) 
    double precision :: eom_cc3_32_tripletp_trans_aibickaick   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, a, i)



    eom_cc3_32_tripletp_trans_aibickaick = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibickaick = eom_cc3_32_tripletp_trans_aibickaick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibickaick
    function eom_cc3_32_tripletp_trans_aibjciajci(a, i, b) 
    double precision :: eom_cc3_32_tripletp_trans_aibjciajci   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, a, i)



    eom_cc3_32_tripletp_trans_aibjciajci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjciajci = eom_cc3_32_tripletp_trans_aibjciajci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjciajci
    function eom_cc3_32_tripletp_trans_aibiakblai(a, i, k, l) 
    double precision :: eom_cc3_32_tripletp_trans_aibiakblai   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, k)



    eom_cc3_32_tripletp_trans_aibiakblai = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibiakblai = eom_cc3_32_tripletp_trans_aibiakblai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibiakblai
    function eom_cc3_32_tripletp_trans_aibiakbiam(a, i, k, m) 
    double precision :: eom_cc3_32_tripletp_trans_aibiakbiam   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, k)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibiakbiam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibiakbiam = eom_cc3_32_tripletp_trans_aibiakbiam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibiakbiam
    function eom_cc3_32_tripletp_trans_aibiakblak(a, i, l) 
    double precision :: eom_cc3_32_tripletp_trans_aibiakblak   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibiakblak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibiakblak = eom_cc3_32_tripletp_trans_aibiakblak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibiakblak
    function eom_cc3_32_tripletp_trans_aibiakbkam(a, i, m) 
    double precision :: eom_cc3_32_tripletp_trans_aibiakbkam   
    integer, intent(in) :: a, i, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)



    eom_cc3_32_tripletp_trans_aibiakbkam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibiakbkam = eom_cc3_32_tripletp_trans_aibiakbkam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibiakbkam
    function eom_cc3_32_tripletp_trans_aibjaiblai(a, i, j, l) 
    double precision :: eom_cc3_32_tripletp_trans_aibjaiblai   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjaiblai = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjaiblai = eom_cc3_32_tripletp_trans_aibjaiblai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjaiblai
    function eom_cc3_32_tripletp_trans_aibjakbjai(a, i, k) 
    double precision :: eom_cc3_32_tripletp_trans_aibjakbjai   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjakbjai = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjakbjai = eom_cc3_32_tripletp_trans_aibjakbjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjakbjai
    function eom_cc3_32_tripletp_trans_aibjakbkai(a, i, j) 
    double precision :: eom_cc3_32_tripletp_trans_aibjakbkai   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)



    eom_cc3_32_tripletp_trans_aibjakbkai = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjakbkai = eom_cc3_32_tripletp_trans_aibjakbkai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjakbkai
    function eom_cc3_32_tripletp_trans_aibjaibiam(a, i, j, m) 
    double precision :: eom_cc3_32_tripletp_trans_aibjaibiam   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    eom_cc3_32_tripletp_trans_aibjaibiam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjaibiam = eom_cc3_32_tripletp_trans_aibjaibiam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjaibiam
    function eom_cc3_32_tripletp_trans_aibjaiblaj(a, i, l) 
    double precision :: eom_cc3_32_tripletp_trans_aibjaiblaj   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)



    eom_cc3_32_tripletp_trans_aibjaiblaj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjaiblaj = eom_cc3_32_tripletp_trans_aibjaiblaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjaiblaj
    function eom_cc3_32_tripletp_trans_aibjaibjam(a, i, m) 
    double precision :: eom_cc3_32_tripletp_trans_aibjaibjam   
    integer, intent(in) :: a, i, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjaibjam = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjaibjam = eom_cc3_32_tripletp_trans_aibjaibjam + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjaibjam
    function eom_cc3_32_tripletp_trans_aibjakbiaj(a, i, k) 
    double precision :: eom_cc3_32_tripletp_trans_aibjakbiaj   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, k)



    eom_cc3_32_tripletp_trans_aibjakbiaj = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjakbiaj = eom_cc3_32_tripletp_trans_aibjakbiaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjakbiaj
    function eom_cc3_32_tripletp_trans_aibjakbiak(a, i, j) 
    double precision :: eom_cc3_32_tripletp_trans_aibjakbiak   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjakbiak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjakbiak = eom_cc3_32_tripletp_trans_aibjakbiak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjakbiak
    function eom_cc3_32_tripletp_trans_aibjakbjak(a, i, b, j, k) 
    double precision :: eom_cc3_32_tripletp_trans_aibjakbjak   
    integer, intent(in) :: a, i, b, j, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvooo(a, i, j, j)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvvvo(b, b, a, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_32_tripletp_trans_aibjakbjak = 0.d+0
    do s = 0, 3
    eom_cc3_32_tripletp_trans_aibjakbjak = eom_cc3_32_tripletp_trans_aibjakbjak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjakbjak
    function eom_cc3_32_tripletp_trans_aibiakbiek(a, i, e) 
    double precision :: eom_cc3_32_tripletp_trans_aibiakbiek   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)



    eom_cc3_32_tripletp_trans_aibiakbiek = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibiakbiek = eom_cc3_32_tripletp_trans_aibiakbiek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibiakbiek
    function eom_cc3_32_tripletp_trans_aibjaibjei(a, i, e) 
    double precision :: eom_cc3_32_tripletp_trans_aibjaibjei   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)



    eom_cc3_32_tripletp_trans_aibjaibjei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjaibjei = eom_cc3_32_tripletp_trans_aibjaibjei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjaibjei
    function eom_cc3_32_tripletp_trans_aibickbiak(a, i, c) 
    double precision :: eom_cc3_32_tripletp_trans_aibickbiak   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, a, i)



    eom_cc3_32_tripletp_trans_aibickbiak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibickbiak = eom_cc3_32_tripletp_trans_aibickbiak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibickbiak
    function eom_cc3_32_tripletp_trans_aibjcibjai(a, i, c) 
    double precision :: eom_cc3_32_tripletp_trans_aibjcibjai   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, a, i)



    eom_cc3_32_tripletp_trans_aibjcibjai = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjcibjai = eom_cc3_32_tripletp_trans_aibjcibjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjcibjai
    function eom_cc3_32_tripletp_trans_aibickbick(a, i, b, c, k) 
    double precision :: eom_cc3_32_tripletp_trans_aibickbick   
    integer, intent(in) :: a, i, b, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvooo(a, i, i, i)
term(2) = term(2) + tvvvo(c, c, a, i)
term(3) = term(3) + tvvvo(b, b, a, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_32_tripletp_trans_aibickbick = 0.d+0
    do s = 0, 3
    eom_cc3_32_tripletp_trans_aibickbick = eom_cc3_32_tripletp_trans_aibickbick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibickbick
    function eom_cc3_32_tripletp_trans_aibjcibjci(a, i, b, j, c) 
    double precision :: eom_cc3_32_tripletp_trans_aibjcibjci   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, i, i, i)
term(2) = term(2) + tvvvo(c, c, a, i)
term(3) = term(3) + tvvvo(b, b, a, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_32_tripletp_trans_aibjcibjci = 0.d+0
    do s = 0, 3
    eom_cc3_32_tripletp_trans_aibjcibjci = eom_cc3_32_tripletp_trans_aibjcibjci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjcibjci
    function eom_cc3_32_tripletp_trans_aiaickciek(a, i, e) 
    double precision :: eom_cc3_32_tripletp_trans_aiaickciek   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiaickciek = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiaickciek = eom_cc3_32_tripletp_trans_aiaickciek + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiaickciek
    function eom_cc3_32_tripletp_trans_aiajcicjei(a, i, e) 
    double precision :: eom_cc3_32_tripletp_trans_aiajcicjei   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiajcicjei = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajcicjei = eom_cc3_32_tripletp_trans_aiajcicjei + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajcicjei
    function eom_cc3_32_tripletp_trans_aibickciak(a, i, b) 
    double precision :: eom_cc3_32_tripletp_trans_aibickciak   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibickciak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibickciak = eom_cc3_32_tripletp_trans_aibickciak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibickciak
    function eom_cc3_32_tripletp_trans_aibjcicjai(a, i, b) 
    double precision :: eom_cc3_32_tripletp_trans_aibjcicjai   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjcicjai = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjcicjai = eom_cc3_32_tripletp_trans_aibjcicjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjcicjai
    function eom_cc3_32_tripletp_trans_aiaickdiak(a, i, c, d) 
    double precision :: eom_cc3_32_tripletp_trans_aiaickdiak   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiaickdiak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiaickdiak = eom_cc3_32_tripletp_trans_aiaickdiak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiaickdiak
    function eom_cc3_32_tripletp_trans_aiajcidjai(a, i, c, d) 
    double precision :: eom_cc3_32_tripletp_trans_aiajcidjai   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aiajcidjai = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajcidjai = eom_cc3_32_tripletp_trans_aiajcidjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajcidjai
    function eom_cc3_32_tripletp_trans_aiaickdick(a, i, d) 
    double precision :: eom_cc3_32_tripletp_trans_aiaickdick   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)



    eom_cc3_32_tripletp_trans_aiaickdick = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiaickdick = eom_cc3_32_tripletp_trans_aiaickdick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiaickdick
    function eom_cc3_32_tripletp_trans_aiajcidjci(a, i, d) 
    double precision :: eom_cc3_32_tripletp_trans_aiajcidjci   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)



    eom_cc3_32_tripletp_trans_aiajcidjci = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aiajcidjci = eom_cc3_32_tripletp_trans_aiajcidjci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajcidjci
    function eom_cc3_32_tripletp_trans_aibiakdiak(a, i, b, d) 
    double precision :: eom_cc3_32_tripletp_trans_aibiakdiak   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    eom_cc3_32_tripletp_trans_aibiakdiak = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibiakdiak = eom_cc3_32_tripletp_trans_aibiakdiak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibiakdiak
    function eom_cc3_32_tripletp_trans_aibjaidjai(a, i, b, d) 
    double precision :: eom_cc3_32_tripletp_trans_aibjaidjai   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    eom_cc3_32_tripletp_trans_aibjaidjai = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjaidjai = eom_cc3_32_tripletp_trans_aibjaidjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjaidjai
    function eom_cc3_32_tripletp_trans_aibiakdibk(a, i, d) 
    double precision :: eom_cc3_32_tripletp_trans_aibiakdibk   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibiakdibk = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibiakdibk = eom_cc3_32_tripletp_trans_aibiakdibk + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibiakdibk
    function eom_cc3_32_tripletp_trans_aibjaidjbi(a, i, d) 
    double precision :: eom_cc3_32_tripletp_trans_aibjaidjbi   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)

term(0) = -term(0) 


    eom_cc3_32_tripletp_trans_aibjaidjbi = 0.d+0
    do s = 0, 0
    eom_cc3_32_tripletp_trans_aibjaidjbi = eom_cc3_32_tripletp_trans_aibjaidjbi + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjaidjbi
    function eom_cc3_32_tripletp_trans_aiaickaick(a, i, c, k) 
    double precision :: eom_cc3_32_tripletp_trans_aiaickaick   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvvvo(a, a, a, i)
term(2) = term(2) + tvooo(a, i, i, i)
term(3) = term(3) + tvvvo(c, c, a, i)

term(0) = -term(0) 
term(2) = -term(2) 


    eom_cc3_32_tripletp_trans_aiaickaick = 0.d+0
    do s = 0, 3
    eom_cc3_32_tripletp_trans_aiaickaick = eom_cc3_32_tripletp_trans_aiaickaick + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiaickaick
    function eom_cc3_32_tripletp_trans_aiajciajci(a, i, j, c) 
    double precision :: eom_cc3_32_tripletp_trans_aiajciajci   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, i, i, i)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvvvo(c, c, a, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_32_tripletp_trans_aiajciajci = 0.d+0
    do s = 0, 3
    eom_cc3_32_tripletp_trans_aiajciajci = eom_cc3_32_tripletp_trans_aiajciajci + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aiajciajci
    function eom_cc3_32_tripletp_trans_aibiakbiak(a, i, b, k) 
    double precision :: eom_cc3_32_tripletp_trans_aibiakbiak   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, k)
term(1) = term(1) + tvvvo(a, a, a, i)
term(2) = term(2) + tvooo(a, i, i, i)
term(3) = term(3) + tvvvo(b, b, a, i)

term(0) = -term(0) 
term(2) = -term(2) 


    eom_cc3_32_tripletp_trans_aibiakbiak = 0.d+0
    do s = 0, 3
    eom_cc3_32_tripletp_trans_aibiakbiak = eom_cc3_32_tripletp_trans_aibiakbiak + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibiakbiak
    function eom_cc3_32_tripletp_trans_aibjaibjai(a, i, b, j) 
    double precision :: eom_cc3_32_tripletp_trans_aibjaibjai   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvvvo(a, a, a, i)
term(2) = term(2) + tvooo(a, i, i, i)
term(3) = term(3) + tvvvo(b, b, a, i)

term(0) = -term(0) 
term(2) = -term(2) 


    eom_cc3_32_tripletp_trans_aibjaibjai = 0.d+0
    do s = 0, 3
    eom_cc3_32_tripletp_trans_aibjaibjai = eom_cc3_32_tripletp_trans_aibjaibjai + term(s) * 0.125d+0
    end do

    end function eom_cc3_32_tripletp_trans_aibjaibjai
    end module eom_cc3_32_tripletp_trans
    
