module v0_eom_cc3_32_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2013-02-27 23:45:57 UTC.
    !
    contains
    
    function v0_eom_cc3_32_trans_aibjciaicm(i, b, j, m) 
    double precision :: v0_eom_cc3_32_trans_aibjciaicm   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, m, i)
term(1) = term(1) + tvooo(b, i, m, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjciaicm = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjciaicm = v0_eom_cc3_32_trans_aibjciaicm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciaicm
    function v0_eom_cc3_32_trans_aibjcjaicm(b, j, m) 
    double precision :: v0_eom_cc3_32_trans_aibjcjaicm   
    integer, intent(in) :: b, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, m, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjaicm = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjaicm = v0_eom_cc3_32_trans_aibjcjaicm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjaicm
    function v0_eom_cc3_32_trans_aibjcialci(i, b, j, l) 
    double precision :: v0_eom_cc3_32_trans_aibjcialci   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcialci = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcialci = v0_eom_cc3_32_trans_aibjcialci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcialci
    function v0_eom_cc3_32_trans_aibjcialcj(i, b, l) 
    double precision :: v0_eom_cc3_32_trans_aibjcialcj   
    integer, intent(in) :: i, b, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, i)



    v0_eom_cc3_32_trans_aibjcialcj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcialcj = v0_eom_cc3_32_trans_aibjcialcj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcialcj
    function v0_eom_cc3_32_trans_aibjcjalci(b, j, l) 
    double precision :: v0_eom_cc3_32_trans_aibjcjalci   
    integer, intent(in) :: b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, j)



    v0_eom_cc3_32_trans_aibjcjalci = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjalci = v0_eom_cc3_32_trans_aibjcjalci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjalci
    function v0_eom_cc3_32_trans_aibjcjajcm(i, b, j, m) 
    double precision :: v0_eom_cc3_32_trans_aibjcjajcm   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, m, i)



    v0_eom_cc3_32_trans_aibjcjajcm = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjajcm = v0_eom_cc3_32_trans_aibjcjajcm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjajcm
    function v0_eom_cc3_32_trans_aibjcjalcj(i, b, j, l) 
    double precision :: v0_eom_cc3_32_trans_aibjcjalcj   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, l, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjalcj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjalcj = v0_eom_cc3_32_trans_aibjcjalcj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjalcj
    function v0_eom_cc3_32_trans_aibjciaibm(i, j, c, m) 
    double precision :: v0_eom_cc3_32_trans_aibjciaibm   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, j)
term(1) = term(1) + tvooo(c, j, m, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjciaibm = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjciaibm = v0_eom_cc3_32_trans_aibjciaibm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciaibm
    function v0_eom_cc3_32_trans_aibjcjaibm(j, c, m) 
    double precision :: v0_eom_cc3_32_trans_aibjcjaibm   
    integer, intent(in) :: j, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjaibm = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjaibm = v0_eom_cc3_32_trans_aibjcjaibm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjaibm
    function v0_eom_cc3_32_trans_aibjcialbi(i, j, c, l) 
    double precision :: v0_eom_cc3_32_trans_aibjcialbi   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, i)



    v0_eom_cc3_32_trans_aibjcialbi = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcialbi = v0_eom_cc3_32_trans_aibjcialbi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcialbi
    function v0_eom_cc3_32_trans_aibjcialbj(i, c, l) 
    double precision :: v0_eom_cc3_32_trans_aibjcialbj   
    integer, intent(in) :: i, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcialbj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcialbj = v0_eom_cc3_32_trans_aibjcialbj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcialbj
    function v0_eom_cc3_32_trans_aibjcjajbm(i, j, c, m) 
    double precision :: v0_eom_cc3_32_trans_aibjcjajbm   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, j)



    v0_eom_cc3_32_trans_aibjcjajbm = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjajbm = v0_eom_cc3_32_trans_aibjcjajbm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjajbm
    function v0_eom_cc3_32_trans_aibjcjalbj(i, j, c, l) 
    double precision :: v0_eom_cc3_32_trans_aibjcjalbj   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, j)
term(1) = term(1) + tvooo(c, j, l, i)

term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcjalbj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjalbj = v0_eom_cc3_32_trans_aibjcjalbj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjalbj
    function v0_eom_cc3_32_trans_aibjciaiei(b, j, c, e) 
    double precision :: v0_eom_cc3_32_trans_aibjciaiei   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, j)
term(1) = term(1) + tvvvo(b, e, c, j)

term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjciaiei = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjciaiei = v0_eom_cc3_32_trans_aibjciaiei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciaiei
    function v0_eom_cc3_32_trans_aibjciaiej(i, b, c, e) 
    double precision :: v0_eom_cc3_32_trans_aibjciaiej   
    integer, intent(in) :: i, b, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, c, i)
term(1) = term(1) + tvvvo(c, e, b, i)

term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjciaiej = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjciaiej = v0_eom_cc3_32_trans_aibjciaiej + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciaiej
    function v0_eom_cc3_32_trans_aibjcjaiej(b, j, c, e) 
    double precision :: v0_eom_cc3_32_trans_aibjcjaiej   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, j)
term(1) = term(1) + tvvvo(b, e, c, j)



    v0_eom_cc3_32_trans_aibjcjaiej = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjaiej = v0_eom_cc3_32_trans_aibjcjaiej + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjaiej
    function v0_eom_cc3_32_trans_aibjcjajei(b, j, c, e) 
    double precision :: v0_eom_cc3_32_trans_aibjcjajei   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, b, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjajei = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjajei = v0_eom_cc3_32_trans_aibjcjajei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjajei
    function v0_eom_cc3_32_trans_aibjcjajej(i, b, c, e) 
    double precision :: v0_eom_cc3_32_trans_aibjcjajej   
    integer, intent(in) :: i, b, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, c, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjajej = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjajej = v0_eom_cc3_32_trans_aibjcjajej + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjajej
    function v0_eom_cc3_32_trans_aibjcidiai(b, j, c, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcidiai   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, j)
term(1) = term(1) + tvvvo(b, d, c, j)

term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcidiai = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcidiai = v0_eom_cc3_32_trans_aibjcidiai + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcidiai
    function v0_eom_cc3_32_trans_aibjcjdiaj(b, j, c, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcjdiaj   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjdiaj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjdiaj = v0_eom_cc3_32_trans_aibjcjdiaj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjdiaj
    function v0_eom_cc3_32_trans_aibjcidjai(i, b, c, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcidjai   
    integer, intent(in) :: i, b, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, c, i)
term(1) = term(1) + tvvvo(c, d, b, i)

term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcidjai = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcidjai = v0_eom_cc3_32_trans_aibjcidjai + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcidjai
    function v0_eom_cc3_32_trans_aibjcjdjai(b, j, c, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcjdjai   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, b, j)
term(1) = term(1) + tvvvo(b, d, c, j)



    v0_eom_cc3_32_trans_aibjcjdjai = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjdjai = v0_eom_cc3_32_trans_aibjcjdjai + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjdjai
    function v0_eom_cc3_32_trans_aibjcjdjaj(i, b, c, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcjdjaj   
    integer, intent(in) :: i, b, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, c, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjdjaj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjdjaj = v0_eom_cc3_32_trans_aibjcjdjaj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjdjaj
    function v0_eom_cc3_32_trans_aibjciciei(a, b, j, e) 
    double precision :: v0_eom_cc3_32_trans_aibjciciei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, j)



    v0_eom_cc3_32_trans_aibjciciei = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjciciei = v0_eom_cc3_32_trans_aibjciciei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciciei
    function v0_eom_cc3_32_trans_aibjciciej(a, i, b, e) 
    double precision :: v0_eom_cc3_32_trans_aibjciciej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)



    v0_eom_cc3_32_trans_aibjciciej = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjciciej = v0_eom_cc3_32_trans_aibjciciej + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciciej
    function v0_eom_cc3_32_trans_aibjcjciej(a, b, j, e) 
    double precision :: v0_eom_cc3_32_trans_aibjcjciej   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, j)
term(1) = term(1) + tvvvo(a, e, b, j)

term(0) = -term(0) 
term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcjciej = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjciej = v0_eom_cc3_32_trans_aibjcjciej + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjciej
    function v0_eom_cc3_32_trans_aibjcicjei(a, i, b, e) 
    double precision :: v0_eom_cc3_32_trans_aibjcicjei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)
term(1) = term(1) + tvvvo(a, e, b, i)

term(0) = -term(0) 
term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcicjei = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcicjei = v0_eom_cc3_32_trans_aibjcicjei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcicjei
    function v0_eom_cc3_32_trans_aibjcjcjei(a, b, j, e) 
    double precision :: v0_eom_cc3_32_trans_aibjcjcjei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, b, j)



    v0_eom_cc3_32_trans_aibjcjcjei = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjcjei = v0_eom_cc3_32_trans_aibjcjcjei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjcjei
    function v0_eom_cc3_32_trans_aibjcjcjej(a, i, b, e) 
    double precision :: v0_eom_cc3_32_trans_aibjcjcjej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)



    v0_eom_cc3_32_trans_aibjcjcjej = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjcjej = v0_eom_cc3_32_trans_aibjcjcjej + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjcjej
    function v0_eom_cc3_32_trans_aibjcibicm(a, i, j, m) 
    double precision :: v0_eom_cc3_32_trans_aibjcibicm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)



    v0_eom_cc3_32_trans_aibjcibicm = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcibicm = v0_eom_cc3_32_trans_aibjcibicm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcibicm
    function v0_eom_cc3_32_trans_aibjciblci(a, i, j, l) 
    double precision :: v0_eom_cc3_32_trans_aibjciblci   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjciblci = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjciblci = v0_eom_cc3_32_trans_aibjciblci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciblci
    function v0_eom_cc3_32_trans_aibjcibjcm(a, i, m) 
    double precision :: v0_eom_cc3_32_trans_aibjcibjcm   
    integer, intent(in) :: a, i, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcibjcm = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcibjcm = v0_eom_cc3_32_trans_aibjcibjcm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcibjcm
    function v0_eom_cc3_32_trans_aibjciblcj(a, i, l) 
    double precision :: v0_eom_cc3_32_trans_aibjciblcj   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)



    v0_eom_cc3_32_trans_aibjciblcj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjciblcj = v0_eom_cc3_32_trans_aibjciblcj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciblcj
    function v0_eom_cc3_32_trans_aibjcjblci(a, j, l) 
    double precision :: v0_eom_cc3_32_trans_aibjcjblci   
    integer, intent(in) :: a, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, j)



    v0_eom_cc3_32_trans_aibjcjblci = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjblci = v0_eom_cc3_32_trans_aibjcjblci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjblci
    function v0_eom_cc3_32_trans_aibjcjbjcm(a, i, j, m) 
    double precision :: v0_eom_cc3_32_trans_aibjcjbjcm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)
term(1) = term(1) + tvooo(a, j, m, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjbjcm = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjbjcm = v0_eom_cc3_32_trans_aibjcjbjcm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjbjcm
    function v0_eom_cc3_32_trans_aibjcjblcj(a, i, j, l) 
    double precision :: v0_eom_cc3_32_trans_aibjcjblcj   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjblcj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjblcj = v0_eom_cc3_32_trans_aibjcjblcj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjblcj
    function v0_eom_cc3_32_trans_aibjcidici(a, b, j, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcidici   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, j)



    v0_eom_cc3_32_trans_aibjcidici = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcidici = v0_eom_cc3_32_trans_aibjcidici + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcidici
    function v0_eom_cc3_32_trans_aibjcidicj(a, i, b, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcidicj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)
term(1) = term(1) + tvvvo(a, d, b, i)

term(0) = -term(0) 
term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcidicj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcidicj = v0_eom_cc3_32_trans_aibjcidicj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcidicj
    function v0_eom_cc3_32_trans_aibjcjdicj(a, b, j, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcjdicj   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, b, j)



    v0_eom_cc3_32_trans_aibjcjdicj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjdicj = v0_eom_cc3_32_trans_aibjcjdicj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjdicj
    function v0_eom_cc3_32_trans_aibjcidjci(a, i, b, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcidjci   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    v0_eom_cc3_32_trans_aibjcidjci = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcidjci = v0_eom_cc3_32_trans_aibjcidjci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcidjci
    function v0_eom_cc3_32_trans_aibjcjdjci(a, b, j, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcjdjci   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, j)
term(1) = term(1) + tvvvo(a, d, b, j)

term(0) = -term(0) 
term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcjdjci = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjdjci = v0_eom_cc3_32_trans_aibjcjdjci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjdjci
    function v0_eom_cc3_32_trans_aibjcjdjcj(a, i, b, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcjdjcj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)



    v0_eom_cc3_32_trans_aibjcjdjcj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjdjcj = v0_eom_cc3_32_trans_aibjcjdjcj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjdjcj
    function v0_eom_cc3_32_trans_aibjcibiei(a, j, c, e) 
    double precision :: v0_eom_cc3_32_trans_aibjcibiei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, c, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcibiei = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcibiei = v0_eom_cc3_32_trans_aibjcibiei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcibiei
    function v0_eom_cc3_32_trans_aibjcibiej(a, i, c, e) 
    double precision :: v0_eom_cc3_32_trans_aibjcibiej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcibiej = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcibiej = v0_eom_cc3_32_trans_aibjcibiej + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcibiej
    function v0_eom_cc3_32_trans_aibjcibjei(a, i, c, e) 
    double precision :: v0_eom_cc3_32_trans_aibjcibjei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)
term(1) = term(1) + tvvvo(a, e, c, i)



    v0_eom_cc3_32_trans_aibjcibjei = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcibjei = v0_eom_cc3_32_trans_aibjcibjei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcibjei
    function v0_eom_cc3_32_trans_aibjcjbjei(a, j, c, e) 
    double precision :: v0_eom_cc3_32_trans_aibjcjbjei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, j)
term(1) = term(1) + tvvvo(a, e, c, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjbjei = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjbjei = v0_eom_cc3_32_trans_aibjcjbjei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjbjei
    function v0_eom_cc3_32_trans_aibjcjbjej(a, i, c, e) 
    double precision :: v0_eom_cc3_32_trans_aibjcjbjej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)
term(1) = term(1) + tvvvo(a, e, c, i)

term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcjbjej = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjbjej = v0_eom_cc3_32_trans_aibjcjbjej + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjbjej
    function v0_eom_cc3_32_trans_aibjcidibi(a, j, c, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcidibi   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, c, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcidibi = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcidibi = v0_eom_cc3_32_trans_aibjcidibi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcidibi
    function v0_eom_cc3_32_trans_aibjcidibj(a, i, c, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcidibj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)
term(1) = term(1) + tvvvo(a, d, c, i)



    v0_eom_cc3_32_trans_aibjcidibj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcidibj = v0_eom_cc3_32_trans_aibjcidibj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcidibj
    function v0_eom_cc3_32_trans_aibjcjdibj(a, j, c, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcjdibj   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, j)
term(1) = term(1) + tvvvo(a, d, c, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjdibj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjdibj = v0_eom_cc3_32_trans_aibjcjdibj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjdibj
    function v0_eom_cc3_32_trans_aibjcidjbi(a, i, c, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcidjbi   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcidjbi = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcidjbi = v0_eom_cc3_32_trans_aibjcidjbi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcidjbi
    function v0_eom_cc3_32_trans_aibjcjdjbj(a, i, c, d) 
    double precision :: v0_eom_cc3_32_trans_aibjcjdjbj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)
term(1) = term(1) + tvvvo(a, d, c, i)

term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcjdjbj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjdjbj = v0_eom_cc3_32_trans_aibjcjdjbj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjdjbj
    function v0_eom_cc3_32_trans_aiajcjaiam(j, c, m) 
    double precision :: v0_eom_cc3_32_trans_aiajcjaiam   
    integer, intent(in) :: j, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, m, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aiajcjaiam = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjaiam = v0_eom_cc3_32_trans_aiajcjaiam + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjaiam
    function v0_eom_cc3_32_trans_aiajcjalai(j, c, l) 
    double precision :: v0_eom_cc3_32_trans_aiajcjalai   
    integer, intent(in) :: j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, l, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aiajcjalai = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjalai = v0_eom_cc3_32_trans_aiajcjalai + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjalai
    function v0_eom_cc3_32_trans_aiajcjajam(i, j, c, m) 
    double precision :: v0_eom_cc3_32_trans_aiajcjajam   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, m, j)
term(1) = term(1) + tvooo(c, j, m, i)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    v0_eom_cc3_32_trans_aiajcjajam = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aiajcjajam = v0_eom_cc3_32_trans_aiajcjajam + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjajam
    function v0_eom_cc3_32_trans_aiajcjalaj(i, j, c, l) 
    double precision :: v0_eom_cc3_32_trans_aiajcjalaj   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, l, j)
term(1) = term(1) + tvooo(c, j, l, i)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    v0_eom_cc3_32_trans_aiajcjalaj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aiajcjalaj = v0_eom_cc3_32_trans_aiajcjalaj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjalaj
    function v0_eom_cc3_32_trans_aibjciaiai(a, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjciaiai   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, a, b, j)
term(1) = term(1) + tvvvo(b, a, c, j)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aibjciaiai = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjciaiai = v0_eom_cc3_32_trans_aibjciaiai + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciaiai
    function v0_eom_cc3_32_trans_aibjciaiaj(a, i, b, c) 
    double precision :: v0_eom_cc3_32_trans_aibjciaiaj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, i)
term(1) = term(1) + tvvvo(c, a, b, i)

term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjciaiaj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjciaiaj = v0_eom_cc3_32_trans_aibjciaiaj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciaiaj
    function v0_eom_cc3_32_trans_aibjcjaiaj(a, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjaiaj   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, j)



    v0_eom_cc3_32_trans_aibjcjaiaj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjaiaj = v0_eom_cc3_32_trans_aibjcjaiaj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjaiaj
    function v0_eom_cc3_32_trans_aibjciajai(a, i, b, c) 
    double precision :: v0_eom_cc3_32_trans_aibjciajai   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, i)
term(1) = term(1) + tvvvo(c, a, b, i)

term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjciajai = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjciajai = v0_eom_cc3_32_trans_aibjciajai + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciajai
    function v0_eom_cc3_32_trans_aibjcjajai(a, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjajai   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, j)



    v0_eom_cc3_32_trans_aibjcjajai = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjajai = v0_eom_cc3_32_trans_aibjcjajai + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjajai
    function v0_eom_cc3_32_trans_aibjcjajaj(a, i, b, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjajaj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, c, i)

term(0) = term(0) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aibjcjajaj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjajaj = v0_eom_cc3_32_trans_aibjcjajaj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjajaj
    function v0_eom_cc3_32_trans_aiajcjaicm(a, j, m) 
    double precision :: v0_eom_cc3_32_trans_aiajcjaicm   
    integer, intent(in) :: a, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, m, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aiajcjaicm = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjaicm = v0_eom_cc3_32_trans_aiajcjaicm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjaicm
    function v0_eom_cc3_32_trans_aiajcjalci(a, j, l) 
    double precision :: v0_eom_cc3_32_trans_aiajcjalci   
    integer, intent(in) :: a, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, l, j)

term(0) = term(0) * 2.0d+0 


    v0_eom_cc3_32_trans_aiajcjalci = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjalci = v0_eom_cc3_32_trans_aiajcjalci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjalci
    function v0_eom_cc3_32_trans_aiajcjajcm(a, i, j, m) 
    double precision :: v0_eom_cc3_32_trans_aiajcjajcm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, j)
term(1) = term(1) + tvooo(a, j, m, i)

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 


    v0_eom_cc3_32_trans_aiajcjajcm = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aiajcjajcm = v0_eom_cc3_32_trans_aiajcjajcm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjajcm
    function v0_eom_cc3_32_trans_aiajcjalcj(a, i, j, l) 
    double precision :: v0_eom_cc3_32_trans_aiajcjalcj   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, j)
term(1) = term(1) + tvooo(a, j, l, i)

term(0) = -term(0) 
term(1) = -term(1) 


    v0_eom_cc3_32_trans_aiajcjalcj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aiajcjalcj = v0_eom_cc3_32_trans_aiajcjalcj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjalcj
    function v0_eom_cc3_32_trans_aiajcjaiej(a, j, c, e) 
    double precision :: v0_eom_cc3_32_trans_aiajcjaiej   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, j)
term(1) = term(1) + tvvvo(a, e, c, j)



    v0_eom_cc3_32_trans_aiajcjaiej = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aiajcjaiej = v0_eom_cc3_32_trans_aiajcjaiej + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjaiej
    function v0_eom_cc3_32_trans_aiajcjajei(a, j, c, e) 
    double precision :: v0_eom_cc3_32_trans_aiajcjajei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, j)
term(1) = term(1) + tvvvo(a, e, c, j)

term(0) = term(0) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aiajcjajei = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aiajcjajei = v0_eom_cc3_32_trans_aiajcjajei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjajei
    function v0_eom_cc3_32_trans_aiajcjajej(a, i, c, e) 
    double precision :: v0_eom_cc3_32_trans_aiajcjajej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, e, a, i)
term(1) = term(1) + tvvvo(a, e, c, i)

term(1) = term(1) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aiajcjajej = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aiajcjajej = v0_eom_cc3_32_trans_aiajcjajej + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjajej
    function v0_eom_cc3_32_trans_aibibkaibm(i, b, k, m) 
    double precision :: v0_eom_cc3_32_trans_aibibkaibm   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, m, i)
term(1) = term(1) + tvooo(b, i, m, k)

term(0) = -term(0) 
term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibibkaibm = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibibkaibm = v0_eom_cc3_32_trans_aibibkaibm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkaibm
    function v0_eom_cc3_32_trans_aibibkalbi(i, b, k, l) 
    double precision :: v0_eom_cc3_32_trans_aibibkalbi   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, k)
term(1) = term(1) + tvooo(b, k, l, i)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibibkalbi = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibibkalbi = v0_eom_cc3_32_trans_aibibkalbi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkalbi
    function v0_eom_cc3_32_trans_aibibkakbm(i, b, m) 
    double precision :: v0_eom_cc3_32_trans_aibibkakbm   
    integer, intent(in) :: i, b, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, m, i)

term(0) = term(0) * 2.0d+0 


    v0_eom_cc3_32_trans_aibibkakbm = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkakbm = v0_eom_cc3_32_trans_aibibkakbm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkakbm
    function v0_eom_cc3_32_trans_aibibkalbk(i, b, l) 
    double precision :: v0_eom_cc3_32_trans_aibibkalbk   
    integer, intent(in) :: i, b, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, l, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibibkalbk = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkalbk = v0_eom_cc3_32_trans_aibibkalbk + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkalbk
    function v0_eom_cc3_32_trans_aibjciaici(a, i, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjciaici   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, i, i)
term(1) = term(1) + tvooo(b, i, i, j)
term(2) = term(2) + tvvvo(a, a, b, j)
term(3) = term(3) + tvvvo(c, c, b, j)
term(4) = term(4) + tvvvo(b, c, c, j)

term(0) = term(0) * (-2.0d+0) 
term(4) = -term(4) 


    v0_eom_cc3_32_trans_aibjciaici = 0.d+0
    do s = 0, 4
    v0_eom_cc3_32_trans_aibjciaici = v0_eom_cc3_32_trans_aibjciaici + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciaici
    function v0_eom_cc3_32_trans_aibjciaicj(a, i, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjciaicj   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)
term(1) = term(1) + tvooo(b, i, j, j)
term(2) = term(2) + tvooo(b, i, i, i)
term(3) = term(3) + tvvvo(b, a, a, i)
term(4) = term(4) + tvvvo(a, a, b, i)
term(5) = term(5) + tvvvo(b, c, c, i)
term(6) = term(6) + tvvvo(c, c, b, i)

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 


    v0_eom_cc3_32_trans_aibjciaicj = 0.d+0
    do s = 0, 6
    v0_eom_cc3_32_trans_aibjciaicj = v0_eom_cc3_32_trans_aibjciaicj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciaicj
    function v0_eom_cc3_32_trans_aibjcjaicj(a, i, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjaicj   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, i, i)
term(1) = term(1) + tvooo(b, j, j, j)
term(2) = term(2) + tvvvo(a, a, b, j)
term(3) = term(3) + tvvvo(c, c, b, j)
term(4) = term(4) + tvvvo(b, c, c, j)

term(0) = -term(0) 
term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcjaicj = 0.d+0
    do s = 0, 4
    v0_eom_cc3_32_trans_aibjcjaicj = v0_eom_cc3_32_trans_aibjcjaicj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjaicj
    function v0_eom_cc3_32_trans_aibjciajci(a, i, b, j) 
    double precision :: v0_eom_cc3_32_trans_aibjciajci   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, j, i)
term(1) = term(1) + tvvvo(b, a, a, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjciajci = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjciajci = v0_eom_cc3_32_trans_aibjciajci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciajci
    function v0_eom_cc3_32_trans_aibjciajcj(i, b, j) 
    double precision :: v0_eom_cc3_32_trans_aibjciajcj   
    integer, intent(in) :: i, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, j, i)



    v0_eom_cc3_32_trans_aibjciajcj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjciajcj = v0_eom_cc3_32_trans_aibjciajcj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciajcj
    function v0_eom_cc3_32_trans_aibjcjajci(a, i, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjajci   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, j, i, i)
term(1) = term(1) + tvooo(b, j, j, j)
term(2) = term(2) + tvvvo(b, a, a, j)
term(3) = term(3) + tvvvo(a, a, b, j)
term(4) = term(4) + tvvvo(c, c, b, j)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 


    v0_eom_cc3_32_trans_aibjcjajci = 0.d+0
    do s = 0, 4
    v0_eom_cc3_32_trans_aibjcjajci = v0_eom_cc3_32_trans_aibjcjajci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjajci
    function v0_eom_cc3_32_trans_aibjcjajcj(a, i, b, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjajcj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, a, i)
term(1) = term(1) + tvvvo(b, c, c, i)

term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcjajcj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjajcj = v0_eom_cc3_32_trans_aibjcjajcj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjajcj
    function v0_eom_cc3_32_trans_aibibkaiei(b, k, e) 
    double precision :: v0_eom_cc3_32_trans_aibibkaiei   
    integer, intent(in) :: b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, b, k)



    v0_eom_cc3_32_trans_aibibkaiei = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkaiei = v0_eom_cc3_32_trans_aibibkaiei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkaiei
    function v0_eom_cc3_32_trans_aibibkaiek(i, b, e) 
    double precision :: v0_eom_cc3_32_trans_aibibkaiek   
    integer, intent(in) :: i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, b, i)



    v0_eom_cc3_32_trans_aibibkaiek = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkaiek = v0_eom_cc3_32_trans_aibibkaiek + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkaiek
    function v0_eom_cc3_32_trans_aibibkakei(i, b, e) 
    double precision :: v0_eom_cc3_32_trans_aibibkakei   
    integer, intent(in) :: i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, b, i)

term(0) = term(0) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aibibkakei = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkakei = v0_eom_cc3_32_trans_aibibkakei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkakei
    function v0_eom_cc3_32_trans_aibjciaibi(a, i, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjciaibi   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, i, i)
term(1) = term(1) + tvooo(c, i, i, j)
term(2) = term(2) + tvvvo(a, a, c, j)
term(3) = term(3) + tvvvo(c, b, b, j)
term(4) = term(4) + tvvvo(b, b, c, j)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(4) = -term(4) 


    v0_eom_cc3_32_trans_aibjciaibi = 0.d+0
    do s = 0, 4
    v0_eom_cc3_32_trans_aibjciaibi = v0_eom_cc3_32_trans_aibjciaibi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciaibi
    function v0_eom_cc3_32_trans_aibjciaibj(a, i, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjciaibj   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, j, j)
term(1) = term(1) + tvooo(c, j, j, i)
term(2) = term(2) + tvooo(c, i, i, i)
term(3) = term(3) + tvvvo(c, a, a, i)
term(4) = term(4) + tvvvo(a, a, c, i)
term(5) = term(5) + tvvvo(b, b, c, i)
term(6) = term(6) + tvvvo(c, b, b, i)

term(0) = -term(0) 
term(2) = -term(2) 
term(6) = -term(6) 


    v0_eom_cc3_32_trans_aibjciaibj = 0.d+0
    do s = 0, 6
    v0_eom_cc3_32_trans_aibjciaibj = v0_eom_cc3_32_trans_aibjciaibj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciaibj
    function v0_eom_cc3_32_trans_aibjcjaibi(i, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjaibi   
    integer, intent(in) :: i, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, i, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjaibi = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjaibi = v0_eom_cc3_32_trans_aibjcjaibi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjaibi
    function v0_eom_cc3_32_trans_aibjcjaibj(a, i, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjaibj   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, i, j)
term(1) = term(1) + tvooo(c, j, i, i)
term(2) = term(2) + tvooo(c, j, j, j)
term(3) = term(3) + tvvvo(c, a, a, j)
term(4) = term(4) + tvvvo(a, a, c, j)
term(5) = term(5) + tvvvo(c, b, b, j)
term(6) = term(6) + tvvvo(b, b, c, j)

term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 


    v0_eom_cc3_32_trans_aibjcjaibj = 0.d+0
    do s = 0, 6
    v0_eom_cc3_32_trans_aibjcjaibj = v0_eom_cc3_32_trans_aibjcjaibj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjaibj
    function v0_eom_cc3_32_trans_aibjciajbi(a, i, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjciajbi   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, j, i)
term(1) = term(1) + tvvvo(c, a, a, i)

term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjciajbi = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjciajbi = v0_eom_cc3_32_trans_aibjciajbi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciajbi
    function v0_eom_cc3_32_trans_aibjciajbj(i, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjciajbj   
    integer, intent(in) :: i, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, j, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjciajbj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjciajbj = v0_eom_cc3_32_trans_aibjciajbj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjciajbj
    function v0_eom_cc3_32_trans_aibjcjajbi(i, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjajbi   
    integer, intent(in) :: i, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, i, j)
term(1) = term(1) + tvvvo(c, b, b, j)

term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcjajbi = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjajbi = v0_eom_cc3_32_trans_aibjcjajbi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjajbi
    function v0_eom_cc3_32_trans_aibjcjajbj(a, i, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjajbj   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, j, j)
term(1) = term(1) + tvooo(c, j, j, i)
term(2) = term(2) + tvvvo(c, a, a, i)
term(3) = term(3) + tvvvo(a, a, c, i)
term(4) = term(4) + tvvvo(b, b, c, i)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(3) = -term(3) 
term(4) = -term(4) 


    v0_eom_cc3_32_trans_aibjcjajbj = 0.d+0
    do s = 0, 4
    v0_eom_cc3_32_trans_aibjcjajbj = v0_eom_cc3_32_trans_aibjcjajbj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjajbj
    function v0_eom_cc3_32_trans_aiajcjdiaj(a, j, c, d) 
    double precision :: v0_eom_cc3_32_trans_aiajcjdiaj   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, j)
term(1) = term(1) + tvvvo(a, d, c, j)

term(0) = term(0) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aiajcjdiaj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aiajcjdiaj = v0_eom_cc3_32_trans_aiajcjdiaj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjdiaj
    function v0_eom_cc3_32_trans_aiajcjdjai(a, j, c, d) 
    double precision :: v0_eom_cc3_32_trans_aiajcjdjai   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, j)
term(1) = term(1) + tvvvo(a, d, c, j)



    v0_eom_cc3_32_trans_aiajcjdjai = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aiajcjdjai = v0_eom_cc3_32_trans_aiajcjdjai + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjdjai
    function v0_eom_cc3_32_trans_aiajcjdjaj(a, i, c, d) 
    double precision :: v0_eom_cc3_32_trans_aiajcjdjaj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, d, a, i)
term(1) = term(1) + tvvvo(a, d, c, i)

term(1) = term(1) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aiajcjdjaj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aiajcjdjaj = v0_eom_cc3_32_trans_aiajcjdjaj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjdjaj
    function v0_eom_cc3_32_trans_aibibkdiai(b, k, d) 
    double precision :: v0_eom_cc3_32_trans_aibibkdiai   
    integer, intent(in) :: b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, b, k)



    v0_eom_cc3_32_trans_aibibkdiai = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkdiai = v0_eom_cc3_32_trans_aibibkdiai + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkdiai
    function v0_eom_cc3_32_trans_aibibkdiak(i, b, d) 
    double precision :: v0_eom_cc3_32_trans_aibibkdiak   
    integer, intent(in) :: i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, b, i)

term(0) = term(0) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aibibkdiak = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkdiak = v0_eom_cc3_32_trans_aibibkdiak + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkdiak
    function v0_eom_cc3_32_trans_aibibkdkai(i, b, d) 
    double precision :: v0_eom_cc3_32_trans_aibibkdkai   
    integer, intent(in) :: i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, b, i)



    v0_eom_cc3_32_trans_aibibkdkai = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkdkai = v0_eom_cc3_32_trans_aibibkdkai + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkdkai
    function v0_eom_cc3_32_trans_aiajcjciej(a, j, e) 
    double precision :: v0_eom_cc3_32_trans_aiajcjciej   
    integer, intent(in) :: a, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, j)

term(0) = term(0) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aiajcjciej = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjciej = v0_eom_cc3_32_trans_aiajcjciej + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjciej
    function v0_eom_cc3_32_trans_aiajcjcjei(a, j, e) 
    double precision :: v0_eom_cc3_32_trans_aiajcjcjei   
    integer, intent(in) :: a, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, j)



    v0_eom_cc3_32_trans_aiajcjcjei = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjcjei = v0_eom_cc3_32_trans_aiajcjcjei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjcjei
    function v0_eom_cc3_32_trans_aiajcjcjej(a, i, e) 
    double precision :: v0_eom_cc3_32_trans_aiajcjcjej   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, e, a, i)



    v0_eom_cc3_32_trans_aiajcjcjej = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjcjej = v0_eom_cc3_32_trans_aiajcjcjej + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjcjej
    function v0_eom_cc3_32_trans_aiajcjdicj(a, j, d) 
    double precision :: v0_eom_cc3_32_trans_aiajcjdicj   
    integer, intent(in) :: a, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, j)



    v0_eom_cc3_32_trans_aiajcjdicj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjdicj = v0_eom_cc3_32_trans_aiajcjdicj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjdicj
    function v0_eom_cc3_32_trans_aiajcjdjci(a, j, d) 
    double precision :: v0_eom_cc3_32_trans_aiajcjdjci   
    integer, intent(in) :: a, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, j)

term(0) = term(0) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aiajcjdjci = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjdjci = v0_eom_cc3_32_trans_aiajcjdjci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjdjci
    function v0_eom_cc3_32_trans_aiajcjdjcj(a, i, d) 
    double precision :: v0_eom_cc3_32_trans_aiajcjdjcj   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, d, a, i)



    v0_eom_cc3_32_trans_aiajcjdjcj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjdjcj = v0_eom_cc3_32_trans_aiajcjdjcj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjdjcj
    function v0_eom_cc3_32_trans_aibibkbibm(a, i, k, m) 
    double precision :: v0_eom_cc3_32_trans_aibibkbibm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, m, i)
term(1) = term(1) + tvooo(a, i, m, k)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibibkbibm = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibibkbibm = v0_eom_cc3_32_trans_aibibkbibm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkbibm
    function v0_eom_cc3_32_trans_aibibkblbi(a, i, k, l) 
    double precision :: v0_eom_cc3_32_trans_aibibkblbi   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, l, i)
term(1) = term(1) + tvooo(a, i, l, k)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibibkblbi = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibibkblbi = v0_eom_cc3_32_trans_aibibkblbi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkblbi
    function v0_eom_cc3_32_trans_aibibkbkbm(a, i, m) 
    double precision :: v0_eom_cc3_32_trans_aibibkbkbm   
    integer, intent(in) :: a, i, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, m, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibibkbkbm = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkbkbm = v0_eom_cc3_32_trans_aibibkbkbm + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkbkbm
    function v0_eom_cc3_32_trans_aibibkblbk(a, i, l) 
    double precision :: v0_eom_cc3_32_trans_aibibkblbk   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, l, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibibkblbk = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkblbk = v0_eom_cc3_32_trans_aibibkblbk + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkblbk
    function v0_eom_cc3_32_trans_aibjcicici(a, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcicici   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, j)

term(0) = term(0) * 2.0d+0 


    v0_eom_cc3_32_trans_aibjcicici = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcicici = v0_eom_cc3_32_trans_aibjcicici + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcicici
    function v0_eom_cc3_32_trans_aibjcicicj(a, i, b, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcicicj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcicicj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcicicj = v0_eom_cc3_32_trans_aibjcicicj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcicicj
    function v0_eom_cc3_32_trans_aibjcjcicj(a, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjcicj   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjcicj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjcicj = v0_eom_cc3_32_trans_aibjcjcicj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjcicj
    function v0_eom_cc3_32_trans_aibjcicjci(a, i, b, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcicjci   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, b, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcicjci = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcicjci = v0_eom_cc3_32_trans_aibjcicjci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcicjci
    function v0_eom_cc3_32_trans_aibjcjcjci(a, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjcjci   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjcjci = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjcjci = v0_eom_cc3_32_trans_aibjcjcjci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjcjci
    function v0_eom_cc3_32_trans_aibjcjcjcj(a, i, b, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjcjcj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, c, a, i)

term(0) = term(0) * 2.0d+0 


    v0_eom_cc3_32_trans_aibjcjcjcj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjcjcj = v0_eom_cc3_32_trans_aibjcjcjcj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjcjcj
    function v0_eom_cc3_32_trans_aibibkbiei(a, b, k, e) 
    double precision :: v0_eom_cc3_32_trans_aibibkbiei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, k)
term(1) = term(1) + tvvvo(a, e, b, k)

term(0) = term(0) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aibibkbiei = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibibkbiei = v0_eom_cc3_32_trans_aibibkbiei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkbiei
    function v0_eom_cc3_32_trans_aibibkbiek(a, i, b, e) 
    double precision :: v0_eom_cc3_32_trans_aibibkbiek   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)
term(1) = term(1) + tvvvo(a, e, b, i)

term(1) = term(1) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aibibkbiek = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibibkbiek = v0_eom_cc3_32_trans_aibibkbiek + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkbiek
    function v0_eom_cc3_32_trans_aibibkbkei(a, i, b, e) 
    double precision :: v0_eom_cc3_32_trans_aibibkbkei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, e, a, i)
term(1) = term(1) + tvvvo(a, e, b, i)



    v0_eom_cc3_32_trans_aibibkbkei = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibibkbkei = v0_eom_cc3_32_trans_aibibkbkei + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkbkei
    function v0_eom_cc3_32_trans_aibibkdibi(a, b, k, d) 
    double precision :: v0_eom_cc3_32_trans_aibibkdibi   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, k)
term(1) = term(1) + tvvvo(a, d, b, k)

term(0) = term(0) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aibibkdibi = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibibkdibi = v0_eom_cc3_32_trans_aibibkdibi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkdibi
    function v0_eom_cc3_32_trans_aibibkdibk(a, i, b, d) 
    double precision :: v0_eom_cc3_32_trans_aibibkdibk   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)
term(1) = term(1) + tvvvo(a, d, b, i)



    v0_eom_cc3_32_trans_aibibkdibk = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibibkdibk = v0_eom_cc3_32_trans_aibibkdibk + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkdibk
    function v0_eom_cc3_32_trans_aibibkdkbi(a, i, b, d) 
    double precision :: v0_eom_cc3_32_trans_aibibkdkbi   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, d, a, i)
term(1) = term(1) + tvvvo(a, d, b, i)

term(1) = term(1) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aibibkdkbi = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibibkdkbi = v0_eom_cc3_32_trans_aibibkdkbi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkdkbi
    function v0_eom_cc3_32_trans_aibjcibici(a, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcibici   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, b, j)
term(1) = term(1) + tvvvo(a, c, c, j)

term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcibici = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcibici = v0_eom_cc3_32_trans_aibjcibici + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcibici
    function v0_eom_cc3_32_trans_aibjcibicj(a, i, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcibicj   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, i, i, i)
term(2) = term(2) + tvvvo(b, b, a, i)
term(3) = term(3) + tvvvo(a, b, b, i)
term(4) = term(4) + tvvvo(c, c, a, i)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 


    v0_eom_cc3_32_trans_aibjcibicj = 0.d+0
    do s = 0, 4
    v0_eom_cc3_32_trans_aibjcibicj = v0_eom_cc3_32_trans_aibjcibicj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcibicj
    function v0_eom_cc3_32_trans_aibjcjbici(a, i, j) 
    double precision :: v0_eom_cc3_32_trans_aibjcjbici   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, j)



    v0_eom_cc3_32_trans_aibjcjbici = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcjbici = v0_eom_cc3_32_trans_aibjcjbici + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjbici
    function v0_eom_cc3_32_trans_aibjcjbicj(a, i, b, j) 
    double precision :: v0_eom_cc3_32_trans_aibjcjbicj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)
term(1) = term(1) + tvvvo(a, b, b, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjbicj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjbicj = v0_eom_cc3_32_trans_aibjcjbicj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjbicj
    function v0_eom_cc3_32_trans_aibjcibjci(a, i, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcibjci   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, i, i, i)
term(2) = term(2) + tvvvo(b, b, a, i)
term(3) = term(3) + tvvvo(c, c, a, i)
term(4) = term(4) + tvvvo(a, c, c, i)

term(0) = -term(0) 
term(1) = -term(1) 


    v0_eom_cc3_32_trans_aibjcibjci = 0.d+0
    do s = 0, 4
    v0_eom_cc3_32_trans_aibjcibjci = v0_eom_cc3_32_trans_aibjcibjci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcibjci
    function v0_eom_cc3_32_trans_aibjcjbjci(a, i, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjbjci   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)
term(1) = term(1) + tvooo(a, j, i, i)
term(2) = term(2) + tvooo(a, j, j, j)
term(3) = term(3) + tvvvo(b, b, a, j)
term(4) = term(4) + tvvvo(a, b, b, j)
term(5) = term(5) + tvvvo(c, c, a, j)
term(6) = term(6) + tvvvo(a, c, c, j)

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 


    v0_eom_cc3_32_trans_aibjcjbjci = 0.d+0
    do s = 0, 6
    v0_eom_cc3_32_trans_aibjcjbjci = v0_eom_cc3_32_trans_aibjcjbjci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjbjci
    function v0_eom_cc3_32_trans_aibjcjbjcj(a, i, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjbjcj   
    integer, intent(in) :: a, i, b, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, j, j, i)
term(2) = term(2) + tvvvo(b, b, a, i)
term(3) = term(3) + tvvvo(c, c, a, i)
term(4) = term(4) + tvvvo(a, c, c, i)

term(0) = term(0) * (-2.0d+0) 
term(4) = -term(4) 


    v0_eom_cc3_32_trans_aibjcjbjcj = 0.d+0
    do s = 0, 4
    v0_eom_cc3_32_trans_aibjcjbjcj = v0_eom_cc3_32_trans_aibjcjbjcj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjbjcj
    function v0_eom_cc3_32_trans_aibjcibibi(a, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcibibi   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, j)

term(0) = term(0) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aibjcibibi = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcibibi = v0_eom_cc3_32_trans_aibjcibibi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcibibi
    function v0_eom_cc3_32_trans_aibjcibibj(a, i, b, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcibibj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, i)



    v0_eom_cc3_32_trans_aibjcibibj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcibibj = v0_eom_cc3_32_trans_aibjcibibj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcibibj
    function v0_eom_cc3_32_trans_aibjcjbibj(a, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjbibj   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, j)
term(1) = term(1) + tvvvo(a, b, c, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjbibj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjbibj = v0_eom_cc3_32_trans_aibjcjbibj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjbibj
    function v0_eom_cc3_32_trans_aibjcibjbi(a, i, b, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcibjbi   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, b, c, i)



    v0_eom_cc3_32_trans_aibjcibjbi = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibjcibjbi = v0_eom_cc3_32_trans_aibjcibjbi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcibjbi
    function v0_eom_cc3_32_trans_aibjcjbjbi(a, b, j, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjbjbi   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, j)
term(1) = term(1) + tvvvo(a, b, c, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibjcjbjbi = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjbjbi = v0_eom_cc3_32_trans_aibjcjbjbi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjbjbi
    function v0_eom_cc3_32_trans_aibjcjbjbj(a, i, b, c) 
    double precision :: v0_eom_cc3_32_trans_aibjcjbjbj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(c, b, a, i)
term(1) = term(1) + tvvvo(a, b, c, i)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aibjcjbjbj = 0.d+0
    do s = 0, 1
    v0_eom_cc3_32_trans_aibjcjbjbj = v0_eom_cc3_32_trans_aibjcjbjbj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibjcjbjbj
    function v0_eom_cc3_32_trans_aiajcjaiai(i, j, c) 
    double precision :: v0_eom_cc3_32_trans_aiajcjaiai   
    integer, intent(in) :: i, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, j, i, j)

term(0) = term(0) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aiajcjaiai = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjaiai = v0_eom_cc3_32_trans_aiajcjaiai + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjaiai
    function v0_eom_cc3_32_trans_aiajcjaiaj(a, i, j, c) 
    double precision :: v0_eom_cc3_32_trans_aiajcjaiaj   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, i, j)
term(1) = term(1) + tvooo(c, j, i, i)
term(2) = term(2) + tvooo(c, j, j, j)
term(3) = term(3) + tvvvo(c, a, a, j)
term(4) = term(4) + tvvvo(a, a, c, j)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 
term(4) = term(4) * 2.0d+0 


    v0_eom_cc3_32_trans_aiajcjaiaj = 0.d+0
    do s = 0, 4
    v0_eom_cc3_32_trans_aiajcjaiaj = v0_eom_cc3_32_trans_aiajcjaiaj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjaiaj
    function v0_eom_cc3_32_trans_aiajcjajaj(a, i, j, c) 
    double precision :: v0_eom_cc3_32_trans_aiajcjajaj   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(c, i, j, j)
term(1) = term(1) + tvooo(c, j, j, i)
term(2) = term(2) + tvvvo(c, a, a, i)
term(3) = term(3) + tvvvo(a, a, c, i)

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-4.0d+0) 


    v0_eom_cc3_32_trans_aiajcjajaj = 0.d+0
    do s = 0, 3
    v0_eom_cc3_32_trans_aiajcjajaj = v0_eom_cc3_32_trans_aiajcjajaj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjajaj
    function v0_eom_cc3_32_trans_aibibkaiai(a, b, k) 
    double precision :: v0_eom_cc3_32_trans_aibibkaiai   
    integer, intent(in) :: a, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, b, k)

term(0) = term(0) * 2.0d+0 


    v0_eom_cc3_32_trans_aibibkaiai = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkaiai = v0_eom_cc3_32_trans_aibibkaiai + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkaiai
    function v0_eom_cc3_32_trans_aibibkaiak(a, i, b) 
    double precision :: v0_eom_cc3_32_trans_aibibkaiak   
    integer, intent(in) :: a, i, b 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(b, a, b, i)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aibibkaiak = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkaiak = v0_eom_cc3_32_trans_aibibkaiak + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkaiak
    function v0_eom_cc3_32_trans_aiajcjaici(a, i, j) 
    double precision :: v0_eom_cc3_32_trans_aiajcjaici   
    integer, intent(in) :: a, i, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, j, i, j)



    v0_eom_cc3_32_trans_aiajcjaici = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjaici = v0_eom_cc3_32_trans_aiajcjaici + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjaici
    function v0_eom_cc3_32_trans_aiajcjaicj(a, i, j, c) 
    double precision :: v0_eom_cc3_32_trans_aiajcjaicj   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)
term(1) = term(1) + tvooo(a, j, i, i)
term(2) = term(2) + tvvvo(a, a, a, j)
term(3) = term(3) + tvooo(a, j, j, j)
term(4) = term(4) + tvvvo(c, c, a, j)
term(5) = term(5) + tvvvo(a, c, c, j)

term(0) = -term(0) 
term(1) = -term(1) 
term(3) = -term(3) 


    v0_eom_cc3_32_trans_aiajcjaicj = 0.d+0
    do s = 0, 5
    v0_eom_cc3_32_trans_aiajcjaicj = v0_eom_cc3_32_trans_aiajcjaicj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjaicj
    function v0_eom_cc3_32_trans_aiajcjajci(a, i, j, c) 
    double precision :: v0_eom_cc3_32_trans_aiajcjajci   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, i, j)
term(1) = term(1) + tvooo(a, j, i, i)
term(2) = term(2) + tvvvo(a, a, a, j)
term(3) = term(3) + tvooo(a, j, j, j)
term(4) = term(4) + tvvvo(c, c, a, j)
term(5) = term(5) + tvvvo(a, c, c, j)

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aiajcjajci = 0.d+0
    do s = 0, 5
    v0_eom_cc3_32_trans_aiajcjajci = v0_eom_cc3_32_trans_aiajcjajci + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjajci
    function v0_eom_cc3_32_trans_aiajcjajcj(a, i, j, c) 
    double precision :: v0_eom_cc3_32_trans_aiajcjajcj   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, j, j)
term(1) = term(1) + tvooo(a, j, j, i)
term(2) = term(2) + tvvvo(a, a, a, i)
term(3) = term(3) + tvvvo(c, c, a, i)
term(4) = term(4) + tvvvo(a, c, c, i)

term(0) = term(0) * (-2.0d+0) 
term(4) = term(4) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aiajcjajcj = 0.d+0
    do s = 0, 4
    v0_eom_cc3_32_trans_aiajcjajcj = v0_eom_cc3_32_trans_aiajcjajcj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjajcj
    function v0_eom_cc3_32_trans_aibibkaibi(a, i, b, k) 
    double precision :: v0_eom_cc3_32_trans_aibibkaibi   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, i, k)
term(1) = term(1) + tvooo(b, k, i, i)
term(2) = term(2) + tvvvo(b, b, b, k)
term(3) = term(3) + tvvvo(b, a, a, k)
term(4) = term(4) + tvvvo(a, a, b, k)

term(1) = term(1) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aibibkaibi = 0.d+0
    do s = 0, 4
    v0_eom_cc3_32_trans_aibibkaibi = v0_eom_cc3_32_trans_aibibkaibi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkaibi
    function v0_eom_cc3_32_trans_aibibkaibk(a, i, b, k) 
    double precision :: v0_eom_cc3_32_trans_aibibkaibk   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, k, k, i)
term(1) = term(1) + tvooo(b, i, k, k)
term(2) = term(2) + tvooo(b, i, i, i)
term(3) = term(3) + tvvvo(b, b, b, i)
term(4) = term(4) + tvvvo(b, a, a, i)
term(5) = term(5) + tvvvo(a, a, b, i)

term(0) = -term(0) 
term(1) = -term(1) 
term(2) = -term(2) 


    v0_eom_cc3_32_trans_aibibkaibk = 0.d+0
    do s = 0, 5
    v0_eom_cc3_32_trans_aibibkaibk = v0_eom_cc3_32_trans_aibibkaibk + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkaibk
    function v0_eom_cc3_32_trans_aibibkakbi(a, i, b, k) 
    double precision :: v0_eom_cc3_32_trans_aibibkakbi   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, k, k)
term(1) = term(1) + tvooo(b, k, k, i)
term(2) = term(2) + tvvvo(b, b, b, i)
term(3) = term(3) + tvooo(b, i, i, i)
term(4) = term(4) + tvvvo(b, a, a, i)
term(5) = term(5) + tvvvo(a, a, b, i)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 2.0d+0 
term(5) = term(5) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aibibkakbi = 0.d+0
    do s = 0, 5
    v0_eom_cc3_32_trans_aibibkakbi = v0_eom_cc3_32_trans_aibibkakbi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkakbi
    function v0_eom_cc3_32_trans_aibibkakbk(i, b, k) 
    double precision :: v0_eom_cc3_32_trans_aibibkakbk   
    integer, intent(in) :: i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(b, i, k, i)



    v0_eom_cc3_32_trans_aibibkakbk = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkakbk = v0_eom_cc3_32_trans_aibibkakbk + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkakbk
    function v0_eom_cc3_32_trans_aiajcjcicj(a, j, c) 
    double precision :: v0_eom_cc3_32_trans_aiajcjcicj   
    integer, intent(in) :: a, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, a, j)

term(0) = -term(0) 


    v0_eom_cc3_32_trans_aiajcjcicj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjcicj = v0_eom_cc3_32_trans_aiajcjcicj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjcicj
    function v0_eom_cc3_32_trans_aiajcjcjcj(a, i, c) 
    double precision :: v0_eom_cc3_32_trans_aiajcjcjcj   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvvo(a, c, a, i)

term(0) = term(0) * 2.0d+0 


    v0_eom_cc3_32_trans_aiajcjcjcj = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aiajcjcjcj = v0_eom_cc3_32_trans_aiajcjcjcj + term(s)
    end do

    end function v0_eom_cc3_32_trans_aiajcjcjcj
    function v0_eom_cc3_32_trans_aibibkbibi(a, i, b, k) 
    double precision :: v0_eom_cc3_32_trans_aibibkbibi   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, i, i)
term(1) = term(1) + tvooo(a, i, i, k)
term(2) = term(2) + tvvvo(b, b, a, k)
term(3) = term(3) + tvvvo(a, b, b, k)

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 2.0d+0 


    v0_eom_cc3_32_trans_aibibkbibi = 0.d+0
    do s = 0, 3
    v0_eom_cc3_32_trans_aibibkbibi = v0_eom_cc3_32_trans_aibibkbibi + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkbibi
    function v0_eom_cc3_32_trans_aibibkbibk(a, i, b, k) 
    double precision :: v0_eom_cc3_32_trans_aibibkbibk   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, k, k, i)
term(1) = term(1) + tvooo(a, i, k, k)
term(2) = term(2) + tvooo(a, i, i, i)
term(3) = term(3) + tvvvo(b, b, a, i)
term(4) = term(4) + tvvvo(a, b, b, i)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 


    v0_eom_cc3_32_trans_aibibkbibk = 0.d+0
    do s = 0, 4
    v0_eom_cc3_32_trans_aibibkbibk = v0_eom_cc3_32_trans_aibibkbibk + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkbibk
    function v0_eom_cc3_32_trans_aibibkbkbk(a, i, k) 
    double precision :: v0_eom_cc3_32_trans_aibibkbkbk   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvooo(a, i, k, i)

term(0) = term(0) * (-2.0d+0) 


    v0_eom_cc3_32_trans_aibibkbkbk = 0.d+0
    do s = 0, 0
    v0_eom_cc3_32_trans_aibibkbkbk = v0_eom_cc3_32_trans_aibibkbkbk + term(s)
    end do

    end function v0_eom_cc3_32_trans_aibibkbkbk
    end module v0_eom_cc3_32_trans
    
