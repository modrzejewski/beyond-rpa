module eom_cc3_23_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_cc3_23_trans_aibjaiblem(j, l, e, m) 
    double precision :: eom_cc3_23_trans_aibjaiblem   
    integer, intent(in) :: j, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, j)
term(1) = term(1) + tovoo(l, e, m, j)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjaiblem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaiblem = eom_cc3_23_trans_aibjaiblem + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiblem
    function eom_cc3_23_trans_aibjakblei(j, k, l, e) 
    double precision :: eom_cc3_23_trans_aibjakblei   
    integer, intent(in) :: j, k, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, l, j)



    eom_cc3_23_trans_aibjakblei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakblei = eom_cc3_23_trans_aibjakblei + term(s)
    end do

    end function eom_cc3_23_trans_aibjakblei
    function eom_cc3_23_trans_aibjakbjem(i, k, e, m) 
    double precision :: eom_cc3_23_trans_aibjakbjem   
    integer, intent(in) :: i, k, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, i)
term(1) = term(1) + tovoo(k, e, m, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjakbjem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakbjem = eom_cc3_23_trans_aibjakbjem + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbjem
    function eom_cc3_23_trans_aibjakblej(i, k, l, e) 
    double precision :: eom_cc3_23_trans_aibjakblej   
    integer, intent(in) :: i, k, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, i)



    eom_cc3_23_trans_aibjakblej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakblej = eom_cc3_23_trans_aibjakblej + term(s)
    end do

    end function eom_cc3_23_trans_aibjakblej
    function eom_cc3_23_trans_aibjaidlbm(j, d, l, m) 
    double precision :: eom_cc3_23_trans_aibjaidlbm   
    integer, intent(in) :: j, d, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, m, j)
term(1) = term(1) + tovoo(m, d, l, j)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjaidlbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaidlbm = eom_cc3_23_trans_aibjaidlbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidlbm
    function eom_cc3_23_trans_aibjakdibm(j, k, d, m) 
    double precision :: eom_cc3_23_trans_aibjakdibm   
    integer, intent(in) :: j, k, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, m, j)



    eom_cc3_23_trans_aibjakdibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakdibm = eom_cc3_23_trans_aibjakdibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdibm
    function eom_cc3_23_trans_aibjakdjbm(i, k, d, m) 
    double precision :: eom_cc3_23_trans_aibjakdjbm   
    integer, intent(in) :: i, k, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, k, i)



    eom_cc3_23_trans_aibjakdjbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakdjbm = eom_cc3_23_trans_aibjakdjbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdjbm
    function eom_cc3_23_trans_aibjakdlbj(i, k, d, l) 
    double precision :: eom_cc3_23_trans_aibjakdlbj   
    integer, intent(in) :: i, k, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, k, i)
term(1) = term(1) + tovoo(k, d, l, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjakdlbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakdlbj = eom_cc3_23_trans_aibjakdlbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdlbj
    function eom_cc3_23_trans_aibjaidjem(b, d, e, m) 
    double precision :: eom_cc3_23_trans_aibjaidjem   
    integer, intent(in) :: b, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, m, e)
term(1) = term(1) + tvvov(b, e, m, d)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibjaidjem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaidjem = eom_cc3_23_trans_aibjaidjem + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidjem
    function eom_cc3_23_trans_aibjaidlej(b, d, l, e) 
    double precision :: eom_cc3_23_trans_aibjaidlej   
    integer, intent(in) :: b, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, l, e)
term(1) = term(1) + tvvov(b, e, l, d)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjaidlej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaidlej = eom_cc3_23_trans_aibjaidlej + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidlej
    function eom_cc3_23_trans_aibjakdiej(b, k, d, e) 
    double precision :: eom_cc3_23_trans_aibjakdiej   
    integer, intent(in) :: b, k, d, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, e, k, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakdiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakdiej = eom_cc3_23_trans_aibjakdiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdiej
    function eom_cc3_23_trans_aibjakdjei(b, k, d, e) 
    double precision :: eom_cc3_23_trans_aibjakdjei   
    integer, intent(in) :: b, k, d, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, k, e)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakdjei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakdjei = eom_cc3_23_trans_aibjakdjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdjei
    function eom_cc3_23_trans_aibjbjdiem(a, d, e, m) 
    double precision :: eom_cc3_23_trans_aibjbjdiem   
    integer, intent(in) :: a, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, e)
term(1) = term(1) + tvvov(a, e, m, d)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibjbjdiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjbjdiem = eom_cc3_23_trans_aibjbjdiem + term(s)
    end do

    end function eom_cc3_23_trans_aibjbjdiem
    function eom_cc3_23_trans_aibjbjdlei(a, d, l, e) 
    double precision :: eom_cc3_23_trans_aibjbjdlei   
    integer, intent(in) :: a, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, e)
term(1) = term(1) + tvvov(a, e, l, d)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjbjdlei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjbjdlei = eom_cc3_23_trans_aibjbjdlei + term(s)
    end do

    end function eom_cc3_23_trans_aibjbjdlei
    function eom_cc3_23_trans_aibjbkdiej(a, k, d, e) 
    double precision :: eom_cc3_23_trans_aibjbkdiej   
    integer, intent(in) :: a, k, d, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, k, e)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjbkdiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbkdiej = eom_cc3_23_trans_aibjbkdiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjbkdiej
    function eom_cc3_23_trans_aibjbkdjei(a, k, d, e) 
    double precision :: eom_cc3_23_trans_aibjbkdjei   
    integer, intent(in) :: a, k, d, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, k, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjbkdjei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbkdjei = eom_cc3_23_trans_aibjbkdjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjbkdjei
    function eom_cc3_23_trans_aibjcialbm(j, c, l, m) 
    double precision :: eom_cc3_23_trans_aibjcialbm   
    integer, intent(in) :: j, c, l, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, m, j)



    eom_cc3_23_trans_aibjcialbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcialbm = eom_cc3_23_trans_aibjcialbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjcialbm
    function eom_cc3_23_trans_aibjcjalbm(i, c, l, m) 
    double precision :: eom_cc3_23_trans_aibjcjalbm   
    integer, intent(in) :: i, c, l, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, c, l, i)



    eom_cc3_23_trans_aibjcjalbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjalbm = eom_cc3_23_trans_aibjcjalbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjalbm
    function eom_cc3_23_trans_aibjckaibm(j, c, k, m) 
    double precision :: eom_cc3_23_trans_aibjckaibm   
    integer, intent(in) :: j, c, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, m, j)
term(1) = term(1) + tovoo(m, c, k, j)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjckaibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjckaibm = eom_cc3_23_trans_aibjckaibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjckaibm
    function eom_cc3_23_trans_aibjckalbj(i, c, k, l) 
    double precision :: eom_cc3_23_trans_aibjckalbj   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, l, i)
term(1) = term(1) + tovoo(l, c, k, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjckalbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjckalbj = eom_cc3_23_trans_aibjckalbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjckalbj
    function eom_cc3_23_trans_aibjcialej(b, c, l, e) 
    double precision :: eom_cc3_23_trans_aibjcialej   
    integer, intent(in) :: b, c, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, e, l, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcialej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcialej = eom_cc3_23_trans_aibjcialej + term(s)
    end do

    end function eom_cc3_23_trans_aibjcialej
    function eom_cc3_23_trans_aibjcjaiem(b, c, e, m) 
    double precision :: eom_cc3_23_trans_aibjcjaiem   
    integer, intent(in) :: b, c, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, e)
term(1) = term(1) + tvvov(b, e, m, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibjcjaiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjaiem = eom_cc3_23_trans_aibjcjaiem + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjaiem
    function eom_cc3_23_trans_aibjcjalei(b, c, l, e) 
    double precision :: eom_cc3_23_trans_aibjcjalei   
    integer, intent(in) :: b, c, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, e)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjalei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjalei = eom_cc3_23_trans_aibjcjalei + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjalei
    function eom_cc3_23_trans_aibjckaiej(b, c, k, e) 
    double precision :: eom_cc3_23_trans_aibjckaiej   
    integer, intent(in) :: b, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, k, e)
term(1) = term(1) + tvvov(b, e, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjckaiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjckaiej = eom_cc3_23_trans_aibjckaiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjckaiej
    function eom_cc3_23_trans_aibjcidjam(b, c, d, m) 
    double precision :: eom_cc3_23_trans_aibjcidjam   
    integer, intent(in) :: b, c, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, m, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcidjam = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcidjam = eom_cc3_23_trans_aibjcidjam + term(s)
    end do

    end function eom_cc3_23_trans_aibjcidjam
    function eom_cc3_23_trans_aibjcjdiam(b, c, d, m) 
    double precision :: eom_cc3_23_trans_aibjcjdiam   
    integer, intent(in) :: b, c, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjdiam = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjdiam = eom_cc3_23_trans_aibjcjdiam + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjdiam
    function eom_cc3_23_trans_aibjcjdlai(b, c, d, l) 
    double precision :: eom_cc3_23_trans_aibjcjdlai   
    integer, intent(in) :: b, c, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, d)
term(1) = term(1) + tvvov(b, d, l, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibjcjdlai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjdlai = eom_cc3_23_trans_aibjcjdlai + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjdlai
    function eom_cc3_23_trans_aibjckdjai(b, c, k, d) 
    double precision :: eom_cc3_23_trans_aibjckdjai   
    integer, intent(in) :: b, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, k, d)
term(1) = term(1) + tvvov(b, d, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjckdjai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjckdjai = eom_cc3_23_trans_aibjckdjai + term(s)
    end do

    end function eom_cc3_23_trans_aibjckdjai
    function eom_cc3_23_trans_aibjcibjem(a, c, e, m) 
    double precision :: eom_cc3_23_trans_aibjcibjem   
    integer, intent(in) :: a, c, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, e)
term(1) = term(1) + tvvov(a, e, m, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibjcibjem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcibjem = eom_cc3_23_trans_aibjcibjem + term(s)
    end do

    end function eom_cc3_23_trans_aibjcibjem
    function eom_cc3_23_trans_aibjciblej(a, c, l, e) 
    double precision :: eom_cc3_23_trans_aibjciblej   
    integer, intent(in) :: a, c, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, e)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjciblej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjciblej = eom_cc3_23_trans_aibjciblej + term(s)
    end do

    end function eom_cc3_23_trans_aibjciblej
    function eom_cc3_23_trans_aibjcjblei(a, c, l, e) 
    double precision :: eom_cc3_23_trans_aibjcjblei   
    integer, intent(in) :: a, c, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, l, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjblei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjblei = eom_cc3_23_trans_aibjcjblei + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjblei
    function eom_cc3_23_trans_aibjckbjei(a, c, k, e) 
    double precision :: eom_cc3_23_trans_aibjckbjei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, e)
term(1) = term(1) + tvvov(a, e, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjckbjei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjckbjei = eom_cc3_23_trans_aibjckbjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjckbjei
    function eom_cc3_23_trans_aibjcidjbm(a, c, d, m) 
    double precision :: eom_cc3_23_trans_aibjcidjbm   
    integer, intent(in) :: a, c, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcidjbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcidjbm = eom_cc3_23_trans_aibjcidjbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjcidjbm
    function eom_cc3_23_trans_aibjcidlbj(a, c, d, l) 
    double precision :: eom_cc3_23_trans_aibjcidlbj   
    integer, intent(in) :: a, c, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, d)
term(1) = term(1) + tvvov(a, d, l, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibjcidlbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcidlbj = eom_cc3_23_trans_aibjcidlbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcidlbj
    function eom_cc3_23_trans_aibjcjdibm(a, c, d, m) 
    double precision :: eom_cc3_23_trans_aibjcjdibm   
    integer, intent(in) :: a, c, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjdibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjdibm = eom_cc3_23_trans_aibjcjdibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjdibm
    function eom_cc3_23_trans_aibjckdibj(a, c, k, d) 
    double precision :: eom_cc3_23_trans_aibjckdibj   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, d)
term(1) = term(1) + tvvov(a, d, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjckdibj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjckdibj = eom_cc3_23_trans_aibjckdibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjckdibj
    function eom_cc3_23_trans_aiajaialem(j, l, e, m) 
    double precision :: eom_cc3_23_trans_aiajaialem   
    integer, intent(in) :: j, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, j)
term(1) = term(1) + tovoo(l, e, m, j)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajaialem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajaialem = eom_cc3_23_trans_aiajaialem + term(s)
    end do

    end function eom_cc3_23_trans_aiajaialem
    function eom_cc3_23_trans_aiajajalem(i, l, e, m) 
    double precision :: eom_cc3_23_trans_aiajajalem   
    integer, intent(in) :: i, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, i)
term(1) = term(1) + tovoo(l, e, m, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajajalem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajajalem = eom_cc3_23_trans_aiajajalem + term(s)
    end do

    end function eom_cc3_23_trans_aiajajalem
    function eom_cc3_23_trans_aiajakaiem(j, k, e, m) 
    double precision :: eom_cc3_23_trans_aiajakaiem   
    integer, intent(in) :: j, k, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, j)
term(1) = term(1) + tovoo(k, e, m, j)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajakaiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajakaiem = eom_cc3_23_trans_aiajakaiem + term(s)
    end do

    end function eom_cc3_23_trans_aiajakaiem
    function eom_cc3_23_trans_aiajakalei(j, k, l, e) 
    double precision :: eom_cc3_23_trans_aiajakalei   
    integer, intent(in) :: j, k, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, j)
term(1) = term(1) + tovoo(k, e, l, j)



    eom_cc3_23_trans_aiajakalei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajakalei = eom_cc3_23_trans_aiajakalei + term(s)
    end do

    end function eom_cc3_23_trans_aiajakalei
    function eom_cc3_23_trans_aiajakajem(i, k, e, m) 
    double precision :: eom_cc3_23_trans_aiajakajem   
    integer, intent(in) :: i, k, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, i)
term(1) = term(1) + tovoo(k, e, m, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajakajem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajakajem = eom_cc3_23_trans_aiajakajem + term(s)
    end do

    end function eom_cc3_23_trans_aiajakajem
    function eom_cc3_23_trans_aiajakalej(i, k, l, e) 
    double precision :: eom_cc3_23_trans_aiajakalej   
    integer, intent(in) :: i, k, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, i)
term(1) = term(1) + tovoo(k, e, l, i)



    eom_cc3_23_trans_aiajakalej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajakalej = eom_cc3_23_trans_aiajakalej + term(s)
    end do

    end function eom_cc3_23_trans_aiajakalej
    function eom_cc3_23_trans_aiajaidjem(a, d, e, m) 
    double precision :: eom_cc3_23_trans_aiajaidjem   
    integer, intent(in) :: a, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, e)
term(1) = term(1) + tvvov(a, e, m, d)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajaidjem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajaidjem = eom_cc3_23_trans_aiajaidjem + term(s)
    end do

    end function eom_cc3_23_trans_aiajaidjem
    function eom_cc3_23_trans_aiajaidlej(a, d, l, e) 
    double precision :: eom_cc3_23_trans_aiajaidlej   
    integer, intent(in) :: a, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, e)
term(1) = term(1) + tvvov(a, e, l, d)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajaidlej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajaidlej = eom_cc3_23_trans_aiajaidlej + term(s)
    end do

    end function eom_cc3_23_trans_aiajaidlej
    function eom_cc3_23_trans_aiajajdiem(a, d, e, m) 
    double precision :: eom_cc3_23_trans_aiajajdiem   
    integer, intent(in) :: a, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, e)
term(1) = term(1) + tvvov(a, e, m, d)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajajdiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajajdiem = eom_cc3_23_trans_aiajajdiem + term(s)
    end do

    end function eom_cc3_23_trans_aiajajdiem
    function eom_cc3_23_trans_aiajajdlei(a, d, l, e) 
    double precision :: eom_cc3_23_trans_aiajajdlei   
    integer, intent(in) :: a, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, e)
term(1) = term(1) + tvvov(a, e, l, d)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajajdlei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajajdlei = eom_cc3_23_trans_aiajajdlei + term(s)
    end do

    end function eom_cc3_23_trans_aiajajdlei
    function eom_cc3_23_trans_aiajakdiej(a, k, d, e) 
    double precision :: eom_cc3_23_trans_aiajakdiej   
    integer, intent(in) :: a, k, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, k, e)
term(1) = term(1) + tvvov(a, e, k, d)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajakdiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajakdiej = eom_cc3_23_trans_aiajakdiej + term(s)
    end do

    end function eom_cc3_23_trans_aiajakdiej
    function eom_cc3_23_trans_aiajakdjei(a, k, d, e) 
    double precision :: eom_cc3_23_trans_aiajakdjei   
    integer, intent(in) :: a, k, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, k, e)
term(1) = term(1) + tvvov(a, e, k, d)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajakdjei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajakdjei = eom_cc3_23_trans_aiajakdjei + term(s)
    end do

    end function eom_cc3_23_trans_aiajakdjei
    function eom_cc3_23_trans_aibjaialbm(a, j, l, m) 
    double precision :: eom_cc3_23_trans_aibjaialbm   
    integer, intent(in) :: a, j, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, a, m, j)
term(1) = term(1) + tovoo(m, a, l, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjaialbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaialbm = eom_cc3_23_trans_aibjaialbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjaialbm
    function eom_cc3_23_trans_aibjajalbm(a, i, l, m) 
    double precision :: eom_cc3_23_trans_aibjajalbm   
    integer, intent(in) :: a, i, l, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, a, l, i)



    eom_cc3_23_trans_aibjajalbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjajalbm = eom_cc3_23_trans_aibjajalbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjajalbm
    function eom_cc3_23_trans_aibjakaibm(a, j, k, m) 
    double precision :: eom_cc3_23_trans_aibjakaibm   
    integer, intent(in) :: a, j, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, m, j)
term(1) = term(1) + tovoo(m, a, k, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakaibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakaibm = eom_cc3_23_trans_aibjakaibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjakaibm
    function eom_cc3_23_trans_aibjakajbm(a, i, k, m) 
    double precision :: eom_cc3_23_trans_aibjakajbm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, a, k, i)



    eom_cc3_23_trans_aibjakajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakajbm = eom_cc3_23_trans_aibjakajbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjakajbm
    function eom_cc3_23_trans_aibjakalbj(a, i, k, l) 
    double precision :: eom_cc3_23_trans_aibjakalbj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, l, i)
term(1) = term(1) + tovoo(l, a, k, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aibjakalbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakalbj = eom_cc3_23_trans_aibjakalbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjakalbj
    function eom_cc3_23_trans_aibjaiajem(a, b, e, m) 
    double precision :: eom_cc3_23_trans_aibjaiajem   
    integer, intent(in) :: a, b, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, m, e)
term(1) = term(1) + tvvov(b, e, m, a)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibjaiajem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaiajem = eom_cc3_23_trans_aibjaiajem + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiajem
    function eom_cc3_23_trans_aibjaialej(a, b, l, e) 
    double precision :: eom_cc3_23_trans_aibjaialej   
    integer, intent(in) :: a, b, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, l, e)
term(1) = term(1) + tvvov(b, e, l, a)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjaialej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaialej = eom_cc3_23_trans_aibjaialej + term(s)
    end do

    end function eom_cc3_23_trans_aibjaialej
    function eom_cc3_23_trans_aibjajaiem(a, b, e, m) 
    double precision :: eom_cc3_23_trans_aibjajaiem   
    integer, intent(in) :: a, b, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, m, e)
term(1) = term(1) + tvvov(b, e, m, a)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibjajaiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajaiem = eom_cc3_23_trans_aibjajaiem + term(s)
    end do

    end function eom_cc3_23_trans_aibjajaiem
    function eom_cc3_23_trans_aibjajalei(a, b, l, e) 
    double precision :: eom_cc3_23_trans_aibjajalei   
    integer, intent(in) :: a, b, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, l, e)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjajalei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjajalei = eom_cc3_23_trans_aibjajalei + term(s)
    end do

    end function eom_cc3_23_trans_aibjajalei
    function eom_cc3_23_trans_aibjakaiej(a, b, k, e) 
    double precision :: eom_cc3_23_trans_aibjakaiej   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, k, e)
term(1) = term(1) + tvvov(b, e, k, a)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakaiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakaiej = eom_cc3_23_trans_aibjakaiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjakaiej
    function eom_cc3_23_trans_aibjakajei(a, b, k, e) 
    double precision :: eom_cc3_23_trans_aibjakajei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, k, e)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakajei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakajei = eom_cc3_23_trans_aibjakajei + term(s)
    end do

    end function eom_cc3_23_trans_aibjakajei
    function eom_cc3_23_trans_aibjaiblbm(b, j, l, m) 
    double precision :: eom_cc3_23_trans_aibjaiblbm   
    integer, intent(in) :: b, j, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, b, m, j)
term(1) = term(1) + tovoo(m, b, l, j)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aibjaiblbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaiblbm = eom_cc3_23_trans_aibjaiblbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiblbm
    function eom_cc3_23_trans_aibjakbibm(b, j, k, m) 
    double precision :: eom_cc3_23_trans_aibjakbibm   
    integer, intent(in) :: b, j, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, m, j)



    eom_cc3_23_trans_aibjakbibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakbibm = eom_cc3_23_trans_aibjakbibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbibm
    function eom_cc3_23_trans_aibjakblbi(b, j, k, l) 
    double precision :: eom_cc3_23_trans_aibjakblbi   
    integer, intent(in) :: b, j, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, l, j)



    eom_cc3_23_trans_aibjakblbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakblbi = eom_cc3_23_trans_aibjakblbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjakblbi
    function eom_cc3_23_trans_aibjakbjbm(i, b, k, m) 
    double precision :: eom_cc3_23_trans_aibjakbjbm   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, b, k, i)
term(1) = term(1) + tovoo(k, b, m, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakbjbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakbjbm = eom_cc3_23_trans_aibjakbjbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbjbm
    function eom_cc3_23_trans_aibjakblbj(i, b, k, l) 
    double precision :: eom_cc3_23_trans_aibjakblbj   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, b, k, i)
term(1) = term(1) + tovoo(k, b, l, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakblbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakblbj = eom_cc3_23_trans_aibjakblbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjakblbj
    function eom_cc3_23_trans_aibiaiblem(i, l, e, m) 
    double precision :: eom_cc3_23_trans_aibiaiblem   
    integer, intent(in) :: i, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, i)
term(1) = term(1) + tovoo(l, e, m, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibiaiblem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiaiblem = eom_cc3_23_trans_aibiaiblem + term(s)
    end do

    end function eom_cc3_23_trans_aibiaiblem
    function eom_cc3_23_trans_aibjaibiem(i, j, e, m) 
    double precision :: eom_cc3_23_trans_aibjaibiem   
    integer, intent(in) :: i, j, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, i, j)
term(1) = term(1) + tovoo(i, e, m, j)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjaibiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaibiem = eom_cc3_23_trans_aibjaibiem + term(s)
    end do

    end function eom_cc3_23_trans_aibjaibiem
    function eom_cc3_23_trans_aibjaiblei(i, j, l, e) 
    double precision :: eom_cc3_23_trans_aibjaiblei   
    integer, intent(in) :: i, j, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, l, j)
term(1) = term(1) + tovoo(l, e, i, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjaiblei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaiblei = eom_cc3_23_trans_aibjaiblei + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiblei
    function eom_cc3_23_trans_aibjaibjem(nocc, a, i, b, j, e, m) 
    double precision :: eom_cc3_23_trans_aibjaibjem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e, m 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, e, n, n)
term(1) = term(1) + tovoo(n, e, m, n)
end do 

term(0) = term(0) * 3.9999999999999996d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(m, e)
term(3) = term(3) + tovoo(m, e, i, i)
term(4) = term(4) + tovoo(m, e, j, j)
term(5) = term(5) + tovoo(i, e, m, i)
term(6) = term(6) + tovoo(j, e, m, j)
term(7) = term(7) + tvvov(a, a, m, e)
term(8) = term(8) + tvvov(b, b, m, e)
term(9) = term(9) + tvvov(b, e, m, b)
term(10) = term(10) + tvvov(a, e, m, a)

term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-1.9999999999999998d+0) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(7) = term(7) * 1.9999999999999998d+0 
term(8) = term(8) * 1.9999999999999998d+0 
term(9) = -term(9) 
term(10) = -term(10) 


    eom_cc3_23_trans_aibjaibjem = 0.d+0
    do s = 0, 10
    eom_cc3_23_trans_aibjaibjem = eom_cc3_23_trans_aibjaibjem + term(s)
    end do

    end function eom_cc3_23_trans_aibjaibjem
    function eom_cc3_23_trans_aibjaiblej(nocc, a, i, b, j, l, e) 
    double precision :: eom_cc3_23_trans_aibjaiblej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, l, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, l, n)
term(1) = term(1) + tovoo(l, e, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(l, e)
term(3) = term(3) + tovoo(l, e, i, i)
term(4) = term(4) + tovoo(j, e, l, j)
term(5) = term(5) + tovoo(l, e, j, j)
term(6) = term(6) + tvvov(a, a, l, e)
term(7) = term(7) + tvvov(b, b, l, e)
term(8) = term(8) + tvvov(b, e, l, b)

term(2) = -term(2) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(6) = -term(6) 
term(7) = -term(7) 
term(8) = term(8) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjaiblej = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjaiblej = eom_cc3_23_trans_aibjaiblej + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiblej
    function eom_cc3_23_trans_aibjaiblel(j, l, e) 
    double precision :: eom_cc3_23_trans_aibjaiblel   
    integer, intent(in) :: j, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, l, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjaiblel = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjaiblel = eom_cc3_23_trans_aibjaiblel + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiblel
    function eom_cc3_23_trans_aibjajbjem(i, j, e, m) 
    double precision :: eom_cc3_23_trans_aibjajbjem   
    integer, intent(in) :: i, j, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, j, i)
term(1) = term(1) + tovoo(j, e, m, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjajbjem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajbjem = eom_cc3_23_trans_aibjajbjem + term(s)
    end do

    end function eom_cc3_23_trans_aibjajbjem
    function eom_cc3_23_trans_aibjajblej(i, j, l, e) 
    double precision :: eom_cc3_23_trans_aibjajblej   
    integer, intent(in) :: i, j, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, j, i)



    eom_cc3_23_trans_aibjajblej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjajblej = eom_cc3_23_trans_aibjajblej + term(s)
    end do

    end function eom_cc3_23_trans_aibjajblej
    function eom_cc3_23_trans_aibjajblei(a, j, l, e) 
    double precision :: eom_cc3_23_trans_aibjajblei   
    integer, intent(in) :: a, j, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, l, j)
term(1) = term(1) + tvvov(a, e, l, a)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjajblei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajblei = eom_cc3_23_trans_aibjajblei + term(s)
    end do

    end function eom_cc3_23_trans_aibjajblei
    function eom_cc3_23_trans_aibjakbkei(j, k, e) 
    double precision :: eom_cc3_23_trans_aibjakbkei   
    integer, intent(in) :: j, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, j)



    eom_cc3_23_trans_aibjakbkei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakbkei = eom_cc3_23_trans_aibjakbkei + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbkei
    function eom_cc3_23_trans_aibjakbkej(i, k, e) 
    double precision :: eom_cc3_23_trans_aibjakbkej   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)



    eom_cc3_23_trans_aibjakbkej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakbkej = eom_cc3_23_trans_aibjakbkej + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbkej
    function eom_cc3_23_trans_aibjakbjek(i, k, e) 
    double precision :: eom_cc3_23_trans_aibjakbjek   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakbjek = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakbjek = eom_cc3_23_trans_aibjakbjek + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbjek
    function eom_cc3_23_trans_aibiakbiem(i, k, e, m) 
    double precision :: eom_cc3_23_trans_aibiakbiem   
    integer, intent(in) :: i, k, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, i)
term(1) = term(1) + tovoo(k, e, m, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibiakbiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiakbiem = eom_cc3_23_trans_aibiakbiem + term(s)
    end do

    end function eom_cc3_23_trans_aibiakbiem
    function eom_cc3_23_trans_aibiakblei(i, k, l, e) 
    double precision :: eom_cc3_23_trans_aibiakblei   
    integer, intent(in) :: i, k, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, i)
term(1) = term(1) + tovoo(k, e, l, i)



    eom_cc3_23_trans_aibiakblei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiakblei = eom_cc3_23_trans_aibiakblei + term(s)
    end do

    end function eom_cc3_23_trans_aibiakblei
    function eom_cc3_23_trans_aibjakbiei(i, j, k, e) 
    double precision :: eom_cc3_23_trans_aibjakbiei   
    integer, intent(in) :: i, j, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, i, j)



    eom_cc3_23_trans_aibjakbiei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakbiei = eom_cc3_23_trans_aibjakbiei + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbiei
    function eom_cc3_23_trans_aibjakbiej(i, b, k, e) 
    double precision :: eom_cc3_23_trans_aibjakbiej   
    integer, intent(in) :: i, b, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, k, i)
term(1) = term(1) + tvvov(b, e, k, b)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjakbiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakbiej = eom_cc3_23_trans_aibjakbiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbiej
    function eom_cc3_23_trans_aibjakbjei(nocc, a, i, b, j, k, e) 
    double precision :: eom_cc3_23_trans_aibjakbjei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, k, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, k, n)
term(1) = term(1) + tovoo(k, e, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(k, e)
term(3) = term(3) + tovoo(i, e, k, i)
term(4) = term(4) + tovoo(k, e, j, j)
term(5) = term(5) + tovoo(k, e, i, i)
term(6) = term(6) + tvvov(a, a, k, e)
term(7) = term(7) + tvvov(a, e, k, a)
term(8) = term(8) + tvvov(b, b, k, e)

term(2) = -term(2) 
term(3) = term(3) * (-1.9999999999999998d+0) 
term(6) = -term(6) 
term(7) = term(7) * 1.9999999999999998d+0 
term(8) = -term(8) 


    eom_cc3_23_trans_aibjakbjei = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjakbjei = eom_cc3_23_trans_aibjakbjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbjei
    function eom_cc3_23_trans_aibjakbjej(i, j, k, e) 
    double precision :: eom_cc3_23_trans_aibjakbjej   
    integer, intent(in) :: i, j, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, k, i)
term(1) = term(1) + tovoo(k, e, j, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakbjej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakbjej = eom_cc3_23_trans_aibjakbjej + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbjej
    function eom_cc3_23_trans_aibiaidlbm(i, d, l, m) 
    double precision :: eom_cc3_23_trans_aibiaidlbm   
    integer, intent(in) :: i, d, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, m, i)
term(1) = term(1) + tovoo(m, d, l, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibiaidlbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiaidlbm = eom_cc3_23_trans_aibiaidlbm + term(s)
    end do

    end function eom_cc3_23_trans_aibiaidlbm
    function eom_cc3_23_trans_aibjaidibm(i, j, d, m) 
    double precision :: eom_cc3_23_trans_aibjaidibm   
    integer, intent(in) :: i, j, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, m, j)
term(1) = term(1) + tovoo(m, d, i, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjaidibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaidibm = eom_cc3_23_trans_aibjaidibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidibm
    function eom_cc3_23_trans_aibjaidlbi(i, j, d, l) 
    double precision :: eom_cc3_23_trans_aibjaidlbi   
    integer, intent(in) :: i, j, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, i, j)
term(1) = term(1) + tovoo(i, d, l, j)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjaidlbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaidlbi = eom_cc3_23_trans_aibjaidlbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidlbi
    function eom_cc3_23_trans_aibjaidjbm(nocc, a, i, b, j, d, m) 
    double precision :: eom_cc3_23_trans_aibjaidjbm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, d, n, n)
term(1) = term(1) + tovoo(n, d, m, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(m, d)
term(3) = term(3) + tovoo(m, d, i, i)
term(4) = term(4) + tovoo(j, d, m, j)
term(5) = term(5) + tovoo(m, d, j, j)
term(6) = term(6) + tvvov(a, a, m, d)
term(7) = term(7) + tvvov(b, d, m, b)
term(8) = term(8) + tvvov(b, b, m, d)

term(2) = -term(2) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(6) = -term(6) 
term(7) = term(7) * 1.9999999999999998d+0 
term(8) = -term(8) 


    eom_cc3_23_trans_aibjaidjbm = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjaidjbm = eom_cc3_23_trans_aibjaidjbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidjbm
    function eom_cc3_23_trans_aibjaidlbj(nocc, a, i, b, j, d, l) 
    double precision :: eom_cc3_23_trans_aibjaidlbj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(l, d, n, n)
term(1) = term(1) + tovoo(n, d, l, n)
end do 

term(0) = term(0) * 3.9999999999999996d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(l, d)
term(3) = term(3) + tovoo(l, d, i, i)
term(4) = term(4) + tovoo(i, d, l, i)
term(5) = term(5) + tovoo(l, d, j, j)
term(6) = term(6) + tovoo(j, d, l, j)
term(7) = term(7) + tvvov(a, a, l, d)
term(8) = term(8) + tvvov(b, d, l, b)
term(9) = term(9) + tvvov(a, d, l, a)
term(10) = term(10) + tvvov(b, b, l, d)

term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-1.9999999999999998d+0) 
term(5) = term(5) * (-1.9999999999999998d+0) 
term(7) = term(7) * 1.9999999999999998d+0 
term(8) = -term(8) 
term(9) = -term(9) 
term(10) = term(10) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjaidlbj = 0.d+0
    do s = 0, 10
    eom_cc3_23_trans_aibjaidlbj = eom_cc3_23_trans_aibjaidlbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidlbj
    function eom_cc3_23_trans_aibjaidlbl(j, d, l) 
    double precision :: eom_cc3_23_trans_aibjaidlbl   
    integer, intent(in) :: j, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, l, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjaidlbl = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjaidlbl = eom_cc3_23_trans_aibjaidlbl + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidlbl
    function eom_cc3_23_trans_aibjajdjbm(i, j, d, m) 
    double precision :: eom_cc3_23_trans_aibjajdjbm   
    integer, intent(in) :: i, j, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, j, i)



    eom_cc3_23_trans_aibjajdjbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjajdjbm = eom_cc3_23_trans_aibjajdjbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjajdjbm
    function eom_cc3_23_trans_aibjajdlbj(i, j, d, l) 
    double precision :: eom_cc3_23_trans_aibjajdlbj   
    integer, intent(in) :: i, j, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, j, i)
term(1) = term(1) + tovoo(j, d, l, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjajdlbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajdlbj = eom_cc3_23_trans_aibjajdlbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjajdlbj
    function eom_cc3_23_trans_aibjajdibm(a, j, d, m) 
    double precision :: eom_cc3_23_trans_aibjajdibm   
    integer, intent(in) :: a, j, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, m, j)
term(1) = term(1) + tvvov(a, d, m, a)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjajdibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajdibm = eom_cc3_23_trans_aibjajdibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjajdibm
    function eom_cc3_23_trans_aibjakdkbj(i, k, d) 
    double precision :: eom_cc3_23_trans_aibjakdkbj   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakdkbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakdkbj = eom_cc3_23_trans_aibjakdkbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdkbj
    function eom_cc3_23_trans_aibjakdibk(j, k, d) 
    double precision :: eom_cc3_23_trans_aibjakdibk   
    integer, intent(in) :: j, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, j)



    eom_cc3_23_trans_aibjakdibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakdibk = eom_cc3_23_trans_aibjakdibk + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdibk
    function eom_cc3_23_trans_aibjakdjbk(i, k, d) 
    double precision :: eom_cc3_23_trans_aibjakdjbk   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, i)



    eom_cc3_23_trans_aibjakdjbk = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakdjbk = eom_cc3_23_trans_aibjakdjbk + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdjbk
    function eom_cc3_23_trans_aibiakdibm(i, k, d, m) 
    double precision :: eom_cc3_23_trans_aibiakdibm   
    integer, intent(in) :: i, k, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, k, i)
term(1) = term(1) + tovoo(k, d, m, i)



    eom_cc3_23_trans_aibiakdibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiakdibm = eom_cc3_23_trans_aibiakdibm + term(s)
    end do

    end function eom_cc3_23_trans_aibiakdibm
    function eom_cc3_23_trans_aibiakdlbi(i, k, d, l) 
    double precision :: eom_cc3_23_trans_aibiakdlbi   
    integer, intent(in) :: i, k, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, k, i)
term(1) = term(1) + tovoo(k, d, l, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibiakdlbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiakdlbi = eom_cc3_23_trans_aibiakdlbi + term(s)
    end do

    end function eom_cc3_23_trans_aibiakdlbi
    function eom_cc3_23_trans_aibjakdibi(i, j, k, d) 
    double precision :: eom_cc3_23_trans_aibjakdibi   
    integer, intent(in) :: i, j, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, i, j)



    eom_cc3_23_trans_aibjakdibi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakdibi = eom_cc3_23_trans_aibjakdibi + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdibi
    function eom_cc3_23_trans_aibjakdibj(nocc, a, i, b, j, k, d) 
    double precision :: eom_cc3_23_trans_aibjakdibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, d, k, n)
term(1) = term(1) + tovoo(k, d, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(k, d)
term(3) = term(3) + tovoo(i, d, k, i)
term(4) = term(4) + tovoo(k, d, i, i)
term(5) = term(5) + tovoo(k, d, j, j)
term(6) = term(6) + tvvov(a, a, k, d)
term(7) = term(7) + tvvov(a, d, k, a)
term(8) = term(8) + tvvov(b, b, k, d)

term(2) = -term(2) 
term(3) = term(3) * (-1.9999999999999998d+0) 
term(6) = -term(6) 
term(7) = term(7) * 1.9999999999999998d+0 
term(8) = -term(8) 


    eom_cc3_23_trans_aibjakdibj = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjakdibj = eom_cc3_23_trans_aibjakdibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdibj
    function eom_cc3_23_trans_aibjakdjbi(i, b, k, d) 
    double precision :: eom_cc3_23_trans_aibjakdjbi   
    integer, intent(in) :: i, b, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, k, i)
term(1) = term(1) + tvvov(b, d, k, b)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjakdjbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakdjbi = eom_cc3_23_trans_aibjakdjbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdjbi
    function eom_cc3_23_trans_aibjakdjbj(i, j, k, d) 
    double precision :: eom_cc3_23_trans_aibjakdjbj   
    integer, intent(in) :: i, j, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, k, i)
term(1) = term(1) + tovoo(k, d, j, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakdjbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakdjbj = eom_cc3_23_trans_aibjakdjbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdjbj
    function eom_cc3_23_trans_aibjaidjdm(b, d, m) 
    double precision :: eom_cc3_23_trans_aibjaidjdm   
    integer, intent(in) :: b, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, m, d)



    eom_cc3_23_trans_aibjaidjdm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjaidjdm = eom_cc3_23_trans_aibjaidjdm + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidjdm
    function eom_cc3_23_trans_aibjaidldj(b, d, l) 
    double precision :: eom_cc3_23_trans_aibjaidldj   
    integer, intent(in) :: b, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, l, d)



    eom_cc3_23_trans_aibjaidldj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjaidldj = eom_cc3_23_trans_aibjaidldj + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidldj
    function eom_cc3_23_trans_aibjakdidj(b, k, d) 
    double precision :: eom_cc3_23_trans_aibjakdidj   
    integer, intent(in) :: b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, k, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakdidj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakdidj = eom_cc3_23_trans_aibjakdidj + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdidj
    function eom_cc3_23_trans_aibjakdjdi(b, k, d) 
    double precision :: eom_cc3_23_trans_aibjakdjdi   
    integer, intent(in) :: b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, k, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakdjdi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakdjdi = eom_cc3_23_trans_aibjakdjdi + term(s)
    end do

    end function eom_cc3_23_trans_aibjakdjdi
    function eom_cc3_23_trans_aibiaidiem(b, d, e, m) 
    double precision :: eom_cc3_23_trans_aibiaidiem   
    integer, intent(in) :: b, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, m, e)
term(1) = term(1) + tvvov(b, e, m, d)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibiaidiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiaidiem = eom_cc3_23_trans_aibiaidiem + term(s)
    end do

    end function eom_cc3_23_trans_aibiaidiem
    function eom_cc3_23_trans_aibiaidlei(b, d, l, e) 
    double precision :: eom_cc3_23_trans_aibiaidlei   
    integer, intent(in) :: b, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, l, e)
term(1) = term(1) + tvvov(b, e, l, d)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibiaidlei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiaidlei = eom_cc3_23_trans_aibiaidlei + term(s)
    end do

    end function eom_cc3_23_trans_aibiaidlei
    function eom_cc3_23_trans_aibjaidiej(i, b, d, e) 
    double precision :: eom_cc3_23_trans_aibjaidiej   
    integer, intent(in) :: i, b, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, i, e)
term(1) = term(1) + tvvov(b, e, i, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjaidiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaidiej = eom_cc3_23_trans_aibjaidiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidiej
    function eom_cc3_23_trans_aibjaidjei(i, b, d, e) 
    double precision :: eom_cc3_23_trans_aibjaidjei   
    integer, intent(in) :: i, b, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, i, e)
term(1) = term(1) + tvvov(b, e, i, d)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjaidjei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaidjei = eom_cc3_23_trans_aibjaidjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidjei
    function eom_cc3_23_trans_aibjaidjej(b, j, d, e) 
    double precision :: eom_cc3_23_trans_aibjaidjej   
    integer, intent(in) :: b, j, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, j, e)
term(1) = term(1) + tvvov(b, e, j, d)



    eom_cc3_23_trans_aibjaidjej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaidjej = eom_cc3_23_trans_aibjaidjej + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidjej
    function eom_cc3_23_trans_aibjajdjei(b, j, d, e) 
    double precision :: eom_cc3_23_trans_aibjajdjei   
    integer, intent(in) :: b, j, d, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, j, e)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjajdjei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjajdjei = eom_cc3_23_trans_aibjajdjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjajdjei
    function eom_cc3_23_trans_aibjajdiej(b, j, d, e) 
    double precision :: eom_cc3_23_trans_aibjajdiej   
    integer, intent(in) :: b, j, d, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, e, j, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjajdiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjajdiej = eom_cc3_23_trans_aibjajdiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjajdiej
    function eom_cc3_23_trans_aibiakdiei(b, k, d, e) 
    double precision :: eom_cc3_23_trans_aibiakdiei   
    integer, intent(in) :: b, k, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, k, e)
term(1) = term(1) + tvvov(b, e, k, d)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aibiakdiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiakdiei = eom_cc3_23_trans_aibiakdiei + term(s)
    end do

    end function eom_cc3_23_trans_aibiakdiei
    function eom_cc3_23_trans_aibjbibjem(a, b, e, m) 
    double precision :: eom_cc3_23_trans_aibjbibjem   
    integer, intent(in) :: a, b, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, m, e)
term(1) = term(1) + tvvov(a, e, m, b)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibjbibjem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjbibjem = eom_cc3_23_trans_aibjbibjem + term(s)
    end do

    end function eom_cc3_23_trans_aibjbibjem
    function eom_cc3_23_trans_aibjbiblej(a, b, l, e) 
    double precision :: eom_cc3_23_trans_aibjbiblej   
    integer, intent(in) :: a, b, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, l, e)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjbiblej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbiblej = eom_cc3_23_trans_aibjbiblej + term(s)
    end do

    end function eom_cc3_23_trans_aibjbiblej
    function eom_cc3_23_trans_aibjbjbiem(a, b, e, m) 
    double precision :: eom_cc3_23_trans_aibjbjbiem   
    integer, intent(in) :: a, b, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, m, e)
term(1) = term(1) + tvvov(a, e, m, b)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibjbjbiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjbjbiem = eom_cc3_23_trans_aibjbjbiem + term(s)
    end do

    end function eom_cc3_23_trans_aibjbjbiem
    function eom_cc3_23_trans_aibjbjblei(a, b, l, e) 
    double precision :: eom_cc3_23_trans_aibjbjblei   
    integer, intent(in) :: a, b, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, l, e)
term(1) = term(1) + tvvov(a, e, l, b)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjbjblei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjbjblei = eom_cc3_23_trans_aibjbjblei + term(s)
    end do

    end function eom_cc3_23_trans_aibjbjblei
    function eom_cc3_23_trans_aibjbkbiej(a, b, k, e) 
    double precision :: eom_cc3_23_trans_aibjbkbiej   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, k, e)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjbkbiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbkbiej = eom_cc3_23_trans_aibjbkbiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjbkbiej
    function eom_cc3_23_trans_aibjbkbjei(a, b, k, e) 
    double precision :: eom_cc3_23_trans_aibjbkbjei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, k, e)
term(1) = term(1) + tvvov(a, e, k, b)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjbkbjei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjbkbjei = eom_cc3_23_trans_aibjbkbjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjbkbjei
    function eom_cc3_23_trans_aibjbjdidm(a, d, m) 
    double precision :: eom_cc3_23_trans_aibjbjdidm   
    integer, intent(in) :: a, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, d)



    eom_cc3_23_trans_aibjbjdidm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbjdidm = eom_cc3_23_trans_aibjbjdidm + term(s)
    end do

    end function eom_cc3_23_trans_aibjbjdidm
    function eom_cc3_23_trans_aibjbjdldi(a, d, l) 
    double precision :: eom_cc3_23_trans_aibjbjdldi   
    integer, intent(in) :: a, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, d)



    eom_cc3_23_trans_aibjbjdldi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbjdldi = eom_cc3_23_trans_aibjbjdldi + term(s)
    end do

    end function eom_cc3_23_trans_aibjbjdldi
    function eom_cc3_23_trans_aibjbkdidj(a, k, d) 
    double precision :: eom_cc3_23_trans_aibjbkdidj   
    integer, intent(in) :: a, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, k, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjbkdidj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbkdidj = eom_cc3_23_trans_aibjbkdidj + term(s)
    end do

    end function eom_cc3_23_trans_aibjbkdidj
    function eom_cc3_23_trans_aibjbkdjdi(a, k, d) 
    double precision :: eom_cc3_23_trans_aibjbkdjdi   
    integer, intent(in) :: a, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, k, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjbkdjdi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbkdjdi = eom_cc3_23_trans_aibjbkdjdi + term(s)
    end do

    end function eom_cc3_23_trans_aibjbkdjdi
    function eom_cc3_23_trans_aibibidiem(a, d, e, m) 
    double precision :: eom_cc3_23_trans_aibibidiem   
    integer, intent(in) :: a, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, e)
term(1) = term(1) + tvvov(a, e, m, d)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibibidiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibibidiem = eom_cc3_23_trans_aibibidiem + term(s)
    end do

    end function eom_cc3_23_trans_aibibidiem
    function eom_cc3_23_trans_aibibidlei(a, d, l, e) 
    double precision :: eom_cc3_23_trans_aibibidlei   
    integer, intent(in) :: a, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, e)
term(1) = term(1) + tvvov(a, e, l, d)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibibidlei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibibidlei = eom_cc3_23_trans_aibibidlei + term(s)
    end do

    end function eom_cc3_23_trans_aibibidlei
    function eom_cc3_23_trans_aibjbidiej(a, i, d, e) 
    double precision :: eom_cc3_23_trans_aibjbidiej   
    integer, intent(in) :: a, i, d, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, e)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjbidiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbidiej = eom_cc3_23_trans_aibjbidiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjbidiej
    function eom_cc3_23_trans_aibjbidjei(a, i, d, e) 
    double precision :: eom_cc3_23_trans_aibjbidjei   
    integer, intent(in) :: a, i, d, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, i, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjbidjei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbidjei = eom_cc3_23_trans_aibjbidjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjbidjei
    function eom_cc3_23_trans_aibjbjdjei(a, j, d, e) 
    double precision :: eom_cc3_23_trans_aibjbjdjei   
    integer, intent(in) :: a, j, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, e)
term(1) = term(1) + tvvov(a, e, j, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjbjdjei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjbjdjei = eom_cc3_23_trans_aibjbjdjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjbjdjei
    function eom_cc3_23_trans_aibjbjdiej(a, j, d, e) 
    double precision :: eom_cc3_23_trans_aibjbjdiej   
    integer, intent(in) :: a, j, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, e)
term(1) = term(1) + tvvov(a, e, j, d)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjbjdiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjbjdiej = eom_cc3_23_trans_aibjbjdiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjbjdiej
    function eom_cc3_23_trans_aibjbjdiei(a, i, d, e) 
    double precision :: eom_cc3_23_trans_aibjbjdiei   
    integer, intent(in) :: a, i, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, e)
term(1) = term(1) + tvvov(a, e, i, d)



    eom_cc3_23_trans_aibjbjdiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjbjdiei = eom_cc3_23_trans_aibjbjdiei + term(s)
    end do

    end function eom_cc3_23_trans_aibjbjdiei
    function eom_cc3_23_trans_aibibkdiei(a, k, d, e) 
    double precision :: eom_cc3_23_trans_aibibkdiei   
    integer, intent(in) :: a, k, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, k, e)
term(1) = term(1) + tvvov(a, e, k, d)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aibibkdiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibibkdiei = eom_cc3_23_trans_aibibkdiei + term(s)
    end do

    end function eom_cc3_23_trans_aibibkdiei
    function eom_cc3_23_trans_aibjcicjam(b, c, m) 
    double precision :: eom_cc3_23_trans_aibjcicjam   
    integer, intent(in) :: b, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcicjam = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcicjam = eom_cc3_23_trans_aibjcicjam + term(s)
    end do

    end function eom_cc3_23_trans_aibjcicjam
    function eom_cc3_23_trans_aibjcjciam(b, c, m) 
    double precision :: eom_cc3_23_trans_aibjcjciam   
    integer, intent(in) :: b, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjciam = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjciam = eom_cc3_23_trans_aibjcjciam + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjciam
    function eom_cc3_23_trans_aibjcjclai(b, c, l) 
    double precision :: eom_cc3_23_trans_aibjcjclai   
    integer, intent(in) :: b, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, c)



    eom_cc3_23_trans_aibjcjclai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjclai = eom_cc3_23_trans_aibjcjclai + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjclai
    function eom_cc3_23_trans_aibjckcjai(b, c, k) 
    double precision :: eom_cc3_23_trans_aibjckcjai   
    integer, intent(in) :: b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, k, c)



    eom_cc3_23_trans_aibjckcjai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjckcjai = eom_cc3_23_trans_aibjckcjai + term(s)
    end do

    end function eom_cc3_23_trans_aibjckcjai
    function eom_cc3_23_trans_aibjcicjbm(a, c, m) 
    double precision :: eom_cc3_23_trans_aibjcicjbm   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcicjbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcicjbm = eom_cc3_23_trans_aibjcicjbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjcicjbm
    function eom_cc3_23_trans_aibjciclbj(a, c, l) 
    double precision :: eom_cc3_23_trans_aibjciclbj   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, c)



    eom_cc3_23_trans_aibjciclbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjciclbj = eom_cc3_23_trans_aibjciclbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjciclbj
    function eom_cc3_23_trans_aibjcjcibm(a, c, m) 
    double precision :: eom_cc3_23_trans_aibjcjcibm   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjcibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjcibm = eom_cc3_23_trans_aibjcjcibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjcibm
    function eom_cc3_23_trans_aibjckcibj(a, c, k) 
    double precision :: eom_cc3_23_trans_aibjckcibj   
    integer, intent(in) :: a, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, c)



    eom_cc3_23_trans_aibjckcibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjckcibj = eom_cc3_23_trans_aibjckcibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjckcibj
    function eom_cc3_23_trans_aiajcialam(j, c, l, m) 
    double precision :: eom_cc3_23_trans_aiajcialam   
    integer, intent(in) :: j, c, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, m, j)
term(1) = term(1) + tovoo(m, c, l, j)



    eom_cc3_23_trans_aiajcialam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcialam = eom_cc3_23_trans_aiajcialam + term(s)
    end do

    end function eom_cc3_23_trans_aiajcialam
    function eom_cc3_23_trans_aiajcjalam(i, c, l, m) 
    double precision :: eom_cc3_23_trans_aiajcjalam   
    integer, intent(in) :: i, c, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, m, i)
term(1) = term(1) + tovoo(m, c, l, i)



    eom_cc3_23_trans_aiajcjalam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcjalam = eom_cc3_23_trans_aiajcjalam + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjalam
    function eom_cc3_23_trans_aiajckaiam(j, c, k, m) 
    double precision :: eom_cc3_23_trans_aiajckaiam   
    integer, intent(in) :: j, c, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, m, j)
term(1) = term(1) + tovoo(m, c, k, j)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajckaiam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajckaiam = eom_cc3_23_trans_aiajckaiam + term(s)
    end do

    end function eom_cc3_23_trans_aiajckaiam
    function eom_cc3_23_trans_aiajckalai(j, c, k, l) 
    double precision :: eom_cc3_23_trans_aiajckalai   
    integer, intent(in) :: j, c, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, l, j)
term(1) = term(1) + tovoo(l, c, k, j)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajckalai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajckalai = eom_cc3_23_trans_aiajckalai + term(s)
    end do

    end function eom_cc3_23_trans_aiajckalai
    function eom_cc3_23_trans_aiajckajam(i, c, k, m) 
    double precision :: eom_cc3_23_trans_aiajckajam   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, m, i)
term(1) = term(1) + tovoo(m, c, k, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajckajam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajckajam = eom_cc3_23_trans_aiajckajam + term(s)
    end do

    end function eom_cc3_23_trans_aiajckajam
    function eom_cc3_23_trans_aiajckalaj(i, c, k, l) 
    double precision :: eom_cc3_23_trans_aiajckalaj   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, l, i)
term(1) = term(1) + tovoo(l, c, k, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajckalaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajckalaj = eom_cc3_23_trans_aiajckalaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajckalaj
    function eom_cc3_23_trans_aiajciajem(a, c, e, m) 
    double precision :: eom_cc3_23_trans_aiajciajem   
    integer, intent(in) :: a, c, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, e)
term(1) = term(1) + tvvov(a, e, m, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajciajem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajciajem = eom_cc3_23_trans_aiajciajem + term(s)
    end do

    end function eom_cc3_23_trans_aiajciajem
    function eom_cc3_23_trans_aiajcialej(a, c, l, e) 
    double precision :: eom_cc3_23_trans_aiajcialej   
    integer, intent(in) :: a, c, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, e)
term(1) = term(1) + tvvov(a, e, l, c)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajcialej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcialej = eom_cc3_23_trans_aiajcialej + term(s)
    end do

    end function eom_cc3_23_trans_aiajcialej
    function eom_cc3_23_trans_aiajcjaiem(a, c, e, m) 
    double precision :: eom_cc3_23_trans_aiajcjaiem   
    integer, intent(in) :: a, c, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, e)
term(1) = term(1) + tvvov(a, e, m, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajcjaiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcjaiem = eom_cc3_23_trans_aiajcjaiem + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjaiem
    function eom_cc3_23_trans_aiajcjalei(a, c, l, e) 
    double precision :: eom_cc3_23_trans_aiajcjalei   
    integer, intent(in) :: a, c, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, e)
term(1) = term(1) + tvvov(a, e, l, c)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajcjalei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcjalei = eom_cc3_23_trans_aiajcjalei + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjalei
    function eom_cc3_23_trans_aiajckaiej(a, c, k, e) 
    double precision :: eom_cc3_23_trans_aiajckaiej   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, e)
term(1) = term(1) + tvvov(a, e, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajckaiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajckaiej = eom_cc3_23_trans_aiajckaiej + term(s)
    end do

    end function eom_cc3_23_trans_aiajckaiej
    function eom_cc3_23_trans_aiajckajei(a, c, k, e) 
    double precision :: eom_cc3_23_trans_aiajckajei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, e)
term(1) = term(1) + tvvov(a, e, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajckajei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajckajei = eom_cc3_23_trans_aiajckajei + term(s)
    end do

    end function eom_cc3_23_trans_aiajckajei
    function eom_cc3_23_trans_aiajcidjam(a, c, d, m) 
    double precision :: eom_cc3_23_trans_aiajcidjam   
    integer, intent(in) :: a, c, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, d)
term(1) = term(1) + tvvov(a, d, m, c)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajcidjam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcidjam = eom_cc3_23_trans_aiajcidjam + term(s)
    end do

    end function eom_cc3_23_trans_aiajcidjam
    function eom_cc3_23_trans_aiajcidlaj(a, c, d, l) 
    double precision :: eom_cc3_23_trans_aiajcidlaj   
    integer, intent(in) :: a, c, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, d)
term(1) = term(1) + tvvov(a, d, l, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajcidlaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcidlaj = eom_cc3_23_trans_aiajcidlaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajcidlaj
    function eom_cc3_23_trans_aiajcjdiam(a, c, d, m) 
    double precision :: eom_cc3_23_trans_aiajcjdiam   
    integer, intent(in) :: a, c, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, d)
term(1) = term(1) + tvvov(a, d, m, c)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajcjdiam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcjdiam = eom_cc3_23_trans_aiajcjdiam + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjdiam
    function eom_cc3_23_trans_aiajcjdlai(a, c, d, l) 
    double precision :: eom_cc3_23_trans_aiajcjdlai   
    integer, intent(in) :: a, c, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, d)
term(1) = term(1) + tvvov(a, d, l, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajcjdlai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcjdlai = eom_cc3_23_trans_aiajcjdlai + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjdlai
    function eom_cc3_23_trans_aiajckdiaj(a, c, k, d) 
    double precision :: eom_cc3_23_trans_aiajckdiaj   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, d)
term(1) = term(1) + tvvov(a, d, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajckdiaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajckdiaj = eom_cc3_23_trans_aiajckdiaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajckdiaj
    function eom_cc3_23_trans_aiajckdjai(a, c, k, d) 
    double precision :: eom_cc3_23_trans_aiajckdjai   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, d)
term(1) = term(1) + tvvov(a, d, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajckdjai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajckdjai = eom_cc3_23_trans_aiajckdjai + term(s)
    end do

    end function eom_cc3_23_trans_aiajckdjai
    function eom_cc3_23_trans_aibjciajam(a, b, c, m) 
    double precision :: eom_cc3_23_trans_aibjciajam   
    integer, intent(in) :: a, b, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, m, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjciajam = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjciajam = eom_cc3_23_trans_aibjciajam + term(s)
    end do

    end function eom_cc3_23_trans_aibjciajam
    function eom_cc3_23_trans_aibjcialaj(a, b, c, l) 
    double precision :: eom_cc3_23_trans_aibjcialaj   
    integer, intent(in) :: a, b, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, l, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcialaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcialaj = eom_cc3_23_trans_aibjcialaj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcialaj
    function eom_cc3_23_trans_aibjcjaiam(a, b, c, m) 
    double precision :: eom_cc3_23_trans_aibjcjaiam   
    integer, intent(in) :: a, b, c, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, a)
term(1) = term(1) + tvvov(b, a, m, c)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjcjaiam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjaiam = eom_cc3_23_trans_aibjcjaiam + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjaiam
    function eom_cc3_23_trans_aibjcjalai(a, b, c, l) 
    double precision :: eom_cc3_23_trans_aibjcjalai   
    integer, intent(in) :: a, b, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, a)
term(1) = term(1) + tvvov(b, a, l, c)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjcjalai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjalai = eom_cc3_23_trans_aibjcjalai + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjalai
    function eom_cc3_23_trans_aibjckaiaj(a, b, c, k) 
    double precision :: eom_cc3_23_trans_aibjckaiaj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, k, a)
term(1) = term(1) + tvvov(b, a, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjckaiaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjckaiaj = eom_cc3_23_trans_aibjckaiaj + term(s)
    end do

    end function eom_cc3_23_trans_aibjckaiaj
    function eom_cc3_23_trans_aibjckajai(a, b, c, k) 
    double precision :: eom_cc3_23_trans_aibjckajai   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, k, a)
term(1) = term(1) + tvvov(b, a, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjckajai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjckajai = eom_cc3_23_trans_aibjckajai + term(s)
    end do

    end function eom_cc3_23_trans_aibjckajai
    function eom_cc3_23_trans_aibicialbm(i, c, l, m) 
    double precision :: eom_cc3_23_trans_aibicialbm   
    integer, intent(in) :: i, c, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, m, i)
term(1) = term(1) + tovoo(m, c, l, i)



    eom_cc3_23_trans_aibicialbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibicialbm = eom_cc3_23_trans_aibicialbm + term(s)
    end do

    end function eom_cc3_23_trans_aibicialbm
    function eom_cc3_23_trans_aibjciaibm(i, j, c, m) 
    double precision :: eom_cc3_23_trans_aibjciaibm   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, c, m, j)
term(1) = term(1) + tovoo(m, c, i, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjciaibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjciaibm = eom_cc3_23_trans_aibjciaibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjciaibm
    function eom_cc3_23_trans_aibjcialbi(i, j, c, l) 
    double precision :: eom_cc3_23_trans_aibjcialbi   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, i, j)



    eom_cc3_23_trans_aibjcialbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcialbi = eom_cc3_23_trans_aibjcialbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjcialbi
    function eom_cc3_23_trans_aibjciajbm(a, j, c, m) 
    double precision :: eom_cc3_23_trans_aibjciajbm   
    integer, intent(in) :: a, j, c, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, c, m, j)
term(1) = term(1) + tvvov(a, c, m, a)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjciajbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjciajbm = eom_cc3_23_trans_aibjciajbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjciajbm
    function eom_cc3_23_trans_aibjcialbj(nocc, a, i, b, j, c, l) 
    double precision :: eom_cc3_23_trans_aibjcialbj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(l, c, n, n)
term(1) = term(1) + tovoo(n, c, l, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(l, c)
term(3) = term(3) + tvvov(a, c, l, a)
term(4) = term(4) + tvvov(a, a, l, c)
term(5) = term(5) + tvvov(b, b, l, c)
term(6) = term(6) + tovoo(i, c, l, i)
term(7) = term(7) + tovoo(l, c, i, i)
term(8) = term(8) + tovoo(l, c, j, j)

term(2) = -term(2) 
term(3) = term(3) * 1.9999999999999998d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = term(6) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjcialbj = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjcialbj = eom_cc3_23_trans_aibjcialbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcialbj
    function eom_cc3_23_trans_aibjcialbl(j, c, l) 
    double precision :: eom_cc3_23_trans_aibjcialbl   
    integer, intent(in) :: j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, l, j)



    eom_cc3_23_trans_aibjcialbl = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcialbl = eom_cc3_23_trans_aibjcialbl + term(s)
    end do

    end function eom_cc3_23_trans_aibjcialbl
    function eom_cc3_23_trans_aibjcjajbm(i, j, c, m) 
    double precision :: eom_cc3_23_trans_aibjcjajbm   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, c, j, i)



    eom_cc3_23_trans_aibjcjajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjajbm = eom_cc3_23_trans_aibjcjajbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjajbm
    function eom_cc3_23_trans_aibjcjalbj(i, j, c, l) 
    double precision :: eom_cc3_23_trans_aibjcjalbj   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, c, l, i)
term(1) = term(1) + tovoo(l, c, j, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjalbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjalbj = eom_cc3_23_trans_aibjcjalbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjalbj
    function eom_cc3_23_trans_aibjcjaibm(nocc, a, i, b, j, c, m) 
    double precision :: eom_cc3_23_trans_aibjcjaibm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, c, n, n)
term(1) = term(1) + tovoo(n, c, m, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(m, c)
term(3) = term(3) + tvvov(b, c, m, b)
term(4) = term(4) + tvvov(a, a, m, c)
term(5) = term(5) + tvvov(b, b, m, c)
term(6) = term(6) + tovoo(j, c, m, j)
term(7) = term(7) + tovoo(m, c, j, j)
term(8) = term(8) + tovoo(m, c, i, i)

term(2) = -term(2) 
term(3) = term(3) * 1.9999999999999998d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = term(6) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjcjaibm = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjcjaibm = eom_cc3_23_trans_aibjcjaibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjaibm
    function eom_cc3_23_trans_aibjcjalbi(i, b, c, l) 
    double precision :: eom_cc3_23_trans_aibjcjalbi   
    integer, intent(in) :: i, b, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, c, l, i)
term(1) = term(1) + tvvov(b, c, l, b)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjcjalbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjalbi = eom_cc3_23_trans_aibjcjalbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjalbi
    function eom_cc3_23_trans_aibjcjalbl(i, c, l) 
    double precision :: eom_cc3_23_trans_aibjcjalbl   
    integer, intent(in) :: i, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, l, i)



    eom_cc3_23_trans_aibjcjalbl = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjalbl = eom_cc3_23_trans_aibjcjalbl + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjalbl
    function eom_cc3_23_trans_aibjckakbj(i, c, k) 
    double precision :: eom_cc3_23_trans_aibjckakbj   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjckakbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjckakbj = eom_cc3_23_trans_aibjckakbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjckakbj
    function eom_cc3_23_trans_aibjckaibk(j, c, k) 
    double precision :: eom_cc3_23_trans_aibjckaibk   
    integer, intent(in) :: j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjckaibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjckaibk = eom_cc3_23_trans_aibjckaibk + term(s)
    end do

    end function eom_cc3_23_trans_aibjckaibk
    function eom_cc3_23_trans_aibickaibm(i, c, k, m) 
    double precision :: eom_cc3_23_trans_aibickaibm   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, m, i)
term(1) = term(1) + tovoo(m, c, k, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibickaibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibickaibm = eom_cc3_23_trans_aibickaibm + term(s)
    end do

    end function eom_cc3_23_trans_aibickaibm
    function eom_cc3_23_trans_aibickalbi(i, c, k, l) 
    double precision :: eom_cc3_23_trans_aibickalbi   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, l, i)
term(1) = term(1) + tovoo(l, c, k, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibickalbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibickalbi = eom_cc3_23_trans_aibickalbi + term(s)
    end do

    end function eom_cc3_23_trans_aibickalbi
    function eom_cc3_23_trans_aibjckaibi(i, j, c, k) 
    double precision :: eom_cc3_23_trans_aibjckaibi   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, i, j)
term(1) = term(1) + tovoo(i, c, k, j)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjckaibi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjckaibi = eom_cc3_23_trans_aibjckaibi + term(s)
    end do

    end function eom_cc3_23_trans_aibjckaibi
    function eom_cc3_23_trans_aibjckaibj(nocc, a, i, b, j, c, k) 
    double precision :: eom_cc3_23_trans_aibjckaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(k, c, n, n)
term(1) = term(1) + tovoo(n, c, k, n)
end do 

term(0) = term(0) * 3.9999999999999996d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(k, c)
term(3) = term(3) + tvvov(a, c, k, a)
term(4) = term(4) + tvvov(b, c, k, b)
term(5) = term(5) + tvvov(a, a, k, c)
term(6) = term(6) + tvvov(b, b, k, c)
term(7) = term(7) + tovoo(k, c, i, i)
term(8) = term(8) + tovoo(k, c, j, j)
term(9) = term(9) + tovoo(i, c, k, i)
term(10) = term(10) + tovoo(j, c, k, j)

term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = term(5) * 1.9999999999999998d+0 
term(6) = term(6) * 1.9999999999999998d+0 
term(7) = term(7) * (-1.9999999999999998d+0) 
term(8) = term(8) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjckaibj = 0.d+0
    do s = 0, 10
    eom_cc3_23_trans_aibjckaibj = eom_cc3_23_trans_aibjckaibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjckaibj
    function eom_cc3_23_trans_aibjckajbj(i, j, c, k) 
    double precision :: eom_cc3_23_trans_aibjckajbj   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, j, i)
term(1) = term(1) + tovoo(j, c, k, i)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjckajbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjckajbj = eom_cc3_23_trans_aibjckajbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjckajbj
    function eom_cc3_23_trans_aibiciaiem(b, c, e, m) 
    double precision :: eom_cc3_23_trans_aibiciaiem   
    integer, intent(in) :: b, c, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, e)
term(1) = term(1) + tvvov(b, e, m, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibiciaiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiciaiem = eom_cc3_23_trans_aibiciaiem + term(s)
    end do

    end function eom_cc3_23_trans_aibiciaiem
    function eom_cc3_23_trans_aibicialei(b, c, l, e) 
    double precision :: eom_cc3_23_trans_aibicialei   
    integer, intent(in) :: b, c, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, e)
term(1) = term(1) + tvvov(b, e, l, c)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aibicialei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibicialei = eom_cc3_23_trans_aibicialei + term(s)
    end do

    end function eom_cc3_23_trans_aibicialei
    function eom_cc3_23_trans_aibjciaiej(i, b, c, e) 
    double precision :: eom_cc3_23_trans_aibjciaiej   
    integer, intent(in) :: i, b, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, i, e)
term(1) = term(1) + tvvov(b, e, i, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjciaiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjciaiej = eom_cc3_23_trans_aibjciaiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjciaiej
    function eom_cc3_23_trans_aibjciajej(b, j, c, e) 
    double precision :: eom_cc3_23_trans_aibjciajej   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, e, j, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjciajej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjciajej = eom_cc3_23_trans_aibjciajej + term(s)
    end do

    end function eom_cc3_23_trans_aibjciajej
    function eom_cc3_23_trans_aibjcjajei(b, j, c, e) 
    double precision :: eom_cc3_23_trans_aibjcjajei   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, e)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjajei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjajei = eom_cc3_23_trans_aibjcjajei + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjajei
    function eom_cc3_23_trans_aibjcjaiej(b, j, c, e) 
    double precision :: eom_cc3_23_trans_aibjcjaiej   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, e)
term(1) = term(1) + tvvov(b, e, j, c)



    eom_cc3_23_trans_aibjcjaiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjaiej = eom_cc3_23_trans_aibjcjaiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjaiej
    function eom_cc3_23_trans_aibjcjaiei(i, b, c, e) 
    double precision :: eom_cc3_23_trans_aibjcjaiei   
    integer, intent(in) :: i, b, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, i, e)
term(1) = term(1) + tvvov(b, e, i, c)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjcjaiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjaiei = eom_cc3_23_trans_aibjcjaiei + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjaiei
    function eom_cc3_23_trans_aibickaiei(b, c, k, e) 
    double precision :: eom_cc3_23_trans_aibickaiei   
    integer, intent(in) :: b, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, k, e)
term(1) = term(1) + tvvov(b, e, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibickaiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibickaiei = eom_cc3_23_trans_aibickaiei + term(s)
    end do

    end function eom_cc3_23_trans_aibickaiei
    function eom_cc3_23_trans_aibicidiam(b, c, d, m) 
    double precision :: eom_cc3_23_trans_aibicidiam   
    integer, intent(in) :: b, c, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, d)
term(1) = term(1) + tvvov(b, d, m, c)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aibicidiam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibicidiam = eom_cc3_23_trans_aibicidiam + term(s)
    end do

    end function eom_cc3_23_trans_aibicidiam
    function eom_cc3_23_trans_aibicidlai(b, c, d, l) 
    double precision :: eom_cc3_23_trans_aibicidlai   
    integer, intent(in) :: b, c, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, d)
term(1) = term(1) + tvvov(b, d, l, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibicidlai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibicidlai = eom_cc3_23_trans_aibicidlai + term(s)
    end do

    end function eom_cc3_23_trans_aibicidlai
    function eom_cc3_23_trans_aibjcidjai(i, b, c, d) 
    double precision :: eom_cc3_23_trans_aibjcidjai   
    integer, intent(in) :: i, b, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, i, d)
term(1) = term(1) + tvvov(b, d, i, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcidjai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcidjai = eom_cc3_23_trans_aibjcidjai + term(s)
    end do

    end function eom_cc3_23_trans_aibjcidjai
    function eom_cc3_23_trans_aibjcidjaj(b, j, c, d) 
    double precision :: eom_cc3_23_trans_aibjcidjaj   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, j, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcidjaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcidjaj = eom_cc3_23_trans_aibjcidjaj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcidjaj
    function eom_cc3_23_trans_aibjcjdjai(b, j, c, d) 
    double precision :: eom_cc3_23_trans_aibjcjdjai   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, d)
term(1) = term(1) + tvvov(b, d, j, c)



    eom_cc3_23_trans_aibjcjdjai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjdjai = eom_cc3_23_trans_aibjcjdjai + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjdjai
    function eom_cc3_23_trans_aibjcjdiaj(b, j, c, d) 
    double precision :: eom_cc3_23_trans_aibjcjdiaj   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjdiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjdiaj = eom_cc3_23_trans_aibjcjdiaj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjdiaj
    function eom_cc3_23_trans_aibjcjdiai(i, b, c, d) 
    double precision :: eom_cc3_23_trans_aibjcjdiai   
    integer, intent(in) :: i, b, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, i, d)
term(1) = term(1) + tvvov(b, d, i, c)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjcjdiai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjdiai = eom_cc3_23_trans_aibjcjdiai + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjdiai
    function eom_cc3_23_trans_aibickdiai(b, c, k, d) 
    double precision :: eom_cc3_23_trans_aibickdiai   
    integer, intent(in) :: b, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, k, d)
term(1) = term(1) + tvvov(b, d, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibickdiai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibickdiai = eom_cc3_23_trans_aibickdiai + term(s)
    end do

    end function eom_cc3_23_trans_aibickdiai
    function eom_cc3_23_trans_aibjcibjbm(a, b, c, m) 
    double precision :: eom_cc3_23_trans_aibjcibjbm   
    integer, intent(in) :: a, b, c, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, b)
term(1) = term(1) + tvvov(a, b, m, c)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjcibjbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcibjbm = eom_cc3_23_trans_aibjcibjbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjcibjbm
    function eom_cc3_23_trans_aibjciblbj(a, b, c, l) 
    double precision :: eom_cc3_23_trans_aibjciblbj   
    integer, intent(in) :: a, b, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, b)
term(1) = term(1) + tvvov(a, b, l, c)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjciblbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjciblbj = eom_cc3_23_trans_aibjciblbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjciblbj
    function eom_cc3_23_trans_aibjcjbibm(a, b, c, m) 
    double precision :: eom_cc3_23_trans_aibjcjbibm   
    integer, intent(in) :: a, b, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, m, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjbibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjbibm = eom_cc3_23_trans_aibjcjbibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjbibm
    function eom_cc3_23_trans_aibjcjblbi(a, b, c, l) 
    double precision :: eom_cc3_23_trans_aibjcjblbi   
    integer, intent(in) :: a, b, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, l, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjblbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjblbi = eom_cc3_23_trans_aibjcjblbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjblbi
    function eom_cc3_23_trans_aibjckbibj(a, b, c, k) 
    double precision :: eom_cc3_23_trans_aibjckbibj   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, b)
term(1) = term(1) + tvvov(a, b, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjckbibj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjckbibj = eom_cc3_23_trans_aibjckbibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjckbibj
    function eom_cc3_23_trans_aibjckbjbi(a, b, c, k) 
    double precision :: eom_cc3_23_trans_aibjckbjbi   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, b)
term(1) = term(1) + tvvov(a, b, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjckbjbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjckbjbi = eom_cc3_23_trans_aibjckbjbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjckbjbi
    function eom_cc3_23_trans_aibicibiem(a, c, e, m) 
    double precision :: eom_cc3_23_trans_aibicibiem   
    integer, intent(in) :: a, c, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, e)
term(1) = term(1) + tvvov(a, e, m, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibicibiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibicibiem = eom_cc3_23_trans_aibicibiem + term(s)
    end do

    end function eom_cc3_23_trans_aibicibiem
    function eom_cc3_23_trans_aibiciblei(a, c, l, e) 
    double precision :: eom_cc3_23_trans_aibiciblei   
    integer, intent(in) :: a, c, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, e)
term(1) = term(1) + tvvov(a, e, l, c)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aibiciblei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiciblei = eom_cc3_23_trans_aibiciblei + term(s)
    end do

    end function eom_cc3_23_trans_aibiciblei
    function eom_cc3_23_trans_aibjcibiej(a, i, c, e) 
    double precision :: eom_cc3_23_trans_aibjcibiej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, e)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcibiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcibiej = eom_cc3_23_trans_aibjcibiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjcibiej
    function eom_cc3_23_trans_aibjcibjei(a, i, c, e) 
    double precision :: eom_cc3_23_trans_aibjcibjei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, e)
term(1) = term(1) + tvvov(a, e, i, c)



    eom_cc3_23_trans_aibjcibjei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcibjei = eom_cc3_23_trans_aibjcibjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjcibjei
    function eom_cc3_23_trans_aibjcibjej(a, j, c, e) 
    double precision :: eom_cc3_23_trans_aibjcibjej   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, e)
term(1) = term(1) + tvvov(a, e, j, c)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjcibjej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcibjej = eom_cc3_23_trans_aibjcibjej + term(s)
    end do

    end function eom_cc3_23_trans_aibjcibjej
    function eom_cc3_23_trans_aibjcjbjei(a, j, c, e) 
    double precision :: eom_cc3_23_trans_aibjcjbjei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, e)
term(1) = term(1) + tvvov(a, e, j, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjbjei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjbjei = eom_cc3_23_trans_aibjcjbjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjbjei
    function eom_cc3_23_trans_aibjcjbiei(a, i, c, e) 
    double precision :: eom_cc3_23_trans_aibjcjbiei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, i, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjbiei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjbiei = eom_cc3_23_trans_aibjcjbiei + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjbiei
    function eom_cc3_23_trans_aibickbiei(a, c, k, e) 
    double precision :: eom_cc3_23_trans_aibickbiei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, e)
term(1) = term(1) + tvvov(a, e, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibickbiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibickbiei = eom_cc3_23_trans_aibickbiei + term(s)
    end do

    end function eom_cc3_23_trans_aibickbiei
    function eom_cc3_23_trans_aibicidibm(a, c, d, m) 
    double precision :: eom_cc3_23_trans_aibicidibm   
    integer, intent(in) :: a, c, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, d)
term(1) = term(1) + tvvov(a, d, m, c)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aibicidibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibicidibm = eom_cc3_23_trans_aibicidibm + term(s)
    end do

    end function eom_cc3_23_trans_aibicidibm
    function eom_cc3_23_trans_aibicidlbi(a, c, d, l) 
    double precision :: eom_cc3_23_trans_aibicidlbi   
    integer, intent(in) :: a, c, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, d)
term(1) = term(1) + tvvov(a, d, l, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aibicidlbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibicidlbi = eom_cc3_23_trans_aibicidlbi + term(s)
    end do

    end function eom_cc3_23_trans_aibicidlbi
    function eom_cc3_23_trans_aibjcidibj(a, i, c, d) 
    double precision :: eom_cc3_23_trans_aibjcidibj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, d)
term(1) = term(1) + tvvov(a, d, i, c)



    eom_cc3_23_trans_aibjcidibj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcidibj = eom_cc3_23_trans_aibjcidibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcidibj
    function eom_cc3_23_trans_aibjcidjbi(a, i, c, d) 
    double precision :: eom_cc3_23_trans_aibjcidjbi   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcidjbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcidjbi = eom_cc3_23_trans_aibjcidjbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjcidjbi
    function eom_cc3_23_trans_aibjcidjbj(a, j, c, d) 
    double precision :: eom_cc3_23_trans_aibjcidjbj   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, d)
term(1) = term(1) + tvvov(a, d, j, c)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjcidjbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcidjbj = eom_cc3_23_trans_aibjcidjbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcidjbj
    function eom_cc3_23_trans_aibjcjdibj(a, j, c, d) 
    double precision :: eom_cc3_23_trans_aibjcjdibj   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, d)
term(1) = term(1) + tvvov(a, d, j, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjdibj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjdibj = eom_cc3_23_trans_aibjcjdibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjdibj
    function eom_cc3_23_trans_aibjcjdibi(a, i, c, d) 
    double precision :: eom_cc3_23_trans_aibjcjdibi   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjdibi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjdibi = eom_cc3_23_trans_aibjcjdibi + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjdibi
    function eom_cc3_23_trans_aibickdibi(a, c, k, d) 
    double precision :: eom_cc3_23_trans_aibickdibi   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, d)
term(1) = term(1) + tvvov(a, d, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibickdibi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibickdibi = eom_cc3_23_trans_aibickdibi + term(s)
    end do

    end function eom_cc3_23_trans_aibickdibi
    function eom_cc3_23_trans_aiaiaialem(i, l, e, m) 
    double precision :: eom_cc3_23_trans_aiaiaialem   
    integer, intent(in) :: i, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, i)
term(1) = term(1) + tovoo(l, e, m, i)

term(0) = term(0) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aiaiaialem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaiaialem = eom_cc3_23_trans_aiaiaialem + term(s)
    end do

    end function eom_cc3_23_trans_aiaiaialem
    function eom_cc3_23_trans_aiajaiaiem(i, j, e, m) 
    double precision :: eom_cc3_23_trans_aiajaiaiem   
    integer, intent(in) :: i, j, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, i, j)
term(1) = term(1) + tovoo(i, e, m, j)

term(0) = term(0) * (-3.9999999999999996d+0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajaiaiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajaiaiem = eom_cc3_23_trans_aiajaiaiem + term(s)
    end do

    end function eom_cc3_23_trans_aiajaiaiem
    function eom_cc3_23_trans_aiajaialei(i, j, l, e) 
    double precision :: eom_cc3_23_trans_aiajaialei   
    integer, intent(in) :: i, j, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, i, j)
term(1) = term(1) + tovoo(i, e, l, j)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajaialei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajaialei = eom_cc3_23_trans_aiajaialei + term(s)
    end do

    end function eom_cc3_23_trans_aiajaialei
    function eom_cc3_23_trans_aiajaiajem(nocc, a, i, j, e, m) 
    double precision :: eom_cc3_23_trans_aiajaiajem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, e, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, e, n, n)
term(1) = term(1) + tovoo(n, e, m, n)
end do 

term(0) = term(0) * 3.9999999999999996d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(m, e)
term(3) = term(3) + tovoo(m, e, i, i)
term(4) = term(4) + tovoo(m, e, j, j)
term(5) = term(5) + tovoo(i, e, m, i)
term(6) = term(6) + tovoo(j, e, m, j)
term(7) = term(7) + tvvov(a, a, m, e)
term(8) = term(8) + tvvov(a, e, m, a)

term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-1.9999999999999998d+0) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(7) = term(7) * 3.9999999999999996d+0 
term(8) = term(8) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajaiajem = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aiajaiajem = eom_cc3_23_trans_aiajaiajem + term(s)
    end do

    end function eom_cc3_23_trans_aiajaiajem
    function eom_cc3_23_trans_aiajaialej(nocc, a, i, j, l, e) 
    double precision :: eom_cc3_23_trans_aiajaialej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, l, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, l, n)
term(1) = term(1) + tovoo(l, e, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(l, e)
term(3) = term(3) + tovoo(l, e, i, i)
term(4) = term(4) + tovoo(i, e, l, i)
term(5) = term(5) + tovoo(j, e, l, j)
term(6) = term(6) + tovoo(l, e, j, j)
term(7) = term(7) + tvvov(a, a, l, e)
term(8) = term(8) + tvvov(a, e, l, a)

term(2) = -term(2) 
term(5) = term(5) * (-1.9999999999999998d+0) 
term(7) = term(7) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajaialej = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aiajaialej = eom_cc3_23_trans_aiajaialej + term(s)
    end do

    end function eom_cc3_23_trans_aiajaialej
    function eom_cc3_23_trans_aiajaialel(j, l, e) 
    double precision :: eom_cc3_23_trans_aiajaialel   
    integer, intent(in) :: j, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, l, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aiajaialel = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajaialel = eom_cc3_23_trans_aiajaialel + term(s)
    end do

    end function eom_cc3_23_trans_aiajaialel
    function eom_cc3_23_trans_aiajajajem(i, j, e, m) 
    double precision :: eom_cc3_23_trans_aiajajajem   
    integer, intent(in) :: i, j, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, j, i)
term(1) = term(1) + tovoo(j, e, m, i)

term(0) = term(0) * (-3.9999999999999996d+0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajajajem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajajajem = eom_cc3_23_trans_aiajajajem + term(s)
    end do

    end function eom_cc3_23_trans_aiajajajem
    function eom_cc3_23_trans_aiajajalej(i, j, l, e) 
    double precision :: eom_cc3_23_trans_aiajajalej   
    integer, intent(in) :: i, j, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, j, i)
term(1) = term(1) + tovoo(j, e, l, i)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aiajajalej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajajalej = eom_cc3_23_trans_aiajajalej + term(s)
    end do

    end function eom_cc3_23_trans_aiajajalej
    function eom_cc3_23_trans_aiajajalei(nocc, a, i, j, l, e) 
    double precision :: eom_cc3_23_trans_aiajajalei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, l, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, l, n)
term(1) = term(1) + tovoo(l, e, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(l, e)
term(3) = term(3) + tovoo(l, e, j, j)
term(4) = term(4) + tovoo(j, e, l, j)
term(5) = term(5) + tovoo(i, e, l, i)
term(6) = term(6) + tovoo(l, e, i, i)
term(7) = term(7) + tvvov(a, a, l, e)
term(8) = term(8) + tvvov(a, e, l, a)

term(2) = -term(2) 
term(5) = term(5) * (-1.9999999999999998d+0) 
term(7) = term(7) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajajalei = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aiajajalei = eom_cc3_23_trans_aiajajalei + term(s)
    end do

    end function eom_cc3_23_trans_aiajajalei
    function eom_cc3_23_trans_aiajajalel(i, l, e) 
    double precision :: eom_cc3_23_trans_aiajajalel   
    integer, intent(in) :: i, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, l, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aiajajalel = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajajalel = eom_cc3_23_trans_aiajajalel + term(s)
    end do

    end function eom_cc3_23_trans_aiajajalel
    function eom_cc3_23_trans_aiajakakei(j, k, e) 
    double precision :: eom_cc3_23_trans_aiajakakei   
    integer, intent(in) :: j, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, j)

term(0) = term(0) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajakakei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajakakei = eom_cc3_23_trans_aiajakakei + term(s)
    end do

    end function eom_cc3_23_trans_aiajakakei
    function eom_cc3_23_trans_aiajakakej(i, k, e) 
    double precision :: eom_cc3_23_trans_aiajakakej   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)

term(0) = term(0) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajakakej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajakakej = eom_cc3_23_trans_aiajakakej + term(s)
    end do

    end function eom_cc3_23_trans_aiajakakej
    function eom_cc3_23_trans_aiajakaiek(j, k, e) 
    double precision :: eom_cc3_23_trans_aiajakaiek   
    integer, intent(in) :: j, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aiajakaiek = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajakaiek = eom_cc3_23_trans_aiajakaiek + term(s)
    end do

    end function eom_cc3_23_trans_aiajakaiek
    function eom_cc3_23_trans_aiajakajek(i, k, e) 
    double precision :: eom_cc3_23_trans_aiajakajek   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aiajakajek = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajakajek = eom_cc3_23_trans_aiajakajek + term(s)
    end do

    end function eom_cc3_23_trans_aiajakajek
    function eom_cc3_23_trans_aiaiakaiem(i, k, e, m) 
    double precision :: eom_cc3_23_trans_aiaiakaiem   
    integer, intent(in) :: i, k, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, i)
term(1) = term(1) + tovoo(k, e, m, i)

term(0) = term(0) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aiaiakaiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaiakaiem = eom_cc3_23_trans_aiaiakaiem + term(s)
    end do

    end function eom_cc3_23_trans_aiaiakaiem
    function eom_cc3_23_trans_aiaiakalei(i, k, l, e) 
    double precision :: eom_cc3_23_trans_aiaiakalei   
    integer, intent(in) :: i, k, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, i)
term(1) = term(1) + tovoo(k, e, l, i)



    eom_cc3_23_trans_aiaiakalei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaiakalei = eom_cc3_23_trans_aiaiakalei + term(s)
    end do

    end function eom_cc3_23_trans_aiaiakalei
    function eom_cc3_23_trans_aiajakaiei(i, j, k, e) 
    double precision :: eom_cc3_23_trans_aiajakaiei   
    integer, intent(in) :: i, j, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, k, j)
term(1) = term(1) + tovoo(k, e, i, j)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajakaiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajakaiei = eom_cc3_23_trans_aiajakaiei + term(s)
    end do

    end function eom_cc3_23_trans_aiajakaiei
    function eom_cc3_23_trans_aiajakaiej(nocc, a, i, j, k, e) 
    double precision :: eom_cc3_23_trans_aiajakaiej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, k, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, k, n)
term(1) = term(1) + tovoo(k, e, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(k, e)
term(3) = term(3) + tovoo(i, e, k, i)
term(4) = term(4) + tovoo(j, e, k, j)
term(5) = term(5) + tovoo(k, e, i, i)
term(6) = term(6) + tovoo(k, e, j, j)
term(7) = term(7) + tvvov(a, a, k, e)
term(8) = term(8) + tvvov(a, e, k, a)

term(2) = -term(2) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(7) = term(7) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajakaiej = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aiajakaiej = eom_cc3_23_trans_aiajakaiej + term(s)
    end do

    end function eom_cc3_23_trans_aiajakaiej
    function eom_cc3_23_trans_aiajakajei(nocc, a, i, j, k, e) 
    double precision :: eom_cc3_23_trans_aiajakajei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, k, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, k, n)
term(1) = term(1) + tovoo(k, e, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(k, e)
term(3) = term(3) + tovoo(j, e, k, j)
term(4) = term(4) + tovoo(i, e, k, i)
term(5) = term(5) + tovoo(k, e, j, j)
term(6) = term(6) + tovoo(k, e, i, i)
term(7) = term(7) + tvvov(a, a, k, e)
term(8) = term(8) + tvvov(a, e, k, a)

term(2) = -term(2) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(7) = term(7) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajakajei = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aiajakajei = eom_cc3_23_trans_aiajakajei + term(s)
    end do

    end function eom_cc3_23_trans_aiajakajei
    function eom_cc3_23_trans_aiajakajej(i, j, k, e) 
    double precision :: eom_cc3_23_trans_aiajakajej   
    integer, intent(in) :: i, j, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, k, i)
term(1) = term(1) + tovoo(k, e, j, i)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajakajej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajakajej = eom_cc3_23_trans_aiajakajej + term(s)
    end do

    end function eom_cc3_23_trans_aiajakajej
    function eom_cc3_23_trans_aiajaidjdm(a, d, m) 
    double precision :: eom_cc3_23_trans_aiajaidjdm   
    integer, intent(in) :: a, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, d)



    eom_cc3_23_trans_aiajaidjdm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajaidjdm = eom_cc3_23_trans_aiajaidjdm + term(s)
    end do

    end function eom_cc3_23_trans_aiajaidjdm
    function eom_cc3_23_trans_aiajaidldj(a, d, l) 
    double precision :: eom_cc3_23_trans_aiajaidldj   
    integer, intent(in) :: a, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, d)



    eom_cc3_23_trans_aiajaidldj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajaidldj = eom_cc3_23_trans_aiajaidldj + term(s)
    end do

    end function eom_cc3_23_trans_aiajaidldj
    function eom_cc3_23_trans_aiajajdidm(a, d, m) 
    double precision :: eom_cc3_23_trans_aiajajdidm   
    integer, intent(in) :: a, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, d)



    eom_cc3_23_trans_aiajajdidm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajajdidm = eom_cc3_23_trans_aiajajdidm + term(s)
    end do

    end function eom_cc3_23_trans_aiajajdidm
    function eom_cc3_23_trans_aiajajdldi(a, d, l) 
    double precision :: eom_cc3_23_trans_aiajajdldi   
    integer, intent(in) :: a, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, d)



    eom_cc3_23_trans_aiajajdldi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajajdldi = eom_cc3_23_trans_aiajajdldi + term(s)
    end do

    end function eom_cc3_23_trans_aiajajdldi
    function eom_cc3_23_trans_aiajakdidj(a, k, d) 
    double precision :: eom_cc3_23_trans_aiajakdidj   
    integer, intent(in) :: a, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, k, d)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajakdidj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajakdidj = eom_cc3_23_trans_aiajakdidj + term(s)
    end do

    end function eom_cc3_23_trans_aiajakdidj
    function eom_cc3_23_trans_aiaiaidiem(a, d, e, m) 
    double precision :: eom_cc3_23_trans_aiaiaidiem   
    integer, intent(in) :: a, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, e)
term(1) = term(1) + tvvov(a, e, m, d)

term(0) = term(0) * 1.9999999999999996d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aiaiaidiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaiaidiem = eom_cc3_23_trans_aiaiaidiem + term(s)
    end do

    end function eom_cc3_23_trans_aiaiaidiem
    function eom_cc3_23_trans_aiaiaidlei(a, d, l, e) 
    double precision :: eom_cc3_23_trans_aiaiaidlei   
    integer, intent(in) :: a, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, e)
term(1) = term(1) + tvvov(a, e, l, d)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiaiaidlei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaiaidlei = eom_cc3_23_trans_aiaiaidlei + term(s)
    end do

    end function eom_cc3_23_trans_aiaiaidlei
    function eom_cc3_23_trans_aiajaidiej(a, i, d, e) 
    double precision :: eom_cc3_23_trans_aiajaidiej   
    integer, intent(in) :: a, i, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, e)
term(1) = term(1) + tvvov(a, e, i, d)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajaidiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajaidiej = eom_cc3_23_trans_aiajaidiej + term(s)
    end do

    end function eom_cc3_23_trans_aiajaidiej
    function eom_cc3_23_trans_aiajaidjei(a, i, d, e) 
    double precision :: eom_cc3_23_trans_aiajaidjei   
    integer, intent(in) :: a, i, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, e)
term(1) = term(1) + tvvov(a, e, i, d)

term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajaidjei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajaidjei = eom_cc3_23_trans_aiajaidjei + term(s)
    end do

    end function eom_cc3_23_trans_aiajaidjei
    function eom_cc3_23_trans_aiajaidjej(a, j, d, e) 
    double precision :: eom_cc3_23_trans_aiajaidjej   
    integer, intent(in) :: a, j, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, e)
term(1) = term(1) + tvvov(a, e, j, d)



    eom_cc3_23_trans_aiajaidjej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajaidjej = eom_cc3_23_trans_aiajaidjej + term(s)
    end do

    end function eom_cc3_23_trans_aiajaidjej
    function eom_cc3_23_trans_aiajajdjei(a, j, d, e) 
    double precision :: eom_cc3_23_trans_aiajajdjei   
    integer, intent(in) :: a, j, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, e)
term(1) = term(1) + tvvov(a, e, j, d)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajajdjei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajajdjei = eom_cc3_23_trans_aiajajdjei + term(s)
    end do

    end function eom_cc3_23_trans_aiajajdjei
    function eom_cc3_23_trans_aiajajdiej(a, j, d, e) 
    double precision :: eom_cc3_23_trans_aiajajdiej   
    integer, intent(in) :: a, j, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, e)
term(1) = term(1) + tvvov(a, e, j, d)

term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajajdiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajajdiej = eom_cc3_23_trans_aiajajdiej + term(s)
    end do

    end function eom_cc3_23_trans_aiajajdiej
    function eom_cc3_23_trans_aiajajdiei(a, i, d, e) 
    double precision :: eom_cc3_23_trans_aiajajdiei   
    integer, intent(in) :: a, i, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, e)
term(1) = term(1) + tvvov(a, e, i, d)



    eom_cc3_23_trans_aiajajdiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajajdiei = eom_cc3_23_trans_aiajajdiei + term(s)
    end do

    end function eom_cc3_23_trans_aiajajdiei
    function eom_cc3_23_trans_aiaiakdiei(a, k, d, e) 
    double precision :: eom_cc3_23_trans_aiaiakdiei   
    integer, intent(in) :: a, k, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, k, e)
term(1) = term(1) + tvvov(a, e, k, d)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aiaiakdiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaiakdiei = eom_cc3_23_trans_aiaiakdiei + term(s)
    end do

    end function eom_cc3_23_trans_aiaiakdiei
    function eom_cc3_23_trans_aibiaialbm(a, i, l, m) 
    double precision :: eom_cc3_23_trans_aibiaialbm   
    integer, intent(in) :: a, i, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, a, m, i)
term(1) = term(1) + tovoo(m, a, l, i)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibiaialbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiaialbm = eom_cc3_23_trans_aibiaialbm + term(s)
    end do

    end function eom_cc3_23_trans_aibiaialbm
    function eom_cc3_23_trans_aibjaiaibm(a, i, j, m) 
    double precision :: eom_cc3_23_trans_aibjaiaibm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, m, j)
term(1) = term(1) + tovoo(m, a, i, j)

term(0) = term(0) * (-1.9999999999999996d+0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjaiaibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaiaibm = eom_cc3_23_trans_aibjaiaibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiaibm
    function eom_cc3_23_trans_aibjaialbi(a, i, j, l) 
    double precision :: eom_cc3_23_trans_aibjaialbi   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, a, i, j)
term(1) = term(1) + tovoo(i, a, l, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjaialbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaialbi = eom_cc3_23_trans_aibjaialbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjaialbi
    function eom_cc3_23_trans_aibjaiajbm(nocc, a, i, b, j, m) 
    double precision :: eom_cc3_23_trans_aibjaiajbm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, a, n, n)
term(1) = term(1) + tovoo(n, a, m, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tvvov(a, a, m, a)
term(3) = term(3) + tov(m, a)
term(4) = term(4) + tvvov(b, a, m, b)
term(5) = term(5) + tvvov(b, b, m, a)
term(6) = term(6) + tovoo(m, a, i, i)
term(7) = term(7) + tovoo(j, a, m, j)
term(8) = term(8) + tovoo(m, a, j, j)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = term(4) * 1.9999999999999998d+0 
term(5) = -term(5) 
term(7) = -term(7) 


    eom_cc3_23_trans_aibjaiajbm = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjaiajbm = eom_cc3_23_trans_aibjaiajbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiajbm
    function eom_cc3_23_trans_aibjaialbj(nocc, a, i, b, j, l) 
    double precision :: eom_cc3_23_trans_aibjaialbj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,n 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(l, a, n, n)
term(1) = term(1) + tovoo(n, a, l, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 

term(2) = term(2) + tvvov(a, a, l, a)
term(3) = term(3) + tov(l, a)
term(4) = term(4) + tvvov(b, a, l, b)
term(5) = term(5) + tvvov(b, b, l, a)
term(6) = term(6) + tovoo(i, a, l, i)
term(7) = term(7) + tovoo(l, a, i, i)
term(8) = term(8) + tovoo(l, a, j, j)
term(9) = term(9) + tovoo(j, a, l, j)

term(4) = -term(4) 
term(6) = -term(6) 
term(7) = -term(7) 
term(8) = -term(8) 


    eom_cc3_23_trans_aibjaialbj = 0.d+0
    do s = 0, 9
    eom_cc3_23_trans_aibjaialbj = eom_cc3_23_trans_aibjaialbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjaialbj
    function eom_cc3_23_trans_aibjajajbm(a, i, j, m) 
    double precision :: eom_cc3_23_trans_aibjajajbm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, a, j, i)

term(0) = term(0) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjajajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjajajbm = eom_cc3_23_trans_aibjajajbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjajajbm
    function eom_cc3_23_trans_aibjajalbj(a, i, j, l) 
    double precision :: eom_cc3_23_trans_aibjajalbj   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, a, j, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjajalbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjajalbj = eom_cc3_23_trans_aibjajalbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjajalbj
    function eom_cc3_23_trans_aibjajaibm(nocc, a, i, b, j, m) 
    double precision :: eom_cc3_23_trans_aibjajaibm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, a, n, n)
term(1) = term(1) + tovoo(n, a, m, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tvvov(a, a, m, a)
term(3) = term(3) + tov(m, a)
term(4) = term(4) + tvvov(b, a, m, b)
term(5) = term(5) + tvvov(b, b, m, a)
term(6) = term(6) + tovoo(j, a, m, j)
term(7) = term(7) + tovoo(m, a, j, j)
term(8) = term(8) + tovoo(m, a, i, i)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = term(4) * 1.9999999999999998d+0 
term(5) = -term(5) 
term(6) = -term(6) 


    eom_cc3_23_trans_aibjajaibm = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjajaibm = eom_cc3_23_trans_aibjajaibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjajaibm
    function eom_cc3_23_trans_aibjajalbi(a, i, b, l) 
    double precision :: eom_cc3_23_trans_aibjajalbi   
    integer, intent(in) :: a, i, b, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, l, i)
term(1) = term(1) + tvvov(b, a, l, b)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjajalbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajalbi = eom_cc3_23_trans_aibjajalbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjajalbi
    function eom_cc3_23_trans_aibjajalbl(a, i, l) 
    double precision :: eom_cc3_23_trans_aibjajalbl   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, a, l, i)



    eom_cc3_23_trans_aibjajalbl = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjajalbl = eom_cc3_23_trans_aibjajalbl + term(s)
    end do

    end function eom_cc3_23_trans_aibjajalbl
    function eom_cc3_23_trans_aibjakakbj(a, i, k) 
    double precision :: eom_cc3_23_trans_aibjakakbj   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, k, i)

term(0) = term(0) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aibjakakbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakakbj = eom_cc3_23_trans_aibjakakbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjakakbj
    function eom_cc3_23_trans_aibjakajbk(a, i, k) 
    double precision :: eom_cc3_23_trans_aibjakajbk   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, k, i)



    eom_cc3_23_trans_aibjakajbk = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakajbk = eom_cc3_23_trans_aibjakajbk + term(s)
    end do

    end function eom_cc3_23_trans_aibjakajbk
    function eom_cc3_23_trans_aibiakaibm(a, i, k, m) 
    double precision :: eom_cc3_23_trans_aibiakaibm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, m, i)
term(1) = term(1) + tovoo(m, a, k, i)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibiakaibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiakaibm = eom_cc3_23_trans_aibiakaibm + term(s)
    end do

    end function eom_cc3_23_trans_aibiakaibm
    function eom_cc3_23_trans_aibiakalbi(a, i, k, l) 
    double precision :: eom_cc3_23_trans_aibiakalbi   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, l, i)
term(1) = term(1) + tovoo(l, a, k, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aibiakalbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiakalbi = eom_cc3_23_trans_aibiakalbi + term(s)
    end do

    end function eom_cc3_23_trans_aibiakalbi
    function eom_cc3_23_trans_aibjakaibi(a, i, j, k) 
    double precision :: eom_cc3_23_trans_aibjakaibi   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, i, j)
term(1) = term(1) + tovoo(i, a, k, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakaibi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakaibi = eom_cc3_23_trans_aibjakaibi + term(s)
    end do

    end function eom_cc3_23_trans_aibjakaibi
    function eom_cc3_23_trans_aibjakaibj(nocc, a, i, b, j, k) 
    double precision :: eom_cc3_23_trans_aibjakaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,n 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(k, a, n, n)
term(1) = term(1) + tovoo(n, a, k, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 

term(2) = term(2) + tvvov(a, a, k, a)
term(3) = term(3) + tov(k, a)
term(4) = term(4) + tvvov(b, a, k, b)
term(5) = term(5) + tvvov(b, b, k, a)
term(6) = term(6) + tovoo(k, a, i, i)
term(7) = term(7) + tovoo(k, a, j, j)
term(8) = term(8) + tovoo(i, a, k, i)
term(9) = term(9) + tovoo(j, a, k, j)

term(4) = -term(4) 
term(6) = -term(6) 
term(7) = -term(7) 
term(8) = -term(8) 


    eom_cc3_23_trans_aibjakaibj = 0.d+0
    do s = 0, 9
    eom_cc3_23_trans_aibjakaibj = eom_cc3_23_trans_aibjakaibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjakaibj
    function eom_cc3_23_trans_aibjakajbi(a, i, b, k) 
    double precision :: eom_cc3_23_trans_aibjakajbi   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, k, i)
term(1) = term(1) + tvvov(b, a, k, b)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjakajbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakajbi = eom_cc3_23_trans_aibjakajbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjakajbi
    function eom_cc3_23_trans_aibjakajbj(a, i, j, k) 
    double precision :: eom_cc3_23_trans_aibjakajbj   
    integer, intent(in) :: a, i, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, j, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjakajbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakajbj = eom_cc3_23_trans_aibjakajbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjakajbj
    function eom_cc3_23_trans_aibiaiaiem(a, b, e, m) 
    double precision :: eom_cc3_23_trans_aibiaiaiem   
    integer, intent(in) :: a, b, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, m, e)
term(1) = term(1) + tvvov(b, e, m, a)

term(0) = term(0) * 3.9999999999999996d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibiaiaiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiaiaiem = eom_cc3_23_trans_aibiaiaiem + term(s)
    end do

    end function eom_cc3_23_trans_aibiaiaiem
    function eom_cc3_23_trans_aibiaialei(a, b, l, e) 
    double precision :: eom_cc3_23_trans_aibiaialei   
    integer, intent(in) :: a, b, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, l, e)
term(1) = term(1) + tvvov(b, e, l, a)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibiaialei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiaialei = eom_cc3_23_trans_aibiaialei + term(s)
    end do

    end function eom_cc3_23_trans_aibiaialei
    function eom_cc3_23_trans_aibjaiaiej(a, i, b, e) 
    double precision :: eom_cc3_23_trans_aibjaiaiej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, i, e)
term(1) = term(1) + tvvov(b, e, i, a)

term(0) = term(0) * (-1.9999999999999998d+0) 
term(1) = term(1) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aibjaiaiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaiaiej = eom_cc3_23_trans_aibjaiaiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiaiej
    function eom_cc3_23_trans_aibjaiajei(a, i, b, e) 
    double precision :: eom_cc3_23_trans_aibjaiajei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, i, e)
term(1) = term(1) + tvvov(b, e, i, a)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjaiajei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaiajei = eom_cc3_23_trans_aibjaiajei + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiajei
    function eom_cc3_23_trans_aibjaiajej(a, b, j, e) 
    double precision :: eom_cc3_23_trans_aibjaiajej   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, j, e)



    eom_cc3_23_trans_aibjaiajej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjaiajej = eom_cc3_23_trans_aibjaiajej + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiajej
    function eom_cc3_23_trans_aibjajajei(a, b, j, e) 
    double precision :: eom_cc3_23_trans_aibjajajei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, j, e)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjajajei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjajajei = eom_cc3_23_trans_aibjajajei + term(s)
    end do

    end function eom_cc3_23_trans_aibjajajei
    function eom_cc3_23_trans_aibjajaiej(a, b, j, e) 
    double precision :: eom_cc3_23_trans_aibjajaiej   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, j, e)



    eom_cc3_23_trans_aibjajaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjajaiej = eom_cc3_23_trans_aibjajaiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjajaiej
    function eom_cc3_23_trans_aibjajaiei(a, i, b, e) 
    double precision :: eom_cc3_23_trans_aibjajaiei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, i, e)
term(1) = term(1) + tvvov(b, e, i, a)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjajaiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajaiei = eom_cc3_23_trans_aibjajaiei + term(s)
    end do

    end function eom_cc3_23_trans_aibjajaiei
    function eom_cc3_23_trans_aibiakaiei(a, b, k, e) 
    double precision :: eom_cc3_23_trans_aibiakaiei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, k, e)
term(1) = term(1) + tvvov(b, e, k, a)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibiakaiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiakaiei = eom_cc3_23_trans_aibiakaiei + term(s)
    end do

    end function eom_cc3_23_trans_aibiakaiei
    function eom_cc3_23_trans_aibiaiblbm(i, b, l, m) 
    double precision :: eom_cc3_23_trans_aibiaiblbm   
    integer, intent(in) :: i, b, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, b, m, i)
term(1) = term(1) + tovoo(m, b, l, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aibiaiblbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiaiblbm = eom_cc3_23_trans_aibiaiblbm + term(s)
    end do

    end function eom_cc3_23_trans_aibiaiblbm
    function eom_cc3_23_trans_aibjaibibm(i, b, j, m) 
    double precision :: eom_cc3_23_trans_aibjaibibm   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, b, i, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjaibibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjaibibm = eom_cc3_23_trans_aibjaibibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjaibibm
    function eom_cc3_23_trans_aibjaiblbi(i, b, j, l) 
    double precision :: eom_cc3_23_trans_aibjaiblbi   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, b, i, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjaiblbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjaiblbi = eom_cc3_23_trans_aibjaiblbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiblbi
    function eom_cc3_23_trans_aibjaibjbm(nocc, a, i, b, j, m) 
    double precision :: eom_cc3_23_trans_aibjaibjbm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, m 
    integer :: s ,n 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, b, n, n)
term(1) = term(1) + tovoo(n, b, m, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 

term(2) = term(2) + tvvov(b, b, m, b)
term(3) = term(3) + tov(m, b)
term(4) = term(4) + tovoo(m, b, i, i)
term(5) = term(5) + tovoo(j, b, m, j)
term(6) = term(6) + tovoo(m, b, j, j)
term(7) = term(7) + tovoo(i, b, m, i)
term(8) = term(8) + tvvov(a, a, m, b)
term(9) = term(9) + tvvov(a, b, m, a)

term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 
term(9) = -term(9) 


    eom_cc3_23_trans_aibjaibjbm = 0.d+0
    do s = 0, 9
    eom_cc3_23_trans_aibjaibjbm = eom_cc3_23_trans_aibjaibjbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjaibjbm
    function eom_cc3_23_trans_aibjaiblbj(nocc, a, i, b, j, l) 
    double precision :: eom_cc3_23_trans_aibjaiblbj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,n 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(l, b, n, n)
term(1) = term(1) + tovoo(n, b, l, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 

term(2) = term(2) + tvvov(b, b, l, b)
term(3) = term(3) + tov(l, b)
term(4) = term(4) + tovoo(l, b, i, i)
term(5) = term(5) + tovoo(i, b, l, i)
term(6) = term(6) + tovoo(l, b, j, j)
term(7) = term(7) + tovoo(j, b, l, j)
term(8) = term(8) + tvvov(a, a, l, b)
term(9) = term(9) + tvvov(a, b, l, a)

term(4) = -term(4) 
term(6) = -term(6) 
term(7) = -term(7) 
term(9) = -term(9) 


    eom_cc3_23_trans_aibjaiblbj = 0.d+0
    do s = 0, 9
    eom_cc3_23_trans_aibjaiblbj = eom_cc3_23_trans_aibjaiblbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiblbj
    function eom_cc3_23_trans_aibjaiblbl(b, j, l) 
    double precision :: eom_cc3_23_trans_aibjaiblbl   
    integer, intent(in) :: b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, b, l, j)

term(0) = term(0) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aibjaiblbl = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjaiblbl = eom_cc3_23_trans_aibjaiblbl + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiblbl
    function eom_cc3_23_trans_aibjajbjbm(i, b, j, m) 
    double precision :: eom_cc3_23_trans_aibjajbjbm   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, b, j, i)
term(1) = term(1) + tovoo(j, b, m, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjajbjbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajbjbm = eom_cc3_23_trans_aibjajbjbm + term(s)
    end do

    end function eom_cc3_23_trans_aibjajbjbm
    function eom_cc3_23_trans_aibjajblbj(i, b, j, l) 
    double precision :: eom_cc3_23_trans_aibjajblbj   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, b, j, i)
term(1) = term(1) + tovoo(j, b, l, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjajblbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajblbj = eom_cc3_23_trans_aibjajblbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjajblbj
    function eom_cc3_23_trans_aibjajbibm(a, b, j, m) 
    double precision :: eom_cc3_23_trans_aibjajbibm   
    integer, intent(in) :: a, b, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, m, j)
term(1) = term(1) + tvvov(a, b, m, a)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjajbibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajbibm = eom_cc3_23_trans_aibjajbibm + term(s)
    end do

    end function eom_cc3_23_trans_aibjajbibm
    function eom_cc3_23_trans_aibjajblbi(a, b, j, l) 
    double precision :: eom_cc3_23_trans_aibjajblbi   
    integer, intent(in) :: a, b, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, l, j)
term(1) = term(1) + tvvov(a, b, l, a)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjajblbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajblbi = eom_cc3_23_trans_aibjajblbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjajblbi
    function eom_cc3_23_trans_aibjakbkbi(b, j, k) 
    double precision :: eom_cc3_23_trans_aibjakbkbi   
    integer, intent(in) :: b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, k, j)



    eom_cc3_23_trans_aibjakbkbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakbkbi = eom_cc3_23_trans_aibjakbkbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbkbi
    function eom_cc3_23_trans_aibjakbibk(b, j, k) 
    double precision :: eom_cc3_23_trans_aibjakbibk   
    integer, intent(in) :: b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, k, j)



    eom_cc3_23_trans_aibjakbibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakbibk = eom_cc3_23_trans_aibjakbibk + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbibk
    function eom_cc3_23_trans_aibiakbibm(i, b, k, m) 
    double precision :: eom_cc3_23_trans_aibiakbibm   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, b, k, i)
term(1) = term(1) + tovoo(k, b, m, i)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibiakbibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiakbibm = eom_cc3_23_trans_aibiakbibm + term(s)
    end do

    end function eom_cc3_23_trans_aibiakbibm
    function eom_cc3_23_trans_aibiakblbi(i, b, k, l) 
    double precision :: eom_cc3_23_trans_aibiakblbi   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, b, k, i)
term(1) = term(1) + tovoo(k, b, l, i)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibiakblbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiakblbi = eom_cc3_23_trans_aibiakblbi + term(s)
    end do

    end function eom_cc3_23_trans_aibiakblbi
    function eom_cc3_23_trans_aibjakbibi(i, b, j, k) 
    double precision :: eom_cc3_23_trans_aibjakbibi   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, i, j)

term(0) = term(0) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjakbibi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjakbibi = eom_cc3_23_trans_aibjakbibi + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbibi
    function eom_cc3_23_trans_aibjakbibj(nocc, a, i, b, j, k) 
    double precision :: eom_cc3_23_trans_aibjakbibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, b, k, n)
term(1) = term(1) + tovoo(k, b, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tvvov(b, b, k, b)
term(3) = term(3) + tov(k, b)
term(4) = term(4) + tovoo(i, b, k, i)
term(5) = term(5) + tovoo(k, b, i, i)
term(6) = term(6) + tovoo(k, b, j, j)
term(7) = term(7) + tvvov(a, a, k, b)
term(8) = term(8) + tvvov(a, b, k, a)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 
term(7) = -term(7) 
term(8) = term(8) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjakbibj = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjakbibj = eom_cc3_23_trans_aibjakbibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbibj
    function eom_cc3_23_trans_aibjakbjbi(nocc, a, i, b, j, k) 
    double precision :: eom_cc3_23_trans_aibjakbjbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, b, k, n)
term(1) = term(1) + tovoo(k, b, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tvvov(b, b, k, b)
term(3) = term(3) + tov(k, b)
term(4) = term(4) + tovoo(i, b, k, i)
term(5) = term(5) + tovoo(k, b, j, j)
term(6) = term(6) + tovoo(k, b, i, i)
term(7) = term(7) + tvvov(a, a, k, b)
term(8) = term(8) + tvvov(a, b, k, a)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 
term(7) = -term(7) 
term(8) = term(8) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjakbjbi = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjakbjbi = eom_cc3_23_trans_aibjakbjbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbjbi
    function eom_cc3_23_trans_aibjakbjbj(i, b, j, k) 
    double precision :: eom_cc3_23_trans_aibjakbjbj   
    integer, intent(in) :: i, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, k, i)
term(1) = term(1) + tovoo(k, b, j, i)

term(0) = term(0) * (-1.9999999999999998d+0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjakbjbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjakbjbj = eom_cc3_23_trans_aibjakbjbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjakbjbj
    function eom_cc3_23_trans_aibiaibiem(nocc, a, i, b, e, m) 
    double precision :: eom_cc3_23_trans_aibiaibiem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, e, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, e, n, n)
term(1) = term(1) + tovoo(n, e, m, n)
end do 

term(0) = term(0) * 3.9999999999999996d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(m, e)
term(3) = term(3) + tovoo(m, e, i, i)
term(4) = term(4) + tovoo(i, e, m, i)
term(5) = term(5) + tvvov(a, a, m, e)
term(6) = term(6) + tvvov(b, b, m, e)
term(7) = term(7) + tvvov(b, e, m, b)
term(8) = term(8) + tvvov(a, e, m, a)

term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-3.9999999999999996d+0) 
term(4) = term(4) * 1.9999999999999998d+0 
term(5) = term(5) * 1.9999999999999998d+0 
term(6) = term(6) * 1.9999999999999998d+0 
term(7) = -term(7) 
term(8) = -term(8) 


    eom_cc3_23_trans_aibiaibiem = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibiaibiem = eom_cc3_23_trans_aibiaibiem + term(s)
    end do

    end function eom_cc3_23_trans_aibiaibiem
    function eom_cc3_23_trans_aibiaiblei(nocc, a, i, b, l, e) 
    double precision :: eom_cc3_23_trans_aibiaiblei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, l, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, l, n)
term(1) = term(1) + tovoo(l, e, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(l, e)
term(3) = term(3) + tovoo(l, e, i, i)
term(4) = term(4) + tovoo(i, e, l, i)
term(5) = term(5) + tvvov(a, a, l, e)
term(6) = term(6) + tvvov(b, b, l, e)
term(7) = term(7) + tvvov(b, e, l, b)
term(8) = term(8) + tvvov(a, e, l, a)

term(2) = -term(2) 
term(3) = term(3) * 1.9999999999999998d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = term(7) * 1.9999999999999998d+0 
term(8) = -term(8) 


    eom_cc3_23_trans_aibiaiblei = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibiaiblei = eom_cc3_23_trans_aibiaiblei + term(s)
    end do

    end function eom_cc3_23_trans_aibiaiblei
    function eom_cc3_23_trans_aibiaiblel(i, l, e) 
    double precision :: eom_cc3_23_trans_aibiaiblel   
    integer, intent(in) :: i, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, l, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibiaiblel = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiaiblel = eom_cc3_23_trans_aibiaiblel + term(s)
    end do

    end function eom_cc3_23_trans_aibiaiblel
    function eom_cc3_23_trans_aibjaibiej(nocc, a, i, b, j, e) 
    double precision :: eom_cc3_23_trans_aibjaibiej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, i, n)
term(1) = term(1) + tovoo(i, e, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tovoo(i, e, i, i)
term(3) = term(3) + tov(i, e)
term(4) = term(4) + tovoo(j, e, i, j)
term(5) = term(5) + tovoo(i, e, j, j)
term(6) = term(6) + tvvov(a, a, i, e)
term(7) = term(7) + tvvov(b, b, i, e)
term(8) = term(8) + tvvov(b, e, i, b)

term(3) = -term(3) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(6) = -term(6) 
term(7) = -term(7) 


    eom_cc3_23_trans_aibjaibiej = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjaibiej = eom_cc3_23_trans_aibjaibiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjaibiej
    function eom_cc3_23_trans_aibjaibjei(nocc, a, i, b, j, e) 
    double precision :: eom_cc3_23_trans_aibjaibjei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e 
    integer :: s ,n 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, i, n)
term(1) = term(1) + tovoo(i, e, n, n)
end do 

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 

term(2) = term(2) + tovoo(i, e, i, i)
term(3) = term(3) + tov(i, e)
term(4) = term(4) + tovoo(i, e, j, j)
term(5) = term(5) + tovoo(j, e, i, j)
term(6) = term(6) + tvvov(a, a, i, e)
term(7) = term(7) + tvvov(a, e, i, a)
term(8) = term(8) + tvvov(b, b, i, e)
term(9) = term(9) + tvvov(b, e, i, b)

term(2) = -term(2) 
term(4) = -term(4) 
term(9) = -term(9) 


    eom_cc3_23_trans_aibjaibjei = 0.d+0
    do s = 0, 9
    eom_cc3_23_trans_aibjaibjei = eom_cc3_23_trans_aibjaibjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjaibjei
    function eom_cc3_23_trans_aibjaibjej(nocc, a, i, b, j, e) 
    double precision :: eom_cc3_23_trans_aibjaibjej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e 
    integer :: s ,n 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, j, n)
term(1) = term(1) + tovoo(j, e, n, n)
end do 

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 

term(2) = term(2) + tovoo(j, e, j, j)
term(3) = term(3) + tov(j, e)
term(4) = term(4) + tovoo(j, e, i, i)
term(5) = term(5) + tovoo(i, e, j, i)
term(6) = term(6) + tvvov(a, a, j, e)
term(7) = term(7) + tvvov(b, b, j, e)
term(8) = term(8) + tvvov(b, e, j, b)
term(9) = term(9) + tvvov(a, e, j, a)

term(2) = -term(2) 
term(4) = -term(4) 
term(9) = -term(9) 


    eom_cc3_23_trans_aibjaibjej = 0.d+0
    do s = 0, 9
    eom_cc3_23_trans_aibjaibjej = eom_cc3_23_trans_aibjaibjej + term(s)
    end do

    end function eom_cc3_23_trans_aibjaibjej
    function eom_cc3_23_trans_aibjajbjei(nocc, a, i, b, j, e) 
    double precision :: eom_cc3_23_trans_aibjajbjei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, j, n)
term(1) = term(1) + tovoo(j, e, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tovoo(j, e, j, j)
term(3) = term(3) + tov(j, e)
term(4) = term(4) + tovoo(i, e, j, i)
term(5) = term(5) + tovoo(j, e, i, i)
term(6) = term(6) + tvvov(a, a, j, e)
term(7) = term(7) + tvvov(a, e, j, a)
term(8) = term(8) + tvvov(b, b, j, e)

term(3) = -term(3) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(6) = -term(6) 
term(8) = -term(8) 


    eom_cc3_23_trans_aibjajbjei = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjajbjei = eom_cc3_23_trans_aibjajbjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjajbjei
    function eom_cc3_23_trans_aibjajbiej(i, b, j, e) 
    double precision :: eom_cc3_23_trans_aibjajbiej   
    integer, intent(in) :: i, b, j, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, j, i)
term(1) = term(1) + tvvov(b, e, j, b)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjajbiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajbiej = eom_cc3_23_trans_aibjajbiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjajbiej
    function eom_cc3_23_trans_aibjajbiei(a, i, j, e) 
    double precision :: eom_cc3_23_trans_aibjajbiei   
    integer, intent(in) :: a, i, j, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, i, j)
term(1) = term(1) + tvvov(a, e, i, a)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjajbiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajbiei = eom_cc3_23_trans_aibjajbiei + term(s)
    end do

    end function eom_cc3_23_trans_aibjajbiei
    function eom_cc3_23_trans_aibiakbkei(i, k, e) 
    double precision :: eom_cc3_23_trans_aibiakbkei   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)

term(0) = term(0) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibiakbkei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiakbkei = eom_cc3_23_trans_aibiakbkei + term(s)
    end do

    end function eom_cc3_23_trans_aibiakbkei
    function eom_cc3_23_trans_aibiakbiek(i, k, e) 
    double precision :: eom_cc3_23_trans_aibiakbiek   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibiakbiek = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiakbiek = eom_cc3_23_trans_aibiakbiek + term(s)
    end do

    end function eom_cc3_23_trans_aibiakbiek
    function eom_cc3_23_trans_aibiakbiei(nocc, a, i, b, k, e) 
    double precision :: eom_cc3_23_trans_aibiakbiei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, k, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, k, n)
term(1) = term(1) + tovoo(k, e, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(k, e)
term(3) = term(3) + tovoo(i, e, k, i)
term(4) = term(4) + tovoo(k, e, i, i)
term(5) = term(5) + tvvov(a, a, k, e)
term(6) = term(6) + tvvov(a, e, k, a)
term(7) = term(7) + tvvov(b, b, k, e)
term(8) = term(8) + tvvov(b, e, k, b)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = term(4) * 1.9999999999999998d+0 
term(5) = -term(5) 
term(6) = term(6) * 1.9999999999999998d+0 
term(7) = -term(7) 
term(8) = -term(8) 


    eom_cc3_23_trans_aibiakbiei = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibiakbiei = eom_cc3_23_trans_aibiakbiei + term(s)
    end do

    end function eom_cc3_23_trans_aibiakbiei
    function eom_cc3_23_trans_aibiaidibm(nocc, a, i, b, d, m) 
    double precision :: eom_cc3_23_trans_aibiaidibm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, d, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, d, n, n)
term(1) = term(1) + tovoo(n, d, m, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(m, d)
term(3) = term(3) + tovoo(m, d, i, i)
term(4) = term(4) + tovoo(i, d, m, i)
term(5) = term(5) + tvvov(a, a, m, d)
term(6) = term(6) + tvvov(b, d, m, b)
term(7) = term(7) + tvvov(a, d, m, a)
term(8) = term(8) + tvvov(b, b, m, d)

term(2) = -term(2) 
term(3) = term(3) * 1.9999999999999998d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = term(6) * 1.9999999999999998d+0 
term(7) = -term(7) 
term(8) = -term(8) 


    eom_cc3_23_trans_aibiaidibm = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibiaidibm = eom_cc3_23_trans_aibiaidibm + term(s)
    end do

    end function eom_cc3_23_trans_aibiaidibm
    function eom_cc3_23_trans_aibiaidlbi(nocc, a, i, b, d, l) 
    double precision :: eom_cc3_23_trans_aibiaidlbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(l, d, n, n)
term(1) = term(1) + tovoo(n, d, l, n)
end do 

term(0) = term(0) * 3.9999999999999996d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(l, d)
term(3) = term(3) + tovoo(l, d, i, i)
term(4) = term(4) + tovoo(i, d, l, i)
term(5) = term(5) + tvvov(a, a, l, d)
term(6) = term(6) + tvvov(b, d, l, b)
term(7) = term(7) + tvvov(a, d, l, a)
term(8) = term(8) + tvvov(b, b, l, d)

term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-3.9999999999999996d+0) 
term(4) = term(4) * 1.9999999999999998d+0 
term(5) = term(5) * 1.9999999999999998d+0 
term(6) = -term(6) 
term(7) = -term(7) 
term(8) = term(8) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibiaidlbi = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibiaidlbi = eom_cc3_23_trans_aibiaidlbi + term(s)
    end do

    end function eom_cc3_23_trans_aibiaidlbi
    function eom_cc3_23_trans_aibiaidlbl(i, d, l) 
    double precision :: eom_cc3_23_trans_aibiaidlbl   
    integer, intent(in) :: i, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, l, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibiaidlbl = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiaidlbl = eom_cc3_23_trans_aibiaidlbl + term(s)
    end do

    end function eom_cc3_23_trans_aibiaidlbl
    function eom_cc3_23_trans_aibjaidibj(nocc, a, i, b, j, d) 
    double precision :: eom_cc3_23_trans_aibjaidibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,n 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, d, i, n)
term(1) = term(1) + tovoo(i, d, n, n)
end do 

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 

term(2) = term(2) + tovoo(i, d, i, i)
term(3) = term(3) + tov(i, d)
term(4) = term(4) + tovoo(i, d, j, j)
term(5) = term(5) + tovoo(j, d, i, j)
term(6) = term(6) + tvvov(a, a, i, d)
term(7) = term(7) + tvvov(a, d, i, a)
term(8) = term(8) + tvvov(b, d, i, b)
term(9) = term(9) + tvvov(b, b, i, d)

term(2) = -term(2) 
term(4) = -term(4) 
term(8) = -term(8) 


    eom_cc3_23_trans_aibjaidibj = 0.d+0
    do s = 0, 9
    eom_cc3_23_trans_aibjaidibj = eom_cc3_23_trans_aibjaidibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidibj
    function eom_cc3_23_trans_aibjaidjbi(nocc, a, i, b, j, d) 
    double precision :: eom_cc3_23_trans_aibjaidjbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(i, d, n, n)
term(1) = term(1) + tovoo(n, d, i, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tovoo(i, d, i, i)
term(3) = term(3) + tov(i, d)
term(4) = term(4) + tovoo(j, d, i, j)
term(5) = term(5) + tovoo(i, d, j, j)
term(6) = term(6) + tvvov(a, a, i, d)
term(7) = term(7) + tvvov(b, d, i, b)
term(8) = term(8) + tvvov(b, b, i, d)

term(3) = -term(3) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(6) = -term(6) 
term(8) = -term(8) 


    eom_cc3_23_trans_aibjaidjbi = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjaidjbi = eom_cc3_23_trans_aibjaidjbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidjbi
    function eom_cc3_23_trans_aibjaidjbj(nocc, a, i, b, j, d) 
    double precision :: eom_cc3_23_trans_aibjaidjbj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,n 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(j, d, n, n)
term(1) = term(1) + tovoo(n, d, j, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 

term(2) = term(2) + tovoo(j, d, j, j)
term(3) = term(3) + tov(j, d)
term(4) = term(4) + tovoo(j, d, i, i)
term(5) = term(5) + tovoo(i, d, j, i)
term(6) = term(6) + tvvov(a, a, j, d)
term(7) = term(7) + tvvov(b, d, j, b)
term(8) = term(8) + tvvov(a, d, j, a)
term(9) = term(9) + tvvov(b, b, j, d)

term(2) = -term(2) 
term(4) = -term(4) 
term(8) = -term(8) 


    eom_cc3_23_trans_aibjaidjbj = 0.d+0
    do s = 0, 9
    eom_cc3_23_trans_aibjaidjbj = eom_cc3_23_trans_aibjaidjbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidjbj
    function eom_cc3_23_trans_aibjajdjbi(i, b, j, d) 
    double precision :: eom_cc3_23_trans_aibjajdjbi   
    integer, intent(in) :: i, b, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, j, i)
term(1) = term(1) + tvvov(b, d, j, b)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjajdjbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajdjbi = eom_cc3_23_trans_aibjajdjbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjajdjbi
    function eom_cc3_23_trans_aibjajdibj(nocc, a, i, b, j, d) 
    double precision :: eom_cc3_23_trans_aibjajdibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, d, j, n)
term(1) = term(1) + tovoo(j, d, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tovoo(j, d, j, j)
term(3) = term(3) + tov(j, d)
term(4) = term(4) + tovoo(i, d, j, i)
term(5) = term(5) + tovoo(j, d, i, i)
term(6) = term(6) + tvvov(a, a, j, d)
term(7) = term(7) + tvvov(a, d, j, a)
term(8) = term(8) + tvvov(b, b, j, d)

term(3) = -term(3) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(6) = -term(6) 
term(8) = -term(8) 


    eom_cc3_23_trans_aibjajdibj = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjajdibj = eom_cc3_23_trans_aibjajdibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjajdibj
    function eom_cc3_23_trans_aibjajdibi(a, i, j, d) 
    double precision :: eom_cc3_23_trans_aibjajdibi   
    integer, intent(in) :: a, i, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, i, j)
term(1) = term(1) + tvvov(a, d, i, a)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjajdibi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajdibi = eom_cc3_23_trans_aibjajdibi + term(s)
    end do

    end function eom_cc3_23_trans_aibjajdibi
    function eom_cc3_23_trans_aibiakdkbi(i, k, d) 
    double precision :: eom_cc3_23_trans_aibiakdkbi   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibiakdkbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiakdkbi = eom_cc3_23_trans_aibiakdkbi + term(s)
    end do

    end function eom_cc3_23_trans_aibiakdkbi
    function eom_cc3_23_trans_aibiakdibk(i, k, d) 
    double precision :: eom_cc3_23_trans_aibiakdibk   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, i)

term(0) = term(0) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibiakdibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiakdibk = eom_cc3_23_trans_aibiakdibk + term(s)
    end do

    end function eom_cc3_23_trans_aibiakdibk
    function eom_cc3_23_trans_aibiakdibi(nocc, a, i, b, k, d) 
    double precision :: eom_cc3_23_trans_aibiakdibi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, d, k, n)
term(1) = term(1) + tovoo(k, d, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(k, d)
term(3) = term(3) + tovoo(i, d, k, i)
term(4) = term(4) + tovoo(k, d, i, i)
term(5) = term(5) + tvvov(a, a, k, d)
term(6) = term(6) + tvvov(a, d, k, a)
term(7) = term(7) + tvvov(b, d, k, b)
term(8) = term(8) + tvvov(b, b, k, d)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = term(4) * 1.9999999999999998d+0 
term(5) = -term(5) 
term(6) = term(6) * 1.9999999999999998d+0 
term(7) = -term(7) 
term(8) = -term(8) 


    eom_cc3_23_trans_aibiakdibi = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibiakdibi = eom_cc3_23_trans_aibiakdibi + term(s)
    end do

    end function eom_cc3_23_trans_aibiakdibi
    function eom_cc3_23_trans_aibiaididm(b, d, m) 
    double precision :: eom_cc3_23_trans_aibiaididm   
    integer, intent(in) :: b, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, m, d)



    eom_cc3_23_trans_aibiaididm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiaididm = eom_cc3_23_trans_aibiaididm + term(s)
    end do

    end function eom_cc3_23_trans_aibiaididm
    function eom_cc3_23_trans_aibiaidldi(b, d, l) 
    double precision :: eom_cc3_23_trans_aibiaidldi   
    integer, intent(in) :: b, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, l, d)



    eom_cc3_23_trans_aibiaidldi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiaidldi = eom_cc3_23_trans_aibiaidldi + term(s)
    end do

    end function eom_cc3_23_trans_aibiaidldi
    function eom_cc3_23_trans_aibjaidjdj(b, j, d) 
    double precision :: eom_cc3_23_trans_aibjaidjdj   
    integer, intent(in) :: b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, j, d)

term(0) = term(0) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aibjaidjdj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjaidjdj = eom_cc3_23_trans_aibjaidjdj + term(s)
    end do

    end function eom_cc3_23_trans_aibjaidjdj
    function eom_cc3_23_trans_aibjajdjdi(b, j, d) 
    double precision :: eom_cc3_23_trans_aibjajdjdi   
    integer, intent(in) :: b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, j, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjajdjdi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjajdjdi = eom_cc3_23_trans_aibjajdjdi + term(s)
    end do

    end function eom_cc3_23_trans_aibjajdjdi
    function eom_cc3_23_trans_aibjajdidj(b, j, d) 
    double precision :: eom_cc3_23_trans_aibjajdidj   
    integer, intent(in) :: b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, j, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjajdidj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjajdidj = eom_cc3_23_trans_aibjajdidj + term(s)
    end do

    end function eom_cc3_23_trans_aibjajdidj
    function eom_cc3_23_trans_aibiakdidi(b, k, d) 
    double precision :: eom_cc3_23_trans_aibiakdidi   
    integer, intent(in) :: b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, k, d)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibiakdidi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiakdidi = eom_cc3_23_trans_aibiakdidi + term(s)
    end do

    end function eom_cc3_23_trans_aibiakdidi
    function eom_cc3_23_trans_aibibibiem(a, b, e, m) 
    double precision :: eom_cc3_23_trans_aibibibiem   
    integer, intent(in) :: a, b, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, m, e)
term(1) = term(1) + tvvov(a, e, m, b)

term(0) = term(0) * 3.9999999999999996d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibibibiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibibibiem = eom_cc3_23_trans_aibibibiem + term(s)
    end do

    end function eom_cc3_23_trans_aibibibiem
    function eom_cc3_23_trans_aibibiblei(a, b, l, e) 
    double precision :: eom_cc3_23_trans_aibibiblei   
    integer, intent(in) :: a, b, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, l, e)
term(1) = term(1) + tvvov(a, e, l, b)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibibiblei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibibiblei = eom_cc3_23_trans_aibibiblei + term(s)
    end do

    end function eom_cc3_23_trans_aibibiblei
    function eom_cc3_23_trans_aibjbibiej(a, i, b, e) 
    double precision :: eom_cc3_23_trans_aibjbibiej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, i, e)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjbibiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbibiej = eom_cc3_23_trans_aibjbibiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjbibiej
    function eom_cc3_23_trans_aibjbibjei(a, i, b, e) 
    double precision :: eom_cc3_23_trans_aibjbibjei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, i, e)



    eom_cc3_23_trans_aibjbibjei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbibjei = eom_cc3_23_trans_aibjbibjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjbibjei
    function eom_cc3_23_trans_aibjbibjej(a, b, j, e) 
    double precision :: eom_cc3_23_trans_aibjbibjej   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, j, e)
term(1) = term(1) + tvvov(a, e, j, b)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjbibjej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjbibjej = eom_cc3_23_trans_aibjbibjej + term(s)
    end do

    end function eom_cc3_23_trans_aibjbibjej
    function eom_cc3_23_trans_aibjbjbjei(a, b, j, e) 
    double precision :: eom_cc3_23_trans_aibjbjbjei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, j, e)
term(1) = term(1) + tvvov(a, e, j, b)

term(0) = term(0) * (-1.9999999999999998d+0) 
term(1) = term(1) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aibjbjbjei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjbjbjei = eom_cc3_23_trans_aibjbjbjei + term(s)
    end do

    end function eom_cc3_23_trans_aibjbjbjei
    function eom_cc3_23_trans_aibjbjbiej(a, b, j, e) 
    double precision :: eom_cc3_23_trans_aibjbjbiej   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, j, e)
term(1) = term(1) + tvvov(a, e, j, b)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjbjbiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjbjbiej = eom_cc3_23_trans_aibjbjbiej + term(s)
    end do

    end function eom_cc3_23_trans_aibjbjbiej
    function eom_cc3_23_trans_aibjbjbiei(a, i, b, e) 
    double precision :: eom_cc3_23_trans_aibjbjbiei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, i, e)



    eom_cc3_23_trans_aibjbjbiei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbjbiei = eom_cc3_23_trans_aibjbjbiei + term(s)
    end do

    end function eom_cc3_23_trans_aibjbjbiei
    function eom_cc3_23_trans_aibibkbiei(a, b, k, e) 
    double precision :: eom_cc3_23_trans_aibibkbiei   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, k, e)
term(1) = term(1) + tvvov(a, e, k, b)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibibkbiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibibkbiei = eom_cc3_23_trans_aibibkbiei + term(s)
    end do

    end function eom_cc3_23_trans_aibibkbiei
    function eom_cc3_23_trans_aibibididm(a, d, m) 
    double precision :: eom_cc3_23_trans_aibibididm   
    integer, intent(in) :: a, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, d)



    eom_cc3_23_trans_aibibididm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibibididm = eom_cc3_23_trans_aibibididm + term(s)
    end do

    end function eom_cc3_23_trans_aibibididm
    function eom_cc3_23_trans_aibibidldi(a, d, l) 
    double precision :: eom_cc3_23_trans_aibibidldi   
    integer, intent(in) :: a, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, d)



    eom_cc3_23_trans_aibibidldi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibibidldi = eom_cc3_23_trans_aibibidldi + term(s)
    end do

    end function eom_cc3_23_trans_aibibidldi
    function eom_cc3_23_trans_aibjbididj(a, i, d) 
    double precision :: eom_cc3_23_trans_aibjbididj   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjbididj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbididj = eom_cc3_23_trans_aibjbididj + term(s)
    end do

    end function eom_cc3_23_trans_aibjbididj
    function eom_cc3_23_trans_aibjbidjdi(a, i, d) 
    double precision :: eom_cc3_23_trans_aibjbidjdi   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjbidjdi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbidjdi = eom_cc3_23_trans_aibjbidjdi + term(s)
    end do

    end function eom_cc3_23_trans_aibjbidjdi
    function eom_cc3_23_trans_aibjbjdidi(a, i, d) 
    double precision :: eom_cc3_23_trans_aibjbjdidi   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, d)

term(0) = term(0) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aibjbjdidi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjbjdidi = eom_cc3_23_trans_aibjbjdidi + term(s)
    end do

    end function eom_cc3_23_trans_aibjbjdidi
    function eom_cc3_23_trans_aibibkdidi(a, k, d) 
    double precision :: eom_cc3_23_trans_aibibkdidi   
    integer, intent(in) :: a, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, k, d)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibibkdidi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibibkdidi = eom_cc3_23_trans_aibibkdidi + term(s)
    end do

    end function eom_cc3_23_trans_aibibkdidi
    function eom_cc3_23_trans_aiajcicjam(a, c, m) 
    double precision :: eom_cc3_23_trans_aiajcicjam   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, c)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajcicjam = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajcicjam = eom_cc3_23_trans_aiajcicjam + term(s)
    end do

    end function eom_cc3_23_trans_aiajcicjam
    function eom_cc3_23_trans_aiajciclaj(a, c, l) 
    double precision :: eom_cc3_23_trans_aiajciclaj   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, c)



    eom_cc3_23_trans_aiajciclaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajciclaj = eom_cc3_23_trans_aiajciclaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajciclaj
    function eom_cc3_23_trans_aiajcjclai(a, c, l) 
    double precision :: eom_cc3_23_trans_aiajcjclai   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, c)



    eom_cc3_23_trans_aiajcjclai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajcjclai = eom_cc3_23_trans_aiajcjclai + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjclai
    function eom_cc3_23_trans_aiajckciaj(a, c, k) 
    double precision :: eom_cc3_23_trans_aiajckciaj   
    integer, intent(in) :: a, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, c)



    eom_cc3_23_trans_aiajckciaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajckciaj = eom_cc3_23_trans_aiajckciaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajckciaj
    function eom_cc3_23_trans_aiajckcjai(a, c, k) 
    double precision :: eom_cc3_23_trans_aiajckcjai   
    integer, intent(in) :: a, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, c)



    eom_cc3_23_trans_aiajckcjai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajckcjai = eom_cc3_23_trans_aiajckcjai + term(s)
    end do

    end function eom_cc3_23_trans_aiajckcjai
    function eom_cc3_23_trans_aibiciciam(b, c, m) 
    double precision :: eom_cc3_23_trans_aibiciciam   
    integer, intent(in) :: b, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, c)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibiciciam = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiciciam = eom_cc3_23_trans_aibiciciam + term(s)
    end do

    end function eom_cc3_23_trans_aibiciciam
    function eom_cc3_23_trans_aibiciclai(b, c, l) 
    double precision :: eom_cc3_23_trans_aibiciclai   
    integer, intent(in) :: b, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, c)



    eom_cc3_23_trans_aibiciclai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiciclai = eom_cc3_23_trans_aibiciclai + term(s)
    end do

    end function eom_cc3_23_trans_aibiciclai
    function eom_cc3_23_trans_aibjcicjaj(b, j, c) 
    double precision :: eom_cc3_23_trans_aibjcicjaj   
    integer, intent(in) :: b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcicjaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcicjaj = eom_cc3_23_trans_aibjcicjaj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcicjaj
    function eom_cc3_23_trans_aibjcjcjai(b, j, c) 
    double precision :: eom_cc3_23_trans_aibjcjcjai   
    integer, intent(in) :: b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, c)

term(0) = term(0) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aibjcjcjai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjcjai = eom_cc3_23_trans_aibjcjcjai + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjcjai
    function eom_cc3_23_trans_aibjcjciaj(b, j, c) 
    double precision :: eom_cc3_23_trans_aibjcjciaj   
    integer, intent(in) :: b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjciaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjciaj = eom_cc3_23_trans_aibjcjciaj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjciaj
    function eom_cc3_23_trans_aibickciai(b, c, k) 
    double precision :: eom_cc3_23_trans_aibickciai   
    integer, intent(in) :: b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, k, c)



    eom_cc3_23_trans_aibickciai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibickciai = eom_cc3_23_trans_aibickciai + term(s)
    end do

    end function eom_cc3_23_trans_aibickciai
    function eom_cc3_23_trans_aibicicibm(a, c, m) 
    double precision :: eom_cc3_23_trans_aibicicibm   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, c)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibicicibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibicicibm = eom_cc3_23_trans_aibicicibm + term(s)
    end do

    end function eom_cc3_23_trans_aibicicibm
    function eom_cc3_23_trans_aibiciclbi(a, c, l) 
    double precision :: eom_cc3_23_trans_aibiciclbi   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, c)



    eom_cc3_23_trans_aibiciclbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiciclbi = eom_cc3_23_trans_aibiciclbi + term(s)
    end do

    end function eom_cc3_23_trans_aibiciclbi
    function eom_cc3_23_trans_aibjcicibj(a, i, c) 
    double precision :: eom_cc3_23_trans_aibjcicibj   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, c)

term(0) = term(0) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aibjcicibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcicibj = eom_cc3_23_trans_aibjcicibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcicibj
    function eom_cc3_23_trans_aibjcicjbi(a, i, c) 
    double precision :: eom_cc3_23_trans_aibjcicjbi   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcicjbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcicjbi = eom_cc3_23_trans_aibjcicjbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjcicjbi
    function eom_cc3_23_trans_aibjcjcibi(a, i, c) 
    double precision :: eom_cc3_23_trans_aibjcjcibi   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjcibi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjcibi = eom_cc3_23_trans_aibjcjcibi + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjcibi
    function eom_cc3_23_trans_aibickcibi(a, c, k) 
    double precision :: eom_cc3_23_trans_aibickcibi   
    integer, intent(in) :: a, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, c)



    eom_cc3_23_trans_aibickcibi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibickcibi = eom_cc3_23_trans_aibickcibi + term(s)
    end do

    end function eom_cc3_23_trans_aibickcibi
    function eom_cc3_23_trans_aiaicialam(i, c, l, m) 
    double precision :: eom_cc3_23_trans_aiaicialam   
    integer, intent(in) :: i, c, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, m, i)
term(1) = term(1) + tovoo(m, c, l, i)



    eom_cc3_23_trans_aiaicialam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaicialam = eom_cc3_23_trans_aiaicialam + term(s)
    end do

    end function eom_cc3_23_trans_aiaicialam
    function eom_cc3_23_trans_aiajciaiam(i, j, c, m) 
    double precision :: eom_cc3_23_trans_aiajciaiam   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, c, m, j)
term(1) = term(1) + tovoo(m, c, i, j)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajciaiam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajciaiam = eom_cc3_23_trans_aiajciaiam + term(s)
    end do

    end function eom_cc3_23_trans_aiajciaiam
    function eom_cc3_23_trans_aiajcialai(i, j, c, l) 
    double precision :: eom_cc3_23_trans_aiajcialai   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, c, l, j)
term(1) = term(1) + tovoo(l, c, i, j)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajcialai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcialai = eom_cc3_23_trans_aiajcialai + term(s)
    end do

    end function eom_cc3_23_trans_aiajcialai
    function eom_cc3_23_trans_aiajciajam(nocc, a, i, j, c, m) 
    double precision :: eom_cc3_23_trans_aiajciajam 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, c, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, c, n, n)
term(1) = term(1) + tovoo(n, c, m, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(m, c)
term(3) = term(3) + tvvov(a, c, m, a)
term(4) = term(4) + tvvov(a, a, m, c)
term(5) = term(5) + tovoo(i, c, m, i)
term(6) = term(6) + tovoo(m, c, i, i)
term(7) = term(7) + tovoo(j, c, m, j)
term(8) = term(8) + tovoo(m, c, j, j)

term(2) = -term(2) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(5) = term(5) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajciajam = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aiajciajam = eom_cc3_23_trans_aiajciajam + term(s)
    end do

    end function eom_cc3_23_trans_aiajciajam
    function eom_cc3_23_trans_aiajcialaj(nocc, a, i, j, c, l) 
    double precision :: eom_cc3_23_trans_aiajcialaj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(l, c, n, n)
term(1) = term(1) + tovoo(n, c, l, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(l, c)
term(3) = term(3) + tvvov(a, c, l, a)
term(4) = term(4) + tvvov(a, a, l, c)
term(5) = term(5) + tovoo(i, c, l, i)
term(6) = term(6) + tovoo(l, c, i, i)
term(7) = term(7) + tovoo(l, c, j, j)
term(8) = term(8) + tovoo(j, c, l, j)

term(2) = -term(2) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(5) = term(5) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajcialaj = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aiajcialaj = eom_cc3_23_trans_aiajcialaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajcialaj
    function eom_cc3_23_trans_aiajcialal(j, c, l) 
    double precision :: eom_cc3_23_trans_aiajcialal   
    integer, intent(in) :: j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, l, j)

term(0) = term(0) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajcialal = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajcialal = eom_cc3_23_trans_aiajcialal + term(s)
    end do

    end function eom_cc3_23_trans_aiajcialal
    function eom_cc3_23_trans_aiajcjajam(i, j, c, m) 
    double precision :: eom_cc3_23_trans_aiajcjajam   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, c, m, i)
term(1) = term(1) + tovoo(m, c, j, i)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajcjajam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcjajam = eom_cc3_23_trans_aiajcjajam + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjajam
    function eom_cc3_23_trans_aiajcjalaj(i, j, c, l) 
    double precision :: eom_cc3_23_trans_aiajcjalaj   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, c, l, i)
term(1) = term(1) + tovoo(l, c, j, i)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajcjalaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcjalaj = eom_cc3_23_trans_aiajcjalaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjalaj
    function eom_cc3_23_trans_aiajcjaiam(nocc, a, i, j, c, m) 
    double precision :: eom_cc3_23_trans_aiajcjaiam 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, c, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, c, n, n)
term(1) = term(1) + tovoo(n, c, m, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(m, c)
term(3) = term(3) + tvvov(a, c, m, a)
term(4) = term(4) + tvvov(a, a, m, c)
term(5) = term(5) + tovoo(j, c, m, j)
term(6) = term(6) + tovoo(m, c, j, j)
term(7) = term(7) + tovoo(i, c, m, i)
term(8) = term(8) + tovoo(m, c, i, i)

term(2) = -term(2) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(5) = term(5) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajcjaiam = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aiajcjaiam = eom_cc3_23_trans_aiajcjaiam + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjaiam
    function eom_cc3_23_trans_aiajcjalai(nocc, a, i, j, c, l) 
    double precision :: eom_cc3_23_trans_aiajcjalai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(l, c, n, n)
term(1) = term(1) + tovoo(n, c, l, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(l, c)
term(3) = term(3) + tvvov(a, c, l, a)
term(4) = term(4) + tvvov(a, a, l, c)
term(5) = term(5) + tovoo(j, c, l, j)
term(6) = term(6) + tovoo(l, c, j, j)
term(7) = term(7) + tovoo(l, c, i, i)
term(8) = term(8) + tovoo(i, c, l, i)

term(2) = -term(2) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(5) = term(5) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajcjalai = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aiajcjalai = eom_cc3_23_trans_aiajcjalai + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjalai
    function eom_cc3_23_trans_aiajcjalal(i, c, l) 
    double precision :: eom_cc3_23_trans_aiajcjalal   
    integer, intent(in) :: i, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, l, i)

term(0) = term(0) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajcjalal = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajcjalal = eom_cc3_23_trans_aiajcjalal + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjalal
    function eom_cc3_23_trans_aiajckakai(j, c, k) 
    double precision :: eom_cc3_23_trans_aiajckakai   
    integer, intent(in) :: j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aiajckakai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajckakai = eom_cc3_23_trans_aiajckakai + term(s)
    end do

    end function eom_cc3_23_trans_aiajckakai
    function eom_cc3_23_trans_aiajckakaj(i, c, k) 
    double precision :: eom_cc3_23_trans_aiajckakaj   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aiajckakaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajckakaj = eom_cc3_23_trans_aiajckakaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajckakaj
    function eom_cc3_23_trans_aiajckaiak(j, c, k) 
    double precision :: eom_cc3_23_trans_aiajckaiak   
    integer, intent(in) :: j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, j)

term(0) = -term(0) 


    eom_cc3_23_trans_aiajckaiak = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajckaiak = eom_cc3_23_trans_aiajckaiak + term(s)
    end do

    end function eom_cc3_23_trans_aiajckaiak
    function eom_cc3_23_trans_aiajckajak(i, c, k) 
    double precision :: eom_cc3_23_trans_aiajckajak   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aiajckajak = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajckajak = eom_cc3_23_trans_aiajckajak + term(s)
    end do

    end function eom_cc3_23_trans_aiajckajak
    function eom_cc3_23_trans_aiaickaiam(i, c, k, m) 
    double precision :: eom_cc3_23_trans_aiaickaiam   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, m, i)
term(1) = term(1) + tovoo(m, c, k, i)

term(0) = term(0) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aiaickaiam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaickaiam = eom_cc3_23_trans_aiaickaiam + term(s)
    end do

    end function eom_cc3_23_trans_aiaickaiam
    function eom_cc3_23_trans_aiaickalai(i, c, k, l) 
    double precision :: eom_cc3_23_trans_aiaickalai   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, l, i)
term(1) = term(1) + tovoo(l, c, k, i)

term(0) = term(0) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aiaickalai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaickalai = eom_cc3_23_trans_aiaickalai + term(s)
    end do

    end function eom_cc3_23_trans_aiaickalai
    function eom_cc3_23_trans_aiajckaiai(i, j, c, k) 
    double precision :: eom_cc3_23_trans_aiajckaiai   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, i, j)
term(1) = term(1) + tovoo(i, c, k, j)

term(0) = term(0) * (-3.9999999999999996d+0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajckaiai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajckaiai = eom_cc3_23_trans_aiajckaiai + term(s)
    end do

    end function eom_cc3_23_trans_aiajckaiai
    function eom_cc3_23_trans_aiajckaiaj(nocc, a, i, j, c, k) 
    double precision :: eom_cc3_23_trans_aiajckaiaj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(k, c, n, n)
term(1) = term(1) + tovoo(n, c, k, n)
end do 

term(0) = term(0) * 3.9999999999999996d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(k, c)
term(3) = term(3) + tvvov(a, c, k, a)
term(4) = term(4) + tvvov(a, a, k, c)
term(5) = term(5) + tovoo(k, c, i, i)
term(6) = term(6) + tovoo(k, c, j, j)
term(7) = term(7) + tovoo(i, c, k, i)
term(8) = term(8) + tovoo(j, c, k, j)

term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-1.9999999999999998d+0) 
term(4) = term(4) * 3.9999999999999996d+0 
term(5) = term(5) * (-1.9999999999999998d+0) 
term(6) = term(6) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajckaiaj = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aiajckaiaj = eom_cc3_23_trans_aiajckaiaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajckaiaj
    function eom_cc3_23_trans_aiajckajaj(i, j, c, k) 
    double precision :: eom_cc3_23_trans_aiajckajaj   
    integer, intent(in) :: i, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, j, i)
term(1) = term(1) + tovoo(j, c, k, i)

term(0) = term(0) * (-3.9999999999999996d+0) 
term(1) = term(1) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajckajaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajckajaj = eom_cc3_23_trans_aiajckajaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajckajaj
    function eom_cc3_23_trans_aiaiciaiem(a, c, e, m) 
    double precision :: eom_cc3_23_trans_aiaiciaiem   
    integer, intent(in) :: a, c, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, e)
term(1) = term(1) + tvvov(a, e, m, c)

term(0) = term(0) * 1.9999999999999996d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aiaiciaiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaiciaiem = eom_cc3_23_trans_aiaiciaiem + term(s)
    end do

    end function eom_cc3_23_trans_aiaiciaiem
    function eom_cc3_23_trans_aiaicialei(a, c, l, e) 
    double precision :: eom_cc3_23_trans_aiaicialei   
    integer, intent(in) :: a, c, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, e)
term(1) = term(1) + tvvov(a, e, l, c)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aiaicialei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaicialei = eom_cc3_23_trans_aiaicialei + term(s)
    end do

    end function eom_cc3_23_trans_aiaicialei
    function eom_cc3_23_trans_aiajciaiej(a, i, c, e) 
    double precision :: eom_cc3_23_trans_aiajciaiej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, e)
term(1) = term(1) + tvvov(a, e, i, c)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajciaiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajciaiej = eom_cc3_23_trans_aiajciaiej + term(s)
    end do

    end function eom_cc3_23_trans_aiajciaiej
    function eom_cc3_23_trans_aiajciajei(a, i, c, e) 
    double precision :: eom_cc3_23_trans_aiajciajei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, e)
term(1) = term(1) + tvvov(a, e, i, c)



    eom_cc3_23_trans_aiajciajei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajciajei = eom_cc3_23_trans_aiajciajei + term(s)
    end do

    end function eom_cc3_23_trans_aiajciajei
    function eom_cc3_23_trans_aiajciajej(a, j, c, e) 
    double precision :: eom_cc3_23_trans_aiajciajej   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, e)
term(1) = term(1) + tvvov(a, e, j, c)

term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajciajej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajciajej = eom_cc3_23_trans_aiajciajej + term(s)
    end do

    end function eom_cc3_23_trans_aiajciajej
    function eom_cc3_23_trans_aiajcjajei(a, j, c, e) 
    double precision :: eom_cc3_23_trans_aiajcjajei   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, e)
term(1) = term(1) + tvvov(a, e, j, c)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajcjajei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcjajei = eom_cc3_23_trans_aiajcjajei + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjajei
    function eom_cc3_23_trans_aiajcjaiej(a, j, c, e) 
    double precision :: eom_cc3_23_trans_aiajcjaiej   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, e)
term(1) = term(1) + tvvov(a, e, j, c)



    eom_cc3_23_trans_aiajcjaiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcjaiej = eom_cc3_23_trans_aiajcjaiej + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjaiej
    function eom_cc3_23_trans_aiajcjaiei(a, i, c, e) 
    double precision :: eom_cc3_23_trans_aiajcjaiei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, e)
term(1) = term(1) + tvvov(a, e, i, c)

term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajcjaiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcjaiei = eom_cc3_23_trans_aiajcjaiei + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjaiei
    function eom_cc3_23_trans_aiaickaiei(a, c, k, e) 
    double precision :: eom_cc3_23_trans_aiaickaiei   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, e)
term(1) = term(1) + tvvov(a, e, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiaickaiei = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaickaiei = eom_cc3_23_trans_aiaickaiei + term(s)
    end do

    end function eom_cc3_23_trans_aiaickaiei
    function eom_cc3_23_trans_aiaicidiam(a, c, d, m) 
    double precision :: eom_cc3_23_trans_aiaicidiam   
    integer, intent(in) :: a, c, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, d)
term(1) = term(1) + tvvov(a, d, m, c)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_trans_aiaicidiam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaicidiam = eom_cc3_23_trans_aiaicidiam + term(s)
    end do

    end function eom_cc3_23_trans_aiaicidiam
    function eom_cc3_23_trans_aiaicidlai(a, c, d, l) 
    double precision :: eom_cc3_23_trans_aiaicidlai   
    integer, intent(in) :: a, c, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, d)
term(1) = term(1) + tvvov(a, d, l, c)

term(0) = term(0) * 1.9999999999999996d+0 
term(1) = -term(1) 


    eom_cc3_23_trans_aiaicidlai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaicidlai = eom_cc3_23_trans_aiaicidlai + term(s)
    end do

    end function eom_cc3_23_trans_aiaicidlai
    function eom_cc3_23_trans_aiajcidiaj(a, i, c, d) 
    double precision :: eom_cc3_23_trans_aiajcidiaj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, d)
term(1) = term(1) + tvvov(a, d, i, c)



    eom_cc3_23_trans_aiajcidiaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcidiaj = eom_cc3_23_trans_aiajcidiaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajcidiaj
    function eom_cc3_23_trans_aiajcidjai(a, i, c, d) 
    double precision :: eom_cc3_23_trans_aiajcidjai   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, d)
term(1) = term(1) + tvvov(a, d, i, c)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajcidjai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcidjai = eom_cc3_23_trans_aiajcidjai + term(s)
    end do

    end function eom_cc3_23_trans_aiajcidjai
    function eom_cc3_23_trans_aiajcidjaj(a, j, c, d) 
    double precision :: eom_cc3_23_trans_aiajcidjaj   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, d)
term(1) = term(1) + tvvov(a, d, j, c)

term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajcidjaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcidjaj = eom_cc3_23_trans_aiajcidjaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajcidjaj
    function eom_cc3_23_trans_aiajcjdjai(a, j, c, d) 
    double precision :: eom_cc3_23_trans_aiajcjdjai   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, d)
term(1) = term(1) + tvvov(a, d, j, c)



    eom_cc3_23_trans_aiajcjdjai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcjdjai = eom_cc3_23_trans_aiajcjdjai + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjdjai
    function eom_cc3_23_trans_aiajcjdiaj(a, j, c, d) 
    double precision :: eom_cc3_23_trans_aiajcjdiaj   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, d)
term(1) = term(1) + tvvov(a, d, j, c)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajcjdiaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcjdiaj = eom_cc3_23_trans_aiajcjdiaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjdiaj
    function eom_cc3_23_trans_aiajcjdiai(a, i, c, d) 
    double precision :: eom_cc3_23_trans_aiajcjdiai   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, d)
term(1) = term(1) + tvvov(a, d, i, c)

term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aiajcjdiai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiajcjdiai = eom_cc3_23_trans_aiajcjdiai + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjdiai
    function eom_cc3_23_trans_aiaickdiai(a, c, k, d) 
    double precision :: eom_cc3_23_trans_aiaickdiai   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, d)
term(1) = term(1) + tvvov(a, d, k, c)

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiaickdiai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aiaickdiai = eom_cc3_23_trans_aiaickdiai + term(s)
    end do

    end function eom_cc3_23_trans_aiaickdiai
    function eom_cc3_23_trans_aibiciaiam(a, b, c, m) 
    double precision :: eom_cc3_23_trans_aibiciaiam   
    integer, intent(in) :: a, b, c, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, a)
term(1) = term(1) + tvvov(b, a, m, c)

term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibiciaiam = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiciaiam = eom_cc3_23_trans_aibiciaiam + term(s)
    end do

    end function eom_cc3_23_trans_aibiciaiam
    function eom_cc3_23_trans_aibicialai(a, b, c, l) 
    double precision :: eom_cc3_23_trans_aibicialai   
    integer, intent(in) :: a, b, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, a)
term(1) = term(1) + tvvov(b, a, l, c)

term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibicialai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibicialai = eom_cc3_23_trans_aibicialai + term(s)
    end do

    end function eom_cc3_23_trans_aibicialai
    function eom_cc3_23_trans_aibjciaiaj(a, i, b, c) 
    double precision :: eom_cc3_23_trans_aibjciaiaj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, i, a)
term(1) = term(1) + tvvov(b, a, i, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjciaiaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjciaiaj = eom_cc3_23_trans_aibjciaiaj + term(s)
    end do

    end function eom_cc3_23_trans_aibjciaiaj
    function eom_cc3_23_trans_aibjciajai(a, i, b, c) 
    double precision :: eom_cc3_23_trans_aibjciajai   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, i, a)
term(1) = term(1) + tvvov(b, a, i, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjciajai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjciajai = eom_cc3_23_trans_aibjciajai + term(s)
    end do

    end function eom_cc3_23_trans_aibjciajai
    function eom_cc3_23_trans_aibjciajaj(a, b, j, c) 
    double precision :: eom_cc3_23_trans_aibjciajaj   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, j, c)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjciajaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjciajaj = eom_cc3_23_trans_aibjciajaj + term(s)
    end do

    end function eom_cc3_23_trans_aibjciajaj
    function eom_cc3_23_trans_aibjcjajai(a, b, j, c) 
    double precision :: eom_cc3_23_trans_aibjcjajai   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, j, c)



    eom_cc3_23_trans_aibjcjajai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjajai = eom_cc3_23_trans_aibjcjajai + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjajai
    function eom_cc3_23_trans_aibjcjaiaj(a, b, j, c) 
    double precision :: eom_cc3_23_trans_aibjcjaiaj   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, j, c)



    eom_cc3_23_trans_aibjcjaiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjaiaj = eom_cc3_23_trans_aibjcjaiaj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjaiaj
    function eom_cc3_23_trans_aibjcjaiai(a, i, b, c) 
    double precision :: eom_cc3_23_trans_aibjcjaiai   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, i, a)
term(1) = term(1) + tvvov(b, a, i, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjcjaiai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjaiai = eom_cc3_23_trans_aibjcjaiai + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjaiai
    function eom_cc3_23_trans_aibickaiai(a, b, c, k) 
    double precision :: eom_cc3_23_trans_aibickaiai   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, k, a)
term(1) = term(1) + tvvov(b, a, k, c)

term(0) = term(0) * (-1.9999999999999998d+0) 
term(1) = term(1) * 3.9999999999999996d+0 


    eom_cc3_23_trans_aibickaiai = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibickaiai = eom_cc3_23_trans_aibickaiai + term(s)
    end do

    end function eom_cc3_23_trans_aibickaiai
    function eom_cc3_23_trans_aibiciaibm(nocc, a, i, b, c, m) 
    double precision :: eom_cc3_23_trans_aibiciaibm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, c, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, c, n, n)
term(1) = term(1) + tovoo(n, c, m, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(m, c)
term(3) = term(3) + tvvov(a, c, m, a)
term(4) = term(4) + tvvov(b, c, m, b)
term(5) = term(5) + tvvov(a, a, m, c)
term(6) = term(6) + tvvov(b, b, m, c)
term(7) = term(7) + tovoo(i, c, m, i)
term(8) = term(8) + tovoo(m, c, i, i)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = term(4) * 1.9999999999999998d+0 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = -term(7) 
term(8) = term(8) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibiciaibm = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibiciaibm = eom_cc3_23_trans_aibiciaibm + term(s)
    end do

    end function eom_cc3_23_trans_aibiciaibm
    function eom_cc3_23_trans_aibicialbi(nocc, a, i, b, c, l) 
    double precision :: eom_cc3_23_trans_aibicialbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(l, c, n, n)
term(1) = term(1) + tovoo(n, c, l, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(l, c)
term(3) = term(3) + tvvov(a, c, l, a)
term(4) = term(4) + tvvov(b, c, l, b)
term(5) = term(5) + tvvov(a, a, l, c)
term(6) = term(6) + tvvov(b, b, l, c)
term(7) = term(7) + tovoo(i, c, l, i)
term(8) = term(8) + tovoo(l, c, i, i)

term(2) = -term(2) 
term(3) = term(3) * 1.9999999999999998d+0 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = -term(7) 
term(8) = term(8) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibicialbi = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibicialbi = eom_cc3_23_trans_aibicialbi + term(s)
    end do

    end function eom_cc3_23_trans_aibicialbi
    function eom_cc3_23_trans_aibicialbl(i, c, l) 
    double precision :: eom_cc3_23_trans_aibicialbl   
    integer, intent(in) :: i, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, l, i)

term(0) = term(0) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibicialbl = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibicialbl = eom_cc3_23_trans_aibicialbl + term(s)
    end do

    end function eom_cc3_23_trans_aibicialbl
    function eom_cc3_23_trans_aibjciaibj(nocc, a, i, b, j, c) 
    double precision :: eom_cc3_23_trans_aibjciaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,n 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(i, c, n, n)
term(1) = term(1) + tovoo(n, c, i, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 

term(2) = term(2) + tovoo(i, c, i, i)
term(3) = term(3) + tov(i, c)
term(4) = term(4) + tvvov(a, c, i, a)
term(5) = term(5) + tvvov(b, c, i, b)
term(6) = term(6) + tvvov(a, a, i, c)
term(7) = term(7) + tvvov(b, b, i, c)
term(8) = term(8) + tovoo(i, c, j, j)
term(9) = term(9) + tovoo(j, c, i, j)

term(2) = -term(2) 
term(5) = -term(5) 
term(8) = -term(8) 


    eom_cc3_23_trans_aibjciaibj = 0.d+0
    do s = 0, 9
    eom_cc3_23_trans_aibjciaibj = eom_cc3_23_trans_aibjciaibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjciaibj
    function eom_cc3_23_trans_aibjciajbi(a, i, j, c) 
    double precision :: eom_cc3_23_trans_aibjciajbi   
    integer, intent(in) :: a, i, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, c, i, j)
term(1) = term(1) + tvvov(a, c, i, a)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjciajbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjciajbi = eom_cc3_23_trans_aibjciajbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjciajbi
    function eom_cc3_23_trans_aibjciajbj(nocc, a, i, b, j, c) 
    double precision :: eom_cc3_23_trans_aibjciajbj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(j, c, n, n)
term(1) = term(1) + tovoo(n, c, j, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tovoo(j, c, j, j)
term(3) = term(3) + tov(j, c)
term(4) = term(4) + tvvov(a, c, j, a)
term(5) = term(5) + tvvov(a, a, j, c)
term(6) = term(6) + tvvov(b, b, j, c)
term(7) = term(7) + tovoo(i, c, j, i)
term(8) = term(8) + tovoo(j, c, i, i)

term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = term(7) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjciajbj = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjciajbj = eom_cc3_23_trans_aibjciajbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjciajbj
    function eom_cc3_23_trans_aibjcjajbi(i, b, j, c) 
    double precision :: eom_cc3_23_trans_aibjcjajbi   
    integer, intent(in) :: i, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, c, j, i)
term(1) = term(1) + tvvov(b, c, j, b)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjcjajbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjajbi = eom_cc3_23_trans_aibjcjajbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjajbi
    function eom_cc3_23_trans_aibjcjaibj(nocc, a, i, b, j, c) 
    double precision :: eom_cc3_23_trans_aibjcjaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,n 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(j, c, n, n)
term(1) = term(1) + tovoo(n, c, j, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 

term(2) = term(2) + tovoo(j, c, j, j)
term(3) = term(3) + tov(j, c)
term(4) = term(4) + tvvov(a, c, j, a)
term(5) = term(5) + tvvov(b, c, j, b)
term(6) = term(6) + tvvov(a, a, j, c)
term(7) = term(7) + tvvov(b, b, j, c)
term(8) = term(8) + tovoo(j, c, i, i)
term(9) = term(9) + tovoo(i, c, j, i)

term(2) = -term(2) 
term(4) = -term(4) 
term(8) = -term(8) 


    eom_cc3_23_trans_aibjcjaibj = 0.d+0
    do s = 0, 9
    eom_cc3_23_trans_aibjcjaibj = eom_cc3_23_trans_aibjcjaibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjaibj
    function eom_cc3_23_trans_aibjcjaibi(nocc, a, i, b, j, c) 
    double precision :: eom_cc3_23_trans_aibjcjaibi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(i, c, n, n)
term(1) = term(1) + tovoo(n, c, i, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tovoo(i, c, i, i)
term(3) = term(3) + tov(i, c)
term(4) = term(4) + tvvov(b, c, i, b)
term(5) = term(5) + tvvov(a, a, i, c)
term(6) = term(6) + tvvov(b, b, i, c)
term(7) = term(7) + tovoo(j, c, i, j)
term(8) = term(8) + tovoo(i, c, j, j)

term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = term(7) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjcjaibi = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjcjaibi = eom_cc3_23_trans_aibjcjaibi + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjaibi
    function eom_cc3_23_trans_aibickakbi(i, c, k) 
    double precision :: eom_cc3_23_trans_aibickakbi   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibickakbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibickakbi = eom_cc3_23_trans_aibickakbi + term(s)
    end do

    end function eom_cc3_23_trans_aibickakbi
    function eom_cc3_23_trans_aibickaibk(i, c, k) 
    double precision :: eom_cc3_23_trans_aibickaibk   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aibickaibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibickaibk = eom_cc3_23_trans_aibickaibk + term(s)
    end do

    end function eom_cc3_23_trans_aibickaibk
    function eom_cc3_23_trans_aibickaibi(nocc, a, i, b, c, k) 
    double precision :: eom_cc3_23_trans_aibickaibi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(k, c, n, n)
term(1) = term(1) + tovoo(n, c, k, n)
end do 

term(0) = term(0) * 3.9999999999999996d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tov(k, c)
term(3) = term(3) + tvvov(a, c, k, a)
term(4) = term(4) + tvvov(b, c, k, b)
term(5) = term(5) + tvvov(a, a, k, c)
term(6) = term(6) + tvvov(b, b, k, c)
term(7) = term(7) + tovoo(k, c, i, i)
term(8) = term(8) + tovoo(i, c, k, i)

term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = term(5) * 1.9999999999999998d+0 
term(6) = term(6) * 1.9999999999999998d+0 
term(7) = term(7) * (-3.9999999999999996d+0) 
term(8) = term(8) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibickaibi = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibickaibi = eom_cc3_23_trans_aibickaibi + term(s)
    end do

    end function eom_cc3_23_trans_aibickaibi
    function eom_cc3_23_trans_aibicibibm(a, b, c, m) 
    double precision :: eom_cc3_23_trans_aibicibibm   
    integer, intent(in) :: a, b, c, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, b)
term(1) = term(1) + tvvov(a, b, m, c)

term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibicibibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibicibibm = eom_cc3_23_trans_aibicibibm + term(s)
    end do

    end function eom_cc3_23_trans_aibicibibm
    function eom_cc3_23_trans_aibiciblbi(a, b, c, l) 
    double precision :: eom_cc3_23_trans_aibiciblbi   
    integer, intent(in) :: a, b, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, b)
term(1) = term(1) + tvvov(a, b, l, c)

term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibiciblbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibiciblbi = eom_cc3_23_trans_aibiciblbi + term(s)
    end do

    end function eom_cc3_23_trans_aibiciblbi
    function eom_cc3_23_trans_aibjcibibj(a, i, b, c) 
    double precision :: eom_cc3_23_trans_aibjcibibj   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, i, c)



    eom_cc3_23_trans_aibjcibibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcibibj = eom_cc3_23_trans_aibjcibibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcibibj
    function eom_cc3_23_trans_aibjcibjbi(a, i, b, c) 
    double precision :: eom_cc3_23_trans_aibjcibjbi   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, i, c)



    eom_cc3_23_trans_aibjcibjbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcibjbi = eom_cc3_23_trans_aibjcibjbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjcibjbi
    function eom_cc3_23_trans_aibjcibjbj(a, b, j, c) 
    double precision :: eom_cc3_23_trans_aibjcibjbj   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, b)
term(1) = term(1) + tvvov(a, b, j, c)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjcibjbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcibjbj = eom_cc3_23_trans_aibjcibjbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcibjbj
    function eom_cc3_23_trans_aibjcjbjbi(a, b, j, c) 
    double precision :: eom_cc3_23_trans_aibjcjbjbi   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, b)
term(1) = term(1) + tvvov(a, b, j, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjbjbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjbjbi = eom_cc3_23_trans_aibjcjbjbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjbjbi
    function eom_cc3_23_trans_aibjcjbibj(a, b, j, c) 
    double precision :: eom_cc3_23_trans_aibjcjbibj   
    integer, intent(in) :: a, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, b)
term(1) = term(1) + tvvov(a, b, j, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjcjbibj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjcjbibj = eom_cc3_23_trans_aibjcjbibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjbibj
    function eom_cc3_23_trans_aibjcjbibi(a, i, b, c) 
    double precision :: eom_cc3_23_trans_aibjcjbibi   
    integer, intent(in) :: a, i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, i, c)

term(0) = term(0) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjcjbibi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibjcjbibi = eom_cc3_23_trans_aibjcjbibi + term(s)
    end do

    end function eom_cc3_23_trans_aibjcjbibi
    function eom_cc3_23_trans_aibickbibi(a, b, c, k) 
    double precision :: eom_cc3_23_trans_aibickbibi   
    integer, intent(in) :: a, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, b)
term(1) = term(1) + tvvov(a, b, k, c)

term(0) = term(0) * (-1.9999999999999998d+0) 
term(1) = term(1) * 3.9999999999999996d+0 


    eom_cc3_23_trans_aibickbibi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibickbibi = eom_cc3_23_trans_aibickbibi + term(s)
    end do

    end function eom_cc3_23_trans_aibickbibi
    function eom_cc3_23_trans_aiaiaiaiem(nocc, a, i, e, m) 
    double precision :: eom_cc3_23_trans_aiaiaiaiem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, e, m 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, e, n, n)
term(1) = term(1) + tovoo(n, e, m, n)
end do 

term(0) = term(0) * 3.999999999999999d+0 
term(1) = term(1) * (-1.9999999999999996d+0) 

term(2) = term(2) + tov(m, e)
term(3) = term(3) + tovoo(m, e, i, i)
term(4) = term(4) + tovoo(i, e, m, i)
term(5) = term(5) + tvvov(a, a, m, e)
term(6) = term(6) + tvvov(a, e, m, a)

term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-3.999999999999999d+0) 
term(4) = term(4) * 1.9999999999999996d+0 
term(5) = term(5) * 3.999999999999999d+0 
term(6) = term(6) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aiaiaiaiem = 0.d+0
    do s = 0, 6
    eom_cc3_23_trans_aiaiaiaiem = eom_cc3_23_trans_aiaiaiaiem + term(s)
    end do

    end function eom_cc3_23_trans_aiaiaiaiem
    function eom_cc3_23_trans_aiaiaialei(nocc, a, i, l, e) 
    double precision :: eom_cc3_23_trans_aiaiaialei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, l, e 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, l, n)
term(1) = term(1) + tovoo(l, e, n, n)
end do 

term(1) = term(1) * (-1.9999999999999996d+0) 

term(2) = term(2) + tov(l, e)
term(3) = term(3) + tovoo(l, e, i, i)
term(4) = term(4) + tovoo(i, e, l, i)
term(5) = term(5) + tvvov(a, a, l, e)
term(6) = term(6) + tvvov(a, e, l, a)

term(2) = -term(2) 
term(3) = term(3) * 1.9999999999999996d+0 
term(4) = -term(4) 
term(5) = term(5) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aiaiaialei = 0.d+0
    do s = 0, 6
    eom_cc3_23_trans_aiaiaialei = eom_cc3_23_trans_aiaiaialei + term(s)
    end do

    end function eom_cc3_23_trans_aiaiaialei
    function eom_cc3_23_trans_aiaiaialel(i, l, e) 
    double precision :: eom_cc3_23_trans_aiaiaialel   
    integer, intent(in) :: i, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, l, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aiaiaialel = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiaiaialel = eom_cc3_23_trans_aiaiaialel + term(s)
    end do

    end function eom_cc3_23_trans_aiaiaialel
    function eom_cc3_23_trans_aiajaiaiej(nocc, a, i, j, e) 
    double precision :: eom_cc3_23_trans_aiajaiaiej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, e 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, i, n)
term(1) = term(1) + tovoo(i, e, n, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = term(1) * (-3.9999999999999996d+0) 

term(2) = term(2) + tovoo(i, e, i, i)
term(3) = term(3) + tov(i, e)
term(4) = term(4) + tovoo(j, e, i, j)
term(5) = term(5) + tovoo(i, e, j, j)
term(6) = term(6) + tvvov(a, a, i, e)
term(7) = term(7) + tvvov(a, e, i, a)

term(2) = term(2) * 1.9999999999999998d+0 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-3.9999999999999996d+0) 
term(5) = term(5) * 1.9999999999999998d+0 
term(6) = term(6) * (-3.9999999999999996d+0) 
term(7) = term(7) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiajaiaiej = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aiajaiaiej = eom_cc3_23_trans_aiajaiaiej + term(s)
    end do

    end function eom_cc3_23_trans_aiajaiaiej
    function eom_cc3_23_trans_aiajaiajei(nocc, a, i, j, e) 
    double precision :: eom_cc3_23_trans_aiajaiajei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, e 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, i, n)
term(1) = term(1) + tovoo(i, e, n, n)
end do 

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 

term(2) = term(2) + tovoo(i, e, i, i)
term(3) = term(3) + tov(i, e)
term(4) = term(4) + tovoo(j, e, i, j)
term(5) = term(5) + tovoo(i, e, j, j)
term(6) = term(6) + tvvov(a, a, i, e)
term(7) = term(7) + tvvov(a, e, i, a)

term(2) = -term(2) 
term(4) = term(4) * 1.9999999999999998d+0 
term(5) = -term(5) 
term(6) = term(6) * 1.9999999999999998d+0 
term(7) = -term(7) 


    eom_cc3_23_trans_aiajaiajei = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aiajaiajei = eom_cc3_23_trans_aiajaiajei + term(s)
    end do

    end function eom_cc3_23_trans_aiajaiajei
    function eom_cc3_23_trans_aiajaiajej(nocc, a, i, j, e) 
    double precision :: eom_cc3_23_trans_aiajaiajej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, e 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, j, n)
term(1) = term(1) + tovoo(j, e, n, n)
end do 

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 

term(2) = term(2) + tovoo(j, e, j, j)
term(3) = term(3) + tov(j, e)
term(4) = term(4) + tovoo(j, e, i, i)
term(5) = term(5) + tovoo(i, e, j, i)
term(6) = term(6) + tvvov(a, a, j, e)
term(7) = term(7) + tvvov(a, e, j, a)

term(2) = -term(2) 
term(4) = -term(4) 
term(5) = term(5) * 1.9999999999999998d+0 
term(6) = term(6) * 1.9999999999999998d+0 
term(7) = -term(7) 


    eom_cc3_23_trans_aiajaiajej = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aiajaiajej = eom_cc3_23_trans_aiajaiajej + term(s)
    end do

    end function eom_cc3_23_trans_aiajaiajej
    function eom_cc3_23_trans_aiajajajei(nocc, a, i, j, e) 
    double precision :: eom_cc3_23_trans_aiajajajei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, e 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, j, n)
term(1) = term(1) + tovoo(j, e, n, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = term(1) * (-3.9999999999999996d+0) 

term(2) = term(2) + tovoo(j, e, j, j)
term(3) = term(3) + tov(j, e)
term(4) = term(4) + tovoo(i, e, j, i)
term(5) = term(5) + tovoo(j, e, i, i)
term(6) = term(6) + tvvov(a, a, j, e)
term(7) = term(7) + tvvov(a, e, j, a)

term(2) = term(2) * 1.9999999999999998d+0 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-3.9999999999999996d+0) 
term(5) = term(5) * 1.9999999999999998d+0 
term(6) = term(6) * (-3.9999999999999996d+0) 
term(7) = term(7) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiajajajei = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aiajajajei = eom_cc3_23_trans_aiajajajei + term(s)
    end do

    end function eom_cc3_23_trans_aiajajajei
    function eom_cc3_23_trans_aiaiakakei(i, k, e) 
    double precision :: eom_cc3_23_trans_aiaiakakei   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)

term(0) = term(0) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiaiakakei = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiaiakakei = eom_cc3_23_trans_aiaiakakei + term(s)
    end do

    end function eom_cc3_23_trans_aiaiakakei
    function eom_cc3_23_trans_aiaiakaiek(i, k, e) 
    double precision :: eom_cc3_23_trans_aiaiakaiek   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aiaiakaiek = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiaiakaiek = eom_cc3_23_trans_aiaiakaiek + term(s)
    end do

    end function eom_cc3_23_trans_aiaiakaiek
    function eom_cc3_23_trans_aiaiakaiei(nocc, a, i, k, e) 
    double precision :: eom_cc3_23_trans_aiaiakaiei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, k, e 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, e, k, n)
term(1) = term(1) + tovoo(k, e, n, n)
end do 

term(1) = term(1) * (-1.9999999999999996d+0) 

term(2) = term(2) + tov(k, e)
term(3) = term(3) + tovoo(i, e, k, i)
term(4) = term(4) + tovoo(k, e, i, i)
term(5) = term(5) + tvvov(a, a, k, e)
term(6) = term(6) + tvvov(a, e, k, a)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = term(4) * 1.9999999999999996d+0 
term(5) = term(5) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aiaiakaiei = 0.d+0
    do s = 0, 6
    eom_cc3_23_trans_aiaiakaiei = eom_cc3_23_trans_aiaiakaiei + term(s)
    end do

    end function eom_cc3_23_trans_aiaiakaiei
    function eom_cc3_23_trans_aiaiaididm(a, d, m) 
    double precision :: eom_cc3_23_trans_aiaiaididm   
    integer, intent(in) :: a, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, d)



    eom_cc3_23_trans_aiaiaididm = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiaiaididm = eom_cc3_23_trans_aiaiaididm + term(s)
    end do

    end function eom_cc3_23_trans_aiaiaididm
    function eom_cc3_23_trans_aiaiaidldi(a, d, l) 
    double precision :: eom_cc3_23_trans_aiaiaidldi   
    integer, intent(in) :: a, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, d)



    eom_cc3_23_trans_aiaiaidldi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiaiaidldi = eom_cc3_23_trans_aiaiaidldi + term(s)
    end do

    end function eom_cc3_23_trans_aiaiaidldi
    function eom_cc3_23_trans_aiajaididj(a, i, d) 
    double precision :: eom_cc3_23_trans_aiajaididj   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aiajaididj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajaididj = eom_cc3_23_trans_aiajaididj + term(s)
    end do

    end function eom_cc3_23_trans_aiajaididj
    function eom_cc3_23_trans_aiajaidjdj(a, j, d) 
    double precision :: eom_cc3_23_trans_aiajaidjdj   
    integer, intent(in) :: a, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, d)

term(0) = term(0) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiajaidjdj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajaidjdj = eom_cc3_23_trans_aiajaidjdj + term(s)
    end do

    end function eom_cc3_23_trans_aiajaidjdj
    function eom_cc3_23_trans_aiajajdidj(a, j, d) 
    double precision :: eom_cc3_23_trans_aiajajdidj   
    integer, intent(in) :: a, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, d)

term(0) = -term(0) 


    eom_cc3_23_trans_aiajajdidj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajajdidj = eom_cc3_23_trans_aiajajdidj + term(s)
    end do

    end function eom_cc3_23_trans_aiajajdidj
    function eom_cc3_23_trans_aiajajdidi(a, i, d) 
    double precision :: eom_cc3_23_trans_aiajajdidi   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, d)

term(0) = term(0) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiajajdidi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajajdidi = eom_cc3_23_trans_aiajajdidi + term(s)
    end do

    end function eom_cc3_23_trans_aiajajdidi
    function eom_cc3_23_trans_aiaiakdidi(a, k, d) 
    double precision :: eom_cc3_23_trans_aiaiakdidi   
    integer, intent(in) :: a, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, k, d)

term(0) = term(0) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aiaiakdidi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiaiakdidi = eom_cc3_23_trans_aiaiakdidi + term(s)
    end do

    end function eom_cc3_23_trans_aiaiakdidi
    function eom_cc3_23_trans_aibiaiaibm(nocc, a, i, b, m) 
    double precision :: eom_cc3_23_trans_aibiaiaibm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, m 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, a, n, n)
term(1) = term(1) + tovoo(n, a, m, n)
end do 

term(0) = term(0) * (-3.9999999999999996d+0) 
term(1) = term(1) * 1.9999999999999998d+0 

term(2) = term(2) + tvvov(a, a, m, a)
term(3) = term(3) + tov(m, a)
term(4) = term(4) + tvvov(b, a, m, b)
term(5) = term(5) + tvvov(b, b, m, a)
term(6) = term(6) + tovoo(i, a, m, i)
term(7) = term(7) + tovoo(m, a, i, i)

term(2) = term(2) * (-1.9999999999999998d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * 3.9999999999999996d+0 
term(5) = term(5) * (-1.9999999999999998d+0) 
term(6) = term(6) * (-1.9999999999999996d+0) 
term(7) = term(7) * 3.9999999999999996d+0 


    eom_cc3_23_trans_aibiaiaibm = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aibiaiaibm = eom_cc3_23_trans_aibiaiaibm + term(s)
    end do

    end function eom_cc3_23_trans_aibiaiaibm
    function eom_cc3_23_trans_aibiaialbi(nocc, a, i, b, l) 
    double precision :: eom_cc3_23_trans_aibiaialbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(l, a, n, n)
term(1) = term(1) + tovoo(n, a, l, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 

term(2) = term(2) + tvvov(a, a, l, a)
term(3) = term(3) + tov(l, a)
term(4) = term(4) + tvvov(b, a, l, b)
term(5) = term(5) + tvvov(b, b, l, a)
term(6) = term(6) + tovoo(i, a, l, i)
term(7) = term(7) + tovoo(l, a, i, i)

term(4) = term(4) * (-1.9999999999999998d+0) 
term(7) = term(7) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aibiaialbi = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aibiaialbi = eom_cc3_23_trans_aibiaialbi + term(s)
    end do

    end function eom_cc3_23_trans_aibiaialbi
    function eom_cc3_23_trans_aibiaialbl(a, i, l) 
    double precision :: eom_cc3_23_trans_aibiaialbl   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, a, l, i)



    eom_cc3_23_trans_aibiaialbl = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiaialbl = eom_cc3_23_trans_aibiaialbl + term(s)
    end do

    end function eom_cc3_23_trans_aibiaialbl
    function eom_cc3_23_trans_aibjaiaibj(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_trans_aibjaiaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(i, a, n, n)
term(1) = term(1) + tovoo(n, a, i, n)
end do 

term(0) = term(0) * 3.999999999999999d+0 
term(1) = term(1) * (-1.9999999999999996d+0) 

term(2) = term(2) + tovoo(i, a, i, i)
term(3) = term(3) + tvvov(a, a, i, a)
term(4) = term(4) + tov(i, a)
term(5) = term(5) + tvvov(b, a, i, b)
term(6) = term(6) + tvvov(b, b, i, a)
term(7) = term(7) + tovoo(i, a, j, j)
term(8) = term(8) + tovoo(j, a, i, j)

term(2) = term(2) * (-1.9999999999999996d+0) 
term(3) = term(3) * 1.9999999999999996d+0 
term(4) = term(4) * 2.0d+0 
term(5) = term(5) * (-1.9999999999999998d+0) 
term(6) = term(6) * 1.9999999999999996d+0 
term(7) = term(7) * (-1.9999999999999996d+0) 
term(8) = term(8) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aibjaiaibj = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjaiaibj = eom_cc3_23_trans_aibjaiaibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiaibj
    function eom_cc3_23_trans_aibjaiajbi(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_trans_aibjaiajbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(i, a, n, n)
term(1) = term(1) + tovoo(n, a, i, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tvvov(a, a, i, a)
term(3) = term(3) + tovoo(i, a, i, i)
term(4) = term(4) + tov(i, a)
term(5) = term(5) + tvvov(b, a, i, b)
term(6) = term(6) + tvvov(b, b, i, a)
term(7) = term(7) + tovoo(j, a, i, j)
term(8) = term(8) + tovoo(i, a, j, j)

term(2) = -term(2) 
term(4) = -term(4) 
term(6) = -term(6) 
term(7) = -term(7) 


    eom_cc3_23_trans_aibjaiajbi = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjaiajbi = eom_cc3_23_trans_aibjaiajbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiajbi
    function eom_cc3_23_trans_aibjaiajbj(a, i, b, j) 
    double precision :: eom_cc3_23_trans_aibjaiajbj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, j, b)
term(1) = term(1) + tovoo(i, a, j, i)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjaiajbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaiajbj = eom_cc3_23_trans_aibjaiajbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjaiajbj
    function eom_cc3_23_trans_aibjajajbi(a, i, b, j) 
    double precision :: eom_cc3_23_trans_aibjajajbi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, j, i)
term(1) = term(1) + tvvov(b, a, j, b)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjajajbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajajbi = eom_cc3_23_trans_aibjajajbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjajajbi
    function eom_cc3_23_trans_aibjajaibj(a, i, b, j) 
    double precision :: eom_cc3_23_trans_aibjajaibj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, j, b)
term(1) = term(1) + tovoo(i, a, j, i)

term(1) = -term(1) 


    eom_cc3_23_trans_aibjajaibj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajaibj = eom_cc3_23_trans_aibjajaibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjajaibj
    function eom_cc3_23_trans_aibjajaibi(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_trans_aibjajaibi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(i, a, n, n)
term(1) = term(1) + tovoo(n, a, i, n)
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

term(2) = term(2) + tvvov(a, a, i, a)
term(3) = term(3) + tovoo(i, a, i, i)
term(4) = term(4) + tov(i, a)
term(5) = term(5) + tvvov(b, a, i, b)
term(6) = term(6) + tvvov(b, b, i, a)
term(7) = term(7) + tovoo(j, a, i, j)
term(8) = term(8) + tovoo(i, a, j, j)

term(2) = -term(2) 
term(4) = -term(4) 
term(6) = -term(6) 
term(7) = -term(7) 


    eom_cc3_23_trans_aibjajaibi = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjajaibi = eom_cc3_23_trans_aibjajaibi + term(s)
    end do

    end function eom_cc3_23_trans_aibjajaibi
    function eom_cc3_23_trans_aibiakakbi(a, i, k) 
    double precision :: eom_cc3_23_trans_aibiakakbi   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, k, i)

term(0) = term(0) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aibiakakbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiakakbi = eom_cc3_23_trans_aibiakakbi + term(s)
    end do

    end function eom_cc3_23_trans_aibiakakbi
    function eom_cc3_23_trans_aibiakaibk(a, i, k) 
    double precision :: eom_cc3_23_trans_aibiakaibk   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, k, i)



    eom_cc3_23_trans_aibiakaibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiakaibk = eom_cc3_23_trans_aibiakaibk + term(s)
    end do

    end function eom_cc3_23_trans_aibiakaibk
    function eom_cc3_23_trans_aibiakaibi(nocc, a, i, b, k) 
    double precision :: eom_cc3_23_trans_aibiakaibi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(k, a, n, n)
term(1) = term(1) + tovoo(n, a, k, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 

term(2) = term(2) + tvvov(a, a, k, a)
term(3) = term(3) + tov(k, a)
term(4) = term(4) + tvvov(b, a, k, b)
term(5) = term(5) + tvvov(b, b, k, a)
term(6) = term(6) + tovoo(k, a, i, i)
term(7) = term(7) + tovoo(i, a, k, i)

term(4) = term(4) * (-1.9999999999999998d+0) 
term(6) = term(6) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aibiakaibi = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aibiakaibi = eom_cc3_23_trans_aibiakaibi + term(s)
    end do

    end function eom_cc3_23_trans_aibiakaibi
    function eom_cc3_23_trans_aibiaibibm(nocc, a, i, b, m) 
    double precision :: eom_cc3_23_trans_aibiaibibm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, m 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, b, n, n)
term(1) = term(1) + tovoo(n, b, m, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 

term(2) = term(2) + tvvov(b, b, m, b)
term(3) = term(3) + tov(m, b)
term(4) = term(4) + tovoo(m, b, i, i)
term(5) = term(5) + tovoo(i, b, m, i)
term(6) = term(6) + tvvov(a, a, m, b)
term(7) = term(7) + tvvov(a, b, m, a)

term(4) = term(4) * (-1.9999999999999996d+0) 
term(7) = term(7) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibiaibibm = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aibiaibibm = eom_cc3_23_trans_aibiaibibm + term(s)
    end do

    end function eom_cc3_23_trans_aibiaibibm
    function eom_cc3_23_trans_aibiaiblbi(nocc, a, i, b, l) 
    double precision :: eom_cc3_23_trans_aibiaiblbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(l, b, n, n)
term(1) = term(1) + tovoo(n, b, l, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 

term(2) = term(2) + tvvov(b, b, l, b)
term(3) = term(3) + tov(l, b)
term(4) = term(4) + tovoo(l, b, i, i)
term(5) = term(5) + tovoo(i, b, l, i)
term(6) = term(6) + tvvov(a, a, l, b)
term(7) = term(7) + tvvov(a, b, l, a)

term(4) = term(4) * (-1.9999999999999996d+0) 
term(7) = term(7) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibiaiblbi = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aibiaiblbi = eom_cc3_23_trans_aibiaiblbi + term(s)
    end do

    end function eom_cc3_23_trans_aibiaiblbi
    function eom_cc3_23_trans_aibiaiblbl(i, b, l) 
    double precision :: eom_cc3_23_trans_aibiaiblbl   
    integer, intent(in) :: i, b, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, b, l, i)

term(0) = term(0) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aibiaiblbl = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiaiblbl = eom_cc3_23_trans_aibiaiblbl + term(s)
    end do

    end function eom_cc3_23_trans_aibiaiblbl
    function eom_cc3_23_trans_aibjaibibj(a, i, b, j) 
    double precision :: eom_cc3_23_trans_aibjaibibj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, i, j)
term(1) = term(1) + tvvov(a, b, i, a)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjaibibj = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaibibj = eom_cc3_23_trans_aibjaibibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjaibibj
    function eom_cc3_23_trans_aibjaibjbi(a, i, b, j) 
    double precision :: eom_cc3_23_trans_aibjaibjbi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, i, j)
term(1) = term(1) + tvvov(a, b, i, a)

term(0) = -term(0) 


    eom_cc3_23_trans_aibjaibjbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjaibjbi = eom_cc3_23_trans_aibjaibjbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjaibjbi
    function eom_cc3_23_trans_aibjaibjbj(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_trans_aibjaibjbj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(j, b, n, n)
term(1) = term(1) + tovoo(n, b, j, n)
end do 

term(0) = term(0) * 3.999999999999999d+0 
term(1) = term(1) * (-1.9999999999999996d+0) 

term(2) = term(2) + tovoo(j, b, j, j)
term(3) = term(3) + tvvov(b, b, j, b)
term(4) = term(4) + tov(j, b)
term(5) = term(5) + tovoo(j, b, i, i)
term(6) = term(6) + tovoo(i, b, j, i)
term(7) = term(7) + tvvov(a, a, j, b)
term(8) = term(8) + tvvov(a, b, j, a)

term(2) = term(2) * (-1.9999999999999996d+0) 
term(3) = term(3) * 1.9999999999999996d+0 
term(4) = term(4) * 2.0d+0 
term(5) = term(5) * (-1.9999999999999998d+0) 
term(6) = term(6) * 1.9999999999999998d+0 
term(7) = term(7) * 1.9999999999999998d+0 
term(8) = term(8) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjaibjbj = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjaibjbj = eom_cc3_23_trans_aibjaibjbj + term(s)
    end do

    end function eom_cc3_23_trans_aibjaibjbj
    function eom_cc3_23_trans_aibjajbjbi(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_trans_aibjajbjbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, b, j, n)
term(1) = term(1) + tovoo(j, b, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tvvov(b, b, j, b)
term(3) = term(3) + tovoo(j, b, j, j)
term(4) = term(4) + tov(j, b)
term(5) = term(5) + tovoo(i, b, j, i)
term(6) = term(6) + tovoo(j, b, i, i)
term(7) = term(7) + tvvov(a, a, j, b)
term(8) = term(8) + tvvov(a, b, j, a)

term(2) = -term(2) 
term(4) = -term(4) 
term(5) = -term(5) 
term(7) = -term(7) 


    eom_cc3_23_trans_aibjajbjbi = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjajbjbi = eom_cc3_23_trans_aibjajbjbi + term(s)
    end do

    end function eom_cc3_23_trans_aibjajbjbi
    function eom_cc3_23_trans_aibjajbibj(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_trans_aibjajbibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, b, j, n)
term(1) = term(1) + tovoo(j, b, n, n)
end do 

term(1) = term(1) * (-1.9999999999999998d+0) 

term(2) = term(2) + tovoo(j, b, j, j)
term(3) = term(3) + tvvov(b, b, j, b)
term(4) = term(4) + tov(j, b)
term(5) = term(5) + tovoo(i, b, j, i)
term(6) = term(6) + tovoo(j, b, i, i)
term(7) = term(7) + tvvov(a, a, j, b)
term(8) = term(8) + tvvov(a, b, j, a)

term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 
term(7) = -term(7) 


    eom_cc3_23_trans_aibjajbibj = 0.d+0
    do s = 0, 8
    eom_cc3_23_trans_aibjajbibj = eom_cc3_23_trans_aibjajbibj + term(s)
    end do

    end function eom_cc3_23_trans_aibjajbibj
    function eom_cc3_23_trans_aibjajbibi(a, i, b, j) 
    double precision :: eom_cc3_23_trans_aibjajbibi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, i, j)
term(1) = term(1) + tvvov(a, b, i, a)

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = term(1) * (-1.9999999999999998d+0) 


    eom_cc3_23_trans_aibjajbibi = 0.d+0
    do s = 0, 1
    eom_cc3_23_trans_aibjajbibi = eom_cc3_23_trans_aibjajbibi + term(s)
    end do

    end function eom_cc3_23_trans_aibjajbibi
    function eom_cc3_23_trans_aibiakbkbi(i, b, k) 
    double precision :: eom_cc3_23_trans_aibiakbkbi   
    integer, intent(in) :: i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, k, i)



    eom_cc3_23_trans_aibiakbkbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiakbkbi = eom_cc3_23_trans_aibiakbkbi + term(s)
    end do

    end function eom_cc3_23_trans_aibiakbkbi
    function eom_cc3_23_trans_aibiakbibk(i, b, k) 
    double precision :: eom_cc3_23_trans_aibiakbibk   
    integer, intent(in) :: i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, k, i)



    eom_cc3_23_trans_aibiakbibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aibiakbibk = eom_cc3_23_trans_aibiakbibk + term(s)
    end do

    end function eom_cc3_23_trans_aibiakbibk
    function eom_cc3_23_trans_aibiakbibi(nocc, a, i, b, k) 
    double precision :: eom_cc3_23_trans_aibiakbibi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(n, b, k, n)
term(1) = term(1) + tovoo(k, b, n, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = term(1) * (-3.9999999999999996d+0) 

term(2) = term(2) + tvvov(b, b, k, b)
term(3) = term(3) + tov(k, b)
term(4) = term(4) + tovoo(i, b, k, i)
term(5) = term(5) + tovoo(k, b, i, i)
term(6) = term(6) + tvvov(a, a, k, b)
term(7) = term(7) + tvvov(a, b, k, a)

term(2) = term(2) * (-1.9999999999999998d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(5) = term(5) * 3.9999999999999996d+0 
term(6) = term(6) * (-1.9999999999999998d+0) 
term(7) = term(7) * 3.9999999999999996d+0 


    eom_cc3_23_trans_aibiakbibi = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aibiakbibi = eom_cc3_23_trans_aibiakbibi + term(s)
    end do

    end function eom_cc3_23_trans_aibiakbibi
    function eom_cc3_23_trans_aiaiciciam(a, c, m) 
    double precision :: eom_cc3_23_trans_aiaiciciam   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, c)

term(0) = term(0) * (-1.9999999999999996d+0) 


    eom_cc3_23_trans_aiaiciciam = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiaiciciam = eom_cc3_23_trans_aiaiciciam + term(s)
    end do

    end function eom_cc3_23_trans_aiaiciciam
    function eom_cc3_23_trans_aiaiciclai(a, c, l) 
    double precision :: eom_cc3_23_trans_aiaiciclai   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, c)



    eom_cc3_23_trans_aiaiciclai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiaiciclai = eom_cc3_23_trans_aiaiciclai + term(s)
    end do

    end function eom_cc3_23_trans_aiaiciclai
    function eom_cc3_23_trans_aiajciciaj(a, i, c) 
    double precision :: eom_cc3_23_trans_aiajciciaj   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, c)

term(0) = term(0) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiajciciaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajciciaj = eom_cc3_23_trans_aiajciciaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajciciaj
    function eom_cc3_23_trans_aiajcicjai(a, i, c) 
    double precision :: eom_cc3_23_trans_aiajcicjai   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aiajcicjai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajcicjai = eom_cc3_23_trans_aiajcicjai + term(s)
    end do

    end function eom_cc3_23_trans_aiajcicjai
    function eom_cc3_23_trans_aiajcicjaj(a, j, c) 
    double precision :: eom_cc3_23_trans_aiajcicjaj   
    integer, intent(in) :: a, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, c)

term(0) = -term(0) 


    eom_cc3_23_trans_aiajcicjaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajcicjaj = eom_cc3_23_trans_aiajcicjaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajcicjaj
    function eom_cc3_23_trans_aiajcjcjai(a, j, c) 
    double precision :: eom_cc3_23_trans_aiajcjcjai   
    integer, intent(in) :: a, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, c)

term(0) = term(0) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiajcjcjai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiajcjcjai = eom_cc3_23_trans_aiajcjcjai + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjcjai
    function eom_cc3_23_trans_aiaickciai(a, c, k) 
    double precision :: eom_cc3_23_trans_aiaickciai   
    integer, intent(in) :: a, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, c)



    eom_cc3_23_trans_aiaickciai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiaickciai = eom_cc3_23_trans_aiaickciai + term(s)
    end do

    end function eom_cc3_23_trans_aiaickciai
    function eom_cc3_23_trans_aiaiciaiam(nocc, a, i, c, m) 
    double precision :: eom_cc3_23_trans_aiaiciaiam 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, c, m 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(m, c, n, n)
term(1) = term(1) + tovoo(n, c, m, n)
end do 

term(0) = term(0) * (-1.9999999999999996d+0) 

term(2) = term(2) + tov(m, c)
term(3) = term(3) + tvvov(a, c, m, a)
term(4) = term(4) + tvvov(a, a, m, c)
term(5) = term(5) + tovoo(i, c, m, i)
term(6) = term(6) + tovoo(m, c, i, i)

term(2) = -term(2) 
term(4) = term(4) * (-1.9999999999999996d+0) 
term(5) = -term(5) 
term(6) = term(6) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiaiciaiam = 0.d+0
    do s = 0, 6
    eom_cc3_23_trans_aiaiciaiam = eom_cc3_23_trans_aiaiciaiam + term(s)
    end do

    end function eom_cc3_23_trans_aiaiciaiam
    function eom_cc3_23_trans_aiaicialai(nocc, a, i, c, l) 
    double precision :: eom_cc3_23_trans_aiaicialai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, c, l 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(l, c, n, n)
term(1) = term(1) + tovoo(n, c, l, n)
end do 

term(0) = term(0) * (-1.9999999999999996d+0) 

term(2) = term(2) + tov(l, c)
term(3) = term(3) + tvvov(a, c, l, a)
term(4) = term(4) + tvvov(a, a, l, c)
term(5) = term(5) + tovoo(i, c, l, i)
term(6) = term(6) + tovoo(l, c, i, i)

term(2) = -term(2) 
term(4) = term(4) * (-1.9999999999999996d+0) 
term(5) = -term(5) 
term(6) = term(6) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiaicialai = 0.d+0
    do s = 0, 6
    eom_cc3_23_trans_aiaicialai = eom_cc3_23_trans_aiaicialai + term(s)
    end do

    end function eom_cc3_23_trans_aiaicialai
    function eom_cc3_23_trans_aiaicialal(i, c, l) 
    double precision :: eom_cc3_23_trans_aiaicialal   
    integer, intent(in) :: i, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, l, i)

term(0) = term(0) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiaicialal = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiaicialal = eom_cc3_23_trans_aiaicialal + term(s)
    end do

    end function eom_cc3_23_trans_aiaicialal
    function eom_cc3_23_trans_aiajciaiaj(nocc, a, i, j, c) 
    double precision :: eom_cc3_23_trans_aiajciaiaj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(i, c, n, n)
term(1) = term(1) + tovoo(n, c, i, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 

term(2) = term(2) + tovoo(i, c, i, i)
term(3) = term(3) + tov(i, c)
term(4) = term(4) + tvvov(a, c, i, a)
term(5) = term(5) + tvvov(a, a, i, c)
term(6) = term(6) + tovoo(i, c, j, j)
term(7) = term(7) + tovoo(j, c, i, j)

term(2) = -term(2) 
term(4) = -term(4) 
term(5) = term(5) * 1.9999999999999996d+0 
term(6) = -term(6) 
term(7) = term(7) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajciaiaj = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aiajciaiaj = eom_cc3_23_trans_aiajciaiaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajciaiaj
    function eom_cc3_23_trans_aiajciajaj(nocc, a, i, j, c) 
    double precision :: eom_cc3_23_trans_aiajciajaj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(j, c, n, n)
term(1) = term(1) + tovoo(n, c, j, n)
end do 

term(0) = term(0) * (-3.9999999999999996d+0) 
term(1) = term(1) * 1.9999999999999998d+0 

term(2) = term(2) + tovoo(j, c, j, j)
term(3) = term(3) + tov(j, c)
term(4) = term(4) + tvvov(a, c, j, a)
term(5) = term(5) + tvvov(a, a, j, c)
term(6) = term(6) + tovoo(i, c, j, i)
term(7) = term(7) + tovoo(j, c, i, i)

term(2) = term(2) * 1.9999999999999998d+0 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * 1.9999999999999998d+0 
term(5) = term(5) * (-3.9999999999999996d+0) 
term(6) = term(6) * (-3.9999999999999996d+0) 
term(7) = term(7) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajciajaj = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aiajciajaj = eom_cc3_23_trans_aiajciajaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajciajaj
    function eom_cc3_23_trans_aiajcjaiaj(nocc, a, i, j, c) 
    double precision :: eom_cc3_23_trans_aiajcjaiaj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(j, c, n, n)
term(1) = term(1) + tovoo(n, c, j, n)
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = -term(1) 

term(2) = term(2) + tovoo(j, c, j, j)
term(3) = term(3) + tov(j, c)
term(4) = term(4) + tvvov(a, c, j, a)
term(5) = term(5) + tvvov(a, a, j, c)
term(6) = term(6) + tovoo(j, c, i, i)
term(7) = term(7) + tovoo(i, c, j, i)

term(2) = -term(2) 
term(4) = -term(4) 
term(5) = term(5) * 1.9999999999999996d+0 
term(6) = -term(6) 
term(7) = term(7) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajcjaiaj = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aiajcjaiaj = eom_cc3_23_trans_aiajcjaiaj + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjaiaj
    function eom_cc3_23_trans_aiajcjaiai(nocc, a, i, j, c) 
    double precision :: eom_cc3_23_trans_aiajcjaiai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(i, c, n, n)
term(1) = term(1) + tovoo(n, c, i, n)
end do 

term(0) = term(0) * (-3.9999999999999996d+0) 
term(1) = term(1) * 1.9999999999999998d+0 

term(2) = term(2) + tovoo(i, c, i, i)
term(3) = term(3) + tov(i, c)
term(4) = term(4) + tvvov(a, c, i, a)
term(5) = term(5) + tvvov(a, a, i, c)
term(6) = term(6) + tovoo(j, c, i, j)
term(7) = term(7) + tovoo(i, c, j, j)

term(2) = term(2) * 1.9999999999999998d+0 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * 1.9999999999999998d+0 
term(5) = term(5) * (-3.9999999999999996d+0) 
term(6) = term(6) * (-3.9999999999999996d+0) 
term(7) = term(7) * 1.9999999999999998d+0 


    eom_cc3_23_trans_aiajcjaiai = 0.d+0
    do s = 0, 7
    eom_cc3_23_trans_aiajcjaiai = eom_cc3_23_trans_aiajcjaiai + term(s)
    end do

    end function eom_cc3_23_trans_aiajcjaiai
    function eom_cc3_23_trans_aiaickakai(i, c, k) 
    double precision :: eom_cc3_23_trans_aiaickakai   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aiaickakai = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiaickakai = eom_cc3_23_trans_aiaickakai + term(s)
    end do

    end function eom_cc3_23_trans_aiaickakai
    function eom_cc3_23_trans_aiaickaiak(i, c, k) 
    double precision :: eom_cc3_23_trans_aiaickaiak   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, i)

term(0) = -term(0) 


    eom_cc3_23_trans_aiaickaiak = 0.d+0
    do s = 0, 0
    eom_cc3_23_trans_aiaickaiak = eom_cc3_23_trans_aiaickaiak + term(s)
    end do

    end function eom_cc3_23_trans_aiaickaiak
    function eom_cc3_23_trans_aiaickaiai(nocc, a, i, c, k) 
    double precision :: eom_cc3_23_trans_aiaickaiai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    do n = 1, nocc 
term(0) = term(0) + tovoo(k, c, n, n)
term(1) = term(1) + tovoo(n, c, k, n)
end do 

term(0) = term(0) * 3.999999999999999d+0 
term(1) = term(1) * (-1.9999999999999996d+0) 

term(2) = term(2) + tov(k, c)
term(3) = term(3) + tvvov(a, c, k, a)
term(4) = term(4) + tvvov(a, a, k, c)
term(5) = term(5) + tovoo(k, c, i, i)
term(6) = term(6) + tovoo(i, c, k, i)

term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-1.9999999999999996d+0) 
term(4) = term(4) * 3.999999999999999d+0 
term(5) = term(5) * (-3.999999999999999d+0) 
term(6) = term(6) * 1.9999999999999996d+0 


    eom_cc3_23_trans_aiaickaiai = 0.d+0
    do s = 0, 6
    eom_cc3_23_trans_aiaickaiai = eom_cc3_23_trans_aiaickaiai + term(s)
    end do

    end function eom_cc3_23_trans_aiaickaiai
    end module eom_cc3_23_trans
    
