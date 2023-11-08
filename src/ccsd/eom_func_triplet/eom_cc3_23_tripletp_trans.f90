module eom_cc3_23_tripletp_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_cc3_23_tripletp_trans_aibjakbiem(j, k, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakbiem   
    integer, intent(in) :: j, k, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, j)
term(1) = term(1) + tovoo(k, e, m, j)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjakbiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjakbiem = eom_cc3_23_tripletp_trans_aibjakbiem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakbiem
    function eom_cc3_23_tripletp_trans_aibjakblei(j, k, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakblei   
    integer, intent(in) :: j, k, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, j)
term(1) = term(1) + tovoo(k, e, l, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjakblei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjakblei = eom_cc3_23_tripletp_trans_aibjakblei + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakblei
    function eom_cc3_23_tripletp_trans_aibjaiblem(j, l, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaiblem   
    integer, intent(in) :: j, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, j)
term(1) = term(1) + tovoo(l, e, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjaiblem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjaiblem = eom_cc3_23_tripletp_trans_aibjaiblem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaiblem
    function eom_cc3_23_tripletp_trans_aibjakbjem(i, k, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakbjem   
    integer, intent(in) :: i, k, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, m, i)
term(1) = term(1) + tovoo(m, e, k, i)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjakbjem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjakbjem = eom_cc3_23_tripletp_trans_aibjakbjem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakbjem
    function eom_cc3_23_tripletp_trans_aibjakblej(i, k, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakblej   
    integer, intent(in) :: i, k, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, l, i)
term(1) = term(1) + tovoo(l, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjakblej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjakblej = eom_cc3_23_tripletp_trans_aibjakblej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakblej
    function eom_cc3_23_tripletp_trans_aibjajblem(i, l, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajblem   
    integer, intent(in) :: i, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, i)
term(1) = term(1) + tovoo(l, e, m, i)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjajblem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjajblem = eom_cc3_23_tripletp_trans_aibjajblem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajblem
    function eom_cc3_23_tripletp_trans_aibjakdibm(j, k, d, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakdibm   
    integer, intent(in) :: j, k, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, k, j)
term(1) = term(1) + tovoo(k, d, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjakdibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjakdibm = eom_cc3_23_tripletp_trans_aibjakdibm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakdibm
    function eom_cc3_23_tripletp_trans_aibjakdlbi(j, k, d, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakdlbi   
    integer, intent(in) :: j, k, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, l, j)
term(1) = term(1) + tovoo(l, d, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjakdlbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjakdlbi = eom_cc3_23_tripletp_trans_aibjakdlbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakdlbi
    function eom_cc3_23_tripletp_trans_aibjaidlbm(j, d, l, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaidlbm   
    integer, intent(in) :: j, d, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, l, j)
term(1) = term(1) + tovoo(l, d, m, j)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjaidlbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjaidlbm = eom_cc3_23_tripletp_trans_aibjaidlbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaidlbm
    function eom_cc3_23_tripletp_trans_aibjakdjbm(i, k, d, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakdjbm   
    integer, intent(in) :: i, k, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, m, i)
term(1) = term(1) + tovoo(m, d, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjakdjbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjakdjbm = eom_cc3_23_tripletp_trans_aibjakdjbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakdjbm
    function eom_cc3_23_tripletp_trans_aibjakdlbj(i, k, d, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakdlbj   
    integer, intent(in) :: i, k, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, l, i)
term(1) = term(1) + tovoo(l, d, k, i)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjakdlbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjakdlbj = eom_cc3_23_tripletp_trans_aibjakdlbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakdlbj
    function eom_cc3_23_tripletp_trans_aibjajdlbm(i, d, l, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajdlbm   
    integer, intent(in) :: i, d, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, l, i)
term(1) = term(1) + tovoo(l, d, m, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjajdlbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjajdlbm = eom_cc3_23_tripletp_trans_aibjajdlbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajdlbm
    function eom_cc3_23_tripletp_trans_aibjakdiej(b, k, d, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakdiej   
    integer, intent(in) :: b, k, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, k, e)
term(1) = term(1) + tvvov(b, e, k, d)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjakdiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjakdiej = eom_cc3_23_tripletp_trans_aibjakdiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakdiej
    function eom_cc3_23_tripletp_trans_aibjajdiem(b, d, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajdiem   
    integer, intent(in) :: b, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, m, e)
term(1) = term(1) + tvvov(b, e, m, d)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjajdiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjajdiem = eom_cc3_23_tripletp_trans_aibjajdiem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajdiem
    function eom_cc3_23_tripletp_trans_aibjajdlei(b, d, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajdlei   
    integer, intent(in) :: b, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, l, e)
term(1) = term(1) + tvvov(b, e, l, d)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjajdlei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjajdlei = eom_cc3_23_tripletp_trans_aibjajdlei + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajdlei
    function eom_cc3_23_tripletp_trans_aibjaidjem(b, d, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaidjem   
    integer, intent(in) :: b, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, m, e)
term(1) = term(1) + tvvov(b, e, m, d)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjaidjem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjaidjem = eom_cc3_23_tripletp_trans_aibjaidjem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaidjem
    function eom_cc3_23_tripletp_trans_aibjaidlej(b, d, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaidlej   
    integer, intent(in) :: b, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, l, e)
term(1) = term(1) + tvvov(b, e, l, d)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjaidlej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjaidlej = eom_cc3_23_tripletp_trans_aibjaidlej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaidlej
    function eom_cc3_23_tripletp_trans_aibjbkaiem(j, k, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkaiem   
    integer, intent(in) :: j, k, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, j)
term(1) = term(1) + tovoo(k, e, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbkaiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbkaiem = eom_cc3_23_tripletp_trans_aibjbkaiem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkaiem
    function eom_cc3_23_tripletp_trans_aibjbkalei(j, k, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkalei   
    integer, intent(in) :: j, k, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, j)
term(1) = term(1) + tovoo(k, e, l, j)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjbkalei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbkalei = eom_cc3_23_tripletp_trans_aibjbkalei + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkalei
    function eom_cc3_23_tripletp_trans_aibjbialem(j, l, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbialem   
    integer, intent(in) :: j, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, j)
term(1) = term(1) + tovoo(l, e, m, j)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjbialem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbialem = eom_cc3_23_tripletp_trans_aibjbialem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbialem
    function eom_cc3_23_tripletp_trans_aibjbkajem(i, k, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkajem   
    integer, intent(in) :: i, k, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, i)
term(1) = term(1) + tovoo(k, e, m, i)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjbkajem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbkajem = eom_cc3_23_tripletp_trans_aibjbkajem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkajem
    function eom_cc3_23_tripletp_trans_aibjbkalej(i, k, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkalej   
    integer, intent(in) :: i, k, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, i)
term(1) = term(1) + tovoo(k, e, l, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbkalej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbkalej = eom_cc3_23_tripletp_trans_aibjbkalej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkalej
    function eom_cc3_23_tripletp_trans_aibjbjalem(i, l, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjalem   
    integer, intent(in) :: i, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, i)
term(1) = term(1) + tovoo(l, e, m, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbjalem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbjalem = eom_cc3_23_tripletp_trans_aibjbjalem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjalem
    function eom_cc3_23_tripletp_trans_aibjbkdiam(j, k, d, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkdiam   
    integer, intent(in) :: j, k, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, k, j)
term(1) = term(1) + tovoo(k, d, m, j)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjbkdiam = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbkdiam = eom_cc3_23_tripletp_trans_aibjbkdiam + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkdiam
    function eom_cc3_23_tripletp_trans_aibjbkdlai(j, k, d, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkdlai   
    integer, intent(in) :: j, k, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, k, j)
term(1) = term(1) + tovoo(k, d, l, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbkdlai = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbkdlai = eom_cc3_23_tripletp_trans_aibjbkdlai + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkdlai
    function eom_cc3_23_tripletp_trans_aibjbidlam(j, d, l, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbidlam   
    integer, intent(in) :: j, d, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, l, j)
term(1) = term(1) + tovoo(l, d, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbidlam = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbidlam = eom_cc3_23_tripletp_trans_aibjbidlam + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbidlam
    function eom_cc3_23_tripletp_trans_aibjbkdjam(i, k, d, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkdjam   
    integer, intent(in) :: i, k, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, k, i)
term(1) = term(1) + tovoo(k, d, m, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbkdjam = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbkdjam = eom_cc3_23_tripletp_trans_aibjbkdjam + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkdjam
    function eom_cc3_23_tripletp_trans_aibjbkdlaj(i, k, d, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkdlaj   
    integer, intent(in) :: i, k, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, l, i)
term(1) = term(1) + tovoo(l, d, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbkdlaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbkdlaj = eom_cc3_23_tripletp_trans_aibjbkdlaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkdlaj
    function eom_cc3_23_tripletp_trans_aibjbjdlam(i, d, l, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjdlam   
    integer, intent(in) :: i, d, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, l, i)
term(1) = term(1) + tovoo(l, d, m, i)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjbjdlam = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbjdlam = eom_cc3_23_tripletp_trans_aibjbjdlam + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjdlam
    function eom_cc3_23_tripletp_trans_aibjbkdiej(a, k, d, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkdiej   
    integer, intent(in) :: a, k, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, k, e)
term(1) = term(1) + tvvov(a, e, k, d)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbkdiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbkdiej = eom_cc3_23_tripletp_trans_aibjbkdiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkdiej
    function eom_cc3_23_tripletp_trans_aibjbjdiem(a, d, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjdiem   
    integer, intent(in) :: a, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, e)
term(1) = term(1) + tvvov(a, e, m, d)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjbjdiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbjdiem = eom_cc3_23_tripletp_trans_aibjbjdiem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjdiem
    function eom_cc3_23_tripletp_trans_aibjbjdlei(a, d, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjdlei   
    integer, intent(in) :: a, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, e)
term(1) = term(1) + tvvov(a, e, l, d)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbjdlei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbjdlei = eom_cc3_23_tripletp_trans_aibjbjdlei + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjdlei
    function eom_cc3_23_tripletp_trans_aibjbidjem(a, d, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbidjem   
    integer, intent(in) :: a, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, e)
term(1) = term(1) + tvvov(a, e, m, d)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbidjem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbidjem = eom_cc3_23_tripletp_trans_aibjbidjem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbidjem
    function eom_cc3_23_tripletp_trans_aibjbidlej(a, d, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbidlej   
    integer, intent(in) :: a, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, e)
term(1) = term(1) + tvvov(a, e, l, d)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjbidlej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjbidlej = eom_cc3_23_tripletp_trans_aibjbidlej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbidlej
    function eom_cc3_23_tripletp_trans_aibjckaibm(j, c, k, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckaibm   
    integer, intent(in) :: j, c, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, m, j)
term(1) = term(1) + tovoo(m, c, k, j)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjckaibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjckaibm = eom_cc3_23_tripletp_trans_aibjckaibm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckaibm
    function eom_cc3_23_tripletp_trans_aibjckalbi(j, c, k, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckalbi   
    integer, intent(in) :: j, c, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, l, j)
term(1) = term(1) + tovoo(l, c, k, j)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjckalbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjckalbi = eom_cc3_23_tripletp_trans_aibjckalbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckalbi
    function eom_cc3_23_tripletp_trans_aibjcialbm(j, c, l, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcialbm   
    integer, intent(in) :: j, c, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, m, j)
term(1) = term(1) + tovoo(m, c, l, j)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjcialbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcialbm = eom_cc3_23_tripletp_trans_aibjcialbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcialbm
    function eom_cc3_23_tripletp_trans_aibjckajbm(i, c, k, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckajbm   
    integer, intent(in) :: i, c, k, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, m, i)
term(1) = term(1) + tovoo(m, c, k, i)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjckajbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjckajbm = eom_cc3_23_tripletp_trans_aibjckajbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckajbm
    function eom_cc3_23_tripletp_trans_aibjckalbj(i, c, k, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckalbj   
    integer, intent(in) :: i, c, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, l, i)
term(1) = term(1) + tovoo(l, c, k, i)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjckalbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjckalbj = eom_cc3_23_tripletp_trans_aibjckalbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckalbj
    function eom_cc3_23_tripletp_trans_aibjcjalbm(i, c, l, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjalbm   
    integer, intent(in) :: i, c, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, c, m, i)
term(1) = term(1) + tovoo(m, c, l, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcjalbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcjalbm = eom_cc3_23_tripletp_trans_aibjcjalbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjalbm
    function eom_cc3_23_tripletp_trans_aibjckaiej(b, c, k, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckaiej   
    integer, intent(in) :: b, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, k, e)
term(1) = term(1) + tvvov(b, e, k, c)

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 


    eom_cc3_23_tripletp_trans_aibjckaiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjckaiej = eom_cc3_23_tripletp_trans_aibjckaiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckaiej
    function eom_cc3_23_tripletp_trans_aibjcjaiem(b, c, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjaiem   
    integer, intent(in) :: b, c, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, e)
term(1) = term(1) + tvvov(b, e, m, c)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjcjaiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcjaiem = eom_cc3_23_tripletp_trans_aibjcjaiem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjaiem
    function eom_cc3_23_tripletp_trans_aibjcjalei(b, c, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjalei   
    integer, intent(in) :: b, c, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, e)
term(1) = term(1) + tvvov(b, e, l, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcjalei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcjalei = eom_cc3_23_tripletp_trans_aibjcjalei + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjalei
    function eom_cc3_23_tripletp_trans_aibjciajem(b, c, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjciajem   
    integer, intent(in) :: b, c, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, e)
term(1) = term(1) + tvvov(b, e, m, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjciajem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjciajem = eom_cc3_23_tripletp_trans_aibjciajem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjciajem
    function eom_cc3_23_tripletp_trans_aibjcialej(b, c, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcialej   
    integer, intent(in) :: b, c, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, e)
term(1) = term(1) + tvvov(b, e, l, c)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjcialej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcialej = eom_cc3_23_tripletp_trans_aibjcialej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcialej
    function eom_cc3_23_tripletp_trans_aibjckbiej(a, c, k, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckbiej   
    integer, intent(in) :: a, c, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, e)
term(1) = term(1) + tvvov(a, e, k, c)

term(1) = term(1) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjckbiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjckbiej = eom_cc3_23_tripletp_trans_aibjckbiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckbiej
    function eom_cc3_23_tripletp_trans_aibjcjbiem(a, c, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjbiem   
    integer, intent(in) :: a, c, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, e)
term(1) = term(1) + tvvov(a, e, m, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcjbiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcjbiem = eom_cc3_23_tripletp_trans_aibjcjbiem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjbiem
    function eom_cc3_23_tripletp_trans_aibjcjblei(a, c, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjblei   
    integer, intent(in) :: a, c, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, e)
term(1) = term(1) + tvvov(a, e, l, c)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjcjblei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcjblei = eom_cc3_23_tripletp_trans_aibjcjblei + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjblei
    function eom_cc3_23_tripletp_trans_aibjcibjem(a, c, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcibjem   
    integer, intent(in) :: a, c, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, m, c)
term(1) = term(1) + tvvov(a, c, m, e)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcibjem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcibjem = eom_cc3_23_tripletp_trans_aibjcibjem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcibjem
    function eom_cc3_23_tripletp_trans_aibjciblej(a, c, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjciblej   
    integer, intent(in) :: a, c, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, l, c)
term(1) = term(1) + tvvov(a, c, l, e)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjciblej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjciblej = eom_cc3_23_tripletp_trans_aibjciblej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjciblej
    function eom_cc3_23_tripletp_trans_aibjckdiaj(b, c, k, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckdiaj   
    integer, intent(in) :: b, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, k, d)
term(1) = term(1) + tvvov(b, d, k, c)

term(1) = term(1) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjckdiaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjckdiaj = eom_cc3_23_tripletp_trans_aibjckdiaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckdiaj
    function eom_cc3_23_tripletp_trans_aibjcjdiam(b, c, d, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjdiam   
    integer, intent(in) :: b, c, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, d)
term(1) = term(1) + tvvov(b, d, m, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcjdiam = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcjdiam = eom_cc3_23_tripletp_trans_aibjcjdiam + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjdiam
    function eom_cc3_23_tripletp_trans_aibjcjdlai(b, c, d, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjdlai   
    integer, intent(in) :: b, c, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, d)
term(1) = term(1) + tvvov(b, d, l, c)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjcjdlai = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcjdlai = eom_cc3_23_tripletp_trans_aibjcjdlai + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjdlai
    function eom_cc3_23_tripletp_trans_aibjcidjam(b, c, d, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcidjam   
    integer, intent(in) :: b, c, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, d)
term(1) = term(1) + tvvov(b, d, m, c)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjcidjam = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcidjam = eom_cc3_23_tripletp_trans_aibjcidjam + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcidjam
    function eom_cc3_23_tripletp_trans_aibjcidlaj(b, c, d, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcidlaj   
    integer, intent(in) :: b, c, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, l, c)
term(1) = term(1) + tvvov(b, c, l, d)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjcidlaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcidlaj = eom_cc3_23_tripletp_trans_aibjcidlaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcidlaj
    function eom_cc3_23_tripletp_trans_aibjckdibj(a, c, k, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckdibj   
    integer, intent(in) :: a, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, d)
term(1) = term(1) + tvvov(a, d, k, c)

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 


    eom_cc3_23_tripletp_trans_aibjckdibj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjckdibj = eom_cc3_23_tripletp_trans_aibjckdibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckdibj
    function eom_cc3_23_tripletp_trans_aibjcjdibm(a, c, d, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjdibm   
    integer, intent(in) :: a, c, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, d)
term(1) = term(1) + tvvov(a, d, m, c)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjcjdibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcjdibm = eom_cc3_23_tripletp_trans_aibjcjdibm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjdibm
    function eom_cc3_23_tripletp_trans_aibjcjdlbi(a, c, d, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjdlbi   
    integer, intent(in) :: a, c, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, c)
term(1) = term(1) + tvvov(a, c, l, d)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjcjdlbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcjdlbi = eom_cc3_23_tripletp_trans_aibjcjdlbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjdlbi
    function eom_cc3_23_tripletp_trans_aibjcidjbm(a, c, d, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcidjbm   
    integer, intent(in) :: a, c, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, c)
term(1) = term(1) + tvvov(a, c, m, d)

term(1) = -term(1) 


    eom_cc3_23_tripletp_trans_aibjcidjbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcidjbm = eom_cc3_23_tripletp_trans_aibjcidjbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcidjbm
    function eom_cc3_23_tripletp_trans_aibjcidlbj(a, c, d, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcidlbj   
    integer, intent(in) :: a, c, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, c)
term(1) = term(1) + tvvov(a, c, l, d)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcidlbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletp_trans_aibjcidlbj = eom_cc3_23_tripletp_trans_aibjcidlbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcidlbj
    function eom_cc3_23_tripletp_trans_aibjakaibm(a, j, k, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakaibm   
    integer, intent(in) :: a, j, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjakaibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjakaibm = eom_cc3_23_tripletp_trans_aibjakaibm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakaibm
    function eom_cc3_23_tripletp_trans_aibjakalbi(a, j, k, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakalbi   
    integer, intent(in) :: a, j, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, l, j)



    eom_cc3_23_tripletp_trans_aibjakalbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjakalbi = eom_cc3_23_tripletp_trans_aibjakalbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakalbi
    function eom_cc3_23_tripletp_trans_aibjakajbm(a, i, k, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakajbm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, m, i)



    eom_cc3_23_tripletp_trans_aibjakajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjakajbm = eom_cc3_23_tripletp_trans_aibjakajbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakajbm
    function eom_cc3_23_tripletp_trans_aibjakalbj(a, i, k, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakalbj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, l, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjakalbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjakalbj = eom_cc3_23_tripletp_trans_aibjakalbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakalbj
    function eom_cc3_23_tripletp_trans_aibjakaiej(a, b, k, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakaiej   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, e, k, a)



    eom_cc3_23_tripletp_trans_aibjakaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjakaiej = eom_cc3_23_tripletp_trans_aibjakaiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakaiej
    function eom_cc3_23_tripletp_trans_aibjbkbiej(a, b, k, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkbiej   
    integer, intent(in) :: a, b, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, k, b)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbkbiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbkbiej = eom_cc3_23_tripletp_trans_aibjbkbiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkbiej
    function eom_cc3_23_tripletp_trans_aibjckciaj(b, c, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckciaj   
    integer, intent(in) :: b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, k, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjckciaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjckciaj = eom_cc3_23_tripletp_trans_aibjckciaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckciaj
    function eom_cc3_23_tripletp_trans_aibjckcibj(a, c, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckcibj   
    integer, intent(in) :: a, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, c)



    eom_cc3_23_tripletp_trans_aibjckcibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjckcibj = eom_cc3_23_tripletp_trans_aibjckcibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckcibj
    function eom_cc3_23_tripletp_trans_aibjakdiaj(a, b, k, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakdiaj   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, k, a)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjakdiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjakdiaj = eom_cc3_23_tripletp_trans_aibjakdiaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakdiaj
    function eom_cc3_23_tripletp_trans_aibjakbiej(nocc, a, i, b, j, k, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakbiej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, k, e 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, k, j)
term(1) = term(1) + tovoo(k, e, i, i)
term(2) = term(2) + tovoo(k, e, j, j)
term(3) = term(3) + tovoo(i, e, k, i)
term(4) = term(4) + tov(k, e)
term(5) = term(5) + tvvov(a, a, k, e)
term(6) = term(6) + tvvov(a, e, k, a)
term(7) = term(7) + tvvov(b, b, k, e)
term(8) = term(8) + tvvov(b, e, k, b)

term(1) = -term(1) 
term(2) = -term(2) 
term(6) = term(6) * (-2.0d+0) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(n, e, k, n)
term(10) = term(10) + tovoo(k, e, n, n)
end do 

term(9) = -term(9) 
term(10) = term(10) * 2.0d+0 


    eom_cc3_23_tripletp_trans_aibjakbiej = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjakbiej = eom_cc3_23_tripletp_trans_aibjakbiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakbiej
    function eom_cc3_23_tripletp_trans_aibjajbiem(nocc, a, i, b, j, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajbiem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e, m 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, j, j)
term(1) = term(1) + tovoo(m, e, i, i)
term(2) = term(2) + tovoo(i, e, m, i)
term(3) = term(3) + tovoo(j, e, m, j)
term(4) = term(4) + tov(m, e)
term(5) = term(5) + tvvov(a, a, m, e)
term(6) = term(6) + tvvov(b, b, m, e)
term(7) = term(7) + tvvov(b, e, m, b)
term(8) = term(8) + tvvov(a, e, m, a)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 

do n = 1, nocc 
term(9) = term(9) + tovoo(m, e, n, n)
term(10) = term(10) + tovoo(n, e, m, n)
end do 

term(9) = term(9) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjajbiem = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjajbiem = eom_cc3_23_tripletp_trans_aibjajbiem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajbiem
    function eom_cc3_23_tripletp_trans_aibjajblei(nocc, a, i, b, j, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajblei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, l, e 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, j, j)
term(1) = term(1) + tovoo(i, e, l, i)
term(2) = term(2) + tovoo(l, e, i, i)
term(3) = term(3) + tovoo(j, e, l, j)
term(4) = term(4) + tov(l, e)
term(5) = term(5) + tvvov(a, a, l, e)
term(6) = term(6) + tvvov(b, b, l, e)
term(7) = term(7) + tvvov(b, e, l, b)
term(8) = term(8) + tvvov(a, e, l, a)

term(0) = -term(0) 
term(2) = -term(2) 
term(7) = -term(7) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(n, e, l, n)
term(10) = term(10) + tovoo(l, e, n, n)
end do 

term(9) = -term(9) 
term(10) = term(10) * 2.0d+0 


    eom_cc3_23_tripletp_trans_aibjajblei = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjajblei = eom_cc3_23_tripletp_trans_aibjajblei + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajblei
    function eom_cc3_23_tripletp_trans_aibjaibjem(nocc, a, i, b, j, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaibjem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e, m 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, m, i)
term(1) = term(1) + tovoo(m, e, i, i)
term(2) = term(2) + tovoo(m, e, j, j)
term(3) = term(3) + tovoo(j, e, m, j)
term(4) = term(4) + tov(m, e)
term(5) = term(5) + tvvov(a, e, m, a)
term(6) = term(6) + tvvov(a, a, m, e)
term(7) = term(7) + tvvov(b, b, m, e)
term(8) = term(8) + tvvov(b, e, m, b)

term(1) = -term(1) 
term(2) = -term(2) 
term(5) = -term(5) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(m, e, n, n)
term(10) = term(10) + tovoo(n, e, m, n)
end do 

term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 


    eom_cc3_23_tripletp_trans_aibjaibjem = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjaibjem = eom_cc3_23_tripletp_trans_aibjaibjem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaibjem
    function eom_cc3_23_tripletp_trans_aibjaiblej(nocc, a, i, b, j, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaiblej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, l, e 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, l, i)
term(1) = term(1) + tovoo(l, e, i, i)
term(2) = term(2) + tovoo(j, e, l, j)
term(3) = term(3) + tovoo(l, e, j, j)
term(4) = term(4) + tov(l, e)
term(5) = term(5) + tvvov(a, e, l, a)
term(6) = term(6) + tvvov(a, a, l, e)
term(7) = term(7) + tvvov(b, b, l, e)
term(8) = term(8) + tvvov(b, e, l, b)

term(0) = -term(0) 
term(2) = -term(2) 
term(4) = -term(4) 
term(6) = -term(6) 
term(7) = -term(7) 

do n = 1, nocc 
term(9) = term(9) + tovoo(n, e, l, n)
term(10) = term(10) + tovoo(l, e, n, n)
end do 

term(10) = term(10) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjaiblej = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjaiblej = eom_cc3_23_tripletp_trans_aibjaiblej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaiblej
    function eom_cc3_23_tripletp_trans_aibjakdibj(nocc, a, i, b, j, k, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakdibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, k, j)
term(1) = term(1) + tovoo(k, d, i, i)
term(2) = term(2) + tovoo(k, d, j, j)
term(3) = term(3) + tovoo(i, d, k, i)
term(4) = term(4) + tov(k, d)
term(5) = term(5) + tvvov(a, a, k, d)
term(6) = term(6) + tvvov(a, d, k, a)
term(7) = term(7) + tvvov(b, d, k, b)
term(8) = term(8) + tvvov(b, b, k, d)

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = term(6) * 2.0d+0 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(n, d, k, n)
term(10) = term(10) + tovoo(k, d, n, n)
end do 

term(10) = term(10) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjakdibj = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjakdibj = eom_cc3_23_tripletp_trans_aibjakdibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakdibj
    function eom_cc3_23_tripletp_trans_aibjajdibm(nocc, a, i, b, j, d, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajdibm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, m 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, j, j)
term(1) = term(1) + tovoo(j, d, m, j)
term(2) = term(2) + tovoo(m, d, i, i)
term(3) = term(3) + tovoo(i, d, m, i)
term(4) = term(4) + tov(m, d)
term(5) = term(5) + tvvov(a, a, m, d)
term(6) = term(6) + tvvov(b, d, m, b)
term(7) = term(7) + tvvov(a, d, m, a)
term(8) = term(8) + tvvov(b, b, m, d)

term(0) = -term(0) 
term(2) = -term(2) 
term(6) = -term(6) 
term(7) = -term(7) 

do n = 1, nocc 
term(9) = term(9) + tovoo(m, d, n, n)
term(10) = term(10) + tovoo(n, d, m, n)
end do 

term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 


    eom_cc3_23_tripletp_trans_aibjajdibm = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjajdibm = eom_cc3_23_tripletp_trans_aibjajdibm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajdibm
    function eom_cc3_23_tripletp_trans_aibjajdlbi(nocc, a, i, b, j, d, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajdlbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, l, j)
term(1) = term(1) + tovoo(i, d, l, i)
term(2) = term(2) + tovoo(l, d, j, j)
term(3) = term(3) + tovoo(l, d, i, i)
term(4) = term(4) + tov(l, d)
term(5) = term(5) + tvvov(b, d, l, b)
term(6) = term(6) + tvvov(a, d, l, a)
term(7) = term(7) + tvvov(a, a, l, d)
term(8) = term(8) + tvvov(b, b, l, d)

term(0) = -term(0) 
term(1) = -term(1) 
term(4) = -term(4) 
term(7) = -term(7) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(l, d, n, n)
term(10) = term(10) + tovoo(n, d, l, n)
end do 

term(9) = term(9) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjajdlbi = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjajdlbi = eom_cc3_23_tripletp_trans_aibjajdlbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajdlbi
    function eom_cc3_23_tripletp_trans_aibjaidjbm(nocc, a, i, b, j, d, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaidjbm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, m 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, m, i)
term(1) = term(1) + tovoo(m, d, i, i)
term(2) = term(2) + tovoo(m, d, j, j)
term(3) = term(3) + tovoo(j, d, m, j)
term(4) = term(4) + tov(m, d)
term(5) = term(5) + tvvov(a, d, m, a)
term(6) = term(6) + tvvov(a, a, m, d)
term(7) = term(7) + tvvov(b, d, m, b)
term(8) = term(8) + tvvov(b, b, m, d)

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(m, d, n, n)
term(10) = term(10) + tovoo(n, d, m, n)
end do 

term(9) = term(9) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjaidjbm = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjaidjbm = eom_cc3_23_tripletp_trans_aibjaidjbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaidjbm
    function eom_cc3_23_tripletp_trans_aibjaidlbj(nocc, a, i, b, j, d, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaidlbj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, l, i)
term(1) = term(1) + tovoo(j, d, l, j)
term(2) = term(2) + tovoo(l, d, i, i)
term(3) = term(3) + tovoo(l, d, j, j)
term(4) = term(4) + tov(l, d)
term(5) = term(5) + tvvov(a, d, l, a)
term(6) = term(6) + tvvov(b, d, l, b)
term(7) = term(7) + tvvov(a, a, l, d)
term(8) = term(8) + tvvov(b, b, l, d)

term(2) = -term(2) 
term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 

do n = 1, nocc 
term(9) = term(9) + tovoo(l, d, n, n)
term(10) = term(10) + tovoo(n, d, l, n)
end do 

term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 


    eom_cc3_23_tripletp_trans_aibjaidlbj = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjaidlbj = eom_cc3_23_tripletp_trans_aibjaidlbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaidlbj
    function eom_cc3_23_tripletp_trans_aibjbkaibm(b, j, k, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkaibm   
    integer, intent(in) :: b, j, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbkaibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbkaibm = eom_cc3_23_tripletp_trans_aibjbkaibm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkaibm
    function eom_cc3_23_tripletp_trans_aibjbkalbi(b, j, k, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkalbi   
    integer, intent(in) :: b, j, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, l, j)



    eom_cc3_23_tripletp_trans_aibjbkalbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbkalbi = eom_cc3_23_tripletp_trans_aibjbkalbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkalbi
    function eom_cc3_23_tripletp_trans_aibjbkajbm(i, b, k, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkajbm   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, m, i)



    eom_cc3_23_tripletp_trans_aibjbkajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbkajbm = eom_cc3_23_tripletp_trans_aibjbkajbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkajbm
    function eom_cc3_23_tripletp_trans_aibjbkalbj(i, b, k, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkalbj   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, l, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbkalbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbkalbj = eom_cc3_23_tripletp_trans_aibjbkalbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkalbj
    function eom_cc3_23_tripletp_trans_aibjbkdibj(a, b, k, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkdibj   
    integer, intent(in) :: a, b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, k, b)



    eom_cc3_23_tripletp_trans_aibjbkdibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbkdibj = eom_cc3_23_tripletp_trans_aibjbkdibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkdibj
    function eom_cc3_23_tripletp_trans_aibjbkaiej(nocc, a, i, b, j, k, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkaiej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, k, e 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, k, i)
term(1) = term(1) + tovoo(j, e, k, j)
term(2) = term(2) + tovoo(k, e, i, i)
term(3) = term(3) + tovoo(k, e, j, j)
term(4) = term(4) + tov(k, e)
term(5) = term(5) + tvvov(b, b, k, e)
term(6) = term(6) + tvvov(b, e, k, b)
term(7) = term(7) + tvvov(a, a, k, e)
term(8) = term(8) + tvvov(a, e, k, a)

term(0) = -term(0) 
term(1) = -term(1) 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 

do n = 1, nocc 
term(9) = term(9) + tovoo(n, e, k, n)
term(10) = term(10) + tovoo(k, e, n, n)
end do 

term(10) = term(10) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjbkaiej = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjbkaiej = eom_cc3_23_tripletp_trans_aibjbkaiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkaiej
    function eom_cc3_23_tripletp_trans_aibjbjaiem(nocc, a, i, b, j, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjaiem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e, m 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, j, j)
term(1) = term(1) + tovoo(m, e, i, i)
term(2) = term(2) + tovoo(i, e, m, i)
term(3) = term(3) + tovoo(j, e, m, j)
term(4) = term(4) + tov(m, e)
term(5) = term(5) + tvvov(b, b, m, e)
term(6) = term(6) + tvvov(a, a, m, e)
term(7) = term(7) + tvvov(a, e, m, a)
term(8) = term(8) + tvvov(b, e, m, b)

term(0) = -term(0) 
term(1) = -term(1) 
term(7) = -term(7) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(m, e, n, n)
term(10) = term(10) + tovoo(n, e, m, n)
end do 

term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 


    eom_cc3_23_tripletp_trans_aibjbjaiem = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjbjaiem = eom_cc3_23_tripletp_trans_aibjbjaiem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjaiem
    function eom_cc3_23_tripletp_trans_aibjbjalei(nocc, a, i, b, j, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjalei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, l, e 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, j, j)
term(1) = term(1) + tovoo(j, e, l, j)
term(2) = term(2) + tovoo(i, e, l, i)
term(3) = term(3) + tovoo(l, e, i, i)
term(4) = term(4) + tov(l, e)
term(5) = term(5) + tvvov(b, b, l, e)
term(6) = term(6) + tvvov(a, a, l, e)
term(7) = term(7) + tvvov(b, e, l, b)
term(8) = term(8) + tvvov(a, e, l, a)

term(1) = -term(1) 
term(2) = -term(2) 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 

do n = 1, nocc 
term(9) = term(9) + tovoo(n, e, l, n)
term(10) = term(10) + tovoo(l, e, n, n)
end do 

term(10) = term(10) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjbjalei = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjbjalei = eom_cc3_23_tripletp_trans_aibjbjalei + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjalei
    function eom_cc3_23_tripletp_trans_aibjbiajem(nocc, a, i, b, j, e, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbiajem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e, m 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, i, i)
term(1) = term(1) + tovoo(m, e, j, j)
term(2) = term(2) + tovoo(i, e, m, i)
term(3) = term(3) + tovoo(j, e, m, j)
term(4) = term(4) + tov(m, e)
term(5) = term(5) + tvvov(b, b, m, e)
term(6) = term(6) + tvvov(a, a, m, e)
term(7) = term(7) + tvvov(a, e, m, a)
term(8) = term(8) + tvvov(b, e, m, b)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 

do n = 1, nocc 
term(9) = term(9) + tovoo(m, e, n, n)
term(10) = term(10) + tovoo(n, e, m, n)
end do 

term(9) = term(9) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjbiajem = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjbiajem = eom_cc3_23_tripletp_trans_aibjbiajem + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbiajem
    function eom_cc3_23_tripletp_trans_aibjbialej(nocc, a, i, b, j, l, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbialej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, l, e 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, i, i)
term(1) = term(1) + tovoo(i, e, l, i)
term(2) = term(2) + tovoo(j, e, l, j)
term(3) = term(3) + tovoo(l, e, j, j)
term(4) = term(4) + tov(l, e)
term(5) = term(5) + tvvov(b, b, l, e)
term(6) = term(6) + tvvov(a, a, l, e)
term(7) = term(7) + tvvov(b, e, l, b)
term(8) = term(8) + tvvov(a, e, l, a)

term(0) = -term(0) 
term(3) = -term(3) 
term(7) = -term(7) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(n, e, l, n)
term(10) = term(10) + tovoo(l, e, n, n)
end do 

term(9) = -term(9) 
term(10) = term(10) * 2.0d+0 


    eom_cc3_23_tripletp_trans_aibjbialej = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjbialej = eom_cc3_23_tripletp_trans_aibjbialej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbialej
    function eom_cc3_23_tripletp_trans_aibjbkdiaj(nocc, a, i, b, j, k, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkdiaj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, k, j)
term(1) = term(1) + tovoo(k, d, i, i)
term(2) = term(2) + tovoo(k, d, j, j)
term(3) = term(3) + tovoo(i, d, k, i)
term(4) = term(4) + tov(k, d)
term(5) = term(5) + tvvov(b, b, k, d)
term(6) = term(6) + tvvov(b, d, k, b)
term(7) = term(7) + tvvov(a, d, k, a)
term(8) = term(8) + tvvov(a, a, k, d)

term(1) = -term(1) 
term(2) = -term(2) 
term(6) = term(6) * (-2.0d+0) 
term(7) = -term(7) 

do n = 1, nocc 
term(9) = term(9) + tovoo(n, d, k, n)
term(10) = term(10) + tovoo(k, d, n, n)
end do 

term(9) = -term(9) 
term(10) = term(10) * 2.0d+0 


    eom_cc3_23_tripletp_trans_aibjbkdiaj = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjbkdiaj = eom_cc3_23_tripletp_trans_aibjbkdiaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkdiaj
    function eom_cc3_23_tripletp_trans_aibjbjdiam(nocc, a, i, b, j, d, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjdiam 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, m 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, j, j)
term(1) = term(1) + tovoo(j, d, m, j)
term(2) = term(2) + tovoo(m, d, i, i)
term(3) = term(3) + tovoo(i, d, m, i)
term(4) = term(4) + tov(m, d)
term(5) = term(5) + tvvov(b, b, m, d)
term(6) = term(6) + tvvov(a, d, m, a)
term(7) = term(7) + tvvov(b, d, m, b)
term(8) = term(8) + tvvov(a, a, m, d)

term(1) = -term(1) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(m, d, n, n)
term(10) = term(10) + tovoo(n, d, m, n)
end do 

term(9) = term(9) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjbjdiam = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjbjdiam = eom_cc3_23_tripletp_trans_aibjbjdiam + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjdiam
    function eom_cc3_23_tripletp_trans_aibjbjdlai(nocc, a, i, b, j, d, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjdlai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, l, i)
term(1) = term(1) + tovoo(l, d, j, j)
term(2) = term(2) + tovoo(l, d, i, i)
term(3) = term(3) + tovoo(j, d, l, j)
term(4) = term(4) + tov(l, d)
term(5) = term(5) + tvvov(a, d, l, a)
term(6) = term(6) + tvvov(b, b, l, d)
term(7) = term(7) + tvvov(a, a, l, d)
term(8) = term(8) + tvvov(b, d, l, b)

term(1) = -term(1) 
term(2) = -term(2) 
term(5) = -term(5) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(l, d, n, n)
term(10) = term(10) + tovoo(n, d, l, n)
end do 

term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 


    eom_cc3_23_tripletp_trans_aibjbjdlai = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjbjdlai = eom_cc3_23_tripletp_trans_aibjbjdlai + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjdlai
    function eom_cc3_23_tripletp_trans_aibjbidjam(nocc, a, i, b, j, d, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbidjam 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, m 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, i, i)
term(1) = term(1) + tovoo(m, d, j, j)
term(2) = term(2) + tovoo(i, d, m, i)
term(3) = term(3) + tovoo(j, d, m, j)
term(4) = term(4) + tov(m, d)
term(5) = term(5) + tvvov(b, b, m, d)
term(6) = term(6) + tvvov(a, d, m, a)
term(7) = term(7) + tvvov(a, a, m, d)
term(8) = term(8) + tvvov(b, d, m, b)

term(0) = -term(0) 
term(1) = -term(1) 
term(6) = -term(6) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(m, d, n, n)
term(10) = term(10) + tovoo(n, d, m, n)
end do 

term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 


    eom_cc3_23_tripletp_trans_aibjbidjam = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjbidjam = eom_cc3_23_tripletp_trans_aibjbidjam + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbidjam
    function eom_cc3_23_tripletp_trans_aibjbidlaj(nocc, a, i, b, j, d, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbidlaj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, l, i)
term(1) = term(1) + tovoo(j, d, l, j)
term(2) = term(2) + tovoo(l, d, i, i)
term(3) = term(3) + tovoo(l, d, j, j)
term(4) = term(4) + tov(l, d)
term(5) = term(5) + tvvov(a, d, l, a)
term(6) = term(6) + tvvov(b, d, l, b)
term(7) = term(7) + tvvov(b, b, l, d)
term(8) = term(8) + tvvov(a, a, l, d)

term(0) = -term(0) 
term(1) = -term(1) 
term(4) = -term(4) 
term(7) = -term(7) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(l, d, n, n)
term(10) = term(10) + tovoo(n, d, l, n)
end do 

term(9) = term(9) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjbidlaj = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjbidlaj = eom_cc3_23_tripletp_trans_aibjbidlaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbidlaj
    function eom_cc3_23_tripletp_trans_aibjckaicj(b, c, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckaicj   
    integer, intent(in) :: b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, k, c)



    eom_cc3_23_tripletp_trans_aibjckaicj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjckaicj = eom_cc3_23_tripletp_trans_aibjckaicj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckaicj
    function eom_cc3_23_tripletp_trans_aibjckbicj(a, c, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckbicj   
    integer, intent(in) :: a, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, k, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjckbicj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjckbicj = eom_cc3_23_tripletp_trans_aibjckbicj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckbicj
    function eom_cc3_23_tripletp_trans_aibjciaibm(i, j, c, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjciaibm   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, c, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjciaibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjciaibm = eom_cc3_23_tripletp_trans_aibjciaibm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjciaibm
    function eom_cc3_23_tripletp_trans_aibjckaibj(nocc, a, i, b, j, c, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, i, i)
term(1) = term(1) + tovoo(k, c, j, j)
term(2) = term(2) + tovoo(i, c, k, i)
term(3) = term(3) + tovoo(j, c, k, j)
term(4) = term(4) + tov(k, c)
term(5) = term(5) + tvvov(a, c, k, a)
term(6) = term(6) + tvvov(b, c, k, b)
term(7) = term(7) + tvvov(a, a, k, c)
term(8) = term(8) + tvvov(b, b, k, c)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (-2.0d+0) 
term(4) = term(4) * 2.0d+0 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = term(7) * 2.0d+0 
term(8) = term(8) * 2.0d+0 

do n = 1, nocc 
term(9) = term(9) + tovoo(k, c, n, n)
term(10) = term(10) + tovoo(n, c, k, n)
end do 

term(9) = term(9) * 4.0d+0 
term(10) = term(10) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjckaibj = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjckaibj = eom_cc3_23_tripletp_trans_aibjckaibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckaibj
    function eom_cc3_23_tripletp_trans_aibjcjaibm(nocc, a, i, b, j, c, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjaibm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c, m 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, c, m, j)
term(1) = term(1) + tovoo(m, c, j, j)
term(2) = term(2) + tovoo(i, c, m, i)
term(3) = term(3) + tovoo(m, c, i, i)
term(4) = term(4) + tov(m, c)
term(5) = term(5) + tvvov(a, c, m, a)
term(6) = term(6) + tvvov(b, c, m, b)
term(7) = term(7) + tvvov(a, a, m, c)
term(8) = term(8) + tvvov(b, b, m, c)

term(0) = term(0) * (-2.0d+0) 
term(2) = -term(2) 
term(4) = -term(4) 
term(7) = -term(7) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(m, c, n, n)
term(10) = term(10) + tovoo(n, c, m, n)
end do 

term(9) = term(9) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjcjaibm = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjcjaibm = eom_cc3_23_tripletp_trans_aibjcjaibm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjaibm
    function eom_cc3_23_tripletp_trans_aibjckaibk(j, c, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckaibk   
    integer, intent(in) :: j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjckaibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjckaibk = eom_cc3_23_tripletp_trans_aibjckaibk + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckaibk
    function eom_cc3_23_tripletp_trans_aibjcialbi(i, j, c, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcialbi   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, c, l, j)



    eom_cc3_23_tripletp_trans_aibjcialbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcialbi = eom_cc3_23_tripletp_trans_aibjcialbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcialbi
    function eom_cc3_23_tripletp_trans_aibjckakbi(j, c, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckakbi   
    integer, intent(in) :: j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, j)



    eom_cc3_23_tripletp_trans_aibjckakbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjckakbi = eom_cc3_23_tripletp_trans_aibjckakbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckakbi
    function eom_cc3_23_tripletp_trans_aibjcjalbi(nocc, a, i, b, j, c, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjalbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, c, l, j)
term(1) = term(1) + tovoo(l, c, j, j)
term(2) = term(2) + tovoo(l, c, i, i)
term(3) = term(3) + tovoo(i, c, l, i)
term(4) = term(4) + tov(l, c)
term(5) = term(5) + tvvov(b, c, l, b)
term(6) = term(6) + tvvov(a, a, l, c)
term(7) = term(7) + tvvov(b, b, l, c)
term(8) = term(8) + tvvov(a, c, l, a)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(5) = -term(5) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(l, c, n, n)
term(10) = term(10) + tovoo(n, c, l, n)
end do 

term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 


    eom_cc3_23_tripletp_trans_aibjcjalbi = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjcjalbi = eom_cc3_23_tripletp_trans_aibjcjalbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjalbi
    function eom_cc3_23_tripletp_trans_aibjciajbm(nocc, a, i, b, j, c, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjciajbm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c, m 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, c, m, i)
term(1) = term(1) + tovoo(m, c, i, i)
term(2) = term(2) + tovoo(j, c, m, j)
term(3) = term(3) + tovoo(m, c, j, j)
term(4) = term(4) + tov(m, c)
term(5) = term(5) + tvvov(b, c, m, b)
term(6) = term(6) + tvvov(a, a, m, c)
term(7) = term(7) + tvvov(b, b, m, c)
term(8) = term(8) + tvvov(a, c, m, a)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(3) = -term(3) 
term(5) = -term(5) 
term(8) = -term(8) 

do n = 1, nocc 
term(9) = term(9) + tovoo(m, c, n, n)
term(10) = term(10) + tovoo(n, c, m, n)
end do 

term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 


    eom_cc3_23_tripletp_trans_aibjciajbm = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjciajbm = eom_cc3_23_tripletp_trans_aibjciajbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjciajbm
    function eom_cc3_23_tripletp_trans_aibjcialbj(nocc, a, i, b, j, c, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcialbj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,n 
    double precision, dimension(0:10) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, c, l, i)
term(1) = term(1) + tovoo(l, c, i, i)
term(2) = term(2) + tovoo(l, c, j, j)
term(3) = term(3) + tovoo(j, c, l, j)
term(4) = term(4) + tov(l, c)
term(5) = term(5) + tvvov(b, c, l, b)
term(6) = term(6) + tvvov(a, a, l, c)
term(7) = term(7) + tvvov(b, b, l, c)
term(8) = term(8) + tvvov(a, c, l, a)

term(0) = term(0) * (-2.0d+0) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 
term(7) = -term(7) 

do n = 1, nocc 
term(9) = term(9) + tovoo(l, c, n, n)
term(10) = term(10) + tovoo(n, c, l, n)
end do 

term(9) = term(9) * (-2.0d+0) 


    eom_cc3_23_tripletp_trans_aibjcialbj = 0.d+0
    do s = 0, 10
    eom_cc3_23_tripletp_trans_aibjcialbj = eom_cc3_23_tripletp_trans_aibjcialbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcialbj
    function eom_cc3_23_tripletp_trans_aibjcjajbm(i, j, c, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjajbm   
    integer, intent(in) :: i, j, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, c, m, i)



    eom_cc3_23_tripletp_trans_aibjcjajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcjajbm = eom_cc3_23_tripletp_trans_aibjcjajbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjajbm
    function eom_cc3_23_tripletp_trans_aibjckajbk(i, c, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckajbk   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, i)



    eom_cc3_23_tripletp_trans_aibjckajbk = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjckajbk = eom_cc3_23_tripletp_trans_aibjckajbk + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckajbk
    function eom_cc3_23_tripletp_trans_aibjckakbj(i, c, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjckakbj   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, c, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjckakbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjckakbj = eom_cc3_23_tripletp_trans_aibjckakbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjckakbj
    function eom_cc3_23_tripletp_trans_aibjcjalbj(i, j, c, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjalbj   
    integer, intent(in) :: i, j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, c, l, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcjalbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcjalbj = eom_cc3_23_tripletp_trans_aibjcjalbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjalbj
    function eom_cc3_23_tripletp_trans_aibjciaiej(i, b, c, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjciaiej   
    integer, intent(in) :: i, b, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, e, i, c)



    eom_cc3_23_tripletp_trans_aibjciaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjciaiej = eom_cc3_23_tripletp_trans_aibjciaiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjciaiej
    function eom_cc3_23_tripletp_trans_aibjcjaiej(b, j, c, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjaiej   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, e, j, c)



    eom_cc3_23_tripletp_trans_aibjcjaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcjaiej = eom_cc3_23_tripletp_trans_aibjcjaiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjaiej
    function eom_cc3_23_tripletp_trans_aibjcibiej(a, i, c, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcibiej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, i, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcibiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcibiej = eom_cc3_23_tripletp_trans_aibjcibiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcibiej
    function eom_cc3_23_tripletp_trans_aibjcjbiej(a, j, c, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjbiej   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, j, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcjbiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcjbiej = eom_cc3_23_tripletp_trans_aibjcjbiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjbiej
    function eom_cc3_23_tripletp_trans_aibjcidiaj(i, b, c, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcidiaj   
    integer, intent(in) :: i, b, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, i, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcidiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcidiaj = eom_cc3_23_tripletp_trans_aibjcidiaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcidiaj
    function eom_cc3_23_tripletp_trans_aibjcjdiaj(b, j, c, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjdiaj   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, j, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcjdiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcjdiaj = eom_cc3_23_tripletp_trans_aibjcjdiaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjdiaj
    function eom_cc3_23_tripletp_trans_aibjcidibj(a, i, c, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcidibj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, c)



    eom_cc3_23_tripletp_trans_aibjcidibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcidibj = eom_cc3_23_tripletp_trans_aibjcidibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcidibj
    function eom_cc3_23_tripletp_trans_aibjcjdibj(a, j, c, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjdibj   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, c)



    eom_cc3_23_tripletp_trans_aibjcjdibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcjdibj = eom_cc3_23_tripletp_trans_aibjcjdibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjdibj
    function eom_cc3_23_tripletp_trans_aibjaiaibm(a, i, j, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaiaibm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjaiaibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjaiaibm = eom_cc3_23_tripletp_trans_aibjaiaibm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaiaibm
    function eom_cc3_23_tripletp_trans_aibjakaibj(nocc, a, i, b, j, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, i, i)
term(1) = term(1) + tovoo(k, a, j, j)
term(2) = term(2) + tvvov(a, a, k, a)
term(3) = term(3) + tov(k, a)
term(4) = term(4) + tvvov(b, b, k, a)

term(0) = -term(0) 
term(1) = -term(1) 

do n = 1, nocc 
term(5) = term(5) + tovoo(k, a, n, n)
term(6) = term(6) + tovoo(n, a, k, n)
end do 

term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 


    eom_cc3_23_tripletp_trans_aibjakaibj = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletp_trans_aibjakaibj = eom_cc3_23_tripletp_trans_aibjakaibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakaibj
    function eom_cc3_23_tripletp_trans_aibjajaibm(a, j, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajaibm   
    integer, intent(in) :: a, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, a, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjajaibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjajaibm = eom_cc3_23_tripletp_trans_aibjajaibm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajaibm
    function eom_cc3_23_tripletp_trans_aibjakaibk(a, j, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakaibk   
    integer, intent(in) :: a, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjakaibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjakaibk = eom_cc3_23_tripletp_trans_aibjakaibk + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakaibk
    function eom_cc3_23_tripletp_trans_aibjaialbi(a, i, j, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaialbi   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, l, j)



    eom_cc3_23_tripletp_trans_aibjaialbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjaialbi = eom_cc3_23_tripletp_trans_aibjaialbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaialbi
    function eom_cc3_23_tripletp_trans_aibjakakbi(a, j, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakakbi   
    integer, intent(in) :: a, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, k, j)



    eom_cc3_23_tripletp_trans_aibjakakbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjakakbi = eom_cc3_23_tripletp_trans_aibjakakbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakakbi
    function eom_cc3_23_tripletp_trans_aibjajalbi(a, j, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajalbi   
    integer, intent(in) :: a, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, a, l, j)



    eom_cc3_23_tripletp_trans_aibjajalbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjajalbi = eom_cc3_23_tripletp_trans_aibjajalbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajalbi
    function eom_cc3_23_tripletp_trans_aibjaiajbm(a, i, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaiajbm   
    integer, intent(in) :: a, i, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, m, i)



    eom_cc3_23_tripletp_trans_aibjaiajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjaiajbm = eom_cc3_23_tripletp_trans_aibjaiajbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaiajbm
    function eom_cc3_23_tripletp_trans_aibjaialbj(a, i, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaialbj   
    integer, intent(in) :: a, i, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, l, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjaialbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjaialbj = eom_cc3_23_tripletp_trans_aibjaialbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaialbj
    function eom_cc3_23_tripletp_trans_aibjajajbm(a, i, j, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajajbm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, a, m, i)



    eom_cc3_23_tripletp_trans_aibjajajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjajajbm = eom_cc3_23_tripletp_trans_aibjajajbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajajbm
    function eom_cc3_23_tripletp_trans_aibjakajbk(a, i, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakajbk   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, k, i)



    eom_cc3_23_tripletp_trans_aibjakajbk = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjakajbk = eom_cc3_23_tripletp_trans_aibjakajbk + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakajbk
    function eom_cc3_23_tripletp_trans_aibjakakbj(a, i, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjakakbj   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjakakbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjakakbj = eom_cc3_23_tripletp_trans_aibjakakbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjakakbj
    function eom_cc3_23_tripletp_trans_aibjajalbj(a, i, j, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajalbj   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, a, l, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjajalbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjajalbj = eom_cc3_23_tripletp_trans_aibjajalbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajalbj
    function eom_cc3_23_tripletp_trans_aibjaiaiej(a, i, b, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaiaiej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, e, i, a)



    eom_cc3_23_tripletp_trans_aibjaiaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjaiaiej = eom_cc3_23_tripletp_trans_aibjaiaiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaiaiej
    function eom_cc3_23_tripletp_trans_aibjajaiej(a, b, j, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajaiej   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, e, j, a)



    eom_cc3_23_tripletp_trans_aibjajaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjajaiej = eom_cc3_23_tripletp_trans_aibjajaiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajaiej
    function eom_cc3_23_tripletp_trans_aibjbibiej(a, i, b, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbibiej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, i, b)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbibiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbibiej = eom_cc3_23_tripletp_trans_aibjbibiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbibiej
    function eom_cc3_23_tripletp_trans_aibjbjbiej(a, b, j, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjbiej   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, j, b)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbjbiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbjbiej = eom_cc3_23_tripletp_trans_aibjbjbiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjbiej
    function eom_cc3_23_tripletp_trans_aibjciciaj(i, b, c) 
    double precision :: eom_cc3_23_tripletp_trans_aibjciciaj   
    integer, intent(in) :: i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, i, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjciciaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjciciaj = eom_cc3_23_tripletp_trans_aibjciciaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjciciaj
    function eom_cc3_23_tripletp_trans_aibjcjciaj(b, j, c) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjciaj   
    integer, intent(in) :: b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcjciaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcjciaj = eom_cc3_23_tripletp_trans_aibjcjciaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjciaj
    function eom_cc3_23_tripletp_trans_aibjcicibj(a, i, c) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcicibj   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, c)



    eom_cc3_23_tripletp_trans_aibjcicibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcicibj = eom_cc3_23_tripletp_trans_aibjcicibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcicibj
    function eom_cc3_23_tripletp_trans_aibjcjcibj(a, j, c) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjcibj   
    integer, intent(in) :: a, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, c)



    eom_cc3_23_tripletp_trans_aibjcjcibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcjcibj = eom_cc3_23_tripletp_trans_aibjcjcibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjcibj
    function eom_cc3_23_tripletp_trans_aibjaidiaj(a, i, b, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaidiaj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, i, a)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjaidiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjaidiaj = eom_cc3_23_tripletp_trans_aibjaidiaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaidiaj
    function eom_cc3_23_tripletp_trans_aibjajdiaj(a, b, j, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajdiaj   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, j, a)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjajdiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjajdiaj = eom_cc3_23_tripletp_trans_aibjajdiaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajdiaj
    function eom_cc3_23_tripletp_trans_aibjaibiej(a, i, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaibiej   
    integer, intent(in) :: a, i, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, i, a)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjaibiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjaibiej = eom_cc3_23_tripletp_trans_aibjaibiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaibiej
    function eom_cc3_23_tripletp_trans_aibjajbiej(a, j, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajbiej   
    integer, intent(in) :: a, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, j, a)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjajbiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjajbiej = eom_cc3_23_tripletp_trans_aibjajbiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajbiej
    function eom_cc3_23_tripletp_trans_aibjaidibj(a, i, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaidibj   
    integer, intent(in) :: a, i, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, a)



    eom_cc3_23_tripletp_trans_aibjaidibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjaidibj = eom_cc3_23_tripletp_trans_aibjaidibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaidibj
    function eom_cc3_23_tripletp_trans_aibjajdibj(a, j, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajdibj   
    integer, intent(in) :: a, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, a)



    eom_cc3_23_tripletp_trans_aibjajdibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjajdibj = eom_cc3_23_tripletp_trans_aibjajdibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajdibj
    function eom_cc3_23_tripletp_trans_aibjbiaibm(i, b, j, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbiaibm   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, b, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbiaibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbiaibm = eom_cc3_23_tripletp_trans_aibjbiaibm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbiaibm
    function eom_cc3_23_tripletp_trans_aibjbkaibj(nocc, a, i, b, j, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, i, i)
term(1) = term(1) + tovoo(k, b, j, j)
term(2) = term(2) + tvvov(b, b, k, b)
term(3) = term(3) + tov(k, b)
term(4) = term(4) + tvvov(a, a, k, b)

term(0) = -term(0) 
term(1) = -term(1) 

do n = 1, nocc 
term(5) = term(5) + tovoo(k, b, n, n)
term(6) = term(6) + tovoo(n, b, k, n)
end do 

term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 


    eom_cc3_23_tripletp_trans_aibjbkaibj = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletp_trans_aibjbkaibj = eom_cc3_23_tripletp_trans_aibjbkaibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkaibj
    function eom_cc3_23_tripletp_trans_aibjbjaibm(b, j, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjaibm   
    integer, intent(in) :: b, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbjaibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbjaibm = eom_cc3_23_tripletp_trans_aibjbjaibm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjaibm
    function eom_cc3_23_tripletp_trans_aibjbkaibk(b, j, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkaibk   
    integer, intent(in) :: b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbkaibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbkaibk = eom_cc3_23_tripletp_trans_aibjbkaibk + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkaibk
    function eom_cc3_23_tripletp_trans_aibjbialbi(i, b, j, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbialbi   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, b, l, j)



    eom_cc3_23_tripletp_trans_aibjbialbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbialbi = eom_cc3_23_tripletp_trans_aibjbialbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbialbi
    function eom_cc3_23_tripletp_trans_aibjbkakbi(b, j, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkakbi   
    integer, intent(in) :: b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, k, j)



    eom_cc3_23_tripletp_trans_aibjbkakbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbkakbi = eom_cc3_23_tripletp_trans_aibjbkakbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkakbi
    function eom_cc3_23_tripletp_trans_aibjbjalbi(b, j, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjalbi   
    integer, intent(in) :: b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, l, j)



    eom_cc3_23_tripletp_trans_aibjbjalbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbjalbi = eom_cc3_23_tripletp_trans_aibjbjalbi + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjalbi
    function eom_cc3_23_tripletp_trans_aibjbiajbm(i, b, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbiajbm   
    integer, intent(in) :: i, b, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, b, m, i)



    eom_cc3_23_tripletp_trans_aibjbiajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbiajbm = eom_cc3_23_tripletp_trans_aibjbiajbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbiajbm
    function eom_cc3_23_tripletp_trans_aibjbialbj(i, b, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbialbj   
    integer, intent(in) :: i, b, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, b, l, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbialbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbialbj = eom_cc3_23_tripletp_trans_aibjbialbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbialbj
    function eom_cc3_23_tripletp_trans_aibjbjajbm(i, b, j, m) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjajbm   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, m, i)



    eom_cc3_23_tripletp_trans_aibjbjajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbjajbm = eom_cc3_23_tripletp_trans_aibjbjajbm + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjajbm
    function eom_cc3_23_tripletp_trans_aibjbkajbk(i, b, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkajbk   
    integer, intent(in) :: i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, k, i)



    eom_cc3_23_tripletp_trans_aibjbkajbk = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbkajbk = eom_cc3_23_tripletp_trans_aibjbkajbk + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkajbk
    function eom_cc3_23_tripletp_trans_aibjbkakbj(i, b, k) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbkakbj   
    integer, intent(in) :: i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbkakbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbkakbj = eom_cc3_23_tripletp_trans_aibjbkakbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbkakbj
    function eom_cc3_23_tripletp_trans_aibjbjalbj(i, b, j, l) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjalbj   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, l, i)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbjalbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbjalbj = eom_cc3_23_tripletp_trans_aibjbjalbj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjalbj
    function eom_cc3_23_tripletp_trans_aibjbidibj(a, i, b, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbidibj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, b)



    eom_cc3_23_tripletp_trans_aibjbidibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbidibj = eom_cc3_23_tripletp_trans_aibjbidibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbidibj
    function eom_cc3_23_tripletp_trans_aibjbjdibj(a, b, j, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjdibj   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, b)



    eom_cc3_23_tripletp_trans_aibjbjdibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbjdibj = eom_cc3_23_tripletp_trans_aibjbjdibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjdibj
    function eom_cc3_23_tripletp_trans_aibjbiaiej(i, b, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbiaiej   
    integer, intent(in) :: i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, e, i, b)



    eom_cc3_23_tripletp_trans_aibjbiaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbiaiej = eom_cc3_23_tripletp_trans_aibjbiaiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbiaiej
    function eom_cc3_23_tripletp_trans_aibjbjaiej(b, j, e) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjaiej   
    integer, intent(in) :: b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, e, j, b)



    eom_cc3_23_tripletp_trans_aibjbjaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbjaiej = eom_cc3_23_tripletp_trans_aibjbjaiej + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjaiej
    function eom_cc3_23_tripletp_trans_aibjbidiaj(i, b, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbidiaj   
    integer, intent(in) :: i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, i, b)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbidiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbidiaj = eom_cc3_23_tripletp_trans_aibjbidiaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbidiaj
    function eom_cc3_23_tripletp_trans_aibjbjdiaj(b, j, d) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjdiaj   
    integer, intent(in) :: b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, j, b)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjbjdiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjbjdiaj = eom_cc3_23_tripletp_trans_aibjbjdiaj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjdiaj
    function eom_cc3_23_tripletp_trans_aibjciaicj(i, b, c) 
    double precision :: eom_cc3_23_tripletp_trans_aibjciaicj   
    integer, intent(in) :: i, b, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, i, c)



    eom_cc3_23_tripletp_trans_aibjciaicj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjciaicj = eom_cc3_23_tripletp_trans_aibjciaicj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjciaicj
    function eom_cc3_23_tripletp_trans_aibjcjaicj(b, j, c) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjaicj   
    integer, intent(in) :: b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, c)



    eom_cc3_23_tripletp_trans_aibjcjaicj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcjaicj = eom_cc3_23_tripletp_trans_aibjcjaicj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjaicj
    function eom_cc3_23_tripletp_trans_aibjcibicj(a, i, c) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcibicj   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcibicj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcibicj = eom_cc3_23_tripletp_trans_aibjcibicj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcibicj
    function eom_cc3_23_tripletp_trans_aibjcjbicj(a, j, c) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjbicj   
    integer, intent(in) :: a, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, c)

term(0) = -term(0) 


    eom_cc3_23_tripletp_trans_aibjcjbicj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletp_trans_aibjcjbicj = eom_cc3_23_tripletp_trans_aibjcjbicj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjbicj
    function eom_cc3_23_tripletp_trans_aibjciaibj(nocc, a, i, b, j, c) 
    double precision :: eom_cc3_23_tripletp_trans_aibjciaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, c, j, j)
term(1) = term(1) + tovoo(i, c, i, i)
term(2) = term(2) + tov(i, c)
term(3) = term(3) + tvvov(a, a, i, c)
term(4) = term(4) + tvvov(b, b, i, c)

term(0) = -term(0) 
term(1) = -term(1) 

do n = 1, nocc 
term(5) = term(5) + tovoo(i, c, n, n)
term(6) = term(6) + tovoo(n, c, i, n)
end do 

term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 


    eom_cc3_23_tripletp_trans_aibjciaibj = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletp_trans_aibjciaibj = eom_cc3_23_tripletp_trans_aibjciaibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjciaibj
    function eom_cc3_23_tripletp_trans_aibjcjaibj(nocc, a, i, b, j, c) 
    double precision :: eom_cc3_23_tripletp_trans_aibjcjaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, c, i, i)
term(1) = term(1) + tovoo(j, c, j, j)
term(2) = term(2) + tov(j, c)
term(3) = term(3) + tvvov(a, a, j, c)
term(4) = term(4) + tvvov(b, b, j, c)

term(0) = -term(0) 
term(1) = -term(1) 

do n = 1, nocc 
term(5) = term(5) + tovoo(j, c, n, n)
term(6) = term(6) + tovoo(n, c, j, n)
end do 

term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 


    eom_cc3_23_tripletp_trans_aibjcjaibj = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletp_trans_aibjcjaibj = eom_cc3_23_tripletp_trans_aibjcjaibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjcjaibj
    function eom_cc3_23_tripletp_trans_aibjaiaibj(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_tripletp_trans_aibjaiaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, j, j)
term(1) = term(1) + tovoo(i, a, i, i)
term(2) = term(2) + tvvov(a, a, i, a)
term(3) = term(3) + tov(i, a)
term(4) = term(4) + tvvov(b, b, i, a)

term(0) = -term(0) 
term(1) = -term(1) 

do n = 1, nocc 
term(5) = term(5) + tovoo(i, a, n, n)
term(6) = term(6) + tovoo(n, a, i, n)
end do 

term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 


    eom_cc3_23_tripletp_trans_aibjaiaibj = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletp_trans_aibjaiaibj = eom_cc3_23_tripletp_trans_aibjaiaibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjaiaibj
    function eom_cc3_23_tripletp_trans_aibjajaibj(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_tripletp_trans_aibjajaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, a, i, i)
term(1) = term(1) + tovoo(j, a, j, j)
term(2) = term(2) + tvvov(a, a, j, a)
term(3) = term(3) + tov(j, a)
term(4) = term(4) + tvvov(b, b, j, a)

term(0) = -term(0) 
term(1) = -term(1) 

do n = 1, nocc 
term(5) = term(5) + tovoo(j, a, n, n)
term(6) = term(6) + tovoo(n, a, j, n)
end do 

term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 


    eom_cc3_23_tripletp_trans_aibjajaibj = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletp_trans_aibjajaibj = eom_cc3_23_tripletp_trans_aibjajaibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjajaibj
    function eom_cc3_23_tripletp_trans_aibjbiaibj(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbiaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, b, j, j)
term(1) = term(1) + tovoo(i, b, i, i)
term(2) = term(2) + tvvov(b, b, i, b)
term(3) = term(3) + tov(i, b)
term(4) = term(4) + tvvov(a, a, i, b)

term(0) = -term(0) 
term(1) = -term(1) 

do n = 1, nocc 
term(5) = term(5) + tovoo(i, b, n, n)
term(6) = term(6) + tovoo(n, b, i, n)
end do 

term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 


    eom_cc3_23_tripletp_trans_aibjbiaibj = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletp_trans_aibjbiaibj = eom_cc3_23_tripletp_trans_aibjbiaibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbiaibj
    function eom_cc3_23_tripletp_trans_aibjbjaibj(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_tripletp_trans_aibjbjaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, i, i)
term(1) = term(1) + tovoo(j, b, j, j)
term(2) = term(2) + tvvov(b, b, j, b)
term(3) = term(3) + tov(j, b)
term(4) = term(4) + tvvov(a, a, j, b)

term(0) = -term(0) 
term(1) = -term(1) 

do n = 1, nocc 
term(5) = term(5) + tovoo(j, b, n, n)
term(6) = term(6) + tovoo(n, b, j, n)
end do 

term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 


    eom_cc3_23_tripletp_trans_aibjbjaibj = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletp_trans_aibjbjaibj = eom_cc3_23_tripletp_trans_aibjbjaibj + term(s)
    end do

    end function eom_cc3_23_tripletp_trans_aibjbjaibj
    end module eom_cc3_23_tripletp_trans
    
