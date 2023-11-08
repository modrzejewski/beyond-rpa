module eom_cc3_23_tripletm_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_cc3_23_tripletm_trans_aibjaiblem(j, l, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaiblem   
    integer, intent(in) :: j, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, j)
term(1) = term(1) + tovoo(l, e, m, j)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjaiblem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaiblem = eom_cc3_23_tripletm_trans_aibjaiblem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaiblem
    function eom_cc3_23_tripletm_trans_aibjakbjem(i, k, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakbjem   
    integer, intent(in) :: i, k, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, i)



    eom_cc3_23_tripletm_trans_aibjakbjem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakbjem = eom_cc3_23_tripletm_trans_aibjakbjem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakbjem
    function eom_cc3_23_tripletm_trans_aibjakblej(i, k, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakblej   
    integer, intent(in) :: i, k, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjakblej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakblej = eom_cc3_23_tripletm_trans_aibjakblej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakblej
    function eom_cc3_23_tripletm_trans_aibjaidlbm(j, d, l, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidlbm   
    integer, intent(in) :: j, d, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, l, j)
term(1) = term(1) + tovoo(l, d, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjaidlbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaidlbm = eom_cc3_23_tripletm_trans_aibjaidlbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidlbm
    function eom_cc3_23_tripletm_trans_aibjakdjbm(i, k, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakdjbm   
    integer, intent(in) :: i, k, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjakdjbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakdjbm = eom_cc3_23_tripletm_trans_aibjakdjbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakdjbm
    function eom_cc3_23_tripletm_trans_aibjakdlbj(i, k, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakdlbj   
    integer, intent(in) :: i, k, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, k, i)



    eom_cc3_23_tripletm_trans_aibjakdlbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakdlbj = eom_cc3_23_tripletm_trans_aibjakdlbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakdlbj
    function eom_cc3_23_tripletm_trans_aibjaidjem(b, d, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidjem   
    integer, intent(in) :: b, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, m, e)
term(1) = term(1) + tvvov(b, e, m, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjaidjem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaidjem = eom_cc3_23_tripletm_trans_aibjaidjem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidjem
    function eom_cc3_23_tripletm_trans_aibjaidlej(b, d, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidlej   
    integer, intent(in) :: b, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, l, e)
term(1) = term(1) + tvvov(b, e, l, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjaidlej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaidlej = eom_cc3_23_tripletm_trans_aibjaidlej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidlej
    function eom_cc3_23_tripletm_trans_aibjbkaiem(j, k, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkaiem   
    integer, intent(in) :: j, k, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbkaiem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkaiem = eom_cc3_23_tripletm_trans_aibjbkaiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkaiem
    function eom_cc3_23_tripletm_trans_aibjbkalei(j, k, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkalei   
    integer, intent(in) :: j, k, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, j)



    eom_cc3_23_tripletm_trans_aibjbkalei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkalei = eom_cc3_23_tripletm_trans_aibjbkalei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkalei
    function eom_cc3_23_tripletm_trans_aibjbjalem(i, l, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjalem   
    integer, intent(in) :: i, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, i)
term(1) = term(1) + tovoo(l, e, m, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbjalem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjalem = eom_cc3_23_tripletm_trans_aibjbjalem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjalem
    function eom_cc3_23_tripletm_trans_aibjbkdiam(j, k, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkdiam   
    integer, intent(in) :: j, k, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, k, j)



    eom_cc3_23_tripletm_trans_aibjbkdiam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkdiam = eom_cc3_23_tripletm_trans_aibjbkdiam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkdiam
    function eom_cc3_23_tripletm_trans_aibjbkdlai(j, k, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkdlai   
    integer, intent(in) :: j, k, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbkdlai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkdlai = eom_cc3_23_tripletm_trans_aibjbkdlai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkdlai
    function eom_cc3_23_tripletm_trans_aibjbjdlam(i, d, l, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdlam   
    integer, intent(in) :: i, d, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, l, i)
term(1) = term(1) + tovoo(l, d, m, i)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjbjdlam = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjdlam = eom_cc3_23_tripletm_trans_aibjbjdlam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdlam
    function eom_cc3_23_tripletm_trans_aibjbjdiem(a, d, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdiem   
    integer, intent(in) :: a, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, e)
term(1) = term(1) + tvvov(a, e, m, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjbjdiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjdiem = eom_cc3_23_tripletm_trans_aibjbjdiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdiem
    function eom_cc3_23_tripletm_trans_aibjbjdlei(a, d, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdlei   
    integer, intent(in) :: a, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, e)
term(1) = term(1) + tvvov(a, e, l, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbjdlei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjdlei = eom_cc3_23_tripletm_trans_aibjbjdlei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdlei
    function eom_cc3_23_tripletm_trans_aibjcjaiem(b, c, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjaiem   
    integer, intent(in) :: b, c, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, e)



    eom_cc3_23_tripletm_trans_aibjcjaiem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjaiem = eom_cc3_23_tripletm_trans_aibjcjaiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjaiem
    function eom_cc3_23_tripletm_trans_aibjcjalei(b, c, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjalei   
    integer, intent(in) :: b, c, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, e)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcjalei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjalei = eom_cc3_23_tripletm_trans_aibjcjalei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjalei
    function eom_cc3_23_tripletm_trans_aibjcibjem(a, c, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcibjem   
    integer, intent(in) :: a, c, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, e)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcibjem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcibjem = eom_cc3_23_tripletm_trans_aibjcibjem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcibjem
    function eom_cc3_23_tripletm_trans_aibjciblej(a, c, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjciblej   
    integer, intent(in) :: a, c, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, e)



    eom_cc3_23_tripletm_trans_aibjciblej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjciblej = eom_cc3_23_tripletm_trans_aibjciblej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjciblej
    function eom_cc3_23_tripletm_trans_aibjcjdiam(b, c, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjdiam   
    integer, intent(in) :: b, c, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcjdiam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjdiam = eom_cc3_23_tripletm_trans_aibjcjdiam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjdiam
    function eom_cc3_23_tripletm_trans_aibjcjdlai(b, c, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjdlai   
    integer, intent(in) :: b, c, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, d)



    eom_cc3_23_tripletm_trans_aibjcjdlai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjdlai = eom_cc3_23_tripletm_trans_aibjcjdlai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjdlai
    function eom_cc3_23_tripletm_trans_aibjcidjbm(a, c, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcidjbm   
    integer, intent(in) :: a, c, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, d)



    eom_cc3_23_tripletm_trans_aibjcidjbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcidjbm = eom_cc3_23_tripletm_trans_aibjcidjbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcidjbm
    function eom_cc3_23_tripletm_trans_aibjcidlbj(a, c, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcidlbj   
    integer, intent(in) :: a, c, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcidlbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcidlbj = eom_cc3_23_tripletm_trans_aibjcidlbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcidlbj
    function eom_cc3_23_tripletm_trans_aiajakaiem(j, k, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakaiem   
    integer, intent(in) :: j, k, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajakaiem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakaiem = eom_cc3_23_tripletm_trans_aiajakaiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakaiem
    function eom_cc3_23_tripletm_trans_aiajakalei(j, k, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakalei   
    integer, intent(in) :: j, k, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, j)



    eom_cc3_23_tripletm_trans_aiajakalei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakalei = eom_cc3_23_tripletm_trans_aiajakalei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakalei
    function eom_cc3_23_tripletm_trans_aiajaialem(j, l, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaialem   
    integer, intent(in) :: j, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, j)
term(1) = term(1) + tovoo(l, e, m, j)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aiajaialem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aiajaialem = eom_cc3_23_tripletm_trans_aiajaialem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaialem
    function eom_cc3_23_tripletm_trans_aiajakajem(i, k, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakajem   
    integer, intent(in) :: i, k, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, i)



    eom_cc3_23_tripletm_trans_aiajakajem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakajem = eom_cc3_23_tripletm_trans_aiajakajem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakajem
    function eom_cc3_23_tripletm_trans_aiajakalej(i, k, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakalej   
    integer, intent(in) :: i, k, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajakalej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakalej = eom_cc3_23_tripletm_trans_aiajakalej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakalej
    function eom_cc3_23_tripletm_trans_aiajajalem(i, l, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajalem   
    integer, intent(in) :: i, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, i)
term(1) = term(1) + tovoo(l, e, m, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajajalem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aiajajalem = eom_cc3_23_tripletm_trans_aiajajalem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajalem
    function eom_cc3_23_tripletm_trans_aibjaialbm(a, j, l, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaialbm   
    integer, intent(in) :: a, j, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, a, l, j)
term(1) = term(1) + tovoo(l, a, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjaialbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaialbm = eom_cc3_23_tripletm_trans_aibjaialbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaialbm
    function eom_cc3_23_tripletm_trans_aibjakajbm(a, i, k, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakajbm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, a, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjakajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakajbm = eom_cc3_23_tripletm_trans_aibjakajbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakajbm
    function eom_cc3_23_tripletm_trans_aibjakalbj(a, i, k, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakalbj   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, a, k, i)



    eom_cc3_23_tripletm_trans_aibjakalbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakalbj = eom_cc3_23_tripletm_trans_aibjakalbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakalbj
    function eom_cc3_23_tripletm_trans_aibjajaiem(a, b, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajaiem   
    integer, intent(in) :: a, b, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, m, e)



    eom_cc3_23_tripletm_trans_aibjajaiem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajaiem = eom_cc3_23_tripletm_trans_aibjajaiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajaiem
    function eom_cc3_23_tripletm_trans_aibjajalei(a, b, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajalei   
    integer, intent(in) :: a, b, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, l, e)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjajalei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajalei = eom_cc3_23_tripletm_trans_aibjajalei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajalei
    function eom_cc3_23_tripletm_trans_aibjaiajem(a, b, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaiajem   
    integer, intent(in) :: a, b, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, m, e)
term(1) = term(1) + tvvov(b, e, m, a)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjaiajem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaiajem = eom_cc3_23_tripletm_trans_aibjaiajem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaiajem
    function eom_cc3_23_tripletm_trans_aibjaialej(a, b, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaialej   
    integer, intent(in) :: a, b, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, l, e)
term(1) = term(1) + tvvov(b, e, l, a)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjaialej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaialej = eom_cc3_23_tripletm_trans_aibjaialej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaialej
    function eom_cc3_23_tripletm_trans_aibjbjbiem(a, b, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjbiem   
    integer, intent(in) :: a, b, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, m, e)
term(1) = term(1) + tvvov(a, e, m, b)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjbjbiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjbiem = eom_cc3_23_tripletm_trans_aibjbjbiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjbiem
    function eom_cc3_23_tripletm_trans_aibjbjblei(a, b, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjblei   
    integer, intent(in) :: a, b, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, l, e)
term(1) = term(1) + tvvov(a, e, l, b)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbjblei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjblei = eom_cc3_23_tripletm_trans_aibjbjblei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjblei
    function eom_cc3_23_tripletm_trans_aibjbibjem(a, b, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbibjem   
    integer, intent(in) :: a, b, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, m, e)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbibjem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbibjem = eom_cc3_23_tripletm_trans_aibjbibjem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbibjem
    function eom_cc3_23_tripletm_trans_aibjbiblej(a, b, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbiblej   
    integer, intent(in) :: a, b, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, l, e)



    eom_cc3_23_tripletm_trans_aibjbiblej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbiblej = eom_cc3_23_tripletm_trans_aibjbiblej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbiblej
    function eom_cc3_23_tripletm_trans_aibjcjciam(b, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjciam   
    integer, intent(in) :: b, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcjciam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjciam = eom_cc3_23_tripletm_trans_aibjcjciam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjciam
    function eom_cc3_23_tripletm_trans_aibjcjclai(b, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjclai   
    integer, intent(in) :: b, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, c)



    eom_cc3_23_tripletm_trans_aibjcjclai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjclai = eom_cc3_23_tripletm_trans_aibjcjclai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjclai
    function eom_cc3_23_tripletm_trans_aibjcicjbm(a, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcicjbm   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, c)



    eom_cc3_23_tripletm_trans_aibjcicjbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcicjbm = eom_cc3_23_tripletm_trans_aibjcicjbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcicjbm
    function eom_cc3_23_tripletm_trans_aibjciclbj(a, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjciclbj   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjciclbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjciclbj = eom_cc3_23_tripletm_trans_aibjciclbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjciclbj
    function eom_cc3_23_tripletm_trans_aiajakdiam(j, k, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakdiam   
    integer, intent(in) :: j, k, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, k, j)



    eom_cc3_23_tripletm_trans_aiajakdiam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakdiam = eom_cc3_23_tripletm_trans_aiajakdiam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakdiam
    function eom_cc3_23_tripletm_trans_aiajakdlai(j, k, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakdlai   
    integer, intent(in) :: j, k, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajakdlai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakdlai = eom_cc3_23_tripletm_trans_aiajakdlai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakdlai
    function eom_cc3_23_tripletm_trans_aiajaidlam(j, d, l, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaidlam   
    integer, intent(in) :: j, d, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, l, j)
term(1) = term(1) + tovoo(l, d, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajaidlam = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aiajaidlam = eom_cc3_23_tripletm_trans_aiajaidlam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaidlam
    function eom_cc3_23_tripletm_trans_aiajakdjam(i, k, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakdjam   
    integer, intent(in) :: i, k, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajakdjam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakdjam = eom_cc3_23_tripletm_trans_aiajakdjam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakdjam
    function eom_cc3_23_tripletm_trans_aiajakdlaj(i, k, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakdlaj   
    integer, intent(in) :: i, k, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, k, i)



    eom_cc3_23_tripletm_trans_aiajakdlaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakdlaj = eom_cc3_23_tripletm_trans_aiajakdlaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakdlaj
    function eom_cc3_23_tripletm_trans_aiajajdlam(i, d, l, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajdlam   
    integer, intent(in) :: i, d, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, l, i)
term(1) = term(1) + tovoo(l, d, m, i)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aiajajdlam = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aiajajdlam = eom_cc3_23_tripletm_trans_aiajajdlam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajdlam
    function eom_cc3_23_tripletm_trans_aiajajdiem(a, d, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajdiem   
    integer, intent(in) :: a, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, e)
term(1) = term(1) + tvvov(a, e, m, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aiajajdiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aiajajdiem = eom_cc3_23_tripletm_trans_aiajajdiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajdiem
    function eom_cc3_23_tripletm_trans_aiajajdlei(a, d, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajdlei   
    integer, intent(in) :: a, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, e)
term(1) = term(1) + tvvov(a, e, l, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajajdlei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aiajajdlei = eom_cc3_23_tripletm_trans_aiajajdlei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajdlei
    function eom_cc3_23_tripletm_trans_aiajaidjem(a, d, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaidjem   
    integer, intent(in) :: a, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, e)
term(1) = term(1) + tvvov(a, e, m, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajaidjem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aiajaidjem = eom_cc3_23_tripletm_trans_aiajaidjem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaidjem
    function eom_cc3_23_tripletm_trans_aiajaidlej(a, d, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaidlej   
    integer, intent(in) :: a, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, e)
term(1) = term(1) + tvvov(a, e, l, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aiajaidlej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aiajaidlej = eom_cc3_23_tripletm_trans_aiajaidlej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaidlej
    function eom_cc3_23_tripletm_trans_aibjajdiam(a, b, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajdiam   
    integer, intent(in) :: a, b, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, m, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjajdiam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajdiam = eom_cc3_23_tripletm_trans_aibjajdiam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajdiam
    function eom_cc3_23_tripletm_trans_aibjajdlai(a, b, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajdlai   
    integer, intent(in) :: a, b, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, l, d)



    eom_cc3_23_tripletm_trans_aibjajdlai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajdlai = eom_cc3_23_tripletm_trans_aibjajdlai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajdlai
    function eom_cc3_23_tripletm_trans_aibjaidjam(a, b, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidjam   
    integer, intent(in) :: a, b, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, m, a)
term(1) = term(1) + tvvov(b, a, m, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjaidjam = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaidjam = eom_cc3_23_tripletm_trans_aibjaidjam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidjam
    function eom_cc3_23_tripletm_trans_aibjaidlaj(a, b, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidlaj   
    integer, intent(in) :: a, b, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, l, a)
term(1) = term(1) + tvvov(b, a, l, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjaidlaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaidlaj = eom_cc3_23_tripletm_trans_aibjaidlaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidlaj
    function eom_cc3_23_tripletm_trans_aibiakbiem(i, k, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiakbiem   
    integer, intent(in) :: i, k, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, i)



    eom_cc3_23_tripletm_trans_aibiakbiem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiakbiem = eom_cc3_23_tripletm_trans_aibiakbiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiakbiem
    function eom_cc3_23_tripletm_trans_aibjaibiem(i, j, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaibiem   
    integer, intent(in) :: i, j, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, i, j)
term(1) = term(1) + tovoo(i, e, m, j)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjaibiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaibiem = eom_cc3_23_tripletm_trans_aibjaibiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaibiem
    function eom_cc3_23_tripletm_trans_aibjakbiej(i, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakbiej   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjakbiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakbiej = eom_cc3_23_tripletm_trans_aibjakbiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakbiej
    function eom_cc3_23_tripletm_trans_aibiakblei(i, k, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibiakblei   
    integer, intent(in) :: i, k, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibiakblei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiakblei = eom_cc3_23_tripletm_trans_aibiakblei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiakblei
    function eom_cc3_23_tripletm_trans_aibiaiblem(i, l, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaiblem   
    integer, intent(in) :: i, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, i)
term(1) = term(1) + tovoo(l, e, m, i)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibiaiblem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibiaiblem = eom_cc3_23_tripletm_trans_aibiaiblem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaiblem
    function eom_cc3_23_tripletm_trans_aibjaiblei(i, j, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaiblei   
    integer, intent(in) :: i, j, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, l, j)
term(1) = term(1) + tovoo(l, e, i, j)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjaiblei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaiblei = eom_cc3_23_tripletm_trans_aibjaiblei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaiblei
    function eom_cc3_23_tripletm_trans_aibjakbjei(i, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakbjei   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, k, i)



    eom_cc3_23_tripletm_trans_aibjakbjei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakbjei = eom_cc3_23_tripletm_trans_aibjakbjei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakbjei
    function eom_cc3_23_tripletm_trans_aibjaibjem(nocc, a, i, b, j, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaibjem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, i, i)
term(1) = term(1) + tovoo(m, e, j, j)
term(2) = term(2) + tovoo(j, e, m, j)
term(3) = term(3) + tov(m, e)
term(4) = term(4) + tvvov(a, a, m, e)
term(5) = term(5) + tvvov(b, b, m, e)
term(6) = term(6) + tvvov(b, e, m, b)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 

do n = 1, nocc 
term(7) = term(7) + tovoo(m, e, n, n)
term(8) = term(8) + tovoo(n, e, m, n)
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibjaibjem = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjaibjem = eom_cc3_23_tripletm_trans_aibjaibjem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaibjem
    function eom_cc3_23_tripletm_trans_aibjaiblej(nocc, a, i, b, j, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaiblej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, l, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, i, i)
term(1) = term(1) + tovoo(j, e, l, j)
term(2) = term(2) + tovoo(l, e, j, j)
term(3) = term(3) + tov(l, e)
term(4) = term(4) + tvvov(a, a, l, e)
term(5) = term(5) + tvvov(b, b, l, e)
term(6) = term(6) + tvvov(b, e, l, b)

term(0) = -term(0) 
term(2) = -term(2) 
term(6) = -term(6) 

do n = 1, nocc 
term(7) = term(7) + tovoo(n, e, l, n)
term(8) = term(8) + tovoo(l, e, n, n)
end do 

term(7) = -term(7) 
term(8) = term(8) * 2.0d+0 


    eom_cc3_23_tripletm_trans_aibjaiblej = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjaiblej = eom_cc3_23_tripletm_trans_aibjaiblej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaiblej
    function eom_cc3_23_tripletm_trans_aibjajbjem(i, j, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajbjem   
    integer, intent(in) :: i, j, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, j, i)



    eom_cc3_23_tripletm_trans_aibjajbjem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajbjem = eom_cc3_23_tripletm_trans_aibjajbjem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajbjem
    function eom_cc3_23_tripletm_trans_aibjakbjek(i, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakbjek   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)



    eom_cc3_23_tripletm_trans_aibjakbjek = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakbjek = eom_cc3_23_tripletm_trans_aibjakbjek + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakbjek
    function eom_cc3_23_tripletm_trans_aibjakbkej(i, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakbkej   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjakbkej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakbkej = eom_cc3_23_tripletm_trans_aibjakbkej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakbkej
    function eom_cc3_23_tripletm_trans_aibjajblej(i, j, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajblej   
    integer, intent(in) :: i, j, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, j, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjajblej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajblej = eom_cc3_23_tripletm_trans_aibjajblej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajblej
    function eom_cc3_23_tripletm_trans_aibiakdibm(i, k, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiakdibm   
    integer, intent(in) :: i, k, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibiakdibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiakdibm = eom_cc3_23_tripletm_trans_aibiakdibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiakdibm
    function eom_cc3_23_tripletm_trans_aibjaidibm(i, j, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidibm   
    integer, intent(in) :: i, j, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, i, j)
term(1) = term(1) + tovoo(i, d, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjaidibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaidibm = eom_cc3_23_tripletm_trans_aibjaidibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidibm
    function eom_cc3_23_tripletm_trans_aibjakdibj(i, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakdibj   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, k, i)



    eom_cc3_23_tripletm_trans_aibjakdibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakdibj = eom_cc3_23_tripletm_trans_aibjakdibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakdibj
    function eom_cc3_23_tripletm_trans_aibiakdlbi(i, k, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibiakdlbi   
    integer, intent(in) :: i, k, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, k, i)



    eom_cc3_23_tripletm_trans_aibiakdlbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiakdlbi = eom_cc3_23_tripletm_trans_aibiakdlbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiakdlbi
    function eom_cc3_23_tripletm_trans_aibiaidlbm(i, d, l, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaidlbm   
    integer, intent(in) :: i, d, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, l, i)
term(1) = term(1) + tovoo(l, d, m, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibiaidlbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibiaidlbm = eom_cc3_23_tripletm_trans_aibiaidlbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaidlbm
    function eom_cc3_23_tripletm_trans_aibjaidlbi(i, j, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidlbi   
    integer, intent(in) :: i, j, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, l, j)
term(1) = term(1) + tovoo(l, d, i, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjaidlbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaidlbi = eom_cc3_23_tripletm_trans_aibjaidlbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidlbi
    function eom_cc3_23_tripletm_trans_aibjakdjbi(i, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakdjbi   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjakdjbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakdjbi = eom_cc3_23_tripletm_trans_aibjakdjbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakdjbi
    function eom_cc3_23_tripletm_trans_aibjaidjbm(nocc, a, i, b, j, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidjbm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, i, i)
term(1) = term(1) + tovoo(m, d, j, j)
term(2) = term(2) + tovoo(j, d, m, j)
term(3) = term(3) + tov(m, d)
term(4) = term(4) + tvvov(a, a, m, d)
term(5) = term(5) + tvvov(b, d, m, b)
term(6) = term(6) + tvvov(b, b, m, d)

term(0) = -term(0) 
term(1) = -term(1) 
term(5) = -term(5) 

do n = 1, nocc 
term(7) = term(7) + tovoo(m, d, n, n)
term(8) = term(8) + tovoo(n, d, m, n)
end do 

term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 


    eom_cc3_23_tripletm_trans_aibjaidjbm = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjaidjbm = eom_cc3_23_tripletm_trans_aibjaidjbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidjbm
    function eom_cc3_23_tripletm_trans_aibjaidlbj(nocc, a, i, b, j, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidlbj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, l, j)
term(1) = term(1) + tovoo(l, d, i, i)
term(2) = term(2) + tovoo(l, d, j, j)
term(3) = term(3) + tov(l, d)
term(4) = term(4) + tvvov(b, d, l, b)
term(5) = term(5) + tvvov(a, a, l, d)
term(6) = term(6) + tvvov(b, b, l, d)

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 

do n = 1, nocc 
term(7) = term(7) + tovoo(l, d, n, n)
term(8) = term(8) + tovoo(n, d, l, n)
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibjaidlbj = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjaidlbj = eom_cc3_23_tripletm_trans_aibjaidlbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidlbj
    function eom_cc3_23_tripletm_trans_aibjajdjbm(i, j, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajdjbm   
    integer, intent(in) :: i, j, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, j, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjajdjbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajdjbm = eom_cc3_23_tripletm_trans_aibjajdjbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajdjbm
    function eom_cc3_23_tripletm_trans_aibjakdjbk(i, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakdjbk   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjakdjbk = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakdjbk = eom_cc3_23_tripletm_trans_aibjakdjbk + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakdjbk
    function eom_cc3_23_tripletm_trans_aibjakdkbj(i, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakdkbj   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, i)



    eom_cc3_23_tripletm_trans_aibjakdkbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakdkbj = eom_cc3_23_tripletm_trans_aibjakdkbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakdkbj
    function eom_cc3_23_tripletm_trans_aibjajdlbj(i, j, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajdlbj   
    integer, intent(in) :: i, j, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, j, i)



    eom_cc3_23_tripletm_trans_aibjajdlbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajdlbj = eom_cc3_23_tripletm_trans_aibjajdlbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajdlbj
    function eom_cc3_23_tripletm_trans_aibiaidiem(b, d, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaidiem   
    integer, intent(in) :: b, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, m, e)
term(1) = term(1) + tvvov(b, e, m, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibiaidiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibiaidiem = eom_cc3_23_tripletm_trans_aibiaidiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaidiem
    function eom_cc3_23_tripletm_trans_aibjaidiej(i, b, d, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidiej   
    integer, intent(in) :: i, b, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, i, e)
term(1) = term(1) + tvvov(b, e, i, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjaidiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaidiej = eom_cc3_23_tripletm_trans_aibjaidiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidiej
    function eom_cc3_23_tripletm_trans_aibiaidlei(b, d, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaidlei   
    integer, intent(in) :: b, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, l, e)
term(1) = term(1) + tvvov(b, e, l, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibiaidlei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibiaidlei = eom_cc3_23_tripletm_trans_aibiaidlei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaidlei
    function eom_cc3_23_tripletm_trans_aibjaidjei(i, b, d, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidjei   
    integer, intent(in) :: i, b, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, i, e)
term(1) = term(1) + tvvov(b, e, i, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjaidjei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaidjei = eom_cc3_23_tripletm_trans_aibjaidjei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidjei
    function eom_cc3_23_tripletm_trans_aibjbkaibm(b, j, k, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkaibm   
    integer, intent(in) :: b, j, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, b, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbkaibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkaibm = eom_cc3_23_tripletm_trans_aibjbkaibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkaibm
    function eom_cc3_23_tripletm_trans_aibjbkalbi(b, j, k, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkalbi   
    integer, intent(in) :: b, j, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, b, k, j)



    eom_cc3_23_tripletm_trans_aibjbkalbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkalbi = eom_cc3_23_tripletm_trans_aibjbkalbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkalbi
    function eom_cc3_23_tripletm_trans_aibjbjalbm(i, b, l, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjalbm   
    integer, intent(in) :: i, b, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, b, l, i)
term(1) = term(1) + tovoo(l, b, m, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbjalbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjalbm = eom_cc3_23_tripletm_trans_aibjbjalbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjalbm
    function eom_cc3_23_tripletm_trans_aibjbjdibm(a, b, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdibm   
    integer, intent(in) :: a, b, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, b)
term(1) = term(1) + tvvov(a, b, m, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjbjdibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjdibm = eom_cc3_23_tripletm_trans_aibjbjdibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdibm
    function eom_cc3_23_tripletm_trans_aibjbjdlbi(a, b, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdlbi   
    integer, intent(in) :: a, b, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, b)
term(1) = term(1) + tvvov(a, b, l, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbjdlbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjdlbi = eom_cc3_23_tripletm_trans_aibjbjdlbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdlbi
    function eom_cc3_23_tripletm_trans_aibjbidjbm(a, b, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbidjbm   
    integer, intent(in) :: a, b, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, m, d)



    eom_cc3_23_tripletm_trans_aibjbidjbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbidjbm = eom_cc3_23_tripletm_trans_aibjbidjbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbidjbm
    function eom_cc3_23_tripletm_trans_aibjbidlbj(a, b, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbidlbj   
    integer, intent(in) :: a, b, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, l, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbidlbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbidlbj = eom_cc3_23_tripletm_trans_aibjbidlbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbidlbj
    function eom_cc3_23_tripletm_trans_aibibkaiem(i, k, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibibkaiem   
    integer, intent(in) :: i, k, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibibkaiem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibkaiem = eom_cc3_23_tripletm_trans_aibibkaiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibkaiem
    function eom_cc3_23_tripletm_trans_aibjbiaiem(i, j, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbiaiem   
    integer, intent(in) :: i, j, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, i, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbiaiem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbiaiem = eom_cc3_23_tripletm_trans_aibjbiaiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbiaiem
    function eom_cc3_23_tripletm_trans_aibjbkaiej(j, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkaiej   
    integer, intent(in) :: j, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbkaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkaiej = eom_cc3_23_tripletm_trans_aibjbkaiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkaiej
    function eom_cc3_23_tripletm_trans_aibjbjaiem(nocc, a, i, b, j, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjaiem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, j, j)
term(1) = term(1) + tovoo(m, e, i, i)
term(2) = term(2) + tovoo(i, e, m, i)
term(3) = term(3) + tov(m, e)
term(4) = term(4) + tvvov(b, b, m, e)
term(5) = term(5) + tvvov(a, a, m, e)
term(6) = term(6) + tvvov(a, e, m, a)

term(0) = -term(0) 
term(1) = -term(1) 
term(6) = -term(6) 

do n = 1, nocc 
term(7) = term(7) + tovoo(m, e, n, n)
term(8) = term(8) + tovoo(n, e, m, n)
end do 

term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 


    eom_cc3_23_tripletm_trans_aibjbjaiem = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjbjaiem = eom_cc3_23_tripletm_trans_aibjbjaiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjaiem
    function eom_cc3_23_tripletm_trans_aibjbkaiek(j, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkaiek   
    integer, intent(in) :: j, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbkaiek = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkaiek = eom_cc3_23_tripletm_trans_aibjbkaiek + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkaiek
    function eom_cc3_23_tripletm_trans_aibibkalei(i, k, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibibkalei   
    integer, intent(in) :: i, k, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, k, i)



    eom_cc3_23_tripletm_trans_aibibkalei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibkalei = eom_cc3_23_tripletm_trans_aibibkalei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibkalei
    function eom_cc3_23_tripletm_trans_aibibialem(i, l, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibibialem   
    integer, intent(in) :: i, l, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, l, i)
term(1) = term(1) + tovoo(l, e, m, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibibialem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibibialem = eom_cc3_23_tripletm_trans_aibibialem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibialem
    function eom_cc3_23_tripletm_trans_aibjbialei(i, j, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbialei   
    integer, intent(in) :: i, j, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, i, j)



    eom_cc3_23_tripletm_trans_aibjbialei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbialei = eom_cc3_23_tripletm_trans_aibjbialei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbialei
    function eom_cc3_23_tripletm_trans_aibjbkajei(j, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkajei   
    integer, intent(in) :: j, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, k, j)



    eom_cc3_23_tripletm_trans_aibjbkajei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkajei = eom_cc3_23_tripletm_trans_aibjbkajei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkajei
    function eom_cc3_23_tripletm_trans_aibjbkakei(j, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkakei   
    integer, intent(in) :: j, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, j)



    eom_cc3_23_tripletm_trans_aibjbkakei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkakei = eom_cc3_23_tripletm_trans_aibjbkakei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkakei
    function eom_cc3_23_tripletm_trans_aibjbjalei(nocc, a, i, b, j, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjalei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, l, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, j, j)
term(1) = term(1) + tovoo(i, e, l, i)
term(2) = term(2) + tovoo(l, e, i, i)
term(3) = term(3) + tov(l, e)
term(4) = term(4) + tvvov(b, b, l, e)
term(5) = term(5) + tvvov(a, a, l, e)
term(6) = term(6) + tvvov(a, e, l, a)

term(1) = -term(1) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 

do n = 1, nocc 
term(7) = term(7) + tovoo(n, e, l, n)
term(8) = term(8) + tovoo(l, e, n, n)
end do 

term(8) = term(8) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibjbjalei = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjbjalei = eom_cc3_23_tripletm_trans_aibjbjalei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjalei
    function eom_cc3_23_tripletm_trans_aibjbjajem(i, j, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjajem   
    integer, intent(in) :: i, j, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, j, i)
term(1) = term(1) + tovoo(j, e, m, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbjajem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjajem = eom_cc3_23_tripletm_trans_aibjbjajem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjajem
    function eom_cc3_23_tripletm_trans_aibjbjalej(i, j, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjalej   
    integer, intent(in) :: i, j, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, l, i)
term(1) = term(1) + tovoo(l, e, j, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbjalej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjalej = eom_cc3_23_tripletm_trans_aibjbjalej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjalej
    function eom_cc3_23_tripletm_trans_aibibkdiam(i, k, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibibkdiam   
    integer, intent(in) :: i, k, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, k, i)



    eom_cc3_23_tripletm_trans_aibibkdiam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibkdiam = eom_cc3_23_tripletm_trans_aibibkdiam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibkdiam
    function eom_cc3_23_tripletm_trans_aibjbidiam(i, j, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbidiam   
    integer, intent(in) :: i, j, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, i, j)



    eom_cc3_23_tripletm_trans_aibjbidiam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbidiam = eom_cc3_23_tripletm_trans_aibjbidiam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbidiam
    function eom_cc3_23_tripletm_trans_aibjbkdiaj(j, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkdiaj   
    integer, intent(in) :: j, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, k, j)



    eom_cc3_23_tripletm_trans_aibjbkdiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkdiaj = eom_cc3_23_tripletm_trans_aibjbkdiaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkdiaj
    function eom_cc3_23_tripletm_trans_aibjbjdiam(nocc, a, i, b, j, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdiam 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, j, j)
term(1) = term(1) + tovoo(m, d, i, i)
term(2) = term(2) + tovoo(i, d, m, i)
term(3) = term(3) + tov(m, d)
term(4) = term(4) + tvvov(b, b, m, d)
term(5) = term(5) + tvvov(a, d, m, a)
term(6) = term(6) + tvvov(a, a, m, d)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 

do n = 1, nocc 
term(7) = term(7) + tovoo(m, d, n, n)
term(8) = term(8) + tovoo(n, d, m, n)
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibjbjdiam = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjbjdiam = eom_cc3_23_tripletm_trans_aibjbjdiam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdiam
    function eom_cc3_23_tripletm_trans_aibjbkdiak(j, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkdiak   
    integer, intent(in) :: j, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, j)



    eom_cc3_23_tripletm_trans_aibjbkdiak = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkdiak = eom_cc3_23_tripletm_trans_aibjbkdiak + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkdiak
    function eom_cc3_23_tripletm_trans_aibibkdlai(i, k, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibibkdlai   
    integer, intent(in) :: i, k, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibibkdlai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibkdlai = eom_cc3_23_tripletm_trans_aibibkdlai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibkdlai
    function eom_cc3_23_tripletm_trans_aibibidlam(i, d, l, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibibidlam   
    integer, intent(in) :: i, d, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, l, i)
term(1) = term(1) + tovoo(l, d, m, i)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibibidlam = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibibidlam = eom_cc3_23_tripletm_trans_aibibidlam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibidlam
    function eom_cc3_23_tripletm_trans_aibjbidlai(i, j, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbidlai   
    integer, intent(in) :: i, j, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, d, i, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbidlai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbidlai = eom_cc3_23_tripletm_trans_aibjbidlai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbidlai
    function eom_cc3_23_tripletm_trans_aibjbkdjai(j, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkdjai   
    integer, intent(in) :: j, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbkdjai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkdjai = eom_cc3_23_tripletm_trans_aibjbkdjai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkdjai
    function eom_cc3_23_tripletm_trans_aibjbkdkai(j, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkdkai   
    integer, intent(in) :: j, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbkdkai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkdkai = eom_cc3_23_tripletm_trans_aibjbkdkai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkdkai
    function eom_cc3_23_tripletm_trans_aibjbjdlai(nocc, a, i, b, j, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdlai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, l, i)
term(1) = term(1) + tovoo(l, d, j, j)
term(2) = term(2) + tovoo(l, d, i, i)
term(3) = term(3) + tov(l, d)
term(4) = term(4) + tvvov(a, d, l, a)
term(5) = term(5) + tvvov(b, b, l, d)
term(6) = term(6) + tvvov(a, a, l, d)

term(1) = -term(1) 
term(2) = -term(2) 
term(4) = -term(4) 

do n = 1, nocc 
term(7) = term(7) + tovoo(l, d, n, n)
term(8) = term(8) + tovoo(n, d, l, n)
end do 

term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 


    eom_cc3_23_tripletm_trans_aibjbjdlai = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjbjdlai = eom_cc3_23_tripletm_trans_aibjbjdlai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdlai
    function eom_cc3_23_tripletm_trans_aibjbjdjam(i, j, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdjam   
    integer, intent(in) :: i, j, d, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, j, i)
term(1) = term(1) + tovoo(j, d, m, i)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjbjdjam = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjdjam = eom_cc3_23_tripletm_trans_aibjbjdjam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdjam
    function eom_cc3_23_tripletm_trans_aibjbjdlaj(i, j, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdlaj   
    integer, intent(in) :: i, j, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, l, i)
term(1) = term(1) + tovoo(l, d, j, i)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjbjdlaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjdlaj = eom_cc3_23_tripletm_trans_aibjbjdlaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdlaj
    function eom_cc3_23_tripletm_trans_aibibidiem(a, d, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibibidiem   
    integer, intent(in) :: a, d, e, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, e)
term(1) = term(1) + tvvov(a, e, m, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibibidiem = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibibidiem = eom_cc3_23_tripletm_trans_aibibidiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibidiem
    function eom_cc3_23_tripletm_trans_aibjbjdiej(a, j, d, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdiej   
    integer, intent(in) :: a, j, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, e)
term(1) = term(1) + tvvov(a, e, j, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjbjdiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjdiej = eom_cc3_23_tripletm_trans_aibjbjdiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdiej
    function eom_cc3_23_tripletm_trans_aibibidlei(a, d, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibibidlei   
    integer, intent(in) :: a, d, l, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, e)
term(1) = term(1) + tvvov(a, e, l, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibibidlei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibibidlei = eom_cc3_23_tripletm_trans_aibibidlei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibidlei
    function eom_cc3_23_tripletm_trans_aibjbjdjei(a, j, d, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdjei   
    integer, intent(in) :: a, j, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, e)
term(1) = term(1) + tvvov(a, e, j, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbjdjei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjdjei = eom_cc3_23_tripletm_trans_aibjbjdjei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdjei
    function eom_cc3_23_tripletm_trans_aibjcjaicm(b, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjaicm   
    integer, intent(in) :: b, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, c)



    eom_cc3_23_tripletm_trans_aibjcjaicm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjaicm = eom_cc3_23_tripletm_trans_aibjcjaicm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjaicm
    function eom_cc3_23_tripletm_trans_aibjcjalci(b, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjalci   
    integer, intent(in) :: b, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcjalci = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjalci = eom_cc3_23_tripletm_trans_aibjcjalci + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjalci
    function eom_cc3_23_tripletm_trans_aibjcibjcm(a, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcibjcm   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcibjcm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcibjcm = eom_cc3_23_tripletm_trans_aibjcibjcm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcibjcm
    function eom_cc3_23_tripletm_trans_aibjciblcj(a, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjciblcj   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, c)



    eom_cc3_23_tripletm_trans_aibjciblcj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjciblcj = eom_cc3_23_tripletm_trans_aibjciblcj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjciblcj
    function eom_cc3_23_tripletm_trans_aiajcjaiem(a, c, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcjaiem   
    integer, intent(in) :: a, c, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, e)



    eom_cc3_23_tripletm_trans_aiajcjaiem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcjaiem = eom_cc3_23_tripletm_trans_aiajcjaiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcjaiem
    function eom_cc3_23_tripletm_trans_aiajcjalei(a, c, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcjalei   
    integer, intent(in) :: a, c, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, e)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajcjalei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcjalei = eom_cc3_23_tripletm_trans_aiajcjalei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcjalei
    function eom_cc3_23_tripletm_trans_aiajciajem(a, c, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajciajem   
    integer, intent(in) :: a, c, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, e)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajciajem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajciajem = eom_cc3_23_tripletm_trans_aiajciajem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajciajem
    function eom_cc3_23_tripletm_trans_aiajcialej(a, c, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcialej   
    integer, intent(in) :: a, c, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, e)



    eom_cc3_23_tripletm_trans_aiajcialej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcialej = eom_cc3_23_tripletm_trans_aiajcialej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcialej
    function eom_cc3_23_tripletm_trans_aibjcjaibm(b, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjaibm   
    integer, intent(in) :: b, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, b)



    eom_cc3_23_tripletm_trans_aibjcjaibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjaibm = eom_cc3_23_tripletm_trans_aibjcjaibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjaibm
    function eom_cc3_23_tripletm_trans_aibjcjalbi(b, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjalbi   
    integer, intent(in) :: b, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, b)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcjalbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjalbi = eom_cc3_23_tripletm_trans_aibjcjalbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjalbi
    function eom_cc3_23_tripletm_trans_aibjciajbm(a, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjciajbm   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, a)



    eom_cc3_23_tripletm_trans_aibjciajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjciajbm = eom_cc3_23_tripletm_trans_aibjciajbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjciajbm
    function eom_cc3_23_tripletm_trans_aibjcialbj(a, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcialbj   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, a)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcialbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcialbj = eom_cc3_23_tripletm_trans_aibjcialbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcialbj
    function eom_cc3_23_tripletm_trans_aibiciaiem(b, c, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiciaiem   
    integer, intent(in) :: b, c, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, e)



    eom_cc3_23_tripletm_trans_aibiciaiem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiciaiem = eom_cc3_23_tripletm_trans_aibiciaiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiciaiem
    function eom_cc3_23_tripletm_trans_aibjcjaiej(b, j, c, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjaiej   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, e)



    eom_cc3_23_tripletm_trans_aibjcjaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjaiej = eom_cc3_23_tripletm_trans_aibjcjaiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjaiej
    function eom_cc3_23_tripletm_trans_aibicialei(b, c, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibicialei   
    integer, intent(in) :: b, c, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, e)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibicialei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibicialei = eom_cc3_23_tripletm_trans_aibicialei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibicialei
    function eom_cc3_23_tripletm_trans_aibjcjajei(b, j, c, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjajei   
    integer, intent(in) :: b, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, e)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcjajei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjajei = eom_cc3_23_tripletm_trans_aibjcjajei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjajei
    function eom_cc3_23_tripletm_trans_aibicibiem(a, c, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibicibiem   
    integer, intent(in) :: a, c, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, e)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibicibiem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibicibiem = eom_cc3_23_tripletm_trans_aibicibiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibicibiem
    function eom_cc3_23_tripletm_trans_aibjcibiej(a, i, c, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcibiej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, e)



    eom_cc3_23_tripletm_trans_aibjcibiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcibiej = eom_cc3_23_tripletm_trans_aibjcibiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcibiej
    function eom_cc3_23_tripletm_trans_aibiciblei(a, c, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibiciblei   
    integer, intent(in) :: a, c, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, e)



    eom_cc3_23_tripletm_trans_aibiciblei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiciblei = eom_cc3_23_tripletm_trans_aibiciblei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiciblei
    function eom_cc3_23_tripletm_trans_aibjcibjei(a, i, c, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcibjei   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, e)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcibjei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcibjei = eom_cc3_23_tripletm_trans_aibjcibjei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcibjei
    function eom_cc3_23_tripletm_trans_aiajcjdiam(a, c, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcjdiam   
    integer, intent(in) :: a, c, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajcjdiam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcjdiam = eom_cc3_23_tripletm_trans_aiajcjdiam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcjdiam
    function eom_cc3_23_tripletm_trans_aiajcjdlai(a, c, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcjdlai   
    integer, intent(in) :: a, c, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, d)



    eom_cc3_23_tripletm_trans_aiajcjdlai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcjdlai = eom_cc3_23_tripletm_trans_aiajcjdlai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcjdlai
    function eom_cc3_23_tripletm_trans_aiajcidjam(a, c, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcidjam   
    integer, intent(in) :: a, c, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, d)



    eom_cc3_23_tripletm_trans_aiajcidjam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcidjam = eom_cc3_23_tripletm_trans_aiajcidjam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcidjam
    function eom_cc3_23_tripletm_trans_aiajcidlaj(a, c, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcidlaj   
    integer, intent(in) :: a, c, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajcidlaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcidlaj = eom_cc3_23_tripletm_trans_aiajcidlaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcidlaj
    function eom_cc3_23_tripletm_trans_aibicidiam(b, c, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibicidiam   
    integer, intent(in) :: b, c, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibicidiam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibicidiam = eom_cc3_23_tripletm_trans_aibicidiam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibicidiam
    function eom_cc3_23_tripletm_trans_aibjcjdiaj(b, j, c, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjdiaj   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcjdiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjdiaj = eom_cc3_23_tripletm_trans_aibjcjdiaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjdiaj
    function eom_cc3_23_tripletm_trans_aibicidlai(b, c, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibicidlai   
    integer, intent(in) :: b, c, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, d)



    eom_cc3_23_tripletm_trans_aibicidlai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibicidlai = eom_cc3_23_tripletm_trans_aibicidlai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibicidlai
    function eom_cc3_23_tripletm_trans_aibjcjdjai(b, j, c, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjdjai   
    integer, intent(in) :: b, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, d)



    eom_cc3_23_tripletm_trans_aibjcjdjai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjdjai = eom_cc3_23_tripletm_trans_aibjcjdjai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjdjai
    function eom_cc3_23_tripletm_trans_aibicidibm(a, c, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibicidibm   
    integer, intent(in) :: a, c, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, d)



    eom_cc3_23_tripletm_trans_aibicidibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibicidibm = eom_cc3_23_tripletm_trans_aibicidibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibicidibm
    function eom_cc3_23_tripletm_trans_aibjcidibj(a, i, c, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcidibj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcidibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcidibj = eom_cc3_23_tripletm_trans_aibjcidibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcidibj
    function eom_cc3_23_tripletm_trans_aibicidlbi(a, c, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibicidlbi   
    integer, intent(in) :: a, c, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibicidlbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibicidlbi = eom_cc3_23_tripletm_trans_aibicidlbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibicidlbi
    function eom_cc3_23_tripletm_trans_aibjcidjbi(a, i, c, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcidjbi   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, d)



    eom_cc3_23_tripletm_trans_aibjcidjbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcidjbi = eom_cc3_23_tripletm_trans_aibjcidjbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcidjbi
    function eom_cc3_23_tripletm_trans_aiajaiaiem(i, j, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaiaiem   
    integer, intent(in) :: i, j, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajaiaiem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajaiaiem = eom_cc3_23_tripletm_trans_aiajaiaiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaiaiem
    function eom_cc3_23_tripletm_trans_aiajakaiej(i, j, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakaiej   
    integer, intent(in) :: i, j, k, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, k, j)
term(1) = term(1) + tovoo(i, e, k, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aiajakaiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aiajakaiej = eom_cc3_23_tripletm_trans_aiajakaiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakaiej
    function eom_cc3_23_tripletm_trans_aiajajaiem(nocc, a, i, j, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajaiem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, e, m 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, j, j)
term(1) = term(1) + tovoo(m, e, i, i)
term(2) = term(2) + tovoo(i, e, m, i)
term(3) = term(3) + tov(m, e)
term(4) = term(4) + tvvov(a, a, m, e)
term(5) = term(5) + tvvov(a, e, m, a)

term(0) = -term(0) 
term(1) = -term(1) 
term(4) = term(4) * 2.0d+0 
term(5) = -term(5) 

do n = 1, nocc 
term(6) = term(6) + tovoo(m, e, n, n)
term(7) = term(7) + tovoo(n, e, m, n)
end do 

term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 


    eom_cc3_23_tripletm_trans_aiajajaiem = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aiajajaiem = eom_cc3_23_tripletm_trans_aiajajaiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajaiem
    function eom_cc3_23_tripletm_trans_aiajakaiek(j, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakaiek   
    integer, intent(in) :: j, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajakaiek = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakaiek = eom_cc3_23_tripletm_trans_aiajakaiek + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakaiek
    function eom_cc3_23_tripletm_trans_aiajaialei(i, j, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaialei   
    integer, intent(in) :: i, j, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, l, j)



    eom_cc3_23_tripletm_trans_aiajaialei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajaialei = eom_cc3_23_tripletm_trans_aiajaialei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaialei
    function eom_cc3_23_tripletm_trans_aiajakakei(j, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakakei   
    integer, intent(in) :: j, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, j)



    eom_cc3_23_tripletm_trans_aiajakakei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakakei = eom_cc3_23_tripletm_trans_aiajakakei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakakei
    function eom_cc3_23_tripletm_trans_aiajajalei(nocc, a, i, j, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajalei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, l, e 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, j, j)
term(1) = term(1) + tovoo(i, e, l, i)
term(2) = term(2) + tovoo(l, e, i, i)
term(3) = term(3) + tov(l, e)
term(4) = term(4) + tvvov(a, a, l, e)
term(5) = term(5) + tvvov(a, e, l, a)

term(1) = -term(1) 
term(3) = -term(3) 
term(4) = term(4) * (-2.0d+0) 

do n = 1, nocc 
term(6) = term(6) + tovoo(n, e, l, n)
term(7) = term(7) + tovoo(l, e, n, n)
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aiajajalei = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aiajajalei = eom_cc3_23_tripletm_trans_aiajajalei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajalei
    function eom_cc3_23_tripletm_trans_aiajaiajem(nocc, a, i, j, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaiajem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, e, m 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, i, i)
term(1) = term(1) + tovoo(m, e, j, j)
term(2) = term(2) + tovoo(j, e, m, j)
term(3) = term(3) + tov(m, e)
term(4) = term(4) + tvvov(a, a, m, e)
term(5) = term(5) + tvvov(a, e, m, a)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = term(4) * (-2.0d+0) 

do n = 1, nocc 
term(6) = term(6) + tovoo(m, e, n, n)
term(7) = term(7) + tovoo(n, e, m, n)
end do 

term(6) = term(6) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aiajaiajem = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aiajaiajem = eom_cc3_23_tripletm_trans_aiajaiajem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaiajem
    function eom_cc3_23_tripletm_trans_aiajaialej(nocc, a, i, j, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaialej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, l, e 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, i, i)
term(1) = term(1) + tovoo(j, e, l, j)
term(2) = term(2) + tovoo(l, e, j, j)
term(3) = term(3) + tov(l, e)
term(4) = term(4) + tvvov(a, a, l, e)
term(5) = term(5) + tvvov(a, e, l, a)

term(0) = -term(0) 
term(2) = -term(2) 
term(4) = term(4) * 2.0d+0 
term(5) = -term(5) 

do n = 1, nocc 
term(6) = term(6) + tovoo(n, e, l, n)
term(7) = term(7) + tovoo(l, e, n, n)
end do 

term(6) = -term(6) 
term(7) = term(7) * 2.0d+0 


    eom_cc3_23_tripletm_trans_aiajaialej = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aiajaialej = eom_cc3_23_tripletm_trans_aiajaialej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaialej
    function eom_cc3_23_tripletm_trans_aiajajajem(i, j, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajajem   
    integer, intent(in) :: i, j, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, m, i)



    eom_cc3_23_tripletm_trans_aiajajajem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajajajem = eom_cc3_23_tripletm_trans_aiajajajem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajajem
    function eom_cc3_23_tripletm_trans_aiajakajek(i, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakajek   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)



    eom_cc3_23_tripletm_trans_aiajakajek = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakajek = eom_cc3_23_tripletm_trans_aiajakajek + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakajek
    function eom_cc3_23_tripletm_trans_aiajakakej(i, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakakej   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajakakej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakakej = eom_cc3_23_tripletm_trans_aiajakakej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakakej
    function eom_cc3_23_tripletm_trans_aiajajalej(i, j, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajalej   
    integer, intent(in) :: i, j, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, l, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajajalej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajajalej = eom_cc3_23_tripletm_trans_aiajajalej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajalej
    function eom_cc3_23_tripletm_trans_aibiakaibm(a, i, k, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiakaibm   
    integer, intent(in) :: a, i, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, a, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibiakaibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiakaibm = eom_cc3_23_tripletm_trans_aibiakaibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiakaibm
    function eom_cc3_23_tripletm_trans_aibjaiaibm(a, i, j, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaiaibm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, a, i, j)
term(1) = term(1) + tovoo(i, a, m, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjaiaibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaiaibm = eom_cc3_23_tripletm_trans_aibjaiaibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaiaibm
    function eom_cc3_23_tripletm_trans_aibjakaibj(a, i, k) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakaibj   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, k, i)



    eom_cc3_23_tripletm_trans_aibjakaibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakaibj = eom_cc3_23_tripletm_trans_aibjakaibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakaibj
    function eom_cc3_23_tripletm_trans_aibjajaibm(a, b, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajaibm   
    integer, intent(in) :: a, b, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, m, b)



    eom_cc3_23_tripletm_trans_aibjajaibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajaibm = eom_cc3_23_tripletm_trans_aibjajaibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajaibm
    function eom_cc3_23_tripletm_trans_aibiakalbi(a, i, k, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibiakalbi   
    integer, intent(in) :: a, i, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, a, k, i)



    eom_cc3_23_tripletm_trans_aibiakalbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiakalbi = eom_cc3_23_tripletm_trans_aibiakalbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiakalbi
    function eom_cc3_23_tripletm_trans_aibiaialbm(a, i, l, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaialbm   
    integer, intent(in) :: a, i, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, a, l, i)
term(1) = term(1) + tovoo(l, a, m, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibiaialbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibiaialbm = eom_cc3_23_tripletm_trans_aibiaialbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaialbm
    function eom_cc3_23_tripletm_trans_aibjaialbi(a, i, j, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaialbi   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, l, j)
term(1) = term(1) + tovoo(l, a, i, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjaialbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaialbi = eom_cc3_23_tripletm_trans_aibjaialbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaialbi
    function eom_cc3_23_tripletm_trans_aibjakajbi(a, i, k) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakajbi   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjakajbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakajbi = eom_cc3_23_tripletm_trans_aibjakajbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakajbi
    function eom_cc3_23_tripletm_trans_aibjajalbi(a, b, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajalbi   
    integer, intent(in) :: a, b, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, l, b)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjajalbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajalbi = eom_cc3_23_tripletm_trans_aibjajalbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajalbi
    function eom_cc3_23_tripletm_trans_aibjaiajbm(nocc, a, i, b, j, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaiajbm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, a, i, i)
term(1) = term(1) + tovoo(m, a, j, j)
term(2) = term(2) + tovoo(j, a, m, j)
term(3) = term(3) + tvvov(a, a, m, a)
term(4) = term(4) + tov(m, a)
term(5) = term(5) + tvvov(b, a, m, b)
term(6) = term(6) + tvvov(b, b, m, a)

term(0) = -term(0) 
term(1) = -term(1) 
term(5) = -term(5) 

do n = 1, nocc 
term(7) = term(7) + tovoo(m, a, n, n)
term(8) = term(8) + tovoo(n, a, m, n)
end do 

term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 


    eom_cc3_23_tripletm_trans_aibjaiajbm = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjaiajbm = eom_cc3_23_tripletm_trans_aibjaiajbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaiajbm
    function eom_cc3_23_tripletm_trans_aibjaialbj(nocc, a, i, b, j, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaialbj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, a, l, j)
term(1) = term(1) + tovoo(l, a, i, i)
term(2) = term(2) + tovoo(l, a, j, j)
term(3) = term(3) + tvvov(a, a, l, a)
term(4) = term(4) + tov(l, a)
term(5) = term(5) + tvvov(b, a, l, b)
term(6) = term(6) + tvvov(b, b, l, a)

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 

do n = 1, nocc 
term(7) = term(7) + tovoo(l, a, n, n)
term(8) = term(8) + tovoo(n, a, l, n)
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibjaialbj = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjaialbj = eom_cc3_23_tripletm_trans_aibjaialbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaialbj
    function eom_cc3_23_tripletm_trans_aibjajajbm(a, i, j, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajajbm   
    integer, intent(in) :: a, i, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, a, j, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjajajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajajbm = eom_cc3_23_tripletm_trans_aibjajajbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajajbm
    function eom_cc3_23_tripletm_trans_aibjakajbk(a, i, k) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakajbk   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjakajbk = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakajbk = eom_cc3_23_tripletm_trans_aibjakajbk + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakajbk
    function eom_cc3_23_tripletm_trans_aibjakakbj(a, i, k) 
    double precision :: eom_cc3_23_tripletm_trans_aibjakakbj   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, k, i)



    eom_cc3_23_tripletm_trans_aibjakakbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjakakbj = eom_cc3_23_tripletm_trans_aibjakakbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjakakbj
    function eom_cc3_23_tripletm_trans_aibjajalbj(a, i, j, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajalbj   
    integer, intent(in) :: a, i, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, a, j, i)



    eom_cc3_23_tripletm_trans_aibjajalbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajalbj = eom_cc3_23_tripletm_trans_aibjajalbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajalbj
    function eom_cc3_23_tripletm_trans_aibiaiaiem(a, b, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaiaiem   
    integer, intent(in) :: a, b, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, e, m, a)



    eom_cc3_23_tripletm_trans_aibiaiaiem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiaiaiem = eom_cc3_23_tripletm_trans_aibiaiaiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaiaiem
    function eom_cc3_23_tripletm_trans_aibjaiaiej(a, i, b, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaiaiej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, i, e)
term(1) = term(1) + tvvov(b, e, i, a)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjaiaiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaiaiej = eom_cc3_23_tripletm_trans_aibjaiaiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaiaiej
    function eom_cc3_23_tripletm_trans_aibjajaiej(a, b, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajaiej   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, j, e)



    eom_cc3_23_tripletm_trans_aibjajaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajaiej = eom_cc3_23_tripletm_trans_aibjajaiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajaiej
    function eom_cc3_23_tripletm_trans_aibiaialei(a, b, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaialei   
    integer, intent(in) :: a, b, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, e, l, a)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibiaialei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiaialei = eom_cc3_23_tripletm_trans_aibiaialei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaialei
    function eom_cc3_23_tripletm_trans_aibjaiajei(a, i, b, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaiajei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, i, e)
term(1) = term(1) + tvvov(b, e, i, a)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjaiajei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaiajei = eom_cc3_23_tripletm_trans_aibjaiajei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaiajei
    function eom_cc3_23_tripletm_trans_aibjajajei(a, b, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajajei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, j, e)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjajajei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajajei = eom_cc3_23_tripletm_trans_aibjajajei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajajei
    function eom_cc3_23_tripletm_trans_aibibibiem(a, b, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibibibiem   
    integer, intent(in) :: a, b, e, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, m, b)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibibibiem = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibibiem = eom_cc3_23_tripletm_trans_aibibibiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibibiem
    function eom_cc3_23_tripletm_trans_aibjbibiej(a, i, b, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbibiej   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, i, e)



    eom_cc3_23_tripletm_trans_aibjbibiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbibiej = eom_cc3_23_tripletm_trans_aibjbibiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbibiej
    function eom_cc3_23_tripletm_trans_aibjbjbiej(a, b, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjbiej   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, j, e)
term(1) = term(1) + tvvov(a, e, j, b)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjbjbiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjbiej = eom_cc3_23_tripletm_trans_aibjbjbiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjbiej
    function eom_cc3_23_tripletm_trans_aibibiblei(a, b, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibibiblei   
    integer, intent(in) :: a, b, l, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, e, l, b)



    eom_cc3_23_tripletm_trans_aibibiblei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibiblei = eom_cc3_23_tripletm_trans_aibibiblei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibiblei
    function eom_cc3_23_tripletm_trans_aibjbibjei(a, i, b, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbibjei   
    integer, intent(in) :: a, i, b, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, i, e)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbibjei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbibjei = eom_cc3_23_tripletm_trans_aibjbibjei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbibjei
    function eom_cc3_23_tripletm_trans_aibjbjbjei(a, b, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjbjei   
    integer, intent(in) :: a, b, j, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, j, e)
term(1) = term(1) + tvvov(a, e, j, b)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbjbjei = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjbjei = eom_cc3_23_tripletm_trans_aibjbjbjei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjbjei
    function eom_cc3_23_tripletm_trans_aiajcjciam(a, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcjciam   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajcjciam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcjciam = eom_cc3_23_tripletm_trans_aiajcjciam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcjciam
    function eom_cc3_23_tripletm_trans_aiajcjclai(a, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcjclai   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, c)



    eom_cc3_23_tripletm_trans_aiajcjclai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcjclai = eom_cc3_23_tripletm_trans_aiajcjclai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcjclai
    function eom_cc3_23_tripletm_trans_aiajcicjam(a, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcicjam   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, c)



    eom_cc3_23_tripletm_trans_aiajcicjam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcicjam = eom_cc3_23_tripletm_trans_aiajcicjam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcicjam
    function eom_cc3_23_tripletm_trans_aiajciclaj(a, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aiajciclaj   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajciclaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajciclaj = eom_cc3_23_tripletm_trans_aiajciclaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajciclaj
    function eom_cc3_23_tripletm_trans_aibiciciam(b, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiciciam   
    integer, intent(in) :: b, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibiciciam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiciciam = eom_cc3_23_tripletm_trans_aibiciciam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiciciam
    function eom_cc3_23_tripletm_trans_aibjcjciaj(b, j, c) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjciaj   
    integer, intent(in) :: b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcjciaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjciaj = eom_cc3_23_tripletm_trans_aibjcjciaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjciaj
    function eom_cc3_23_tripletm_trans_aibiciclai(b, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibiciclai   
    integer, intent(in) :: b, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, c)



    eom_cc3_23_tripletm_trans_aibiciclai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiciclai = eom_cc3_23_tripletm_trans_aibiciclai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiciclai
    function eom_cc3_23_tripletm_trans_aibjcjcjai(b, j, c) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjcjai   
    integer, intent(in) :: b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, c)



    eom_cc3_23_tripletm_trans_aibjcjcjai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjcjai = eom_cc3_23_tripletm_trans_aibjcjcjai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjcjai
    function eom_cc3_23_tripletm_trans_aibicicibm(a, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibicicibm   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, c)



    eom_cc3_23_tripletm_trans_aibicicibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibicicibm = eom_cc3_23_tripletm_trans_aibicicibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibicicibm
    function eom_cc3_23_tripletm_trans_aibjcicibj(a, i, c) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcicibj   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcicibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcicibj = eom_cc3_23_tripletm_trans_aibjcicibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcicibj
    function eom_cc3_23_tripletm_trans_aibiciclbi(a, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibiciclbi   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibiciclbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiciclbi = eom_cc3_23_tripletm_trans_aibiciclbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiciclbi
    function eom_cc3_23_tripletm_trans_aibjcicjbi(a, i, c) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcicjbi   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, c)



    eom_cc3_23_tripletm_trans_aibjcicjbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcicjbi = eom_cc3_23_tripletm_trans_aibjcicjbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcicjbi
    function eom_cc3_23_tripletm_trans_aiajaidiam(i, j, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaidiam   
    integer, intent(in) :: i, j, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, m, j)



    eom_cc3_23_tripletm_trans_aiajaidiam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajaidiam = eom_cc3_23_tripletm_trans_aiajaidiam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaidiam
    function eom_cc3_23_tripletm_trans_aiajakdiaj(i, j, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakdiaj   
    integer, intent(in) :: i, j, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, k, j)
term(1) = term(1) + tovoo(i, d, k, i)



    eom_cc3_23_tripletm_trans_aiajakdiaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aiajakdiaj = eom_cc3_23_tripletm_trans_aiajakdiaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakdiaj
    function eom_cc3_23_tripletm_trans_aiajajdiam(nocc, a, i, j, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajdiam 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, d, m 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, j, j)
term(1) = term(1) + tovoo(m, d, i, i)
term(2) = term(2) + tovoo(i, d, m, i)
term(3) = term(3) + tov(m, d)
term(4) = term(4) + tvvov(a, a, m, d)
term(5) = term(5) + tvvov(a, d, m, a)

term(2) = -term(2) 
term(3) = -term(3) 
term(4) = term(4) * (-2.0d+0) 

do n = 1, nocc 
term(6) = term(6) + tovoo(m, d, n, n)
term(7) = term(7) + tovoo(n, d, m, n)
end do 

term(6) = term(6) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aiajajdiam = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aiajajdiam = eom_cc3_23_tripletm_trans_aiajajdiam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajdiam
    function eom_cc3_23_tripletm_trans_aiajakdiak(j, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakdiak   
    integer, intent(in) :: j, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, j)



    eom_cc3_23_tripletm_trans_aiajakdiak = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakdiak = eom_cc3_23_tripletm_trans_aiajakdiak + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakdiak
    function eom_cc3_23_tripletm_trans_aiajaidlai(i, j, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaidlai   
    integer, intent(in) :: i, j, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, l, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajaidlai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajaidlai = eom_cc3_23_tripletm_trans_aiajaidlai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaidlai
    function eom_cc3_23_tripletm_trans_aiajakdkai(j, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakdkai   
    integer, intent(in) :: j, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajakdkai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakdkai = eom_cc3_23_tripletm_trans_aiajakdkai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakdkai
    function eom_cc3_23_tripletm_trans_aiajajdlai(nocc, a, i, j, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajdlai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, l, i)
term(1) = term(1) + tovoo(l, d, j, j)
term(2) = term(2) + tovoo(l, d, i, i)
term(3) = term(3) + tov(l, d)
term(4) = term(4) + tvvov(a, d, l, a)
term(5) = term(5) + tvvov(a, a, l, d)

term(1) = -term(1) 
term(2) = -term(2) 
term(4) = -term(4) 
term(5) = term(5) * 2.0d+0 

do n = 1, nocc 
term(6) = term(6) + tovoo(l, d, n, n)
term(7) = term(7) + tovoo(n, d, l, n)
end do 

term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 


    eom_cc3_23_tripletm_trans_aiajajdlai = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aiajajdlai = eom_cc3_23_tripletm_trans_aiajajdlai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajdlai
    function eom_cc3_23_tripletm_trans_aiajaidjam(nocc, a, i, j, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaidjam 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, d, m 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, i, i)
term(1) = term(1) + tovoo(m, d, j, j)
term(2) = term(2) + tovoo(j, d, m, j)
term(3) = term(3) + tov(m, d)
term(4) = term(4) + tvvov(a, a, m, d)
term(5) = term(5) + tvvov(a, d, m, a)

term(0) = -term(0) 
term(1) = -term(1) 
term(4) = term(4) * 2.0d+0 
term(5) = -term(5) 

do n = 1, nocc 
term(6) = term(6) + tovoo(m, d, n, n)
term(7) = term(7) + tovoo(n, d, m, n)
end do 

term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 


    eom_cc3_23_tripletm_trans_aiajaidjam = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aiajaidjam = eom_cc3_23_tripletm_trans_aiajaidjam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaidjam
    function eom_cc3_23_tripletm_trans_aiajaidlaj(nocc, a, i, j, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaidlaj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, l, j)
term(1) = term(1) + tovoo(l, d, i, i)
term(2) = term(2) + tovoo(l, d, j, j)
term(3) = term(3) + tov(l, d)
term(4) = term(4) + tvvov(a, d, l, a)
term(5) = term(5) + tvvov(a, a, l, d)

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = term(5) * (-2.0d+0) 

do n = 1, nocc 
term(6) = term(6) + tovoo(l, d, n, n)
term(7) = term(7) + tovoo(n, d, l, n)
end do 

term(6) = term(6) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aiajaidlaj = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aiajaidlaj = eom_cc3_23_tripletm_trans_aiajaidlaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaidlaj
    function eom_cc3_23_tripletm_trans_aiajajdjam(i, j, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajdjam   
    integer, intent(in) :: i, j, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, m, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajajdjam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajajdjam = eom_cc3_23_tripletm_trans_aiajajdjam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajdjam
    function eom_cc3_23_tripletm_trans_aiajakdjak(i, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakdjak   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajakdjak = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakdjak = eom_cc3_23_tripletm_trans_aiajakdjak + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakdjak
    function eom_cc3_23_tripletm_trans_aiajakdkaj(i, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aiajakdkaj   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, i)



    eom_cc3_23_tripletm_trans_aiajakdkaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajakdkaj = eom_cc3_23_tripletm_trans_aiajakdkaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajakdkaj
    function eom_cc3_23_tripletm_trans_aiajajdlaj(i, j, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajdlaj   
    integer, intent(in) :: i, j, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, l, i)



    eom_cc3_23_tripletm_trans_aiajajdlaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajajdlaj = eom_cc3_23_tripletm_trans_aiajajdlaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajdlaj
    function eom_cc3_23_tripletm_trans_aiajaidiej(a, i, d, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaidiej   
    integer, intent(in) :: a, i, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, i, e)
term(1) = term(1) + tvvov(a, e, i, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aiajaidiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aiajaidiej = eom_cc3_23_tripletm_trans_aiajaidiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaidiej
    function eom_cc3_23_tripletm_trans_aiajajdiej(a, j, d, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajdiej   
    integer, intent(in) :: a, j, d, e 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, e)
term(1) = term(1) + tvvov(a, e, j, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aiajajdiej = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aiajajdiej = eom_cc3_23_tripletm_trans_aiajajdiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajdiej
    function eom_cc3_23_tripletm_trans_aibiaidiam(a, b, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaidiam   
    integer, intent(in) :: a, b, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, m, a)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibiaidiam = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiaidiam = eom_cc3_23_tripletm_trans_aibiaidiam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaidiam
    function eom_cc3_23_tripletm_trans_aibjaidiaj(a, i, b, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidiaj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, i, a)
term(1) = term(1) + tvvov(b, a, i, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjaidiaj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaidiaj = eom_cc3_23_tripletm_trans_aibjaidiaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidiaj
    function eom_cc3_23_tripletm_trans_aibjajdiaj(a, b, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajdiaj   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, j, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjajdiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajdiaj = eom_cc3_23_tripletm_trans_aibjajdiaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajdiaj
    function eom_cc3_23_tripletm_trans_aibiaidlai(a, b, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaidlai   
    integer, intent(in) :: a, b, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, l, a)



    eom_cc3_23_tripletm_trans_aibiaidlai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiaidlai = eom_cc3_23_tripletm_trans_aibiaidlai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaidlai
    function eom_cc3_23_tripletm_trans_aibjaidjai(a, i, b, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidjai   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, d, i, a)
term(1) = term(1) + tvvov(b, a, i, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjaidjai = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjaidjai = eom_cc3_23_tripletm_trans_aibjaidjai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidjai
    function eom_cc3_23_tripletm_trans_aibjajdjai(a, b, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajdjai   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, a, j, d)



    eom_cc3_23_tripletm_trans_aibjajdjai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajdjai = eom_cc3_23_tripletm_trans_aibjajdjai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajdjai
    function eom_cc3_23_tripletm_trans_aibiaibiem(nocc, a, i, b, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaibiem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, e, m 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, i, i)
term(1) = term(1) + tovoo(i, e, m, i)
term(2) = term(2) + tov(m, e)
term(3) = term(3) + tvvov(a, a, m, e)
term(4) = term(4) + tvvov(b, b, m, e)
term(5) = term(5) + tvvov(b, e, m, b)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 

do n = 1, nocc 
term(6) = term(6) + tovoo(m, e, n, n)
term(7) = term(7) + tovoo(n, e, m, n)
end do 

term(6) = term(6) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibiaibiem = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aibiaibiem = eom_cc3_23_tripletm_trans_aibiaibiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaibiem
    function eom_cc3_23_tripletm_trans_aibiakbiek(i, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibiakbiek   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)



    eom_cc3_23_tripletm_trans_aibiakbiek = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiakbiek = eom_cc3_23_tripletm_trans_aibiakbiek + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiakbiek
    function eom_cc3_23_tripletm_trans_aibjaibiej(nocc, a, i, b, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaibiej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, i, j)
term(1) = term(1) + tovoo(i, e, j, j)
term(2) = term(2) + tovoo(i, e, i, i)
term(3) = term(3) + tov(i, e)
term(4) = term(4) + tvvov(a, a, i, e)
term(5) = term(5) + tvvov(b, b, i, e)
term(6) = term(6) + tvvov(b, e, i, b)

term(1) = -term(1) 
term(2) = -term(2) 
term(6) = -term(6) 

do n = 1, nocc 
term(7) = term(7) + tovoo(n, e, i, n)
term(8) = term(8) + tovoo(i, e, n, n)
end do 

term(7) = -term(7) 
term(8) = term(8) * 2.0d+0 


    eom_cc3_23_tripletm_trans_aibjaibiej = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjaibiej = eom_cc3_23_tripletm_trans_aibjaibiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaibiej
    function eom_cc3_23_tripletm_trans_aibjajbiej(i, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajbiej   
    integer, intent(in) :: i, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, j, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjajbiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajbiej = eom_cc3_23_tripletm_trans_aibjajbiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajbiej
    function eom_cc3_23_tripletm_trans_aibiaiblei(nocc, a, i, b, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaiblei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, l, e 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, i, i)
term(1) = term(1) + tovoo(i, e, l, i)
term(2) = term(2) + tov(l, e)
term(3) = term(3) + tvvov(a, a, l, e)
term(4) = term(4) + tvvov(b, b, l, e)
term(5) = term(5) + tvvov(b, e, l, b)

term(0) = term(0) * (-2.0d+0) 
term(5) = -term(5) 

do n = 1, nocc 
term(6) = term(6) + tovoo(n, e, l, n)
term(7) = term(7) + tovoo(l, e, n, n)
end do 

term(6) = -term(6) 
term(7) = term(7) * 2.0d+0 


    eom_cc3_23_tripletm_trans_aibiaiblei = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aibiaiblei = eom_cc3_23_tripletm_trans_aibiaiblei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaiblei
    function eom_cc3_23_tripletm_trans_aibiakbkei(i, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibiakbkei   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibiakbkei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiakbkei = eom_cc3_23_tripletm_trans_aibiakbkei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiakbkei
    function eom_cc3_23_tripletm_trans_aibjaibjei(nocc, a, i, b, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaibjei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, j, j)
term(1) = term(1) + tovoo(j, e, i, j)
term(2) = term(2) + tovoo(i, e, i, i)
term(3) = term(3) + tov(i, e)
term(4) = term(4) + tvvov(a, a, i, e)
term(5) = term(5) + tvvov(b, b, i, e)
term(6) = term(6) + tvvov(b, e, i, b)

term(1) = -term(1) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 

do n = 1, nocc 
term(7) = term(7) + tovoo(i, e, n, n)
term(8) = term(8) + tovoo(n, e, i, n)
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibjaibjei = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjaibjei = eom_cc3_23_tripletm_trans_aibjaibjei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaibjei
    function eom_cc3_23_tripletm_trans_aibjajbjei(i, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajbjei   
    integer, intent(in) :: i, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, j, i)



    eom_cc3_23_tripletm_trans_aibjajbjei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajbjei = eom_cc3_23_tripletm_trans_aibjajbjei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajbjei
    function eom_cc3_23_tripletm_trans_aibiaidibm(nocc, a, i, b, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaidibm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, d, m 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, i, i)
term(1) = term(1) + tovoo(i, d, m, i)
term(2) = term(2) + tov(m, d)
term(3) = term(3) + tvvov(a, a, m, d)
term(4) = term(4) + tvvov(b, d, m, b)
term(5) = term(5) + tvvov(b, b, m, d)

term(0) = term(0) * (-2.0d+0) 
term(4) = -term(4) 

do n = 1, nocc 
term(6) = term(6) + tovoo(m, d, n, n)
term(7) = term(7) + tovoo(n, d, m, n)
end do 

term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 


    eom_cc3_23_tripletm_trans_aibiaidibm = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aibiaidibm = eom_cc3_23_tripletm_trans_aibiaidibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaidibm
    function eom_cc3_23_tripletm_trans_aibiakdibk(i, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibiakdibk   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibiakdibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiakdibk = eom_cc3_23_tripletm_trans_aibiakdibk + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiakdibk
    function eom_cc3_23_tripletm_trans_aibjaidibj(nocc, a, i, b, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, i, j)
term(1) = term(1) + tovoo(i, d, j, j)
term(2) = term(2) + tovoo(i, d, i, i)
term(3) = term(3) + tov(i, d)
term(4) = term(4) + tvvov(b, d, i, b)
term(5) = term(5) + tvvov(a, a, i, d)
term(6) = term(6) + tvvov(b, b, i, d)

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = -term(5) 
term(6) = -term(6) 

do n = 1, nocc 
term(7) = term(7) + tovoo(i, d, n, n)
term(8) = term(8) + tovoo(n, d, i, n)
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibjaidibj = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjaidibj = eom_cc3_23_tripletm_trans_aibjaidibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidibj
    function eom_cc3_23_tripletm_trans_aibjajdibj(i, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajdibj   
    integer, intent(in) :: i, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, j, i)



    eom_cc3_23_tripletm_trans_aibjajdibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajdibj = eom_cc3_23_tripletm_trans_aibjajdibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajdibj
    function eom_cc3_23_tripletm_trans_aibiaidlbi(nocc, a, i, b, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaidlbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, l, i)
term(1) = term(1) + tovoo(l, d, i, i)
term(2) = term(2) + tov(l, d)
term(3) = term(3) + tvvov(b, d, l, b)
term(4) = term(4) + tvvov(a, a, l, d)
term(5) = term(5) + tvvov(b, b, l, d)

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = -term(2) 
term(4) = -term(4) 
term(5) = -term(5) 

do n = 1, nocc 
term(6) = term(6) + tovoo(l, d, n, n)
term(7) = term(7) + tovoo(n, d, l, n)
end do 

term(6) = term(6) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibiaidlbi = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aibiaidlbi = eom_cc3_23_tripletm_trans_aibiaidlbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaidlbi
    function eom_cc3_23_tripletm_trans_aibiakdkbi(i, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibiakdkbi   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, i)



    eom_cc3_23_tripletm_trans_aibiakdkbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiakdkbi = eom_cc3_23_tripletm_trans_aibiakdkbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiakdkbi
    function eom_cc3_23_tripletm_trans_aibjaidjbi(nocc, a, i, b, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaidjbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, j, j)
term(1) = term(1) + tovoo(j, d, i, j)
term(2) = term(2) + tovoo(i, d, i, i)
term(3) = term(3) + tov(i, d)
term(4) = term(4) + tvvov(a, a, i, d)
term(5) = term(5) + tvvov(b, d, i, b)
term(6) = term(6) + tvvov(b, b, i, d)

term(0) = -term(0) 
term(2) = -term(2) 
term(5) = -term(5) 

do n = 1, nocc 
term(7) = term(7) + tovoo(i, d, n, n)
term(8) = term(8) + tovoo(n, d, i, n)
end do 

term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 


    eom_cc3_23_tripletm_trans_aibjaidjbi = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjaidjbi = eom_cc3_23_tripletm_trans_aibjaidjbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaidjbi
    function eom_cc3_23_tripletm_trans_aibjajdjbi(i, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajdjbi   
    integer, intent(in) :: i, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, j, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjajdjbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjajdjbi = eom_cc3_23_tripletm_trans_aibjajdjbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajdjbi
    function eom_cc3_23_tripletm_trans_aibibkaibm(i, b, k, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibibkaibm   
    integer, intent(in) :: i, b, k, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, b, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibibkaibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibkaibm = eom_cc3_23_tripletm_trans_aibibkaibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibkaibm
    function eom_cc3_23_tripletm_trans_aibjbiaibm(i, b, j, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbiaibm   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, b, i, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbiaibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbiaibm = eom_cc3_23_tripletm_trans_aibjbiaibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbiaibm
    function eom_cc3_23_tripletm_trans_aibjbkaibj(b, j, k) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkaibj   
    integer, intent(in) :: b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbkaibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkaibj = eom_cc3_23_tripletm_trans_aibjbkaibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkaibj
    function eom_cc3_23_tripletm_trans_aibjbjaibm(nocc, a, i, b, j, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjaibm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, m 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, b, j, j)
term(1) = term(1) + tovoo(m, b, i, i)
term(2) = term(2) + tovoo(i, b, m, i)
term(3) = term(3) + tvvov(b, b, m, b)
term(4) = term(4) + tov(m, b)
term(5) = term(5) + tvvov(a, a, m, b)
term(6) = term(6) + tvvov(a, b, m, a)

term(0) = -term(0) 
term(1) = -term(1) 
term(6) = -term(6) 

do n = 1, nocc 
term(7) = term(7) + tovoo(m, b, n, n)
term(8) = term(8) + tovoo(n, b, m, n)
end do 

term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 


    eom_cc3_23_tripletm_trans_aibjbjaibm = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjbjaibm = eom_cc3_23_tripletm_trans_aibjbjaibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjaibm
    function eom_cc3_23_tripletm_trans_aibjbkaibk(b, j, k) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkaibk   
    integer, intent(in) :: b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, k, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbkaibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkaibk = eom_cc3_23_tripletm_trans_aibjbkaibk + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkaibk
    function eom_cc3_23_tripletm_trans_aibibkalbi(i, b, k, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibibkalbi   
    integer, intent(in) :: i, b, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, b, k, i)



    eom_cc3_23_tripletm_trans_aibibkalbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibkalbi = eom_cc3_23_tripletm_trans_aibibkalbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibkalbi
    function eom_cc3_23_tripletm_trans_aibibialbm(i, b, l, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibibialbm   
    integer, intent(in) :: i, b, l, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, b, l, i)
term(1) = term(1) + tovoo(l, b, m, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibibialbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibibialbm = eom_cc3_23_tripletm_trans_aibibialbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibialbm
    function eom_cc3_23_tripletm_trans_aibjbialbi(i, b, j, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbialbi   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, b, i, j)



    eom_cc3_23_tripletm_trans_aibjbialbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbialbi = eom_cc3_23_tripletm_trans_aibjbialbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbialbi
    function eom_cc3_23_tripletm_trans_aibjbkajbi(b, j, k) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkajbi   
    integer, intent(in) :: b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, k, j)



    eom_cc3_23_tripletm_trans_aibjbkajbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkajbi = eom_cc3_23_tripletm_trans_aibjbkajbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkajbi
    function eom_cc3_23_tripletm_trans_aibjbkakbi(b, j, k) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbkakbi   
    integer, intent(in) :: b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, k, j)



    eom_cc3_23_tripletm_trans_aibjbkakbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbkakbi = eom_cc3_23_tripletm_trans_aibjbkakbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbkakbi
    function eom_cc3_23_tripletm_trans_aibjbjalbi(nocc, a, i, b, j, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjalbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, b, j, j)
term(1) = term(1) + tovoo(i, b, l, i)
term(2) = term(2) + tovoo(l, b, i, i)
term(3) = term(3) + tvvov(b, b, l, b)
term(4) = term(4) + tov(l, b)
term(5) = term(5) + tvvov(a, a, l, b)
term(6) = term(6) + tvvov(a, b, l, a)

term(1) = -term(1) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 

do n = 1, nocc 
term(7) = term(7) + tovoo(n, b, l, n)
term(8) = term(8) + tovoo(l, b, n, n)
end do 

term(8) = term(8) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibjbjalbi = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjbjalbi = eom_cc3_23_tripletm_trans_aibjbjalbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjalbi
    function eom_cc3_23_tripletm_trans_aibjbiajbm(a, b, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbiajbm   
    integer, intent(in) :: a, b, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, m, a)



    eom_cc3_23_tripletm_trans_aibjbiajbm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbiajbm = eom_cc3_23_tripletm_trans_aibjbiajbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbiajbm
    function eom_cc3_23_tripletm_trans_aibjbialbj(a, b, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbialbj   
    integer, intent(in) :: a, b, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, l, a)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbialbj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbialbj = eom_cc3_23_tripletm_trans_aibjbialbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbialbj
    function eom_cc3_23_tripletm_trans_aibjbjajbm(i, b, j, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjajbm   
    integer, intent(in) :: i, b, j, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, b, j, i)
term(1) = term(1) + tovoo(j, b, m, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbjajbm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjajbm = eom_cc3_23_tripletm_trans_aibjbjajbm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjajbm
    function eom_cc3_23_tripletm_trans_aibjbjalbj(i, b, j, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjalbj   
    integer, intent(in) :: i, b, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, l, i)
term(1) = term(1) + tovoo(l, b, j, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbjalbj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjalbj = eom_cc3_23_tripletm_trans_aibjbjalbj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjalbj
    function eom_cc3_23_tripletm_trans_aibibidibm(a, b, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibibidibm   
    integer, intent(in) :: a, b, d, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, m, b)



    eom_cc3_23_tripletm_trans_aibibidibm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibidibm = eom_cc3_23_tripletm_trans_aibibidibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibidibm
    function eom_cc3_23_tripletm_trans_aibjbidibj(a, i, b, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbidibj   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, i, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbidibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbidibj = eom_cc3_23_tripletm_trans_aibjbidibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbidibj
    function eom_cc3_23_tripletm_trans_aibjbjdibj(a, b, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdibj   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, b)
term(1) = term(1) + tvvov(a, b, j, d)

term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjbjdibj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjdibj = eom_cc3_23_tripletm_trans_aibjbjdibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdibj
    function eom_cc3_23_tripletm_trans_aibibidlbi(a, b, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibibidlbi   
    integer, intent(in) :: a, b, d, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, l, b)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibibidlbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibidlbi = eom_cc3_23_tripletm_trans_aibibidlbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibidlbi
    function eom_cc3_23_tripletm_trans_aibjbidjbi(a, i, b, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbidjbi   
    integer, intent(in) :: a, i, b, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, b, i, d)



    eom_cc3_23_tripletm_trans_aibjbidjbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbidjbi = eom_cc3_23_tripletm_trans_aibjbidjbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbidjbi
    function eom_cc3_23_tripletm_trans_aibjbjdjbi(a, b, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdjbi   
    integer, intent(in) :: a, b, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, d, j, b)
term(1) = term(1) + tvvov(a, b, j, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbjdjbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbjdjbi = eom_cc3_23_tripletm_trans_aibjbjdjbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdjbi
    function eom_cc3_23_tripletm_trans_aibibiaiem(nocc, a, i, b, e, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibibiaiem 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, e, m 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, e, i, i)
term(1) = term(1) + tovoo(i, e, m, i)
term(2) = term(2) + tov(m, e)
term(3) = term(3) + tvvov(b, b, m, e)
term(4) = term(4) + tvvov(a, a, m, e)
term(5) = term(5) + tvvov(a, e, m, a)

term(0) = term(0) * (-2.0d+0) 
term(5) = -term(5) 

do n = 1, nocc 
term(6) = term(6) + tovoo(m, e, n, n)
term(7) = term(7) + tovoo(n, e, m, n)
end do 

term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 


    eom_cc3_23_tripletm_trans_aibibiaiem = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aibibiaiem = eom_cc3_23_tripletm_trans_aibibiaiem + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibiaiem
    function eom_cc3_23_tripletm_trans_aibibkaiek(i, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibibkaiek   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibibkaiek = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibkaiek = eom_cc3_23_tripletm_trans_aibibkaiek + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibkaiek
    function eom_cc3_23_tripletm_trans_aibjbiaiej(i, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbiaiej   
    integer, intent(in) :: i, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, i, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbiaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbiaiej = eom_cc3_23_tripletm_trans_aibjbiaiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbiaiej
    function eom_cc3_23_tripletm_trans_aibjbjaiej(nocc, a, i, b, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjaiej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, i, i)
term(1) = term(1) + tovoo(i, e, j, i)
term(2) = term(2) + tovoo(j, e, j, j)
term(3) = term(3) + tov(j, e)
term(4) = term(4) + tvvov(b, b, j, e)
term(5) = term(5) + tvvov(a, a, j, e)
term(6) = term(6) + tvvov(a, e, j, a)

term(0) = -term(0) 
term(2) = -term(2) 
term(6) = -term(6) 

do n = 1, nocc 
term(7) = term(7) + tovoo(j, e, n, n)
term(8) = term(8) + tovoo(n, e, j, n)
end do 

term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 


    eom_cc3_23_tripletm_trans_aibjbjaiej = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjbjaiej = eom_cc3_23_tripletm_trans_aibjbjaiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjaiej
    function eom_cc3_23_tripletm_trans_aibibialei(nocc, a, i, b, l, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibibialei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, l, e 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, e, i, i)
term(1) = term(1) + tovoo(i, e, l, i)
term(2) = term(2) + tov(l, e)
term(3) = term(3) + tvvov(b, b, l, e)
term(4) = term(4) + tvvov(a, a, l, e)
term(5) = term(5) + tvvov(a, e, l, a)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 

do n = 1, nocc 
term(6) = term(6) + tovoo(n, e, l, n)
term(7) = term(7) + tovoo(l, e, n, n)
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibibialei = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aibibialei = eom_cc3_23_tripletm_trans_aibibialei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibialei
    function eom_cc3_23_tripletm_trans_aibibkakei(i, k, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibibkakei   
    integer, intent(in) :: i, k, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, e, k, i)



    eom_cc3_23_tripletm_trans_aibibkakei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibkakei = eom_cc3_23_tripletm_trans_aibibkakei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibkakei
    function eom_cc3_23_tripletm_trans_aibjbiajei(i, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbiajei   
    integer, intent(in) :: i, j, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, i, j)



    eom_cc3_23_tripletm_trans_aibjbiajei = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbiajei = eom_cc3_23_tripletm_trans_aibjbiajei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbiajei
    function eom_cc3_23_tripletm_trans_aibjbjajei(nocc, a, i, b, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjajei 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, e 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, j, i)
term(1) = term(1) + tovoo(j, e, i, i)
term(2) = term(2) + tovoo(j, e, j, j)
term(3) = term(3) + tov(j, e)
term(4) = term(4) + tvvov(b, b, j, e)
term(5) = term(5) + tvvov(a, a, j, e)
term(6) = term(6) + tvvov(a, e, j, a)

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 

do n = 1, nocc 
term(7) = term(7) + tovoo(n, e, j, n)
term(8) = term(8) + tovoo(j, e, n, n)
end do 

term(8) = term(8) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibjbjajei = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjbjajei = eom_cc3_23_tripletm_trans_aibjbjajei + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjajei
    function eom_cc3_23_tripletm_trans_aibibidiam(nocc, a, i, b, d, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibibidiam 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, d, m 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, d, i, i)
term(1) = term(1) + tovoo(i, d, m, i)
term(2) = term(2) + tov(m, d)
term(3) = term(3) + tvvov(b, b, m, d)
term(4) = term(4) + tvvov(a, d, m, a)
term(5) = term(5) + tvvov(a, a, m, d)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 
term(5) = -term(5) 

do n = 1, nocc 
term(6) = term(6) + tovoo(m, d, n, n)
term(7) = term(7) + tovoo(n, d, m, n)
end do 

term(6) = term(6) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibibidiam = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aibibidiam = eom_cc3_23_tripletm_trans_aibibidiam + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibidiam
    function eom_cc3_23_tripletm_trans_aibibkdiak(i, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibibkdiak   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, i)



    eom_cc3_23_tripletm_trans_aibibkdiak = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibkdiak = eom_cc3_23_tripletm_trans_aibibkdiak + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibkdiak
    function eom_cc3_23_tripletm_trans_aibjbidiaj(i, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbidiaj   
    integer, intent(in) :: i, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, i, j)



    eom_cc3_23_tripletm_trans_aibjbidiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbidiaj = eom_cc3_23_tripletm_trans_aibjbidiaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbidiaj
    function eom_cc3_23_tripletm_trans_aibjbjdiaj(nocc, a, i, b, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdiaj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, i, i)
term(1) = term(1) + tovoo(i, d, j, i)
term(2) = term(2) + tovoo(j, d, j, j)
term(3) = term(3) + tov(j, d)
term(4) = term(4) + tvvov(b, b, j, d)
term(5) = term(5) + tvvov(a, d, j, a)
term(6) = term(6) + tvvov(a, a, j, d)

term(1) = -term(1) 
term(3) = -term(3) 
term(4) = -term(4) 
term(6) = -term(6) 

do n = 1, nocc 
term(7) = term(7) + tovoo(j, d, n, n)
term(8) = term(8) + tovoo(n, d, j, n)
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibjbjdiaj = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjbjdiaj = eom_cc3_23_tripletm_trans_aibjbjdiaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdiaj
    function eom_cc3_23_tripletm_trans_aibibidlai(nocc, a, i, b, d, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibibidlai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,n 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, l, i)
term(1) = term(1) + tovoo(l, d, i, i)
term(2) = term(2) + tov(l, d)
term(3) = term(3) + tvvov(a, d, l, a)
term(4) = term(4) + tvvov(b, b, l, d)
term(5) = term(5) + tvvov(a, a, l, d)

term(1) = term(1) * (-2.0d+0) 
term(3) = -term(3) 

do n = 1, nocc 
term(6) = term(6) + tovoo(l, d, n, n)
term(7) = term(7) + tovoo(n, d, l, n)
end do 

term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 


    eom_cc3_23_tripletm_trans_aibibidlai = 0.d+0
    do s = 0, 7
    eom_cc3_23_tripletm_trans_aibibidlai = eom_cc3_23_tripletm_trans_aibibidlai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibidlai
    function eom_cc3_23_tripletm_trans_aibibkdkai(i, k, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibibkdkai   
    integer, intent(in) :: i, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, d, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibibkdkai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibkdkai = eom_cc3_23_tripletm_trans_aibibkdkai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibkdkai
    function eom_cc3_23_tripletm_trans_aibjbidjai(i, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbidjai   
    integer, intent(in) :: i, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, i, j)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjbidjai = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjbidjai = eom_cc3_23_tripletm_trans_aibjbidjai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbidjai
    function eom_cc3_23_tripletm_trans_aibjbjdjai(nocc, a, i, b, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjdjai 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, j, i)
term(1) = term(1) + tovoo(j, d, i, i)
term(2) = term(2) + tovoo(j, d, j, j)
term(3) = term(3) + tov(j, d)
term(4) = term(4) + tvvov(a, d, j, a)
term(5) = term(5) + tvvov(b, b, j, d)
term(6) = term(6) + tvvov(a, a, j, d)

term(1) = -term(1) 
term(2) = -term(2) 
term(4) = -term(4) 

do n = 1, nocc 
term(7) = term(7) + tovoo(j, d, n, n)
term(8) = term(8) + tovoo(n, d, j, n)
end do 

term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 


    eom_cc3_23_tripletm_trans_aibjbjdjai = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjbjdjai = eom_cc3_23_tripletm_trans_aibjbjdjai + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjdjai
    function eom_cc3_23_tripletm_trans_aiajcjaicm(a, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcjaicm   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, c)



    eom_cc3_23_tripletm_trans_aiajcjaicm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcjaicm = eom_cc3_23_tripletm_trans_aiajcjaicm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcjaicm
    function eom_cc3_23_tripletm_trans_aiajcjalci(a, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcjalci   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajcjalci = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcjalci = eom_cc3_23_tripletm_trans_aiajcjalci + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcjalci
    function eom_cc3_23_tripletm_trans_aiajciajcm(a, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aiajciajcm   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajciajcm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajciajcm = eom_cc3_23_tripletm_trans_aiajciajcm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajciajcm
    function eom_cc3_23_tripletm_trans_aiajcialcj(a, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcialcj   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, c)



    eom_cc3_23_tripletm_trans_aiajcialcj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcialcj = eom_cc3_23_tripletm_trans_aiajcialcj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcialcj
    function eom_cc3_23_tripletm_trans_aibiciaicm(b, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiciaicm   
    integer, intent(in) :: b, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, c)



    eom_cc3_23_tripletm_trans_aibiciaicm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiciaicm = eom_cc3_23_tripletm_trans_aibiciaicm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiciaicm
    function eom_cc3_23_tripletm_trans_aibjcjaicj(b, j, c) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjaicj   
    integer, intent(in) :: b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, c)



    eom_cc3_23_tripletm_trans_aibjcjaicj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjaicj = eom_cc3_23_tripletm_trans_aibjcjaicj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjaicj
    function eom_cc3_23_tripletm_trans_aibicialci(b, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibicialci   
    integer, intent(in) :: b, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibicialci = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibicialci = eom_cc3_23_tripletm_trans_aibicialci + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibicialci
    function eom_cc3_23_tripletm_trans_aibjcjajci(b, j, c) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjajci   
    integer, intent(in) :: b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcjajci = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjajci = eom_cc3_23_tripletm_trans_aibjcjajci + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjajci
    function eom_cc3_23_tripletm_trans_aibicibicm(a, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibicibicm   
    integer, intent(in) :: a, c, m 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, m, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibicibicm = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibicibicm = eom_cc3_23_tripletm_trans_aibicibicm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibicibicm
    function eom_cc3_23_tripletm_trans_aibjcibicj(a, i, c) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcibicj   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, c)



    eom_cc3_23_tripletm_trans_aibjcibicj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcibicj = eom_cc3_23_tripletm_trans_aibjcibicj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcibicj
    function eom_cc3_23_tripletm_trans_aibiciblci(a, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibiciblci   
    integer, intent(in) :: a, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, l, c)



    eom_cc3_23_tripletm_trans_aibiciblci = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiciblci = eom_cc3_23_tripletm_trans_aibiciblci + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiciblci
    function eom_cc3_23_tripletm_trans_aibjcibjci(a, i, c) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcibjci   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcibjci = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcibjci = eom_cc3_23_tripletm_trans_aibjcibjci + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcibjci
    function eom_cc3_23_tripletm_trans_aiajciaiej(a, i, c, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajciaiej   
    integer, intent(in) :: a, i, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, e)



    eom_cc3_23_tripletm_trans_aiajciaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajciaiej = eom_cc3_23_tripletm_trans_aiajciaiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajciaiej
    function eom_cc3_23_tripletm_trans_aiajcjaiej(a, j, c, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcjaiej   
    integer, intent(in) :: a, j, c, e 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, e)



    eom_cc3_23_tripletm_trans_aiajcjaiej = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcjaiej = eom_cc3_23_tripletm_trans_aiajcjaiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcjaiej
    function eom_cc3_23_tripletm_trans_aibiciaibm(a, b, c, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiciaibm   
    integer, intent(in) :: a, b, c, m 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, m, b)
term(1) = term(1) + tvvov(a, c, m, a)



    eom_cc3_23_tripletm_trans_aibiciaibm = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibiciaibm = eom_cc3_23_tripletm_trans_aibiciaibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiciaibm
    function eom_cc3_23_tripletm_trans_aibjciaibj(a, i, c) 
    double precision :: eom_cc3_23_tripletm_trans_aibjciaibj   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, a)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjciaibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjciaibj = eom_cc3_23_tripletm_trans_aibjciaibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjciaibj
    function eom_cc3_23_tripletm_trans_aibjcjaibj(b, j, c) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjaibj   
    integer, intent(in) :: b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, b)



    eom_cc3_23_tripletm_trans_aibjcjaibj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjaibj = eom_cc3_23_tripletm_trans_aibjcjaibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjaibj
    function eom_cc3_23_tripletm_trans_aibicialbi(a, b, c, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibicialbi   
    integer, intent(in) :: a, b, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, l, b)
term(1) = term(1) + tvvov(a, c, l, a)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibicialbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibicialbi = eom_cc3_23_tripletm_trans_aibicialbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibicialbi
    function eom_cc3_23_tripletm_trans_aibjciajbi(a, i, c) 
    double precision :: eom_cc3_23_tripletm_trans_aibjciajbi   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, a)



    eom_cc3_23_tripletm_trans_aibjciajbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjciajbi = eom_cc3_23_tripletm_trans_aibjciajbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjciajbi
    function eom_cc3_23_tripletm_trans_aibjcjajbi(b, j, c) 
    double precision :: eom_cc3_23_tripletm_trans_aibjcjajbi   
    integer, intent(in) :: b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(b, c, j, b)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibjcjajbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibjcjajbi = eom_cc3_23_tripletm_trans_aibjcjajbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjcjajbi
    function eom_cc3_23_tripletm_trans_aiajcidiaj(a, i, c, d) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcidiaj   
    integer, intent(in) :: a, i, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajcidiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcidiaj = eom_cc3_23_tripletm_trans_aiajcidiaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcidiaj
    function eom_cc3_23_tripletm_trans_aiajcjdiaj(a, j, c, d) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcjdiaj   
    integer, intent(in) :: a, j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, d)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajcjdiaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcjdiaj = eom_cc3_23_tripletm_trans_aiajcjdiaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcjdiaj
    function eom_cc3_23_tripletm_trans_aiajaiaiej(nocc, a, i, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaiaiej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, e 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, e, j, j)
term(1) = term(1) + tovoo(i, e, i, i)
term(2) = term(2) + tov(i, e)
term(3) = term(3) + tvvov(a, a, i, e)
term(4) = term(4) + tvvov(a, e, i, a)

term(0) = -term(0) 
term(1) = -term(1) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 

do n = 1, nocc 
term(5) = term(5) + tovoo(n, e, i, n)
term(6) = term(6) + tovoo(i, e, n, n)
end do 

term(5) = -term(5) 
term(6) = term(6) * 2.0d+0 


    eom_cc3_23_tripletm_trans_aiajaiaiej = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletm_trans_aiajaiaiej = eom_cc3_23_tripletm_trans_aiajaiaiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaiaiej
    function eom_cc3_23_tripletm_trans_aiajajaiej(nocc, a, i, j, e) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajaiej 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, e 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, e, i, i)
term(1) = term(1) + tovoo(j, e, j, j)
term(2) = term(2) + tov(j, e)
term(3) = term(3) + tvvov(a, a, j, e)
term(4) = term(4) + tvvov(a, e, j, a)

term(0) = -term(0) 
term(1) = -term(1) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 

do n = 1, nocc 
term(5) = term(5) + tovoo(j, e, n, n)
term(6) = term(6) + tovoo(n, e, j, n)
end do 

term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 


    eom_cc3_23_tripletm_trans_aiajajaiej = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletm_trans_aiajajaiej = eom_cc3_23_tripletm_trans_aiajajaiej + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajaiej
    function eom_cc3_23_tripletm_trans_aibiaiaibm(nocc, a, i, b, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaiaibm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, m 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, a, i, i)
term(1) = term(1) + tovoo(i, a, m, i)
term(2) = term(2) + tvvov(a, a, m, a)
term(3) = term(3) + tov(m, a)
term(4) = term(4) + tvvov(b, b, m, a)

term(0) = term(0) * (-2.0d+0) 

do n = 1, nocc 
term(5) = term(5) + tovoo(m, a, n, n)
term(6) = term(6) + tovoo(n, a, m, n)
end do 

term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 


    eom_cc3_23_tripletm_trans_aibiaiaibm = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletm_trans_aibiaiaibm = eom_cc3_23_tripletm_trans_aibiaiaibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaiaibm
    function eom_cc3_23_tripletm_trans_aibiakaibk(a, i, k) 
    double precision :: eom_cc3_23_tripletm_trans_aibiakaibk   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibiakaibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiakaibk = eom_cc3_23_tripletm_trans_aibiakaibk + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiakaibk
    function eom_cc3_23_tripletm_trans_aibjaiaibj(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaiaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, a, i, j)
term(1) = term(1) + tovoo(i, a, j, j)
term(2) = term(2) + tvvov(a, a, i, a)
term(3) = term(3) + tovoo(i, a, i, i)
term(4) = term(4) + tov(i, a)
term(5) = term(5) + tvvov(b, a, i, b)
term(6) = term(6) + tvvov(b, b, i, a)

term(0) = -term(0) 
term(2) = -term(2) 
term(4) = -term(4) 
term(6) = -term(6) 

do n = 1, nocc 
term(7) = term(7) + tovoo(i, a, n, n)
term(8) = term(8) + tovoo(n, a, i, n)
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibjaiaibj = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjaiaibj = eom_cc3_23_tripletm_trans_aibjaiaibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaiaibj
    function eom_cc3_23_tripletm_trans_aibjajaibj(a, i, b, j) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajaibj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, j, i)
term(1) = term(1) + tvvov(b, a, j, b)



    eom_cc3_23_tripletm_trans_aibjajaibj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjajaibj = eom_cc3_23_tripletm_trans_aibjajaibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajaibj
    function eom_cc3_23_tripletm_trans_aibiaialbi(nocc, a, i, b, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibiaialbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, l, i)
term(1) = term(1) + tovoo(l, a, i, i)
term(2) = term(2) + tvvov(a, a, l, a)
term(3) = term(3) + tov(l, a)
term(4) = term(4) + tvvov(b, b, l, a)

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 

do n = 1, nocc 
term(5) = term(5) + tovoo(l, a, n, n)
term(6) = term(6) + tovoo(n, a, l, n)
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibiaialbi = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletm_trans_aibiaialbi = eom_cc3_23_tripletm_trans_aibiaialbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiaialbi
    function eom_cc3_23_tripletm_trans_aibiakakbi(a, i, k) 
    double precision :: eom_cc3_23_tripletm_trans_aibiakakbi   
    integer, intent(in) :: a, i, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, a, k, i)



    eom_cc3_23_tripletm_trans_aibiakakbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibiakakbi = eom_cc3_23_tripletm_trans_aibiakakbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibiakakbi
    function eom_cc3_23_tripletm_trans_aibjaiajbi(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_tripletm_trans_aibjaiajbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, j, j)
term(1) = term(1) + tovoo(j, a, i, j)
term(2) = term(2) + tvvov(a, a, i, a)
term(3) = term(3) + tovoo(i, a, i, i)
term(4) = term(4) + tov(i, a)
term(5) = term(5) + tvvov(b, a, i, b)
term(6) = term(6) + tvvov(b, b, i, a)

term(0) = -term(0) 
term(3) = -term(3) 
term(5) = -term(5) 

do n = 1, nocc 
term(7) = term(7) + tovoo(i, a, n, n)
term(8) = term(8) + tovoo(n, a, i, n)
end do 

term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 


    eom_cc3_23_tripletm_trans_aibjaiajbi = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjaiajbi = eom_cc3_23_tripletm_trans_aibjaiajbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjaiajbi
    function eom_cc3_23_tripletm_trans_aibjajajbi(a, i, b, j) 
    double precision :: eom_cc3_23_tripletm_trans_aibjajajbi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, a, j, i)
term(1) = term(1) + tvvov(b, a, j, b)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjajajbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjajajbi = eom_cc3_23_tripletm_trans_aibjajajbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjajajbi
    function eom_cc3_23_tripletm_trans_aiajciciaj(a, i, c) 
    double precision :: eom_cc3_23_tripletm_trans_aiajciciaj   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajciciaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajciciaj = eom_cc3_23_tripletm_trans_aiajciciaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajciciaj
    function eom_cc3_23_tripletm_trans_aiajcjciaj(a, j, c) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcjciaj   
    integer, intent(in) :: a, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, c)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aiajcjciaj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcjciaj = eom_cc3_23_tripletm_trans_aiajcjciaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcjciaj
    function eom_cc3_23_tripletm_trans_aiajaidiaj(nocc, a, i, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aiajaidiaj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, d, j, j)
term(1) = term(1) + tovoo(i, d, i, i)
term(2) = term(2) + tov(i, d)
term(3) = term(3) + tvvov(a, d, i, a)
term(4) = term(4) + tvvov(a, a, i, d)

term(2) = -term(2) 
term(4) = term(4) * (-2.0d+0) 

do n = 1, nocc 
term(5) = term(5) + tovoo(i, d, n, n)
term(6) = term(6) + tovoo(n, d, i, n)
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aiajaidiaj = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletm_trans_aiajaidiaj = eom_cc3_23_tripletm_trans_aiajaidiaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajaidiaj
    function eom_cc3_23_tripletm_trans_aiajajdiaj(nocc, a, i, j, d) 
    double precision :: eom_cc3_23_tripletm_trans_aiajajdiaj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, d, i, i)
term(1) = term(1) + tovoo(j, d, j, j)
term(2) = term(2) + tov(j, d)
term(3) = term(3) + tvvov(a, a, j, d)
term(4) = term(4) + tvvov(a, d, j, a)

term(2) = -term(2) 
term(3) = term(3) * (-2.0d+0) 

do n = 1, nocc 
term(5) = term(5) + tovoo(j, d, n, n)
term(6) = term(6) + tovoo(n, d, j, n)
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aiajajdiaj = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletm_trans_aiajajdiaj = eom_cc3_23_tripletm_trans_aiajajdiaj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajajdiaj
    function eom_cc3_23_tripletm_trans_aibibiaibm(nocc, a, i, b, m) 
    double precision :: eom_cc3_23_tripletm_trans_aibibiaibm 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, m 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(m, b, i, i)
term(1) = term(1) + tovoo(i, b, m, i)
term(2) = term(2) + tvvov(b, b, m, b)
term(3) = term(3) + tov(m, b)
term(4) = term(4) + tvvov(a, a, m, b)

term(0) = term(0) * (-2.0d+0) 

do n = 1, nocc 
term(5) = term(5) + tovoo(m, b, n, n)
term(6) = term(6) + tovoo(n, b, m, n)
end do 

term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 


    eom_cc3_23_tripletm_trans_aibibiaibm = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletm_trans_aibibiaibm = eom_cc3_23_tripletm_trans_aibibiaibm + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibiaibm
    function eom_cc3_23_tripletm_trans_aibibkaibk(i, b, k) 
    double precision :: eom_cc3_23_tripletm_trans_aibibkaibk   
    integer, intent(in) :: i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, k, i)

term(0) = -term(0) 


    eom_cc3_23_tripletm_trans_aibibkaibk = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibkaibk = eom_cc3_23_tripletm_trans_aibibkaibk + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibkaibk
    function eom_cc3_23_tripletm_trans_aibjbiaibj(a, i, b, j) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbiaibj   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, i, j)
term(1) = term(1) + tvvov(a, b, i, a)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_23_tripletm_trans_aibjbiaibj = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbiaibj = eom_cc3_23_tripletm_trans_aibjbiaibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbiaibj
    function eom_cc3_23_tripletm_trans_aibjbjaibj(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjaibj 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, i, i)
term(1) = term(1) + tovoo(i, b, j, i)
term(2) = term(2) + tvvov(b, b, j, b)
term(3) = term(3) + tovoo(j, b, j, j)
term(4) = term(4) + tov(j, b)
term(5) = term(5) + tvvov(a, a, j, b)
term(6) = term(6) + tvvov(a, b, j, a)

term(0) = -term(0) 
term(3) = -term(3) 
term(6) = -term(6) 

do n = 1, nocc 
term(7) = term(7) + tovoo(j, b, n, n)
term(8) = term(8) + tovoo(n, b, j, n)
end do 

term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 


    eom_cc3_23_tripletm_trans_aibjbjaibj = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjbjaibj = eom_cc3_23_tripletm_trans_aibjbjaibj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjaibj
    function eom_cc3_23_tripletm_trans_aibibialbi(nocc, a, i, b, l) 
    double precision :: eom_cc3_23_tripletm_trans_aibibialbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,n 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(l, b, i, i)
term(1) = term(1) + tovoo(i, b, l, i)
term(2) = term(2) + tvvov(b, b, l, b)
term(3) = term(3) + tov(l, b)
term(4) = term(4) + tvvov(a, a, l, b)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = -term(3) 
term(4) = -term(4) 

do n = 1, nocc 
term(5) = term(5) + tovoo(n, b, l, n)
term(6) = term(6) + tovoo(l, b, n, n)
end do 

term(6) = term(6) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibibialbi = 0.d+0
    do s = 0, 6
    eom_cc3_23_tripletm_trans_aibibialbi = eom_cc3_23_tripletm_trans_aibibialbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibialbi
    function eom_cc3_23_tripletm_trans_aibibkakbi(i, b, k) 
    double precision :: eom_cc3_23_tripletm_trans_aibibkakbi   
    integer, intent(in) :: i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(k, b, k, i)



    eom_cc3_23_tripletm_trans_aibibkakbi = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aibibkakbi = eom_cc3_23_tripletm_trans_aibibkakbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibibkakbi
    function eom_cc3_23_tripletm_trans_aibjbiajbi(a, i, b, j) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbiajbi   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(j, b, i, j)
term(1) = term(1) + tvvov(a, b, i, a)



    eom_cc3_23_tripletm_trans_aibjbiajbi = 0.d+0
    do s = 0, 1
    eom_cc3_23_tripletm_trans_aibjbiajbi = eom_cc3_23_tripletm_trans_aibjbiajbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbiajbi
    function eom_cc3_23_tripletm_trans_aibjbjajbi(nocc, a, i, b, j) 
    double precision :: eom_cc3_23_tripletm_trans_aibjbjajbi 
    integer, intent(in) :: nocc 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,n 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovoo(i, b, j, i)
term(1) = term(1) + tovoo(j, b, i, i)
term(2) = term(2) + tvvov(b, b, j, b)
term(3) = term(3) + tovoo(j, b, j, j)
term(4) = term(4) + tov(j, b)
term(5) = term(5) + tvvov(a, a, j, b)
term(6) = term(6) + tvvov(a, b, j, a)

term(0) = -term(0) 
term(2) = -term(2) 
term(4) = -term(4) 
term(5) = -term(5) 

do n = 1, nocc 
term(7) = term(7) + tovoo(n, b, j, n)
term(8) = term(8) + tovoo(j, b, n, n)
end do 

term(8) = term(8) * (-2.0d+0) 


    eom_cc3_23_tripletm_trans_aibjbjajbi = 0.d+0
    do s = 0, 8
    eom_cc3_23_tripletm_trans_aibjbjajbi = eom_cc3_23_tripletm_trans_aibjbjajbi + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aibjbjajbi
    function eom_cc3_23_tripletm_trans_aiajciaicj(a, i, c) 
    double precision :: eom_cc3_23_tripletm_trans_aiajciaicj   
    integer, intent(in) :: a, i, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, i, c)



    eom_cc3_23_tripletm_trans_aiajciaicj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajciaicj = eom_cc3_23_tripletm_trans_aiajciaicj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajciaicj
    function eom_cc3_23_tripletm_trans_aiajcjaicj(a, j, c) 
    double precision :: eom_cc3_23_tripletm_trans_aiajcjaicj   
    integer, intent(in) :: a, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvov(a, c, j, c)



    eom_cc3_23_tripletm_trans_aiajcjaicj = 0.d+0
    do s = 0, 0
    eom_cc3_23_tripletm_trans_aiajcjaicj = eom_cc3_23_tripletm_trans_aiajcjaicj + term(s)
    end do

    end function eom_cc3_23_tripletm_trans_aiajcjaicj
    end module eom_cc3_23_tripletm_trans
    
