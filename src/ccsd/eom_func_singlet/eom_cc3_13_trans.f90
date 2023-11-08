module eom_cc3_13_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_cc3_13_trans_aiaickdl(c, k, d, l) 
    double precision :: eom_cc3_13_trans_aiaickdl   
    integer, intent(in) :: c, k, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, l, c, k)
term(1) = term(1) + vovo(d, k, c, l)

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_trans_aiaickdl = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aiaickdl = eom_cc3_13_trans_aiaickdl + term(s)
    end do

    end function eom_cc3_13_trans_aiaickdl
    function eom_cc3_13_trans_aiajckdi(j, c, k, d) 
    double precision :: eom_cc3_13_trans_aiajckdi   
    integer, intent(in) :: j, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, k, c, j)
term(1) = term(1) + vovo(d, j, c, k)

term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_trans_aiajckdi = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aiajckdi = eom_cc3_13_trans_aiajckdi + term(s)
    end do

    end function eom_cc3_13_trans_aiajckdi
    function eom_cc3_13_trans_aiajcidl(j, c, d, l) 
    double precision :: eom_cc3_13_trans_aiajcidl   
    integer, intent(in) :: j, c, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, l, c, j)
term(1) = term(1) + vovo(d, j, c, l)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_trans_aiajcidl = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aiajcidl = eom_cc3_13_trans_aiajcidl + term(s)
    end do

    end function eom_cc3_13_trans_aiajcidl
    function eom_cc3_13_trans_aibiakdl(b, k, d, l) 
    double precision :: eom_cc3_13_trans_aibiakdl   
    integer, intent(in) :: b, k, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, l, b, k)
term(1) = term(1) + vovo(d, k, b, l)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_trans_aibiakdl = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibiakdl = eom_cc3_13_trans_aibiakdl + term(s)
    end do

    end function eom_cc3_13_trans_aibiakdl
    function eom_cc3_13_trans_aibjakdi(b, j, k, d) 
    double precision :: eom_cc3_13_trans_aibjakdi   
    integer, intent(in) :: b, j, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, k, b, j)
term(1) = term(1) + vovo(d, j, b, k)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_trans_aibjakdi = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibjakdi = eom_cc3_13_trans_aibjakdi + term(s)
    end do

    end function eom_cc3_13_trans_aibjakdi
    function eom_cc3_13_trans_aibjaidl(b, j, d, l) 
    double precision :: eom_cc3_13_trans_aibjaidl   
    integer, intent(in) :: b, j, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, l, b, j)
term(1) = term(1) + vovo(d, j, b, l)

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_trans_aibjaidl = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibjaidl = eom_cc3_13_trans_aibjaidl + term(s)
    end do

    end function eom_cc3_13_trans_aibjaidl
    function eom_cc3_13_trans_aibickal(b, c, k, l) 
    double precision :: eom_cc3_13_trans_aibickal   
    integer, intent(in) :: b, c, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, k, b, l)
term(1) = term(1) + vovo(c, l, b, k)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_trans_aibickal = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibickal = eom_cc3_13_trans_aibickal + term(s)
    end do

    end function eom_cc3_13_trans_aibickal
    function eom_cc3_13_trans_aibjckai(b, j, c, k) 
    double precision :: eom_cc3_13_trans_aibjckai   
    integer, intent(in) :: b, j, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, k, b, j)
term(1) = term(1) + vovo(c, j, b, k)

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_trans_aibjckai = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibjckai = eom_cc3_13_trans_aibjckai + term(s)
    end do

    end function eom_cc3_13_trans_aibjckai
    function eom_cc3_13_trans_aibjcial(b, j, c, l) 
    double precision :: eom_cc3_13_trans_aibjcial   
    integer, intent(in) :: b, j, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, l, b, j)
term(1) = term(1) + vovo(c, j, b, l)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_trans_aibjcial = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibjcial = eom_cc3_13_trans_aibjcial + term(s)
    end do

    end function eom_cc3_13_trans_aibjcial
    function eom_cc3_13_trans_aiaiakdl(a, k, d, l) 
    double precision :: eom_cc3_13_trans_aiaiakdl   
    integer, intent(in) :: a, k, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, l, a, k)
term(1) = term(1) + vovo(d, k, a, l)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    eom_cc3_13_trans_aiaiakdl = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aiaiakdl = eom_cc3_13_trans_aiaiakdl + term(s)
    end do

    end function eom_cc3_13_trans_aiaiakdl
    function eom_cc3_13_trans_aiajakdi(a, j, k, d) 
    double precision :: eom_cc3_13_trans_aiajakdi   
    integer, intent(in) :: a, j, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, k, a, j)
term(1) = term(1) + vovo(d, j, a, k)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_13_trans_aiajakdi = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aiajakdi = eom_cc3_13_trans_aiajakdi + term(s)
    end do

    end function eom_cc3_13_trans_aiajakdi
    function eom_cc3_13_trans_aiajaidl(a, j, d, l) 
    double precision :: eom_cc3_13_trans_aiajaidl   
    integer, intent(in) :: a, j, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, l, a, j)
term(1) = term(1) + vovo(d, j, a, l)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    eom_cc3_13_trans_aiajaidl = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aiajaidl = eom_cc3_13_trans_aiajaidl + term(s)
    end do

    end function eom_cc3_13_trans_aiajaidl
    function eom_cc3_13_trans_aibibkal(b, k, l) 
    double precision :: eom_cc3_13_trans_aibibkal   
    integer, intent(in) :: b, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, l, b, k)

term(0) = -term(0) 


    eom_cc3_13_trans_aibibkal = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibibkal = eom_cc3_13_trans_aibibkal + term(s)
    end do

    end function eom_cc3_13_trans_aibibkal
    function eom_cc3_13_trans_aibjbkai(b, j, k) 
    double precision :: eom_cc3_13_trans_aibjbkai   
    integer, intent(in) :: b, j, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, k, b, j)

term(0) = term(0) * 2.0d+0 


    eom_cc3_13_trans_aibjbkai = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibjbkai = eom_cc3_13_trans_aibjbkai + term(s)
    end do

    end function eom_cc3_13_trans_aibjbkai
    function eom_cc3_13_trans_aibjbial(b, j, l) 
    double precision :: eom_cc3_13_trans_aibjbial   
    integer, intent(in) :: b, j, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, l, b, j)

term(0) = -term(0) 


    eom_cc3_13_trans_aibjbial = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibjbial = eom_cc3_13_trans_aibjbial + term(s)
    end do

    end function eom_cc3_13_trans_aibjbial
    function eom_cc3_13_trans_aiaickcl(c, k, l) 
    double precision :: eom_cc3_13_trans_aiaickcl   
    integer, intent(in) :: c, k, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, l, c, k)

term(0) = term(0) * 2.0d+0 


    eom_cc3_13_trans_aiaickcl = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiaickcl = eom_cc3_13_trans_aiaickcl + term(s)
    end do

    end function eom_cc3_13_trans_aiaickcl
    function eom_cc3_13_trans_aiajckci(j, c, k) 
    double precision :: eom_cc3_13_trans_aiajckci   
    integer, intent(in) :: j, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, k, c, j)

term(0) = -term(0) 


    eom_cc3_13_trans_aiajckci = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiajckci = eom_cc3_13_trans_aiajckci + term(s)
    end do

    end function eom_cc3_13_trans_aiajckci
    function eom_cc3_13_trans_aiajcicl(j, c, l) 
    double precision :: eom_cc3_13_trans_aiajcicl   
    integer, intent(in) :: j, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, l, c, j)

term(0) = -term(0) 


    eom_cc3_13_trans_aiajcicl = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiajcicl = eom_cc3_13_trans_aiajcicl + term(s)
    end do

    end function eom_cc3_13_trans_aiajcicl
    function eom_cc3_13_trans_aiaickdi(i, c, k, d) 
    double precision :: eom_cc3_13_trans_aiaickdi   
    integer, intent(in) :: i, c, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, k, c, i)
term(1) = term(1) + vovo(d, i, c, k)

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 


    eom_cc3_13_trans_aiaickdi = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aiaickdi = eom_cc3_13_trans_aiaickdi + term(s)
    end do

    end function eom_cc3_13_trans_aiaickdi
    function eom_cc3_13_trans_aiajcidj(j, c, d) 
    double precision :: eom_cc3_13_trans_aiajcidj   
    integer, intent(in) :: j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, j, c, j)

term(0) = -term(0) 


    eom_cc3_13_trans_aiajcidj = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiajcidj = eom_cc3_13_trans_aiajcidj + term(s)
    end do

    end function eom_cc3_13_trans_aiajcidj
    function eom_cc3_13_trans_aiaicidl(i, c, d, l) 
    double precision :: eom_cc3_13_trans_aiaicidl   
    integer, intent(in) :: i, c, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, l, c, i)
term(1) = term(1) + vovo(d, i, c, l)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    eom_cc3_13_trans_aiaicidl = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aiaicidl = eom_cc3_13_trans_aiaicidl + term(s)
    end do

    end function eom_cc3_13_trans_aiaicidl
    function eom_cc3_13_trans_aiaickdk(c, k, d) 
    double precision :: eom_cc3_13_trans_aiaickdk   
    integer, intent(in) :: c, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, k, c, k)

term(0) = term(0) * 2.0d+0 


    eom_cc3_13_trans_aiaickdk = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiaickdk = eom_cc3_13_trans_aiaickdk + term(s)
    end do

    end function eom_cc3_13_trans_aiaickdk
    function eom_cc3_13_trans_aiajcjdi(j, c, d) 
    double precision :: eom_cc3_13_trans_aiajcjdi   
    integer, intent(in) :: j, c, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, j, c, j)

term(0) = -term(0) 


    eom_cc3_13_trans_aiajcjdi = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiajcjdi = eom_cc3_13_trans_aiajcjdi + term(s)
    end do

    end function eom_cc3_13_trans_aiajcjdi
    function eom_cc3_13_trans_aiajcidi(i, j, c, d) 
    double precision :: eom_cc3_13_trans_aiajcidi   
    integer, intent(in) :: i, j, c, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, i, c, j)
term(1) = term(1) + vovo(d, j, c, i)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_13_trans_aiajcidi = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aiajcidi = eom_cc3_13_trans_aiajcidi + term(s)
    end do

    end function eom_cc3_13_trans_aiajcidi
    function eom_cc3_13_trans_aibiakal(a, b, k, l) 
    double precision :: eom_cc3_13_trans_aibiakal   
    integer, intent(in) :: a, b, k, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, l, a, k)
term(1) = term(1) + vovo(b, k, a, l)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_13_trans_aibiakal = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibiakal = eom_cc3_13_trans_aibiakal + term(s)
    end do

    end function eom_cc3_13_trans_aibiakal
    function eom_cc3_13_trans_aibjakai(a, b, j, k) 
    double precision :: eom_cc3_13_trans_aibjakai   
    integer, intent(in) :: a, b, j, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, j, a, k)
term(1) = term(1) + vovo(b, k, a, j)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    eom_cc3_13_trans_aibjakai = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibjakai = eom_cc3_13_trans_aibjakai + term(s)
    end do

    end function eom_cc3_13_trans_aibjakai
    function eom_cc3_13_trans_aibjaial(a, b, j, l) 
    double precision :: eom_cc3_13_trans_aibjaial   
    integer, intent(in) :: a, b, j, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, j, a, l)
term(1) = term(1) + vovo(b, l, a, j)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    eom_cc3_13_trans_aibjaial = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibjaial = eom_cc3_13_trans_aibjaial + term(s)
    end do

    end function eom_cc3_13_trans_aibjaial
    function eom_cc3_13_trans_aibiakdi(i, b, k, d) 
    double precision :: eom_cc3_13_trans_aibiakdi   
    integer, intent(in) :: i, b, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, k, b, i)
term(1) = term(1) + vovo(d, i, b, k)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_13_trans_aibiakdi = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibiakdi = eom_cc3_13_trans_aibiakdi + term(s)
    end do

    end function eom_cc3_13_trans_aibiakdi
    function eom_cc3_13_trans_aibjaidj(b, j, d) 
    double precision :: eom_cc3_13_trans_aibjaidj   
    integer, intent(in) :: b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, j, b, j)

term(0) = term(0) * 2.0d+0 


    eom_cc3_13_trans_aibjaidj = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibjaidj = eom_cc3_13_trans_aibjaidj + term(s)
    end do

    end function eom_cc3_13_trans_aibjaidj
    function eom_cc3_13_trans_aibiaidl(i, b, d, l) 
    double precision :: eom_cc3_13_trans_aibiaidl   
    integer, intent(in) :: i, b, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, l, b, i)
term(1) = term(1) + vovo(d, i, b, l)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    eom_cc3_13_trans_aibiaidl = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibiaidl = eom_cc3_13_trans_aibiaidl + term(s)
    end do

    end function eom_cc3_13_trans_aibiaidl
    function eom_cc3_13_trans_aibiakdk(b, k, d) 
    double precision :: eom_cc3_13_trans_aibiakdk   
    integer, intent(in) :: b, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, k, b, k)

term(0) = -term(0) 


    eom_cc3_13_trans_aibiakdk = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibiakdk = eom_cc3_13_trans_aibiakdk + term(s)
    end do

    end function eom_cc3_13_trans_aibiakdk
    function eom_cc3_13_trans_aibjajdi(b, j, d) 
    double precision :: eom_cc3_13_trans_aibjajdi   
    integer, intent(in) :: b, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, j, b, j)

term(0) = -term(0) 


    eom_cc3_13_trans_aibjajdi = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibjajdi = eom_cc3_13_trans_aibjajdi + term(s)
    end do

    end function eom_cc3_13_trans_aibjajdi
    function eom_cc3_13_trans_aibjaidi(i, b, j, d) 
    double precision :: eom_cc3_13_trans_aibjaidi   
    integer, intent(in) :: i, b, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, i, b, j)
term(1) = term(1) + vovo(d, j, b, i)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    eom_cc3_13_trans_aibjaidi = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibjaidi = eom_cc3_13_trans_aibjaidi + term(s)
    end do

    end function eom_cc3_13_trans_aibjaidi
    function eom_cc3_13_trans_aibickai(i, b, c, k) 
    double precision :: eom_cc3_13_trans_aibickai   
    integer, intent(in) :: i, b, c, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, k, b, i)
term(1) = term(1) + vovo(c, i, b, k)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    eom_cc3_13_trans_aibickai = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibickai = eom_cc3_13_trans_aibickai + term(s)
    end do

    end function eom_cc3_13_trans_aibickai
    function eom_cc3_13_trans_aibjciaj(b, j, c) 
    double precision :: eom_cc3_13_trans_aibjciaj   
    integer, intent(in) :: b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, j, b, j)

term(0) = -term(0) 


    eom_cc3_13_trans_aibjciaj = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibjciaj = eom_cc3_13_trans_aibjciaj + term(s)
    end do

    end function eom_cc3_13_trans_aibjciaj
    function eom_cc3_13_trans_aibicial(i, b, c, l) 
    double precision :: eom_cc3_13_trans_aibicial   
    integer, intent(in) :: i, b, c, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, l, b, i)
term(1) = term(1) + vovo(c, i, b, l)

term(0) = -term(0) 
term(1) = -term(1) 


    eom_cc3_13_trans_aibicial = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibicial = eom_cc3_13_trans_aibicial + term(s)
    end do

    end function eom_cc3_13_trans_aibicial
    function eom_cc3_13_trans_aibickak(b, c, k) 
    double precision :: eom_cc3_13_trans_aibickak   
    integer, intent(in) :: b, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, k, b, k)

term(0) = -term(0) 


    eom_cc3_13_trans_aibickak = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibickak = eom_cc3_13_trans_aibickak + term(s)
    end do

    end function eom_cc3_13_trans_aibickak
    function eom_cc3_13_trans_aibjcjai(b, j, c) 
    double precision :: eom_cc3_13_trans_aibjcjai   
    integer, intent(in) :: b, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, j, b, j)

term(0) = term(0) * 2.0d+0 


    eom_cc3_13_trans_aibjcjai = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibjcjai = eom_cc3_13_trans_aibjcjai + term(s)
    end do

    end function eom_cc3_13_trans_aibjcjai
    function eom_cc3_13_trans_aibjciai(i, b, j, c) 
    double precision :: eom_cc3_13_trans_aibjciai   
    integer, intent(in) :: i, b, j, c 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, i, b, j)
term(1) = term(1) + vovo(c, j, b, i)

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 


    eom_cc3_13_trans_aibjciai = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibjciai = eom_cc3_13_trans_aibjciai + term(s)
    end do

    end function eom_cc3_13_trans_aibjciai
    function eom_cc3_13_trans_aiaiakdi(a, i, k, d) 
    double precision :: eom_cc3_13_trans_aiaiakdi   
    integer, intent(in) :: a, i, k, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, k, a, i)
term(1) = term(1) + vovo(d, i, a, k)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_trans_aiaiakdi = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aiaiakdi = eom_cc3_13_trans_aiaiakdi + term(s)
    end do

    end function eom_cc3_13_trans_aiaiakdi
    function eom_cc3_13_trans_aiajaidj(a, j, d) 
    double precision :: eom_cc3_13_trans_aiajaidj   
    integer, intent(in) :: a, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, j, a, j)



    eom_cc3_13_trans_aiajaidj = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiajaidj = eom_cc3_13_trans_aiajaidj + term(s)
    end do

    end function eom_cc3_13_trans_aiajaidj
    function eom_cc3_13_trans_aiaiaidl(a, i, d, l) 
    double precision :: eom_cc3_13_trans_aiaiaidl   
    integer, intent(in) :: a, i, d, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, l, a, i)
term(1) = term(1) + vovo(d, i, a, l)

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_trans_aiaiaidl = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aiaiaidl = eom_cc3_13_trans_aiaiaidl + term(s)
    end do

    end function eom_cc3_13_trans_aiaiaidl
    function eom_cc3_13_trans_aiaiakdk(a, k, d) 
    double precision :: eom_cc3_13_trans_aiaiakdk   
    integer, intent(in) :: a, k, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, k, a, k)



    eom_cc3_13_trans_aiaiakdk = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiaiakdk = eom_cc3_13_trans_aiaiakdk + term(s)
    end do

    end function eom_cc3_13_trans_aiaiakdk
    function eom_cc3_13_trans_aiajajdi(a, j, d) 
    double precision :: eom_cc3_13_trans_aiajajdi   
    integer, intent(in) :: a, j, d 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, j, a, j)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_trans_aiajajdi = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiajajdi = eom_cc3_13_trans_aiajajdi + term(s)
    end do

    end function eom_cc3_13_trans_aiajajdi
    function eom_cc3_13_trans_aiajaidi(a, i, j, d) 
    double precision :: eom_cc3_13_trans_aiajaidi   
    integer, intent(in) :: a, i, j, d 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(d, i, a, j)
term(1) = term(1) + vovo(d, j, a, i)

term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_trans_aiajaidi = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aiajaidi = eom_cc3_13_trans_aiajaidi + term(s)
    end do

    end function eom_cc3_13_trans_aiajaidi
    function eom_cc3_13_trans_aibibkai(i, b, k) 
    double precision :: eom_cc3_13_trans_aibibkai   
    integer, intent(in) :: i, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, k, b, i)



    eom_cc3_13_trans_aibibkai = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibibkai = eom_cc3_13_trans_aibibkai + term(s)
    end do

    end function eom_cc3_13_trans_aibibkai
    function eom_cc3_13_trans_aibjbiaj(b, j) 
    double precision :: eom_cc3_13_trans_aibjbiaj   
    integer, intent(in) :: b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, j, b, j)

term(0) = -term(0) 


    eom_cc3_13_trans_aibjbiaj = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibjbiaj = eom_cc3_13_trans_aibjbiaj + term(s)
    end do

    end function eom_cc3_13_trans_aibjbiaj
    function eom_cc3_13_trans_aibibial(i, b, l) 
    double precision :: eom_cc3_13_trans_aibibial   
    integer, intent(in) :: i, b, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, l, b, i)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_trans_aibibial = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibibial = eom_cc3_13_trans_aibibial + term(s)
    end do

    end function eom_cc3_13_trans_aibibial
    function eom_cc3_13_trans_aibibkak(b, k) 
    double precision :: eom_cc3_13_trans_aibibkak   
    integer, intent(in) :: b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, k, b, k)

term(0) = -term(0) 


    eom_cc3_13_trans_aibibkak = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibibkak = eom_cc3_13_trans_aibibkak + term(s)
    end do

    end function eom_cc3_13_trans_aibibkak
    function eom_cc3_13_trans_aibjbjai(b, j) 
    double precision :: eom_cc3_13_trans_aibjbjai   
    integer, intent(in) :: b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, j, b, j)

term(0) = term(0) * 2.0d+0 


    eom_cc3_13_trans_aibjbjai = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibjbjai = eom_cc3_13_trans_aibjbjai + term(s)
    end do

    end function eom_cc3_13_trans_aibjbjai
    function eom_cc3_13_trans_aibjbiai(i, b, j) 
    double precision :: eom_cc3_13_trans_aibjbiai   
    integer, intent(in) :: i, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, j, b, i)



    eom_cc3_13_trans_aibjbiai = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibjbiai = eom_cc3_13_trans_aibjbiai + term(s)
    end do

    end function eom_cc3_13_trans_aibjbiai
    function eom_cc3_13_trans_aiaickci(i, c, k) 
    double precision :: eom_cc3_13_trans_aiaickci   
    integer, intent(in) :: i, c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, k, c, i)



    eom_cc3_13_trans_aiaickci = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiaickci = eom_cc3_13_trans_aiaickci + term(s)
    end do

    end function eom_cc3_13_trans_aiaickci
    function eom_cc3_13_trans_aiajcicj(j, c) 
    double precision :: eom_cc3_13_trans_aiajcicj   
    integer, intent(in) :: j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, j, c, j)

term(0) = -term(0) 


    eom_cc3_13_trans_aiajcicj = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiajcicj = eom_cc3_13_trans_aiajcicj + term(s)
    end do

    end function eom_cc3_13_trans_aiajcicj
    function eom_cc3_13_trans_aiaicicl(i, c, l) 
    double precision :: eom_cc3_13_trans_aiaicicl   
    integer, intent(in) :: i, c, l 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, l, c, i)



    eom_cc3_13_trans_aiaicicl = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiaicicl = eom_cc3_13_trans_aiaicicl + term(s)
    end do

    end function eom_cc3_13_trans_aiaicicl
    function eom_cc3_13_trans_aiaickck(c, k) 
    double precision :: eom_cc3_13_trans_aiaickck   
    integer, intent(in) :: c, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, k, c, k)

term(0) = term(0) * 2.0d+0 


    eom_cc3_13_trans_aiaickck = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiaickck = eom_cc3_13_trans_aiaickck + term(s)
    end do

    end function eom_cc3_13_trans_aiaickck
    function eom_cc3_13_trans_aiajcjci(j, c) 
    double precision :: eom_cc3_13_trans_aiajcjci   
    integer, intent(in) :: j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, j, c, j)

term(0) = -term(0) 


    eom_cc3_13_trans_aiajcjci = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiajcjci = eom_cc3_13_trans_aiajcjci + term(s)
    end do

    end function eom_cc3_13_trans_aiajcjci
    function eom_cc3_13_trans_aiajcici(i, j, c) 
    double precision :: eom_cc3_13_trans_aiajcici   
    integer, intent(in) :: i, j, c 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(c, j, c, i)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_trans_aiajcici = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aiajcici = eom_cc3_13_trans_aiajcici + term(s)
    end do

    end function eom_cc3_13_trans_aiajcici
    function eom_cc3_13_trans_aibiakai(a, i, b, k) 
    double precision :: eom_cc3_13_trans_aibiakai   
    integer, intent(in) :: a, i, b, k 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, i, a, k)
term(1) = term(1) + vovo(b, k, a, i)

term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_trans_aibiakai = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibiakai = eom_cc3_13_trans_aibiakai + term(s)
    end do

    end function eom_cc3_13_trans_aibiakai
    function eom_cc3_13_trans_aibjaiaj(a, b, j) 
    double precision :: eom_cc3_13_trans_aibjaiaj   
    integer, intent(in) :: a, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, j, a, j)



    eom_cc3_13_trans_aibjaiaj = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibjaiaj = eom_cc3_13_trans_aibjaiaj + term(s)
    end do

    end function eom_cc3_13_trans_aibjaiaj
    function eom_cc3_13_trans_aibiaial(a, i, b, l) 
    double precision :: eom_cc3_13_trans_aibiaial   
    integer, intent(in) :: a, i, b, l 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, i, a, l)
term(1) = term(1) + vovo(b, l, a, i)

term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_trans_aibiaial = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibiaial = eom_cc3_13_trans_aibiaial + term(s)
    end do

    end function eom_cc3_13_trans_aibiaial
    function eom_cc3_13_trans_aibiakak(a, b, k) 
    double precision :: eom_cc3_13_trans_aibiakak   
    integer, intent(in) :: a, b, k 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, k, a, k)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_trans_aibiakak = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibiakak = eom_cc3_13_trans_aibiakak + term(s)
    end do

    end function eom_cc3_13_trans_aibiakak
    function eom_cc3_13_trans_aibjajai(a, b, j) 
    double precision :: eom_cc3_13_trans_aibjajai   
    integer, intent(in) :: a, b, j 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, j, a, j)



    eom_cc3_13_trans_aibjajai = 0.d+0
    do s = 0, 0
    eom_cc3_13_trans_aibjajai = eom_cc3_13_trans_aibjajai + term(s)
    end do

    end function eom_cc3_13_trans_aibjajai
    function eom_cc3_13_trans_aibjaiai(a, i, b, j) 
    double precision :: eom_cc3_13_trans_aibjaiai   
    integer, intent(in) :: a, i, b, j 
    integer :: s  
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + vovo(b, j, a, i)
term(1) = term(1) + vovo(b, i, a, j)

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_trans_aibjaiai = 0.d+0
    do s = 0, 1
    eom_cc3_13_trans_aibjaiai = eom_cc3_13_trans_aibjaiai + term(s)
    end do

    end function eom_cc3_13_trans_aibjaiai
    end module eom_cc3_13_trans
    
