module eom_cc3_13_triplet_trans

    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none
    !               
    ! File generated automatically on 2017-01-20 18:02:30  
    !  
    contains
    
    function eom_cc3_13_triplet_trans_aiajckdi(j, c, k, d) 
    real(F64) :: eom_cc3_13_triplet_trans_aiajckdi   
    integer, intent(in) :: j, c, k, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, d, j, c)
term(1) = term(1) + tovov(k, c, j, d)

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aiajckdi = 0.d+0
    do s = 0, 1
    eom_cc3_13_triplet_trans_aiajckdi = eom_cc3_13_triplet_trans_aiajckdi + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiajckdi
    function eom_cc3_13_triplet_trans_aiajcidl(j, c, d, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aiajcidl   
    integer, intent(in) :: j, c, d, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, d, j, c)
term(1) = term(1) + tovov(l, c, j, d)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aiajcidl = 0.d+0
    do s = 0, 1
    eom_cc3_13_triplet_trans_aiajcidl = eom_cc3_13_triplet_trans_aiajcidl + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiajcidl
    function eom_cc3_13_triplet_trans_aibiakdl(b, k, d, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aibiakdl   
    integer, intent(in) :: b, k, d, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, d, k, b)
term(1) = term(1) + tovov(l, b, k, d)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aibiakdl = 0.d+0
    do s = 0, 1
    eom_cc3_13_triplet_trans_aibiakdl = eom_cc3_13_triplet_trans_aibiakdl + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibiakdl
    function eom_cc3_13_triplet_trans_aibjakdi(b, j, k, d) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjakdi   
    integer, intent(in) :: b, j, k, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, d, j, b)
term(1) = term(1) + tovov(k, b, j, d)

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aibjakdi = 0.d+0
    do s = 0, 1
    eom_cc3_13_triplet_trans_aibjakdi = eom_cc3_13_triplet_trans_aibjakdi + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjakdi
    function eom_cc3_13_triplet_trans_aibjaidl(b, j, d, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjaidl   
    integer, intent(in) :: b, j, d, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, d, j, b)
term(1) = term(1) + tovov(l, b, j, d)

term(0) = term(0) * (4.0d+0) 
term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aibjaidl = 0.d+0
    do s = 0, 1
    eom_cc3_13_triplet_trans_aibjaidl = eom_cc3_13_triplet_trans_aibjaidl + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjaidl
    function eom_cc3_13_triplet_trans_aibickal(b, c, k, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aibickal   
    integer, intent(in) :: b, c, k, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, c, k, b)
term(1) = term(1) + tovov(l, b, k, c)

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aibickal = 0.d+0
    do s = 0, 1
    eom_cc3_13_triplet_trans_aibickal = eom_cc3_13_triplet_trans_aibickal + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibickal
    function eom_cc3_13_triplet_trans_aibjckai(b, j, c, k) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjckai   
    integer, intent(in) :: b, j, c, k 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, c, j, b)
term(1) = term(1) + tovov(k, b, j, c)

term(0) = term(0) * (4.0d+0) 
term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aibjckai = 0.d+0
    do s = 0, 1
    eom_cc3_13_triplet_trans_aibjckai = eom_cc3_13_triplet_trans_aibjckai + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjckai
    function eom_cc3_13_triplet_trans_aibjcial(b, j, c, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjcial   
    integer, intent(in) :: b, j, c, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, c, j, b)
term(1) = term(1) + tovov(l, b, j, c)

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aibjcial = 0.d+0
    do s = 0, 1
    eom_cc3_13_triplet_trans_aibjcial = eom_cc3_13_triplet_trans_aibjcial + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjcial
    function eom_cc3_13_triplet_trans_aiaiakdl(a, k, d, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aiaiakdl   
    integer, intent(in) :: a, k, d, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, d, k, a)
term(1) = term(1) + tovov(l, a, k, d)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aiaiakdl = 0.d+0
    do s = 0, 1
    eom_cc3_13_triplet_trans_aiaiakdl = eom_cc3_13_triplet_trans_aiaiakdl + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiaiakdl
    function eom_cc3_13_triplet_trans_aiajakdi(a, j, k, d) 
    real(F64) :: eom_cc3_13_triplet_trans_aiajakdi   
    integer, intent(in) :: a, j, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, d, j, a)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aiajakdi = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aiajakdi = eom_cc3_13_triplet_trans_aiajakdi + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiajakdi
    function eom_cc3_13_triplet_trans_aiajaidl(a, j, d, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aiajaidl   
    integer, intent(in) :: a, j, d, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, d, j, a)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aiajaidl = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aiajaidl = eom_cc3_13_triplet_trans_aiajaidl + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiajaidl
    function eom_cc3_13_triplet_trans_aibjbkai(b, j, k) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjbkai   
    integer, intent(in) :: b, j, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, b, j, b)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aibjbkai = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibjbkai = eom_cc3_13_triplet_trans_aibjbkai + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjbkai
    function eom_cc3_13_triplet_trans_aibjbial(b, j, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjbial   
    integer, intent(in) :: b, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, b, j, b)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aibjbial = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibjbial = eom_cc3_13_triplet_trans_aibjbial + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjbial
    function eom_cc3_13_triplet_trans_aiaickal(a, c, k, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aiaickal   
    integer, intent(in) :: a, c, k, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, c, k, a)
term(1) = term(1) + tovov(l, a, k, c)

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aiaickal = 0.d+0
    do s = 0, 1
    eom_cc3_13_triplet_trans_aiaickal = eom_cc3_13_triplet_trans_aiaickal + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiaickal
    function eom_cc3_13_triplet_trans_aiajckai(a, j, c, k) 
    real(F64) :: eom_cc3_13_triplet_trans_aiajckai   
    integer, intent(in) :: a, j, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, c, j, a)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aiajckai = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aiajckai = eom_cc3_13_triplet_trans_aiajckai + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiajckai
    function eom_cc3_13_triplet_trans_aiajcial(a, j, c, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aiajcial   
    integer, intent(in) :: a, j, c, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, c, j, a)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aiajcial = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aiajcial = eom_cc3_13_triplet_trans_aiajcial + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiajcial
    function eom_cc3_13_triplet_trans_aiaickdi(i, c, k, d) 
    real(F64) :: eom_cc3_13_triplet_trans_aiaickdi   
    integer, intent(in) :: i, c, k, d 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, d, i, c)
term(1) = term(1) + tovov(k, c, i, d)

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aiaickdi = 0.d+0
    do s = 0, 1
    eom_cc3_13_triplet_trans_aiaickdi = eom_cc3_13_triplet_trans_aiaickdi + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiaickdi
    function eom_cc3_13_triplet_trans_aiaicidl(i, c, d, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aiaicidl   
    integer, intent(in) :: i, c, d, l 
    integer :: s  
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, d, i, c)
term(1) = term(1) + tovov(l, c, i, d)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aiaicidl = 0.d+0
    do s = 0, 1
    eom_cc3_13_triplet_trans_aiaicidl = eom_cc3_13_triplet_trans_aiaicidl + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiaicidl
    function eom_cc3_13_triplet_trans_aibjakbi(b, j, k) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjakbi   
    integer, intent(in) :: b, j, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, b, j, b)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aibjakbi = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibjakbi = eom_cc3_13_triplet_trans_aibjakbi + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjakbi
    function eom_cc3_13_triplet_trans_aibjaibl(b, j, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjaibl   
    integer, intent(in) :: b, j, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, b, j, b)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aibjaibl = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibjaibl = eom_cc3_13_triplet_trans_aibjaibl + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjaibl
    function eom_cc3_13_triplet_trans_aibiakdi(i, b, k, d) 
    real(F64) :: eom_cc3_13_triplet_trans_aibiakdi   
    integer, intent(in) :: i, b, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, d, i, b)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aibiakdi = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibiakdi = eom_cc3_13_triplet_trans_aibiakdi + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibiakdi
    function eom_cc3_13_triplet_trans_aibjaidj(b, j, d) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjaidj   
    integer, intent(in) :: b, j, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(j, d, j, b)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aibjaidj = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibjaidj = eom_cc3_13_triplet_trans_aibjaidj + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjaidj
    function eom_cc3_13_triplet_trans_aibiaidl(i, b, d, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aibiaidl   
    integer, intent(in) :: i, b, d, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, d, i, b)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aibiaidl = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibiaidl = eom_cc3_13_triplet_trans_aibiaidl + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibiaidl
    function eom_cc3_13_triplet_trans_aibjajdi(b, j, d) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjajdi   
    integer, intent(in) :: b, j, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(j, d, j, b)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aibjajdi = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibjajdi = eom_cc3_13_triplet_trans_aibjajdi + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjajdi
    function eom_cc3_13_triplet_trans_aibickai(i, b, c, k) 
    real(F64) :: eom_cc3_13_triplet_trans_aibickai   
    integer, intent(in) :: i, b, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, c, i, b)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aibickai = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibickai = eom_cc3_13_triplet_trans_aibickai + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibickai
    function eom_cc3_13_triplet_trans_aibjciaj(b, j, c) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjciaj   
    integer, intent(in) :: b, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(j, c, j, b)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aibjciaj = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibjciaj = eom_cc3_13_triplet_trans_aibjciaj + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjciaj
    function eom_cc3_13_triplet_trans_aibicial(i, b, c, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aibicial   
    integer, intent(in) :: i, b, c, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, c, i, b)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aibicial = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibicial = eom_cc3_13_triplet_trans_aibicial + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibicial
    function eom_cc3_13_triplet_trans_aibjcjai(b, j, c) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjcjai   
    integer, intent(in) :: b, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(j, c, j, b)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aibjcjai = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibjcjai = eom_cc3_13_triplet_trans_aibjcjai + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjcjai
    function eom_cc3_13_triplet_trans_aiaiakdi(a, i, k, d) 
    real(F64) :: eom_cc3_13_triplet_trans_aiaiakdi   
    integer, intent(in) :: a, i, k, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, a, i, d)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aiaiakdi = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aiaiakdi = eom_cc3_13_triplet_trans_aiaiakdi + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiaiakdi
    function eom_cc3_13_triplet_trans_aiajaidj(a, j, d) 
    real(F64) :: eom_cc3_13_triplet_trans_aiajaidj   
    integer, intent(in) :: a, j, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(j, d, j, a)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aiajaidj = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aiajaidj = eom_cc3_13_triplet_trans_aiajaidj + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiajaidj
    function eom_cc3_13_triplet_trans_aiaiaidl(a, i, d, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aiaiaidl   
    integer, intent(in) :: a, i, d, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, a, i, d)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aiaiaidl = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aiaiaidl = eom_cc3_13_triplet_trans_aiaiaidl + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiaiaidl
    function eom_cc3_13_triplet_trans_aiajajdi(a, j, d) 
    real(F64) :: eom_cc3_13_triplet_trans_aiajajdi   
    integer, intent(in) :: a, j, d 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(j, d, j, a)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aiajajdi = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aiajajdi = eom_cc3_13_triplet_trans_aiajajdi + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiajajdi
    function eom_cc3_13_triplet_trans_aibibkai(i, b, k) 
    real(F64) :: eom_cc3_13_triplet_trans_aibibkai   
    integer, intent(in) :: i, b, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, b, i, b)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aibibkai = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibibkai = eom_cc3_13_triplet_trans_aibibkai + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibibkai
    function eom_cc3_13_triplet_trans_aibjbiaj(b, j) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjbiaj   
    integer, intent(in) :: b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(j, b, j, b)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aibjbiaj = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibjbiaj = eom_cc3_13_triplet_trans_aibjbiaj + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjbiaj
    function eom_cc3_13_triplet_trans_aibibial(i, b, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aibibial   
    integer, intent(in) :: i, b, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, b, i, b)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aibibial = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibibial = eom_cc3_13_triplet_trans_aibibial + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibibial
    function eom_cc3_13_triplet_trans_aibjbjai(b, j) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjbjai   
    integer, intent(in) :: b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(j, b, j, b)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aibjbjai = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibjbjai = eom_cc3_13_triplet_trans_aibjbjai + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjbjai
    function eom_cc3_13_triplet_trans_aiaickai(a, i, c, k) 
    real(F64) :: eom_cc3_13_triplet_trans_aiaickai   
    integer, intent(in) :: a, i, c, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, a, i, c)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aiaickai = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aiaickai = eom_cc3_13_triplet_trans_aiaickai + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiaickai
    function eom_cc3_13_triplet_trans_aiajciaj(a, j, c) 
    real(F64) :: eom_cc3_13_triplet_trans_aiajciaj   
    integer, intent(in) :: a, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(j, c, j, a)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aiajciaj = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aiajciaj = eom_cc3_13_triplet_trans_aiajciaj + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiajciaj
    function eom_cc3_13_triplet_trans_aiaicial(a, i, c, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aiaicial   
    integer, intent(in) :: a, i, c, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, a, i, c)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aiaicial = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aiaicial = eom_cc3_13_triplet_trans_aiaicial + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiaicial
    function eom_cc3_13_triplet_trans_aiajcjai(a, j, c) 
    real(F64) :: eom_cc3_13_triplet_trans_aiajcjai   
    integer, intent(in) :: a, j, c 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(j, c, j, a)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aiajcjai = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aiajcjai = eom_cc3_13_triplet_trans_aiajcjai + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aiajcjai
    function eom_cc3_13_triplet_trans_aibiakbi(i, b, k) 
    real(F64) :: eom_cc3_13_triplet_trans_aibiakbi   
    integer, intent(in) :: i, b, k 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(k, b, i, b)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aibiakbi = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibiakbi = eom_cc3_13_triplet_trans_aibiakbi + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibiakbi
    function eom_cc3_13_triplet_trans_aibjaibj(b, j) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjaibj   
    integer, intent(in) :: b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(j, b, j, b)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aibjaibj = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibjaibj = eom_cc3_13_triplet_trans_aibjaibj + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjaibj
    function eom_cc3_13_triplet_trans_aibiaibl(i, b, l) 
    real(F64) :: eom_cc3_13_triplet_trans_aibiaibl   
    integer, intent(in) :: i, b, l 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(l, b, i, b)

term(0) = term(0) * (2.0d+0) 


    eom_cc3_13_triplet_trans_aibiaibl = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibiaibl = eom_cc3_13_triplet_trans_aibiaibl + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibiaibl
    function eom_cc3_13_triplet_trans_aibjajbi(b, j) 
    real(F64) :: eom_cc3_13_triplet_trans_aibjajbi   
    integer, intent(in) :: b, j 
    integer :: s  
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + tovov(j, b, j, b)

term(0) = term(0) * (-2.0d+0) 


    eom_cc3_13_triplet_trans_aibjajbi = 0.d+0
    do s = 0, 0
    eom_cc3_13_triplet_trans_aibjajbi = eom_cc3_13_triplet_trans_aibjajbi + term(s)
    end do

    end function eom_cc3_13_triplet_trans_aibjajbi
    end module eom_cc3_13_triplet_trans
    