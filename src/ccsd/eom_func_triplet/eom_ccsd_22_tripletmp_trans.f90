module eom_ccsd_22_tripletmp_trans

    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none
    !               
    ! File generated automatically on 2018-12-06 14:24:56  
    !  
    contains
    
    function eom_ccsd_22_tripletmp_trans_aibjbkdl_aijkdl(t2, nocc, nactive, a, i, j, k, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbkdl_aijkdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, d, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, k, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbkdl_aijkdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbkdl_aijkdl = eom_ccsd_22_tripletmp_trans_aibjbkdl_aijkdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbkdl_aijkdl
    function eom_ccsd_22_tripletmp_trans_aibjckbl_aijckl(t2, nocc, nactive, a, i, j, c, k, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckbl_aijckl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, c, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckbl_aijckl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjckbl_aijckl = eom_ccsd_22_tripletmp_trans_aibjckbl_aijckl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckbl_aijckl
    function eom_ccsd_22_tripletmp_trans_aibjakdl_ibjkdl(t2, nocc, nactive, i, b, j, k, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakdl_ibjkdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, d, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, d)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakdl_ibjkdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjakdl_ibjkdl = eom_ccsd_22_tripletmp_trans_aibjakdl_ibjkdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakdl_ibjkdl
    function eom_ccsd_22_tripletmp_trans_aibjckal_ibjckl(t2, nocc, nactive, i, b, j, c, k, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckal_ibjckl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, c, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckal_ibjckl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjckal_ibjckl = eom_ccsd_22_tripletmp_trans_aibjckal_ibjckl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckal_ibjckl
    function eom_ccsd_22_tripletmp_trans_aibjckdj_aibckd(t2, nocc, nactive, a, i, b, c, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckdj_aibckd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, k, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckdj_aibckd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjckdj_aibckd = eom_ccsd_22_tripletmp_trans_aibjckdj_aibckd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckdj_aibckd
    function eom_ccsd_22_tripletmp_trans_aibjcjdl_aibcdl(t2, nocc, nactive, a, i, b, c, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjdl_aibcdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d, l 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, l, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjdl_aibcdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcjdl_aibcdl = eom_ccsd_22_tripletmp_trans_aibjcjdl_aibcdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjdl_aibcdl
    function eom_ccsd_22_tripletmp_trans_aibjckdi_abjckd(t2, nocc, nactive, a, b, j, c, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckdi_abjckd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, k, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckdi_abjckd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjckdi_abjckd = eom_ccsd_22_tripletmp_trans_aibjckdi_abjckd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckdi_abjckd
    function eom_ccsd_22_tripletmp_trans_aibjcidl_abjcdl(t2, nocc, nactive, a, b, j, c, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcidl_abjcdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d, l 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, l, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcidl_abjcdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcidl_abjcdl = eom_ccsd_22_tripletmp_trans_aibjcidl_abjcdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcidl_abjcdl
    function eom_ccsd_22_tripletmp_trans_aiajakdl_aijkdl(t2, nocc, nactive, a, i, j, k, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajakdl_aijkdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d, l 
    integer :: s ,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, d, k, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, k, d)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, d, k, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, k, d)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajakdl_aijkdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aiajakdl_aijkdl = eom_ccsd_22_tripletmp_trans_aiajakdl_aijkdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajakdl_aijkdl
    function eom_ccsd_22_tripletmp_trans_aibjbkdj_aikd(t2, nocc, nactive, a, i, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbkdj_aikd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, d)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, d, k, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, k, d)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbkdj_aikd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjbkdj_aikd = eom_ccsd_22_tripletmp_trans_aibjbkdj_aikd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbkdj_aikd
    function eom_ccsd_22_tripletmp_trans_aibjbkdj_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbkdj_aijkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, j, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, d, j, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbkdj_aijkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbkdj_aijkd = eom_ccsd_22_tripletmp_trans_aibjbkdj_aijkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbkdj_aijkd
    function eom_ccsd_22_tripletmp_trans_aibjbkdj_aibkd(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbkdj_aibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, b, k, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, k, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbkdj_aibkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbkdj_aibkd = eom_ccsd_22_tripletmp_trans_aibjbkdj_aibkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbkdj_aibkd
    function eom_ccsd_22_tripletmp_trans_aibibkdl_aikdl(t2, nocc, nactive, a, i, k, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibibkdl_aikdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(l, d, k, e)
term(1) = term(1) + t2(a,e,i,i) * tovov(l, e, k, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibibkdl_aikdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibibkdl_aikdl = eom_ccsd_22_tripletmp_trans_aibibkdl_aikdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibibkdl_aikdl
    function eom_ccsd_22_tripletmp_trans_aibjbjdl_aidl(t2, nocc, nactive, a, i, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbjdl_aidl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, d, l, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbjdl_aidl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjbjdl_aidl = eom_ccsd_22_tripletmp_trans_aibjbjdl_aidl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbjdl_aidl
    function eom_ccsd_22_tripletmp_trans_aibjbjdl_aijdl(t2, nocc, nactive, a, i, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbjdl_aijdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, d, j, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, j, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbjdl_aijdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbjdl_aijdl = eom_ccsd_22_tripletmp_trans_aibjbjdl_aijdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbjdl_aijdl
    function eom_ccsd_22_tripletmp_trans_aibjbjdl_aibdl(t2, nocc, nactive, a, i, b, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbjdl_aibdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, b, l, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, l, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbjdl_aibdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbjdl_aibdl = eom_ccsd_22_tripletmp_trans_aibjbjdl_aibdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbjdl_aibdl
    function eom_ccsd_22_tripletmp_trans_aibjbkdi_abjkd(t2, nocc, nactive, a, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbkdi_abjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k, d 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, k, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, k, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbkdi_abjkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbkdi_abjkd = eom_ccsd_22_tripletmp_trans_aibjbkdi_abjkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbkdi_abjkd
    function eom_ccsd_22_tripletmp_trans_aibjbkdi_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbkdi_aijkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, i, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, d, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbkdi_aijkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbkdi_aijkd = eom_ccsd_22_tripletmp_trans_aibjbkdi_aijkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbkdi_aijkd
    function eom_ccsd_22_tripletmp_trans_aibjbidl_abjdl(t2, nocc, nactive, a, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbidl_abjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d, l 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, l, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, l, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbidl_abjdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbidl_abjdl = eom_ccsd_22_tripletmp_trans_aibjbidl_abjdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbidl_abjdl
    function eom_ccsd_22_tripletmp_trans_aibjbidl_aijdl(t2, nocc, nactive, a, i, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbidl_aijdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, d, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, i, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbidl_aijdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbidl_aijdl = eom_ccsd_22_tripletmp_trans_aibjbidl_aijdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbidl_aijdl
    function eom_ccsd_22_tripletmp_trans_aiajckal_aijckl(t2, nocc, nactive, a, i, j, c, k, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajckal_aijckl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, c, k, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, k, c)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, c, k, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajckal_aijckl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aiajckal_aijckl = eom_ccsd_22_tripletmp_trans_aiajckal_aijckl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajckal_aijckl
    function eom_ccsd_22_tripletmp_trans_aiajckdj_aickd(t2, nocc, nactive, a, i, c, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajckdj_aickd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, k, d)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, d, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajckdj_aickd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aiajckdj_aickd = eom_ccsd_22_tripletmp_trans_aiajckdj_aickd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajckdj_aickd
    function eom_ccsd_22_tripletmp_trans_aiajcjdl_aicdl(t2, nocc, nactive, a, i, c, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajcjdl_aicdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, d, l 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, d, l, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajcjdl_aicdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aiajcjdl_aicdl = eom_ccsd_22_tripletmp_trans_aiajcjdl_aicdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajcjdl_aicdl
    function eom_ccsd_22_tripletmp_trans_aiajckdi_ajckd(t2, nocc, nactive, a, j, c, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajckdi_ajckd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, k, d)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajckdi_ajckd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aiajckdi_ajckd = eom_ccsd_22_tripletmp_trans_aiajckdi_ajckd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajckdi_ajckd
    function eom_ccsd_22_tripletmp_trans_aiajcidl_ajcdl(t2, nocc, nactive, a, j, c, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajcidl_ajcdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, d, l 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, l, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajcidl_ajcdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aiajcidl_ajcdl = eom_ccsd_22_tripletmp_trans_aiajcidl_ajcdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajcidl_ajcdl
    function eom_ccsd_22_tripletmp_trans_aibjakbl_ibjkl(t2, nocc, nactive, i, b, j, k, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakbl_ibjkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, b, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakbl_ibjkl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjakbl_ibjkl = eom_ccsd_22_tripletmp_trans_aibjakbl_ibjkl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakbl_ibjkl
    function eom_ccsd_22_tripletmp_trans_aibjakbl_aijkl(t2, nocc, nactive, a, i, j, k, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakbl_aijkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, a, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, k, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakbl_aijkl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjakbl_aijkl = eom_ccsd_22_tripletmp_trans_aibjakbl_aijkl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakbl_aijkl
    function eom_ccsd_22_tripletmp_trans_aibjckbj_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckbj_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, c, k, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckbj_aick = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjckbj_aick = eom_ccsd_22_tripletmp_trans_aibjckbj_aick + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckbj_aick
    function eom_ccsd_22_tripletmp_trans_aibjckbj_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckbj_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, j, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, c, j, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckbj_aijck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjckbj_aijck = eom_ccsd_22_tripletmp_trans_aibjckbj_aijck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckbj_aijck
    function eom_ccsd_22_tripletmp_trans_aibjckbj_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckbj_aibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, k, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckbj_aibck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjckbj_aibck = eom_ccsd_22_tripletmp_trans_aibjckbj_aibck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckbj_aibck
    function eom_ccsd_22_tripletmp_trans_aibickbl_aickl(t2, nocc, nactive, a, i, c, k, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibickbl_aickl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(l, c, k, e)
term(1) = term(1) + t2(a,e,i,i) * tovov(l, e, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibickbl_aickl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibickbl_aickl = eom_ccsd_22_tripletmp_trans_aibickbl_aickl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibickbl_aickl
    function eom_ccsd_22_tripletmp_trans_aibjcjbl_aicl(t2, nocc, nactive, a, i, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjbl_aicl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, c)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, c, l, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, l, c)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjbl_aicl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjcjbl_aicl = eom_ccsd_22_tripletmp_trans_aibjcjbl_aicl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjbl_aicl
    function eom_ccsd_22_tripletmp_trans_aibjcjbl_aijcl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjbl_aijcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, c, j, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, j, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjbl_aijcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcjbl_aijcl = eom_ccsd_22_tripletmp_trans_aibjcjbl_aijcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjbl_aijcl
    function eom_ccsd_22_tripletmp_trans_aibjcjbl_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjbl_aibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, l, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, l, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjbl_aibcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcjbl_aibcl = eom_ccsd_22_tripletmp_trans_aibjcjbl_aibcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjbl_aibcl
    function eom_ccsd_22_tripletmp_trans_aibjckbi_abjck(t2, nocc, nactive, a, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckbi_abjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, k, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckbi_abjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjckbi_abjck = eom_ccsd_22_tripletmp_trans_aibjckbi_abjck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckbi_abjck
    function eom_ccsd_22_tripletmp_trans_aibjckbi_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckbi_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, i, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, c, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckbi_aijck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjckbi_aijck = eom_ccsd_22_tripletmp_trans_aibjckbi_aijck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckbi_aijck
    function eom_ccsd_22_tripletmp_trans_aibjcibl_abjcl(t2, nocc, nactive, a, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcibl_abjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, l 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, l, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcibl_abjcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcibl_abjcl = eom_ccsd_22_tripletmp_trans_aibjcibl_abjcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcibl_abjcl
    function eom_ccsd_22_tripletmp_trans_aibjcibl_aijcl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcibl_aijcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, c, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcibl_aijcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcibl_aijcl = eom_ccsd_22_tripletmp_trans_aibjcibl_aijcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcibl_aijcl
    function eom_ccsd_22_tripletmp_trans_aibjakdj_ibjkd(t2, nocc, nactive, i, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakdj_ibjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, j, d)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, d, j, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakdj_ibjkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjakdj_ibjkd = eom_ccsd_22_tripletmp_trans_aibjakdj_ibjkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakdj_ibjkd
    function eom_ccsd_22_tripletmp_trans_aibjakdj_aibkd(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakdj_aibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, k, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, k, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakdj_aibkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjakdj_aibkd = eom_ccsd_22_tripletmp_trans_aibjakdj_aibkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakdj_aibkd
    function eom_ccsd_22_tripletmp_trans_aibiakdl_ibkdl(t2, nocc, nactive, i, b, k, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibiakdl_ibkdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k, d, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(l, d, k, e)
term(1) = term(1) + t2(b,e,i,i) * tovov(l, e, k, d)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibiakdl_ibkdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibiakdl_ibkdl = eom_ccsd_22_tripletmp_trans_aibiakdl_ibkdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibiakdl_ibkdl
    function eom_ccsd_22_tripletmp_trans_aibjajdl_ibjdl(t2, nocc, nactive, i, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajdl_ibjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, d, j, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, j, d)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajdl_ibjdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjajdl_ibjdl = eom_ccsd_22_tripletmp_trans_aibjajdl_ibjdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajdl_ibjdl
    function eom_ccsd_22_tripletmp_trans_aibjajdl_aibdl(t2, nocc, nactive, a, i, b, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajdl_aibdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, l, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, l, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajdl_aibdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjajdl_aibdl = eom_ccsd_22_tripletmp_trans_aibjajdl_aibdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajdl_aibdl
    function eom_ccsd_22_tripletmp_trans_aibjakdi_bjkd(t2, nocc, nactive, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakdi_bjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, d, k, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, k, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, k, d)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakdi_bjkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjakdi_bjkd = eom_ccsd_22_tripletmp_trans_aibjakdi_bjkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakdi_bjkd
    function eom_ccsd_22_tripletmp_trans_aibjakdi_ibjkd(t2, nocc, nactive, i, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakdi_ibjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, i, d)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, d, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakdi_ibjkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjakdi_ibjkd = eom_ccsd_22_tripletmp_trans_aibjakdi_ibjkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakdi_ibjkd
    function eom_ccsd_22_tripletmp_trans_aibjakdi_abjkd(t2, nocc, nactive, a, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakdi_abjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, k, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, k, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakdi_abjkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjakdi_abjkd = eom_ccsd_22_tripletmp_trans_aibjakdi_abjkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakdi_abjkd
    function eom_ccsd_22_tripletmp_trans_aibjaidl_bjdl(t2, nocc, nactive, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaidl_bjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, l, d)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, d, l, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, l, d)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaidl_bjdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjaidl_bjdl = eom_ccsd_22_tripletmp_trans_aibjaidl_bjdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaidl_bjdl
    function eom_ccsd_22_tripletmp_trans_aibjaidl_ibjdl(t2, nocc, nactive, i, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaidl_ibjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, d, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, i, d)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaidl_ibjdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjaidl_ibjdl = eom_ccsd_22_tripletmp_trans_aibjaidl_ibjdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaidl_ibjdl
    function eom_ccsd_22_tripletmp_trans_aibjaidl_abjdl(t2, nocc, nactive, a, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaidl_abjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, l, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, l, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaidl_abjdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjaidl_abjdl = eom_ccsd_22_tripletmp_trans_aibjaidl_abjdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaidl_abjdl
    function eom_ccsd_22_tripletmp_trans_aibjckaj_ibjck(t2, nocc, nactive, i, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckaj_ibjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, j, c)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, c, j, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckaj_ibjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjckaj_ibjck = eom_ccsd_22_tripletmp_trans_aibjckaj_ibjck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckaj_ibjck
    function eom_ccsd_22_tripletmp_trans_aibjckaj_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckaj_aibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, k, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckaj_aibck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjckaj_aibck = eom_ccsd_22_tripletmp_trans_aibjckaj_aibck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckaj_aibck
    function eom_ccsd_22_tripletmp_trans_aibickal_ibckl(t2, nocc, nactive, i, b, c, k, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibickal_ibckl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(l, c, k, e)
term(1) = term(1) + t2(b,e,i,i) * tovov(l, e, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibickal_ibckl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibickal_ibckl = eom_ccsd_22_tripletmp_trans_aibickal_ibckl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibickal_ibckl
    function eom_ccsd_22_tripletmp_trans_aibjcjal_ibjcl(t2, nocc, nactive, i, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjal_ibjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, c, j, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjal_ibjcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcjal_ibjcl = eom_ccsd_22_tripletmp_trans_aibjcjal_ibjcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjal_ibjcl
    function eom_ccsd_22_tripletmp_trans_aibjcjal_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjal_aibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, l, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, l, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjal_aibcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcjal_aibcl = eom_ccsd_22_tripletmp_trans_aibjcjal_aibcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjal_aibcl
    function eom_ccsd_22_tripletmp_trans_aibjckai_bjck(t2, nocc, nactive, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckai_bjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, c)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, c, k, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckai_bjck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjckai_bjck = eom_ccsd_22_tripletmp_trans_aibjckai_bjck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckai_bjck
    function eom_ccsd_22_tripletmp_trans_aibjckai_ibjck(t2, nocc, nactive, i, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckai_ibjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, i, c)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, c, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckai_ibjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjckai_ibjck = eom_ccsd_22_tripletmp_trans_aibjckai_ibjck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckai_ibjck
    function eom_ccsd_22_tripletmp_trans_aibjckai_abjck(t2, nocc, nactive, a, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjckai_abjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, k, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjckai_abjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjckai_abjck = eom_ccsd_22_tripletmp_trans_aibjckai_abjck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjckai_abjck
    function eom_ccsd_22_tripletmp_trans_aibjcial_bjcl(t2, nocc, nactive, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcial_bjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, l, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, c, l, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, l, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, l, c)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcial_bjcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjcial_bjcl = eom_ccsd_22_tripletmp_trans_aibjcial_bjcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcial_bjcl
    function eom_ccsd_22_tripletmp_trans_aibjcial_ibjcl(t2, nocc, nactive, i, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcial_ibjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, c, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, i, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcial_ibjcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcial_ibjcl = eom_ccsd_22_tripletmp_trans_aibjcial_ibjcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcial_ibjcl
    function eom_ccsd_22_tripletmp_trans_aibjcial_abjcl(t2, nocc, nactive, a, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcial_abjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, l, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcial_abjcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcial_abjcl = eom_ccsd_22_tripletmp_trans_aibjcial_abjcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcial_abjcl
    function eom_ccsd_22_tripletmp_trans_aibickdi_aibckd(t2, nocc, nactive, a, i, b, c, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibickdi_aibckd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, k, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, k, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, k, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibickdi_aibckd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibickdi_aibckd = eom_ccsd_22_tripletmp_trans_aibickdi_aibckd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibickdi_aibckd
    function eom_ccsd_22_tripletmp_trans_aibjcidj_abjcd(t2, nocc, nactive, a, b, j, c, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcidj_abjcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcidj_abjcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcidj_abjcd = eom_ccsd_22_tripletmp_trans_aibjcidj_abjcd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcidj_abjcd
    function eom_ccsd_22_tripletmp_trans_aibjcidj_aibcd(t2, nocc, nactive, a, i, b, c, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcidj_aibcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcidj_aibcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcidj_aibcd = eom_ccsd_22_tripletmp_trans_aibjcidj_aibcd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcidj_aibcd
    function eom_ccsd_22_tripletmp_trans_aibicidl_aibcdl(t2, nocc, nactive, a, i, b, c, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibicidl_aibcdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d, l 
    integer :: s ,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, l, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, l, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, l, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibicidl_aibcdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibicidl_aibcdl = eom_ccsd_22_tripletmp_trans_aibicidl_aibcdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibicidl_aibcdl
    function eom_ccsd_22_tripletmp_trans_aibjcjdi_abjcd(t2, nocc, nactive, a, b, j, c, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjdi_abjcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjdi_abjcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcjdi_abjcd = eom_ccsd_22_tripletmp_trans_aibjcjdi_abjcd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjdi_abjcd
    function eom_ccsd_22_tripletmp_trans_aibjcjdi_aibcd(t2, nocc, nactive, a, i, b, c, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjdi_aibcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjdi_aibcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcjdi_aibcd = eom_ccsd_22_tripletmp_trans_aibjcjdi_aibcd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjdi_aibcd
    function eom_ccsd_22_tripletmp_trans_aiajakdj_aikd(t2, nocc, nactive, a, i, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajakdj_aikd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,a,i,m) * tovov(m, a, k, d)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, d, k, a)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, k, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, k, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajakdj_aikd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aiajakdj_aikd = eom_ccsd_22_tripletmp_trans_aiajakdj_aikd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajakdj_aikd
    function eom_ccsd_22_tripletmp_trans_aiajakdj_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajakdj_aijkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, j, d)
term(1) = term(1) + t2(a,e,j,i) * tovov(k, d, j, e)
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, j, d)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, d, j, e)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajakdj_aijkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aiajakdj_aijkd = eom_ccsd_22_tripletmp_trans_aiajakdj_aijkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajakdj_aijkd
    function eom_ccsd_22_tripletmp_trans_aiajajdl_aidl(t2, nocc, nactive, a, i, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajajdl_aidl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, d)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,a,i,m) * tovov(m, a, l, d)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, d, l, a)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, l, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajajdl_aidl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aiajajdl_aidl = eom_ccsd_22_tripletmp_trans_aiajajdl_aidl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajajdl_aidl
    function eom_ccsd_22_tripletmp_trans_aiajajdl_aijdl(t2, nocc, nactive, a, i, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajajdl_aijdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, d, j, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, j, d)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, d, j, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, j, d)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajajdl_aijdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aiajajdl_aijdl = eom_ccsd_22_tripletmp_trans_aiajajdl_aijdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajajdl_aijdl
    function eom_ccsd_22_tripletmp_trans_aiajakdi_ajkd(t2, nocc, nactive, a, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajakdi_ajkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, k, d)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,a,j,m) * tovov(m, a, k, d)
term(2) = term(2) + t2(a,a,j,m) * tovov(m, d, k, a)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, d, k, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, k, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, k, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajakdi_ajkd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aiajakdi_ajkd = eom_ccsd_22_tripletmp_trans_aiajakdi_ajkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajakdi_ajkd
    function eom_ccsd_22_tripletmp_trans_aiajakdi_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajakdi_aijkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, i, d)
term(1) = term(1) + t2(a,e,j,i) * tovov(k, d, i, e)
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, i, d)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, d, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajakdi_aijkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aiajakdi_aijkd = eom_ccsd_22_tripletmp_trans_aiajakdi_aijkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajakdi_aijkd
    function eom_ccsd_22_tripletmp_trans_aiajaidl_ajdl(t2, nocc, nactive, a, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajaidl_ajdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, l, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,a,j,m) * tovov(m, a, l, d)
term(2) = term(2) + t2(a,a,j,m) * tovov(m, d, l, a)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, d, l, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, l, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajaidl_ajdl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aiajaidl_ajdl = eom_ccsd_22_tripletmp_trans_aiajaidl_ajdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajaidl_ajdl
    function eom_ccsd_22_tripletmp_trans_aiajaidl_aijdl(t2, nocc, nactive, a, i, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajaidl_aijdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, d, i, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, i, d)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, d, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, i, d)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajaidl_aijdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aiajaidl_aijdl = eom_ccsd_22_tripletmp_trans_aiajaidl_aijdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajaidl_aijdl
    function eom_ccsd_22_tripletmp_trans_aibibkdi_aikd(t2, nocc, nactive, a, i, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibibkdi_aikd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d 
    integer :: s ,m,e 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, d)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,i) * tovov(k, e, i, d)
term(2) = term(2) + t2(a,e,i,i) * tovov(k, d, i, e)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, k, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, k, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibibkdi_aikd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aibibkdi_aikd = eom_ccsd_22_tripletmp_trans_aibibkdi_aikd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibibkdi_aikd
    function eom_ccsd_22_tripletmp_trans_aibibkdi_aibkd(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibibkdi_aibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, k, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, k, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, k, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, k, b)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibibkdi_aibkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibibkdi_aibkd = eom_ccsd_22_tripletmp_trans_aibibkdi_aibkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibibkdi_aibkd
    function eom_ccsd_22_tripletmp_trans_aibjbidj_aid(t2, nocc, nactive, a, i, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbidj_aid 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, d)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, d, i, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbidj_aid = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjbidj_aid = eom_ccsd_22_tripletmp_trans_aibjbidj_aid + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbidj_aid
    function eom_ccsd_22_tripletmp_trans_aibjbidj_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbidj_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbidj_abjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbidj_abjd = eom_ccsd_22_tripletmp_trans_aibjbidj_abjd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbidj_abjd
    function eom_ccsd_22_tripletmp_trans_aibjbidj_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbidj_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, b, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbidj_aibd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbidj_aibd = eom_ccsd_22_tripletmp_trans_aibjbidj_aibd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbidj_aibd
    function eom_ccsd_22_tripletmp_trans_aibjbidj_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbidj_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, d, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, e, i, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbidj_aijd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbidj_aijd = eom_ccsd_22_tripletmp_trans_aibjbidj_aijd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbidj_aijd
    function eom_ccsd_22_tripletmp_trans_aibibidl_aidl(t2, nocc, nactive, a, i, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibibidl_aidl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d, l 
    integer :: s ,m,e 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,i) * tovov(l, d, i, e)
term(2) = term(2) + t2(a,e,i,i) * tovov(l, e, i, d)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, l, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibibidl_aidl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aibibidl_aidl = eom_ccsd_22_tripletmp_trans_aibibidl_aidl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibibidl_aidl
    function eom_ccsd_22_tripletmp_trans_aibibidl_aibdl(t2, nocc, nactive, a, i, b, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibibidl_aibdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, l, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, l, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, l, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, l, b)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibibidl_aibdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibibidl_aibdl = eom_ccsd_22_tripletmp_trans_aibibidl_aibdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibibidl_aibdl
    function eom_ccsd_22_tripletmp_trans_aibjbjdi_aid(t2, nocc, nactive, a, i, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbjdi_aid 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, d, i, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbjdi_aid = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjbjdi_aid = eom_ccsd_22_tripletmp_trans_aibjbjdi_aid + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbjdi_aid
    function eom_ccsd_22_tripletmp_trans_aibjbjdi_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbjdi_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbjdi_abjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbjdi_abjd = eom_ccsd_22_tripletmp_trans_aibjbjdi_abjd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbjdi_abjd
    function eom_ccsd_22_tripletmp_trans_aibjbjdi_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbjdi_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, b, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbjdi_aibd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbjdi_aibd = eom_ccsd_22_tripletmp_trans_aibjbjdi_aibd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbjdi_aibd
    function eom_ccsd_22_tripletmp_trans_aibjbjdi_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjbjdi_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, e, i, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, d, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjbjdi_aijd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjbjdi_aijd = eom_ccsd_22_tripletmp_trans_aibjbjdi_aijd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjbjdi_aijd
    function eom_ccsd_22_tripletmp_trans_aiajckaj_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajckaj_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, c)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,a,i,m) * tovov(m, c, k, a)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, c, k, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajckaj_aick = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aiajckaj_aick = eom_ccsd_22_tripletmp_trans_aiajckaj_aick + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajckaj_aick
    function eom_ccsd_22_tripletmp_trans_aiajckaj_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajckaj_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, j, c)
term(1) = term(1) + t2(a,e,j,i) * tovov(k, c, j, e)
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, j, c)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, c, j, e)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajckaj_aijck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aiajckaj_aijck = eom_ccsd_22_tripletmp_trans_aiajckaj_aijck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajckaj_aijck
    function eom_ccsd_22_tripletmp_trans_aiajcjal_aicl(t2, nocc, nactive, a, i, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajcjal_aicl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, c)


do m = 1, nocc 
term(1) = term(1) + t2(a,a,i,m) * tovov(m, c, l, a)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, l, c)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, c, l, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, l, c)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajcjal_aicl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aiajcjal_aicl = eom_ccsd_22_tripletmp_trans_aiajcjal_aicl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajcjal_aicl
    function eom_ccsd_22_tripletmp_trans_aiajcjal_aijcl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajcjal_aijcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, c, j, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, j, c)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, c, j, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajcjal_aijcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aiajcjal_aijcl = eom_ccsd_22_tripletmp_trans_aiajcjal_aijcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajcjal_aijcl
    function eom_ccsd_22_tripletmp_trans_aiajckai_ajck(t2, nocc, nactive, a, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajckai_ajck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, k, c)


do m = 1, nocc 
term(1) = term(1) + t2(a,a,j,m) * tovov(m, c, k, a)
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, k, c)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, c, k, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajckai_ajck = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aiajckai_ajck = eom_ccsd_22_tripletmp_trans_aiajckai_ajck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajckai_ajck
    function eom_ccsd_22_tripletmp_trans_aiajckai_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajckai_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, i, c)
term(1) = term(1) + t2(a,e,j,i) * tovov(k, c, i, e)
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, i, c)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, c, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajckai_aijck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aiajckai_aijck = eom_ccsd_22_tripletmp_trans_aiajckai_aijck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajckai_aijck
    function eom_ccsd_22_tripletmp_trans_aiajcial_ajcl(t2, nocc, nactive, a, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajcial_ajcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, l, c)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,a,j,m) * tovov(m, c, l, a)
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, l, c)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, c, l, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, l, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, l, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajcial_ajcl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aiajcial_ajcl = eom_ccsd_22_tripletmp_trans_aiajcial_ajcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajcial_ajcl
    function eom_ccsd_22_tripletmp_trans_aiajcial_aijcl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajcial_aijcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, c, i, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, i, c)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, c, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, i, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajcial_aijcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aiajcial_aijcl = eom_ccsd_22_tripletmp_trans_aiajcial_aijcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajcial_aijcl
    function eom_ccsd_22_tripletmp_trans_aiajcidj_ajcd(t2, nocc, nactive, a, j, c, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajcidj_ajcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, j, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajcidj_ajcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aiajcidj_ajcd = eom_ccsd_22_tripletmp_trans_aiajcidj_ajcd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajcidj_ajcd
    function eom_ccsd_22_tripletmp_trans_aiajcidj_aicd(t2, nocc, nactive, a, i, c, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajcidj_aicd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, d, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajcidj_aicd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aiajcidj_aicd = eom_ccsd_22_tripletmp_trans_aiajcidj_aicd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajcidj_aicd
    function eom_ccsd_22_tripletmp_trans_aibjakbj_aik(t2, nocc, nactive, a, i, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakbj_aik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, a)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, a, k, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakbj_aik = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjakbj_aik = eom_ccsd_22_tripletmp_trans_aibjakbj_aik + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakbj_aik
    function eom_ccsd_22_tripletmp_trans_aibjakbj_ibjk(t2, nocc, nactive, i, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakbj_ibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, j, b)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, b, j, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakbj_ibjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjakbj_ibjk = eom_ccsd_22_tripletmp_trans_aibjakbj_ibjk + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakbj_ibjk
    function eom_ccsd_22_tripletmp_trans_aibjakbj_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakbj_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, j, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, a, j, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakbj_aijk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjakbj_aijk = eom_ccsd_22_tripletmp_trans_aibjakbj_aijk + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakbj_aijk
    function eom_ccsd_22_tripletmp_trans_aibjakbj_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakbj_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, k, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, k, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakbj_aibk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjakbj_aibk = eom_ccsd_22_tripletmp_trans_aibjakbj_aibk + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakbj_aibk
    function eom_ccsd_22_tripletmp_trans_aibiakbl_ibkl(t2, nocc, nactive, i, b, k, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibiakbl_ibkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(l, b, k, e)
term(1) = term(1) + t2(b,e,i,i) * tovov(l, e, k, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibiakbl_ibkl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibiakbl_ibkl = eom_ccsd_22_tripletmp_trans_aibiakbl_ibkl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibiakbl_ibkl
    function eom_ccsd_22_tripletmp_trans_aibiakbl_aikl(t2, nocc, nactive, a, i, k, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibiakbl_aikl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(l, a, k, e)
term(1) = term(1) + t2(a,e,i,i) * tovov(l, e, k, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibiakbl_aikl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibiakbl_aikl = eom_ccsd_22_tripletmp_trans_aibiakbl_aikl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibiakbl_aikl
    function eom_ccsd_22_tripletmp_trans_aibjajbl_ail(t2, nocc, nactive, a, i, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajbl_ail 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, a)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, a, l, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajbl_ail = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjajbl_ail = eom_ccsd_22_tripletmp_trans_aibjajbl_ail + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajbl_ail
    function eom_ccsd_22_tripletmp_trans_aibjajbl_ibjl(t2, nocc, nactive, i, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajbl_ibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, b, j, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, j, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajbl_ibjl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjajbl_ibjl = eom_ccsd_22_tripletmp_trans_aibjajbl_ibjl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajbl_ibjl
    function eom_ccsd_22_tripletmp_trans_aibjajbl_aijl(t2, nocc, nactive, a, i, j, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajbl_aijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, a, j, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, j, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajbl_aijl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjajbl_aijl = eom_ccsd_22_tripletmp_trans_aibjajbl_aijl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajbl_aijl
    function eom_ccsd_22_tripletmp_trans_aibjajbl_aibl(t2, nocc, nactive, a, i, b, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajbl_aibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, l, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, l, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajbl_aibl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjajbl_aibl = eom_ccsd_22_tripletmp_trans_aibjajbl_aibl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajbl_aibl
    function eom_ccsd_22_tripletmp_trans_aibjakbi_bjk(t2, nocc, nactive, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakbi_bjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, b)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, b, k, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, k, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, k, b)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakbi_bjk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjakbi_bjk = eom_ccsd_22_tripletmp_trans_aibjakbi_bjk + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakbi_bjk
    function eom_ccsd_22_tripletmp_trans_aibjakbi_ibjk(t2, nocc, nactive, i, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakbi_ibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, i, b)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, b, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakbi_ibjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjakbi_ibjk = eom_ccsd_22_tripletmp_trans_aibjakbi_ibjk + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakbi_ibjk
    function eom_ccsd_22_tripletmp_trans_aibjakbi_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakbi_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, i, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, a, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakbi_aijk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjakbi_aijk = eom_ccsd_22_tripletmp_trans_aibjakbi_aijk + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakbi_aijk
    function eom_ccsd_22_tripletmp_trans_aibjakbi_abjk(t2, nocc, nactive, a, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjakbi_abjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, k, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, k, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjakbi_abjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjakbi_abjk = eom_ccsd_22_tripletmp_trans_aibjakbi_abjk + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjakbi_abjk
    function eom_ccsd_22_tripletmp_trans_aibjaibl_bjl(t2, nocc, nactive, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaibl_bjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, l, b)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, b, l, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, l, b)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, l, b)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaibl_bjl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjaibl_bjl = eom_ccsd_22_tripletmp_trans_aibjaibl_bjl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaibl_bjl
    function eom_ccsd_22_tripletmp_trans_aibjaibl_ibjl(t2, nocc, nactive, i, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaibl_ibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, b, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, i, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaibl_ibjl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjaibl_ibjl = eom_ccsd_22_tripletmp_trans_aibjaibl_ibjl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaibl_ibjl
    function eom_ccsd_22_tripletmp_trans_aibjaibl_aijl(t2, nocc, nactive, a, i, j, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaibl_aijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, a, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaibl_aijl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjaibl_aijl = eom_ccsd_22_tripletmp_trans_aibjaibl_aijl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaibl_aijl
    function eom_ccsd_22_tripletmp_trans_aibjaibl_abjl(t2, nocc, nactive, a, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaibl_abjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, l, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, l, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaibl_abjl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjaibl_abjl = eom_ccsd_22_tripletmp_trans_aibjaibl_abjl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaibl_abjl
    function eom_ccsd_22_tripletmp_trans_aibickbi_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibickbi_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,m,e 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,i) * tovov(k, e, i, c)
term(2) = term(2) + t2(a,e,i,i) * tovov(k, c, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, c, k, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibickbi_aick = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aibickbi_aick = eom_ccsd_22_tripletmp_trans_aibickbi_aick + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibickbi_aick
    function eom_ccsd_22_tripletmp_trans_aibickbi_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibickbi_aibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, k, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, k, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, k, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibickbi_aibck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibickbi_aibck = eom_ccsd_22_tripletmp_trans_aibickbi_aibck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibickbi_aibck
    function eom_ccsd_22_tripletmp_trans_aibjcibj_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcibj_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, c, i, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcibj_aic = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjcibj_aic = eom_ccsd_22_tripletmp_trans_aibjcibj_aic + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcibj_aic
    function eom_ccsd_22_tripletmp_trans_aibjcibj_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcibj_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcibj_abjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcibj_abjc = eom_ccsd_22_tripletmp_trans_aibjcibj_abjc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcibj_abjc
    function eom_ccsd_22_tripletmp_trans_aibjcibj_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcibj_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcibj_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcibj_aibc = eom_ccsd_22_tripletmp_trans_aibjcibj_aibc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcibj_aibc
    function eom_ccsd_22_tripletmp_trans_aibjcibj_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcibj_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, c, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, e, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcibj_aijc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcibj_aijc = eom_ccsd_22_tripletmp_trans_aibjcibj_aijc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcibj_aijc
    function eom_ccsd_22_tripletmp_trans_aibicibl_aicl(t2, nocc, nactive, a, i, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibicibl_aicl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, l 
    integer :: s ,m,e 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, c)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,i) * tovov(l, c, i, e)
term(2) = term(2) + t2(a,e,i,i) * tovov(l, e, i, c)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, c, l, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, l, c)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibicibl_aicl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aibicibl_aicl = eom_ccsd_22_tripletmp_trans_aibicibl_aicl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibicibl_aicl
    function eom_ccsd_22_tripletmp_trans_aibicibl_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibicibl_aibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, l, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, l, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, l, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibicibl_aibcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibicibl_aibcl = eom_ccsd_22_tripletmp_trans_aibicibl_aibcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibicibl_aibcl
    function eom_ccsd_22_tripletmp_trans_aibjcjbi_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjbi_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, c)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, c, i, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjbi_aic = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjcjbi_aic = eom_ccsd_22_tripletmp_trans_aibjcjbi_aic + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjbi_aic
    function eom_ccsd_22_tripletmp_trans_aibjcjbi_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjbi_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjbi_abjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcjbi_abjc = eom_ccsd_22_tripletmp_trans_aibjcjbi_abjc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjbi_abjc
    function eom_ccsd_22_tripletmp_trans_aibjcjbi_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjbi_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjbi_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcjbi_aibc = eom_ccsd_22_tripletmp_trans_aibjcjbi_aibc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjbi_aibc
    function eom_ccsd_22_tripletmp_trans_aibjcjbi_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjbi_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, e, i, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, c, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjbi_aijc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcjbi_aijc = eom_ccsd_22_tripletmp_trans_aibjcjbi_aijc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjbi_aijc
    function eom_ccsd_22_tripletmp_trans_aibiakdi_ibkd(t2, nocc, nactive, i, b, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibiakdi_ibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,i) * tovov(k, e, i, d)
term(2) = term(2) + t2(b,e,i,i) * tovov(k, d, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, d, k, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, k, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibiakdi_ibkd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aibiakdi_ibkd = eom_ccsd_22_tripletmp_trans_aibiakdi_ibkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibiakdi_ibkd
    function eom_ccsd_22_tripletmp_trans_aibiakdi_aibkd(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibiakdi_aibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, k, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, k, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, k, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, k, a)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibiakdi_aibkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibiakdi_aibkd = eom_ccsd_22_tripletmp_trans_aibiakdi_aibkd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibiakdi_aibkd
    function eom_ccsd_22_tripletmp_trans_aibjaidj_bjd(t2, nocc, nactive, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaidj_bjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, j, d)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, d, j, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, j, d)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaidj_bjd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjaidj_bjd = eom_ccsd_22_tripletmp_trans_aibjaidj_bjd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaidj_bjd
    function eom_ccsd_22_tripletmp_trans_aibjaidj_ibjd(t2, nocc, nactive, i, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaidj_ibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, d, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, i, d)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaidj_ibjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjaidj_ibjd = eom_ccsd_22_tripletmp_trans_aibjaidj_ibjd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaidj_ibjd
    function eom_ccsd_22_tripletmp_trans_aibjaidj_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaidj_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaidj_abjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjaidj_abjd = eom_ccsd_22_tripletmp_trans_aibjaidj_abjd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaidj_abjd
    function eom_ccsd_22_tripletmp_trans_aibjaidj_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaidj_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaidj_aibd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjaidj_aibd = eom_ccsd_22_tripletmp_trans_aibjaidj_aibd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaidj_aibd
    function eom_ccsd_22_tripletmp_trans_aibiaidl_ibdl(t2, nocc, nactive, i, b, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibiaidl_ibdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, d)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,i) * tovov(l, d, i, e)
term(2) = term(2) + t2(b,e,i,i) * tovov(l, e, i, d)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, d, l, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibiaidl_ibdl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aibiaidl_ibdl = eom_ccsd_22_tripletmp_trans_aibiaidl_ibdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibiaidl_ibdl
    function eom_ccsd_22_tripletmp_trans_aibiaidl_aibdl(t2, nocc, nactive, a, i, b, d, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibiaidl_aibdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, l, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, l, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, l, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, l, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibiaidl_aibdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibiaidl_aibdl = eom_ccsd_22_tripletmp_trans_aibiaidl_aibdl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibiaidl_aibdl
    function eom_ccsd_22_tripletmp_trans_aibjajdi_bjd(t2, nocc, nactive, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajdi_bjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, j, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, d, j, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, j, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajdi_bjd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjajdi_bjd = eom_ccsd_22_tripletmp_trans_aibjajdi_bjd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajdi_bjd
    function eom_ccsd_22_tripletmp_trans_aibjajdi_ibjd(t2, nocc, nactive, i, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajdi_ibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, e, i, d)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, d, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajdi_ibjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjajdi_ibjd = eom_ccsd_22_tripletmp_trans_aibjajdi_ibjd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajdi_ibjd
    function eom_ccsd_22_tripletmp_trans_aibjajdi_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajdi_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajdi_abjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjajdi_abjd = eom_ccsd_22_tripletmp_trans_aibjajdi_abjd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajdi_abjd
    function eom_ccsd_22_tripletmp_trans_aibjajdi_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajdi_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajdi_aibd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjajdi_aibd = eom_ccsd_22_tripletmp_trans_aibjajdi_aibd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajdi_aibd
    function eom_ccsd_22_tripletmp_trans_aibickai_ibck(t2, nocc, nactive, i, b, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibickai_ibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, c)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,i) * tovov(k, e, i, c)
term(2) = term(2) + t2(b,e,i,i) * tovov(k, c, i, e)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, c, k, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibickai_ibck = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aibickai_ibck = eom_ccsd_22_tripletmp_trans_aibickai_ibck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibickai_ibck
    function eom_ccsd_22_tripletmp_trans_aibickai_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibickai_aibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, k, a)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, k, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, k, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibickai_aibck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibickai_aibck = eom_ccsd_22_tripletmp_trans_aibickai_aibck + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibickai_aibck
    function eom_ccsd_22_tripletmp_trans_aibjciaj_bjc(t2, nocc, nactive, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjciaj_bjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, j, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, c, j, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, j, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjciaj_bjc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjciaj_bjc = eom_ccsd_22_tripletmp_trans_aibjciaj_bjc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjciaj_bjc
    function eom_ccsd_22_tripletmp_trans_aibjciaj_ibjc(t2, nocc, nactive, i, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjciaj_ibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, c, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, i, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjciaj_ibjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjciaj_ibjc = eom_ccsd_22_tripletmp_trans_aibjciaj_ibjc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjciaj_ibjc
    function eom_ccsd_22_tripletmp_trans_aibjciaj_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjciaj_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, j, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjciaj_abjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjciaj_abjc = eom_ccsd_22_tripletmp_trans_aibjciaj_abjc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjciaj_abjc
    function eom_ccsd_22_tripletmp_trans_aibjciaj_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjciaj_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjciaj_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjciaj_aibc = eom_ccsd_22_tripletmp_trans_aibjciaj_aibc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjciaj_aibc
    function eom_ccsd_22_tripletmp_trans_aibicial_ibcl(t2, nocc, nactive, i, b, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibicial_ibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,i) * tovov(l, c, i, e)
term(2) = term(2) + t2(b,e,i,i) * tovov(l, e, i, c)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, c, l, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, l, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibicial_ibcl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aibicial_ibcl = eom_ccsd_22_tripletmp_trans_aibicial_ibcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibicial_ibcl
    function eom_ccsd_22_tripletmp_trans_aibicial_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibicial_aibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, a)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, l, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, l, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, l, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibicial_aibcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibicial_aibcl = eom_ccsd_22_tripletmp_trans_aibicial_aibcl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibicial_aibcl
    function eom_ccsd_22_tripletmp_trans_aibjcjai_bjc(t2, nocc, nactive, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjai_bjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, j, c)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, c, j, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, j, c)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjai_bjc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjcjai_bjc = eom_ccsd_22_tripletmp_trans_aibjcjai_bjc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjai_bjc
    function eom_ccsd_22_tripletmp_trans_aibjcjai_ibjc(t2, nocc, nactive, i, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjai_ibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, e, i, c)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, c, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjai_ibjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcjai_ibjc = eom_ccsd_22_tripletmp_trans_aibjcjai_ibjc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjai_ibjc
    function eom_ccsd_22_tripletmp_trans_aibjcjai_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjai_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjai_abjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcjai_abjc = eom_ccsd_22_tripletmp_trans_aibjcjai_abjc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjai_abjc
    function eom_ccsd_22_tripletmp_trans_aibjcjai_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjcjai_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjcjai_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjcjai_aibc = eom_ccsd_22_tripletmp_trans_aibjcjai_aibc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjcjai_aibc
    function eom_ccsd_22_tripletmp_trans_aiajaidj_ajd(t2, nocc, nactive, a, j, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajaidj_ajd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, j, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,a,j,m) * tovov(m, a, j, d)
term(2) = term(2) + t2(a,a,j,m) * tovov(m, d, j, a)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, d, j, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, j, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajaidj_ajd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aiajaidj_ajd = eom_ccsd_22_tripletmp_trans_aiajaidj_ajd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajaidj_ajd
    function eom_ccsd_22_tripletmp_trans_aiajaidj_aid(t2, nocc, nactive, a, i, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajaidj_aid 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,a,i,m) * tovov(m, a, i, d)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, d, i, a)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, i, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajaidj_aid = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aiajaidj_aid = eom_ccsd_22_tripletmp_trans_aiajaidj_aid + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajaidj_aid
    function eom_ccsd_22_tripletmp_trans_aiajaidj_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajaidj_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, d, i, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(j, e, i, d)
term(2) = term(2) + t2(a,e,i,j) * tovov(j, d, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, e, i, d)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajaidj_aijd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aiajaidj_aijd = eom_ccsd_22_tripletmp_trans_aiajaidj_aijd + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajaidj_aijd
    function eom_ccsd_22_tripletmp_trans_aiajciaj_ajc(t2, nocc, nactive, a, j, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajciaj_ajc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, j, c)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,a,j,m) * tovov(m, c, j, a)
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, c, j, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, j, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajciaj_ajc = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aiajciaj_ajc = eom_ccsd_22_tripletmp_trans_aiajciaj_ajc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajciaj_ajc
    function eom_ccsd_22_tripletmp_trans_aiajciaj_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajciaj_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, c)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,a,i,m) * tovov(m, c, i, a)
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, i, c)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, c, i, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajciaj_aic = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aiajciaj_aic = eom_ccsd_22_tripletmp_trans_aiajciaj_aic + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajciaj_aic
    function eom_ccsd_22_tripletmp_trans_aiajciaj_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aiajciaj_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, c, i, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(j, e, i, c)
term(2) = term(2) + t2(a,e,i,j) * tovov(j, c, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, e, i, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aiajciaj_aijc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aiajciaj_aijc = eom_ccsd_22_tripletmp_trans_aiajciaj_aijc + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aiajciaj_aijc
    function eom_ccsd_22_tripletmp_trans_aibiakbi_ibk(t2, nocc, nactive, i, b, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibiakbi_ibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, b)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,i) * tovov(k, e, i, b)
term(2) = term(2) + t2(b,e,i,i) * tovov(k, b, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, b, k, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, k, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, k, b)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibiakbi_ibk = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aibiakbi_ibk = eom_ccsd_22_tripletmp_trans_aibiakbi_ibk + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibiakbi_ibk
    function eom_ccsd_22_tripletmp_trans_aibiakbi_aik(t2, nocc, nactive, a, i, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibiakbi_aik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, a)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,i) * tovov(k, e, i, a)
term(2) = term(2) + t2(a,e,i,i) * tovov(k, a, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, a, k, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibiakbi_aik = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aibiakbi_aik = eom_ccsd_22_tripletmp_trans_aibiakbi_aik + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibiakbi_aik
    function eom_ccsd_22_tripletmp_trans_aibiakbi_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibiakbi_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, k, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, k, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, k, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, k, a)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibiakbi_aibk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibiakbi_aibk = eom_ccsd_22_tripletmp_trans_aibiakbi_aibk + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibiakbi_aibk
    function eom_ccsd_22_tripletmp_trans_aibjaibj_bj(t2, nocc, nactive, b, j) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaibj_bj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, j, b)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, b, j, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, j, b)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, j, b)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaibj_bj = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjaibj_bj = eom_ccsd_22_tripletmp_trans_aibjaibj_bj + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaibj_bj
    function eom_ccsd_22_tripletmp_trans_aibjaibj_ai(t2, nocc, nactive, a, i) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaibj_ai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, a)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, a, i, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaibj_ai = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjaibj_ai = eom_ccsd_22_tripletmp_trans_aibjaibj_ai + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaibj_ai
    function eom_ccsd_22_tripletmp_trans_aibjaibj_ibj(t2, nocc, nactive, i, b, j) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaibj_ibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, b, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, i, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaibj_ibj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjaibj_ibj = eom_ccsd_22_tripletmp_trans_aibjaibj_ibj + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaibj_ibj
    function eom_ccsd_22_tripletmp_trans_aibjaibj_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaibj_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, a, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaibj_aij = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjaibj_aij = eom_ccsd_22_tripletmp_trans_aibjaibj_aij + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaibj_aij
    function eom_ccsd_22_tripletmp_trans_aibjaibj_abj(t2, nocc, nactive, a, b, j) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaibj_abj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaibj_abj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjaibj_abj = eom_ccsd_22_tripletmp_trans_aibjaibj_abj + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaibj_abj
    function eom_ccsd_22_tripletmp_trans_aibjaibj_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjaibj_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, i, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjaibj_aib = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjaibj_aib = eom_ccsd_22_tripletmp_trans_aibjaibj_aib + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjaibj_aib
    function eom_ccsd_22_tripletmp_trans_aibiaibl_ibl(t2, nocc, nactive, i, b, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibiaibl_ibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, b)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,i) * tovov(l, b, i, e)
term(2) = term(2) + t2(b,e,i,i) * tovov(l, e, i, b)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, b, l, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, l, b)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, l, b)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibiaibl_ibl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aibiaibl_ibl = eom_ccsd_22_tripletmp_trans_aibiaibl_ibl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibiaibl_ibl
    function eom_ccsd_22_tripletmp_trans_aibiaibl_ail(t2, nocc, nactive, a, i, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibiaibl_ail 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, a)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,i) * tovov(l, a, i, e)
term(2) = term(2) + t2(a,e,i,i) * tovov(l, e, i, a)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, a, l, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibiaibl_ail = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletmp_trans_aibiaibl_ail = eom_ccsd_22_tripletmp_trans_aibiaibl_ail + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibiaibl_ail
    function eom_ccsd_22_tripletmp_trans_aibiaibl_aibl(t2, nocc, nactive, a, i, b, l) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibiaibl_aibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, l, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, l, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, l, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, l, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibiaibl_aibl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibiaibl_aibl = eom_ccsd_22_tripletmp_trans_aibiaibl_aibl + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibiaibl_aibl
    function eom_ccsd_22_tripletmp_trans_aibjajbi_bj(t2, nocc, nactive, b, j) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajbi_bj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, j, b)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, b, j, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, j, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, j, b)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajbi_bj = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjajbi_bj = eom_ccsd_22_tripletmp_trans_aibjajbi_bj + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajbi_bj
    function eom_ccsd_22_tripletmp_trans_aibjajbi_ai(t2, nocc, nactive, a, i) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajbi_ai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, a)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, a, i, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajbi_ai = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletmp_trans_aibjajbi_ai = eom_ccsd_22_tripletmp_trans_aibjajbi_ai + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajbi_ai
    function eom_ccsd_22_tripletmp_trans_aibjajbi_ibj(t2, nocc, nactive, i, b, j) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajbi_ibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, e, i, b)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, b, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajbi_ibj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjajbi_ibj = eom_ccsd_22_tripletmp_trans_aibjajbi_ibj + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajbi_ibj
    function eom_ccsd_22_tripletmp_trans_aibjajbi_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajbi_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, e, i, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, a, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajbi_aij = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjajbi_aij = eom_ccsd_22_tripletmp_trans_aibjajbi_aij + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajbi_aij
    function eom_ccsd_22_tripletmp_trans_aibjajbi_abj(t2, nocc, nactive, a, b, j) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajbi_abj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajbi_abj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjajbi_abj = eom_ccsd_22_tripletmp_trans_aibjajbi_abj + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajbi_abj
    function eom_ccsd_22_tripletmp_trans_aibjajbi_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_tripletmp_trans_aibjajbi_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, i, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletmp_trans_aibjajbi_aib = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletmp_trans_aibjajbi_aib = eom_ccsd_22_tripletmp_trans_aibjajbi_aib + term(s)
    end do

    end function eom_ccsd_22_tripletmp_trans_aibjajbi_aib
    end module eom_ccsd_22_tripletmp_trans
    