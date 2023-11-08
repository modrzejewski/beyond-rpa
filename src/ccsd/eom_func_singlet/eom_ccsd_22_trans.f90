module eom_ccsd_22_trans

    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none
    !               
    ! File generated automatically on 2018-11-14 14:20:53  
    !  
    contains
    
    function eom_ccsd_22_trans_aibjakdl_ibjkdl(t2, nocc, nactive, i, b, j, k, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjakdl_ibjkdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakdl_ibjkdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakdl_ibjkdl = eom_ccsd_22_trans_aibjakdl_ibjkdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakdl_ibjkdl
    function eom_ccsd_22_trans_aibjckal_ibjckl(t2, nocc, nactive, i, b, j, c, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibjckal_ibjckl 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjckal_ibjckl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjckal_ibjckl = eom_ccsd_22_trans_aibjckal_ibjckl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckal_ibjckl
    function eom_ccsd_22_trans_aibjbkdl_aijkdl(t2, nocc, nactive, a, i, j, k, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjbkdl_aijkdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbkdl_aijkdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbkdl_aijkdl = eom_ccsd_22_trans_aibjbkdl_aijkdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkdl_aijkdl
    function eom_ccsd_22_trans_aibjckbl_aijckl(t2, nocc, nactive, a, i, j, c, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibjckbl_aijckl 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjckbl_aijckl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjckbl_aijckl = eom_ccsd_22_trans_aibjckbl_aijckl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckbl_aijckl
    function eom_ccsd_22_trans_aibjcidl_abjcdl(t2, nocc, nactive, a, b, j, c, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcidl_abjcdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcidl_abjcdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcidl_abjcdl = eom_ccsd_22_trans_aibjcidl_abjcdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcidl_abjcdl
    function eom_ccsd_22_trans_aibjckdi_abjckd(t2, nocc, nactive, a, b, j, c, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjckdi_abjckd 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjckdi_abjckd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjckdi_abjckd = eom_ccsd_22_trans_aibjckdi_abjckd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckdi_abjckd
    function eom_ccsd_22_trans_aibjcjdl_aibcdl(t2, nocc, nactive, a, i, b, c, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcjdl_aibcdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjdl_aibcdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjdl_aibcdl = eom_ccsd_22_trans_aibjcjdl_aibcdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjdl_aibcdl
    function eom_ccsd_22_trans_aibjckdj_aibckd(t2, nocc, nactive, a, i, b, c, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjckdj_aibckd 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjckdj_aibckd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjckdj_aibckd = eom_ccsd_22_trans_aibjckdj_aibckd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckdj_aibckd
    function eom_ccsd_22_trans_aiajakdl_aijkdl(t2, nocc, nactive, a, i, j, k, d, l) 
    real(F64) :: eom_ccsd_22_trans_aiajakdl_aijkdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d, l 
    integer :: s ,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, d, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, d, k, e)
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, k, d)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, k, d)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajakdl_aijkdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajakdl_aijkdl = eom_ccsd_22_trans_aiajakdl_aijkdl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakdl_aijkdl
    function eom_ccsd_22_trans_aibjakal_aibjkl(t2, nocc, nactive, a, i, b, j, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibjakal_aibjkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, a, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjakal_aibjkl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakal_aibjkl = eom_ccsd_22_trans_aibjakal_aibjkl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakal_aibjkl
    function eom_ccsd_22_trans_aibjakbl_ijkl(t2, nocc, nactive, i, j, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibjakbl_ijkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, k, l 
    integer :: s ,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, j, k, i)


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,j) * tovov(l, f, k, e)
end do 
end do 



    eom_ccsd_22_trans_aibjakbl_ijkl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakbl_ijkl = eom_ccsd_22_trans_aibjakbl_ijkl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbl_ijkl
    function eom_ccsd_22_trans_aibjakbl_aijkl(t2, nocc, nactive, a, i, j, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibjakbl_aijkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, l 
    integer :: s ,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, a, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, k, a)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakbl_aijkl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakbl_aijkl = eom_ccsd_22_trans_aibjakbl_aijkl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbl_aijkl
    function eom_ccsd_22_trans_aibjakbl_ibjkl(t2, nocc, nactive, i, b, j, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibjakbl_ibjkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, l 
    integer :: s ,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, b, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, b)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakbl_ibjkl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakbl_ibjkl = eom_ccsd_22_trans_aibjakbl_ibjkl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbl_ibjkl
    function eom_ccsd_22_trans_aibjaidl_bjdl(t2, nocc, nactive, b, j, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjaidl_bjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, l, j)
term(1) = term(1) + tvoov(b, j, l, d)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,j) * tovov(m, d, l, e)
term(3) = term(3) + t2(b,e,j,m) * tovov(m, d, l, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, l, d)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjaidl_bjdl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_trans_aibjaidl_bjdl = eom_ccsd_22_trans_aibjaidl_bjdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaidl_bjdl
    function eom_ccsd_22_trans_aibjaidl_ibjdl(t2, nocc, nactive, i, b, j, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjaidl_ibjdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaidl_ibjdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaidl_ibjdl = eom_ccsd_22_trans_aibjaidl_ibjdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaidl_ibjdl
    function eom_ccsd_22_trans_aibjaidl_abjdl(t2, nocc, nactive, a, b, j, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjaidl_abjdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaidl_abjdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaidl_abjdl = eom_ccsd_22_trans_aibjaidl_abjdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaidl_abjdl
    function eom_ccsd_22_trans_aibiakdl_ibkdl(t2, nocc, nactive, i, b, k, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibiakdl_ibkdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiakdl_ibkdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibiakdl_ibkdl = eom_ccsd_22_trans_aibiakdl_ibkdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakdl_ibkdl
    function eom_ccsd_22_trans_aibjakdi_bjkd(t2, nocc, nactive, b, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjakdi_bjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, d)

term(0) = term(0) * (-0.9999999999999999d+0) 

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

term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakdi_bjkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjakdi_bjkd = eom_ccsd_22_trans_aibjakdi_bjkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakdi_bjkd
    function eom_ccsd_22_trans_aibjakdi_ibjkd(t2, nocc, nactive, i, b, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjakdi_ibjkd 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakdi_ibjkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakdi_ibjkd = eom_ccsd_22_trans_aibjakdi_ibjkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakdi_ibjkd
    function eom_ccsd_22_trans_aibjakdi_abjkd(t2, nocc, nactive, a, b, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjakdi_abjkd 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakdi_abjkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakdi_abjkd = eom_ccsd_22_trans_aibjakdi_abjkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakdi_abjkd
    function eom_ccsd_22_trans_aibjajdl_ibjdl(t2, nocc, nactive, i, b, j, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjajdl_ibjdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajdl_ibjdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajdl_ibjdl = eom_ccsd_22_trans_aibjajdl_ibjdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajdl_ibjdl
    function eom_ccsd_22_trans_aibjajdl_aibdl(t2, nocc, nactive, a, i, b, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjajdl_aibdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajdl_aibdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajdl_aibdl = eom_ccsd_22_trans_aibjajdl_aibdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajdl_aibdl
    function eom_ccsd_22_trans_aibjakdk_ibjkd(t2, nocc, nactive, i, b, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjakdk_ibjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, k, d)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjakdk_ibjkd = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjakdk_ibjkd = eom_ccsd_22_trans_aibjakdk_ibjkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakdk_ibjkd
    function eom_ccsd_22_trans_aibjakdj_ibkd(t2, nocc, nactive, i, b, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjakdj_ibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, k, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,m,i) * tovov(m, d, k, e)
end do 
end do 



    eom_ccsd_22_trans_aibjakdj_ibkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakdj_ibkd = eom_ccsd_22_trans_aibjakdj_ibkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakdj_ibkd
    function eom_ccsd_22_trans_aibjakdj_ibjkd(t2, nocc, nactive, i, b, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjakdj_ibjkd 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakdj_ibjkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakdj_ibjkd = eom_ccsd_22_trans_aibjakdj_ibjkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakdj_ibjkd
    function eom_ccsd_22_trans_aibjakdj_aibkd(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjakdj_aibkd 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakdj_aibkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakdj_aibkd = eom_ccsd_22_trans_aibjakdj_aibkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakdj_aibkd
    function eom_ccsd_22_trans_aiajckal_aijckl(t2, nocc, nactive, a, i, j, c, k, l) 
    real(F64) :: eom_ccsd_22_trans_aiajckal_aijckl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, c, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, c, k, e)
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, k, c)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, k, c)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajckal_aijckl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajckal_aijckl = eom_ccsd_22_trans_aiajckal_aijckl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajckal_aijckl
    function eom_ccsd_22_trans_aiajcidl_ajcdl(t2, nocc, nactive, a, j, c, d, l) 
    real(F64) :: eom_ccsd_22_trans_aiajcidl_ajcdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcidl_ajcdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajcidl_ajcdl = eom_ccsd_22_trans_aiajcidl_ajcdl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcidl_ajcdl
    function eom_ccsd_22_trans_aiajckdi_ajckd(t2, nocc, nactive, a, j, c, k, d) 
    real(F64) :: eom_ccsd_22_trans_aiajckdi_ajckd 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajckdi_ajckd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajckdi_ajckd = eom_ccsd_22_trans_aiajckdi_ajckd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajckdi_ajckd
    function eom_ccsd_22_trans_aiajcjdl_aicdl(t2, nocc, nactive, a, i, c, d, l) 
    real(F64) :: eom_ccsd_22_trans_aiajcjdl_aicdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcjdl_aicdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajcjdl_aicdl = eom_ccsd_22_trans_aiajcjdl_aicdl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjdl_aicdl
    function eom_ccsd_22_trans_aiajckdj_aickd(t2, nocc, nactive, a, i, c, k, d) 
    real(F64) :: eom_ccsd_22_trans_aiajckdj_aickd 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajckdj_aickd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajckdj_aickd = eom_ccsd_22_trans_aiajckdj_aickd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajckdj_aickd
    function eom_ccsd_22_trans_aibjcial_bjcl(t2, nocc, nactive, b, j, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcial_bjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, l, c)

term(0) = term(0) * (-0.9999999999999999d+0) 

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

term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcial_bjcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjcial_bjcl = eom_ccsd_22_trans_aibjcial_bjcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcial_bjcl
    function eom_ccsd_22_trans_aibjcial_ibjcl(t2, nocc, nactive, i, b, j, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcial_ibjcl 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcial_ibjcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcial_ibjcl = eom_ccsd_22_trans_aibjcial_ibjcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcial_ibjcl
    function eom_ccsd_22_trans_aibjcial_abjcl(t2, nocc, nactive, a, b, j, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcial_abjcl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcial_abjcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcial_abjcl = eom_ccsd_22_trans_aibjcial_abjcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcial_abjcl
    function eom_ccsd_22_trans_aibickal_ibckl(t2, nocc, nactive, i, b, c, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibickal_ibckl 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibickal_ibckl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibickal_ibckl = eom_ccsd_22_trans_aibickal_ibckl + term(s)
    end do

    end function eom_ccsd_22_trans_aibickal_ibckl
    function eom_ccsd_22_trans_aibjckai_bjck(t2, nocc, nactive, b, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckai_bjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, k, j)
term(1) = term(1) + tvoov(b, j, k, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,j) * tovov(m, c, k, e)
term(3) = term(3) + t2(b,e,j,m) * tovov(m, c, k, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjckai_bjck = 0.d+0
    do s = 0, 5
    eom_ccsd_22_trans_aibjckai_bjck = eom_ccsd_22_trans_aibjckai_bjck + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckai_bjck
    function eom_ccsd_22_trans_aibjckai_ibjck(t2, nocc, nactive, i, b, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckai_ibjck 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjckai_ibjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjckai_ibjck = eom_ccsd_22_trans_aibjckai_ibjck + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckai_ibjck
    function eom_ccsd_22_trans_aibjckai_abjck(t2, nocc, nactive, a, b, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckai_abjck 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjckai_abjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjckai_abjck = eom_ccsd_22_trans_aibjckai_abjck + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckai_abjck
    function eom_ccsd_22_trans_aibjcjal_ibcl(t2, nocc, nactive, i, b, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcjal_ibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, l, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,m,i) * tovov(m, c, l, e)
end do 
end do 



    eom_ccsd_22_trans_aibjcjal_ibcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjal_ibcl = eom_ccsd_22_trans_aibjcjal_ibcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjal_ibcl
    function eom_ccsd_22_trans_aibjcjal_ibjcl(t2, nocc, nactive, i, b, j, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcjal_ibjcl 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjal_ibjcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjal_ibjcl = eom_ccsd_22_trans_aibjcjal_ibjcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjal_ibjcl
    function eom_ccsd_22_trans_aibjcjal_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcjal_aibcl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjal_aibcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjal_aibcl = eom_ccsd_22_trans_aibjcjal_aibcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjal_aibcl
    function eom_ccsd_22_trans_aibjckak_ibjck(t2, nocc, nactive, i, b, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckak_ibjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, k, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjckak_ibjck = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjckak_ibjck = eom_ccsd_22_trans_aibjckak_ibjck + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckak_ibjck
    function eom_ccsd_22_trans_aibjckaj_ibjck(t2, nocc, nactive, i, b, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckaj_ibjck 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjckaj_ibjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjckaj_ibjck = eom_ccsd_22_trans_aibjckaj_ibjck + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckaj_ibjck
    function eom_ccsd_22_trans_aibjckaj_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckaj_aibck 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjckaj_aibck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjckaj_aibck = eom_ccsd_22_trans_aibjckaj_aibck + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckaj_aibck
    function eom_ccsd_22_trans_aibjbkbl_aibjkl(t2, nocc, nactive, a, i, b, j, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibjbkbl_aibjkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, b, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, k, b)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjbkbl_aibjkl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbkbl_aibjkl = eom_ccsd_22_trans_aibjbkbl_aibjkl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkbl_aibjkl
    function eom_ccsd_22_trans_aibjbidl_aijdl(t2, nocc, nactive, a, i, j, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjbidl_aijdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, d, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, i, d)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbidl_aijdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbidl_aijdl = eom_ccsd_22_trans_aibjbidl_aijdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbidl_aijdl
    function eom_ccsd_22_trans_aibjbidl_abjdl(t2, nocc, nactive, a, b, j, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjbidl_abjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, l, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, l, b)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbidl_abjdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbidl_abjdl = eom_ccsd_22_trans_aibjbidl_abjdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbidl_abjdl
    function eom_ccsd_22_trans_aibibkdl_aikdl(t2, nocc, nactive, a, i, k, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibibkdl_aikdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibibkdl_aikdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibibkdl_aikdl = eom_ccsd_22_trans_aibibkdl_aikdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibibkdl_aikdl
    function eom_ccsd_22_trans_aibjbkdi_ajkd(t2, nocc, nactive, a, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbkdi_ajkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, k, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,m,j) * tovov(m, d, k, e)
end do 
end do 



    eom_ccsd_22_trans_aibjbkdi_ajkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbkdi_ajkd = eom_ccsd_22_trans_aibjbkdi_ajkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkdi_ajkd
    function eom_ccsd_22_trans_aibjbkdi_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbkdi_aijkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, i, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, d, i, e)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbkdi_aijkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbkdi_aijkd = eom_ccsd_22_trans_aibjbkdi_aijkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkdi_aijkd
    function eom_ccsd_22_trans_aibjbkdi_abjkd(t2, nocc, nactive, a, b, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbkdi_abjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, k, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, k, b)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbkdi_abjkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbkdi_abjkd = eom_ccsd_22_trans_aibjbkdi_abjkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkdi_abjkd
    function eom_ccsd_22_trans_aibjbjdl_aidl(t2, nocc, nactive, a, i, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjbjdl_aidl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, l, i)
term(1) = term(1) + tvoov(a, i, l, d)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, d, l, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, l, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjbjdl_aidl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_trans_aibjbjdl_aidl = eom_ccsd_22_trans_aibjbjdl_aidl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjdl_aidl
    function eom_ccsd_22_trans_aibjbjdl_aijdl(t2, nocc, nactive, a, i, j, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjbjdl_aijdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbjdl_aijdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbjdl_aijdl = eom_ccsd_22_trans_aibjbjdl_aijdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjdl_aijdl
    function eom_ccsd_22_trans_aibjbjdl_aibdl(t2, nocc, nactive, a, i, b, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibjbjdl_aibdl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbjdl_aibdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbjdl_aibdl = eom_ccsd_22_trans_aibjbjdl_aibdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjdl_aibdl
    function eom_ccsd_22_trans_aibjbkdk_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbkdk_aijkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, k, d)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjbkdk_aijkd = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjbkdk_aijkd = eom_ccsd_22_trans_aibjbkdk_aijkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkdk_aijkd
    function eom_ccsd_22_trans_aibjbkdj_aikd(t2, nocc, nactive, a, i, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbkdj_aikd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, d)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, d, k, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, k, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbkdj_aikd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjbkdj_aikd = eom_ccsd_22_trans_aibjbkdj_aikd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkdj_aikd
    function eom_ccsd_22_trans_aibjbkdj_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbkdj_aijkd 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbkdj_aijkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbkdj_aijkd = eom_ccsd_22_trans_aibjbkdj_aijkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkdj_aijkd
    function eom_ccsd_22_trans_aibjbkdj_aibkd(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbkdj_aibkd 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbkdj_aibkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbkdj_aibkd = eom_ccsd_22_trans_aibjbkdj_aibkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkdj_aibkd
    function eom_ccsd_22_trans_aibjcicl_abjcl(t2, nocc, nactive, a, b, j, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcicl_abjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, l 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjcicl_abjcl = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjcicl_abjcl = eom_ccsd_22_trans_aibjcicl_abjcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcicl_abjcl
    function eom_ccsd_22_trans_aibjckci_abjck(t2, nocc, nactive, a, b, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckci_abjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, k, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjckci_abjck = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjckci_abjck = eom_ccsd_22_trans_aibjckci_abjck + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckci_abjck
    function eom_ccsd_22_trans_aibjcjcl_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcjcl_aibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, l, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjcjcl_aibcl = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjcjcl_aibcl = eom_ccsd_22_trans_aibjcjcl_aibcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjcl_aibcl
    function eom_ccsd_22_trans_aibjckcj_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckcj_aibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, k, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjckcj_aibck = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjckcj_aibck = eom_ccsd_22_trans_aibjckcj_aibck + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckcj_aibck
    function eom_ccsd_22_trans_aibjcibl_ajcl(t2, nocc, nactive, a, j, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcibl_ajcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, l, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,m,j) * tovov(m, c, l, e)
end do 
end do 



    eom_ccsd_22_trans_aibjcibl_ajcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcibl_ajcl = eom_ccsd_22_trans_aibjcibl_ajcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcibl_ajcl
    function eom_ccsd_22_trans_aibjcibl_aijcl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcibl_aijcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, c, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, i, c)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcibl_aijcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcibl_aijcl = eom_ccsd_22_trans_aibjcibl_aijcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcibl_aijcl
    function eom_ccsd_22_trans_aibjcibl_abjcl(t2, nocc, nactive, a, b, j, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcibl_abjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, l, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcibl_abjcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcibl_abjcl = eom_ccsd_22_trans_aibjcibl_abjcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcibl_abjcl
    function eom_ccsd_22_trans_aibickbl_aickl(t2, nocc, nactive, a, i, c, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibickbl_aickl 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibickbl_aickl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibickbl_aickl = eom_ccsd_22_trans_aibickbl_aickl + term(s)
    end do

    end function eom_ccsd_22_trans_aibickbl_aickl
    function eom_ccsd_22_trans_aibjckbi_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckbi_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, i, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, c, i, e)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjckbi_aijck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjckbi_aijck = eom_ccsd_22_trans_aibjckbi_aijck + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckbi_aijck
    function eom_ccsd_22_trans_aibjckbi_abjck(t2, nocc, nactive, a, b, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckbi_abjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, k, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, k, c)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjckbi_abjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjckbi_abjck = eom_ccsd_22_trans_aibjckbi_abjck + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckbi_abjck
    function eom_ccsd_22_trans_aibjcjbl_aicl(t2, nocc, nactive, a, i, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcjbl_aicl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, c)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,m,i) * tovov(m, e, l, c)
term(2) = term(2) + t2(a,e,i,m) * tovov(m, c, l, e)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjbl_aicl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjcjbl_aicl = eom_ccsd_22_trans_aibjcjbl_aicl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjbl_aicl
    function eom_ccsd_22_trans_aibjcjbl_aijcl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcjbl_aijcl 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjbl_aijcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjbl_aijcl = eom_ccsd_22_trans_aibjcjbl_aijcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjbl_aijcl
    function eom_ccsd_22_trans_aibjcjbl_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibjcjbl_aibcl 
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

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjbl_aibcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjbl_aibcl = eom_ccsd_22_trans_aibjcjbl_aibcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjbl_aibcl
    function eom_ccsd_22_trans_aibjckbk_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckbk_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, k, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjckbk_aijck = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjckbk_aijck = eom_ccsd_22_trans_aibjckbk_aijck + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckbk_aijck
    function eom_ccsd_22_trans_aibjckbj_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckbj_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, k, i)
term(1) = term(1) + tvoov(a, i, k, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, c, k, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, c, k, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjckbj_aick = 0.d+0
    do s = 0, 5
    eom_ccsd_22_trans_aibjckbj_aick = eom_ccsd_22_trans_aibjckbj_aick + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckbj_aick
    function eom_ccsd_22_trans_aibjckbj_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckbj_aijck 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjckbj_aijck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjckbj_aijck = eom_ccsd_22_trans_aibjckbj_aijck + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckbj_aijck
    function eom_ccsd_22_trans_aibjckbj_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibjckbj_aibck 
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

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjckbj_aibck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjckbj_aibck = eom_ccsd_22_trans_aibjckbj_aibck + term(s)
    end do

    end function eom_ccsd_22_trans_aibjckbj_aibck
    function eom_ccsd_22_trans_aibicidl_aibcdl(t2, nocc, nactive, a, i, b, c, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibicidl_aibcdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d, l 
    integer :: s ,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, l, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, l, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibicidl_aibcdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibicidl_aibcdl = eom_ccsd_22_trans_aibicidl_aibcdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibicidl_aibcdl
    function eom_ccsd_22_trans_aibjcidi_aibjcd(t2, nocc, nactive, a, i, b, j, c, d) 
    real(F64) :: eom_ccsd_22_trans_aibjcidi_aibjcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, i, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjcidi_aibjcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcidi_aibjcd = eom_ccsd_22_trans_aibjcidi_aibjcd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcidi_aibjcd
    function eom_ccsd_22_trans_aibjcidj_abcd(t2, nocc, nactive, a, b, c, d) 
    real(F64) :: eom_ccsd_22_trans_aibjcidj_abcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, d, a, c)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, d, m, c)
end do 
end do 



    eom_ccsd_22_trans_aibjcidj_abcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcidj_abcd = eom_ccsd_22_trans_aibjcidj_abcd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcidj_abcd
    function eom_ccsd_22_trans_aibjcidj_aibcd(t2, nocc, nactive, a, i, b, c, d) 
    real(F64) :: eom_ccsd_22_trans_aibjcidj_aibcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcidj_aibcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcidj_aibcd = eom_ccsd_22_trans_aibjcidj_aibcd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcidj_aibcd
    function eom_ccsd_22_trans_aibjcidj_abjcd(t2, nocc, nactive, a, b, j, c, d) 
    real(F64) :: eom_ccsd_22_trans_aibjcidj_abjcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcidj_abjcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcidj_abjcd = eom_ccsd_22_trans_aibjcidj_abjcd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcidj_abjcd
    function eom_ccsd_22_trans_aibickdi_aibckd(t2, nocc, nactive, a, i, b, c, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibickdi_aibckd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, k, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, k, d)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, d, k, c)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, d, k, c)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibickdi_aibckd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibickdi_aibckd = eom_ccsd_22_trans_aibickdi_aibckd + term(s)
    end do

    end function eom_ccsd_22_trans_aibickdi_aibckd
    function eom_ccsd_22_trans_aibjcjdi_abcd(t2, nocc, nactive, a, b, c, d) 
    real(F64) :: eom_ccsd_22_trans_aibjcjdi_abcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, c, a, d)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, c, m, d)
end do 
end do 



    eom_ccsd_22_trans_aibjcjdi_abcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjdi_abcd = eom_ccsd_22_trans_aibjcjdi_abcd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjdi_abcd
    function eom_ccsd_22_trans_aibjcjdi_abjcd(t2, nocc, nactive, a, b, j, c, d) 
    real(F64) :: eom_ccsd_22_trans_aibjcjdi_abjcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, c)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjdi_abjcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjdi_abjcd = eom_ccsd_22_trans_aibjcjdi_abjcd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjdi_abjcd
    function eom_ccsd_22_trans_aibjcjdi_aibcd(t2, nocc, nactive, a, i, b, c, d) 
    real(F64) :: eom_ccsd_22_trans_aibjcjdi_aibcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjdi_aibcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjdi_aibcd = eom_ccsd_22_trans_aibjcjdi_aibcd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjdi_aibcd
    function eom_ccsd_22_trans_aibjcjdj_aibjcd(t2, nocc, nactive, a, i, b, j, c, d) 
    real(F64) :: eom_ccsd_22_trans_aibjcjdj_aibjcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, j, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjcjdj_aibjcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjdj_aibjcd = eom_ccsd_22_trans_aibjcjdj_aibjcd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjdj_aibjcd
    function eom_ccsd_22_trans_aiajakal_ijkl(t2, nocc, nactive, i, j, k, l) 
    real(F64) :: eom_ccsd_22_trans_aiajakal_ijkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, k, l 
    integer :: s ,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, i, k, j)
term(1) = term(1) + toooo(l, j, k, i)


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(l, f, k, e)
end do 
end do 


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(l, e, k, f)
end do 
end do 



    eom_ccsd_22_trans_aiajakal_ijkl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajakal_ijkl = eom_ccsd_22_trans_aiajakal_ijkl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakal_ijkl
    function eom_ccsd_22_trans_aiajakal_aijkl(t2, nocc, nactive, a, i, j, k, l) 
    real(F64) :: eom_ccsd_22_trans_aiajakal_aijkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, l 
    integer :: s ,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, a, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, a, k, e)
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, k, a)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, k, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajakal_aijkl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajakal_aijkl = eom_ccsd_22_trans_aiajakal_aijkl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakal_aijkl
    function eom_ccsd_22_trans_aiajaidl_ajdl(t2, nocc, nactive, a, j, d, l) 
    real(F64) :: eom_ccsd_22_trans_aiajaidl_ajdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, l, j)
term(1) = term(1) + tvoov(a, j, l, d)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, l, d)
term(3) = term(3) + t2(a,a,j,m) * tovov(m, d, l, a)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,j) * tovov(m, d, l, e)
term(5) = term(5) + t2(a,e,j,m) * tovov(m, d, l, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aiajaidl_ajdl = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajaidl_ajdl = eom_ccsd_22_trans_aiajaidl_ajdl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaidl_ajdl
    function eom_ccsd_22_trans_aiajaidl_aijdl(t2, nocc, nactive, a, i, j, d, l) 
    real(F64) :: eom_ccsd_22_trans_aiajaidl_aijdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, d, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, d, i, e)
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, i, d)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, i, d)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajaidl_aijdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajaidl_aijdl = eom_ccsd_22_trans_aiajaidl_aijdl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaidl_aijdl
    function eom_ccsd_22_trans_aiaiakdl_aikdl(t2, nocc, nactive, a, i, k, d, l) 
    real(F64) :: eom_ccsd_22_trans_aiaiakdl_aikdl 
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

term(0) = term(0) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aiaiakdl_aikdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiaiakdl_aikdl = eom_ccsd_22_trans_aiaiakdl_aikdl + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiakdl_aikdl
    function eom_ccsd_22_trans_aiajakdi_ajkd(t2, nocc, nactive, a, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aiajakdi_ajkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, k, d)
term(1) = term(1) + tvvoo(a, d, k, j)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, k, d)
term(3) = term(3) + t2(a,a,j,m) * tovov(m, d, k, a)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,j,m) * tovov(m, d, k, e)
term(5) = term(5) + t2(a,e,m,j) * tovov(m, d, k, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, e, k, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, k, d)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajakdi_ajkd = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajakdi_ajkd = eom_ccsd_22_trans_aiajakdi_ajkd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakdi_ajkd
    function eom_ccsd_22_trans_aiajakdi_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aiajakdi_aijkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, i, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, i, d)
term(2) = term(2) + t2(a,e,j,i) * tovov(k, d, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, d, i, e)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajakdi_aijkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajakdi_aijkd = eom_ccsd_22_trans_aiajakdi_aijkd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakdi_aijkd
    function eom_ccsd_22_trans_aiajajdl_aidl(t2, nocc, nactive, a, i, d, l) 
    real(F64) :: eom_ccsd_22_trans_aiajajdl_aidl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, l, i)
term(1) = term(1) + tvoov(a, i, l, d)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, l, d)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, d, l, a)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, d, l, e)
term(5) = term(5) + t2(a,e,i,m) * tovov(m, d, l, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aiajajdl_aidl = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajajdl_aidl = eom_ccsd_22_trans_aiajajdl_aidl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajajdl_aidl
    function eom_ccsd_22_trans_aiajajdl_aijdl(t2, nocc, nactive, a, i, j, d, l) 
    real(F64) :: eom_ccsd_22_trans_aiajajdl_aijdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, d, j, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, d, j, e)
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, j, d)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, j, d)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajajdl_aijdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajajdl_aijdl = eom_ccsd_22_trans_aiajajdl_aijdl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajajdl_aijdl
    function eom_ccsd_22_trans_aiajakdk_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aiajakdk_aijkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, k, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, k, d)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajakdk_aijkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajakdk_aijkd = eom_ccsd_22_trans_aiajakdk_aijkd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakdk_aijkd
    function eom_ccsd_22_trans_aiajakdj_aikd(t2, nocc, nactive, a, i, k, d) 
    real(F64) :: eom_ccsd_22_trans_aiajakdj_aikd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, d)
term(1) = term(1) + tvvoo(a, d, k, i)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, k, d)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, d, k, a)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,i,m) * tovov(m, d, k, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, d, k, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, k, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajakdj_aikd = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajakdj_aikd = eom_ccsd_22_trans_aiajakdj_aikd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakdj_aikd
    function eom_ccsd_22_trans_aiajakdj_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_trans_aiajakdj_aijkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, j, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, j, d)
term(2) = term(2) + t2(a,e,j,i) * tovov(k, d, j, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, d, j, e)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajakdj_aijkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajakdj_aijkd = eom_ccsd_22_trans_aiajakdj_aijkd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakdj_aijkd
    function eom_ccsd_22_trans_aibjaial_abjl(t2, nocc, nactive, a, b, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjaial_abjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, l, a)
term(1) = term(1) + tvvoo(b, a, l, j)

term(1) = term(1) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, l, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, a, l, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, l, a)
term(5) = term(5) + t2(b,e,m,j) * tovov(m, a, l, e)
end do 
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,m) * tovov(m, e, l, a)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaial_abjl = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjaial_abjl = eom_ccsd_22_trans_aibjaial_abjl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaial_abjl
    function eom_ccsd_22_trans_aibjaial_aibjl(t2, nocc, nactive, a, i, b, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjaial_aibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, a, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, i, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjaial_aibjl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaial_aibjl = eom_ccsd_22_trans_aibjaial_aibjl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaial_aibjl
    function eom_ccsd_22_trans_aibiakal_aibkl(t2, nocc, nactive, a, i, b, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibiakal_aibkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(l, a, k, e)
term(1) = term(1) + t2(b,e,i,i) * tovov(l, e, k, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibiakal_aibkl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibiakal_aibkl = eom_ccsd_22_trans_aibiakal_aibkl + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakal_aibkl
    function eom_ccsd_22_trans_aibjakai_abjk(t2, nocc, nactive, a, b, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakai_abjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, k, j)
term(1) = term(1) + tvoov(b, j, k, a)

term(0) = term(0) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, k, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,j) * tovov(m, a, k, e)
term(4) = term(4) + t2(b,e,j,m) * tovov(m, a, k, e)
term(5) = term(5) + t2(b,e,m,j) * tovov(m, e, k, a)
end do 
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,m) * tovov(m, e, k, a)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakai_abjk = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjakai_abjk = eom_ccsd_22_trans_aibjakai_abjk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakai_abjk
    function eom_ccsd_22_trans_aibjakai_aibjk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakai_aibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, i, a)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, a, i, e)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjakai_aibjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakai_aibjk = eom_ccsd_22_trans_aibjakai_aibjk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakai_aibjk
    function eom_ccsd_22_trans_aibjajal_aibl(t2, nocc, nactive, a, i, b, l) 
    real(F64) :: eom_ccsd_22_trans_aibjajal_aibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,e,m 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, l, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, l, a)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,i) * tovov(m, a, l, e)
end do 
end do 



    eom_ccsd_22_trans_aibjajal_aibl = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjajal_aibl = eom_ccsd_22_trans_aibjajal_aibl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajal_aibl
    function eom_ccsd_22_trans_aibjajal_aibjl(t2, nocc, nactive, a, i, b, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjajal_aibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, a, j, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, j, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjajal_aibjl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajal_aibjl = eom_ccsd_22_trans_aibjajal_aibjl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajal_aibjl
    function eom_ccsd_22_trans_aibjakak_aibjk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakak_aibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, k, a)
end do 

term(0) = term(0) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aibjakak_aibjk = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjakak_aibjk = eom_ccsd_22_trans_aibjakak_aibjk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakak_aibjk
    function eom_ccsd_22_trans_aibjakaj_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakaj_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, k, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, k, a)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,i) * tovov(m, a, k, e)
end do 
end do 



    eom_ccsd_22_trans_aibjakaj_aibk = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjakaj_aibk = eom_ccsd_22_trans_aibjakaj_aibk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakaj_aibk
    function eom_ccsd_22_trans_aibjakaj_aibjk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakaj_aibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, j, a)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, a, j, e)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjakaj_aibjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakaj_aibjk = eom_ccsd_22_trans_aibjakaj_aibjk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakaj_aibjk
    function eom_ccsd_22_trans_aibjaibl_jl(t2, nocc, nactive, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjaibl_jl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(l, j)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + toooo(m, j, l, m)
term(2) = term(2) + toooo(m, m, l, j)
end do 

term(2) = term(2) * (-1.9999999999999998d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,j,m) * tovov(m, e, l, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,j,m) * tovov(m, f, l, e)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibl_jl = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibjaibl_jl = eom_ccsd_22_trans_aibjaibl_jl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibl_jl
    function eom_ccsd_22_trans_aibjaibl_ajl(t2, nocc, nactive, a, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjaibl_ajl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, l, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,m,j) * tovov(m, a, l, e)
end do 
end do 



    eom_ccsd_22_trans_aibjaibl_ajl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibl_ajl = eom_ccsd_22_trans_aibjaibl_ajl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibl_ajl
    function eom_ccsd_22_trans_aibjaibl_bjl(t2, nocc, nactive, b, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjaibl_bjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, b, l, j)
term(1) = term(1) + tvoov(b, j, l, b)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,j) * tovov(m, b, l, e)
term(3) = term(3) + t2(b,e,j,m) * tovov(m, b, l, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, l, b)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, l, b)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjaibl_bjl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_trans_aibjaibl_bjl = eom_ccsd_22_trans_aibjaibl_bjl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibl_bjl
    function eom_ccsd_22_trans_aibjaibl_ijl(t2, nocc, nactive, i, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjaibl_ijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, j, i, i)


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,j) * tovov(l, f, i, e)
end do 
end do 



    eom_ccsd_22_trans_aibjaibl_ijl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibl_ijl = eom_ccsd_22_trans_aibjaibl_ijl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibl_ijl
    function eom_ccsd_22_trans_aibjaibl_aijl(t2, nocc, nactive, a, i, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjaibl_aijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, a, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, i, a)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibl_aijl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibl_aijl = eom_ccsd_22_trans_aibjaibl_aijl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibl_aijl
    function eom_ccsd_22_trans_aibjaibl_ibjl(t2, nocc, nactive, i, b, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjaibl_ibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, b, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, i, b)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibl_ibjl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibl_ibjl = eom_ccsd_22_trans_aibjaibl_ibjl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibl_ibjl
    function eom_ccsd_22_trans_aibjaibl_abjl(t2, nocc, nactive, a, b, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjaibl_abjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, l, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, l, a)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibl_abjl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibl_abjl = eom_ccsd_22_trans_aibjaibl_abjl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibl_abjl
    function eom_ccsd_22_trans_aibiakbl_ikl(t2, nocc, nactive, i, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibiakbl_ikl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, k, l 
    integer :: s ,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, i, k, i)


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,i) * tovov(l, f, k, e)
end do 
end do 



    eom_ccsd_22_trans_aibiakbl_ikl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibiakbl_ikl = eom_ccsd_22_trans_aibiakbl_ikl + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakbl_ikl
    function eom_ccsd_22_trans_aibiakbl_aikl(t2, nocc, nactive, a, i, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibiakbl_aikl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, l 
    integer :: s ,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(l, a, k, e)
term(1) = term(1) + t2(a,e,i,i) * tovov(l, e, k, a)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiakbl_aikl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibiakbl_aikl = eom_ccsd_22_trans_aibiakbl_aikl + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakbl_aikl
    function eom_ccsd_22_trans_aibiakbl_ibkl(t2, nocc, nactive, i, b, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibiakbl_ibkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k, l 
    integer :: s ,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(l, b, k, e)
term(1) = term(1) + t2(b,e,i,i) * tovov(l, e, k, b)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiakbl_ibkl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibiakbl_ibkl = eom_ccsd_22_trans_aibiakbl_ibkl + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakbl_ibkl
    function eom_ccsd_22_trans_aibjakbi_ijk(t2, nocc, nactive, i, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbi_ijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, k 
    integer :: s ,e,m,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(k, i, i, j)


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,j) * tovov(k, e, i, f)
end do 
end do 



    eom_ccsd_22_trans_aibjakbi_ijk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakbi_ijk = eom_ccsd_22_trans_aibjakbi_ijk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbi_ijk
    function eom_ccsd_22_trans_aibjakbi_bjk(t2, nocc, nactive, b, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbi_bjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, k 
    integer :: s ,e,m,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, b)

term(0) = term(0) * (-0.9999999999999999d+0) 

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

term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakbi_bjk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjakbi_bjk = eom_ccsd_22_trans_aibjakbi_bjk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbi_bjk
    function eom_ccsd_22_trans_aibjakbi_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbi_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,e,m,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, i, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, a, i, e)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakbi_aijk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakbi_aijk = eom_ccsd_22_trans_aibjakbi_aijk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbi_aijk
    function eom_ccsd_22_trans_aibjakbi_ibjk(t2, nocc, nactive, i, b, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbi_ibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k 
    integer :: s ,e,m,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, i, b)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, b, i, e)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakbi_ibjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakbi_ibjk = eom_ccsd_22_trans_aibjakbi_ibjk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbi_ibjk
    function eom_ccsd_22_trans_aibjakbi_abjk(t2, nocc, nactive, a, b, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbi_abjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k 
    integer :: s ,e,m,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, k, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, k, a)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakbi_abjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakbi_abjk = eom_ccsd_22_trans_aibjakbi_abjk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbi_abjk
    function eom_ccsd_22_trans_aibjajbl_ail(t2, nocc, nactive, a, i, l) 
    real(F64) :: eom_ccsd_22_trans_aibjajbl_ail 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, l 
    integer :: s ,e,m,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, a)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,m,i) * tovov(m, e, l, a)
term(2) = term(2) + t2(a,e,i,m) * tovov(m, a, l, e)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajbl_ail = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjajbl_ail = eom_ccsd_22_trans_aibjajbl_ail + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbl_ail
    function eom_ccsd_22_trans_aibjajbl_ijl(t2, nocc, nactive, i, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjajbl_ijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, l 
    integer :: s ,e,m,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, j, j, i)


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,j) * tovov(l, f, j, e)
end do 
end do 



    eom_ccsd_22_trans_aibjajbl_ijl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajbl_ijl = eom_ccsd_22_trans_aibjajbl_ijl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbl_ijl
    function eom_ccsd_22_trans_aibjajbl_aijl(t2, nocc, nactive, a, i, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjajbl_aijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,e,m,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, a, j, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, j, a)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajbl_aijl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajbl_aijl = eom_ccsd_22_trans_aibjajbl_aijl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbl_aijl
    function eom_ccsd_22_trans_aibjajbl_ibjl(t2, nocc, nactive, i, b, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjajbl_ibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, l 
    integer :: s ,e,m,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, b, j, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, j, b)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajbl_ibjl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajbl_ibjl = eom_ccsd_22_trans_aibjajbl_ibjl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbl_ibjl
    function eom_ccsd_22_trans_aibjajbl_aibl(t2, nocc, nactive, a, i, b, l) 
    real(F64) :: eom_ccsd_22_trans_aibjajbl_aibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,e,m,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, l, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, l, a)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajbl_aibl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajbl_aibl = eom_ccsd_22_trans_aibjajbl_aibl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbl_aibl
    function eom_ccsd_22_trans_aibjakbk_ijk(t2, nocc, nactive, i, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbk_ijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, k 
    integer :: s ,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(k, j, k, i)


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,j) * tovov(k, f, k, e)
end do 
end do 



    eom_ccsd_22_trans_aibjakbk_ijk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakbk_ijk = eom_ccsd_22_trans_aibjakbk_ijk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbk_ijk
    function eom_ccsd_22_trans_aibjakbk_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbk_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,e,f 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, k, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjakbk_aijk = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjakbk_aijk = eom_ccsd_22_trans_aibjakbk_aijk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbk_aijk
    function eom_ccsd_22_trans_aibjakbk_ibjk(t2, nocc, nactive, i, b, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbk_ibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k 
    integer :: s ,e,f 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, k, b)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjakbk_ibjk = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjakbk_ibjk = eom_ccsd_22_trans_aibjakbk_ibjk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbk_ibjk
    function eom_ccsd_22_trans_aibjakbj_ik(t2, nocc, nactive, i, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbj_ik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(k, i)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + toooo(m, i, k, m)
term(2) = term(2) + toooo(m, m, k, i)
end do 

term(2) = term(2) * (-1.9999999999999998d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,m) * tovov(m, e, k, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,m) * tovov(m, f, k, e)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakbj_ik = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibjakbj_ik = eom_ccsd_22_trans_aibjakbj_ik + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbj_ik
    function eom_ccsd_22_trans_aibjakbj_aik(t2, nocc, nactive, a, i, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbj_aik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, k, i)
term(1) = term(1) + tvoov(a, i, k, a)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, a, k, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, a, k, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjakbj_aik = 0.d+0
    do s = 0, 5
    eom_ccsd_22_trans_aibjakbj_aik = eom_ccsd_22_trans_aibjakbj_aik + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbj_aik
    function eom_ccsd_22_trans_aibjakbj_ibk(t2, nocc, nactive, i, b, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbj_ibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, b, k, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,m,i) * tovov(m, b, k, e)
end do 
end do 



    eom_ccsd_22_trans_aibjakbj_ibk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakbj_ibk = eom_ccsd_22_trans_aibjakbj_ibk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbj_ibk
    function eom_ccsd_22_trans_aibjakbj_ijk(t2, nocc, nactive, i, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbj_ijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(k, i, j, j)


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,j) * tovov(k, e, j, f)
end do 
end do 



    eom_ccsd_22_trans_aibjakbj_ijk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakbj_ijk = eom_ccsd_22_trans_aibjakbj_ijk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbj_ijk
    function eom_ccsd_22_trans_aibjakbj_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbj_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, j, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, a, j, e)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakbj_aijk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakbj_aijk = eom_ccsd_22_trans_aibjakbj_aijk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbj_aijk
    function eom_ccsd_22_trans_aibjakbj_ibjk(t2, nocc, nactive, i, b, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbj_ibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, j, b)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, b, j, e)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakbj_ibjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakbj_ibjk = eom_ccsd_22_trans_aibjakbj_ibjk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbj_ibjk
    function eom_ccsd_22_trans_aibjakbj_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_trans_aibjakbj_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, k, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, k, a)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjakbj_aibk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakbj_aibk = eom_ccsd_22_trans_aibjakbj_aibk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakbj_aibk
    function eom_ccsd_22_trans_aibiaidl_ibdl(t2, nocc, nactive, i, b, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibiaidl_ibdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, l, i)
term(1) = term(1) + tvoov(b, i, l, d)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(l, d, i, e)
term(3) = term(3) + t2(b,e,i,i) * tovov(l, e, i, d)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,i) * tovov(m, d, l, e)
term(5) = term(5) + t2(b,e,i,m) * tovov(m, d, l, e)
term(6) = term(6) + t2(b,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibiaidl_ibdl = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibiaidl_ibdl = eom_ccsd_22_trans_aibiaidl_ibdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaidl_ibdl
    function eom_ccsd_22_trans_aibiaidl_aibdl(t2, nocc, nactive, a, i, b, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibiaidl_aibdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, l, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, l, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, l, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, l, a)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiaidl_aibdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibiaidl_aibdl = eom_ccsd_22_trans_aibiaidl_aibdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaidl_aibdl
    function eom_ccsd_22_trans_aibjaidi_ibjd(t2, nocc, nactive, i, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjaidi_ibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, i, d)
term(1) = term(1) + tvvoo(b, d, i, j)

term(1) = term(1) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(i, e, i, d)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, d, i, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, i, d)
term(5) = term(5) + t2(b,e,m,j) * tovov(m, d, i, e)
end do 
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,m) * tovov(m, e, i, d)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaidi_ibjd = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjaidi_ibjd = eom_ccsd_22_trans_aibjaidi_ibjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaidi_ibjd
    function eom_ccsd_22_trans_aibjaidi_aibjd(t2, nocc, nactive, a, i, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjaidi_aibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, i, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, i, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjaidi_aibjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaidi_aibjd = eom_ccsd_22_trans_aibjaidi_aibjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaidi_aibjd
    function eom_ccsd_22_trans_aibjaidj_bd(t2, nocc, nactive, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibjaidj_bd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(b, d)


do m = 1, nocc 
term(1) = term(1) + tvvoo(b, d, m, m)
term(2) = term(2) + tvoov(b, m, m, d)
end do 

term(1) = term(1) * (1.9999999999999998d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaidj_bd = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibjaidj_bd = eom_ccsd_22_trans_aibjaidj_bd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaidj_bd
    function eom_ccsd_22_trans_aibjaidj_abd(t2, nocc, nactive, a, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibjaidj_abd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, d, a, a)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, d, m, a)
end do 
end do 



    eom_ccsd_22_trans_aibjaidj_abd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaidj_abd = eom_ccsd_22_trans_aibjaidj_abd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaidj_abd
    function eom_ccsd_22_trans_aibjaidj_ibd(t2, nocc, nactive, i, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibjaidj_ibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, i, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,m,i) * tovov(m, d, i, e)
end do 
end do 



    eom_ccsd_22_trans_aibjaidj_ibd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaidj_ibd = eom_ccsd_22_trans_aibjaidj_ibd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaidj_ibd
    function eom_ccsd_22_trans_aibjaidj_bjd(t2, nocc, nactive, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjaidj_bjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, j, j)
term(1) = term(1) + tvoov(b, j, j, d)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,j) * tovov(m, d, j, e)
term(3) = term(3) + t2(b,e,j,m) * tovov(m, d, j, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, j, d)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjaidj_bjd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_trans_aibjaidj_bjd = eom_ccsd_22_trans_aibjaidj_bjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaidj_bjd
    function eom_ccsd_22_trans_aibjaidj_ibjd(t2, nocc, nactive, i, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjaidj_ibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, d, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, i, d)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaidj_ibjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaidj_ibjd = eom_ccsd_22_trans_aibjaidj_ibjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaidj_ibjd
    function eom_ccsd_22_trans_aibjaidj_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibjaidj_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, a)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaidj_aibd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaidj_aibd = eom_ccsd_22_trans_aibjaidj_aibd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaidj_aibd
    function eom_ccsd_22_trans_aibjaidj_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjaidj_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, a)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaidj_abjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaidj_abjd = eom_ccsd_22_trans_aibjaidj_abjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaidj_abjd
    function eom_ccsd_22_trans_aibiakdi_ibkd(t2, nocc, nactive, i, b, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibiakdi_ibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, d)
term(1) = term(1) + tvvoo(b, d, k, i)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(k, e, i, d)
term(3) = term(3) + t2(b,e,i,i) * tovov(k, d, i, e)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,i,m) * tovov(m, d, k, e)
term(5) = term(5) + t2(b,e,m,i) * tovov(m, d, k, e)
term(6) = term(6) + t2(b,e,m,i) * tovov(m, e, k, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiakdi_ibkd = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibiakdi_ibkd = eom_ccsd_22_trans_aibiakdi_ibkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakdi_ibkd
    function eom_ccsd_22_trans_aibiakdi_aibkd(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibiakdi_aibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, k, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, k, d)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, d, k, a)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, d, k, a)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiakdi_aibkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibiakdi_aibkd = eom_ccsd_22_trans_aibiakdi_aibkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakdi_aibkd
    function eom_ccsd_22_trans_aibiakdk_ibkd(t2, nocc, nactive, i, b, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibiakdk_ibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k, d 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(k, e, k, d)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibiakdk_ibkd = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibiakdk_ibkd = eom_ccsd_22_trans_aibiakdk_ibkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakdk_ibkd
    function eom_ccsd_22_trans_aibjajdi_abd(t2, nocc, nactive, a, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibjajdi_abd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, d 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, a, a, d)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, a, m, d)
end do 
end do 



    eom_ccsd_22_trans_aibjajdi_abd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajdi_abd = eom_ccsd_22_trans_aibjajdi_abd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajdi_abd
    function eom_ccsd_22_trans_aibjajdi_bjd(t2, nocc, nactive, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjajdi_bjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, d 
    integer :: s ,e,m,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, j, d)

term(0) = term(0) * (-0.9999999999999999d+0) 

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

term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajdi_bjd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjajdi_bjd = eom_ccsd_22_trans_aibjajdi_bjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajdi_bjd
    function eom_ccsd_22_trans_aibjajdi_ibjd(t2, nocc, nactive, i, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjajdi_ibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, e, i, d)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, d, i, e)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajdi_ibjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajdi_ibjd = eom_ccsd_22_trans_aibjajdi_ibjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajdi_ibjd
    function eom_ccsd_22_trans_aibjajdi_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjajdi_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, a)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajdi_abjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajdi_abjd = eom_ccsd_22_trans_aibjajdi_abjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajdi_abjd
    function eom_ccsd_22_trans_aibjajdi_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibjajdi_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, a)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajdi_aibd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajdi_aibd = eom_ccsd_22_trans_aibjajdi_aibd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajdi_aibd
    function eom_ccsd_22_trans_aibjajdj_ibjd(t2, nocc, nactive, i, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjajdj_ibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, j, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, j, d)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,i) * tovov(m, d, j, e)
end do 
end do 



    eom_ccsd_22_trans_aibjajdj_ibjd = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjajdj_ibjd = eom_ccsd_22_trans_aibjajdj_ibjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajdj_ibjd
    function eom_ccsd_22_trans_aibjajdj_aibjd(t2, nocc, nactive, a, i, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjajdj_aibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, j, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjajdj_aibjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajdj_aibjd = eom_ccsd_22_trans_aibjajdj_aibjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajdj_aibjd
    function eom_ccsd_22_trans_aiajcial_ajcl(t2, nocc, nactive, a, j, c, l) 
    real(F64) :: eom_ccsd_22_trans_aiajcial_ajcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, l, j)
term(1) = term(1) + tvoov(a, j, l, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,j,m) * tovov(m, c, l, a)
term(3) = term(3) + t2(a,a,j,m) * tovov(m, a, l, c)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,j,m) * tovov(m, c, l, e)
term(5) = term(5) + t2(a,e,m,j) * tovov(m, c, l, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, e, l, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, l, c)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcial_ajcl = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajcial_ajcl = eom_ccsd_22_trans_aiajcial_ajcl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcial_ajcl
    function eom_ccsd_22_trans_aiajcial_aijcl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: eom_ccsd_22_trans_aiajcial_aijcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, c, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, c, i, e)
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, i, c)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, i, c)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcial_aijcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajcial_aijcl = eom_ccsd_22_trans_aiajcial_aijcl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcial_aijcl
    function eom_ccsd_22_trans_aiaickal_aickl(t2, nocc, nactive, a, i, c, k, l) 
    real(F64) :: eom_ccsd_22_trans_aiaickal_aickl 
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

term(1) = term(1) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aiaickal_aickl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiaickal_aickl = eom_ccsd_22_trans_aiaickal_aickl + term(s)
    end do

    end function eom_ccsd_22_trans_aiaickal_aickl
    function eom_ccsd_22_trans_aiajckai_ajck(t2, nocc, nactive, a, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aiajckai_ajck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, k, j)
term(1) = term(1) + tvoov(a, j, k, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,j,m) * tovov(m, c, k, a)
term(3) = term(3) + t2(a,a,j,m) * tovov(m, a, k, c)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,j) * tovov(m, c, k, e)
term(5) = term(5) + t2(a,e,j,m) * tovov(m, c, k, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aiajckai_ajck = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajckai_ajck = eom_ccsd_22_trans_aiajckai_ajck + term(s)
    end do

    end function eom_ccsd_22_trans_aiajckai_ajck
    function eom_ccsd_22_trans_aiajckai_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aiajckai_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, i, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, i, c)
term(2) = term(2) + t2(a,e,j,i) * tovov(k, c, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, c, i, e)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajckai_aijck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajckai_aijck = eom_ccsd_22_trans_aiajckai_aijck + term(s)
    end do

    end function eom_ccsd_22_trans_aiajckai_aijck
    function eom_ccsd_22_trans_aiajcjal_aicl(t2, nocc, nactive, a, i, c, l) 
    real(F64) :: eom_ccsd_22_trans_aiajcjal_aicl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, l, i)
term(1) = term(1) + tvoov(a, i, l, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,i,m) * tovov(m, c, l, a)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, a, l, c)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, l, c)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, c, l, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, c, l, e)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcjal_aicl = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajcjal_aicl = eom_ccsd_22_trans_aiajcjal_aicl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjal_aicl
    function eom_ccsd_22_trans_aiajcjal_aijcl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: eom_ccsd_22_trans_aiajcjal_aijcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, c, j, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, c, j, e)
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, j, c)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, j, c)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcjal_aijcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajcjal_aijcl = eom_ccsd_22_trans_aiajcjal_aijcl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjal_aijcl
    function eom_ccsd_22_trans_aiajckak_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aiajckak_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, k, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, k, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajckak_aijck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajckak_aijck = eom_ccsd_22_trans_aiajckak_aijck + term(s)
    end do

    end function eom_ccsd_22_trans_aiajckak_aijck
    function eom_ccsd_22_trans_aiajckaj_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_ccsd_22_trans_aiajckaj_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, k, i)
term(1) = term(1) + tvoov(a, i, k, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,i,m) * tovov(m, c, k, a)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, a, k, c)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, c, k, e)
term(5) = term(5) + t2(a,e,i,m) * tovov(m, c, k, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aiajckaj_aick = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajckaj_aick = eom_ccsd_22_trans_aiajckaj_aick + term(s)
    end do

    end function eom_ccsd_22_trans_aiajckaj_aick
    function eom_ccsd_22_trans_aiajckaj_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aiajckaj_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, j, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, j, c)
term(2) = term(2) + t2(a,e,j,i) * tovov(k, c, j, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, c, j, e)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajckaj_aijck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajckaj_aijck = eom_ccsd_22_trans_aiajckaj_aijck + term(s)
    end do

    end function eom_ccsd_22_trans_aiajckaj_aijck
    function eom_ccsd_22_trans_aiajcicl_ajcl(t2, nocc, nactive, a, j, c, l) 
    real(F64) :: eom_ccsd_22_trans_aiajcicl_ajcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, l 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, l, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajcicl_ajcl = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiajcicl_ajcl = eom_ccsd_22_trans_aiajcicl_ajcl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcicl_ajcl
    function eom_ccsd_22_trans_aiajckci_ajck(t2, nocc, nactive, a, j, c, k) 
    real(F64) :: eom_ccsd_22_trans_aiajckci_ajck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, k, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajckci_ajck = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiajckci_ajck = eom_ccsd_22_trans_aiajckci_ajck + term(s)
    end do

    end function eom_ccsd_22_trans_aiajckci_ajck
    function eom_ccsd_22_trans_aiajcjcl_aicl(t2, nocc, nactive, a, i, c, l) 
    real(F64) :: eom_ccsd_22_trans_aiajcjcl_aicl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, l 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, l, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajcjcl_aicl = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiajcjcl_aicl = eom_ccsd_22_trans_aiajcjcl_aicl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjcl_aicl
    function eom_ccsd_22_trans_aiajckcj_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_ccsd_22_trans_aiajckcj_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, k, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajckcj_aick = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiajckcj_aick = eom_ccsd_22_trans_aiajckcj_aick + term(s)
    end do

    end function eom_ccsd_22_trans_aiajckcj_aick
    function eom_ccsd_22_trans_aiaicidl_aicdl(t2, nocc, nactive, a, i, c, d, l) 
    real(F64) :: eom_ccsd_22_trans_aiaicidl_aicdl 
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

term(0) = term(0) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aiaicidl_aicdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiaicidl_aicdl = eom_ccsd_22_trans_aiaicidl_aicdl + term(s)
    end do

    end function eom_ccsd_22_trans_aiaicidl_aicdl
    function eom_ccsd_22_trans_aiajcidi_aijcd(t2, nocc, nactive, a, i, j, c, d) 
    real(F64) :: eom_ccsd_22_trans_aiajcidi_aijcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, i, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajcidi_aijcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajcidi_aijcd = eom_ccsd_22_trans_aiajcidi_aijcd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcidi_aijcd
    function eom_ccsd_22_trans_aiajcidj_acd(t2, nocc, nactive, a, c, d) 
    real(F64) :: eom_ccsd_22_trans_aiajcidj_acd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(a, d, a, c)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,a,m,n) * tovov(n, d, m, c)
end do 
end do 



    eom_ccsd_22_trans_aiajcidj_acd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajcidj_acd = eom_ccsd_22_trans_aiajcidj_acd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcidj_acd
    function eom_ccsd_22_trans_aiajcidj_aicd(t2, nocc, nactive, a, i, c, d) 
    real(F64) :: eom_ccsd_22_trans_aiajcidj_aicd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, d, i, c)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcidj_aicd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajcidj_aicd = eom_ccsd_22_trans_aiajcidj_aicd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcidj_aicd
    function eom_ccsd_22_trans_aiajcidj_ajcd(t2, nocc, nactive, a, j, c, d) 
    real(F64) :: eom_ccsd_22_trans_aiajcidj_ajcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, j, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcidj_ajcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajcidj_ajcd = eom_ccsd_22_trans_aiajcidj_ajcd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcidj_ajcd
    function eom_ccsd_22_trans_aiaickdi_aickd(t2, nocc, nactive, a, i, c, k, d) 
    real(F64) :: eom_ccsd_22_trans_aiaickdi_aickd 
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

term(1) = term(1) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aiaickdi_aickd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiaickdi_aickd = eom_ccsd_22_trans_aiaickdi_aickd + term(s)
    end do

    end function eom_ccsd_22_trans_aiaickdi_aickd
    function eom_ccsd_22_trans_aiajcjdi_acd(t2, nocc, nactive, a, c, d) 
    real(F64) :: eom_ccsd_22_trans_aiajcjdi_acd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(a, d, a, c)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,a,m,n) * tovov(n, c, m, d)
end do 
end do 



    eom_ccsd_22_trans_aiajcjdi_acd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajcjdi_acd = eom_ccsd_22_trans_aiajcjdi_acd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjdi_acd
    function eom_ccsd_22_trans_aiajcjdi_ajcd(t2, nocc, nactive, a, j, c, d) 
    real(F64) :: eom_ccsd_22_trans_aiajcjdi_ajcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,a,j,m) * tovov(m, d, j, c)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcjdi_ajcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajcjdi_ajcd = eom_ccsd_22_trans_aiajcjdi_ajcd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjdi_ajcd
    function eom_ccsd_22_trans_aiajcjdi_aicd(t2, nocc, nactive, a, i, c, d) 
    real(F64) :: eom_ccsd_22_trans_aiajcjdi_aicd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, d, i, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcjdi_aicd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajcjdi_aicd = eom_ccsd_22_trans_aiajcjdi_aicd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjdi_aicd
    function eom_ccsd_22_trans_aiajcjdj_aijcd(t2, nocc, nactive, a, i, j, c, d) 
    real(F64) :: eom_ccsd_22_trans_aiajcjdj_aijcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, d, j, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajcjdj_aijcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajcjdj_aijcd = eom_ccsd_22_trans_aiajcjdj_aijcd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjdj_aijcd
    function eom_ccsd_22_trans_aibicial_ibcl(t2, nocc, nactive, i, b, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibicial_ibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, l, i)
term(1) = term(1) + tvoov(b, i, l, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(l, c, i, e)
term(3) = term(3) + t2(b,e,i,i) * tovov(l, e, i, c)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,i,m) * tovov(m, c, l, e)
term(5) = term(5) + t2(b,e,m,i) * tovov(m, e, l, c)
term(6) = term(6) + t2(b,e,m,i) * tovov(m, c, l, e)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibicial_ibcl = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibicial_ibcl = eom_ccsd_22_trans_aibicial_ibcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibicial_ibcl
    function eom_ccsd_22_trans_aibicial_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibicial_aibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, a)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, l, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, l, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibicial_aibcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibicial_aibcl = eom_ccsd_22_trans_aibicial_aibcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibicial_aibcl
    function eom_ccsd_22_trans_aibjciai_ibjc(t2, nocc, nactive, i, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjciai_ibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, i, j)
term(1) = term(1) + tvoov(b, j, i, c)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(i, e, i, c)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,j) * tovov(m, c, i, e)
term(4) = term(4) + t2(b,e,j,m) * tovov(m, c, i, e)
term(5) = term(5) + t2(b,e,m,j) * tovov(m, e, i, c)
end do 
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,m) * tovov(m, e, i, c)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjciai_ibjc = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjciai_ibjc = eom_ccsd_22_trans_aibjciai_ibjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjciai_ibjc
    function eom_ccsd_22_trans_aibjciai_aibjc(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjciai_aibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, i, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, i, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjciai_aibjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjciai_aibjc = eom_ccsd_22_trans_aibjciai_aibjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjciai_aibjc
    function eom_ccsd_22_trans_aibjciaj_bjc(t2, nocc, nactive, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjciaj_bjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, c 
    integer :: s ,e,m,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, j, c)

term(0) = term(0) * (-0.9999999999999999d+0) 

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

term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjciaj_bjc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjciaj_bjc = eom_ccsd_22_trans_aibjciaj_bjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjciaj_bjc
    function eom_ccsd_22_trans_aibjciaj_abc(t2, nocc, nactive, a, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjciaj_abc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, a, a, c)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, a, m, c)
end do 
end do 



    eom_ccsd_22_trans_aibjciaj_abc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjciaj_abc = eom_ccsd_22_trans_aibjciaj_abc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjciaj_abc
    function eom_ccsd_22_trans_aibjciaj_ibjc(t2, nocc, nactive, i, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjciaj_ibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, c, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, i, c)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjciaj_ibjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjciaj_ibjc = eom_ccsd_22_trans_aibjciaj_ibjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjciaj_ibjc
    function eom_ccsd_22_trans_aibjciaj_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjciaj_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, c)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjciaj_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjciaj_aibc = eom_ccsd_22_trans_aibjciaj_aibc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjciaj_aibc
    function eom_ccsd_22_trans_aibjciaj_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjciaj_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, j, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjciaj_abjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjciaj_abjc = eom_ccsd_22_trans_aibjciaj_abjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjciaj_abjc
    function eom_ccsd_22_trans_aibickai_ibck(t2, nocc, nactive, i, b, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibickai_ibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, k, i)
term(1) = term(1) + tvoov(b, i, k, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(k, e, i, c)
term(3) = term(3) + t2(b,e,i,i) * tovov(k, c, i, e)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,i) * tovov(m, c, k, e)
term(5) = term(5) + t2(b,e,i,m) * tovov(m, c, k, e)
term(6) = term(6) + t2(b,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibickai_ibck = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibickai_ibck = eom_ccsd_22_trans_aibickai_ibck + term(s)
    end do

    end function eom_ccsd_22_trans_aibickai_ibck
    function eom_ccsd_22_trans_aibickai_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibickai_aibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, k, a)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, k, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, k, c)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, a, k, c)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibickai_aibck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibickai_aibck = eom_ccsd_22_trans_aibickai_aibck + term(s)
    end do

    end function eom_ccsd_22_trans_aibickai_aibck
    function eom_ccsd_22_trans_aibickak_ibck(t2, nocc, nactive, i, b, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibickak_ibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, k 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(k, e, k, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibickak_ibck = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibickak_ibck = eom_ccsd_22_trans_aibickak_ibck + term(s)
    end do

    end function eom_ccsd_22_trans_aibickak_ibck
    function eom_ccsd_22_trans_aibjcjai_bc(t2, nocc, nactive, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjai_bc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(b, c)


do m = 1, nocc 
term(1) = term(1) + tvvoo(b, c, m, m)
term(2) = term(2) + tvoov(b, m, m, c)
end do 

term(1) = term(1) * (1.9999999999999998d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjai_bc = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibjcjai_bc = eom_ccsd_22_trans_aibjcjai_bc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjai_bc
    function eom_ccsd_22_trans_aibjcjai_ibc(t2, nocc, nactive, i, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjai_ibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, i, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,m,i) * tovov(m, c, i, e)
end do 
end do 



    eom_ccsd_22_trans_aibjcjai_ibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjai_ibc = eom_ccsd_22_trans_aibjcjai_ibc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjai_ibc
    function eom_ccsd_22_trans_aibjcjai_bjc(t2, nocc, nactive, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjai_bjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, j, j)
term(1) = term(1) + tvoov(b, j, j, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,j) * tovov(m, c, j, e)
term(3) = term(3) + t2(b,e,j,m) * tovov(m, c, j, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, j, c)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjcjai_bjc = 0.d+0
    do s = 0, 5
    eom_ccsd_22_trans_aibjcjai_bjc = eom_ccsd_22_trans_aibjcjai_bjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjai_bjc
    function eom_ccsd_22_trans_aibjcjai_abc(t2, nocc, nactive, a, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjai_abc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, c, a, a)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, c, m, a)
end do 
end do 



    eom_ccsd_22_trans_aibjcjai_abc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjai_abc = eom_ccsd_22_trans_aibjcjai_abc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjai_abc
    function eom_ccsd_22_trans_aibjcjai_ibjc(t2, nocc, nactive, i, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjai_ibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, e, i, c)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, c, i, e)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjai_ibjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjai_ibjc = eom_ccsd_22_trans_aibjcjai_ibjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjai_ibjc
    function eom_ccsd_22_trans_aibjcjai_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjai_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, j, c)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjai_abjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjai_abjc = eom_ccsd_22_trans_aibjcjai_abjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjai_abjc
    function eom_ccsd_22_trans_aibjcjai_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjai_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjai_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjai_aibc = eom_ccsd_22_trans_aibjcjai_aibc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjai_aibc
    function eom_ccsd_22_trans_aibjcjaj_ibjc(t2, nocc, nactive, i, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjaj_ibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, j, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, j, c)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,i) * tovov(m, c, j, e)
end do 
end do 



    eom_ccsd_22_trans_aibjcjaj_ibjc = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjcjaj_ibjc = eom_ccsd_22_trans_aibjcjaj_ibjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjaj_ibjc
    function eom_ccsd_22_trans_aibjcjaj_aibjc(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjaj_aibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, j, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjcjaj_aibjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjaj_aibjc = eom_ccsd_22_trans_aibjcjaj_aibjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjaj_aibjc
    function eom_ccsd_22_trans_aibjbibl_abjl(t2, nocc, nactive, a, b, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjbibl_abjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, l, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, l, b)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,j) * tovov(m, b, l, e)
end do 
end do 



    eom_ccsd_22_trans_aibjbibl_abjl = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjbibl_abjl = eom_ccsd_22_trans_aibjbibl_abjl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbibl_abjl
    function eom_ccsd_22_trans_aibjbibl_aibjl(t2, nocc, nactive, a, i, b, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjbibl_aibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, b, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, i, b)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjbibl_aibjl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbibl_aibjl = eom_ccsd_22_trans_aibjbibl_aibjl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbibl_aibjl
    function eom_ccsd_22_trans_aibibkbl_aibkl(t2, nocc, nactive, a, i, b, k, l) 
    real(F64) :: eom_ccsd_22_trans_aibibkbl_aibkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(l, b, k, e)
term(1) = term(1) + t2(a,e,i,i) * tovov(l, e, k, b)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibibkbl_aibkl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibibkbl_aibkl = eom_ccsd_22_trans_aibibkbl_aibkl + term(s)
    end do

    end function eom_ccsd_22_trans_aibibkbl_aibkl
    function eom_ccsd_22_trans_aibjbkbi_abjk(t2, nocc, nactive, a, b, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjbkbi_abjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, k, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, k, b)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,j) * tovov(m, b, k, e)
end do 
end do 



    eom_ccsd_22_trans_aibjbkbi_abjk = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjbkbi_abjk = eom_ccsd_22_trans_aibjbkbi_abjk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkbi_abjk
    function eom_ccsd_22_trans_aibjbkbi_aibjk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjbkbi_aibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, i, b)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, b, i, e)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjbkbi_aibjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbkbi_aibjk = eom_ccsd_22_trans_aibjbkbi_aibjk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkbi_aibjk
    function eom_ccsd_22_trans_aibjbjbl_aibl(t2, nocc, nactive, a, i, b, l) 
    real(F64) :: eom_ccsd_22_trans_aibjbjbl_aibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,e,m 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, b)
term(1) = term(1) + tvvoo(a, b, l, i)

term(1) = term(1) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, l, b)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,i) * tovov(m, e, l, b)
term(4) = term(4) + t2(a,e,i,m) * tovov(m, b, l, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, b, l, e)
end do 
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, l, b)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbjbl_aibl = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjbjbl_aibl = eom_ccsd_22_trans_aibjbjbl_aibl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjbl_aibl
    function eom_ccsd_22_trans_aibjbjbl_aibjl(t2, nocc, nactive, a, i, b, j, l) 
    real(F64) :: eom_ccsd_22_trans_aibjbjbl_aibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(l, b, j, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, j, b)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjbjbl_aibjl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbjbl_aibjl = eom_ccsd_22_trans_aibjbjbl_aibjl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjbl_aibjl
    function eom_ccsd_22_trans_aibjbkbk_aibjk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjbkbk_aibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, k, b)
end do 

term(0) = term(0) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aibjbkbk_aibjk = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjbkbk_aibjk = eom_ccsd_22_trans_aibjbkbk_aibjk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkbk_aibjk
    function eom_ccsd_22_trans_aibjbkbj_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_trans_aibjbkbj_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, k, i)
term(1) = term(1) + tvoov(a, i, k, b)

term(0) = term(0) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, k, b)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,i) * tovov(m, b, k, e)
term(4) = term(4) + t2(a,e,i,m) * tovov(m, b, k, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, e, k, b)
end do 
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, k, b)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbkbj_aibk = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjbkbj_aibk = eom_ccsd_22_trans_aibjbkbj_aibk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkbj_aibk
    function eom_ccsd_22_trans_aibjbkbj_aibjk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: eom_ccsd_22_trans_aibjbkbj_aibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(k, e, j, b)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, b, j, e)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjbkbj_aibjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbkbj_aibjk = eom_ccsd_22_trans_aibjbkbj_aibjk + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbkbj_aibjk
    function eom_ccsd_22_trans_aibibidl_aidl(t2, nocc, nactive, a, i, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibibidl_aidl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, l, i)
term(1) = term(1) + tvoov(a, i, l, d)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(l, d, i, e)
term(3) = term(3) + t2(a,e,i,i) * tovov(l, e, i, d)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, d, l, e)
term(5) = term(5) + t2(a,e,i,m) * tovov(m, d, l, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibibidl_aidl = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibibidl_aidl = eom_ccsd_22_trans_aibibidl_aidl + term(s)
    end do

    end function eom_ccsd_22_trans_aibibidl_aidl
    function eom_ccsd_22_trans_aibibidl_aibdl(t2, nocc, nactive, a, i, b, d, l) 
    real(F64) :: eom_ccsd_22_trans_aibibidl_aibdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, l, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, l, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, l, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, l, b)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibibidl_aibdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibibidl_aibdl = eom_ccsd_22_trans_aibibidl_aibdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibibidl_aibdl
    function eom_ccsd_22_trans_aibjbidi_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbidi_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, i, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,j) * tovov(i, e, i, d)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,j) * tovov(m, d, i, e)
end do 
end do 



    eom_ccsd_22_trans_aibjbidi_aijd = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjbidi_aijd = eom_ccsd_22_trans_aibjbidi_aijd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbidi_aijd
    function eom_ccsd_22_trans_aibjbidi_aibjd(t2, nocc, nactive, a, i, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbidi_aibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, i, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, i, b)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjbidi_aibjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbidi_aibjd = eom_ccsd_22_trans_aibjbidi_aibjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbidi_aibjd
    function eom_ccsd_22_trans_aibjbidj_abd(t2, nocc, nactive, a, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbidj_abd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, d 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, d, a, b)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, d, m, b)
end do 
end do 



    eom_ccsd_22_trans_aibjbidj_abd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbidj_abd = eom_ccsd_22_trans_aibjbidj_abd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbidj_abd
    function eom_ccsd_22_trans_aibjbidj_aid(t2, nocc, nactive, a, i, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbidj_aid 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d 
    integer :: s ,e,m,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, d)

term(0) = term(0) * (-0.9999999999999999d+0) 

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

term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbidj_aid = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjbidj_aid = eom_ccsd_22_trans_aibjbidj_aid + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbidj_aid
    function eom_ccsd_22_trans_aibjbidj_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbidj_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, d, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, e, i, d)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbidj_aijd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbidj_aijd = eom_ccsd_22_trans_aibjbidj_aijd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbidj_aijd
    function eom_ccsd_22_trans_aibjbidj_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbidj_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, b, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, b)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbidj_aibd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbidj_aibd = eom_ccsd_22_trans_aibjbidj_aibd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbidj_aibd
    function eom_ccsd_22_trans_aibjbidj_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbidj_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, b)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbidj_abjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbidj_abjd = eom_ccsd_22_trans_aibjbidj_abjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbidj_abjd
    function eom_ccsd_22_trans_aibibkdi_aikd(t2, nocc, nactive, a, i, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibibkdi_aikd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, d)
term(1) = term(1) + tvvoo(a, d, k, i)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(k, e, i, d)
term(3) = term(3) + t2(a,e,i,i) * tovov(k, d, i, e)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,i,m) * tovov(m, d, k, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, d, k, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, k, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibibkdi_aikd = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibibkdi_aikd = eom_ccsd_22_trans_aibibkdi_aikd + term(s)
    end do

    end function eom_ccsd_22_trans_aibibkdi_aikd
    function eom_ccsd_22_trans_aibibkdi_aibkd(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibibkdi_aibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, b, k, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, k, d)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, d, k, b)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, d, k, b)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibibkdi_aibkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibibkdi_aibkd = eom_ccsd_22_trans_aibibkdi_aibkd + term(s)
    end do

    end function eom_ccsd_22_trans_aibibkdi_aibkd
    function eom_ccsd_22_trans_aibibkdk_aikd(t2, nocc, nactive, a, i, k, d) 
    real(F64) :: eom_ccsd_22_trans_aibibkdk_aikd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(k, e, k, d)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibibkdk_aikd = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibibkdk_aikd = eom_ccsd_22_trans_aibibkdk_aikd + term(s)
    end do

    end function eom_ccsd_22_trans_aibibkdk_aikd
    function eom_ccsd_22_trans_aibjbjdi_ad(t2, nocc, nactive, a, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbjdi_ad 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, d)


do m = 1, nocc 
term(1) = term(1) + tvvoo(a, d, m, m)
term(2) = term(2) + tvoov(a, m, m, d)
end do 

term(1) = term(1) * (1.9999999999999998d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbjdi_ad = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibjbjdi_ad = eom_ccsd_22_trans_aibjbjdi_ad + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjdi_ad
    function eom_ccsd_22_trans_aibjbjdi_abd(t2, nocc, nactive, a, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbjdi_abd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, b, a, d)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, b, m, d)
end do 
end do 



    eom_ccsd_22_trans_aibjbjdi_abd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbjdi_abd = eom_ccsd_22_trans_aibjbjdi_abd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjdi_abd
    function eom_ccsd_22_trans_aibjbjdi_ajd(t2, nocc, nactive, a, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbjdi_ajd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, j, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,m,j) * tovov(m, d, j, e)
end do 
end do 



    eom_ccsd_22_trans_aibjbjdi_ajd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbjdi_ajd = eom_ccsd_22_trans_aibjbjdi_ajd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjdi_ajd
    function eom_ccsd_22_trans_aibjbjdi_aid(t2, nocc, nactive, a, i, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbjdi_aid 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, i, i)
term(1) = term(1) + tvoov(a, i, i, d)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, d, i, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, i, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjbjdi_aid = 0.d+0
    do s = 0, 5
    eom_ccsd_22_trans_aibjbjdi_aid = eom_ccsd_22_trans_aibjbjdi_aid + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjdi_aid
    function eom_ccsd_22_trans_aibjbjdi_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbjdi_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, e, i, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, d, i, e)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbjdi_aijd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbjdi_aijd = eom_ccsd_22_trans_aibjbjdi_aijd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjdi_aijd
    function eom_ccsd_22_trans_aibjbjdi_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbjdi_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, j, b)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbjdi_abjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbjdi_abjd = eom_ccsd_22_trans_aibjbjdi_abjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjdi_abjd
    function eom_ccsd_22_trans_aibjbjdi_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbjdi_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, b, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, b)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbjdi_aibd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbjdi_aibd = eom_ccsd_22_trans_aibjbjdi_aibd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjdi_aibd
    function eom_ccsd_22_trans_aibjbjdj_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbjdj_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, j, d)
term(1) = term(1) + tvvoo(a, d, j, i)

term(1) = term(1) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(j, e, j, d)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, j, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, j, d)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, d, j, e)
end do 
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, j, d)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbjdj_aijd = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjbjdj_aijd = eom_ccsd_22_trans_aibjbjdj_aijd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjdj_aijd
    function eom_ccsd_22_trans_aibjbjdj_aibjd(t2, nocc, nactive, a, i, b, j, d) 
    real(F64) :: eom_ccsd_22_trans_aibjbjdj_aibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, b, j, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, j, b)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjbjdj_aibjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbjdj_aibjd = eom_ccsd_22_trans_aibjbjdj_aibjd + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjdj_aibjd
    function eom_ccsd_22_trans_aibicicl_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibicicl_aibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibicicl_aibcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibicicl_aibcl = eom_ccsd_22_trans_aibicicl_aibcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibicicl_aibcl
    function eom_ccsd_22_trans_aibjcici_aibjc(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcici_aibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, i, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcici_aibjc = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjcici_aibjc = eom_ccsd_22_trans_aibjcici_aibjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcici_aibjc
    function eom_ccsd_22_trans_aibjcicj_abc(t2, nocc, nactive, a, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcicj_abc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, c, a, c)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, c, m, c)
end do 
end do 



    eom_ccsd_22_trans_aibjcicj_abc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcicj_abc = eom_ccsd_22_trans_aibjcicj_abc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcicj_abc
    function eom_ccsd_22_trans_aibjcicj_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcicj_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,n 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjcicj_aibc = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjcicj_aibc = eom_ccsd_22_trans_aibjcicj_aibc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcicj_aibc
    function eom_ccsd_22_trans_aibjcicj_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcicj_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,m,n 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjcicj_abjc = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjcicj_abjc = eom_ccsd_22_trans_aibjcicj_abjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcicj_abjc
    function eom_ccsd_22_trans_aibickci_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibickci_aibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, k, c)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, k, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibickci_aibck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibickci_aibck = eom_ccsd_22_trans_aibickci_aibck + term(s)
    end do

    end function eom_ccsd_22_trans_aibickci_aibck
    function eom_ccsd_22_trans_aibjcjci_abc(t2, nocc, nactive, a, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjci_abc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, c, a, c)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, c, m, c)
end do 
end do 



    eom_ccsd_22_trans_aibjcjci_abc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjci_abc = eom_ccsd_22_trans_aibjcjci_abc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjci_abc
    function eom_ccsd_22_trans_aibjcjci_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjci_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,m,n 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjcjci_abjc = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjcjci_abjc = eom_ccsd_22_trans_aibjcjci_abjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjci_abjc
    function eom_ccsd_22_trans_aibjcjci_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjci_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,n 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjcjci_aibc = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjcjci_aibc = eom_ccsd_22_trans_aibjcjci_aibc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjci_aibc
    function eom_ccsd_22_trans_aibjcjcj_aibjc(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjcj_aibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, j, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjcj_aibjc = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibjcjcj_aibjc = eom_ccsd_22_trans_aibjcjcj_aibjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjcj_aibjc
    function eom_ccsd_22_trans_aibicibl_aicl(t2, nocc, nactive, a, i, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibicibl_aicl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, l, i)
term(1) = term(1) + tvoov(a, i, l, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(l, c, i, e)
term(3) = term(3) + t2(a,e,i,i) * tovov(l, e, i, c)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, c, l, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, e, l, c)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, c, l, e)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibicibl_aicl = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibicibl_aicl = eom_ccsd_22_trans_aibicibl_aicl + term(s)
    end do

    end function eom_ccsd_22_trans_aibicibl_aicl
    function eom_ccsd_22_trans_aibicibl_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_trans_aibicibl_aibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, b)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, l, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, l, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibicibl_aibcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibicibl_aibcl = eom_ccsd_22_trans_aibicibl_aibcl + term(s)
    end do

    end function eom_ccsd_22_trans_aibicibl_aibcl
    function eom_ccsd_22_trans_aibjcibi_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcibi_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, i, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,j) * tovov(i, e, i, c)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,j) * tovov(m, c, i, e)
end do 
end do 



    eom_ccsd_22_trans_aibjcibi_aijc = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjcibi_aijc = eom_ccsd_22_trans_aibjcibi_aijc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcibi_aijc
    function eom_ccsd_22_trans_aibjcibi_aibjc(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcibi_aibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, i, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, i, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjcibi_aibjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcibi_aibjc = eom_ccsd_22_trans_aibjcibi_aibjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcibi_aibjc
    function eom_ccsd_22_trans_aibjcibj_ac(t2, nocc, nactive, a, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcibj_ac 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, c)


do m = 1, nocc 
term(1) = term(1) + tvvoo(a, c, m, m)
term(2) = term(2) + tvoov(a, m, m, c)
end do 

term(1) = term(1) * (1.9999999999999998d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcibj_ac = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibjcibj_ac = eom_ccsd_22_trans_aibjcibj_ac + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcibj_ac
    function eom_ccsd_22_trans_aibjcibj_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcibj_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, i, i)
term(1) = term(1) + tvoov(a, i, i, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, c, i, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, c, i, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjcibj_aic = 0.d+0
    do s = 0, 5
    eom_ccsd_22_trans_aibjcibj_aic = eom_ccsd_22_trans_aibjcibj_aic + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcibj_aic
    function eom_ccsd_22_trans_aibjcibj_ajc(t2, nocc, nactive, a, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcibj_ajc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, j, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,m,j) * tovov(m, c, j, e)
end do 
end do 



    eom_ccsd_22_trans_aibjcibj_ajc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcibj_ajc = eom_ccsd_22_trans_aibjcibj_ajc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcibj_ajc
    function eom_ccsd_22_trans_aibjcibj_abc(t2, nocc, nactive, a, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcibj_abc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, b, a, c)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, b, m, c)
end do 
end do 



    eom_ccsd_22_trans_aibjcibj_abc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcibj_abc = eom_ccsd_22_trans_aibjcibj_abc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcibj_abc
    function eom_ccsd_22_trans_aibjcibj_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcibj_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, c, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, e, i, c)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcibj_aijc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcibj_aijc = eom_ccsd_22_trans_aibjcibj_aijc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcibj_aijc
    function eom_ccsd_22_trans_aibjcibj_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcibj_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, c)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcibj_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcibj_aibc = eom_ccsd_22_trans_aibjcibj_aibc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcibj_aibc
    function eom_ccsd_22_trans_aibjcibj_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcibj_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcibj_abjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcibj_abjc = eom_ccsd_22_trans_aibjcibj_abjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcibj_abjc
    function eom_ccsd_22_trans_aibickbi_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibickbi_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, k, i)
term(1) = term(1) + tvoov(a, i, k, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(k, e, i, c)
term(3) = term(3) + t2(a,e,i,i) * tovov(k, c, i, e)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, c, k, e)
term(5) = term(5) + t2(a,e,i,m) * tovov(m, c, k, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibickbi_aick = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibickbi_aick = eom_ccsd_22_trans_aibickbi_aick + term(s)
    end do

    end function eom_ccsd_22_trans_aibickbi_aick
    function eom_ccsd_22_trans_aibickbi_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibickbi_aibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, k, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, k, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, k, c)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, b, k, c)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibickbi_aibck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibickbi_aibck = eom_ccsd_22_trans_aibickbi_aibck + term(s)
    end do

    end function eom_ccsd_22_trans_aibickbi_aibck
    function eom_ccsd_22_trans_aibickbk_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_ccsd_22_trans_aibickbk_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(k, e, k, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibickbk_aick = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibickbk_aick = eom_ccsd_22_trans_aibickbk_aick + term(s)
    end do

    end function eom_ccsd_22_trans_aibickbk_aick
    function eom_ccsd_22_trans_aibjcjbi_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjbi_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,e,m,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, c)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,m,i) * tovov(m, e, i, c)
term(2) = term(2) + t2(a,e,i,m) * tovov(m, c, i, e)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjbi_aic = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjcjbi_aic = eom_ccsd_22_trans_aibjcjbi_aic + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjbi_aic
    function eom_ccsd_22_trans_aibjcjbi_abc(t2, nocc, nactive, a, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjbi_abc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, c, a, b)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, c, m, b)
end do 
end do 



    eom_ccsd_22_trans_aibjcjbi_abc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjbi_abc = eom_ccsd_22_trans_aibjcjbi_abc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjbi_abc
    function eom_ccsd_22_trans_aibjcjbi_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjbi_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, e, i, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, c, i, e)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjbi_aijc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjbi_aijc = eom_ccsd_22_trans_aibjcjbi_aijc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjbi_aijc
    function eom_ccsd_22_trans_aibjcjbi_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjbi_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, c)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjbi_abjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjbi_abjc = eom_ccsd_22_trans_aibjcjbi_abjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjbi_abjc
    function eom_ccsd_22_trans_aibjcjbi_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjbi_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,e,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjbi_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjbi_aibc = eom_ccsd_22_trans_aibjcjbi_aibc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjbi_aibc
    function eom_ccsd_22_trans_aibjcjbj_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjbj_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, j, i)
term(1) = term(1) + tvoov(a, i, j, c)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(j, e, j, c)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,i) * tovov(m, c, j, e)
term(4) = term(4) + t2(a,e,i,m) * tovov(m, c, j, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, e, j, c)
end do 
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, j, c)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjcjbj_aijc = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjcjbj_aijc = eom_ccsd_22_trans_aibjcjbj_aijc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjbj_aijc
    function eom_ccsd_22_trans_aibjcjbj_aibjc(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: eom_ccsd_22_trans_aibjcjbj_aibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, j, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjcjbj_aibjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjcjbj_aibjc = eom_ccsd_22_trans_aibjcjbj_aibjc + term(s)
    end do

    end function eom_ccsd_22_trans_aibjcjbj_aibjc
    function eom_ccsd_22_trans_aibicidi_abcd(t2, nocc, nactive, a, b, c, d) 
    real(F64) :: eom_ccsd_22_trans_aibicidi_abcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, d, a, c)
term(1) = term(1) + read_ftvvvv(b, c, a, d)


do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,n) * tovov(n, d, m, c)
term(3) = term(3) + t2(a,b,m,n) * tovov(n, c, m, d)
end do 
end do 



    eom_ccsd_22_trans_aibicidi_abcd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibicidi_abcd = eom_ccsd_22_trans_aibicidi_abcd + term(s)
    end do

    end function eom_ccsd_22_trans_aibicidi_abcd
    function eom_ccsd_22_trans_aibicidi_aibcd(t2, nocc, nactive, a, i, b, c, d) 
    real(F64) :: eom_ccsd_22_trans_aibicidi_aibcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, i, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, i, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibicidi_aibcd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibicidi_aibcd = eom_ccsd_22_trans_aibicidi_aibcd + term(s)
    end do

    end function eom_ccsd_22_trans_aibicidi_aibcd
    function eom_ccsd_22_trans_aiajaial_jl(t2, nocc, nactive, j, l) 
    real(F64) :: eom_ccsd_22_trans_aiajaial_jl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(l, j)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + toooo(m, j, l, m)
term(2) = term(2) + toooo(m, m, l, j)
end do 

term(2) = term(2) * (-1.9999999999999998d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,j,m) * tovov(m, e, l, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,j,m) * tovov(m, f, l, e)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajaial_jl = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aiajaial_jl = eom_ccsd_22_trans_aiajaial_jl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaial_jl
    function eom_ccsd_22_trans_aiajaial_ajl(t2, nocc, nactive, a, j, l) 
    real(F64) :: eom_ccsd_22_trans_aiajaial_ajl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, l, j)
term(1) = term(1) + tvoov(a, j, l, a)

term(0) = term(0) * (-1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, l, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, a, l, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, a, l, e)
term(5) = term(5) + t2(a,e,m,j) * tovov(m, e, l, a)
end do 
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,j,m) * tovov(m, e, l, a)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajaial_ajl = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiajaial_ajl = eom_ccsd_22_trans_aiajaial_ajl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaial_ajl
    function eom_ccsd_22_trans_aiajaial_ijl(t2, nocc, nactive, i, j, l) 
    real(F64) :: eom_ccsd_22_trans_aiajaial_ijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, i, i, j)
term(1) = term(1) + toooo(l, j, i, i)


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(l, f, i, e)
end do 
end do 


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(l, e, i, f)
end do 
end do 



    eom_ccsd_22_trans_aiajaial_ijl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajaial_ijl = eom_ccsd_22_trans_aiajaial_ijl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaial_ijl
    function eom_ccsd_22_trans_aiajaial_aijl(t2, nocc, nactive, a, i, j, l) 
    real(F64) :: eom_ccsd_22_trans_aiajaial_aijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, a, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, a, i, e)
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, i, a)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, i, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajaial_aijl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajaial_aijl = eom_ccsd_22_trans_aiajaial_aijl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaial_aijl
    function eom_ccsd_22_trans_aiaiakal_ikl(t2, nocc, nactive, i, k, l) 
    real(F64) :: eom_ccsd_22_trans_aiaiakal_ikl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, k, l 
    integer :: s ,e,f 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, i, k, i)


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,i) * tovov(l, f, k, e)
end do 
end do 

term(1) = term(1) * (0.5000000000000001d+0) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,i) * tovov(l, e, k, f)
end do 
end do 

term(2) = term(2) * (0.5000000000000001d+0) 


    eom_ccsd_22_trans_aiaiakal_ikl = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aiaiakal_ikl = eom_ccsd_22_trans_aiaiakal_ikl + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiakal_ikl
    function eom_ccsd_22_trans_aiaiakal_aikl(t2, nocc, nactive, a, i, k, l) 
    real(F64) :: eom_ccsd_22_trans_aiaiakal_aikl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, l 
    integer :: s ,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(l, a, k, e)
term(1) = term(1) + t2(a,e,i,i) * tovov(l, e, k, a)
end do 

term(0) = term(0) * (-0.9999999999999998d+0) 
term(1) = term(1) * (-1.0000000000000007d+0) 


    eom_ccsd_22_trans_aiaiakal_aikl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiaiakal_aikl = eom_ccsd_22_trans_aiaiakal_aikl + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiakal_aikl
    function eom_ccsd_22_trans_aiajakai_jk(t2, nocc, nactive, j, k) 
    real(F64) :: eom_ccsd_22_trans_aiajakai_jk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(k, j)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + toooo(m, j, k, m)
term(2) = term(2) + toooo(m, m, k, j)
end do 

term(2) = term(2) * (-1.9999999999999998d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,j,m) * tovov(m, e, k, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,j,m) * tovov(m, f, k, e)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajakai_jk = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aiajakai_jk = eom_ccsd_22_trans_aiajakai_jk + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakai_jk
    function eom_ccsd_22_trans_aiajakai_ajk(t2, nocc, nactive, a, j, k) 
    real(F64) :: eom_ccsd_22_trans_aiajakai_ajk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, k, j)
term(1) = term(1) + tvoov(a, j, k, a)

term(0) = term(0) * (-1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, k, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,j) * tovov(m, a, k, e)
term(4) = term(4) + t2(a,e,j,m) * tovov(m, a, k, e)
term(5) = term(5) + t2(a,e,m,j) * tovov(m, e, k, a)
end do 
end do 

term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,j,m) * tovov(m, e, k, a)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajakai_ajk = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiajakai_ajk = eom_ccsd_22_trans_aiajakai_ajk + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakai_ajk
    function eom_ccsd_22_trans_aiajakai_ijk(t2, nocc, nactive, i, j, k) 
    real(F64) :: eom_ccsd_22_trans_aiajakai_ijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(k, j, i, i)
term(1) = term(1) + toooo(k, i, i, j)


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(k, e, i, f)
end do 
end do 


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(k, f, i, e)
end do 
end do 



    eom_ccsd_22_trans_aiajakai_ijk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajakai_ijk = eom_ccsd_22_trans_aiajakai_ijk + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakai_ijk
    function eom_ccsd_22_trans_aiajakai_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_ccsd_22_trans_aiajakai_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, i, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, i, a)
term(2) = term(2) + t2(a,e,j,i) * tovov(k, a, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, a, i, e)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajakai_aijk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajakai_aijk = eom_ccsd_22_trans_aiajakai_aijk + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakai_aijk
    function eom_ccsd_22_trans_aiajajal_il(t2, nocc, nactive, i, l) 
    real(F64) :: eom_ccsd_22_trans_aiajajal_il 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(l, i)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + toooo(m, i, l, m)
term(2) = term(2) + toooo(m, m, l, i)
end do 

term(2) = term(2) * (-1.9999999999999998d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,m) * tovov(m, e, l, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,m) * tovov(m, f, l, e)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajajal_il = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aiajajal_il = eom_ccsd_22_trans_aiajajal_il + term(s)
    end do

    end function eom_ccsd_22_trans_aiajajal_il
    function eom_ccsd_22_trans_aiajajal_ail(t2, nocc, nactive, a, i, l) 
    real(F64) :: eom_ccsd_22_trans_aiajajal_ail 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, l, i)
term(1) = term(1) + tvoov(a, i, l, a)

term(0) = term(0) * (-1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, l, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,i) * tovov(m, e, l, a)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, a, l, e)
term(5) = term(5) + t2(a,e,i,m) * tovov(m, a, l, e)
end do 
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajajal_ail = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiajajal_ail = eom_ccsd_22_trans_aiajajal_ail + term(s)
    end do

    end function eom_ccsd_22_trans_aiajajal_ail
    function eom_ccsd_22_trans_aiajajal_ijl(t2, nocc, nactive, i, j, l) 
    real(F64) :: eom_ccsd_22_trans_aiajajal_ijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, i, j, j)
term(1) = term(1) + toooo(l, j, j, i)


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(l, f, j, e)
end do 
end do 


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(l, e, j, f)
end do 
end do 



    eom_ccsd_22_trans_aiajajal_ijl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajajal_ijl = eom_ccsd_22_trans_aiajajal_ijl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajajal_ijl
    function eom_ccsd_22_trans_aiajajal_aijl(t2, nocc, nactive, a, i, j, l) 
    real(F64) :: eom_ccsd_22_trans_aiajajal_aijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, a, j, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, a, j, e)
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, j, a)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, j, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajajal_aijl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajajal_aijl = eom_ccsd_22_trans_aiajajal_aijl + term(s)
    end do

    end function eom_ccsd_22_trans_aiajajal_aijl
    function eom_ccsd_22_trans_aiajakak_ijk(t2, nocc, nactive, i, j, k) 
    real(F64) :: eom_ccsd_22_trans_aiajakak_ijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, k 
    integer :: s ,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(k, j, k, i)

term(0) = term(0) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,j) * tovov(k, f, k, e)
end do 
end do 

term(1) = term(1) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajakak_ijk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajakak_ijk = eom_ccsd_22_trans_aiajakak_ijk + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakak_ijk
    function eom_ccsd_22_trans_aiajakak_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_ccsd_22_trans_aiajakak_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, k, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, k, a)
end do 

term(0) = term(0) * (-2.000000000000001d+0) 
term(1) = term(1) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aiajakak_aijk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajakak_aijk = eom_ccsd_22_trans_aiajakak_aijk + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakak_aijk
    function eom_ccsd_22_trans_aiajakaj_ik(t2, nocc, nactive, i, k) 
    real(F64) :: eom_ccsd_22_trans_aiajakaj_ik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(k, i)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + toooo(m, i, k, m)
term(2) = term(2) + toooo(m, m, k, i)
end do 

term(2) = term(2) * (-1.9999999999999998d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,m) * tovov(m, e, k, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,m) * tovov(m, f, k, e)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajakaj_ik = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aiajakaj_ik = eom_ccsd_22_trans_aiajakaj_ik + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakaj_ik
    function eom_ccsd_22_trans_aiajakaj_aik(t2, nocc, nactive, a, i, k) 
    real(F64) :: eom_ccsd_22_trans_aiajakaj_aik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, k, i)
term(1) = term(1) + tvoov(a, i, k, a)

term(0) = term(0) * (-1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, k, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,i) * tovov(m, a, k, e)
term(4) = term(4) + t2(a,e,i,m) * tovov(m, a, k, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajakaj_aik = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiajakaj_aik = eom_ccsd_22_trans_aiajakaj_aik + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakaj_aik
    function eom_ccsd_22_trans_aiajakaj_ijk(t2, nocc, nactive, i, j, k) 
    real(F64) :: eom_ccsd_22_trans_aiajakaj_ijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(k, j, j, i)
term(1) = term(1) + toooo(k, i, j, j)


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(k, e, j, f)
end do 
end do 


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(k, f, j, e)
end do 
end do 



    eom_ccsd_22_trans_aiajakaj_ijk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajakaj_ijk = eom_ccsd_22_trans_aiajakaj_ijk + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakaj_ijk
    function eom_ccsd_22_trans_aiajakaj_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_ccsd_22_trans_aiajakaj_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, j, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, j, a)
term(2) = term(2) + t2(a,e,j,i) * tovov(k, a, j, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, a, j, e)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajakaj_aijk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajakaj_aijk = eom_ccsd_22_trans_aiajakaj_aijk + term(s)
    end do

    end function eom_ccsd_22_trans_aiajakaj_aijk
    function eom_ccsd_22_trans_aiaiaidl_aidl(t2, nocc, nactive, a, i, d, l) 
    real(F64) :: eom_ccsd_22_trans_aiaiaidl_aidl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, l, i)
term(1) = term(1) + tvoov(a, i, l, d)

term(0) = term(0) * (-0.9999999999999998d+0) 
term(1) = term(1) * (1.9999999999999996d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(l, d, i, e)
term(3) = term(3) + t2(a,e,i,i) * tovov(l, e, i, d)
end do 

term(2) = term(2) * (-2.000000000000001d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,a,i,m) * tovov(m, a, l, d)
term(5) = term(5) + t2(a,a,i,m) * tovov(m, d, l, a)
end do 

term(4) = term(4) * (-2.000000000000001d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,i) * tovov(m, d, l, e)
term(7) = term(7) + t2(a,e,i,m) * tovov(m, d, l, e)
term(8) = term(8) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * (-2.000000000000001d+0) 
term(8) = term(8) * (-2.000000000000001d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(9) = term(9) * (4.000000000000002d+0) 


    eom_ccsd_22_trans_aiaiaidl_aidl = 0.d+0
    do s = 0, 9
    eom_ccsd_22_trans_aiaiaidl_aidl = eom_ccsd_22_trans_aiaiaidl_aidl + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiaidl_aidl
    function eom_ccsd_22_trans_aiajaidi_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_trans_aiajaidi_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, i, d)
term(1) = term(1) + tvvoo(a, d, i, j)

term(1) = term(1) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(i, e, i, d)
term(3) = term(3) + t2(a,e,i,j) * tovov(i, e, i, d)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,a,j,m) * tovov(m, a, i, d)
term(5) = term(5) + t2(a,a,j,m) * tovov(m, d, i, a)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,j,m) * tovov(m, d, i, e)
term(7) = term(7) + t2(a,e,m,j) * tovov(m, d, i, e)
term(8) = term(8) + t2(a,e,m,j) * tovov(m, e, i, d)
end do 
end do 

term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (2.0000000000000004d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,j,m) * tovov(m, e, i, d)
end do 
end do 

term(9) = term(9) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajaidi_aijd = 0.d+0
    do s = 0, 9
    eom_ccsd_22_trans_aiajaidi_aijd = eom_ccsd_22_trans_aiajaidi_aijd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaidi_aijd
    function eom_ccsd_22_trans_aiajaidj_ad(t2, nocc, nactive, a, d) 
    real(F64) :: eom_ccsd_22_trans_aiajaidj_ad 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, d)
term(1) = term(1) + read_ftvvvv(a, d, a, a)


do m = 1, nocc 
term(2) = term(2) + tvvoo(a, d, m, m)
term(3) = term(3) + tvoov(a, m, m, d)
end do 

term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-0.9999999999999999d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,a,m,n) * tovov(n, d, m, a)
end do 
end do 


do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajaidj_ad = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiajaidj_ad = eom_ccsd_22_trans_aiajaidj_ad + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaidj_ad
    function eom_ccsd_22_trans_aiajaidj_aid(t2, nocc, nactive, a, i, d) 
    real(F64) :: eom_ccsd_22_trans_aiajaidj_aid 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, d)
term(1) = term(1) + tvvoo(a, d, i, i)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, i, d)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, d, i, a)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,i,m) * tovov(m, d, i, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, d, i, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajaidj_aid = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajaidj_aid = eom_ccsd_22_trans_aiajaidj_aid + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaidj_aid
    function eom_ccsd_22_trans_aiajaidj_ajd(t2, nocc, nactive, a, j, d) 
    real(F64) :: eom_ccsd_22_trans_aiajaidj_ajd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, j, j)
term(1) = term(1) + tvoov(a, j, j, d)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, j, d)
term(3) = term(3) + t2(a,a,j,m) * tovov(m, d, j, a)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,j) * tovov(m, d, j, e)
term(5) = term(5) + t2(a,e,j,m) * tovov(m, d, j, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, e, j, d)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aiajaidj_ajd = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajaidj_ajd = eom_ccsd_22_trans_aiajaidj_ajd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaidj_ajd
    function eom_ccsd_22_trans_aiajaidj_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_trans_aiajaidj_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, d, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, d, i, e)
term(2) = term(2) + t2(a,e,j,i) * tovov(j, e, i, d)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, e, i, d)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajaidj_aijd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajaidj_aijd = eom_ccsd_22_trans_aiajaidj_aijd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaidj_aijd
    function eom_ccsd_22_trans_aiaiakdi_aikd(t2, nocc, nactive, a, i, k, d) 
    real(F64) :: eom_ccsd_22_trans_aiaiakdi_aikd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, d)
term(1) = term(1) + tvvoo(a, d, k, i)

term(0) = term(0) * (-0.9999999999999998d+0) 
term(1) = term(1) * (-0.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(k, e, i, d)
term(3) = term(3) + t2(a,e,i,i) * tovov(k, d, i, e)
end do 

term(2) = term(2) * (-2.000000000000001d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,a,i,m) * tovov(m, a, k, d)
term(5) = term(5) + t2(a,a,i,m) * tovov(m, d, k, a)
end do 

term(5) = term(5) * (-2.000000000000001d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, d, k, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, d, k, e)
term(8) = term(8) + t2(a,e,m,i) * tovov(m, e, k, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(9) = term(9) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aiaiakdi_aikd = 0.d+0
    do s = 0, 9
    eom_ccsd_22_trans_aiaiakdi_aikd = eom_ccsd_22_trans_aiaiakdi_aikd + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiakdi_aikd
    function eom_ccsd_22_trans_aiaiakdk_aikd(t2, nocc, nactive, a, i, k, d) 
    real(F64) :: eom_ccsd_22_trans_aiaiakdk_aikd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(k, e, k, d)
end do 

term(0) = term(0) * (-1.0000000000000007d+0) 


    eom_ccsd_22_trans_aiaiakdk_aikd = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiaiakdk_aikd = eom_ccsd_22_trans_aiaiakdk_aikd + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiakdk_aikd
    function eom_ccsd_22_trans_aiajajdi_ad(t2, nocc, nactive, a, d) 
    real(F64) :: eom_ccsd_22_trans_aiajajdi_ad 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, d)
term(1) = term(1) + read_ftvvvv(a, d, a, a)


do m = 1, nocc 
term(2) = term(2) + tvvoo(a, d, m, m)
term(3) = term(3) + tvoov(a, m, m, d)
end do 

term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-0.9999999999999999d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,a,m,n) * tovov(n, a, m, d)
end do 
end do 


do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajajdi_ad = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiajajdi_ad = eom_ccsd_22_trans_aiajajdi_ad + term(s)
    end do

    end function eom_ccsd_22_trans_aiajajdi_ad
    function eom_ccsd_22_trans_aiajajdi_ajd(t2, nocc, nactive, a, j, d) 
    real(F64) :: eom_ccsd_22_trans_aiajajdi_ajd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, j, d)
term(1) = term(1) + tvvoo(a, d, j, j)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, j, d)
term(3) = term(3) + t2(a,a,j,m) * tovov(m, d, j, a)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,j,m) * tovov(m, d, j, e)
term(5) = term(5) + t2(a,e,m,j) * tovov(m, d, j, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, e, j, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajajdi_ajd = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajajdi_ajd = eom_ccsd_22_trans_aiajajdi_ajd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajajdi_ajd
    function eom_ccsd_22_trans_aiajajdi_aid(t2, nocc, nactive, a, i, d) 
    real(F64) :: eom_ccsd_22_trans_aiajajdi_aid 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, i, i)
term(1) = term(1) + tvoov(a, i, i, d)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, i, d)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, d, i, a)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, d, i, e)
term(5) = term(5) + t2(a,e,i,m) * tovov(m, d, i, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aiajajdi_aid = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajajdi_aid = eom_ccsd_22_trans_aiajajdi_aid + term(s)
    end do

    end function eom_ccsd_22_trans_aiajajdi_aid
    function eom_ccsd_22_trans_aiajajdi_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_trans_aiajajdi_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, e, i, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, e, i, d)
term(2) = term(2) + t2(a,e,j,i) * tovov(j, d, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, d, i, e)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajajdi_aijd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajajdi_aijd = eom_ccsd_22_trans_aiajajdi_aijd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajajdi_aijd
    function eom_ccsd_22_trans_aiajajdj_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_trans_aiajajdj_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, j, d)
term(1) = term(1) + tvvoo(a, d, j, i)

term(1) = term(1) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(j, e, j, d)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, e, j, d)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,a,i,m) * tovov(m, a, j, d)
term(5) = term(5) + t2(a,a,i,m) * tovov(m, d, j, a)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, d, j, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, d, j, e)
term(8) = term(8) + t2(a,e,m,i) * tovov(m, e, j, d)
end do 
end do 

term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (2.0000000000000004d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,i,m) * tovov(m, e, j, d)
end do 
end do 

term(9) = term(9) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajajdj_aijd = 0.d+0
    do s = 0, 9
    eom_ccsd_22_trans_aiajajdj_aijd = eom_ccsd_22_trans_aiajajdj_aijd + term(s)
    end do

    end function eom_ccsd_22_trans_aiajajdj_aijd
    function eom_ccsd_22_trans_aibiaial_aibl(t2, nocc, nactive, a, i, b, l) 
    real(F64) :: eom_ccsd_22_trans_aibiaial_aibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,e,m 
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, l, i)
term(1) = term(1) + tvoov(b, i, l, a)

term(0) = term(0) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(l, a, i, e)
term(3) = term(3) + t2(b,e,i,i) * tovov(l, e, i, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,i) * tovov(m, a, l, a)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, a, l, a)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, a, l, e)
term(7) = term(7) + t2(b,e,m,i) * tovov(m, e, l, a)
term(8) = term(8) + t2(b,e,m,i) * tovov(m, a, l, e)
end do 
end do 

term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(9) = term(9) + t2(b,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(9) = term(9) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiaial_aibl = 0.d+0
    do s = 0, 9
    eom_ccsd_22_trans_aibiaial_aibl = eom_ccsd_22_trans_aibiaial_aibl + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaial_aibl
    function eom_ccsd_22_trans_aibjaiai_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaiai_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, i, j)
term(1) = term(1) + tvoov(b, j, i, a)

term(0) = term(0) * (-1.9999999999999998d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(i, e, i, a)
end do 

term(2) = term(2) * (-2.000000000000001d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,j) * tovov(m, a, i, a)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,j) * tovov(m, a, i, e)
term(5) = term(5) + t2(b,e,j,m) * tovov(m, a, i, e)
term(6) = term(6) + t2(b,e,m,j) * tovov(m, e, i, a)
end do 
end do 

term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,j,m) * tovov(m, e, i, a)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjaiai_aibj = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibjaiai_aibj = eom_ccsd_22_trans_aibjaiai_aibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaiai_aibj
    function eom_ccsd_22_trans_aibjaiaj_ab(t2, nocc, nactive, a, b) 
    real(F64) :: eom_ccsd_22_trans_aibjaiaj_ab 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(b, a)
term(1) = term(1) + read_ftvvvv(b, a, a, a)


do m = 1, nocc 
term(2) = term(2) + tvvoo(b, a, m, m)
term(3) = term(3) + tvoov(b, m, m, a)
end do 

term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-0.9999999999999999d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,n) * tovov(n, a, m, a)
end do 
end do 


do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,m,n) * tovov(n, a, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,m,n) * tovov(n, e, m, a)
end do 
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaiaj_ab = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjaiaj_ab = eom_ccsd_22_trans_aibjaiaj_ab + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaiaj_ab
    function eom_ccsd_22_trans_aibjaiaj_abj(t2, nocc, nactive, a, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaiaj_abj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, j, a)
term(1) = term(1) + tvvoo(b, a, j, j)

term(1) = term(1) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, j, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, a, j, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, j, a)
term(5) = term(5) + t2(b,e,m,j) * tovov(m, a, j, e)
end do 
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,m) * tovov(m, e, j, a)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaiaj_abj = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjaiaj_abj = eom_ccsd_22_trans_aibjaiaj_abj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaiaj_abj
    function eom_ccsd_22_trans_aibjaiaj_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_trans_aibjaiaj_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,m,e,n 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, i, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, a)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,i) * tovov(m, a, i, e)
end do 
end do 



    eom_ccsd_22_trans_aibjaiaj_aib = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjaiaj_aib = eom_ccsd_22_trans_aibjaiaj_aib + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaiaj_aib
    function eom_ccsd_22_trans_aibjaiaj_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaiaj_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, a, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, i, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjaiaj_aibj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaiaj_aibj = eom_ccsd_22_trans_aibjaiaj_aibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaiaj_aibj
    function eom_ccsd_22_trans_aibiakai_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_trans_aibiakai_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, k, i)
term(1) = term(1) + tvoov(b, i, k, a)

term(0) = term(0) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(k, e, i, a)
term(3) = term(3) + t2(b,e,i,i) * tovov(k, a, i, e)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, k, a)
term(5) = term(5) + t2(a,b,m,i) * tovov(m, a, k, a)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,m,i) * tovov(m, a, k, e)
term(7) = term(7) + t2(b,e,i,m) * tovov(m, a, k, e)
term(8) = term(8) + t2(b,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(9) = term(9) + t2(b,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(9) = term(9) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiakai_aibk = 0.d+0
    do s = 0, 9
    eom_ccsd_22_trans_aibiakai_aibk = eom_ccsd_22_trans_aibiakai_aibk + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakai_aibk
    function eom_ccsd_22_trans_aibiakak_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_trans_aibiakak_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(k, e, k, a)
end do 

term(0) = term(0) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aibiakak_aibk = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibiakak_aibk = eom_ccsd_22_trans_aibiakak_aibk + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakak_aibk
    function eom_ccsd_22_trans_aibjajai_ab(t2, nocc, nactive, a, b) 
    real(F64) :: eom_ccsd_22_trans_aibjajai_ab 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(b, a)
term(1) = term(1) + read_ftvvvv(b, a, a, a)


do m = 1, nocc 
term(2) = term(2) + tvvoo(b, a, m, m)
term(3) = term(3) + tvoov(b, m, m, a)
end do 

term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-0.9999999999999999d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,n) * tovov(n, a, m, a)
end do 
end do 


do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,m,n) * tovov(n, a, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,m,n) * tovov(n, e, m, a)
end do 
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajai_ab = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjajai_ab = eom_ccsd_22_trans_aibjajai_ab + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajai_ab
    function eom_ccsd_22_trans_aibjajai_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_trans_aibjajai_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,m,e,n 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, i, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, a)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,i) * tovov(m, a, i, e)
end do 
end do 



    eom_ccsd_22_trans_aibjajai_aib = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjajai_aib = eom_ccsd_22_trans_aibjajai_aib + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajai_aib
    function eom_ccsd_22_trans_aibjajai_abj(t2, nocc, nactive, a, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjajai_abj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, j, j)
term(1) = term(1) + tvoov(b, j, j, a)

term(0) = term(0) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, j, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,j) * tovov(m, a, j, e)
term(4) = term(4) + t2(b,e,j,m) * tovov(m, a, j, e)
term(5) = term(5) + t2(b,e,m,j) * tovov(m, e, j, a)
end do 
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,m) * tovov(m, e, j, a)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajai_abj = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjajai_abj = eom_ccsd_22_trans_aibjajai_abj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajai_abj
    function eom_ccsd_22_trans_aibjajai_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjajai_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, e, i, a)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, a, i, e)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjajai_aibj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajai_aibj = eom_ccsd_22_trans_aibjajai_aibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajai_aibj
    function eom_ccsd_22_trans_aibjajaj_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjajaj_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, j, i)

term(0) = term(0) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, j, a)
end do 

term(1) = term(1) * (-2.000000000000001d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, j, a)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,i) * tovov(m, a, j, e)
end do 
end do 

term(3) = term(3) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajaj_aibj = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjajaj_aibj = eom_ccsd_22_trans_aibjajaj_aibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajaj_aibj
    function eom_ccsd_22_trans_aibiaibl_il(t2, nocc, nactive, i, l) 
    real(F64) :: eom_ccsd_22_trans_aibiaibl_il 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(l, i)
term(1) = term(1) + toooo(l, i, i, i)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(2) = term(2) + toooo(m, i, l, m)
term(3) = term(3) + toooo(m, m, l, i)
end do 

term(3) = term(3) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,i) * tovov(l, f, i, e)
end do 
end do 


do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(e,f,i,m) * tovov(m, e, l, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(6) = term(6) + t2(e,f,i,m) * tovov(m, f, l, e)
end do 
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiaibl_il = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibiaibl_il = eom_ccsd_22_trans_aibiaibl_il + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaibl_il
    function eom_ccsd_22_trans_aibiaibl_ail(t2, nocc, nactive, a, i, l) 
    real(F64) :: eom_ccsd_22_trans_aibiaibl_ail 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, l, i)
term(1) = term(1) + tvoov(a, i, l, a)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(l, a, i, e)
term(3) = term(3) + t2(a,e,i,i) * tovov(l, e, i, a)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, a, l, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, e, l, a)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, a, l, e)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiaibl_ail = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibiaibl_ail = eom_ccsd_22_trans_aibiaibl_ail + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaibl_ail
    function eom_ccsd_22_trans_aibiaibl_ibl(t2, nocc, nactive, i, b, l) 
    real(F64) :: eom_ccsd_22_trans_aibiaibl_ibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, b, l, i)
term(1) = term(1) + tvoov(b, i, l, b)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(l, b, i, e)
term(3) = term(3) + t2(b,e,i,i) * tovov(l, e, i, b)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,i) * tovov(m, b, l, e)
term(5) = term(5) + t2(b,e,i,m) * tovov(m, b, l, e)
term(6) = term(6) + t2(b,e,m,i) * tovov(m, e, l, b)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, l, b)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibiaibl_ibl = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibiaibl_ibl = eom_ccsd_22_trans_aibiaibl_ibl + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaibl_ibl
    function eom_ccsd_22_trans_aibiaibl_aibl(t2, nocc, nactive, a, i, b, l) 
    real(F64) :: eom_ccsd_22_trans_aibiaibl_aibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, l, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, l, b)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, l, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, l, a)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiaibl_aibl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibiaibl_aibl = eom_ccsd_22_trans_aibiaibl_aibl + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaibl_aibl
    function eom_ccsd_22_trans_aibjaibi_ij(t2, nocc, nactive, i, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaibi_ij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j 
    integer :: s ,m,e,f 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(i, j)
term(1) = term(1) + toooo(i, j, i, i)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(2) = term(2) + toooo(m, j, i, m)
term(3) = term(3) + toooo(m, m, i, j)
end do 

term(3) = term(3) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,j) * tovov(i, f, i, e)
end do 
end do 


do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(e,f,j,m) * tovov(m, e, i, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(6) = term(6) + t2(e,f,j,m) * tovov(m, f, i, e)
end do 
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibi_ij = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjaibi_ij = eom_ccsd_22_trans_aibjaibi_ij + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibi_ij
    function eom_ccsd_22_trans_aibjaibi_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaibi_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,m,e,f 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, i, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,j) * tovov(i, e, i, a)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,j) * tovov(m, a, i, e)
end do 
end do 



    eom_ccsd_22_trans_aibjaibi_aij = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjaibi_aij = eom_ccsd_22_trans_aibjaibi_aij + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibi_aij
    function eom_ccsd_22_trans_aibjaibi_ibj(t2, nocc, nactive, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaibi_ibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j 
    integer :: s ,m,e,f 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, i, b)
term(1) = term(1) + tvvoo(b, b, i, j)

term(1) = term(1) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,j,i) * tovov(i, e, i, b)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, b, i, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, i, b)
term(5) = term(5) + t2(b,e,m,j) * tovov(m, b, i, e)
end do 
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,j,m) * tovov(m, e, i, b)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibi_ibj = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjaibi_ibj = eom_ccsd_22_trans_aibjaibi_ibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibi_ibj
    function eom_ccsd_22_trans_aibjaibi_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaibi_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, i, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, i, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjaibi_aibj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibi_aibj = eom_ccsd_22_trans_aibjaibi_aibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibi_aibj
    function eom_ccsd_22_trans_aibjaibj_a(t2, nocc, nactive, a) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_a 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, a)


do m = 1, nocc 
term(1) = term(1) + tvvoo(a, a, m, m)
term(2) = term(2) + tvoov(a, m, m, a)
end do 

term(1) = term(1) * (1.9999999999999998d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,n) * tovov(n, a, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,n) * tovov(n, e, m, a)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibj_a = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibjaibj_a = eom_ccsd_22_trans_aibjaibj_a + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_a
    function eom_ccsd_22_trans_aibjaibj_b(t2, nocc, nactive, b) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_b 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(b, b)


do m = 1, nocc 
term(1) = term(1) + tvvoo(b, b, m, m)
term(2) = term(2) + tvoov(b, m, m, b)
end do 

term(1) = term(1) * (1.9999999999999998d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibj_b = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibjaibj_b = eom_ccsd_22_trans_aibjaibj_b + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_b
    function eom_ccsd_22_trans_aibjaibj_i(t2, nocc, nactive, i) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_i 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(i, i)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + toooo(m, i, i, m)
term(2) = term(2) + toooo(m, m, i, i)
end do 

term(2) = term(2) * (-1.9999999999999998d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,m) * tovov(m, e, i, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,m) * tovov(m, f, i, e)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibj_i = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibjaibj_i = eom_ccsd_22_trans_aibjaibj_i + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_i
    function eom_ccsd_22_trans_aibjaibj_j(t2, nocc, nactive, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_j 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(j, j)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + toooo(m, j, j, m)
term(2) = term(2) + toooo(m, m, j, j)
end do 

term(2) = term(2) * (-1.9999999999999998d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,j,m) * tovov(m, e, j, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,j,m) * tovov(m, f, j, e)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibj_j = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibjaibj_j = eom_ccsd_22_trans_aibjaibj_j + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_j
    function eom_ccsd_22_trans_aibjaibj_ai(t2, nocc, nactive, a, i) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_ai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, i, i)
term(1) = term(1) + tvoov(a, i, i, a)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, a, i, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, a, i, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjaibj_ai = 0.d+0
    do s = 0, 5
    eom_ccsd_22_trans_aibjaibj_ai = eom_ccsd_22_trans_aibjaibj_ai + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_ai
    function eom_ccsd_22_trans_aibjaibj_aj(t2, nocc, nactive, a, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_aj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, j, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,m,j) * tovov(m, a, j, e)
end do 
end do 



    eom_ccsd_22_trans_aibjaibj_aj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibj_aj = eom_ccsd_22_trans_aibjaibj_aj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_aj
    function eom_ccsd_22_trans_aibjaibj_ib(t2, nocc, nactive, i, b) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_ib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, b, i, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,m,i) * tovov(m, b, i, e)
end do 
end do 



    eom_ccsd_22_trans_aibjaibj_ib = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibj_ib = eom_ccsd_22_trans_aibjaibj_ib + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_ib
    function eom_ccsd_22_trans_aibjaibj_bj(t2, nocc, nactive, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_bj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, b, j, j)
term(1) = term(1) + tvoov(b, j, j, b)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,j) * tovov(m, b, j, e)
term(3) = term(3) + t2(b,e,j,m) * tovov(m, b, j, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, j, b)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, j, b)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjaibj_bj = 0.d+0
    do s = 0, 5
    eom_ccsd_22_trans_aibjaibj_bj = eom_ccsd_22_trans_aibjaibj_bj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_bj
    function eom_ccsd_22_trans_aibjaibj_ab(t2, nocc, nactive, a, b) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_ab 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, b, a, a)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, b, m, a)
end do 
end do 



    eom_ccsd_22_trans_aibjaibj_ab = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibj_ab = eom_ccsd_22_trans_aibjaibj_ab + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_ab
    function eom_ccsd_22_trans_aibjaibj_ij(t2, nocc, nactive, i, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_ij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(j, j, i, i)


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,j) * tovov(j, f, i, e)
end do 
end do 



    eom_ccsd_22_trans_aibjaibj_ij = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibj_ij = eom_ccsd_22_trans_aibjaibj_ij + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_ij
    function eom_ccsd_22_trans_aibjaibj_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, a, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibj_aij = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibj_aij = eom_ccsd_22_trans_aibjaibj_aij + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_aij
    function eom_ccsd_22_trans_aibjaibj_ibj(t2, nocc, nactive, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_ibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, b, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, i, b)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibj_ibj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibj_ibj = eom_ccsd_22_trans_aibjaibj_ibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_ibj
    function eom_ccsd_22_trans_aibjaibj_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, i, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, a)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibj_aib = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibj_aib = eom_ccsd_22_trans_aibjaibj_aib + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_aib
    function eom_ccsd_22_trans_aibjaibj_abj(t2, nocc, nactive, a, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjaibj_abj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, a)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjaibj_abj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjaibj_abj = eom_ccsd_22_trans_aibjaibj_abj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj_abj
    function eom_ccsd_22_trans_aibiakbi_ik(t2, nocc, nactive, i, k) 
    real(F64) :: eom_ccsd_22_trans_aibiakbi_ik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(k, i)
term(1) = term(1) + toooo(k, i, i, i)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(2) = term(2) + toooo(m, i, k, m)
term(3) = term(3) + toooo(m, m, k, i)
end do 

term(3) = term(3) * (-1.9999999999999998d+0) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,i) * tovov(k, e, i, f)
end do 
end do 


do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(e,f,i,m) * tovov(m, e, k, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(6) = term(6) + t2(e,f,i,m) * tovov(m, f, k, e)
end do 
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiakbi_ik = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibiakbi_ik = eom_ccsd_22_trans_aibiakbi_ik + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakbi_ik
    function eom_ccsd_22_trans_aibiakbi_aik(t2, nocc, nactive, a, i, k) 
    real(F64) :: eom_ccsd_22_trans_aibiakbi_aik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, k, i)
term(1) = term(1) + tvoov(a, i, k, a)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(k, e, i, a)
term(3) = term(3) + t2(a,e,i,i) * tovov(k, a, i, e)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, a, k, e)
term(5) = term(5) + t2(a,e,i,m) * tovov(m, a, k, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibiakbi_aik = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibiakbi_aik = eom_ccsd_22_trans_aibiakbi_aik + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakbi_aik
    function eom_ccsd_22_trans_aibiakbi_ibk(t2, nocc, nactive, i, b, k) 
    real(F64) :: eom_ccsd_22_trans_aibiakbi_ibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, b)
term(1) = term(1) + tvvoo(b, b, k, i)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(k, e, i, b)
term(3) = term(3) + t2(b,e,i,i) * tovov(k, b, i, e)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,i,m) * tovov(m, b, k, e)
term(5) = term(5) + t2(b,e,m,i) * tovov(m, b, k, e)
term(6) = term(6) + t2(b,e,m,i) * tovov(m, e, k, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, k, b)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiakbi_ibk = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibiakbi_ibk = eom_ccsd_22_trans_aibiakbi_ibk + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakbi_ibk
    function eom_ccsd_22_trans_aibiakbi_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_trans_aibiakbi_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, k, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, k, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, k, a)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, b, k, a)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiakbi_aibk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibiakbi_aibk = eom_ccsd_22_trans_aibiakbi_aibk + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakbi_aibk
    function eom_ccsd_22_trans_aibiakbk_ik(t2, nocc, nactive, i, k) 
    real(F64) :: eom_ccsd_22_trans_aibiakbk_ik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, k 
    integer :: s ,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(k, i, k, i)


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,i) * tovov(k, f, k, e)
end do 
end do 



    eom_ccsd_22_trans_aibiakbk_ik = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibiakbk_ik = eom_ccsd_22_trans_aibiakbk_ik + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakbk_ik
    function eom_ccsd_22_trans_aibiakbk_aik(t2, nocc, nactive, a, i, k) 
    real(F64) :: eom_ccsd_22_trans_aibiakbk_aik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k 
    integer :: s ,e,f 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(k, e, k, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibiakbk_aik = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibiakbk_aik = eom_ccsd_22_trans_aibiakbk_aik + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakbk_aik
    function eom_ccsd_22_trans_aibiakbk_ibk(t2, nocc, nactive, i, b, k) 
    real(F64) :: eom_ccsd_22_trans_aibiakbk_ibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k 
    integer :: s ,e,f 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tovov(k, e, k, b)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibiakbk_ibk = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibiakbk_ibk = eom_ccsd_22_trans_aibiakbk_ibk + term(s)
    end do

    end function eom_ccsd_22_trans_aibiakbk_ibk
    function eom_ccsd_22_trans_aibjajbi_ai(t2, nocc, nactive, a, i) 
    real(F64) :: eom_ccsd_22_trans_aibjajbi_ai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i 
    integer :: s ,e,m,f,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, a)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,m,i) * tovov(m, e, i, a)
term(2) = term(2) + t2(a,e,i,m) * tovov(m, a, i, e)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajbi_ai = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjajbi_ai = eom_ccsd_22_trans_aibjajbi_ai + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbi_ai
    function eom_ccsd_22_trans_aibjajbi_bj(t2, nocc, nactive, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjajbi_bj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j 
    integer :: s ,e,m,f,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, j, b)

term(0) = term(0) * (-0.9999999999999999d+0) 

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

term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajbi_bj = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjajbi_bj = eom_ccsd_22_trans_aibjajbi_bj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbi_bj
    function eom_ccsd_22_trans_aibjajbi_ab(t2, nocc, nactive, a, b) 
    real(F64) :: eom_ccsd_22_trans_aibjajbi_ab 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b 
    integer :: s ,e,m,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, a, a, b)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, a, m, b)
end do 
end do 



    eom_ccsd_22_trans_aibjajbi_ab = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajbi_ab = eom_ccsd_22_trans_aibjajbi_ab + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbi_ab
    function eom_ccsd_22_trans_aibjajbi_ij(t2, nocc, nactive, i, j) 
    real(F64) :: eom_ccsd_22_trans_aibjajbi_ij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j 
    integer :: s ,e,m,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(j, i, i, j)


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,j) * tovov(j, e, i, f)
end do 
end do 



    eom_ccsd_22_trans_aibjajbi_ij = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajbi_ij = eom_ccsd_22_trans_aibjajbi_ij + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbi_ij
    function eom_ccsd_22_trans_aibjajbi_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_trans_aibjajbi_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,e,m,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, e, i, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, a, i, e)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajbi_aij = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajbi_aij = eom_ccsd_22_trans_aibjajbi_aij + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbi_aij
    function eom_ccsd_22_trans_aibjajbi_ibj(t2, nocc, nactive, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjajbi_ibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j 
    integer :: s ,e,m,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, e, i, b)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, b, i, e)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajbi_ibj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajbi_ibj = eom_ccsd_22_trans_aibjajbi_ibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbi_ibj
    function eom_ccsd_22_trans_aibjajbi_abj(t2, nocc, nactive, a, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjajbi_abj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j 
    integer :: s ,e,m,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, a)
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajbi_abj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajbi_abj = eom_ccsd_22_trans_aibjajbi_abj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbi_abj
    function eom_ccsd_22_trans_aibjajbi_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_trans_aibjajbi_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,e,m,f,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, i, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, a)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajbi_aib = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajbi_aib = eom_ccsd_22_trans_aibjajbi_aib + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbi_aib
    function eom_ccsd_22_trans_aibjajbj_ij(t2, nocc, nactive, i, j) 
    real(F64) :: eom_ccsd_22_trans_aibjajbj_ij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j 
    integer :: s ,m,e,f 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(j, i)
term(1) = term(1) + toooo(j, j, j, i)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(2) = term(2) + toooo(m, i, j, m)
term(3) = term(3) + toooo(m, m, j, i)
end do 

term(3) = term(3) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,j) * tovov(j, f, j, e)
end do 
end do 


do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(e,f,i,m) * tovov(m, e, j, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(6) = term(6) + t2(e,f,i,m) * tovov(m, f, j, e)
end do 
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajbj_ij = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjajbj_ij = eom_ccsd_22_trans_aibjajbj_ij + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbj_ij
    function eom_ccsd_22_trans_aibjajbj_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_trans_aibjajbj_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,m,e,f 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, j, i)
term(1) = term(1) + tvoov(a, i, j, a)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(j, e, j, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,i) * tovov(m, a, j, e)
term(4) = term(4) + t2(a,e,i,m) * tovov(m, a, j, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, e, j, a)
end do 
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, j, a)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjajbj_aij = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjajbj_aij = eom_ccsd_22_trans_aibjajbj_aij + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbj_aij
    function eom_ccsd_22_trans_aibjajbj_ibj(t2, nocc, nactive, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjajbj_ibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j 
    integer :: s ,m,e,f 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, b, j, i)

term(0) = term(0) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, j, b)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,i) * tovov(m, b, j, e)
end do 
end do 



    eom_ccsd_22_trans_aibjajbj_ibj = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjajbj_ibj = eom_ccsd_22_trans_aibjajbj_ibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbj_ibj
    function eom_ccsd_22_trans_aibjajbj_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjajbj_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, j, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjajbj_aibj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjajbj_aibj = eom_ccsd_22_trans_aibjajbj_aibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjajbj_aibj
    function eom_ccsd_22_trans_aibiaidi_bd(t2, nocc, nactive, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibiaidi_bd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(b, d)


do m = 1, nocc 
term(1) = term(1) + tvvoo(b, d, m, m)
term(2) = term(2) + tvoov(b, m, m, d)
end do 

term(1) = term(1) * (1.9999999999999998d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiaidi_bd = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibiaidi_bd = eom_ccsd_22_trans_aibiaidi_bd + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaidi_bd
    function eom_ccsd_22_trans_aibiaidi_abd(t2, nocc, nactive, a, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibiaidi_abd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, d, a, a)
term(1) = term(1) + read_ftvvvv(b, a, a, d)


do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,n) * tovov(n, d, m, a)
term(3) = term(3) + t2(a,b,m,n) * tovov(n, a, m, d)
end do 
end do 



    eom_ccsd_22_trans_aibiaidi_abd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibiaidi_abd = eom_ccsd_22_trans_aibiaidi_abd + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaidi_abd
    function eom_ccsd_22_trans_aibiaidi_ibd(t2, nocc, nactive, i, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibiaidi_ibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, i, d)
term(1) = term(1) + tvvoo(b, d, i, i)

term(1) = term(1) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(i, e, i, d)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, d, i, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, d, i, e)
term(5) = term(5) + t2(b,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiaidi_ibd = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibiaidi_ibd = eom_ccsd_22_trans_aibiaidi_ibd + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaidi_ibd
    function eom_ccsd_22_trans_aibiaidi_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibiaidi_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, i, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, i, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, i, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, i, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibiaidi_aibd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibiaidi_aibd = eom_ccsd_22_trans_aibiaidi_aibd + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaidi_aibd
    function eom_ccsd_22_trans_aiaicial_aicl(t2, nocc, nactive, a, i, c, l) 
    real(F64) :: eom_ccsd_22_trans_aiaicial_aicl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, l, i)
term(1) = term(1) + tvoov(a, i, l, c)

term(0) = term(0) * (-0.9999999999999998d+0) 
term(1) = term(1) * (-0.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(l, c, i, e)
term(3) = term(3) + t2(a,e,i,i) * tovov(l, e, i, c)
end do 

term(3) = term(3) * (-2.000000000000001d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,a,i,m) * tovov(m, c, l, a)
term(5) = term(5) + t2(a,a,i,m) * tovov(m, a, l, c)
end do 

term(4) = term(4) * (-2.000000000000001d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, c, l, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, c, l, e)
term(8) = term(8) + t2(a,e,m,i) * tovov(m, e, l, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(9) = term(9) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aiaicial_aicl = 0.d+0
    do s = 0, 9
    eom_ccsd_22_trans_aiaicial_aicl = eom_ccsd_22_trans_aiaicial_aicl + term(s)
    end do

    end function eom_ccsd_22_trans_aiaicial_aicl
    function eom_ccsd_22_trans_aiajciai_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_trans_aiajciai_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, i, j)
term(1) = term(1) + tvoov(a, j, i, c)

term(0) = term(0) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(i, e, i, c)
term(3) = term(3) + t2(a,e,i,j) * tovov(i, e, i, c)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,a,j,m) * tovov(m, c, i, a)
term(5) = term(5) + t2(a,a,j,m) * tovov(m, a, i, c)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,j) * tovov(m, c, i, e)
term(7) = term(7) + t2(a,e,j,m) * tovov(m, c, i, e)
term(8) = term(8) + t2(a,e,m,j) * tovov(m, e, i, c)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,j,m) * tovov(m, e, i, c)
end do 
end do 

term(9) = term(9) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajciai_aijc = 0.d+0
    do s = 0, 9
    eom_ccsd_22_trans_aiajciai_aijc = eom_ccsd_22_trans_aiajciai_aijc + term(s)
    end do

    end function eom_ccsd_22_trans_aiajciai_aijc
    function eom_ccsd_22_trans_aiajciaj_ac(t2, nocc, nactive, a, c) 
    real(F64) :: eom_ccsd_22_trans_aiajciaj_ac 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, c)
term(1) = term(1) + read_ftvvvv(a, c, a, a)


do m = 1, nocc 
term(2) = term(2) + tvvoo(a, c, m, m)
term(3) = term(3) + tvoov(a, m, m, c)
end do 

term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-0.9999999999999999d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,a,m,n) * tovov(n, a, m, c)
end do 
end do 


do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajciaj_ac = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiajciaj_ac = eom_ccsd_22_trans_aiajciaj_ac + term(s)
    end do

    end function eom_ccsd_22_trans_aiajciaj_ac
    function eom_ccsd_22_trans_aiajciaj_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_ccsd_22_trans_aiajciaj_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, i, i)
term(1) = term(1) + tvoov(a, i, i, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,i,m) * tovov(m, c, i, a)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, a, i, c)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, c, i, e)
term(5) = term(5) + t2(a,e,i,m) * tovov(m, c, i, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aiajciaj_aic = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajciaj_aic = eom_ccsd_22_trans_aiajciaj_aic + term(s)
    end do

    end function eom_ccsd_22_trans_aiajciaj_aic
    function eom_ccsd_22_trans_aiajciaj_ajc(t2, nocc, nactive, a, j, c) 
    real(F64) :: eom_ccsd_22_trans_aiajciaj_ajc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, j, j)
term(1) = term(1) + tvoov(a, j, j, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,j,m) * tovov(m, c, j, a)
term(3) = term(3) + t2(a,a,j,m) * tovov(m, a, j, c)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,j,m) * tovov(m, c, j, e)
term(5) = term(5) + t2(a,e,m,j) * tovov(m, c, j, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, e, j, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajciaj_ajc = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajciaj_ajc = eom_ccsd_22_trans_aiajciaj_ajc + term(s)
    end do

    end function eom_ccsd_22_trans_aiajciaj_ajc
    function eom_ccsd_22_trans_aiajciaj_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_trans_aiajciaj_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, c, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, c, i, e)
term(2) = term(2) + t2(a,e,j,i) * tovov(j, e, i, c)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, e, i, c)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajciaj_aijc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajciaj_aijc = eom_ccsd_22_trans_aiajciaj_aijc + term(s)
    end do

    end function eom_ccsd_22_trans_aiajciaj_aijc
    function eom_ccsd_22_trans_aiaickai_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_ccsd_22_trans_aiaickai_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, k, i)
term(1) = term(1) + tvoov(a, i, k, c)

term(0) = term(0) * (-0.9999999999999998d+0) 
term(1) = term(1) * (1.9999999999999996d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(k, e, i, c)
term(3) = term(3) + t2(a,e,i,i) * tovov(k, c, i, e)
end do 

term(3) = term(3) * (-2.000000000000001d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,a,i,m) * tovov(m, c, k, a)
term(5) = term(5) + t2(a,a,i,m) * tovov(m, a, k, c)
end do 

term(5) = term(5) * (-2.000000000000001d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,i) * tovov(m, c, k, e)
term(7) = term(7) + t2(a,e,i,m) * tovov(m, c, k, e)
term(8) = term(8) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * (-2.000000000000001d+0) 
term(8) = term(8) * (-2.000000000000001d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(9) = term(9) * (4.000000000000002d+0) 


    eom_ccsd_22_trans_aiaickai_aick = 0.d+0
    do s = 0, 9
    eom_ccsd_22_trans_aiaickai_aick = eom_ccsd_22_trans_aiaickai_aick + term(s)
    end do

    end function eom_ccsd_22_trans_aiaickai_aick
    function eom_ccsd_22_trans_aiaickak_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_ccsd_22_trans_aiaickak_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(k, e, k, c)
end do 

term(0) = term(0) * (-0.9999999999999998d+0) 


    eom_ccsd_22_trans_aiaickak_aick = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiaickak_aick = eom_ccsd_22_trans_aiaickak_aick + term(s)
    end do

    end function eom_ccsd_22_trans_aiaickak_aick
    function eom_ccsd_22_trans_aiajcjai_ac(t2, nocc, nactive, a, c) 
    real(F64) :: eom_ccsd_22_trans_aiajcjai_ac 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, c)
term(1) = term(1) + read_ftvvvv(a, c, a, a)


do m = 1, nocc 
term(2) = term(2) + tvvoo(a, c, m, m)
term(3) = term(3) + tvoov(a, m, m, c)
end do 

term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-0.9999999999999999d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,a,m,n) * tovov(n, c, m, a)
end do 
end do 


do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcjai_ac = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiajcjai_ac = eom_ccsd_22_trans_aiajcjai_ac + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjai_ac
    function eom_ccsd_22_trans_aiajcjai_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_ccsd_22_trans_aiajcjai_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, i, i)
term(1) = term(1) + tvoov(a, i, i, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,i,m) * tovov(m, c, i, a)
term(3) = term(3) + t2(a,a,i,m) * tovov(m, a, i, c)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, i, c)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, c, i, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, c, i, e)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(7) = term(7) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcjai_aic = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajcjai_aic = eom_ccsd_22_trans_aiajcjai_aic + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjai_aic
    function eom_ccsd_22_trans_aiajcjai_ajc(t2, nocc, nactive, a, j, c) 
    real(F64) :: eom_ccsd_22_trans_aiajcjai_ajc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, j, j)
term(1) = term(1) + tvoov(a, j, j, c)

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,j,m) * tovov(m, c, j, a)
term(3) = term(3) + t2(a,a,j,m) * tovov(m, a, j, c)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,j) * tovov(m, c, j, e)
term(5) = term(5) + t2(a,e,j,m) * tovov(m, c, j, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, e, j, c)
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aiajcjai_ajc = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiajcjai_ajc = eom_ccsd_22_trans_aiajcjai_ajc + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjai_ajc
    function eom_ccsd_22_trans_aiajcjai_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_trans_aiajcjai_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, e, i, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, e, i, c)
term(2) = term(2) + t2(a,e,j,i) * tovov(j, c, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, c, i, e)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcjai_aijc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajcjai_aijc = eom_ccsd_22_trans_aiajcjai_aijc + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjai_aijc
    function eom_ccsd_22_trans_aiajcjaj_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_trans_aiajcjaj_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, j, i)
term(1) = term(1) + tvoov(a, i, j, c)

term(0) = term(0) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(j, e, j, c)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, e, j, c)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,a,i,m) * tovov(m, c, j, a)
term(5) = term(5) + t2(a,a,i,m) * tovov(m, a, j, c)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,i) * tovov(m, c, j, e)
term(7) = term(7) + t2(a,e,i,m) * tovov(m, c, j, e)
term(8) = term(8) + t2(a,e,m,i) * tovov(m, e, j, c)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,i,m) * tovov(m, e, j, c)
end do 
end do 

term(9) = term(9) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcjaj_aijc = 0.d+0
    do s = 0, 9
    eom_ccsd_22_trans_aiajcjaj_aijc = eom_ccsd_22_trans_aiajcjaj_aijc + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjaj_aijc
    function eom_ccsd_22_trans_aiaicicl_aicl(t2, nocc, nactive, a, i, c, l) 
    real(F64) :: eom_ccsd_22_trans_aiaicicl_aicl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, l 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, l, c)
end do 

term(0) = term(0) * (-1.0000000000000007d+0) 


    eom_ccsd_22_trans_aiaicicl_aicl = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiaicicl_aicl = eom_ccsd_22_trans_aiaicicl_aicl + term(s)
    end do

    end function eom_ccsd_22_trans_aiaicicl_aicl
    function eom_ccsd_22_trans_aiajcici_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_trans_aiajcici_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, i, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcici_aijc = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiajcici_aijc = eom_ccsd_22_trans_aiajcici_aijc + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcici_aijc
    function eom_ccsd_22_trans_aiajcicj_ac(t2, nocc, nactive, a, c) 
    real(F64) :: eom_ccsd_22_trans_aiajcicj_ac 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, c 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(a, c, a, c)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,a,m,n) * tovov(n, c, m, c)
end do 
end do 



    eom_ccsd_22_trans_aiajcicj_ac = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiajcicj_ac = eom_ccsd_22_trans_aiajcicj_ac + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcicj_ac
    function eom_ccsd_22_trans_aiajcicj_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_ccsd_22_trans_aiajcicj_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,m,n 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, i, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajcicj_aic = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiajcicj_aic = eom_ccsd_22_trans_aiajcicj_aic + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcicj_aic
    function eom_ccsd_22_trans_aiajcicj_ajc(t2, nocc, nactive, a, j, c) 
    real(F64) :: eom_ccsd_22_trans_aiajcicj_ajc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c 
    integer :: s ,m,n 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,j,m) * tovov(m, c, j, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajcicj_ajc = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiajcicj_ajc = eom_ccsd_22_trans_aiajcicj_ajc + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcicj_ajc
    function eom_ccsd_22_trans_aiaickci_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_ccsd_22_trans_aiaickci_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, k, c)
end do 

term(0) = term(0) * (-0.9999999999999998d+0) 


    eom_ccsd_22_trans_aiaickci_aick = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiaickci_aick = eom_ccsd_22_trans_aiaickci_aick + term(s)
    end do

    end function eom_ccsd_22_trans_aiaickci_aick
    function eom_ccsd_22_trans_aiajcjcj_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_trans_aiajcjcj_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, j, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajcjcj_aijc = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiajcjcj_aijc = eom_ccsd_22_trans_aiajcjcj_aijc + term(s)
    end do

    end function eom_ccsd_22_trans_aiajcjcj_aijc
    function eom_ccsd_22_trans_aiaicidi_acd(t2, nocc, nactive, a, c, d) 
    real(F64) :: eom_ccsd_22_trans_aiaicidi_acd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(a, d, a, c)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,a,m,n) * tovov(n, d, m, c)
term(2) = term(2) + t2(a,a,m,n) * tovov(n, c, m, d)
end do 
end do 

term(1) = term(1) * (0.5000000000000001d+0) 
term(2) = term(2) * (0.5000000000000001d+0) 


    eom_ccsd_22_trans_aiaicidi_acd = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aiaicidi_acd = eom_ccsd_22_trans_aiaicidi_acd + term(s)
    end do

    end function eom_ccsd_22_trans_aiaicidi_acd
    function eom_ccsd_22_trans_aiaicidi_aicd(t2, nocc, nactive, a, i, c, d) 
    real(F64) :: eom_ccsd_22_trans_aiaicidi_aicd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,a,i,m) * tovov(m, d, i, c)
end do 

term(0) = term(0) * (-0.9999999999999998d+0) 
term(1) = term(1) * (-0.9999999999999998d+0) 


    eom_ccsd_22_trans_aiaicidi_aicd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiaicidi_aicd = eom_ccsd_22_trans_aiaicidi_aicd + term(s)
    end do

    end function eom_ccsd_22_trans_aiaicidi_aicd
    function eom_ccsd_22_trans_aibiciai_bc(t2, nocc, nactive, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibiciai_bc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(b, c)


do m = 1, nocc 
term(1) = term(1) + tvvoo(b, c, m, m)
term(2) = term(2) + tvoov(b, m, m, c)
end do 

term(1) = term(1) * (1.9999999999999998d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiciai_bc = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibiciai_bc = eom_ccsd_22_trans_aibiciai_bc + term(s)
    end do

    end function eom_ccsd_22_trans_aibiciai_bc
    function eom_ccsd_22_trans_aibiciai_ibc(t2, nocc, nactive, i, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibiciai_ibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, i, i)
term(1) = term(1) + tvoov(b, i, i, c)

term(0) = term(0) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(i, e, i, c)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,i) * tovov(m, c, i, e)
term(4) = term(4) + t2(b,e,i,m) * tovov(m, c, i, e)
term(5) = term(5) + t2(b,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiciai_ibc = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibiciai_ibc = eom_ccsd_22_trans_aibiciai_ibc + term(s)
    end do

    end function eom_ccsd_22_trans_aibiciai_ibc
    function eom_ccsd_22_trans_aibiciai_abc(t2, nocc, nactive, a, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibiciai_abc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, a, a, c)
term(1) = term(1) + read_ftvvvv(b, c, a, a)


do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,n) * tovov(n, a, m, c)
term(3) = term(3) + t2(a,b,m,n) * tovov(n, c, m, a)
end do 
end do 



    eom_ccsd_22_trans_aibiciai_abc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibiciai_abc = eom_ccsd_22_trans_aibiciai_abc + term(s)
    end do

    end function eom_ccsd_22_trans_aibiciai_abc
    function eom_ccsd_22_trans_aibiciai_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibiciai_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, a)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, i, a)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, i, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, i, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibiciai_aibc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibiciai_aibc = eom_ccsd_22_trans_aibiciai_aibc + term(s)
    end do

    end function eom_ccsd_22_trans_aibiciai_aibc
    function eom_ccsd_22_trans_aibibibl_aibl(t2, nocc, nactive, a, i, b, l) 
    real(F64) :: eom_ccsd_22_trans_aibibibl_aibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,e,m 
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, l, i)
term(1) = term(1) + tvoov(a, i, l, b)

term(0) = term(0) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(l, b, i, e)
term(3) = term(3) + t2(a,e,i,i) * tovov(l, e, i, b)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,i) * tovov(m, b, l, b)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, b, l, b)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,i) * tovov(m, b, l, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, l, b)
term(8) = term(8) + t2(a,e,i,m) * tovov(m, b, l, e)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,i,m) * tovov(m, e, l, b)
end do 
end do 

term(9) = term(9) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibibibl_aibl = 0.d+0
    do s = 0, 9
    eom_ccsd_22_trans_aibibibl_aibl = eom_ccsd_22_trans_aibibibl_aibl + term(s)
    end do

    end function eom_ccsd_22_trans_aibibibl_aibl
    function eom_ccsd_22_trans_aibjbibi_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjbibi_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, i, j)

term(0) = term(0) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,i,j) * tovov(i, e, i, b)
end do 

term(1) = term(1) * (-2.000000000000001d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, i, b)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,j) * tovov(m, b, i, e)
end do 
end do 

term(3) = term(3) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbibi_aibj = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibjbibi_aibj = eom_ccsd_22_trans_aibjbibi_aibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbibi_aibj
    function eom_ccsd_22_trans_aibjbibj_ab(t2, nocc, nactive, a, b) 
    real(F64) :: eom_ccsd_22_trans_aibjbibj_ab 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, b)
term(1) = term(1) + read_ftvvvv(b, b, a, b)


do m = 1, nocc 
term(2) = term(2) + tvvoo(a, b, m, m)
term(3) = term(3) + tvoov(a, m, m, b)
end do 

term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-0.9999999999999999d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,n) * tovov(n, b, m, b)
end do 
end do 


do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbibj_ab = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjbibj_ab = eom_ccsd_22_trans_aibjbibj_ab + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbibj_ab
    function eom_ccsd_22_trans_aibjbibj_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_trans_aibjbibj_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, i, i)
term(1) = term(1) + tvoov(a, i, i, b)

term(0) = term(0) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, i, b)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,i) * tovov(m, b, i, e)
term(4) = term(4) + t2(a,e,i,m) * tovov(m, b, i, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, e, i, b)
end do 
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, i, b)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbibj_aib = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjbibj_aib = eom_ccsd_22_trans_aibjbibj_aib + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbibj_aib
    function eom_ccsd_22_trans_aibjbibj_abj(t2, nocc, nactive, a, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjbibj_abj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j 
    integer :: s ,m,e,n 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, j, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, b)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,j) * tovov(m, b, j, e)
end do 
end do 



    eom_ccsd_22_trans_aibjbibj_abj = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjbibj_abj = eom_ccsd_22_trans_aibjbibj_abj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbibj_abj
    function eom_ccsd_22_trans_aibjbibj_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjbibj_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, b, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, e, i, b)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjbibj_aibj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbibj_aibj = eom_ccsd_22_trans_aibjbibj_aibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbibj_aibj
    function eom_ccsd_22_trans_aibibkbi_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_trans_aibibkbi_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, k, i)
term(1) = term(1) + tvoov(a, i, k, b)

term(0) = term(0) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(k, e, i, b)
term(3) = term(3) + t2(a,e,i,i) * tovov(k, b, i, e)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,i,m) * tovov(m, b, k, b)
term(5) = term(5) + t2(a,b,m,i) * tovov(m, b, k, b)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,i) * tovov(m, b, k, e)
term(7) = term(7) + t2(a,e,i,m) * tovov(m, b, k, e)
term(8) = term(8) + t2(a,e,m,i) * tovov(m, e, k, b)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(9) = term(9) + t2(a,e,i,m) * tovov(m, e, k, b)
end do 
end do 

term(9) = term(9) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibibkbi_aibk = 0.d+0
    do s = 0, 9
    eom_ccsd_22_trans_aibibkbi_aibk = eom_ccsd_22_trans_aibibkbi_aibk + term(s)
    end do

    end function eom_ccsd_22_trans_aibibkbi_aibk
    function eom_ccsd_22_trans_aibibkbk_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_trans_aibibkbk_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(k, e, k, b)
end do 

term(0) = term(0) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aibibkbk_aibk = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aibibkbk_aibk = eom_ccsd_22_trans_aibibkbk_aibk + term(s)
    end do

    end function eom_ccsd_22_trans_aibibkbk_aibk
    function eom_ccsd_22_trans_aibjbjbi_ab(t2, nocc, nactive, a, b) 
    real(F64) :: eom_ccsd_22_trans_aibjbjbi_ab 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, b)
term(1) = term(1) + read_ftvvvv(b, b, a, b)


do m = 1, nocc 
term(2) = term(2) + tvvoo(a, b, m, m)
term(3) = term(3) + tvoov(a, m, m, b)
end do 

term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-0.9999999999999999d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,n) * tovov(n, b, m, b)
end do 
end do 


do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbjbi_ab = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjbjbi_ab = eom_ccsd_22_trans_aibjbjbi_ab + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjbi_ab
    function eom_ccsd_22_trans_aibjbjbi_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_trans_aibjbjbi_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, b)
term(1) = term(1) + tvvoo(a, b, i, i)

term(1) = term(1) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, i, b)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,i) * tovov(m, e, i, b)
term(4) = term(4) + t2(a,e,i,m) * tovov(m, b, i, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, b, i, e)
end do 
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, i, b)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibjbjbi_aib = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibjbjbi_aib = eom_ccsd_22_trans_aibjbjbi_aib + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjbi_aib
    function eom_ccsd_22_trans_aibjbjbi_abj(t2, nocc, nactive, a, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjbjbi_abj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j 
    integer :: s ,m,e,n 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, j, j)

term(0) = term(0) * (-0.9999999999999999d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, b)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,j) * tovov(m, b, j, e)
end do 
end do 



    eom_ccsd_22_trans_aibjbjbi_abj = 0.d+0
    do s = 0, 2
    eom_ccsd_22_trans_aibjbjbi_abj = eom_ccsd_22_trans_aibjbjbi_abj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjbi_abj
    function eom_ccsd_22_trans_aibjbjbi_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjbjbi_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tovov(j, e, i, b)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, b, i, e)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibjbjbi_aibj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjbjbi_aibj = eom_ccsd_22_trans_aibjbjbi_aibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjbi_aibj
    function eom_ccsd_22_trans_aibjbjbj_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_trans_aibjbjbj_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, j, i)
term(1) = term(1) + tvoov(a, i, j, b)

term(0) = term(0) * (-1.9999999999999998d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,j) * tovov(j, e, j, b)
end do 

term(2) = term(2) * (-2.000000000000001d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, j, b)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, b, j, e)
term(5) = term(5) + t2(a,e,i,m) * tovov(m, b, j, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, j, b)
end do 
end do 

term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-2.0000000000000004d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, j, b)
end do 
end do 

term(7) = term(7) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibjbjbj_aibj = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aibjbjbj_aibj = eom_ccsd_22_trans_aibjbjbj_aibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjbjbj_aibj
    function eom_ccsd_22_trans_aibibidi_ad(t2, nocc, nactive, a, d) 
    real(F64) :: eom_ccsd_22_trans_aibibidi_ad 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, d)


do m = 1, nocc 
term(1) = term(1) + tvvoo(a, d, m, m)
term(2) = term(2) + tvoov(a, m, m, d)
end do 

term(1) = term(1) * (1.9999999999999998d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibibidi_ad = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibibidi_ad = eom_ccsd_22_trans_aibibidi_ad + term(s)
    end do

    end function eom_ccsd_22_trans_aibibidi_ad
    function eom_ccsd_22_trans_aibibidi_abd(t2, nocc, nactive, a, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibibidi_abd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, d, a, b)
term(1) = term(1) + read_ftvvvv(b, b, a, d)


do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,n) * tovov(n, d, m, b)
term(3) = term(3) + t2(a,b,m,n) * tovov(n, b, m, d)
end do 
end do 



    eom_ccsd_22_trans_aibibidi_abd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibibidi_abd = eom_ccsd_22_trans_aibibidi_abd + term(s)
    end do

    end function eom_ccsd_22_trans_aibibidi_abd
    function eom_ccsd_22_trans_aibibidi_aid(t2, nocc, nactive, a, i, d) 
    real(F64) :: eom_ccsd_22_trans_aibibidi_aid 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, d)
term(1) = term(1) + tvvoo(a, d, i, i)

term(1) = term(1) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(i, e, i, d)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, i, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, d, i, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibibidi_aid = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibibidi_aid = eom_ccsd_22_trans_aibibidi_aid + term(s)
    end do

    end function eom_ccsd_22_trans_aibibidi_aid
    function eom_ccsd_22_trans_aibibidi_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_trans_aibibidi_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, b, i, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, i, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, i, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, i, b)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibibidi_aibd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibibidi_aibd = eom_ccsd_22_trans_aibibidi_aibd + term(s)
    end do

    end function eom_ccsd_22_trans_aibibidi_aibd
    function eom_ccsd_22_trans_aibicici_abc(t2, nocc, nactive, a, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibicici_abc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, c, a, c)

term(0) = term(0) * (1.9999999999999998d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,n) * tovov(n, c, m, c)
end do 
end do 

term(1) = term(1) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibicici_abc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibicici_abc = eom_ccsd_22_trans_aibicici_abc + term(s)
    end do

    end function eom_ccsd_22_trans_aibicici_abc
    function eom_ccsd_22_trans_aibicici_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibicici_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, c)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, i, c)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibicici_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibicici_aibc = eom_ccsd_22_trans_aibicici_aibc + term(s)
    end do

    end function eom_ccsd_22_trans_aibicici_aibc
    function eom_ccsd_22_trans_aibicibi_ac(t2, nocc, nactive, a, c) 
    real(F64) :: eom_ccsd_22_trans_aibicibi_ac 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, c)


do m = 1, nocc 
term(1) = term(1) + tvvoo(a, c, m, m)
term(2) = term(2) + tvoov(a, m, m, c)
end do 

term(1) = term(1) * (1.9999999999999998d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibicibi_ac = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibicibi_ac = eom_ccsd_22_trans_aibicibi_ac + term(s)
    end do

    end function eom_ccsd_22_trans_aibicibi_ac
    function eom_ccsd_22_trans_aibicibi_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_ccsd_22_trans_aibicibi_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, i, i)
term(1) = term(1) + tvoov(a, i, i, c)

term(0) = term(0) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(i, e, i, c)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,i) * tovov(m, c, i, e)
term(4) = term(4) + t2(a,e,i,m) * tovov(m, c, i, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibicibi_aic = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibicibi_aic = eom_ccsd_22_trans_aibicibi_aic + term(s)
    end do

    end function eom_ccsd_22_trans_aibicibi_aic
    function eom_ccsd_22_trans_aibicibi_abc(t2, nocc, nactive, a, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibicibi_abc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, b, a, c)
term(1) = term(1) + read_ftvvvv(b, c, a, b)


do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,n) * tovov(n, b, m, c)
term(3) = term(3) + t2(a,b,m,n) * tovov(n, c, m, b)
end do 
end do 



    eom_ccsd_22_trans_aibicibi_abc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibicibi_abc = eom_ccsd_22_trans_aibicibi_abc + term(s)
    end do

    end function eom_ccsd_22_trans_aibicibi_abc
    function eom_ccsd_22_trans_aibicibi_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_trans_aibicibi_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, c, i, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, i, b)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, i, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibicibi_aibc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibicibi_aibc = eom_ccsd_22_trans_aibicibi_aibc + term(s)
    end do

    end function eom_ccsd_22_trans_aibicibi_aibc
    function eom_ccsd_22_trans_aiaiaial_il(t2, nocc, nactive, i, l) 
    real(F64) :: eom_ccsd_22_trans_aiaiaial_il 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(l, i)
term(1) = term(1) + toooo(l, i, i, i)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(2) = term(2) + toooo(m, i, l, m)
term(3) = term(3) + toooo(m, m, l, i)
end do 

term(3) = term(3) * (-1.9999999999999996d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,i) * tovov(l, f, i, e)
end do 
end do 

term(4) = term(4) * (0.5000000000000001d+0) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(e,f,i,i) * tovov(l, e, i, f)
end do 
end do 

term(5) = term(5) * (0.5000000000000001d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(e,f,i,m) * tovov(m, e, l, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(7) = term(7) + t2(e,f,i,m) * tovov(m, f, l, e)
end do 
end do 
end do 

term(7) = term(7) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aiaiaial_il = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiaiaial_il = eom_ccsd_22_trans_aiaiaial_il + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiaial_il
    function eom_ccsd_22_trans_aiaiaial_ail(t2, nocc, nactive, a, i, l) 
    real(F64) :: eom_ccsd_22_trans_aiaiaial_ail 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, l, i)
term(1) = term(1) + tvoov(a, i, l, a)

term(0) = term(0) * (-1.9999999999999996d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(l, a, i, e)
term(3) = term(3) + t2(a,e,i,i) * tovov(l, e, i, a)
end do 

term(2) = term(2) * (-0.9999999999999998d+0) 
term(3) = term(3) * (-1.0000000000000007d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,a,i,m) * tovov(m, a, l, a)
end do 

term(4) = term(4) * (-1.0000000000000007d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, a, l, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, a, l, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 

term(5) = term(5) * (-0.9999999999999998d+0) 
term(6) = term(6) * (2.000000000000001d+0) 
term(7) = term(7) * (-0.9999999999999998d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(8) = term(8) * (1.9999999999999996d+0) 


    eom_ccsd_22_trans_aiaiaial_ail = 0.d+0
    do s = 0, 8
    eom_ccsd_22_trans_aiaiaial_ail = eom_ccsd_22_trans_aiaiaial_ail + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiaial_ail
    function eom_ccsd_22_trans_aiajaiai_ij(t2, nocc, nactive, i, j) 
    real(F64) :: eom_ccsd_22_trans_aiajaiai_ij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j 
    integer :: s ,m,e,f 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(i, j)
term(1) = term(1) + toooo(i, j, i, i)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + toooo(m, j, i, m)
term(3) = term(3) + toooo(m, m, i, j)
end do 

term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-3.9999999999999996d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,j) * tovov(i, f, i, e)
end do 
end do 

term(4) = term(4) * (2.0000000000000004d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(e,f,j,m) * tovov(m, e, i, f)
end do 
end do 
end do 

term(5) = term(5) * (2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(6) = term(6) + t2(e,f,j,m) * tovov(m, f, i, e)
end do 
end do 
end do 

term(6) = term(6) * (-4.000000000000001d+0) 


    eom_ccsd_22_trans_aiajaiai_ij = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiajaiai_ij = eom_ccsd_22_trans_aiajaiai_ij + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaiai_ij
    function eom_ccsd_22_trans_aiajaiai_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_trans_aiajaiai_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,m,e,f 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, i, j)
term(1) = term(1) + tvoov(a, j, i, a)

term(0) = term(0) * (-3.9999999999999996d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(i, e, i, a)
term(3) = term(3) + t2(a,e,i,j) * tovov(i, e, i, a)
end do 

term(2) = term(2) * (-2.000000000000001d+0) 
term(3) = term(3) * (-2.000000000000001d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,a,j,m) * tovov(m, a, i, a)
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,j) * tovov(m, a, i, e)
term(6) = term(6) + t2(a,e,j,m) * tovov(m, a, i, e)
term(7) = term(7) + t2(a,e,m,j) * tovov(m, e, i, a)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 
term(7) = term(7) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,j,m) * tovov(m, e, i, a)
end do 
end do 

term(8) = term(8) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aiajaiai_aij = 0.d+0
    do s = 0, 8
    eom_ccsd_22_trans_aiajaiai_aij = eom_ccsd_22_trans_aiajaiai_aij + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaiai_aij
    function eom_ccsd_22_trans_aiajaiaj_a(t2, nocc, nactive, a) 
    real(F64) :: eom_ccsd_22_trans_aiajaiaj_a 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, a)
term(1) = term(1) + read_ftvvvv(a, a, a, a)

term(0) = term(0) * (2.0d+0) 

do m = 1, nocc 
term(2) = term(2) + tvvoo(a, a, m, m)
term(3) = term(3) + tvoov(a, m, m, a)
end do 

term(2) = term(2) * (3.9999999999999996d+0) 
term(3) = term(3) * (-1.9999999999999998d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,a,m,n) * tovov(n, a, m, a)
end do 
end do 


do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,n) * tovov(n, a, m, e)
end do 
end do 
end do 

term(5) = term(5) * (2.0000000000000004d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, a)
end do 
end do 
end do 

term(6) = term(6) * (-4.000000000000001d+0) 


    eom_ccsd_22_trans_aiajaiaj_a = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiajaiaj_a = eom_ccsd_22_trans_aiajaiaj_a + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaiaj_a
    function eom_ccsd_22_trans_aiajaiaj_i(t2, nocc, nactive, i) 
    real(F64) :: eom_ccsd_22_trans_aiajaiaj_i 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(i, i)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + toooo(m, i, i, m)
term(2) = term(2) + toooo(m, m, i, i)
end do 

term(2) = term(2) * (-1.9999999999999998d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,m) * tovov(m, e, i, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,m) * tovov(m, f, i, e)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajaiaj_i = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aiajaiaj_i = eom_ccsd_22_trans_aiajaiaj_i + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaiaj_i
    function eom_ccsd_22_trans_aiajaiaj_j(t2, nocc, nactive, j) 
    real(F64) :: eom_ccsd_22_trans_aiajaiaj_j 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(j, j)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + toooo(m, j, j, m)
term(2) = term(2) + toooo(m, m, j, j)
end do 

term(2) = term(2) * (-1.9999999999999998d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,j,m) * tovov(m, e, j, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,j,m) * tovov(m, f, j, e)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajaiaj_j = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aiajaiaj_j = eom_ccsd_22_trans_aiajaiaj_j + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaiaj_j
    function eom_ccsd_22_trans_aiajaiaj_ai(t2, nocc, nactive, a, i) 
    real(F64) :: eom_ccsd_22_trans_aiajaiaj_ai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, i, i)
term(1) = term(1) + tvoov(a, i, i, a)

term(0) = term(0) * (-1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,i,m) * tovov(m, a, i, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,i) * tovov(m, a, i, e)
term(4) = term(4) + t2(a,e,i,m) * tovov(m, a, i, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajaiaj_ai = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiajaiaj_ai = eom_ccsd_22_trans_aiajaiaj_ai + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaiaj_ai
    function eom_ccsd_22_trans_aiajaiaj_aj(t2, nocc, nactive, a, j) 
    real(F64) :: eom_ccsd_22_trans_aiajaiaj_aj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, j, j)
term(1) = term(1) + tvoov(a, j, j, a)

term(0) = term(0) * (-1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + t2(a,a,j,m) * tovov(m, a, j, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, a, j, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, a, j, e)
term(5) = term(5) + t2(a,e,m,j) * tovov(m, e, j, a)
end do 
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,j,m) * tovov(m, e, j, a)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aiajaiaj_aj = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiajaiaj_aj = eom_ccsd_22_trans_aiajaiaj_aj + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaiaj_aj
    function eom_ccsd_22_trans_aiajaiaj_ij(t2, nocc, nactive, i, j) 
    real(F64) :: eom_ccsd_22_trans_aiajaiaj_ij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(j, i, i, j)
term(1) = term(1) + toooo(j, j, i, i)


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(j, f, i, e)
end do 
end do 


do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(j, e, i, f)
end do 
end do 



    eom_ccsd_22_trans_aiajaiaj_ij = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajaiaj_ij = eom_ccsd_22_trans_aiajaiaj_ij + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaiaj_ij
    function eom_ccsd_22_trans_aiajaiaj_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_trans_aiajaiaj_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, a, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, a, i, e)
term(2) = term(2) + t2(a,e,j,i) * tovov(j, e, i, a)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aiajaiaj_aij = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aiajaiaj_aij = eom_ccsd_22_trans_aiajaiaj_aij + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaiaj_aij
    function eom_ccsd_22_trans_aiaiakai_ik(t2, nocc, nactive, i, k) 
    real(F64) :: eom_ccsd_22_trans_aiaiakai_ik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(k, i)
term(1) = term(1) + toooo(k, i, i, i)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(2) = term(2) + toooo(m, i, k, m)
term(3) = term(3) + toooo(m, m, k, i)
end do 

term(3) = term(3) * (-1.9999999999999996d+0) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,i) * tovov(k, e, i, f)
end do 
end do 

term(4) = term(4) * (0.5000000000000001d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(5) = term(5) + t2(e,f,i,i) * tovov(k, f, i, e)
end do 
end do 

term(5) = term(5) * (0.5000000000000001d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(e,f,i,m) * tovov(m, e, k, f)
end do 
end do 
end do 


do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(7) = term(7) + t2(e,f,i,m) * tovov(m, f, k, e)
end do 
end do 
end do 

term(7) = term(7) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aiaiakai_ik = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiaiakai_ik = eom_ccsd_22_trans_aiaiakai_ik + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiakai_ik
    function eom_ccsd_22_trans_aiaiakai_aik(t2, nocc, nactive, a, i, k) 
    real(F64) :: eom_ccsd_22_trans_aiaiakai_aik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, k, i)
term(1) = term(1) + tvoov(a, i, k, a)

term(0) = term(0) * (-1.9999999999999996d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(k, e, i, a)
term(3) = term(3) + t2(a,e,i,i) * tovov(k, a, i, e)
end do 

term(2) = term(2) * (-0.9999999999999998d+0) 
term(3) = term(3) * (-1.0000000000000007d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,a,i,m) * tovov(m, a, k, a)
end do 

term(4) = term(4) * (-0.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, a, k, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, a, k, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(5) = term(5) * (2.000000000000001d+0) 
term(6) = term(6) * (-1.0000000000000007d+0) 
term(7) = term(7) * (-1.0000000000000007d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(8) = term(8) * (2.0000000000000013d+0) 


    eom_ccsd_22_trans_aiaiakai_aik = 0.d+0
    do s = 0, 8
    eom_ccsd_22_trans_aiaiakai_aik = eom_ccsd_22_trans_aiaiakai_aik + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiakai_aik
    function eom_ccsd_22_trans_aiaiakak_ik(t2, nocc, nactive, i, k) 
    real(F64) :: eom_ccsd_22_trans_aiaiakak_ik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, k 
    integer :: s ,e,f 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(k, i, k, i)


do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(1) = term(1) + t2(e,f,i,i) * tovov(k, f, k, e)
end do 
end do 



    eom_ccsd_22_trans_aiaiakak_ik = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiaiakak_ik = eom_ccsd_22_trans_aiaiakak_ik + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiakak_ik
    function eom_ccsd_22_trans_aiaiakak_aik(t2, nocc, nactive, a, i, k) 
    real(F64) :: eom_ccsd_22_trans_aiaiakak_aik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k 
    integer :: s ,e,f 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tovov(k, e, k, a)
end do 

term(0) = term(0) * (-2.0000000000000018d+0) 


    eom_ccsd_22_trans_aiaiakak_aik = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiaiakak_aik = eom_ccsd_22_trans_aiaiakak_aik + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiakak_aik
    function eom_ccsd_22_trans_aiajajaj_ij(t2, nocc, nactive, i, j) 
    real(F64) :: eom_ccsd_22_trans_aiajajaj_ij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j 
    integer :: s ,m,e,f 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(j, i)
term(1) = term(1) + toooo(j, j, j, i)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + toooo(m, i, j, m)
term(3) = term(3) + toooo(m, m, j, i)
end do 

term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-3.9999999999999996d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,j) * tovov(j, f, j, e)
end do 
end do 

term(4) = term(4) * (2.0000000000000004d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(e,f,i,m) * tovov(m, e, j, f)
end do 
end do 
end do 

term(5) = term(5) * (2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(6) = term(6) + t2(e,f,i,m) * tovov(m, f, j, e)
end do 
end do 
end do 

term(6) = term(6) * (-4.000000000000001d+0) 


    eom_ccsd_22_trans_aiajajaj_ij = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiajajaj_ij = eom_ccsd_22_trans_aiajajaj_ij + term(s)
    end do

    end function eom_ccsd_22_trans_aiajajaj_ij
    function eom_ccsd_22_trans_aiajajaj_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_trans_aiajajaj_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,m,e,f 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, j, i)
term(1) = term(1) + tvoov(a, i, j, a)

term(0) = term(0) * (-3.9999999999999996d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(j, e, j, a)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, e, j, a)
end do 

term(2) = term(2) * (-2.000000000000001d+0) 
term(3) = term(3) * (-2.000000000000001d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,a,i,m) * tovov(m, a, j, a)
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, a, j, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, a, j, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, j, a)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 
term(7) = term(7) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, j, a)
end do 
end do 

term(8) = term(8) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aiajajaj_aij = 0.d+0
    do s = 0, 8
    eom_ccsd_22_trans_aiajajaj_aij = eom_ccsd_22_trans_aiajajaj_aij + term(s)
    end do

    end function eom_ccsd_22_trans_aiajajaj_aij
    function eom_ccsd_22_trans_aiaiaidi_ad(t2, nocc, nactive, a, d) 
    real(F64) :: eom_ccsd_22_trans_aiaiaidi_ad 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, d)
term(1) = term(1) + read_ftvvvv(a, d, a, a)


do m = 1, nocc 
term(2) = term(2) + tvvoo(a, d, m, m)
term(3) = term(3) + tvoov(a, m, m, d)
end do 

term(2) = term(2) * (1.9999999999999996d+0) 
term(3) = term(3) * (-0.9999999999999998d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,a,m,n) * tovov(n, d, m, a)
term(5) = term(5) + t2(a,a,m,n) * tovov(n, a, m, d)
end do 
end do 

term(4) = term(4) * (0.5000000000000001d+0) 
term(5) = term(5) * (0.5000000000000001d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(7) = term(7) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aiaiaidi_ad = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiaiaidi_ad = eom_ccsd_22_trans_aiaiaidi_ad + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiaidi_ad
    function eom_ccsd_22_trans_aiaiaidi_aid(t2, nocc, nactive, a, i, d) 
    real(F64) :: eom_ccsd_22_trans_aiaiaidi_aid 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, d)
term(1) = term(1) + tvvoo(a, d, i, i)

term(1) = term(1) * (-1.9999999999999996d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(i, e, i, d)
end do 

term(2) = term(2) * (-1.0000000000000007d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,a,i,m) * tovov(m, a, i, d)
term(4) = term(4) + t2(a,a,i,m) * tovov(m, d, i, a)
end do 

term(3) = term(3) * (-0.9999999999999998d+0) 
term(4) = term(4) * (-0.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, d, i, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, d, i, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(5) = term(5) * (-0.9999999999999998d+0) 
term(6) = term(6) * (2.000000000000001d+0) 
term(7) = term(7) * (-0.9999999999999998d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(8) = term(8) * (1.9999999999999996d+0) 


    eom_ccsd_22_trans_aiaiaidi_aid = 0.d+0
    do s = 0, 8
    eom_ccsd_22_trans_aiaiaidi_aid = eom_ccsd_22_trans_aiaiaidi_aid + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiaidi_aid
    function eom_ccsd_22_trans_aibiaiai_ab(t2, nocc, nactive, a, b) 
    real(F64) :: eom_ccsd_22_trans_aibiaiai_ab 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(b, a)
term(1) = term(1) + read_ftvvvv(b, a, a, a)

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + tvvoo(b, a, m, m)
term(3) = term(3) + tvoov(b, m, m, a)
end do 

term(2) = term(2) * (3.9999999999999996d+0) 
term(3) = term(3) * (-1.9999999999999998d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,n) * tovov(n, a, m, a)
end do 
end do 

term(4) = term(4) * (2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,m,n) * tovov(n, a, m, e)
end do 
end do 
end do 

term(5) = term(5) * (2.0000000000000004d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,m,n) * tovov(n, e, m, a)
end do 
end do 
end do 

term(6) = term(6) * (-4.000000000000001d+0) 


    eom_ccsd_22_trans_aibiaiai_ab = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibiaiai_ab = eom_ccsd_22_trans_aibiaiai_ab + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaiai_ab
    function eom_ccsd_22_trans_aibiaiai_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_trans_aibiaiai_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,m,e,n 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, a, i, i)
term(1) = term(1) + tvoov(b, i, i, a)

term(0) = term(0) * (-3.9999999999999996d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(i, e, i, a)
end do 

term(2) = term(2) * (-2.000000000000001d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, i, a)
term(4) = term(4) + t2(a,b,m,i) * tovov(m, a, i, a)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,m,i) * tovov(m, a, i, e)
term(6) = term(6) + t2(b,e,i,m) * tovov(m, a, i, e)
term(7) = term(7) + t2(b,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 
term(7) = term(7) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(8) = term(8) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibiaiai_aib = 0.d+0
    do s = 0, 8
    eom_ccsd_22_trans_aibiaiai_aib = eom_ccsd_22_trans_aibiaiai_aib + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaiai_aib
    function eom_ccsd_22_trans_aibiaibi_a(t2, nocc, nactive, a) 
    real(F64) :: eom_ccsd_22_trans_aibiaibi_a 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, a)


do m = 1, nocc 
term(1) = term(1) + tvvoo(a, a, m, m)
term(2) = term(2) + tvoov(a, m, m, a)
end do 

term(1) = term(1) * (1.9999999999999998d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,n) * tovov(n, a, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,n) * tovov(n, e, m, a)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiaibi_a = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibiaibi_a = eom_ccsd_22_trans_aibiaibi_a + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaibi_a
    function eom_ccsd_22_trans_aibiaibi_b(t2, nocc, nactive, b) 
    real(F64) :: eom_ccsd_22_trans_aibiaibi_b 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(b, b)


do m = 1, nocc 
term(1) = term(1) + tvvoo(b, b, m, m)
term(2) = term(2) + tvoov(b, m, m, b)
end do 

term(1) = term(1) * (1.9999999999999998d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiaibi_b = 0.d+0
    do s = 0, 4
    eom_ccsd_22_trans_aibiaibi_b = eom_ccsd_22_trans_aibiaibi_b + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaibi_b
    function eom_ccsd_22_trans_aibiaibi_i(t2, nocc, nactive, i) 
    real(F64) :: eom_ccsd_22_trans_aibiaibi_i 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(i, i)
term(1) = term(1) + toooo(i, i, i, i)

term(0) = term(0) * (-2.0d+0) 

do m = 1, nocc 
term(2) = term(2) + toooo(m, i, i, m)
term(3) = term(3) + toooo(m, m, i, i)
end do 

term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-3.9999999999999996d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,i) * tovov(i, f, i, e)
end do 
end do 


do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(e,f,i,m) * tovov(m, e, i, f)
end do 
end do 
end do 

term(5) = term(5) * (2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(6) = term(6) + t2(e,f,i,m) * tovov(m, f, i, e)
end do 
end do 
end do 

term(6) = term(6) * (-4.000000000000001d+0) 


    eom_ccsd_22_trans_aibiaibi_i = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibiaibi_i = eom_ccsd_22_trans_aibiaibi_i + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaibi_i
    function eom_ccsd_22_trans_aibiaibi_ai(t2, nocc, nactive, a, i) 
    real(F64) :: eom_ccsd_22_trans_aibiaibi_ai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, i, i)
term(1) = term(1) + tvoov(a, i, i, a)

term(0) = term(0) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(i, e, i, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,i) * tovov(m, a, i, e)
term(4) = term(4) + t2(a,e,i,m) * tovov(m, a, i, e)
term(5) = term(5) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiaibi_ai = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibiaibi_ai = eom_ccsd_22_trans_aibiaibi_ai + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaibi_ai
    function eom_ccsd_22_trans_aibiaibi_ib(t2, nocc, nactive, i, b) 
    real(F64) :: eom_ccsd_22_trans_aibiaibi_ib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, i, b)
term(1) = term(1) + tvvoo(b, b, i, i)

term(1) = term(1) * (-1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,i) * tovov(i, e, i, b)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, b, i, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, b, i, e)
term(5) = term(5) + t2(b,e,m,i) * tovov(m, e, i, b)
end do 
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, e, i, b)
end do 
end do 

term(6) = term(6) * (2.0000000000000004d+0) 


    eom_ccsd_22_trans_aibiaibi_ib = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibiaibi_ib = eom_ccsd_22_trans_aibiaibi_ib + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaibi_ib
    function eom_ccsd_22_trans_aibiaibi_ab(t2, nocc, nactive, a, b) 
    real(F64) :: eom_ccsd_22_trans_aibiaibi_ab 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, b, a, a)
term(1) = term(1) + read_ftvvvv(b, a, a, b)


do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,n) * tovov(n, b, m, a)
term(3) = term(3) + t2(a,b,m,n) * tovov(n, a, m, b)
end do 
end do 



    eom_ccsd_22_trans_aibiaibi_ab = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibiaibi_ab = eom_ccsd_22_trans_aibiaibi_ab + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaibi_ab
    function eom_ccsd_22_trans_aibiaibi_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_trans_aibiaibi_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, i, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, i, b)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, i, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 


    eom_ccsd_22_trans_aibiaibi_aib = 0.d+0
    do s = 0, 3
    eom_ccsd_22_trans_aibiaibi_aib = eom_ccsd_22_trans_aibiaibi_aib + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaibi_aib
    function eom_ccsd_22_trans_aiaiciai_ac(t2, nocc, nactive, a, c) 
    real(F64) :: eom_ccsd_22_trans_aiaiciai_ac 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, c)
term(1) = term(1) + read_ftvvvv(a, c, a, a)


do m = 1, nocc 
term(2) = term(2) + tvvoo(a, c, m, m)
term(3) = term(3) + tvoov(a, m, m, c)
end do 

term(2) = term(2) * (1.9999999999999996d+0) 
term(3) = term(3) * (-0.9999999999999998d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,a,m,n) * tovov(n, a, m, c)
term(5) = term(5) + t2(a,a,m,n) * tovov(n, c, m, a)
end do 
end do 

term(4) = term(4) * (0.5000000000000001d+0) 
term(5) = term(5) * (0.5000000000000001d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 


do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(7) = term(7) + t2(a,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(7) = term(7) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aiaiciai_ac = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiaiciai_ac = eom_ccsd_22_trans_aiaiciai_ac + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiciai_ac
    function eom_ccsd_22_trans_aiaiciai_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_ccsd_22_trans_aiaiciai_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, i, i)
term(1) = term(1) + tvoov(a, i, i, c)

term(0) = term(0) * (-1.9999999999999996d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(i, e, i, c)
end do 

term(2) = term(2) * (-0.9999999999999998d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,a,i,m) * tovov(m, c, i, a)
term(4) = term(4) + t2(a,a,i,m) * tovov(m, a, i, c)
end do 

term(3) = term(3) * (-0.9999999999999998d+0) 
term(4) = term(4) * (-0.9999999999999998d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, c, i, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, c, i, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(5) = term(5) * (2.000000000000001d+0) 
term(6) = term(6) * (-1.0000000000000007d+0) 
term(7) = term(7) * (-0.9999999999999998d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(8) = term(8) * (1.9999999999999996d+0) 


    eom_ccsd_22_trans_aiaiciai_aic = 0.d+0
    do s = 0, 8
    eom_ccsd_22_trans_aiaiciai_aic = eom_ccsd_22_trans_aiaiciai_aic + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiciai_aic
    function eom_ccsd_22_trans_aiaicici_ac(t2, nocc, nactive, a, c) 
    real(F64) :: eom_ccsd_22_trans_aiaicici_ac 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, c 
    integer :: s ,m,n 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(a, c, a, c)


do n = 1, nocc 
do m = 1, nocc 
term(1) = term(1) + t2(a,a,m,n) * tovov(n, c, m, c)
end do 
end do 



    eom_ccsd_22_trans_aiaicici_ac = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aiaicici_ac = eom_ccsd_22_trans_aiaicici_ac + term(s)
    end do

    end function eom_ccsd_22_trans_aiaicici_ac
    function eom_ccsd_22_trans_aiaicici_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_ccsd_22_trans_aiaicici_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,m,n 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,a,i,m) * tovov(m, c, i, c)
end do 

term(0) = term(0) * (-2.000000000000001d+0) 


    eom_ccsd_22_trans_aiaicici_aic = 0.d+0
    do s = 0, 0
    eom_ccsd_22_trans_aiaicici_aic = eom_ccsd_22_trans_aiaicici_aic + term(s)
    end do

    end function eom_ccsd_22_trans_aiaicici_aic
    function eom_ccsd_22_trans_aibibibi_ab(t2, nocc, nactive, a, b) 
    real(F64) :: eom_ccsd_22_trans_aibibibi_ab 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b 
    integer :: s ,m,e,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, b)
term(1) = term(1) + read_ftvvvv(b, b, a, b)

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do m = 1, nocc 
term(2) = term(2) + tvvoo(a, b, m, m)
term(3) = term(3) + tvoov(a, m, m, b)
end do 

term(2) = term(2) * (3.9999999999999996d+0) 
term(3) = term(3) * (-1.9999999999999998d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,n) * tovov(n, b, m, b)
end do 
end do 

term(4) = term(4) * (2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,n) * tovov(n, b, m, e)
end do 
end do 
end do 

term(5) = term(5) * (2.0000000000000004d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, b)
end do 
end do 
end do 

term(6) = term(6) * (-4.000000000000001d+0) 


    eom_ccsd_22_trans_aibibibi_ab = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aibibibi_ab = eom_ccsd_22_trans_aibibibi_ab + term(s)
    end do

    end function eom_ccsd_22_trans_aibibibi_ab
    function eom_ccsd_22_trans_aibibibi_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_trans_aibibibi_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,m,e,n 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, b, i, i)
term(1) = term(1) + tvoov(a, i, i, b)

term(0) = term(0) * (-3.9999999999999996d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(i, e, i, b)
end do 

term(2) = term(2) * (-2.000000000000001d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, b)
term(4) = term(4) + t2(a,b,m,i) * tovov(m, b, i, b)
end do 

term(3) = term(3) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,i) * tovov(m, b, i, e)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, b, i, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, i, b)
end do 
end do 

term(5) = term(5) * (4.000000000000001d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 
term(7) = term(7) * (-2.0000000000000004d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, e, i, b)
end do 
end do 

term(8) = term(8) * (4.000000000000001d+0) 


    eom_ccsd_22_trans_aibibibi_aib = 0.d+0
    do s = 0, 8
    eom_ccsd_22_trans_aibibibi_aib = eom_ccsd_22_trans_aibibibi_aib + term(s)
    end do

    end function eom_ccsd_22_trans_aibibibi_aib
    function eom_ccsd_22_trans_aiaiaiai_a(t2, nocc, nactive, a) 
    real(F64) :: eom_ccsd_22_trans_aiaiaiai_a 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, a)
term(1) = term(1) + read_ftvvvv(a, a, a, a)

term(0) = term(0) * (2.0d+0) 

do m = 1, nocc 
term(2) = term(2) + tvvoo(a, a, m, m)
term(3) = term(3) + tvoov(a, m, m, a)
end do 

term(2) = term(2) * (3.999999999999999d+0) 
term(3) = term(3) * (-1.9999999999999996d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(4) = term(4) + t2(a,a,m,n) * tovov(n, a, m, a)
end do 
end do 


do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,m,n) * tovov(n, a, m, e)
end do 
end do 
end do 

term(5) = term(5) * (2.000000000000001d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,m,n) * tovov(n, e, m, a)
end do 
end do 
end do 

term(6) = term(6) * (-4.000000000000002d+0) 


    eom_ccsd_22_trans_aiaiaiai_a = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiaiaiai_a = eom_ccsd_22_trans_aiaiaiai_a + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiaiai_a
    function eom_ccsd_22_trans_aiaiaiai_i(t2, nocc, nactive, i) 
    real(F64) :: eom_ccsd_22_trans_aiaiaiai_i 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(i, i)
term(1) = term(1) + toooo(i, i, i, i)

term(0) = term(0) * (-2.0d+0) 

do m = 1, nocc 
term(2) = term(2) + toooo(m, i, i, m)
term(3) = term(3) + toooo(m, m, i, i)
end do 

term(2) = term(2) * (1.9999999999999996d+0) 
term(3) = term(3) * (-3.999999999999999d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,i) * tovov(i, f, i, e)
end do 
end do 


do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(e,f,i,m) * tovov(m, e, i, f)
end do 
end do 
end do 

term(5) = term(5) * (2.000000000000001d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(6) = term(6) + t2(e,f,i,m) * tovov(m, f, i, e)
end do 
end do 
end do 

term(6) = term(6) * (-4.000000000000002d+0) 


    eom_ccsd_22_trans_aiaiaiai_i = 0.d+0
    do s = 0, 6
    eom_ccsd_22_trans_aiaiaiai_i = eom_ccsd_22_trans_aiaiaiai_i + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiaiai_i
    function eom_ccsd_22_trans_aiaiaiai_ai(t2, nocc, nactive, a, i) 
    real(F64) :: eom_ccsd_22_trans_aiaiaiai_ai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, i, i)
term(1) = term(1) + tvoov(a, i, i, a)

term(0) = term(0) * (-3.999999999999999d+0) 
term(1) = term(1) * (1.9999999999999991d+0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,i,i) * tovov(i, e, i, a)
end do 

term(2) = term(2) * (-2.0000000000000018d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,a,i,m) * tovov(m, a, i, a)
end do 

term(3) = term(3) * (-2.000000000000001d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,i) * tovov(m, a, i, e)
term(5) = term(5) + t2(a,e,i,m) * tovov(m, a, i, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(4) = term(4) * (4.000000000000002d+0) 
term(5) = term(5) * (-2.0000000000000018d+0) 
term(6) = term(6) * (-2.000000000000001d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(7) = term(7) * (4.000000000000002d+0) 


    eom_ccsd_22_trans_aiaiaiai_ai = 0.d+0
    do s = 0, 7
    eom_ccsd_22_trans_aiaiaiai_ai = eom_ccsd_22_trans_aiaiaiai_ai + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiaiai_ai
!-------------------------------------------------------------functions for diag
   function eom_ccsd_22_trans_aibjaibj(t2, nocc, nactive, a, i, b, j)
    double precision :: eom_ccsd_22_trans_aibjaibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e,f,n 
    double precision, dimension(0:47) :: term 
    term = 0.d+0
    do m = 1, nocc
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, i, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, j, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, j, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, a)
term(4) = term(4) + toooo(m, i, i, m)
term(5) = term(5) + toooo(m, m, i, i)
term(6) = term(6) + toooo(m, j, j, m)
term(7) = term(7) + toooo(m, m, j, j)
term(8) = term(8) + tvvoo(a, a, m, m)
term(9) = term(9) + tvvoo(b, b, m, m)
term(10) = term(10) + tvoov(a, m, m, a)
term(11) = term(11) + tvoov(b, m, m, b)
end do

term(1) = term(1) * (-2.0d+0)
term(3) = term(3) * (-2.0d+0)
term(5) = term(5) * (-2.0d+0)
term(7) = term(7) * (-2.0d+0)
term(8) = term(8) * 2.0d+0
term(9) = term(9) * 2.0d+0
term(10) = -term(10)
term(11) = -term(11)

do e = nocc + 1, nactive
do n = 1, nocc
do m = 1, nocc
term(12) = term(12) + t2(a,e,m,n) * tovov(n, a, m, e)
term(13) = term(13) + t2(b,e,m,n) * tovov(n, b, m, e)
end do
end do
end do


do e = nocc + 1, nactive
do f = nocc + 1, nactive
term(14) = term(14) + t2(e,f,i,j) * tovov(j, f, i, e)
end do
end do


do e = nocc + 1, nactive
term(15) = term(15) + t2(b,e,j,i) * tovov(j, b, i, e)
term(16) = term(16) + t2(a,e,i,j) * tovov(j, a, i, e)
term(17) = term(17) + t2(b,e,j,i) * tovov(j, e, i, b)
term(18) = term(18) + t2(a,e,i,j) * tovov(j, e, i, a)
end do

term(15) = term(15) * (-2.0d+0)
term(18) = term(18) * (-2.0d+0)
term(19) = term(19) + tvv(a, a)
term(20) = term(20) + tvv(b, b)
term(21) = term(21) + too(i, i)
term(22) = term(22) + too(j, j)
term(23) = term(23) + read_ftvvvv(b, b, a, a)
term(24) = term(24) + tvvoo(a, a, i, i)
term(25) = term(25) + tvvoo(a, a, j, j)
term(26) = term(26) + tvoov(a, i, i, a)
term(27) = term(27) + tvvoo(b, b, i, i)
term(28) = term(28) + tvvoo(b, b, j, j)
term(29) = term(29) + tvoov(b, j, j, b)
term(30) = term(30) + toooo(j, j, i, i)

term(21) = -term(21)
term(22) = -term(22)
term(24) = -term(24)
term(25) = -term(25)
term(26) = term(26) * 2.0d+0
term(27) = -term(27)
term(28) = -term(28)
term(29) = term(29) * 2.0d+0
do e = nocc + 1, nactive
do m = 1, nocc
term(31) = term(31) + t2(b,e,m,i) * tovov(m, b, i, e)
term(32) = term(32) + t2(a,e,m,i) * tovov(m, a, i, e)
term(33) = term(33) + t2(a,e,i,m) * tovov(m, a, i, e)
term(34) = term(34) + t2(b,e,m,j) * tovov(m, b, j, e)
term(35) = term(35) + t2(a,e,m,j) * tovov(m, a, j, e)
term(36) = term(36) + t2(b,e,j,m) * tovov(m, b, j, e)
term(37) = term(37) + t2(a,e,m,i) * tovov(m, e, i, a)
term(38) = term(38) + t2(b,e,m,j) * tovov(m, e, j, b)
end do
end do

term(33) = term(33) * (-2.0d+0)
term(36) = term(36) * (-2.0d+0)
term(37) = term(37) * (-2.0d+0)
term(38) = term(38) * (-2.0d+0)

do n = 1, nocc
do m = 1, nocc
term(39) = term(39) + t2(a,b,m,n) * tovov(n, b, m, a)
end do
end do


do m = 1, nocc
do e = nocc + 1, nactive
term(40) = term(40) + t2(a,e,i,m) * tovov(m, e, i, a)
term(41) = term(41) + t2(b,e,j,m) * tovov(m, e, j, b)
end do
end do

term(40) = term(40) * 4.0d+0
term(41) = term(41) * 4.0d+0
do f = nocc + 1, nactive
do m = 1, nocc
do e = nocc + 1, nactive
term(42) = term(42) + t2(e,f,i,m) * tovov(m, e, i, f)
term(43) = term(43) + t2(e,f,j,m) * tovov(m, e, j, f)
end do
end do
end do


do e = nocc + 1, nactive
do m = 1, nocc
do f = nocc + 1, nactive
term(44) = term(44) + t2(e,f,i,m) * tovov(m, f, i, e)
term(45) = term(45) + t2(e,f,j,m) * tovov(m, f, j, e)
end do
end do
end do
term(44) = term(44) * (-2.0d+0)
term(45) = term(45) * (-2.0d+0)

do n = 1, nocc
do e = nocc + 1, nactive
do m = 1, nocc
term(46) = term(46) + t2(a,e,m,n) * tovov(n, e, m, a)
term(47) = term(47) + t2(b,e,m,n) * tovov(n, e, m, b)
end do
end do
end do

term(46) = term(46) * (-2.0d+0)
term(47) = term(47) * (-2.0d+0)


    eom_ccsd_22_trans_aibjaibj = 0.d+0
    do s = 0, 47
    eom_ccsd_22_trans_aibjaibj = eom_ccsd_22_trans_aibjaibj + term(s)
    end do

    end function eom_ccsd_22_trans_aibjaibj


   function eom_ccsd_22_trans_aiajaiaj(t2, nocc, nactive, a, i, j)
    double precision :: eom_ccsd_22_trans_aiajaiaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,m,e,n,f 
    double precision, dimension(0:38) :: term 
    term = 0.d+0
    do m = 1, nocc
term(0) = term(0) + toooo(m, i, i, m)
term(1) = term(1) + toooo(m, m, i, i)
term(2) = term(2) + toooo(m, j, j, m)
term(3) = term(3) + toooo(m, m, j, j)
term(4) = term(4) + tvvoo(a, a, m, m)
term(5) = term(5) + tvoov(a, m, m, a)
term(6) = term(6) + t2(a,a,i,m) * tovov(m, a, i, a)
term(7) = term(7) + t2(a,a,j,m) * tovov(m, a, j, a)
end do

term(1) = term(1) * (-2.0d+0)
term(3) = term(3) * (-2.0d+0)
term(4) = term(4) * 4.0d+0
term(5) = term(5) * (-2.0d+0)
term(6) = -term(6)
term(7) = -term(7)

term(8) = term(8) + tvv(a, a)
term(9) = term(9) + read_ftvvvv(a, a, a, a)
term(10) = term(10) + too(i, i)
term(11) = term(11) + too(j, j)
term(12) = term(12) + tvvoo(a, a, i, i)
term(13) = term(13) + tvvoo(a, a, j, j)
term(14) = term(14) + tvoov(a, i, i, a)
term(15) = term(15) + tvoov(a, j, j, a)
term(16) = term(16) + toooo(j, i, i, j)
term(17) = term(17) + toooo(j, j, i, i)
term(8) = term(8) * 2.0d+0
term(10) = -term(10)
term(11) = -term(11)
term(12) = term(12) * (-2.0d+0)
term(13) = term(13) * (-2.0d+0)

do e = nocc + 1, nactive
do m = 1, nocc
term(18) = term(18) + t2(a,e,m,i) * tovov(m, a, i, e)
term(19) = term(19) + t2(a,e,i,m) * tovov(m, a, i, e)
term(20) = term(20) + t2(a,e,m,j) * tovov(m, a, j, e)
term(21) = term(21) + t2(a,e,j,m) * tovov(m, a, j, e)
term(22) = term(22) + t2(a,e,m,i) * tovov(m, e, i, a)
term(23) = term(23) + t2(a,e,m,j) * tovov(m, e, j, a)
end do
end do

term(18) = term(18) * 2.0d+0
term(19) = -term(19)
term(20) = term(20) * 2.0d+0
term(21) = -term(21)
term(22) = -term(22)
term(23) = -term(23)

do m = 1, nocc
do e = nocc + 1, nactive
term(24) = term(24) + t2(a,e,i,m) * tovov(m, e, i, a)
term(25) = term(25) + t2(a,e,j,m) * tovov(m, e, j, a)
end do
end do

term(24) = term(24) * 2.0d+0
term(25) = term(25) * 2.0d+0

do n = 1, nocc
do m = 1, nocc
term(26) = term(26) + t2(a,a,m,n) * tovov(n, a, m, a)
end do
end do


do f = nocc + 1, nactive
do m = 1, nocc
do e = nocc + 1, nactive
term(27) = term(27) + t2(e,f,i,m) * tovov(m, e, i, f)
term(28) = term(28) + t2(e,f,j,m) * tovov(m, e, j, f)
end do
end do
end do


do e = nocc + 1, nactive
do m = 1, nocc
do f = nocc + 1, nactive
term(29) = term(29) + t2(e,f,i,m) * tovov(m, f, i, e)
term(30) = term(30) + t2(e,f,j,m) * tovov(m, f, j, e)
end do
end do
end do

term(29) = term(29) * (-2.0d+0)
term(30) = term(30) * (-2.0d+0)

do n = 1, nocc
do e = nocc + 1, nactive
do m = 1, nocc
term(31) = term(31) + t2(a,e,m,n) * tovov(n, e, m, a)
end do
end do
end do

term(31) = term(31) * (-4.0d+0)

do e = nocc + 1, nactive
do n = 1, nocc
do m = 1, nocc
term(32) = term(32) + t2(a,e,m,n) * tovov(n, a, m, e)
end do
end do
end do

term(32) = term(32) * 2.0d+0

do f = nocc + 1, nactive
do e = nocc + 1, nactive
term(33) = term(33) + t2(e,f,i,j) * tovov(j, e, i, f)
end do
end do


do e = nocc + 1, nactive
do f = nocc + 1, nactive
term(34) = term(34) + t2(e,f,i,j) * tovov(j, f, i, e)
end do
end do
do e = nocc + 1, nactive
term(35) = term(35) + t2(a,e,i,j) * tovov(j, a, i, e)
term(36) = term(36) + t2(a,e,j,i) * tovov(j, a, i, e)
term(37) = term(37) + t2(a,e,i,j) * tovov(j, e, i, a)
term(38) = term(38) + t2(a,e,j,i) * tovov(j, e, i, a)
end do

term(35) = -term(35)
term(36) = -term(36)
term(37) = -term(37)
term(38) = -term(38)


    eom_ccsd_22_trans_aiajaiaj = 0.d+0
    do s = 0, 38
    eom_ccsd_22_trans_aiajaiaj = eom_ccsd_22_trans_aiajaiaj + term(s)
    end do

    end function eom_ccsd_22_trans_aiajaiaj
    function eom_ccsd_22_trans_aibiaibi(t2, nocc, nactive, a, i, b)
    double precision :: eom_ccsd_22_trans_aibiaibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,m,e,f,n 
    double precision, dimension(0:38) :: term 
    term = 0.d+0
    do m = 1, nocc
term(0) = term(0) + t2(a,b,i,m) * tovov(m, a, i, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, i, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, i, a)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, b, i, a)
term(4) = term(4) + toooo(m, i, i, m)
term(5) = term(5) + toooo(m, m, i, i)
term(6) = term(6) + tvvoo(a, a, m, m)
term(7) = term(7) + tvvoo(b, b, m, m)
term(8) = term(8) + tvoov(a, m, m, a)
term(9) = term(9) + tvoov(b, m, m, b)
end do
term(0) = -term(0)
term(1) = -term(1)
term(2) = -term(2)
term(3) = -term(3)
term(4) = term(4) * 2.0d+0
term(5) = term(5) * (-4.0d+0)
term(6) = term(6) * 2.0d+0
term(7) = term(7) * 2.0d+0
term(8) = -term(8)
term(9) = -term(9)

do n = 1, nocc
do m = 1, nocc
term(10) = term(10) + t2(a,b,m,n) * tovov(n, a, m, b)
term(11) = term(11) + t2(a,b,m,n) * tovov(n, b, m, a)
end do
end do


term(12) = term(12) + tvv(a, a)
term(13) = term(13) + tvv(b, b)
term(14) = term(14) + too(i, i)
term(15) = term(15) + toooo(i, i, i, i)
term(16) = term(16) + tvvoo(a, a, i, i)
term(17) = term(17) + tvoov(a, i, i, a)
term(18) = term(18) + tvvoo(b, b, i, i)
term(19) = term(19) + tvoov(b, i, i, b)
term(20) = term(20) + read_ftvvvv(b, a, a, b)
term(21) = term(21) + read_ftvvvv(b, b, a, a)

term(14) = term(14) * (-2.0d+0)
term(16) = term(16) * (-2.0d+0)
term(18) = term(18) * (-2.0d+0)

do e = nocc + 1, nactive
do m = 1, nocc
term(22) = term(22) + t2(b,e,m,i) * tovov(m, b, i, e)
term(23) = term(23) + t2(a,e,m,i) * tovov(m, a, i, e)
term(24) = term(24) + t2(b,e,i,m) * tovov(m, b, i, e)
term(25) = term(25) + t2(a,e,i,m) * tovov(m, a, i, e)
term(26) = term(26) + t2(a,e,m,i) * tovov(m, e, i, a)
term(27) = term(27) + t2(b,e,m,i) * tovov(m, e, i, b)
end do
end do

term(22) = term(22) * 2.0d+0
term(23) = term(23) * 2.0d+0
term(24) = -term(24)
term(25) = -term(25)
term(26) = -term(26)
term(27) = -term(27)

do m = 1, nocc
do e = nocc + 1, nactive
term(28) = term(28) + t2(a,e,i,m) * tovov(m, e, i, a)
term(29) = term(29) + t2(b,e,i,m) * tovov(m, e, i, b)
end do
end do

term(28) = term(28) * 2.0d+0
term(29) = term(29) * 2.0d+0

do f = nocc + 1, nactive
do m = 1, nocc
do e = nocc + 1, nactive
term(30) = term(30) + t2(e,f,i,m) * tovov(m, e, i, f)
end do
end do
end do

term(30) = term(30) * 2.0d+0

do e = nocc + 1, nactive
do m = 1, nocc
do f = nocc + 1, nactive
term(31) = term(31) + t2(e,f,i,m) * tovov(m, f, i, e)
end do
end do
end do

term(31) = term(31) * (-4.0d+0)

do n = 1, nocc
do e = nocc + 1, nactive
do m = 1, nocc
term(32) = term(32) + t2(a,e,m,n) * tovov(n, e, m, a)
term(33) = term(33) + t2(b,e,m,n) * tovov(n, e, m, b)
end do
end do
end do

term(32) = term(32) * (-2.0d+0)
term(33) = term(33) * (-2.0d+0)

do e = nocc + 1, nactive
do n = 1, nocc
do m = 1, nocc
term(34) = term(34) + t2(a,e,m,n) * tovov(n, a, m, e)
term(35) = term(35) + t2(b,e,m,n) * tovov(n, b, m, e)
end do
end do
end do


do e = nocc + 1, nactive
term(36) = term(36) + t2(b,e,i,i) * tovov(i, e, i, b)
term(37) = term(37) + t2(a,e,i,i) * tovov(i, e, i, a)
end do

term(36) = -term(36)
term(37) = -term(37)

do e = nocc + 1, nactive
do f = nocc + 1, nactive
term(38) = term(38) + t2(e,f,i,i) * tovov(i, f, i, e)
end do
end do



    eom_ccsd_22_trans_aibiaibi = 0.d+0
    do s = 0, 38
    eom_ccsd_22_trans_aibiaibi = eom_ccsd_22_trans_aibiaibi + term(s)
    end do

    end function eom_ccsd_22_trans_aibiaibi
    function eom_ccsd_22_trans_aiaiaiai(t2, nocc, nactive, a, i)
    double precision :: eom_ccsd_22_trans_aiaiaiai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i 
    integer :: s ,m,e,n,f 
    double precision, dimension(0:21) :: term 
    term = 0.d+0
    do m = 1, nocc
term(0) = term(0) + toooo(m, i, i, m)
term(1) = term(1) + toooo(m, m, i, i)
term(2) = term(2) + tvvoo(a, a, m, m)
term(3) = term(3) + tvoov(a, m, m, a)
term(4) = term(4) + t2(a,a,i,m) * tovov(m, a, i, a)
end do

term(0) = term(0) * 2.0d+0
term(1) = term(1) * (-4.0d+0)
term(2) = term(2) * 4.0d+0
term(3) = term(3) * (-2.0d+0)
term(4) = term(4) * (-2.0d+0)

term(5) = term(5) + tvv(a, a)
term(6) = term(6) + read_ftvvvv(a, a, a, a)
term(7) = term(7) + too(i, i)
term(8) = term(8) + toooo(i, i, i, i)
term(9) = term(9) + tvvoo(a, a, i, i)
term(10) = term(10) + tvoov(a, i, i, a)

term(5) = term(5) * 2.0d+0
term(7) = term(7) * (-2.0d+0)
term(9) = term(9) * (-4.0d+0)
term(10) = term(10) * 2.0d+0

do e = nocc + 1, nactive
do m = 1, nocc
term(11) = term(11) + t2(a,e,m,i) * tovov(m, a, i, e)
term(12) = term(12) + t2(a,e,i,m) * tovov(m, a, i, e)
term(13) = term(13) + t2(a,e,m,i) * tovov(m, e, i, a)
end do
end do

term(11) = term(11) * 4.0d+0
term(12) = term(12) * (-2.0d+0)
term(13) = term(13) * (-2.0d+0)

do m = 1, nocc
do e = nocc + 1, nactive
term(14) = term(14) + t2(a,e,i,m) * tovov(m, e, i, a)
end do
end do

term(14) = term(14) * 4.0d+0

do n = 1, nocc
do m = 1, nocc
term(15) = term(15) + t2(a,a,m,n) * tovov(n, a, m, a)
end do
end do


do f = nocc + 1, nactive
do m = 1, nocc
do e = nocc + 1, nactive
term(16) = term(16) + t2(e,f,i,m) * tovov(m, e, i, f)
end do
end do
end do

term(16) = term(16) * 2.0d+0

do e = nocc + 1, nactive
do m = 1, nocc
do f = nocc + 1, nactive
term(17) = term(17) + t2(e,f,i,m) * tovov(m, f, i, e)
end do
end do
end do

term(17) = term(17) * (-4.0d+0)

do n = 1, nocc
do e = nocc + 1, nactive
do m = 1, nocc
term(18) = term(18) + t2(a,e,m,n) * tovov(n, e, m, a)
end do
end do
end do

term(18) = term(18) * (-4.0d+0)

do e = nocc + 1, nactive
do n = 1, nocc
do m = 1, nocc
term(19) = term(19) + t2(a,e,m,n) * tovov(n, a, m, e)
end do
end do
end do

term(19) = term(19) * 2.0d+0

do e = nocc + 1, nactive
term(20) = term(20) + t2(a,e,i,i) * tovov(i, e, i, a)
end do

term(20) = term(20) * (-2.0d+0)

do e = nocc + 1, nactive
do f = nocc + 1, nactive
term(21) = term(21) + t2(e,f,i,i) * tovov(i, f, i, e)
end do
end do



    eom_ccsd_22_trans_aiaiaiai = 0.d+0
    do s = 0, 21
    eom_ccsd_22_trans_aiaiaiai = eom_ccsd_22_trans_aiaiaiai + term(s)
    end do

    end function eom_ccsd_22_trans_aiaiaiai



   function eom_ccsd_22_trans_aibjakdl(t2, nocc, nactive, i, b, j, k, d, l)
    double precision :: eom_ccsd_22_trans_aibjakdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0
    do e = nocc + 1, nactive
term(0) = term(0) + t2(b,e,j,i) * tovov(l, d, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, d)
end do

term(0) = term(0) * (-2.0d+0)


    eom_ccsd_22_trans_aibjakdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_trans_aibjakdl = eom_ccsd_22_trans_aibjakdl + term(s)
    end do

    end function eom_ccsd_22_trans_aibjakdl


    end module eom_ccsd_22_trans
    
