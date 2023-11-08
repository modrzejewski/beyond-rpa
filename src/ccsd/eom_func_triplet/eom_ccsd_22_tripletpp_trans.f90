module eom_ccsd_22_tripletpp_trans

    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none
    !               
    ! File generated automatically on 2018-12-05 15:48:37  
    !  
    contains
    
    function eom_ccsd_22_tripletpp_trans_aibjakdl_ibjkdl(t2, nocc, nactive, i, b, j, k, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakdl_ibjkdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d, l 
    integer :: s ,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, d, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, d, k, e)
term(2) = term(2) + t2(b,e,j,i) * tovov(l, e, k, d)
term(3) = term(3) + t2(b,e,i,j) * tovov(l, e, k, d)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakdl_ibjkdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakdl_ibjkdl = eom_ccsd_22_tripletpp_trans_aibjakdl_ibjkdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakdl_ibjkdl
    function eom_ccsd_22_tripletpp_trans_aibjckal_ibjckl(t2, nocc, nactive, i, b, j, c, k, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckal_ibjckl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k, l 
    integer :: s ,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, c, k, e)
term(1) = term(1) + t2(b,e,i,j) * tovov(l, c, k, e)
term(2) = term(2) + t2(b,e,i,j) * tovov(l, e, k, c)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckal_ibjckl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjckal_ibjckl = eom_ccsd_22_tripletpp_trans_aibjckal_ibjckl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckal_ibjckl
    function eom_ccsd_22_tripletpp_trans_aibjbkdl_aijkdl(t2, nocc, nactive, a, i, j, k, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbkdl_aijkdl 
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

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbkdl_aijkdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjbkdl_aijkdl = eom_ccsd_22_tripletpp_trans_aibjbkdl_aijkdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbkdl_aijkdl
    function eom_ccsd_22_tripletpp_trans_aibjckbl_aijckl(t2, nocc, nactive, a, i, j, c, k, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckbl_aijckl 
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

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckbl_aijckl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjckbl_aijckl = eom_ccsd_22_tripletpp_trans_aibjckbl_aijckl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckbl_aijckl
    function eom_ccsd_22_tripletpp_trans_aibjckdi_abjckd(t2, nocc, nactive, a, b, j, c, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckdi_abjckd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k, d 
    integer :: s ,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, k, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, k, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, d, k, c)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, d, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckdi_abjckd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjckdi_abjckd = eom_ccsd_22_tripletpp_trans_aibjckdi_abjckd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckdi_abjckd
    function eom_ccsd_22_tripletpp_trans_aibjcidl_abjcdl(t2, nocc, nactive, a, b, j, c, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcidl_abjcdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d, l 
    integer :: s ,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, l, d)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, d, l, c)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, d, l, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcidl_abjcdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcidl_abjcdl = eom_ccsd_22_tripletpp_trans_aibjcidl_abjcdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcidl_abjcdl
    function eom_ccsd_22_tripletpp_trans_aibjckdj_aibckd(t2, nocc, nactive, a, i, b, c, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckdj_aibckd 
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

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckdj_aibckd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjckdj_aibckd = eom_ccsd_22_tripletpp_trans_aibjckdj_aibckd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckdj_aibckd
    function eom_ccsd_22_tripletpp_trans_aibjcjdl_aibcdl(t2, nocc, nactive, a, i, b, c, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcjdl_aibcdl 
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

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcjdl_aibcdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcjdl_aibcdl = eom_ccsd_22_tripletpp_trans_aibjcjdl_aibcdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcjdl_aibcdl
    function eom_ccsd_22_tripletpp_trans_aibjakbl_ijkl(t2, nocc, nactive, i, j, k, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbl_ijkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, k, l 
    integer :: s ,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, i, k, j)
term(1) = term(1) + toooo(l, j, k, i)

term(0) = term(0) * (-1.0d+0) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(l, e, k, f)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(l, f, k, e)
end do 
end do 



    eom_ccsd_22_tripletpp_trans_aibjakbl_ijkl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakbl_ijkl = eom_ccsd_22_tripletpp_trans_aibjakbl_ijkl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbl_ijkl
    function eom_ccsd_22_tripletpp_trans_aibjakbl_aijkl(t2, nocc, nactive, a, i, j, k, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbl_aijkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, l 
    integer :: s ,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, a, k, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, k, a)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, a, k, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, k, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbl_aijkl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakbl_aijkl = eom_ccsd_22_tripletpp_trans_aibjakbl_aijkl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbl_aijkl
    function eom_ccsd_22_tripletpp_trans_aibjakbl_ibjkl(t2, nocc, nactive, i, b, j, k, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbl_ibjkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, l 
    integer :: s ,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, b, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, b, k, e)
term(2) = term(2) + t2(b,e,j,i) * tovov(l, e, k, b)
term(3) = term(3) + t2(b,e,i,j) * tovov(l, e, k, b)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbl_ibjkl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakbl_ibjkl = eom_ccsd_22_tripletpp_trans_aibjakbl_ibjkl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbl_ibjkl
    function eom_ccsd_22_tripletpp_trans_aibjakdi_bjkd(t2, nocc, nactive, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakdi_bjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, d)
term(1) = term(1) + tvvoo(b, d, k, j)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,j,m) * tovov(m, d, k, e)
term(3) = term(3) + t2(b,e,m,j) * tovov(m, d, k, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, k, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, k, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakdi_bjkd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjakdi_bjkd = eom_ccsd_22_tripletpp_trans_aibjakdi_bjkd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakdi_bjkd
    function eom_ccsd_22_tripletpp_trans_aibjakdi_ibjkd(t2, nocc, nactive, i, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakdi_ibjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, i, d)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, i, d)
term(2) = term(2) + t2(b,e,j,i) * tovov(k, d, i, e)
term(3) = term(3) + t2(b,e,i,j) * tovov(k, d, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakdi_ibjkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakdi_ibjkd = eom_ccsd_22_tripletpp_trans_aibjakdi_ibjkd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakdi_ibjkd
    function eom_ccsd_22_tripletpp_trans_aibjakdi_abjkd(t2, nocc, nactive, a, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakdi_abjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, a, k, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, k, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, d, k, a)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, d, k, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakdi_abjkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakdi_abjkd = eom_ccsd_22_tripletpp_trans_aibjakdi_abjkd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakdi_abjkd
    function eom_ccsd_22_tripletpp_trans_aibjaidl_bjdl(t2, nocc, nactive, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaidl_bjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, l, j)
term(1) = term(1) + tvoov(b, j, l, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,j) * tovov(m, d, l, e)
term(3) = term(3) + t2(b,e,j,m) * tovov(m, d, l, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, l, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaidl_bjdl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjaidl_bjdl = eom_ccsd_22_tripletpp_trans_aibjaidl_bjdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaidl_bjdl
    function eom_ccsd_22_tripletpp_trans_aibjaidl_ibjdl(t2, nocc, nactive, i, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaidl_ibjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, d, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, d, i, e)
term(2) = term(2) + t2(b,e,j,i) * tovov(l, e, i, d)
term(3) = term(3) + t2(b,e,i,j) * tovov(l, e, i, d)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaidl_ibjdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaidl_ibjdl = eom_ccsd_22_tripletpp_trans_aibjaidl_ibjdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaidl_ibjdl
    function eom_ccsd_22_tripletpp_trans_aibjaidl_abjdl(t2, nocc, nactive, a, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaidl_abjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, l, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, l, d)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, d, l, a)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, d, l, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaidl_abjdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaidl_abjdl = eom_ccsd_22_tripletpp_trans_aibjaidl_abjdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaidl_abjdl
    function eom_ccsd_22_tripletpp_trans_aibjakdj_ibkd(t2, nocc, nactive, i, b, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakdj_ibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, d)
term(1) = term(1) + tvvoo(b, d, k, i)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,i,m) * tovov(m, d, k, e)
term(3) = term(3) + t2(b,e,m,i) * tovov(m, d, k, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, k, d)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakdj_ibkd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjakdj_ibkd = eom_ccsd_22_tripletpp_trans_aibjakdj_ibkd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakdj_ibkd
    function eom_ccsd_22_tripletpp_trans_aibjakdj_ibjkd(t2, nocc, nactive, i, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakdj_ibjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, j, d)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, j, d)
term(2) = term(2) + t2(b,e,j,i) * tovov(k, d, j, e)
term(3) = term(3) + t2(b,e,i,j) * tovov(k, d, j, e)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakdj_ibjkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakdj_ibjkd = eom_ccsd_22_tripletpp_trans_aibjakdj_ibjkd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakdj_ibjkd
    function eom_ccsd_22_tripletpp_trans_aibjakdj_aibkd(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakdj_aibkd 
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

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakdj_aibkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakdj_aibkd = eom_ccsd_22_tripletpp_trans_aibjakdj_aibkd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakdj_aibkd
    function eom_ccsd_22_tripletpp_trans_aibjajdl_ibdl(t2, nocc, nactive, i, b, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjajdl_ibdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, l, i)
term(1) = term(1) + tvoov(b, i, l, d)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,i) * tovov(m, d, l, e)
term(3) = term(3) + t2(b,e,i,m) * tovov(m, d, l, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjajdl_ibdl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjajdl_ibdl = eom_ccsd_22_tripletpp_trans_aibjajdl_ibdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjajdl_ibdl
    function eom_ccsd_22_tripletpp_trans_aibjajdl_ibjdl(t2, nocc, nactive, i, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjajdl_ibjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, d, j, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, d, j, e)
term(2) = term(2) + t2(b,e,j,i) * tovov(l, e, j, d)
term(3) = term(3) + t2(b,e,i,j) * tovov(l, e, j, d)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjajdl_ibjdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjajdl_ibjdl = eom_ccsd_22_tripletpp_trans_aibjajdl_ibjdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjajdl_ibjdl
    function eom_ccsd_22_tripletpp_trans_aibjajdl_aibdl(t2, nocc, nactive, a, i, b, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjajdl_aibdl 
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

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjajdl_aibdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjajdl_aibdl = eom_ccsd_22_tripletpp_trans_aibjajdl_aibdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjajdl_aibdl
    function eom_ccsd_22_tripletpp_trans_aibjckai_bjck(t2, nocc, nactive, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckai_bjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, k, j)
term(1) = term(1) + tvoov(b, j, k, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,j) * tovov(m, c, k, e)
term(3) = term(3) + t2(b,e,j,m) * tovov(m, c, k, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckai_bjck = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjckai_bjck = eom_ccsd_22_tripletpp_trans_aibjckai_bjck + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckai_bjck
    function eom_ccsd_22_tripletpp_trans_aibjckai_ibjck(t2, nocc, nactive, i, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckai_ibjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, i, c)
term(1) = term(1) + t2(b,e,i,j) * tovov(k, e, i, c)
term(2) = term(2) + t2(b,e,i,j) * tovov(k, c, i, e)
term(3) = term(3) + t2(b,e,j,i) * tovov(k, c, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckai_ibjck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjckai_ibjck = eom_ccsd_22_tripletpp_trans_aibjckai_ibjck + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckai_ibjck
    function eom_ccsd_22_tripletpp_trans_aibjckai_abjck(t2, nocc, nactive, a, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckai_abjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, k, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, k, a)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, k, c)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, a, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckai_abjck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjckai_abjck = eom_ccsd_22_tripletpp_trans_aibjckai_abjck + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckai_abjck
    function eom_ccsd_22_tripletpp_trans_aibjcial_bjcl(t2, nocc, nactive, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcial_bjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, l, j)
term(1) = term(1) + tvoov(b, j, l, c)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,j,m) * tovov(m, c, l, e)
term(3) = term(3) + t2(b,e,m,j) * tovov(m, c, l, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, l, c)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, l, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcial_bjcl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjcial_bjcl = eom_ccsd_22_tripletpp_trans_aibjcial_bjcl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcial_bjcl
    function eom_ccsd_22_tripletpp_trans_aibjcial_ibjcl(t2, nocc, nactive, i, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcial_ibjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, c, i, e)
term(1) = term(1) + t2(b,e,i,j) * tovov(l, c, i, e)
term(2) = term(2) + t2(b,e,i,j) * tovov(l, e, i, c)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, i, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcial_ibjcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcial_ibjcl = eom_ccsd_22_tripletpp_trans_aibjcial_ibjcl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcial_ibjcl
    function eom_ccsd_22_tripletpp_trans_aibjcial_abjcl(t2, nocc, nactive, a, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcial_abjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, a)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, l, a)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, a, l, c)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, a, l, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcial_abjcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcial_abjcl = eom_ccsd_22_tripletpp_trans_aibjcial_abjcl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcial_abjcl
    function eom_ccsd_22_tripletpp_trans_aibjckaj_ibck(t2, nocc, nactive, i, b, c, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckaj_ibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, k, i)
term(1) = term(1) + tvoov(b, i, k, c)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,i) * tovov(m, c, k, e)
term(3) = term(3) + t2(b,e,i,m) * tovov(m, c, k, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckaj_ibck = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjckaj_ibck = eom_ccsd_22_tripletpp_trans_aibjckaj_ibck + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckaj_ibck
    function eom_ccsd_22_tripletpp_trans_aibjckaj_ibjck(t2, nocc, nactive, i, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckaj_ibjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(k, e, j, c)
term(1) = term(1) + t2(b,e,i,j) * tovov(k, e, j, c)
term(2) = term(2) + t2(b,e,i,j) * tovov(k, c, j, e)
term(3) = term(3) + t2(b,e,j,i) * tovov(k, c, j, e)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckaj_ibjck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjckaj_ibjck = eom_ccsd_22_tripletpp_trans_aibjckaj_ibjck + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckaj_ibjck
    function eom_ccsd_22_tripletpp_trans_aibjckaj_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckaj_aibck 
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

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckaj_aibck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjckaj_aibck = eom_ccsd_22_tripletpp_trans_aibjckaj_aibck + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckaj_aibck
    function eom_ccsd_22_tripletpp_trans_aibjcjal_ibcl(t2, nocc, nactive, i, b, c, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcjal_ibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, c)
term(1) = term(1) + tvvoo(b, c, l, i)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,i) * tovov(m, e, l, c)
term(3) = term(3) + t2(b,e,i,m) * tovov(m, c, l, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, c, l, e)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcjal_ibcl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjcjal_ibcl = eom_ccsd_22_tripletpp_trans_aibjcjal_ibcl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcjal_ibcl
    function eom_ccsd_22_tripletpp_trans_aibjcjal_ibjcl(t2, nocc, nactive, i, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcjal_ibjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(l, c, j, e)
term(1) = term(1) + t2(b,e,i,j) * tovov(l, c, j, e)
term(2) = term(2) + t2(b,e,i,j) * tovov(l, e, j, c)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcjal_ibjcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcjal_ibjcl = eom_ccsd_22_tripletpp_trans_aibjcjal_ibjcl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcjal_ibjcl
    function eom_ccsd_22_tripletpp_trans_aibjcjal_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcjal_aibcl 
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

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcjal_aibcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcjal_aibcl = eom_ccsd_22_tripletpp_trans_aibjcjal_aibcl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcjal_aibcl
    function eom_ccsd_22_tripletpp_trans_aibjbkdi_ajkd(t2, nocc, nactive, a, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbkdi_ajkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, k, d)
term(1) = term(1) + tvvoo(a, d, k, j)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,j,m) * tovov(m, d, k, e)
term(3) = term(3) + t2(a,e,m,j) * tovov(m, e, k, d)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, d, k, e)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, k, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbkdi_ajkd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjbkdi_ajkd = eom_ccsd_22_tripletpp_trans_aibjbkdi_ajkd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbkdi_ajkd
    function eom_ccsd_22_tripletpp_trans_aibjbkdi_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbkdi_aijkd 
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

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbkdi_aijkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjbkdi_aijkd = eom_ccsd_22_tripletpp_trans_aibjbkdi_aijkd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbkdi_aijkd
    function eom_ccsd_22_tripletpp_trans_aibjbkdi_abjkd(t2, nocc, nactive, a, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbkdi_abjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, b, k, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, k, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, d, k, b)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, d, k, b)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbkdi_abjkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjbkdi_abjkd = eom_ccsd_22_tripletpp_trans_aibjbkdi_abjkd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbkdi_abjkd
    function eom_ccsd_22_tripletpp_trans_aibjbidl_ajdl(t2, nocc, nactive, a, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbidl_ajdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, l, j)
term(1) = term(1) + tvoov(a, j, l, d)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,j) * tovov(m, d, l, e)
term(3) = term(3) + t2(a,e,j,m) * tovov(m, d, l, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, l, d)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbidl_ajdl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjbidl_ajdl = eom_ccsd_22_tripletpp_trans_aibjbidl_ajdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbidl_ajdl
    function eom_ccsd_22_tripletpp_trans_aibjbidl_aijdl(t2, nocc, nactive, a, i, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbidl_aijdl 
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

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbidl_aijdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjbidl_aijdl = eom_ccsd_22_tripletpp_trans_aibjbidl_aijdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbidl_aijdl
    function eom_ccsd_22_tripletpp_trans_aibjbidl_abjdl(t2, nocc, nactive, a, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbidl_abjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, l, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, l, d)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, d, l, b)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, d, l, b)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbidl_abjdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjbidl_abjdl = eom_ccsd_22_tripletpp_trans_aibjbidl_abjdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbidl_abjdl
    function eom_ccsd_22_tripletpp_trans_aibjbkdj_aikd(t2, nocc, nactive, a, i, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbkdj_aikd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, k, i)
term(1) = term(1) + tvoov(a, i, k, d)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, d, k, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, k, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, k, d)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, k, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbkdj_aikd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjbkdj_aikd = eom_ccsd_22_tripletpp_trans_aibjbkdj_aikd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbkdj_aikd
    function eom_ccsd_22_tripletpp_trans_aibjbkdj_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbkdj_aijkd 
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

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbkdj_aijkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjbkdj_aijkd = eom_ccsd_22_tripletpp_trans_aibjbkdj_aijkd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbkdj_aijkd
    function eom_ccsd_22_tripletpp_trans_aibjbkdj_aibkd(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbkdj_aibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, k, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, k, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, k, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, k, b)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbkdj_aibkd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjbkdj_aibkd = eom_ccsd_22_tripletpp_trans_aibjbkdj_aibkd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbkdj_aibkd
    function eom_ccsd_22_tripletpp_trans_aibjbjdl_aidl(t2, nocc, nactive, a, i, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbjdl_aidl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, l, i)
term(1) = term(1) + tvoov(a, i, l, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, d, l, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, l, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbjdl_aidl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjbjdl_aidl = eom_ccsd_22_tripletpp_trans_aibjbjdl_aidl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbjdl_aidl
    function eom_ccsd_22_tripletpp_trans_aibjbjdl_aijdl(t2, nocc, nactive, a, i, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbjdl_aijdl 
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

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbjdl_aijdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjbjdl_aijdl = eom_ccsd_22_tripletpp_trans_aibjbjdl_aijdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbjdl_aijdl
    function eom_ccsd_22_tripletpp_trans_aibjbjdl_aibdl(t2, nocc, nactive, a, i, b, d, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbjdl_aibdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, l, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, l, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, l, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, l, b)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbjdl_aibdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjbjdl_aibdl = eom_ccsd_22_tripletpp_trans_aibjbjdl_aibdl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbjdl_aibdl
    function eom_ccsd_22_tripletpp_trans_aibjckbi_ajck(t2, nocc, nactive, a, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckbi_ajck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, k, j)
term(1) = term(1) + tvoov(a, j, k, c)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,j) * tovov(m, c, k, e)
term(3) = term(3) + t2(a,e,j,m) * tovov(m, c, k, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckbi_ajck = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjckbi_ajck = eom_ccsd_22_tripletpp_trans_aibjckbi_ajck + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckbi_ajck
    function eom_ccsd_22_tripletpp_trans_aibjckbi_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckbi_aijck 
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

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckbi_aijck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjckbi_aijck = eom_ccsd_22_tripletpp_trans_aibjckbi_aijck + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckbi_aijck
    function eom_ccsd_22_tripletpp_trans_aibjckbi_abjck(t2, nocc, nactive, a, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckbi_abjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, k, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, k, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, k, c)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, b, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckbi_abjck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjckbi_abjck = eom_ccsd_22_tripletpp_trans_aibjckbi_abjck + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckbi_abjck
    function eom_ccsd_22_tripletpp_trans_aibjcibl_ajcl(t2, nocc, nactive, a, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcibl_ajcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, l, j)
term(1) = term(1) + tvoov(a, j, l, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,j,m) * tovov(m, c, l, e)
term(3) = term(3) + t2(a,e,m,j) * tovov(m, e, l, c)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, c, l, e)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, l, c)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcibl_ajcl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjcibl_ajcl = eom_ccsd_22_tripletpp_trans_aibjcibl_ajcl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcibl_ajcl
    function eom_ccsd_22_tripletpp_trans_aibjcibl_aijcl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcibl_aijcl 
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

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcibl_aijcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcibl_aijcl = eom_ccsd_22_tripletpp_trans_aibjcibl_aijcl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcibl_aijcl
    function eom_ccsd_22_tripletpp_trans_aibjcibl_abjcl(t2, nocc, nactive, a, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcibl_abjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, l, b)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, b, l, c)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, b, l, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcibl_abjcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcibl_abjcl = eom_ccsd_22_tripletpp_trans_aibjcibl_abjcl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcibl_abjcl
    function eom_ccsd_22_tripletpp_trans_aibjckbj_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckbj_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, k, i)
term(1) = term(1) + tvoov(a, i, k, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, c, k, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, c, k, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckbj_aick = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjckbj_aick = eom_ccsd_22_tripletpp_trans_aibjckbj_aick + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckbj_aick
    function eom_ccsd_22_tripletpp_trans_aibjckbj_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckbj_aijck 
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

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckbj_aijck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjckbj_aijck = eom_ccsd_22_tripletpp_trans_aibjckbj_aijck + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckbj_aijck
    function eom_ccsd_22_tripletpp_trans_aibjckbj_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjckbj_aibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, k, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, k, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, k, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjckbj_aibck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjckbj_aibck = eom_ccsd_22_tripletpp_trans_aibjckbj_aibck + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjckbj_aibck
    function eom_ccsd_22_tripletpp_trans_aibjcjbl_aicl(t2, nocc, nactive, a, i, c, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcjbl_aicl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, l, i)
term(1) = term(1) + tvoov(a, i, l, c)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, c, l, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, c, l, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, l, c)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, l, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcjbl_aicl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjcjbl_aicl = eom_ccsd_22_tripletpp_trans_aibjcjbl_aicl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcjbl_aicl
    function eom_ccsd_22_tripletpp_trans_aibjcjbl_aijcl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcjbl_aijcl 
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

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcjbl_aijcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcjbl_aijcl = eom_ccsd_22_tripletpp_trans_aibjcjbl_aijcl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcjbl_aijcl
    function eom_ccsd_22_tripletpp_trans_aibjcjbl_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcjbl_aibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, l, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, l, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, l, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcjbl_aibcl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcjbl_aibcl = eom_ccsd_22_tripletpp_trans_aibjcjbl_aibcl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcjbl_aibcl
    function eom_ccsd_22_tripletpp_trans_aibjcidj_abcd(t2, nocc, nactive, a, b, c, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcidj_abcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, d, a, c)
term(1) = term(1) + read_ftvvvv(b, c, a, d)

term(1) = term(1) * (-1.0d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,n) * tovov(n, d, m, c)
term(3) = term(3) + t2(a,b,m,n) * tovov(n, c, m, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcidj_abcd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcidj_abcd = eom_ccsd_22_tripletpp_trans_aibjcidj_abcd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcidj_abcd
    function eom_ccsd_22_tripletpp_trans_aibjcidj_aibcd(t2, nocc, nactive, a, i, b, c, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcidj_aibcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, i, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, i, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcidj_aibcd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcidj_aibcd = eom_ccsd_22_tripletpp_trans_aibjcidj_aibcd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcidj_aibcd
    function eom_ccsd_22_tripletpp_trans_aibjcidj_abjcd(t2, nocc, nactive, a, b, j, c, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcidj_abjcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d 
    integer :: s ,m,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, j, d)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, d, j, c)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, d, j, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcidj_abjcd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcidj_abjcd = eom_ccsd_22_tripletpp_trans_aibjcidj_abjcd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcidj_abjcd
    function eom_ccsd_22_tripletpp_trans_aibjakbi_jk(t2, nocc, nactive, j, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbi_jk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(k, j)


do m = 1, nocc 
term(1) = term(1) + toooo(m, j, k, m)
term(2) = term(2) + toooo(m, m, k, j)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (2.0d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,j,m) * tovov(m, e, k, f)
end do 
end do 
end do 

term(3) = term(3) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,j,m) * tovov(m, f, k, e)
end do 
end do 
end do 

term(4) = term(4) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbi_jk = 0.d+0
    do s = 0, 4
    eom_ccsd_22_tripletpp_trans_aibjakbi_jk = eom_ccsd_22_tripletpp_trans_aibjakbi_jk + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbi_jk
    function eom_ccsd_22_tripletpp_trans_aibjakbi_ajk(t2, nocc, nactive, a, j, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbi_ajk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, k, j)
term(1) = term(1) + tvoov(a, j, k, a)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,j) * tovov(m, a, k, e)
term(3) = term(3) + t2(a,e,j,m) * tovov(m, a, k, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, k, a)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, k, a)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbi_ajk = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjakbi_ajk = eom_ccsd_22_tripletpp_trans_aibjakbi_ajk + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbi_ajk
    function eom_ccsd_22_tripletpp_trans_aibjakbi_bjk(t2, nocc, nactive, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbi_bjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, b)
term(1) = term(1) + tvvoo(b, b, k, j)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,j,m) * tovov(m, b, k, e)
term(3) = term(3) + t2(b,e,m,j) * tovov(m, b, k, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, k, b)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, k, b)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbi_bjk = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjakbi_bjk = eom_ccsd_22_tripletpp_trans_aibjakbi_bjk + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbi_bjk
    function eom_ccsd_22_tripletpp_trans_aibjakbi_ijk(t2, nocc, nactive, i, j, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbi_ijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(k, j, i, i)
term(1) = term(1) + toooo(k, i, i, j)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(k, f, i, e)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(k, e, i, f)
end do 
end do 



    eom_ccsd_22_tripletpp_trans_aibjakbi_ijk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakbi_ijk = eom_ccsd_22_tripletpp_trans_aibjakbi_ijk + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbi_ijk
    function eom_ccsd_22_tripletpp_trans_aibjakbi_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbi_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, i, a)
term(1) = term(1) + t2(a,e,j,i) * tovov(k, a, i, e)
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, i, a)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, a, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbi_aijk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakbi_aijk = eom_ccsd_22_tripletpp_trans_aibjakbi_aijk + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbi_aijk
    function eom_ccsd_22_tripletpp_trans_aibjakbi_ibjk(t2, nocc, nactive, i, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbi_ibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, i, b)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, i, b)
term(2) = term(2) + t2(b,e,j,i) * tovov(k, b, i, e)
term(3) = term(3) + t2(b,e,i,j) * tovov(k, b, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbi_ibjk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakbi_ibjk = eom_ccsd_22_tripletpp_trans_aibjakbi_ibjk + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbi_ibjk
    function eom_ccsd_22_tripletpp_trans_aibjakbi_abjk(t2, nocc, nactive, a, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbi_abjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, a, k, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, k, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, k, a)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, b, k, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbi_abjk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakbi_abjk = eom_ccsd_22_tripletpp_trans_aibjakbi_abjk + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbi_abjk
    function eom_ccsd_22_tripletpp_trans_aibjaibl_jl(t2, nocc, nactive, j, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibl_jl 
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

term(2) = term(2) * (-2.0d+0) 

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

term(4) = term(4) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibl_jl = 0.d+0
    do s = 0, 4
    eom_ccsd_22_tripletpp_trans_aibjaibl_jl = eom_ccsd_22_tripletpp_trans_aibjaibl_jl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibl_jl
    function eom_ccsd_22_tripletpp_trans_aibjaibl_ajl(t2, nocc, nactive, a, j, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibl_ajl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, l, j)
term(1) = term(1) + tvoov(a, j, l, a)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,j,m) * tovov(m, a, l, e)
term(3) = term(3) + t2(a,e,m,j) * tovov(m, e, l, a)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, a, l, e)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, l, a)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibl_ajl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjaibl_ajl = eom_ccsd_22_tripletpp_trans_aibjaibl_ajl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibl_ajl
    function eom_ccsd_22_tripletpp_trans_aibjaibl_bjl(t2, nocc, nactive, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibl_bjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, b, l, j)
term(1) = term(1) + tvoov(b, j, l, b)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,j) * tovov(m, b, l, e)
term(3) = term(3) + t2(b,e,j,m) * tovov(m, b, l, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, l, b)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, l, b)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibl_bjl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjaibl_bjl = eom_ccsd_22_tripletpp_trans_aibjaibl_bjl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibl_bjl
    function eom_ccsd_22_tripletpp_trans_aibjaibl_ijl(t2, nocc, nactive, i, j, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibl_ijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, i, i, j)
term(1) = term(1) + toooo(l, j, i, i)

term(0) = term(0) * (-1.0d+0) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(l, e, i, f)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(l, f, i, e)
end do 
end do 



    eom_ccsd_22_tripletpp_trans_aibjaibl_ijl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaibl_ijl = eom_ccsd_22_tripletpp_trans_aibjaibl_ijl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibl_ijl
    function eom_ccsd_22_tripletpp_trans_aibjaibl_aijl(t2, nocc, nactive, a, i, j, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibl_aijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, a, i, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, i, a)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, a, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibl_aijl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaibl_aijl = eom_ccsd_22_tripletpp_trans_aibjaibl_aijl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibl_aijl
    function eom_ccsd_22_tripletpp_trans_aibjaibl_ibjl(t2, nocc, nactive, i, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibl_ibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, b, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, b, i, e)
term(2) = term(2) + t2(b,e,j,i) * tovov(l, e, i, b)
term(3) = term(3) + t2(b,e,i,j) * tovov(l, e, i, b)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibl_ibjl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaibl_ibjl = eom_ccsd_22_tripletpp_trans_aibjaibl_ibjl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibl_ibjl
    function eom_ccsd_22_tripletpp_trans_aibjaibl_abjl(t2, nocc, nactive, a, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibl_abjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, l, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, l, b)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, b, l, a)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, b, l, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibl_abjl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaibl_abjl = eom_ccsd_22_tripletpp_trans_aibjaibl_abjl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibl_abjl
    function eom_ccsd_22_tripletpp_trans_aibjakbj_ik(t2, nocc, nactive, i, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbj_ik 
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

term(2) = term(2) * (-2.0d+0) 

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

term(4) = term(4) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbj_ik = 0.d+0
    do s = 0, 4
    eom_ccsd_22_tripletpp_trans_aibjakbj_ik = eom_ccsd_22_tripletpp_trans_aibjakbj_ik + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbj_ik
    function eom_ccsd_22_tripletpp_trans_aibjakbj_aik(t2, nocc, nactive, a, i, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbj_aik 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, k, i)
term(1) = term(1) + tvoov(a, i, k, a)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, a, k, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, a, k, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbj_aik = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjakbj_aik = eom_ccsd_22_tripletpp_trans_aibjakbj_aik + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbj_aik
    function eom_ccsd_22_tripletpp_trans_aibjakbj_ibk(t2, nocc, nactive, i, b, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbj_ibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, b)
term(1) = term(1) + tvvoo(b, b, k, i)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,i,m) * tovov(m, b, k, e)
term(3) = term(3) + t2(b,e,m,i) * tovov(m, b, k, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, k, b)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, k, b)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbj_ibk = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjakbj_ibk = eom_ccsd_22_tripletpp_trans_aibjakbj_ibk + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbj_ibk
    function eom_ccsd_22_tripletpp_trans_aibjakbj_ijk(t2, nocc, nactive, i, j, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbj_ijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(k, j, j, i)
term(1) = term(1) + toooo(k, i, j, j)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(k, f, j, e)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(k, e, j, f)
end do 
end do 



    eom_ccsd_22_tripletpp_trans_aibjakbj_ijk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakbj_ijk = eom_ccsd_22_tripletpp_trans_aibjakbj_ijk + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbj_ijk
    function eom_ccsd_22_tripletpp_trans_aibjakbj_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbj_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, j, a)
term(1) = term(1) + t2(a,e,j,i) * tovov(k, a, j, e)
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, j, a)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, a, j, e)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbj_aijk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakbj_aijk = eom_ccsd_22_tripletpp_trans_aibjakbj_aijk + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbj_aijk
    function eom_ccsd_22_tripletpp_trans_aibjakbj_ibjk(t2, nocc, nactive, i, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbj_ibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, j, b)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, j, b)
term(2) = term(2) + t2(b,e,j,i) * tovov(k, b, j, e)
term(3) = term(3) + t2(b,e,i,j) * tovov(k, b, j, e)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbj_ibjk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakbj_ibjk = eom_ccsd_22_tripletpp_trans_aibjakbj_ibjk + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbj_ibjk
    function eom_ccsd_22_tripletpp_trans_aibjakbj_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjakbj_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, k, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, k, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, k, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, k, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjakbj_aibk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjakbj_aibk = eom_ccsd_22_tripletpp_trans_aibjakbj_aibk + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjakbj_aibk
    function eom_ccsd_22_tripletpp_trans_aibjajbl_il(t2, nocc, nactive, i, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjajbl_il 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + too(l, i)


do m = 1, nocc 
term(1) = term(1) + toooo(m, i, l, m)
term(2) = term(2) + toooo(m, m, l, i)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (2.0d+0) 

do f = nocc + 1, nactive 
do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,m) * tovov(m, e, l, f)
end do 
end do 
end do 

term(3) = term(3) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
do f = nocc + 1, nactive 
term(4) = term(4) + t2(e,f,i,m) * tovov(m, f, l, e)
end do 
end do 
end do 

term(4) = term(4) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjajbl_il = 0.d+0
    do s = 0, 4
    eom_ccsd_22_tripletpp_trans_aibjajbl_il = eom_ccsd_22_tripletpp_trans_aibjajbl_il + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjajbl_il
    function eom_ccsd_22_tripletpp_trans_aibjajbl_ail(t2, nocc, nactive, a, i, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjajbl_ail 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, l, i)
term(1) = term(1) + tvoov(a, i, l, a)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, a, l, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, a, l, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjajbl_ail = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjajbl_ail = eom_ccsd_22_tripletpp_trans_aibjajbl_ail + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjajbl_ail
    function eom_ccsd_22_tripletpp_trans_aibjajbl_ibl(t2, nocc, nactive, i, b, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjajbl_ibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, b, l, i)
term(1) = term(1) + tvoov(b, i, l, b)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,i) * tovov(m, b, l, e)
term(3) = term(3) + t2(b,e,i,m) * tovov(m, b, l, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, l, b)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, l, b)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjajbl_ibl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjajbl_ibl = eom_ccsd_22_tripletpp_trans_aibjajbl_ibl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjajbl_ibl
    function eom_ccsd_22_tripletpp_trans_aibjajbl_ijl(t2, nocc, nactive, i, j, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjajbl_ijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(l, i, j, j)
term(1) = term(1) + toooo(l, j, j, i)

term(0) = term(0) * (-1.0d+0) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(l, e, j, f)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(l, f, j, e)
end do 
end do 



    eom_ccsd_22_tripletpp_trans_aibjajbl_ijl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjajbl_ijl = eom_ccsd_22_tripletpp_trans_aibjajbl_ijl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjajbl_ijl
    function eom_ccsd_22_tripletpp_trans_aibjajbl_aijl(t2, nocc, nactive, a, i, j, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjajbl_aijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, a, j, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, j, a)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, a, j, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, j, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjajbl_aijl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjajbl_aijl = eom_ccsd_22_tripletpp_trans_aibjajbl_aijl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjajbl_aijl
    function eom_ccsd_22_tripletpp_trans_aibjajbl_ibjl(t2, nocc, nactive, i, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjajbl_ibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, b, j, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, b, j, e)
term(2) = term(2) + t2(b,e,j,i) * tovov(l, e, j, b)
term(3) = term(3) + t2(b,e,i,j) * tovov(l, e, j, b)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjajbl_ibjl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjajbl_ibjl = eom_ccsd_22_tripletpp_trans_aibjajbl_ibjl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjajbl_ibjl
    function eom_ccsd_22_tripletpp_trans_aibjajbl_aibl(t2, nocc, nactive, a, i, b, l) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjajbl_aibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,m,e,f 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, l, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, l, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, l, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, l, a)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjajbl_aibl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjajbl_aibl = eom_ccsd_22_tripletpp_trans_aibjajbl_aibl + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjajbl_aibl
    function eom_ccsd_22_tripletpp_trans_aibjaidj_bd(t2, nocc, nactive, b, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaidj_bd 
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

term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 

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

term(4) = term(4) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaidj_bd = 0.d+0
    do s = 0, 4
    eom_ccsd_22_tripletpp_trans_aibjaidj_bd = eom_ccsd_22_tripletpp_trans_aibjaidj_bd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaidj_bd
    function eom_ccsd_22_tripletpp_trans_aibjaidj_abd(t2, nocc, nactive, a, b, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaidj_abd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, d, a, a)
term(1) = term(1) + read_ftvvvv(b, a, a, d)

term(1) = term(1) * (-1.0d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,n) * tovov(n, d, m, a)
term(3) = term(3) + t2(a,b,m,n) * tovov(n, a, m, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaidj_abd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaidj_abd = eom_ccsd_22_tripletpp_trans_aibjaidj_abd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaidj_abd
    function eom_ccsd_22_tripletpp_trans_aibjaidj_ibd(t2, nocc, nactive, i, b, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaidj_ibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, i, d)
term(1) = term(1) + tvvoo(b, d, i, i)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,i,m) * tovov(m, d, i, e)
term(3) = term(3) + t2(b,e,m,i) * tovov(m, d, i, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaidj_ibd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjaidj_ibd = eom_ccsd_22_tripletpp_trans_aibjaidj_ibd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaidj_ibd
    function eom_ccsd_22_tripletpp_trans_aibjaidj_bjd(t2, nocc, nactive, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaidj_bjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, d, j, j)
term(1) = term(1) + tvoov(b, j, j, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,j) * tovov(m, d, j, e)
term(3) = term(3) + t2(b,e,j,m) * tovov(m, d, j, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, j, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaidj_bjd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjaidj_bjd = eom_ccsd_22_tripletpp_trans_aibjaidj_bjd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaidj_bjd
    function eom_ccsd_22_tripletpp_trans_aibjaidj_ibjd(t2, nocc, nactive, i, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaidj_ibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(j, d, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, d, i, e)
term(2) = term(2) + t2(b,e,j,i) * tovov(j, e, i, d)
term(3) = term(3) + t2(b,e,i,j) * tovov(j, e, i, d)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaidj_ibjd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaidj_ibjd = eom_ccsd_22_tripletpp_trans_aibjaidj_ibjd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaidj_ibjd
    function eom_ccsd_22_tripletpp_trans_aibjaidj_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaidj_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, i, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, i, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, i, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaidj_aibd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaidj_aibd = eom_ccsd_22_tripletpp_trans_aibjaidj_aibd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaidj_aibd
    function eom_ccsd_22_tripletpp_trans_aibjaidj_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaidj_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, j, d)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, d, j, a)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, d, j, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaidj_abjd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaidj_abjd = eom_ccsd_22_tripletpp_trans_aibjaidj_abjd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaidj_abjd
    function eom_ccsd_22_tripletpp_trans_aibjciaj_bc(t2, nocc, nactive, b, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjciaj_bc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(b, c)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + tvvoo(b, c, m, m)
term(2) = term(2) + tvoov(b, m, m, c)
end do 

term(1) = term(1) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,m,n) * tovov(n, c, m, e)
end do 
end do 
end do 

term(3) = term(3) * (-1.0d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,m,n) * tovov(n, e, m, c)
end do 
end do 
end do 

term(4) = term(4) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjciaj_bc = 0.d+0
    do s = 0, 4
    eom_ccsd_22_tripletpp_trans_aibjciaj_bc = eom_ccsd_22_tripletpp_trans_aibjciaj_bc + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjciaj_bc
    function eom_ccsd_22_tripletpp_trans_aibjciaj_ibc(t2, nocc, nactive, i, b, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjciaj_ibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, i, i)
term(1) = term(1) + tvoov(b, i, i, c)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,i) * tovov(m, c, i, e)
term(3) = term(3) + t2(b,e,i,m) * tovov(m, c, i, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjciaj_ibc = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjciaj_ibc = eom_ccsd_22_tripletpp_trans_aibjciaj_ibc + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjciaj_ibc
    function eom_ccsd_22_tripletpp_trans_aibjciaj_bjc(t2, nocc, nactive, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjciaj_bjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, c, j, j)
term(1) = term(1) + tvoov(b, j, j, c)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,j,m) * tovov(m, c, j, e)
term(3) = term(3) + t2(b,e,m,j) * tovov(m, c, j, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, j, c)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjciaj_bjc = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjciaj_bjc = eom_ccsd_22_tripletpp_trans_aibjciaj_bjc + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjciaj_bjc
    function eom_ccsd_22_tripletpp_trans_aibjciaj_abc(t2, nocc, nactive, a, b, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjciaj_abc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, a, a, c)
term(1) = term(1) + read_ftvvvv(b, c, a, a)

term(1) = term(1) * (-1.0d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,n) * tovov(n, a, m, c)
term(3) = term(3) + t2(a,b,m,n) * tovov(n, c, m, a)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjciaj_abc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjciaj_abc = eom_ccsd_22_tripletpp_trans_aibjciaj_abc + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjciaj_abc
    function eom_ccsd_22_tripletpp_trans_aibjciaj_ibjc(t2, nocc, nactive, i, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjciaj_ibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tovov(j, c, i, e)
term(1) = term(1) + t2(b,e,i,j) * tovov(j, c, i, e)
term(2) = term(2) + t2(b,e,i,j) * tovov(j, e, i, c)
term(3) = term(3) + t2(b,e,j,i) * tovov(j, e, i, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjciaj_ibjc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjciaj_ibjc = eom_ccsd_22_tripletpp_trans_aibjciaj_ibjc + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjciaj_ibjc
    function eom_ccsd_22_tripletpp_trans_aibjciaj_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjciaj_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, i, a)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, i, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, i, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjciaj_aibc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjciaj_aibc = eom_ccsd_22_tripletpp_trans_aibjciaj_aibc + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjciaj_aibc
    function eom_ccsd_22_tripletpp_trans_aibjciaj_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjciaj_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, j, a)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, a, j, c)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, a, j, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjciaj_abjc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjciaj_abjc = eom_ccsd_22_tripletpp_trans_aibjciaj_abjc + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjciaj_abjc
    function eom_ccsd_22_tripletpp_trans_aibjbidj_ad(t2, nocc, nactive, a, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbidj_ad 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvv(a, d)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + tvvoo(a, d, m, m)
term(2) = term(2) + tvoov(a, m, m, d)
end do 

term(1) = term(1) * (-2.0d+0) 

do e = nocc + 1, nactive 
do n = 1, nocc 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,m,n) * tovov(n, d, m, e)
end do 
end do 
end do 

term(3) = term(3) * (-1.0d+0) 

do n = 1, nocc 
do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,m,n) * tovov(n, e, m, d)
end do 
end do 
end do 

term(4) = term(4) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbidj_ad = 0.d+0
    do s = 0, 4
    eom_ccsd_22_tripletpp_trans_aibjbidj_ad = eom_ccsd_22_tripletpp_trans_aibjbidj_ad + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbidj_ad
    function eom_ccsd_22_tripletpp_trans_aibjbidj_abd(t2, nocc, nactive, a, b, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbidj_abd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, d, a, b)
term(1) = term(1) + read_ftvvvv(b, b, a, d)

term(1) = term(1) * (-1.0d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,n) * tovov(n, d, m, b)
term(3) = term(3) + t2(a,b,m,n) * tovov(n, b, m, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbidj_abd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjbidj_abd = eom_ccsd_22_tripletpp_trans_aibjbidj_abd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbidj_abd
    function eom_ccsd_22_tripletpp_trans_aibjbidj_aid(t2, nocc, nactive, a, i, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbidj_aid 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, i, i)
term(1) = term(1) + tvoov(a, i, i, d)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, d, i, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, i, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbidj_aid = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjbidj_aid = eom_ccsd_22_tripletpp_trans_aibjbidj_aid + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbidj_aid
    function eom_ccsd_22_tripletpp_trans_aibjbidj_ajd(t2, nocc, nactive, a, j, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbidj_ajd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, d, j, j)
term(1) = term(1) + tvoov(a, j, j, d)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,j) * tovov(m, d, j, e)
term(3) = term(3) + t2(a,e,j,m) * tovov(m, d, j, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, j, d)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbidj_ajd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjbidj_ajd = eom_ccsd_22_tripletpp_trans_aibjbidj_ajd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbidj_ajd
    function eom_ccsd_22_tripletpp_trans_aibjbidj_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbidj_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, d, i, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(j, e, i, d)
term(2) = term(2) + t2(a,e,i,j) * tovov(j, d, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, e, i, d)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbidj_aijd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjbidj_aijd = eom_ccsd_22_tripletpp_trans_aibjbidj_aijd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbidj_aijd
    function eom_ccsd_22_tripletpp_trans_aibjbidj_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbidj_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, i, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, i, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, i, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, i, b)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbidj_aibd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjbidj_aibd = eom_ccsd_22_tripletpp_trans_aibjbidj_aibd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbidj_aibd
    function eom_ccsd_22_tripletpp_trans_aibjbidj_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjbidj_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, j, d)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, d, j, b)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, d, j, b)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjbidj_abjd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjbidj_abjd = eom_ccsd_22_tripletpp_trans_aibjbidj_abjd + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjbidj_abjd
    function eom_ccsd_22_tripletpp_trans_aibjcibj_ac(t2, nocc, nactive, a, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcibj_ac 
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

term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 

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

term(4) = term(4) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcibj_ac = 0.d+0
    do s = 0, 4
    eom_ccsd_22_tripletpp_trans_aibjcibj_ac = eom_ccsd_22_tripletpp_trans_aibjcibj_ac + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcibj_ac
    function eom_ccsd_22_tripletpp_trans_aibjcibj_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcibj_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, i, i)
term(1) = term(1) + tvoov(a, i, i, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, c, i, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, c, i, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcibj_aic = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjcibj_aic = eom_ccsd_22_tripletpp_trans_aibjcibj_aic + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcibj_aic
    function eom_ccsd_22_tripletpp_trans_aibjcibj_ajc(t2, nocc, nactive, a, j, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcibj_ajc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, c, j, j)
term(1) = term(1) + tvoov(a, j, j, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,j,m) * tovov(m, c, j, e)
term(3) = term(3) + t2(a,e,m,j) * tovov(m, e, j, c)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, c, j, e)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcibj_ajc = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjcibj_ajc = eom_ccsd_22_tripletpp_trans_aibjcibj_ajc + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcibj_ajc
    function eom_ccsd_22_tripletpp_trans_aibjcibj_abc(t2, nocc, nactive, a, b, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcibj_abc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, b, a, c)
term(1) = term(1) + read_ftvvvv(b, c, a, b)

term(1) = term(1) * (-1.0d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,n) * tovov(n, b, m, c)
term(3) = term(3) + t2(a,b,m,n) * tovov(n, c, m, b)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcibj_abc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcibj_abc = eom_ccsd_22_tripletpp_trans_aibjcibj_abc + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcibj_abc
    function eom_ccsd_22_tripletpp_trans_aibjcibj_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcibj_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, c, i, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(j, e, i, c)
term(2) = term(2) + t2(a,e,i,j) * tovov(j, c, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, e, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcibj_aijc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcibj_aijc = eom_ccsd_22_tripletpp_trans_aibjcibj_aijc + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcibj_aijc
    function eom_ccsd_22_tripletpp_trans_aibjcibj_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcibj_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, i, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, i, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, i, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcibj_aibc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcibj_aibc = eom_ccsd_22_tripletpp_trans_aibjcibj_aibc + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcibj_aibc
    function eom_ccsd_22_tripletpp_trans_aibjcibj_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjcibj_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,m,e,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, j, b)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, b, j, c)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, b, j, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjcibj_abjc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjcibj_abjc = eom_ccsd_22_tripletpp_trans_aibjcibj_abjc + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjcibj_abjc
    function eom_ccsd_22_tripletpp_trans_aibjaibj_a(t2, nocc, nactive, a) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_a 
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

term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 

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

term(4) = term(4) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibj_a = 0.d+0
    do s = 0, 4
    eom_ccsd_22_tripletpp_trans_aibjaibj_a = eom_ccsd_22_tripletpp_trans_aibjaibj_a + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_a
    function eom_ccsd_22_tripletpp_trans_aibjaibj_b(t2, nocc, nactive, b) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_b 
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

term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 

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

term(4) = term(4) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibj_b = 0.d+0
    do s = 0, 4
    eom_ccsd_22_tripletpp_trans_aibjaibj_b = eom_ccsd_22_tripletpp_trans_aibjaibj_b + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_b
    function eom_ccsd_22_tripletpp_trans_aibjaibj_i(t2, nocc, nactive, i) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_i 
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

term(2) = term(2) * (-2.0d+0) 

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

term(4) = term(4) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibj_i = 0.d+0
    do s = 0, 4
    eom_ccsd_22_tripletpp_trans_aibjaibj_i = eom_ccsd_22_tripletpp_trans_aibjaibj_i + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_i
    function eom_ccsd_22_tripletpp_trans_aibjaibj_j(t2, nocc, nactive, j) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_j 
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

term(2) = term(2) * (-2.0d+0) 

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

term(4) = term(4) * (-2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibj_j = 0.d+0
    do s = 0, 4
    eom_ccsd_22_tripletpp_trans_aibjaibj_j = eom_ccsd_22_tripletpp_trans_aibjaibj_j + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_j
    function eom_ccsd_22_tripletpp_trans_aibjaibj_ai(t2, nocc, nactive, a, i) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_ai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, i, i)
term(1) = term(1) + tvoov(a, i, i, a)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,m,i) * tovov(m, a, i, e)
term(3) = term(3) + t2(a,e,i,m) * tovov(m, a, i, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibj_ai = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjaibj_ai = eom_ccsd_22_tripletpp_trans_aibjaibj_ai + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_ai
    function eom_ccsd_22_tripletpp_trans_aibjaibj_aj(t2, nocc, nactive, a, j) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_aj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(a, a, j, j)
term(1) = term(1) + tvoov(a, j, j, a)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(a,e,j,m) * tovov(m, a, j, e)
term(3) = term(3) + t2(a,e,m,j) * tovov(m, e, j, a)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, a, j, e)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, j, a)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibj_aj = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjaibj_aj = eom_ccsd_22_tripletpp_trans_aibjaibj_aj + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_aj
    function eom_ccsd_22_tripletpp_trans_aibjaibj_ib(t2, nocc, nactive, i, b) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_ib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, i, b)
term(1) = term(1) + tvvoo(b, b, i, i)

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,i,m) * tovov(m, b, i, e)
term(3) = term(3) + t2(b,e,m,i) * tovov(m, b, i, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, i, b)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, i, b)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibj_ib = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjaibj_ib = eom_ccsd_22_tripletpp_trans_aibjaibj_ib + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_ib
    function eom_ccsd_22_tripletpp_trans_aibjaibj_bj(t2, nocc, nactive, b, j) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_bj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvvoo(b, b, j, j)
term(1) = term(1) + tvoov(b, j, j, b)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(2) = term(2) + t2(b,e,m,j) * tovov(m, b, j, e)
term(3) = term(3) + t2(b,e,j,m) * tovov(m, b, j, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, j, b)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, j, b)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibj_bj = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpp_trans_aibjaibj_bj = eom_ccsd_22_tripletpp_trans_aibjaibj_bj + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_bj
    function eom_ccsd_22_tripletpp_trans_aibjaibj_ab(t2, nocc, nactive, a, b) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_ab 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + read_ftvvvv(b, b, a, a)
term(1) = term(1) + read_ftvvvv(b, a, a, b)

term(1) = term(1) * (-1.0d+0) 

do n = 1, nocc 
do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,n) * tovov(n, b, m, a)
term(3) = term(3) + t2(a,b,m,n) * tovov(n, a, m, b)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibj_ab = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaibj_ab = eom_ccsd_22_tripletpp_trans_aibjaibj_ab + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_ab
    function eom_ccsd_22_tripletpp_trans_aibjaibj_ij(t2, nocc, nactive, i, j) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_ij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + toooo(j, i, i, j)
term(1) = term(1) + toooo(j, j, i, i)

term(0) = term(0) * (-1.0d+0) 

do f = nocc + 1, nactive 
do e = nocc + 1, nactive 
term(2) = term(2) + t2(e,f,i,j) * tovov(j, e, i, f)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do f = nocc + 1, nactive 
term(3) = term(3) + t2(e,f,i,j) * tovov(j, f, i, e)
end do 
end do 



    eom_ccsd_22_tripletpp_trans_aibjaibj_ij = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaibj_ij = eom_ccsd_22_tripletpp_trans_aibjaibj_ij + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_ij
    function eom_ccsd_22_tripletpp_trans_aibjaibj_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, a, i, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(j, e, i, a)
term(2) = term(2) + t2(a,e,i,j) * tovov(j, a, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibj_aij = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaibj_aij = eom_ccsd_22_tripletpp_trans_aibjaibj_aij + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_aij
    function eom_ccsd_22_tripletpp_trans_aibjaibj_ibj(t2, nocc, nactive, i, b, j) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_ibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(j, b, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, b, i, e)
term(2) = term(2) + t2(b,e,j,i) * tovov(j, e, i, b)
term(3) = term(3) + t2(b,e,i,j) * tovov(j, e, i, b)
end do 

term(1) = term(1) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibj_ibj = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaibj_ibj = eom_ccsd_22_tripletpp_trans_aibjaibj_ibj + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_ibj
    function eom_ccsd_22_tripletpp_trans_aibjaibj_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, i, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, i, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, i, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibj_aib = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaibj_aib = eom_ccsd_22_tripletpp_trans_aibjaibj_aib + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_aib
    function eom_ccsd_22_tripletpp_trans_aibjaibj_abj(t2, nocc, nactive, a, b, j) 
    real(F64) :: eom_ccsd_22_tripletpp_trans_aibjaibj_abj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j 
    integer :: s ,m,e,f,n 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, j, b)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, b, j, a)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, b, j, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpp_trans_aibjaibj_abj = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpp_trans_aibjaibj_abj = eom_ccsd_22_tripletpp_trans_aibjaibj_abj + term(s)
    end do

    end function eom_ccsd_22_tripletpp_trans_aibjaibj_abj
    end module eom_ccsd_22_tripletpp_trans
    