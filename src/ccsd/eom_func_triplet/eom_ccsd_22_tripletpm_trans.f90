module eom_ccsd_22_tripletpm_trans

    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none
    !               
    ! File generated automatically on 2018-12-06 14:26:28  
    !  
    contains
    
    function eom_ccsd_22_tripletpm_trans_aibjckbl_aijckl(t2, nocc, nactive, a, i, j, c, k, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckbl_aijckl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, e, k, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckbl_aijckl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckbl_aijckl = eom_ccsd_22_tripletpm_trans_aibjckbl_aijckl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckbl_aijckl
    function eom_ccsd_22_tripletpm_trans_aibjbkdl_aijkdl(t2, nocc, nactive, a, i, j, k, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbkdl_aijkdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, d, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, d, k, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbkdl_aijkdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbkdl_aijkdl = eom_ccsd_22_tripletpm_trans_aibjbkdl_aijkdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbkdl_aijkdl
    function eom_ccsd_22_tripletpm_trans_aibjckal_ibjckl(t2, nocc, nactive, i, b, j, c, k, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckal_ibjckl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, e, k, c)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckal_ibjckl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckal_ibjckl = eom_ccsd_22_tripletpm_trans_aibjckal_ibjckl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckal_ibjckl
    function eom_ccsd_22_tripletpm_trans_aibjakdl_ibjkdl(t2, nocc, nactive, i, b, j, k, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakdl_ibjkdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, d, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, d, k, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakdl_ibjkdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakdl_ibjkdl = eom_ccsd_22_tripletpm_trans_aibjakdl_ibjkdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakdl_ibjkdl
    function eom_ccsd_22_tripletpm_trans_aibjckdi_abjckd(t2, nocc, nactive, a, b, j, c, k, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckdi_abjckd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, d, k, c)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, d, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckdi_abjckd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckdi_abjckd = eom_ccsd_22_tripletpm_trans_aibjckdi_abjckd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckdi_abjckd
    function eom_ccsd_22_tripletpm_trans_aibjcidl_abjcdl(t2, nocc, nactive, a, b, j, c, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcidl_abjcdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d, l 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, l, d)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcidl_abjcdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcidl_abjcdl = eom_ccsd_22_tripletpm_trans_aibjcidl_abjcdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcidl_abjcdl
    function eom_ccsd_22_tripletpm_trans_aibjckdj_aibckd(t2, nocc, nactive, a, i, b, c, k, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckdj_aibckd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, d, k, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckdj_aibckd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckdj_aibckd = eom_ccsd_22_tripletpm_trans_aibjckdj_aibckd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckdj_aibckd
    function eom_ccsd_22_tripletpm_trans_aibjcjdl_aibcdl(t2, nocc, nactive, a, i, b, c, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjdl_aibcdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d, l 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjdl_aibcdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjdl_aibcdl = eom_ccsd_22_tripletpm_trans_aibjcjdl_aibcdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjdl_aibcdl
    function eom_ccsd_22_tripletpm_trans_aibjbkbl_aibjkl(t2, nocc, nactive, a, i, b, j, k, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbkbl_aibjkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, b, k, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, k, b)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, b, k, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, k, b)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbkbl_aibjkl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjbkbl_aibjkl = eom_ccsd_22_tripletpm_trans_aibjbkbl_aibjkl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbkbl_aibjkl
    function eom_ccsd_22_tripletpm_trans_aibjakbl_ibjkl(t2, nocc, nactive, i, b, j, k, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakbl_ibjkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, b, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, b, k, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakbl_ibjkl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakbl_ibjkl = eom_ccsd_22_tripletpm_trans_aibjakbl_ibjkl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakbl_ibjkl
    function eom_ccsd_22_tripletpm_trans_aibjakbl_aijkl(t2, nocc, nactive, a, i, j, k, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakbl_aijkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, l 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, e, k, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, k, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakbl_aijkl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakbl_aijkl = eom_ccsd_22_tripletpm_trans_aibjakbl_aijkl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakbl_aijkl
    function eom_ccsd_22_tripletpm_trans_aibjckbi_ajck(t2, nocc, nactive, a, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckbi_ajck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, k, c)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,j,m) * tovov(m, c, k, e)
term(2) = term(2) + t2(a,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckbi_ajck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjckbi_ajck = eom_ccsd_22_tripletpm_trans_aibjckbi_ajck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckbi_ajck
    function eom_ccsd_22_tripletpm_trans_aibjckbi_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckbi_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, c, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, c, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckbi_aijck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckbi_aijck = eom_ccsd_22_tripletpm_trans_aibjckbi_aijck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckbi_aijck
    function eom_ccsd_22_tripletpm_trans_aibjckbi_abjck(t2, nocc, nactive, a, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckbi_abjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, k, c)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckbi_abjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckbi_abjck = eom_ccsd_22_tripletpm_trans_aibjckbi_abjck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckbi_abjck
    function eom_ccsd_22_tripletpm_trans_aibjcibl_abjcl(t2, nocc, nactive, a, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcibl_abjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, l 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, l, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcibl_abjcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcibl_abjcl = eom_ccsd_22_tripletpm_trans_aibjcibl_abjcl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcibl_abjcl
    function eom_ccsd_22_tripletpm_trans_aibjcibl_aijcl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcibl_aijcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, e, i, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcibl_aijcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcibl_aijcl = eom_ccsd_22_tripletpm_trans_aibjcibl_aijcl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcibl_aijcl
    function eom_ccsd_22_tripletpm_trans_aibjckbj_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckbj_aick 
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


    eom_ccsd_22_tripletpm_trans_aibjckbj_aick = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjckbj_aick = eom_ccsd_22_tripletpm_trans_aibjckbj_aick + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckbj_aick
    function eom_ccsd_22_tripletpm_trans_aibjckbj_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckbj_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, c, j, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, c, j, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckbj_aijck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckbj_aijck = eom_ccsd_22_tripletpm_trans_aibjckbj_aijck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckbj_aijck
    function eom_ccsd_22_tripletpm_trans_aibjckbj_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckbj_aibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, k, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckbj_aibck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckbj_aibck = eom_ccsd_22_tripletpm_trans_aibjckbj_aibck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckbj_aibck
    function eom_ccsd_22_tripletpm_trans_aibjckbk_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckbk_aijck 
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

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckbk_aijck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckbk_aijck = eom_ccsd_22_tripletpm_trans_aibjckbk_aijck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckbk_aijck
    function eom_ccsd_22_tripletpm_trans_aibjcjbl_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjbl_aibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjbl_aibcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjbl_aibcl = eom_ccsd_22_tripletpm_trans_aibjcjbl_aibcl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjbl_aibcl
    function eom_ccsd_22_tripletpm_trans_aibjcjbl_aijcl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjbl_aijcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, e, j, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, j, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjbl_aijcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjbl_aijcl = eom_ccsd_22_tripletpm_trans_aibjcjbl_aijcl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjbl_aijcl
    function eom_ccsd_22_tripletpm_trans_aibjbkdi_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbkdi_aijkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, i, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, i, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbkdi_aijkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbkdi_aijkd = eom_ccsd_22_tripletpm_trans_aibjbkdi_aijkd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbkdi_aijkd
    function eom_ccsd_22_tripletpm_trans_aibjbkdi_abjkd(t2, nocc, nactive, a, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbkdi_abjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, d, k, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, d, k, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbkdi_abjkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbkdi_abjkd = eom_ccsd_22_tripletpm_trans_aibjbkdi_abjkd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbkdi_abjkd
    function eom_ccsd_22_tripletpm_trans_aibjbidl_ajdl(t2, nocc, nactive, a, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbidl_ajdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, l, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,j,m) * tovov(m, d, l, e)
term(2) = term(2) + t2(a,e,m,j) * tovov(m, e, l, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbidl_ajdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjbidl_ajdl = eom_ccsd_22_tripletpm_trans_aibjbidl_ajdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbidl_ajdl
    function eom_ccsd_22_tripletpm_trans_aibjbidl_aijdl(t2, nocc, nactive, a, i, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbidl_aijdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, d, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, d, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbidl_aijdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbidl_aijdl = eom_ccsd_22_tripletpm_trans_aibjbidl_aijdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbidl_aijdl
    function eom_ccsd_22_tripletpm_trans_aibjbidl_abjdl(t2, nocc, nactive, a, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbidl_abjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, l, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, l, d)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbidl_abjdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbidl_abjdl = eom_ccsd_22_tripletpm_trans_aibjbidl_abjdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbidl_abjdl
    function eom_ccsd_22_tripletpm_trans_aibjbkdj_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbkdj_aijkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, j, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, j, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbkdj_aijkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbkdj_aijkd = eom_ccsd_22_tripletpm_trans_aibjbkdj_aijkd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbkdj_aijkd
    function eom_ccsd_22_tripletpm_trans_aibjbkdj_aibkd(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbkdj_aibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, d, k, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, k, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbkdj_aibkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbkdj_aibkd = eom_ccsd_22_tripletpm_trans_aibjbkdj_aibkd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbkdj_aibkd
    function eom_ccsd_22_tripletpm_trans_aibjbkdk_aijkd(t2, nocc, nactive, a, i, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbkdk_aijkd 
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

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbkdk_aijkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbkdk_aijkd = eom_ccsd_22_tripletpm_trans_aibjbkdk_aijkd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbkdk_aijkd
    function eom_ccsd_22_tripletpm_trans_aibjbjdl_aidl(t2, nocc, nactive, a, i, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbjdl_aidl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, d)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, d, l, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbjdl_aidl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjbjdl_aidl = eom_ccsd_22_tripletpm_trans_aibjbjdl_aidl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbjdl_aidl
    function eom_ccsd_22_tripletpm_trans_aibjbjdl_aijdl(t2, nocc, nactive, a, i, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbjdl_aijdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, d, j, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, d, j, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbjdl_aijdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbjdl_aijdl = eom_ccsd_22_tripletpm_trans_aibjbjdl_aijdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbjdl_aijdl
    function eom_ccsd_22_tripletpm_trans_aibjbjdl_aibdl(t2, nocc, nactive, a, i, b, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbjdl_aibdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, l, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, l, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbjdl_aibdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbjdl_aibdl = eom_ccsd_22_tripletpm_trans_aibjbjdl_aibdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbjdl_aibdl
    function eom_ccsd_22_tripletpm_trans_aibjakal_aibjkl(t2, nocc, nactive, a, i, b, j, k, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakal_aibjkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, a, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, a, k, e)
term(2) = term(2) + t2(b,e,i,j) * tovov(l, e, k, a)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, k, a)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakal_aibjkl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjakal_aibjkl = eom_ccsd_22_tripletpm_trans_aibjakal_aibjkl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakal_aibjkl
    function eom_ccsd_22_tripletpm_trans_aibjckai_bjck(t2, nocc, nactive, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckai_bjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, c, k, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, k, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckai_bjck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjckai_bjck = eom_ccsd_22_tripletpm_trans_aibjckai_bjck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckai_bjck
    function eom_ccsd_22_tripletpm_trans_aibjckai_ibjck(t2, nocc, nactive, i, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckai_ibjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, c, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, c, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckai_ibjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckai_ibjck = eom_ccsd_22_tripletpm_trans_aibjckai_ibjck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckai_ibjck
    function eom_ccsd_22_tripletpm_trans_aibjckai_abjck(t2, nocc, nactive, a, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckai_abjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, k, c)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckai_abjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckai_abjck = eom_ccsd_22_tripletpm_trans_aibjckai_abjck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckai_abjck
    function eom_ccsd_22_tripletpm_trans_aibjcial_abjcl(t2, nocc, nactive, a, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcial_abjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, l 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, a)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, l, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcial_abjcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcial_abjcl = eom_ccsd_22_tripletpm_trans_aibjcial_abjcl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcial_abjcl
    function eom_ccsd_22_tripletpm_trans_aibjcial_ibjcl(t2, nocc, nactive, i, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcial_ibjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, l 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, e, i, c)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcial_ibjcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcial_ibjcl = eom_ccsd_22_tripletpm_trans_aibjcial_ibjcl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcial_ibjcl
    function eom_ccsd_22_tripletpm_trans_aibjckaj_ibck(t2, nocc, nactive, i, b, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckaj_ibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, c)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,i,m) * tovov(m, c, k, e)
term(2) = term(2) + t2(b,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckaj_ibck = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjckaj_ibck = eom_ccsd_22_tripletpm_trans_aibjckaj_ibck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckaj_ibck
    function eom_ccsd_22_tripletpm_trans_aibjckaj_ibjck(t2, nocc, nactive, i, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckaj_ibjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, c, j, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, c, j, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckaj_ibjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckaj_ibjck = eom_ccsd_22_tripletpm_trans_aibjckaj_ibjck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckaj_ibjck
    function eom_ccsd_22_tripletpm_trans_aibjckaj_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckaj_aibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, k, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckaj_aibck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckaj_aibck = eom_ccsd_22_tripletpm_trans_aibjckaj_aibck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckaj_aibck
    function eom_ccsd_22_tripletpm_trans_aibjckak_ibjck(t2, nocc, nactive, i, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckak_ibjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, k, c)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckak_ibjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckak_ibjck = eom_ccsd_22_tripletpm_trans_aibjckak_ibjck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckak_ibjck
    function eom_ccsd_22_tripletpm_trans_aibjcjal_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjal_aibcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjal_aibcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjal_aibcl = eom_ccsd_22_tripletpm_trans_aibjcjal_aibcl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjal_aibcl
    function eom_ccsd_22_tripletpm_trans_aibjcjal_ibjcl(t2, nocc, nactive, i, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjal_ibjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, l 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, e, j, c)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, j, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjal_ibjcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjal_ibjcl = eom_ccsd_22_tripletpm_trans_aibjcjal_ibjcl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjal_ibjcl
    function eom_ccsd_22_tripletpm_trans_aibjckci_abjck(t2, nocc, nactive, a, b, j, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckci_abjck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, k, c)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, k, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckci_abjck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckci_abjck = eom_ccsd_22_tripletpm_trans_aibjckci_abjck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckci_abjck
    function eom_ccsd_22_tripletpm_trans_aibjcicl_abjcl(t2, nocc, nactive, a, b, j, c, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcicl_abjcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, l 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, l, c)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, l, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcicl_abjcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcicl_abjcl = eom_ccsd_22_tripletpm_trans_aibjcicl_abjcl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcicl_abjcl
    function eom_ccsd_22_tripletpm_trans_aibjckcj_aibck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjckcj_aibck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, k, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, k, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjckcj_aibck = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjckcj_aibck = eom_ccsd_22_tripletpm_trans_aibjckcj_aibck + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjckcj_aibck
    function eom_ccsd_22_tripletpm_trans_aibjcjcl_aibcl(t2, nocc, nactive, a, i, b, c, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjcl_aibcl 
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

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjcl_aibcl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjcl_aibcl = eom_ccsd_22_tripletpm_trans_aibjcjcl_aibcl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjcl_aibcl
    function eom_ccsd_22_tripletpm_trans_aibjakdi_ibjkd(t2, nocc, nactive, i, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakdi_ibjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, i, d)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, i, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakdi_ibjkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakdi_ibjkd = eom_ccsd_22_tripletpm_trans_aibjakdi_ibjkd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakdi_ibjkd
    function eom_ccsd_22_tripletpm_trans_aibjakdi_abjkd(t2, nocc, nactive, a, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakdi_abjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, d, k, a)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, d, k, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakdi_abjkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakdi_abjkd = eom_ccsd_22_tripletpm_trans_aibjakdi_abjkd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakdi_abjkd
    function eom_ccsd_22_tripletpm_trans_aibjaidl_bjdl(t2, nocc, nactive, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaidl_bjdl 
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


    eom_ccsd_22_tripletpm_trans_aibjaidl_bjdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjaidl_bjdl = eom_ccsd_22_tripletpm_trans_aibjaidl_bjdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaidl_bjdl
    function eom_ccsd_22_tripletpm_trans_aibjaidl_ibjdl(t2, nocc, nactive, i, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaidl_ibjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, d, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, d, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaidl_ibjdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjaidl_ibjdl = eom_ccsd_22_tripletpm_trans_aibjaidl_ibjdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaidl_ibjdl
    function eom_ccsd_22_tripletpm_trans_aibjaidl_abjdl(t2, nocc, nactive, a, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaidl_abjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, l, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, l, d)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaidl_abjdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjaidl_abjdl = eom_ccsd_22_tripletpm_trans_aibjaidl_abjdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaidl_abjdl
    function eom_ccsd_22_tripletpm_trans_aibjakdj_ibjkd(t2, nocc, nactive, i, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakdj_ibjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, j, d)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, j, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakdj_ibjkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakdj_ibjkd = eom_ccsd_22_tripletpm_trans_aibjakdj_ibjkd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakdj_ibjkd
    function eom_ccsd_22_tripletpm_trans_aibjakdj_aibkd(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakdj_aibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, d, k, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, k, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakdj_aibkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakdj_aibkd = eom_ccsd_22_tripletpm_trans_aibjakdj_aibkd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakdj_aibkd
    function eom_ccsd_22_tripletpm_trans_aibjakdk_ibjkd(t2, nocc, nactive, i, b, j, k, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakdk_ibjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, k, d)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, k, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakdk_ibjkd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakdk_ibjkd = eom_ccsd_22_tripletpm_trans_aibjakdk_ibjkd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakdk_ibjkd
    function eom_ccsd_22_tripletpm_trans_aibjajdl_ibdl(t2, nocc, nactive, i, b, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajdl_ibdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,i,m) * tovov(m, d, l, e)
term(2) = term(2) + t2(b,e,m,i) * tovov(m, e, l, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajdl_ibdl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjajdl_ibdl = eom_ccsd_22_tripletpm_trans_aibjajdl_ibdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajdl_ibdl
    function eom_ccsd_22_tripletpm_trans_aibjajdl_ibjdl(t2, nocc, nactive, i, b, j, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajdl_ibjdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, d, j, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, d, j, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajdl_ibjdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjajdl_ibjdl = eom_ccsd_22_tripletpm_trans_aibjajdl_ibjdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajdl_ibjdl
    function eom_ccsd_22_tripletpm_trans_aibjajdl_aibdl(t2, nocc, nactive, a, i, b, d, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajdl_aibdl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, l, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, l, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajdl_aibdl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjajdl_aibdl = eom_ccsd_22_tripletpm_trans_aibjajdl_aibdl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajdl_aibdl
    function eom_ccsd_22_tripletpm_trans_aibjcidi_aibjcd(t2, nocc, nactive, a, i, b, j, c, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcidi_aibjcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, i, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, d, i, c)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, d, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcidi_aibjcd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjcidi_aibjcd = eom_ccsd_22_tripletpm_trans_aibjcidi_aibjcd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcidi_aibjcd
    function eom_ccsd_22_tripletpm_trans_aibjcjdi_aibcd(t2, nocc, nactive, a, i, b, c, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjdi_aibcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, i, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjdi_aibcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjdi_aibcd = eom_ccsd_22_tripletpm_trans_aibjcjdi_aibcd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjdi_aibcd
    function eom_ccsd_22_tripletpm_trans_aibjcjdi_abjcd(t2, nocc, nactive, a, b, j, c, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjdi_abjcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, d, j, c)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, d, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjdi_abjcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjdi_abjcd = eom_ccsd_22_tripletpm_trans_aibjcjdi_abjcd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjdi_abjcd
    function eom_ccsd_22_tripletpm_trans_aibjcidj_abjcd(t2, nocc, nactive, a, b, j, c, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcidj_abjcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, j, d)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcidj_abjcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcidj_abjcd = eom_ccsd_22_tripletpm_trans_aibjcidj_abjcd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcidj_abjcd
    function eom_ccsd_22_tripletpm_trans_aibjcidj_aibcd(t2, nocc, nactive, a, i, b, c, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcidj_aibcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, d, i, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcidj_aibcd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcidj_aibcd = eom_ccsd_22_tripletpm_trans_aibjcidj_aibcd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcidj_aibcd
    function eom_ccsd_22_tripletpm_trans_aibjcjdj_aibjcd(t2, nocc, nactive, a, i, b, j, c, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjdj_aibjcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, j, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, j, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjdj_aibjcd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjcjdj_aibjcd = eom_ccsd_22_tripletpm_trans_aibjcjdj_aibjcd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjdj_aibjcd
    function eom_ccsd_22_tripletpm_trans_aibjbkbi_abjk(t2, nocc, nactive, a, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbkbi_abjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, k, b)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, k, b)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, b, k, b)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, b, k, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, k, b)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, k, b)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbkbi_abjk = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjbkbi_abjk = eom_ccsd_22_tripletpm_trans_aibjbkbi_abjk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbkbi_abjk
    function eom_ccsd_22_tripletpm_trans_aibjbkbi_aibjk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbkbi_aibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, i, b)
term(1) = term(1) + t2(a,e,j,i) * tovov(k, b, i, e)
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, i, b)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, b, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbkbi_aibjk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjbkbi_aibjk = eom_ccsd_22_tripletpm_trans_aibjbkbi_aibjk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbkbi_aibjk
    function eom_ccsd_22_tripletpm_trans_aibjbibl_abjl(t2, nocc, nactive, a, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbibl_abjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, l, b)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, l, b)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, b, l, b)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, b, l, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, l, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, l, b)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbibl_abjl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjbibl_abjl = eom_ccsd_22_tripletpm_trans_aibjbibl_abjl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbibl_abjl
    function eom_ccsd_22_tripletpm_trans_aibjbibl_aibjl(t2, nocc, nactive, a, i, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbibl_aibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, b, i, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, i, b)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, b, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, i, b)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbibl_aibjl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjbibl_aibjl = eom_ccsd_22_tripletpm_trans_aibjbibl_aibjl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbibl_aibjl
    function eom_ccsd_22_tripletpm_trans_aibjbkbj_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbkbj_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, b)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, k, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, k, b)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, b, k, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, k, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, k, b)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbkbj_aibk = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjbkbj_aibk = eom_ccsd_22_tripletpm_trans_aibjbkbj_aibk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbkbj_aibk
    function eom_ccsd_22_tripletpm_trans_aibjbkbj_aibjk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbkbj_aibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, j, b)
term(1) = term(1) + t2(a,e,j,i) * tovov(k, b, j, e)
term(2) = term(2) + t2(a,e,i,j) * tovov(k, e, j, b)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, b, j, e)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbkbj_aibjk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjbkbj_aibjk = eom_ccsd_22_tripletpm_trans_aibjbkbj_aibjk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbkbj_aibjk
    function eom_ccsd_22_tripletpm_trans_aibjbjbl_aibl(t2, nocc, nactive, a, i, b, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbjbl_aibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, b)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, l, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, l, b)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, b, l, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, l, b)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, l, b)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbjbl_aibl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjbjbl_aibl = eom_ccsd_22_tripletpm_trans_aibjbjbl_aibl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbjbl_aibl
    function eom_ccsd_22_tripletpm_trans_aibjbjbl_aibjl(t2, nocc, nactive, a, i, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbjbl_aibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, b, j, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, j, b)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, b, j, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, j, b)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbjbl_aibjl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjbjbl_aibjl = eom_ccsd_22_tripletpm_trans_aibjbjbl_aibjl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbjbl_aibjl
    function eom_ccsd_22_tripletpm_trans_aibjakbi_ajk(t2, nocc, nactive, a, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakbi_ajk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, k, a)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,j,m) * tovov(m, a, k, e)
term(2) = term(2) + t2(a,e,m,j) * tovov(m, e, k, a)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, e, k, a)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakbi_ajk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjakbi_ajk = eom_ccsd_22_tripletpm_trans_aibjakbi_ajk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakbi_ajk
    function eom_ccsd_22_tripletpm_trans_aibjakbi_ibjk(t2, nocc, nactive, i, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakbi_ibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, i, b)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, i, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakbi_ibjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakbi_ibjk = eom_ccsd_22_tripletpm_trans_aibjakbi_ibjk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakbi_ibjk
    function eom_ccsd_22_tripletpm_trans_aibjakbi_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakbi_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, a, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, a, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakbi_aijk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakbi_aijk = eom_ccsd_22_tripletpm_trans_aibjakbi_aijk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakbi_aijk
    function eom_ccsd_22_tripletpm_trans_aibjakbi_abjk(t2, nocc, nactive, a, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakbi_abjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, k, a)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, k, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakbi_abjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakbi_abjk = eom_ccsd_22_tripletpm_trans_aibjakbi_abjk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakbi_abjk
    function eom_ccsd_22_tripletpm_trans_aibjaibl_bjl(t2, nocc, nactive, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaibl_bjl 
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


    eom_ccsd_22_tripletpm_trans_aibjaibl_bjl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjaibl_bjl = eom_ccsd_22_tripletpm_trans_aibjaibl_bjl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaibl_bjl
    function eom_ccsd_22_tripletpm_trans_aibjaibl_ibjl(t2, nocc, nactive, i, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaibl_ibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, b, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, b, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaibl_ibjl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjaibl_ibjl = eom_ccsd_22_tripletpm_trans_aibjaibl_ibjl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaibl_ibjl
    function eom_ccsd_22_tripletpm_trans_aibjaibl_aijl(t2, nocc, nactive, a, i, j, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaibl_aijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, e, i, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaibl_aijl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjaibl_aijl = eom_ccsd_22_tripletpm_trans_aibjaibl_aijl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaibl_aijl
    function eom_ccsd_22_tripletpm_trans_aibjaibl_abjl(t2, nocc, nactive, a, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaibl_abjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, l, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, l, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaibl_abjl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjaibl_abjl = eom_ccsd_22_tripletpm_trans_aibjaibl_abjl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaibl_abjl
    function eom_ccsd_22_tripletpm_trans_aibjakbj_aik(t2, nocc, nactive, a, i, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakbj_aik 
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


    eom_ccsd_22_tripletpm_trans_aibjakbj_aik = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjakbj_aik = eom_ccsd_22_tripletpm_trans_aibjakbj_aik + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakbj_aik
    function eom_ccsd_22_tripletpm_trans_aibjakbj_ibjk(t2, nocc, nactive, i, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakbj_ibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, j, b)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, j, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakbj_ibjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakbj_ibjk = eom_ccsd_22_tripletpm_trans_aibjakbj_ibjk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakbj_ibjk
    function eom_ccsd_22_tripletpm_trans_aibjakbj_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakbj_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, a, j, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, a, j, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakbj_aijk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakbj_aijk = eom_ccsd_22_tripletpm_trans_aibjakbj_aijk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakbj_aijk
    function eom_ccsd_22_tripletpm_trans_aibjakbj_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakbj_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, k, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, k, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakbj_aibk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakbj_aibk = eom_ccsd_22_tripletpm_trans_aibjakbj_aibk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakbj_aibk
    function eom_ccsd_22_tripletpm_trans_aibjakbk_ibjk(t2, nocc, nactive, i, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakbk_ibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, k, b)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, k, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakbk_ibjk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakbk_ibjk = eom_ccsd_22_tripletpm_trans_aibjakbk_ibjk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakbk_ibjk
    function eom_ccsd_22_tripletpm_trans_aibjakbk_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakbk_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, k, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, k, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakbk_aijk = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjakbk_aijk = eom_ccsd_22_tripletpm_trans_aibjakbk_aijk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakbk_aijk
    function eom_ccsd_22_tripletpm_trans_aibjajbl_ibl(t2, nocc, nactive, i, b, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajbl_ibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, b)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,i,m) * tovov(m, b, l, e)
term(2) = term(2) + t2(b,e,m,i) * tovov(m, e, l, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, e, l, b)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajbl_ibl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjajbl_ibl = eom_ccsd_22_tripletpm_trans_aibjajbl_ibl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajbl_ibl
    function eom_ccsd_22_tripletpm_trans_aibjajbl_ibjl(t2, nocc, nactive, i, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajbl_ibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, b, j, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, b, j, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajbl_ibjl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjajbl_ibjl = eom_ccsd_22_tripletpm_trans_aibjajbl_ibjl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajbl_ibjl
    function eom_ccsd_22_tripletpm_trans_aibjajbl_aijl(t2, nocc, nactive, a, i, j, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajbl_aijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, e, j, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, j, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajbl_aijl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjajbl_aijl = eom_ccsd_22_tripletpm_trans_aibjajbl_aijl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajbl_aijl
    function eom_ccsd_22_tripletpm_trans_aibjajbl_aibl(t2, nocc, nactive, a, i, b, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajbl_aibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, l, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, l, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajbl_aibl = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjajbl_aibl = eom_ccsd_22_tripletpm_trans_aibjajbl_aibl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajbl_aibl
    function eom_ccsd_22_tripletpm_trans_aibjcibi_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcibi_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, i, c)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,j,i) * tovov(i, e, i, c)
term(2) = term(2) + t2(a,e,i,j) * tovov(i, e, i, c)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, c, i, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, i, c)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, i, c)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcibi_aijc = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjcibi_aijc = eom_ccsd_22_tripletpm_trans_aibjcibi_aijc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcibi_aijc
    function eom_ccsd_22_tripletpm_trans_aibjcibi_aibjc(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcibi_aibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, i, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, i, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, i, c)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, b, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcibi_aibjc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjcibi_aibjc = eom_ccsd_22_tripletpm_trans_aibjcibi_aibjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcibi_aibjc
    function eom_ccsd_22_tripletpm_trans_aibjcjbi_ajc(t2, nocc, nactive, a, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjbi_ajc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, j, c)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,j,m) * tovov(m, c, j, e)
term(2) = term(2) + t2(a,e,m,j) * tovov(m, e, j, c)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjbi_ajc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjcjbi_ajc = eom_ccsd_22_tripletpm_trans_aibjcjbi_ajc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjbi_ajc
    function eom_ccsd_22_tripletpm_trans_aibjcjbi_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjbi_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, i, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, i, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjbi_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjbi_aibc = eom_ccsd_22_tripletpm_trans_aibjcjbi_aibc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjbi_aibc
    function eom_ccsd_22_tripletpm_trans_aibjcjbi_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjbi_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, c)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjbi_abjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjbi_abjc = eom_ccsd_22_tripletpm_trans_aibjcjbi_abjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjbi_abjc
    function eom_ccsd_22_tripletpm_trans_aibjcjbi_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjbi_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, c, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, c, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjbi_aijc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjbi_aijc = eom_ccsd_22_tripletpm_trans_aibjcjbi_aijc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjbi_aijc
    function eom_ccsd_22_tripletpm_trans_aibjcibj_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcibj_aic 
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


    eom_ccsd_22_tripletpm_trans_aibjcibj_aic = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjcibj_aic = eom_ccsd_22_tripletpm_trans_aibjcibj_aic + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcibj_aic
    function eom_ccsd_22_tripletpm_trans_aibjcibj_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcibj_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, j, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcibj_abjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcibj_abjc = eom_ccsd_22_tripletpm_trans_aibjcibj_abjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcibj_abjc
    function eom_ccsd_22_tripletpm_trans_aibjcibj_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcibj_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, i, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcibj_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcibj_aibc = eom_ccsd_22_tripletpm_trans_aibjcibj_aibc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcibj_aibc
    function eom_ccsd_22_tripletpm_trans_aibjcibj_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcibj_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, e, i, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, e, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcibj_aijc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcibj_aijc = eom_ccsd_22_tripletpm_trans_aibjcibj_aijc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcibj_aijc
    function eom_ccsd_22_tripletpm_trans_aibjcjbj_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjbj_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, j, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,j,i) * tovov(j, e, j, c)
term(2) = term(2) + t2(a,e,i,j) * tovov(j, e, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, c, j, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, j, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, j, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjbj_aijc = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjcjbj_aijc = eom_ccsd_22_tripletpm_trans_aibjcjbj_aijc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjbj_aijc
    function eom_ccsd_22_tripletpm_trans_aibjcjbj_aibjc(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjbj_aibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, j, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, j, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjbj_aibjc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjcjbj_aibjc = eom_ccsd_22_tripletpm_trans_aibjcjbj_aibjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjbj_aibjc
    function eom_ccsd_22_tripletpm_trans_aibjbidi_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbidi_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, i, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,j,i) * tovov(i, e, i, d)
term(2) = term(2) + t2(a,e,i,j) * tovov(i, e, i, d)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, d, i, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, i, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, i, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbidi_aijd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjbidi_aijd = eom_ccsd_22_tripletpm_trans_aibjbidi_aijd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbidi_aijd
    function eom_ccsd_22_tripletpm_trans_aibjbidi_aibjd(t2, nocc, nactive, a, i, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbidi_aibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, i, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, i, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, d, i, b)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, d, i, b)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbidi_aibjd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjbidi_aibjd = eom_ccsd_22_tripletpm_trans_aibjbidi_aibjd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbidi_aibjd
    function eom_ccsd_22_tripletpm_trans_aibjbjdi_aid(t2, nocc, nactive, a, i, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbjdi_aid 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, d 
    integer :: s ,e,m 
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


    eom_ccsd_22_tripletpm_trans_aibjbjdi_aid = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjbjdi_aid = eom_ccsd_22_tripletpm_trans_aibjbjdi_aid + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbjdi_aid
    function eom_ccsd_22_tripletpm_trans_aibjbjdi_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbjdi_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, e, i, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, e, i, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbjdi_aijd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbjdi_aijd = eom_ccsd_22_tripletpm_trans_aibjbjdi_aijd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbjdi_aijd
    function eom_ccsd_22_tripletpm_trans_aibjbjdi_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbjdi_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbjdi_aibd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbjdi_aibd = eom_ccsd_22_tripletpm_trans_aibjbjdi_aibd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbjdi_aibd
    function eom_ccsd_22_tripletpm_trans_aibjbjdi_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbjdi_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, d, j, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, d, j, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbjdi_abjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbjdi_abjd = eom_ccsd_22_tripletpm_trans_aibjbjdi_abjd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbjdi_abjd
    function eom_ccsd_22_tripletpm_trans_aibjbidj_ajd(t2, nocc, nactive, a, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbidj_ajd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, j, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,j,m) * tovov(m, d, j, e)
term(2) = term(2) + t2(a,e,m,j) * tovov(m, e, j, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbidj_ajd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjbidj_ajd = eom_ccsd_22_tripletpm_trans_aibjbidj_ajd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbidj_ajd
    function eom_ccsd_22_tripletpm_trans_aibjbidj_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbidj_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, d, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, d, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbidj_aijd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbidj_aijd = eom_ccsd_22_tripletpm_trans_aibjbidj_aijd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbidj_aijd
    function eom_ccsd_22_tripletpm_trans_aibjbidj_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbidj_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, j, d)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbidj_abjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbidj_abjd = eom_ccsd_22_tripletpm_trans_aibjbidj_abjd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbidj_abjd
    function eom_ccsd_22_tripletpm_trans_aibjbidj_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbidj_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, d, i, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbidj_aibd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjbidj_aibd = eom_ccsd_22_tripletpm_trans_aibjbidj_aibd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbidj_aibd
    function eom_ccsd_22_tripletpm_trans_aibjbjdj_aijd(t2, nocc, nactive, a, i, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbjdj_aijd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, j, d)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,j,i) * tovov(j, e, j, d)
term(2) = term(2) + t2(a,e,i,j) * tovov(j, e, j, d)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, j, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, j, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, j, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbjdj_aijd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjbjdj_aijd = eom_ccsd_22_tripletpm_trans_aibjbjdj_aijd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbjdj_aijd
    function eom_ccsd_22_tripletpm_trans_aibjbjdj_aibjd(t2, nocc, nactive, a, i, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbjdj_aibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, j, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, j, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, j, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, j, b)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbjdj_aibjd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjbjdj_aibjd = eom_ccsd_22_tripletpm_trans_aibjbjdj_aibjd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbjdj_aibjd
    function eom_ccsd_22_tripletpm_trans_aibjakai_abjk(t2, nocc, nactive, a, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakai_abjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, a)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, k, a)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, a, k, a)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, a, k, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, k, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, k, a)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakai_abjk = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjakai_abjk = eom_ccsd_22_tripletpm_trans_aibjakai_abjk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakai_abjk
    function eom_ccsd_22_tripletpm_trans_aibjakai_aibjk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakai_aibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, i, a)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, i, a)
term(2) = term(2) + t2(b,e,i,j) * tovov(k, a, i, e)
term(3) = term(3) + t2(b,e,j,i) * tovov(k, a, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakai_aibjk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjakai_aibjk = eom_ccsd_22_tripletpm_trans_aibjakai_aibjk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakai_aibjk
    function eom_ccsd_22_tripletpm_trans_aibjaial_abjl(t2, nocc, nactive, a, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaial_abjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, l, a)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, l, a)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, a, l, a)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, a, l, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, l, a)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, l, a)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaial_abjl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjaial_abjl = eom_ccsd_22_tripletpm_trans_aibjaial_abjl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaial_abjl
    function eom_ccsd_22_tripletpm_trans_aibjaial_aibjl(t2, nocc, nactive, a, i, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaial_aibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, a, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, a, i, e)
term(2) = term(2) + t2(b,e,i,j) * tovov(l, e, i, a)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, i, a)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaial_aibjl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjaial_aibjl = eom_ccsd_22_tripletpm_trans_aibjaial_aibjl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaial_aibjl
    function eom_ccsd_22_tripletpm_trans_aibjakaj_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakaj_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, a)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, k, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, k, a)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, a, k, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakaj_aibk = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjakaj_aibk = eom_ccsd_22_tripletpm_trans_aibjakaj_aibk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakaj_aibk
    function eom_ccsd_22_tripletpm_trans_aibjakaj_aibjk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjakaj_aibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, j, a)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, j, a)
term(2) = term(2) + t2(b,e,i,j) * tovov(k, a, j, e)
term(3) = term(3) + t2(b,e,j,i) * tovov(k, a, j, e)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjakaj_aibjk = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjakaj_aibjk = eom_ccsd_22_tripletpm_trans_aibjakaj_aibjk + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjakaj_aibjk
    function eom_ccsd_22_tripletpm_trans_aibjajal_aibl(t2, nocc, nactive, a, i, b, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajal_aibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, l 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, a)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, l, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, l, a)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, a, l, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, l, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajal_aibl = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjajal_aibl = eom_ccsd_22_tripletpm_trans_aibjajal_aibl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajal_aibl
    function eom_ccsd_22_tripletpm_trans_aibjajal_aibjl(t2, nocc, nactive, a, i, b, j, l) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajal_aibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, a, j, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, a, j, e)
term(2) = term(2) + t2(b,e,i,j) * tovov(l, e, j, a)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, j, a)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajal_aibjl = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjajal_aibjl = eom_ccsd_22_tripletpm_trans_aibjajal_aibjl + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajal_aibjl
    function eom_ccsd_22_tripletpm_trans_aibjciai_ibjc(t2, nocc, nactive, i, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjciai_ibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, i, c)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,j) * tovov(i, e, i, c)
term(2) = term(2) + t2(b,e,j,i) * tovov(i, e, i, c)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, c, i, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, i, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, i, c)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjciai_ibjc = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjciai_ibjc = eom_ccsd_22_tripletpm_trans_aibjciai_ibjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjciai_ibjc
    function eom_ccsd_22_tripletpm_trans_aibjciai_aibjc(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjciai_aibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, i, a)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, i, a)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, i, c)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, a, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjciai_aibjc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjciai_aibjc = eom_ccsd_22_tripletpm_trans_aibjciai_aibjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjciai_aibjc
    function eom_ccsd_22_tripletpm_trans_aibjcjai_bjc(t2, nocc, nactive, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjai_bjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: b, j, c 
    integer :: s ,m,e 
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


    eom_ccsd_22_tripletpm_trans_aibjcjai_bjc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjcjai_bjc = eom_ccsd_22_tripletpm_trans_aibjcjai_bjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjai_bjc
    function eom_ccsd_22_tripletpm_trans_aibjcjai_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjai_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, i, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, i, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjai_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjai_aibc = eom_ccsd_22_tripletpm_trans_aibjcjai_aibc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjai_aibc
    function eom_ccsd_22_tripletpm_trans_aibjcjai_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjai_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, c)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjai_abjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjai_abjc = eom_ccsd_22_tripletpm_trans_aibjcjai_abjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjai_abjc
    function eom_ccsd_22_tripletpm_trans_aibjcjai_ibjc(t2, nocc, nactive, i, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjai_ibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(j, c, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, c, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjai_ibjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcjai_ibjc = eom_ccsd_22_tripletpm_trans_aibjcjai_ibjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjai_ibjc
    function eom_ccsd_22_tripletpm_trans_aibjciaj_ibc(t2, nocc, nactive, i, b, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjciaj_ibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, i, c)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,i,m) * tovov(m, c, i, e)
term(2) = term(2) + t2(b,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjciaj_ibc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjciaj_ibc = eom_ccsd_22_tripletpm_trans_aibjciaj_ibc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjciaj_ibc
    function eom_ccsd_22_tripletpm_trans_aibjciaj_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjciaj_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, j, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjciaj_abjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjciaj_abjc = eom_ccsd_22_tripletpm_trans_aibjciaj_abjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjciaj_abjc
    function eom_ccsd_22_tripletpm_trans_aibjciaj_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjciaj_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, i, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjciaj_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjciaj_aibc = eom_ccsd_22_tripletpm_trans_aibjciaj_aibc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjciaj_aibc
    function eom_ccsd_22_tripletpm_trans_aibjciaj_ibjc(t2, nocc, nactive, i, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjciaj_ibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(j, e, i, c)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjciaj_ibjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjciaj_ibjc = eom_ccsd_22_tripletpm_trans_aibjciaj_ibjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjciaj_ibjc
    function eom_ccsd_22_tripletpm_trans_aibjcjaj_ibjc(t2, nocc, nactive, i, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjaj_ibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, j, c)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,j) * tovov(j, e, j, c)
term(2) = term(2) + t2(b,e,j,i) * tovov(j, e, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, c, j, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, j, c)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, j, c)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjaj_ibjc = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjcjaj_ibjc = eom_ccsd_22_tripletpm_trans_aibjcjaj_ibjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjaj_ibjc
    function eom_ccsd_22_tripletpm_trans_aibjcjaj_aibjc(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcjaj_aibjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, j, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, j, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, j, c)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcjaj_aibjc = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjcjaj_aibjc = eom_ccsd_22_tripletpm_trans_aibjcjaj_aibjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcjaj_aibjc
    function eom_ccsd_22_tripletpm_trans_aibjcicj_abjc(t2, nocc, nactive, a, b, j, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcicj_abjc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, c, j, c)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, j, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcicj_abjc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcicj_abjc = eom_ccsd_22_tripletpm_trans_aibjcicj_abjc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcicj_abjc
    function eom_ccsd_22_tripletpm_trans_aibjcicj_aibc(t2, nocc, nactive, a, i, b, c) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjcicj_aibc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c 
    integer :: s ,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, i, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, i, c)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjcicj_aibc = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjcicj_aibc = eom_ccsd_22_tripletpm_trans_aibjcicj_aibc + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjcicj_aibc
    function eom_ccsd_22_tripletpm_trans_aibjaidi_ibjd(t2, nocc, nactive, i, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaidi_ibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, i, d)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,j) * tovov(i, e, i, d)
term(2) = term(2) + t2(b,e,j,i) * tovov(i, e, i, d)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, d, i, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, i, d)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, i, d)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaidi_ibjd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjaidi_ibjd = eom_ccsd_22_tripletpm_trans_aibjaidi_ibjd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaidi_ibjd
    function eom_ccsd_22_tripletpm_trans_aibjaidi_aibjd(t2, nocc, nactive, a, i, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaidi_aibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, i, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, i, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, d, i, a)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, d, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaidi_aibjd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjaidi_aibjd = eom_ccsd_22_tripletpm_trans_aibjaidi_aibjd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaidi_aibjd
    function eom_ccsd_22_tripletpm_trans_aibjajdi_ibd(t2, nocc, nactive, i, b, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajdi_ibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, i, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,i,m) * tovov(m, d, i, e)
term(2) = term(2) + t2(b,e,m,i) * tovov(m, e, i, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajdi_ibd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjajdi_ibd = eom_ccsd_22_tripletpm_trans_aibjajdi_ibd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajdi_ibd
    function eom_ccsd_22_tripletpm_trans_aibjajdi_ibjd(t2, nocc, nactive, i, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajdi_ibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(j, e, i, d)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, i, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajdi_ibjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjajdi_ibjd = eom_ccsd_22_tripletpm_trans_aibjajdi_ibjd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajdi_ibjd
    function eom_ccsd_22_tripletpm_trans_aibjajdi_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajdi_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, i, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, d)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajdi_aibd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjajdi_aibd = eom_ccsd_22_tripletpm_trans_aibjajdi_aibd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajdi_aibd
    function eom_ccsd_22_tripletpm_trans_aibjajdi_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajdi_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, d, j, a)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, d, j, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajdi_abjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjajdi_abjd = eom_ccsd_22_tripletpm_trans_aibjajdi_abjd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajdi_abjd
    function eom_ccsd_22_tripletpm_trans_aibjaidj_bjd(t2, nocc, nactive, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaidj_bjd 
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


    eom_ccsd_22_tripletpm_trans_aibjaidj_bjd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjaidj_bjd = eom_ccsd_22_tripletpm_trans_aibjaidj_bjd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaidj_bjd
    function eom_ccsd_22_tripletpm_trans_aibjaidj_ibjd(t2, nocc, nactive, i, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaidj_ibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(j, d, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, d, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaidj_ibjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjaidj_ibjd = eom_ccsd_22_tripletpm_trans_aibjaidj_ibjd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaidj_ibjd
    function eom_ccsd_22_tripletpm_trans_aibjaidj_abjd(t2, nocc, nactive, a, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaidj_abjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, j, d)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaidj_abjd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjaidj_abjd = eom_ccsd_22_tripletpm_trans_aibjaidj_abjd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaidj_abjd
    function eom_ccsd_22_tripletpm_trans_aibjaidj_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaidj_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, d, i, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaidj_aibd = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjaidj_aibd = eom_ccsd_22_tripletpm_trans_aibjaidj_aibd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaidj_aibd
    function eom_ccsd_22_tripletpm_trans_aibjajdj_ibjd(t2, nocc, nactive, i, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajdj_ibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, j, d)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,j) * tovov(j, e, j, d)
term(2) = term(2) + t2(b,e,j,i) * tovov(j, e, j, d)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, d, j, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, j, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, j, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajdj_ibjd = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjajdj_ibjd = eom_ccsd_22_tripletpm_trans_aibjajdj_ibjd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajdj_ibjd
    function eom_ccsd_22_tripletpm_trans_aibjajdj_aibjd(t2, nocc, nactive, a, i, b, j, d) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajdj_aibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, j, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, j, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, j, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, j, a)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajdj_aibjd = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjajdj_aibjd = eom_ccsd_22_tripletpm_trans_aibjajdj_aibjd + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajdj_aibjd
    function eom_ccsd_22_tripletpm_trans_aibjbibj_abj(t2, nocc, nactive, a, b, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbibj_abj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, j, b)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, b)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, b, j, b)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, b, j, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, j, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, j, b)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbibj_abj = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjbibj_abj = eom_ccsd_22_tripletpm_trans_aibjbibj_abj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbibj_abj
    function eom_ccsd_22_tripletpm_trans_aibjbibj_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbibj_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, i, b)

term(0) = term(0) * (-1.0d+0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, i, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, i, b)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, b, i, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, i, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, i, b)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbibj_aib = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjbibj_aib = eom_ccsd_22_tripletpm_trans_aibjbibj_aib + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbibj_aib
    function eom_ccsd_22_tripletpm_trans_aibjbibj_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjbibj_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, b, i, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(j, e, i, b)
term(2) = term(2) + t2(a,e,i,j) * tovov(j, b, i, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(j, e, i, b)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjbibj_aibj = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjbibj_aibj = eom_ccsd_22_tripletpm_trans_aibjbibj_aibj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjbibj_aibj
    function eom_ccsd_22_tripletpm_trans_aibjaibi_ibj(t2, nocc, nactive, i, b, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaibi_ibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, i, b)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,j) * tovov(i, e, i, b)
term(2) = term(2) + t2(b,e,j,i) * tovov(i, e, i, b)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, b, i, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, i, b)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, i, b)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaibi_ibj = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjaibi_ibj = eom_ccsd_22_tripletpm_trans_aibjaibi_ibj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaibi_ibj
    function eom_ccsd_22_tripletpm_trans_aibjaibi_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaibi_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, i, a)


do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,j,i) * tovov(i, e, i, a)
term(2) = term(2) + t2(a,e,i,j) * tovov(i, e, i, a)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, a, i, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, i, a)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, i, a)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaibi_aij = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjaibi_aij = eom_ccsd_22_tripletpm_trans_aibjaibi_aij + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaibi_aij
    function eom_ccsd_22_tripletpm_trans_aibjaibi_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaibi_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, i, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, i, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, i, a)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, b, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaibi_aibj = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjaibi_aibj = eom_ccsd_22_tripletpm_trans_aibjaibi_aibj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaibi_aibj
    function eom_ccsd_22_tripletpm_trans_aibjajbi_ib(t2, nocc, nactive, i, b) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajbi_ib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, i, b)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,i,m) * tovov(m, b, i, e)
term(2) = term(2) + t2(b,e,m,i) * tovov(m, e, i, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, e, i, b)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajbi_ib = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjajbi_ib = eom_ccsd_22_tripletpm_trans_aibjajbi_ib + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajbi_ib
    function eom_ccsd_22_tripletpm_trans_aibjajbi_aj(t2, nocc, nactive, a, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajbi_aj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, j, a)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,j,m) * tovov(m, a, j, e)
term(2) = term(2) + t2(a,e,m,j) * tovov(m, e, j, a)
end do 
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, e, j, a)
end do 
end do 

term(3) = term(3) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajbi_aj = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjajbi_aj = eom_ccsd_22_tripletpm_trans_aibjajbi_aj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajbi_aj
    function eom_ccsd_22_tripletpm_trans_aibjajbi_ibj(t2, nocc, nactive, i, b, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajbi_ibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(j, e, i, b)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, i, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajbi_ibj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjajbi_ibj = eom_ccsd_22_tripletpm_trans_aibjajbi_ibj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajbi_ibj
    function eom_ccsd_22_tripletpm_trans_aibjajbi_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajbi_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, a, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, a, i, e)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajbi_aij = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjajbi_aij = eom_ccsd_22_tripletpm_trans_aibjajbi_aij + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajbi_aij
    function eom_ccsd_22_tripletpm_trans_aibjajbi_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajbi_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, i, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, a, i, b)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajbi_aib = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjajbi_aib = eom_ccsd_22_tripletpm_trans_aibjajbi_aib + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajbi_aib
    function eom_ccsd_22_tripletpm_trans_aibjajbi_abj(t2, nocc, nactive, a, b, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajbi_abj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, b, j, a)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, j, a)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajbi_abj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjajbi_abj = eom_ccsd_22_tripletpm_trans_aibjajbi_abj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajbi_abj
    function eom_ccsd_22_tripletpm_trans_aibjaibj_bj(t2, nocc, nactive, b, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaibj_bj 
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


    eom_ccsd_22_tripletpm_trans_aibjaibj_bj = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjaibj_bj = eom_ccsd_22_tripletpm_trans_aibjaibj_bj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaibj_bj
    function eom_ccsd_22_tripletpm_trans_aibjaibj_ai(t2, nocc, nactive, a, i) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaibj_ai 
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


    eom_ccsd_22_tripletpm_trans_aibjaibj_ai = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjaibj_ai = eom_ccsd_22_tripletpm_trans_aibjaibj_ai + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaibj_ai
    function eom_ccsd_22_tripletpm_trans_aibjaibj_ibj(t2, nocc, nactive, i, b, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaibj_ibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(j, b, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, b, i, e)
end do 

term(1) = term(1) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaibj_ibj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjaibj_ibj = eom_ccsd_22_tripletpm_trans_aibjaibj_ibj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaibj_ibj
    function eom_ccsd_22_tripletpm_trans_aibjaibj_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaibj_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, e, i, a)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaibj_aij = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjaibj_aij = eom_ccsd_22_tripletpm_trans_aibjaibj_aij + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaibj_aij
    function eom_ccsd_22_tripletpm_trans_aibjaibj_abj(t2, nocc, nactive, a, b, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaibj_abj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,j) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, j, b)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaibj_abj = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjaibj_abj = eom_ccsd_22_tripletpm_trans_aibjaibj_abj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaibj_abj
    function eom_ccsd_22_tripletpm_trans_aibjaibj_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaibj_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,e,m 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, i, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, b, i, a)
end do 

term(0) = term(0) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaibj_aib = 0.d+0
    do s = 0, 1
    eom_ccsd_22_tripletpm_trans_aibjaibj_aib = eom_ccsd_22_tripletpm_trans_aibjaibj_aib + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaibj_aib
    function eom_ccsd_22_tripletpm_trans_aibjajbj_ibj(t2, nocc, nactive, i, b, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajbj_ibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, j, b)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(b,e,i,j) * tovov(j, e, j, b)
term(2) = term(2) + t2(b,e,j,i) * tovov(j, e, j, b)
end do 

term(2) = term(2) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, b, j, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, j, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, j, b)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajbj_ibj = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjajbj_ibj = eom_ccsd_22_tripletpm_trans_aibjajbj_ibj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajbj_ibj
    function eom_ccsd_22_tripletpm_trans_aibjajbj_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajbj_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, j, a)

term(0) = term(0) * (-1.0d+0) 

do e = nocc + 1, nactive 
term(1) = term(1) + t2(a,e,j,i) * tovov(j, e, j, a)
term(2) = term(2) + t2(a,e,i,j) * tovov(j, e, j, a)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, a, j, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, j, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, j, a)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajbj_aij = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjajbj_aij = eom_ccsd_22_tripletpm_trans_aibjajbj_aij + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajbj_aij
    function eom_ccsd_22_tripletpm_trans_aibjajbj_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjajbj_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, j, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, j, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, j, a)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjajbj_aibj = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjajbj_aibj = eom_ccsd_22_tripletpm_trans_aibjajbj_aibj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjajbj_aibj
    function eom_ccsd_22_tripletpm_trans_aibjaiaj_abj(t2, nocc, nactive, a, b, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaiaj_abj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, j, a)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, j, a)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, a, j, a)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, a, j, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, j, a)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, j, a)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaiaj_abj = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjaiaj_abj = eom_ccsd_22_tripletpm_trans_aibjaiaj_abj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaiaj_abj
    function eom_ccsd_22_tripletpm_trans_aibjaiaj_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaiaj_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,e,m 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, i, a)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, i, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, i, a)
end do 

term(1) = term(1) * (-1.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, a, i, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(5) = term(5) * (2.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaiaj_aib = 0.d+0
    do s = 0, 5
    eom_ccsd_22_tripletpm_trans_aibjaiaj_aib = eom_ccsd_22_tripletpm_trans_aibjaiaj_aib + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaiaj_aib
    function eom_ccsd_22_tripletpm_trans_aibjaiaj_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: eom_ccsd_22_tripletpm_trans_aibjaiaj_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(j, a, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, a, i, e)
term(2) = term(2) + t2(b,e,i,j) * tovov(j, e, i, a)
term(3) = term(3) + t2(b,e,j,i) * tovov(j, e, i, a)
end do 

term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 


    eom_ccsd_22_tripletpm_trans_aibjaiaj_aibj = 0.d+0
    do s = 0, 3
    eom_ccsd_22_tripletpm_trans_aibjaiaj_aibj = eom_ccsd_22_tripletpm_trans_aibjaiaj_aibj + term(s)
    end do

    end function eom_ccsd_22_tripletpm_trans_aibjaiaj_aibj
    end module eom_ccsd_22_tripletpm_trans
    