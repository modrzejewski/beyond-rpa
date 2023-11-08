module v7_eom_cc3_31_trans

    use ccsd_transformed_integrals                                                                                                                                   
    use t1_transformed_int                                                                                                                   
    use eom_vectors                                                                                                                               
    use basis                                                                                                                           
    use arithmetic                                                                                                                           
    use s_gen                                                                    
    use cc3_intermediates 
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2019-01-28 16:56:44 UTC.
    !
    contains
    
    function v7_eom_cc3_31_trans_aibjbkbl_aibjkl(t2, nocc, nactive, a, i, b, j, k, l) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkbl_aibjkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(b,e,i,k) * tvoov(a, j, l, e)
term(2) = term(2) + t2(b,e,j,k) * tvoov(a, i, l, e)
term(3) = term(3) + t2(b,e,k,j) * tvoov(a, i, l, e)
term(4) = term(4) + t2(a,e,j,k) * tvoov(b, i, l, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(b, k, l, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(b, k, l, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(b, j, l, e)
term(8) = term(8) + t2(b,e,k,j) * tvvoo(a, e, l, i)
term(9) = term(9) + t2(a,e,j,k) * tvvoo(b, e, l, i)
term(10) = term(10) + t2(b,e,j,i) * tvvoo(a, e, l, k)
term(11) = term(11) + t2(b,e,k,i) * tvvoo(a, e, l, j)
term(12) = term(12) + t2(b,e,i,j) * tvvoo(a, e, l, k)
term(13) = term(13) + t2(a,e,j,i) * tvvoo(b, e, l, k)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(b, e, l, k)
term(15) = term(15) + t2(a,e,i,k) * tvvoo(b, e, l, j)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 
term(14) = term(14) * (-1.0000000000000002d+0) 
term(15) = term(15) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(a,b,m,k) * toooo(m, j, l, i)
term(17) = term(17) + t2(a,b,j,m) * toooo(m, k, l, i)
term(18) = term(18) + t2(a,b,m,j) * toooo(m, i, l, k)
term(19) = term(19) + t2(a,b,m,k) * toooo(m, i, l, j)
term(20) = term(20) + t2(a,b,m,i) * toooo(m, j, l, k)
term(21) = term(21) + t2(a,b,j,m) * toooo(m, i, l, k)
term(22) = term(22) + t2(a,b,i,m) * toooo(m, j, l, k)
term(23) = term(23) + t2(a,b,i,m) * toooo(m, k, l, j)
end do 

term(16) = term(16) * (-1.0000000000000002d+0) 
term(17) = term(17) * (-1.0000000000000002d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(21) = term(21) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkbl_aibjkl = 0.d+0
    do s = 0, 23
    v7_eom_cc3_31_trans_aibjbkbl_aibjkl = v7_eom_cc3_31_trans_aibjbkbl_aibjkl + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkbl_aibjkl
    function v7_eom_cc3_31_trans_aibjbkal_ibjkl(t2, nocc, nactive, i, b, j, k, l) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkal_ibjkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * tvoov(b, i, l, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(b, k, l, e)
term(2) = term(2) + t2(b,e,k,i) * tvoov(b, j, l, e)
term(3) = term(3) + t2(b,e,i,j) * tvoov(b, k, l, e)
term(4) = term(4) + t2(b,e,k,i) * tvvoo(b, e, l, j)
term(5) = term(5) + t2(b,e,i,k) * tvvoo(b, e, l, j)
term(6) = term(6) + t2(b,e,j,k) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(b,e,k,j) * tvvoo(b, e, l, i)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(b,b,k,m) * toooo(m, i, l, j)
term(9) = term(9) + t2(b,b,i,m) * toooo(m, k, l, j)
term(10) = term(10) + t2(b,b,j,m) * toooo(m, k, l, i)
term(11) = term(11) + t2(b,b,k,m) * toooo(m, j, l, i)
end do 

term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkal_ibjkl = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aibjbkal_ibjkl = v7_eom_cc3_31_trans_aibjbkal_ibjkl + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkal_ibjkl
    function v7_eom_cc3_31_trans_aibjbkdi_abjkd(t2, nocc, nactive, a, b, j, k, d) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkdi_abjkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,k) * read_ftvvvv(b, e, a, d)
term(1) = term(1) + t2(b,e,k,j) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(b,e,k,j) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(a,e,j,k) * read_ftvvvv(b, e, b, d)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(b,b,k,m) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,b,m,j) * tvoov(b, k, m, d)
term(6) = term(6) + t2(a,b,m,k) * tvoov(b, j, m, d)
term(7) = term(7) + t2(a,b,j,m) * tvoov(b, k, m, d)
term(8) = term(8) + t2(b,b,j,m) * tvvoo(a, d, m, k)
term(9) = term(9) + t2(b,b,k,m) * tvvoo(a, d, m, j)
term(10) = term(10) + t2(a,b,m,k) * tvvoo(b, d, m, j)
term(11) = term(11) + t2(a,b,j,m) * tvvoo(b, d, m, k)
end do 

term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkdi_abjkd = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aibjbkdi_abjkd = v7_eom_cc3_31_trans_aibjbkdi_abjkd + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkdi_abjkd
    function v7_eom_cc3_31_trans_aibjbkdk_aibjd(t2, nocc, nactive, a, i, b, j, d) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkdk_aibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * read_ftvvvv(b, d, a, e)
term(1) = term(1) + t2(b,e,i,j) * read_ftvvvv(b, d, a, e)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(b, e, b, d)
term(3) = term(3) + t2(a,e,i,j) * read_ftvvvv(b, e, b, d)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(b,b,i,m) * tvoov(a, j, m, d)
term(5) = term(5) + t2(b,b,j,m) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,b,j,m) * tvoov(b, i, m, d)
term(7) = term(7) + t2(a,b,i,m) * tvoov(b, j, m, d)
term(8) = term(8) + t2(a,b,m,j) * tvvoo(b, d, m, i)
term(9) = term(9) + t2(a,b,m,i) * tvvoo(b, d, m, j)
term(10) = term(10) + t2(a,b,j,m) * tvvoo(b, d, m, i)
term(11) = term(11) + t2(a,b,i,m) * tvvoo(b, d, m, j)
end do 

term(5) = term(5) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkdk_aibjd = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aibjbkdk_aibjd = v7_eom_cc3_31_trans_aibjbkdk_aibjd + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkdk_aibjd
    function v7_eom_cc3_31_trans_aibjbkdj_aibkd(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkdj_aibkd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * read_ftvvvv(b, e, a, d)
term(1) = term(1) + t2(b,e,i,k) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(b,e,k,i) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(b, e, b, d)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(b,b,k,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,b,m,k) * tvoov(b, i, m, d)
term(6) = term(6) + t2(a,b,m,i) * tvoov(b, k, m, d)
term(7) = term(7) + t2(a,b,i,m) * tvoov(b, k, m, d)
term(8) = term(8) + t2(b,b,k,m) * tvvoo(a, d, m, i)
term(9) = term(9) + t2(b,b,i,m) * tvvoo(a, d, m, k)
term(10) = term(10) + t2(a,b,m,k) * tvvoo(b, d, m, i)
term(11) = term(11) + t2(a,b,i,m) * tvvoo(b, d, m, k)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkdj_aibkd = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aibjbkdj_aibkd = v7_eom_cc3_31_trans_aibjbkdj_aibkd + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkdj_aibkd
    function v7_eom_cc3_31_trans_aiajckcl_aijkl(t2, nocc, nactive, a, i, j, k, l) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckcl_aijkl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * tvoov(a, j, l, e)
term(1) = term(1) + t2(a,e,k,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,j,k) * tvoov(a, i, l, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(a, k, l, e)
term(4) = term(4) + t2(a,e,k,i) * tvvoo(a, e, l, j)
term(5) = term(5) + t2(a,e,i,j) * tvvoo(a, e, l, k)
term(6) = term(6) + t2(a,e,j,i) * tvvoo(a, e, l, k)
term(7) = term(7) + t2(a,e,i,k) * tvvoo(a, e, l, j)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,a,k,m) * toooo(m, i, l, j)
term(9) = term(9) + t2(a,a,i,m) * toooo(m, j, l, k)
term(10) = term(10) + t2(a,a,j,m) * toooo(m, i, l, k)
term(11) = term(11) + t2(a,a,i,m) * toooo(m, k, l, j)
end do 

term(8) = term(8) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckcl_aijkl = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aiajckcl_aijkl = v7_eom_cc3_31_trans_aiajckcl_aijkl + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckcl_aijkl
    function v7_eom_cc3_31_trans_aiajckal_aijckl(t2, nocc, nactive, a, i, j, c, k, l) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckal_aijckl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,k,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(c,e,j,k) * tvoov(a, i, l, e)
term(2) = term(2) + t2(c,e,k,j) * tvoov(a, i, l, e)
term(3) = term(3) + t2(c,e,j,i) * tvoov(a, k, l, e)
term(4) = term(4) + t2(a,e,k,i) * tvoov(c, j, l, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(c, k, l, e)
term(6) = term(6) + t2(a,e,j,i) * tvoov(c, k, l, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(c, j, l, e)
term(8) = term(8) + t2(c,e,k,i) * tvvoo(a, e, l, j)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(c, e, l, j)
term(10) = term(10) + t2(c,e,j,k) * tvvoo(a, e, l, i)
term(11) = term(11) + t2(c,e,k,j) * tvvoo(a, e, l, i)
term(12) = term(12) + t2(a,e,k,j) * tvvoo(c, e, l, i)
term(13) = term(13) + t2(a,e,j,k) * tvvoo(c, e, l, i)
term(14) = term(14) + t2(c,e,j,i) * tvvoo(a, e, l, k)
term(15) = term(15) + t2(a,e,i,j) * tvvoo(c, e, l, k)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 
term(13) = term(13) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(a,c,m,k) * toooo(m, i, l, j)
term(17) = term(17) + t2(a,c,i,m) * toooo(m, k, l, j)
term(18) = term(18) + t2(a,c,m,j) * toooo(m, k, l, i)
term(19) = term(19) + t2(a,c,m,k) * toooo(m, j, l, i)
term(20) = term(20) + t2(a,c,k,m) * toooo(m, j, l, i)
term(21) = term(21) + t2(a,c,j,m) * toooo(m, k, l, i)
term(22) = term(22) + t2(a,c,m,j) * toooo(m, i, l, k)
term(23) = term(23) + t2(a,c,i,m) * toooo(m, j, l, k)
end do 

term(18) = term(18) * (-1.0000000000000002d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(22) = term(22) * (-1.0000000000000002d+0) 
term(23) = term(23) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckal_aijckl = 0.d+0
    do s = 0, 23
    v7_eom_cc3_31_trans_aiajckal_aijckl = v7_eom_cc3_31_trans_aiajckal_aijckl + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckal_aijckl
    function v7_eom_cc3_31_trans_aiajckdi_ajckd(t2, nocc, nactive, a, j, c, k, d) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckdi_ajckd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,k) * read_ftvvvv(a, e, a, d)
term(1) = term(1) + t2(c,e,k,j) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,k,j) * read_ftvvvv(c, e, a, d)
term(3) = term(3) + t2(a,e,j,k) * read_ftvvvv(c, e, a, d)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,m,k) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,c,m,j) * tvoov(a, k, m, d)
term(6) = term(6) + t2(a,a,k,m) * tvoov(c, j, m, d)
term(7) = term(7) + t2(a,a,j,m) * tvoov(c, k, m, d)
term(8) = term(8) + t2(a,c,m,j) * tvvoo(a, d, m, k)
term(9) = term(9) + t2(a,c,m,k) * tvvoo(a, d, m, j)
term(10) = term(10) + t2(a,c,k,m) * tvvoo(a, d, m, j)
term(11) = term(11) + t2(a,c,j,m) * tvvoo(a, d, m, k)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckdi_ajckd = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aiajckdi_ajckd = v7_eom_cc3_31_trans_aiajckdi_ajckd + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckdi_ajckd
    function v7_eom_cc3_31_trans_aiajckdk_aijcd(t2, nocc, nactive, a, i, j, c, d) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckdk_aijcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * read_ftvvvv(a, e, a, d)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, d, a, e)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(c, d, a, e)
term(3) = term(3) + t2(a,e,i,j) * read_ftvvvv(c, e, a, d)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,i,m) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,c,m,j) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,c,j,m) * tvoov(a, i, m, d)
term(7) = term(7) + t2(a,a,i,m) * tvoov(c, j, m, d)
term(8) = term(8) + t2(a,c,m,j) * tvvoo(a, d, m, i)
term(9) = term(9) + t2(a,a,i,m) * tvvoo(c, d, m, j)
term(10) = term(10) + t2(a,a,j,m) * tvvoo(c, d, m, i)
term(11) = term(11) + t2(a,c,i,m) * tvvoo(a, d, m, j)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckdk_aijcd = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aiajckdk_aijcd = v7_eom_cc3_31_trans_aiajckdk_aijcd + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckdk_aijcd
    function v7_eom_cc3_31_trans_aiajckdj_aickd(t2, nocc, nactive, a, i, c, k, d) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckdj_aickd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,k,i) * read_ftvvvv(a, e, a, d)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(c, e, a, d)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(c, d, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(c, d, a, e)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,m,k) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,c,k,m) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,a,i,m) * tvoov(c, k, m, d)
term(7) = term(7) + t2(a,c,i,m) * tvoov(a, k, m, d)
term(8) = term(8) + t2(a,c,m,k) * tvvoo(a, d, m, i)
term(9) = term(9) + t2(a,c,i,m) * tvvoo(a, d, m, k)
term(10) = term(10) + t2(a,a,k,m) * tvvoo(c, d, m, i)
term(11) = term(11) + t2(a,a,i,m) * tvvoo(c, d, m, k)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckdj_aickd = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aiajckdj_aickd = v7_eom_cc3_31_trans_aiajckdj_aickd + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckdj_aickd
    function v7_eom_cc3_31_trans_aibjbibl_aibjl(t2, nocc, nactive, a, i, b, j, l) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbibl_aibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(a, i, l, e)
term(2) = term(2) + t2(b,e,i,j) * tvoov(a, i, l, e)
term(3) = term(3) + t2(a,e,j,i) * tvoov(b, i, l, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(b, i, l, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(b, j, l, e)
term(6) = term(6) + t2(b,e,i,j) * tvvoo(a, e, l, i)
term(7) = term(7) + t2(a,e,j,i) * tvvoo(b, e, l, i)
term(8) = term(8) + t2(b,e,j,i) * tvvoo(a, e, l, i)
term(9) = term(9) + t2(b,e,i,i) * tvvoo(a, e, l, j)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(b, e, l, i)
term(11) = term(11) + t2(a,e,i,i) * tvvoo(b, e, l, j)
end do 

term(0) = term(0) * (2.0000000000000004d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (2.0000000000000004d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,m,i) * toooo(m, j, l, i)
term(13) = term(13) + t2(a,b,j,m) * toooo(m, i, l, i)
term(14) = term(14) + t2(a,b,m,j) * toooo(m, i, l, i)
term(15) = term(15) + t2(a,b,m,i) * toooo(m, i, l, j)
term(16) = term(16) + t2(a,b,i,m) * toooo(m, j, l, i)
term(17) = term(17) + t2(a,b,i,m) * toooo(m, i, l, j)
end do 

term(12) = term(12) * (-2.0000000000000004d+0) 
term(13) = term(13) * (-2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aibjbibl_aibjl = 0.d+0
    do s = 0, 17
    v7_eom_cc3_31_trans_aibjbibl_aibjl = v7_eom_cc3_31_trans_aibjbibl_aibjl + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbibl_aibjl
    function v7_eom_cc3_31_trans_aibjbkbi_aibjk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkbi_aibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * tvoov(a, j, i, e)
term(1) = term(1) + t2(b,e,i,k) * tvoov(a, j, i, e)
term(2) = term(2) + t2(b,e,j,k) * tvoov(a, i, i, e)
term(3) = term(3) + t2(b,e,k,j) * tvoov(a, i, i, e)
term(4) = term(4) + t2(a,e,j,k) * tvoov(b, i, i, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(b, k, i, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(b, k, i, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(b, j, i, e)
term(8) = term(8) + t2(b,e,k,j) * tvvoo(a, e, i, i)
term(9) = term(9) + t2(a,e,j,k) * tvvoo(b, e, i, i)
term(10) = term(10) + t2(b,e,j,i) * tvvoo(a, e, i, k)
term(11) = term(11) + t2(b,e,k,i) * tvvoo(a, e, i, j)
term(12) = term(12) + t2(b,e,i,j) * tvvoo(a, e, i, k)
term(13) = term(13) + t2(a,e,j,i) * tvvoo(b, e, i, k)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(b, e, i, k)
term(15) = term(15) + t2(a,e,i,k) * tvvoo(b, e, i, j)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 
term(14) = term(14) * (-1.0000000000000002d+0) 
term(15) = term(15) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(a,b,m,k) * toooo(m, j, i, i)
term(17) = term(17) + t2(a,b,j,m) * toooo(m, k, i, i)
term(18) = term(18) + t2(a,b,m,j) * toooo(m, i, i, k)
term(19) = term(19) + t2(a,b,m,k) * toooo(m, i, i, j)
term(20) = term(20) + t2(a,b,m,i) * toooo(m, j, i, k)
term(21) = term(21) + t2(a,b,j,m) * toooo(m, i, i, k)
term(22) = term(22) + t2(a,b,i,m) * toooo(m, j, i, k)
term(23) = term(23) + t2(a,b,i,m) * toooo(m, k, i, j)
end do 

term(16) = term(16) * (-1.0000000000000002d+0) 
term(17) = term(17) * (-1.0000000000000002d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(21) = term(21) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkbi_aibjk = 0.d+0
    do s = 0, 23
    v7_eom_cc3_31_trans_aibjbkbi_aibjk = v7_eom_cc3_31_trans_aibjbkbi_aibjk + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkbi_aibjk
    function v7_eom_cc3_31_trans_aibjbkbi_abjk(t2, nocc, nactive, a, b, j, k) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkbi_abjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,k) * read_ftvvvv(b, e, a, b)
term(1) = term(1) + t2(b,e,k,j) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(b,e,k,j) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(a,e,j,k) * read_ftvvvv(b, e, b, b)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(b,b,k,m) * tvoov(a, j, m, b)
term(5) = term(5) + t2(a,b,m,j) * tvoov(b, k, m, b)
term(6) = term(6) + t2(a,b,m,k) * tvoov(b, j, m, b)
term(7) = term(7) + t2(a,b,j,m) * tvoov(b, k, m, b)
term(8) = term(8) + t2(b,b,j,m) * tvvoo(a, b, m, k)
term(9) = term(9) + t2(b,b,k,m) * tvvoo(a, b, m, j)
term(10) = term(10) + t2(a,b,m,k) * tvvoo(b, b, m, j)
term(11) = term(11) + t2(a,b,j,m) * tvvoo(b, b, m, k)
end do 

term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkbi_abjk = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aibjbkbi_abjk = v7_eom_cc3_31_trans_aibjbkbi_abjk + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkbi_abjk
    function v7_eom_cc3_31_trans_aibjbkbk_aibjk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkbk_aibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * tvoov(a, j, k, e)
term(1) = term(1) + t2(b,e,i,k) * tvoov(a, j, k, e)
term(2) = term(2) + t2(b,e,j,k) * tvoov(a, i, k, e)
term(3) = term(3) + t2(b,e,k,j) * tvoov(a, i, k, e)
term(4) = term(4) + t2(a,e,j,k) * tvoov(b, i, k, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(b, k, k, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(b, k, k, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(b, j, k, e)
term(8) = term(8) + t2(b,e,k,j) * tvvoo(a, e, k, i)
term(9) = term(9) + t2(a,e,j,k) * tvvoo(b, e, k, i)
term(10) = term(10) + t2(b,e,j,i) * tvvoo(a, e, k, k)
term(11) = term(11) + t2(b,e,k,i) * tvvoo(a, e, k, j)
term(12) = term(12) + t2(b,e,i,j) * tvvoo(a, e, k, k)
term(13) = term(13) + t2(a,e,j,i) * tvvoo(b, e, k, k)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(b, e, k, k)
term(15) = term(15) + t2(a,e,i,k) * tvvoo(b, e, k, j)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 
term(14) = term(14) * (-1.0000000000000002d+0) 
term(15) = term(15) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(a,b,m,k) * toooo(m, j, k, i)
term(17) = term(17) + t2(a,b,j,m) * toooo(m, k, k, i)
term(18) = term(18) + t2(a,b,m,j) * toooo(m, i, k, k)
term(19) = term(19) + t2(a,b,m,k) * toooo(m, i, k, j)
term(20) = term(20) + t2(a,b,m,i) * toooo(m, j, k, k)
term(21) = term(21) + t2(a,b,j,m) * toooo(m, i, k, k)
term(22) = term(22) + t2(a,b,i,m) * toooo(m, j, k, k)
term(23) = term(23) + t2(a,b,i,m) * toooo(m, k, k, j)
end do 

term(16) = term(16) * (-1.0000000000000002d+0) 
term(17) = term(17) * (-1.0000000000000002d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(21) = term(21) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkbk_aibjk = 0.d+0
    do s = 0, 23
    v7_eom_cc3_31_trans_aibjbkbk_aibjk = v7_eom_cc3_31_trans_aibjbkbk_aibjk + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkbk_aibjk
    function v7_eom_cc3_31_trans_aibjbkbk_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkbk_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * read_ftvvvv(b, b, a, e)
term(1) = term(1) + t2(b,e,i,j) * read_ftvvvv(b, b, a, e)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(b, e, b, b)
term(3) = term(3) + t2(a,e,i,j) * read_ftvvvv(b, e, b, b)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(b,b,i,m) * tvoov(a, j, m, b)
term(5) = term(5) + t2(b,b,j,m) * tvoov(a, i, m, b)
term(6) = term(6) + t2(a,b,j,m) * tvoov(b, i, m, b)
term(7) = term(7) + t2(a,b,i,m) * tvoov(b, j, m, b)
term(8) = term(8) + t2(a,b,m,j) * tvvoo(b, b, m, i)
term(9) = term(9) + t2(a,b,m,i) * tvvoo(b, b, m, j)
term(10) = term(10) + t2(a,b,j,m) * tvvoo(b, b, m, i)
term(11) = term(11) + t2(a,b,i,m) * tvvoo(b, b, m, j)
end do 

term(5) = term(5) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkbk_aibj = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aibjbkbk_aibj = v7_eom_cc3_31_trans_aibjbkbk_aibj + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkbk_aibj
    function v7_eom_cc3_31_trans_aibjbkbj_aibjk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkbj_aibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * tvoov(a, j, j, e)
term(1) = term(1) + t2(b,e,i,k) * tvoov(a, j, j, e)
term(2) = term(2) + t2(b,e,j,k) * tvoov(a, i, j, e)
term(3) = term(3) + t2(b,e,k,j) * tvoov(a, i, j, e)
term(4) = term(4) + t2(a,e,j,k) * tvoov(b, i, j, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(b, k, j, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(b, k, j, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(b, j, j, e)
term(8) = term(8) + t2(b,e,k,j) * tvvoo(a, e, j, i)
term(9) = term(9) + t2(a,e,j,k) * tvvoo(b, e, j, i)
term(10) = term(10) + t2(b,e,j,i) * tvvoo(a, e, j, k)
term(11) = term(11) + t2(b,e,k,i) * tvvoo(a, e, j, j)
term(12) = term(12) + t2(b,e,i,j) * tvvoo(a, e, j, k)
term(13) = term(13) + t2(a,e,j,i) * tvvoo(b, e, j, k)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(b, e, j, k)
term(15) = term(15) + t2(a,e,i,k) * tvvoo(b, e, j, j)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 
term(14) = term(14) * (-1.0000000000000002d+0) 
term(15) = term(15) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(a,b,m,k) * toooo(m, j, j, i)
term(17) = term(17) + t2(a,b,j,m) * toooo(m, k, j, i)
term(18) = term(18) + t2(a,b,m,j) * toooo(m, i, j, k)
term(19) = term(19) + t2(a,b,m,k) * toooo(m, i, j, j)
term(20) = term(20) + t2(a,b,m,i) * toooo(m, j, j, k)
term(21) = term(21) + t2(a,b,j,m) * toooo(m, i, j, k)
term(22) = term(22) + t2(a,b,i,m) * toooo(m, j, j, k)
term(23) = term(23) + t2(a,b,i,m) * toooo(m, k, j, j)
end do 

term(16) = term(16) * (-1.0000000000000002d+0) 
term(17) = term(17) * (-1.0000000000000002d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(21) = term(21) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkbj_aibjk = 0.d+0
    do s = 0, 23
    v7_eom_cc3_31_trans_aibjbkbj_aibjk = v7_eom_cc3_31_trans_aibjbkbj_aibjk + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkbj_aibjk
    function v7_eom_cc3_31_trans_aibjbkbj_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkbj_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * read_ftvvvv(b, e, a, b)
term(1) = term(1) + t2(b,e,i,k) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(b,e,k,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(b, e, b, b)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(b,b,k,m) * tvoov(a, i, m, b)
term(5) = term(5) + t2(a,b,m,k) * tvoov(b, i, m, b)
term(6) = term(6) + t2(a,b,m,i) * tvoov(b, k, m, b)
term(7) = term(7) + t2(a,b,i,m) * tvoov(b, k, m, b)
term(8) = term(8) + t2(b,b,k,m) * tvvoo(a, b, m, i)
term(9) = term(9) + t2(b,b,i,m) * tvvoo(a, b, m, k)
term(10) = term(10) + t2(a,b,m,k) * tvvoo(b, b, m, i)
term(11) = term(11) + t2(a,b,i,m) * tvvoo(b, b, m, k)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkbj_aibk = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aibjbkbj_aibk = v7_eom_cc3_31_trans_aibjbkbj_aibk + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkbj_aibk
    function v7_eom_cc3_31_trans_aibjbial_ibjl(t2, nocc, nactive, i, b, j, l) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbial_ibjl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tvoov(b, i, l, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(b, i, l, e)
term(2) = term(2) + t2(b,e,i,i) * tvoov(b, j, l, e)
term(3) = term(3) + t2(b,e,i,i) * tvvoo(b, e, l, j)
term(4) = term(4) + t2(b,e,j,i) * tvvoo(b, e, l, i)
term(5) = term(5) + t2(b,e,i,j) * tvvoo(b, e, l, i)
end do 

term(0) = term(0) * (2.0000000000000004d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(b,b,i,m) * toooo(m, i, l, j)
term(7) = term(7) + t2(b,b,j,m) * toooo(m, i, l, i)
term(8) = term(8) + t2(b,b,i,m) * toooo(m, j, l, i)
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aibjbial_ibjl = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_trans_aibjbial_ibjl = v7_eom_cc3_31_trans_aibjbial_ibjl + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbial_ibjl
    function v7_eom_cc3_31_trans_aibjbkai_ibjk(t2, nocc, nactive, i, b, j, k) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkai_ibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * tvoov(b, i, i, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(b, k, i, e)
term(2) = term(2) + t2(b,e,k,i) * tvoov(b, j, i, e)
term(3) = term(3) + t2(b,e,i,j) * tvoov(b, k, i, e)
term(4) = term(4) + t2(b,e,k,i) * tvvoo(b, e, i, j)
term(5) = term(5) + t2(b,e,i,k) * tvvoo(b, e, i, j)
term(6) = term(6) + t2(b,e,j,k) * tvvoo(b, e, i, i)
term(7) = term(7) + t2(b,e,k,j) * tvvoo(b, e, i, i)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(b,b,k,m) * toooo(m, i, i, j)
term(9) = term(9) + t2(b,b,i,m) * toooo(m, k, i, j)
term(10) = term(10) + t2(b,b,j,m) * toooo(m, k, i, i)
term(11) = term(11) + t2(b,b,k,m) * toooo(m, j, i, i)
end do 

term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkai_ibjk = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aibjbkai_ibjk = v7_eom_cc3_31_trans_aibjbkai_ibjk + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkai_ibjk
    function v7_eom_cc3_31_trans_aibjbkai_abjk(t2, nocc, nactive, a, b, j, k) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkai_abjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,k) * read_ftvvvv(b, e, a, a)
term(1) = term(1) + t2(b,e,k,j) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(b,e,k,j) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(a,e,j,k) * read_ftvvvv(b, e, b, a)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(b,b,k,m) * tvoov(a, j, m, a)
term(5) = term(5) + t2(a,b,m,j) * tvoov(b, k, m, a)
term(6) = term(6) + t2(a,b,m,k) * tvoov(b, j, m, a)
term(7) = term(7) + t2(a,b,j,m) * tvoov(b, k, m, a)
term(8) = term(8) + t2(b,b,j,m) * tvvoo(a, a, m, k)
term(9) = term(9) + t2(b,b,k,m) * tvvoo(a, a, m, j)
term(10) = term(10) + t2(a,b,m,k) * tvvoo(b, a, m, j)
term(11) = term(11) + t2(a,b,j,m) * tvvoo(b, a, m, k)
end do 

term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkai_abjk = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aibjbkai_abjk = v7_eom_cc3_31_trans_aibjbkai_abjk + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkai_abjk
    function v7_eom_cc3_31_trans_aibjbkak_ibjk(t2, nocc, nactive, i, b, j, k) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkak_ibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * tvoov(b, i, k, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(b, k, k, e)
term(2) = term(2) + t2(b,e,k,i) * tvoov(b, j, k, e)
term(3) = term(3) + t2(b,e,i,j) * tvoov(b, k, k, e)
term(4) = term(4) + t2(b,e,k,i) * tvvoo(b, e, k, j)
term(5) = term(5) + t2(b,e,i,k) * tvvoo(b, e, k, j)
term(6) = term(6) + t2(b,e,j,k) * tvvoo(b, e, k, i)
term(7) = term(7) + t2(b,e,k,j) * tvvoo(b, e, k, i)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(b,b,k,m) * toooo(m, i, k, j)
term(9) = term(9) + t2(b,b,i,m) * toooo(m, k, k, j)
term(10) = term(10) + t2(b,b,j,m) * toooo(m, k, k, i)
term(11) = term(11) + t2(b,b,k,m) * toooo(m, j, k, i)
end do 

term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkak_ibjk = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aibjbkak_ibjk = v7_eom_cc3_31_trans_aibjbkak_ibjk + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkak_ibjk
    function v7_eom_cc3_31_trans_aibjbkak_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkak_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * read_ftvvvv(b, a, a, e)
term(1) = term(1) + t2(b,e,i,j) * read_ftvvvv(b, a, a, e)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(b, e, b, a)
term(3) = term(3) + t2(a,e,i,j) * read_ftvvvv(b, e, b, a)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(b,b,i,m) * tvoov(a, j, m, a)
term(5) = term(5) + t2(b,b,j,m) * tvoov(a, i, m, a)
term(6) = term(6) + t2(a,b,j,m) * tvoov(b, i, m, a)
term(7) = term(7) + t2(a,b,i,m) * tvoov(b, j, m, a)
term(8) = term(8) + t2(a,b,m,j) * tvvoo(b, a, m, i)
term(9) = term(9) + t2(a,b,m,i) * tvvoo(b, a, m, j)
term(10) = term(10) + t2(a,b,j,m) * tvvoo(b, a, m, i)
term(11) = term(11) + t2(a,b,i,m) * tvvoo(b, a, m, j)
end do 

term(5) = term(5) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkak_aibj = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aibjbkak_aibj = v7_eom_cc3_31_trans_aibjbkak_aibj + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkak_aibj
    function v7_eom_cc3_31_trans_aibjbkaj_ibjk(t2, nocc, nactive, i, b, j, k) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkaj_ibjk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * tvoov(b, i, j, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(b, k, j, e)
term(2) = term(2) + t2(b,e,k,i) * tvoov(b, j, j, e)
term(3) = term(3) + t2(b,e,i,j) * tvoov(b, k, j, e)
term(4) = term(4) + t2(b,e,k,i) * tvvoo(b, e, j, j)
term(5) = term(5) + t2(b,e,i,k) * tvvoo(b, e, j, j)
term(6) = term(6) + t2(b,e,j,k) * tvvoo(b, e, j, i)
term(7) = term(7) + t2(b,e,k,j) * tvvoo(b, e, j, i)
end do 

term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(b,b,k,m) * toooo(m, i, j, j)
term(9) = term(9) + t2(b,b,i,m) * toooo(m, k, j, j)
term(10) = term(10) + t2(b,b,j,m) * toooo(m, k, j, i)
term(11) = term(11) + t2(b,b,k,m) * toooo(m, j, j, i)
end do 

term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkaj_ibjk = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aibjbkaj_ibjk = v7_eom_cc3_31_trans_aibjbkaj_ibjk + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkaj_ibjk
    function v7_eom_cc3_31_trans_aibjbkaj_aibk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbkaj_aibk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * read_ftvvvv(b, e, a, a)
term(1) = term(1) + t2(b,e,i,k) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(b,e,k,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(b, e, b, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(b,b,k,m) * tvoov(a, i, m, a)
term(5) = term(5) + t2(a,b,m,k) * tvoov(b, i, m, a)
term(6) = term(6) + t2(a,b,m,i) * tvoov(b, k, m, a)
term(7) = term(7) + t2(a,b,i,m) * tvoov(b, k, m, a)
term(8) = term(8) + t2(b,b,k,m) * tvvoo(a, a, m, i)
term(9) = term(9) + t2(b,b,i,m) * tvvoo(a, a, m, k)
term(10) = term(10) + t2(a,b,m,k) * tvvoo(b, a, m, i)
term(11) = term(11) + t2(a,b,i,m) * tvvoo(b, a, m, k)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbkaj_aibk = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aibjbkaj_aibk = v7_eom_cc3_31_trans_aibjbkaj_aibk + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbkaj_aibk
    function v7_eom_cc3_31_trans_aibjbidi_aibjd(t2, nocc, nactive, a, i, b, j, d) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbidi_aibjd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * read_ftvvvv(b, e, a, d)
term(1) = term(1) + t2(b,e,i,j) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(b,e,i,j) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(b, e, b, d)
term(4) = term(4) + t2(b,e,j,i) * read_ftvvvv(b, d, a, e)
term(5) = term(5) + t2(a,e,i,j) * read_ftvvvv(b, e, b, d)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(b,b,i,m) * tvoov(a, j, m, d)
term(7) = term(7) + t2(b,b,j,m) * tvoov(a, i, m, d)
term(8) = term(8) + t2(a,b,j,m) * tvoov(b, i, m, d)
term(9) = term(9) + t2(a,b,m,j) * tvoov(b, i, m, d)
term(10) = term(10) + t2(a,b,m,i) * tvoov(b, j, m, d)
term(11) = term(11) + t2(a,b,i,m) * tvoov(b, j, m, d)
term(12) = term(12) + t2(b,b,j,m) * tvvoo(a, d, m, i)
term(13) = term(13) + t2(b,b,i,m) * tvvoo(a, d, m, j)
term(14) = term(14) + t2(a,b,m,i) * tvvoo(b, d, m, j)
term(15) = term(15) + t2(a,b,j,m) * tvvoo(b, d, m, i)
term(16) = term(16) + t2(a,b,m,j) * tvvoo(b, d, m, i)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(b, d, m, j)
end do 

term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 
term(12) = term(12) * (-1.0000000000000002d+0) 
term(13) = term(13) * (-1.0000000000000002d+0) 
term(14) = term(14) * (2.0000000000000004d+0) 
term(15) = term(15) * (2.0000000000000004d+0) 
term(16) = term(16) * (-1.0000000000000002d+0) 
term(17) = term(17) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbidi_aibjd = 0.d+0
    do s = 0, 17
    v7_eom_cc3_31_trans_aibjbidi_aibjd = v7_eom_cc3_31_trans_aibjbidi_aibjd + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbidi_aibjd
    function v7_eom_cc3_31_trans_aibjbidj_aibd(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbidj_aibd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * read_ftvvvv(b, e, a, d)
term(1) = term(1) + t2(b,e,i,i) * read_ftvvvv(b, d, a, e)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(b, e, b, d)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(b,b,i,m) * tvoov(a, i, m, d)
term(4) = term(4) + t2(a,b,m,i) * tvoov(b, i, m, d)
term(5) = term(5) + t2(a,b,i,m) * tvoov(b, i, m, d)
term(6) = term(6) + t2(b,b,i,m) * tvvoo(a, d, m, i)
term(7) = term(7) + t2(a,b,m,i) * tvvoo(b, d, m, i)
term(8) = term(8) + t2(a,b,i,m) * tvvoo(b, d, m, i)
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbidj_aibd = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_trans_aibjbidj_aibd = v7_eom_cc3_31_trans_aibjbidj_aibd + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbidj_aibd
    function v7_eom_cc3_31_trans_aiajcicl_aijl(t2, nocc, nactive, a, i, j, l) 
    real(F64) :: v7_eom_cc3_31_trans_aiajcicl_aijl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(a,e,i,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,j,i) * tvoov(a, i, l, e)
term(3) = term(3) + t2(a,e,i,i) * tvvoo(a, e, l, j)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(a, e, l, i)
term(5) = term(5) + t2(a,e,j,i) * tvvoo(a, e, l, i)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (2.0000000000000004d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,a,i,m) * toooo(m, i, l, j)
term(7) = term(7) + t2(a,a,i,m) * toooo(m, j, l, i)
term(8) = term(8) + t2(a,a,j,m) * toooo(m, i, l, i)
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aiajcicl_aijl = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_trans_aiajcicl_aijl = v7_eom_cc3_31_trans_aiajcicl_aijl + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajcicl_aijl
    function v7_eom_cc3_31_trans_aiajckci_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckci_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * tvoov(a, j, i, e)
term(1) = term(1) + t2(a,e,k,j) * tvoov(a, i, i, e)
term(2) = term(2) + t2(a,e,j,k) * tvoov(a, i, i, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(a, k, i, e)
term(4) = term(4) + t2(a,e,k,i) * tvvoo(a, e, i, j)
term(5) = term(5) + t2(a,e,i,j) * tvvoo(a, e, i, k)
term(6) = term(6) + t2(a,e,j,i) * tvvoo(a, e, i, k)
term(7) = term(7) + t2(a,e,i,k) * tvvoo(a, e, i, j)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,a,k,m) * toooo(m, i, i, j)
term(9) = term(9) + t2(a,a,i,m) * toooo(m, j, i, k)
term(10) = term(10) + t2(a,a,j,m) * toooo(m, i, i, k)
term(11) = term(11) + t2(a,a,i,m) * toooo(m, k, i, j)
end do 

term(8) = term(8) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckci_aijk = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aiajckci_aijk = v7_eom_cc3_31_trans_aiajckci_aijk + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckci_aijk
    function v7_eom_cc3_31_trans_aiajckci_ajck(t2, nocc, nactive, a, j, c, k) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckci_ajck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,k) * read_ftvvvv(a, e, a, c)
term(1) = term(1) + t2(c,e,k,j) * read_ftvvvv(a, e, a, c)
term(2) = term(2) + t2(a,e,k,j) * read_ftvvvv(c, e, a, c)
term(3) = term(3) + t2(a,e,j,k) * read_ftvvvv(c, e, a, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,m,k) * tvoov(a, j, m, c)
term(5) = term(5) + t2(a,c,m,j) * tvoov(a, k, m, c)
term(6) = term(6) + t2(a,a,k,m) * tvoov(c, j, m, c)
term(7) = term(7) + t2(a,a,j,m) * tvoov(c, k, m, c)
term(8) = term(8) + t2(a,c,m,j) * tvvoo(a, c, m, k)
term(9) = term(9) + t2(a,c,m,k) * tvvoo(a, c, m, j)
term(10) = term(10) + t2(a,c,k,m) * tvvoo(a, c, m, j)
term(11) = term(11) + t2(a,c,j,m) * tvvoo(a, c, m, k)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckci_ajck = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aiajckci_ajck = v7_eom_cc3_31_trans_aiajckci_ajck + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckci_ajck
    function v7_eom_cc3_31_trans_aiajckck_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckck_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * tvoov(a, j, k, e)
term(1) = term(1) + t2(a,e,k,j) * tvoov(a, i, k, e)
term(2) = term(2) + t2(a,e,j,k) * tvoov(a, i, k, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(a, k, k, e)
term(4) = term(4) + t2(a,e,k,i) * tvvoo(a, e, k, j)
term(5) = term(5) + t2(a,e,i,j) * tvvoo(a, e, k, k)
term(6) = term(6) + t2(a,e,j,i) * tvvoo(a, e, k, k)
term(7) = term(7) + t2(a,e,i,k) * tvvoo(a, e, k, j)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,a,k,m) * toooo(m, i, k, j)
term(9) = term(9) + t2(a,a,i,m) * toooo(m, j, k, k)
term(10) = term(10) + t2(a,a,j,m) * toooo(m, i, k, k)
term(11) = term(11) + t2(a,a,i,m) * toooo(m, k, k, j)
end do 

term(8) = term(8) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckck_aijk = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aiajckck_aijk = v7_eom_cc3_31_trans_aiajckck_aijk + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckck_aijk
    function v7_eom_cc3_31_trans_aiajckck_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckck_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * read_ftvvvv(a, e, a, c)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, c, a, e)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(c, c, a, e)
term(3) = term(3) + t2(a,e,i,j) * read_ftvvvv(c, e, a, c)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,i,m) * tvoov(a, j, m, c)
term(5) = term(5) + t2(a,c,m,j) * tvoov(a, i, m, c)
term(6) = term(6) + t2(a,c,j,m) * tvoov(a, i, m, c)
term(7) = term(7) + t2(a,a,i,m) * tvoov(c, j, m, c)
term(8) = term(8) + t2(a,c,m,j) * tvvoo(a, c, m, i)
term(9) = term(9) + t2(a,a,i,m) * tvvoo(c, c, m, j)
term(10) = term(10) + t2(a,a,j,m) * tvvoo(c, c, m, i)
term(11) = term(11) + t2(a,c,i,m) * tvvoo(a, c, m, j)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckck_aijc = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aiajckck_aijc = v7_eom_cc3_31_trans_aiajckck_aijc + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckck_aijc
    function v7_eom_cc3_31_trans_aiajckcj_aijk(t2, nocc, nactive, a, i, j, k) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckcj_aijk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * tvoov(a, j, j, e)
term(1) = term(1) + t2(a,e,k,j) * tvoov(a, i, j, e)
term(2) = term(2) + t2(a,e,j,k) * tvoov(a, i, j, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(a, k, j, e)
term(4) = term(4) + t2(a,e,k,i) * tvvoo(a, e, j, j)
term(5) = term(5) + t2(a,e,i,j) * tvvoo(a, e, j, k)
term(6) = term(6) + t2(a,e,j,i) * tvvoo(a, e, j, k)
term(7) = term(7) + t2(a,e,i,k) * tvvoo(a, e, j, j)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,a,k,m) * toooo(m, i, j, j)
term(9) = term(9) + t2(a,a,i,m) * toooo(m, j, j, k)
term(10) = term(10) + t2(a,a,j,m) * toooo(m, i, j, k)
term(11) = term(11) + t2(a,a,i,m) * toooo(m, k, j, j)
end do 

term(8) = term(8) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckcj_aijk = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aiajckcj_aijk = v7_eom_cc3_31_trans_aiajckcj_aijk + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckcj_aijk
    function v7_eom_cc3_31_trans_aiajckcj_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckcj_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,k,i) * read_ftvvvv(a, e, a, c)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(c, e, a, c)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(c, c, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(c, c, a, e)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,m,k) * tvoov(a, i, m, c)
term(5) = term(5) + t2(a,c,k,m) * tvoov(a, i, m, c)
term(6) = term(6) + t2(a,a,i,m) * tvoov(c, k, m, c)
term(7) = term(7) + t2(a,c,i,m) * tvoov(a, k, m, c)
term(8) = term(8) + t2(a,c,m,k) * tvvoo(a, c, m, i)
term(9) = term(9) + t2(a,c,i,m) * tvvoo(a, c, m, k)
term(10) = term(10) + t2(a,a,k,m) * tvvoo(c, c, m, i)
term(11) = term(11) + t2(a,a,i,m) * tvvoo(c, c, m, k)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckcj_aick = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aiajckcj_aick = v7_eom_cc3_31_trans_aiajckcj_aick + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckcj_aick
    function v7_eom_cc3_31_trans_aiajcial_aijcl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: v7_eom_cc3_31_trans_aiajcial_aijcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(c,e,j,i) * tvoov(a, i, l, e)
term(2) = term(2) + t2(c,e,i,j) * tvoov(a, i, l, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(c, j, l, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(c, i, l, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(c, i, l, e)
term(6) = term(6) + t2(c,e,i,i) * tvvoo(a, e, l, j)
term(7) = term(7) + t2(a,e,i,i) * tvvoo(c, e, l, j)
term(8) = term(8) + t2(c,e,j,i) * tvvoo(a, e, l, i)
term(9) = term(9) + t2(c,e,i,j) * tvvoo(a, e, l, i)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(c, e, l, i)
term(11) = term(11) + t2(a,e,j,i) * tvvoo(c, e, l, i)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (2.0000000000000004d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(10) = term(10) * (2.0000000000000004d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,m,i) * toooo(m, i, l, j)
term(13) = term(13) + t2(a,c,i,m) * toooo(m, i, l, j)
term(14) = term(14) + t2(a,c,m,j) * toooo(m, i, l, i)
term(15) = term(15) + t2(a,c,m,i) * toooo(m, j, l, i)
term(16) = term(16) + t2(a,c,i,m) * toooo(m, j, l, i)
term(17) = term(17) + t2(a,c,j,m) * toooo(m, i, l, i)
end do 

term(14) = term(14) * (-2.0000000000000004d+0) 
term(16) = term(16) * (-2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aiajcial_aijcl = 0.d+0
    do s = 0, 17
    v7_eom_cc3_31_trans_aiajcial_aijcl = v7_eom_cc3_31_trans_aiajcial_aijcl + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajcial_aijcl
    function v7_eom_cc3_31_trans_aiajckai_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckai_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,k,i) * tvoov(a, j, i, e)
term(1) = term(1) + t2(c,e,j,k) * tvoov(a, i, i, e)
term(2) = term(2) + t2(c,e,k,j) * tvoov(a, i, i, e)
term(3) = term(3) + t2(c,e,j,i) * tvoov(a, k, i, e)
term(4) = term(4) + t2(a,e,k,i) * tvoov(c, j, i, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(c, k, i, e)
term(6) = term(6) + t2(a,e,j,i) * tvoov(c, k, i, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(c, j, i, e)
term(8) = term(8) + t2(c,e,k,i) * tvvoo(a, e, i, j)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(c, e, i, j)
term(10) = term(10) + t2(c,e,j,k) * tvvoo(a, e, i, i)
term(11) = term(11) + t2(c,e,k,j) * tvvoo(a, e, i, i)
term(12) = term(12) + t2(a,e,k,j) * tvvoo(c, e, i, i)
term(13) = term(13) + t2(a,e,j,k) * tvvoo(c, e, i, i)
term(14) = term(14) + t2(c,e,j,i) * tvvoo(a, e, i, k)
term(15) = term(15) + t2(a,e,i,j) * tvvoo(c, e, i, k)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 
term(13) = term(13) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(a,c,m,k) * toooo(m, i, i, j)
term(17) = term(17) + t2(a,c,i,m) * toooo(m, k, i, j)
term(18) = term(18) + t2(a,c,m,j) * toooo(m, k, i, i)
term(19) = term(19) + t2(a,c,m,k) * toooo(m, j, i, i)
term(20) = term(20) + t2(a,c,k,m) * toooo(m, j, i, i)
term(21) = term(21) + t2(a,c,j,m) * toooo(m, k, i, i)
term(22) = term(22) + t2(a,c,m,j) * toooo(m, i, i, k)
term(23) = term(23) + t2(a,c,i,m) * toooo(m, j, i, k)
end do 

term(18) = term(18) * (-1.0000000000000002d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(22) = term(22) * (-1.0000000000000002d+0) 
term(23) = term(23) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckai_aijck = 0.d+0
    do s = 0, 23
    v7_eom_cc3_31_trans_aiajckai_aijck = v7_eom_cc3_31_trans_aiajckai_aijck + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckai_aijck
    function v7_eom_cc3_31_trans_aiajckai_ajck(t2, nocc, nactive, a, j, c, k) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckai_ajck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,k) * read_ftvvvv(a, e, a, a)
term(1) = term(1) + t2(c,e,k,j) * read_ftvvvv(a, e, a, a)
term(2) = term(2) + t2(a,e,k,j) * read_ftvvvv(c, e, a, a)
term(3) = term(3) + t2(a,e,j,k) * read_ftvvvv(c, e, a, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,m,k) * tvoov(a, j, m, a)
term(5) = term(5) + t2(a,c,m,j) * tvoov(a, k, m, a)
term(6) = term(6) + t2(a,a,k,m) * tvoov(c, j, m, a)
term(7) = term(7) + t2(a,a,j,m) * tvoov(c, k, m, a)
term(8) = term(8) + t2(a,c,m,j) * tvvoo(a, a, m, k)
term(9) = term(9) + t2(a,c,m,k) * tvvoo(a, a, m, j)
term(10) = term(10) + t2(a,c,k,m) * tvvoo(a, a, m, j)
term(11) = term(11) + t2(a,c,j,m) * tvvoo(a, a, m, k)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckai_ajck = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aiajckai_ajck = v7_eom_cc3_31_trans_aiajckai_ajck + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckai_ajck
    function v7_eom_cc3_31_trans_aiajckak_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckak_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,k,i) * tvoov(a, j, k, e)
term(1) = term(1) + t2(c,e,j,k) * tvoov(a, i, k, e)
term(2) = term(2) + t2(c,e,k,j) * tvoov(a, i, k, e)
term(3) = term(3) + t2(c,e,j,i) * tvoov(a, k, k, e)
term(4) = term(4) + t2(a,e,k,i) * tvoov(c, j, k, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(c, k, k, e)
term(6) = term(6) + t2(a,e,j,i) * tvoov(c, k, k, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(c, j, k, e)
term(8) = term(8) + t2(c,e,k,i) * tvvoo(a, e, k, j)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(c, e, k, j)
term(10) = term(10) + t2(c,e,j,k) * tvvoo(a, e, k, i)
term(11) = term(11) + t2(c,e,k,j) * tvvoo(a, e, k, i)
term(12) = term(12) + t2(a,e,k,j) * tvvoo(c, e, k, i)
term(13) = term(13) + t2(a,e,j,k) * tvvoo(c, e, k, i)
term(14) = term(14) + t2(c,e,j,i) * tvvoo(a, e, k, k)
term(15) = term(15) + t2(a,e,i,j) * tvvoo(c, e, k, k)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 
term(13) = term(13) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(a,c,m,k) * toooo(m, i, k, j)
term(17) = term(17) + t2(a,c,i,m) * toooo(m, k, k, j)
term(18) = term(18) + t2(a,c,m,j) * toooo(m, k, k, i)
term(19) = term(19) + t2(a,c,m,k) * toooo(m, j, k, i)
term(20) = term(20) + t2(a,c,k,m) * toooo(m, j, k, i)
term(21) = term(21) + t2(a,c,j,m) * toooo(m, k, k, i)
term(22) = term(22) + t2(a,c,m,j) * toooo(m, i, k, k)
term(23) = term(23) + t2(a,c,i,m) * toooo(m, j, k, k)
end do 

term(18) = term(18) * (-1.0000000000000002d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(22) = term(22) * (-1.0000000000000002d+0) 
term(23) = term(23) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckak_aijck = 0.d+0
    do s = 0, 23
    v7_eom_cc3_31_trans_aiajckak_aijck = v7_eom_cc3_31_trans_aiajckak_aijck + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckak_aijck
    function v7_eom_cc3_31_trans_aiajckak_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckak_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * read_ftvvvv(a, e, a, a)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, a, a, e)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(c, a, a, e)
term(3) = term(3) + t2(a,e,i,j) * read_ftvvvv(c, e, a, a)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,i,m) * tvoov(a, j, m, a)
term(5) = term(5) + t2(a,c,m,j) * tvoov(a, i, m, a)
term(6) = term(6) + t2(a,c,j,m) * tvoov(a, i, m, a)
term(7) = term(7) + t2(a,a,i,m) * tvoov(c, j, m, a)
term(8) = term(8) + t2(a,c,m,j) * tvvoo(a, a, m, i)
term(9) = term(9) + t2(a,a,i,m) * tvvoo(c, a, m, j)
term(10) = term(10) + t2(a,a,j,m) * tvvoo(c, a, m, i)
term(11) = term(11) + t2(a,c,i,m) * tvvoo(a, a, m, j)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckak_aijc = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aiajckak_aijc = v7_eom_cc3_31_trans_aiajckak_aijc + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckak_aijc
    function v7_eom_cc3_31_trans_aiajckaj_aijck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckaj_aijck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,k,i) * tvoov(a, j, j, e)
term(1) = term(1) + t2(c,e,j,k) * tvoov(a, i, j, e)
term(2) = term(2) + t2(c,e,k,j) * tvoov(a, i, j, e)
term(3) = term(3) + t2(c,e,j,i) * tvoov(a, k, j, e)
term(4) = term(4) + t2(a,e,k,i) * tvoov(c, j, j, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(c, k, j, e)
term(6) = term(6) + t2(a,e,j,i) * tvoov(c, k, j, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(c, j, j, e)
term(8) = term(8) + t2(c,e,k,i) * tvvoo(a, e, j, j)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(c, e, j, j)
term(10) = term(10) + t2(c,e,j,k) * tvvoo(a, e, j, i)
term(11) = term(11) + t2(c,e,k,j) * tvvoo(a, e, j, i)
term(12) = term(12) + t2(a,e,k,j) * tvvoo(c, e, j, i)
term(13) = term(13) + t2(a,e,j,k) * tvvoo(c, e, j, i)
term(14) = term(14) + t2(c,e,j,i) * tvvoo(a, e, j, k)
term(15) = term(15) + t2(a,e,i,j) * tvvoo(c, e, j, k)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 
term(13) = term(13) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(a,c,m,k) * toooo(m, i, j, j)
term(17) = term(17) + t2(a,c,i,m) * toooo(m, k, j, j)
term(18) = term(18) + t2(a,c,m,j) * toooo(m, k, j, i)
term(19) = term(19) + t2(a,c,m,k) * toooo(m, j, j, i)
term(20) = term(20) + t2(a,c,k,m) * toooo(m, j, j, i)
term(21) = term(21) + t2(a,c,j,m) * toooo(m, k, j, i)
term(22) = term(22) + t2(a,c,m,j) * toooo(m, i, j, k)
term(23) = term(23) + t2(a,c,i,m) * toooo(m, j, j, k)
end do 

term(18) = term(18) * (-1.0000000000000002d+0) 
term(20) = term(20) * (-1.0000000000000002d+0) 
term(22) = term(22) * (-1.0000000000000002d+0) 
term(23) = term(23) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckaj_aijck = 0.d+0
    do s = 0, 23
    v7_eom_cc3_31_trans_aiajckaj_aijck = v7_eom_cc3_31_trans_aiajckaj_aijck + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckaj_aijck
    function v7_eom_cc3_31_trans_aiajckaj_aick(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: v7_eom_cc3_31_trans_aiajckaj_aick 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,k,i) * read_ftvvvv(a, e, a, a)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(c, e, a, a)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(c, a, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(c, a, a, e)
end do 

term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,m,k) * tvoov(a, i, m, a)
term(5) = term(5) + t2(a,c,k,m) * tvoov(a, i, m, a)
term(6) = term(6) + t2(a,a,i,m) * tvoov(c, k, m, a)
term(7) = term(7) + t2(a,c,i,m) * tvoov(a, k, m, a)
term(8) = term(8) + t2(a,c,m,k) * tvvoo(a, a, m, i)
term(9) = term(9) + t2(a,c,i,m) * tvvoo(a, a, m, k)
term(10) = term(10) + t2(a,a,k,m) * tvvoo(c, a, m, i)
term(11) = term(11) + t2(a,a,i,m) * tvvoo(c, a, m, k)
end do 

term(4) = term(4) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajckaj_aick = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_trans_aiajckaj_aick = v7_eom_cc3_31_trans_aiajckaj_aick + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajckaj_aick
    function v7_eom_cc3_31_trans_aiajcidi_aijcd(t2, nocc, nactive, a, i, j, c, d) 
    real(F64) :: v7_eom_cc3_31_trans_aiajcidi_aijcd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,e,m 
    real(F64), dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * read_ftvvvv(a, e, a, d)
term(1) = term(1) + t2(c,e,i,j) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, e, a, d)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(c, e, a, d)
term(4) = term(4) + t2(a,e,i,j) * read_ftvvvv(c, d, a, e)
term(5) = term(5) + t2(a,e,j,i) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(2) = term(2) * (-2.0000000000000004d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,c,m,i) * tvoov(a, j, m, d)
term(7) = term(7) + t2(a,c,i,m) * tvoov(a, j, m, d)
term(8) = term(8) + t2(a,c,m,j) * tvoov(a, i, m, d)
term(9) = term(9) + t2(a,c,j,m) * tvoov(a, i, m, d)
term(10) = term(10) + t2(a,a,i,m) * tvoov(c, j, m, d)
term(11) = term(11) + t2(a,a,j,m) * tvoov(c, i, m, d)
term(12) = term(12) + t2(a,c,m,j) * tvvoo(a, d, m, i)
term(13) = term(13) + t2(a,c,m,i) * tvvoo(a, d, m, j)
term(14) = term(14) + t2(a,c,i,m) * tvvoo(a, d, m, j)
term(15) = term(15) + t2(a,c,j,m) * tvvoo(a, d, m, i)
term(16) = term(16) + t2(a,a,i,m) * tvvoo(c, d, m, j)
term(17) = term(17) + t2(a,a,j,m) * tvvoo(c, d, m, i)
end do 

term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(10) = term(10) * (2.0000000000000004d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 
term(12) = term(12) * (2.0000000000000004d+0) 
term(13) = term(13) * (-1.0000000000000002d+0) 
term(14) = term(14) * (2.0000000000000004d+0) 
term(15) = term(15) * (-1.0000000000000002d+0) 
term(16) = term(16) * (-1.0000000000000002d+0) 
term(17) = term(17) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajcidi_aijcd = 0.d+0
    do s = 0, 17
    v7_eom_cc3_31_trans_aiajcidi_aijcd = v7_eom_cc3_31_trans_aiajcidi_aijcd + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajcidi_aijcd
    function v7_eom_cc3_31_trans_aiajcidj_aicd(t2, nocc, nactive, a, i, c, d) 
    real(F64) :: v7_eom_cc3_31_trans_aiajcidj_aicd 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, d 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,i) * read_ftvvvv(a, e, a, d)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(c, e, a, d)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, d, a, e)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,c,m,i) * tvoov(a, i, m, d)
term(4) = term(4) + t2(a,c,i,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,a,i,m) * tvoov(c, i, m, d)
term(6) = term(6) + t2(a,c,m,i) * tvvoo(a, d, m, i)
term(7) = term(7) + t2(a,c,i,m) * tvvoo(a, d, m, i)
term(8) = term(8) + t2(a,a,i,m) * tvvoo(c, d, m, i)
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aiajcidj_aicd = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_trans_aiajcidj_aicd = v7_eom_cc3_31_trans_aiajcidj_aicd + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajcidj_aicd
    function v7_eom_cc3_31_trans_aibjbibi_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbibi_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tvoov(a, j, i, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(a, i, i, e)
term(2) = term(2) + t2(b,e,i,j) * tvoov(a, i, i, e)
term(3) = term(3) + t2(a,e,j,i) * tvoov(b, i, i, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(b, i, i, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(b, j, i, e)
term(6) = term(6) + t2(b,e,j,i) * read_ftvvvv(b, e, a, b)
term(7) = term(7) + t2(b,e,i,j) * read_ftvvvv(b, e, a, b)
term(8) = term(8) + t2(b,e,i,j) * read_ftvvvv(b, b, a, e)
term(9) = term(9) + t2(a,e,j,i) * read_ftvvvv(b, e, b, b)
term(10) = term(10) + t2(b,e,j,i) * read_ftvvvv(b, b, a, e)
term(11) = term(11) + t2(a,e,i,j) * read_ftvvvv(b, e, b, b)
term(12) = term(12) + t2(b,e,i,j) * tvvoo(a, e, i, i)
term(13) = term(13) + t2(a,e,j,i) * tvvoo(b, e, i, i)
term(14) = term(14) + t2(b,e,j,i) * tvvoo(a, e, i, i)
term(15) = term(15) + t2(b,e,i,i) * tvvoo(a, e, i, j)
term(16) = term(16) + t2(a,e,i,j) * tvvoo(b, e, i, i)
term(17) = term(17) + t2(a,e,i,i) * tvvoo(b, e, i, j)
end do 

term(0) = term(0) * (2.0000000000000004d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-2.0000000000000004d+0) 
term(9) = term(9) * (-2.0000000000000004d+0) 
term(12) = term(12) * (2.0000000000000004d+0) 
term(13) = term(13) * (2.0000000000000004d+0) 
term(14) = term(14) * (-1.0000000000000002d+0) 
term(15) = term(15) * (-1.0000000000000002d+0) 
term(16) = term(16) * (-1.0000000000000002d+0) 
term(17) = term(17) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(18) = term(18) + t2(b,b,i,m) * tvoov(a, j, m, b)
term(19) = term(19) + t2(b,b,j,m) * tvoov(a, i, m, b)
term(20) = term(20) + t2(a,b,j,m) * tvoov(b, i, m, b)
term(21) = term(21) + t2(a,b,m,j) * tvoov(b, i, m, b)
term(22) = term(22) + t2(a,b,m,i) * tvoov(b, j, m, b)
term(23) = term(23) + t2(a,b,i,m) * tvoov(b, j, m, b)
term(24) = term(24) + t2(b,b,j,m) * tvvoo(a, b, m, i)
term(25) = term(25) + t2(b,b,i,m) * tvvoo(a, b, m, j)
term(26) = term(26) + t2(a,b,m,i) * tvvoo(b, b, m, j)
term(27) = term(27) + t2(a,b,j,m) * tvvoo(b, b, m, i)
term(28) = term(28) + t2(a,b,m,j) * tvvoo(b, b, m, i)
term(29) = term(29) + t2(a,b,i,m) * tvvoo(b, b, m, j)
term(30) = term(30) + t2(a,b,m,i) * toooo(m, j, i, i)
term(31) = term(31) + t2(a,b,j,m) * toooo(m, i, i, i)
term(32) = term(32) + t2(a,b,m,j) * toooo(m, i, i, i)
term(33) = term(33) + t2(a,b,m,i) * toooo(m, i, i, j)
term(34) = term(34) + t2(a,b,i,m) * toooo(m, j, i, i)
term(35) = term(35) + t2(a,b,i,m) * toooo(m, i, i, j)
end do 

term(18) = term(18) * (2.0000000000000004d+0) 
term(19) = term(19) * (-1.0000000000000002d+0) 
term(20) = term(20) * (2.0000000000000004d+0) 
term(21) = term(21) * (-1.0000000000000002d+0) 
term(22) = term(22) * (-1.0000000000000002d+0) 
term(23) = term(23) * (-1.0000000000000002d+0) 
term(24) = term(24) * (-1.0000000000000002d+0) 
term(25) = term(25) * (-1.0000000000000002d+0) 
term(26) = term(26) * (2.0000000000000004d+0) 
term(27) = term(27) * (2.0000000000000004d+0) 
term(28) = term(28) * (-1.0000000000000002d+0) 
term(29) = term(29) * (-1.0000000000000002d+0) 
term(30) = term(30) * (-2.0000000000000004d+0) 
term(31) = term(31) * (-2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aibjbibi_aibj = 0.d+0
    do s = 0, 35
    v7_eom_cc3_31_trans_aibjbibi_aibj = v7_eom_cc3_31_trans_aibjbibi_aibj + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbibi_aibj
    function v7_eom_cc3_31_trans_aibjbibj_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbibj_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tvoov(a, j, j, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(a, i, j, e)
term(2) = term(2) + t2(b,e,i,j) * tvoov(a, i, j, e)
term(3) = term(3) + t2(a,e,j,i) * tvoov(b, i, j, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(b, i, j, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(b, j, j, e)
term(6) = term(6) + t2(b,e,i,j) * tvvoo(a, e, j, i)
term(7) = term(7) + t2(a,e,j,i) * tvvoo(b, e, j, i)
term(8) = term(8) + t2(b,e,j,i) * tvvoo(a, e, j, i)
term(9) = term(9) + t2(b,e,i,i) * tvvoo(a, e, j, j)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(b, e, j, i)
term(11) = term(11) + t2(a,e,i,i) * tvvoo(b, e, j, j)
end do 

term(0) = term(0) * (2.0000000000000004d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (2.0000000000000004d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,m,i) * toooo(m, j, j, i)
term(13) = term(13) + t2(a,b,j,m) * toooo(m, i, j, i)
term(14) = term(14) + t2(a,b,m,j) * toooo(m, i, j, i)
term(15) = term(15) + t2(a,b,m,i) * toooo(m, i, j, j)
term(16) = term(16) + t2(a,b,i,m) * toooo(m, j, j, i)
term(17) = term(17) + t2(a,b,i,m) * toooo(m, i, j, j)
end do 

term(12) = term(12) * (-2.0000000000000004d+0) 
term(13) = term(13) * (-2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aibjbibj_aibj = 0.d+0
    do s = 0, 17
    v7_eom_cc3_31_trans_aibjbibj_aibj = v7_eom_cc3_31_trans_aibjbibj_aibj + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbibj_aibj
    function v7_eom_cc3_31_trans_aibjbibj_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbibj_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * read_ftvvvv(b, e, a, b)
term(1) = term(1) + t2(b,e,i,i) * read_ftvvvv(b, b, a, e)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(b, e, b, b)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(b,b,i,m) * tvoov(a, i, m, b)
term(4) = term(4) + t2(a,b,m,i) * tvoov(b, i, m, b)
term(5) = term(5) + t2(a,b,i,m) * tvoov(b, i, m, b)
term(6) = term(6) + t2(b,b,i,m) * tvvoo(a, b, m, i)
term(7) = term(7) + t2(a,b,m,i) * tvvoo(b, b, m, i)
term(8) = term(8) + t2(a,b,i,m) * tvvoo(b, b, m, i)
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbibj_aib = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_trans_aibjbibj_aib = v7_eom_cc3_31_trans_aibjbibj_aib + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbibj_aib
    function v7_eom_cc3_31_trans_aibjbiai_ibj(t2, nocc, nactive, i, b, j) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbiai_ibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tvoov(b, i, i, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(b, i, i, e)
term(2) = term(2) + t2(b,e,i,i) * tvoov(b, j, i, e)
term(3) = term(3) + t2(b,e,i,i) * tvvoo(b, e, i, j)
term(4) = term(4) + t2(b,e,j,i) * tvvoo(b, e, i, i)
term(5) = term(5) + t2(b,e,i,j) * tvvoo(b, e, i, i)
end do 

term(0) = term(0) * (2.0000000000000004d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(b,b,i,m) * toooo(m, i, i, j)
term(7) = term(7) + t2(b,b,j,m) * toooo(m, i, i, i)
term(8) = term(8) + t2(b,b,i,m) * toooo(m, j, i, i)
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aibjbiai_ibj = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_trans_aibjbiai_ibj = v7_eom_cc3_31_trans_aibjbiai_ibj + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbiai_ibj
    function v7_eom_cc3_31_trans_aibjbiai_aibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbiai_aibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * read_ftvvvv(b, e, a, a)
term(1) = term(1) + t2(b,e,i,j) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(b,e,i,j) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(b, e, b, a)
term(4) = term(4) + t2(b,e,j,i) * read_ftvvvv(b, a, a, e)
term(5) = term(5) + t2(a,e,i,j) * read_ftvvvv(b, e, b, a)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(3) = term(3) * (-2.0000000000000004d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(b,b,i,m) * tvoov(a, j, m, a)
term(7) = term(7) + t2(b,b,j,m) * tvoov(a, i, m, a)
term(8) = term(8) + t2(a,b,j,m) * tvoov(b, i, m, a)
term(9) = term(9) + t2(a,b,m,j) * tvoov(b, i, m, a)
term(10) = term(10) + t2(a,b,m,i) * tvoov(b, j, m, a)
term(11) = term(11) + t2(a,b,i,m) * tvoov(b, j, m, a)
term(12) = term(12) + t2(b,b,j,m) * tvvoo(a, a, m, i)
term(13) = term(13) + t2(b,b,i,m) * tvvoo(a, a, m, j)
term(14) = term(14) + t2(a,b,m,i) * tvvoo(b, a, m, j)
term(15) = term(15) + t2(a,b,j,m) * tvvoo(b, a, m, i)
term(16) = term(16) + t2(a,b,m,j) * tvvoo(b, a, m, i)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(b, a, m, j)
end do 

term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(10) = term(10) * (-1.0000000000000002d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 
term(12) = term(12) * (-1.0000000000000002d+0) 
term(13) = term(13) * (-1.0000000000000002d+0) 
term(14) = term(14) * (2.0000000000000004d+0) 
term(15) = term(15) * (2.0000000000000004d+0) 
term(16) = term(16) * (-1.0000000000000002d+0) 
term(17) = term(17) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbiai_aibj = 0.d+0
    do s = 0, 17
    v7_eom_cc3_31_trans_aibjbiai_aibj = v7_eom_cc3_31_trans_aibjbiai_aibj + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbiai_aibj
    function v7_eom_cc3_31_trans_aibjbiaj_ibj(t2, nocc, nactive, i, b, j) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbiaj_ibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tvoov(b, i, j, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(b, i, j, e)
term(2) = term(2) + t2(b,e,i,i) * tvoov(b, j, j, e)
term(3) = term(3) + t2(b,e,i,i) * tvvoo(b, e, j, j)
term(4) = term(4) + t2(b,e,j,i) * tvvoo(b, e, j, i)
term(5) = term(5) + t2(b,e,i,j) * tvvoo(b, e, j, i)
end do 

term(0) = term(0) * (2.0000000000000004d+0) 
term(1) = term(1) * (-1.0000000000000002d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(b,b,i,m) * toooo(m, i, j, j)
term(7) = term(7) + t2(b,b,j,m) * toooo(m, i, j, i)
term(8) = term(8) + t2(b,b,i,m) * toooo(m, j, j, i)
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aibjbiaj_ibj = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_trans_aibjbiaj_ibj = v7_eom_cc3_31_trans_aibjbiaj_ibj + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbiaj_ibj
    function v7_eom_cc3_31_trans_aibjbiaj_aib(t2, nocc, nactive, a, i, b) 
    real(F64) :: v7_eom_cc3_31_trans_aibjbiaj_aib 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * read_ftvvvv(b, e, a, a)
term(1) = term(1) + t2(b,e,i,i) * read_ftvvvv(b, a, a, e)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(b, e, b, a)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(b,b,i,m) * tvoov(a, i, m, a)
term(4) = term(4) + t2(a,b,m,i) * tvoov(b, i, m, a)
term(5) = term(5) + t2(a,b,i,m) * tvoov(b, i, m, a)
term(6) = term(6) + t2(b,b,i,m) * tvvoo(a, a, m, i)
term(7) = term(7) + t2(a,b,m,i) * tvvoo(b, a, m, i)
term(8) = term(8) + t2(a,b,i,m) * tvvoo(b, a, m, i)
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (2.0000000000000004d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aibjbiaj_aib = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_trans_aibjbiaj_aib = v7_eom_cc3_31_trans_aibjbiaj_aib + term(s)
    end do

    end function v7_eom_cc3_31_trans_aibjbiaj_aib
    function v7_eom_cc3_31_trans_aiajcici_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: v7_eom_cc3_31_trans_aiajcici_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tvoov(a, j, i, e)
term(1) = term(1) + t2(a,e,i,j) * tvoov(a, i, i, e)
term(2) = term(2) + t2(a,e,j,i) * tvoov(a, i, i, e)
term(3) = term(3) + t2(a,e,i,i) * tvvoo(a, e, i, j)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(a, e, i, i)
term(5) = term(5) + t2(a,e,j,i) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (2.0000000000000004d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,a,i,m) * toooo(m, i, i, j)
term(7) = term(7) + t2(a,a,i,m) * toooo(m, j, i, i)
term(8) = term(8) + t2(a,a,j,m) * toooo(m, i, i, i)
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aiajcici_aij = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_trans_aiajcici_aij = v7_eom_cc3_31_trans_aiajcici_aij + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajcici_aij
    function v7_eom_cc3_31_trans_aiajcici_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: v7_eom_cc3_31_trans_aiajcici_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * read_ftvvvv(a, e, a, c)
term(1) = term(1) + t2(c,e,i,j) * read_ftvvvv(a, e, a, c)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, e, a, c)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(c, e, a, c)
term(4) = term(4) + t2(a,e,i,j) * read_ftvvvv(c, c, a, e)
term(5) = term(5) + t2(a,e,j,i) * read_ftvvvv(c, c, a, e)
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(2) = term(2) * (-2.0000000000000004d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,c,m,i) * tvoov(a, j, m, c)
term(7) = term(7) + t2(a,c,i,m) * tvoov(a, j, m, c)
term(8) = term(8) + t2(a,c,m,j) * tvoov(a, i, m, c)
term(9) = term(9) + t2(a,c,j,m) * tvoov(a, i, m, c)
term(10) = term(10) + t2(a,a,i,m) * tvoov(c, j, m, c)
term(11) = term(11) + t2(a,a,j,m) * tvoov(c, i, m, c)
term(12) = term(12) + t2(a,c,m,j) * tvvoo(a, c, m, i)
term(13) = term(13) + t2(a,c,m,i) * tvvoo(a, c, m, j)
term(14) = term(14) + t2(a,c,i,m) * tvvoo(a, c, m, j)
term(15) = term(15) + t2(a,c,j,m) * tvvoo(a, c, m, i)
term(16) = term(16) + t2(a,a,i,m) * tvvoo(c, c, m, j)
term(17) = term(17) + t2(a,a,j,m) * tvvoo(c, c, m, i)
end do 

term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(10) = term(10) * (2.0000000000000004d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 
term(12) = term(12) * (2.0000000000000004d+0) 
term(13) = term(13) * (-1.0000000000000002d+0) 
term(14) = term(14) * (2.0000000000000004d+0) 
term(15) = term(15) * (-1.0000000000000002d+0) 
term(16) = term(16) * (-1.0000000000000002d+0) 
term(17) = term(17) * (-1.0000000000000002d+0) 


    v7_eom_cc3_31_trans_aiajcici_aijc = 0.d+0
    do s = 0, 17
    v7_eom_cc3_31_trans_aiajcici_aijc = v7_eom_cc3_31_trans_aiajcici_aijc + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajcici_aijc
    function v7_eom_cc3_31_trans_aiajcicj_aij(t2, nocc, nactive, a, i, j) 
    real(F64) :: v7_eom_cc3_31_trans_aiajcicj_aij 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tvoov(a, j, j, e)
term(1) = term(1) + t2(a,e,i,j) * tvoov(a, i, j, e)
term(2) = term(2) + t2(a,e,j,i) * tvoov(a, i, j, e)
term(3) = term(3) + t2(a,e,i,i) * tvvoo(a, e, j, j)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(a, e, j, i)
term(5) = term(5) + t2(a,e,j,i) * tvvoo(a, e, j, i)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (2.0000000000000004d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,a,i,m) * toooo(m, i, j, j)
term(7) = term(7) + t2(a,a,i,m) * toooo(m, j, j, i)
term(8) = term(8) + t2(a,a,j,m) * toooo(m, i, j, i)
end do 

term(6) = term(6) * (-2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aiajcicj_aij = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_trans_aiajcicj_aij = v7_eom_cc3_31_trans_aiajcicj_aij + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajcicj_aij
    function v7_eom_cc3_31_trans_aiajcicj_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: v7_eom_cc3_31_trans_aiajcicj_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,i) * read_ftvvvv(a, e, a, c)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(c, e, a, c)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, c, a, e)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,c,m,i) * tvoov(a, i, m, c)
term(4) = term(4) + t2(a,c,i,m) * tvoov(a, i, m, c)
term(5) = term(5) + t2(a,a,i,m) * tvoov(c, i, m, c)
term(6) = term(6) + t2(a,c,m,i) * tvvoo(a, c, m, i)
term(7) = term(7) + t2(a,c,i,m) * tvvoo(a, c, m, i)
term(8) = term(8) + t2(a,a,i,m) * tvvoo(c, c, m, i)
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aiajcicj_aic = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_trans_aiajcicj_aic = v7_eom_cc3_31_trans_aiajcicj_aic + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajcicj_aic
    function v7_eom_cc3_31_trans_aiajciai_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: v7_eom_cc3_31_trans_aiajciai_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,i) * tvoov(a, j, i, e)
term(1) = term(1) + t2(c,e,j,i) * tvoov(a, i, i, e)
term(2) = term(2) + t2(c,e,i,j) * tvoov(a, i, i, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(c, j, i, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(c, i, i, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(c, i, i, e)
term(6) = term(6) + t2(c,e,j,i) * read_ftvvvv(a, e, a, a)
term(7) = term(7) + t2(c,e,i,j) * read_ftvvvv(a, e, a, a)
term(8) = term(8) + t2(a,e,i,j) * read_ftvvvv(c, e, a, a)
term(9) = term(9) + t2(a,e,j,i) * read_ftvvvv(c, e, a, a)
term(10) = term(10) + t2(a,e,i,j) * read_ftvvvv(c, a, a, e)
term(11) = term(11) + t2(a,e,j,i) * read_ftvvvv(c, a, a, e)
term(12) = term(12) + t2(c,e,i,i) * tvvoo(a, e, i, j)
term(13) = term(13) + t2(a,e,i,i) * tvvoo(c, e, i, j)
term(14) = term(14) + t2(c,e,j,i) * tvvoo(a, e, i, i)
term(15) = term(15) + t2(c,e,i,j) * tvvoo(a, e, i, i)
term(16) = term(16) + t2(a,e,i,j) * tvvoo(c, e, i, i)
term(17) = term(17) + t2(a,e,j,i) * tvvoo(c, e, i, i)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (2.0000000000000004d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-2.0000000000000004d+0) 
term(8) = term(8) * (-2.0000000000000004d+0) 
term(12) = term(12) * (-1.0000000000000002d+0) 
term(13) = term(13) * (-1.0000000000000002d+0) 
term(14) = term(14) * (2.0000000000000004d+0) 
term(15) = term(15) * (-1.0000000000000002d+0) 
term(16) = term(16) * (2.0000000000000004d+0) 
term(17) = term(17) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(18) = term(18) + t2(a,c,m,i) * tvoov(a, j, m, a)
term(19) = term(19) + t2(a,c,i,m) * tvoov(a, j, m, a)
term(20) = term(20) + t2(a,c,m,j) * tvoov(a, i, m, a)
term(21) = term(21) + t2(a,c,j,m) * tvoov(a, i, m, a)
term(22) = term(22) + t2(a,a,i,m) * tvoov(c, j, m, a)
term(23) = term(23) + t2(a,a,j,m) * tvoov(c, i, m, a)
term(24) = term(24) + t2(a,c,m,j) * tvvoo(a, a, m, i)
term(25) = term(25) + t2(a,c,m,i) * tvvoo(a, a, m, j)
term(26) = term(26) + t2(a,c,i,m) * tvvoo(a, a, m, j)
term(27) = term(27) + t2(a,c,j,m) * tvvoo(a, a, m, i)
term(28) = term(28) + t2(a,a,i,m) * tvvoo(c, a, m, j)
term(29) = term(29) + t2(a,a,j,m) * tvvoo(c, a, m, i)
term(30) = term(30) + t2(a,c,m,i) * toooo(m, i, i, j)
term(31) = term(31) + t2(a,c,i,m) * toooo(m, i, i, j)
term(32) = term(32) + t2(a,c,m,j) * toooo(m, i, i, i)
term(33) = term(33) + t2(a,c,m,i) * toooo(m, j, i, i)
term(34) = term(34) + t2(a,c,i,m) * toooo(m, j, i, i)
term(35) = term(35) + t2(a,c,j,m) * toooo(m, i, i, i)
end do 

term(18) = term(18) * (-1.0000000000000002d+0) 
term(19) = term(19) * (-1.0000000000000002d+0) 
term(20) = term(20) * (2.0000000000000004d+0) 
term(21) = term(21) * (-1.0000000000000002d+0) 
term(22) = term(22) * (2.0000000000000004d+0) 
term(23) = term(23) * (-1.0000000000000002d+0) 
term(24) = term(24) * (2.0000000000000004d+0) 
term(25) = term(25) * (-1.0000000000000002d+0) 
term(26) = term(26) * (2.0000000000000004d+0) 
term(27) = term(27) * (-1.0000000000000002d+0) 
term(28) = term(28) * (-1.0000000000000002d+0) 
term(29) = term(29) * (-1.0000000000000002d+0) 
term(32) = term(32) * (-2.0000000000000004d+0) 
term(34) = term(34) * (-2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aiajciai_aijc = 0.d+0
    do s = 0, 35
    v7_eom_cc3_31_trans_aiajciai_aijc = v7_eom_cc3_31_trans_aiajciai_aijc + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajciai_aijc
    function v7_eom_cc3_31_trans_aiajciaj_aijc(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: v7_eom_cc3_31_trans_aiajciaj_aijc 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,i) * tvoov(a, j, j, e)
term(1) = term(1) + t2(c,e,j,i) * tvoov(a, i, j, e)
term(2) = term(2) + t2(c,e,i,j) * tvoov(a, i, j, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(c, j, j, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(c, i, j, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(c, i, j, e)
term(6) = term(6) + t2(c,e,i,i) * tvvoo(a, e, j, j)
term(7) = term(7) + t2(a,e,i,i) * tvvoo(c, e, j, j)
term(8) = term(8) + t2(c,e,j,i) * tvvoo(a, e, j, i)
term(9) = term(9) + t2(c,e,i,j) * tvvoo(a, e, j, i)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(c, e, j, i)
term(11) = term(11) + t2(a,e,j,i) * tvvoo(c, e, j, i)
end do 

term(0) = term(0) * (-1.0000000000000002d+0) 
term(1) = term(1) * (2.0000000000000004d+0) 
term(2) = term(2) * (-1.0000000000000002d+0) 
term(3) = term(3) * (2.0000000000000004d+0) 
term(4) = term(4) * (-1.0000000000000002d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 
term(9) = term(9) * (-1.0000000000000002d+0) 
term(10) = term(10) * (2.0000000000000004d+0) 
term(11) = term(11) * (-1.0000000000000002d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,m,i) * toooo(m, i, j, j)
term(13) = term(13) + t2(a,c,i,m) * toooo(m, i, j, j)
term(14) = term(14) + t2(a,c,m,j) * toooo(m, i, j, i)
term(15) = term(15) + t2(a,c,m,i) * toooo(m, j, j, i)
term(16) = term(16) + t2(a,c,i,m) * toooo(m, j, j, i)
term(17) = term(17) + t2(a,c,j,m) * toooo(m, i, j, i)
end do 

term(14) = term(14) * (-2.0000000000000004d+0) 
term(16) = term(16) * (-2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aiajciaj_aijc = 0.d+0
    do s = 0, 17
    v7_eom_cc3_31_trans_aiajciaj_aijc = v7_eom_cc3_31_trans_aiajciaj_aijc + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajciaj_aijc
    function v7_eom_cc3_31_trans_aiajciaj_aic(t2, nocc, nactive, a, i, c) 
    real(F64) :: v7_eom_cc3_31_trans_aiajciaj_aic 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,i) * read_ftvvvv(a, e, a, a)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(c, e, a, a)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, a, a, e)
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,c,m,i) * tvoov(a, i, m, a)
term(4) = term(4) + t2(a,c,i,m) * tvoov(a, i, m, a)
term(5) = term(5) + t2(a,a,i,m) * tvoov(c, i, m, a)
term(6) = term(6) + t2(a,c,m,i) * tvvoo(a, a, m, i)
term(7) = term(7) + t2(a,c,i,m) * tvvoo(a, a, m, i)
term(8) = term(8) + t2(a,a,i,m) * tvvoo(c, a, m, i)
end do 

term(3) = term(3) * (-1.0000000000000002d+0) 
term(4) = term(4) * (2.0000000000000004d+0) 
term(5) = term(5) * (-1.0000000000000002d+0) 
term(6) = term(6) * (-1.0000000000000002d+0) 
term(7) = term(7) * (-1.0000000000000002d+0) 
term(8) = term(8) * (2.0000000000000004d+0) 


    v7_eom_cc3_31_trans_aiajciaj_aic = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_trans_aiajciaj_aic = v7_eom_cc3_31_trans_aiajciaj_aic + term(s)
    end do

    end function v7_eom_cc3_31_trans_aiajciaj_aic
    end module v7_eom_cc3_31_trans
    
