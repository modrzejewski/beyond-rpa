module v2_eom_cc3_31_triplet_trans

    use ccsd_transformed_integrals                                                                                                                                   
    use t1_transformed_int                                                                                                                   
                                                                                                                                   
    use basis                                                                                                                           
    use arithmetic                                                                                                                           
                                                                        
    use cc3_intermediates 
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    
    implicit none
    !
    ! File generated automatically on 2017-01-20 13:57:15 UTC.
    !
    contains
    
    function v2_eom_cc3_31_triplet_trans_aibjakbl(t2, nocc, nactive, a, i, j, k, l) 
    real(F64) :: v2_eom_cc3_31_triplet_trans_aibjakbl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * tvoov(a, j, l, e)
term(1) = term(1) + t2(a,e,j,k) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(a, k, l, e)
term(3) = term(3) + t2(a,e,k,j) * tvoov(a, i, l, e)
term(4) = term(4) + t2(a,e,j,i) * tvvoo(a, e, l, k)
term(5) = term(5) + t2(a,e,i,j) * tvvoo(a, e, l, k)
term(6) = term(6) + t2(a,e,k,i) * tvvoo(a, e, l, j)
term(7) = term(7) + t2(a,e,i,k) * tvvoo(a, e, l, j)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,a,j,m) * toooo(m, i, l, k)
term(9) = term(9) + t2(a,a,i,m) * toooo(m, j, l, k)
term(10) = term(10) + t2(a,a,k,m) * toooo(m, i, l, j)
term(11) = term(11) + t2(a,a,i,m) * toooo(m, k, l, j)
end do 

term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v2_eom_cc3_31_triplet_trans_aibjakbl = 0.d+0
    do s = 0, 11
    v2_eom_cc3_31_triplet_trans_aibjakbl = v2_eom_cc3_31_triplet_trans_aibjakbl + term(s)
    end do

    end function v2_eom_cc3_31_triplet_trans_aibjakbl
    function v2_eom_cc3_31_triplet_trans_aibjakal(t2, nocc, nactive, a, i, b, j, k, l) 
    real(F64) :: v2_eom_cc3_31_triplet_trans_aibjakal 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(b,e,k,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,j,i) * tvoov(b, k, l, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(b, k, l, e)
term(4) = term(4) + t2(b,e,k,i) * tvvoo(a, e, l, j)
term(5) = term(5) + t2(a,e,i,k) * tvvoo(b, e, l, j)
term(6) = term(6) + t2(a,e,j,k) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(b,e,k,j) * tvvoo(a, e, l, i)
term(8) = term(8) + t2(b,e,j,i) * tvoov(a, k, l, e)
term(9) = term(9) + t2(b,e,j,k) * tvoov(a, i, l, e)
term(10) = term(10) + t2(a,e,k,i) * tvoov(b, j, l, e)
term(11) = term(11) + t2(a,e,i,k) * tvoov(b, j, l, e)
term(12) = term(12) + t2(a,e,i,j) * tvvoo(b, e, l, k)
term(13) = term(13) + t2(b,e,j,i) * tvvoo(a, e, l, k)
term(14) = term(14) + t2(a,e,k,j) * tvvoo(b, e, l, i)
term(15) = term(15) + t2(b,e,j,k) * tvvoo(a, e, l, i)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (0.5d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(a,b,m,k) * toooo(m, i, l, j)
term(17) = term(17) + t2(a,b,i,m) * toooo(m, k, l, j)
term(18) = term(18) + t2(a,b,j,m) * toooo(m, k, l, i)
term(19) = term(19) + t2(a,b,m,k) * toooo(m, j, l, i)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, j, l, k)
term(21) = term(21) + t2(a,b,m,j) * toooo(m, i, l, k)
term(22) = term(22) + t2(a,b,k,m) * toooo(m, j, l, i)
term(23) = term(23) + t2(a,b,m,j) * toooo(m, k, l, i)
end do 

term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (-0.5d+0) 


    v2_eom_cc3_31_triplet_trans_aibjakal = 0.d+0
    do s = 0, 23
    v2_eom_cc3_31_triplet_trans_aibjakal = v2_eom_cc3_31_triplet_trans_aibjakal + term(s)
    end do

    end function v2_eom_cc3_31_triplet_trans_aibjakal
    function v2_eom_cc3_31_triplet_trans_aibjakdk(t2, nocc, nactive, a, i, b, j, d) 
    real(F64) :: v2_eom_cc3_31_triplet_trans_aibjakdk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * read_ftvvvv(b, d, a, e)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(b, d, a, e)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(b, e, a, d)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(a, e, a, d)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,i,m) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,b,j,m) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,a,j,m) * tvvoo(b, d, m, i)
term(7) = term(7) + t2(a,a,i,m) * tvvoo(b, d, m, j)
term(8) = term(8) + t2(a,b,m,j) * tvoov(a, i, m, d)
term(9) = term(9) + t2(a,a,i,m) * tvoov(b, j, m, d)
term(10) = term(10) + t2(a,b,i,m) * tvvoo(a, d, m, j)
term(11) = term(11) + t2(a,b,m,j) * tvvoo(a, d, m, i)
end do 

term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 


    v2_eom_cc3_31_triplet_trans_aibjakdk = 0.d+0
    do s = 0, 11
    v2_eom_cc3_31_triplet_trans_aibjakdk = v2_eom_cc3_31_triplet_trans_aibjakdk + term(s)
    end do

    end function v2_eom_cc3_31_triplet_trans_aibjakdk
    function v2_eom_cc3_31_triplet_trans_aibjakdj(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: v2_eom_cc3_31_triplet_trans_aibjakdj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * read_ftvvvv(a, e, a, d)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(b, d, a, e)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,k) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,b,i,m) * tvoov(a, k, m, d)
term(6) = term(6) + t2(a,b,k,m) * tvoov(a, i, m, d)
term(7) = term(7) + t2(a,a,i,m) * tvoov(b, k, m, d)
term(8) = term(8) + t2(a,b,m,k) * tvvoo(a, d, m, i)
term(9) = term(9) + t2(a,b,i,m) * tvvoo(a, d, m, k)
term(10) = term(10) + t2(a,a,k,m) * tvvoo(b, d, m, i)
term(11) = term(11) + t2(a,a,i,m) * tvvoo(b, d, m, k)
end do 

term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 


    v2_eom_cc3_31_triplet_trans_aibjakdj = 0.d+0
    do s = 0, 11
    v2_eom_cc3_31_triplet_trans_aibjakdj = v2_eom_cc3_31_triplet_trans_aibjakdj + term(s)
    end do

    end function v2_eom_cc3_31_triplet_trans_aibjakdj
    function v2_eom_cc3_31_triplet_trans_aibjakdi(t2, nocc, nactive, a, b, j, k, d) 
    real(F64) :: v2_eom_cc3_31_triplet_trans_aibjakdi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,k) * read_ftvvvv(b, e, a, d)
term(1) = term(1) + t2(b,e,k,j) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,k,j) * read_ftvvvv(b, e, a, d)
term(3) = term(3) + t2(b,e,j,k) * read_ftvvvv(a, e, a, d)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,k) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,a,j,m) * tvoov(b, k, m, d)
term(6) = term(6) + t2(a,b,j,m) * tvvoo(a, d, m, k)
term(7) = term(7) + t2(a,b,m,k) * tvvoo(a, d, m, j)
term(8) = term(8) + t2(a,b,m,j) * tvoov(a, k, m, d)
term(9) = term(9) + t2(a,a,k,m) * tvoov(b, j, m, d)
term(10) = term(10) + t2(a,b,k,m) * tvvoo(a, d, m, j)
term(11) = term(11) + t2(a,b,m,j) * tvvoo(a, d, m, k)
end do 

term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v2_eom_cc3_31_triplet_trans_aibjakdi = 0.d+0
    do s = 0, 11
    v2_eom_cc3_31_triplet_trans_aibjakdi = v2_eom_cc3_31_triplet_trans_aibjakdi + term(s)
    end do

    end function v2_eom_cc3_31_triplet_trans_aibjakdi
    function v2_eom_cc3_31_triplet_trans_aibjakbk(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: v2_eom_cc3_31_triplet_trans_aibjakbk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * read_ftvvvv(b, b, a, e)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(b, b, a, e)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(b, e, a, b)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(a, e, a, b)
term(4) = term(4) + t2(a,e,i,k) * tvoov(a, j, k, e)
term(5) = term(5) + t2(a,e,j,k) * tvoov(a, i, k, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(a, k, k, e)
term(7) = term(7) + t2(a,e,k,j) * tvoov(a, i, k, e)
term(8) = term(8) + t2(a,e,j,i) * tvvoo(a, e, k, k)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(a, e, k, k)
term(10) = term(10) + t2(a,e,k,i) * tvvoo(a, e, k, j)
term(11) = term(11) + t2(a,e,i,k) * tvvoo(a, e, k, j)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,i,m) * tvoov(a, j, m, b)
term(13) = term(13) + t2(a,b,j,m) * tvoov(a, i, m, b)
term(14) = term(14) + t2(a,a,j,m) * tvvoo(b, b, m, i)
term(15) = term(15) + t2(a,a,i,m) * tvvoo(b, b, m, j)
term(16) = term(16) + t2(a,b,m,j) * tvoov(a, i, m, b)
term(17) = term(17) + t2(a,a,i,m) * tvoov(b, j, m, b)
term(18) = term(18) + t2(a,b,i,m) * tvvoo(a, b, m, j)
term(19) = term(19) + t2(a,b,m,j) * tvvoo(a, b, m, i)
term(20) = term(20) + t2(a,a,j,m) * toooo(m, i, k, k)
term(21) = term(21) + t2(a,a,i,m) * toooo(m, j, k, k)
term(22) = term(22) + t2(a,a,k,m) * toooo(m, i, k, j)
term(23) = term(23) + t2(a,a,i,m) * toooo(m, k, k, j)
end do 

term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (0.5d+0) 


    v2_eom_cc3_31_triplet_trans_aibjakbk = 0.d+0
    do s = 0, 23
    v2_eom_cc3_31_triplet_trans_aibjakbk = v2_eom_cc3_31_triplet_trans_aibjakbk + term(s)
    end do

    end function v2_eom_cc3_31_triplet_trans_aibjakbk
    function v2_eom_cc3_31_triplet_trans_aibjakbj(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: v2_eom_cc3_31_triplet_trans_aibjakbj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * read_ftvvvv(a, e, a, b)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(b, b, a, e)
term(4) = term(4) + t2(a,e,i,k) * tvoov(a, j, j, e)
term(5) = term(5) + t2(a,e,j,k) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(a, k, j, e)
term(7) = term(7) + t2(a,e,k,j) * tvoov(a, i, j, e)
term(8) = term(8) + t2(a,e,j,i) * tvvoo(a, e, j, k)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(a, e, j, k)
term(10) = term(10) + t2(a,e,k,i) * tvvoo(a, e, j, j)
term(11) = term(11) + t2(a,e,i,k) * tvvoo(a, e, j, j)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,m,k) * tvoov(a, i, m, b)
term(13) = term(13) + t2(a,b,i,m) * tvoov(a, k, m, b)
term(14) = term(14) + t2(a,b,k,m) * tvoov(a, i, m, b)
term(15) = term(15) + t2(a,a,i,m) * tvoov(b, k, m, b)
term(16) = term(16) + t2(a,b,m,k) * tvvoo(a, b, m, i)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(a, b, m, k)
term(18) = term(18) + t2(a,a,k,m) * tvvoo(b, b, m, i)
term(19) = term(19) + t2(a,a,i,m) * tvvoo(b, b, m, k)
term(20) = term(20) + t2(a,a,j,m) * toooo(m, i, j, k)
term(21) = term(21) + t2(a,a,i,m) * toooo(m, j, j, k)
term(22) = term(22) + t2(a,a,k,m) * toooo(m, i, j, j)
term(23) = term(23) + t2(a,a,i,m) * toooo(m, k, j, j)
end do 

term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (0.5d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (0.5d+0) 


    v2_eom_cc3_31_triplet_trans_aibjakbj = 0.d+0
    do s = 0, 23
    v2_eom_cc3_31_triplet_trans_aibjakbj = v2_eom_cc3_31_triplet_trans_aibjakbj + term(s)
    end do

    end function v2_eom_cc3_31_triplet_trans_aibjakbj
    function v2_eom_cc3_31_triplet_trans_aibjakbi(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: v2_eom_cc3_31_triplet_trans_aibjakbi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,k) * read_ftvvvv(b, e, a, b)
term(1) = term(1) + t2(b,e,k,j) * read_ftvvvv(a, e, a, b)
term(2) = term(2) + t2(a,e,k,j) * read_ftvvvv(b, e, a, b)
term(3) = term(3) + t2(b,e,j,k) * read_ftvvvv(a, e, a, b)
term(4) = term(4) + t2(a,e,i,k) * tvoov(a, j, i, e)
term(5) = term(5) + t2(a,e,j,k) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(a, k, i, e)
term(7) = term(7) + t2(a,e,k,j) * tvoov(a, i, i, e)
term(8) = term(8) + t2(a,e,j,i) * tvvoo(a, e, i, k)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(a, e, i, k)
term(10) = term(10) + t2(a,e,k,i) * tvvoo(a, e, i, j)
term(11) = term(11) + t2(a,e,i,k) * tvvoo(a, e, i, j)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,m,k) * tvoov(a, j, m, b)
term(13) = term(13) + t2(a,a,j,m) * tvoov(b, k, m, b)
term(14) = term(14) + t2(a,b,j,m) * tvvoo(a, b, m, k)
term(15) = term(15) + t2(a,b,m,k) * tvvoo(a, b, m, j)
term(16) = term(16) + t2(a,b,m,j) * tvoov(a, k, m, b)
term(17) = term(17) + t2(a,a,k,m) * tvoov(b, j, m, b)
term(18) = term(18) + t2(a,b,k,m) * tvvoo(a, b, m, j)
term(19) = term(19) + t2(a,b,m,j) * tvvoo(a, b, m, k)
term(20) = term(20) + t2(a,a,j,m) * toooo(m, i, i, k)
term(21) = term(21) + t2(a,a,i,m) * toooo(m, j, i, k)
term(22) = term(22) + t2(a,a,k,m) * toooo(m, i, i, j)
term(23) = term(23) + t2(a,a,i,m) * toooo(m, k, i, j)
end do 

term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (0.5d+0) 


    v2_eom_cc3_31_triplet_trans_aibjakbi = 0.d+0
    do s = 0, 23
    v2_eom_cc3_31_triplet_trans_aibjakbi = v2_eom_cc3_31_triplet_trans_aibjakbi + term(s)
    end do

    end function v2_eom_cc3_31_triplet_trans_aibjakbi
    function v2_eom_cc3_31_triplet_trans_aibjakak(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: v2_eom_cc3_31_triplet_trans_aibjakak 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * read_ftvvvv(b, a, a, e)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(b, a, a, e)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(b, e, a, a)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(a, e, a, a)
term(4) = term(4) + t2(b,e,k,i) * tvoov(a, j, k, e)
term(5) = term(5) + t2(b,e,k,j) * tvoov(a, i, k, e)
term(6) = term(6) + t2(a,e,j,i) * tvoov(b, k, k, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(b, k, k, e)
term(8) = term(8) + t2(b,e,k,i) * tvvoo(a, e, k, j)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(b, e, k, j)
term(10) = term(10) + t2(a,e,j,k) * tvvoo(b, e, k, i)
term(11) = term(11) + t2(b,e,k,j) * tvvoo(a, e, k, i)
term(12) = term(12) + t2(b,e,j,i) * tvoov(a, k, k, e)
term(13) = term(13) + t2(b,e,j,k) * tvoov(a, i, k, e)
term(14) = term(14) + t2(a,e,k,i) * tvoov(b, j, k, e)
term(15) = term(15) + t2(a,e,i,k) * tvoov(b, j, k, e)
term(16) = term(16) + t2(a,e,i,j) * tvvoo(b, e, k, k)
term(17) = term(17) + t2(b,e,j,i) * tvvoo(a, e, k, k)
term(18) = term(18) + t2(a,e,k,j) * tvvoo(b, e, k, i)
term(19) = term(19) + t2(b,e,j,k) * tvvoo(a, e, k, i)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 

do m = 1, nocc 
term(20) = term(20) + t2(a,b,i,m) * tvoov(a, j, m, a)
term(21) = term(21) + t2(a,b,j,m) * tvoov(a, i, m, a)
term(22) = term(22) + t2(a,a,j,m) * tvvoo(b, a, m, i)
term(23) = term(23) + t2(a,a,i,m) * tvvoo(b, a, m, j)
term(24) = term(24) + t2(a,b,m,j) * tvoov(a, i, m, a)
term(25) = term(25) + t2(a,a,i,m) * tvoov(b, j, m, a)
term(26) = term(26) + t2(a,b,i,m) * tvvoo(a, a, m, j)
term(27) = term(27) + t2(a,b,m,j) * tvvoo(a, a, m, i)
term(28) = term(28) + t2(a,b,m,k) * toooo(m, i, k, j)
term(29) = term(29) + t2(a,b,i,m) * toooo(m, k, k, j)
term(30) = term(30) + t2(a,b,j,m) * toooo(m, k, k, i)
term(31) = term(31) + t2(a,b,m,k) * toooo(m, j, k, i)
term(32) = term(32) + t2(a,b,i,m) * toooo(m, j, k, k)
term(33) = term(33) + t2(a,b,m,j) * toooo(m, i, k, k)
term(34) = term(34) + t2(a,b,k,m) * toooo(m, j, k, i)
term(35) = term(35) + t2(a,b,m,j) * toooo(m, k, k, i)
end do 

term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * (-0.5d+0) 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * (-0.5d+0) 
term(28) = term(28) * (-0.5d+0) 
term(29) = term(29) * (-0.5d+0) 
term(30) = term(30) * (0.5d+0) 
term(31) = term(31) * (0.5d+0) 
term(32) = term(32) * (0.5d+0) 
term(33) = term(33) * (0.5d+0) 
term(34) = term(34) * (-0.5d+0) 
term(35) = term(35) * (-0.5d+0) 


    v2_eom_cc3_31_triplet_trans_aibjakak = 0.d+0
    do s = 0, 35
    v2_eom_cc3_31_triplet_trans_aibjakak = v2_eom_cc3_31_triplet_trans_aibjakak + term(s)
    end do

    end function v2_eom_cc3_31_triplet_trans_aibjakak
    function v2_eom_cc3_31_triplet_trans_aibjakaj(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: v2_eom_cc3_31_triplet_trans_aibjakaj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * read_ftvvvv(a, e, a, a)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(b,e,k,i) * tvoov(a, j, j, e)
term(5) = term(5) + t2(b,e,k,j) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,j,i) * tvoov(b, k, j, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(b, k, j, e)
term(8) = term(8) + t2(b,e,k,i) * tvvoo(a, e, j, j)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(b, e, j, j)
term(10) = term(10) + t2(a,e,j,k) * tvvoo(b, e, j, i)
term(11) = term(11) + t2(b,e,k,j) * tvvoo(a, e, j, i)
term(12) = term(12) + t2(b,e,j,i) * tvoov(a, k, j, e)
term(13) = term(13) + t2(b,e,j,k) * tvoov(a, i, j, e)
term(14) = term(14) + t2(a,e,k,i) * tvoov(b, j, j, e)
term(15) = term(15) + t2(a,e,i,k) * tvoov(b, j, j, e)
term(16) = term(16) + t2(a,e,i,j) * tvvoo(b, e, j, k)
term(17) = term(17) + t2(b,e,j,i) * tvvoo(a, e, j, k)
term(18) = term(18) + t2(a,e,k,j) * tvvoo(b, e, j, i)
term(19) = term(19) + t2(b,e,j,k) * tvvoo(a, e, j, i)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 

do m = 1, nocc 
term(20) = term(20) + t2(a,b,m,k) * tvoov(a, i, m, a)
term(21) = term(21) + t2(a,b,i,m) * tvoov(a, k, m, a)
term(22) = term(22) + t2(a,b,k,m) * tvoov(a, i, m, a)
term(23) = term(23) + t2(a,a,i,m) * tvoov(b, k, m, a)
term(24) = term(24) + t2(a,b,m,k) * tvvoo(a, a, m, i)
term(25) = term(25) + t2(a,b,i,m) * tvvoo(a, a, m, k)
term(26) = term(26) + t2(a,a,k,m) * tvvoo(b, a, m, i)
term(27) = term(27) + t2(a,a,i,m) * tvvoo(b, a, m, k)
term(28) = term(28) + t2(a,b,m,k) * toooo(m, i, j, j)
term(29) = term(29) + t2(a,b,i,m) * toooo(m, k, j, j)
term(30) = term(30) + t2(a,b,j,m) * toooo(m, k, j, i)
term(31) = term(31) + t2(a,b,m,k) * toooo(m, j, j, i)
term(32) = term(32) + t2(a,b,i,m) * toooo(m, j, j, k)
term(33) = term(33) + t2(a,b,m,j) * toooo(m, i, j, k)
term(34) = term(34) + t2(a,b,k,m) * toooo(m, j, j, i)
term(35) = term(35) + t2(a,b,m,j) * toooo(m, k, j, i)
end do 

term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (0.5d+0) 
term(24) = term(24) * (0.5d+0) 
term(25) = term(25) * (0.5d+0) 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * (-0.5d+0) 
term(28) = term(28) * (-0.5d+0) 
term(29) = term(29) * (-0.5d+0) 
term(30) = term(30) * (0.5d+0) 
term(31) = term(31) * (0.5d+0) 
term(32) = term(32) * (0.5d+0) 
term(33) = term(33) * (0.5d+0) 
term(34) = term(34) * (-0.5d+0) 
term(35) = term(35) * (-0.5d+0) 


    v2_eom_cc3_31_triplet_trans_aibjakaj = 0.d+0
    do s = 0, 35
    v2_eom_cc3_31_triplet_trans_aibjakaj = v2_eom_cc3_31_triplet_trans_aibjakaj + term(s)
    end do

    end function v2_eom_cc3_31_triplet_trans_aibjakaj
    function v2_eom_cc3_31_triplet_trans_aibjakai(t2, nocc, nactive, a, i, b, j, k) 
    real(F64) :: v2_eom_cc3_31_triplet_trans_aibjakai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,k) * read_ftvvvv(b, e, a, a)
term(1) = term(1) + t2(b,e,k,j) * read_ftvvvv(a, e, a, a)
term(2) = term(2) + t2(a,e,k,j) * read_ftvvvv(b, e, a, a)
term(3) = term(3) + t2(b,e,j,k) * read_ftvvvv(a, e, a, a)
term(4) = term(4) + t2(b,e,k,i) * tvoov(a, j, i, e)
term(5) = term(5) + t2(b,e,k,j) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,j,i) * tvoov(b, k, i, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(b, k, i, e)
term(8) = term(8) + t2(b,e,k,i) * tvvoo(a, e, i, j)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(b, e, i, j)
term(10) = term(10) + t2(a,e,j,k) * tvvoo(b, e, i, i)
term(11) = term(11) + t2(b,e,k,j) * tvvoo(a, e, i, i)
term(12) = term(12) + t2(b,e,j,i) * tvoov(a, k, i, e)
term(13) = term(13) + t2(b,e,j,k) * tvoov(a, i, i, e)
term(14) = term(14) + t2(a,e,k,i) * tvoov(b, j, i, e)
term(15) = term(15) + t2(a,e,i,k) * tvoov(b, j, i, e)
term(16) = term(16) + t2(a,e,i,j) * tvvoo(b, e, i, k)
term(17) = term(17) + t2(b,e,j,i) * tvvoo(a, e, i, k)
term(18) = term(18) + t2(a,e,k,j) * tvvoo(b, e, i, i)
term(19) = term(19) + t2(b,e,j,k) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 

do m = 1, nocc 
term(20) = term(20) + t2(a,b,m,k) * tvoov(a, j, m, a)
term(21) = term(21) + t2(a,a,j,m) * tvoov(b, k, m, a)
term(22) = term(22) + t2(a,b,j,m) * tvvoo(a, a, m, k)
term(23) = term(23) + t2(a,b,m,k) * tvvoo(a, a, m, j)
term(24) = term(24) + t2(a,b,m,j) * tvoov(a, k, m, a)
term(25) = term(25) + t2(a,a,k,m) * tvoov(b, j, m, a)
term(26) = term(26) + t2(a,b,k,m) * tvvoo(a, a, m, j)
term(27) = term(27) + t2(a,b,m,j) * tvvoo(a, a, m, k)
term(28) = term(28) + t2(a,b,m,k) * toooo(m, i, i, j)
term(29) = term(29) + t2(a,b,i,m) * toooo(m, k, i, j)
term(30) = term(30) + t2(a,b,j,m) * toooo(m, k, i, i)
term(31) = term(31) + t2(a,b,m,k) * toooo(m, j, i, i)
term(32) = term(32) + t2(a,b,i,m) * toooo(m, j, i, k)
term(33) = term(33) + t2(a,b,m,j) * toooo(m, i, i, k)
term(34) = term(34) + t2(a,b,k,m) * toooo(m, j, i, i)
term(35) = term(35) + t2(a,b,m,j) * toooo(m, k, i, i)
end do 

term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (-0.5d+0) 
term(24) = term(24) * (0.5d+0) 
term(25) = term(25) * (0.5d+0) 
term(26) = term(26) * (0.5d+0) 
term(27) = term(27) * (0.5d+0) 
term(28) = term(28) * (-0.5d+0) 
term(29) = term(29) * (-0.5d+0) 
term(30) = term(30) * (0.5d+0) 
term(31) = term(31) * (0.5d+0) 
term(32) = term(32) * (0.5d+0) 
term(33) = term(33) * (0.5d+0) 
term(34) = term(34) * (-0.5d+0) 
term(35) = term(35) * (-0.5d+0) 


    v2_eom_cc3_31_triplet_trans_aibjakai = 0.d+0
    do s = 0, 35
    v2_eom_cc3_31_triplet_trans_aibjakai = v2_eom_cc3_31_triplet_trans_aibjakai + term(s)
    end do

    end function v2_eom_cc3_31_triplet_trans_aibjakai
    end module v2_eom_cc3_31_triplet_trans
    