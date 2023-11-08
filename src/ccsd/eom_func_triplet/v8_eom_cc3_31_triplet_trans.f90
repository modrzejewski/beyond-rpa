module v8_eom_cc3_31_triplet_trans

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
    
    function v8_eom_cc3_31_triplet_trans_aibjaibl(t2, nocc, nactive, a, i, j, l) 
    real(F64) :: v8_eom_cc3_31_triplet_trans_aibjaibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(a,e,j,i) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(a, i, l, e)
term(3) = term(3) + t2(a,e,i,j) * tvvoo(a, e, l, i)
term(4) = term(4) + t2(a,e,j,i) * tvvoo(a, e, l, i)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(a, e, l, j)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-1.0d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,a,i,m) * toooo(m, j, l, i)
term(7) = term(7) + t2(a,a,j,m) * toooo(m, i, l, i)
term(8) = term(8) + t2(a,a,i,m) * toooo(m, i, l, j)
end do 

term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 


    v8_eom_cc3_31_triplet_trans_aibjaibl = 0.d+0
    do s = 0, 8
    v8_eom_cc3_31_triplet_trans_aibjaibl = v8_eom_cc3_31_triplet_trans_aibjaibl + term(s)
    end do

    end function v8_eom_cc3_31_triplet_trans_aibjaibl
    function v8_eom_cc3_31_triplet_trans_aibjaial(t2, nocc, nactive, a, i, b, j, l) 
    real(F64) :: v8_eom_cc3_31_triplet_trans_aibjaial 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(b,e,i,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(b, i, l, e)
term(3) = term(3) + t2(a,e,j,i) * tvoov(b, i, l, e)
term(4) = term(4) + t2(b,e,i,i) * tvvoo(a, e, l, j)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(b, e, l, j)
term(6) = term(6) + t2(b,e,i,j) * tvvoo(a, e, l, i)
term(7) = term(7) + t2(a,e,j,i) * tvvoo(b, e, l, i)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,b,m,i) * toooo(m, i, l, j)
term(9) = term(9) + t2(a,b,i,m) * toooo(m, i, l, j)
term(10) = term(10) + t2(a,b,m,i) * toooo(m, j, l, i)
term(11) = term(11) + t2(a,b,j,m) * toooo(m, i, l, i)
end do 

term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v8_eom_cc3_31_triplet_trans_aibjaial = 0.d+0
    do s = 0, 11
    v8_eom_cc3_31_triplet_trans_aibjaial = v8_eom_cc3_31_triplet_trans_aibjaial + term(s)
    end do

    end function v8_eom_cc3_31_triplet_trans_aibjaial
    function v8_eom_cc3_31_triplet_trans_aibjaidi(t2, nocc, nactive, a, i, b, j, d) 
    real(F64) :: v8_eom_cc3_31_triplet_trans_aibjaidi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * read_ftvvvv(a, e, a, d)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(b, d, a, e)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,i) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,b,i,m) * tvoov(a, j, m, d)
term(6) = term(6) + t2(a,b,j,m) * tvoov(a, i, m, d)
term(7) = term(7) + t2(a,a,j,m) * tvoov(b, i, m, d)
term(8) = term(8) + t2(a,b,m,i) * tvvoo(a, d, m, j)
term(9) = term(9) + t2(a,b,j,m) * tvvoo(a, d, m, i)
term(10) = term(10) + t2(a,a,i,m) * tvvoo(b, d, m, j)
term(11) = term(11) + t2(a,a,j,m) * tvvoo(b, d, m, i)
end do 

term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v8_eom_cc3_31_triplet_trans_aibjaidi = 0.d+0
    do s = 0, 11
    v8_eom_cc3_31_triplet_trans_aibjaidi = v8_eom_cc3_31_triplet_trans_aibjaidi + term(s)
    end do

    end function v8_eom_cc3_31_triplet_trans_aibjaidi
    function v8_eom_cc3_31_triplet_trans_aibjaidj(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: v8_eom_cc3_31_triplet_trans_aibjaidj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * read_ftvvvv(a, e, a, d)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(b, d, a, e)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,i) * tvoov(a, i, m, d)
term(4) = term(4) + t2(a,b,i,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,a,i,m) * tvoov(b, i, m, d)
term(6) = term(6) + t2(a,b,m,i) * tvvoo(a, d, m, i)
term(7) = term(7) + t2(a,b,i,m) * tvvoo(a, d, m, i)
term(8) = term(8) + t2(a,a,i,m) * tvvoo(b, d, m, i)
end do 

term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-1.0d+0) 


    v8_eom_cc3_31_triplet_trans_aibjaidj = 0.d+0
    do s = 0, 8
    v8_eom_cc3_31_triplet_trans_aibjaidj = v8_eom_cc3_31_triplet_trans_aibjaidj + term(s)
    end do

    end function v8_eom_cc3_31_triplet_trans_aibjaidj
    function v8_eom_cc3_31_triplet_trans_aibjaibi(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: v8_eom_cc3_31_triplet_trans_aibjaibi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * read_ftvvvv(a, e, a, b)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(b, b, a, e)
term(4) = term(4) + t2(a,e,i,i) * tvoov(a, j, i, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(a, i, i, e)
term(7) = term(7) + t2(a,e,i,j) * tvvoo(a, e, i, i)
term(8) = term(8) + t2(a,e,j,i) * tvvoo(a, e, i, i)
term(9) = term(9) + t2(a,e,i,i) * tvvoo(a, e, i, j)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (-1.0d+0) 

do m = 1, nocc 
term(10) = term(10) + t2(a,b,m,i) * tvoov(a, j, m, b)
term(11) = term(11) + t2(a,b,i,m) * tvoov(a, j, m, b)
term(12) = term(12) + t2(a,b,j,m) * tvoov(a, i, m, b)
term(13) = term(13) + t2(a,a,j,m) * tvoov(b, i, m, b)
term(14) = term(14) + t2(a,b,m,i) * tvvoo(a, b, m, j)
term(15) = term(15) + t2(a,b,j,m) * tvvoo(a, b, m, i)
term(16) = term(16) + t2(a,a,i,m) * tvvoo(b, b, m, j)
term(17) = term(17) + t2(a,a,j,m) * tvvoo(b, b, m, i)
term(18) = term(18) + t2(a,a,i,m) * toooo(m, j, i, i)
term(19) = term(19) + t2(a,a,j,m) * toooo(m, i, i, i)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, i, i, j)
end do 

term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (0.5d+0) 
term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (-0.5d+0) 


    v8_eom_cc3_31_triplet_trans_aibjaibi = 0.d+0
    do s = 0, 20
    v8_eom_cc3_31_triplet_trans_aibjaibi = v8_eom_cc3_31_triplet_trans_aibjaibi + term(s)
    end do

    end function v8_eom_cc3_31_triplet_trans_aibjaibi
    function v8_eom_cc3_31_triplet_trans_aibjaibj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: v8_eom_cc3_31_triplet_trans_aibjaibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * read_ftvvvv(a, e, a, b)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(a, j, j, e)
term(4) = term(4) + t2(a,e,j,i) * tvoov(a, i, j, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(a, e, j, i)
term(7) = term(7) + t2(a,e,j,i) * tvvoo(a, e, j, i)
term(8) = term(8) + t2(a,e,i,i) * tvvoo(a, e, j, j)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-1.0d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-1.0d+0) 

do m = 1, nocc 
term(9) = term(9) + t2(a,b,m,i) * tvoov(a, i, m, b)
term(10) = term(10) + t2(a,b,i,m) * tvoov(a, i, m, b)
term(11) = term(11) + t2(a,a,i,m) * tvoov(b, i, m, b)
term(12) = term(12) + t2(a,b,m,i) * tvvoo(a, b, m, i)
term(13) = term(13) + t2(a,b,i,m) * tvvoo(a, b, m, i)
term(14) = term(14) + t2(a,a,i,m) * tvvoo(b, b, m, i)
term(15) = term(15) + t2(a,a,i,m) * toooo(m, j, j, i)
term(16) = term(16) + t2(a,a,j,m) * toooo(m, i, j, i)
term(17) = term(17) + t2(a,a,i,m) * toooo(m, i, j, j)
end do 

term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (0.5d+0) 
term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (-1.0d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 


    v8_eom_cc3_31_triplet_trans_aibjaibj = 0.d+0
    do s = 0, 17
    v8_eom_cc3_31_triplet_trans_aibjaibj = v8_eom_cc3_31_triplet_trans_aibjaibj + term(s)
    end do

    end function v8_eom_cc3_31_triplet_trans_aibjaibj
    function v8_eom_cc3_31_triplet_trans_aibjaiai(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: v8_eom_cc3_31_triplet_trans_aibjaiai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tvoov(a, j, i, e)
term(1) = term(1) + t2(b,e,i,j) * tvoov(a, i, i, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(b, i, i, e)
term(3) = term(3) + t2(a,e,j,i) * tvoov(b, i, i, e)
term(4) = term(4) + t2(b,e,i,j) * read_ftvvvv(a, e, a, a)
term(5) = term(5) + t2(a,e,j,i) * read_ftvvvv(b, e, a, a)
term(6) = term(6) + t2(a,e,i,j) * read_ftvvvv(b, a, a, e)
term(7) = term(7) + t2(a,e,j,i) * read_ftvvvv(b, a, a, e)
term(8) = term(8) + t2(b,e,i,i) * tvvoo(a, e, i, j)
term(9) = term(9) + t2(a,e,i,i) * tvvoo(b, e, i, j)
term(10) = term(10) + t2(b,e,i,j) * tvvoo(a, e, i, i)
term(11) = term(11) + t2(a,e,j,i) * tvvoo(b, e, i, i)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (0.5d+0) 
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
term(12) = term(12) + t2(a,b,m,i) * tvoov(a, j, m, a)
term(13) = term(13) + t2(a,b,i,m) * tvoov(a, j, m, a)
term(14) = term(14) + t2(a,b,j,m) * tvoov(a, i, m, a)
term(15) = term(15) + t2(a,a,j,m) * tvoov(b, i, m, a)
term(16) = term(16) + t2(a,b,m,i) * tvvoo(a, a, m, j)
term(17) = term(17) + t2(a,b,j,m) * tvvoo(a, a, m, i)
term(18) = term(18) + t2(a,a,i,m) * tvvoo(b, a, m, j)
term(19) = term(19) + t2(a,a,j,m) * tvvoo(b, a, m, i)
term(20) = term(20) + t2(a,b,m,i) * toooo(m, i, i, j)
term(21) = term(21) + t2(a,b,i,m) * toooo(m, i, i, j)
term(22) = term(22) + t2(a,b,m,i) * toooo(m, j, i, i)
term(23) = term(23) + t2(a,b,j,m) * toooo(m, i, i, i)
end do 

term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (0.5d+0) 


    v8_eom_cc3_31_triplet_trans_aibjaiai = 0.d+0
    do s = 0, 23
    v8_eom_cc3_31_triplet_trans_aibjaiai = v8_eom_cc3_31_triplet_trans_aibjaiai + term(s)
    end do

    end function v8_eom_cc3_31_triplet_trans_aibjaiai
    function v8_eom_cc3_31_triplet_trans_aibjaiaj(t2, nocc, nactive, a, i, b, j) 
    real(F64) :: v8_eom_cc3_31_triplet_trans_aibjaiaj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    real(F64), dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * read_ftvvvv(a, e, a, a)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(b,e,i,i) * tvoov(a, j, j, e)
term(4) = term(4) + t2(b,e,i,j) * tvoov(a, i, j, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(b, i, j, e)
term(6) = term(6) + t2(a,e,j,i) * tvoov(b, i, j, e)
term(7) = term(7) + t2(b,e,i,i) * tvvoo(a, e, j, j)
term(8) = term(8) + t2(a,e,i,i) * tvvoo(b, e, j, j)
term(9) = term(9) + t2(b,e,i,j) * tvvoo(a, e, j, i)
term(10) = term(10) + t2(a,e,j,i) * tvvoo(b, e, j, i)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 

do m = 1, nocc 
term(11) = term(11) + t2(a,b,m,i) * tvoov(a, i, m, a)
term(12) = term(12) + t2(a,b,i,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,a,i,m) * tvoov(b, i, m, a)
term(14) = term(14) + t2(a,b,m,i) * tvvoo(a, a, m, i)
term(15) = term(15) + t2(a,b,i,m) * tvvoo(a, a, m, i)
term(16) = term(16) + t2(a,a,i,m) * tvvoo(b, a, m, i)
term(17) = term(17) + t2(a,b,m,i) * toooo(m, i, j, j)
term(18) = term(18) + t2(a,b,i,m) * toooo(m, i, j, j)
term(19) = term(19) + t2(a,b,m,i) * toooo(m, j, j, i)
term(20) = term(20) + t2(a,b,j,m) * toooo(m, i, j, i)
end do 

term(11) = term(11) * (0.5d+0) 
term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (0.5d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (0.5d+0) 


    v8_eom_cc3_31_triplet_trans_aibjaiaj = 0.d+0
    do s = 0, 20
    v8_eom_cc3_31_triplet_trans_aibjaiaj = v8_eom_cc3_31_triplet_trans_aibjaiaj + term(s)
    end do

    end function v8_eom_cc3_31_triplet_trans_aibjaiaj
end module v8_eom_cc3_31_triplet_trans
