module v4_eom_cc3_31_triplet_trans

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
    
    function v4_eom_cc3_31_triplet_trans_aibjcibl(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: v4_eom_cc3_31_triplet_trans_aibjcibl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * tvoov(a, i, l, e)
term(1) = term(1) + t2(c,e,i,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(c, i, l, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(c, j, l, e)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(c, e, l, i)
term(5) = term(5) + t2(c,e,j,i) * tvvoo(a, e, l, i)
term(6) = term(6) + t2(a,e,i,i) * tvvoo(c, e, l, j)
term(7) = term(7) + t2(c,e,i,i) * tvvoo(a, e, l, j)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,c,i,m) * toooo(m, j, l, i)
term(9) = term(9) + t2(a,c,m,j) * toooo(m, i, l, i)
term(10) = term(10) + t2(a,c,i,m) * toooo(m, i, l, j)
term(11) = term(11) + t2(a,c,m,i) * toooo(m, i, l, j)
end do 

term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v4_eom_cc3_31_triplet_trans_aibjcibl = 0.d+0
    do s = 0, 11
    v4_eom_cc3_31_triplet_trans_aibjcibl = v4_eom_cc3_31_triplet_trans_aibjcibl + term(s)
    end do

    end function v4_eom_cc3_31_triplet_trans_aibjcibl
    function v4_eom_cc3_31_triplet_trans_aibjcial(t2, nocc, nactive, i, b, j, c, l) 
    real(F64) :: v4_eom_cc3_31_triplet_trans_aibjcial 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * tvoov(b, i, l, e)
term(1) = term(1) + t2(b,e,i,i) * tvoov(c, j, l, e)
term(2) = term(2) + t2(b,e,i,j) * tvvoo(c, e, l, i)
term(3) = term(3) + t2(c,e,j,i) * tvvoo(b, e, l, i)
term(4) = term(4) + t2(b,e,j,i) * tvoov(c, i, l, e)
term(5) = term(5) + t2(c,e,i,i) * tvoov(b, j, l, e)
term(6) = term(6) + t2(c,e,i,j) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(b,e,j,i) * tvvoo(c, e, l, i)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,i,m) * toooo(m, j, l, i)
term(9) = term(9) + t2(b,c,m,j) * toooo(m, i, l, i)
term(10) = term(10) + t2(b,c,m,i) * toooo(m, j, l, i)
term(11) = term(11) + t2(b,c,j,m) * toooo(m, i, l, i)
end do 

term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 


    v4_eom_cc3_31_triplet_trans_aibjcial = 0.d+0
    do s = 0, 11
    v4_eom_cc3_31_triplet_trans_aibjcial = v4_eom_cc3_31_triplet_trans_aibjcial + term(s)
    end do

    end function v4_eom_cc3_31_triplet_trans_aibjcial
    function v4_eom_cc3_31_triplet_trans_aibjcicl(t2, nocc, nactive, a, i, b, j, l) 
    real(F64) :: v4_eom_cc3_31_triplet_trans_aibjcicl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,j) * tvoov(b, i, l, e)
term(2) = term(2) + t2(a,e,i,i) * tvvoo(b, e, l, j)
term(3) = term(3) + t2(b,e,i,i) * tvvoo(a, e, l, j)
term(4) = term(4) + t2(b,e,j,i) * tvoov(a, i, l, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(b, j, l, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(b,e,j,i) * tvvoo(a, e, l, i)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,b,i,m) * toooo(m, i, l, j)
term(9) = term(9) + t2(a,b,m,i) * toooo(m, i, l, j)
term(10) = term(10) + t2(a,b,i,m) * toooo(m, j, l, i)
term(11) = term(11) + t2(a,b,m,j) * toooo(m, i, l, i)
end do 

term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v4_eom_cc3_31_triplet_trans_aibjcicl = 0.d+0
    do s = 0, 11
    v4_eom_cc3_31_triplet_trans_aibjcicl = v4_eom_cc3_31_triplet_trans_aibjcicl + term(s)
    end do

    end function v4_eom_cc3_31_triplet_trans_aibjcicl
    function v4_eom_cc3_31_triplet_trans_aibjcidi(t2, nocc, nactive, a, i, b, j, c, d) 
    real(F64) :: v4_eom_cc3_31_triplet_trans_aibjcidi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(c,e,j,i) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, e, b, d)
term(3) = term(3) + t2(c,e,j,i) * read_ftvvvv(b, d, a, e)
term(4) = term(4) + t2(c,e,i,j) * read_ftvvvv(b, e, a, d)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(c, e, a, d)
term(6) = term(6) + t2(a,e,i,j) * read_ftvvvv(c, d, b, e)
term(7) = term(7) + t2(b,e,j,i) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,m,j) * tvoov(a, i, m, d)
term(9) = term(9) + t2(a,c,m,j) * tvoov(b, i, m, d)
term(10) = term(10) + t2(a,b,i,m) * tvoov(c, j, m, d)
term(11) = term(11) + t2(a,b,m,i) * tvoov(c, j, m, d)
term(12) = term(12) + t2(b,c,i,m) * tvvoo(a, d, m, j)
term(13) = term(13) + t2(b,c,m,j) * tvvoo(a, d, m, i)
term(14) = term(14) + t2(a,c,i,m) * tvvoo(b, d, m, j)
term(15) = term(15) + t2(a,c,m,j) * tvvoo(b, d, m, i)
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, i, m, d)
term(17) = term(17) + t2(a,b,m,j) * tvoov(c, i, m, d)
term(18) = term(18) + t2(a,c,i,m) * tvoov(b, j, m, d)
term(19) = term(19) + t2(a,c,m,i) * tvoov(b, j, m, d)
term(20) = term(20) + t2(b,c,m,i) * tvvoo(a, d, m, j)
term(21) = term(21) + t2(b,c,j,m) * tvvoo(a, d, m, i)
term(22) = term(22) + t2(a,b,i,m) * tvvoo(c, d, m, j)
term(23) = term(23) + t2(a,b,m,j) * tvvoo(c, d, m, i)
end do 

term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (-0.5d+0) 


    v4_eom_cc3_31_triplet_trans_aibjcidi = 0.d+0
    do s = 0, 23
    v4_eom_cc3_31_triplet_trans_aibjcidi = v4_eom_cc3_31_triplet_trans_aibjcidi + term(s)
    end do

    end function v4_eom_cc3_31_triplet_trans_aibjcidi
    function v4_eom_cc3_31_triplet_trans_aibjcidj(t2, nocc, nactive, a, i, b, c, d) 
    real(F64) :: v4_eom_cc3_31_triplet_trans_aibjcidj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, b, d)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(c, d, b, e)
term(2) = term(2) + t2(c,e,i,i) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(b,e,i,i) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,i,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(b,c,m,i) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvoov(b, i, m, d)
term(7) = term(7) + t2(a,b,i,m) * tvoov(c, i, m, d)
term(8) = term(8) + t2(a,c,i,m) * tvvoo(b, d, m, i)
term(9) = term(9) + t2(a,b,i,m) * tvvoo(c, d, m, i)
term(10) = term(10) + t2(a,c,m,i) * tvvoo(b, d, m, i)
term(11) = term(11) + t2(a,b,m,i) * tvvoo(c, d, m, i)
end do 

term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v4_eom_cc3_31_triplet_trans_aibjcidj = 0.d+0
    do s = 0, 11
    v4_eom_cc3_31_triplet_trans_aibjcidj = v4_eom_cc3_31_triplet_trans_aibjcidj + term(s)
    end do

    end function v4_eom_cc3_31_triplet_trans_aibjcidj
    function v4_eom_cc3_31_triplet_trans_aibjcibi(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_31_triplet_trans_aibjcibi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * read_ftvvvv(c, e, a, b)
term(1) = term(1) + t2(c,e,j,i) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, e, b, b)
term(3) = term(3) + t2(c,e,j,i) * read_ftvvvv(b, b, a, e)
term(4) = term(4) + t2(c,e,i,j) * read_ftvvvv(b, e, a, b)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(c, e, a, b)
term(6) = term(6) + t2(a,e,i,j) * read_ftvvvv(c, b, b, e)
term(7) = term(7) + t2(b,e,j,i) * read_ftvvvv(c, b, a, e)
term(8) = term(8) + t2(c,e,j,i) * tvoov(a, i, i, e)
term(9) = term(9) + t2(c,e,i,j) * tvoov(a, i, i, e)
term(10) = term(10) + t2(a,e,i,j) * tvoov(c, i, i, e)
term(11) = term(11) + t2(a,e,i,i) * tvoov(c, j, i, e)
term(12) = term(12) + t2(a,e,i,j) * tvvoo(c, e, i, i)
term(13) = term(13) + t2(c,e,j,i) * tvvoo(a, e, i, i)
term(14) = term(14) + t2(a,e,i,i) * tvvoo(c, e, i, j)
term(15) = term(15) + t2(c,e,i,i) * tvvoo(a, e, i, j)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (0.5d+0) 
term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,m,j) * tvoov(a, i, m, b)
term(17) = term(17) + t2(a,c,m,j) * tvoov(b, i, m, b)
term(18) = term(18) + t2(a,b,i,m) * tvoov(c, j, m, b)
term(19) = term(19) + t2(a,b,m,i) * tvoov(c, j, m, b)
term(20) = term(20) + t2(b,c,i,m) * tvvoo(a, b, m, j)
term(21) = term(21) + t2(b,c,m,j) * tvvoo(a, b, m, i)
term(22) = term(22) + t2(a,c,i,m) * tvvoo(b, b, m, j)
term(23) = term(23) + t2(a,c,m,j) * tvvoo(b, b, m, i)
term(24) = term(24) + t2(b,c,j,m) * tvoov(a, i, m, b)
term(25) = term(25) + t2(a,b,m,j) * tvoov(c, i, m, b)
term(26) = term(26) + t2(a,c,i,m) * tvoov(b, j, m, b)
term(27) = term(27) + t2(a,c,m,i) * tvoov(b, j, m, b)
term(28) = term(28) + t2(b,c,m,i) * tvvoo(a, b, m, j)
term(29) = term(29) + t2(b,c,j,m) * tvvoo(a, b, m, i)
term(30) = term(30) + t2(a,b,i,m) * tvvoo(c, b, m, j)
term(31) = term(31) + t2(a,b,m,j) * tvvoo(c, b, m, i)
term(32) = term(32) + t2(a,c,i,m) * toooo(m, j, i, i)
term(33) = term(33) + t2(a,c,m,j) * toooo(m, i, i, i)
term(34) = term(34) + t2(a,c,i,m) * toooo(m, i, i, j)
term(35) = term(35) + t2(a,c,m,i) * toooo(m, i, i, j)
end do 

term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * (0.5d+0) 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * (0.5d+0) 
term(28) = term(28) * (0.5d+0) 
term(29) = term(29) * (0.5d+0) 
term(30) = term(30) * (-0.5d+0) 
term(31) = term(31) * (-0.5d+0) 
term(32) = term(32) * (-0.5d+0) 
term(33) = term(33) * (-0.5d+0) 
term(34) = term(34) * (0.5d+0) 
term(35) = term(35) * (0.5d+0) 


    v4_eom_cc3_31_triplet_trans_aibjcibi = 0.d+0
    do s = 0, 35
    v4_eom_cc3_31_triplet_trans_aibjcibi = v4_eom_cc3_31_triplet_trans_aibjcibi + term(s)
    end do

    end function v4_eom_cc3_31_triplet_trans_aibjcibi
    function v4_eom_cc3_31_triplet_trans_aibjcibj(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_31_triplet_trans_aibjcibj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * tvoov(a, i, j, e)
term(1) = term(1) + t2(c,e,i,j) * tvoov(a, i, j, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(c, i, j, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(c, j, j, e)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(c, e, j, i)
term(5) = term(5) + t2(c,e,j,i) * tvvoo(a, e, j, i)
term(6) = term(6) + t2(a,e,i,i) * tvvoo(c, e, j, j)
term(7) = term(7) + t2(c,e,i,i) * tvvoo(a, e, j, j)
term(8) = term(8) + t2(a,e,i,i) * read_ftvvvv(c, e, b, b)
term(9) = term(9) + t2(a,e,i,i) * read_ftvvvv(c, b, b, e)
term(10) = term(10) + t2(c,e,i,i) * read_ftvvvv(b, b, a, e)
term(11) = term(11) + t2(b,e,i,i) * read_ftvvvv(c, b, a, e)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
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

do m = 1, nocc 
term(12) = term(12) + t2(a,c,i,m) * toooo(m, j, j, i)
term(13) = term(13) + t2(a,c,m,j) * toooo(m, i, j, i)
term(14) = term(14) + t2(a,c,i,m) * toooo(m, i, j, j)
term(15) = term(15) + t2(a,c,m,i) * toooo(m, i, j, j)
term(16) = term(16) + t2(b,c,i,m) * tvoov(a, i, m, b)
term(17) = term(17) + t2(b,c,m,i) * tvoov(a, i, m, b)
term(18) = term(18) + t2(a,c,i,m) * tvoov(b, i, m, b)
term(19) = term(19) + t2(a,b,i,m) * tvoov(c, i, m, b)
term(20) = term(20) + t2(a,c,i,m) * tvvoo(b, b, m, i)
term(21) = term(21) + t2(a,b,i,m) * tvvoo(c, b, m, i)
term(22) = term(22) + t2(a,c,m,i) * tvvoo(b, b, m, i)
term(23) = term(23) + t2(a,b,m,i) * tvvoo(c, b, m, i)
end do 

term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (0.5d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (0.5d+0) 


    v4_eom_cc3_31_triplet_trans_aibjcibj = 0.d+0
    do s = 0, 23
    v4_eom_cc3_31_triplet_trans_aibjcibj = v4_eom_cc3_31_triplet_trans_aibjcibj + term(s)
    end do

    end function v4_eom_cc3_31_triplet_trans_aibjcibj
    function v4_eom_cc3_31_triplet_trans_aibjciai(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_31_triplet_trans_aibjciai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(c,e,j,i) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, e, b, a)
term(3) = term(3) + t2(c,e,j,i) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(c,e,i,j) * read_ftvvvv(b, e, a, a)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(c, e, a, a)
term(6) = term(6) + t2(a,e,i,j) * read_ftvvvv(c, a, b, e)
term(7) = term(7) + t2(b,e,j,i) * read_ftvvvv(c, a, a, e)
term(8) = term(8) + t2(c,e,j,i) * tvoov(b, i, i, e)
term(9) = term(9) + t2(b,e,i,i) * tvoov(c, j, i, e)
term(10) = term(10) + t2(b,e,i,j) * tvvoo(c, e, i, i)
term(11) = term(11) + t2(c,e,j,i) * tvvoo(b, e, i, i)
term(12) = term(12) + t2(b,e,j,i) * tvoov(c, i, i, e)
term(13) = term(13) + t2(c,e,i,i) * tvoov(b, j, i, e)
term(14) = term(14) + t2(c,e,i,j) * tvvoo(b, e, i, i)
term(15) = term(15) + t2(b,e,j,i) * tvvoo(c, e, i, i)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (0.5d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,m,j) * tvoov(a, i, m, a)
term(17) = term(17) + t2(a,c,m,j) * tvoov(b, i, m, a)
term(18) = term(18) + t2(a,b,i,m) * tvoov(c, j, m, a)
term(19) = term(19) + t2(a,b,m,i) * tvoov(c, j, m, a)
term(20) = term(20) + t2(b,c,i,m) * tvvoo(a, a, m, j)
term(21) = term(21) + t2(b,c,m,j) * tvvoo(a, a, m, i)
term(22) = term(22) + t2(a,c,i,m) * tvvoo(b, a, m, j)
term(23) = term(23) + t2(a,c,m,j) * tvvoo(b, a, m, i)
term(24) = term(24) + t2(b,c,j,m) * tvoov(a, i, m, a)
term(25) = term(25) + t2(a,b,m,j) * tvoov(c, i, m, a)
term(26) = term(26) + t2(a,c,i,m) * tvoov(b, j, m, a)
term(27) = term(27) + t2(a,c,m,i) * tvoov(b, j, m, a)
term(28) = term(28) + t2(b,c,m,i) * tvvoo(a, a, m, j)
term(29) = term(29) + t2(b,c,j,m) * tvvoo(a, a, m, i)
term(30) = term(30) + t2(a,b,i,m) * tvvoo(c, a, m, j)
term(31) = term(31) + t2(a,b,m,j) * tvvoo(c, a, m, i)
term(32) = term(32) + t2(b,c,i,m) * toooo(m, j, i, i)
term(33) = term(33) + t2(b,c,m,j) * toooo(m, i, i, i)
term(34) = term(34) + t2(b,c,m,i) * toooo(m, j, i, i)
term(35) = term(35) + t2(b,c,j,m) * toooo(m, i, i, i)
end do 

term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * (0.5d+0) 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * (0.5d+0) 
term(28) = term(28) * (0.5d+0) 
term(29) = term(29) * (0.5d+0) 
term(30) = term(30) * (-0.5d+0) 
term(31) = term(31) * (-0.5d+0) 
term(32) = term(32) * (0.5d+0) 
term(33) = term(33) * (0.5d+0) 
term(34) = term(34) * (-0.5d+0) 
term(35) = term(35) * (-0.5d+0) 


    v4_eom_cc3_31_triplet_trans_aibjciai = 0.d+0
    do s = 0, 35
    v4_eom_cc3_31_triplet_trans_aibjciai = v4_eom_cc3_31_triplet_trans_aibjciai + term(s)
    end do

    end function v4_eom_cc3_31_triplet_trans_aibjciai
    function v4_eom_cc3_31_triplet_trans_aibjciaj(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_31_triplet_trans_aibjciaj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * tvoov(b, i, j, e)
term(1) = term(1) + t2(b,e,i,i) * tvoov(c, j, j, e)
term(2) = term(2) + t2(b,e,i,j) * tvvoo(c, e, j, i)
term(3) = term(3) + t2(c,e,j,i) * tvvoo(b, e, j, i)
term(4) = term(4) + t2(b,e,j,i) * tvoov(c, i, j, e)
term(5) = term(5) + t2(c,e,i,i) * tvoov(b, j, j, e)
term(6) = term(6) + t2(c,e,i,j) * tvvoo(b, e, j, i)
term(7) = term(7) + t2(b,e,j,i) * tvvoo(c, e, j, i)
term(8) = term(8) + t2(a,e,i,i) * read_ftvvvv(c, e, b, a)
term(9) = term(9) + t2(a,e,i,i) * read_ftvvvv(c, a, b, e)
term(10) = term(10) + t2(c,e,i,i) * read_ftvvvv(b, a, a, e)
term(11) = term(11) + t2(b,e,i,i) * read_ftvvvv(c, a, a, e)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,i,m) * toooo(m, j, j, i)
term(13) = term(13) + t2(b,c,m,j) * toooo(m, i, j, i)
term(14) = term(14) + t2(b,c,m,i) * toooo(m, j, j, i)
term(15) = term(15) + t2(b,c,j,m) * toooo(m, i, j, i)
term(16) = term(16) + t2(b,c,i,m) * tvoov(a, i, m, a)
term(17) = term(17) + t2(b,c,m,i) * tvoov(a, i, m, a)
term(18) = term(18) + t2(a,c,i,m) * tvoov(b, i, m, a)
term(19) = term(19) + t2(a,b,i,m) * tvoov(c, i, m, a)
term(20) = term(20) + t2(a,c,i,m) * tvvoo(b, a, m, i)
term(21) = term(21) + t2(a,b,i,m) * tvvoo(c, a, m, i)
term(22) = term(22) + t2(a,c,m,i) * tvvoo(b, a, m, i)
term(23) = term(23) + t2(a,b,m,i) * tvvoo(c, a, m, i)
end do 

term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (0.5d+0) 


    v4_eom_cc3_31_triplet_trans_aibjciaj = 0.d+0
    do s = 0, 23
    v4_eom_cc3_31_triplet_trans_aibjciaj = v4_eom_cc3_31_triplet_trans_aibjciaj + term(s)
    end do

    end function v4_eom_cc3_31_triplet_trans_aibjciaj
    function v4_eom_cc3_31_triplet_trans_aibjcici(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_31_triplet_trans_aibjcici 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,j,i) * read_ftvvvv(b, e, a, c)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, e, b, c)
term(3) = term(3) + t2(c,e,j,i) * read_ftvvvv(b, c, a, e)
term(4) = term(4) + t2(c,e,i,j) * read_ftvvvv(b, e, a, c)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(c, e, a, c)
term(6) = term(6) + t2(a,e,i,j) * read_ftvvvv(c, c, b, e)
term(7) = term(7) + t2(b,e,j,i) * read_ftvvvv(c, c, a, e)
term(8) = term(8) + t2(b,e,i,j) * tvoov(a, i, i, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(b, i, i, e)
term(10) = term(10) + t2(a,e,i,i) * tvvoo(b, e, i, j)
term(11) = term(11) + t2(b,e,i,i) * tvvoo(a, e, i, j)
term(12) = term(12) + t2(b,e,j,i) * tvoov(a, i, i, e)
term(13) = term(13) + t2(a,e,i,i) * tvoov(b, j, i, e)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(b, e, i, i)
term(15) = term(15) + t2(b,e,j,i) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,m,j) * tvoov(a, i, m, c)
term(17) = term(17) + t2(a,c,m,j) * tvoov(b, i, m, c)
term(18) = term(18) + t2(a,b,i,m) * tvoov(c, j, m, c)
term(19) = term(19) + t2(a,b,m,i) * tvoov(c, j, m, c)
term(20) = term(20) + t2(b,c,i,m) * tvvoo(a, c, m, j)
term(21) = term(21) + t2(b,c,m,j) * tvvoo(a, c, m, i)
term(22) = term(22) + t2(a,c,i,m) * tvvoo(b, c, m, j)
term(23) = term(23) + t2(a,c,m,j) * tvvoo(b, c, m, i)
term(24) = term(24) + t2(b,c,j,m) * tvoov(a, i, m, c)
term(25) = term(25) + t2(a,b,m,j) * tvoov(c, i, m, c)
term(26) = term(26) + t2(a,c,i,m) * tvoov(b, j, m, c)
term(27) = term(27) + t2(a,c,m,i) * tvoov(b, j, m, c)
term(28) = term(28) + t2(b,c,m,i) * tvvoo(a, c, m, j)
term(29) = term(29) + t2(b,c,j,m) * tvvoo(a, c, m, i)
term(30) = term(30) + t2(a,b,i,m) * tvvoo(c, c, m, j)
term(31) = term(31) + t2(a,b,m,j) * tvvoo(c, c, m, i)
term(32) = term(32) + t2(a,b,i,m) * toooo(m, i, i, j)
term(33) = term(33) + t2(a,b,m,i) * toooo(m, i, i, j)
term(34) = term(34) + t2(a,b,i,m) * toooo(m, j, i, i)
term(35) = term(35) + t2(a,b,m,j) * toooo(m, i, i, i)
end do 

term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * (0.5d+0) 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * (0.5d+0) 
term(28) = term(28) * (0.5d+0) 
term(29) = term(29) * (0.5d+0) 
term(30) = term(30) * (-0.5d+0) 
term(31) = term(31) * (-0.5d+0) 
term(32) = term(32) * (-0.5d+0) 
term(33) = term(33) * (-0.5d+0) 
term(34) = term(34) * (0.5d+0) 
term(35) = term(35) * (0.5d+0) 


    v4_eom_cc3_31_triplet_trans_aibjcici = 0.d+0
    do s = 0, 35
    v4_eom_cc3_31_triplet_trans_aibjcici = v4_eom_cc3_31_triplet_trans_aibjcici + term(s)
    end do

    end function v4_eom_cc3_31_triplet_trans_aibjcici
    function v4_eom_cc3_31_triplet_trans_aibjcicj(t2, nocc, nactive, a, i, b, j, c) 
    real(F64) :: v4_eom_cc3_31_triplet_trans_aibjcicj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tvoov(a, i, j, e)
term(1) = term(1) + t2(a,e,i,j) * tvoov(b, i, j, e)
term(2) = term(2) + t2(a,e,i,i) * tvvoo(b, e, j, j)
term(3) = term(3) + t2(b,e,i,i) * tvvoo(a, e, j, j)
term(4) = term(4) + t2(b,e,j,i) * tvoov(a, i, j, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(b, j, j, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(b, e, j, i)
term(7) = term(7) + t2(b,e,j,i) * tvvoo(a, e, j, i)
term(8) = term(8) + t2(a,e,i,i) * read_ftvvvv(c, e, b, c)
term(9) = term(9) + t2(a,e,i,i) * read_ftvvvv(c, c, b, e)
term(10) = term(10) + t2(c,e,i,i) * read_ftvvvv(b, c, a, e)
term(11) = term(11) + t2(b,e,i,i) * read_ftvvvv(c, c, a, e)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,i,m) * toooo(m, i, j, j)
term(13) = term(13) + t2(a,b,m,i) * toooo(m, i, j, j)
term(14) = term(14) + t2(a,b,i,m) * toooo(m, j, j, i)
term(15) = term(15) + t2(a,b,m,j) * toooo(m, i, j, i)
term(16) = term(16) + t2(b,c,i,m) * tvoov(a, i, m, c)
term(17) = term(17) + t2(b,c,m,i) * tvoov(a, i, m, c)
term(18) = term(18) + t2(a,c,i,m) * tvoov(b, i, m, c)
term(19) = term(19) + t2(a,b,i,m) * tvoov(c, i, m, c)
term(20) = term(20) + t2(a,c,i,m) * tvvoo(b, c, m, i)
term(21) = term(21) + t2(a,b,i,m) * tvvoo(c, c, m, i)
term(22) = term(22) + t2(a,c,m,i) * tvvoo(b, c, m, i)
term(23) = term(23) + t2(a,b,m,i) * tvvoo(c, c, m, i)
end do 

term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (0.5d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (0.5d+0) 


    v4_eom_cc3_31_triplet_trans_aibjcicj = 0.d+0
    do s = 0, 23
    v4_eom_cc3_31_triplet_trans_aibjcicj = v4_eom_cc3_31_triplet_trans_aibjcicj + term(s)
    end do

    end function v4_eom_cc3_31_triplet_trans_aibjcicj
    end module v4_eom_cc3_31_triplet_trans
    