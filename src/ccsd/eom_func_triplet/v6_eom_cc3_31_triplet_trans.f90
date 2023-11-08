module v6_eom_cc3_31_triplet_trans

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
    
    function v6_eom_cc3_31_triplet_trans_aiajcial(t2, nocc, nactive, a, i, j, c, l) 
    real(F64) :: v6_eom_cc3_31_triplet_trans_aiajcial 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(c,e,i,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,j,i) * tvoov(c, i, l, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(c, i, l, e)
term(4) = term(4) + t2(a,e,i,i) * tvvoo(c, e, l, j)
term(5) = term(5) + t2(c,e,i,i) * tvvoo(a, e, l, j)
term(6) = term(6) + t2(a,e,j,i) * tvvoo(c, e, l, i)
term(7) = term(7) + t2(c,e,i,j) * tvvoo(a, e, l, i)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,c,i,m) * toooo(m, i, l, j)
term(9) = term(9) + t2(a,c,m,i) * toooo(m, i, l, j)
term(10) = term(10) + t2(a,c,j,m) * toooo(m, i, l, i)
term(11) = term(11) + t2(a,c,m,i) * toooo(m, j, l, i)
end do 

term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 


    v6_eom_cc3_31_triplet_trans_aiajcial = 0.d+0
    do s = 0, 11
    v6_eom_cc3_31_triplet_trans_aiajcial = v6_eom_cc3_31_triplet_trans_aiajcial + term(s)
    end do

    end function v6_eom_cc3_31_triplet_trans_aiajcial
    function v6_eom_cc3_31_triplet_trans_aiajcicl(t2, nocc, nactive, a, i, j, l) 
    real(F64) :: v6_eom_cc3_31_triplet_trans_aiajcicl 
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
term(3) = term(3) + t2(a,e,j,i) * tvvoo(a, e, l, i)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(a, e, l, i)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(a, e, l, j)
end do 

term(0) = term(0) * (-0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,a,j,m) * toooo(m, i, l, i)
term(7) = term(7) + t2(a,a,i,m) * toooo(m, j, l, i)
term(8) = term(8) + t2(a,a,i,m) * toooo(m, i, l, j)
end do 

term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-1.0d+0) 


    v6_eom_cc3_31_triplet_trans_aiajcicl = 0.d+0
    do s = 0, 8
    v6_eom_cc3_31_triplet_trans_aiajcicl = v6_eom_cc3_31_triplet_trans_aiajcicl + term(s)
    end do

    end function v6_eom_cc3_31_triplet_trans_aiajcicl
    function v6_eom_cc3_31_triplet_trans_aiajcidi(t2, nocc, nactive, a, i, j, c, d) 
    real(F64) :: v6_eom_cc3_31_triplet_trans_aiajcidi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, d, a, e)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, d, a, e)
term(3) = term(3) + t2(c,e,i,j) * read_ftvvvv(a, e, a, d)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,i,m) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,c,m,i) * tvoov(a, j, m, d)
term(6) = term(6) + t2(a,a,j,m) * tvoov(c, i, m, d)
term(7) = term(7) + t2(a,c,j,m) * tvoov(a, i, m, d)
term(8) = term(8) + t2(a,c,j,m) * tvvoo(a, d, m, i)
term(9) = term(9) + t2(a,a,j,m) * tvvoo(c, d, m, i)
term(10) = term(10) + t2(a,a,i,m) * tvvoo(c, d, m, j)
term(11) = term(11) + t2(a,c,m,i) * tvvoo(a, d, m, j)
end do 

term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v6_eom_cc3_31_triplet_trans_aiajcidi = 0.d+0
    do s = 0, 11
    v6_eom_cc3_31_triplet_trans_aiajcidi = v6_eom_cc3_31_triplet_trans_aiajcidi + term(s)
    end do

    end function v6_eom_cc3_31_triplet_trans_aiajcidi
    function v6_eom_cc3_31_triplet_trans_aiajcidj(t2, nocc, nactive, a, i, c, d) 
    real(F64) :: v6_eom_cc3_31_triplet_trans_aiajcidj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, d 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(c,e,i,i) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,c,i,m) * tvoov(a, i, m, d)
term(4) = term(4) + t2(a,c,m,i) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,a,i,m) * tvoov(c, i, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvvoo(a, d, m, i)
term(7) = term(7) + t2(a,c,m,i) * tvvoo(a, d, m, i)
term(8) = term(8) + t2(a,a,i,m) * tvvoo(c, d, m, i)
end do 

term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 


    v6_eom_cc3_31_triplet_trans_aiajcidj = 0.d+0
    do s = 0, 8
    v6_eom_cc3_31_triplet_trans_aiajcidj = v6_eom_cc3_31_triplet_trans_aiajcidj + term(s)
    end do

    end function v6_eom_cc3_31_triplet_trans_aiajcidj
    function v6_eom_cc3_31_triplet_trans_aiajciai(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: v6_eom_cc3_31_triplet_trans_aiajciai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,i) * tvoov(a, j, i, e)
term(1) = term(1) + t2(c,e,i,j) * tvoov(a, i, i, e)
term(2) = term(2) + t2(a,e,j,i) * tvoov(c, i, i, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(c, i, i, e)
term(4) = term(4) + t2(a,e,j,i) * read_ftvvvv(c, e, a, a)
term(5) = term(5) + t2(a,e,j,i) * read_ftvvvv(c, a, a, e)
term(6) = term(6) + t2(a,e,i,j) * read_ftvvvv(c, a, a, e)
term(7) = term(7) + t2(c,e,i,j) * read_ftvvvv(a, e, a, a)
term(8) = term(8) + t2(a,e,i,i) * tvvoo(c, e, i, j)
term(9) = term(9) + t2(c,e,i,i) * tvvoo(a, e, i, j)
term(10) = term(10) + t2(a,e,j,i) * tvvoo(c, e, i, i)
term(11) = term(11) + t2(c,e,i,j) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,i,m) * tvoov(a, j, m, a)
term(13) = term(13) + t2(a,c,m,i) * tvoov(a, j, m, a)
term(14) = term(14) + t2(a,a,j,m) * tvoov(c, i, m, a)
term(15) = term(15) + t2(a,c,j,m) * tvoov(a, i, m, a)
term(16) = term(16) + t2(a,c,j,m) * tvvoo(a, a, m, i)
term(17) = term(17) + t2(a,a,j,m) * tvvoo(c, a, m, i)
term(18) = term(18) + t2(a,a,i,m) * tvvoo(c, a, m, j)
term(19) = term(19) + t2(a,c,m,i) * tvvoo(a, a, m, j)
term(20) = term(20) + t2(a,c,i,m) * toooo(m, i, i, j)
term(21) = term(21) + t2(a,c,m,i) * toooo(m, i, i, j)
term(22) = term(22) + t2(a,c,j,m) * toooo(m, i, i, i)
term(23) = term(23) + t2(a,c,m,i) * toooo(m, j, i, i)
end do 

term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (-0.5d+0) 


    v6_eom_cc3_31_triplet_trans_aiajciai = 0.d+0
    do s = 0, 23
    v6_eom_cc3_31_triplet_trans_aiajciai = v6_eom_cc3_31_triplet_trans_aiajciai + term(s)
    end do

    end function v6_eom_cc3_31_triplet_trans_aiajciai
    function v6_eom_cc3_31_triplet_trans_aiajciaj(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: v6_eom_cc3_31_triplet_trans_aiajciaj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(c,e,i,i) * read_ftvvvv(a, e, a, a)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, a, a, e)
term(3) = term(3) + t2(c,e,i,i) * tvoov(a, j, j, e)
term(4) = term(4) + t2(c,e,i,j) * tvoov(a, i, j, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(c, i, j, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(c, i, j, e)
term(7) = term(7) + t2(a,e,i,i) * tvvoo(c, e, j, j)
term(8) = term(8) + t2(c,e,i,i) * tvvoo(a, e, j, j)
term(9) = term(9) + t2(a,e,j,i) * tvvoo(c, e, j, i)
term(10) = term(10) + t2(c,e,i,j) * tvvoo(a, e, j, i)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (0.5d+0) 

do m = 1, nocc 
term(11) = term(11) + t2(a,c,i,m) * tvoov(a, i, m, a)
term(12) = term(12) + t2(a,c,m,i) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,a,i,m) * tvoov(c, i, m, a)
term(14) = term(14) + t2(a,c,i,m) * tvvoo(a, a, m, i)
term(15) = term(15) + t2(a,c,m,i) * tvvoo(a, a, m, i)
term(16) = term(16) + t2(a,a,i,m) * tvvoo(c, a, m, i)
term(17) = term(17) + t2(a,c,i,m) * toooo(m, i, j, j)
term(18) = term(18) + t2(a,c,m,i) * toooo(m, i, j, j)
term(19) = term(19) + t2(a,c,j,m) * toooo(m, i, j, i)
term(20) = term(20) + t2(a,c,m,i) * toooo(m, j, j, i)
end do 

term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 


    v6_eom_cc3_31_triplet_trans_aiajciaj = 0.d+0
    do s = 0, 20
    v6_eom_cc3_31_triplet_trans_aiajciaj = v6_eom_cc3_31_triplet_trans_aiajciaj + term(s)
    end do

    end function v6_eom_cc3_31_triplet_trans_aiajciaj
    function v6_eom_cc3_31_triplet_trans_aiajcici(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: v6_eom_cc3_31_triplet_trans_aiajcici 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, c, a, e)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, c, a, e)
term(3) = term(3) + t2(c,e,i,j) * read_ftvvvv(a, e, a, c)
term(4) = term(4) + t2(a,e,i,i) * tvoov(a, j, i, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,j,i) * tvoov(a, i, i, e)
term(7) = term(7) + t2(a,e,j,i) * tvvoo(a, e, i, i)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(a, e, i, i)
term(9) = term(9) + t2(a,e,i,i) * tvvoo(a, e, i, j)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 

do m = 1, nocc 
term(10) = term(10) + t2(a,c,i,m) * tvoov(a, j, m, c)
term(11) = term(11) + t2(a,c,m,i) * tvoov(a, j, m, c)
term(12) = term(12) + t2(a,a,j,m) * tvoov(c, i, m, c)
term(13) = term(13) + t2(a,c,j,m) * tvoov(a, i, m, c)
term(14) = term(14) + t2(a,c,j,m) * tvvoo(a, c, m, i)
term(15) = term(15) + t2(a,a,j,m) * tvvoo(c, c, m, i)
term(16) = term(16) + t2(a,a,i,m) * tvvoo(c, c, m, j)
term(17) = term(17) + t2(a,c,m,i) * tvvoo(a, c, m, j)
term(18) = term(18) + t2(a,a,j,m) * toooo(m, i, i, i)
term(19) = term(19) + t2(a,a,i,m) * toooo(m, j, i, i)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, i, i, j)
end do 

term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (0.5d+0) 
term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (-1.0d+0) 


    v6_eom_cc3_31_triplet_trans_aiajcici = 0.d+0
    do s = 0, 20
    v6_eom_cc3_31_triplet_trans_aiajcici = v6_eom_cc3_31_triplet_trans_aiajcici + term(s)
    end do

    end function v6_eom_cc3_31_triplet_trans_aiajcici
    function v6_eom_cc3_31_triplet_trans_aiajcicj(t2, nocc, nactive, a, i, j, c) 
    real(F64) :: v6_eom_cc3_31_triplet_trans_aiajcicj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    real(F64), dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,i,i) * read_ftvvvv(a, e, a, c)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, c, a, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(a, j, j, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(a, i, j, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,j,i) * tvvoo(a, e, j, i)
term(7) = term(7) + t2(a,e,i,j) * tvvoo(a, e, j, i)
term(8) = term(8) + t2(a,e,i,i) * tvvoo(a, e, j, j)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 

do m = 1, nocc 
term(9) = term(9) + t2(a,c,i,m) * tvoov(a, i, m, c)
term(10) = term(10) + t2(a,c,m,i) * tvoov(a, i, m, c)
term(11) = term(11) + t2(a,a,i,m) * tvoov(c, i, m, c)
term(12) = term(12) + t2(a,c,i,m) * tvvoo(a, c, m, i)
term(13) = term(13) + t2(a,c,m,i) * tvvoo(a, c, m, i)
term(14) = term(14) + t2(a,a,i,m) * tvvoo(c, c, m, i)
term(15) = term(15) + t2(a,a,j,m) * toooo(m, i, j, i)
term(16) = term(16) + t2(a,a,i,m) * toooo(m, j, j, i)
term(17) = term(17) + t2(a,a,i,m) * toooo(m, i, j, j)
end do 

term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(15) = term(15) * (0.5d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-1.0d+0) 


    v6_eom_cc3_31_triplet_trans_aiajcicj = 0.d+0
    do s = 0, 17
    v6_eom_cc3_31_triplet_trans_aiajcicj = v6_eom_cc3_31_triplet_trans_aiajcicj + term(s)
    end do

    end function v6_eom_cc3_31_triplet_trans_aiajcicj
    end module v6_eom_cc3_31_triplet_trans
    