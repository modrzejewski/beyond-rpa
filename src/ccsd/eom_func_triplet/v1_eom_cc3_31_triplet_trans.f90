module v1_eom_cc3_31_triplet_trans

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
    
    function v1_eom_cc3_31_triplet_trans_aiajckal(t2, nocc, nactive, a, i, j, c, k, l) 
    real(F64) :: v1_eom_cc3_31_triplet_trans_aiajckal 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,k,i) * tvoov(a, j, l, e)
term(1) = term(1) + t2(c,e,j,i) * tvoov(a, k, l, e)
term(2) = term(2) + t2(c,e,j,k) * tvoov(a, i, l, e)
term(3) = term(3) + t2(c,e,k,j) * tvoov(a, i, l, e)
term(4) = term(4) + t2(a,e,k,i) * tvoov(c, j, l, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(c, j, l, e)
term(6) = term(6) + t2(a,e,j,i) * tvoov(c, k, l, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(c, k, l, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(c, e, l, j)
term(9) = term(9) + t2(c,e,k,i) * tvvoo(a, e, l, j)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(c, e, l, k)
term(11) = term(11) + t2(c,e,j,i) * tvvoo(a, e, l, k)
term(12) = term(12) + t2(a,e,k,j) * tvvoo(c, e, l, i)
term(13) = term(13) + t2(a,e,j,k) * tvvoo(c, e, l, i)
term(14) = term(14) + t2(c,e,j,k) * tvvoo(a, e, l, i)
term(15) = term(15) + t2(c,e,k,j) * tvvoo(a, e, l, i)
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
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (0.5d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(a,c,i,m) * toooo(m, k, l, j)
term(17) = term(17) + t2(a,c,m,k) * toooo(m, i, l, j)
term(18) = term(18) + t2(a,c,i,m) * toooo(m, j, l, k)
term(19) = term(19) + t2(a,c,m,j) * toooo(m, i, l, k)
term(20) = term(20) + t2(a,c,k,m) * toooo(m, j, l, i)
term(21) = term(21) + t2(a,c,j,m) * toooo(m, k, l, i)
term(22) = term(22) + t2(a,c,m,j) * toooo(m, k, l, i)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, j, l, i)
end do 

term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (-0.5d+0) 


    v1_eom_cc3_31_triplet_trans_aiajckal = 0.d+0
    do s = 0, 23
    v1_eom_cc3_31_triplet_trans_aiajckal = v1_eom_cc3_31_triplet_trans_aiajckal + term(s)
    end do

    end function v1_eom_cc3_31_triplet_trans_aiajckal
    function v1_eom_cc3_31_triplet_trans_aiajckcl(t2, nocc, nactive, a, i, j, k, l) 
    real(F64) :: v1_eom_cc3_31_triplet_trans_aiajckcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tvoov(a, k, l, e)
term(1) = term(1) + t2(a,e,k,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,k,i) * tvvoo(a, e, l, j)
term(3) = term(3) + t2(a,e,i,k) * tvvoo(a, e, l, j)
term(4) = term(4) + t2(a,e,i,k) * tvoov(a, j, l, e)
term(5) = term(5) + t2(a,e,j,k) * tvoov(a, i, l, e)
term(6) = term(6) + t2(a,e,j,i) * tvvoo(a, e, l, k)
term(7) = term(7) + t2(a,e,i,j) * tvvoo(a, e, l, k)
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
term(8) = term(8) + t2(a,a,k,m) * toooo(m, i, l, j)
term(9) = term(9) + t2(a,a,i,m) * toooo(m, k, l, j)
term(10) = term(10) + t2(a,a,j,m) * toooo(m, i, l, k)
term(11) = term(11) + t2(a,a,i,m) * toooo(m, j, l, k)
end do 

term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v1_eom_cc3_31_triplet_trans_aiajckcl = 0.d+0
    do s = 0, 11
    v1_eom_cc3_31_triplet_trans_aiajckcl = v1_eom_cc3_31_triplet_trans_aiajckcl + term(s)
    end do

    end function v1_eom_cc3_31_triplet_trans_aiajckcl
    function v1_eom_cc3_31_triplet_trans_aiajckdk(t2, nocc, nactive, a, i, j, c, d) 
    real(F64) :: v1_eom_cc3_31_triplet_trans_aiajckdk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(c,e,j,i) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(c, d, a, e)
term(3) = term(3) + t2(a,e,i,j) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,m,j) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,a,i,m) * tvoov(c, j, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvvoo(a, d, m, j)
term(7) = term(7) + t2(a,c,m,j) * tvvoo(a, d, m, i)
term(8) = term(8) + t2(a,c,i,m) * tvoov(a, j, m, d)
term(9) = term(9) + t2(a,c,j,m) * tvoov(a, i, m, d)
term(10) = term(10) + t2(a,a,j,m) * tvvoo(c, d, m, i)
term(11) = term(11) + t2(a,a,i,m) * tvvoo(c, d, m, j)
end do 

term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 


    v1_eom_cc3_31_triplet_trans_aiajckdk = 0.d+0
    do s = 0, 11
    v1_eom_cc3_31_triplet_trans_aiajckdk = v1_eom_cc3_31_triplet_trans_aiajckdk + term(s)
    end do

    end function v1_eom_cc3_31_triplet_trans_aiajckdk
    function v1_eom_cc3_31_triplet_trans_aiajckdj(t2, nocc, nactive, a, i, c, k, d) 
    real(F64) :: v1_eom_cc3_31_triplet_trans_aiajckdj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(c,e,k,i) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(c, d, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,i,m) * tvoov(a, k, m, d)
term(5) = term(5) + t2(a,c,k,m) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,c,m,k) * tvoov(a, i, m, d)
term(7) = term(7) + t2(a,a,i,m) * tvoov(c, k, m, d)
term(8) = term(8) + t2(a,c,i,m) * tvvoo(a, d, m, k)
term(9) = term(9) + t2(a,c,m,k) * tvvoo(a, d, m, i)
term(10) = term(10) + t2(a,a,k,m) * tvvoo(c, d, m, i)
term(11) = term(11) + t2(a,a,i,m) * tvvoo(c, d, m, k)
end do 

term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v1_eom_cc3_31_triplet_trans_aiajckdj = 0.d+0
    do s = 0, 11
    v1_eom_cc3_31_triplet_trans_aiajckdj = v1_eom_cc3_31_triplet_trans_aiajckdj + term(s)
    end do

    end function v1_eom_cc3_31_triplet_trans_aiajckdj
    function v1_eom_cc3_31_triplet_trans_aiajckdi(t2, nocc, nactive, a, j, c, k, d) 
    real(F64) :: v1_eom_cc3_31_triplet_trans_aiajckdi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,j) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(a,e,j,k) * read_ftvvvv(c, e, a, d)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(a, e, a, d)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(a, e, a, d)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,m,k) * tvoov(a, j, m, d)
term(5) = term(5) + t2(a,c,m,j) * tvoov(a, k, m, d)
term(6) = term(6) + t2(a,a,k,m) * tvoov(c, j, m, d)
term(7) = term(7) + t2(a,a,j,m) * tvoov(c, k, m, d)
term(8) = term(8) + t2(a,c,k,m) * tvvoo(a, d, m, j)
term(9) = term(9) + t2(a,c,j,m) * tvvoo(a, d, m, k)
term(10) = term(10) + t2(a,c,m,j) * tvvoo(a, d, m, k)
term(11) = term(11) + t2(a,c,m,k) * tvvoo(a, d, m, j)
end do 

term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v1_eom_cc3_31_triplet_trans_aiajckdi = 0.d+0
    do s = 0, 11
    v1_eom_cc3_31_triplet_trans_aiajckdi = v1_eom_cc3_31_triplet_trans_aiajckdi + term(s)
    end do

    end function v1_eom_cc3_31_triplet_trans_aiajckdi
    function v1_eom_cc3_31_triplet_trans_aiajckak(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: v1_eom_cc3_31_triplet_trans_aiajckak 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(c,e,j,i) * read_ftvvvv(a, e, a, a)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(c, a, a, e)
term(3) = term(3) + t2(a,e,i,j) * read_ftvvvv(c, a, a, e)
term(4) = term(4) + t2(c,e,k,i) * tvoov(a, j, k, e)
term(5) = term(5) + t2(c,e,j,i) * tvoov(a, k, k, e)
term(6) = term(6) + t2(c,e,j,k) * tvoov(a, i, k, e)
term(7) = term(7) + t2(c,e,k,j) * tvoov(a, i, k, e)
term(8) = term(8) + t2(a,e,k,i) * tvoov(c, j, k, e)
term(9) = term(9) + t2(a,e,i,k) * tvoov(c, j, k, e)
term(10) = term(10) + t2(a,e,j,i) * tvoov(c, k, k, e)
term(11) = term(11) + t2(a,e,i,j) * tvoov(c, k, k, e)
term(12) = term(12) + t2(a,e,i,k) * tvvoo(c, e, k, j)
term(13) = term(13) + t2(c,e,k,i) * tvvoo(a, e, k, j)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(c, e, k, k)
term(15) = term(15) + t2(c,e,j,i) * tvvoo(a, e, k, k)
term(16) = term(16) + t2(a,e,k,j) * tvvoo(c, e, k, i)
term(17) = term(17) + t2(a,e,j,k) * tvvoo(c, e, k, i)
term(18) = term(18) + t2(c,e,j,k) * tvvoo(a, e, k, i)
term(19) = term(19) + t2(c,e,k,j) * tvvoo(a, e, k, i)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (0.5d+0) 
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

do m = 1, nocc 
term(20) = term(20) + t2(a,c,m,j) * tvoov(a, i, m, a)
term(21) = term(21) + t2(a,a,i,m) * tvoov(c, j, m, a)
term(22) = term(22) + t2(a,c,i,m) * tvvoo(a, a, m, j)
term(23) = term(23) + t2(a,c,m,j) * tvvoo(a, a, m, i)
term(24) = term(24) + t2(a,c,i,m) * tvoov(a, j, m, a)
term(25) = term(25) + t2(a,c,j,m) * tvoov(a, i, m, a)
term(26) = term(26) + t2(a,a,j,m) * tvvoo(c, a, m, i)
term(27) = term(27) + t2(a,a,i,m) * tvvoo(c, a, m, j)
term(28) = term(28) + t2(a,c,i,m) * toooo(m, k, k, j)
term(29) = term(29) + t2(a,c,m,k) * toooo(m, i, k, j)
term(30) = term(30) + t2(a,c,i,m) * toooo(m, j, k, k)
term(31) = term(31) + t2(a,c,m,j) * toooo(m, i, k, k)
term(32) = term(32) + t2(a,c,k,m) * toooo(m, j, k, i)
term(33) = term(33) + t2(a,c,j,m) * toooo(m, k, k, i)
term(34) = term(34) + t2(a,c,m,j) * toooo(m, k, k, i)
term(35) = term(35) + t2(a,c,m,k) * toooo(m, j, k, i)
end do 

term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * (-0.5d+0) 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * (-0.5d+0) 
term(28) = term(28) * (0.5d+0) 
term(29) = term(29) * (0.5d+0) 
term(30) = term(30) * (-0.5d+0) 
term(31) = term(31) * (-0.5d+0) 
term(32) = term(32) * (0.5d+0) 
term(33) = term(33) * (-0.5d+0) 
term(34) = term(34) * (0.5d+0) 
term(35) = term(35) * (-0.5d+0) 


    v1_eom_cc3_31_triplet_trans_aiajckak = 0.d+0
    do s = 0, 35
    v1_eom_cc3_31_triplet_trans_aiajckak = v1_eom_cc3_31_triplet_trans_aiajckak + term(s)
    end do

    end function v1_eom_cc3_31_triplet_trans_aiajckak
    function v1_eom_cc3_31_triplet_trans_aiajckaj(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: v1_eom_cc3_31_triplet_trans_aiajckaj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(c,e,k,i) * read_ftvvvv(a, e, a, a)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(c, a, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(c, a, a, e)
term(4) = term(4) + t2(c,e,k,i) * tvoov(a, j, j, e)
term(5) = term(5) + t2(c,e,j,i) * tvoov(a, k, j, e)
term(6) = term(6) + t2(c,e,j,k) * tvoov(a, i, j, e)
term(7) = term(7) + t2(c,e,k,j) * tvoov(a, i, j, e)
term(8) = term(8) + t2(a,e,k,i) * tvoov(c, j, j, e)
term(9) = term(9) + t2(a,e,i,k) * tvoov(c, j, j, e)
term(10) = term(10) + t2(a,e,j,i) * tvoov(c, k, j, e)
term(11) = term(11) + t2(a,e,i,j) * tvoov(c, k, j, e)
term(12) = term(12) + t2(a,e,i,k) * tvvoo(c, e, j, j)
term(13) = term(13) + t2(c,e,k,i) * tvvoo(a, e, j, j)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(c, e, j, k)
term(15) = term(15) + t2(c,e,j,i) * tvvoo(a, e, j, k)
term(16) = term(16) + t2(a,e,k,j) * tvvoo(c, e, j, i)
term(17) = term(17) + t2(a,e,j,k) * tvvoo(c, e, j, i)
term(18) = term(18) + t2(c,e,j,k) * tvvoo(a, e, j, i)
term(19) = term(19) + t2(c,e,k,j) * tvvoo(a, e, j, i)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (0.5d+0) 
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

do m = 1, nocc 
term(20) = term(20) + t2(a,c,i,m) * tvoov(a, k, m, a)
term(21) = term(21) + t2(a,c,k,m) * tvoov(a, i, m, a)
term(22) = term(22) + t2(a,c,m,k) * tvoov(a, i, m, a)
term(23) = term(23) + t2(a,a,i,m) * tvoov(c, k, m, a)
term(24) = term(24) + t2(a,c,i,m) * tvvoo(a, a, m, k)
term(25) = term(25) + t2(a,c,m,k) * tvvoo(a, a, m, i)
term(26) = term(26) + t2(a,a,k,m) * tvvoo(c, a, m, i)
term(27) = term(27) + t2(a,a,i,m) * tvvoo(c, a, m, k)
term(28) = term(28) + t2(a,c,i,m) * toooo(m, k, j, j)
term(29) = term(29) + t2(a,c,m,k) * toooo(m, i, j, j)
term(30) = term(30) + t2(a,c,i,m) * toooo(m, j, j, k)
term(31) = term(31) + t2(a,c,m,j) * toooo(m, i, j, k)
term(32) = term(32) + t2(a,c,k,m) * toooo(m, j, j, i)
term(33) = term(33) + t2(a,c,j,m) * toooo(m, k, j, i)
term(34) = term(34) + t2(a,c,m,j) * toooo(m, k, j, i)
term(35) = term(35) + t2(a,c,m,k) * toooo(m, j, j, i)
end do 

term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (-0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * (-0.5d+0) 
term(26) = term(26) * (0.5d+0) 
term(27) = term(27) * (0.5d+0) 
term(28) = term(28) * (0.5d+0) 
term(29) = term(29) * (0.5d+0) 
term(30) = term(30) * (-0.5d+0) 
term(31) = term(31) * (-0.5d+0) 
term(32) = term(32) * (0.5d+0) 
term(33) = term(33) * (-0.5d+0) 
term(34) = term(34) * (0.5d+0) 
term(35) = term(35) * (-0.5d+0) 


    v1_eom_cc3_31_triplet_trans_aiajckaj = 0.d+0
    do s = 0, 35
    v1_eom_cc3_31_triplet_trans_aiajckaj = v1_eom_cc3_31_triplet_trans_aiajckaj + term(s)
    end do

    end function v1_eom_cc3_31_triplet_trans_aiajckaj
    function v1_eom_cc3_31_triplet_trans_aiajckai(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: v1_eom_cc3_31_triplet_trans_aiajckai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,j) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(a,e,j,k) * read_ftvvvv(c, e, a, a)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(a, e, a, a)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(a, e, a, a)
term(4) = term(4) + t2(c,e,k,i) * tvoov(a, j, i, e)
term(5) = term(5) + t2(c,e,j,i) * tvoov(a, k, i, e)
term(6) = term(6) + t2(c,e,j,k) * tvoov(a, i, i, e)
term(7) = term(7) + t2(c,e,k,j) * tvoov(a, i, i, e)
term(8) = term(8) + t2(a,e,k,i) * tvoov(c, j, i, e)
term(9) = term(9) + t2(a,e,i,k) * tvoov(c, j, i, e)
term(10) = term(10) + t2(a,e,j,i) * tvoov(c, k, i, e)
term(11) = term(11) + t2(a,e,i,j) * tvoov(c, k, i, e)
term(12) = term(12) + t2(a,e,i,k) * tvvoo(c, e, i, j)
term(13) = term(13) + t2(c,e,k,i) * tvvoo(a, e, i, j)
term(14) = term(14) + t2(a,e,i,j) * tvvoo(c, e, i, k)
term(15) = term(15) + t2(c,e,j,i) * tvvoo(a, e, i, k)
term(16) = term(16) + t2(a,e,k,j) * tvvoo(c, e, i, i)
term(17) = term(17) + t2(a,e,j,k) * tvvoo(c, e, i, i)
term(18) = term(18) + t2(c,e,j,k) * tvvoo(a, e, i, i)
term(19) = term(19) + t2(c,e,k,j) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (0.5d+0) 
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

do m = 1, nocc 
term(20) = term(20) + t2(a,c,m,k) * tvoov(a, j, m, a)
term(21) = term(21) + t2(a,c,m,j) * tvoov(a, k, m, a)
term(22) = term(22) + t2(a,a,k,m) * tvoov(c, j, m, a)
term(23) = term(23) + t2(a,a,j,m) * tvoov(c, k, m, a)
term(24) = term(24) + t2(a,c,k,m) * tvvoo(a, a, m, j)
term(25) = term(25) + t2(a,c,j,m) * tvvoo(a, a, m, k)
term(26) = term(26) + t2(a,c,m,j) * tvvoo(a, a, m, k)
term(27) = term(27) + t2(a,c,m,k) * tvvoo(a, a, m, j)
term(28) = term(28) + t2(a,c,i,m) * toooo(m, k, i, j)
term(29) = term(29) + t2(a,c,m,k) * toooo(m, i, i, j)
term(30) = term(30) + t2(a,c,i,m) * toooo(m, j, i, k)
term(31) = term(31) + t2(a,c,m,j) * toooo(m, i, i, k)
term(32) = term(32) + t2(a,c,k,m) * toooo(m, j, i, i)
term(33) = term(33) + t2(a,c,j,m) * toooo(m, k, i, i)
term(34) = term(34) + t2(a,c,m,j) * toooo(m, k, i, i)
term(35) = term(35) + t2(a,c,m,k) * toooo(m, j, i, i)
end do 

term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
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
term(33) = term(33) * (-0.5d+0) 
term(34) = term(34) * (0.5d+0) 
term(35) = term(35) * (-0.5d+0) 


    v1_eom_cc3_31_triplet_trans_aiajckai = 0.d+0
    do s = 0, 35
    v1_eom_cc3_31_triplet_trans_aiajckai = v1_eom_cc3_31_triplet_trans_aiajckai + term(s)
    end do

    end function v1_eom_cc3_31_triplet_trans_aiajckai
    function v1_eom_cc3_31_triplet_trans_aiajckck(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: v1_eom_cc3_31_triplet_trans_aiajckck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,j,i) * read_ftvvvv(a, e, a, c)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(c, c, a, e)
term(3) = term(3) + t2(a,e,i,j) * read_ftvvvv(c, c, a, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(a, k, k, e)
term(5) = term(5) + t2(a,e,k,j) * tvoov(a, i, k, e)
term(6) = term(6) + t2(a,e,k,i) * tvvoo(a, e, k, j)
term(7) = term(7) + t2(a,e,i,k) * tvvoo(a, e, k, j)
term(8) = term(8) + t2(a,e,i,k) * tvoov(a, j, k, e)
term(9) = term(9) + t2(a,e,j,k) * tvoov(a, i, k, e)
term(10) = term(10) + t2(a,e,j,i) * tvvoo(a, e, k, k)
term(11) = term(11) + t2(a,e,i,j) * tvvoo(a, e, k, k)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,m,j) * tvoov(a, i, m, c)
term(13) = term(13) + t2(a,a,i,m) * tvoov(c, j, m, c)
term(14) = term(14) + t2(a,c,i,m) * tvvoo(a, c, m, j)
term(15) = term(15) + t2(a,c,m,j) * tvvoo(a, c, m, i)
term(16) = term(16) + t2(a,c,i,m) * tvoov(a, j, m, c)
term(17) = term(17) + t2(a,c,j,m) * tvoov(a, i, m, c)
term(18) = term(18) + t2(a,a,j,m) * tvvoo(c, c, m, i)
term(19) = term(19) + t2(a,a,i,m) * tvvoo(c, c, m, j)
term(20) = term(20) + t2(a,a,k,m) * toooo(m, i, k, j)
term(21) = term(21) + t2(a,a,i,m) * toooo(m, k, k, j)
term(22) = term(22) + t2(a,a,j,m) * toooo(m, i, k, k)
term(23) = term(23) + t2(a,a,i,m) * toooo(m, j, k, k)
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


    v1_eom_cc3_31_triplet_trans_aiajckck = 0.d+0
    do s = 0, 23
    v1_eom_cc3_31_triplet_trans_aiajckck = v1_eom_cc3_31_triplet_trans_aiajckck + term(s)
    end do

    end function v1_eom_cc3_31_triplet_trans_aiajckck
    function v1_eom_cc3_31_triplet_trans_aiajckcj(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: v1_eom_cc3_31_triplet_trans_aiajckcj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,k,i) * read_ftvvvv(a, e, a, c)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(c, c, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(c, c, a, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(a, k, j, e)
term(5) = term(5) + t2(a,e,k,j) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,k,i) * tvvoo(a, e, j, j)
term(7) = term(7) + t2(a,e,i,k) * tvvoo(a, e, j, j)
term(8) = term(8) + t2(a,e,i,k) * tvoov(a, j, j, e)
term(9) = term(9) + t2(a,e,j,k) * tvoov(a, i, j, e)
term(10) = term(10) + t2(a,e,j,i) * tvvoo(a, e, j, k)
term(11) = term(11) + t2(a,e,i,j) * tvvoo(a, e, j, k)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,i,m) * tvoov(a, k, m, c)
term(13) = term(13) + t2(a,c,k,m) * tvoov(a, i, m, c)
term(14) = term(14) + t2(a,c,m,k) * tvoov(a, i, m, c)
term(15) = term(15) + t2(a,a,i,m) * tvoov(c, k, m, c)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(a, c, m, k)
term(17) = term(17) + t2(a,c,m,k) * tvvoo(a, c, m, i)
term(18) = term(18) + t2(a,a,k,m) * tvvoo(c, c, m, i)
term(19) = term(19) + t2(a,a,i,m) * tvvoo(c, c, m, k)
term(20) = term(20) + t2(a,a,k,m) * toooo(m, i, j, j)
term(21) = term(21) + t2(a,a,i,m) * toooo(m, k, j, j)
term(22) = term(22) + t2(a,a,j,m) * toooo(m, i, j, k)
term(23) = term(23) + t2(a,a,i,m) * toooo(m, j, j, k)
end do 

term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (0.5d+0) 


    v1_eom_cc3_31_triplet_trans_aiajckcj = 0.d+0
    do s = 0, 23
    v1_eom_cc3_31_triplet_trans_aiajckcj = v1_eom_cc3_31_triplet_trans_aiajckcj + term(s)
    end do

    end function v1_eom_cc3_31_triplet_trans_aiajckcj
    function v1_eom_cc3_31_triplet_trans_aiajckci(t2, nocc, nactive, a, i, j, c, k) 
    real(F64) :: v1_eom_cc3_31_triplet_trans_aiajckci 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,j) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(a,e,j,k) * read_ftvvvv(c, e, a, c)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(a, e, a, c)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(a, e, a, c)
term(4) = term(4) + t2(a,e,i,j) * tvoov(a, k, i, e)
term(5) = term(5) + t2(a,e,k,j) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,k,i) * tvvoo(a, e, i, j)
term(7) = term(7) + t2(a,e,i,k) * tvvoo(a, e, i, j)
term(8) = term(8) + t2(a,e,i,k) * tvoov(a, j, i, e)
term(9) = term(9) + t2(a,e,j,k) * tvoov(a, i, i, e)
term(10) = term(10) + t2(a,e,j,i) * tvvoo(a, e, i, k)
term(11) = term(11) + t2(a,e,i,j) * tvvoo(a, e, i, k)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,m,k) * tvoov(a, j, m, c)
term(13) = term(13) + t2(a,c,m,j) * tvoov(a, k, m, c)
term(14) = term(14) + t2(a,a,k,m) * tvoov(c, j, m, c)
term(15) = term(15) + t2(a,a,j,m) * tvoov(c, k, m, c)
term(16) = term(16) + t2(a,c,k,m) * tvvoo(a, c, m, j)
term(17) = term(17) + t2(a,c,j,m) * tvvoo(a, c, m, k)
term(18) = term(18) + t2(a,c,m,j) * tvvoo(a, c, m, k)
term(19) = term(19) + t2(a,c,m,k) * tvvoo(a, c, m, j)
term(20) = term(20) + t2(a,a,k,m) * toooo(m, i, i, j)
term(21) = term(21) + t2(a,a,i,m) * toooo(m, k, i, j)
term(22) = term(22) + t2(a,a,j,m) * toooo(m, i, i, k)
term(23) = term(23) + t2(a,a,i,m) * toooo(m, j, i, k)
end do 

term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (0.5d+0) 


    v1_eom_cc3_31_triplet_trans_aiajckci = 0.d+0
    do s = 0, 23
    v1_eom_cc3_31_triplet_trans_aiajckci = v1_eom_cc3_31_triplet_trans_aiajckci + term(s)
    end do

    end function v1_eom_cc3_31_triplet_trans_aiajckci
    end module v1_eom_cc3_31_triplet_trans
    