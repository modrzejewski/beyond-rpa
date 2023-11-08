module v9_eom_cc3_31_triplet_trans

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
    
    function v9_eom_cc3_31_triplet_trans_aibjckbl(t2, nocc, nactive, a, i, j, c, k, l) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckbl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,k) * tvoov(a, i, l, e)
term(1) = term(1) + t2(c,e,k,j) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,k) * tvoov(c, j, l, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(c, k, l, e)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(c, e, l, k)
term(5) = term(5) + t2(a,e,i,k) * tvvoo(c, e, l, j)
term(6) = term(6) + t2(c,e,j,i) * tvvoo(a, e, l, k)
term(7) = term(7) + t2(c,e,k,i) * tvvoo(a, e, l, j)
end do 

term(0) = term(0) * (0.4999999999999998d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * (0.4999999999999998d+0) 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * (0.4999999999999998d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (0.4999999999999998d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,c,i,m) * toooo(m, j, l, k)
term(9) = term(9) + t2(a,c,i,m) * toooo(m, k, l, j)
term(10) = term(10) + t2(a,c,m,j) * toooo(m, i, l, k)
term(11) = term(11) + t2(a,c,m,k) * toooo(m, i, l, j)
end do 

term(8) = term(8) * (-0.4999999999999998d+0) 
term(9) = term(9) * (0.49999999999999983d+0) 
term(10) = term(10) * (-0.4999999999999998d+0) 
term(11) = term(11) * (0.49999999999999983d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckbl = 0.d+0
    do s = 0, 11
    v9_eom_cc3_31_triplet_trans_aibjckbl = v9_eom_cc3_31_triplet_trans_aibjckbl + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckbl
    function v9_eom_cc3_31_triplet_trans_aibjckal(t2, nocc, nactive, i, b, j, c, k, l) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckal 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * tvoov(b, k, l, e)
term(1) = term(1) + t2(b,e,k,i) * tvoov(c, j, l, e)
term(2) = term(2) + t2(b,e,k,j) * tvvoo(c, e, l, i)
term(3) = term(3) + t2(c,e,j,k) * tvvoo(b, e, l, i)
term(4) = term(4) + t2(b,e,j,i) * tvoov(c, k, l, e)
term(5) = term(5) + t2(c,e,k,i) * tvoov(b, j, l, e)
term(6) = term(6) + t2(c,e,k,j) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(b,e,j,k) * tvvoo(c, e, l, i)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5000000000000001d+0) 
term(5) = term(5) * (0.5000000000000001d+0) 
term(6) = term(6) * (0.5000000000000001d+0) 
term(7) = term(7) * (0.5000000000000001d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,k,m) * toooo(m, j, l, i)
term(9) = term(9) + t2(b,c,m,j) * toooo(m, k, l, i)
term(10) = term(10) + t2(b,c,m,k) * toooo(m, j, l, i)
term(11) = term(11) + t2(b,c,j,m) * toooo(m, k, l, i)
end do 

term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5000000000000001d+0) 
term(11) = term(11) * (-0.5000000000000001d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckal = 0.d+0
    do s = 0, 11
    v9_eom_cc3_31_triplet_trans_aibjckal = v9_eom_cc3_31_triplet_trans_aibjckal + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckal
    function v9_eom_cc3_31_triplet_trans_aibjckcl(t2, nocc, nactive, a, i, b, j, k, l) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,j) * tvoov(b, k, l, e)
term(2) = term(2) + t2(a,e,i,k) * tvvoo(b, e, l, j)
term(3) = term(3) + t2(b,e,k,i) * tvvoo(a, e, l, j)
term(4) = term(4) + t2(b,e,j,k) * tvoov(a, i, l, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(b, j, l, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(b, e, l, k)
term(7) = term(7) + t2(b,e,j,i) * tvvoo(a, e, l, k)
end do 

term(0) = term(0) * (0.49999999999999983d+0) 
term(1) = term(1) * (0.49999999999999983d+0) 
term(2) = term(2) * (0.49999999999999983d+0) 
term(3) = term(3) * (0.49999999999999983d+0) 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,b,i,m) * toooo(m, k, l, j)
term(9) = term(9) + t2(a,b,m,k) * toooo(m, i, l, j)
term(10) = term(10) + t2(a,b,i,m) * toooo(m, j, l, k)
term(11) = term(11) + t2(a,b,m,j) * toooo(m, i, l, k)
end do 

term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (0.49999999999999983d+0) 
term(11) = term(11) * (0.49999999999999983d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckcl = 0.d+0
    do s = 0, 11
    v9_eom_cc3_31_triplet_trans_aibjckcl = v9_eom_cc3_31_triplet_trans_aibjckcl + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckcl
    function v9_eom_cc3_31_triplet_trans_aibjckdk(t2, nocc, nactive, a, i, b, j, c, d) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckdk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, d)
term(1) = term(1) + t2(c,e,j,i) * read_ftvvvv(b, d, a, e)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, d, b, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * (-0.4999999999999998d+0) 
term(2) = term(2) * (0.49999999999999983d+0) 
term(3) = term(3) * (0.49999999999999983d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,m,j) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,b,i,m) * tvoov(c, j, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvvoo(b, d, m, j)
term(7) = term(7) + t2(a,c,m,j) * tvvoo(b, d, m, i)
term(8) = term(8) + t2(b,c,j,m) * tvoov(a, i, m, d)
term(9) = term(9) + t2(a,c,i,m) * tvoov(b, j, m, d)
term(10) = term(10) + t2(a,b,i,m) * tvvoo(c, d, m, j)
term(11) = term(11) + t2(a,b,m,j) * tvvoo(c, d, m, i)
end do 

term(4) = term(4) * (0.4999999999999998d+0) 
term(5) = term(5) * (0.4999999999999998d+0) 
term(6) = term(6) * (0.4999999999999998d+0) 
term(7) = term(7) * (0.4999999999999998d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckdk = 0.d+0
    do s = 0, 11
    v9_eom_cc3_31_triplet_trans_aibjckdk = v9_eom_cc3_31_triplet_trans_aibjckdk + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckdk
    function v9_eom_cc3_31_triplet_trans_aibjckdj(t2, nocc, nactive, a, i, b, c, k, d) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckdj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, b, d)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(c, d, b, e)
term(2) = term(2) + t2(c,e,k,i) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(b,e,k,i) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * (0.49999999999999983d+0) 
term(3) = term(3) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,k,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(b,c,m,k) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvoov(b, k, m, d)
term(7) = term(7) + t2(a,b,i,m) * tvoov(c, k, m, d)
term(8) = term(8) + t2(a,c,i,m) * tvvoo(b, d, m, k)
term(9) = term(9) + t2(a,b,i,m) * tvvoo(c, d, m, k)
term(10) = term(10) + t2(a,c,m,k) * tvvoo(b, d, m, i)
term(11) = term(11) + t2(a,b,m,k) * tvvoo(c, d, m, i)
end do 

term(4) = term(4) * (0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (0.49999999999999983d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckdj = 0.d+0
    do s = 0, 11
    v9_eom_cc3_31_triplet_trans_aibjckdj = v9_eom_cc3_31_triplet_trans_aibjckdj + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckdj
    function v9_eom_cc3_31_triplet_trans_aibjckdi(t2, nocc, nactive, a, b, j, c, k, d) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckdi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(c,e,j,k) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(c,e,k,j) * read_ftvvvv(b, e, a, d)
term(3) = term(3) + t2(b,e,j,k) * read_ftvvvv(c, e, a, d)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5000000000000001d+0) 
term(3) = term(3) * (-0.5000000000000001d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,m,j) * tvoov(b, k, m, d)
term(5) = term(5) + t2(a,b,m,k) * tvoov(c, j, m, d)
term(6) = term(6) + t2(b,c,k,m) * tvvoo(a, d, m, j)
term(7) = term(7) + t2(b,c,m,j) * tvvoo(a, d, m, k)
term(8) = term(8) + t2(a,b,m,j) * tvoov(c, k, m, d)
term(9) = term(9) + t2(a,c,m,k) * tvoov(b, j, m, d)
term(10) = term(10) + t2(b,c,m,k) * tvvoo(a, d, m, j)
term(11) = term(11) + t2(b,c,j,m) * tvvoo(a, d, m, k)
end do 

term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (0.5000000000000001d+0) 
term(9) = term(9) * (0.5000000000000001d+0) 
term(10) = term(10) * (0.5000000000000001d+0) 
term(11) = term(11) * (0.5000000000000001d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckdi = 0.d+0
    do s = 0, 11
    v9_eom_cc3_31_triplet_trans_aibjckdi = v9_eom_cc3_31_triplet_trans_aibjckdi + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckdi
    function v9_eom_cc3_31_triplet_trans_aibjckbk(t2, nocc, nactive, a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckbk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, b)
term(1) = term(1) + t2(c,e,j,i) * read_ftvvvv(b, b, a, e)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, b, b, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, b, a, e)
term(4) = term(4) + t2(c,e,j,k) * tvoov(a, i, k, e)
term(5) = term(5) + t2(c,e,k,j) * tvoov(a, i, k, e)
term(6) = term(6) + t2(a,e,i,k) * tvoov(c, j, k, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(c, k, k, e)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(c, e, k, k)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(c, e, k, j)
term(10) = term(10) + t2(c,e,j,i) * tvvoo(a, e, k, k)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(a, e, k, j)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * (-0.4999999999999998d+0) 
term(2) = term(2) * (0.49999999999999983d+0) 
term(3) = term(3) * (0.49999999999999983d+0) 
term(4) = term(4) * (0.4999999999999998d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (0.4999999999999998d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * (0.4999999999999998d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (0.4999999999999998d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,m,j) * tvoov(a, i, m, b)
term(13) = term(13) + t2(a,b,i,m) * tvoov(c, j, m, b)
term(14) = term(14) + t2(a,c,i,m) * tvvoo(b, b, m, j)
term(15) = term(15) + t2(a,c,m,j) * tvvoo(b, b, m, i)
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, i, m, b)
term(17) = term(17) + t2(a,c,i,m) * tvoov(b, j, m, b)
term(18) = term(18) + t2(a,b,i,m) * tvvoo(c, b, m, j)
term(19) = term(19) + t2(a,b,m,j) * tvvoo(c, b, m, i)
term(20) = term(20) + t2(a,c,i,m) * toooo(m, j, k, k)
term(21) = term(21) + t2(a,c,i,m) * toooo(m, k, k, j)
term(22) = term(22) + t2(a,c,m,j) * toooo(m, i, k, k)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, i, k, j)
end do 

term(12) = term(12) * (0.4999999999999998d+0) 
term(13) = term(13) * (0.4999999999999998d+0) 
term(14) = term(14) * (0.4999999999999998d+0) 
term(15) = term(15) * (0.4999999999999998d+0) 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * (-0.4999999999999998d+0) 
term(21) = term(21) * (0.49999999999999983d+0) 
term(22) = term(22) * (-0.4999999999999998d+0) 
term(23) = term(23) * (0.49999999999999983d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckbk = 0.d+0
    do s = 0, 23
    v9_eom_cc3_31_triplet_trans_aibjckbk = v9_eom_cc3_31_triplet_trans_aibjckbk + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckbk
    function v9_eom_cc3_31_triplet_trans_aibjckbj(t2, nocc, nactive, a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckbj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, b, b)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(c, b, b, e)
term(2) = term(2) + t2(c,e,k,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(b,e,k,i) * read_ftvvvv(c, b, a, e)
term(4) = term(4) + t2(c,e,j,k) * tvoov(a, i, j, e)
term(5) = term(5) + t2(c,e,k,j) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,i,k) * tvoov(c, j, j, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(c, k, j, e)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(c, e, j, k)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(c, e, j, j)
term(10) = term(10) + t2(c,e,j,i) * tvvoo(a, e, j, k)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(a, e, j, j)
end do 

term(0) = term(0) * (0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * (0.49999999999999983d+0) 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * (0.4999999999999998d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (0.4999999999999998d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * (0.4999999999999998d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (0.4999999999999998d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,k,m) * tvoov(a, i, m, b)
term(13) = term(13) + t2(b,c,m,k) * tvoov(a, i, m, b)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, k, m, b)
term(15) = term(15) + t2(a,b,i,m) * tvoov(c, k, m, b)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(b, b, m, k)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(c, b, m, k)
term(18) = term(18) + t2(a,c,m,k) * tvvoo(b, b, m, i)
term(19) = term(19) + t2(a,b,m,k) * tvvoo(c, b, m, i)
term(20) = term(20) + t2(a,c,i,m) * toooo(m, j, j, k)
term(21) = term(21) + t2(a,c,i,m) * toooo(m, k, j, j)
term(22) = term(22) + t2(a,c,m,j) * toooo(m, i, j, k)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, i, j, j)
end do 

term(12) = term(12) * (0.49999999999999983d+0) 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * (0.49999999999999983d+0) 
term(15) = term(15) * (-0.49999999999999983d+0) 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (0.49999999999999983d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * (0.49999999999999983d+0) 
term(20) = term(20) * (-0.4999999999999998d+0) 
term(21) = term(21) * (0.49999999999999983d+0) 
term(22) = term(22) * (-0.4999999999999998d+0) 
term(23) = term(23) * (0.49999999999999983d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckbj = 0.d+0
    do s = 0, 23
    v9_eom_cc3_31_triplet_trans_aibjckbj = v9_eom_cc3_31_triplet_trans_aibjckbj + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckbj
    function v9_eom_cc3_31_triplet_trans_aibjckbi(t2, nocc, nactive, a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckbi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(c, e, a, b)
term(1) = term(1) + t2(c,e,j,k) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(c,e,k,j) * read_ftvvvv(b, e, a, b)
term(3) = term(3) + t2(b,e,j,k) * read_ftvvvv(c, e, a, b)
term(4) = term(4) + t2(c,e,j,k) * tvoov(a, i, i, e)
term(5) = term(5) + t2(c,e,k,j) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,i,k) * tvoov(c, j, i, e)
term(7) = term(7) + t2(a,e,i,j) * tvoov(c, k, i, e)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(c, e, i, k)
term(9) = term(9) + t2(a,e,i,k) * tvvoo(c, e, i, j)
term(10) = term(10) + t2(c,e,j,i) * tvvoo(a, e, i, k)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(a, e, i, j)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5000000000000001d+0) 
term(3) = term(3) * (-0.5000000000000001d+0) 
term(4) = term(4) * (0.4999999999999998d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (0.4999999999999998d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * (0.4999999999999998d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (0.4999999999999998d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,m,j) * tvoov(b, k, m, b)
term(13) = term(13) + t2(a,b,m,k) * tvoov(c, j, m, b)
term(14) = term(14) + t2(b,c,k,m) * tvvoo(a, b, m, j)
term(15) = term(15) + t2(b,c,m,j) * tvvoo(a, b, m, k)
term(16) = term(16) + t2(a,b,m,j) * tvoov(c, k, m, b)
term(17) = term(17) + t2(a,c,m,k) * tvoov(b, j, m, b)
term(18) = term(18) + t2(b,c,m,k) * tvvoo(a, b, m, j)
term(19) = term(19) + t2(b,c,j,m) * tvvoo(a, b, m, k)
term(20) = term(20) + t2(a,c,i,m) * toooo(m, j, i, k)
term(21) = term(21) + t2(a,c,i,m) * toooo(m, k, i, j)
term(22) = term(22) + t2(a,c,m,j) * toooo(m, i, i, k)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, i, i, j)
end do 

term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (0.5000000000000001d+0) 
term(17) = term(17) * (0.5000000000000001d+0) 
term(18) = term(18) * (0.5000000000000001d+0) 
term(19) = term(19) * (0.5000000000000001d+0) 
term(20) = term(20) * (-0.4999999999999998d+0) 
term(21) = term(21) * (0.49999999999999983d+0) 
term(22) = term(22) * (-0.4999999999999998d+0) 
term(23) = term(23) * (0.49999999999999983d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckbi = 0.d+0
    do s = 0, 23
    v9_eom_cc3_31_triplet_trans_aibjckbi = v9_eom_cc3_31_triplet_trans_aibjckbi + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckbi
    function v9_eom_cc3_31_triplet_trans_aibjckak(t2, nocc, nactive, a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckak 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, a)
term(1) = term(1) + t2(c,e,j,i) * read_ftvvvv(b, a, a, e)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, a, b, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, a, a, e)
term(4) = term(4) + t2(c,e,j,i) * tvoov(b, k, k, e)
term(5) = term(5) + t2(b,e,k,i) * tvoov(c, j, k, e)
term(6) = term(6) + t2(b,e,k,j) * tvvoo(c, e, k, i)
term(7) = term(7) + t2(c,e,j,k) * tvvoo(b, e, k, i)
term(8) = term(8) + t2(b,e,j,i) * tvoov(c, k, k, e)
term(9) = term(9) + t2(c,e,k,i) * tvoov(b, j, k, e)
term(10) = term(10) + t2(c,e,k,j) * tvvoo(b, e, k, i)
term(11) = term(11) + t2(b,e,j,k) * tvvoo(c, e, k, i)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * (-0.4999999999999998d+0) 
term(2) = term(2) * (0.49999999999999983d+0) 
term(3) = term(3) * (0.49999999999999983d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (0.5000000000000001d+0) 
term(9) = term(9) * (0.5000000000000001d+0) 
term(10) = term(10) * (0.5000000000000001d+0) 
term(11) = term(11) * (0.5000000000000001d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,m,j) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,b,i,m) * tvoov(c, j, m, a)
term(14) = term(14) + t2(a,c,i,m) * tvvoo(b, a, m, j)
term(15) = term(15) + t2(a,c,m,j) * tvvoo(b, a, m, i)
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, i, m, a)
term(17) = term(17) + t2(a,c,i,m) * tvoov(b, j, m, a)
term(18) = term(18) + t2(a,b,i,m) * tvvoo(c, a, m, j)
term(19) = term(19) + t2(a,b,m,j) * tvvoo(c, a, m, i)
term(20) = term(20) + t2(b,c,k,m) * toooo(m, j, k, i)
term(21) = term(21) + t2(b,c,m,j) * toooo(m, k, k, i)
term(22) = term(22) + t2(b,c,m,k) * toooo(m, j, k, i)
term(23) = term(23) + t2(b,c,j,m) * toooo(m, k, k, i)
end do 

term(12) = term(12) * (0.4999999999999998d+0) 
term(13) = term(13) * (0.4999999999999998d+0) 
term(14) = term(14) * (0.4999999999999998d+0) 
term(15) = term(15) * (0.4999999999999998d+0) 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (-0.5000000000000001d+0) 
term(23) = term(23) * (-0.5000000000000001d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckak = 0.d+0
    do s = 0, 23
    v9_eom_cc3_31_triplet_trans_aibjckak = v9_eom_cc3_31_triplet_trans_aibjckak + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckak
    function v9_eom_cc3_31_triplet_trans_aibjckaj(t2, nocc, nactive, a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckaj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, b, a)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(c, a, b, e)
term(2) = term(2) + t2(c,e,k,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(b,e,k,i) * read_ftvvvv(c, a, a, e)
term(4) = term(4) + t2(c,e,j,i) * tvoov(b, k, j, e)
term(5) = term(5) + t2(b,e,k,i) * tvoov(c, j, j, e)
term(6) = term(6) + t2(b,e,k,j) * tvvoo(c, e, j, i)
term(7) = term(7) + t2(c,e,j,k) * tvvoo(b, e, j, i)
term(8) = term(8) + t2(b,e,j,i) * tvoov(c, k, j, e)
term(9) = term(9) + t2(c,e,k,i) * tvoov(b, j, j, e)
term(10) = term(10) + t2(c,e,k,j) * tvvoo(b, e, j, i)
term(11) = term(11) + t2(b,e,j,k) * tvvoo(c, e, j, i)
end do 

term(0) = term(0) * (0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * (0.49999999999999983d+0) 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (0.5000000000000001d+0) 
term(9) = term(9) * (0.5000000000000001d+0) 
term(10) = term(10) * (0.5000000000000001d+0) 
term(11) = term(11) * (0.5000000000000001d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,k,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(b,c,m,k) * tvoov(a, i, m, a)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, k, m, a)
term(15) = term(15) + t2(a,b,i,m) * tvoov(c, k, m, a)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(b, a, m, k)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(c, a, m, k)
term(18) = term(18) + t2(a,c,m,k) * tvvoo(b, a, m, i)
term(19) = term(19) + t2(a,b,m,k) * tvvoo(c, a, m, i)
term(20) = term(20) + t2(b,c,k,m) * toooo(m, j, j, i)
term(21) = term(21) + t2(b,c,m,j) * toooo(m, k, j, i)
term(22) = term(22) + t2(b,c,m,k) * toooo(m, j, j, i)
term(23) = term(23) + t2(b,c,j,m) * toooo(m, k, j, i)
end do 

term(12) = term(12) * (0.49999999999999983d+0) 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * (0.49999999999999983d+0) 
term(15) = term(15) * (-0.49999999999999983d+0) 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (0.49999999999999983d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * (0.49999999999999983d+0) 
term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (-0.5000000000000001d+0) 
term(23) = term(23) * (-0.5000000000000001d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckaj = 0.d+0
    do s = 0, 23
    v9_eom_cc3_31_triplet_trans_aibjckaj = v9_eom_cc3_31_triplet_trans_aibjckaj + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckaj
    function v9_eom_cc3_31_triplet_trans_aibjckai(t2, nocc, nactive, a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(c,e,j,k) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(c,e,k,j) * read_ftvvvv(b, e, a, a)
term(3) = term(3) + t2(b,e,j,k) * read_ftvvvv(c, e, a, a)
term(4) = term(4) + t2(c,e,j,i) * tvoov(b, k, i, e)
term(5) = term(5) + t2(b,e,k,i) * tvoov(c, j, i, e)
term(6) = term(6) + t2(b,e,k,j) * tvvoo(c, e, i, i)
term(7) = term(7) + t2(c,e,j,k) * tvvoo(b, e, i, i)
term(8) = term(8) + t2(b,e,j,i) * tvoov(c, k, i, e)
term(9) = term(9) + t2(c,e,k,i) * tvoov(b, j, i, e)
term(10) = term(10) + t2(c,e,k,j) * tvvoo(b, e, i, i)
term(11) = term(11) + t2(b,e,j,k) * tvvoo(c, e, i, i)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5000000000000001d+0) 
term(3) = term(3) * (-0.5000000000000001d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (0.5000000000000001d+0) 
term(9) = term(9) * (0.5000000000000001d+0) 
term(10) = term(10) * (0.5000000000000001d+0) 
term(11) = term(11) * (0.5000000000000001d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,m,j) * tvoov(b, k, m, a)
term(13) = term(13) + t2(a,b,m,k) * tvoov(c, j, m, a)
term(14) = term(14) + t2(b,c,k,m) * tvvoo(a, a, m, j)
term(15) = term(15) + t2(b,c,m,j) * tvvoo(a, a, m, k)
term(16) = term(16) + t2(a,b,m,j) * tvoov(c, k, m, a)
term(17) = term(17) + t2(a,c,m,k) * tvoov(b, j, m, a)
term(18) = term(18) + t2(b,c,m,k) * tvvoo(a, a, m, j)
term(19) = term(19) + t2(b,c,j,m) * tvvoo(a, a, m, k)
term(20) = term(20) + t2(b,c,k,m) * toooo(m, j, i, i)
term(21) = term(21) + t2(b,c,m,j) * toooo(m, k, i, i)
term(22) = term(22) + t2(b,c,m,k) * toooo(m, j, i, i)
term(23) = term(23) + t2(b,c,j,m) * toooo(m, k, i, i)
end do 

term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (0.5000000000000001d+0) 
term(17) = term(17) * (0.5000000000000001d+0) 
term(18) = term(18) * (0.5000000000000001d+0) 
term(19) = term(19) * (0.5000000000000001d+0) 
term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (-0.5000000000000001d+0) 
term(23) = term(23) * (-0.5000000000000001d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckai = 0.d+0
    do s = 0, 23
    v9_eom_cc3_31_triplet_trans_aibjckai = v9_eom_cc3_31_triplet_trans_aibjckai + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckai
    function v9_eom_cc3_31_triplet_trans_aibjckck(t2, nocc, nactive, a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, c)
term(1) = term(1) + t2(c,e,j,i) * read_ftvvvv(b, c, a, e)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(c, c, b, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, c, a, e)
term(4) = term(4) + t2(b,e,k,j) * tvoov(a, i, k, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(b, k, k, e)
term(6) = term(6) + t2(a,e,i,k) * tvvoo(b, e, k, j)
term(7) = term(7) + t2(b,e,k,i) * tvvoo(a, e, k, j)
term(8) = term(8) + t2(b,e,j,k) * tvoov(a, i, k, e)
term(9) = term(9) + t2(a,e,i,k) * tvoov(b, j, k, e)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(b, e, k, k)
term(11) = term(11) + t2(b,e,j,i) * tvvoo(a, e, k, k)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * (-0.4999999999999998d+0) 
term(2) = term(2) * (0.49999999999999983d+0) 
term(3) = term(3) * (0.49999999999999983d+0) 
term(4) = term(4) * (0.49999999999999983d+0) 
term(5) = term(5) * (0.49999999999999983d+0) 
term(6) = term(6) * (0.49999999999999983d+0) 
term(7) = term(7) * (0.49999999999999983d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,m,j) * tvoov(a, i, m, c)
term(13) = term(13) + t2(a,b,i,m) * tvoov(c, j, m, c)
term(14) = term(14) + t2(a,c,i,m) * tvvoo(b, c, m, j)
term(15) = term(15) + t2(a,c,m,j) * tvvoo(b, c, m, i)
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, i, m, c)
term(17) = term(17) + t2(a,c,i,m) * tvoov(b, j, m, c)
term(18) = term(18) + t2(a,b,i,m) * tvvoo(c, c, m, j)
term(19) = term(19) + t2(a,b,m,j) * tvvoo(c, c, m, i)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, k, k, j)
term(21) = term(21) + t2(a,b,m,k) * toooo(m, i, k, j)
term(22) = term(22) + t2(a,b,i,m) * toooo(m, j, k, k)
term(23) = term(23) + t2(a,b,m,j) * toooo(m, i, k, k)
end do 

term(12) = term(12) * (0.4999999999999998d+0) 
term(13) = term(13) * (0.4999999999999998d+0) 
term(14) = term(14) * (0.4999999999999998d+0) 
term(15) = term(15) * (0.4999999999999998d+0) 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * (-0.49999999999999983d+0) 
term(22) = term(22) * (0.49999999999999983d+0) 
term(23) = term(23) * (0.49999999999999983d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckck = 0.d+0
    do s = 0, 23
    v9_eom_cc3_31_triplet_trans_aibjckck = v9_eom_cc3_31_triplet_trans_aibjckck + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckck
    function v9_eom_cc3_31_triplet_trans_aibjckcj(t2, nocc, nactive, a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckcj 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, e, b, c)
term(1) = term(1) + t2(a,e,i,k) * read_ftvvvv(c, c, b, e)
term(2) = term(2) + t2(c,e,k,i) * read_ftvvvv(b, c, a, e)
term(3) = term(3) + t2(b,e,k,i) * read_ftvvvv(c, c, a, e)
term(4) = term(4) + t2(b,e,k,j) * tvoov(a, i, j, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(b, k, j, e)
term(6) = term(6) + t2(a,e,i,k) * tvvoo(b, e, j, j)
term(7) = term(7) + t2(b,e,k,i) * tvvoo(a, e, j, j)
term(8) = term(8) + t2(b,e,j,k) * tvoov(a, i, j, e)
term(9) = term(9) + t2(a,e,i,k) * tvoov(b, j, j, e)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(b, e, j, k)
term(11) = term(11) + t2(b,e,j,i) * tvvoo(a, e, j, k)
end do 

term(0) = term(0) * (0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * (0.49999999999999983d+0) 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * (0.49999999999999983d+0) 
term(5) = term(5) * (0.49999999999999983d+0) 
term(6) = term(6) * (0.49999999999999983d+0) 
term(7) = term(7) * (0.49999999999999983d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,k,m) * tvoov(a, i, m, c)
term(13) = term(13) + t2(b,c,m,k) * tvoov(a, i, m, c)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, k, m, c)
term(15) = term(15) + t2(a,b,i,m) * tvoov(c, k, m, c)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(b, c, m, k)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(c, c, m, k)
term(18) = term(18) + t2(a,c,m,k) * tvvoo(b, c, m, i)
term(19) = term(19) + t2(a,b,m,k) * tvvoo(c, c, m, i)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, k, j, j)
term(21) = term(21) + t2(a,b,m,k) * toooo(m, i, j, j)
term(22) = term(22) + t2(a,b,i,m) * toooo(m, j, j, k)
term(23) = term(23) + t2(a,b,m,j) * toooo(m, i, j, k)
end do 

term(12) = term(12) * (0.49999999999999983d+0) 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * (0.49999999999999983d+0) 
term(15) = term(15) * (-0.49999999999999983d+0) 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (0.49999999999999983d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * (0.49999999999999983d+0) 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * (-0.49999999999999983d+0) 
term(22) = term(22) * (0.49999999999999983d+0) 
term(23) = term(23) * (0.49999999999999983d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckcj = 0.d+0
    do s = 0, 23
    v9_eom_cc3_31_triplet_trans_aibjckcj = v9_eom_cc3_31_triplet_trans_aibjckcj + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckcj
    function v9_eom_cc3_31_triplet_trans_aibjckci(t2, nocc, nactive, a, i, b, j, c, k) 
    real(F64) :: v9_eom_cc3_31_triplet_trans_aibjckci 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,j,k) * read_ftvvvv(b, e, a, c)
term(2) = term(2) + t2(c,e,k,j) * read_ftvvvv(b, e, a, c)
term(3) = term(3) + t2(b,e,j,k) * read_ftvvvv(c, e, a, c)
term(4) = term(4) + t2(b,e,k,j) * tvoov(a, i, i, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(b, k, i, e)
term(6) = term(6) + t2(a,e,i,k) * tvvoo(b, e, i, j)
term(7) = term(7) + t2(b,e,k,i) * tvvoo(a, e, i, j)
term(8) = term(8) + t2(b,e,j,k) * tvoov(a, i, i, e)
term(9) = term(9) + t2(a,e,i,k) * tvoov(b, j, i, e)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(b, e, i, k)
term(11) = term(11) + t2(b,e,j,i) * tvvoo(a, e, i, k)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5000000000000001d+0) 
term(3) = term(3) * (-0.5000000000000001d+0) 
term(4) = term(4) * (0.49999999999999983d+0) 
term(5) = term(5) * (0.49999999999999983d+0) 
term(6) = term(6) * (0.49999999999999983d+0) 
term(7) = term(7) * (0.49999999999999983d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,m,j) * tvoov(b, k, m, c)
term(13) = term(13) + t2(a,b,m,k) * tvoov(c, j, m, c)
term(14) = term(14) + t2(b,c,k,m) * tvvoo(a, c, m, j)
term(15) = term(15) + t2(b,c,m,j) * tvvoo(a, c, m, k)
term(16) = term(16) + t2(a,b,m,j) * tvoov(c, k, m, c)
term(17) = term(17) + t2(a,c,m,k) * tvoov(b, j, m, c)
term(18) = term(18) + t2(b,c,m,k) * tvvoo(a, c, m, j)
term(19) = term(19) + t2(b,c,j,m) * tvvoo(a, c, m, k)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, k, i, j)
term(21) = term(21) + t2(a,b,m,k) * toooo(m, i, i, j)
term(22) = term(22) + t2(a,b,i,m) * toooo(m, j, i, k)
term(23) = term(23) + t2(a,b,m,j) * toooo(m, i, i, k)
end do 

term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (0.5000000000000001d+0) 
term(17) = term(17) * (0.5000000000000001d+0) 
term(18) = term(18) * (0.5000000000000001d+0) 
term(19) = term(19) * (0.5000000000000001d+0) 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * (-0.49999999999999983d+0) 
term(22) = term(22) * (0.49999999999999983d+0) 
term(23) = term(23) * (0.49999999999999983d+0) 


    v9_eom_cc3_31_triplet_trans_aibjckci = 0.d+0
    do s = 0, 23
    v9_eom_cc3_31_triplet_trans_aibjckci = v9_eom_cc3_31_triplet_trans_aibjckci + term(s)
    end do

    end function v9_eom_cc3_31_triplet_trans_aibjckci
    end module v9_eom_cc3_31_triplet_trans
    