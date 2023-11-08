module v5_eom_cc3_31_triplet_trans

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
    
    function v5_eom_cc3_31_triplet_trans_aiaickal(t2, nocc, nactive, a, i, c, k, l) 
    real(F64) :: v5_eom_cc3_31_triplet_trans_aiaickal 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,k) * tvoov(a, i, l, e)
term(1) = term(1) + t2(c,e,i,i) * tvoov(a, k, l, e)
term(2) = term(2) + t2(a,e,i,k) * tvoov(c, i, l, e)
term(3) = term(3) + t2(a,e,k,i) * tvoov(c, i, l, e)
term(4) = term(4) + t2(a,e,k,i) * tvvoo(c, e, l, i)
term(5) = term(5) + t2(c,e,i,k) * tvvoo(a, e, l, i)
term(6) = term(6) + t2(a,e,i,i) * tvvoo(c, e, l, k)
term(7) = term(7) + t2(c,e,i,i) * tvvoo(a, e, l, k)
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
term(8) = term(8) + t2(a,c,k,m) * toooo(m, i, l, i)
term(9) = term(9) + t2(a,c,m,i) * toooo(m, k, l, i)
term(10) = term(10) + t2(a,c,i,m) * toooo(m, i, l, k)
term(11) = term(11) + t2(a,c,m,i) * toooo(m, i, l, k)
end do 

term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 


    v5_eom_cc3_31_triplet_trans_aiaickal = 0.d+0
    do s = 0, 11
    v5_eom_cc3_31_triplet_trans_aiaickal = v5_eom_cc3_31_triplet_trans_aiaickal + term(s)
    end do

    end function v5_eom_cc3_31_triplet_trans_aiaickal
    function v5_eom_cc3_31_triplet_trans_aiaickcl(t2, nocc, nactive, a, i, k, l) 
    real(F64) :: v5_eom_cc3_31_triplet_trans_aiaickcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,i) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,i) * tvoov(a, k, l, e)
term(2) = term(2) + t2(a,e,i,k) * tvvoo(a, e, l, i)
term(3) = term(3) + t2(a,e,k,i) * tvvoo(a, e, l, i)
term(4) = term(4) + t2(a,e,i,k) * tvoov(a, i, l, e)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(a, e, l, k)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,a,i,m) * toooo(m, k, l, i)
term(7) = term(7) + t2(a,a,k,m) * toooo(m, i, l, i)
term(8) = term(8) + t2(a,a,i,m) * toooo(m, i, l, k)
end do 

term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 


    v5_eom_cc3_31_triplet_trans_aiaickcl = 0.d+0
    do s = 0, 8
    v5_eom_cc3_31_triplet_trans_aiaickcl = v5_eom_cc3_31_triplet_trans_aiaickcl + term(s)
    end do

    end function v5_eom_cc3_31_triplet_trans_aiaickcl
    function v5_eom_cc3_31_triplet_trans_aiaickdk(t2, nocc, nactive, a, i, c, d) 
    real(F64) :: v5_eom_cc3_31_triplet_trans_aiaickdk 
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

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,c,m,i) * tvoov(a, i, m, d)
term(4) = term(4) + t2(a,a,i,m) * tvoov(c, i, m, d)
term(5) = term(5) + t2(a,c,i,m) * tvvoo(a, d, m, i)
term(6) = term(6) + t2(a,c,m,i) * tvvoo(a, d, m, i)
term(7) = term(7) + t2(a,c,i,m) * tvoov(a, i, m, d)
term(8) = term(8) + t2(a,a,i,m) * tvvoo(c, d, m, i)
end do 

term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (-1.0d+0) 
term(8) = term(8) * (-1.0d+0) 


    v5_eom_cc3_31_triplet_trans_aiaickdk = 0.d+0
    do s = 0, 8
    v5_eom_cc3_31_triplet_trans_aiaickdk = v5_eom_cc3_31_triplet_trans_aiaickdk + term(s)
    end do

    end function v5_eom_cc3_31_triplet_trans_aiaickdk
    function v5_eom_cc3_31_triplet_trans_aiaickdi(t2, nocc, nactive, a, i, c, k, d) 
    real(F64) :: v5_eom_cc3_31_triplet_trans_aiaickdi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,i) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(c,e,i,k) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, d, a, e)
term(3) = term(3) + t2(a,e,k,i) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,k,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,c,i,m) * tvoov(a, k, m, d)
term(6) = term(6) + t2(a,c,m,i) * tvoov(a, k, m, d)
term(7) = term(7) + t2(a,a,k,m) * tvoov(c, i, m, d)
term(8) = term(8) + t2(a,c,k,m) * tvvoo(a, d, m, i)
term(9) = term(9) + t2(a,c,m,i) * tvvoo(a, d, m, k)
term(10) = term(10) + t2(a,a,i,m) * tvvoo(c, d, m, k)
term(11) = term(11) + t2(a,a,k,m) * tvvoo(c, d, m, i)
end do 

term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v5_eom_cc3_31_triplet_trans_aiaickdi = 0.d+0
    do s = 0, 11
    v5_eom_cc3_31_triplet_trans_aiaickdi = v5_eom_cc3_31_triplet_trans_aiaickdi + term(s)
    end do

    end function v5_eom_cc3_31_triplet_trans_aiaickdi
    function v5_eom_cc3_31_triplet_trans_aiaickak(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: v5_eom_cc3_31_triplet_trans_aiaickak 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(c,e,i,i) * read_ftvvvv(a, e, a, a)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, a, a, e)
term(3) = term(3) + t2(c,e,i,k) * tvoov(a, i, k, e)
term(4) = term(4) + t2(c,e,i,i) * tvoov(a, k, k, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(c, i, k, e)
term(6) = term(6) + t2(a,e,k,i) * tvoov(c, i, k, e)
term(7) = term(7) + t2(a,e,k,i) * tvvoo(c, e, k, i)
term(8) = term(8) + t2(c,e,i,k) * tvvoo(a, e, k, i)
term(9) = term(9) + t2(a,e,i,i) * tvvoo(c, e, k, k)
term(10) = term(10) + t2(c,e,i,i) * tvvoo(a, e, k, k)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (0.5d+0) 

do m = 1, nocc 
term(11) = term(11) + t2(a,c,m,i) * tvoov(a, i, m, a)
term(12) = term(12) + t2(a,a,i,m) * tvoov(c, i, m, a)
term(13) = term(13) + t2(a,c,i,m) * tvvoo(a, a, m, i)
term(14) = term(14) + t2(a,c,m,i) * tvvoo(a, a, m, i)
term(15) = term(15) + t2(a,c,i,m) * tvoov(a, i, m, a)
term(16) = term(16) + t2(a,a,i,m) * tvvoo(c, a, m, i)
term(17) = term(17) + t2(a,c,k,m) * toooo(m, i, k, i)
term(18) = term(18) + t2(a,c,m,i) * toooo(m, k, k, i)
term(19) = term(19) + t2(a,c,i,m) * toooo(m, i, k, k)
term(20) = term(20) + t2(a,c,m,i) * toooo(m, i, k, k)
end do 

term(11) = term(11) * (0.5d+0) 
term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (-1.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 


    v5_eom_cc3_31_triplet_trans_aiaickak = 0.d+0
    do s = 0, 20
    v5_eom_cc3_31_triplet_trans_aiaickak = v5_eom_cc3_31_triplet_trans_aiaickak + term(s)
    end do

    end function v5_eom_cc3_31_triplet_trans_aiaickak
    function v5_eom_cc3_31_triplet_trans_aiaickai(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: v5_eom_cc3_31_triplet_trans_aiaickai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,k) * tvoov(a, i, i, e)
term(1) = term(1) + t2(c,e,i,i) * tvoov(a, k, i, e)
term(2) = term(2) + t2(a,e,i,k) * tvoov(c, i, i, e)
term(3) = term(3) + t2(a,e,k,i) * tvoov(c, i, i, e)
term(4) = term(4) + t2(a,e,k,i) * read_ftvvvv(c, e, a, a)
term(5) = term(5) + t2(c,e,i,k) * read_ftvvvv(a, e, a, a)
term(6) = term(6) + t2(a,e,i,k) * read_ftvvvv(c, a, a, e)
term(7) = term(7) + t2(a,e,k,i) * read_ftvvvv(c, a, a, e)
term(8) = term(8) + t2(a,e,k,i) * tvvoo(c, e, i, i)
term(9) = term(9) + t2(c,e,i,k) * tvvoo(a, e, i, i)
term(10) = term(10) + t2(a,e,i,i) * tvvoo(c, e, i, k)
term(11) = term(11) + t2(c,e,i,i) * tvvoo(a, e, i, k)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,k,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,c,i,m) * tvoov(a, k, m, a)
term(14) = term(14) + t2(a,c,m,i) * tvoov(a, k, m, a)
term(15) = term(15) + t2(a,a,k,m) * tvoov(c, i, m, a)
term(16) = term(16) + t2(a,c,k,m) * tvvoo(a, a, m, i)
term(17) = term(17) + t2(a,c,m,i) * tvvoo(a, a, m, k)
term(18) = term(18) + t2(a,a,i,m) * tvvoo(c, a, m, k)
term(19) = term(19) + t2(a,a,k,m) * tvvoo(c, a, m, i)
term(20) = term(20) + t2(a,c,k,m) * toooo(m, i, i, i)
term(21) = term(21) + t2(a,c,m,i) * toooo(m, k, i, i)
term(22) = term(22) + t2(a,c,i,m) * toooo(m, i, i, k)
term(23) = term(23) + t2(a,c,m,i) * toooo(m, i, i, k)
end do 

term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (-0.5d+0) 


    v5_eom_cc3_31_triplet_trans_aiaickai = 0.d+0
    do s = 0, 23
    v5_eom_cc3_31_triplet_trans_aiaickai = v5_eom_cc3_31_triplet_trans_aiaickai + term(s)
    end do

    end function v5_eom_cc3_31_triplet_trans_aiaickai
    function v5_eom_cc3_31_triplet_trans_aiaickck(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: v5_eom_cc3_31_triplet_trans_aiaickck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,i,i) * read_ftvvvv(a, e, a, c)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, c, a, e)
term(3) = term(3) + t2(a,e,k,i) * tvoov(a, i, k, e)
term(4) = term(4) + t2(a,e,i,i) * tvoov(a, k, k, e)
term(5) = term(5) + t2(a,e,i,k) * tvvoo(a, e, k, i)
term(6) = term(6) + t2(a,e,k,i) * tvvoo(a, e, k, i)
term(7) = term(7) + t2(a,e,i,k) * tvoov(a, i, k, e)
term(8) = term(8) + t2(a,e,i,i) * tvvoo(a, e, k, k)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (-1.0d+0) 
term(8) = term(8) * (-1.0d+0) 

do m = 1, nocc 
term(9) = term(9) + t2(a,c,m,i) * tvoov(a, i, m, c)
term(10) = term(10) + t2(a,a,i,m) * tvoov(c, i, m, c)
term(11) = term(11) + t2(a,c,i,m) * tvvoo(a, c, m, i)
term(12) = term(12) + t2(a,c,m,i) * tvvoo(a, c, m, i)
term(13) = term(13) + t2(a,c,i,m) * tvoov(a, i, m, c)
term(14) = term(14) + t2(a,a,i,m) * tvvoo(c, c, m, i)
term(15) = term(15) + t2(a,a,i,m) * toooo(m, k, k, i)
term(16) = term(16) + t2(a,a,k,m) * toooo(m, i, k, i)
term(17) = term(17) + t2(a,a,i,m) * toooo(m, i, k, k)
end do 

term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 
term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (-1.0d+0) 
term(14) = term(14) * (-1.0d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 


    v5_eom_cc3_31_triplet_trans_aiaickck = 0.d+0
    do s = 0, 17
    v5_eom_cc3_31_triplet_trans_aiaickck = v5_eom_cc3_31_triplet_trans_aiaickck + term(s)
    end do

    end function v5_eom_cc3_31_triplet_trans_aiaickck
    function v5_eom_cc3_31_triplet_trans_aiaickci(t2, nocc, nactive, a, i, c, k) 
    real(F64) :: v5_eom_cc3_31_triplet_trans_aiaickci 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,i) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(c,e,i,k) * read_ftvvvv(a, e, a, c)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, c, a, e)
term(3) = term(3) + t2(a,e,k,i) * read_ftvvvv(c, c, a, e)
term(4) = term(4) + t2(a,e,k,i) * tvoov(a, i, i, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(a, k, i, e)
term(6) = term(6) + t2(a,e,i,k) * tvvoo(a, e, i, i)
term(7) = term(7) + t2(a,e,k,i) * tvvoo(a, e, i, i)
term(8) = term(8) + t2(a,e,i,k) * tvoov(a, i, i, e)
term(9) = term(9) + t2(a,e,i,i) * tvvoo(a, e, i, k)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-1.0d+0) 
term(9) = term(9) * (-1.0d+0) 

do m = 1, nocc 
term(10) = term(10) + t2(a,c,k,m) * tvoov(a, i, m, c)
term(11) = term(11) + t2(a,c,i,m) * tvoov(a, k, m, c)
term(12) = term(12) + t2(a,c,m,i) * tvoov(a, k, m, c)
term(13) = term(13) + t2(a,a,k,m) * tvoov(c, i, m, c)
term(14) = term(14) + t2(a,c,k,m) * tvvoo(a, c, m, i)
term(15) = term(15) + t2(a,c,m,i) * tvvoo(a, c, m, k)
term(16) = term(16) + t2(a,a,i,m) * tvvoo(c, c, m, k)
term(17) = term(17) + t2(a,a,k,m) * tvvoo(c, c, m, i)
term(18) = term(18) + t2(a,a,i,m) * toooo(m, k, i, i)
term(19) = term(19) + t2(a,a,k,m) * toooo(m, i, i, i)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, i, i, k)
end do 

term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (-0.5d+0) 


    v5_eom_cc3_31_triplet_trans_aiaickci = 0.d+0
    do s = 0, 20
    v5_eom_cc3_31_triplet_trans_aiaickci = v5_eom_cc3_31_triplet_trans_aiaickci + term(s)
    end do

    end function v5_eom_cc3_31_triplet_trans_aiaickci
    end module v5_eom_cc3_31_triplet_trans
    