module v3_eom_cc3_31_triplet_trans

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
    
    function v3_eom_cc3_31_triplet_trans_aibickbl(t2, nocc, nactive, a, i, c, k, l) 
    real(F64) :: v3_eom_cc3_31_triplet_trans_aibickbl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,k) * tvoov(a, i, l, e)
term(1) = term(1) + t2(c,e,k,i) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,k) * tvoov(c, i, l, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(c, k, l, e)
term(4) = term(4) + t2(a,e,i,k) * tvvoo(c, e, l, i)
term(5) = term(5) + t2(c,e,k,i) * tvvoo(a, e, l, i)
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
term(8) = term(8) + t2(a,c,i,m) * toooo(m, k, l, i)
term(9) = term(9) + t2(a,c,m,k) * toooo(m, i, l, i)
term(10) = term(10) + t2(a,c,i,m) * toooo(m, i, l, k)
term(11) = term(11) + t2(a,c,m,i) * toooo(m, i, l, k)
end do 

term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 


    v3_eom_cc3_31_triplet_trans_aibickbl = 0.d+0
    do s = 0, 11
    v3_eom_cc3_31_triplet_trans_aibickbl = v3_eom_cc3_31_triplet_trans_aibickbl + term(s)
    end do

    end function v3_eom_cc3_31_triplet_trans_aibickbl
    function v3_eom_cc3_31_triplet_trans_aibickal(t2, nocc, nactive, i, b, c, k, l) 
    real(F64) :: v3_eom_cc3_31_triplet_trans_aibickal 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * tvoov(c, i, l, e)
term(1) = term(1) + t2(c,e,k,i) * tvoov(b, i, l, e)
term(2) = term(2) + t2(c,e,i,i) * tvoov(b, k, l, e)
term(3) = term(3) + t2(b,e,i,i) * tvoov(c, k, l, e)
term(4) = term(4) + t2(b,e,k,i) * tvvoo(c, e, l, i)
term(5) = term(5) + t2(b,e,i,k) * tvvoo(c, e, l, i)
term(6) = term(6) + t2(c,e,i,k) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(c,e,k,i) * tvvoo(b, e, l, i)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (0.5d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,k,m) * toooo(m, i, l, i)
term(9) = term(9) + t2(b,c,i,m) * toooo(m, k, l, i)
term(10) = term(10) + t2(b,c,m,i) * toooo(m, k, l, i)
term(11) = term(11) + t2(b,c,m,k) * toooo(m, i, l, i)
end do 

term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (-0.5d+0) 


    v3_eom_cc3_31_triplet_trans_aibickal = 0.d+0
    do s = 0, 11
    v3_eom_cc3_31_triplet_trans_aibickal = v3_eom_cc3_31_triplet_trans_aibickal + term(s)
    end do

    end function v3_eom_cc3_31_triplet_trans_aibickal
    function v3_eom_cc3_31_triplet_trans_aibickcl(t2, nocc, nactive, a, i, b, k, l) 
    real(F64) :: v3_eom_cc3_31_triplet_trans_aibickcl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,i) * tvoov(b, k, l, e)
term(2) = term(2) + t2(a,e,i,k) * tvvoo(b, e, l, i)
term(3) = term(3) + t2(b,e,k,i) * tvvoo(a, e, l, i)
term(4) = term(4) + t2(b,e,i,k) * tvoov(a, i, l, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(b, i, l, e)
term(6) = term(6) + t2(a,e,i,i) * tvvoo(b, e, l, k)
term(7) = term(7) + t2(b,e,i,i) * tvvoo(a, e, l, k)
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
term(8) = term(8) + t2(a,b,i,m) * toooo(m, k, l, i)
term(9) = term(9) + t2(a,b,m,k) * toooo(m, i, l, i)
term(10) = term(10) + t2(a,b,i,m) * toooo(m, i, l, k)
term(11) = term(11) + t2(a,b,m,i) * toooo(m, i, l, k)
end do 

term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v3_eom_cc3_31_triplet_trans_aibickcl = 0.d+0
    do s = 0, 11
    v3_eom_cc3_31_triplet_trans_aibickcl = v3_eom_cc3_31_triplet_trans_aibickcl + term(s)
    end do

    end function v3_eom_cc3_31_triplet_trans_aibickcl
    function v3_eom_cc3_31_triplet_trans_aibickdk(t2, nocc, nactive, a, i, b, c, d) 
    real(F64) :: v3_eom_cc3_31_triplet_trans_aibickdk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, b, d)
term(1) = term(1) + t2(c,e,i,i) * read_ftvvvv(b, d, a, e)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, d, b, e)
term(3) = term(3) + t2(b,e,i,i) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,m,i) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,b,i,m) * tvoov(c, i, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvvoo(b, d, m, i)
term(7) = term(7) + t2(a,c,m,i) * tvvoo(b, d, m, i)
term(8) = term(8) + t2(b,c,i,m) * tvoov(a, i, m, d)
term(9) = term(9) + t2(a,c,i,m) * tvoov(b, i, m, d)
term(10) = term(10) + t2(a,b,i,m) * tvvoo(c, d, m, i)
term(11) = term(11) + t2(a,b,m,i) * tvvoo(c, d, m, i)
end do 

term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 


    v3_eom_cc3_31_triplet_trans_aibickdk = 0.d+0
    do s = 0, 11
    v3_eom_cc3_31_triplet_trans_aibickdk = v3_eom_cc3_31_triplet_trans_aibickdk + term(s)
    end do

    end function v3_eom_cc3_31_triplet_trans_aibickdk
    function v3_eom_cc3_31_triplet_trans_aibickdi(t2, nocc, nactive, a, i, b, c, k, d) 
    real(F64) :: v3_eom_cc3_31_triplet_trans_aibickdi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(b,e,i,k) * read_ftvvvv(c, e, a, d)
term(2) = term(2) + t2(c,e,i,k) * read_ftvvvv(b, e, a, d)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, e, a, d)
term(4) = term(4) + t2(a,e,i,k) * read_ftvvvv(c, e, b, d)
term(5) = term(5) + t2(a,e,i,k) * read_ftvvvv(c, d, b, e)
term(6) = term(6) + t2(b,e,k,i) * read_ftvvvv(c, d, a, e)
term(7) = term(7) + t2(c,e,k,i) * read_ftvvvv(b, d, a, e)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (0.5d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,k,m) * tvoov(a, i, m, d)
term(9) = term(9) + t2(b,c,m,k) * tvoov(a, i, m, d)
term(10) = term(10) + t2(a,b,m,k) * tvoov(c, i, m, d)
term(11) = term(11) + t2(a,c,m,k) * tvoov(b, i, m, d)
term(12) = term(12) + t2(a,c,i,m) * tvoov(b, k, m, d)
term(13) = term(13) + t2(a,b,i,m) * tvoov(c, k, m, d)
term(14) = term(14) + t2(a,c,m,i) * tvoov(b, k, m, d)
term(15) = term(15) + t2(a,b,m,i) * tvoov(c, k, m, d)
term(16) = term(16) + t2(b,c,k,m) * tvvoo(a, d, m, i)
term(17) = term(17) + t2(b,c,i,m) * tvvoo(a, d, m, k)
term(18) = term(18) + t2(b,c,m,i) * tvvoo(a, d, m, k)
term(19) = term(19) + t2(b,c,m,k) * tvvoo(a, d, m, i)
term(20) = term(20) + t2(a,c,i,m) * tvvoo(b, d, m, k)
term(21) = term(21) + t2(a,b,i,m) * tvvoo(c, d, m, k)
term(22) = term(22) + t2(a,b,m,k) * tvvoo(c, d, m, i)
term(23) = term(23) + t2(a,c,m,k) * tvvoo(b, d, m, i)
end do 

term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (0.5d+0) 
term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (0.5d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (-0.5d+0) 


    v3_eom_cc3_31_triplet_trans_aibickdi = 0.d+0
    do s = 0, 23
    v3_eom_cc3_31_triplet_trans_aibickdi = v3_eom_cc3_31_triplet_trans_aibickdi + term(s)
    end do

    end function v3_eom_cc3_31_triplet_trans_aibickdi
    function v3_eom_cc3_31_triplet_trans_aibickbk(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_31_triplet_trans_aibickbk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,k) * tvoov(a, i, k, e)
term(1) = term(1) + t2(c,e,k,i) * tvoov(a, i, k, e)
term(2) = term(2) + t2(a,e,i,k) * tvoov(c, i, k, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(c, k, k, e)
term(4) = term(4) + t2(a,e,i,k) * tvvoo(c, e, k, i)
term(5) = term(5) + t2(c,e,k,i) * tvvoo(a, e, k, i)
term(6) = term(6) + t2(a,e,i,i) * tvvoo(c, e, k, k)
term(7) = term(7) + t2(c,e,i,i) * tvvoo(a, e, k, k)
term(8) = term(8) + t2(a,e,i,i) * read_ftvvvv(c, e, b, b)
term(9) = term(9) + t2(c,e,i,i) * read_ftvvvv(b, b, a, e)
term(10) = term(10) + t2(a,e,i,i) * read_ftvvvv(c, b, b, e)
term(11) = term(11) + t2(b,e,i,i) * read_ftvvvv(c, b, a, e)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,i,m) * toooo(m, k, k, i)
term(13) = term(13) + t2(a,c,m,k) * toooo(m, i, k, i)
term(14) = term(14) + t2(a,c,i,m) * toooo(m, i, k, k)
term(15) = term(15) + t2(a,c,m,i) * toooo(m, i, k, k)
term(16) = term(16) + t2(b,c,m,i) * tvoov(a, i, m, b)
term(17) = term(17) + t2(a,b,i,m) * tvoov(c, i, m, b)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(b, b, m, i)
term(19) = term(19) + t2(a,c,m,i) * tvvoo(b, b, m, i)
term(20) = term(20) + t2(b,c,i,m) * tvoov(a, i, m, b)
term(21) = term(21) + t2(a,c,i,m) * tvoov(b, i, m, b)
term(22) = term(22) + t2(a,b,i,m) * tvvoo(c, b, m, i)
term(23) = term(23) + t2(a,b,m,i) * tvvoo(c, b, m, i)
end do 

term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (-0.5d+0) 


    v3_eom_cc3_31_triplet_trans_aibickbk = 0.d+0
    do s = 0, 23
    v3_eom_cc3_31_triplet_trans_aibickbk = v3_eom_cc3_31_triplet_trans_aibickbk + term(s)
    end do

    end function v3_eom_cc3_31_triplet_trans_aibickbk
    function v3_eom_cc3_31_triplet_trans_aibickbi(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_31_triplet_trans_aibickbi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * read_ftvvvv(c, e, a, b)
term(1) = term(1) + t2(b,e,i,k) * read_ftvvvv(c, e, a, b)
term(2) = term(2) + t2(c,e,i,k) * read_ftvvvv(b, e, a, b)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, e, a, b)
term(4) = term(4) + t2(a,e,i,k) * read_ftvvvv(c, e, b, b)
term(5) = term(5) + t2(a,e,i,k) * read_ftvvvv(c, b, b, e)
term(6) = term(6) + t2(b,e,k,i) * read_ftvvvv(c, b, a, e)
term(7) = term(7) + t2(c,e,k,i) * read_ftvvvv(b, b, a, e)
term(8) = term(8) + t2(c,e,i,k) * tvoov(a, i, i, e)
term(9) = term(9) + t2(c,e,k,i) * tvoov(a, i, i, e)
term(10) = term(10) + t2(a,e,i,k) * tvoov(c, i, i, e)
term(11) = term(11) + t2(a,e,i,i) * tvoov(c, k, i, e)
term(12) = term(12) + t2(a,e,i,k) * tvvoo(c, e, i, i)
term(13) = term(13) + t2(c,e,k,i) * tvvoo(a, e, i, i)
term(14) = term(14) + t2(a,e,i,i) * tvvoo(c, e, i, k)
term(15) = term(15) + t2(c,e,i,i) * tvvoo(a, e, i, k)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (0.5d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,k,m) * tvoov(a, i, m, b)
term(17) = term(17) + t2(b,c,m,k) * tvoov(a, i, m, b)
term(18) = term(18) + t2(a,b,m,k) * tvoov(c, i, m, b)
term(19) = term(19) + t2(a,c,m,k) * tvoov(b, i, m, b)
term(20) = term(20) + t2(a,c,i,m) * tvoov(b, k, m, b)
term(21) = term(21) + t2(a,b,i,m) * tvoov(c, k, m, b)
term(22) = term(22) + t2(a,c,m,i) * tvoov(b, k, m, b)
term(23) = term(23) + t2(a,b,m,i) * tvoov(c, k, m, b)
term(24) = term(24) + t2(b,c,k,m) * tvvoo(a, b, m, i)
term(25) = term(25) + t2(b,c,i,m) * tvvoo(a, b, m, k)
term(26) = term(26) + t2(b,c,m,i) * tvvoo(a, b, m, k)
term(27) = term(27) + t2(b,c,m,k) * tvvoo(a, b, m, i)
term(28) = term(28) + t2(a,c,i,m) * tvvoo(b, b, m, k)
term(29) = term(29) + t2(a,b,i,m) * tvvoo(c, b, m, k)
term(30) = term(30) + t2(a,b,m,k) * tvvoo(c, b, m, i)
term(31) = term(31) + t2(a,c,m,k) * tvvoo(b, b, m, i)
term(32) = term(32) + t2(a,c,i,m) * toooo(m, k, i, i)
term(33) = term(33) + t2(a,c,m,k) * toooo(m, i, i, i)
term(34) = term(34) + t2(a,c,i,m) * toooo(m, i, i, k)
term(35) = term(35) + t2(a,c,m,i) * toooo(m, i, i, k)
end do 

term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * (0.5d+0) 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * (0.5d+0) 
term(28) = term(28) * (-0.5d+0) 
term(29) = term(29) * (0.5d+0) 
term(30) = term(30) * (0.5d+0) 
term(31) = term(31) * (-0.5d+0) 
term(32) = term(32) * (0.5d+0) 
term(33) = term(33) * (0.5d+0) 
term(34) = term(34) * (-0.5d+0) 
term(35) = term(35) * (-0.5d+0) 


    v3_eom_cc3_31_triplet_trans_aibickbi = 0.d+0
    do s = 0, 35
    v3_eom_cc3_31_triplet_trans_aibickbi = v3_eom_cc3_31_triplet_trans_aibickbi + term(s)
    end do

    end function v3_eom_cc3_31_triplet_trans_aibickbi
    function v3_eom_cc3_31_triplet_trans_aibickak(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_31_triplet_trans_aibickak 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * tvoov(c, i, k, e)
term(1) = term(1) + t2(c,e,k,i) * tvoov(b, i, k, e)
term(2) = term(2) + t2(c,e,i,i) * tvoov(b, k, k, e)
term(3) = term(3) + t2(b,e,i,i) * tvoov(c, k, k, e)
term(4) = term(4) + t2(b,e,k,i) * tvvoo(c, e, k, i)
term(5) = term(5) + t2(b,e,i,k) * tvvoo(c, e, k, i)
term(6) = term(6) + t2(c,e,i,k) * tvvoo(b, e, k, i)
term(7) = term(7) + t2(c,e,k,i) * tvvoo(b, e, k, i)
term(8) = term(8) + t2(a,e,i,i) * read_ftvvvv(c, e, b, a)
term(9) = term(9) + t2(c,e,i,i) * read_ftvvvv(b, a, a, e)
term(10) = term(10) + t2(a,e,i,i) * read_ftvvvv(c, a, b, e)
term(11) = term(11) + t2(b,e,i,i) * read_ftvvvv(c, a, a, e)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,k,m) * toooo(m, i, k, i)
term(13) = term(13) + t2(b,c,i,m) * toooo(m, k, k, i)
term(14) = term(14) + t2(b,c,m,i) * toooo(m, k, k, i)
term(15) = term(15) + t2(b,c,m,k) * toooo(m, i, k, i)
term(16) = term(16) + t2(b,c,m,i) * tvoov(a, i, m, a)
term(17) = term(17) + t2(a,b,i,m) * tvoov(c, i, m, a)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(b, a, m, i)
term(19) = term(19) + t2(a,c,m,i) * tvvoo(b, a, m, i)
term(20) = term(20) + t2(b,c,i,m) * tvoov(a, i, m, a)
term(21) = term(21) + t2(a,c,i,m) * tvoov(b, i, m, a)
term(22) = term(22) + t2(a,b,i,m) * tvvoo(c, a, m, i)
term(23) = term(23) + t2(a,b,m,i) * tvvoo(c, a, m, i)
end do 

term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (-0.5d+0) 


    v3_eom_cc3_31_triplet_trans_aibickak = 0.d+0
    do s = 0, 23
    v3_eom_cc3_31_triplet_trans_aibickak = v3_eom_cc3_31_triplet_trans_aibickak + term(s)
    end do

    end function v3_eom_cc3_31_triplet_trans_aibickak
    function v3_eom_cc3_31_triplet_trans_aibickai(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_31_triplet_trans_aibickai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(b,e,i,k) * read_ftvvvv(c, e, a, a)
term(2) = term(2) + t2(c,e,i,k) * read_ftvvvv(b, e, a, a)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, e, a, a)
term(4) = term(4) + t2(a,e,i,k) * read_ftvvvv(c, e, b, a)
term(5) = term(5) + t2(a,e,i,k) * read_ftvvvv(c, a, b, e)
term(6) = term(6) + t2(b,e,k,i) * read_ftvvvv(c, a, a, e)
term(7) = term(7) + t2(c,e,k,i) * read_ftvvvv(b, a, a, e)
term(8) = term(8) + t2(b,e,k,i) * tvoov(c, i, i, e)
term(9) = term(9) + t2(c,e,k,i) * tvoov(b, i, i, e)
term(10) = term(10) + t2(c,e,i,i) * tvoov(b, k, i, e)
term(11) = term(11) + t2(b,e,i,i) * tvoov(c, k, i, e)
term(12) = term(12) + t2(b,e,k,i) * tvvoo(c, e, i, i)
term(13) = term(13) + t2(b,e,i,k) * tvvoo(c, e, i, i)
term(14) = term(14) + t2(c,e,i,k) * tvvoo(b, e, i, i)
term(15) = term(15) + t2(c,e,k,i) * tvvoo(b, e, i, i)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (0.5d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,k,m) * tvoov(a, i, m, a)
term(17) = term(17) + t2(b,c,m,k) * tvoov(a, i, m, a)
term(18) = term(18) + t2(a,b,m,k) * tvoov(c, i, m, a)
term(19) = term(19) + t2(a,c,m,k) * tvoov(b, i, m, a)
term(20) = term(20) + t2(a,c,i,m) * tvoov(b, k, m, a)
term(21) = term(21) + t2(a,b,i,m) * tvoov(c, k, m, a)
term(22) = term(22) + t2(a,c,m,i) * tvoov(b, k, m, a)
term(23) = term(23) + t2(a,b,m,i) * tvoov(c, k, m, a)
term(24) = term(24) + t2(b,c,k,m) * tvvoo(a, a, m, i)
term(25) = term(25) + t2(b,c,i,m) * tvvoo(a, a, m, k)
term(26) = term(26) + t2(b,c,m,i) * tvvoo(a, a, m, k)
term(27) = term(27) + t2(b,c,m,k) * tvvoo(a, a, m, i)
term(28) = term(28) + t2(a,c,i,m) * tvvoo(b, a, m, k)
term(29) = term(29) + t2(a,b,i,m) * tvvoo(c, a, m, k)
term(30) = term(30) + t2(a,b,m,k) * tvvoo(c, a, m, i)
term(31) = term(31) + t2(a,c,m,k) * tvvoo(b, a, m, i)
term(32) = term(32) + t2(b,c,k,m) * toooo(m, i, i, i)
term(33) = term(33) + t2(b,c,i,m) * toooo(m, k, i, i)
term(34) = term(34) + t2(b,c,m,i) * toooo(m, k, i, i)
term(35) = term(35) + t2(b,c,m,k) * toooo(m, i, i, i)
end do 

term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * (0.5d+0) 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * (0.5d+0) 
term(28) = term(28) * (-0.5d+0) 
term(29) = term(29) * (0.5d+0) 
term(30) = term(30) * (0.5d+0) 
term(31) = term(31) * (-0.5d+0) 
term(32) = term(32) * (0.5d+0) 
term(33) = term(33) * (-0.5d+0) 
term(34) = term(34) * (0.5d+0) 
term(35) = term(35) * (-0.5d+0) 


    v3_eom_cc3_31_triplet_trans_aibickai = 0.d+0
    do s = 0, 35
    v3_eom_cc3_31_triplet_trans_aibickai = v3_eom_cc3_31_triplet_trans_aibickai + term(s)
    end do

    end function v3_eom_cc3_31_triplet_trans_aibickai
    function v3_eom_cc3_31_triplet_trans_aibickck(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_31_triplet_trans_aibickck 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * tvoov(a, i, k, e)
term(1) = term(1) + t2(a,e,i,i) * tvoov(b, k, k, e)
term(2) = term(2) + t2(a,e,i,k) * tvvoo(b, e, k, i)
term(3) = term(3) + t2(b,e,k,i) * tvvoo(a, e, k, i)
term(4) = term(4) + t2(b,e,i,k) * tvoov(a, i, k, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(b, i, k, e)
term(6) = term(6) + t2(a,e,i,i) * tvvoo(b, e, k, k)
term(7) = term(7) + t2(b,e,i,i) * tvvoo(a, e, k, k)
term(8) = term(8) + t2(a,e,i,i) * read_ftvvvv(c, e, b, c)
term(9) = term(9) + t2(c,e,i,i) * read_ftvvvv(b, c, a, e)
term(10) = term(10) + t2(a,e,i,i) * read_ftvvvv(c, c, b, e)
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
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,i,m) * toooo(m, k, k, i)
term(13) = term(13) + t2(a,b,m,k) * toooo(m, i, k, i)
term(14) = term(14) + t2(a,b,i,m) * toooo(m, i, k, k)
term(15) = term(15) + t2(a,b,m,i) * toooo(m, i, k, k)
term(16) = term(16) + t2(b,c,m,i) * tvoov(a, i, m, c)
term(17) = term(17) + t2(a,b,i,m) * tvoov(c, i, m, c)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(b, c, m, i)
term(19) = term(19) + t2(a,c,m,i) * tvvoo(b, c, m, i)
term(20) = term(20) + t2(b,c,i,m) * tvoov(a, i, m, c)
term(21) = term(21) + t2(a,c,i,m) * tvoov(b, i, m, c)
term(22) = term(22) + t2(a,b,i,m) * tvvoo(c, c, m, i)
term(23) = term(23) + t2(a,b,m,i) * tvvoo(c, c, m, i)
end do 

term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (0.5d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (-0.5d+0) 


    v3_eom_cc3_31_triplet_trans_aibickck = 0.d+0
    do s = 0, 23
    v3_eom_cc3_31_triplet_trans_aibickck = v3_eom_cc3_31_triplet_trans_aibickck + term(s)
    end do

    end function v3_eom_cc3_31_triplet_trans_aibickck
    function v3_eom_cc3_31_triplet_trans_aibickci(t2, nocc, nactive, a, i, b, c, k) 
    real(F64) :: v3_eom_cc3_31_triplet_trans_aibickci 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(b,e,i,k) * read_ftvvvv(c, e, a, c)
term(2) = term(2) + t2(c,e,i,k) * read_ftvvvv(b, e, a, c)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, e, a, c)
term(4) = term(4) + t2(a,e,i,k) * read_ftvvvv(c, e, b, c)
term(5) = term(5) + t2(a,e,i,k) * read_ftvvvv(c, c, b, e)
term(6) = term(6) + t2(b,e,k,i) * read_ftvvvv(c, c, a, e)
term(7) = term(7) + t2(c,e,k,i) * read_ftvvvv(b, c, a, e)
term(8) = term(8) + t2(b,e,k,i) * tvoov(a, i, i, e)
term(9) = term(9) + t2(a,e,i,i) * tvoov(b, k, i, e)
term(10) = term(10) + t2(a,e,i,k) * tvvoo(b, e, i, i)
term(11) = term(11) + t2(b,e,k,i) * tvvoo(a, e, i, i)
term(12) = term(12) + t2(b,e,i,k) * tvoov(a, i, i, e)
term(13) = term(13) + t2(a,e,i,k) * tvoov(b, i, i, e)
term(14) = term(14) + t2(a,e,i,i) * tvvoo(b, e, i, k)
term(15) = term(15) + t2(b,e,i,i) * tvvoo(a, e, i, k)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
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
term(16) = term(16) + t2(b,c,k,m) * tvoov(a, i, m, c)
term(17) = term(17) + t2(b,c,m,k) * tvoov(a, i, m, c)
term(18) = term(18) + t2(a,b,m,k) * tvoov(c, i, m, c)
term(19) = term(19) + t2(a,c,m,k) * tvoov(b, i, m, c)
term(20) = term(20) + t2(a,c,i,m) * tvoov(b, k, m, c)
term(21) = term(21) + t2(a,b,i,m) * tvoov(c, k, m, c)
term(22) = term(22) + t2(a,c,m,i) * tvoov(b, k, m, c)
term(23) = term(23) + t2(a,b,m,i) * tvoov(c, k, m, c)
term(24) = term(24) + t2(b,c,k,m) * tvvoo(a, c, m, i)
term(25) = term(25) + t2(b,c,i,m) * tvvoo(a, c, m, k)
term(26) = term(26) + t2(b,c,m,i) * tvvoo(a, c, m, k)
term(27) = term(27) + t2(b,c,m,k) * tvvoo(a, c, m, i)
term(28) = term(28) + t2(a,c,i,m) * tvvoo(b, c, m, k)
term(29) = term(29) + t2(a,b,i,m) * tvvoo(c, c, m, k)
term(30) = term(30) + t2(a,b,m,k) * tvvoo(c, c, m, i)
term(31) = term(31) + t2(a,c,m,k) * tvvoo(b, c, m, i)
term(32) = term(32) + t2(a,b,i,m) * toooo(m, k, i, i)
term(33) = term(33) + t2(a,b,m,k) * toooo(m, i, i, i)
term(34) = term(34) + t2(a,b,i,m) * toooo(m, i, i, k)
term(35) = term(35) + t2(a,b,m,i) * toooo(m, i, i, k)
end do 

term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * (0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * (0.5d+0) 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * (0.5d+0) 
term(28) = term(28) * (-0.5d+0) 
term(29) = term(29) * (0.5d+0) 
term(30) = term(30) * (0.5d+0) 
term(31) = term(31) * (-0.5d+0) 
term(32) = term(32) * (-0.5d+0) 
term(33) = term(33) * (-0.5d+0) 
term(34) = term(34) * (0.5d+0) 
term(35) = term(35) * (0.5d+0) 


    v3_eom_cc3_31_triplet_trans_aibickci = 0.d+0
    do s = 0, 35
    v3_eom_cc3_31_triplet_trans_aibickci = v3_eom_cc3_31_triplet_trans_aibickci + term(s)
    end do

    end function v3_eom_cc3_31_triplet_trans_aibickci
    end module v3_eom_cc3_31_triplet_trans
    