module v7_eom_cc3_31_triplet_trans

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
    
    function v7_eom_cc3_31_triplet_trans_aibiakbl(t2, nocc, nactive, a, i, k, l) 
    real(F64) :: v7_eom_cc3_31_triplet_trans_aibiakbl 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,k,i) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,i) * tvoov(a, k, l, e)
term(3) = term(3) + t2(a,e,k,i) * tvvoo(a, e, l, i)
term(4) = term(4) + t2(a,e,i,k) * tvvoo(a, e, l, i)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(a, e, l, k)
end do 

term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,a,k,m) * toooo(m, i, l, i)
term(7) = term(7) + t2(a,a,i,m) * toooo(m, k, l, i)
term(8) = term(8) + t2(a,a,i,m) * toooo(m, i, l, k)
end do 

term(6) = term(6) * (0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (-1.0d+0) 


    v7_eom_cc3_31_triplet_trans_aibiakbl = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_triplet_trans_aibiakbl = v7_eom_cc3_31_triplet_trans_aibiakbl + term(s)
    end do

    end function v7_eom_cc3_31_triplet_trans_aibiakbl
    function v7_eom_cc3_31_triplet_trans_aibiakal(t2, nocc, nactive, a, i, b, k, l) 
    real(F64) :: v7_eom_cc3_31_triplet_trans_aibiakal 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, l 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tvoov(a, k, l, e)
term(1) = term(1) + t2(a,e,k,i) * tvoov(b, i, l, e)
term(2) = term(2) + t2(a,e,i,k) * tvoov(b, i, l, e)
term(3) = term(3) + t2(b,e,i,k) * tvoov(a, i, l, e)
term(4) = term(4) + t2(b,e,i,k) * tvvoo(a, e, l, i)
term(5) = term(5) + t2(a,e,k,i) * tvvoo(b, e, l, i)
term(6) = term(6) + t2(b,e,i,i) * tvvoo(a, e, l, k)
term(7) = term(7) + t2(a,e,i,i) * tvvoo(b, e, l, k)
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
term(8) = term(8) + t2(a,b,m,i) * toooo(m, k, l, i)
term(9) = term(9) + t2(a,b,k,m) * toooo(m, i, l, i)
term(10) = term(10) + t2(a,b,m,i) * toooo(m, i, l, k)
term(11) = term(11) + t2(a,b,i,m) * toooo(m, i, l, k)
end do 

term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (0.5d+0) 
term(11) = term(11) * (0.5d+0) 


    v7_eom_cc3_31_triplet_trans_aibiakal = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_triplet_trans_aibiakal = v7_eom_cc3_31_triplet_trans_aibiakal + term(s)
    end do

    end function v7_eom_cc3_31_triplet_trans_aibiakal
    function v7_eom_cc3_31_triplet_trans_aibiakdk(t2, nocc, nactive, a, i, b, d) 
    real(F64) :: v7_eom_cc3_31_triplet_trans_aibiakdk 
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

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-1.0d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,b,i,m) * tvoov(a, i, m, d)
term(4) = term(4) + t2(a,a,i,m) * tvoov(b, i, m, d)
term(5) = term(5) + t2(a,b,m,i) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,b,m,i) * tvvoo(a, d, m, i)
term(7) = term(7) + t2(a,b,i,m) * tvvoo(a, d, m, i)
term(8) = term(8) + t2(a,a,i,m) * tvvoo(b, d, m, i)
end do 

term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 


    v7_eom_cc3_31_triplet_trans_aibiakdk = 0.d+0
    do s = 0, 8
    v7_eom_cc3_31_triplet_trans_aibiakdk = v7_eom_cc3_31_triplet_trans_aibiakdk + term(s)
    end do

    end function v7_eom_cc3_31_triplet_trans_aibiakdk
    function v7_eom_cc3_31_triplet_trans_aibiakdi(t2, nocc, nactive, a, i, b, k, d) 
    real(F64) :: v7_eom_cc3_31_triplet_trans_aibiakdi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,k) * read_ftvvvv(a, e, a, d)
term(1) = term(1) + t2(a,e,k,i) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(b, d, a, e)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,k,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,b,m,i) * tvoov(a, k, m, d)
term(6) = term(6) + t2(a,b,i,m) * tvoov(a, k, m, d)
term(7) = term(7) + t2(a,a,k,m) * tvoov(b, i, m, d)
term(8) = term(8) + t2(a,b,m,i) * tvvoo(a, d, m, k)
term(9) = term(9) + t2(a,b,k,m) * tvvoo(a, d, m, i)
term(10) = term(10) + t2(a,a,k,m) * tvvoo(b, d, m, i)
term(11) = term(11) + t2(a,a,i,m) * tvvoo(b, d, m, k)
end do 

term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 


    v7_eom_cc3_31_triplet_trans_aibiakdi = 0.d+0
    do s = 0, 11
    v7_eom_cc3_31_triplet_trans_aibiakdi = v7_eom_cc3_31_triplet_trans_aibiakdi + term(s)
    end do

    end function v7_eom_cc3_31_triplet_trans_aibiakdi
    function v7_eom_cc3_31_triplet_trans_aibiakbk(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: v7_eom_cc3_31_triplet_trans_aibiakbk 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * read_ftvvvv(a, e, a, b)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(a,e,i,k) * tvoov(a, i, k, e)
term(4) = term(4) + t2(a,e,k,i) * tvoov(a, i, k, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(a, k, k, e)
term(6) = term(6) + t2(a,e,k,i) * tvvoo(a, e, k, i)
term(7) = term(7) + t2(a,e,i,k) * tvvoo(a, e, k, i)
term(8) = term(8) + t2(a,e,i,i) * tvvoo(a, e, k, k)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-1.0d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 

do m = 1, nocc 
term(9) = term(9) + t2(a,b,i,m) * tvoov(a, i, m, b)
term(10) = term(10) + t2(a,a,i,m) * tvoov(b, i, m, b)
term(11) = term(11) + t2(a,b,m,i) * tvoov(a, i, m, b)
term(12) = term(12) + t2(a,b,m,i) * tvvoo(a, b, m, i)
term(13) = term(13) + t2(a,b,i,m) * tvvoo(a, b, m, i)
term(14) = term(14) + t2(a,a,i,m) * tvvoo(b, b, m, i)
term(15) = term(15) + t2(a,a,k,m) * toooo(m, i, k, i)
term(16) = term(16) + t2(a,a,i,m) * toooo(m, k, k, i)
term(17) = term(17) + t2(a,a,i,m) * toooo(m, i, k, k)
end do 

term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(15) = term(15) * (0.5d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-1.0d+0) 


    v7_eom_cc3_31_triplet_trans_aibiakbk = 0.d+0
    do s = 0, 17
    v7_eom_cc3_31_triplet_trans_aibiakbk = v7_eom_cc3_31_triplet_trans_aibiakbk + term(s)
    end do

    end function v7_eom_cc3_31_triplet_trans_aibiakbk
    function v7_eom_cc3_31_triplet_trans_aibiakbi(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: v7_eom_cc3_31_triplet_trans_aibiakbi 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,k) * read_ftvvvv(a, e, a, b)
term(1) = term(1) + t2(a,e,k,i) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(a,e,i,k) * read_ftvvvv(b, b, a, e)
term(4) = term(4) + t2(a,e,i,k) * tvoov(a, i, i, e)
term(5) = term(5) + t2(a,e,k,i) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,i,i) * tvoov(a, k, i, e)
term(7) = term(7) + t2(a,e,k,i) * tvvoo(a, e, i, i)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(a, e, i, i)
term(9) = term(9) + t2(a,e,i,i) * tvvoo(a, e, i, k)
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 

do m = 1, nocc 
term(10) = term(10) + t2(a,b,k,m) * tvoov(a, i, m, b)
term(11) = term(11) + t2(a,b,m,i) * tvoov(a, k, m, b)
term(12) = term(12) + t2(a,b,i,m) * tvoov(a, k, m, b)
term(13) = term(13) + t2(a,a,k,m) * tvoov(b, i, m, b)
term(14) = term(14) + t2(a,b,m,i) * tvvoo(a, b, m, k)
term(15) = term(15) + t2(a,b,k,m) * tvvoo(a, b, m, i)
term(16) = term(16) + t2(a,a,k,m) * tvvoo(b, b, m, i)
term(17) = term(17) + t2(a,a,i,m) * tvvoo(b, b, m, k)
term(18) = term(18) + t2(a,a,k,m) * toooo(m, i, i, i)
term(19) = term(19) + t2(a,a,i,m) * toooo(m, k, i, i)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, i, i, k)
end do 

term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (0.5d+0) 
term(14) = term(14) * (0.5d+0) 
term(15) = term(15) * (0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (-1.0d+0) 


    v7_eom_cc3_31_triplet_trans_aibiakbi = 0.d+0
    do s = 0, 20
    v7_eom_cc3_31_triplet_trans_aibiakbi = v7_eom_cc3_31_triplet_trans_aibiakbi + term(s)
    end do

    end function v7_eom_cc3_31_triplet_trans_aibiakbi
    function v7_eom_cc3_31_triplet_trans_aibiakak(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: v7_eom_cc3_31_triplet_trans_aibiakak 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * read_ftvvvv(a, e, a, a)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(b,e,i,i) * tvoov(a, k, k, e)
term(4) = term(4) + t2(a,e,k,i) * tvoov(b, i, k, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(b, i, k, e)
term(6) = term(6) + t2(b,e,i,k) * tvoov(a, i, k, e)
term(7) = term(7) + t2(b,e,i,k) * tvvoo(a, e, k, i)
term(8) = term(8) + t2(a,e,k,i) * tvvoo(b, e, k, i)
term(9) = term(9) + t2(b,e,i,i) * tvvoo(a, e, k, k)
term(10) = term(10) + t2(a,e,i,i) * tvvoo(b, e, k, k)
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (0.5d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (0.5d+0) 
term(8) = term(8) * (0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 

do m = 1, nocc 
term(11) = term(11) + t2(a,b,i,m) * tvoov(a, i, m, a)
term(12) = term(12) + t2(a,a,i,m) * tvoov(b, i, m, a)
term(13) = term(13) + t2(a,b,m,i) * tvoov(a, i, m, a)
term(14) = term(14) + t2(a,b,m,i) * tvvoo(a, a, m, i)
term(15) = term(15) + t2(a,b,i,m) * tvvoo(a, a, m, i)
term(16) = term(16) + t2(a,a,i,m) * tvvoo(b, a, m, i)
term(17) = term(17) + t2(a,b,m,i) * toooo(m, k, k, i)
term(18) = term(18) + t2(a,b,k,m) * toooo(m, i, k, i)
term(19) = term(19) + t2(a,b,m,i) * toooo(m, i, k, k)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, i, k, k)
end do 

term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (0.5d+0) 
term(20) = term(20) * (0.5d+0) 


    v7_eom_cc3_31_triplet_trans_aibiakak = 0.d+0
    do s = 0, 20
    v7_eom_cc3_31_triplet_trans_aibiakak = v7_eom_cc3_31_triplet_trans_aibiakak + term(s)
    end do

    end function v7_eom_cc3_31_triplet_trans_aibiakak
    function v7_eom_cc3_31_triplet_trans_aibiakai(t2, nocc, nactive, a, i, b, k) 
    real(F64) :: v7_eom_cc3_31_triplet_trans_aibiakai 
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    real(F64), dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tvoov(a, k, i, e)
term(1) = term(1) + t2(a,e,k,i) * tvoov(b, i, i, e)
term(2) = term(2) + t2(a,e,i,k) * tvoov(b, i, i, e)
term(3) = term(3) + t2(b,e,i,k) * tvoov(a, i, i, e)
term(4) = term(4) + t2(b,e,i,k) * read_ftvvvv(a, e, a, a)
term(5) = term(5) + t2(a,e,k,i) * read_ftvvvv(b, e, a, a)
term(6) = term(6) + t2(a,e,k,i) * read_ftvvvv(b, a, a, e)
term(7) = term(7) + t2(a,e,i,k) * read_ftvvvv(b, a, a, e)
term(8) = term(8) + t2(b,e,i,k) * tvvoo(a, e, i, i)
term(9) = term(9) + t2(a,e,k,i) * tvvoo(b, e, i, i)
term(10) = term(10) + t2(b,e,i,i) * tvvoo(a, e, i, k)
term(11) = term(11) + t2(a,e,i,i) * tvvoo(b, e, i, k)
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
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,k,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,b,m,i) * tvoov(a, k, m, a)
term(14) = term(14) + t2(a,b,i,m) * tvoov(a, k, m, a)
term(15) = term(15) + t2(a,a,k,m) * tvoov(b, i, m, a)
term(16) = term(16) + t2(a,b,m,i) * tvvoo(a, a, m, k)
term(17) = term(17) + t2(a,b,k,m) * tvvoo(a, a, m, i)
term(18) = term(18) + t2(a,a,k,m) * tvvoo(b, a, m, i)
term(19) = term(19) + t2(a,a,i,m) * tvvoo(b, a, m, k)
term(20) = term(20) + t2(a,b,m,i) * toooo(m, k, i, i)
term(21) = term(21) + t2(a,b,k,m) * toooo(m, i, i, i)
term(22) = term(22) + t2(a,b,m,i) * toooo(m, i, i, k)
term(23) = term(23) + t2(a,b,i,m) * toooo(m, i, i, k)
end do 

term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (0.5d+0) 
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


    v7_eom_cc3_31_triplet_trans_aibiakai = 0.d+0
    do s = 0, 23
    v7_eom_cc3_31_triplet_trans_aibiakai = v7_eom_cc3_31_triplet_trans_aibiakai + term(s)
    end do

    end function v7_eom_cc3_31_triplet_trans_aibiakai
    end module v7_eom_cc3_31_triplet_trans
    