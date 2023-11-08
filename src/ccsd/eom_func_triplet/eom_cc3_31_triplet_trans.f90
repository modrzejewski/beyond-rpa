module eom_cc3_31_triplet_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_cc3_31_triplet_trans_aibjckal(t2, nocc, nactive, i, b, j, c, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aibjckal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tvoov(c, k, l, e)
term(1) = term(1) + t2(c,e,j,i) * tvoov(b, k, l, e)
term(2) = term(2) + t2(c,e,k,i) * tvoov(b, j, l, e)
term(3) = term(3) + t2(b,e,k,i) * tvoov(c, j, l, e)
term(4) = term(4) + t2(b,e,k,j) * tvvoo(c, e, l, i)
term(5) = term(5) + t2(b,e,j,k) * tvvoo(c, e, l, i)
term(6) = term(6) + t2(c,e,j,k) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(c,e,k,j) * tvvoo(b, e, l, i)
end do 

term(0) = term(0) * 0.5000000000000001d+0 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5000000000000001d+0 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * 0.5000000000000001d+0 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * 0.5000000000000001d+0 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,k,m) * toooo(m, j, l, i)
term(9) = term(9) + t2(b,c,j,m) * toooo(m, k, l, i)
term(10) = term(10) + t2(b,c,m,j) * toooo(m, k, l, i)
term(11) = term(11) + t2(b,c,m,k) * toooo(m, j, l, i)
end do 

term(8) = term(8) * 0.5d+0 
term(9) = term(9) * (-0.5000000000000001d+0) 
term(10) = term(10) * 0.5d+0 
term(11) = term(11) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibjckal = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjckal = eom_cc3_31_triplet_trans_aibjckal + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckal
    function eom_cc3_31_triplet_trans_aibjckbl(t2, nocc, nactive, a, i, j, c, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aibjckbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,k) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,k) * tvoov(c, j, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvvoo(c, e, l, k)
term(3) = term(3) + t2(c,e,j,i) * tvvoo(a, e, l, k)
term(4) = term(4) + t2(c,e,k,j) * tvoov(a, i, l, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(c, k, l, e)
term(6) = term(6) + t2(a,e,i,k) * tvvoo(c, e, l, j)
term(7) = term(7) + t2(c,e,k,i) * tvvoo(a, e, l, j)
end do 

term(0) = term(0) * 0.4999999999999998d+0 
term(1) = term(1) * 0.4999999999999998d+0 
term(2) = term(2) * 0.4999999999999998d+0 
term(3) = term(3) * 0.4999999999999998d+0 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,c,i,m) * toooo(m, j, l, k)
term(9) = term(9) + t2(a,c,m,j) * toooo(m, i, l, k)
term(10) = term(10) + t2(a,c,i,m) * toooo(m, k, l, j)
term(11) = term(11) + t2(a,c,m,k) * toooo(m, i, l, j)
end do 

term(8) = term(8) * (-0.4999999999999998d+0) 
term(9) = term(9) * (-0.4999999999999998d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjckbl = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjckbl = eom_cc3_31_triplet_trans_aibjckbl + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckbl
    function eom_cc3_31_triplet_trans_aibjckcl(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aibjckcl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * tvoov(a, i, l, e)
term(1) = term(1) + t2(b,e,j,k) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(b, k, l, e)
term(3) = term(3) + t2(a,e,i,k) * tvoov(b, j, l, e)
term(4) = term(4) + t2(a,e,i,k) * tvvoo(b, e, l, j)
term(5) = term(5) + t2(a,e,i,j) * tvvoo(b, e, l, k)
term(6) = term(6) + t2(b,e,k,i) * tvvoo(a, e, l, j)
term(7) = term(7) + t2(b,e,j,i) * tvvoo(a, e, l, k)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,b,i,m) * toooo(m, k, l, j)
term(9) = term(9) + t2(a,b,i,m) * toooo(m, j, l, k)
term(10) = term(10) + t2(a,b,m,k) * toooo(m, i, l, j)
term(11) = term(11) + t2(a,b,m,j) * toooo(m, i, l, k)
end do 

term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * 0.49999999999999983d+0 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjckcl = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjckcl = eom_cc3_31_triplet_trans_aibjckcl + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckcl
    function eom_cc3_31_triplet_trans_aibjckdi(t2, nocc, nactive, a, b, j, c, k, d) 
    double precision :: eom_cc3_31_triplet_trans_aibjckdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(c, e, a, d)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(b, e, a, d)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(b, e, a, d)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5000000000000001d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5000000000000001d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,j) * tvoov(c, k, m, d)
term(5) = term(5) + t2(a,c,m,j) * tvoov(b, k, m, d)
term(6) = term(6) + t2(a,c,m,k) * tvoov(b, j, m, d)
term(7) = term(7) + t2(a,b,m,k) * tvoov(c, j, m, d)
term(8) = term(8) + t2(b,c,k,m) * tvvoo(a, d, m, j)
term(9) = term(9) + t2(b,c,j,m) * tvvoo(a, d, m, k)
term(10) = term(10) + t2(b,c,m,j) * tvvoo(a, d, m, k)
term(11) = term(11) + t2(b,c,m,k) * tvvoo(a, d, m, j)
end do 

term(4) = term(4) * 0.5000000000000001d+0 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * 0.5000000000000001d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * 0.5000000000000001d+0 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * 0.5000000000000001d+0 


    eom_cc3_31_triplet_trans_aibjckdi = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjckdi = eom_cc3_31_triplet_trans_aibjckdi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckdi
    function eom_cc3_31_triplet_trans_aibjckdj(t2, nocc, nactive, a, i, b, c, k, d) 
    double precision :: eom_cc3_31_triplet_trans_aibjckdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, d, b, e)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(c, d, a, e)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, b, d)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, d, a, e)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,k,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,c,i,m) * tvoov(b, k, m, d)
term(6) = term(6) + t2(a,b,i,m) * tvvoo(c, d, m, k)
term(7) = term(7) + t2(a,b,m,k) * tvvoo(c, d, m, i)
term(8) = term(8) + t2(b,c,m,k) * tvoov(a, i, m, d)
term(9) = term(9) + t2(a,b,i,m) * tvoov(c, k, m, d)
term(10) = term(10) + t2(a,c,i,m) * tvvoo(b, d, m, k)
term(11) = term(11) + t2(a,c,m,k) * tvvoo(b, d, m, i)
end do 

term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * 0.49999999999999983d+0 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * 0.49999999999999983d+0 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aibjckdj = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjckdj = eom_cc3_31_triplet_trans_aibjckdj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckdj
    function eom_cc3_31_triplet_trans_aibjckdk(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_31_triplet_trans_aibjckdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, d)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, d, b, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,j,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(b,c,m,j) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvoov(b, j, m, d)
term(7) = term(7) + t2(a,b,i,m) * tvoov(c, j, m, d)
term(8) = term(8) + t2(a,c,i,m) * tvvoo(b, d, m, j)
term(9) = term(9) + t2(a,b,i,m) * tvvoo(c, d, m, j)
term(10) = term(10) + t2(a,c,m,j) * tvvoo(b, d, m, i)
term(11) = term(11) + t2(a,b,m,j) * tvvoo(c, d, m, i)
end do 

term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * 0.4999999999999998d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.4999999999999998d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aibjckdk = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjckdk = eom_cc3_31_triplet_trans_aibjckdk + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckdk
    function eom_cc3_31_triplet_trans_aiajckal(t2, nocc, nactive, a, i, j, c, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aiajckal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,k) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,k) * tvoov(c, j, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvvoo(c, e, l, k)
term(3) = term(3) + t2(c,e,j,i) * tvvoo(a, e, l, k)
term(4) = term(4) + t2(c,e,k,j) * tvoov(a, i, l, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(c, k, l, e)
term(6) = term(6) + t2(a,e,i,k) * tvvoo(c, e, l, j)
term(7) = term(7) + t2(c,e,k,i) * tvvoo(a, e, l, j)
term(8) = term(8) + t2(a,e,j,i) * tvoov(c, k, l, e)
term(9) = term(9) + t2(c,e,j,i) * tvoov(a, k, l, e)
term(10) = term(10) + t2(c,e,k,i) * tvoov(a, j, l, e)
term(11) = term(11) + t2(a,e,k,i) * tvoov(c, j, l, e)
term(12) = term(12) + t2(a,e,k,j) * tvvoo(c, e, l, i)
term(13) = term(13) + t2(a,e,j,k) * tvvoo(c, e, l, i)
term(14) = term(14) + t2(c,e,j,k) * tvvoo(a, e, l, i)
term(15) = term(15) + t2(c,e,k,j) * tvvoo(a, e, l, i)
end do 

term(0) = term(0) * 0.4999999999999998d+0 
term(1) = term(1) * 0.4999999999999998d+0 
term(2) = term(2) * 0.4999999999999998d+0 
term(3) = term(3) * 0.4999999999999998d+0 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * 0.5000000000000001d+0 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * 0.5000000000000001d+0 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * 0.5000000000000001d+0 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * 0.5000000000000001d+0 

do m = 1, nocc 
term(16) = term(16) + t2(a,c,i,m) * toooo(m, j, l, k)
term(17) = term(17) + t2(a,c,m,j) * toooo(m, i, l, k)
term(18) = term(18) + t2(a,c,i,m) * toooo(m, k, l, j)
term(19) = term(19) + t2(a,c,m,k) * toooo(m, i, l, j)
term(20) = term(20) + t2(a,c,k,m) * toooo(m, j, l, i)
term(21) = term(21) + t2(a,c,j,m) * toooo(m, k, l, i)
term(22) = term(22) + t2(a,c,m,j) * toooo(m, k, l, i)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, j, l, i)
end do 

term(16) = term(16) * (-0.4999999999999998d+0) 
term(17) = term(17) * (-0.4999999999999998d+0) 
term(18) = term(18) * 0.49999999999999983d+0 
term(19) = term(19) * 0.49999999999999983d+0 
term(20) = term(20) * 0.5d+0 
term(21) = term(21) * (-0.5000000000000001d+0) 
term(22) = term(22) * 0.5d+0 
term(23) = term(23) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aiajckal = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aiajckal = eom_cc3_31_triplet_trans_aiajckal + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajckal
    function eom_cc3_31_triplet_trans_aiajckcl(t2, nocc, nactive, a, i, j, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aiajckcl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,j) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,j,k) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(a, k, l, e)
term(3) = term(3) + t2(a,e,i,k) * tvoov(a, j, l, e)
term(4) = term(4) + t2(a,e,i,k) * tvvoo(a, e, l, j)
term(5) = term(5) + t2(a,e,i,j) * tvvoo(a, e, l, k)
term(6) = term(6) + t2(a,e,k,i) * tvvoo(a, e, l, j)
term(7) = term(7) + t2(a,e,j,i) * tvvoo(a, e, l, k)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,a,i,m) * toooo(m, k, l, j)
term(9) = term(9) + t2(a,a,i,m) * toooo(m, j, l, k)
term(10) = term(10) + t2(a,a,k,m) * toooo(m, i, l, j)
term(11) = term(11) + t2(a,a,j,m) * toooo(m, i, l, k)
end do 

term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * 0.49999999999999983d+0 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aiajckcl = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aiajckcl = eom_cc3_31_triplet_trans_aiajckcl + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajckcl
    function eom_cc3_31_triplet_trans_aiajckdi(t2, nocc, nactive, a, j, c, k, d) 
    double precision :: eom_cc3_31_triplet_trans_aiajckdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, j, c, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,j) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(a,e,j,k) * read_ftvvvv(c, e, a, d)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(a, e, a, d)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(a, e, a, d)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5000000000000001d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5000000000000001d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,a,j,m) * tvoov(c, k, m, d)
term(5) = term(5) + t2(a,c,m,j) * tvoov(a, k, m, d)
term(6) = term(6) + t2(a,c,m,k) * tvoov(a, j, m, d)
term(7) = term(7) + t2(a,a,k,m) * tvoov(c, j, m, d)
term(8) = term(8) + t2(a,c,k,m) * tvvoo(a, d, m, j)
term(9) = term(9) + t2(a,c,j,m) * tvvoo(a, d, m, k)
term(10) = term(10) + t2(a,c,m,j) * tvvoo(a, d, m, k)
term(11) = term(11) + t2(a,c,m,k) * tvvoo(a, d, m, j)
end do 

term(4) = term(4) * 0.5000000000000001d+0 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * 0.5000000000000001d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * 0.5000000000000001d+0 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * 0.5000000000000001d+0 


    eom_cc3_31_triplet_trans_aiajckdi = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aiajckdi = eom_cc3_31_triplet_trans_aiajckdi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajckdi
    function eom_cc3_31_triplet_trans_aiajckdj(t2, nocc, nactive, a, i, c, k, d) 
    double precision :: eom_cc3_31_triplet_trans_aiajckdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, d, a, e)
term(1) = term(1) + t2(a,e,k,i) * read_ftvvvv(c, d, a, e)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, a, d)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(a, e, a, d)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,k,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,c,i,m) * tvoov(a, k, m, d)
term(6) = term(6) + t2(a,a,i,m) * tvvoo(c, d, m, k)
term(7) = term(7) + t2(a,a,k,m) * tvvoo(c, d, m, i)
term(8) = term(8) + t2(a,c,m,k) * tvoov(a, i, m, d)
term(9) = term(9) + t2(a,a,i,m) * tvoov(c, k, m, d)
term(10) = term(10) + t2(a,c,i,m) * tvvoo(a, d, m, k)
term(11) = term(11) + t2(a,c,m,k) * tvvoo(a, d, m, i)
end do 

term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * 0.49999999999999983d+0 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * 0.49999999999999983d+0 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aiajckdj = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aiajckdj = eom_cc3_31_triplet_trans_aiajckdj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajckdj
    function eom_cc3_31_triplet_trans_aiajckdk(t2, nocc, nactive, a, i, j, c, d) 
    double precision :: eom_cc3_31_triplet_trans_aiajckdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, d, a, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(a, e, a, d)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,j,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,c,m,j) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvoov(a, j, m, d)
term(7) = term(7) + t2(a,a,i,m) * tvoov(c, j, m, d)
term(8) = term(8) + t2(a,c,i,m) * tvvoo(a, d, m, j)
term(9) = term(9) + t2(a,a,i,m) * tvvoo(c, d, m, j)
term(10) = term(10) + t2(a,c,m,j) * tvvoo(a, d, m, i)
term(11) = term(11) + t2(a,a,j,m) * tvvoo(c, d, m, i)
end do 

term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * 0.4999999999999998d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.4999999999999998d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aiajckdk = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aiajckdk = eom_cc3_31_triplet_trans_aiajckdk + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajckdk
    function eom_cc3_31_triplet_trans_aibjakal(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aibjakal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * tvoov(a, i, l, e)
term(1) = term(1) + t2(b,e,j,k) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(b, k, l, e)
term(3) = term(3) + t2(a,e,i,k) * tvoov(b, j, l, e)
term(4) = term(4) + t2(a,e,i,k) * tvvoo(b, e, l, j)
term(5) = term(5) + t2(a,e,i,j) * tvvoo(b, e, l, k)
term(6) = term(6) + t2(b,e,k,i) * tvvoo(a, e, l, j)
term(7) = term(7) + t2(b,e,j,i) * tvvoo(a, e, l, k)
term(8) = term(8) + t2(b,e,j,i) * tvoov(a, k, l, e)
term(9) = term(9) + t2(a,e,j,i) * tvoov(b, k, l, e)
term(10) = term(10) + t2(a,e,k,i) * tvoov(b, j, l, e)
term(11) = term(11) + t2(b,e,k,i) * tvoov(a, j, l, e)
term(12) = term(12) + t2(b,e,k,j) * tvvoo(a, e, l, i)
term(13) = term(13) + t2(b,e,j,k) * tvvoo(a, e, l, i)
term(14) = term(14) + t2(a,e,j,k) * tvvoo(b, e, l, i)
term(15) = term(15) + t2(a,e,k,j) * tvvoo(b, e, l, i)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * 0.5000000000000001d+0 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * 0.5000000000000001d+0 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * 0.5000000000000001d+0 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * 0.5000000000000001d+0 

do m = 1, nocc 
term(16) = term(16) + t2(a,b,i,m) * toooo(m, k, l, j)
term(17) = term(17) + t2(a,b,i,m) * toooo(m, j, l, k)
term(18) = term(18) + t2(a,b,m,k) * toooo(m, i, l, j)
term(19) = term(19) + t2(a,b,m,j) * toooo(m, i, l, k)
term(20) = term(20) + t2(a,b,m,k) * toooo(m, j, l, i)
term(21) = term(21) + t2(a,b,m,j) * toooo(m, k, l, i)
term(22) = term(22) + t2(a,b,j,m) * toooo(m, k, l, i)
term(23) = term(23) + t2(a,b,k,m) * toooo(m, j, l, i)
end do 

term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * 0.49999999999999983d+0 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * 0.49999999999999983d+0 
term(20) = term(20) * 0.5d+0 
term(21) = term(21) * (-0.5000000000000001d+0) 
term(22) = term(22) * 0.5d+0 
term(23) = term(23) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibjakal = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjakal = eom_cc3_31_triplet_trans_aibjakal + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjakal
    function eom_cc3_31_triplet_trans_aibjakbl(t2, nocc, nactive, a, i, j, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aibjakbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,k) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,k) * tvoov(a, j, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvvoo(a, e, l, k)
term(3) = term(3) + t2(a,e,j,i) * tvvoo(a, e, l, k)
term(4) = term(4) + t2(a,e,k,j) * tvoov(a, i, l, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(a, k, l, e)
term(6) = term(6) + t2(a,e,i,k) * tvvoo(a, e, l, j)
term(7) = term(7) + t2(a,e,k,i) * tvvoo(a, e, l, j)
end do 

term(0) = term(0) * 0.4999999999999998d+0 
term(1) = term(1) * 0.4999999999999998d+0 
term(2) = term(2) * 0.4999999999999998d+0 
term(3) = term(3) * 0.4999999999999998d+0 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,a,i,m) * toooo(m, j, l, k)
term(9) = term(9) + t2(a,a,j,m) * toooo(m, i, l, k)
term(10) = term(10) + t2(a,a,i,m) * toooo(m, k, l, j)
term(11) = term(11) + t2(a,a,k,m) * toooo(m, i, l, j)
end do 

term(8) = term(8) * (-0.4999999999999998d+0) 
term(9) = term(9) * (-0.4999999999999998d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjakbl = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjakbl = eom_cc3_31_triplet_trans_aibjakbl + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjakbl
    function eom_cc3_31_triplet_trans_aibjakdi(t2, nocc, nactive, a, b, j, k, d) 
    double precision :: eom_cc3_31_triplet_trans_aibjakdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(a, e, a, d)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,j,k) * read_ftvvvv(b, e, a, d)
term(3) = term(3) + t2(a,e,k,j) * read_ftvvvv(b, e, a, d)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5000000000000001d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5000000000000001d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,j) * tvoov(a, k, m, d)
term(5) = term(5) + t2(a,a,j,m) * tvoov(b, k, m, d)
term(6) = term(6) + t2(a,a,k,m) * tvoov(b, j, m, d)
term(7) = term(7) + t2(a,b,m,k) * tvoov(a, j, m, d)
term(8) = term(8) + t2(a,b,m,k) * tvvoo(a, d, m, j)
term(9) = term(9) + t2(a,b,m,j) * tvvoo(a, d, m, k)
term(10) = term(10) + t2(a,b,j,m) * tvvoo(a, d, m, k)
term(11) = term(11) + t2(a,b,k,m) * tvvoo(a, d, m, j)
end do 

term(4) = term(4) * 0.5000000000000001d+0 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * 0.5000000000000001d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * 0.5000000000000001d+0 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * 0.5000000000000001d+0 


    eom_cc3_31_triplet_trans_aibjakdi = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjakdi = eom_cc3_31_triplet_trans_aibjakdi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjakdi
    function eom_cc3_31_triplet_trans_aibjakdj(t2, nocc, nactive, a, i, b, k, d) 
    double precision :: eom_cc3_31_triplet_trans_aibjakdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(b, e, a, d)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(a,e,k,i) * read_ftvvvv(b, d, a, e)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,k) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,a,i,m) * tvoov(b, k, m, d)
term(6) = term(6) + t2(a,b,i,m) * tvvoo(a, d, m, k)
term(7) = term(7) + t2(a,b,m,k) * tvvoo(a, d, m, i)
term(8) = term(8) + t2(a,b,k,m) * tvoov(a, i, m, d)
term(9) = term(9) + t2(a,b,i,m) * tvoov(a, k, m, d)
term(10) = term(10) + t2(a,a,i,m) * tvvoo(b, d, m, k)
term(11) = term(11) + t2(a,a,k,m) * tvvoo(b, d, m, i)
end do 

term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * 0.49999999999999983d+0 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * 0.49999999999999983d+0 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aibjakdj = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjakdj = eom_cc3_31_triplet_trans_aibjakdj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjakdj
    function eom_cc3_31_triplet_trans_aibjakdk(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_31_triplet_trans_aibjakdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(b, d, a, e)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(a, e, a, d)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,j) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,b,j,m) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,a,i,m) * tvoov(b, j, m, d)
term(7) = term(7) + t2(a,b,i,m) * tvoov(a, j, m, d)
term(8) = term(8) + t2(a,a,i,m) * tvvoo(b, d, m, j)
term(9) = term(9) + t2(a,b,i,m) * tvvoo(a, d, m, j)
term(10) = term(10) + t2(a,a,j,m) * tvvoo(b, d, m, i)
term(11) = term(11) + t2(a,b,m,j) * tvvoo(a, d, m, i)
end do 

term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * 0.4999999999999998d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.4999999999999998d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aibjakdk = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjakdk = eom_cc3_31_triplet_trans_aibjakdk + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjakdk
    function eom_cc3_31_triplet_trans_aibjckai(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjckai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(c, e, a, a)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(b, e, a, a)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(b, e, a, a)
term(4) = term(4) + t2(b,e,j,i) * tvoov(c, k, i, e)
term(5) = term(5) + t2(c,e,j,i) * tvoov(b, k, i, e)
term(6) = term(6) + t2(c,e,k,i) * tvoov(b, j, i, e)
term(7) = term(7) + t2(b,e,k,i) * tvoov(c, j, i, e)
term(8) = term(8) + t2(b,e,k,j) * tvvoo(c, e, i, i)
term(9) = term(9) + t2(b,e,j,k) * tvvoo(c, e, i, i)
term(10) = term(10) + t2(c,e,j,k) * tvvoo(b, e, i, i)
term(11) = term(11) + t2(c,e,k,j) * tvvoo(b, e, i, i)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5000000000000001d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5000000000000001d+0) 
term(4) = term(4) * 0.5000000000000001d+0 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * 0.5000000000000001d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * 0.5000000000000001d+0 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * 0.5000000000000001d+0 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,m,j) * tvoov(c, k, m, a)
term(13) = term(13) + t2(a,c,m,j) * tvoov(b, k, m, a)
term(14) = term(14) + t2(a,c,m,k) * tvoov(b, j, m, a)
term(15) = term(15) + t2(a,b,m,k) * tvoov(c, j, m, a)
term(16) = term(16) + t2(b,c,k,m) * tvvoo(a, a, m, j)
term(17) = term(17) + t2(b,c,j,m) * tvvoo(a, a, m, k)
term(18) = term(18) + t2(b,c,m,j) * tvvoo(a, a, m, k)
term(19) = term(19) + t2(b,c,m,k) * tvvoo(a, a, m, j)
term(20) = term(20) + t2(b,c,k,m) * toooo(m, j, i, i)
term(21) = term(21) + t2(b,c,j,m) * toooo(m, k, i, i)
term(22) = term(22) + t2(b,c,m,j) * toooo(m, k, i, i)
term(23) = term(23) + t2(b,c,m,k) * toooo(m, j, i, i)
end do 

term(12) = term(12) * 0.5000000000000001d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5000000000000001d+0 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * 0.5000000000000001d+0 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * 0.5000000000000001d+0 
term(20) = term(20) * 0.5d+0 
term(21) = term(21) * (-0.5000000000000001d+0) 
term(22) = term(22) * 0.5d+0 
term(23) = term(23) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibjckai = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjckai = eom_cc3_31_triplet_trans_aibjckai + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckai
    function eom_cc3_31_triplet_trans_aibjckaj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjckaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, a, b, e)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(c, a, a, e)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, b, a)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(b,e,j,i) * tvoov(c, k, j, e)
term(5) = term(5) + t2(c,e,j,i) * tvoov(b, k, j, e)
term(6) = term(6) + t2(c,e,k,i) * tvoov(b, j, j, e)
term(7) = term(7) + t2(b,e,k,i) * tvoov(c, j, j, e)
term(8) = term(8) + t2(b,e,k,j) * tvvoo(c, e, j, i)
term(9) = term(9) + t2(b,e,j,k) * tvvoo(c, e, j, i)
term(10) = term(10) + t2(c,e,j,k) * tvvoo(b, e, j, i)
term(11) = term(11) + t2(c,e,k,j) * tvvoo(b, e, j, i)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.5000000000000001d+0 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * 0.5000000000000001d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * 0.5000000000000001d+0 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * 0.5000000000000001d+0 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,k,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,c,i,m) * tvoov(b, k, m, a)
term(14) = term(14) + t2(a,b,i,m) * tvvoo(c, a, m, k)
term(15) = term(15) + t2(a,b,m,k) * tvvoo(c, a, m, i)
term(16) = term(16) + t2(b,c,m,k) * tvoov(a, i, m, a)
term(17) = term(17) + t2(a,b,i,m) * tvoov(c, k, m, a)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(b, a, m, k)
term(19) = term(19) + t2(a,c,m,k) * tvvoo(b, a, m, i)
term(20) = term(20) + t2(b,c,k,m) * toooo(m, j, j, i)
term(21) = term(21) + t2(b,c,j,m) * toooo(m, k, j, i)
term(22) = term(22) + t2(b,c,m,j) * toooo(m, k, j, i)
term(23) = term(23) + t2(b,c,m,k) * toooo(m, j, j, i)
end do 

term(12) = term(12) * 0.49999999999999983d+0 
term(13) = term(13) * 0.49999999999999983d+0 
term(14) = term(14) * 0.49999999999999983d+0 
term(15) = term(15) * 0.49999999999999983d+0 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * 0.5d+0 
term(21) = term(21) * (-0.5000000000000001d+0) 
term(22) = term(22) * 0.5d+0 
term(23) = term(23) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibjckaj = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjckaj = eom_cc3_31_triplet_trans_aibjckaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckaj
    function eom_cc3_31_triplet_trans_aibjckak(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjckak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, a)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, a, b, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, a, a, e)
term(4) = term(4) + t2(b,e,j,i) * tvoov(c, k, k, e)
term(5) = term(5) + t2(c,e,j,i) * tvoov(b, k, k, e)
term(6) = term(6) + t2(c,e,k,i) * tvoov(b, j, k, e)
term(7) = term(7) + t2(b,e,k,i) * tvoov(c, j, k, e)
term(8) = term(8) + t2(b,e,k,j) * tvvoo(c, e, k, i)
term(9) = term(9) + t2(b,e,j,k) * tvvoo(c, e, k, i)
term(10) = term(10) + t2(c,e,j,k) * tvvoo(b, e, k, i)
term(11) = term(11) + t2(c,e,k,j) * tvvoo(b, e, k, i)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.5000000000000001d+0 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * 0.5000000000000001d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * 0.5000000000000001d+0 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * 0.5000000000000001d+0 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,j,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(b,c,m,j) * tvoov(a, i, m, a)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, j, m, a)
term(15) = term(15) + t2(a,b,i,m) * tvoov(c, j, m, a)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(b, a, m, j)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(c, a, m, j)
term(18) = term(18) + t2(a,c,m,j) * tvvoo(b, a, m, i)
term(19) = term(19) + t2(a,b,m,j) * tvvoo(c, a, m, i)
term(20) = term(20) + t2(b,c,k,m) * toooo(m, j, k, i)
term(21) = term(21) + t2(b,c,j,m) * toooo(m, k, k, i)
term(22) = term(22) + t2(b,c,m,j) * toooo(m, k, k, i)
term(23) = term(23) + t2(b,c,m,k) * toooo(m, j, k, i)
end do 

term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * 0.4999999999999998d+0 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * 0.4999999999999998d+0 
term(16) = term(16) * 0.4999999999999998d+0 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * 0.4999999999999998d+0 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * 0.5d+0 
term(21) = term(21) * (-0.5000000000000001d+0) 
term(22) = term(22) * 0.5d+0 
term(23) = term(23) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibjckak = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjckak = eom_cc3_31_triplet_trans_aibjckak + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckak
    function eom_cc3_31_triplet_trans_aibickal(t2, nocc, nactive, i, b, c, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aibickal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, c, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,i) * tvoov(c, k, l, e)
term(1) = term(1) + t2(c,e,i,i) * tvoov(b, k, l, e)
term(2) = term(2) + t2(c,e,k,i) * tvoov(b, i, l, e)
term(3) = term(3) + t2(b,e,k,i) * tvoov(c, i, l, e)
term(4) = term(4) + t2(b,e,k,i) * tvvoo(c, e, l, i)
term(5) = term(5) + t2(b,e,i,k) * tvvoo(c, e, l, i)
term(6) = term(6) + t2(c,e,i,k) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(c,e,k,i) * tvvoo(b, e, l, i)
end do 

term(0) = term(0) * 0.5000000000000001d+0 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5000000000000001d+0 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * 0.5000000000000001d+0 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * 0.5000000000000001d+0 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,k,m) * toooo(m, i, l, i)
term(9) = term(9) + t2(b,c,i,m) * toooo(m, k, l, i)
term(10) = term(10) + t2(b,c,m,i) * toooo(m, k, l, i)
term(11) = term(11) + t2(b,c,m,k) * toooo(m, i, l, i)
end do 

term(8) = term(8) * 0.5d+0 
term(9) = term(9) * (-0.5000000000000001d+0) 
term(10) = term(10) * 0.5d+0 
term(11) = term(11) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibickal = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibickal = eom_cc3_31_triplet_trans_aibickal + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibickal
    function eom_cc3_31_triplet_trans_aibjcial(t2, nocc, nactive, i, b, j, c, l) 
    double precision :: eom_cc3_31_triplet_trans_aibjcial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,j,i) * tvoov(c, i, l, e)
term(1) = term(1) + t2(c,e,j,i) * tvoov(b, i, l, e)
term(2) = term(2) + t2(c,e,i,i) * tvoov(b, j, l, e)
term(3) = term(3) + t2(b,e,i,i) * tvoov(c, j, l, e)
term(4) = term(4) + t2(b,e,i,j) * tvvoo(c, e, l, i)
term(5) = term(5) + t2(b,e,j,i) * tvvoo(c, e, l, i)
term(6) = term(6) + t2(c,e,j,i) * tvvoo(b, e, l, i)
term(7) = term(7) + t2(c,e,i,j) * tvvoo(b, e, l, i)
end do 

term(0) = term(0) * 0.5000000000000001d+0 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5000000000000001d+0 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * 0.5000000000000001d+0 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * 0.5000000000000001d+0 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,i,m) * toooo(m, j, l, i)
term(9) = term(9) + t2(b,c,j,m) * toooo(m, i, l, i)
term(10) = term(10) + t2(b,c,m,j) * toooo(m, i, l, i)
term(11) = term(11) + t2(b,c,m,i) * toooo(m, j, l, i)
end do 

term(8) = term(8) * 0.5d+0 
term(9) = term(9) * (-0.5000000000000001d+0) 
term(10) = term(10) * 0.5d+0 
term(11) = term(11) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibjcial = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjcial = eom_cc3_31_triplet_trans_aibjcial + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjcial
    function eom_cc3_31_triplet_trans_aibjckbi(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjckbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(c, e, a, b)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(c, e, a, b)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(b, e, a, b)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(b, e, a, b)
term(4) = term(4) + t2(c,e,j,k) * tvoov(a, i, i, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(c, j, i, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(c, e, i, k)
term(7) = term(7) + t2(c,e,j,i) * tvvoo(a, e, i, k)
term(8) = term(8) + t2(c,e,k,j) * tvoov(a, i, i, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(c, k, i, e)
term(10) = term(10) + t2(a,e,i,k) * tvvoo(c, e, i, j)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(a, e, i, j)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5000000000000001d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5000000000000001d+0) 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,m,j) * tvoov(c, k, m, b)
term(13) = term(13) + t2(a,c,m,j) * tvoov(b, k, m, b)
term(14) = term(14) + t2(a,c,m,k) * tvoov(b, j, m, b)
term(15) = term(15) + t2(a,b,m,k) * tvoov(c, j, m, b)
term(16) = term(16) + t2(b,c,k,m) * tvvoo(a, b, m, j)
term(17) = term(17) + t2(b,c,j,m) * tvvoo(a, b, m, k)
term(18) = term(18) + t2(b,c,m,j) * tvvoo(a, b, m, k)
term(19) = term(19) + t2(b,c,m,k) * tvvoo(a, b, m, j)
term(20) = term(20) + t2(a,c,i,m) * toooo(m, j, i, k)
term(21) = term(21) + t2(a,c,m,j) * toooo(m, i, i, k)
term(22) = term(22) + t2(a,c,i,m) * toooo(m, k, i, j)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, i, i, j)
end do 

term(12) = term(12) * 0.5000000000000001d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5000000000000001d+0 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * 0.5000000000000001d+0 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * 0.5000000000000001d+0 
term(20) = term(20) * (-0.4999999999999998d+0) 
term(21) = term(21) * (-0.4999999999999998d+0) 
term(22) = term(22) * 0.49999999999999983d+0 
term(23) = term(23) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjckbi = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjckbi = eom_cc3_31_triplet_trans_aibjckbi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckbi
    function eom_cc3_31_triplet_trans_aibjckbj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjckbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, b, b, e)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(c, b, a, e)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, b, b)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, b, a, e)
term(4) = term(4) + t2(c,e,j,k) * tvoov(a, i, j, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(c, j, j, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(c, e, j, k)
term(7) = term(7) + t2(c,e,j,i) * tvvoo(a, e, j, k)
term(8) = term(8) + t2(c,e,k,j) * tvoov(a, i, j, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(c, k, j, e)
term(10) = term(10) + t2(a,e,i,k) * tvvoo(c, e, j, j)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(a, e, j, j)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,k,m) * tvoov(a, i, m, b)
term(13) = term(13) + t2(a,c,i,m) * tvoov(b, k, m, b)
term(14) = term(14) + t2(a,b,i,m) * tvvoo(c, b, m, k)
term(15) = term(15) + t2(a,b,m,k) * tvvoo(c, b, m, i)
term(16) = term(16) + t2(b,c,m,k) * tvoov(a, i, m, b)
term(17) = term(17) + t2(a,b,i,m) * tvoov(c, k, m, b)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(b, b, m, k)
term(19) = term(19) + t2(a,c,m,k) * tvvoo(b, b, m, i)
term(20) = term(20) + t2(a,c,i,m) * toooo(m, j, j, k)
term(21) = term(21) + t2(a,c,m,j) * toooo(m, i, j, k)
term(22) = term(22) + t2(a,c,i,m) * toooo(m, k, j, j)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, i, j, j)
end do 

term(12) = term(12) * 0.49999999999999983d+0 
term(13) = term(13) * 0.49999999999999983d+0 
term(14) = term(14) * 0.49999999999999983d+0 
term(15) = term(15) * 0.49999999999999983d+0 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * (-0.4999999999999998d+0) 
term(21) = term(21) * (-0.4999999999999998d+0) 
term(22) = term(22) * 0.49999999999999983d+0 
term(23) = term(23) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjckbj = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjckbj = eom_cc3_31_triplet_trans_aibjckbj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckbj
    function eom_cc3_31_triplet_trans_aibjckbk(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjckbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, b)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, b, b, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, b, a, e)
term(4) = term(4) + t2(c,e,j,k) * tvoov(a, i, k, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(c, j, k, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(c, e, k, k)
term(7) = term(7) + t2(c,e,j,i) * tvvoo(a, e, k, k)
term(8) = term(8) + t2(c,e,k,j) * tvoov(a, i, k, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(c, k, k, e)
term(10) = term(10) + t2(a,e,i,k) * tvvoo(c, e, k, j)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(a, e, k, j)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,j,m) * tvoov(a, i, m, b)
term(13) = term(13) + t2(b,c,m,j) * tvoov(a, i, m, b)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, j, m, b)
term(15) = term(15) + t2(a,b,i,m) * tvoov(c, j, m, b)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(b, b, m, j)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(c, b, m, j)
term(18) = term(18) + t2(a,c,m,j) * tvvoo(b, b, m, i)
term(19) = term(19) + t2(a,b,m,j) * tvvoo(c, b, m, i)
term(20) = term(20) + t2(a,c,i,m) * toooo(m, j, k, k)
term(21) = term(21) + t2(a,c,m,j) * toooo(m, i, k, k)
term(22) = term(22) + t2(a,c,i,m) * toooo(m, k, k, j)
term(23) = term(23) + t2(a,c,m,k) * toooo(m, i, k, j)
end do 

term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * 0.4999999999999998d+0 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * 0.4999999999999998d+0 
term(16) = term(16) * 0.4999999999999998d+0 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * 0.4999999999999998d+0 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * (-0.4999999999999998d+0) 
term(21) = term(21) * (-0.4999999999999998d+0) 
term(22) = term(22) * 0.49999999999999983d+0 
term(23) = term(23) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjckbk = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjckbk = eom_cc3_31_triplet_trans_aibjckbk + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckbk
    function eom_cc3_31_triplet_trans_aibickbl(t2, nocc, nactive, a, i, c, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aibickbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,k) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,k) * tvoov(c, i, l, e)
term(2) = term(2) + t2(a,e,i,i) * tvvoo(c, e, l, k)
term(3) = term(3) + t2(c,e,i,i) * tvvoo(a, e, l, k)
term(4) = term(4) + t2(c,e,k,i) * tvoov(a, i, l, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(c, k, l, e)
term(6) = term(6) + t2(a,e,i,k) * tvvoo(c, e, l, i)
term(7) = term(7) + t2(c,e,k,i) * tvvoo(a, e, l, i)
end do 

term(0) = term(0) * 0.4999999999999998d+0 
term(1) = term(1) * 0.4999999999999998d+0 
term(2) = term(2) * 0.4999999999999998d+0 
term(3) = term(3) * 0.4999999999999998d+0 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,c,i,m) * toooo(m, i, l, k)
term(9) = term(9) + t2(a,c,m,i) * toooo(m, i, l, k)
term(10) = term(10) + t2(a,c,i,m) * toooo(m, k, l, i)
term(11) = term(11) + t2(a,c,m,k) * toooo(m, i, l, i)
end do 

term(8) = term(8) * (-0.4999999999999998d+0) 
term(9) = term(9) * (-0.4999999999999998d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibickbl = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibickbl = eom_cc3_31_triplet_trans_aibickbl + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibickbl
    function eom_cc3_31_triplet_trans_aibjcibl(t2, nocc, nactive, a, i, j, c, l) 
    double precision :: eom_cc3_31_triplet_trans_aibjcibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,i) * tvoov(c, j, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvvoo(c, e, l, i)
term(3) = term(3) + t2(c,e,j,i) * tvvoo(a, e, l, i)
term(4) = term(4) + t2(c,e,i,j) * tvoov(a, i, l, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(c, i, l, e)
term(6) = term(6) + t2(a,e,i,i) * tvvoo(c, e, l, j)
term(7) = term(7) + t2(c,e,i,i) * tvvoo(a, e, l, j)
end do 

term(0) = term(0) * 0.4999999999999998d+0 
term(1) = term(1) * 0.4999999999999998d+0 
term(2) = term(2) * 0.4999999999999998d+0 
term(3) = term(3) * 0.4999999999999998d+0 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,c,i,m) * toooo(m, j, l, i)
term(9) = term(9) + t2(a,c,m,j) * toooo(m, i, l, i)
term(10) = term(10) + t2(a,c,i,m) * toooo(m, i, l, j)
term(11) = term(11) + t2(a,c,m,i) * toooo(m, i, l, j)
end do 

term(8) = term(8) * (-0.4999999999999998d+0) 
term(9) = term(9) * (-0.4999999999999998d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjcibl = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjcibl = eom_cc3_31_triplet_trans_aibjcibl + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjcibl
    function eom_cc3_31_triplet_trans_aibjckci(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjckci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(c, e, a, c)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(b, e, a, c)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(b, e, a, c)
term(4) = term(4) + t2(b,e,k,j) * tvoov(a, i, i, e)
term(5) = term(5) + t2(b,e,j,k) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(b, k, i, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(b, j, i, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(b, e, i, j)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(b, e, i, k)
term(10) = term(10) + t2(b,e,k,i) * tvvoo(a, e, i, j)
term(11) = term(11) + t2(b,e,j,i) * tvvoo(a, e, i, k)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5000000000000001d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5000000000000001d+0) 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,m,j) * tvoov(c, k, m, c)
term(13) = term(13) + t2(a,c,m,j) * tvoov(b, k, m, c)
term(14) = term(14) + t2(a,c,m,k) * tvoov(b, j, m, c)
term(15) = term(15) + t2(a,b,m,k) * tvoov(c, j, m, c)
term(16) = term(16) + t2(b,c,k,m) * tvvoo(a, c, m, j)
term(17) = term(17) + t2(b,c,j,m) * tvvoo(a, c, m, k)
term(18) = term(18) + t2(b,c,m,j) * tvvoo(a, c, m, k)
term(19) = term(19) + t2(b,c,m,k) * tvvoo(a, c, m, j)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, k, i, j)
term(21) = term(21) + t2(a,b,i,m) * toooo(m, j, i, k)
term(22) = term(22) + t2(a,b,m,k) * toooo(m, i, i, j)
term(23) = term(23) + t2(a,b,m,j) * toooo(m, i, i, k)
end do 

term(12) = term(12) * 0.5000000000000001d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5000000000000001d+0 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * 0.5000000000000001d+0 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * 0.5000000000000001d+0 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * 0.49999999999999983d+0 
term(22) = term(22) * (-0.49999999999999983d+0) 
term(23) = term(23) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjckci = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjckci = eom_cc3_31_triplet_trans_aibjckci + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckci
    function eom_cc3_31_triplet_trans_aibjckcj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjckcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, c, b, e)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(c, c, a, e)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, b, c)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, c, a, e)
term(4) = term(4) + t2(b,e,k,j) * tvoov(a, i, j, e)
term(5) = term(5) + t2(b,e,j,k) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(b, k, j, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(b, j, j, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(b, e, j, j)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(b, e, j, k)
term(10) = term(10) + t2(b,e,k,i) * tvvoo(a, e, j, j)
term(11) = term(11) + t2(b,e,j,i) * tvvoo(a, e, j, k)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,k,m) * tvoov(a, i, m, c)
term(13) = term(13) + t2(a,c,i,m) * tvoov(b, k, m, c)
term(14) = term(14) + t2(a,b,i,m) * tvvoo(c, c, m, k)
term(15) = term(15) + t2(a,b,m,k) * tvvoo(c, c, m, i)
term(16) = term(16) + t2(b,c,m,k) * tvoov(a, i, m, c)
term(17) = term(17) + t2(a,b,i,m) * tvoov(c, k, m, c)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(b, c, m, k)
term(19) = term(19) + t2(a,c,m,k) * tvvoo(b, c, m, i)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, k, j, j)
term(21) = term(21) + t2(a,b,i,m) * toooo(m, j, j, k)
term(22) = term(22) + t2(a,b,m,k) * toooo(m, i, j, j)
term(23) = term(23) + t2(a,b,m,j) * toooo(m, i, j, k)
end do 

term(12) = term(12) * 0.49999999999999983d+0 
term(13) = term(13) * 0.49999999999999983d+0 
term(14) = term(14) * 0.49999999999999983d+0 
term(15) = term(15) * 0.49999999999999983d+0 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * 0.49999999999999983d+0 
term(22) = term(22) * (-0.49999999999999983d+0) 
term(23) = term(23) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjckcj = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjckcj = eom_cc3_31_triplet_trans_aibjckcj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckcj
    function eom_cc3_31_triplet_trans_aibjckck(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjckck 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, c)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, c, b, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, c, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, c, a, e)
term(4) = term(4) + t2(b,e,k,j) * tvoov(a, i, k, e)
term(5) = term(5) + t2(b,e,j,k) * tvoov(a, i, k, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(b, k, k, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(b, j, k, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(b, e, k, j)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(b, e, k, k)
term(10) = term(10) + t2(b,e,k,i) * tvvoo(a, e, k, j)
term(11) = term(11) + t2(b,e,j,i) * tvvoo(a, e, k, k)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,j,m) * tvoov(a, i, m, c)
term(13) = term(13) + t2(b,c,m,j) * tvoov(a, i, m, c)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, j, m, c)
term(15) = term(15) + t2(a,b,i,m) * tvoov(c, j, m, c)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(b, c, m, j)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(c, c, m, j)
term(18) = term(18) + t2(a,c,m,j) * tvvoo(b, c, m, i)
term(19) = term(19) + t2(a,b,m,j) * tvvoo(c, c, m, i)
term(20) = term(20) + t2(a,b,i,m) * toooo(m, k, k, j)
term(21) = term(21) + t2(a,b,i,m) * toooo(m, j, k, k)
term(22) = term(22) + t2(a,b,m,k) * toooo(m, i, k, j)
term(23) = term(23) + t2(a,b,m,j) * toooo(m, i, k, k)
end do 

term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * 0.4999999999999998d+0 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * 0.4999999999999998d+0 
term(16) = term(16) * 0.4999999999999998d+0 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * 0.4999999999999998d+0 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * 0.49999999999999983d+0 
term(22) = term(22) * (-0.49999999999999983d+0) 
term(23) = term(23) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjckck = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjckck = eom_cc3_31_triplet_trans_aibjckck + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjckck
    function eom_cc3_31_triplet_trans_aibickcl(t2, nocc, nactive, a, i, b, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aibickcl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * tvoov(a, i, l, e)
term(1) = term(1) + t2(b,e,i,k) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,i) * tvoov(b, k, l, e)
term(3) = term(3) + t2(a,e,i,k) * tvoov(b, i, l, e)
term(4) = term(4) + t2(a,e,i,k) * tvvoo(b, e, l, i)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(b, e, l, k)
term(6) = term(6) + t2(b,e,k,i) * tvvoo(a, e, l, i)
term(7) = term(7) + t2(b,e,i,i) * tvvoo(a, e, l, k)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,b,i,m) * toooo(m, k, l, i)
term(9) = term(9) + t2(a,b,i,m) * toooo(m, i, l, k)
term(10) = term(10) + t2(a,b,m,k) * toooo(m, i, l, i)
term(11) = term(11) + t2(a,b,m,i) * toooo(m, i, l, k)
end do 

term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * 0.49999999999999983d+0 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibickcl = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibickcl = eom_cc3_31_triplet_trans_aibickcl + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibickcl
    function eom_cc3_31_triplet_trans_aibjcicl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_31_triplet_trans_aibjcicl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tvoov(a, i, l, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(b, i, l, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(b, j, l, e)
term(4) = term(4) + t2(a,e,i,i) * tvvoo(b, e, l, j)
term(5) = term(5) + t2(a,e,i,j) * tvvoo(b, e, l, i)
term(6) = term(6) + t2(b,e,i,i) * tvvoo(a, e, l, j)
term(7) = term(7) + t2(b,e,j,i) * tvvoo(a, e, l, i)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,b,i,m) * toooo(m, i, l, j)
term(9) = term(9) + t2(a,b,i,m) * toooo(m, j, l, i)
term(10) = term(10) + t2(a,b,m,i) * toooo(m, i, l, j)
term(11) = term(11) + t2(a,b,m,j) * toooo(m, i, l, i)
end do 

term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * 0.49999999999999983d+0 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjcicl = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjcicl = eom_cc3_31_triplet_trans_aibjcicl + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjcicl
    function eom_cc3_31_triplet_trans_aibickdi(t2, nocc, nactive, a, i, b, c, k, d) 
    double precision :: eom_cc3_31_triplet_trans_aibickdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, d, b, e)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(c, d, a, e)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, b, d)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, d, a, e)
term(4) = term(4) + t2(b,e,k,i) * read_ftvvvv(c, e, a, d)
term(5) = term(5) + t2(b,e,i,k) * read_ftvvvv(c, e, a, d)
term(6) = term(6) + t2(c,e,i,k) * read_ftvvvv(b, e, a, d)
term(7) = term(7) + t2(c,e,k,i) * read_ftvvvv(b, e, a, d)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5000000000000001d+0) 
term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5000000000000001d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,k,m) * tvoov(a, i, m, d)
term(9) = term(9) + t2(a,c,i,m) * tvoov(b, k, m, d)
term(10) = term(10) + t2(a,b,i,m) * tvvoo(c, d, m, k)
term(11) = term(11) + t2(a,b,m,k) * tvvoo(c, d, m, i)
term(12) = term(12) + t2(b,c,m,k) * tvoov(a, i, m, d)
term(13) = term(13) + t2(a,b,i,m) * tvoov(c, k, m, d)
term(14) = term(14) + t2(a,c,i,m) * tvvoo(b, d, m, k)
term(15) = term(15) + t2(a,c,m,k) * tvvoo(b, d, m, i)
term(16) = term(16) + t2(a,b,m,i) * tvoov(c, k, m, d)
term(17) = term(17) + t2(a,c,m,i) * tvoov(b, k, m, d)
term(18) = term(18) + t2(a,c,m,k) * tvoov(b, i, m, d)
term(19) = term(19) + t2(a,b,m,k) * tvoov(c, i, m, d)
term(20) = term(20) + t2(b,c,k,m) * tvvoo(a, d, m, i)
term(21) = term(21) + t2(b,c,i,m) * tvvoo(a, d, m, k)
term(22) = term(22) + t2(b,c,m,i) * tvvoo(a, d, m, k)
term(23) = term(23) + t2(b,c,m,k) * tvvoo(a, d, m, i)
end do 

term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * 0.49999999999999983d+0 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * 0.49999999999999983d+0 
term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * (-0.49999999999999983d+0) 
term(16) = term(16) * 0.5000000000000001d+0 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * 0.5000000000000001d+0 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * 0.5000000000000001d+0 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * 0.5000000000000001d+0 


    eom_cc3_31_triplet_trans_aibickdi = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibickdi = eom_cc3_31_triplet_trans_aibickdi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibickdi
    function eom_cc3_31_triplet_trans_aibjcidi(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_31_triplet_trans_aibjcidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, d)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, d, b, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, d, a, e)
term(4) = term(4) + t2(b,e,i,j) * read_ftvvvv(c, e, a, d)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(c, e, a, d)
term(6) = term(6) + t2(c,e,j,i) * read_ftvvvv(b, e, a, d)
term(7) = term(7) + t2(c,e,i,j) * read_ftvvvv(b, e, a, d)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5000000000000001d+0) 
term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5000000000000001d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(b,c,j,m) * tvoov(a, i, m, d)
term(9) = term(9) + t2(b,c,m,j) * tvoov(a, i, m, d)
term(10) = term(10) + t2(a,c,i,m) * tvoov(b, j, m, d)
term(11) = term(11) + t2(a,b,i,m) * tvoov(c, j, m, d)
term(12) = term(12) + t2(a,c,i,m) * tvvoo(b, d, m, j)
term(13) = term(13) + t2(a,b,i,m) * tvvoo(c, d, m, j)
term(14) = term(14) + t2(a,c,m,j) * tvvoo(b, d, m, i)
term(15) = term(15) + t2(a,b,m,j) * tvvoo(c, d, m, i)
term(16) = term(16) + t2(a,b,m,j) * tvoov(c, i, m, d)
term(17) = term(17) + t2(a,c,m,j) * tvoov(b, i, m, d)
term(18) = term(18) + t2(a,c,m,i) * tvoov(b, j, m, d)
term(19) = term(19) + t2(a,b,m,i) * tvoov(c, j, m, d)
term(20) = term(20) + t2(b,c,i,m) * tvvoo(a, d, m, j)
term(21) = term(21) + t2(b,c,j,m) * tvvoo(a, d, m, i)
term(22) = term(22) + t2(b,c,m,j) * tvvoo(a, d, m, i)
term(23) = term(23) + t2(b,c,m,i) * tvvoo(a, d, m, j)
end do 

term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * 0.4999999999999998d+0 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * 0.4999999999999998d+0 
term(12) = term(12) * 0.4999999999999998d+0 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * 0.4999999999999998d+0 
term(15) = term(15) * (-0.49999999999999983d+0) 
term(16) = term(16) * 0.5000000000000001d+0 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * 0.5000000000000001d+0 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * 0.5000000000000001d+0 
term(22) = term(22) * (-0.5d+0) 
term(23) = term(23) * 0.5000000000000001d+0 


    eom_cc3_31_triplet_trans_aibjcidi = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjcidi = eom_cc3_31_triplet_trans_aibjcidi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjcidi
    function eom_cc3_31_triplet_trans_aibjcidj(t2, nocc, nactive, a, i, b, c, d) 
    double precision :: eom_cc3_31_triplet_trans_aibjcidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, d, b, e)
term(1) = term(1) + t2(b,e,i,i) * read_ftvvvv(c, d, a, e)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, e, b, d)
term(3) = term(3) + t2(c,e,i,i) * read_ftvvvv(b, d, a, e)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 

do m = 1, nocc 
term(4) = term(4) + t2(b,c,i,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,c,i,m) * tvoov(b, i, m, d)
term(6) = term(6) + t2(a,b,i,m) * tvvoo(c, d, m, i)
term(7) = term(7) + t2(a,b,m,i) * tvvoo(c, d, m, i)
term(8) = term(8) + t2(b,c,m,i) * tvoov(a, i, m, d)
term(9) = term(9) + t2(a,b,i,m) * tvoov(c, i, m, d)
term(10) = term(10) + t2(a,c,i,m) * tvvoo(b, d, m, i)
term(11) = term(11) + t2(a,c,m,i) * tvvoo(b, d, m, i)
end do 

term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * 0.49999999999999983d+0 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * 0.49999999999999983d+0 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aibjcidj = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjcidj = eom_cc3_31_triplet_trans_aibjcidj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjcidj
    function eom_cc3_31_triplet_trans_aibickdk(t2, nocc, nactive, a, i, b, c, d) 
    double precision :: eom_cc3_31_triplet_trans_aibickdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, b, d)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(c, d, b, e)
term(2) = term(2) + t2(c,e,i,i) * read_ftvvvv(b, d, a, e)
term(3) = term(3) + t2(b,e,i,i) * read_ftvvvv(c, d, a, e)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 

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

term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * 0.4999999999999998d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.4999999999999998d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aibickdk = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibickdk = eom_cc3_31_triplet_trans_aibickdk + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibickdk
    function eom_cc3_31_triplet_trans_aiajckai(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aiajckai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,j) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(a,e,j,k) * read_ftvvvv(c, e, a, a)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(a, e, a, a)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(a, e, a, a)
term(4) = term(4) + t2(c,e,j,k) * tvoov(a, i, i, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(c, j, i, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(c, e, i, k)
term(7) = term(7) + t2(c,e,j,i) * tvvoo(a, e, i, k)
term(8) = term(8) + t2(c,e,k,j) * tvoov(a, i, i, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(c, k, i, e)
term(10) = term(10) + t2(a,e,i,k) * tvvoo(c, e, i, j)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(a, e, i, j)
term(12) = term(12) + t2(a,e,j,i) * tvoov(c, k, i, e)
term(13) = term(13) + t2(c,e,j,i) * tvoov(a, k, i, e)
term(14) = term(14) + t2(c,e,k,i) * tvoov(a, j, i, e)
term(15) = term(15) + t2(a,e,k,i) * tvoov(c, j, i, e)
term(16) = term(16) + t2(a,e,k,j) * tvvoo(c, e, i, i)
term(17) = term(17) + t2(a,e,j,k) * tvvoo(c, e, i, i)
term(18) = term(18) + t2(c,e,j,k) * tvvoo(a, e, i, i)
term(19) = term(19) + t2(c,e,k,j) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5000000000000001d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5000000000000001d+0) 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 
term(12) = term(12) * 0.5000000000000001d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5000000000000001d+0 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * 0.5000000000000001d+0 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * 0.5000000000000001d+0 

do m = 1, nocc 
term(20) = term(20) + t2(a,a,j,m) * tvoov(c, k, m, a)
term(21) = term(21) + t2(a,c,m,j) * tvoov(a, k, m, a)
term(22) = term(22) + t2(a,c,m,k) * tvoov(a, j, m, a)
term(23) = term(23) + t2(a,a,k,m) * tvoov(c, j, m, a)
term(24) = term(24) + t2(a,c,k,m) * tvvoo(a, a, m, j)
term(25) = term(25) + t2(a,c,j,m) * tvvoo(a, a, m, k)
term(26) = term(26) + t2(a,c,m,j) * tvvoo(a, a, m, k)
term(27) = term(27) + t2(a,c,m,k) * tvvoo(a, a, m, j)
term(28) = term(28) + t2(a,c,i,m) * toooo(m, j, i, k)
term(29) = term(29) + t2(a,c,m,j) * toooo(m, i, i, k)
term(30) = term(30) + t2(a,c,i,m) * toooo(m, k, i, j)
term(31) = term(31) + t2(a,c,m,k) * toooo(m, i, i, j)
term(32) = term(32) + t2(a,c,k,m) * toooo(m, j, i, i)
term(33) = term(33) + t2(a,c,j,m) * toooo(m, k, i, i)
term(34) = term(34) + t2(a,c,m,j) * toooo(m, k, i, i)
term(35) = term(35) + t2(a,c,m,k) * toooo(m, j, i, i)
end do 

term(20) = term(20) * 0.5000000000000001d+0 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * 0.5000000000000001d+0 
term(23) = term(23) * (-0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * 0.5000000000000001d+0 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * 0.5000000000000001d+0 
term(28) = term(28) * (-0.4999999999999998d+0) 
term(29) = term(29) * (-0.4999999999999998d+0) 
term(30) = term(30) * 0.49999999999999983d+0 
term(31) = term(31) * 0.49999999999999983d+0 
term(32) = term(32) * 0.5d+0 
term(33) = term(33) * (-0.5000000000000001d+0) 
term(34) = term(34) * 0.5d+0 
term(35) = term(35) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aiajckai = 0.d+0
    do s = 0, 35
    eom_cc3_31_triplet_trans_aiajckai = eom_cc3_31_triplet_trans_aiajckai + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajckai
    function eom_cc3_31_triplet_trans_aiajckaj(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aiajckaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, a, a, e)
term(1) = term(1) + t2(a,e,k,i) * read_ftvvvv(c, a, a, e)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, a, a)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(a, e, a, a)
term(4) = term(4) + t2(c,e,j,k) * tvoov(a, i, j, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(c, j, j, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(c, e, j, k)
term(7) = term(7) + t2(c,e,j,i) * tvvoo(a, e, j, k)
term(8) = term(8) + t2(c,e,k,j) * tvoov(a, i, j, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(c, k, j, e)
term(10) = term(10) + t2(a,e,i,k) * tvvoo(c, e, j, j)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(a, e, j, j)
term(12) = term(12) + t2(a,e,j,i) * tvoov(c, k, j, e)
term(13) = term(13) + t2(c,e,j,i) * tvoov(a, k, j, e)
term(14) = term(14) + t2(c,e,k,i) * tvoov(a, j, j, e)
term(15) = term(15) + t2(a,e,k,i) * tvoov(c, j, j, e)
term(16) = term(16) + t2(a,e,k,j) * tvvoo(c, e, j, i)
term(17) = term(17) + t2(a,e,j,k) * tvvoo(c, e, j, i)
term(18) = term(18) + t2(c,e,j,k) * tvvoo(a, e, j, i)
term(19) = term(19) + t2(c,e,k,j) * tvvoo(a, e, j, i)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 
term(12) = term(12) * 0.5000000000000001d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5000000000000001d+0 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * 0.5000000000000001d+0 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * 0.5000000000000001d+0 

do m = 1, nocc 
term(20) = term(20) + t2(a,c,k,m) * tvoov(a, i, m, a)
term(21) = term(21) + t2(a,c,i,m) * tvoov(a, k, m, a)
term(22) = term(22) + t2(a,a,i,m) * tvvoo(c, a, m, k)
term(23) = term(23) + t2(a,a,k,m) * tvvoo(c, a, m, i)
term(24) = term(24) + t2(a,c,m,k) * tvoov(a, i, m, a)
term(25) = term(25) + t2(a,a,i,m) * tvoov(c, k, m, a)
term(26) = term(26) + t2(a,c,i,m) * tvvoo(a, a, m, k)
term(27) = term(27) + t2(a,c,m,k) * tvvoo(a, a, m, i)
term(28) = term(28) + t2(a,c,i,m) * toooo(m, j, j, k)
term(29) = term(29) + t2(a,c,m,j) * toooo(m, i, j, k)
term(30) = term(30) + t2(a,c,i,m) * toooo(m, k, j, j)
term(31) = term(31) + t2(a,c,m,k) * toooo(m, i, j, j)
term(32) = term(32) + t2(a,c,k,m) * toooo(m, j, j, i)
term(33) = term(33) + t2(a,c,j,m) * toooo(m, k, j, i)
term(34) = term(34) + t2(a,c,m,j) * toooo(m, k, j, i)
term(35) = term(35) + t2(a,c,m,k) * toooo(m, j, j, i)
end do 

term(20) = term(20) * 0.49999999999999983d+0 
term(21) = term(21) * 0.49999999999999983d+0 
term(22) = term(22) * 0.49999999999999983d+0 
term(23) = term(23) * 0.49999999999999983d+0 
term(24) = term(24) * (-0.49999999999999983d+0) 
term(25) = term(25) * (-0.49999999999999983d+0) 
term(26) = term(26) * (-0.49999999999999983d+0) 
term(27) = term(27) * (-0.49999999999999983d+0) 
term(28) = term(28) * (-0.4999999999999998d+0) 
term(29) = term(29) * (-0.4999999999999998d+0) 
term(30) = term(30) * 0.49999999999999983d+0 
term(31) = term(31) * 0.49999999999999983d+0 
term(32) = term(32) * 0.5d+0 
term(33) = term(33) * (-0.5000000000000001d+0) 
term(34) = term(34) * 0.5d+0 
term(35) = term(35) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aiajckaj = 0.d+0
    do s = 0, 35
    eom_cc3_31_triplet_trans_aiajckaj = eom_cc3_31_triplet_trans_aiajckaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajckaj
    function eom_cc3_31_triplet_trans_aiajckak(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aiajckak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, a, a, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(a, e, a, a)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(c, a, a, e)
term(4) = term(4) + t2(c,e,j,k) * tvoov(a, i, k, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(c, j, k, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(c, e, k, k)
term(7) = term(7) + t2(c,e,j,i) * tvvoo(a, e, k, k)
term(8) = term(8) + t2(c,e,k,j) * tvoov(a, i, k, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(c, k, k, e)
term(10) = term(10) + t2(a,e,i,k) * tvvoo(c, e, k, j)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(a, e, k, j)
term(12) = term(12) + t2(a,e,j,i) * tvoov(c, k, k, e)
term(13) = term(13) + t2(c,e,j,i) * tvoov(a, k, k, e)
term(14) = term(14) + t2(c,e,k,i) * tvoov(a, j, k, e)
term(15) = term(15) + t2(a,e,k,i) * tvoov(c, j, k, e)
term(16) = term(16) + t2(a,e,k,j) * tvvoo(c, e, k, i)
term(17) = term(17) + t2(a,e,j,k) * tvvoo(c, e, k, i)
term(18) = term(18) + t2(c,e,j,k) * tvvoo(a, e, k, i)
term(19) = term(19) + t2(c,e,k,j) * tvvoo(a, e, k, i)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 
term(12) = term(12) * 0.5000000000000001d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5000000000000001d+0 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * 0.5000000000000001d+0 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * 0.5000000000000001d+0 

do m = 1, nocc 
term(20) = term(20) + t2(a,c,j,m) * tvoov(a, i, m, a)
term(21) = term(21) + t2(a,c,m,j) * tvoov(a, i, m, a)
term(22) = term(22) + t2(a,c,i,m) * tvoov(a, j, m, a)
term(23) = term(23) + t2(a,a,i,m) * tvoov(c, j, m, a)
term(24) = term(24) + t2(a,c,i,m) * tvvoo(a, a, m, j)
term(25) = term(25) + t2(a,a,i,m) * tvvoo(c, a, m, j)
term(26) = term(26) + t2(a,c,m,j) * tvvoo(a, a, m, i)
term(27) = term(27) + t2(a,a,j,m) * tvvoo(c, a, m, i)
term(28) = term(28) + t2(a,c,i,m) * toooo(m, j, k, k)
term(29) = term(29) + t2(a,c,m,j) * toooo(m, i, k, k)
term(30) = term(30) + t2(a,c,i,m) * toooo(m, k, k, j)
term(31) = term(31) + t2(a,c,m,k) * toooo(m, i, k, j)
term(32) = term(32) + t2(a,c,k,m) * toooo(m, j, k, i)
term(33) = term(33) + t2(a,c,j,m) * toooo(m, k, k, i)
term(34) = term(34) + t2(a,c,m,j) * toooo(m, k, k, i)
term(35) = term(35) + t2(a,c,m,k) * toooo(m, j, k, i)
end do 

term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * 0.4999999999999998d+0 
term(22) = term(22) * (-0.49999999999999983d+0) 
term(23) = term(23) * 0.4999999999999998d+0 
term(24) = term(24) * 0.4999999999999998d+0 
term(25) = term(25) * (-0.49999999999999983d+0) 
term(26) = term(26) * 0.4999999999999998d+0 
term(27) = term(27) * (-0.49999999999999983d+0) 
term(28) = term(28) * (-0.4999999999999998d+0) 
term(29) = term(29) * (-0.4999999999999998d+0) 
term(30) = term(30) * 0.49999999999999983d+0 
term(31) = term(31) * 0.49999999999999983d+0 
term(32) = term(32) * 0.5d+0 
term(33) = term(33) * (-0.5000000000000001d+0) 
term(34) = term(34) * 0.5d+0 
term(35) = term(35) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aiajckak = 0.d+0
    do s = 0, 35
    eom_cc3_31_triplet_trans_aiajckak = eom_cc3_31_triplet_trans_aiajckak + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajckak
    function eom_cc3_31_triplet_trans_aiaickal(t2, nocc, nactive, a, i, c, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aiaickal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,k) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,k) * tvoov(c, i, l, e)
term(2) = term(2) + t2(a,e,i,i) * tvvoo(c, e, l, k)
term(3) = term(3) + t2(c,e,i,i) * tvvoo(a, e, l, k)
term(4) = term(4) + t2(c,e,i,i) * tvoov(a, k, l, e)
term(5) = term(5) + t2(a,e,k,i) * tvoov(c, i, l, e)
term(6) = term(6) + t2(a,e,k,i) * tvvoo(c, e, l, i)
term(7) = term(7) + t2(c,e,i,k) * tvvoo(a, e, l, i)
end do 

term(0) = term(0) * 0.4999999999999998d+0 
term(1) = term(1) * 0.4999999999999998d+0 
term(2) = term(2) * 0.4999999999999998d+0 
term(3) = term(3) * 0.4999999999999998d+0 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,c,i,m) * toooo(m, i, l, k)
term(9) = term(9) + t2(a,c,m,i) * toooo(m, i, l, k)
term(10) = term(10) + t2(a,c,k,m) * toooo(m, i, l, i)
term(11) = term(11) + t2(a,c,m,i) * toooo(m, k, l, i)
end do 

term(8) = term(8) * (-0.4999999999999998d+0) 
term(9) = term(9) * (-0.4999999999999998d+0) 
term(10) = term(10) * 0.5d+0 
term(11) = term(11) * 0.5d+0 


    eom_cc3_31_triplet_trans_aiaickal = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aiaickal = eom_cc3_31_triplet_trans_aiaickal + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiaickal
    function eom_cc3_31_triplet_trans_aiajcial(t2, nocc, nactive, a, i, j, c, l) 
    double precision :: eom_cc3_31_triplet_trans_aiajcial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,j) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,j) * tvoov(c, i, l, e)
term(2) = term(2) + t2(a,e,i,i) * tvvoo(c, e, l, j)
term(3) = term(3) + t2(c,e,i,i) * tvvoo(a, e, l, j)
term(4) = term(4) + t2(a,e,j,i) * tvoov(c, i, l, e)
term(5) = term(5) + t2(c,e,i,i) * tvoov(a, j, l, e)
term(6) = term(6) + t2(a,e,j,i) * tvvoo(c, e, l, i)
term(7) = term(7) + t2(c,e,i,j) * tvvoo(a, e, l, i)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * (-0.49999999999999983d+0) 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * 0.5000000000000001d+0 
term(5) = term(5) * 0.5000000000000001d+0 
term(6) = term(6) * 0.5000000000000001d+0 
term(7) = term(7) * 0.5000000000000001d+0 

do m = 1, nocc 
term(8) = term(8) + t2(a,c,i,m) * toooo(m, i, l, j)
term(9) = term(9) + t2(a,c,m,i) * toooo(m, i, l, j)
term(10) = term(10) + t2(a,c,j,m) * toooo(m, i, l, i)
term(11) = term(11) + t2(a,c,m,i) * toooo(m, j, l, i)
end do 

term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * 0.49999999999999983d+0 
term(10) = term(10) * (-0.5000000000000001d+0) 
term(11) = term(11) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aiajcial = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aiajcial = eom_cc3_31_triplet_trans_aiajcial + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajcial
    function eom_cc3_31_triplet_trans_aiajckci(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aiajckci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,j) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(a,e,j,k) * read_ftvvvv(c, e, a, c)
term(2) = term(2) + t2(c,e,j,k) * read_ftvvvv(a, e, a, c)
term(3) = term(3) + t2(c,e,k,j) * read_ftvvvv(a, e, a, c)
term(4) = term(4) + t2(a,e,k,j) * tvoov(a, i, i, e)
term(5) = term(5) + t2(a,e,j,k) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(a, k, i, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(a, j, i, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(a, e, i, j)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(a, e, i, k)
term(10) = term(10) + t2(a,e,k,i) * tvvoo(a, e, i, j)
term(11) = term(11) + t2(a,e,j,i) * tvvoo(a, e, i, k)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5000000000000001d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5000000000000001d+0) 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,a,j,m) * tvoov(c, k, m, c)
term(13) = term(13) + t2(a,c,m,j) * tvoov(a, k, m, c)
term(14) = term(14) + t2(a,c,m,k) * tvoov(a, j, m, c)
term(15) = term(15) + t2(a,a,k,m) * tvoov(c, j, m, c)
term(16) = term(16) + t2(a,c,k,m) * tvvoo(a, c, m, j)
term(17) = term(17) + t2(a,c,j,m) * tvvoo(a, c, m, k)
term(18) = term(18) + t2(a,c,m,j) * tvvoo(a, c, m, k)
term(19) = term(19) + t2(a,c,m,k) * tvvoo(a, c, m, j)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, k, i, j)
term(21) = term(21) + t2(a,a,i,m) * toooo(m, j, i, k)
term(22) = term(22) + t2(a,a,k,m) * toooo(m, i, i, j)
term(23) = term(23) + t2(a,a,j,m) * toooo(m, i, i, k)
end do 

term(12) = term(12) * 0.5000000000000001d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5000000000000001d+0 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * 0.5000000000000001d+0 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * 0.5000000000000001d+0 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * 0.49999999999999983d+0 
term(22) = term(22) * (-0.49999999999999983d+0) 
term(23) = term(23) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aiajckci = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aiajckci = eom_cc3_31_triplet_trans_aiajckci + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajckci
    function eom_cc3_31_triplet_trans_aiajckcj(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aiajckcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, c, a, e)
term(1) = term(1) + t2(a,e,k,i) * read_ftvvvv(c, c, a, e)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, a, c)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(a, e, a, c)
term(4) = term(4) + t2(a,e,k,j) * tvoov(a, i, j, e)
term(5) = term(5) + t2(a,e,j,k) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(a, k, j, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(a, j, j, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(a, e, j, j)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(a, e, j, k)
term(10) = term(10) + t2(a,e,k,i) * tvvoo(a, e, j, j)
term(11) = term(11) + t2(a,e,j,i) * tvvoo(a, e, j, k)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,k,m) * tvoov(a, i, m, c)
term(13) = term(13) + t2(a,c,i,m) * tvoov(a, k, m, c)
term(14) = term(14) + t2(a,a,i,m) * tvvoo(c, c, m, k)
term(15) = term(15) + t2(a,a,k,m) * tvvoo(c, c, m, i)
term(16) = term(16) + t2(a,c,m,k) * tvoov(a, i, m, c)
term(17) = term(17) + t2(a,a,i,m) * tvoov(c, k, m, c)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(a, c, m, k)
term(19) = term(19) + t2(a,c,m,k) * tvvoo(a, c, m, i)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, k, j, j)
term(21) = term(21) + t2(a,a,i,m) * toooo(m, j, j, k)
term(22) = term(22) + t2(a,a,k,m) * toooo(m, i, j, j)
term(23) = term(23) + t2(a,a,j,m) * toooo(m, i, j, k)
end do 

term(12) = term(12) * 0.49999999999999983d+0 
term(13) = term(13) * 0.49999999999999983d+0 
term(14) = term(14) * 0.49999999999999983d+0 
term(15) = term(15) * 0.49999999999999983d+0 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * 0.49999999999999983d+0 
term(22) = term(22) * (-0.49999999999999983d+0) 
term(23) = term(23) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aiajckcj = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aiajckcj = eom_cc3_31_triplet_trans_aiajckcj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajckcj
    function eom_cc3_31_triplet_trans_aiajckck(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aiajckck 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, c, a, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(a, e, a, c)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(c, c, a, e)
term(4) = term(4) + t2(a,e,k,j) * tvoov(a, i, k, e)
term(5) = term(5) + t2(a,e,j,k) * tvoov(a, i, k, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(a, k, k, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(a, j, k, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(a, e, k, j)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(a, e, k, k)
term(10) = term(10) + t2(a,e,k,i) * tvvoo(a, e, k, j)
term(11) = term(11) + t2(a,e,j,i) * tvvoo(a, e, k, k)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,j,m) * tvoov(a, i, m, c)
term(13) = term(13) + t2(a,c,m,j) * tvoov(a, i, m, c)
term(14) = term(14) + t2(a,c,i,m) * tvoov(a, j, m, c)
term(15) = term(15) + t2(a,a,i,m) * tvoov(c, j, m, c)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(a, c, m, j)
term(17) = term(17) + t2(a,a,i,m) * tvvoo(c, c, m, j)
term(18) = term(18) + t2(a,c,m,j) * tvvoo(a, c, m, i)
term(19) = term(19) + t2(a,a,j,m) * tvvoo(c, c, m, i)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, k, k, j)
term(21) = term(21) + t2(a,a,i,m) * toooo(m, j, k, k)
term(22) = term(22) + t2(a,a,k,m) * toooo(m, i, k, j)
term(23) = term(23) + t2(a,a,j,m) * toooo(m, i, k, k)
end do 

term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * 0.4999999999999998d+0 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * 0.4999999999999998d+0 
term(16) = term(16) * 0.4999999999999998d+0 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * 0.4999999999999998d+0 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * 0.49999999999999983d+0 
term(22) = term(22) * (-0.49999999999999983d+0) 
term(23) = term(23) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aiajckck = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aiajckck = eom_cc3_31_triplet_trans_aiajckck + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajckck
    function eom_cc3_31_triplet_trans_aiaickcl(t2, nocc, nactive, a, i, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aiaickcl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,k,i) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,k) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,i) * tvoov(a, k, l, e)
term(3) = term(3) + t2(a,e,i,k) * tvvoo(a, e, l, i)
term(4) = term(4) + t2(a,e,i,i) * tvvoo(a, e, l, k)
term(5) = term(5) + t2(a,e,k,i) * tvvoo(a, e, l, i)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = -term(1) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = -term(4) 
term(5) = term(5) * 0.49999999999999983d+0 

do m = 1, nocc 
term(6) = term(6) + t2(a,a,i,m) * toooo(m, k, l, i)
term(7) = term(7) + t2(a,a,i,m) * toooo(m, i, l, k)
term(8) = term(8) + t2(a,a,k,m) * toooo(m, i, l, i)
end do 

term(6) = term(6) * (-0.49999999999999983d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aiaickcl = 0.d+0
    do s = 0, 8
    eom_cc3_31_triplet_trans_aiaickcl = eom_cc3_31_triplet_trans_aiaickcl + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiaickcl
    function eom_cc3_31_triplet_trans_aiajcicl(t2, nocc, nactive, a, i, j, l) 
    double precision :: eom_cc3_31_triplet_trans_aiajcicl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,j,i) * tvoov(a, i, l, e)
term(2) = term(2) + t2(a,e,i,i) * tvoov(a, j, l, e)
term(3) = term(3) + t2(a,e,i,i) * tvvoo(a, e, l, j)
term(4) = term(4) + t2(a,e,i,j) * tvvoo(a, e, l, i)
term(5) = term(5) + t2(a,e,j,i) * tvvoo(a, e, l, i)
end do 

term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * (-0.49999999999999983d+0) 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,a,i,m) * toooo(m, i, l, j)
term(7) = term(7) + t2(a,a,i,m) * toooo(m, j, l, i)
term(8) = term(8) + t2(a,a,j,m) * toooo(m, i, l, i)
end do 

term(6) = -term(6) 
term(7) = term(7) * 0.49999999999999983d+0 
term(8) = term(8) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aiajcicl = 0.d+0
    do s = 0, 8
    eom_cc3_31_triplet_trans_aiajcicl = eom_cc3_31_triplet_trans_aiajcicl + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajcicl
    function eom_cc3_31_triplet_trans_aiaickdi(t2, nocc, nactive, a, i, c, k, d) 
    double precision :: eom_cc3_31_triplet_trans_aiaickdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, d, a, e)
term(1) = term(1) + t2(a,e,k,i) * read_ftvvvv(c, d, a, e)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(c, e, a, d)
term(3) = term(3) + t2(c,e,i,k) * read_ftvvvv(a, e, a, d)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * 0.5d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,k,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,c,i,m) * tvoov(a, k, m, d)
term(6) = term(6) + t2(a,a,i,m) * tvvoo(c, d, m, k)
term(7) = term(7) + t2(a,a,k,m) * tvvoo(c, d, m, i)
term(8) = term(8) + t2(a,c,m,i) * tvoov(a, k, m, d)
term(9) = term(9) + t2(a,a,k,m) * tvoov(c, i, m, d)
term(10) = term(10) + t2(a,c,k,m) * tvvoo(a, d, m, i)
term(11) = term(11) + t2(a,c,m,i) * tvvoo(a, d, m, k)
end do 

term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * 0.49999999999999983d+0 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * 0.49999999999999983d+0 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 


    eom_cc3_31_triplet_trans_aiaickdi = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aiaickdi = eom_cc3_31_triplet_trans_aiaickdi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiaickdi
    function eom_cc3_31_triplet_trans_aiajcidi(t2, nocc, nactive, a, i, j, c, d) 
    double precision :: eom_cc3_31_triplet_trans_aiajcidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, d, a, e)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, d, a, e)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(c, e, a, d)
term(3) = term(3) + t2(c,e,i,j) * read_ftvvvv(a, e, a, d)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.5000000000000001d+0) 
term(3) = term(3) * (-0.5000000000000001d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,c,j,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,c,i,m) * tvoov(a, j, m, d)
term(6) = term(6) + t2(a,a,i,m) * tvvoo(c, d, m, j)
term(7) = term(7) + t2(a,a,j,m) * tvvoo(c, d, m, i)
term(8) = term(8) + t2(a,a,j,m) * tvoov(c, i, m, d)
term(9) = term(9) + t2(a,c,m,i) * tvoov(a, j, m, d)
term(10) = term(10) + t2(a,c,j,m) * tvvoo(a, d, m, i)
term(11) = term(11) + t2(a,c,m,i) * tvvoo(a, d, m, j)
end do 

term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * 0.5000000000000001d+0 
term(9) = term(9) * 0.5000000000000001d+0 
term(10) = term(10) * 0.5000000000000001d+0 
term(11) = term(11) * 0.5000000000000001d+0 


    eom_cc3_31_triplet_trans_aiajcidi = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aiajcidi = eom_cc3_31_triplet_trans_aiajcidi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajcidi
    function eom_cc3_31_triplet_trans_aiajcidj(t2, nocc, nactive, a, i, c, d) 
    double precision :: eom_cc3_31_triplet_trans_aiajcidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, d, a, e)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(c, e, a, d)
term(2) = term(2) + t2(c,e,i,i) * read_ftvvvv(a, e, a, d)
end do 

term(0) = -term(0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * 0.49999999999999983d+0 

do m = 1, nocc 
term(3) = term(3) + t2(a,c,i,m) * tvoov(a, i, m, d)
term(4) = term(4) + t2(a,a,i,m) * tvvoo(c, d, m, i)
term(5) = term(5) + t2(a,c,m,i) * tvoov(a, i, m, d)
term(6) = term(6) + t2(a,a,i,m) * tvoov(c, i, m, d)
term(7) = term(7) + t2(a,c,i,m) * tvvoo(a, d, m, i)
term(8) = term(8) + t2(a,c,m,i) * tvvoo(a, d, m, i)
end do 

term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aiajcidj = 0.d+0
    do s = 0, 8
    eom_cc3_31_triplet_trans_aiajcidj = eom_cc3_31_triplet_trans_aiajcidj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajcidj
    function eom_cc3_31_triplet_trans_aiaickdk(t2, nocc, nactive, a, i, c, d) 
    double precision :: eom_cc3_31_triplet_trans_aiaickdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, d 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, a, d)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(c, d, a, e)
term(2) = term(2) + t2(c,e,i,i) * read_ftvvvv(a, e, a, d)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(2) = term(2) * (-0.4999999999999998d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,c,i,m) * tvoov(a, i, m, d)
term(4) = term(4) + t2(a,c,m,i) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,a,i,m) * tvoov(c, i, m, d)
term(6) = term(6) + t2(a,c,i,m) * tvvoo(a, d, m, i)
term(7) = term(7) + t2(a,a,i,m) * tvvoo(c, d, m, i)
term(8) = term(8) + t2(a,c,m,i) * tvvoo(a, d, m, i)
end do 

term(3) = -term(3) 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = -term(7) 
term(8) = term(8) * 0.4999999999999998d+0 


    eom_cc3_31_triplet_trans_aiaickdk = 0.d+0
    do s = 0, 8
    eom_cc3_31_triplet_trans_aiaickdk = eom_cc3_31_triplet_trans_aiaickdk + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiaickdk
    function eom_cc3_31_triplet_trans_aibjakai(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjakai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(a, e, a, a)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(a, e, a, a)
term(2) = term(2) + t2(a,e,j,k) * read_ftvvvv(b, e, a, a)
term(3) = term(3) + t2(a,e,k,j) * read_ftvvvv(b, e, a, a)
term(4) = term(4) + t2(b,e,k,j) * tvoov(a, i, i, e)
term(5) = term(5) + t2(b,e,j,k) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(b, k, i, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(b, j, i, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(b, e, i, j)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(b, e, i, k)
term(10) = term(10) + t2(b,e,k,i) * tvvoo(a, e, i, j)
term(11) = term(11) + t2(b,e,j,i) * tvvoo(a, e, i, k)
term(12) = term(12) + t2(b,e,j,i) * tvoov(a, k, i, e)
term(13) = term(13) + t2(a,e,j,i) * tvoov(b, k, i, e)
term(14) = term(14) + t2(a,e,k,i) * tvoov(b, j, i, e)
term(15) = term(15) + t2(b,e,k,i) * tvoov(a, j, i, e)
term(16) = term(16) + t2(b,e,k,j) * tvvoo(a, e, i, i)
term(17) = term(17) + t2(b,e,j,k) * tvvoo(a, e, i, i)
term(18) = term(18) + t2(a,e,j,k) * tvvoo(b, e, i, i)
term(19) = term(19) + t2(a,e,k,j) * tvvoo(b, e, i, i)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5000000000000001d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5000000000000001d+0) 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 
term(12) = term(12) * 0.5000000000000001d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5000000000000001d+0 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * 0.5000000000000001d+0 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * 0.5000000000000001d+0 

do m = 1, nocc 
term(20) = term(20) + t2(a,b,m,j) * tvoov(a, k, m, a)
term(21) = term(21) + t2(a,a,j,m) * tvoov(b, k, m, a)
term(22) = term(22) + t2(a,a,k,m) * tvoov(b, j, m, a)
term(23) = term(23) + t2(a,b,m,k) * tvoov(a, j, m, a)
term(24) = term(24) + t2(a,b,m,k) * tvvoo(a, a, m, j)
term(25) = term(25) + t2(a,b,m,j) * tvvoo(a, a, m, k)
term(26) = term(26) + t2(a,b,j,m) * tvvoo(a, a, m, k)
term(27) = term(27) + t2(a,b,k,m) * tvvoo(a, a, m, j)
term(28) = term(28) + t2(a,b,i,m) * toooo(m, k, i, j)
term(29) = term(29) + t2(a,b,i,m) * toooo(m, j, i, k)
term(30) = term(30) + t2(a,b,m,k) * toooo(m, i, i, j)
term(31) = term(31) + t2(a,b,m,j) * toooo(m, i, i, k)
term(32) = term(32) + t2(a,b,m,k) * toooo(m, j, i, i)
term(33) = term(33) + t2(a,b,m,j) * toooo(m, k, i, i)
term(34) = term(34) + t2(a,b,j,m) * toooo(m, k, i, i)
term(35) = term(35) + t2(a,b,k,m) * toooo(m, j, i, i)
end do 

term(20) = term(20) * 0.5000000000000001d+0 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * 0.5000000000000001d+0 
term(23) = term(23) * (-0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * 0.5000000000000001d+0 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * 0.5000000000000001d+0 
term(28) = term(28) * (-0.49999999999999983d+0) 
term(29) = term(29) * 0.49999999999999983d+0 
term(30) = term(30) * (-0.49999999999999983d+0) 
term(31) = term(31) * 0.49999999999999983d+0 
term(32) = term(32) * 0.5d+0 
term(33) = term(33) * (-0.5000000000000001d+0) 
term(34) = term(34) * 0.5d+0 
term(35) = term(35) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibjakai = 0.d+0
    do s = 0, 35
    eom_cc3_31_triplet_trans_aibjakai = eom_cc3_31_triplet_trans_aibjakai + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjakai
    function eom_cc3_31_triplet_trans_aibjakaj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjakaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(b, e, a, a)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(a, e, a, a)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(a,e,k,i) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(b,e,k,j) * tvoov(a, i, j, e)
term(5) = term(5) + t2(b,e,j,k) * tvoov(a, i, j, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(b, k, j, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(b, j, j, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(b, e, j, j)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(b, e, j, k)
term(10) = term(10) + t2(b,e,k,i) * tvvoo(a, e, j, j)
term(11) = term(11) + t2(b,e,j,i) * tvvoo(a, e, j, k)
term(12) = term(12) + t2(b,e,j,i) * tvoov(a, k, j, e)
term(13) = term(13) + t2(a,e,j,i) * tvoov(b, k, j, e)
term(14) = term(14) + t2(a,e,k,i) * tvoov(b, j, j, e)
term(15) = term(15) + t2(b,e,k,i) * tvoov(a, j, j, e)
term(16) = term(16) + t2(b,e,k,j) * tvvoo(a, e, j, i)
term(17) = term(17) + t2(b,e,j,k) * tvvoo(a, e, j, i)
term(18) = term(18) + t2(a,e,j,k) * tvvoo(b, e, j, i)
term(19) = term(19) + t2(a,e,k,j) * tvvoo(b, e, j, i)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 
term(12) = term(12) * 0.5000000000000001d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5000000000000001d+0 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * 0.5000000000000001d+0 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * 0.5000000000000001d+0 

do m = 1, nocc 
term(20) = term(20) + t2(a,b,m,k) * tvoov(a, i, m, a)
term(21) = term(21) + t2(a,a,i,m) * tvoov(b, k, m, a)
term(22) = term(22) + t2(a,b,i,m) * tvvoo(a, a, m, k)
term(23) = term(23) + t2(a,b,m,k) * tvvoo(a, a, m, i)
term(24) = term(24) + t2(a,b,k,m) * tvoov(a, i, m, a)
term(25) = term(25) + t2(a,b,i,m) * tvoov(a, k, m, a)
term(26) = term(26) + t2(a,a,i,m) * tvvoo(b, a, m, k)
term(27) = term(27) + t2(a,a,k,m) * tvvoo(b, a, m, i)
term(28) = term(28) + t2(a,b,i,m) * toooo(m, k, j, j)
term(29) = term(29) + t2(a,b,i,m) * toooo(m, j, j, k)
term(30) = term(30) + t2(a,b,m,k) * toooo(m, i, j, j)
term(31) = term(31) + t2(a,b,m,j) * toooo(m, i, j, k)
term(32) = term(32) + t2(a,b,m,k) * toooo(m, j, j, i)
term(33) = term(33) + t2(a,b,m,j) * toooo(m, k, j, i)
term(34) = term(34) + t2(a,b,j,m) * toooo(m, k, j, i)
term(35) = term(35) + t2(a,b,k,m) * toooo(m, j, j, i)
end do 

term(20) = term(20) * 0.49999999999999983d+0 
term(21) = term(21) * 0.49999999999999983d+0 
term(22) = term(22) * 0.49999999999999983d+0 
term(23) = term(23) * 0.49999999999999983d+0 
term(24) = term(24) * (-0.49999999999999983d+0) 
term(25) = term(25) * (-0.49999999999999983d+0) 
term(26) = term(26) * (-0.49999999999999983d+0) 
term(27) = term(27) * (-0.49999999999999983d+0) 
term(28) = term(28) * (-0.49999999999999983d+0) 
term(29) = term(29) * 0.49999999999999983d+0 
term(30) = term(30) * (-0.49999999999999983d+0) 
term(31) = term(31) * 0.49999999999999983d+0 
term(32) = term(32) * 0.5d+0 
term(33) = term(33) * (-0.5000000000000001d+0) 
term(34) = term(34) * 0.5d+0 
term(35) = term(35) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibjakaj = 0.d+0
    do s = 0, 35
    eom_cc3_31_triplet_trans_aibjakaj = eom_cc3_31_triplet_trans_aibjakaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjakaj
    function eom_cc3_31_triplet_trans_aibjakak(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjakak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(b, a, a, e)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(a, e, a, a)
term(4) = term(4) + t2(b,e,k,j) * tvoov(a, i, k, e)
term(5) = term(5) + t2(b,e,j,k) * tvoov(a, i, k, e)
term(6) = term(6) + t2(a,e,i,j) * tvoov(b, k, k, e)
term(7) = term(7) + t2(a,e,i,k) * tvoov(b, j, k, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(b, e, k, j)
term(9) = term(9) + t2(a,e,i,j) * tvvoo(b, e, k, k)
term(10) = term(10) + t2(b,e,k,i) * tvvoo(a, e, k, j)
term(11) = term(11) + t2(b,e,j,i) * tvvoo(a, e, k, k)
term(12) = term(12) + t2(b,e,j,i) * tvoov(a, k, k, e)
term(13) = term(13) + t2(a,e,j,i) * tvoov(b, k, k, e)
term(14) = term(14) + t2(a,e,k,i) * tvoov(b, j, k, e)
term(15) = term(15) + t2(b,e,k,i) * tvoov(a, j, k, e)
term(16) = term(16) + t2(b,e,k,j) * tvvoo(a, e, k, i)
term(17) = term(17) + t2(b,e,j,k) * tvvoo(a, e, k, i)
term(18) = term(18) + t2(a,e,j,k) * tvvoo(b, e, k, i)
term(19) = term(19) + t2(a,e,k,j) * tvvoo(b, e, k, i)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 
term(12) = term(12) * 0.5000000000000001d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5000000000000001d+0 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * 0.5000000000000001d+0 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * 0.5000000000000001d+0 

do m = 1, nocc 
term(20) = term(20) + t2(a,b,m,j) * tvoov(a, i, m, a)
term(21) = term(21) + t2(a,b,j,m) * tvoov(a, i, m, a)
term(22) = term(22) + t2(a,a,i,m) * tvoov(b, j, m, a)
term(23) = term(23) + t2(a,b,i,m) * tvoov(a, j, m, a)
term(24) = term(24) + t2(a,a,i,m) * tvvoo(b, a, m, j)
term(25) = term(25) + t2(a,b,i,m) * tvvoo(a, a, m, j)
term(26) = term(26) + t2(a,a,j,m) * tvvoo(b, a, m, i)
term(27) = term(27) + t2(a,b,m,j) * tvvoo(a, a, m, i)
term(28) = term(28) + t2(a,b,i,m) * toooo(m, k, k, j)
term(29) = term(29) + t2(a,b,i,m) * toooo(m, j, k, k)
term(30) = term(30) + t2(a,b,m,k) * toooo(m, i, k, j)
term(31) = term(31) + t2(a,b,m,j) * toooo(m, i, k, k)
term(32) = term(32) + t2(a,b,m,k) * toooo(m, j, k, i)
term(33) = term(33) + t2(a,b,m,j) * toooo(m, k, k, i)
term(34) = term(34) + t2(a,b,j,m) * toooo(m, k, k, i)
term(35) = term(35) + t2(a,b,k,m) * toooo(m, j, k, i)
end do 

term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * 0.4999999999999998d+0 
term(22) = term(22) * (-0.49999999999999983d+0) 
term(23) = term(23) * 0.4999999999999998d+0 
term(24) = term(24) * 0.4999999999999998d+0 
term(25) = term(25) * (-0.49999999999999983d+0) 
term(26) = term(26) * 0.4999999999999998d+0 
term(27) = term(27) * (-0.49999999999999983d+0) 
term(28) = term(28) * (-0.49999999999999983d+0) 
term(29) = term(29) * 0.49999999999999983d+0 
term(30) = term(30) * (-0.49999999999999983d+0) 
term(31) = term(31) * 0.49999999999999983d+0 
term(32) = term(32) * 0.5d+0 
term(33) = term(33) * (-0.5000000000000001d+0) 
term(34) = term(34) * 0.5d+0 
term(35) = term(35) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibjakak = 0.d+0
    do s = 0, 35
    eom_cc3_31_triplet_trans_aibjakak = eom_cc3_31_triplet_trans_aibjakak + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjakak
    function eom_cc3_31_triplet_trans_aibiakal(t2, nocc, nactive, a, i, b, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aibiakal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,k) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,k) * tvoov(b, i, l, e)
term(2) = term(2) + t2(a,e,i,i) * tvvoo(b, e, l, k)
term(3) = term(3) + t2(b,e,i,i) * tvvoo(a, e, l, k)
term(4) = term(4) + t2(b,e,i,i) * tvoov(a, k, l, e)
term(5) = term(5) + t2(a,e,k,i) * tvoov(b, i, l, e)
term(6) = term(6) + t2(b,e,i,k) * tvvoo(a, e, l, i)
term(7) = term(7) + t2(a,e,k,i) * tvvoo(b, e, l, i)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * (-0.49999999999999983d+0) 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * 0.5000000000000001d+0 
term(5) = term(5) * 0.5000000000000001d+0 
term(6) = term(6) * 0.5000000000000001d+0 
term(7) = term(7) * 0.5000000000000001d+0 

do m = 1, nocc 
term(8) = term(8) + t2(a,b,i,m) * toooo(m, i, l, k)
term(9) = term(9) + t2(a,b,m,i) * toooo(m, i, l, k)
term(10) = term(10) + t2(a,b,m,i) * toooo(m, k, l, i)
term(11) = term(11) + t2(a,b,k,m) * toooo(m, i, l, i)
end do 

term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * 0.49999999999999983d+0 
term(10) = term(10) * (-0.5000000000000001d+0) 
term(11) = term(11) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibiakal = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibiakal = eom_cc3_31_triplet_trans_aibiakal + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibiakal
    function eom_cc3_31_triplet_trans_aibjaial(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_31_triplet_trans_aibjaial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,j) * tvoov(b, i, l, e)
term(2) = term(2) + t2(a,e,i,i) * tvvoo(b, e, l, j)
term(3) = term(3) + t2(b,e,i,i) * tvvoo(a, e, l, j)
term(4) = term(4) + t2(a,e,j,i) * tvoov(b, i, l, e)
term(5) = term(5) + t2(b,e,i,i) * tvoov(a, j, l, e)
term(6) = term(6) + t2(b,e,i,j) * tvvoo(a, e, l, i)
term(7) = term(7) + t2(a,e,j,i) * tvvoo(b, e, l, i)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * (-0.5d+0) 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 

do m = 1, nocc 
term(8) = term(8) + t2(a,b,i,m) * toooo(m, i, l, j)
term(9) = term(9) + t2(a,b,m,i) * toooo(m, i, l, j)
term(10) = term(10) + t2(a,b,m,i) * toooo(m, j, l, i)
term(11) = term(11) + t2(a,b,j,m) * toooo(m, i, l, i)
end do 

term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.5d+0 
term(11) = term(11) * 0.5d+0 


    eom_cc3_31_triplet_trans_aibjaial = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjaial = eom_cc3_31_triplet_trans_aibjaial + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjaial
    function eom_cc3_31_triplet_trans_aibjakbi(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjakbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,j) * read_ftvvvv(a, e, a, b)
term(1) = term(1) + t2(b,e,j,k) * read_ftvvvv(a, e, a, b)
term(2) = term(2) + t2(a,e,j,k) * read_ftvvvv(b, e, a, b)
term(3) = term(3) + t2(a,e,k,j) * read_ftvvvv(b, e, a, b)
term(4) = term(4) + t2(a,e,j,k) * tvoov(a, i, i, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(a, j, i, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(a, e, i, k)
term(7) = term(7) + t2(a,e,j,i) * tvvoo(a, e, i, k)
term(8) = term(8) + t2(a,e,k,j) * tvoov(a, i, i, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(a, k, i, e)
term(10) = term(10) + t2(a,e,i,k) * tvvoo(a, e, i, j)
term(11) = term(11) + t2(a,e,k,i) * tvvoo(a, e, i, j)
end do 

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * (-0.5000000000000001d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * (-0.5000000000000001d+0) 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,m,j) * tvoov(a, k, m, b)
term(13) = term(13) + t2(a,a,j,m) * tvoov(b, k, m, b)
term(14) = term(14) + t2(a,a,k,m) * tvoov(b, j, m, b)
term(15) = term(15) + t2(a,b,m,k) * tvoov(a, j, m, b)
term(16) = term(16) + t2(a,b,m,k) * tvvoo(a, b, m, j)
term(17) = term(17) + t2(a,b,m,j) * tvvoo(a, b, m, k)
term(18) = term(18) + t2(a,b,j,m) * tvvoo(a, b, m, k)
term(19) = term(19) + t2(a,b,k,m) * tvvoo(a, b, m, j)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, j, i, k)
term(21) = term(21) + t2(a,a,j,m) * toooo(m, i, i, k)
term(22) = term(22) + t2(a,a,i,m) * toooo(m, k, i, j)
term(23) = term(23) + t2(a,a,k,m) * toooo(m, i, i, j)
end do 

term(12) = term(12) * 0.5000000000000001d+0 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * 0.5000000000000001d+0 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * 0.5000000000000001d+0 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * 0.5000000000000001d+0 
term(20) = term(20) * (-0.4999999999999998d+0) 
term(21) = term(21) * (-0.4999999999999998d+0) 
term(22) = term(22) * 0.49999999999999983d+0 
term(23) = term(23) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjakbi = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjakbi = eom_cc3_31_triplet_trans_aibjakbi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjakbi
    function eom_cc3_31_triplet_trans_aibjakbj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjakbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(b, e, a, b)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(a, e, a, b)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(a,e,k,i) * read_ftvvvv(b, b, a, e)
term(4) = term(4) + t2(a,e,j,k) * tvoov(a, i, j, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(a, j, j, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(a, e, j, k)
term(7) = term(7) + t2(a,e,j,i) * tvvoo(a, e, j, k)
term(8) = term(8) + t2(a,e,k,j) * tvoov(a, i, j, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(a, k, j, e)
term(10) = term(10) + t2(a,e,i,k) * tvvoo(a, e, j, j)
term(11) = term(11) + t2(a,e,k,i) * tvvoo(a, e, j, j)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,m,k) * tvoov(a, i, m, b)
term(13) = term(13) + t2(a,a,i,m) * tvoov(b, k, m, b)
term(14) = term(14) + t2(a,b,i,m) * tvvoo(a, b, m, k)
term(15) = term(15) + t2(a,b,m,k) * tvvoo(a, b, m, i)
term(16) = term(16) + t2(a,b,k,m) * tvoov(a, i, m, b)
term(17) = term(17) + t2(a,b,i,m) * tvoov(a, k, m, b)
term(18) = term(18) + t2(a,a,i,m) * tvvoo(b, b, m, k)
term(19) = term(19) + t2(a,a,k,m) * tvvoo(b, b, m, i)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, j, j, k)
term(21) = term(21) + t2(a,a,j,m) * toooo(m, i, j, k)
term(22) = term(22) + t2(a,a,i,m) * toooo(m, k, j, j)
term(23) = term(23) + t2(a,a,k,m) * toooo(m, i, j, j)
end do 

term(12) = term(12) * 0.49999999999999983d+0 
term(13) = term(13) * 0.49999999999999983d+0 
term(14) = term(14) * 0.49999999999999983d+0 
term(15) = term(15) * 0.49999999999999983d+0 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * (-0.4999999999999998d+0) 
term(21) = term(21) * (-0.4999999999999998d+0) 
term(22) = term(22) * 0.49999999999999983d+0 
term(23) = term(23) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjakbj = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjakbj = eom_cc3_31_triplet_trans_aibjakbj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjakbj
    function eom_cc3_31_triplet_trans_aibjakbk(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_31_triplet_trans_aibjakbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(b, b, a, e)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(a, e, a, b)
term(4) = term(4) + t2(a,e,j,k) * tvoov(a, i, k, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(a, j, k, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(a, e, k, k)
term(7) = term(7) + t2(a,e,j,i) * tvvoo(a, e, k, k)
term(8) = term(8) + t2(a,e,k,j) * tvoov(a, i, k, e)
term(9) = term(9) + t2(a,e,i,j) * tvoov(a, k, k, e)
term(10) = term(10) + t2(a,e,i,k) * tvvoo(a, e, k, j)
term(11) = term(11) + t2(a,e,k,i) * tvvoo(a, e, k, j)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,m,j) * tvoov(a, i, m, b)
term(13) = term(13) + t2(a,b,j,m) * tvoov(a, i, m, b)
term(14) = term(14) + t2(a,a,i,m) * tvoov(b, j, m, b)
term(15) = term(15) + t2(a,b,i,m) * tvoov(a, j, m, b)
term(16) = term(16) + t2(a,a,i,m) * tvvoo(b, b, m, j)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(a, b, m, j)
term(18) = term(18) + t2(a,a,j,m) * tvvoo(b, b, m, i)
term(19) = term(19) + t2(a,b,m,j) * tvvoo(a, b, m, i)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, j, k, k)
term(21) = term(21) + t2(a,a,j,m) * toooo(m, i, k, k)
term(22) = term(22) + t2(a,a,i,m) * toooo(m, k, k, j)
term(23) = term(23) + t2(a,a,k,m) * toooo(m, i, k, j)
end do 

term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * 0.4999999999999998d+0 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * 0.4999999999999998d+0 
term(16) = term(16) * 0.4999999999999998d+0 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * 0.4999999999999998d+0 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * (-0.4999999999999998d+0) 
term(21) = term(21) * (-0.4999999999999998d+0) 
term(22) = term(22) * 0.49999999999999983d+0 
term(23) = term(23) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjakbk = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjakbk = eom_cc3_31_triplet_trans_aibjakbk + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjakbk
    function eom_cc3_31_triplet_trans_aibiakbl(t2, nocc, nactive, a, i, k, l) 
    double precision :: eom_cc3_31_triplet_trans_aibiakbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, k, l 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,i) * tvvoo(a, e, l, k)
term(2) = term(2) + t2(a,e,k,i) * tvoov(a, i, l, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(a, k, l, e)
term(4) = term(4) + t2(a,e,i,k) * tvvoo(a, e, l, i)
term(5) = term(5) + t2(a,e,k,i) * tvvoo(a, e, l, i)
end do 

term(2) = term(2) * (-0.49999999999999983d+0) 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,a,i,m) * toooo(m, i, l, k)
term(7) = term(7) + t2(a,a,i,m) * toooo(m, k, l, i)
term(8) = term(8) + t2(a,a,k,m) * toooo(m, i, l, i)
end do 

term(6) = -term(6) 
term(7) = term(7) * 0.49999999999999983d+0 
term(8) = term(8) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibiakbl = 0.d+0
    do s = 0, 8
    eom_cc3_31_triplet_trans_aibiakbl = eom_cc3_31_triplet_trans_aibiakbl + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibiakbl
    function eom_cc3_31_triplet_trans_aibjaibl(t2, nocc, nactive, a, i, j, l) 
    double precision :: eom_cc3_31_triplet_trans_aibjaibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tvoov(a, i, l, e)
term(1) = term(1) + t2(a,e,i,i) * tvoov(a, j, l, e)
term(2) = term(2) + t2(a,e,i,j) * tvvoo(a, e, l, i)
term(3) = term(3) + t2(a,e,j,i) * tvvoo(a, e, l, i)
term(4) = term(4) + t2(a,e,i,j) * tvoov(a, i, l, e)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(a, e, l, j)
end do 

term(0) = term(0) * 0.4999999999999998d+0 
term(1) = term(1) * 0.4999999999999998d+0 
term(2) = term(2) * 0.4999999999999998d+0 
term(3) = term(3) * 0.4999999999999998d+0 
term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
term(6) = term(6) + t2(a,a,i,m) * toooo(m, j, l, i)
term(7) = term(7) + t2(a,a,j,m) * toooo(m, i, l, i)
term(8) = term(8) + t2(a,a,i,m) * toooo(m, i, l, j)
end do 

term(6) = term(6) * (-0.4999999999999998d+0) 
term(7) = term(7) * (-0.4999999999999998d+0) 


    eom_cc3_31_triplet_trans_aibjaibl = 0.d+0
    do s = 0, 8
    eom_cc3_31_triplet_trans_aibjaibl = eom_cc3_31_triplet_trans_aibjaibl + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjaibl
    function eom_cc3_31_triplet_trans_aibiakdi(t2, nocc, nactive, a, i, b, k, d) 
    double precision :: eom_cc3_31_triplet_trans_aibiakdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(b, d, a, e)
term(1) = term(1) + t2(a,e,k,i) * read_ftvvvv(b, d, a, e)
term(2) = term(2) + t2(b,e,i,k) * read_ftvvvv(a, e, a, d)
term(3) = term(3) + t2(a,e,k,i) * read_ftvvvv(b, e, a, d)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.5000000000000001d+0) 
term(3) = term(3) * (-0.5000000000000001d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,k,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,b,i,m) * tvoov(a, k, m, d)
term(6) = term(6) + t2(a,a,i,m) * tvvoo(b, d, m, k)
term(7) = term(7) + t2(a,a,k,m) * tvvoo(b, d, m, i)
term(8) = term(8) + t2(a,b,m,i) * tvoov(a, k, m, d)
term(9) = term(9) + t2(a,a,k,m) * tvoov(b, i, m, d)
term(10) = term(10) + t2(a,b,m,i) * tvvoo(a, d, m, k)
term(11) = term(11) + t2(a,b,k,m) * tvvoo(a, d, m, i)
end do 

term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * 0.5000000000000001d+0 
term(9) = term(9) * 0.5000000000000001d+0 
term(10) = term(10) * 0.5000000000000001d+0 
term(11) = term(11) * 0.5000000000000001d+0 


    eom_cc3_31_triplet_trans_aibiakdi = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibiakdi = eom_cc3_31_triplet_trans_aibiakdi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibiakdi
    function eom_cc3_31_triplet_trans_aibjaidi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_31_triplet_trans_aibjaidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(b, d, a, e)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(b, d, a, e)
term(2) = term(2) + t2(b,e,i,j) * read_ftvvvv(a, e, a, d)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(b, e, a, d)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * (-0.4999999999999998d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * 0.5d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,j,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,b,i,m) * tvoov(a, j, m, d)
term(6) = term(6) + t2(a,a,i,m) * tvvoo(b, d, m, j)
term(7) = term(7) + t2(a,a,j,m) * tvvoo(b, d, m, i)
term(8) = term(8) + t2(a,a,j,m) * tvoov(b, i, m, d)
term(9) = term(9) + t2(a,b,m,i) * tvoov(a, j, m, d)
term(10) = term(10) + t2(a,b,m,i) * tvvoo(a, d, m, j)
term(11) = term(11) + t2(a,b,j,m) * tvvoo(a, d, m, i)
end do 

term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 


    eom_cc3_31_triplet_trans_aibjaidi = 0.d+0
    do s = 0, 11
    eom_cc3_31_triplet_trans_aibjaidi = eom_cc3_31_triplet_trans_aibjaidi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjaidi
    function eom_cc3_31_triplet_trans_aibjaidj(t2, nocc, nactive, a, i, b, d) 
    double precision :: eom_cc3_31_triplet_trans_aibjaidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(b, e, a, d)
term(1) = term(1) + t2(b,e,i,i) * read_ftvvvv(a, e, a, d)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(b, d, a, e)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,i) * tvoov(a, i, m, d)
term(4) = term(4) + t2(a,a,i,m) * tvoov(b, i, m, d)
term(5) = term(5) + t2(a,b,i,m) * tvvoo(a, d, m, i)
term(6) = term(6) + t2(a,b,m,i) * tvvoo(a, d, m, i)
term(7) = term(7) + t2(a,b,i,m) * tvoov(a, i, m, d)
term(8) = term(8) + t2(a,a,i,m) * tvvoo(b, d, m, i)
end do 

term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * 0.49999999999999983d+0 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = -term(7) 
term(8) = -term(8) 


    eom_cc3_31_triplet_trans_aibjaidj = 0.d+0
    do s = 0, 8
    eom_cc3_31_triplet_trans_aibjaidj = eom_cc3_31_triplet_trans_aibjaidj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjaidj
    function eom_cc3_31_triplet_trans_aibiakdk(t2, nocc, nactive, a, i, b, d) 
    double precision :: eom_cc3_31_triplet_trans_aibiakdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, d 
    integer :: s ,e,m 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(b, d, a, e)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(b, e, a, d)
term(2) = term(2) + t2(b,e,i,i) * read_ftvvvv(a, e, a, d)
end do 

term(0) = -term(0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * 0.49999999999999983d+0 

do m = 1, nocc 
term(3) = term(3) + t2(a,b,m,i) * tvoov(a, i, m, d)
term(4) = term(4) + t2(a,b,i,m) * tvoov(a, i, m, d)
term(5) = term(5) + t2(a,a,i,m) * tvoov(b, i, m, d)
term(6) = term(6) + t2(a,a,i,m) * tvvoo(b, d, m, i)
term(7) = term(7) + t2(a,b,i,m) * tvvoo(a, d, m, i)
term(8) = term(8) + t2(a,b,m,i) * tvvoo(a, d, m, i)
end do 

term(3) = term(3) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aibiakdk = 0.d+0
    do s = 0, 8
    eom_cc3_31_triplet_trans_aibiakdk = eom_cc3_31_triplet_trans_aibiakdk + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibiakdk
    function eom_cc3_31_triplet_trans_aibickai(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibickai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, a, b, e)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(c, a, a, e)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, b, a)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(b,e,k,i) * read_ftvvvv(c, e, a, a)
term(5) = term(5) + t2(b,e,i,k) * read_ftvvvv(c, e, a, a)
term(6) = term(6) + t2(c,e,i,k) * read_ftvvvv(b, e, a, a)
term(7) = term(7) + t2(c,e,k,i) * read_ftvvvv(b, e, a, a)
term(8) = term(8) + t2(b,e,i,i) * tvoov(c, k, i, e)
term(9) = term(9) + t2(c,e,i,i) * tvoov(b, k, i, e)
term(10) = term(10) + t2(c,e,k,i) * tvoov(b, i, i, e)
term(11) = term(11) + t2(b,e,k,i) * tvoov(c, i, i, e)
term(12) = term(12) + t2(b,e,k,i) * tvvoo(c, e, i, i)
term(13) = term(13) + t2(b,e,i,k) * tvvoo(c, e, i, i)
term(14) = term(14) + t2(c,e,i,k) * tvvoo(b, e, i, i)
term(15) = term(15) + t2(c,e,k,i) * tvvoo(b, e, i, i)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5000000000000001d+0) 
term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5000000000000001d+0) 
term(8) = term(8) * 0.5000000000000001d+0 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * 0.5000000000000001d+0 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * 0.5000000000000001d+0 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * 0.5000000000000001d+0 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,k,m) * tvoov(a, i, m, a)
term(17) = term(17) + t2(a,c,i,m) * tvoov(b, k, m, a)
term(18) = term(18) + t2(a,b,i,m) * tvvoo(c, a, m, k)
term(19) = term(19) + t2(a,b,m,k) * tvvoo(c, a, m, i)
term(20) = term(20) + t2(b,c,m,k) * tvoov(a, i, m, a)
term(21) = term(21) + t2(a,b,i,m) * tvoov(c, k, m, a)
term(22) = term(22) + t2(a,c,i,m) * tvvoo(b, a, m, k)
term(23) = term(23) + t2(a,c,m,k) * tvvoo(b, a, m, i)
term(24) = term(24) + t2(a,b,m,i) * tvoov(c, k, m, a)
term(25) = term(25) + t2(a,c,m,i) * tvoov(b, k, m, a)
term(26) = term(26) + t2(a,c,m,k) * tvoov(b, i, m, a)
term(27) = term(27) + t2(a,b,m,k) * tvoov(c, i, m, a)
term(28) = term(28) + t2(b,c,k,m) * tvvoo(a, a, m, i)
term(29) = term(29) + t2(b,c,i,m) * tvvoo(a, a, m, k)
term(30) = term(30) + t2(b,c,m,i) * tvvoo(a, a, m, k)
term(31) = term(31) + t2(b,c,m,k) * tvvoo(a, a, m, i)
term(32) = term(32) + t2(b,c,k,m) * toooo(m, i, i, i)
term(33) = term(33) + t2(b,c,i,m) * toooo(m, k, i, i)
term(34) = term(34) + t2(b,c,m,i) * toooo(m, k, i, i)
term(35) = term(35) + t2(b,c,m,k) * toooo(m, i, i, i)
end do 

term(16) = term(16) * 0.49999999999999983d+0 
term(17) = term(17) * 0.49999999999999983d+0 
term(18) = term(18) * 0.49999999999999983d+0 
term(19) = term(19) * 0.49999999999999983d+0 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * (-0.49999999999999983d+0) 
term(22) = term(22) * (-0.49999999999999983d+0) 
term(23) = term(23) * (-0.49999999999999983d+0) 
term(24) = term(24) * 0.5000000000000001d+0 
term(25) = term(25) * (-0.5d+0) 
term(26) = term(26) * 0.5000000000000001d+0 
term(27) = term(27) * (-0.5d+0) 
term(28) = term(28) * (-0.5d+0) 
term(29) = term(29) * 0.5000000000000001d+0 
term(30) = term(30) * (-0.5d+0) 
term(31) = term(31) * 0.5000000000000001d+0 
term(32) = term(32) * 0.5d+0 
term(33) = term(33) * (-0.5000000000000001d+0) 
term(34) = term(34) * 0.5d+0 
term(35) = term(35) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibickai = 0.d+0
    do s = 0, 35
    eom_cc3_31_triplet_trans_aibickai = eom_cc3_31_triplet_trans_aibickai + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibickai
    function eom_cc3_31_triplet_trans_aibjciai(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_31_triplet_trans_aibjciai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, a)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, a, b, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, a, a, e)
term(4) = term(4) + t2(b,e,i,j) * read_ftvvvv(c, e, a, a)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(c, e, a, a)
term(6) = term(6) + t2(c,e,j,i) * read_ftvvvv(b, e, a, a)
term(7) = term(7) + t2(c,e,i,j) * read_ftvvvv(b, e, a, a)
term(8) = term(8) + t2(b,e,j,i) * tvoov(c, i, i, e)
term(9) = term(9) + t2(c,e,j,i) * tvoov(b, i, i, e)
term(10) = term(10) + t2(c,e,i,i) * tvoov(b, j, i, e)
term(11) = term(11) + t2(b,e,i,i) * tvoov(c, j, i, e)
term(12) = term(12) + t2(b,e,i,j) * tvvoo(c, e, i, i)
term(13) = term(13) + t2(b,e,j,i) * tvvoo(c, e, i, i)
term(14) = term(14) + t2(c,e,j,i) * tvvoo(b, e, i, i)
term(15) = term(15) + t2(c,e,i,j) * tvvoo(b, e, i, i)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5000000000000001d+0) 
term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5000000000000001d+0) 
term(8) = term(8) * 0.5000000000000001d+0 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * 0.5000000000000001d+0 
term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * 0.5000000000000001d+0 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * 0.5000000000000001d+0 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, i, m, a)
term(17) = term(17) + t2(b,c,m,j) * tvoov(a, i, m, a)
term(18) = term(18) + t2(a,c,i,m) * tvoov(b, j, m, a)
term(19) = term(19) + t2(a,b,i,m) * tvoov(c, j, m, a)
term(20) = term(20) + t2(a,c,i,m) * tvvoo(b, a, m, j)
term(21) = term(21) + t2(a,b,i,m) * tvvoo(c, a, m, j)
term(22) = term(22) + t2(a,c,m,j) * tvvoo(b, a, m, i)
term(23) = term(23) + t2(a,b,m,j) * tvvoo(c, a, m, i)
term(24) = term(24) + t2(a,b,m,j) * tvoov(c, i, m, a)
term(25) = term(25) + t2(a,c,m,j) * tvoov(b, i, m, a)
term(26) = term(26) + t2(a,c,m,i) * tvoov(b, j, m, a)
term(27) = term(27) + t2(a,b,m,i) * tvoov(c, j, m, a)
term(28) = term(28) + t2(b,c,i,m) * tvvoo(a, a, m, j)
term(29) = term(29) + t2(b,c,j,m) * tvvoo(a, a, m, i)
term(30) = term(30) + t2(b,c,m,j) * tvvoo(a, a, m, i)
term(31) = term(31) + t2(b,c,m,i) * tvvoo(a, a, m, j)
term(32) = term(32) + t2(b,c,i,m) * toooo(m, j, i, i)
term(33) = term(33) + t2(b,c,j,m) * toooo(m, i, i, i)
term(34) = term(34) + t2(b,c,m,j) * toooo(m, i, i, i)
term(35) = term(35) + t2(b,c,m,i) * toooo(m, j, i, i)
end do 

term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * 0.4999999999999998d+0 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * 0.4999999999999998d+0 
term(20) = term(20) * 0.4999999999999998d+0 
term(21) = term(21) * (-0.49999999999999983d+0) 
term(22) = term(22) * 0.4999999999999998d+0 
term(23) = term(23) * (-0.49999999999999983d+0) 
term(24) = term(24) * 0.5000000000000001d+0 
term(25) = term(25) * (-0.5d+0) 
term(26) = term(26) * 0.5000000000000001d+0 
term(27) = term(27) * (-0.5d+0) 
term(28) = term(28) * (-0.5d+0) 
term(29) = term(29) * 0.5000000000000001d+0 
term(30) = term(30) * (-0.5d+0) 
term(31) = term(31) * 0.5000000000000001d+0 
term(32) = term(32) * 0.5d+0 
term(33) = term(33) * (-0.5000000000000001d+0) 
term(34) = term(34) * 0.5d+0 
term(35) = term(35) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibjciai = 0.d+0
    do s = 0, 35
    eom_cc3_31_triplet_trans_aibjciai = eom_cc3_31_triplet_trans_aibjciai + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjciai
    function eom_cc3_31_triplet_trans_aibjciaj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_31_triplet_trans_aibjciaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, a, b, e)
term(1) = term(1) + t2(b,e,i,i) * read_ftvvvv(c, a, a, e)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(c, e, b, a)
term(3) = term(3) + t2(c,e,i,i) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(b,e,j,i) * tvoov(c, i, j, e)
term(5) = term(5) + t2(c,e,j,i) * tvoov(b, i, j, e)
term(6) = term(6) + t2(c,e,i,i) * tvoov(b, j, j, e)
term(7) = term(7) + t2(b,e,i,i) * tvoov(c, j, j, e)
term(8) = term(8) + t2(b,e,i,j) * tvvoo(c, e, j, i)
term(9) = term(9) + t2(b,e,j,i) * tvvoo(c, e, j, i)
term(10) = term(10) + t2(c,e,j,i) * tvvoo(b, e, j, i)
term(11) = term(11) + t2(c,e,i,j) * tvvoo(b, e, j, i)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.5000000000000001d+0 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * 0.5000000000000001d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * 0.5000000000000001d+0 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * 0.5000000000000001d+0 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,i,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,c,i,m) * tvoov(b, i, m, a)
term(14) = term(14) + t2(a,b,i,m) * tvvoo(c, a, m, i)
term(15) = term(15) + t2(a,b,m,i) * tvvoo(c, a, m, i)
term(16) = term(16) + t2(b,c,m,i) * tvoov(a, i, m, a)
term(17) = term(17) + t2(a,b,i,m) * tvoov(c, i, m, a)
term(18) = term(18) + t2(a,c,i,m) * tvvoo(b, a, m, i)
term(19) = term(19) + t2(a,c,m,i) * tvvoo(b, a, m, i)
term(20) = term(20) + t2(b,c,i,m) * toooo(m, j, j, i)
term(21) = term(21) + t2(b,c,j,m) * toooo(m, i, j, i)
term(22) = term(22) + t2(b,c,m,j) * toooo(m, i, j, i)
term(23) = term(23) + t2(b,c,m,i) * toooo(m, j, j, i)
end do 

term(12) = term(12) * 0.49999999999999983d+0 
term(13) = term(13) * 0.49999999999999983d+0 
term(14) = term(14) * 0.49999999999999983d+0 
term(15) = term(15) * 0.49999999999999983d+0 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * 0.5d+0 
term(21) = term(21) * (-0.5000000000000001d+0) 
term(22) = term(22) * 0.5d+0 
term(23) = term(23) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibjciaj = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjciaj = eom_cc3_31_triplet_trans_aibjciaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjciaj
    function eom_cc3_31_triplet_trans_aibickak(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibickak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, b, a)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(c, a, b, e)
term(2) = term(2) + t2(c,e,i,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(b,e,i,i) * read_ftvvvv(c, a, a, e)
term(4) = term(4) + t2(b,e,i,i) * tvoov(c, k, k, e)
term(5) = term(5) + t2(c,e,i,i) * tvoov(b, k, k, e)
term(6) = term(6) + t2(c,e,k,i) * tvoov(b, i, k, e)
term(7) = term(7) + t2(b,e,k,i) * tvoov(c, i, k, e)
term(8) = term(8) + t2(b,e,k,i) * tvvoo(c, e, k, i)
term(9) = term(9) + t2(b,e,i,k) * tvvoo(c, e, k, i)
term(10) = term(10) + t2(c,e,i,k) * tvvoo(b, e, k, i)
term(11) = term(11) + t2(c,e,k,i) * tvvoo(b, e, k, i)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.5000000000000001d+0 
term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * 0.5000000000000001d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * 0.5000000000000001d+0 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * 0.5000000000000001d+0 

do m = 1, nocc 
term(12) = term(12) + t2(b,c,i,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(b,c,m,i) * tvoov(a, i, m, a)
term(14) = term(14) + t2(a,c,i,m) * tvoov(b, i, m, a)
term(15) = term(15) + t2(a,b,i,m) * tvoov(c, i, m, a)
term(16) = term(16) + t2(a,c,i,m) * tvvoo(b, a, m, i)
term(17) = term(17) + t2(a,b,i,m) * tvvoo(c, a, m, i)
term(18) = term(18) + t2(a,c,m,i) * tvvoo(b, a, m, i)
term(19) = term(19) + t2(a,b,m,i) * tvvoo(c, a, m, i)
term(20) = term(20) + t2(b,c,k,m) * toooo(m, i, k, i)
term(21) = term(21) + t2(b,c,i,m) * toooo(m, k, k, i)
term(22) = term(22) + t2(b,c,m,i) * toooo(m, k, k, i)
term(23) = term(23) + t2(b,c,m,k) * toooo(m, i, k, i)
end do 

term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * 0.4999999999999998d+0 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * 0.4999999999999998d+0 
term(16) = term(16) * 0.4999999999999998d+0 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * 0.4999999999999998d+0 
term(19) = term(19) * (-0.49999999999999983d+0) 
term(20) = term(20) * 0.5d+0 
term(21) = term(21) * (-0.5000000000000001d+0) 
term(22) = term(22) * 0.5d+0 
term(23) = term(23) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibickak = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibickak = eom_cc3_31_triplet_trans_aibickak + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibickak
    function eom_cc3_31_triplet_trans_aibickbi(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibickbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, b, b, e)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(c, b, a, e)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, b, b)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, b, a, e)
term(4) = term(4) + t2(b,e,k,i) * read_ftvvvv(c, e, a, b)
term(5) = term(5) + t2(b,e,i,k) * read_ftvvvv(c, e, a, b)
term(6) = term(6) + t2(c,e,i,k) * read_ftvvvv(b, e, a, b)
term(7) = term(7) + t2(c,e,k,i) * read_ftvvvv(b, e, a, b)
term(8) = term(8) + t2(c,e,i,k) * tvoov(a, i, i, e)
term(9) = term(9) + t2(a,e,i,k) * tvoov(c, i, i, e)
term(10) = term(10) + t2(a,e,i,i) * tvvoo(c, e, i, k)
term(11) = term(11) + t2(c,e,i,i) * tvvoo(a, e, i, k)
term(12) = term(12) + t2(c,e,k,i) * tvoov(a, i, i, e)
term(13) = term(13) + t2(a,e,i,i) * tvoov(c, k, i, e)
term(14) = term(14) + t2(a,e,i,k) * tvvoo(c, e, i, i)
term(15) = term(15) + t2(c,e,k,i) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5000000000000001d+0) 
term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5000000000000001d+0) 
term(8) = term(8) * 0.4999999999999998d+0 
term(9) = term(9) * 0.4999999999999998d+0 
term(10) = term(10) * 0.4999999999999998d+0 
term(11) = term(11) * 0.4999999999999998d+0 
term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,k,m) * tvoov(a, i, m, b)
term(17) = term(17) + t2(a,c,i,m) * tvoov(b, k, m, b)
term(18) = term(18) + t2(a,b,i,m) * tvvoo(c, b, m, k)
term(19) = term(19) + t2(a,b,m,k) * tvvoo(c, b, m, i)
term(20) = term(20) + t2(b,c,m,k) * tvoov(a, i, m, b)
term(21) = term(21) + t2(a,b,i,m) * tvoov(c, k, m, b)
term(22) = term(22) + t2(a,c,i,m) * tvvoo(b, b, m, k)
term(23) = term(23) + t2(a,c,m,k) * tvvoo(b, b, m, i)
term(24) = term(24) + t2(a,b,m,i) * tvoov(c, k, m, b)
term(25) = term(25) + t2(a,c,m,i) * tvoov(b, k, m, b)
term(26) = term(26) + t2(a,c,m,k) * tvoov(b, i, m, b)
term(27) = term(27) + t2(a,b,m,k) * tvoov(c, i, m, b)
term(28) = term(28) + t2(b,c,k,m) * tvvoo(a, b, m, i)
term(29) = term(29) + t2(b,c,i,m) * tvvoo(a, b, m, k)
term(30) = term(30) + t2(b,c,m,i) * tvvoo(a, b, m, k)
term(31) = term(31) + t2(b,c,m,k) * tvvoo(a, b, m, i)
term(32) = term(32) + t2(a,c,i,m) * toooo(m, i, i, k)
term(33) = term(33) + t2(a,c,m,i) * toooo(m, i, i, k)
term(34) = term(34) + t2(a,c,i,m) * toooo(m, k, i, i)
term(35) = term(35) + t2(a,c,m,k) * toooo(m, i, i, i)
end do 

term(16) = term(16) * 0.49999999999999983d+0 
term(17) = term(17) * 0.49999999999999983d+0 
term(18) = term(18) * 0.49999999999999983d+0 
term(19) = term(19) * 0.49999999999999983d+0 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * (-0.49999999999999983d+0) 
term(22) = term(22) * (-0.49999999999999983d+0) 
term(23) = term(23) * (-0.49999999999999983d+0) 
term(24) = term(24) * 0.5000000000000001d+0 
term(25) = term(25) * (-0.5d+0) 
term(26) = term(26) * 0.5000000000000001d+0 
term(27) = term(27) * (-0.5d+0) 
term(28) = term(28) * (-0.5d+0) 
term(29) = term(29) * 0.5000000000000001d+0 
term(30) = term(30) * (-0.5d+0) 
term(31) = term(31) * 0.5000000000000001d+0 
term(32) = term(32) * (-0.4999999999999998d+0) 
term(33) = term(33) * (-0.4999999999999998d+0) 
term(34) = term(34) * 0.49999999999999983d+0 
term(35) = term(35) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibickbi = 0.d+0
    do s = 0, 35
    eom_cc3_31_triplet_trans_aibickbi = eom_cc3_31_triplet_trans_aibickbi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibickbi
    function eom_cc3_31_triplet_trans_aibjcibi(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_31_triplet_trans_aibjcibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, b)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, b, b, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, b, a, e)
term(4) = term(4) + t2(b,e,i,j) * read_ftvvvv(c, e, a, b)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(c, e, a, b)
term(6) = term(6) + t2(c,e,j,i) * read_ftvvvv(b, e, a, b)
term(7) = term(7) + t2(c,e,i,j) * read_ftvvvv(b, e, a, b)
term(8) = term(8) + t2(c,e,j,i) * tvoov(a, i, i, e)
term(9) = term(9) + t2(a,e,i,i) * tvoov(c, j, i, e)
term(10) = term(10) + t2(a,e,i,j) * tvvoo(c, e, i, i)
term(11) = term(11) + t2(c,e,j,i) * tvvoo(a, e, i, i)
term(12) = term(12) + t2(c,e,i,j) * tvoov(a, i, i, e)
term(13) = term(13) + t2(a,e,i,j) * tvoov(c, i, i, e)
term(14) = term(14) + t2(a,e,i,i) * tvvoo(c, e, i, j)
term(15) = term(15) + t2(c,e,i,i) * tvvoo(a, e, i, j)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5000000000000001d+0) 
term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5000000000000001d+0) 
term(8) = term(8) * 0.4999999999999998d+0 
term(9) = term(9) * 0.4999999999999998d+0 
term(10) = term(10) * 0.4999999999999998d+0 
term(11) = term(11) * 0.4999999999999998d+0 
term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, i, m, b)
term(17) = term(17) + t2(b,c,m,j) * tvoov(a, i, m, b)
term(18) = term(18) + t2(a,c,i,m) * tvoov(b, j, m, b)
term(19) = term(19) + t2(a,b,i,m) * tvoov(c, j, m, b)
term(20) = term(20) + t2(a,c,i,m) * tvvoo(b, b, m, j)
term(21) = term(21) + t2(a,b,i,m) * tvvoo(c, b, m, j)
term(22) = term(22) + t2(a,c,m,j) * tvvoo(b, b, m, i)
term(23) = term(23) + t2(a,b,m,j) * tvvoo(c, b, m, i)
term(24) = term(24) + t2(a,b,m,j) * tvoov(c, i, m, b)
term(25) = term(25) + t2(a,c,m,j) * tvoov(b, i, m, b)
term(26) = term(26) + t2(a,c,m,i) * tvoov(b, j, m, b)
term(27) = term(27) + t2(a,b,m,i) * tvoov(c, j, m, b)
term(28) = term(28) + t2(b,c,i,m) * tvvoo(a, b, m, j)
term(29) = term(29) + t2(b,c,j,m) * tvvoo(a, b, m, i)
term(30) = term(30) + t2(b,c,m,j) * tvvoo(a, b, m, i)
term(31) = term(31) + t2(b,c,m,i) * tvvoo(a, b, m, j)
term(32) = term(32) + t2(a,c,i,m) * toooo(m, j, i, i)
term(33) = term(33) + t2(a,c,m,j) * toooo(m, i, i, i)
term(34) = term(34) + t2(a,c,i,m) * toooo(m, i, i, j)
term(35) = term(35) + t2(a,c,m,i) * toooo(m, i, i, j)
end do 

term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * 0.4999999999999998d+0 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * 0.4999999999999998d+0 
term(20) = term(20) * 0.4999999999999998d+0 
term(21) = term(21) * (-0.49999999999999983d+0) 
term(22) = term(22) * 0.4999999999999998d+0 
term(23) = term(23) * (-0.49999999999999983d+0) 
term(24) = term(24) * 0.5000000000000001d+0 
term(25) = term(25) * (-0.5d+0) 
term(26) = term(26) * 0.5000000000000001d+0 
term(27) = term(27) * (-0.5d+0) 
term(28) = term(28) * (-0.5d+0) 
term(29) = term(29) * 0.5000000000000001d+0 
term(30) = term(30) * (-0.5d+0) 
term(31) = term(31) * 0.5000000000000001d+0 
term(32) = term(32) * (-0.4999999999999998d+0) 
term(33) = term(33) * (-0.4999999999999998d+0) 
term(34) = term(34) * 0.49999999999999983d+0 
term(35) = term(35) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjcibi = 0.d+0
    do s = 0, 35
    eom_cc3_31_triplet_trans_aibjcibi = eom_cc3_31_triplet_trans_aibjcibi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjcibi
    function eom_cc3_31_triplet_trans_aibjcibj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_31_triplet_trans_aibjcibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,j,i) * tvoov(a, i, j, e)
term(1) = term(1) + t2(a,e,i,i) * tvoov(c, j, j, e)
term(2) = term(2) + t2(a,e,i,j) * tvvoo(c, e, j, i)
term(3) = term(3) + t2(c,e,j,i) * tvvoo(a, e, j, i)
term(4) = term(4) + t2(c,e,i,j) * tvoov(a, i, j, e)
term(5) = term(5) + t2(a,e,i,j) * tvoov(c, i, j, e)
term(6) = term(6) + t2(a,e,i,i) * tvvoo(c, e, j, j)
term(7) = term(7) + t2(c,e,i,i) * tvvoo(a, e, j, j)
term(8) = term(8) + t2(a,e,i,i) * read_ftvvvv(c, b, b, e)
term(9) = term(9) + t2(b,e,i,i) * read_ftvvvv(c, b, a, e)
term(10) = term(10) + t2(a,e,i,i) * read_ftvvvv(c, e, b, b)
term(11) = term(11) + t2(c,e,i,i) * read_ftvvvv(b, b, a, e)
end do 

term(0) = term(0) * 0.4999999999999998d+0 
term(1) = term(1) * 0.4999999999999998d+0 
term(2) = term(2) * 0.4999999999999998d+0 
term(3) = term(3) * 0.4999999999999998d+0 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * 0.49999999999999983d+0 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,i,m) * toooo(m, j, j, i)
term(13) = term(13) + t2(a,c,m,j) * toooo(m, i, j, i)
term(14) = term(14) + t2(a,c,i,m) * toooo(m, i, j, j)
term(15) = term(15) + t2(a,c,m,i) * toooo(m, i, j, j)
term(16) = term(16) + t2(b,c,i,m) * tvoov(a, i, m, b)
term(17) = term(17) + t2(a,c,i,m) * tvoov(b, i, m, b)
term(18) = term(18) + t2(a,b,i,m) * tvvoo(c, b, m, i)
term(19) = term(19) + t2(a,b,m,i) * tvvoo(c, b, m, i)
term(20) = term(20) + t2(b,c,m,i) * tvoov(a, i, m, b)
term(21) = term(21) + t2(a,b,i,m) * tvoov(c, i, m, b)
term(22) = term(22) + t2(a,c,i,m) * tvvoo(b, b, m, i)
term(23) = term(23) + t2(a,c,m,i) * tvvoo(b, b, m, i)
end do 

term(12) = term(12) * (-0.4999999999999998d+0) 
term(13) = term(13) * (-0.4999999999999998d+0) 
term(14) = term(14) * 0.49999999999999983d+0 
term(15) = term(15) * 0.49999999999999983d+0 
term(16) = term(16) * 0.49999999999999983d+0 
term(17) = term(17) * 0.49999999999999983d+0 
term(18) = term(18) * 0.49999999999999983d+0 
term(19) = term(19) * 0.49999999999999983d+0 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * (-0.49999999999999983d+0) 
term(22) = term(22) * (-0.49999999999999983d+0) 
term(23) = term(23) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aibjcibj = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjcibj = eom_cc3_31_triplet_trans_aibjcibj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjcibj
    function eom_cc3_31_triplet_trans_aibickbk(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibickbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,k) * tvoov(a, i, k, e)
term(1) = term(1) + t2(a,e,i,k) * tvoov(c, i, k, e)
term(2) = term(2) + t2(a,e,i,i) * tvvoo(c, e, k, k)
term(3) = term(3) + t2(c,e,i,i) * tvvoo(a, e, k, k)
term(4) = term(4) + t2(c,e,k,i) * tvoov(a, i, k, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(c, k, k, e)
term(6) = term(6) + t2(a,e,i,k) * tvvoo(c, e, k, i)
term(7) = term(7) + t2(c,e,k,i) * tvvoo(a, e, k, i)
term(8) = term(8) + t2(a,e,i,i) * read_ftvvvv(c, e, b, b)
term(9) = term(9) + t2(a,e,i,i) * read_ftvvvv(c, b, b, e)
term(10) = term(10) + t2(c,e,i,i) * read_ftvvvv(b, b, a, e)
term(11) = term(11) + t2(b,e,i,i) * read_ftvvvv(c, b, a, e)
end do 

term(0) = term(0) * 0.4999999999999998d+0 
term(1) = term(1) * 0.4999999999999998d+0 
term(2) = term(2) * 0.4999999999999998d+0 
term(3) = term(3) * 0.4999999999999998d+0 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * (-0.4999999999999998d+0) 
term(9) = term(9) * 0.49999999999999983d+0 
term(10) = term(10) * (-0.4999999999999998d+0) 
term(11) = term(11) * 0.49999999999999983d+0 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,i,m) * toooo(m, i, k, k)
term(13) = term(13) + t2(a,c,m,i) * toooo(m, i, k, k)
term(14) = term(14) + t2(a,c,i,m) * toooo(m, k, k, i)
term(15) = term(15) + t2(a,c,m,k) * toooo(m, i, k, i)
term(16) = term(16) + t2(b,c,i,m) * tvoov(a, i, m, b)
term(17) = term(17) + t2(b,c,m,i) * tvoov(a, i, m, b)
term(18) = term(18) + t2(a,c,i,m) * tvoov(b, i, m, b)
term(19) = term(19) + t2(a,b,i,m) * tvoov(c, i, m, b)
term(20) = term(20) + t2(a,c,i,m) * tvvoo(b, b, m, i)
term(21) = term(21) + t2(a,b,i,m) * tvvoo(c, b, m, i)
term(22) = term(22) + t2(a,c,m,i) * tvvoo(b, b, m, i)
term(23) = term(23) + t2(a,b,m,i) * tvvoo(c, b, m, i)
end do 

term(12) = term(12) * (-0.4999999999999998d+0) 
term(13) = term(13) * (-0.4999999999999998d+0) 
term(14) = term(14) * 0.49999999999999983d+0 
term(15) = term(15) * 0.49999999999999983d+0 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * 0.4999999999999998d+0 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * 0.4999999999999998d+0 
term(20) = term(20) * 0.4999999999999998d+0 
term(21) = term(21) * (-0.49999999999999983d+0) 
term(22) = term(22) * 0.4999999999999998d+0 
term(23) = term(23) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aibickbk = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibickbk = eom_cc3_31_triplet_trans_aibickbk + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibickbk
    function eom_cc3_31_triplet_trans_aibickci(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibickci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, c, b, e)
term(1) = term(1) + t2(b,e,k,i) * read_ftvvvv(c, c, a, e)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, e, b, c)
term(3) = term(3) + t2(c,e,k,i) * read_ftvvvv(b, c, a, e)
term(4) = term(4) + t2(b,e,k,i) * read_ftvvvv(c, e, a, c)
term(5) = term(5) + t2(b,e,i,k) * read_ftvvvv(c, e, a, c)
term(6) = term(6) + t2(c,e,i,k) * read_ftvvvv(b, e, a, c)
term(7) = term(7) + t2(c,e,k,i) * read_ftvvvv(b, e, a, c)
term(8) = term(8) + t2(b,e,k,i) * tvoov(a, i, i, e)
term(9) = term(9) + t2(b,e,i,k) * tvoov(a, i, i, e)
term(10) = term(10) + t2(a,e,i,i) * tvoov(b, k, i, e)
term(11) = term(11) + t2(a,e,i,k) * tvoov(b, i, i, e)
term(12) = term(12) + t2(a,e,i,k) * tvvoo(b, e, i, i)
term(13) = term(13) + t2(a,e,i,i) * tvvoo(b, e, i, k)
term(14) = term(14) + t2(b,e,k,i) * tvvoo(a, e, i, i)
term(15) = term(15) + t2(b,e,i,i) * tvvoo(a, e, i, k)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5000000000000001d+0) 
term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5000000000000001d+0) 
term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 
term(12) = term(12) * 0.49999999999999983d+0 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * 0.49999999999999983d+0 
term(15) = term(15) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,k,m) * tvoov(a, i, m, c)
term(17) = term(17) + t2(a,c,i,m) * tvoov(b, k, m, c)
term(18) = term(18) + t2(a,b,i,m) * tvvoo(c, c, m, k)
term(19) = term(19) + t2(a,b,m,k) * tvvoo(c, c, m, i)
term(20) = term(20) + t2(b,c,m,k) * tvoov(a, i, m, c)
term(21) = term(21) + t2(a,b,i,m) * tvoov(c, k, m, c)
term(22) = term(22) + t2(a,c,i,m) * tvvoo(b, c, m, k)
term(23) = term(23) + t2(a,c,m,k) * tvvoo(b, c, m, i)
term(24) = term(24) + t2(a,b,m,i) * tvoov(c, k, m, c)
term(25) = term(25) + t2(a,c,m,i) * tvoov(b, k, m, c)
term(26) = term(26) + t2(a,c,m,k) * tvoov(b, i, m, c)
term(27) = term(27) + t2(a,b,m,k) * tvoov(c, i, m, c)
term(28) = term(28) + t2(b,c,k,m) * tvvoo(a, c, m, i)
term(29) = term(29) + t2(b,c,i,m) * tvvoo(a, c, m, k)
term(30) = term(30) + t2(b,c,m,i) * tvvoo(a, c, m, k)
term(31) = term(31) + t2(b,c,m,k) * tvvoo(a, c, m, i)
term(32) = term(32) + t2(a,b,i,m) * toooo(m, k, i, i)
term(33) = term(33) + t2(a,b,i,m) * toooo(m, i, i, k)
term(34) = term(34) + t2(a,b,m,k) * toooo(m, i, i, i)
term(35) = term(35) + t2(a,b,m,i) * toooo(m, i, i, k)
end do 

term(16) = term(16) * 0.49999999999999983d+0 
term(17) = term(17) * 0.49999999999999983d+0 
term(18) = term(18) * 0.49999999999999983d+0 
term(19) = term(19) * 0.49999999999999983d+0 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * (-0.49999999999999983d+0) 
term(22) = term(22) * (-0.49999999999999983d+0) 
term(23) = term(23) * (-0.49999999999999983d+0) 
term(24) = term(24) * 0.5000000000000001d+0 
term(25) = term(25) * (-0.5d+0) 
term(26) = term(26) * 0.5000000000000001d+0 
term(27) = term(27) * (-0.5d+0) 
term(28) = term(28) * (-0.5d+0) 
term(29) = term(29) * 0.5000000000000001d+0 
term(30) = term(30) * (-0.5d+0) 
term(31) = term(31) * 0.5000000000000001d+0 
term(32) = term(32) * (-0.49999999999999983d+0) 
term(33) = term(33) * 0.49999999999999983d+0 
term(34) = term(34) * (-0.49999999999999983d+0) 
term(35) = term(35) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibickci = 0.d+0
    do s = 0, 35
    eom_cc3_31_triplet_trans_aibickci = eom_cc3_31_triplet_trans_aibickci + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibickci
    function eom_cc3_31_triplet_trans_aibjcici(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_31_triplet_trans_aibjcici 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:35) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, e, b, c)
term(1) = term(1) + t2(a,e,i,j) * read_ftvvvv(c, c, b, e)
term(2) = term(2) + t2(c,e,j,i) * read_ftvvvv(b, c, a, e)
term(3) = term(3) + t2(b,e,j,i) * read_ftvvvv(c, c, a, e)
term(4) = term(4) + t2(b,e,i,j) * read_ftvvvv(c, e, a, c)
term(5) = term(5) + t2(b,e,j,i) * read_ftvvvv(c, e, a, c)
term(6) = term(6) + t2(c,e,j,i) * read_ftvvvv(b, e, a, c)
term(7) = term(7) + t2(c,e,i,j) * read_ftvvvv(b, e, a, c)
term(8) = term(8) + t2(b,e,i,j) * tvoov(a, i, i, e)
term(9) = term(9) + t2(b,e,j,i) * tvoov(a, i, i, e)
term(10) = term(10) + t2(a,e,i,j) * tvoov(b, i, i, e)
term(11) = term(11) + t2(a,e,i,i) * tvoov(b, j, i, e)
term(12) = term(12) + t2(a,e,i,i) * tvvoo(b, e, i, j)
term(13) = term(13) + t2(a,e,i,j) * tvvoo(b, e, i, i)
term(14) = term(14) + t2(b,e,i,i) * tvvoo(a, e, i, j)
term(15) = term(15) + t2(b,e,j,i) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.5d+0 
term(5) = term(5) * (-0.5000000000000001d+0) 
term(6) = term(6) * 0.5d+0 
term(7) = term(7) * (-0.5000000000000001d+0) 
term(8) = term(8) * 0.49999999999999983d+0 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * (-0.49999999999999983d+0) 
term(12) = term(12) * 0.49999999999999983d+0 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * 0.49999999999999983d+0 
term(15) = term(15) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(16) = term(16) + t2(b,c,j,m) * tvoov(a, i, m, c)
term(17) = term(17) + t2(b,c,m,j) * tvoov(a, i, m, c)
term(18) = term(18) + t2(a,c,i,m) * tvoov(b, j, m, c)
term(19) = term(19) + t2(a,b,i,m) * tvoov(c, j, m, c)
term(20) = term(20) + t2(a,c,i,m) * tvvoo(b, c, m, j)
term(21) = term(21) + t2(a,b,i,m) * tvvoo(c, c, m, j)
term(22) = term(22) + t2(a,c,m,j) * tvvoo(b, c, m, i)
term(23) = term(23) + t2(a,b,m,j) * tvvoo(c, c, m, i)
term(24) = term(24) + t2(a,b,m,j) * tvoov(c, i, m, c)
term(25) = term(25) + t2(a,c,m,j) * tvoov(b, i, m, c)
term(26) = term(26) + t2(a,c,m,i) * tvoov(b, j, m, c)
term(27) = term(27) + t2(a,b,m,i) * tvoov(c, j, m, c)
term(28) = term(28) + t2(b,c,i,m) * tvvoo(a, c, m, j)
term(29) = term(29) + t2(b,c,j,m) * tvvoo(a, c, m, i)
term(30) = term(30) + t2(b,c,m,j) * tvvoo(a, c, m, i)
term(31) = term(31) + t2(b,c,m,i) * tvvoo(a, c, m, j)
term(32) = term(32) + t2(a,b,i,m) * toooo(m, i, i, j)
term(33) = term(33) + t2(a,b,i,m) * toooo(m, j, i, i)
term(34) = term(34) + t2(a,b,m,i) * toooo(m, i, i, j)
term(35) = term(35) + t2(a,b,m,j) * toooo(m, i, i, i)
end do 

term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * 0.4999999999999998d+0 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * 0.4999999999999998d+0 
term(20) = term(20) * 0.4999999999999998d+0 
term(21) = term(21) * (-0.49999999999999983d+0) 
term(22) = term(22) * 0.4999999999999998d+0 
term(23) = term(23) * (-0.49999999999999983d+0) 
term(24) = term(24) * 0.5000000000000001d+0 
term(25) = term(25) * (-0.5d+0) 
term(26) = term(26) * 0.5000000000000001d+0 
term(27) = term(27) * (-0.5d+0) 
term(28) = term(28) * (-0.5d+0) 
term(29) = term(29) * 0.5000000000000001d+0 
term(30) = term(30) * (-0.5d+0) 
term(31) = term(31) * 0.5000000000000001d+0 
term(32) = term(32) * (-0.49999999999999983d+0) 
term(33) = term(33) * 0.49999999999999983d+0 
term(34) = term(34) * (-0.49999999999999983d+0) 
term(35) = term(35) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibjcici = 0.d+0
    do s = 0, 35
    eom_cc3_31_triplet_trans_aibjcici = eom_cc3_31_triplet_trans_aibjcici + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjcici
    function eom_cc3_31_triplet_trans_aibjcicj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_31_triplet_trans_aibjcicj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tvoov(a, i, j, e)
term(1) = term(1) + t2(b,e,j,i) * tvoov(a, i, j, e)
term(2) = term(2) + t2(a,e,i,j) * tvoov(b, i, j, e)
term(3) = term(3) + t2(a,e,i,i) * tvoov(b, j, j, e)
term(4) = term(4) + t2(a,e,i,i) * tvvoo(b, e, j, j)
term(5) = term(5) + t2(a,e,i,j) * tvvoo(b, e, j, i)
term(6) = term(6) + t2(b,e,i,i) * tvvoo(a, e, j, j)
term(7) = term(7) + t2(b,e,j,i) * tvvoo(a, e, j, i)
term(8) = term(8) + t2(a,e,i,i) * read_ftvvvv(c, c, b, e)
term(9) = term(9) + t2(b,e,i,i) * read_ftvvvv(c, c, a, e)
term(10) = term(10) + t2(a,e,i,i) * read_ftvvvv(c, e, b, c)
term(11) = term(11) + t2(c,e,i,i) * read_ftvvvv(b, c, a, e)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * 0.49999999999999983d+0 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,i,m) * toooo(m, i, j, j)
term(13) = term(13) + t2(a,b,i,m) * toooo(m, j, j, i)
term(14) = term(14) + t2(a,b,m,i) * toooo(m, i, j, j)
term(15) = term(15) + t2(a,b,m,j) * toooo(m, i, j, i)
term(16) = term(16) + t2(b,c,i,m) * tvoov(a, i, m, c)
term(17) = term(17) + t2(a,c,i,m) * tvoov(b, i, m, c)
term(18) = term(18) + t2(a,b,i,m) * tvvoo(c, c, m, i)
term(19) = term(19) + t2(a,b,m,i) * tvvoo(c, c, m, i)
term(20) = term(20) + t2(b,c,m,i) * tvoov(a, i, m, c)
term(21) = term(21) + t2(a,b,i,m) * tvoov(c, i, m, c)
term(22) = term(22) + t2(a,c,i,m) * tvvoo(b, c, m, i)
term(23) = term(23) + t2(a,c,m,i) * tvvoo(b, c, m, i)
end do 

term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * 0.49999999999999983d+0 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * 0.49999999999999983d+0 
term(16) = term(16) * 0.49999999999999983d+0 
term(17) = term(17) * 0.49999999999999983d+0 
term(18) = term(18) * 0.49999999999999983d+0 
term(19) = term(19) * 0.49999999999999983d+0 
term(20) = term(20) * (-0.49999999999999983d+0) 
term(21) = term(21) * (-0.49999999999999983d+0) 
term(22) = term(22) * (-0.49999999999999983d+0) 
term(23) = term(23) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aibjcicj = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjcicj = eom_cc3_31_triplet_trans_aibjcicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjcicj
    function eom_cc3_31_triplet_trans_aibickck(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aibickck 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,k,i) * tvoov(a, i, k, e)
term(1) = term(1) + t2(b,e,i,k) * tvoov(a, i, k, e)
term(2) = term(2) + t2(a,e,i,i) * tvoov(b, k, k, e)
term(3) = term(3) + t2(a,e,i,k) * tvoov(b, i, k, e)
term(4) = term(4) + t2(a,e,i,k) * tvvoo(b, e, k, i)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(b, e, k, k)
term(6) = term(6) + t2(b,e,k,i) * tvvoo(a, e, k, i)
term(7) = term(7) + t2(b,e,i,i) * tvvoo(a, e, k, k)
term(8) = term(8) + t2(a,e,i,i) * read_ftvvvv(c, e, b, c)
term(9) = term(9) + t2(a,e,i,i) * read_ftvvvv(c, c, b, e)
term(10) = term(10) + t2(c,e,i,i) * read_ftvvvv(b, c, a, e)
term(11) = term(11) + t2(b,e,i,i) * read_ftvvvv(c, c, a, e)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * (-0.4999999999999998d+0) 
term(9) = term(9) * 0.49999999999999983d+0 
term(10) = term(10) * (-0.4999999999999998d+0) 
term(11) = term(11) * 0.49999999999999983d+0 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,i,m) * toooo(m, k, k, i)
term(13) = term(13) + t2(a,b,i,m) * toooo(m, i, k, k)
term(14) = term(14) + t2(a,b,m,k) * toooo(m, i, k, i)
term(15) = term(15) + t2(a,b,m,i) * toooo(m, i, k, k)
term(16) = term(16) + t2(b,c,i,m) * tvoov(a, i, m, c)
term(17) = term(17) + t2(b,c,m,i) * tvoov(a, i, m, c)
term(18) = term(18) + t2(a,c,i,m) * tvoov(b, i, m, c)
term(19) = term(19) + t2(a,b,i,m) * tvoov(c, i, m, c)
term(20) = term(20) + t2(a,c,i,m) * tvvoo(b, c, m, i)
term(21) = term(21) + t2(a,b,i,m) * tvvoo(c, c, m, i)
term(22) = term(22) + t2(a,c,m,i) * tvvoo(b, c, m, i)
term(23) = term(23) + t2(a,b,m,i) * tvvoo(c, c, m, i)
end do 

term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * 0.49999999999999983d+0 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * 0.49999999999999983d+0 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * 0.4999999999999998d+0 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * 0.4999999999999998d+0 
term(20) = term(20) * 0.4999999999999998d+0 
term(21) = term(21) * (-0.49999999999999983d+0) 
term(22) = term(22) * 0.4999999999999998d+0 
term(23) = term(23) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aibickck = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibickck = eom_cc3_31_triplet_trans_aibickck + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibickck
    function eom_cc3_31_triplet_trans_aiaickai(t2, nocc, nactive, a, i, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aiaickai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(c,e,i,k) * tvoov(a, i, i, e)
term(1) = term(1) + t2(a,e,i,k) * tvoov(c, i, i, e)
term(2) = term(2) + t2(a,e,i,k) * read_ftvvvv(c, a, a, e)
term(3) = term(3) + t2(a,e,k,i) * read_ftvvvv(c, a, a, e)
term(4) = term(4) + t2(a,e,i,i) * tvvoo(c, e, i, k)
term(5) = term(5) + t2(c,e,i,i) * tvvoo(a, e, i, k)
term(6) = term(6) + t2(c,e,i,i) * tvoov(a, k, i, e)
term(7) = term(7) + t2(a,e,k,i) * tvoov(c, i, i, e)
term(8) = term(8) + t2(a,e,k,i) * read_ftvvvv(c, e, a, a)
term(9) = term(9) + t2(c,e,i,k) * read_ftvvvv(a, e, a, a)
term(10) = term(10) + t2(a,e,k,i) * tvvoo(c, e, i, i)
term(11) = term(11) + t2(c,e,i,k) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * 0.4999999999999998d+0 
term(1) = term(1) * 0.4999999999999998d+0 
term(2) = term(2) * (-0.49999999999999983d+0) 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * 0.5d+0 
term(9) = term(9) * 0.5d+0 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,k,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,c,i,m) * tvoov(a, k, m, a)
term(14) = term(14) + t2(a,a,i,m) * tvvoo(c, a, m, k)
term(15) = term(15) + t2(a,a,k,m) * tvvoo(c, a, m, i)
term(16) = term(16) + t2(a,c,i,m) * toooo(m, i, i, k)
term(17) = term(17) + t2(a,c,m,i) * toooo(m, i, i, k)
term(18) = term(18) + t2(a,c,m,i) * tvoov(a, k, m, a)
term(19) = term(19) + t2(a,a,k,m) * tvoov(c, i, m, a)
term(20) = term(20) + t2(a,c,k,m) * tvvoo(a, a, m, i)
term(21) = term(21) + t2(a,c,m,i) * tvvoo(a, a, m, k)
term(22) = term(22) + t2(a,c,k,m) * toooo(m, i, i, i)
term(23) = term(23) + t2(a,c,m,i) * toooo(m, k, i, i)
end do 

term(12) = term(12) * 0.49999999999999983d+0 
term(13) = term(13) * 0.49999999999999983d+0 
term(14) = term(14) * 0.49999999999999983d+0 
term(15) = term(15) * 0.49999999999999983d+0 
term(16) = term(16) * (-0.4999999999999998d+0) 
term(17) = term(17) * (-0.4999999999999998d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * 0.5d+0 
term(23) = term(23) * 0.5d+0 


    eom_cc3_31_triplet_trans_aiaickai = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aiaickai = eom_cc3_31_triplet_trans_aiaickai + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiaickai
    function eom_cc3_31_triplet_trans_aiajciai(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_31_triplet_trans_aiajciai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, a, a, e)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, a, a, e)
term(2) = term(2) + t2(c,e,i,j) * tvoov(a, i, i, e)
term(3) = term(3) + t2(a,e,i,j) * tvoov(c, i, i, e)
term(4) = term(4) + t2(a,e,i,i) * tvvoo(c, e, i, j)
term(5) = term(5) + t2(c,e,i,i) * tvvoo(a, e, i, j)
term(6) = term(6) + t2(a,e,j,i) * tvoov(c, i, i, e)
term(7) = term(7) + t2(c,e,i,i) * tvoov(a, j, i, e)
term(8) = term(8) + t2(a,e,j,i) * read_ftvvvv(c, e, a, a)
term(9) = term(9) + t2(c,e,i,j) * read_ftvvvv(a, e, a, a)
term(10) = term(10) + t2(a,e,j,i) * tvvoo(c, e, i, i)
term(11) = term(11) + t2(c,e,i,j) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.49999999999999983d+0) 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * 0.5000000000000001d+0 
term(7) = term(7) * 0.5000000000000001d+0 
term(8) = term(8) * (-0.5000000000000001d+0) 
term(9) = term(9) * (-0.5000000000000001d+0) 
term(10) = term(10) * 0.5000000000000001d+0 
term(11) = term(11) * 0.5000000000000001d+0 

do m = 1, nocc 
term(12) = term(12) + t2(a,c,j,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,c,i,m) * tvoov(a, j, m, a)
term(14) = term(14) + t2(a,a,i,m) * tvvoo(c, a, m, j)
term(15) = term(15) + t2(a,a,j,m) * tvvoo(c, a, m, i)
term(16) = term(16) + t2(a,c,i,m) * toooo(m, i, i, j)
term(17) = term(17) + t2(a,c,m,i) * toooo(m, i, i, j)
term(18) = term(18) + t2(a,a,j,m) * tvoov(c, i, m, a)
term(19) = term(19) + t2(a,c,m,i) * tvoov(a, j, m, a)
term(20) = term(20) + t2(a,c,j,m) * tvvoo(a, a, m, i)
term(21) = term(21) + t2(a,c,m,i) * tvvoo(a, a, m, j)
term(22) = term(22) + t2(a,c,j,m) * toooo(m, i, i, i)
term(23) = term(23) + t2(a,c,m,i) * toooo(m, j, i, i)
end do 

term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * (-0.49999999999999983d+0) 
term(16) = term(16) * 0.49999999999999983d+0 
term(17) = term(17) * 0.49999999999999983d+0 
term(18) = term(18) * 0.5000000000000001d+0 
term(19) = term(19) * 0.5000000000000001d+0 
term(20) = term(20) * 0.5000000000000001d+0 
term(21) = term(21) * 0.5000000000000001d+0 
term(22) = term(22) * (-0.5000000000000001d+0) 
term(23) = term(23) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aiajciai = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aiajciai = eom_cc3_31_triplet_trans_aiajciai + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajciai
    function eom_cc3_31_triplet_trans_aiajciaj(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_31_triplet_trans_aiajciaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, a, a, e)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(c, e, a, a)
term(2) = term(2) + t2(c,e,i,i) * read_ftvvvv(a, e, a, a)
term(3) = term(3) + t2(c,e,i,j) * tvoov(a, i, j, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(c, i, j, e)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(c, e, j, j)
term(6) = term(6) + t2(c,e,i,i) * tvvoo(a, e, j, j)
term(7) = term(7) + t2(a,e,j,i) * tvoov(c, i, j, e)
term(8) = term(8) + t2(c,e,i,i) * tvoov(a, j, j, e)
term(9) = term(9) + t2(a,e,j,i) * tvvoo(c, e, j, i)
term(10) = term(10) + t2(c,e,i,j) * tvvoo(a, e, j, i)
end do 

term(0) = -term(0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * 0.5000000000000001d+0 
term(8) = term(8) * 0.5000000000000001d+0 
term(9) = term(9) * 0.5000000000000001d+0 
term(10) = term(10) * 0.5000000000000001d+0 

do m = 1, nocc 
term(11) = term(11) + t2(a,c,i,m) * tvoov(a, i, m, a)
term(12) = term(12) + t2(a,a,i,m) * tvvoo(c, a, m, i)
term(13) = term(13) + t2(a,c,m,i) * tvoov(a, i, m, a)
term(14) = term(14) + t2(a,a,i,m) * tvoov(c, i, m, a)
term(15) = term(15) + t2(a,c,i,m) * tvvoo(a, a, m, i)
term(16) = term(16) + t2(a,c,m,i) * tvvoo(a, a, m, i)
term(17) = term(17) + t2(a,c,i,m) * toooo(m, i, j, j)
term(18) = term(18) + t2(a,c,m,i) * toooo(m, i, j, j)
term(19) = term(19) + t2(a,c,j,m) * toooo(m, i, j, i)
term(20) = term(20) + t2(a,c,m,i) * toooo(m, j, j, i)
end do 

term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * (-0.49999999999999983d+0) 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * 0.49999999999999983d+0 
term(18) = term(18) * 0.49999999999999983d+0 
term(19) = term(19) * (-0.5000000000000001d+0) 
term(20) = term(20) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aiajciaj = 0.d+0
    do s = 0, 20
    eom_cc3_31_triplet_trans_aiajciaj = eom_cc3_31_triplet_trans_aiajciaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajciaj
    function eom_cc3_31_triplet_trans_aiaickak(t2, nocc, nactive, a, i, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aiaickak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, a, a)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(c, a, a, e)
term(2) = term(2) + t2(c,e,i,i) * read_ftvvvv(a, e, a, a)
term(3) = term(3) + t2(c,e,i,k) * tvoov(a, i, k, e)
term(4) = term(4) + t2(a,e,i,k) * tvoov(c, i, k, e)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(c, e, k, k)
term(6) = term(6) + t2(c,e,i,i) * tvvoo(a, e, k, k)
term(7) = term(7) + t2(c,e,i,i) * tvoov(a, k, k, e)
term(8) = term(8) + t2(a,e,k,i) * tvoov(c, i, k, e)
term(9) = term(9) + t2(a,e,k,i) * tvvoo(c, e, k, i)
term(10) = term(10) + t2(c,e,i,k) * tvvoo(a, e, k, i)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.4999999999999998d+0 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 

do m = 1, nocc 
term(11) = term(11) + t2(a,c,i,m) * tvoov(a, i, m, a)
term(12) = term(12) + t2(a,c,m,i) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,a,i,m) * tvoov(c, i, m, a)
term(14) = term(14) + t2(a,c,i,m) * tvvoo(a, a, m, i)
term(15) = term(15) + t2(a,a,i,m) * tvvoo(c, a, m, i)
term(16) = term(16) + t2(a,c,m,i) * tvvoo(a, a, m, i)
term(17) = term(17) + t2(a,c,i,m) * toooo(m, i, k, k)
term(18) = term(18) + t2(a,c,m,i) * toooo(m, i, k, k)
term(19) = term(19) + t2(a,c,k,m) * toooo(m, i, k, i)
term(20) = term(20) + t2(a,c,m,i) * toooo(m, k, k, i)
end do 

term(11) = -term(11) 
term(12) = term(12) * 0.4999999999999998d+0 
term(13) = term(13) * 0.4999999999999998d+0 
term(14) = term(14) * 0.4999999999999998d+0 
term(15) = -term(15) 
term(16) = term(16) * 0.4999999999999998d+0 
term(17) = term(17) * (-0.4999999999999998d+0) 
term(18) = term(18) * (-0.4999999999999998d+0) 
term(19) = term(19) * 0.5d+0 
term(20) = term(20) * 0.5d+0 


    eom_cc3_31_triplet_trans_aiaickak = 0.d+0
    do s = 0, 20
    eom_cc3_31_triplet_trans_aiaickak = eom_cc3_31_triplet_trans_aiaickak + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiaickak
    function eom_cc3_31_triplet_trans_aiaickci(t2, nocc, nactive, a, i, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aiaickci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(c, c, a, e)
term(1) = term(1) + t2(a,e,k,i) * read_ftvvvv(c, c, a, e)
term(2) = term(2) + t2(a,e,k,i) * read_ftvvvv(c, e, a, c)
term(3) = term(3) + t2(c,e,i,k) * read_ftvvvv(a, e, a, c)
term(4) = term(4) + t2(a,e,k,i) * tvoov(a, i, i, e)
term(5) = term(5) + t2(a,e,i,k) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,i,i) * tvoov(a, k, i, e)
term(7) = term(7) + t2(a,e,i,k) * tvvoo(a, e, i, i)
term(8) = term(8) + t2(a,e,i,i) * tvvoo(a, e, i, k)
term(9) = term(9) + t2(a,e,k,i) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * 0.5d+0 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = -term(5) 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * 0.49999999999999983d+0 
term(8) = -term(8) 
term(9) = term(9) * 0.49999999999999983d+0 

do m = 1, nocc 
term(10) = term(10) + t2(a,c,k,m) * tvoov(a, i, m, c)
term(11) = term(11) + t2(a,c,i,m) * tvoov(a, k, m, c)
term(12) = term(12) + t2(a,a,i,m) * tvvoo(c, c, m, k)
term(13) = term(13) + t2(a,a,k,m) * tvvoo(c, c, m, i)
term(14) = term(14) + t2(a,c,m,i) * tvoov(a, k, m, c)
term(15) = term(15) + t2(a,a,k,m) * tvoov(c, i, m, c)
term(16) = term(16) + t2(a,c,k,m) * tvvoo(a, c, m, i)
term(17) = term(17) + t2(a,c,m,i) * tvvoo(a, c, m, k)
term(18) = term(18) + t2(a,a,i,m) * toooo(m, k, i, i)
term(19) = term(19) + t2(a,a,i,m) * toooo(m, i, i, k)
term(20) = term(20) + t2(a,a,k,m) * toooo(m, i, i, i)
end do 

term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * 0.49999999999999983d+0 
term(12) = term(12) * 0.49999999999999983d+0 
term(13) = term(13) * 0.49999999999999983d+0 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(20) = term(20) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aiaickci = 0.d+0
    do s = 0, 20
    eom_cc3_31_triplet_trans_aiaickci = eom_cc3_31_triplet_trans_aiaickci + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiaickci
    function eom_cc3_31_triplet_trans_aiajcici(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_31_triplet_trans_aiajcici 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(c, c, a, e)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(c, c, a, e)
term(2) = term(2) + t2(a,e,j,i) * read_ftvvvv(c, e, a, c)
term(3) = term(3) + t2(c,e,i,j) * read_ftvvvv(a, e, a, c)
term(4) = term(4) + t2(a,e,i,j) * tvoov(a, i, i, e)
term(5) = term(5) + t2(a,e,j,i) * tvoov(a, i, i, e)
term(6) = term(6) + t2(a,e,i,i) * tvoov(a, j, i, e)
term(7) = term(7) + t2(a,e,i,i) * tvvoo(a, e, i, j)
term(8) = term(8) + t2(a,e,i,j) * tvvoo(a, e, i, i)
term(9) = term(9) + t2(a,e,j,i) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.5000000000000001d+0) 
term(3) = term(3) * (-0.5000000000000001d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(10) = term(10) + t2(a,c,j,m) * tvoov(a, i, m, c)
term(11) = term(11) + t2(a,c,i,m) * tvoov(a, j, m, c)
term(12) = term(12) + t2(a,a,i,m) * tvvoo(c, c, m, j)
term(13) = term(13) + t2(a,a,j,m) * tvvoo(c, c, m, i)
term(14) = term(14) + t2(a,a,j,m) * tvoov(c, i, m, c)
term(15) = term(15) + t2(a,c,m,i) * tvoov(a, j, m, c)
term(16) = term(16) + t2(a,c,j,m) * tvvoo(a, c, m, i)
term(17) = term(17) + t2(a,c,m,i) * tvvoo(a, c, m, j)
term(18) = term(18) + t2(a,a,i,m) * toooo(m, i, i, j)
term(19) = term(19) + t2(a,a,i,m) * toooo(m, j, i, i)
term(20) = term(20) + t2(a,a,j,m) * toooo(m, i, i, i)
end do 

term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 
term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * 0.5000000000000001d+0 
term(15) = term(15) * 0.5000000000000001d+0 
term(16) = term(16) * 0.5000000000000001d+0 
term(17) = term(17) * 0.5000000000000001d+0 
term(18) = -term(18) 
term(19) = term(19) * 0.49999999999999983d+0 
term(20) = term(20) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aiajcici = 0.d+0
    do s = 0, 20
    eom_cc3_31_triplet_trans_aiajcici = eom_cc3_31_triplet_trans_aiajcici + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajcici
    function eom_cc3_31_triplet_trans_aiajcicj(t2, nocc, nactive, a, i, j, c) 
    double precision :: eom_cc3_31_triplet_trans_aiajcicj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, c, a, e)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(c, e, a, c)
term(2) = term(2) + t2(c,e,i,i) * read_ftvvvv(a, e, a, c)
term(3) = term(3) + t2(a,e,i,j) * tvoov(a, i, j, e)
term(4) = term(4) + t2(a,e,j,i) * tvoov(a, i, j, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(a, j, j, e)
term(6) = term(6) + t2(a,e,i,i) * tvvoo(a, e, j, j)
term(7) = term(7) + t2(a,e,i,j) * tvvoo(a, e, j, i)
term(8) = term(8) + t2(a,e,j,i) * tvvoo(a, e, j, i)
end do 

term(0) = -term(0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * 0.49999999999999983d+0 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(9) = term(9) + t2(a,c,i,m) * tvoov(a, i, m, c)
term(10) = term(10) + t2(a,a,i,m) * tvvoo(c, c, m, i)
term(11) = term(11) + t2(a,c,m,i) * tvoov(a, i, m, c)
term(12) = term(12) + t2(a,a,i,m) * tvoov(c, i, m, c)
term(13) = term(13) + t2(a,c,i,m) * tvvoo(a, c, m, i)
term(14) = term(14) + t2(a,c,m,i) * tvvoo(a, c, m, i)
term(15) = term(15) + t2(a,a,i,m) * toooo(m, i, j, j)
term(16) = term(16) + t2(a,a,i,m) * toooo(m, j, j, i)
term(17) = term(17) + t2(a,a,j,m) * toooo(m, i, j, i)
end do 

term(11) = term(11) * (-0.49999999999999983d+0) 
term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = -term(15) 
term(16) = term(16) * 0.49999999999999983d+0 
term(17) = term(17) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aiajcicj = 0.d+0
    do s = 0, 17
    eom_cc3_31_triplet_trans_aiajcicj = eom_cc3_31_triplet_trans_aiajcicj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiajcicj
    function eom_cc3_31_triplet_trans_aiaickck(t2, nocc, nactive, a, i, c, k) 
    double precision :: eom_cc3_31_triplet_trans_aiaickck 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, c, k 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(c, e, a, c)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(c, c, a, e)
term(2) = term(2) + t2(c,e,i,i) * read_ftvvvv(a, e, a, c)
term(3) = term(3) + t2(a,e,k,i) * tvoov(a, i, k, e)
term(4) = term(4) + t2(a,e,i,k) * tvoov(a, i, k, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(a, k, k, e)
term(6) = term(6) + t2(a,e,i,k) * tvvoo(a, e, k, i)
term(7) = term(7) + t2(a,e,i,i) * tvvoo(a, e, k, k)
term(8) = term(8) + t2(a,e,k,i) * tvvoo(a, e, k, i)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = -term(4) 
term(5) = term(5) * 0.49999999999999983d+0 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = -term(7) 
term(8) = term(8) * 0.49999999999999983d+0 

do m = 1, nocc 
term(9) = term(9) + t2(a,c,i,m) * tvoov(a, i, m, c)
term(10) = term(10) + t2(a,c,m,i) * tvoov(a, i, m, c)
term(11) = term(11) + t2(a,a,i,m) * tvoov(c, i, m, c)
term(12) = term(12) + t2(a,c,i,m) * tvvoo(a, c, m, i)
term(13) = term(13) + t2(a,a,i,m) * tvvoo(c, c, m, i)
term(14) = term(14) + t2(a,c,m,i) * tvvoo(a, c, m, i)
term(15) = term(15) + t2(a,a,i,m) * toooo(m, k, k, i)
term(16) = term(16) + t2(a,a,i,m) * toooo(m, i, k, k)
term(17) = term(17) + t2(a,a,k,m) * toooo(m, i, k, i)
end do 

term(9) = -term(9) 
term(10) = term(10) * 0.4999999999999998d+0 
term(11) = term(11) * 0.4999999999999998d+0 
term(12) = term(12) * 0.4999999999999998d+0 
term(13) = -term(13) 
term(14) = term(14) * 0.4999999999999998d+0 
term(15) = term(15) * (-0.49999999999999983d+0) 
term(17) = term(17) * (-0.49999999999999983d+0) 


    eom_cc3_31_triplet_trans_aiaickck = 0.d+0
    do s = 0, 17
    eom_cc3_31_triplet_trans_aiaickck = eom_cc3_31_triplet_trans_aiaickck + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aiaickck
    function eom_cc3_31_triplet_trans_aibiakai(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_31_triplet_trans_aibiakai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,k) * tvoov(a, i, i, e)
term(1) = term(1) + t2(a,e,i,k) * tvoov(b, i, i, e)
term(2) = term(2) + t2(a,e,i,i) * tvvoo(b, e, i, k)
term(3) = term(3) + t2(b,e,i,i) * tvvoo(a, e, i, k)
term(4) = term(4) + t2(a,e,i,k) * read_ftvvvv(b, a, a, e)
term(5) = term(5) + t2(a,e,k,i) * read_ftvvvv(b, a, a, e)
term(6) = term(6) + t2(b,e,i,i) * tvoov(a, k, i, e)
term(7) = term(7) + t2(a,e,k,i) * tvoov(b, i, i, e)
term(8) = term(8) + t2(b,e,i,k) * read_ftvvvv(a, e, a, a)
term(9) = term(9) + t2(a,e,k,i) * read_ftvvvv(b, e, a, a)
term(10) = term(10) + t2(b,e,i,k) * tvvoo(a, e, i, i)
term(11) = term(11) + t2(a,e,k,i) * tvvoo(b, e, i, i)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(2) = term(2) * (-0.49999999999999983d+0) 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * 0.49999999999999983d+0 
term(6) = term(6) * 0.5000000000000001d+0 
term(7) = term(7) * 0.5000000000000001d+0 
term(8) = term(8) * (-0.5000000000000001d+0) 
term(9) = term(9) * (-0.5000000000000001d+0) 
term(10) = term(10) * 0.5000000000000001d+0 
term(11) = term(11) * 0.5000000000000001d+0 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,i,m) * toooo(m, i, i, k)
term(13) = term(13) + t2(a,b,m,i) * toooo(m, i, i, k)
term(14) = term(14) + t2(a,b,k,m) * tvoov(a, i, m, a)
term(15) = term(15) + t2(a,b,i,m) * tvoov(a, k, m, a)
term(16) = term(16) + t2(a,a,i,m) * tvvoo(b, a, m, k)
term(17) = term(17) + t2(a,a,k,m) * tvvoo(b, a, m, i)
term(18) = term(18) + t2(a,b,m,i) * tvoov(a, k, m, a)
term(19) = term(19) + t2(a,a,k,m) * tvoov(b, i, m, a)
term(20) = term(20) + t2(a,b,m,i) * tvvoo(a, a, m, k)
term(21) = term(21) + t2(a,b,k,m) * tvvoo(a, a, m, i)
term(22) = term(22) + t2(a,b,m,i) * toooo(m, k, i, i)
term(23) = term(23) + t2(a,b,k,m) * toooo(m, i, i, i)
end do 

term(12) = term(12) * 0.49999999999999983d+0 
term(13) = term(13) * 0.49999999999999983d+0 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = term(15) * (-0.49999999999999983d+0) 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * 0.5000000000000001d+0 
term(19) = term(19) * 0.5000000000000001d+0 
term(20) = term(20) * 0.5000000000000001d+0 
term(21) = term(21) * 0.5000000000000001d+0 
term(22) = term(22) * (-0.5000000000000001d+0) 
term(23) = term(23) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibiakai = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibiakai = eom_cc3_31_triplet_trans_aibiakai + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibiakai
    function eom_cc3_31_triplet_trans_aibjaiai(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_31_triplet_trans_aibjaiai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tvoov(a, i, i, e)
term(1) = term(1) + t2(a,e,i,j) * tvoov(b, i, i, e)
term(2) = term(2) + t2(a,e,i,j) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(b, a, a, e)
term(4) = term(4) + t2(a,e,i,i) * tvvoo(b, e, i, j)
term(5) = term(5) + t2(b,e,i,i) * tvvoo(a, e, i, j)
term(6) = term(6) + t2(a,e,j,i) * tvoov(b, i, i, e)
term(7) = term(7) + t2(b,e,i,i) * tvoov(a, j, i, e)
term(8) = term(8) + t2(b,e,i,j) * read_ftvvvv(a, e, a, a)
term(9) = term(9) + t2(a,e,j,i) * read_ftvvvv(b, e, a, a)
term(10) = term(10) + t2(b,e,i,j) * tvvoo(a, e, i, i)
term(11) = term(11) + t2(a,e,j,i) * tvvoo(b, e, i, i)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.4999999999999998d+0) 
term(3) = term(3) * (-0.4999999999999998d+0) 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * 0.49999999999999983d+0 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * 0.5d+0 
term(9) = term(9) * 0.5d+0 
term(10) = term(10) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 

do m = 1, nocc 
term(12) = term(12) + t2(a,b,j,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,b,i,m) * tvoov(a, j, m, a)
term(14) = term(14) + t2(a,a,i,m) * tvvoo(b, a, m, j)
term(15) = term(15) + t2(a,a,j,m) * tvvoo(b, a, m, i)
term(16) = term(16) + t2(a,b,i,m) * toooo(m, i, i, j)
term(17) = term(17) + t2(a,b,m,i) * toooo(m, i, i, j)
term(18) = term(18) + t2(a,a,j,m) * tvoov(b, i, m, a)
term(19) = term(19) + t2(a,b,m,i) * tvoov(a, j, m, a)
term(20) = term(20) + t2(a,b,m,i) * tvvoo(a, a, m, j)
term(21) = term(21) + t2(a,b,j,m) * tvvoo(a, a, m, i)
term(22) = term(22) + t2(a,b,m,i) * toooo(m, j, i, i)
term(23) = term(23) + t2(a,b,j,m) * toooo(m, i, i, i)
end do 

term(12) = term(12) * 0.4999999999999998d+0 
term(13) = term(13) * 0.4999999999999998d+0 
term(14) = term(14) * 0.4999999999999998d+0 
term(15) = term(15) * 0.4999999999999998d+0 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * (-0.5d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * 0.5d+0 
term(23) = term(23) * 0.5d+0 


    eom_cc3_31_triplet_trans_aibjaiai = 0.d+0
    do s = 0, 23
    eom_cc3_31_triplet_trans_aibjaiai = eom_cc3_31_triplet_trans_aibjaiai + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjaiai
    function eom_cc3_31_triplet_trans_aibjaiaj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_31_triplet_trans_aibjaiaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    double precision, dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(b, e, a, a)
term(1) = term(1) + t2(b,e,i,i) * read_ftvvvv(a, e, a, a)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(b, a, a, e)
term(3) = term(3) + t2(b,e,i,j) * tvoov(a, i, j, e)
term(4) = term(4) + t2(a,e,i,j) * tvoov(b, i, j, e)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(b, e, j, j)
term(6) = term(6) + t2(b,e,i,i) * tvvoo(a, e, j, j)
term(7) = term(7) + t2(a,e,j,i) * tvoov(b, i, j, e)
term(8) = term(8) + t2(b,e,i,i) * tvoov(a, j, j, e)
term(9) = term(9) + t2(b,e,i,j) * tvvoo(a, e, j, i)
term(10) = term(10) + t2(a,e,j,i) * tvvoo(b, e, j, i)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(3) = term(3) * 0.49999999999999983d+0 
term(4) = term(4) * 0.49999999999999983d+0 
term(5) = term(5) * 0.49999999999999983d+0 
term(6) = term(6) * 0.49999999999999983d+0 
term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-0.5d+0) 
term(9) = term(9) * (-0.5d+0) 
term(10) = term(10) * (-0.5d+0) 

do m = 1, nocc 
term(11) = term(11) + t2(a,b,m,i) * tvoov(a, i, m, a)
term(12) = term(12) + t2(a,a,i,m) * tvoov(b, i, m, a)
term(13) = term(13) + t2(a,b,i,m) * tvvoo(a, a, m, i)
term(14) = term(14) + t2(a,b,m,i) * tvvoo(a, a, m, i)
term(15) = term(15) + t2(a,b,i,m) * tvoov(a, i, m, a)
term(16) = term(16) + t2(a,a,i,m) * tvvoo(b, a, m, i)
term(17) = term(17) + t2(a,b,i,m) * toooo(m, i, j, j)
term(18) = term(18) + t2(a,b,m,i) * toooo(m, i, j, j)
term(19) = term(19) + t2(a,b,m,i) * toooo(m, j, j, i)
term(20) = term(20) + t2(a,b,j,m) * toooo(m, i, j, i)
end do 

term(11) = term(11) * 0.49999999999999983d+0 
term(12) = term(12) * 0.49999999999999983d+0 
term(13) = term(13) * 0.49999999999999983d+0 
term(14) = term(14) * 0.49999999999999983d+0 
term(15) = -term(15) 
term(16) = -term(16) 
term(17) = term(17) * (-0.49999999999999983d+0) 
term(18) = term(18) * (-0.49999999999999983d+0) 
term(19) = term(19) * 0.5d+0 
term(20) = term(20) * 0.5d+0 


    eom_cc3_31_triplet_trans_aibjaiaj = 0.d+0
    do s = 0, 20
    eom_cc3_31_triplet_trans_aibjaiaj = eom_cc3_31_triplet_trans_aibjaiaj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjaiaj
    function eom_cc3_31_triplet_trans_aibiakak(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_31_triplet_trans_aibiakak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    double precision, dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(b, a, a, e)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(b, e, a, a)
term(2) = term(2) + t2(b,e,i,i) * read_ftvvvv(a, e, a, a)
term(3) = term(3) + t2(b,e,i,k) * tvoov(a, i, k, e)
term(4) = term(4) + t2(a,e,i,k) * tvoov(b, i, k, e)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(b, e, k, k)
term(6) = term(6) + t2(b,e,i,i) * tvvoo(a, e, k, k)
term(7) = term(7) + t2(b,e,i,i) * tvoov(a, k, k, e)
term(8) = term(8) + t2(a,e,k,i) * tvoov(b, i, k, e)
term(9) = term(9) + t2(b,e,i,k) * tvvoo(a, e, k, i)
term(10) = term(10) + t2(a,e,k,i) * tvvoo(b, e, k, i)
end do 

term(0) = -term(0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * 0.49999999999999983d+0 
term(3) = term(3) * (-0.49999999999999983d+0) 
term(4) = term(4) * (-0.49999999999999983d+0) 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * 0.5000000000000001d+0 
term(8) = term(8) * 0.5000000000000001d+0 
term(9) = term(9) * 0.5000000000000001d+0 
term(10) = term(10) * 0.5000000000000001d+0 

do m = 1, nocc 
term(11) = term(11) + t2(a,b,m,i) * tvoov(a, i, m, a)
term(12) = term(12) + t2(a,b,i,m) * tvoov(a, i, m, a)
term(13) = term(13) + t2(a,a,i,m) * tvoov(b, i, m, a)
term(14) = term(14) + t2(a,a,i,m) * tvvoo(b, a, m, i)
term(15) = term(15) + t2(a,b,i,m) * tvvoo(a, a, m, i)
term(16) = term(16) + t2(a,b,m,i) * tvvoo(a, a, m, i)
term(17) = term(17) + t2(a,b,i,m) * toooo(m, i, k, k)
term(18) = term(18) + t2(a,b,m,i) * toooo(m, i, k, k)
term(19) = term(19) + t2(a,b,m,i) * toooo(m, k, k, i)
term(20) = term(20) + t2(a,b,k,m) * toooo(m, i, k, i)
end do 

term(11) = term(11) * (-0.49999999999999983d+0) 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(15) = term(15) * (-0.49999999999999983d+0) 
term(16) = term(16) * (-0.49999999999999983d+0) 
term(17) = term(17) * 0.49999999999999983d+0 
term(18) = term(18) * 0.49999999999999983d+0 
term(19) = term(19) * (-0.5000000000000001d+0) 
term(20) = term(20) * (-0.5000000000000001d+0) 


    eom_cc3_31_triplet_trans_aibiakak = 0.d+0
    do s = 0, 20
    eom_cc3_31_triplet_trans_aibiakak = eom_cc3_31_triplet_trans_aibiakak + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibiakak
    function eom_cc3_31_triplet_trans_aibiakbi(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_31_triplet_trans_aibiakbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    double precision, dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,k) * read_ftvvvv(b, b, a, e)
term(1) = term(1) + t2(a,e,k,i) * read_ftvvvv(b, b, a, e)
term(2) = term(2) + t2(b,e,i,k) * read_ftvvvv(a, e, a, b)
term(3) = term(3) + t2(a,e,k,i) * read_ftvvvv(b, e, a, b)
term(4) = term(4) + t2(a,e,i,k) * tvoov(a, i, i, e)
term(5) = term(5) + t2(a,e,i,i) * tvvoo(a, e, i, k)
term(6) = term(6) + t2(a,e,k,i) * tvoov(a, i, i, e)
term(7) = term(7) + t2(a,e,i,i) * tvoov(a, k, i, e)
term(8) = term(8) + t2(a,e,i,k) * tvvoo(a, e, i, i)
term(9) = term(9) + t2(a,e,k,i) * tvvoo(a, e, i, i)
end do 

term(0) = term(0) * 0.49999999999999983d+0 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * (-0.5000000000000001d+0) 
term(3) = term(3) * (-0.5000000000000001d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 
term(9) = term(9) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(10) = term(10) + t2(a,b,k,m) * tvoov(a, i, m, b)
term(11) = term(11) + t2(a,b,i,m) * tvoov(a, k, m, b)
term(12) = term(12) + t2(a,a,i,m) * tvvoo(b, b, m, k)
term(13) = term(13) + t2(a,a,k,m) * tvvoo(b, b, m, i)
term(14) = term(14) + t2(a,b,m,i) * tvoov(a, k, m, b)
term(15) = term(15) + t2(a,a,k,m) * tvoov(b, i, m, b)
term(16) = term(16) + t2(a,b,m,i) * tvvoo(a, b, m, k)
term(17) = term(17) + t2(a,b,k,m) * tvvoo(a, b, m, i)
term(18) = term(18) + t2(a,a,i,m) * toooo(m, i, i, k)
term(19) = term(19) + t2(a,a,i,m) * toooo(m, k, i, i)
term(20) = term(20) + t2(a,a,k,m) * toooo(m, i, i, i)
end do 

term(10) = term(10) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 
term(12) = term(12) * (-0.49999999999999983d+0) 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * 0.5000000000000001d+0 
term(15) = term(15) * 0.5000000000000001d+0 
term(16) = term(16) * 0.5000000000000001d+0 
term(17) = term(17) * 0.5000000000000001d+0 
term(18) = -term(18) 
term(19) = term(19) * 0.49999999999999983d+0 
term(20) = term(20) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibiakbi = 0.d+0
    do s = 0, 20
    eom_cc3_31_triplet_trans_aibiakbi = eom_cc3_31_triplet_trans_aibiakbi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibiakbi
    function eom_cc3_31_triplet_trans_aibjaibi(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_31_triplet_trans_aibjaibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    double precision, dimension(0:20) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,j) * read_ftvvvv(b, b, a, e)
term(1) = term(1) + t2(a,e,j,i) * read_ftvvvv(b, b, a, e)
term(2) = term(2) + t2(b,e,i,j) * read_ftvvvv(a, e, a, b)
term(3) = term(3) + t2(a,e,j,i) * read_ftvvvv(b, e, a, b)
term(4) = term(4) + t2(a,e,j,i) * tvoov(a, i, i, e)
term(5) = term(5) + t2(a,e,i,i) * tvoov(a, j, i, e)
term(6) = term(6) + t2(a,e,i,j) * tvvoo(a, e, i, i)
term(7) = term(7) + t2(a,e,j,i) * tvvoo(a, e, i, i)
term(8) = term(8) + t2(a,e,i,j) * tvoov(a, i, i, e)
term(9) = term(9) + t2(a,e,i,i) * tvvoo(a, e, i, j)
end do 

term(0) = term(0) * (-0.4999999999999998d+0) 
term(1) = term(1) * (-0.4999999999999998d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * 0.5d+0 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = term(7) * 0.4999999999999998d+0 
term(8) = -term(8) 
term(9) = -term(9) 

do m = 1, nocc 
term(10) = term(10) + t2(a,b,j,m) * tvoov(a, i, m, b)
term(11) = term(11) + t2(a,b,i,m) * tvoov(a, j, m, b)
term(12) = term(12) + t2(a,a,i,m) * tvvoo(b, b, m, j)
term(13) = term(13) + t2(a,a,j,m) * tvvoo(b, b, m, i)
term(14) = term(14) + t2(a,a,j,m) * tvoov(b, i, m, b)
term(15) = term(15) + t2(a,b,m,i) * tvoov(a, j, m, b)
term(16) = term(16) + t2(a,b,m,i) * tvvoo(a, b, m, j)
term(17) = term(17) + t2(a,b,j,m) * tvvoo(a, b, m, i)
term(18) = term(18) + t2(a,a,i,m) * toooo(m, j, i, i)
term(19) = term(19) + t2(a,a,j,m) * toooo(m, i, i, i)
term(20) = term(20) + t2(a,a,i,m) * toooo(m, i, i, j)
end do 

term(10) = term(10) * 0.4999999999999998d+0 
term(11) = term(11) * 0.4999999999999998d+0 
term(12) = term(12) * 0.4999999999999998d+0 
term(13) = term(13) * 0.4999999999999998d+0 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.4999999999999998d+0) 
term(19) = term(19) * (-0.4999999999999998d+0) 


    eom_cc3_31_triplet_trans_aibjaibi = 0.d+0
    do s = 0, 20
    eom_cc3_31_triplet_trans_aibjaibi = eom_cc3_31_triplet_trans_aibjaibi + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjaibi
    function eom_cc3_31_triplet_trans_aibjaibj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_31_triplet_trans_aibjaibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(b, e, a, b)
term(1) = term(1) + t2(b,e,i,i) * read_ftvvvv(a, e, a, b)
term(2) = term(2) + t2(a,e,i,i) * read_ftvvvv(b, b, a, e)
term(3) = term(3) + t2(a,e,j,i) * tvoov(a, i, j, e)
term(4) = term(4) + t2(a,e,i,i) * tvoov(a, j, j, e)
term(5) = term(5) + t2(a,e,i,j) * tvvoo(a, e, j, i)
term(6) = term(6) + t2(a,e,j,i) * tvvoo(a, e, j, i)
term(7) = term(7) + t2(a,e,i,j) * tvoov(a, i, j, e)
term(8) = term(8) + t2(a,e,i,i) * tvvoo(a, e, j, j)
end do 

term(0) = term(0) * (-0.49999999999999983d+0) 
term(1) = term(1) * (-0.49999999999999983d+0) 
term(3) = term(3) * 0.4999999999999998d+0 
term(4) = term(4) * 0.4999999999999998d+0 
term(5) = term(5) * 0.4999999999999998d+0 
term(6) = term(6) * 0.4999999999999998d+0 
term(7) = -term(7) 
term(8) = -term(8) 

do m = 1, nocc 
term(9) = term(9) + t2(a,b,m,i) * tvoov(a, i, m, b)
term(10) = term(10) + t2(a,a,i,m) * tvoov(b, i, m, b)
term(11) = term(11) + t2(a,b,i,m) * tvvoo(a, b, m, i)
term(12) = term(12) + t2(a,b,m,i) * tvvoo(a, b, m, i)
term(13) = term(13) + t2(a,b,i,m) * tvoov(a, i, m, b)
term(14) = term(14) + t2(a,a,i,m) * tvvoo(b, b, m, i)
term(15) = term(15) + t2(a,a,i,m) * toooo(m, j, j, i)
term(16) = term(16) + t2(a,a,j,m) * toooo(m, i, j, i)
term(17) = term(17) + t2(a,a,i,m) * toooo(m, i, j, j)
end do 

term(9) = term(9) * 0.49999999999999983d+0 
term(10) = term(10) * 0.49999999999999983d+0 
term(11) = term(11) * 0.49999999999999983d+0 
term(12) = term(12) * 0.49999999999999983d+0 
term(13) = -term(13) 
term(14) = -term(14) 
term(15) = term(15) * (-0.4999999999999998d+0) 
term(16) = term(16) * (-0.4999999999999998d+0) 


    eom_cc3_31_triplet_trans_aibjaibj = 0.d+0
    do s = 0, 17
    eom_cc3_31_triplet_trans_aibjaibj = eom_cc3_31_triplet_trans_aibjaibj + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibjaibj
    function eom_cc3_31_triplet_trans_aibiakbk(t2, nocc, nactive, a, i, b, k) 
    double precision :: eom_cc3_31_triplet_trans_aibiakbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, k 
    integer :: s ,e,m 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,i,i) * read_ftvvvv(b, b, a, e)
term(1) = term(1) + t2(a,e,i,i) * read_ftvvvv(b, e, a, b)
term(2) = term(2) + t2(b,e,i,i) * read_ftvvvv(a, e, a, b)
term(3) = term(3) + t2(a,e,i,k) * tvoov(a, i, k, e)
term(4) = term(4) + t2(a,e,i,i) * tvvoo(a, e, k, k)
term(5) = term(5) + t2(a,e,k,i) * tvoov(a, i, k, e)
term(6) = term(6) + t2(a,e,i,i) * tvoov(a, k, k, e)
term(7) = term(7) + t2(a,e,i,k) * tvvoo(a, e, k, i)
term(8) = term(8) + t2(a,e,k,i) * tvvoo(a, e, k, i)
end do 

term(0) = -term(0) 
term(1) = term(1) * 0.49999999999999983d+0 
term(2) = term(2) * 0.49999999999999983d+0 
term(5) = term(5) * (-0.49999999999999983d+0) 
term(6) = term(6) * (-0.49999999999999983d+0) 
term(7) = term(7) * (-0.49999999999999983d+0) 
term(8) = term(8) * (-0.49999999999999983d+0) 

do m = 1, nocc 
term(9) = term(9) + t2(a,b,m,i) * tvoov(a, i, m, b)
term(10) = term(10) + t2(a,b,i,m) * tvoov(a, i, m, b)
term(11) = term(11) + t2(a,a,i,m) * tvoov(b, i, m, b)
term(12) = term(12) + t2(a,a,i,m) * tvvoo(b, b, m, i)
term(13) = term(13) + t2(a,b,i,m) * tvvoo(a, b, m, i)
term(14) = term(14) + t2(a,b,m,i) * tvvoo(a, b, m, i)
term(15) = term(15) + t2(a,a,i,m) * toooo(m, i, k, k)
term(16) = term(16) + t2(a,a,i,m) * toooo(m, k, k, i)
term(17) = term(17) + t2(a,a,k,m) * toooo(m, i, k, i)
end do 

term(9) = term(9) * (-0.49999999999999983d+0) 
term(11) = term(11) * (-0.49999999999999983d+0) 
term(13) = term(13) * (-0.49999999999999983d+0) 
term(14) = term(14) * (-0.49999999999999983d+0) 
term(15) = -term(15) 
term(16) = term(16) * 0.49999999999999983d+0 
term(17) = term(17) * 0.49999999999999983d+0 


    eom_cc3_31_triplet_trans_aibiakbk = 0.d+0
    do s = 0, 17
    eom_cc3_31_triplet_trans_aibiakbk = eom_cc3_31_triplet_trans_aibiakbk + term(s) * 0.125d+0
    end do

    end function eom_cc3_31_triplet_trans_aibiakbk
    end module eom_cc3_31_triplet_trans
    
