module eom_cc3_22_tripletpm_trans
use cc_gparams
    use ccsd_transformed_integrals
    use t1_transformed_int
    use cc3_intermediates_for_21
    use basis
    
    implicit none

    contains
    
    function eom_cc3_22_tripletpm_trans_aibjckal(t2, nocc, nactive, i, b, j, c, k, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjckal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, e, k, c)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, e, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletpm_trans_aibjckal = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjckal = eom_cc3_22_tripletpm_trans_aibjckal + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjckal
    function eom_cc3_22_tripletpm_trans_aibjakdl(t2, nocc, nactive, i, b, j, k, d, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjakdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, d, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, d, k, e)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletpm_trans_aibjakdl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjakdl = eom_cc3_22_tripletpm_trans_aibjakdl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjakdl
    function eom_cc3_22_tripletpm_trans_aibjckbl(t2, nocc, nactive, a, i, j, c, k, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjckbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, e, k, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, e, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletpm_trans_aibjckbl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjckbl = eom_cc3_22_tripletpm_trans_aibjckbl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjckbl
    function eom_cc3_22_tripletpm_trans_aibjbkdl(t2, nocc, nactive, a, i, j, k, d, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbkdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d, l 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, d, k, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(l, d, k, e)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletpm_trans_aibjbkdl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjbkdl = eom_cc3_22_tripletpm_trans_aibjbkdl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbkdl
    function eom_cc3_22_tripletpm_trans_aibjckdj(t2, nocc, nactive, a, i, b, c, k, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjckdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k, d 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, d, k, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletpm_trans_aibjckdj = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjckdj = eom_cc3_22_tripletpm_trans_aibjckdj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjckdj
    function eom_cc3_22_tripletpm_trans_aibjcjdl(t2, nocc, nactive, a, i, b, c, d, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcjdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, d, l 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, d)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletpm_trans_aibjcjdl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjcjdl = eom_cc3_22_tripletpm_trans_aibjcjdl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcjdl
    function eom_cc3_22_tripletpm_trans_aibjckdi(t2, nocc, nactive, a, b, j, c, k, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjckdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k, d 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, d, k, c)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletpm_trans_aibjckdi = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjckdi = eom_cc3_22_tripletpm_trans_aibjckdi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjckdi
    function eom_cc3_22_tripletpm_trans_aibjcidl(t2, nocc, nactive, a, b, j, c, d, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, d, l 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, l, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, l, d)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletpm_trans_aibjcidl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjcidl = eom_cc3_22_tripletpm_trans_aibjcidl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcidl
    function eom_cc3_22_tripletpm_trans_aibjakal(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjakal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, a, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, a, k, e)
term(2) = term(2) + t2(b,e,i,j) * tovov(l, e, k, a)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, k, a)
end do 

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletpm_trans_aibjakal = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjakal = eom_cc3_22_tripletpm_trans_aibjakal + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjakal
    function eom_cc3_22_tripletpm_trans_aibjbkal(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbkal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, a, k, e)
term(1) = term(1) + t2(b,e,i,j) * tovov(l, e, k, b)
term(2) = term(2) + t2(b,e,j,i) * tovov(l, e, k, b)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, a, k, e)
end do 

term(1) = -term(1) 
term(3) = -term(3) 


    eom_cc3_22_tripletpm_trans_aibjbkal = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjbkal = eom_cc3_22_tripletpm_trans_aibjbkal + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbkal
    function eom_cc3_22_tripletpm_trans_aibjckaj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjckaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, c)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, k, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, k, c)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,j) * tovov(k, c, j, e)
term(4) = term(4) + t2(b,e,j,i) * tovov(k, c, j, e)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, c, k, e)
term(6) = term(6) + t2(b,e,m,i) * tovov(m, e, k, c)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 


    eom_cc3_22_tripletpm_trans_aibjckaj = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletpm_trans_aibjckaj = eom_cc3_22_tripletpm_trans_aibjckaj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjckaj
    function eom_cc3_22_tripletpm_trans_aibjcjal(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcjal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, a)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,j) * tovov(l, e, j, c)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, j, c)
end do 

term(2) = -term(2) 


    eom_cc3_22_tripletpm_trans_aibjcjal = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjcjal = eom_cc3_22_tripletpm_trans_aibjcjal + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcjal
    function eom_cc3_22_tripletpm_trans_aibjckai(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjckai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, c)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, k, c)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, k, c)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,j) * tovov(k, c, i, e)
term(4) = term(4) + t2(b,e,j,i) * tovov(k, c, i, e)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, c, k, e)
term(6) = term(6) + t2(b,e,m,j) * tovov(m, e, k, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletpm_trans_aibjckai = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletpm_trans_aibjckai = eom_cc3_22_tripletpm_trans_aibjckai + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjckai
    function eom_cc3_22_tripletpm_trans_aibjckak(t2, nocc, nactive, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjckak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, c, k 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, k, c)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletpm_trans_aibjckak = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjckak = eom_cc3_22_tripletpm_trans_aibjckak + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjckak
    function eom_cc3_22_tripletpm_trans_aibjcial(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, l, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, l, a)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,j) * tovov(l, e, i, c)
term(3) = term(3) + t2(b,e,j,i) * tovov(l, e, i, c)
end do 

term(2) = -term(2) 


    eom_cc3_22_tripletpm_trans_aibjcial = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjcial = eom_cc3_22_tripletpm_trans_aibjcial + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcial
    function eom_cc3_22_tripletpm_trans_aibjakbl(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjakbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(l, b, k, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(l, b, k, e)
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, k, a)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, k, a)
end do 

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletpm_trans_aibjakbl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjakbl = eom_cc3_22_tripletpm_trans_aibjakbl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjakbl
    function eom_cc3_22_tripletpm_trans_aibjakdj(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjakdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, d, k, a)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, k, a)
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,j) * tovov(k, e, j, d)
term(3) = term(3) + t2(b,e,j,i) * tovov(k, e, j, d)
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletpm_trans_aibjakdj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjakdj = eom_cc3_22_tripletpm_trans_aibjakdj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjakdj
    function eom_cc3_22_tripletpm_trans_aibjajdl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjajdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, d)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, l, d)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, l, d)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,j) * tovov(l, d, j, e)
term(4) = term(4) + t2(b,e,j,i) * tovov(l, d, j, e)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, d, l, e)
term(6) = term(6) + t2(b,e,m,i) * tovov(m, e, l, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletpm_trans_aibjajdl = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletpm_trans_aibjajdl = eom_cc3_22_tripletpm_trans_aibjajdl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjajdl
    function eom_cc3_22_tripletpm_trans_aibjakdi(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjakdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, d, k, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, k, a)
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(b,e,i,j) * tovov(k, e, i, d)
term(3) = term(3) + t2(b,e,j,i) * tovov(k, e, i, d)
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletpm_trans_aibjakdi = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjakdi = eom_cc3_22_tripletpm_trans_aibjakdi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjakdi
    function eom_cc3_22_tripletpm_trans_aibjakdk(t2, nocc, nactive, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjakdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: i, b, j, k, d 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, k, d)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, k, d)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletpm_trans_aibjakdk = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjakdk = eom_cc3_22_tripletpm_trans_aibjakdk + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjakdk
    function eom_cc3_22_tripletpm_trans_aibjaidl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjaidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, l, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, l, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, l, d)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,j) * tovov(l, d, i, e)
term(4) = term(4) + t2(b,e,j,i) * tovov(l, d, i, e)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, d, l, e)
term(6) = term(6) + t2(b,e,m,j) * tovov(m, e, l, d)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * 2.0d+0 


    eom_cc3_22_tripletpm_trans_aibjaidl = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletpm_trans_aibjaidl = eom_cc3_22_tripletpm_trans_aibjaidl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjaidl
    function eom_cc3_22_tripletpm_trans_aibjbkbl(t2, nocc, nactive, a, i, b, j, k, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbkbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, l 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(l, b, k, e)
term(1) = term(1) + t2(a,e,j,i) * tovov(l, e, k, b)
term(2) = term(2) + t2(a,e,i,j) * tovov(l, b, k, e)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, k, b)
end do 

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletpm_trans_aibjbkbl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjbkbl = eom_cc3_22_tripletpm_trans_aibjbkbl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbkbl
    function eom_cc3_22_tripletpm_trans_aibjckcj(t2, nocc, nactive, a, i, b, c, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjckcj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, k 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, k, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletpm_trans_aibjckcj = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjckcj = eom_cc3_22_tripletpm_trans_aibjckcj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjckcj
    function eom_cc3_22_tripletpm_trans_aibjcjcl(t2, nocc, nactive, a, i, b, c, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcjcl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, c, l 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, c)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletpm_trans_aibjcjcl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjcjcl = eom_cc3_22_tripletpm_trans_aibjcjcl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcjcl
    function eom_cc3_22_tripletpm_trans_aibjckci(t2, nocc, nactive, a, b, j, c, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjckci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, k 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, k, c)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletpm_trans_aibjckci = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjckci = eom_cc3_22_tripletpm_trans_aibjckci + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjckci
    function eom_cc3_22_tripletpm_trans_aibjcicl(t2, nocc, nactive, a, b, j, c, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcicl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, b, j, c, l 
    integer :: s ,m 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, l, c)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, l, c)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletpm_trans_aibjcicl = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjcicl = eom_cc3_22_tripletpm_trans_aibjcicl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcicl
    function eom_cc3_22_tripletpm_trans_aibjckbj(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjckbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, c)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, k, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, k, c)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(k, c, j, e)
term(4) = term(4) + t2(a,e,i,j) * tovov(k, c, j, e)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, c, k, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, k, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletpm_trans_aibjckbj = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletpm_trans_aibjckbj = eom_cc3_22_tripletpm_trans_aibjckbj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjckbj
    function eom_cc3_22_tripletpm_trans_aibjcjbl(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcjbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, l, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, c, l, b)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, j, c)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, j, c)
end do 

term(2) = -term(2) 


    eom_cc3_22_tripletpm_trans_aibjcjbl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjcjbl = eom_cc3_22_tripletpm_trans_aibjcjbl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcjbl
    function eom_cc3_22_tripletpm_trans_aibjckbi(t2, nocc, nactive, a, i, b, j, c, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjckbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, k 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, k, c)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, k, c)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, k, c)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(k, c, i, e)
term(4) = term(4) + t2(a,e,i,j) * tovov(k, c, i, e)
end do 

term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, c, k, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, e, k, c)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, k, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 


    eom_cc3_22_tripletpm_trans_aibjckbi = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletpm_trans_aibjckbi = eom_cc3_22_tripletpm_trans_aibjckbi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjckbi
    function eom_cc3_22_tripletpm_trans_aibjckbk(t2, nocc, nactive, a, i, j, c, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjckbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, c, k 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, k, c)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, k, c)
end do 

term(0) = -term(0) 


    eom_cc3_22_tripletpm_trans_aibjckbk = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjckbk = eom_cc3_22_tripletpm_trans_aibjckbk + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjckbk
    function eom_cc3_22_tripletpm_trans_aibjcibl(t2, nocc, nactive, a, i, b, j, c, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, l 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, l, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, l, b)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(l, e, i, c)
term(3) = term(3) + t2(a,e,i,j) * tovov(l, e, i, c)
end do 

term(2) = -term(2) 


    eom_cc3_22_tripletpm_trans_aibjcibl = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjcibl = eom_cc3_22_tripletpm_trans_aibjcibl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcibl
    function eom_cc3_22_tripletpm_trans_aibjbkdj(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbkdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, d, k, b)
term(1) = term(1) + t2(a,b,i,m) * tovov(m, d, k, b)
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(k, e, j, d)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, e, j, d)
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletpm_trans_aibjbkdj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjbkdj = eom_cc3_22_tripletpm_trans_aibjbkdj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbkdj
    function eom_cc3_22_tripletpm_trans_aibjbjdl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbjdl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, l, d)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, l, d)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(l, d, j, e)
term(4) = term(4) + t2(a,e,i,j) * tovov(l, d, j, e)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, d, l, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, l, d)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * 2.0d+0 


    eom_cc3_22_tripletpm_trans_aibjbjdl = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletpm_trans_aibjbjdl = eom_cc3_22_tripletpm_trans_aibjbjdl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbjdl
    function eom_cc3_22_tripletpm_trans_aibjbkdi(t2, nocc, nactive, a, i, b, j, k, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbkdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k, d 
    integer :: s ,m,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, d, k, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, d, k, b)
end do 

term(0) = -term(0) 

do e = nocc + 1, nactive 
term(2) = term(2) + t2(a,e,j,i) * tovov(k, e, i, d)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, e, i, d)
end do 

term(3) = -term(3) 


    eom_cc3_22_tripletpm_trans_aibjbkdi = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjbkdi = eom_cc3_22_tripletpm_trans_aibjbkdi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbkdi
    function eom_cc3_22_tripletpm_trans_aibjbkdk(t2, nocc, nactive, a, i, j, k, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbkdk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, j, k, d 
    integer :: s ,e 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, k, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(k, e, k, d)
end do 

term(1) = -term(1) 


    eom_cc3_22_tripletpm_trans_aibjbkdk = 0.d+0
    do s = 0, 1
    eom_cc3_22_tripletpm_trans_aibjbkdk = eom_cc3_22_tripletpm_trans_aibjbkdk + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbkdk
    function eom_cc3_22_tripletpm_trans_aibjbidl(t2, nocc, nactive, a, i, b, j, d, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbidl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d, l 
    integer :: s ,m,e 
    double precision, dimension(0:7) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, l, d)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, l, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, l, d)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(l, d, i, e)
term(4) = term(4) + t2(a,e,i,j) * tovov(l, d, i, e)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, d, l, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, e, l, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, l, d)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 


    eom_cc3_22_tripletpm_trans_aibjbidl = 0.d+0
    do s = 0, 7
    eom_cc3_22_tripletpm_trans_aibjbidl = eom_cc3_22_tripletpm_trans_aibjbidl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbidl
    function eom_cc3_22_tripletpm_trans_aibjcjdj(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcjdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,b,m,i) * tovov(m, d, j, c)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, c, j, d)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, j, c)
end do 

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletpm_trans_aibjcjdj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjcjdj = eom_cc3_22_tripletpm_trans_aibjcjdj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcjdj
    function eom_cc3_22_tripletpm_trans_aibjcidj(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, j, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, j, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, i, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, d, i, c)
end do 

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletpm_trans_aibjcidj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjcidj = eom_cc3_22_tripletpm_trans_aibjcidj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcidj
    function eom_cc3_22_tripletpm_trans_aibjcjdi(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcjdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, d, j, c)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, d, j, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, c, i, d)
end do 

term(1) = -term(1) 
term(3) = -term(3) 


    eom_cc3_22_tripletpm_trans_aibjcjdi = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjcjdi = eom_cc3_22_tripletpm_trans_aibjcjdi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcjdi
    function eom_cc3_22_tripletpm_trans_aibjcidi(t2, nocc, nactive, a, i, b, j, c, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c, d 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, i, d)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, i, d)
term(2) = term(2) + t2(a,b,j,m) * tovov(m, d, i, c)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, d, i, c)
end do 

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletpm_trans_aibjcidi = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjcidi = eom_cc3_22_tripletpm_trans_aibjcidi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcidi
    function eom_cc3_22_tripletpm_trans_aibjakaj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjakaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, a)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, k, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, k, a)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,j) * tovov(k, e, j, a)
term(4) = term(4) + t2(b,e,j,i) * tovov(k, e, j, a)
term(5) = term(5) + t2(b,e,i,j) * tovov(k, a, j, e)
term(6) = term(6) + t2(b,e,j,i) * tovov(k, a, j, e)
end do 

term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,i,m) * tovov(m, a, k, e)
term(9) = term(9) + t2(b,e,m,i) * tovov(m, e, k, a)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjakaj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjakaj = eom_cc3_22_tripletpm_trans_aibjakaj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjakaj
    function eom_cc3_22_tripletpm_trans_aibjajal(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjajal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, a)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, l, a)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, a, l, a)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,j) * tovov(l, a, j, e)
term(4) = term(4) + t2(b,e,j,i) * tovov(l, a, j, e)
term(5) = term(5) + t2(b,e,i,j) * tovov(l, e, j, a)
term(6) = term(6) + t2(b,e,j,i) * tovov(l, e, j, a)
end do 

term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,i,m) * tovov(m, a, l, e)
term(9) = term(9) + t2(b,e,m,i) * tovov(m, e, l, a)
end do 
end do 



    eom_cc3_22_tripletpm_trans_aibjajal = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjajal = eom_cc3_22_tripletpm_trans_aibjajal + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjajal
    function eom_cc3_22_tripletpm_trans_aibjakai(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjakai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, a)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, k, a)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, k, a)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,j) * tovov(k, e, i, a)
term(4) = term(4) + t2(b,e,j,i) * tovov(k, e, i, a)
term(5) = term(5) + t2(b,e,i,j) * tovov(k, a, i, e)
term(6) = term(6) + t2(b,e,j,i) * tovov(k, a, i, e)
end do 

term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,j,m) * tovov(m, e, k, a)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,j,m) * tovov(m, a, k, e)
term(9) = term(9) + t2(b,e,m,j) * tovov(m, e, k, a)
end do 
end do 



    eom_cc3_22_tripletpm_trans_aibjakai = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjakai = eom_cc3_22_tripletpm_trans_aibjakai + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjakai
    function eom_cc3_22_tripletpm_trans_aibjaial(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjaial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, l, a)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, l, a)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, l, a)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,j) * tovov(l, a, i, e)
term(4) = term(4) + t2(b,e,j,i) * tovov(l, a, i, e)
term(5) = term(5) + t2(b,e,i,j) * tovov(l, e, i, a)
term(6) = term(6) + t2(b,e,j,i) * tovov(l, e, i, a)
end do 

term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,j,m) * tovov(m, e, l, a)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,j,m) * tovov(m, a, l, e)
term(9) = term(9) + t2(b,e,m,j) * tovov(m, e, l, a)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjaial = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjaial = eom_cc3_22_tripletpm_trans_aibjaial + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjaial
    function eom_cc3_22_tripletpm_trans_aibjbkaj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbkaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, k, b)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,i,m) * tovov(m, b, k, e)
term(2) = term(2) + t2(b,e,m,i) * tovov(m, e, k, b)
end do 
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, e, k, b)
end do 
end do 

term(3) = term(3) * 2.0d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,i) * tovov(m, a, k, b)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, a, k, b)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,j,i) * tovov(k, e, j, a)
term(7) = term(7) + t2(b,e,i,j) * tovov(k, b, j, e)
term(8) = term(8) + t2(b,e,j,i) * tovov(k, b, j, e)
term(9) = term(9) + t2(a,e,i,j) * tovov(k, e, j, a)
end do 

term(7) = -term(7) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjbkaj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjbkaj = eom_cc3_22_tripletpm_trans_aibjbkaj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbkaj
    function eom_cc3_22_tripletpm_trans_aibjbjal(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbjal 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, a)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, a, l, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, l, a)
end do 
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, l, a)
end do 
end do 

term(3) = term(3) * 2.0d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,i) * tovov(m, b, l, a)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, b, l, a)
end do 

term(5) = -term(5) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,j,i) * tovov(l, a, j, e)
term(7) = term(7) + t2(b,e,i,j) * tovov(l, e, j, b)
term(8) = term(8) + t2(b,e,j,i) * tovov(l, e, j, b)
term(9) = term(9) + t2(a,e,i,j) * tovov(l, a, j, e)
end do 

term(7) = -term(7) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjbjal = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjbjal = eom_cc3_22_tripletpm_trans_aibjbjal + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbjal
    function eom_cc3_22_tripletpm_trans_aibjbkai(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbkai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, k, b)

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, b, k, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, k, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, k, b)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,j,m) * tovov(m, a, k, b)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, a, k, b)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,j,i) * tovov(k, e, i, a)
term(7) = term(7) + t2(b,e,i,j) * tovov(k, b, i, e)
term(8) = term(8) + t2(b,e,j,i) * tovov(k, b, i, e)
term(9) = term(9) + t2(a,e,i,j) * tovov(k, e, i, a)
end do 

term(7) = -term(7) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjbkai = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjbkai = eom_cc3_22_tripletpm_trans_aibjbkai + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbkai
    function eom_cc3_22_tripletpm_trans_aibjbkak(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbkak 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(k, e, k, a)
term(1) = term(1) + t2(b,e,i,j) * tovov(k, e, k, b)
term(2) = term(2) + t2(b,e,j,i) * tovov(k, e, k, b)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, e, k, a)
end do 

term(1) = -term(1) 
term(3) = -term(3) 


    eom_cc3_22_tripletpm_trans_aibjbkak = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjbkak = eom_cc3_22_tripletpm_trans_aibjbkak + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbkak
    function eom_cc3_22_tripletpm_trans_aibjbial(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbial 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, l, a)

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,j,m) * tovov(m, a, l, e)
term(2) = term(2) + t2(a,e,m,j) * tovov(m, e, l, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, e, l, a)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,j,m) * tovov(m, b, l, a)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, b, l, a)
end do 

term(5) = -term(5) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(a,e,j,i) * tovov(l, a, i, e)
term(7) = term(7) + t2(b,e,i,j) * tovov(l, e, i, b)
term(8) = term(8) + t2(b,e,j,i) * tovov(l, e, i, b)
term(9) = term(9) + t2(a,e,i,j) * tovov(l, a, i, e)
end do 

term(7) = -term(7) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjbial = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjbial = eom_cc3_22_tripletpm_trans_aibjbial + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbial
    function eom_cc3_22_tripletpm_trans_aibjcjaj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcjaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, j, c)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, j, a)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, j, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, c, j, a)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, j, c)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,j) * tovov(j, e, j, c)
term(6) = term(6) + t2(b,e,j,i) * tovov(j, e, j, c)
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, j, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,i,m) * tovov(m, c, j, e)
term(9) = term(9) + t2(b,e,m,i) * tovov(m, e, j, c)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjcjaj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjcjaj = eom_cc3_22_tripletpm_trans_aibjcjaj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcjaj
    function eom_cc3_22_tripletpm_trans_aibjciaj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjciaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, j, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, j, a)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, i, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, i, c)
end do 

term(1) = -term(1) 
term(2) = -term(2) 

term(4) = term(4) + tvoov(b, i, i, c)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, c, i, e)
term(6) = term(6) + t2(b,e,m,i) * tovov(m, e, i, c)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,i,j) * tovov(j, e, i, c)
term(9) = term(9) + t2(b,e,j,i) * tovov(j, e, i, c)
end do 

term(8) = -term(8) 


    eom_cc3_22_tripletpm_trans_aibjciaj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjciaj = eom_cc3_22_tripletpm_trans_aibjciaj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjciaj
    function eom_cc3_22_tripletpm_trans_aibjcjai(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, i, a)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, j, c)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, j, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, c, i, a)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

term(4) = term(4) + tvoov(b, j, j, c)

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, c, j, e)
term(6) = term(6) + t2(b,e,m,j) * tovov(m, e, j, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,i,j) * tovov(j, c, i, e)
term(9) = term(9) + t2(b,e,j,i) * tovov(j, c, i, e)
end do 

term(8) = -term(8) 


    eom_cc3_22_tripletpm_trans_aibjcjai = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjcjai = eom_cc3_22_tripletpm_trans_aibjcjai + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcjai
    function eom_cc3_22_tripletpm_trans_aibjciai(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjciai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, i, c)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, i, a)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, c, i, a)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, a, i, c)
term(4) = term(4) + t2(a,b,m,j) * tovov(m, a, i, c)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,j) * tovov(i, e, i, c)
term(6) = term(6) + t2(b,e,j,i) * tovov(i, e, i, c)
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,j,m) * tovov(m, e, i, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,j,m) * tovov(m, c, i, e)
term(9) = term(9) + t2(b,e,m,j) * tovov(m, e, i, c)
end do 
end do 



    eom_cc3_22_tripletpm_trans_aibjciai = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjciai = eom_cc3_22_tripletpm_trans_aibjciai + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjciai
    function eom_cc3_22_tripletpm_trans_aibjakbj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjakbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, a)

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,i,m) * tovov(m, a, k, e)
term(2) = term(2) + t2(a,e,m,i) * tovov(m, e, k, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, e, k, a)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,i) * tovov(m, b, k, a)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, b, k, a)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,i,j) * tovov(k, e, j, b)
term(7) = term(7) + t2(b,e,j,i) * tovov(k, e, j, b)
term(8) = term(8) + t2(a,e,j,i) * tovov(k, a, j, e)
term(9) = term(9) + t2(a,e,i,j) * tovov(k, a, j, e)
end do 

term(7) = -term(7) 
term(8) = -term(8) 


    eom_cc3_22_tripletpm_trans_aibjakbj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjakbj = eom_cc3_22_tripletpm_trans_aibjakbj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjakbj
    function eom_cc3_22_tripletpm_trans_aibjajbl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjajbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, l, b)

term(0) = -term(0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,i,m) * tovov(m, b, l, e)
term(2) = term(2) + t2(b,e,m,i) * tovov(m, e, l, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, e, l, b)
end do 
end do 

term(3) = term(3) * (-2.0d+0) 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,m,i) * tovov(m, a, l, b)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, a, l, b)
end do 

term(5) = -term(5) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,i,j) * tovov(l, b, j, e)
term(7) = term(7) + t2(b,e,j,i) * tovov(l, b, j, e)
term(8) = term(8) + t2(a,e,j,i) * tovov(l, e, j, a)
term(9) = term(9) + t2(a,e,i,j) * tovov(l, e, j, a)
end do 

term(7) = -term(7) 
term(8) = -term(8) 


    eom_cc3_22_tripletpm_trans_aibjajbl = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjajbl = eom_cc3_22_tripletpm_trans_aibjajbl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjajbl
    function eom_cc3_22_tripletpm_trans_aibjakbi(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjakbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, k, a)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(a,e,j,m) * tovov(m, a, k, e)
term(2) = term(2) + t2(a,e,m,j) * tovov(m, e, k, a)
end do 
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, e, k, a)
end do 
end do 

term(3) = term(3) * 2.0d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,j,m) * tovov(m, b, k, a)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, b, k, a)
end do 

term(4) = -term(4) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,i,j) * tovov(k, e, i, b)
term(7) = term(7) + t2(b,e,j,i) * tovov(k, e, i, b)
term(8) = term(8) + t2(a,e,j,i) * tovov(k, a, i, e)
term(9) = term(9) + t2(a,e,i,j) * tovov(k, a, i, e)
end do 

term(7) = -term(7) 
term(8) = -term(8) 


    eom_cc3_22_tripletpm_trans_aibjakbi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjakbi = eom_cc3_22_tripletpm_trans_aibjakbi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjakbi
    function eom_cc3_22_tripletpm_trans_aibjakbk(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjakbk 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,e 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(k, e, k, b)
term(1) = term(1) + t2(b,e,j,i) * tovov(k, e, k, b)
term(2) = term(2) + t2(a,e,j,i) * tovov(k, e, k, a)
term(3) = term(3) + t2(a,e,i,j) * tovov(k, e, k, a)
end do 

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletpm_trans_aibjakbk = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjakbk = eom_cc3_22_tripletpm_trans_aibjakbk + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjakbk
    function eom_cc3_22_tripletpm_trans_aibjaibl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjaibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, l, b)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(1) = term(1) + t2(b,e,j,m) * tovov(m, b, l, e)
term(2) = term(2) + t2(b,e,m,j) * tovov(m, e, l, b)
end do 
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, e, l, b)
end do 
end do 

term(3) = term(3) * 2.0d+0 

do m = 1, nocc 
term(4) = term(4) + t2(a,b,j,m) * tovov(m, a, l, b)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, a, l, b)
end do 

term(5) = -term(5) 

do e = nocc + 1, nactive 
term(6) = term(6) + t2(b,e,i,j) * tovov(l, b, i, e)
term(7) = term(7) + t2(b,e,j,i) * tovov(l, b, i, e)
term(8) = term(8) + t2(a,e,j,i) * tovov(l, e, i, a)
term(9) = term(9) + t2(a,e,i,j) * tovov(l, e, i, a)
end do 

term(7) = -term(7) 
term(8) = -term(8) 


    eom_cc3_22_tripletpm_trans_aibjaibl = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjaibl = eom_cc3_22_tripletpm_trans_aibjaibl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjaibl
    function eom_cc3_22_tripletpm_trans_aibjajdj(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjajdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, j, d)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, a, j, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, j, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, j, d)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, d, j, a)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,j) * tovov(j, e, j, d)
term(6) = term(6) + t2(b,e,j,i) * tovov(j, e, j, d)
end do 

term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,i,m) * tovov(m, e, j, d)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,i,m) * tovov(m, d, j, e)
term(9) = term(9) + t2(b,e,m,i) * tovov(m, e, j, d)
end do 
end do 



    eom_cc3_22_tripletpm_trans_aibjajdj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjajdj = eom_cc3_22_tripletpm_trans_aibjajdj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjajdj
    function eom_cc3_22_tripletpm_trans_aibjaidj(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjaidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(j, d, i, e)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, d, i, e)
end do 

term(1) = -term(1) 

term(2) = term(2) + tvoov(b, j, j, d)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,j,m) * tovov(m, d, j, e)
term(4) = term(4) + t2(b,e,m,j) * tovov(m, e, j, d)
end do 
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(5) = term(5) * 2.0d+0 

do m = 1, nocc 
term(6) = term(6) + t2(a,b,j,m) * tovov(m, a, j, d)
term(7) = term(7) + t2(a,b,m,j) * tovov(m, a, j, d)
term(8) = term(8) + t2(a,b,m,i) * tovov(m, d, i, a)
term(9) = term(9) + t2(a,b,i,m) * tovov(m, d, i, a)
end do 

term(7) = -term(7) 
term(8) = -term(8) 


    eom_cc3_22_tripletpm_trans_aibjaidj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjaidj = eom_cc3_22_tripletpm_trans_aibjaidj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjaidj
    function eom_cc3_22_tripletpm_trans_aibjajdi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjajdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(b,e,i,j) * tovov(j, e, i, d)
term(1) = term(1) + t2(b,e,j,i) * tovov(j, e, i, d)
end do 

term(1) = -term(1) 

term(2) = term(2) + tvoov(b, i, i, d)

term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(b,e,i,m) * tovov(m, d, i, e)
term(4) = term(4) + t2(b,e,m,i) * tovov(m, e, i, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,b,m,i) * tovov(m, a, i, d)
term(7) = term(7) + t2(a,b,j,m) * tovov(m, d, j, a)
term(8) = term(8) + t2(a,b,m,j) * tovov(m, d, j, a)
term(9) = term(9) + t2(a,b,i,m) * tovov(m, a, i, d)
end do 

term(7) = -term(7) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjajdi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjajdi = eom_cc3_22_tripletpm_trans_aibjajdi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjajdi
    function eom_cc3_22_tripletpm_trans_aibjaidi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjaidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, i, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, i, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, i, d)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, d, i, a)
term(4) = term(4) + t2(a,b,m,j) * tovov(m, d, i, a)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(b,e,i,j) * tovov(i, e, i, d)
term(6) = term(6) + t2(b,e,j,i) * tovov(i, e, i, d)
end do 

term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(b,e,j,m) * tovov(m, e, i, d)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(b,e,j,m) * tovov(m, d, i, e)
term(9) = term(9) + t2(b,e,m,j) * tovov(m, e, i, d)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjaidi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjaidi = eom_cc3_22_tripletpm_trans_aibjaidi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjaidi
    function eom_cc3_22_tripletpm_trans_aibjbkbj(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbkbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, k, b)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, k, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, k, b)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(k, e, j, b)
term(4) = term(4) + t2(a,e,j,i) * tovov(k, b, j, e)
term(5) = term(5) + t2(a,e,i,j) * tovov(k, e, j, b)
term(6) = term(6) + t2(a,e,i,j) * tovov(k, b, j, e)
end do 

term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, k, b)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, b, k, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, k, b)
end do 
end do 



    eom_cc3_22_tripletpm_trans_aibjbkbj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjbkbj = eom_cc3_22_tripletpm_trans_aibjbkbj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbkbj
    function eom_cc3_22_tripletpm_trans_aibjbjbl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbjbl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, l, b)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, l, b)
term(2) = term(2) + t2(a,b,i,m) * tovov(m, b, l, b)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(l, b, j, e)
term(4) = term(4) + t2(a,e,j,i) * tovov(l, e, j, b)
term(5) = term(5) + t2(a,e,i,j) * tovov(l, b, j, e)
term(6) = term(6) + t2(a,e,i,j) * tovov(l, e, j, b)
end do 

term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, l, b)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, b, l, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, l, b)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjbjbl = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjbjbl = eom_cc3_22_tripletpm_trans_aibjbjbl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbjbl
    function eom_cc3_22_tripletpm_trans_aibjbkbi(t2, nocc, nactive, a, i, b, j, k) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbkbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, k 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, k, b)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, k, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, k, b)
end do 

term(1) = -term(1) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(k, e, i, b)
term(4) = term(4) + t2(a,e,j,i) * tovov(k, b, i, e)
term(5) = term(5) + t2(a,e,i,j) * tovov(k, e, i, b)
term(6) = term(6) + t2(a,e,i,j) * tovov(k, b, i, e)
end do 

term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, k, b)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,j,m) * tovov(m, b, k, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, e, k, b)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjbkbi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjbkbi = eom_cc3_22_tripletpm_trans_aibjbkbi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbkbi
    function eom_cc3_22_tripletpm_trans_aibjbibl(t2, nocc, nactive, a, i, b, j, l) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbibl 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, l 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, l, b)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, l, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, l, b)
end do 

term(2) = -term(2) 

do e = nocc + 1, nactive 
term(3) = term(3) + t2(a,e,j,i) * tovov(l, b, i, e)
term(4) = term(4) + t2(a,e,j,i) * tovov(l, e, i, b)
term(5) = term(5) + t2(a,e,i,j) * tovov(l, b, i, e)
term(6) = term(6) + t2(a,e,i,j) * tovov(l, e, i, b)
end do 

term(4) = -term(4) 
term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, l, b)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,j,m) * tovov(m, b, l, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, e, l, b)
end do 
end do 



    eom_cc3_22_tripletpm_trans_aibjbibl = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjbibl = eom_cc3_22_tripletpm_trans_aibjbibl + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbibl


    function eom_cc3_22_tripletpm_trans_aibjcicj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcicj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, j, c)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, j, c)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, c, i, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, c, i, c)
end do 

term(1) = -term(1) 
term(2) = -term(2) 


    eom_cc3_22_tripletpm_trans_aibjcicj = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjcicj = eom_cc3_22_tripletpm_trans_aibjcicj + term(s)
    print*, 's - aibjcicj', term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcicj
    function eom_cc3_22_tripletpm_trans_aibjcjci(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcjci 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, i, c)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, j, c)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, c, j, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, c, i, c)
end do 

term(1) = -term(1) 
term(3) = -term(3) 


    eom_cc3_22_tripletpm_trans_aibjcjci = 0.d+0
    do s = 0, 3
    eom_cc3_22_tripletpm_trans_aibjcjci = eom_cc3_22_tripletpm_trans_aibjcjci + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcjci
    function eom_cc3_22_tripletpm_trans_aibjcjbj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcjbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, j, c)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, c, j, b)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, j, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, c, j, b)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, b, j, c)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,i) * tovov(j, e, j, c)
term(6) = term(6) + t2(a,e,i,j) * tovov(j, e, j, c)
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, j, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, c, j, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, j, c)
end do 
end do 



    eom_cc3_22_tripletpm_trans_aibjcjbj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjcjbj = eom_cc3_22_tripletpm_trans_aibjcjbj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcjbj
    function eom_cc3_22_tripletpm_trans_aibjcibj(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, c, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, c, j, b)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, i, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, c)
end do 

term(1) = -term(1) 
term(2) = -term(2) 

term(4) = term(4) + tvoov(a, i, i, c)

term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, c, i, e)
term(6) = term(6) + t2(a,e,m,i) * tovov(m, e, i, c)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, i, c)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,j,i) * tovov(j, e, i, c)
term(9) = term(9) + t2(a,e,i,j) * tovov(j, e, i, c)
end do 

term(8) = -term(8) 


    eom_cc3_22_tripletpm_trans_aibjcibj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjcibj = eom_cc3_22_tripletpm_trans_aibjcibj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcibj
    function eom_cc3_22_tripletpm_trans_aibjcjbi(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcjbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, c, i, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, j, c)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, j, c)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, c, i, b)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

term(4) = term(4) + tvoov(a, j, j, c)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, c, j, e)
term(6) = term(6) + t2(a,e,m,j) * tovov(m, e, j, c)
end do 
end do 

term(5) = -term(5) 
term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, j, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,j,i) * tovov(j, c, i, e)
term(9) = term(9) + t2(a,e,i,j) * tovov(j, c, i, e)
end do 

term(8) = -term(8) 


    eom_cc3_22_tripletpm_trans_aibjcjbi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjcjbi = eom_cc3_22_tripletpm_trans_aibjcjbi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcjbi
    function eom_cc3_22_tripletpm_trans_aibjcibi(t2, nocc, nactive, a, i, b, j, c) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjcibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, c 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, i, c)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,j,m) * tovov(m, c, i, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, c, i, b)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, b, i, c)
term(4) = term(4) + t2(a,b,m,j) * tovov(m, b, i, c)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,i) * tovov(i, e, i, c)
term(6) = term(6) + t2(a,e,i,j) * tovov(i, e, i, c)
end do 

term(5) = -term(5) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, i, c)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,j,m) * tovov(m, c, i, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, e, i, c)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjcibi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjcibi = eom_cc3_22_tripletpm_trans_aibjcibi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjcibi
    function eom_cc3_22_tripletpm_trans_aibjbjdj(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbjdj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, i, j, d)


do m = 1, nocc 
term(1) = term(1) + t2(a,b,m,i) * tovov(m, b, j, d)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, d, j, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, j, d)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, d, j, b)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,i) * tovov(j, e, j, d)
term(6) = term(6) + t2(a,e,i,j) * tovov(j, e, j, d)
end do 

term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,i,m) * tovov(m, e, j, d)
end do 
end do 

term(7) = term(7) * 2.0d+0 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,i,m) * tovov(m, d, j, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, j, d)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjbjdj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjbjdj = eom_cc3_22_tripletpm_trans_aibjbjdj + term(s)
    print*, 'plusz58', s, term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbjdj
    function eom_cc3_22_tripletpm_trans_aibjbidj(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbidj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, d, i, e)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, d, i, e)
end do 

term(1) = -term(1) 

term(2) = term(2) + tvoov(a, j, j, d)

term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,j,m) * tovov(m, d, j, e)
term(4) = term(4) + t2(a,e,m,j) * tovov(m, e, j, d)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,m) * tovov(m, e, j, d)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 

do m = 1, nocc 
term(6) = term(6) + t2(a,b,j,m) * tovov(m, b, j, d)
term(7) = term(7) + t2(a,b,m,j) * tovov(m, b, j, d)
term(8) = term(8) + t2(a,b,m,i) * tovov(m, d, i, b)
term(9) = term(9) + t2(a,b,i,m) * tovov(m, d, i, b)
end do 

term(7) = -term(7) 
term(8) = -term(8) 


    eom_cc3_22_tripletpm_trans_aibjbidj = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjbidj = eom_cc3_22_tripletpm_trans_aibjbidj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbidj
    function eom_cc3_22_tripletpm_trans_aibjbjdi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbjdi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,e,m 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do e = nocc + 1, nactive 
term(0) = term(0) + t2(a,e,j,i) * tovov(j, e, i, d)
term(1) = term(1) + t2(a,e,i,j) * tovov(j, e, i, d)
end do 

term(1) = -term(1) 

term(2) = term(2) + tvoov(a, i, i, d)


do e = nocc + 1, nactive 
do m = 1, nocc 
term(3) = term(3) + t2(a,e,i,m) * tovov(m, d, i, e)
term(4) = term(4) + t2(a,e,m,i) * tovov(m, e, i, d)
end do 
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,i,m) * tovov(m, e, i, d)
end do 
end do 

term(5) = term(5) * 2.0d+0 

do m = 1, nocc 
term(6) = term(6) + t2(a,b,m,i) * tovov(m, b, i, d)
term(7) = term(7) + t2(a,b,j,m) * tovov(m, d, j, b)
term(8) = term(8) + t2(a,b,m,j) * tovov(m, d, j, b)
term(9) = term(9) + t2(a,b,i,m) * tovov(m, b, i, d)
end do 

term(7) = -term(7) 
term(9) = -term(9) 


    eom_cc3_22_tripletpm_trans_aibjbjdi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjbjdi = eom_cc3_22_tripletpm_trans_aibjbjdi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbjdi
    function eom_cc3_22_tripletpm_trans_aibjbidi(t2, nocc, nactive, a, i, b, j, d) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbidi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j, d 
    integer :: s ,m,e 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, i, d)

term(0) = -term(0) 

do m = 1, nocc 
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, i, d)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, i, d)
term(3) = term(3) + t2(a,b,j,m) * tovov(m, d, i, b)
term(4) = term(4) + t2(a,b,m,j) * tovov(m, d, i, b)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(5) = term(5) + t2(a,e,j,i) * tovov(i, e, i, d)
term(6) = term(6) + t2(a,e,i,j) * tovov(i, e, i, d)
end do 

term(6) = -term(6) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(7) = term(7) + t2(a,e,j,m) * tovov(m, e, i, d)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(8) = term(8) + t2(a,e,j,m) * tovov(m, d, i, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, e, i, d)
end do 
end do 



    eom_cc3_22_tripletpm_trans_aibjbidi = 0.d+0
    do s = 0, 9
    eom_cc3_22_tripletpm_trans_aibjbidi = eom_cc3_22_tripletpm_trans_aibjbidi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbidi
    function eom_cc3_22_tripletpm_trans_aibjaiaj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjaiaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, a, j, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, j, a)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, i, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, i, a)
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,j,m) * tovov(m, a, j, e)
term(5) = term(5) + t2(b,e,m,j) * tovov(m, e, j, a)
term(6) = term(6) + t2(b,e,i,m) * tovov(m, a, i, e)
term(7) = term(7) + t2(b,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = -term(7) 

do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,i,j) * tovov(j, a, i, e)
term(9) = term(9) + t2(b,e,j,i) * tovov(j, a, i, e)
term(10) = term(10) + t2(b,e,i,j) * tovov(j, e, i, a)
term(11) = term(11) + t2(b,e,j,i) * tovov(j, e, i, a)
end do 

term(9) = -term(9) 
term(10) = -term(10) 

term(12) = term(12) + tvoov(b, j, j, a)
term(13) = term(13) + tvoov(b, i, i, a)


do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(b,e,j,m) * tovov(m, e, j, a)
term(15) = term(15) + t2(b,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(14) = term(14) * 2.0d+0 
term(15) = term(15) * 2.0d+0 


    eom_cc3_22_tripletpm_trans_aibjaiaj = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletpm_trans_aibjaiaj = eom_cc3_22_tripletpm_trans_aibjaiaj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjaiaj
    function eom_cc3_22_tripletpm_trans_aibjajai(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjajai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, i, a)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, j, a)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, j, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, i, a)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(b,e,i,m) * tovov(m, a, i, e)
term(5) = term(5) + t2(b,e,m,i) * tovov(m, e, i, a)
term(6) = term(6) + t2(b,e,j,m) * tovov(m, a, j, e)
term(7) = term(7) + t2(b,e,m,j) * tovov(m, e, j, a)
end do 
end do 


do e = nocc + 1, nactive 
term(8) = term(8) + t2(b,e,i,j) * tovov(j, e, i, a)
term(9) = term(9) + t2(b,e,j,i) * tovov(j, e, i, a)
term(10) = term(10) + t2(b,e,i,j) * tovov(j, a, i, e)
term(11) = term(11) + t2(b,e,j,i) * tovov(j, a, i, e)
end do 

term(9) = -term(9) 
term(10) = -term(10) 

term(12) = term(12) + tvoov(b, i, i, a)
term(13) = term(13) + tvoov(b, j, j, a)

term(12) = -term(12) 
term(13) = -term(13) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(b,e,i,m) * tovov(m, e, i, a)
term(15) = term(15) + t2(b,e,j,m) * tovov(m, e, j, a)
end do 
end do 

term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (-2.0d+0) 


    eom_cc3_22_tripletpm_trans_aibjajai = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletpm_trans_aibjajai = eom_cc3_22_tripletpm_trans_aibjajai + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjajai
    function eom_cc3_22_tripletpm_trans_aibjbjaj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbjaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, j, b)
term(1) = term(1) + tvoov(a, i, j, a)


do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, j, a)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, a, j, b)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, b, j, a)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, a, j, b)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, b, j, e)
term(7) = term(7) + t2(b,e,m,i) * tovov(m, e, j, b)
term(8) = term(8) + t2(a,e,i,m) * tovov(m, a, j, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, j, a)
end do 
end do 

term(6) = -term(6) 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = -term(9) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,m) * tovov(m, e, j, b)
term(11) = term(11) + t2(a,e,i,m) * tovov(m, e, j, a)
end do 
end do 

term(10) = term(10) * 2.0d+0 
term(11) = term(11) * 2.0d+0 

do e = nocc + 1, nactive 
term(12) = term(12) + t2(a,e,j,i) * tovov(j, e, j, a)
term(13) = term(13) + t2(b,e,i,j) * tovov(j, e, j, b)
term(14) = term(14) + t2(b,e,j,i) * tovov(j, e, j, b)
term(15) = term(15) + t2(a,e,i,j) * tovov(j, e, j, a)
end do 

term(13) = -term(13) 
term(15) = -term(15) 


    eom_cc3_22_tripletpm_trans_aibjbjaj = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletpm_trans_aibjbjaj = eom_cc3_22_tripletpm_trans_aibjbjaj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbjaj
    function eom_cc3_22_tripletpm_trans_aibjbiaj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbiaj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, b, j, a)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, a)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, i, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, i, b)
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(a,e,j,i) * tovov(j, a, i, e)
term(5) = term(5) + t2(b,e,i,j) * tovov(j, e, i, b)
term(6) = term(6) + t2(b,e,j,i) * tovov(j, e, i, b)
term(7) = term(7) + t2(a,e,i,j) * tovov(j, a, i, e)
end do 

term(5) = -term(5) 
term(7) = -term(7) 

term(8) = term(8) + tvoov(a, j, j, a)
term(9) = term(9) + tvoov(b, i, i, b)

term(8) = -term(8) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(a,e,j,m) * tovov(m, a, j, e)
term(11) = term(11) + t2(a,e,m,j) * tovov(m, e, j, a)
term(12) = term(12) + t2(b,e,i,m) * tovov(m, b, i, e)
term(13) = term(13) + t2(b,e,m,i) * tovov(m, e, i, b)
end do 
end do 

term(12) = -term(12) 
term(13) = -term(13) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(a,e,j,m) * tovov(m, e, j, a)
term(15) = term(15) + t2(b,e,i,m) * tovov(m, e, i, b)
end do 
end do 

term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * 2.0d+0 


    eom_cc3_22_tripletpm_trans_aibjbiaj = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletpm_trans_aibjbiaj = eom_cc3_22_tripletpm_trans_aibjbiaj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbiaj
    function eom_cc3_22_tripletpm_trans_aibjbjai(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbjai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, i, a)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, a, j, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, a, j, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, a)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(a,e,j,i) * tovov(j, e, i, a)
term(5) = term(5) + t2(b,e,i,j) * tovov(j, b, i, e)
term(6) = term(6) + t2(b,e,j,i) * tovov(j, b, i, e)
term(7) = term(7) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 

term(5) = -term(5) 
term(7) = -term(7) 

term(8) = term(8) + tvoov(b, j, j, b)
term(9) = term(9) + tvoov(a, i, i, a)

term(8) = -term(8) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(b,e,j,m) * tovov(m, b, j, e)
term(11) = term(11) + t2(b,e,m,j) * tovov(m, e, j, b)
term(12) = term(12) + t2(a,e,i,m) * tovov(m, a, i, e)
term(13) = term(13) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(12) = -term(12) 
term(13) = -term(13) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(b,e,j,m) * tovov(m, e, j, b)
term(15) = term(15) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * 2.0d+0 


    eom_cc3_22_tripletpm_trans_aibjbjai = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletpm_trans_aibjbjai = eom_cc3_22_tripletpm_trans_aibjbjai + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbjai
    function eom_cc3_22_tripletpm_trans_aibjbiai(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbiai 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(a, j, i, a)
term(1) = term(1) + tvoov(b, j, i, b)

term(0) = -term(0) 
term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,j,m) * tovov(m, b, i, a)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, b, i, a)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, a, i, b)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, a, i, b)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(a,e,j,m) * tovov(m, a, i, e)
term(7) = term(7) + t2(a,e,m,j) * tovov(m, e, i, a)
term(8) = term(8) + t2(b,e,j,m) * tovov(m, b, i, e)
term(9) = term(9) + t2(b,e,m,j) * tovov(m, e, i, b)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(a,e,j,m) * tovov(m, e, i, a)
term(11) = term(11) + t2(b,e,j,m) * tovov(m, e, i, b)
end do 
end do 

term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(12) = term(12) + t2(a,e,j,i) * tovov(i, e, i, a)
term(13) = term(13) + t2(b,e,i,j) * tovov(i, e, i, b)
term(14) = term(14) + t2(b,e,j,i) * tovov(i, e, i, b)
term(15) = term(15) + t2(a,e,i,j) * tovov(i, e, i, a)
end do 

term(13) = -term(13) 
term(15) = -term(15) 


    eom_cc3_22_tripletpm_trans_aibjbiai = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletpm_trans_aibjbiai = eom_cc3_22_tripletpm_trans_aibjbiai + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbiai
    function eom_cc3_22_tripletpm_trans_aibjajbj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjajbj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, i, j, b)
term(1) = term(1) + tvoov(a, i, j, a)

term(0) = -term(0) 
term(1) = -term(1) 

do m = 1, nocc 
term(2) = term(2) + t2(a,b,m,i) * tovov(m, a, j, b)
term(3) = term(3) + t2(a,b,m,i) * tovov(m, b, j, a)
term(4) = term(4) + t2(a,b,i,m) * tovov(m, a, j, b)
term(5) = term(5) + t2(a,b,i,m) * tovov(m, b, j, a)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,i,m) * tovov(m, b, j, e)
term(7) = term(7) + t2(b,e,m,i) * tovov(m, e, j, b)
term(8) = term(8) + t2(a,e,i,m) * tovov(m, a, j, e)
term(9) = term(9) + t2(a,e,m,i) * tovov(m, e, j, a)
end do 
end do 


do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,i,m) * tovov(m, e, j, b)
term(11) = term(11) + t2(a,e,i,m) * tovov(m, e, j, a)
end do 
end do 

term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (-2.0d+0) 

do e = nocc + 1, nactive 
term(12) = term(12) + t2(b,e,i,j) * tovov(j, e, j, b)
term(13) = term(13) + t2(b,e,j,i) * tovov(j, e, j, b)
term(14) = term(14) + t2(a,e,j,i) * tovov(j, e, j, a)
term(15) = term(15) + t2(a,e,i,j) * tovov(j, e, j, a)
end do 

term(13) = -term(13) 
term(14) = -term(14) 


    eom_cc3_22_tripletpm_trans_aibjajbj = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletpm_trans_aibjajbj = eom_cc3_22_tripletpm_trans_aibjajbj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjajbj
    function eom_cc3_22_tripletpm_trans_aibjaibj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjaibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, a, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, a, j, b)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, i, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, a)
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(b,e,i,j) * tovov(j, b, i, e)
term(5) = term(5) + t2(b,e,j,i) * tovov(j, b, i, e)
term(6) = term(6) + t2(a,e,j,i) * tovov(j, e, i, a)
term(7) = term(7) + t2(a,e,i,j) * tovov(j, e, i, a)
end do 

term(5) = -term(5) 
term(6) = -term(6) 

term(8) = term(8) + tvoov(b, j, j, b)
term(9) = term(9) + tvoov(a, i, i, a)

term(9) = -term(9) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(b,e,j,m) * tovov(m, b, j, e)
term(11) = term(11) + t2(b,e,m,j) * tovov(m, e, j, b)
term(12) = term(12) + t2(a,e,i,m) * tovov(m, a, i, e)
term(13) = term(13) + t2(a,e,m,i) * tovov(m, e, i, a)
end do 
end do 

term(10) = -term(10) 
term(11) = -term(11) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(b,e,j,m) * tovov(m, e, j, b)
term(15) = term(15) + t2(a,e,i,m) * tovov(m, e, i, a)
end do 
end do 

term(14) = term(14) * 2.0d+0 
term(15) = term(15) * (-2.0d+0) 


    eom_cc3_22_tripletpm_trans_aibjaibj = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletpm_trans_aibjaibj = eom_cc3_22_tripletpm_trans_aibjaibj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjaibj
    function eom_cc3_22_tripletpm_trans_aibjajbi(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjajbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, a, i, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, j, a)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, j, a)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, a, i, b)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
term(4) = term(4) + t2(b,e,i,j) * tovov(j, e, i, b)
term(5) = term(5) + t2(b,e,j,i) * tovov(j, e, i, b)
term(6) = term(6) + t2(a,e,j,i) * tovov(j, a, i, e)
term(7) = term(7) + t2(a,e,i,j) * tovov(j, a, i, e)
end do 

term(5) = -term(5) 
term(6) = -term(6) 

term(8) = term(8) + tvoov(b, i, i, b)
term(9) = term(9) + tvoov(a, j, j, a)

term(8) = -term(8) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(10) = term(10) + t2(b,e,i,m) * tovov(m, b, i, e)
term(11) = term(11) + t2(b,e,m,i) * tovov(m, e, i, b)
term(12) = term(12) + t2(a,e,j,m) * tovov(m, a, j, e)
term(13) = term(13) + t2(a,e,m,j) * tovov(m, e, j, a)
end do 
end do 

term(12) = -term(12) 
term(13) = -term(13) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(b,e,i,m) * tovov(m, e, i, b)
term(15) = term(15) + t2(a,e,j,m) * tovov(m, e, j, a)
end do 
end do 

term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * 2.0d+0 


    eom_cc3_22_tripletpm_trans_aibjajbi = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletpm_trans_aibjajbi = eom_cc3_22_tripletpm_trans_aibjajbi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjajbi
    function eom_cc3_22_tripletpm_trans_aibjaibi(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjaibi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,e,m 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    term(0) = term(0) + tvoov(b, j, i, b)
term(1) = term(1) + tvoov(a, j, i, a)


do m = 1, nocc 
term(2) = term(2) + t2(a,b,j,m) * tovov(m, a, i, b)
term(3) = term(3) + t2(a,b,m,j) * tovov(m, a, i, b)
term(4) = term(4) + t2(a,b,j,m) * tovov(m, b, i, a)
term(5) = term(5) + t2(a,b,m,j) * tovov(m, b, i, a)
end do 

term(3) = -term(3) 
term(4) = -term(4) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(6) = term(6) + t2(b,e,j,m) * tovov(m, b, i, e)
term(7) = term(7) + t2(b,e,m,j) * tovov(m, e, i, b)
term(8) = term(8) + t2(a,e,j,m) * tovov(m, a, i, e)
term(9) = term(9) + t2(a,e,m,j) * tovov(m, e, i, a)
end do 
end do 

term(6) = -term(6) 
term(7) = -term(7) 
term(8) = -term(8) 
term(9) = -term(9) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(10) = term(10) + t2(b,e,j,m) * tovov(m, e, i, b)
term(11) = term(11) + t2(a,e,j,m) * tovov(m, e, i, a)
end do 
end do 

term(10) = term(10) * 2.0d+0 
term(11) = term(11) * 2.0d+0 

do e = nocc + 1, nactive 
term(12) = term(12) + t2(b,e,i,j) * tovov(i, e, i, b)
term(13) = term(13) + t2(b,e,j,i) * tovov(i, e, i, b)
term(14) = term(14) + t2(a,e,j,i) * tovov(i, e, i, a)
term(15) = term(15) + t2(a,e,i,j) * tovov(i, e, i, a)
end do 

term(13) = -term(13) 
term(14) = -term(14) 


    eom_cc3_22_tripletpm_trans_aibjaibi = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletpm_trans_aibjaibi = eom_cc3_22_tripletpm_trans_aibjaibi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjaibi
    function eom_cc3_22_tripletpm_trans_aibjbibj(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbibj 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,j,m) * tovov(m, b, j, b)
term(1) = term(1) + t2(a,b,m,j) * tovov(m, b, j, b)
term(2) = term(2) + t2(a,b,m,i) * tovov(m, b, i, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, b)
end do 

term(1) = -term(1) 
term(2) = -term(2) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,j,m) * tovov(m, b, j, e)
term(5) = term(5) + t2(a,e,m,j) * tovov(m, e, j, b)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, b, i, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, i, b)
end do 
end do 


do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,j,i) * tovov(j, b, i, e)
term(9) = term(9) + t2(a,e,j,i) * tovov(j, e, i, b)
term(10) = term(10) + t2(a,e,i,j) * tovov(j, b, i, e)
term(11) = term(11) + t2(a,e,i,j) * tovov(j, e, i, b)
end do 

term(9) = -term(9) 
term(10) = -term(10) 

term(12) = term(12) + tvoov(a, j, j, b)
term(13) = term(13) + tvoov(a, i, i, b)

term(12) = -term(12) 
term(13) = -term(13) 

do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(a,e,j,m) * tovov(m, e, j, b)
term(15) = term(15) + t2(a,e,i,m) * tovov(m, e, i, b)
end do 
end do 

term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (-2.0d+0) 


    eom_cc3_22_tripletpm_trans_aibjbibj = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletpm_trans_aibjbibj = eom_cc3_22_tripletpm_trans_aibjbibj + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbibj
    function eom_cc3_22_tripletpm_trans_aibjbjbi(t2, nocc, nactive, a, i, b, j) 
    double precision :: eom_cc3_22_tripletpm_trans_aibjbjbi 
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a, i, b, j 
    integer :: s ,m,e 
    double precision, dimension(0:15) :: term 
    term = 0.d+0 
    do m = 1, nocc 
term(0) = term(0) + t2(a,b,m,i) * tovov(m, b, i, b)
term(1) = term(1) + t2(a,b,j,m) * tovov(m, b, j, b)
term(2) = term(2) + t2(a,b,m,j) * tovov(m, b, j, b)
term(3) = term(3) + t2(a,b,i,m) * tovov(m, b, i, b)
end do 

term(1) = -term(1) 
term(3) = -term(3) 

do e = nocc + 1, nactive 
do m = 1, nocc 
term(4) = term(4) + t2(a,e,j,m) * tovov(m, b, j, e)
term(5) = term(5) + t2(a,e,m,j) * tovov(m, e, j, b)
term(6) = term(6) + t2(a,e,i,m) * tovov(m, b, i, e)
term(7) = term(7) + t2(a,e,m,i) * tovov(m, e, i, b)
end do 
end do 

term(4) = -term(4) 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = -term(7) 

do e = nocc + 1, nactive 
term(8) = term(8) + t2(a,e,j,i) * tovov(j, e, i, b)
term(9) = term(9) + t2(a,e,j,i) * tovov(j, b, i, e)
term(10) = term(10) + t2(a,e,i,j) * tovov(j, e, i, b)
term(11) = term(11) + t2(a,e,i,j) * tovov(j, b, i, e)
end do 

term(9) = -term(9) 
term(10) = -term(10) 

term(12) = term(12) + tvoov(a, j, j, b)
term(13) = term(13) + tvoov(a, i, i, b)


do m = 1, nocc 
do e = nocc + 1, nactive 
term(14) = term(14) + t2(a,e,j,m) * tovov(m, e, j, b)
term(15) = term(15) + t2(a,e,i,m) * tovov(m, e, i, b)
end do 
end do 

term(14) = term(14) * 2.0d+0 
term(15) = term(15) * 2.0d+0 


    eom_cc3_22_tripletpm_trans_aibjbjbi = 0.d+0
    do s = 0, 15
    eom_cc3_22_tripletpm_trans_aibjbjbi = eom_cc3_22_tripletpm_trans_aibjbjbi + term(s)
    end do

    end function eom_cc3_22_tripletpm_trans_aibjbjbi
    end module eom_cc3_22_tripletpm_trans
    
