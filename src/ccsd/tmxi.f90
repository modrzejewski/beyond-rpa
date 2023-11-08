module tmxi

    use cc3_intermediates
    use s_gen
    use basis
    
    implicit none
    !
    ! File generated automatcally on 2012-08-22 14:35:58
    !
    contains
    
    function xinu1_ai(Obs, t2, t1, nocc, nactive, a,i) 
    double precision :: xinu1_ai
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    integer, intent(in) :: a,i 
    integer :: s ,d,l 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t1(d,i) * Obs(a, d)
end do 


do l = 1, nocc 
term(1) = term(1) + t1(a,l) * Obs(i, l)
end do 

term(1) = -term(1) 

term(2) = term(2) + Obs(a, i)


do l = 1, nocc 
do d = nocc + 1, nactive 
term(3) = term(3) + t2(a,d,i,l) * Obs(d, l)
end do 
end do 

term(3) = term(3) * 2.0d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(4) = term(4) + t2(a,d,l,i) * Obs(d, l)
end do 
end do 

term(4) = -term(4) 


    xinu1_ai = 0.d+0 
    do s = 0, 4
    xinu1_ai = xinu1_ai + term(s)
    end do

    end function xinu1_ai
    
    function xinu2_aibj(Obs, t2,  nocc, nactive, a,i,b,j) 
    double precision :: xinu2_aibj
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,b,j 
    integer :: s ,d,l 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,i,j) * Obs(b, d)
term(1) = term(1) + t2(b,d,j,i) * Obs(a, d)
end do 


do l = 1, nocc 
term(2) = term(2) + t2(a,b,i,l) * Obs(j, l)
term(3) = term(3) + t2(a,b,l,j) * Obs(i, l)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(4) = term(4) + t3(nocc, nactive, a,b,d,i,j,l) * Obs(d, l)
term(5) = term(5) + t3(nocc, nactive, a,b,d,i,l,j) * Obs(d, l)
term(6) = term(6) + t3(nocc, nactive, a,b,d,l,j,i) * Obs(d, l)
end do 
end do 

term(4) = term(4) * 2.0d+0 
term(5) = -term(5) 
term(6) = -term(6) 


    xinu2_aibj = 0.d+0 
    do s = 0, 6
    xinu2_aibj = xinu2_aibj + term(s)
    end do

    end function xinu2_aibj
    
    function xinu2_aibi(Obs, t2,  nocc, nactive, a,i,b) 
    double precision :: xinu2_aibi
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,b 
    integer :: s ,d,l 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,i,i) * Obs(b, d)
term(1) = term(1) + t2(b,d,i,i) * Obs(a, d)
end do 


do l = 1, nocc 
term(2) = term(2) + t2(a,b,i,l) * Obs(i, l)
term(3) = term(3) + t2(a,b,l,i) * Obs(i, l)
end do 

term(2) = -term(2) 
term(3) = -term(3) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(4) = term(4) + t3(nocc, nactive, a,b,d,i,i,l) * Obs(d, l)
term(5) = term(5) + t3(nocc, nactive, a,b,d,i,l,i) * Obs(d, l)
term(6) = term(6) + t3(nocc, nactive, a,b,d,l,i,i) * Obs(d, l)
end do 
end do 

term(4) = term(4) * 2.0d+0 
term(5) = -term(5) 
term(6) = -term(6) 


    xinu2_aibi = 0.d+0 
    do s = 0, 6
    xinu2_aibi = xinu2_aibi + term(s)
    end do

    end function xinu2_aibi
    
    function xinu2_aiaj(Obs, t2, nocc, nactive, a,i,j) 
    double precision :: xinu2_aiaj
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,j 
    integer :: s ,l,d 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    do l = 1, nocc 
do d = nocc + 1, nactive 
term(0) = term(0) + t3(nocc, nactive, a,a,d,i,j,l) * Obs(d, l)
term(1) = term(1) + t3(nocc, nactive, a,a,d,i,l,j) * Obs(d, l)
term(2) = term(2) + t3(nocc, nactive, a,a,d,j,l,i) * Obs(d, l)
end do 
end do 

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 

do d = nocc + 1, nactive 
term(3) = term(3) + t2(a,d,i,j) * Obs(a, d)
term(4) = term(4) + t2(a,d,j,i) * Obs(a, d)
end do 


do l = 1, nocc 
term(5) = term(5) + t2(a,a,i,l) * Obs(j, l)
term(6) = term(6) + t2(a,a,j,l) * Obs(i, l)
end do 

term(5) = -term(5) 
term(6) = -term(6) 


    xinu2_aiaj = 0.d+0 
    do s = 0, 6
    xinu2_aiaj = xinu2_aiaj + term(s)
    end do

    end function xinu2_aiaj
    
    function xinu2_aiai(Obs, t2, nocc, nactive, a,i) 
    double precision :: xinu2_aiai
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i 
    integer :: s ,d,l 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,i,i) * Obs(a, d)
end do 


do l = 1, nocc 
term(1) = term(1) + t2(a,a,i,l) * Obs(i, l)
end do 

term(1) = -term(1) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(2) = term(2) + t3(nocc, nactive, a,a,d,i,i,l) * Obs(d, l)
term(3) = term(3) + t3(nocc, nactive, a,a,d,i,l,i) * Obs(d, l)
end do 
end do 

term(3) = -term(3) 


    xinu2_aiai = 0.d+0 
    do s = 0, 3
    xinu2_aiai = xinu2_aiai + term(s)
    end do

    end function xinu2_aiai
    
    end module tmxi
    
