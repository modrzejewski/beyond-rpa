module tmxi_ccsd

    use cc3_intermediates
    use s_gen
    use basis
    
    implicit none
    !
    ! File generated automatcally on 2012-10-02 17:09:33
    !
    contains
    
    function xinu_ccsd1_ai(Obs, t2, t1,nocc, nactive, a,i) 
    double precision :: xinu_ccsd1_ai
    double precision, dimension(:,:) :: Obs
   integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    integer, intent(in) :: a,i 
    integer :: s ,b,j 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    do b = nocc + 1, nactive 
term(0) = term(0) + t1(b,i) * Obs(a, b)
end do 


do j = 1, nocc 
term(1) = term(1) + t1(a,j) * Obs(i, j)
end do 

term(1) = -term(1) 

term(2) = term(2) + Obs(a, i)


do j = 1, nocc 
do b = nocc + 1, nactive 
term(3) = term(3) + t2(a,b,i,j) * Obs(b, j)
end do 
end do 

term(3) = term(3) * 2.0d+0 

do b = nocc + 1, nactive 
do j = 1, nocc 
term(4) = term(4) + t2(a,b,j,i) * Obs(b, j)
end do 
end do 

term(4) = -term(4) 


    xinu_ccsd1_ai = 0.d+0 
    do s = 0, 4
    xinu_ccsd1_ai = xinu_ccsd1_ai + term(s)
    end do

    end function xinu_ccsd1_ai
    
    function xinu_ccsd2_aibj(Obs, t2, nocc, nactive, a,i,b,j) 
    double precision :: xinu_ccsd2_aibj
    double precision, dimension(:,:) :: Obs
   integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,b,j 
    integer :: s ,c,k 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do c = nocc + 1, nactive 
term(0) = term(0) + t2(a,c,i,j) * Obs(b, c)
term(1) = term(1) + t2(b,c,j,i) * Obs(a, c)
! if(b.eq.3.and.c.eq.9.or.b.eq.9.and.c.eq.3)then
!       if(abs(t2(a,c,i,j)).gt.1.d-10)print*, a, i, b, j, '+', a, c, i, j, t2(a, c, i, j)
! end if
! if(a.eq.3.and.c.eq.9.or.a.eq.9.and.c.eq.3)then
!       if(abs(t2(b,c,j,i)).gt.1.d-10)print*, a, i, b, j, '+',b, c, j, i, t2(b, c, j, i)
! end if
end do 


do k = 1, nocc 
term(2) = term(2) + t2(a,b,i,k) * Obs(j, k)
term(3) = term(3) + t2(a,b,k,j) * Obs(i, k)
end do 

term(2) = -term(2) 
term(3) = -term(3) 


    xinu_ccsd2_aibj = 0.d+0 
    do s = 0, 3
    xinu_ccsd2_aibj = xinu_ccsd2_aibj + term(s)
    end do

    end function xinu_ccsd2_aibj
    
    function xinu_ccsd2_aibi(Obs, t2, nocc, nactive, a,i,b) 
    double precision :: xinu_ccsd2_aibi
    double precision, dimension(:,:) :: Obs
   integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,b 
    integer :: s ,c,j 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do c = nocc + 1, nactive 
term(0) = term(0) + t2(a,c,i,i) * Obs(b, c)
term(1) = term(1) + t2(b,c,i,i) * Obs(a, c)
! if(b.eq.3.and.c.eq.9.or.b.eq.9.and.c.eq.3)then
!       if(abs(t2(a,c,i,i)).gt.1.d-10)print*, a, i, b, '+',a, c, i, i, t2(a, c, i, i)
! end if
! if(a.eq.3.and.c.eq.9.or.a.eq.9.and.c.eq.3)then
!       if(abs(t2(b,c,i,i)).gt.1.d-10)print*, a, i, b, '+',b, c, i, i, t2(b, c, i, i)
! end if
end do 


do j = 1, nocc 
term(2) = term(2) + t2(a,b,i,j) * Obs(i, j)
term(3) = term(3) + t2(a,b,j,i) * Obs(i, j)
end do 

term(2) = -term(2) 
term(3) = -term(3) 


    xinu_ccsd2_aibi = 0.d+0 
    do s = 0, 3
    xinu_ccsd2_aibi = xinu_ccsd2_aibi + term(s)
    end do

    end function xinu_ccsd2_aibi
    
    function xinu_ccsd2_aiaj(Obs, t2, nocc, nactive, a,i,j) 
    double precision :: xinu_ccsd2_aiaj
    double precision, dimension(:,:) :: Obs
   integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,j 
    integer :: s ,b,k 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do b = nocc + 1, nactive 
term(0) = term(0) + t2(a,b,i,j) * Obs(a, b)
term(1) = term(1) + t2(a,b,j,i) * Obs(a, b)
! if(b.eq.3.and.a.eq.9.or.b.eq.9.and.a.eq.3)then
!       if(abs(t2(a,b,i,j)).gt.1.d-10)print*, a, i, j, '+', a, b, i, j, t2(a, b, i, j)
!       if(abs(t2(a,b,j,i)).gt.1.d-10)print*, a, i, j, '+', a, b, j, i, t2(a, b, j, i)
! end if
end do 


do k = 1, nocc 
term(2) = term(2) + t2(a,a,i,k) * Obs(j, k)
term(3) = term(3) + t2(a,a,j,k) * Obs(i, k)
end do 

term(2) = -term(2) 
term(3) = -term(3) 


    xinu_ccsd2_aiaj = 0.d+0 
    do s = 0, 3
    xinu_ccsd2_aiaj = xinu_ccsd2_aiaj + term(s)
    end do

    end function xinu_ccsd2_aiaj
    
    function xinu_ccsd2_aiai(Obs, t2, nocc, nactive, a,i) 
    double precision :: xinu_ccsd2_aiai
    double precision, dimension(:,:) :: Obs
   integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i 
    integer :: s ,b,j 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do b = nocc + 1, nactive 
term(0) = term(0) + t2(a,b,i,i) * Obs(a, b)
! if(b.eq.3.and.a.eq.9.or.b.eq.9.and.a.eq.3)then
!       if(abs(t2(a,b,i,i)).gt.1.d-10)print*, a, i, '+', a, b, i, i, t2(a, b, i, i)
! end if
end do 


do j = 1, nocc 
term(1) = term(1) + t2(a,a,i,j) * Obs(i, j)
end do 

term(1) = -term(1) 


    xinu_ccsd2_aiai = 0.d+0 
    do s = 0, 1
    xinu_ccsd2_aiai = xinu_ccsd2_aiai + term(s)
    end do

    end function xinu_ccsd2_aiai
    
    end module tmxi_ccsd
    
