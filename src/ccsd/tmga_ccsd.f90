module tmga_ccsd

    use cc3_intermediates
    use s_gen
    use basis
    
    implicit none
    !
    ! File generated automatically on 2012-10-02 17:10:21
    !
    contains
    
    function ganu_ccsd1_ai(Obs, t2, t1, s2, s1, nocc, nactive, a,i) 
    double precision :: ganu_ccsd1_ai
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,i 
    integer :: s ,b,j,c,k 
    double precision, dimension(0:20) :: term 
    term = 0.d+0 
    do b = nocc + 1, nactive 
term(0) = term(0) + s1(b,i) * Obs(a, b)
end do 

term(0) = term(0) * 2.0d+0 

do j = 1, nocc 
term(1) = term(1) + s1(a,j) * Obs(i, j)
end do 

term(1) = term(1) * (-2.0d+0) 

term(2) = term(2) + Obs(a, i)

term(2) = term(2) * 2.0d+0 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
term(3) = term(3) + s2(a,b,k,j) * t2(b,c,k,j) * Obs(i, c)
end do 
end do 
end do 
end do 

term(3) = term(3) * 2.0d+0 

do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(4) = term(4) + s2(a,c,k,i) * t2(b,c,j,k) * Obs(b, j)
term(5) = term(5) + s2(a,c,i,k) * t2(b,c,j,k) * Obs(b, j)
term(6) = term(6) + s2(b,c,j,i) * t2(b,c,j,k) * Obs(a, k)
end do 
end do 
end do 
end do 

term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * 8.0d+0 
term(6) = term(6) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(7) = term(7) + s2(a,b,k,j) * t2(b,c,j,k) * Obs(i, c)
term(8) = term(8) + s2(b,c,i,j) * t2(b,c,j,k) * Obs(a, k)
term(9) = term(9) + s2(a,c,i,k) * t2(b,c,k,j) * Obs(b, j)
end do 
end do 
end do 
end do 

term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * 1.5d+0 
term(9) = term(9) * (-4.0d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(10) = term(10) + s2(b,c,j,i) * t2(b,c,k,j) * Obs(a, k)
term(11) = term(11) + s2(b,c,i,j) * t2(b,c,k,j) * Obs(a, k)
term(12) = term(12) + s2(a,c,k,i) * t2(b,c,k,j) * Obs(b, j)
end do 
end do 
end do 
end do 

term(10) = term(10) * 0.5d+0 
term(11) = term(11) * (-2.0d+0) 
term(12) = term(12) * 2.0d+0 

do b = nocc + 1, nactive 
do j = 1, nocc 
term(13) = term(13) + s2(a,b,j,i) * Obs(b, j)
end do 
end do 

term(13) = term(13) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
term(14) = term(14) + s2(a,b,i,j) * Obs(b, j)
end do 
end do 

term(14) = term(14) * 4.0d+0 

do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(15) = term(15) + s2(a,c,j,i) * t1(b,j) * Obs(b, c)
end do 
end do 
end do 

term(15) = term(15) * (-2.0d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(16) = term(16) + s2(a,c,i,j) * t1(b,j) * Obs(b, c)
term(17) = term(17) + s2(a,c,i,j) * t1(b,j) * Obs(c, b)
end do 
end do 
end do 

term(16) = term(16) * 2.0d+0 
term(17) = term(17) * 2.0d+0 

do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
term(18) = term(18) + s2(a,b,k,i) * t1(b,j) * Obs(j, k)
end do 
end do 
end do 

term(18) = term(18) * 2.0d+0 

do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
term(19) = term(19) + s2(a,b,i,k) * t1(b,j) * Obs(j, k)
term(20) = term(20) + s2(a,b,i,k) * t1(b,j) * Obs(k, j)
end do 
end do 
end do 

term(19) = term(19) * (-2.0d+0) 
term(20) = term(20) * (-2.0d+0) 


    ganu_ccsd1_ai = 0.d+0 
    do s = 0, 20
    ganu_ccsd1_ai = ganu_ccsd1_ai + term(s)
    end do

    end function ganu_ccsd1_ai
    
    function ganu_ccsd2_aibj(Obs, s2, s1, nocc, nactive, a,i,b,j) 
    double precision :: ganu_ccsd2_aibj
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,i,b,j 
    integer :: s ,c,k 
    double precision, dimension(0:11) :: term 
    term = 0.d+0 
    term(0) = term(0) + s1(a,i) * Obs(b, j)
term(1) = term(1) + s1(b,i) * Obs(a, j)
term(2) = term(2) + s1(b,j) * Obs(a, i)
term(3) = term(3) + s1(a,j) * Obs(b, i)

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * 4.0d+0 
term(3) = term(3) * (-2.0d+0) 

do k = 1, nocc 
term(4) = term(4) + s2(a,b,k,i) * Obs(j, k)
term(5) = term(5) + s2(a,b,k,j) * Obs(i, k)
term(6) = term(6) + s2(a,b,j,k) * Obs(i, k)
term(7) = term(7) + s2(a,b,i,k) * Obs(j, k)
end do 

term(4) = term(4) * 2.0d+0 
term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * 2.0d+0 
term(7) = term(7) * (-4.0d+0) 

do c = nocc + 1, nactive 
term(8) = term(8) + s2(b,c,i,j) * Obs(a, c)
term(9) = term(9) + s2(b,c,j,i) * Obs(a, c)
term(10) = term(10) + s2(a,c,j,i) * Obs(b, c)
term(11) = term(11) + s2(a,c,i,j) * Obs(b, c)
end do 

term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * 4.0d+0 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * 4.0d+0 


    ganu_ccsd2_aibj = 0.d+0 
    do s = 0, 11
    ganu_ccsd2_aibj = ganu_ccsd2_aibj + term(s)
    end do

    end function ganu_ccsd2_aibj
    
    function ganu_ccsd2_aibi(Obs, s2, s1, nocc, nactive, a,i,b) 
    double precision :: ganu_ccsd2_aibi
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,i,b 
    integer :: s ,c,j 
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    do c = nocc + 1, nactive 
term(0) = term(0) + s2(b,c,i,i) * Obs(a, c)
term(1) = term(1) + s2(a,c,i,i) * Obs(b, c)
end do 

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * 2.0d+0 

do j = 1, nocc 
term(2) = term(2) + s2(a,b,j,i) * Obs(i, j)
term(3) = term(3) + s2(a,b,i,j) * Obs(i, j)
end do 

term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 

term(4) = term(4) + s1(a,i) * Obs(b, i)
term(5) = term(5) + s1(b,i) * Obs(a, i)

term(4) = term(4) * 2.0d+0 
term(5) = term(5) * 2.0d+0 


    ganu_ccsd2_aibi = 0.d+0 
    do s = 0, 5
    ganu_ccsd2_aibi = ganu_ccsd2_aibi + term(s)
    end do

    end function ganu_ccsd2_aibi
    
    function ganu_ccsd2_aiaj(Obs, s2, s1, nocc, nactive, a,i,j) 
    double precision :: ganu_ccsd2_aiaj
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,i,j 
    integer :: s ,b,k 
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    term(0) = term(0) + s1(a,i) * Obs(a, j)
term(1) = term(1) + s1(a,j) * Obs(a, i)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * 2.0d+0 

do b = nocc + 1, nactive 
term(2) = term(2) + s2(a,b,i,j) * Obs(a, b)
term(3) = term(3) + s2(a,b,j,i) * Obs(a, b)
end do 

term(2) = term(2) * 2.0d+0 
term(3) = term(3) * 2.0d+0 

do k = 1, nocc 
term(4) = term(4) + s2(a,a,i,k) * Obs(j, k)
term(5) = term(5) + s2(a,a,j,k) * Obs(i, k)
end do 

term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (-2.0d+0) 


    ganu_ccsd2_aiaj = 0.d+0 
    do s = 0, 5
    ganu_ccsd2_aiaj = ganu_ccsd2_aiaj + term(s)
    end do

    end function ganu_ccsd2_aiaj
    
    function ganu_ccsd2_aiai(Obs, s2, s1, nocc, nactive,  a,i) 
    double precision :: ganu_ccsd2_aiai
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,i 
    integer :: s ,b,j 
    double precision, dimension(0:2) :: term 
    term = 0.d+0 
    do b = nocc + 1, nactive 
term(0) = term(0) + s2(a,b,i,i) * Obs(a, b)
end do 

term(0) = term(0) * 4.0d+0 

do j = 1, nocc 
term(1) = term(1) + s2(a,a,i,j) * Obs(i, j)
end do 

term(1) = term(1) * (-4.0d+0) 

term(2) = term(2) + s1(a,i) * Obs(a, i)

term(2) = term(2) * 4.0d+0 


    ganu_ccsd2_aiai = 0.d+0 
    do s = 0, 2
    ganu_ccsd2_aiai = ganu_ccsd2_aiai + term(s)
    end do
    end function ganu_ccsd2_aiai
    

    
    end module tmga_ccsd
    
