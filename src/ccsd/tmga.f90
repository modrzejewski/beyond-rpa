module tmga

    use cc3_intermediates
    use s_gen
    use basis
    
    implicit none
    !
    ! File generated automatically on 2012-08-26 22:13:37
    !
    contains
    
    function ganu1_ai(Obs, t2, t1, s2, s1, nocc, nactive,  a,i) 
    double precision :: ganu1_ai
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,i 
    integer :: s ,j,k,c,d,b,l 
    double precision, dimension(0:42) :: term 
    term = 0.d+0 
    do j = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(0) = term(0) + t2(b,c,k,j) * t3(nocc, nactive, a,c,d,k,i,j) * Obs(b, d)
term(1) = term(1) + t2(b,d,j,k) * t3(nocc, nactive, a,c,d,k,i,j) * Obs(b, c)
term(2) = term(2) + t2(b,c,j,k) * t3(nocc, nactive, a,c,d,k,i,j) * Obs(b, d)
term(3) = term(3) + t2(b,d,k,j) * t3(nocc, nactive, a,c,d,k,i,j) * Obs(b, c)
term(4) = term(4) + t2(b,d,j,k) * t3(nocc, nactive, a,c,d,k,i,j) * Obs(c, b)
term(5) = term(5) + t2(b,c,k,j) * t3(nocc, nactive, a,c,d,k,i,j) * Obs(d, b)
term(6) = term(6) + t2(b,d,k,j) * t3(nocc, nactive, a,c,d,k,i,j) * Obs(c, b)
term(7) = term(7) + t2(b,c,j,k) * t3(nocc, nactive, a,c,d,k,i,j) * Obs(d, b)
end do 
end do 
end do 
end do 
end do 

term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(8) = term(8) + t2(b,c,j,k) * t3(nocc, nactive, a,b,d,i,k,j) * Obs(c, d)
term(9) = term(9) + t2(b,c,j,k) * t3(nocc, nactive, a,b,d,i,k,j) * Obs(d, c)
end do 
end do 
end do 
end do 
end do 

term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * (-2.0d+0) 

do b = nocc + 1, nactive 
term(10) = term(10) + s1(b,i) * Obs(a, b)
end do 

term(10) = term(10) * 2.0d+0 

do j = 1, nocc 
term(11) = term(11) + s1(a,j) * Obs(i, j)
end do 

term(11) = term(11) * (-2.0d+0) 

term(12) = term(12) + Obs(a, i)

term(12) = term(12) * 2.0d+0 

do k = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do d = nocc + 1, nactive 
term(13) = term(13) + t2(b,c,j,k) * t3(nocc, nactive, a,b,d,i,j,k) * Obs(c, d)
term(14) = term(14) + t2(b,c,j,k) * t3(nocc, nactive, a,b,d,i,j,k) * Obs(d, c)
end do 
end do 
end do 
end do 
end do 

term(13) = term(13) * 4.0d+0 
term(14) = term(14) * 4.0d+0 

do k = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
term(15) = term(15) + t2(b,c,j,l) * t3(nocc, nactive, a,b,c,k,l,i) * Obs(k, j)
term(16) = term(16) + t2(b,c,j,l) * t3(nocc, nactive, a,b,c,k,j,i) * Obs(k, l)
end do 
end do 
end do 
end do 
end do 

term(15) = -term(15) 
term(16) = term(16) * 3.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do l = 1, nocc 
term(17) = term(17) + t2(b,c,j,k) * t3(nocc, nactive, a,b,c,i,l,j) * Obs(l, k)
end do 
end do 
end do 
end do 
end do 

term(17) = term(17) * 2.0d+0 

do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do l = 1, nocc 
term(18) = term(18) + t2(b,c,k,j) * t3(nocc, nactive, a,b,c,k,l,i) * Obs(l, j)
term(19) = term(19) + t2(b,c,l,j) * t3(nocc, nactive, a,b,c,k,j,i) * Obs(k, l)
term(20) = term(20) + t2(b,c,l,j) * t3(nocc, nactive, a,b,c,k,l,i) * Obs(k, j)
end do 
end do 
end do 
end do 
end do 

term(18) = term(18) * (-2.0d+0) 
term(19) = -term(19) 

do k = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do l = 1, nocc 
do j = 1, nocc 
term(21) = term(21) + t2(b,c,j,k) * t3(nocc, nactive, a,b,c,k,l,i) * Obs(l, j)
term(22) = term(22) + t2(b,c,j,k) * t3(nocc, nactive, a,b,c,i,l,k) * Obs(l, j)
end do 
end do 
end do 
end do 
end do 

term(21) = term(21) * 4.0d+0 
term(22) = term(22) * (-4.0d+0) 

do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
term(23) = term(23) + t2(b,c,j,k) * t3(nocc, nactive, a,b,c,i,k,l) * Obs(l, j)
term(24) = term(24) + t2(b,c,j,k) * t3(nocc, nactive, a,b,c,i,j,l) * Obs(l, k)
end do 
end do 
end do 
end do 
end do 

term(23) = term(23) * 2.0d+0 
term(24) = term(24) * (-4.0d+0) 

do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
term(25) = term(25) + s2(a,b,k,i) * t1(b,j) * Obs(j, k)
end do 
end do 
end do 

term(25) = term(25) * 2.0d+0 

do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
term(26) = term(26) + s2(a,b,i,k) * t1(b,j) * Obs(j, k)
term(27) = term(27) + s2(a,b,i,k) * t1(b,j) * Obs(k, j)
end do 
end do 
end do 

term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * (-2.0d+0) 

do b = nocc + 1, nactive 
do j = 1, nocc 
term(28) = term(28) + s2(a,b,j,i) * Obs(b, j)
end do 
end do 

term(28) = term(28) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
term(29) = term(29) + s2(a,b,i,j) * Obs(b, j)
end do 
end do 

term(29) = term(29) * 4.0d+0 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
term(30) = term(30) + s2(a,b,k,j) * t2(b,c,k,j) * Obs(i, c)
end do 
end do 
end do 
end do 

term(30) = term(30) * 2.0d+0 

do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(31) = term(31) + s2(a,c,k,i) * t2(b,c,j,k) * Obs(b, j)
term(32) = term(32) + s2(a,c,i,k) * t2(b,c,j,k) * Obs(b, j)
term(33) = term(33) + s2(b,c,j,i) * t2(b,c,j,k) * Obs(a, k)
end do 
end do 
end do 
end do 

term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * 8.0d+0 
term(33) = term(33) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(34) = term(34) + s2(a,b,k,j) * t2(b,c,j,k) * Obs(i, c)
term(35) = term(35) + s2(b,c,i,j) * t2(b,c,j,k) * Obs(a, k)
term(36) = term(36) + s2(a,c,i,k) * t2(b,c,k,j) * Obs(b, j)
end do 
end do 
end do 
end do 

term(34) = term(34) * (-4.0d+0) 
term(35) = term(35) * 1.5d+0 
term(36) = term(36) * (-4.0d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(37) = term(37) + s2(a,c,i,j) * t1(b,j) * Obs(b, c)
term(38) = term(38) + s2(a,c,i,j) * t1(b,j) * Obs(c, b)
end do 
end do 
end do 

term(37) = term(37) * 2.0d+0 
term(38) = term(38) * 2.0d+0 

do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(39) = term(39) + s2(b,c,j,i) * t2(b,c,k,j) * Obs(a, k)
term(40) = term(40) + s2(b,c,i,j) * t2(b,c,k,j) * Obs(a, k)
term(41) = term(41) + s2(a,c,k,i) * t2(b,c,k,j) * Obs(b, j)
end do 
end do 
end do 
end do 

term(39) = term(39) * 0.5d+0 
term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * 2.0d+0 

do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(42) = term(42) + s2(a,c,j,i) * t1(b,j) * Obs(b, c)
end do 
end do 
end do 

term(42) = term(42) * (-2.0d+0) 


    ganu1_ai = 0.d+0 
    do s = 0, 42
    ganu1_ai = ganu1_ai + term(s)
    end do

    end function ganu1_ai
    
    function ganu2_aibj(Obs, s2, s1, nocc, nactive,  a,i,b,j) 
    double precision :: ganu2_aibj
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,i,b,j 
    integer :: s ,c,k 
    double precision, dimension(0:17) :: term 
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

do k = 1, nocc 
do c = nocc + 1, nactive 
term(12) = term(12) + t3(nocc, nactive, a,b,c,k,i,j) * Obs(c, k)
term(13) = term(13) + t3(nocc, nactive, a,b,c,k,j,i) * Obs(c, k)
term(14) = term(14) + t3(nocc, nactive, a,b,c,j,k,i) * Obs(c, k)
term(15) = term(15) + t3(nocc, nactive, a,b,c,i,k,j) * Obs(c, k)
term(16) = term(16) + t3(nocc, nactive, a,b,c,j,i,k) * Obs(c, k)
term(17) = term(17) + t3(nocc, nactive, a,b,c,i,j,k) * Obs(c, k)
end do 
end do 

term(12) = term(12) * 2.0d+0 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * 2.0d+0 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (-4.0d+0) 
term(17) = term(17) * 8.0d+0 


    ganu2_aibj = 0.d+0 
    do s = 0, 17
    ganu2_aibj = ganu2_aibj + term(s)
    end do

    end function ganu2_aibj
    
    function ganu2_aibi(Obs, s2, s1, nocc, nactive,  a,i,b) 
    double precision :: ganu2_aibi
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,i,b 
    integer :: s ,c,j 
    double precision, dimension(0:8) :: term 
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

do j = 1, nocc 
do c = nocc + 1, nactive 
term(6) = term(6) + t3(nocc, nactive, a,b,c,j,i,i) * Obs(c, j)
term(7) = term(7) + t3(nocc, nactive, a,b,c,i,j,i) * Obs(c, j)
term(8) = term(8) + t3(nocc, nactive, a,b,c,i,i,j) * Obs(c, j)
end do 
end do 

term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * 4.0d+0 


    ganu2_aibi = 0.d+0 
    do s = 0, 8
    ganu2_aibi = ganu2_aibi + term(s)
    end do

    end function ganu2_aibi
    
    function ganu2_aiaj(Obs, s2, s1, nocc, nactive,  a,i,j) 
    double precision :: ganu2_aiaj
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,i,j 
    integer :: s ,k,b 
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + s1(a,i) * Obs(a, j)
term(1) = term(1) + s1(a,j) * Obs(a, i)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * 2.0d+0 

do k = 1, nocc 
do b = nocc + 1, nactive 
term(2) = term(2) + t3(nocc, nactive, a,a,b,i,k,j) * Obs(b, k)
term(3) = term(3) + t3(nocc, nactive, a,a,b,j,k,i) * Obs(b, k)
term(4) = term(4) + t3(nocc, nactive, a,a,b,i,j,k) * Obs(b, k)
end do 
end do 

term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * 4.0d+0 

do b = nocc + 1, nactive 
term(5) = term(5) + s2(a,b,i,j) * Obs(a, b)
term(6) = term(6) + s2(a,b,j,i) * Obs(a, b)
end do 

term(5) = term(5) * 2.0d+0 
term(6) = term(6) * 2.0d+0 

do k = 1, nocc 
term(7) = term(7) + s2(a,a,i,k) * Obs(j, k)
term(8) = term(8) + s2(a,a,j,k) * Obs(i, k)
end do 

term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (-2.0d+0) 


    ganu2_aiaj = 0.d+0 
    do s = 0, 8
    ganu2_aiaj = ganu2_aiaj + term(s)
    end do

    end function ganu2_aiaj
    
    function ganu2_aiai(Obs, s2, s1, nocc, nactive,  a,i) 
    double precision :: ganu2_aiai
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,i 
    integer :: s ,b,j 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    do b = nocc + 1, nactive 
term(0) = term(0) + s2(a,b,i,i) * Obs(a, b)
end do 

term(0) = term(0) * 4.0d+0 

do j = 1, nocc 
term(1) = term(1) + s2(a,a,i,j) * Obs(i, j)
end do 

term(1) = term(1) * (-4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
term(2) = term(2) + t3(nocc, nactive, a,a,b,i,j,i) * Obs(b, j)
term(3) = term(3) + t3(nocc, nactive, a,a,b,i,i,j) * Obs(b, j)
end do 
end do 

term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 4.0d+0 

term(4) = term(4) + s1(a,i) * Obs(a, i)

term(4) = term(4) * 4.0d+0 


    ganu2_aiai = 0.d+0 
    do s = 0, 4
    ganu2_aiai = ganu2_aiai + term(s)
    end do

    end function ganu2_aiai
    
    function ganu3_aiajck(Obs, s2, nocc, nactive,  a,i,j,c,k) 
    double precision :: ganu3_aiajck
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,j,c,k 
    integer :: s  
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,a,i,k) * Obs(c, j)
term(1) = term(1) + s2(a,a,i,j) * Obs(c, k)
term(2) = term(2) + s2(a,c,i,j) * Obs(a, k)
term(3) = term(3) + s2(a,c,i,k) * Obs(a, j)
term(4) = term(4) + s2(a,c,j,i) * Obs(a, k)
term(5) = term(5) + s2(a,c,j,k) * Obs(a, i)
term(6) = term(6) + s2(a,a,j,k) * Obs(c, i)
term(7) = term(7) + s2(a,c,k,i) * Obs(a, j)
term(8) = term(8) + s2(a,c,k,j) * Obs(a, i)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 4.0d+0 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 4.0d+0 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (-2.0d+0) 


    ganu3_aiajck = 0.d+0 
    do s = 0, 8
    ganu3_aiajck = ganu3_aiajck + term(s)
    end do

    end function ganu3_aiajck
    
    function ganu3_aibjak(Obs, s2, nocc, nactive, a,i,b,j,k) 
    double precision :: ganu3_aibjak
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,b,j,k 
    integer :: s  
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,b,i,k) * Obs(a, j)
term(1) = term(1) + s2(a,b,i,j) * Obs(a, k)
term(2) = term(2) + s2(a,a,i,j) * Obs(b, k)
term(3) = term(3) + s2(a,a,i,k) * Obs(b, j)
term(4) = term(4) + s2(a,b,j,i) * Obs(a, k)
term(5) = term(5) + s2(a,b,k,i) * Obs(a, j)
term(6) = term(6) + s2(a,b,k,j) * Obs(a, i)
term(7) = term(7) + s2(a,a,j,k) * Obs(b, i)
term(8) = term(8) + s2(a,b,j,k) * Obs(a, i)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 4.0d+0 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 4.0d+0 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * 4.0d+0 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (-2.0d+0) 


    ganu3_aibjak = 0.d+0 
    do s = 0, 8
    ganu3_aibjak = ganu3_aibjak + term(s)
    end do

    end function ganu3_aibjak
    
    function ganu3_aibjbk(Obs, s2, nocc, nactive,  a,i,b,j,k) 
    double precision :: ganu3_aibjbk
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,b,j,k 
    integer :: s  
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,b,i,k) * Obs(b, j)
term(1) = term(1) + s2(a,b,i,j) * Obs(b, k)
term(2) = term(2) + s2(b,b,i,j) * Obs(a, k)
term(3) = term(3) + s2(b,b,i,k) * Obs(a, j)
term(4) = term(4) + s2(b,b,j,k) * Obs(a, i)
term(5) = term(5) + s2(a,b,k,i) * Obs(b, j)
term(6) = term(6) + s2(a,b,k,j) * Obs(b, i)
term(7) = term(7) + s2(a,b,j,i) * Obs(b, k)
term(8) = term(8) + s2(a,b,j,k) * Obs(b, i)

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * 4.0d+0 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * 4.0d+0 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (-2.0d+0) 


    ganu3_aibjbk = 0.d+0 
    do s = 0, 8
    ganu3_aibjbk = ganu3_aibjbk + term(s)
    end do

    end function ganu3_aibjbk
    
    function ganu3_aibjci(Obs, s2, nocc, nactive,  a,i,b,j,c) 
    double precision :: ganu3_aibjci
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,b,j,c 
    integer :: s  
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,b,i,i) * Obs(c, j)
term(1) = term(1) + s2(a,b,i,j) * Obs(c, i)
term(2) = term(2) + s2(a,c,i,j) * Obs(b, i)
term(3) = term(3) + s2(a,c,i,i) * Obs(b, j)
term(4) = term(4) + s2(b,c,i,j) * Obs(a, i)
term(5) = term(5) + s2(b,c,j,i) * Obs(a, i)
term(6) = term(6) + s2(b,c,i,i) * Obs(a, j)
term(7) = term(7) + s2(a,b,j,i) * Obs(c, i)
term(8) = term(8) + s2(a,c,j,i) * Obs(b, i)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 4.0d+0 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 4.0d+0 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (-2.0d+0) 


    ganu3_aibjci = 0.d+0 
    do s = 0, 8
    ganu3_aibjci = ganu3_aibjci + term(s)
    end do

    end function ganu3_aibjci
    
    function ganu3_aibick(Obs, s2, nocc, nactive,  a,i,b,c,k) 
    double precision :: ganu3_aibick
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,b,c,k 
    integer :: s  
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,b,i,k) * Obs(c, i)
term(1) = term(1) + s2(a,b,i,i) * Obs(c, k)
term(2) = term(2) + s2(a,c,i,i) * Obs(b, k)
term(3) = term(3) + s2(a,c,i,k) * Obs(b, i)
term(4) = term(4) + s2(b,c,i,i) * Obs(a, k)
term(5) = term(5) + s2(b,c,i,k) * Obs(a, i)
term(6) = term(6) + s2(a,b,k,i) * Obs(c, i)
term(7) = term(7) + s2(b,c,k,i) * Obs(a, i)
term(8) = term(8) + s2(a,c,k,i) * Obs(b, i)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 4.0d+0 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 4.0d+0 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (-2.0d+0) 


    ganu3_aibick = 0.d+0 
    do s = 0, 8
    ganu3_aibick = ganu3_aibick + term(s)
    end do

    end function ganu3_aibick
    
    function ganu3_aibjcj(Obs, s2, nocc, nactive,  a,i,b,j,c) 
    double precision :: ganu3_aibjcj
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,b,j,c 
    integer :: s  
    double precision, dimension(0:8) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,b,i,j) * Obs(c, j)
term(1) = term(1) + s2(a,c,i,j) * Obs(b, j)
term(2) = term(2) + s2(b,c,i,j) * Obs(a, j)
term(3) = term(3) + s2(b,c,j,i) * Obs(a, j)
term(4) = term(4) + s2(b,c,j,j) * Obs(a, i)
term(5) = term(5) + s2(a,b,j,i) * Obs(c, j)
term(6) = term(6) + s2(a,b,j,j) * Obs(c, i)
term(7) = term(7) + s2(a,c,j,i) * Obs(b, j)
term(8) = term(8) + s2(a,c,j,j) * Obs(b, i)

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * 4.0d+0 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * 4.0d+0 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (-2.0d+0) 


    ganu3_aibjcj = 0.d+0 
    do s = 0, 8
    ganu3_aibjcj = ganu3_aibjcj + term(s)
    end do

    end function ganu3_aibjcj
    
    function ganu3_aiajci(Obs, s2, nocc, nactive,  a,i,j,c) 
    double precision :: ganu3_aiajci
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,j,c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,a,i,i) * Obs(c, j)
term(1) = term(1) + s2(a,a,i,j) * Obs(c, i)
term(2) = term(2) + s2(a,c,i,j) * Obs(a, i)
term(3) = term(3) + s2(a,c,i,i) * Obs(a, j)
term(4) = term(4) + s2(a,c,j,i) * Obs(a, i)

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * 2.0d+0 


    ganu3_aiajci = 0.d+0 
    do s = 0, 4
    ganu3_aiajci = ganu3_aiajci + term(s)
    end do

    end function ganu3_aiajci
    
    function ganu3_aiaick(Obs, s2, nocc, nactive,  a,i,c,k) 
    double precision :: ganu3_aiaick
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,c,k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,a,i,k) * Obs(c, i)
term(1) = term(1) + s2(a,a,i,i) * Obs(c, k)
term(2) = term(2) + s2(a,c,i,i) * Obs(a, k)
term(3) = term(3) + s2(a,c,i,k) * Obs(a, i)
term(4) = term(4) + s2(a,c,k,i) * Obs(a, i)

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * 4.0d+0 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 8.0d+0 
term(4) = term(4) * (-4.0d+0) 


    ganu3_aiaick = 0.d+0 
    do s = 0, 4
    ganu3_aiaick = ganu3_aiaick + term(s)
    end do

    end function ganu3_aiaick
    
    function ganu3_aiajcj(Obs, s2, nocc, nactive,  a,i,j,c) 
    double precision :: ganu3_aiajcj
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,j,c 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,a,i,j) * Obs(c, j)
term(1) = term(1) + s2(a,c,i,j) * Obs(a, j)
term(2) = term(2) + s2(a,c,j,i) * Obs(a, j)
term(3) = term(3) + s2(a,c,j,j) * Obs(a, i)
term(4) = term(4) + s2(a,a,j,j) * Obs(c, i)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * (-2.0d+0) 


    ganu3_aiajcj = 0.d+0 
    do s = 0, 4
    ganu3_aiajcj = ganu3_aiajcj + term(s)
    end do

    end function ganu3_aiajcj
    
    function ganu3_aibjai(Obs, s2, nocc, nactive,  a,i,b,j) 
    double precision :: ganu3_aibjai
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,b,j 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,b,i,i) * Obs(a, j)
term(1) = term(1) + s2(a,b,i,j) * Obs(a, i)
term(2) = term(2) + s2(a,a,i,j) * Obs(b, i)
term(3) = term(3) + s2(a,a,i,i) * Obs(b, j)
term(4) = term(4) + s2(a,b,j,i) * Obs(a, i)

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * 8.0d+0 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 4.0d+0 
term(4) = term(4) * (-4.0d+0) 


    ganu3_aibjai = 0.d+0 
    do s = 0, 4
    ganu3_aibjai = ganu3_aibjai + term(s)
    end do

    end function ganu3_aibjai
    
    function ganu3_aibiak(Obs, s2, nocc, nactive,  a,i,b,k) 
    double precision :: ganu3_aibiak
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,b,k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,b,i,k) * Obs(a, i)
term(1) = term(1) + s2(a,b,i,i) * Obs(a, k)
term(2) = term(2) + s2(a,a,i,i) * Obs(b, k)
term(3) = term(3) + s2(a,a,i,k) * Obs(b, i)
term(4) = term(4) + s2(a,b,k,i) * Obs(a, i)

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * 2.0d+0 


    ganu3_aibiak = 0.d+0 
    do s = 0, 4
    ganu3_aibiak = ganu3_aibiak + term(s)
    end do

    end function ganu3_aibiak
    
    function ganu3_aibjaj(Obs, s2, nocc, nactive,  a,i,b,j) 
    double precision :: ganu3_aibjaj
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,b,j 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,b,i,j) * Obs(a, j)
term(1) = term(1) + s2(a,a,i,j) * Obs(b, j)
term(2) = term(2) + s2(a,b,j,i) * Obs(a, j)
term(3) = term(3) + s2(a,b,j,j) * Obs(a, i)
term(4) = term(4) + s2(a,a,j,j) * Obs(b, i)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * (-2.0d+0) 


    ganu3_aibjaj = 0.d+0 
    do s = 0, 4
    ganu3_aibjaj = ganu3_aibjaj + term(s)
    end do

    end function ganu3_aibjaj
    
    function ganu3_aibjbi(Obs, s2, nocc, nactive,  a,i,b,j) 
    double precision :: ganu3_aibjbi
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,b,j 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,b,i,i) * Obs(b, j)
term(1) = term(1) + s2(a,b,i,j) * Obs(b, i)
term(2) = term(2) + s2(b,b,i,j) * Obs(a, i)
term(3) = term(3) + s2(b,b,i,i) * Obs(a, j)
term(4) = term(4) + s2(a,b,j,i) * Obs(b, i)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-4.0d+0) 


    ganu3_aibjbi = 0.d+0 
    do s = 0, 4
    ganu3_aibjbi = ganu3_aibjbi + term(s)
    end do

    end function ganu3_aibjbi
    
    function ganu3_aibibk(Obs, s2, nocc, nactive,  a,i,b,k) 
    double precision :: ganu3_aibibk
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,b,k 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,b,i,k) * Obs(b, i)
term(1) = term(1) + s2(a,b,i,i) * Obs(b, k)
term(2) = term(2) + s2(b,b,i,i) * Obs(a, k)
term(3) = term(3) + s2(b,b,i,k) * Obs(a, i)
term(4) = term(4) + s2(a,b,k,i) * Obs(b, i)

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * (-4.0d+0) 


    ganu3_aibibk = 0.d+0 
    do s = 0, 4
    ganu3_aibibk = ganu3_aibibk + term(s)
    end do

    end function ganu3_aibibk
    
    function ganu3_aibjbj(Obs, s2, nocc, nactive, a,i,b,j) 
    double precision :: ganu3_aibjbj
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,b,j 
    integer :: s  
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,b,i,j) * Obs(b, j)
term(1) = term(1) + s2(b,b,i,j) * Obs(a, j)
term(2) = term(2) + s2(b,b,j,j) * Obs(a, i)
term(3) = term(3) + s2(a,b,j,i) * Obs(b, j)
term(4) = term(4) + s2(a,b,j,j) * Obs(b, i)

term(0) = term(0) * 8.0d+0 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * 4.0d+0 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (-4.0d+0) 


    ganu3_aibjbj = 0.d+0 
    do s = 0, 4
    ganu3_aibjbj = ganu3_aibjbj + term(s)
    end do

    end function ganu3_aibjbj
    
    function ganu3_aibjck(Obs, s2, nocc, nactive,  a,i,b,j,c,k) 
    double precision :: ganu3_aibjck
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    integer, intent(in) :: a,i,b,j,c,k 
    integer :: s  
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    term(0) = term(0) + s2(a,b,i,k) * Obs(c, j)
term(1) = term(1) + s2(a,b,i,j) * Obs(c, k)
term(2) = term(2) + s2(a,c,i,j) * Obs(b, k)
term(3) = term(3) + s2(a,c,i,k) * Obs(b, j)
term(4) = term(4) + s2(b,c,i,j) * Obs(a, k)
term(5) = term(5) + s2(b,c,j,i) * Obs(a, k)
term(6) = term(6) + s2(b,c,i,k) * Obs(a, j)
term(7) = term(7) + s2(b,c,j,k) * Obs(a, i)
term(8) = term(8) + s2(a,b,k,i) * Obs(c, j)
term(9) = term(9) + s2(a,b,k,j) * Obs(c, i)
term(10) = term(10) + s2(a,b,j,i) * Obs(c, k)
term(11) = term(11) + s2(a,c,j,i) * Obs(b, k)
term(12) = term(12) + s2(a,c,j,k) * Obs(b, i)
term(13) = term(13) + s2(a,b,j,k) * Obs(c, i)
term(14) = term(14) + s2(b,c,k,i) * Obs(a, j)
term(15) = term(15) + s2(b,c,k,j) * Obs(a, i)
term(16) = term(16) + s2(a,c,k,j) * Obs(b, i)
term(17) = term(17) + s2(a,c,k,i) * Obs(b, j)

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * 8.0d+0 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 8.0d+0 
term(4) = term(4) * 2.0d+0 
term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * 8.0d+0 
term(8) = term(8) * 2.0d+0 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * 2.0d+0 
term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * 2.0d+0 
term(14) = term(14) * 2.0d+0 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * 2.0d+0 
term(17) = term(17) * (-4.0d+0) 


    ganu3_aibjck = 0.d+0 
    do s = 0, 17
    ganu3_aibjck = ganu3_aibjck + term(s)
    end do

    end function ganu3_aibjck
    
    end module tmga
    
