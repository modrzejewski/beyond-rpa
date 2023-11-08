module v0_tmxi

    use cc3_intermediates
    use s_gen
    use basis
    
    implicit none
    !
    ! File generated automatcally on 2012-08-27 13:52:54
    !
    contains
    
    function v0_tmxi_aibjci(Obs, t2,nocc, nactive, a,i,b,j,c) 
    double precision :: v0_tmxi_aibjci
    double precision, dimension(:,:) :: Obs
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,b,j,c 
    integer :: s ,d,l 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t3(nocc, nactive, a,b,d,i,j,i) * Obs(c, d)
term(1) = term(1) + t3(nocc, nactive, a,b,d,i,i,j) * Obs(c, d)
term(2) = term(2) + t3(nocc, nactive, a,c,d,i,j,i) * Obs(b, d)
term(3) = term(3) + t3(nocc, nactive, a,c,d,i,i,j) * Obs(b, d)
term(4) = term(4) + t3(nocc, nactive, b,c,d,j,i,i) * Obs(a, d)
term(5) = term(5) + t3(nocc, nactive, b,c,d,i,j,i) * Obs(a, d)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(5) = -term(5) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(a,d,i,i) * t2(b,c,j,l) * Obs(d, l)
term(7) = term(7) + t2(a,d,i,j) * t2(b,c,i,l) * Obs(d, l)
term(8) = term(8) + t2(a,b,i,l) * t2(c,d,i,j) * Obs(d, l)
term(9) = term(9) + t2(a,c,i,l) * t2(b,d,i,j) * Obs(d, l)
term(10) = term(10) + t2(a,c,i,l) * t2(b,d,j,i) * Obs(d, l)
term(11) = term(11) + t2(a,b,i,l) * t2(c,d,j,i) * Obs(d, l)
end do 
end do 

term(6) = -term(6) 
term(8) = -term(8) 
term(10) = -term(10) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(12) = term(12) + t2(a,d,i,i) * t2(b,c,l,j) * Obs(d, l)
term(13) = term(13) + t2(a,d,i,j) * t2(b,c,l,i) * Obs(d, l)
term(14) = term(14) + t2(a,b,l,j) * t2(c,d,i,i) * Obs(d, l)
term(15) = term(15) + t2(a,c,l,i) * t2(b,d,j,i) * Obs(d, l)
term(16) = term(16) + t2(a,c,l,j) * t2(b,d,i,i) * Obs(d, l)
term(17) = term(17) + t2(a,b,l,i) * t2(c,d,j,i) * Obs(d, l)
end do 
end do 

term(13) = -term(13) 
term(14) = -term(14) 
term(15) = -term(15) 

do l = 1, nocc 
term(18) = term(18) + t3(nocc, nactive, a,b,c,i,j,l) * Obs(i, l)
term(19) = term(19) + t3(nocc, nactive, a,b,c,i,i,l) * Obs(j, l)
term(20) = term(20) + t3(nocc, nactive, a,b,c,i,l,j) * Obs(i, l)
term(21) = term(21) + t3(nocc, nactive, a,b,c,i,l,i) * Obs(j, l)
term(22) = term(22) + t3(nocc, nactive, a,b,c,l,j,i) * Obs(i, l)
term(23) = term(23) + t3(nocc, nactive, a,b,c,l,i,j) * Obs(i, l)
end do 

term(18) = -term(18) 
term(21) = -term(21) 
term(22) = -term(22) 


    v0_tmxi_aibjci = 0.d+0 
    do s = 0, 23
    v0_tmxi_aibjci = v0_tmxi_aibjci + term(s)
    end do

    end function v0_tmxi_aibjci
    
    function v0_tmxi_aibjcj(Obs, t2,nocc, nactive, a,i,b,j,c) 
    double precision :: v0_tmxi_aibjcj
    double precision, dimension(:,:) :: Obs
integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,b,j,c 
    integer :: s ,d,l 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t3(nocc, nactive, a,b,d,j,j,i) * Obs(c, d)
term(1) = term(1) + t3(nocc, nactive, a,b,d,i,j,j) * Obs(c, d)
term(2) = term(2) + t3(nocc, nactive, a,c,d,i,j,j) * Obs(b, d)
term(3) = term(3) + t3(nocc, nactive, a,c,d,j,i,j) * Obs(b, d)
term(4) = term(4) + t3(nocc, nactive, b,c,d,j,i,j) * Obs(a, d)
term(5) = term(5) + t3(nocc, nactive, b,c,d,j,j,i) * Obs(a, d)
end do 

term(0) = -term(0) 
term(3) = -term(3) 
term(4) = -term(4) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(a,d,j,i) * t2(b,c,j,l) * Obs(d, l)
term(7) = term(7) + t2(a,d,i,j) * t2(b,c,j,l) * Obs(d, l)
term(8) = term(8) + t2(a,b,j,l) * t2(c,d,i,j) * Obs(d, l)
term(9) = term(9) + t2(a,c,i,l) * t2(b,d,j,j) * Obs(d, l)
term(10) = term(10) + t2(a,c,j,l) * t2(b,d,j,i) * Obs(d, l)
term(11) = term(11) + t2(a,b,i,l) * t2(c,d,j,j) * Obs(d, l)
end do 
end do 

term(7) = -term(7) 
term(9) = -term(9) 
term(11) = -term(11) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(12) = term(12) + t2(a,d,i,j) * t2(b,c,l,j) * Obs(d, l)
term(13) = term(13) + t2(a,d,j,j) * t2(b,c,l,i) * Obs(d, l)
term(14) = term(14) + t2(a,b,l,j) * t2(c,d,i,j) * Obs(d, l)
term(15) = term(15) + t2(a,c,l,i) * t2(b,d,j,j) * Obs(d, l)
term(16) = term(16) + t2(a,c,l,j) * t2(b,d,j,i) * Obs(d, l)
term(17) = term(17) + t2(a,b,l,j) * t2(c,d,j,i) * Obs(d, l)
end do 
end do 

term(12) = -term(12) 
term(16) = -term(16) 
term(17) = -term(17) 

do l = 1, nocc 
term(18) = term(18) + t3(nocc, nactive, a,b,c,j,j,l) * Obs(i, l)
term(19) = term(19) + t3(nocc, nactive, a,b,c,i,j,l) * Obs(j, l)
term(20) = term(20) + t3(nocc, nactive, a,b,c,i,l,j) * Obs(j, l)
term(21) = term(21) + t3(nocc, nactive, a,b,c,j,l,i) * Obs(j, l)
term(22) = term(22) + t3(nocc, nactive, a,b,c,l,j,i) * Obs(j, l)
term(23) = term(23) + t3(nocc, nactive, a,b,c,l,j,j) * Obs(i, l)
end do 

term(19) = -term(19) 
term(20) = -term(20) 
term(23) = -term(23) 


    v0_tmxi_aibjcj = 0.d+0 
    do s = 0, 23
    v0_tmxi_aibjcj = v0_tmxi_aibjcj + term(s)
    end do

    end function v0_tmxi_aibjcj
    
    function v0_tmxi_aiajcj(Obs, t2,nocc, nactive, a,i,j,c) 
    double precision :: v0_tmxi_aiajcj
    double precision, dimension(:,:) :: Obs
integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,j,c 
    integer :: s ,l,d 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do l = 1, nocc 
do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,c,j,l) * t2(a,d,j,i) * Obs(d, l)
term(1) = term(1) + t2(a,d,i,j) * t2(a,c,j,l) * Obs(d, l)
term(2) = term(2) + t2(a,a,j,l) * t2(c,d,i,j) * Obs(d, l)
term(3) = term(3) + t2(a,c,i,l) * t2(a,d,j,j) * Obs(d, l)
term(4) = term(4) + t2(a,a,i,l) * t2(c,d,j,j) * Obs(d, l)
term(5) = term(5) + t2(a,a,j,l) * t2(c,d,j,i) * Obs(d, l)
end do 
end do 

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 
term(4) = -term(4) 
term(5) = -term(5) 

do d = nocc + 1, nactive 
term(6) = term(6) + t3(nocc, nactive, a,a,d,j,j,i) * Obs(c, d)
term(7) = term(7) + t3(nocc, nactive, a,a,d,i,j,j) * Obs(c, d)
term(8) = term(8) + t3(nocc, nactive, a,c,d,i,j,j) * Obs(a, d)
term(9) = term(9) + t3(nocc, nactive, a,c,d,j,i,j) * Obs(a, d)
term(10) = term(10) + t3(nocc, nactive, a,c,d,j,j,i) * Obs(a, d)
end do 

term(6) = -term(6) 
term(9) = term(9) * (-2.0d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(11) = term(11) + t2(a,c,l,j) * t2(a,d,i,j) * Obs(d, l)
term(12) = term(12) + t2(a,c,l,i) * t2(a,d,j,j) * Obs(d, l)
term(13) = term(13) + t2(a,c,l,j) * t2(a,d,j,i) * Obs(d, l)
end do 
end do 

term(11) = -term(11) 
term(12) = term(12) * 2.0d+0 
term(13) = -term(13) 

do l = 1, nocc 
term(14) = term(14) + t3(nocc, nactive, a,a,c,j,j,l) * Obs(i, l)
term(15) = term(15) + t3(nocc, nactive, a,a,c,i,j,l) * Obs(j, l)
term(16) = term(16) + t3(nocc, nactive, a,a,c,i,l,j) * Obs(j, l)
term(17) = term(17) + t3(nocc, nactive, a,a,c,j,l,i) * Obs(j, l)
term(18) = term(18) + t3(nocc, nactive, a,a,c,j,l,j) * Obs(i, l)
end do 

term(15) = -term(15) 
term(16) = -term(16) 
term(17) = term(17) * 2.0d+0 
term(18) = -term(18) 


    v0_tmxi_aiajcj = 0.d+0 
    do s = 0, 18
    v0_tmxi_aiajcj = v0_tmxi_aiajcj + term(s)
    end do

    end function v0_tmxi_aiajcj
    
    function v0_tmxi_aibibk(Obs, t2, nocc, nactive, a,i,b,k) 
    double precision :: v0_tmxi_aibibk
    double precision, dimension(:,:) :: Obs
integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,b,k 
    integer :: s ,l,d 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do l = 1, nocc 
do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,k,i) * t2(b,b,i,l) * Obs(d, l)
term(1) = term(1) + t2(a,d,i,i) * t2(b,b,k,l) * Obs(d, l)
term(2) = term(2) + t2(a,d,i,k) * t2(b,b,i,l) * Obs(d, l)
term(3) = term(3) + t2(a,b,k,l) * t2(b,d,i,i) * Obs(d, l)
term(4) = term(4) + t2(a,b,i,l) * t2(b,d,k,i) * Obs(d, l)
term(5) = term(5) + t2(a,b,i,l) * t2(b,d,i,k) * Obs(d, l)
end do 
end do 

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 

do d = nocc + 1, nactive 
term(6) = term(6) + t3(nocc, nactive, a,b,d,k,i,i) * Obs(b, d)
term(7) = term(7) + t3(nocc, nactive, a,b,d,i,k,i) * Obs(b, d)
term(8) = term(8) + t3(nocc, nactive, a,b,d,i,i,k) * Obs(b, d)
term(9) = term(9) + t3(nocc, nactive, b,b,d,i,i,k) * Obs(a, d)
term(10) = term(10) + t3(nocc, nactive, b,b,d,i,k,i) * Obs(a, d)
end do 

term(6) = term(6) * (-2.0d+0) 
term(9) = -term(9) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(11) = term(11) + t2(a,b,l,i) * t2(b,d,i,k) * Obs(d, l)
term(12) = term(12) + t2(a,b,l,i) * t2(b,d,k,i) * Obs(d, l)
term(13) = term(13) + t2(a,b,l,k) * t2(b,d,i,i) * Obs(d, l)
end do 
end do 

term(11) = term(11) * 2.0d+0 
term(12) = -term(12) 
term(13) = -term(13) 

do l = 1, nocc 
term(14) = term(14) + t3(nocc, nactive, a,b,b,k,i,l) * Obs(i, l)
term(15) = term(15) + t3(nocc, nactive, a,b,b,i,k,l) * Obs(i, l)
term(16) = term(16) + t3(nocc, nactive, a,b,b,i,i,l) * Obs(k, l)
term(17) = term(17) + t3(nocc, nactive, a,b,b,l,i,i) * Obs(k, l)
term(18) = term(18) + t3(nocc, nactive, a,b,b,l,i,k) * Obs(i, l)
end do 

term(14) = term(14) * 2.0d+0 
term(15) = -term(15) 
term(16) = -term(16) 
term(18) = -term(18) 


    v0_tmxi_aibibk = 0.d+0 
    do s = 0, 18
    v0_tmxi_aibibk = v0_tmxi_aibibk + term(s)
    end do

    end function v0_tmxi_aibibk
    
    end module v0_tmxi
    
