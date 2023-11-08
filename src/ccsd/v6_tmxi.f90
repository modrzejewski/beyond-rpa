module v6_tmxi

    use cc3_intermediates
    use s_gen
    use basis
    
    implicit none
    !
    ! File generated automatcally on 2012-08-27 13:52:54
    !
    contains
    
    function v6_tmxi_aiajck(Obs, t2,nocc, nactive, a,i,j,c,k) 
    double precision :: v6_tmxi_aiajck
    double precision, dimension(:,:) :: Obs
integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,j,c,k 
    integer :: s ,d,l 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t3(nocc, nactive, a,a,d,i,j,k) * Obs(c, d)
term(1) = term(1) + t3(nocc, nactive, a,c,d,j,k,i) * Obs(a, d)
term(2) = term(2) + t3(nocc, nactive, a,a,d,i,k,j) * Obs(c, d)
term(3) = term(3) + t3(nocc, nactive, a,c,d,i,j,k) * Obs(a, d)
term(4) = term(4) + t3(nocc, nactive, a,c,d,i,k,j) * Obs(a, d)
term(5) = term(5) + t3(nocc, nactive, a,c,d,k,j,i) * Obs(a, d)
end do 

term(2) = -term(2) 
term(3) = -term(3) 
term(5) = -term(5) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(a,c,i,l) * t2(a,d,j,k) * Obs(d, l)
term(7) = term(7) + t2(a,d,i,j) * t2(a,c,k,l) * Obs(d, l)
term(8) = term(8) + t2(a,d,i,k) * t2(a,c,j,l) * Obs(d, l)
term(9) = term(9) + t2(a,c,i,l) * t2(a,d,k,j) * Obs(d, l)
term(10) = term(10) + t2(a,a,i,l) * t2(c,d,j,k) * Obs(d, l)
term(11) = term(11) + t2(a,a,j,l) * t2(c,d,k,i) * Obs(d, l)
term(12) = term(12) + t2(a,a,i,l) * t2(c,d,k,j) * Obs(d, l)
term(13) = term(13) + t2(a,a,k,l) * t2(c,d,j,i) * Obs(d, l)
end do 
end do 

term(6) = -term(6) 
term(8) = -term(8) 
term(11) = -term(11) 
term(12) = -term(12) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(14) = term(14) + t2(a,c,l,k) * t2(a,d,j,i) * Obs(d, l)
term(15) = term(15) + t2(a,c,l,j) * t2(a,d,i,k) * Obs(d, l)
term(16) = term(16) + t2(a,c,l,k) * t2(a,d,i,j) * Obs(d, l)
term(17) = term(17) + t2(a,c,l,j) * t2(a,d,k,i) * Obs(d, l)
end do 
end do 

term(14) = -term(14) 
term(16) = -term(16) 

do l = 1, nocc 
term(18) = term(18) + t3(nocc, nactive, a,a,c,i,j,l) * Obs(k, l)
term(19) = term(19) + t3(nocc, nactive, a,a,c,j,l,k) * Obs(i, l)
term(20) = term(20) + t3(nocc, nactive, a,a,c,i,k,l) * Obs(j, l)
term(21) = term(21) + t3(nocc, nactive, a,a,c,i,l,j) * Obs(k, l)
term(22) = term(22) + t3(nocc, nactive, a,a,c,i,l,k) * Obs(j, l)
term(23) = term(23) + t3(nocc, nactive, a,a,c,k,l,j) * Obs(i, l)
end do 

term(18) = -term(18) 
term(19) = -term(19) 
term(22) = -term(22) 


    v6_tmxi_aiajck = 0.d+0 
    do s = 0, 23
    v6_tmxi_aiajck = v6_tmxi_aiajck + term(s)

    end do

    end function v6_tmxi_aiajck
    
    function v6_tmxi_aibjbk(Obs, t2,nocc, nactive, a,i,b,j,k) 
    double precision :: v6_tmxi_aibjbk
    double precision, dimension(:,:) :: Obs
integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,b,j,k 
    integer :: s ,d,l 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t3(nocc, nactive, a,b,d,j,i,k) * Obs(b, d)
term(1) = term(1) + t3(nocc, nactive, a,b,d,j,k,i) * Obs(b, d)
term(2) = term(2) + t3(nocc, nactive, a,b,d,i,k,j) * Obs(b, d)
term(3) = term(3) + t3(nocc, nactive, a,b,d,i,j,k) * Obs(b, d)
term(4) = term(4) + t3(nocc, nactive, b,b,d,i,k,j) * Obs(a, d)
term(5) = term(5) + t3(nocc, nactive, b,b,d,j,k,i) * Obs(a, d)
end do 

term(0) = -term(0) 
term(1) = -term(1) 
term(4) = -term(4) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(6) = term(6) + t2(a,b,l,k) * t2(b,d,i,j) * Obs(d, l)
term(7) = term(7) + t2(a,b,l,i) * t2(b,d,k,j) * Obs(d, l)
term(8) = term(8) + t2(a,b,l,j) * t2(b,d,k,i) * Obs(d, l)
term(9) = term(9) + t2(a,b,l,k) * t2(b,d,j,i) * Obs(d, l)
end do 
end do 

term(8) = -term(8) 
term(9) = -term(9) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(10) = term(10) + t2(a,d,j,k) * t2(b,b,i,l) * Obs(d, l)
term(11) = term(11) + t2(a,d,j,i) * t2(b,b,k,l) * Obs(d, l)
term(12) = term(12) + t2(a,d,i,j) * t2(b,b,k,l) * Obs(d, l)
term(13) = term(13) + t2(a,d,i,k) * t2(b,b,j,l) * Obs(d, l)
term(14) = term(14) + t2(a,b,j,l) * t2(b,d,i,k) * Obs(d, l)
term(15) = term(15) + t2(a,b,i,l) * t2(b,d,k,j) * Obs(d, l)
term(16) = term(16) + t2(a,b,i,l) * t2(b,d,j,k) * Obs(d, l)
term(17) = term(17) + t2(a,b,j,l) * t2(b,d,k,i) * Obs(d, l)
end do 
end do 

term(12) = -term(12) 
term(13) = -term(13) 
term(15) = -term(15) 
term(16) = -term(16) 

do l = 1, nocc 
term(18) = term(18) + t3(nocc, nactive, a,b,b,j,i,l) * Obs(k, l)
term(19) = term(19) + t3(nocc, nactive, a,b,b,j,k,l) * Obs(i, l)
term(20) = term(20) + t3(nocc, nactive, a,b,b,i,k,l) * Obs(j, l)
term(21) = term(21) + t3(nocc, nactive, a,b,b,i,j,l) * Obs(k, l)
term(22) = term(22) + t3(nocc, nactive, a,b,b,l,i,k) * Obs(j, l)
term(23) = term(23) + t3(nocc, nactive, a,b,b,l,j,k) * Obs(i, l)
end do 

term(20) = -term(20) 
term(21) = -term(21) 
term(23) = -term(23) 


    v6_tmxi_aibjbk = 0.d+0 
    do s = 0, 23
    v6_tmxi_aibjbk = v6_tmxi_aibjbk + term(s)

    end do

    end function v6_tmxi_aibjbk
    
    function v6_tmxi_aiajci(Obs, t2,nocc, nactive, a,i,j,c) 
    double precision :: v6_tmxi_aiajci
    double precision, dimension(:,:) :: Obs
integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,j,c 
    integer :: s ,l,d 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do l = 1, nocc 
do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,c,i,l) * t2(a,d,j,i) * Obs(d, l)
term(1) = term(1) + t2(a,c,i,l) * t2(a,d,i,j) * Obs(d, l)
term(2) = term(2) + t2(a,d,i,i) * t2(a,c,j,l) * Obs(d, l)
term(3) = term(3) + t2(a,a,i,l) * t2(c,d,j,i) * Obs(d, l)
term(4) = term(4) + t2(a,a,j,l) * t2(c,d,i,i) * Obs(d, l)
term(5) = term(5) + t2(a,a,i,l) * t2(c,d,i,j) * Obs(d, l)
end do 
end do 

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(6) = term(6) + t2(a,c,l,i) * t2(a,d,j,i) * Obs(d, l)
term(7) = term(7) + t2(a,c,l,j) * t2(a,d,i,i) * Obs(d, l)
term(8) = term(8) + t2(a,c,l,i) * t2(a,d,i,j) * Obs(d, l)
end do 
end do 

term(6) = -term(6) 
term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 

do l = 1, nocc 
term(9) = term(9) + t3(nocc, nactive, a,a,c,i,j,l) * Obs(i, l)
term(10) = term(10) + t3(nocc, nactive, a,a,c,j,l,i) * Obs(i, l)
term(11) = term(11) + t3(nocc, nactive, a,a,c,i,i,l) * Obs(j, l)
term(12) = term(12) + t3(nocc, nactive, a,a,c,i,l,j) * Obs(i, l)
term(13) = term(13) + t3(nocc, nactive, a,a,c,i,l,i) * Obs(j, l)
end do 

term(9) = -term(9) 
term(10) = -term(10) 
term(12) = term(12) * 2.0d+0 
term(13) = -term(13) 

do d = nocc + 1, nactive 
term(14) = term(14) + t3(nocc, nactive, a,a,d,i,j,i) * Obs(c, d)
term(15) = term(15) + t3(nocc, nactive, a,c,d,j,i,i) * Obs(a, d)
term(16) = term(16) + t3(nocc, nactive, a,a,d,i,i,j) * Obs(c, d)
term(17) = term(17) + t3(nocc, nactive, a,c,d,i,j,i) * Obs(a, d)
term(18) = term(18) + t3(nocc, nactive, a,c,d,i,i,j) * Obs(a, d)
end do 

term(16) = -term(16) 
term(17) = term(17) * (-2.0d+0) 


    v6_tmxi_aiajci = 0.d+0 
    do s = 0, 18
    v6_tmxi_aiajci = v6_tmxi_aiajci + term(s)

    end do

    end function v6_tmxi_aiajci
    
    function v6_tmxi_aibjbi(Obs, t2,nocc, nactive, a,i,b,j) 
    double precision :: v6_tmxi_aibjbi
    double precision, dimension(:,:) :: Obs
integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,b,j 
    integer :: s ,l,d 
    double precision, dimension(0:18) :: term 
    term = 0.d+0 
    do l = 1, nocc 
do d = nocc + 1, nactive 
term(0) = term(0) + t2(a,d,j,i) * t2(b,b,i,l) * Obs(d, l)
term(1) = term(1) + t2(a,d,i,j) * t2(b,b,i,l) * Obs(d, l)
term(2) = term(2) + t2(a,d,i,i) * t2(b,b,j,l) * Obs(d, l)
term(3) = term(3) + t2(a,b,j,l) * t2(b,d,i,i) * Obs(d, l)
term(4) = term(4) + t2(a,b,i,l) * t2(b,d,i,j) * Obs(d, l)
term(5) = term(5) + t2(a,b,i,l) * t2(b,d,j,i) * Obs(d, l)
end do 
end do 

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = -term(5) 

do d = nocc + 1, nactive 
term(6) = term(6) + t3(nocc, nactive, a,b,d,j,i,i) * Obs(b, d)
term(7) = term(7) + t3(nocc, nactive, a,b,d,i,i,j) * Obs(b, d)
term(8) = term(8) + t3(nocc, nactive, a,b,d,i,j,i) * Obs(b, d)
term(9) = term(9) + t3(nocc, nactive, b,b,d,i,i,j) * Obs(a, d)
term(10) = term(10) + t3(nocc, nactive, b,b,d,i,j,i) * Obs(a, d)
end do 

term(6) = term(6) * (-2.0d+0) 
term(9) = -term(9) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(11) = term(11) + t2(a,b,l,i) * t2(b,d,i,j) * Obs(d, l)
term(12) = term(12) + t2(a,b,l,j) * t2(b,d,i,i) * Obs(d, l)
term(13) = term(13) + t2(a,b,l,i) * t2(b,d,j,i) * Obs(d, l)
end do 
end do 

term(11) = term(11) * 2.0d+0 
term(12) = -term(12) 
term(13) = -term(13) 

do l = 1, nocc 
term(14) = term(14) + t3(nocc, nactive, a,b,b,j,i,l) * Obs(i, l)
term(15) = term(15) + t3(nocc, nactive, a,b,b,i,i,l) * Obs(j, l)
term(16) = term(16) + t3(nocc, nactive, a,b,b,i,j,l) * Obs(i, l)
term(17) = term(17) + t3(nocc, nactive, a,b,b,l,i,i) * Obs(j, l)
term(18) = term(18) + t3(nocc, nactive, a,b,b,l,i,j) * Obs(i, l)
end do 

term(14) = term(14) * 2.0d+0 
term(15) = -term(15) 
term(16) = -term(16) 
term(18) = -term(18) 


    v6_tmxi_aibjbi = 0.d+0 
    do s = 0, 18
    v6_tmxi_aibjbi = v6_tmxi_aibjbi + term(s)
    end do

    end function v6_tmxi_aibjbi
    
    end module v6_tmxi
    
