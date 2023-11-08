module v3_tmxi

    use cc3_intermediates
    use s_gen
    use basis
    
    implicit none
    !
    ! File generated automatcally on 2012-08-27 13:52:54
    !
    contains
    
    function v3_tmxi_aibjck(Obs, t2,nocc, nactive, a,i,b,j,c,k) 
    double precision :: v3_tmxi_aibjck
    double precision, dimension(:,:) :: Obs
integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    integer, intent(in) :: a,i,b,j,c,k 
    integer :: s ,d,l 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do d = nocc + 1, nactive 
term(0) = term(0) + t3(nocc, nactive, a,b,d,i,j,k) * Obs(c, d)
term(1) = term(1) + t3(nocc, nactive, a,b,d,k,i,j) * Obs(c, d)
term(2) = term(2) + t3(nocc, nactive, a,c,d,k,j,i) * Obs(b, d)
term(3) = term(3) + t3(nocc, nactive, a,c,d,i,k,j) * Obs(b, d)
term(4) = term(4) + t3(nocc, nactive, b,c,d,j,k,i) * Obs(a, d)
term(5) = term(5) + t3(nocc, nactive, b,c,d,i,j,k) * Obs(a, d)
end do 

term(1) = -term(1) 
term(2) = -term(2) 
term(5) = -term(5) 

do l = 1, nocc 
do d = nocc + 1, nactive 
term(6) = term(6) + t2(a,d,i,k) * t2(b,c,j,l) * Obs(d, l)
term(7) = term(7) + t2(a,d,k,j) * t2(b,c,i,l) * Obs(d, l)
term(8) = term(8) + t2(a,b,i,l) * t2(c,d,k,j) * Obs(d, l)
term(9) = term(9) + t2(a,c,k,l) * t2(b,d,i,j) * Obs(d, l)
term(10) = term(10) + t2(a,c,i,l) * t2(b,d,j,k) * Obs(d, l)
term(11) = term(11) + t2(a,b,k,l) * t2(c,d,j,i) * Obs(d, l)
end do 
end do 

term(6) = -term(6) 
term(8) = -term(8) 
term(10) = -term(10) 

do d = nocc + 1, nactive 
do l = 1, nocc 
term(12) = term(12) + t2(a,d,k,i) * t2(b,c,l,j) * Obs(d, l)
term(13) = term(13) + t2(a,d,i,j) * t2(b,c,l,k) * Obs(d, l)
term(14) = term(14) + t2(a,b,l,j) * t2(c,d,k,i) * Obs(d, l)
term(15) = term(15) + t2(a,c,l,k) * t2(b,d,j,i) * Obs(d, l)
term(16) = term(16) + t2(a,c,l,j) * t2(b,d,i,k) * Obs(d, l)
term(17) = term(17) + t2(a,b,l,i) * t2(c,d,j,k) * Obs(d, l)
end do 
end do 

term(13) = -term(13) 
term(14) = -term(14) 
term(15) = -term(15) 

do l = 1, nocc 
term(18) = term(18) + t3(nocc, nactive, a,b,c,i,j,l) * Obs(k, l)
term(19) = term(19) + t3(nocc, nactive, a,b,c,k,i,l) * Obs(j, l)
term(20) = term(20) + t3(nocc, nactive, a,b,c,k,l,j) * Obs(i, l)
term(21) = term(21) + t3(nocc, nactive, a,b,c,i,l,k) * Obs(j, l)
term(22) = term(22) + t3(nocc, nactive, a,b,c,l,j,k) * Obs(i, l)
term(23) = term(23) + t3(nocc, nactive, a,b,c,l,i,j) * Obs(k, l)
end do 

term(18) = -term(18) 
term(21) = -term(21) 
term(22) = -term(22) 


    v3_tmxi_aibjck = 0.d+0 
    do s = 0, 23
    v3_tmxi_aibjck = v3_tmxi_aibjck + term(s)
    end do

    end function v3_tmxi_aibjck
    
    end module v3_tmxi
    
