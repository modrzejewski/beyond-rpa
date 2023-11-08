module density_gr_exc_functions

      use cc3_intermediates
      use s_gen
      use basis
      use symmetry
      use eom_vectors
      


      implicit none

contains

      function calc_D_oo_gamma(t1, s2, s1, vrdav, nocc, nactive,  i,j) 
            double precision :: calc_D_oo_gamma
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            double precision, dimension(:), intent(in) :: vrdav
            integer, intent(in) :: i,j 
            integer :: s ,k,b,a 
            double precision, dimension(0:7) :: term 
            term = 0.d+0 
            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        do a = nocc + 1, nactive 
                              term(0) = term(0) + r1(vrdav, a,k) * s2(a,b,j,k) * t1(b,i)
                              term(1) = term(1) + r2(vrdav, a,k,b,i) * s2(a,b,j,k)
                              term(2) = term(2) + r2(vrdav, a,i,b,k) * s2(a,b,j,k)
                              term(3) = term(3) + r2(vrdav, a,i,b,k) * s2(a,b,k,j)
                              term(4) = term(4) + r1(vrdav, a,k) * s2(a,b,k,j) * t1(b,i)
                              term(5) = term(5) + r1(vrdav, a,k) * s2(a,b,k,i) * t1(b,j)
                              term(6) = term(6) + r2(vrdav, a,k,b,i) * s2(a,b,k,j)
                        end do
                  end do
            end do

            term(0) = term(0) * 2.0d+0 
            term(2) = term(2) * (-2.0d+0) 
            term(4) = term(4) * (-2.0d+0) 
            term(5) = term(5) * (-2.0d+0) 
            term(6) = term(6) * (-2.0d+0) 

            do a = nocc + 1, nactive 
                  term(7) = term(7) + r1(vrdav, a,i) * s1(a,j)
            end do

            term(7) = term(7) * (-2.0d+0) 


            calc_D_oo_gamma = 0.d+0 
            do s = 0, 7
                  calc_D_oo_gamma = calc_D_oo_gamma + term(s)
            end do

      end function calc_D_oo_gamma

      function calc_D_ov_gamma(t2, s2, vrdav, nocc, nactive, i,a) 
            double precision :: calc_D_ov_gamma
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(:), intent(in) :: vrdav
            integer, intent(in) :: i,a 
            
            integer :: s ,k,j,c,b 
            double precision, dimension(0:1) :: term 
            term = 0.d+0 
            do k = 1, nocc 
                  do j = 1, nocc 
                        do c = nocc + 1, nactive 
                              do b = nocc + 1, nactive 
                                    term(0) = term(0) + r1(vrdav, b,i) * s2(b,c,k,j) * t2(a,c,j,k)
                                    term(1) = term(1) + r1(vrdav, b,i) * s2(b,c,k,j) * t2(a,c,k,j)
                              end do
                        end do
                  end do
            end do

            term(0) = term(0) * 2.0d+0 
            term(1) = term(1) * (-4.0d+0) 


            calc_D_ov_gamma = 0.d+0 
            do s = 0, 1
                  calc_D_ov_gamma = calc_D_ov_gamma + term(s)
            end do

      end function calc_D_ov_gamma

      function calc_D_vo_gamma(t2, s2, s1, vrdav, nocc, nactive,  a,i) 
            double precision :: calc_D_vo_gamma
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            double precision, dimension(:), intent(in) :: vrdav
            integer, intent(in) :: a,i 
            
            integer :: s ,j,b,k,c 
            double precision, dimension(0:14) :: term 
            term = 0.d+0 
            do j = 1, nocc 
                  do b = nocc + 1, nactive 
                        term(0) = term(0) + r1(vrdav, b,j) * s2(b,a,i,j)
                        term(1) = term(1) + r1(vrdav, b,j) * s2(b,a,j,i)
                        term(2) = term(2) + r2(vrdav, b,j,a,i) * s1(b,j)
                        term(3) = term(3) + r2(vrdav, a,i,b,j) * s1(b,j)
                        term(4) = term(4) + r2(vrdav, b,i,a,j) * s1(b,j)
                        term(14) = term(14) + r2(vrdav, a,j,b,i) * s1(b,j)
                  end do
            end do

            term(0) = term(0) * (-2.0d+0) 
            term(1) = term(1) * 4.0d+0 
            term(2) = term(2) * 2.0d+0 
            term(3) = term(3) * 2.0d+0 
            term(4) = -term(4) 
            term(14) = -term(14) 

            term(5) = term(5) + r1(vrdav, a,i)

            term(5) = term(5) * 2.0d+0 

            do k = 1, nocc 
                  do j = 1, nocc 
                        do c = nocc + 1, nactive 
                              do b = nocc + 1, nactive 
                                    term(6) = term(6) + r1(vrdav, b,j) * s2(b,c,k,j) * t2(a,c,i,k)
                                    term(7) = term(7) + r1(vrdav, a,j) * s2(b,c,k,j) * t2(b,c,i,k)
                                    term(8) = term(8) + r1(vrdav, b,j) * s2(b,c,j,k) * t2(a,c,i,k)
                                    term(9) = term(9) + r1(vrdav, a,j) * s2(b,c,j,k) * t2(b,c,k,i)
                                    term(10) = term(10) + r1(vrdav, a,j) * s2(b,c,j,k) * t2(b,c,i,k)
                                    term(11) = term(11) + r1(vrdav, b,j) * s2(b,c,j,k) * t2(a,c,k,i)
                                    term(12) = term(12) + r1(vrdav, b,j) * s2(b,c,k,j) * t2(a,c,k,i)
                                    term(13) = term(13) + r1(vrdav, a,j) * s2(b,c,k,j) * t2(b,c,k,i)
                              end do
                        end do
                  end do
            end do

            term(6) = term(6) * (-4.0d+0) 
            term(7) = term(7) * 0.5d+0 
            term(8) = term(8) * 8.0d+0 
            term(9) = term(9) * 1.5d+0 
            term(10) = term(10) * (-2.0d+0) 
            term(11) = term(11) * (-4.0d+0) 
            term(12) = term(12) * 2.0d+0 
            term(13) = term(13) * (-2.0d+0) 
            
            calc_D_vo_gamma = 0.d+0 
            do s = 0, 14
                  calc_D_vo_gamma = calc_D_vo_gamma + term(s)
            end do

      end function calc_D_vo_gamma

      function calc_D_vv_gamma(t1, s2, s1, vrdav, nocc, nactive,  a,b) 
            double precision :: calc_D_vv_gamma
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            double precision, dimension(:), intent(in) :: vrdav
            integer, intent(in) :: a,b 
            
            integer :: s ,i,j,c 
            double precision, dimension(0:7) :: term 
            term = 0.d+0   

            do j = 1, nocc 
                  do i = 1, nocc 
                        do c = nocc + 1, nactive 
                              term(0) = term(0) + r1(vrdav, c,i) * s2(c,b,j,i) * t1(a,j)
                              term(1) = term(1) + r1(vrdav, c,i) * s2(c,b,i,j) * t1(a,j)
                              term(2) = term(2) + r1(vrdav, c,i) * s2(c,a,i,j) * t1(b,j)
                              term(3) = term(3) + r2(vrdav, a,i,c,j) * s2(b,c,j,i)
                              term(4) = term(4) + r2(vrdav, c,i,a,j) * s2(c,b,j,i)
                              term(5) = term(5) + r2(vrdav, c,i,a,j) * s2(c,b,i,j)
                              term(7) = term(7) + r2(vrdav, a,i,c,j) * s2(b,c,i,j)
                        end do
                  end do
            end do

            term(0) = term(0) * (-2.0d+0) 
            term(1) = term(1) * 2.0d+0 
            term(2) = term(2) * 2.0d+0 
            term(3) = -term(3) 
            term(4) = -term(4) 
            term(5) = term(5) * 2.0d+0 
            term(7) = term(7) * 2.0d+0 

            do i = 1, nocc 
                  term(6) = term(6) + r1(vrdav, a,i) * s1(b,i)
            end do

            term(6) = term(6) * 2.0d+0 

            calc_D_vv_gamma = 0.d+0 
            do s = 0, 7
                  calc_D_vv_gamma = calc_D_vv_gamma + term(s)
            end do

      end function calc_D_vv_gamma

      function calc_D_oo_xi(t2, t1, vldav, nocc, nactive,  i,j) 
            double precision :: calc_D_oo_xi
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(:), intent(in) :: vldav
            integer, intent(in) :: i,j 
            
            integer :: s ,a,b,k 
            double precision, dimension(0:3) :: term 
            term = 0.d+0 
            do a = nocc + 1, nactive 
                  term(0) = term(0) + l1(vldav, a,j) * t1(a,i)
                  term(1) = term(1) + l2(vldav, a,j,a,j) * t2(a,a,j,i)
            end do

            term(0) = -term(0) 
            term(1) = term(1) * 0.5d+0 

            do b = nocc + 1, nactive 
                  do k = 1, nocc 
                        do a = nocc + 1, nactive 
                              term(2) = term(2) + l2(vldav, a,k,b,i) * t2(a,b,k,j)
                              term(3) = term(3) + l2(vldav, a,i,b,k) * t2(a,b,j,k)
                        end do
                  end do
            end do

            term(2) = term(2) * (-0.5d+0) 
            term(3) = term(3) * (-0.5d+0) 


            calc_D_oo_xi = 0.d+0 
            do s = 0, 3
                  calc_D_oo_xi = calc_D_oo_xi + term(s)
            end do

      end function calc_D_oo_xi

      function calc_D_ov_xi()
            double precision :: calc_D_ov_xi

            calc_D_ov_xi = 0.d+0 

      end function calc_D_ov_xi

      function calc_D_vo_xi(t2, vldav, nocc, nactive,  a,i) 
            double precision :: calc_D_vo_xi
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(:), intent(in) :: vldav
            integer, intent(in) :: a,i 
            
            integer :: s ,j,b 
            double precision, dimension(0:2) :: term 

            term = 0.d+0 
            do j = 1, nocc 
                  do b = nocc + 1, nactive 
                        term(0) = term(0) + l1(vldav, b,j) * t2(b,a,j,i)
                        term(1) = term(1) + l1(vldav, b,j) * t2(b,a,i,j)
                  end do
            end do

            term(0) = term(0) * 2.0d+0 
            term(1) = -term(1) 

            term(2) = term(2) + l1(vldav, a,i)

            calc_D_vo_xi = 0.d+0 
            do s = 0, 2
                  calc_D_vo_xi = calc_D_vo_xi + term(s)
            end do

      end function calc_D_vo_xi

      function calc_D_vv_xi(t2, t1, vldav, nocc, nactive,  a,b) 
            double precision :: calc_D_vv_xi
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(:), intent(in) :: vldav
            integer, intent(in) :: a,b 
            
            integer :: s ,i,j,c 
            double precision, dimension(0:3) :: term 
            term = 0.d+0 
            do i = 1, nocc 
                  term(0) = term(0) + l1(vldav, b,i) * t1(a,i)
                  term(1) = term(1) + l2(vldav, b,i,b,i) * t2(b,a,i,i)
            end do

            term(1) = term(1) * (-0.5d+0) 

            do j = 1, nocc 
                  do i = 1, nocc 
                        do c = nocc + 1, nactive 
                              term(2) = term(2) + l2(vldav, c,i,a,j) * t2(c,b,i,j)
                              term(3) = term(3) + l2(vldav, a,i,c,j) * t2(b,c,i,j)
                        end do
                  end do
            end do

            term(2) = term(2) * 0.5d+0 
            term(3) = term(3) * 0.5d+0 

            calc_D_vv_xi = 0.d+0 
            do s = 0, 3
                  calc_D_vv_xi = calc_D_vv_xi + term(s)
            end do

      end function calc_D_vv_xi


    

    
    function calc_D_oo_gamma_cc3(t2, t1, s2, s1, vrdav, nocc, nactive, i,j) 
    double precision :: calc_D_oo_gamma_cc3
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    
    double precision, dimension(:), intent(in) :: vrdav
    integer, intent(in) :: i,j 
    integer :: s ,a,b,k,l,c 
    double precision, dimension(0:67) :: term 
    term = 0.d+0 
    do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav, a,i) * s1(a,j)
end do 

term(0) = term(0) * (-2.0d+0) 

do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1) = term(1) + r1(vrdav, b,k) * s2(a,b,k,j) * t1(a,i)
end do 
end do 
end do 

term(1) = term(1) * 2.0d+0 

do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(2) = term(2) + r1(vrdav, b,k) * s2(a,b,j,k) * t1(a,i)
term(3) = term(3) + r1(vrdav, b,k) * s2(a,b,i,k) * t1(a,j)
term(4) = term(4) + r2(vrdav, a,i,b,k) * s2(a,b,j,k)
term(5) = term(5) + r2(vrdav, a,i,b,k) * s2(a,b,k,j)
term(6) = term(6) + r2(vrdav, b,k,a,i) * s2(a,b,j,k)
end do 
end do 
end do 

term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(6) = term(6) * (-2.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(7) = term(7) + r2(vrdav, b,k,a,i) * s2(a,b,k,j)
end do 
end do 
end do 


do k = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(8) = term(8) + r1(vrdav, a,k) * t2(b,c,j,l) * t3(nocc, nactive, a,b,c,i,l,k)
term(9) = term(9) + r1(vrdav, c,k) * t2(a,b,l,j) * t3(nocc, nactive, a,b,c,i,l,k)
term(10) = term(10) + r1(vrdav, b,k) * t2(a,c,j,l) * t3(nocc, nactive, a,b,c,i,l,k)
end do 
end do 
end do 
end do 
end do 

term(8) = term(8) * (-1.9999999999999998d+0) 
term(9) = term(9) * 3.9999999999999996d+0 
term(10) = term(10) * 4.0d+0 

do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(11) = term(11) + r1(vrdav, c,k) * t2(a,b,l,j) * t3(nocc, nactive, a,b,c,i,k,l)
term(12) = term(12) + r3(vrdav, c,k,a,i,b,l) * s1(b,j) * s2(a,c,l,k)
term(13) = term(13) + r3(vrdav, a,i,c,k,b,l) * s1(b,j) * s2(a,c,l,k)
term(14) = term(14) + r3(vrdav, a,i,c,k,b,l) * s1(b,j) * s2(a,c,k,l)
end do 
end do 
end do 
end do 
end do 

term(11) = term(11) * (-1.9999999999999998d+0) 
term(12) = term(12) * 0.6666666666666666d+0 
term(13) = term(13) * 0.6666666666666666d+0 
term(14) = term(14) * (-0.3333333333333333d+0) 

do l = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(15) = term(15) + r1(vrdav, a,k) * t2(b,c,j,l) * t3(nocc, nactive, a,b,c,i,k,l)
term(16) = term(16) + r1(vrdav, b,k) * t2(a,c,j,l) * t3(nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
end do 
end do 

term(15) = term(15) * 4.0d+0 
term(16) = term(16) * (-7.999999999999999d+0) 

do a = nocc + 1, nactive 
do l = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(17) = term(17) + r3(vrdav, b,k,c,l,a,i) * s1(a,j) * s2(b,c,k,l)
term(18) = term(18) + r3(vrdav, b,k,c,l,a,i) * s1(b,j) * s2(a,c,k,l)
term(19) = term(19) + r3(vrdav, b,k,c,l,a,i) * s1(b,k) * s2(a,c,l,j)
term(20) = term(20) + r3(vrdav, b,k,c,l,a,i) * s1(b,k) * s2(a,c,j,l)
term(21) = term(21) + r3(vrdav, b,k,c,l,a,i) * s1(b,l) * s2(a,c,k,j)
term(22) = term(22) + r3(vrdav, b,k,c,l,a,i) * s1(a,k) * s2(b,c,l,j)
term(23) = term(23) + r3(vrdav, b,k,c,l,a,i) * s1(a,l) * s2(b,c,k,j)
term(24) = term(24) + r3(vrdav, b,k,c,l,a,i) * s1(a,k) * s2(b,c,j,l)
end do 
end do 
end do 
end do 
end do 

term(17) = term(17) * (-1.3333333333333333d+0) 
term(18) = term(18) * 0.6666666666666666d+0 
term(19) = term(19) * 0.6666666666666666d+0 
term(20) = term(20) * (-1.3333333333333333d+0) 
term(21) = term(21) * (-0.3333333333333333d+0) 
term(22) = term(22) * (-0.3333333333333333d+0) 
term(23) = term(23) * 0.6666666666666666d+0 
term(24) = term(24) * 0.6666666666666666d+0 

do a = nocc + 1, nactive 
do l = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(25) = term(25) + r3(vrdav, c,k,b,l,a,i) * s1(b,j) * s2(a,c,l,k)
term(26) = term(26) + r3(vrdav, b,k,c,l,a,i) * s1(b,j) * s2(a,c,l,k)
term(27) = term(27) + r3(vrdav, b,k,c,l,a,i) * s1(a,j) * s2(b,c,l,k)
term(28) = term(28) + r3(vrdav, b,k,c,l,a,i) * s1(c,l) * s2(a,b,j,k)
term(29) = term(29) + r3(vrdav, b,k,c,l,a,i) * s1(b,l) * s2(a,c,j,k)
term(30) = term(30) + r3(vrdav, b,k,c,l,a,i) * s1(a,l) * s2(b,c,j,k)
end do 
end do 
end do 
end do 
end do 

term(25) = term(25) * 0.6666666666666666d+0 
term(26) = term(26) * (-0.3333333333333333d+0) 
term(27) = term(27) * 0.6666666666666666d+0 
term(28) = term(28) * (-1.3333333333333333d+0) 
term(29) = term(29) * 0.6666666666666666d+0 
term(30) = term(30) * (-0.3333333333333333d+0) 

do l = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(31) = term(31) + r3(vrdav, b,k,a,i,c,l) * s1(a,j) * s2(b,c,k,l)
term(32) = term(32) + r3(vrdav, b,k,a,i,c,l) * s1(b,j) * s2(a,c,k,l)
term(33) = term(33) + r3(vrdav, b,k,a,i,c,l) * s1(b,k) * s2(a,c,j,l)
term(34) = term(34) + r3(vrdav, b,k,a,i,c,l) * s1(b,k) * s2(a,c,l,j)
term(35) = term(35) + r3(vrdav, b,k,a,i,c,l) * s1(a,k) * s2(b,c,l,j)
term(36) = term(36) + r3(vrdav, b,k,a,i,c,l) * s1(a,k) * s2(b,c,j,l)
term(37) = term(37) + r3(vrdav, b,k,a,i,c,l) * s1(a,l) * s2(b,c,k,j)
term(38) = term(38) + r3(vrdav, b,k,a,i,c,l) * s1(b,l) * s2(a,c,k,j)
end do 
end do 
end do 
end do 
end do 

term(31) = term(31) * (-1.3333333333333333d+0) 
term(32) = term(32) * 0.6666666666666666d+0 
term(33) = term(33) * (-1.3333333333333333d+0) 
term(34) = term(34) * 0.6666666666666666d+0 
term(35) = term(35) * (-0.3333333333333333d+0) 
term(36) = term(36) * 0.6666666666666666d+0 
term(37) = term(37) * 0.6666666666666666d+0 
term(38) = term(38) * (-0.3333333333333333d+0) 

do l = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(39) = term(39) + r3(vrdav, a,i,b,k,c,l) * s1(a,j) * s2(b,c,k,l)
term(40) = term(40) + r3(vrdav, a,i,b,k,c,l) * s1(b,j) * s2(a,c,k,l)
term(41) = term(41) + r3(vrdav, a,i,b,k,c,l) * s1(a,j) * s2(b,c,l,k)
term(42) = term(42) + r3(vrdav, a,i,b,k,c,l) * s1(b,j) * s2(a,c,l,k)
term(43) = term(43) + r3(vrdav, a,i,b,k,c,l) * s1(b,k) * s2(a,c,l,j)
term(44) = term(44) + r3(vrdav, a,i,b,k,c,l) * s1(b,k) * s2(a,c,j,l)
term(45) = term(45) + r3(vrdav, a,i,b,k,c,l) * s1(a,k) * s2(b,c,l,j)
term(46) = term(46) + r3(vrdav, a,i,b,k,c,l) * s1(a,k) * s2(b,c,j,l)
term(47) = term(47) + r3(vrdav, a,i,b,k,c,l) * s1(c,k) * s2(a,b,l,j)
term(48) = term(48) + r3(vrdav, a,i,b,k,c,l) * s1(c,l) * s2(a,b,k,j)
term(49) = term(49) + r3(vrdav, b,k,a,i,c,l) * s1(c,l) * s2(a,b,j,k)
term(50) = term(50) + r3(vrdav, a,i,b,k,c,l) * s1(c,k) * s2(a,b,j,l)
term(51) = term(51) + r3(vrdav, a,i,b,k,c,l) * s1(c,l) * s2(a,b,j,k)
term(52) = term(52) + r3(vrdav, a,i,b,k,c,l) * s1(b,l) * s2(a,c,k,j)
term(53) = term(53) + r3(vrdav, a,i,b,k,c,l) * s1(b,l) * s2(a,c,j,k)
term(54) = term(54) + r3(vrdav, a,i,b,k,c,l) * s1(a,l) * s2(b,c,j,k)
term(55) = term(55) + r3(vrdav, a,i,b,k,c,l) * s1(a,l) * s2(b,c,k,j)
end do 
end do 
end do 
end do 
end do 

term(39) = term(39) * (-1.3333333333333333d+0) 
term(40) = term(40) * 0.6666666666666666d+0 
term(41) = term(41) * 0.6666666666666666d+0 
term(42) = term(42) * (-0.3333333333333333d+0) 
term(43) = term(43) * 0.6666666666666666d+0 
term(44) = term(44) * (-1.3333333333333333d+0) 
term(45) = term(45) * (-0.3333333333333333d+0) 
term(46) = term(46) * 0.6666666666666666d+0 
term(47) = term(47) * (-0.3333333333333333d+0) 
term(48) = term(48) * 0.6666666666666666d+0 
term(49) = term(49) * (-1.3333333333333333d+0) 
term(50) = term(50) * 0.6666666666666666d+0 
term(51) = term(51) * (-1.3333333333333333d+0) 
term(52) = term(52) * (-0.3333333333333333d+0) 
term(53) = term(53) * 0.6666666666666666d+0 
term(54) = term(54) * (-0.3333333333333333d+0) 
term(55) = term(55) * 0.6666666666666666d+0 

do l = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(56) = term(56) + r3(vrdav, c,k,a,i,b,l) * s1(b,j) * s2(a,c,k,l)
end do 
end do 
end do 
end do 
end do 

term(56) = term(56) * (-0.3333333333333333d+0) 

do a = nocc + 1, nactive 
do l = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
term(57) = term(57) + r3(vrdav, c,k,b,l,a,i) * s1(b,j) * s2(a,c,k,l)
term(58) = term(58) + r3(vrdav, b,k,c,l,a,i) * s1(c,k) * s2(a,b,j,l)
term(59) = term(59) + r3(vrdav, b,k,c,l,a,i) * s1(c,k) * s2(a,b,l,j)
term(60) = term(60) + r3(vrdav, b,k,c,l,a,i) * s1(c,l) * s2(a,b,k,j)
end do 
end do 
end do 
end do 
end do 

term(57) = term(57) * (-0.3333333333333333d+0) 
term(58) = term(58) * 0.6666666666666666d+0 
term(59) = term(59) * (-0.3333333333333333d+0) 
term(60) = term(60) * 0.6666666666666666d+0 

do l = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(61) = term(61) + r3(vrdav, b,k,a,i,c,l) * s1(b,j) * s2(a,c,l,k)
term(62) = term(62) + r3(vrdav, b,k,a,i,c,l) * s1(a,j) * s2(b,c,l,k)
term(63) = term(63) + r3(vrdav, b,k,a,i,c,l) * s1(a,l) * s2(b,c,j,k)
term(64) = term(64) + r3(vrdav, b,k,a,i,c,l) * s1(b,l) * s2(a,c,j,k)
end do 
end do 
end do 
end do 
end do 

term(61) = term(61) * (-0.3333333333333333d+0) 
term(62) = term(62) * 0.6666666666666666d+0 
term(63) = term(63) * (-0.3333333333333333d+0) 
term(64) = term(64) * 0.6666666666666666d+0 

do l = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(65) = term(65) + r3(vrdav, b,k,a,i,c,l) * s1(c,k) * s2(a,b,j,l)
term(66) = term(66) + r3(vrdav, b,k,a,i,c,l) * s1(c,k) * s2(a,b,l,j)
term(67) = term(67) + r3(vrdav, b,k,a,i,c,l) * s1(c,l) * s2(a,b,k,j)
end do 
end do 
end do 
end do 
end do 

term(65) = term(65) * 0.6666666666666666d+0 
term(66) = term(66) * (-0.3333333333333333d+0) 
term(67) = term(67) * 0.6666666666666666d+0 


    calc_D_oo_gamma_cc3 = 0.d+0 
    do s = 0, 67
    calc_D_oo_gamma_cc3 = calc_D_oo_gamma_cc3 + term(s)
    end do

    end function calc_D_oo_gamma_cc3
    
    function calc_D_ov_gamma_cc3(t2, t1, s2, vrdav, nocc, nactive, i,a) 
    double precision :: calc_D_ov_gamma_cc3
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    
    double precision, dimension(:), intent(in) :: vrdav
    integer, intent(in) :: i,a 
    integer :: s ,j,k,c,l,d,b 
    double precision, dimension(0:17) :: term 
    term = 0.d+0 
    do j = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do l = 1, nocc 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav, c,k,b,i) * t2(a,d,j,l) * t3(nocc, nactive, b,c,d,k,l,j)
term(1) = term(1) + r2(vrdav, b,i,c,k) * t2(a,d,j,l) * t3(nocc, nactive, b,c,d,k,l,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(0) = -term(0) 
term(1) = -term(1) 

do j = 1, nocc 
do l = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(2) = term(2) + r2(vrdav, b,i,c,k) * t2(a,d,j,l) * t3(nocc, nactive, b,c,d,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(2) = term(2) * 2.0d+0 

do k = 1, nocc 
do l = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(3) = term(3) + r2(vrdav, b,i,c,k) * t2(a,d,j,l) * t3(nocc, nactive, b,c,d,l,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(3) = -term(3) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do l = 1, nocc 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(4) = term(4) + r2(vrdav, c,k,b,i) * t2(a,d,j,l) * t3(nocc, nactive, b,c,d,j,l,k)
term(5) = term(5) + r2(vrdav, b,i,c,k) * t2(a,d,j,l) * t3(nocc, nactive, b,c,d,j,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(4) = term(4) * 2.0d+0 
term(5) = term(5) * 2.0d+0 

do k = 1, nocc 
do l = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
term(6) = term(6) + r2(vrdav, c,k,b,i) * t2(a,d,j,l) * t3(nocc, nactive, b,c,d,l,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(6) = -term(6) 

do j = 1, nocc 
do l = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
term(7) = term(7) + r2(vrdav, c,k,b,i) * t2(a,d,j,l) * t3(nocc, nactive, b,c,d,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(7) = term(7) * 2.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
term(8) = term(8) + r2(vrdav, c,k,b,i) * t2(a,d,j,l) * t3(nocc, nactive, b,c,d,k,j,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(8) = term(8) * 2.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(9) = term(9) + r2(vrdav, b,i,c,k) * t2(a,d,j,l) * t3(nocc, nactive, b,c,d,j,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(9) = term(9) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(10) = term(10) + r2(vrdav, b,i,c,k) * t2(a,d,j,l) * t3(nocc, nactive, b,c,d,k,j,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(10) = term(10) * 2.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
term(11) = term(11) + r2(vrdav, c,k,b,i) * t2(a,d,j,l) * t3(nocc, nactive, b,c,d,j,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(11) = term(11) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(12) = term(12) + r1(vrdav, b,i) * s2(b,c,k,j) * t2(a,c,j,k)
term(13) = term(13) + r2(vrdav, b,i,c,k) * s2(b,c,k,j) * t1(a,j)
end do 
end do 
end do 
end do 

term(12) = term(12) * 2.0d+0 

do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(14) = term(14) + r1(vrdav, b,i) * s2(b,c,j,k) * t2(a,c,j,k)
term(15) = term(15) + r2(vrdav, b,i,c,k) * s2(b,c,j,k) * t1(a,j)
end do 
end do 
end do 
end do 

term(14) = term(14) * (-4.0d+0) 
term(15) = term(15) * (-2.0d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
term(16) = term(16) + r2(vrdav, c,k,b,i) * s2(b,c,k,j) * t1(a,j)
end do 
end do 
end do 
end do 


do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
term(17) = term(17) + r2(vrdav, c,k,b,i) * s2(b,c,j,k) * t1(a,j)
end do 
end do 
end do 
end do 

term(17) = term(17) * (-2.0d+0) 


    calc_D_ov_gamma_cc3 = 0.d+0 
    do s = 0, 17
    calc_D_ov_gamma_cc3 = calc_D_ov_gamma_cc3 + term(s)
    end do

    end function calc_D_ov_gamma_cc3
    
    function calc_D_vo_gamma_cc3(t2, t1, s2, s1, vrdav, nocc, nactive, a,i) 
    double precision :: calc_D_vo_gamma_cc3
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    
    double precision, dimension(:), intent(in) :: vrdav
    integer, intent(in) :: a,i 
    integer :: s ,k,l,c,d,j,b 
    double precision, dimension(0:116) :: term 
    term = 0.d+0 
    do k = 1, nocc 
do l = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav, b,j,c,k) * t2(a,d,i,l) * t3(nocc, nactive, b,c,d,l,j,k)
term(1) = term(1) + r2(vrdav, a,j,c,k) * t2(b,d,i,l) * t3(nocc, nactive, b,c,d,l,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * 2.0d+0 
term(1) = -term(1) 

do j = 1, nocc 
do l = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(2) = term(2) + r2(vrdav, b,j,c,k) * t2(a,d,i,l) * t3(nocc, nactive, b,c,d,l,k,j)
term(3) = term(3) + r2(vrdav, a,j,c,k) * t2(b,d,i,l) * t3(nocc, nactive, b,c,d,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 2.0d+0 

do j = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do l = 1, nocc 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(4) = term(4) + r2(vrdav, b,j,c,k) * t2(a,d,i,l) * t3(nocc, nactive, b,c,d,k,l,j)
term(5) = term(5) + r2(vrdav, a,j,c,k) * t2(b,d,i,l) * t3(nocc, nactive, b,c,d,k,l,j)
term(6) = term(6) + r2(vrdav, c,k,a,j) * t2(b,d,i,l) * t3(nocc, nactive, b,c,d,k,l,j)
term(7) = term(7) + r2(vrdav, c,k,d,l) * t2(a,b,j,i) * t3(nocc, nactive, b,c,d,k,l,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(4) = term(4) * 2.0d+0 
term(5) = -term(5) 
term(6) = -term(6) 
term(7) = -term(7) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do l = 1, nocc 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(8) = term(8) + r2(vrdav, b,j,c,k) * t2(a,d,i,l) * t3(nocc, nactive, b,c,d,j,l,k)
term(9) = term(9) + r2(vrdav, a,j,c,k) * t2(b,d,i,l) * t3(nocc, nactive, b,c,d,j,l,k)
term(10) = term(10) + r2(vrdav, c,k,a,j) * t2(b,d,i,l) * t3(nocc, nactive, b,c,d,j,l,k)
term(11) = term(11) + r2(vrdav, c,k,d,l) * t2(a,b,j,i) * t3(nocc, nactive, b,c,d,j,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * 2.0d+0 
term(10) = term(10) * 2.0d+0 
term(11) = term(11) * 2.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(12) = term(12) + r2(vrdav, b,j,c,k) * t2(a,d,i,l) * t3(nocc, nactive, b,c,d,k,j,l)
term(13) = term(13) + r2(vrdav, a,j,c,k) * t2(b,d,i,l) * t3(nocc, nactive, b,c,d,k,j,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * 2.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(14) = term(14) + r2(vrdav, b,j,c,k) * t2(a,d,i,l) * t3(nocc, nactive, b,c,d,j,k,l)
term(15) = term(15) + r2(vrdav, a,j,c,k) * t2(b,d,i,l) * t3(nocc, nactive, b,c,d,j,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(14) = term(14) * 8.0d+0 
term(15) = term(15) * (-4.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(16) = term(16) + r2(vrdav, c,k,a,j) * t2(b,d,i,l) * t3(nocc, nactive, b,c,d,l,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(16) = -term(16) 

do j = 1, nocc 
do l = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(17) = term(17) + r2(vrdav, c,k,a,j) * t2(b,d,i,l) * t3(nocc, nactive, b,c,d,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(17) = term(17) * 2.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(18) = term(18) + r2(vrdav, c,k,a,j) * t2(b,d,i,l) * t3(nocc, nactive, b,c,d,k,j,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(18) = term(18) * 2.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(19) = term(19) + r2(vrdav, c,k,a,j) * t2(b,d,i,l) * t3(nocc, nactive, b,c,d,j,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(19) = term(19) * (-4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
term(20) = term(20) + r2(vrdav, c,k,d,l) * t2(a,b,j,i) * t3(nocc, nactive, b,c,d,j,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(20) = term(20) * (-4.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
term(21) = term(21) + r2(vrdav, c,k,d,l) * t2(a,b,j,i) * t3(nocc, nactive, b,c,d,l,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(21) = -term(21) 

do l = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
term(22) = term(22) + r2(vrdav, c,k,d,l) * t2(a,b,j,i) * t3(nocc, nactive, b,c,d,k,j,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(22) = term(22) * 2.0d+0 

do j = 1, nocc 
do l = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
term(23) = term(23) + r2(vrdav, c,k,d,l) * t2(a,b,j,i) * t3(nocc, nactive, b,c,d,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(23) = term(23) * 2.0d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(24) = term(24) + r3(vrdav, b,j,c,k,d,l) * s2(b,c,j,l) * s2(a,d,k,i)
term(25) = term(25) + r3(vrdav, b,j,c,k,d,l) * s2(b,c,j,k) * s2(a,d,l,i)
term(26) = term(26) + r3(vrdav, b,j,c,k,d,l) * s2(b,c,j,l) * s2(a,d,i,k)
term(27) = term(27) + r3(vrdav, b,j,c,k,d,l) * s2(b,c,j,k) * s2(a,d,i,l)
term(28) = term(28) + r3(vrdav, b,j,c,k,d,l) * s2(b,d,j,k) * s2(a,c,l,i)
term(29) = term(29) + r3(vrdav, b,j,c,k,d,l) * s2(b,d,j,k) * s2(a,c,i,l)
term(30) = term(30) + r3(vrdav, b,j,c,k,d,l) * s2(b,d,j,l) * s2(a,c,k,i)
term(31) = term(31) + r3(vrdav, b,j,c,k,d,l) * s2(b,d,j,l) * s2(a,c,i,k)
term(32) = term(32) + r3(vrdav, b,j,c,k,d,l) * s2(b,c,l,k) * s2(a,d,j,i)
term(33) = term(33) + r3(vrdav, b,j,c,k,d,l) * s2(b,d,k,l) * s2(a,c,j,i)
term(34) = term(34) + r3(vrdav, b,j,c,k,d,l) * s2(b,c,k,l) * s2(a,d,j,i)
term(35) = term(35) + r3(vrdav, b,j,c,k,d,l) * s2(b,d,l,k) * s2(a,c,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(24) = term(24) * 0.6666666666666666d+0 
term(25) = term(25) * (-1.3333333333333333d+0) 
term(26) = term(26) * (-1.3333333333333333d+0) 
term(27) = term(27) * 2.6666666666666665d+0 
term(28) = term(28) * 0.6666666666666666d+0 
term(29) = term(29) * (-1.3333333333333333d+0) 
term(30) = term(30) * (-1.3333333333333333d+0) 
term(31) = term(31) * 2.6666666666666665d+0 
term(32) = term(32) * 0.6666666666666666d+0 
term(33) = term(33) * 0.6666666666666666d+0 
term(34) = term(34) * (-0.3333333333333333d+0) 
term(35) = term(35) * (-0.3333333333333333d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
term(36) = term(36) + r3(vrdav, b,j,c,k,d,l) * s2(c,d,j,k) * s2(a,b,l,i)
term(37) = term(37) + r3(vrdav, b,j,c,k,d,l) * s2(c,d,j,l) * s2(a,b,k,i)
term(38) = term(38) + r3(vrdav, b,j,c,k,d,l) * s2(c,d,k,l) * s2(a,b,j,i)
term(39) = term(39) + r3(vrdav, b,j,c,k,d,l) * s2(c,d,j,k) * s2(a,b,i,l)
term(40) = term(40) + r3(vrdav, b,j,c,k,d,l) * s2(c,d,j,l) * s2(a,b,i,k)
term(41) = term(41) + r3(vrdav, b,j,c,k,d,l) * s2(c,d,l,k) * s2(a,b,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(36) = term(36) * (-0.3333333333333333d+0) 
term(37) = term(37) * 0.6666666666666666d+0 
term(38) = term(38) * (-1.3333333333333333d+0) 
term(39) = term(39) * 0.6666666666666666d+0 
term(40) = term(40) * (-1.3333333333333333d+0) 
term(41) = term(41) * 0.6666666666666666d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(42) = term(42) + r3(vrdav, b,j,c,k,d,l) * s2(c,d,k,j) * s2(a,b,l,i)
term(43) = term(43) + r3(vrdav, b,j,c,k,d,l) * s2(c,d,k,j) * s2(a,b,i,l)
term(44) = term(44) + r3(vrdav, b,j,c,k,d,l) * s2(c,d,k,l) * s2(a,b,i,j)
term(45) = term(45) + r3(vrdav, b,j,c,k,d,l) * s2(b,c,l,j) * s2(a,d,k,i)
term(46) = term(46) + r3(vrdav, b,j,c,k,d,l) * s2(b,c,l,j) * s2(a,d,i,k)
term(47) = term(47) + r3(vrdav, b,j,c,k,d,l) * s2(b,c,l,k) * s2(a,d,i,j)
term(48) = term(48) + r3(vrdav, b,j,c,k,d,l) * s2(b,c,k,j) * s2(a,d,l,i)
term(49) = term(49) + r3(vrdav, b,j,c,k,d,l) * s2(b,c,k,j) * s2(a,d,i,l)
term(50) = term(50) + r3(vrdav, b,j,c,k,d,l) * s2(b,d,k,j) * s2(a,c,l,i)
term(51) = term(51) + r3(vrdav, b,j,c,k,d,l) * s2(b,d,k,j) * s2(a,c,i,l)
term(52) = term(52) + r3(vrdav, b,j,c,k,d,l) * s2(b,d,k,l) * s2(a,c,i,j)
term(53) = term(53) + r3(vrdav, b,j,c,k,d,l) * s2(b,c,k,l) * s2(a,d,i,j)
term(54) = term(54) + r3(vrdav, b,j,c,k,d,l) * s2(c,d,l,j) * s2(a,b,k,i)
term(55) = term(55) + r3(vrdav, b,j,c,k,d,l) * s2(c,d,l,j) * s2(a,b,i,k)
term(56) = term(56) + r3(vrdav, b,j,c,k,d,l) * s2(c,d,l,k) * s2(a,b,i,j)
term(57) = term(57) + r3(vrdav, b,j,c,k,d,l) * s2(b,d,l,k) * s2(a,c,i,j)
term(58) = term(58) + r3(vrdav, b,j,c,k,d,l) * s2(b,d,l,j) * s2(a,c,k,i)
term(59) = term(59) + r3(vrdav, b,j,c,k,d,l) * s2(b,d,l,j) * s2(a,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(42) = term(42) * 0.6666666666666666d+0 
term(43) = term(43) * (-1.3333333333333333d+0) 
term(44) = term(44) * 2.6666666666666665d+0 
term(45) = term(45) * (-0.3333333333333333d+0) 
term(46) = term(46) * 0.6666666666666666d+0 
term(47) = term(47) * (-1.3333333333333333d+0) 
term(48) = term(48) * 0.6666666666666666d+0 
term(49) = term(49) * (-1.3333333333333333d+0) 
term(50) = term(50) * (-0.3333333333333333d+0) 
term(51) = term(51) * 0.6666666666666666d+0 
term(52) = term(52) * (-1.3333333333333333d+0) 
term(53) = term(53) * 0.6666666666666666d+0 
term(54) = term(54) * (-0.3333333333333333d+0) 
term(55) = term(55) * 0.6666666666666666d+0 
term(56) = term(56) * (-1.3333333333333333d+0) 
term(57) = term(57) * 0.6666666666666666d+0 
term(58) = term(58) * 0.6666666666666666d+0 
term(59) = term(59) * (-1.3333333333333333d+0) 

term(60) = term(60) + r1(vrdav, a,i)

term(60) = term(60) * 2.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(61) = term(61) + r2(vrdav, b,k,c,j) * t3(nocc, nactive, a,b,c,j,i,k)
term(62) = term(62) + r2(vrdav, b,j,c,k) * t3(nocc, nactive, a,b,c,j,i,k)
term(63) = term(63) + r2(vrdav, c,j,b,k) * t3(nocc, nactive, a,b,c,j,i,k)
term(64) = term(64) + r2(vrdav, c,k,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 

term(62) = term(62) * (-1.9999999999999998d+0) 
term(64) = term(64) * (-1.9999999999999998d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
term(65) = term(65) + r2(vrdav, b,j,c,k) * t3(nocc, nactive, a,b,c,i,k,j)
end do 
end do 
end do 
end do 

term(65) = term(65) * (-1.9999999999999998d+0) 

do k = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
term(66) = term(66) + r2(vrdav, b,j,c,k) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 

term(66) = term(66) * 3.9999999999999996d+0 

do j = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(67) = term(67) + r3(vrdav, c,k,b,i,a,j) * s2(b,c,j,k)
term(68) = term(68) + r3(vrdav, b,i,c,k,a,j) * s2(b,c,j,k)
term(69) = term(69) + r3(vrdav, b,i,c,k,a,j) * s2(b,c,k,j)
end do 
end do 
end do 
end do 

term(67) = term(67) * (-0.6666666666666666d+0) 
term(68) = term(68) * (-0.6666666666666666d+0) 
term(69) = term(69) * 0.3333333333333333d+0 

do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(70) = term(70) + r3(vrdav, b,j,c,k,a,i) * s2(b,c,j,k)
term(71) = term(71) + r3(vrdav, b,j,a,i,c,k) * s2(b,c,j,k)
term(72) = term(72) + r3(vrdav, a,i,b,j,c,k) * s2(b,c,j,k)
term(73) = term(73) + r3(vrdav, b,i,a,j,c,k) * s2(b,c,j,k)
term(74) = term(74) + r3(vrdav, a,j,b,i,c,k) * s2(b,c,k,j)
term(75) = term(75) + r3(vrdav, a,i,b,j,c,k) * s2(b,c,k,j)
term(76) = term(76) + r3(vrdav, b,i,a,j,c,k) * s2(b,c,k,j)
term(77) = term(77) + r3(vrdav, b,j,a,i,c,k) * s2(b,c,k,j)
term(78) = term(78) + r1(vrdav, b,j) * s2(b,c,j,k) * t2(a,c,i,k)
term(79) = term(79) + r1(vrdav, a,j) * s2(b,c,j,k) * t2(b,c,i,k)
term(80) = term(80) + r2(vrdav, b,j,c,k) * s1(b,i) * s2(a,c,j,k)
term(81) = term(81) + r2(vrdav, b,j,c,k) * s1(a,j) * s2(b,c,i,k)
term(82) = term(82) + r2(vrdav, b,j,c,k) * s1(a,j) * s2(b,c,k,i)
term(83) = term(83) + r2(vrdav, a,j,c,k) * s2(b,c,j,k) * t1(b,i)
term(84) = term(84) + r2(vrdav, b,j,c,k) * s1(b,j) * s2(a,c,k,i)
term(85) = term(85) + r2(vrdav, b,j,c,k) * s1(b,j) * s2(a,c,i,k)
term(86) = term(86) + r2(vrdav, b,j,c,k) * s1(b,k) * s2(a,c,j,i)
end do 
end do 
end do 
end do 

term(70) = term(70) * 1.3333333333333333d+0 
term(71) = term(71) * 1.3333333333333333d+0 
term(72) = term(72) * 1.3333333333333333d+0 
term(73) = term(73) * (-0.6666666666666666d+0) 
term(74) = term(74) * 0.3333333333333333d+0 
term(75) = term(75) * (-0.6666666666666666d+0) 
term(76) = term(76) * 0.3333333333333333d+0 
term(77) = term(77) * (-0.6666666666666666d+0) 
term(78) = term(78) * 8.0d+0 
term(79) = term(79) * (-4.0d+0) 
term(80) = term(80) * 2.0d+0 
term(81) = term(81) * 2.0d+0 
term(82) = -term(82) 
term(83) = term(83) * (-2.0d+0) 
term(84) = term(84) * (-2.0d+0) 
term(85) = term(85) * 4.0d+0 

do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
term(87) = term(87) + r3(vrdav, c,k,a,j,b,i) * s2(b,c,j,k)
term(88) = term(88) + r3(vrdav, a,j,c,k,b,i) * s2(b,c,k,j)
end do 
end do 
end do 
end do 

term(87) = term(87) * (-0.6666666666666666d+0) 
term(88) = term(88) * 0.3333333333333333d+0 

do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
term(89) = term(89) + r3(vrdav, a,j,c,k,b,i) * s2(b,c,j,k)
end do 
end do 
end do 
end do 

term(89) = term(89) * (-0.6666666666666666d+0) 

do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
term(90) = term(90) + r3(vrdav, a,j,b,i,c,k) * s2(b,c,j,k)
term(91) = term(91) + r1(vrdav, c,k) * s2(b,c,j,k) * t2(a,b,j,i)
term(92) = term(92) + r2(vrdav, c,j,b,k) * s1(b,i) * s2(a,c,j,k)
term(93) = term(93) + r2(vrdav, b,j,c,k) * s1(c,j) * s2(a,b,k,i)
term(94) = term(94) + r2(vrdav, b,j,c,k) * s1(c,k) * s2(a,b,j,i)
term(95) = term(95) + r2(vrdav, b,j,c,k) * s1(c,j) * s2(a,b,i,k)
end do 
end do 
end do 
end do 

term(90) = term(90) * (-0.6666666666666666d+0) 
term(91) = term(91) * (-4.0d+0) 
term(92) = -term(92) 
term(94) = term(94) * (-2.0d+0) 
term(95) = term(95) * (-2.0d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
term(96) = term(96) + r3(vrdav, c,k,b,i,a,j) * s2(b,c,k,j)
term(97) = term(97) + r1(vrdav, c,k) * s2(b,c,k,j) * t2(a,b,j,i)
end do 
end do 
end do 
end do 

term(96) = term(96) * 0.3333333333333333d+0 
term(97) = term(97) * 2.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(98) = term(98) + r3(vrdav, b,j,c,k,a,i) * s2(b,c,k,j)
term(99) = term(99) + r1(vrdav, b,j) * s2(b,c,k,j) * t2(a,c,i,k)
term(100) = term(100) + r1(vrdav, a,j) * s2(b,c,k,j) * t2(b,c,i,k)
term(101) = term(101) + r2(vrdav, b,j,c,k) * s1(b,i) * s2(a,c,k,j)
term(102) = term(102) + r2(vrdav, c,j,b,k) * s1(b,i) * s2(a,c,k,j)
term(103) = term(103) + r2(vrdav, b,k,c,j) * s1(a,j) * s2(b,c,i,k)
term(104) = term(104) + r2(vrdav, a,j,c,k) * s2(b,c,k,j) * t1(b,i)
term(105) = term(105) + r2(vrdav, c,k,a,j) * s2(b,c,j,k) * t1(b,i)
term(106) = term(106) + r2(vrdav, b,j,c,k) * s1(c,k) * s2(a,b,i,j)
term(107) = term(107) + r2(vrdav, b,j,c,k) * s1(b,k) * s2(a,c,i,j)
end do 
end do 
end do 
end do 

term(98) = term(98) * (-0.6666666666666666d+0) 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * 2.0d+0 
term(101) = -term(101) 
term(102) = term(102) * 2.0d+0 
term(103) = -term(103) 
term(105) = term(105) * (-2.0d+0) 
term(106) = term(106) * 4.0d+0 
term(107) = term(107) * (-2.0d+0) 

do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
term(108) = term(108) + r3(vrdav, c,k,a,j,b,i) * s2(b,c,k,j)
end do 
end do 
end do 
end do 

term(108) = term(108) * 0.3333333333333333d+0 

do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(109) = term(109) + r2(vrdav, b,k,c,j) * s1(a,j) * s2(b,c,k,i)
term(110) = term(110) + r2(vrdav, c,k,a,j) * s2(b,c,k,j) * t1(b,i)
end do 
end do 
end do 
end do 

term(109) = term(109) * 2.0d+0 

do b = nocc + 1, nactive 
do j = 1, nocc 
term(111) = term(111) + r1(vrdav, b,j) * s2(a,b,j,i)
term(112) = term(112) + r2(vrdav, a,j,b,i) * s1(b,j)
end do 
end do 

term(111) = term(111) * (-2.0d+0) 
term(112) = -term(112) 

do j = 1, nocc 
do b = nocc + 1, nactive 
term(113) = term(113) + r1(vrdav, b,j) * s2(a,b,i,j)
term(114) = term(114) + r2(vrdav, b,j,a,i) * s1(b,j)
term(115) = term(115) + r2(vrdav, a,i,b,j) * s1(b,j)
term(116) = term(116) + r2(vrdav, b,i,a,j) * s1(b,j)
end do 
end do 

term(113) = term(113) * 4.0d+0 
term(114) = term(114) * 2.0d+0 
term(115) = term(115) * 2.0d+0 
term(116) = -term(116) 


    calc_D_vo_gamma_cc3 = 0.d+0 
    do s = 0, 116
    calc_D_vo_gamma_cc3 = calc_D_vo_gamma_cc3 + term(s)
    end do

    end function calc_D_vo_gamma_cc3
    
    function calc_D_vv_gamma_cc3(t2, t1, s2, s1, vrdav, nocc, nactive, a,b) 
    double precision :: calc_D_vv_gamma_cc3
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    
    double precision, dimension(:), intent(in) :: vrdav
    integer, intent(in) :: a,b 
    integer :: s ,i,c,j,k,d 
    double precision, dimension(0:73) :: term 
    term = 0.d+0 
    do i = 1, nocc 
term(0) = term(0) + r1(vrdav, a,i) * s1(b,i)
end do 

term(0) = term(0) * 2.0d+0 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
term(1) = term(1) + r1(vrdav, c,j) * s2(b,c,j,i) * t1(a,i)
term(2) = term(2) + r2(vrdav, c,j,a,i) * s2(b,c,j,i)
end do 
end do 
end do 

term(1) = term(1) * (-2.0d+0) 
term(2) = -term(2) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do i = 1, nocc 
term(3) = term(3) + r1(vrdav, c,j) * s2(b,c,i,j) * t1(a,i)
term(4) = term(4) + r1(vrdav, c,j) * s2(a,c,i,j) * t1(b,i)
term(5) = term(5) + r2(vrdav, a,i,c,j) * s2(b,c,i,j)
end do 
end do 
end do 

term(3) = term(3) * 2.0d+0 
term(4) = term(4) * 2.0d+0 
term(5) = term(5) * 2.0d+0 

do k = 1, nocc 
do d = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(6) = term(6) + r3(vrdav, a,i,c,j,d,k) * s1(b,j) * s2(c,d,k,i)
term(7) = term(7) + r3(vrdav, c,j,a,i,d,k) * s1(b,i) * s2(c,d,k,j)
term(8) = term(8) + r3(vrdav, a,i,c,j,d,k) * s1(c,j) * s2(b,d,k,i)
term(9) = term(9) + r3(vrdav, c,j,a,i,d,k) * s1(c,i) * s2(b,d,k,j)
term(10) = term(10) + r3(vrdav, c,j,a,i,d,k) * s1(d,i) * s2(b,c,k,j)
term(11) = term(11) + r3(vrdav, a,i,c,j,d,k) * s1(d,j) * s2(b,c,k,i)
term(12) = term(12) + r3(vrdav, c,j,a,i,d,k) * s1(d,k) * s2(b,c,i,j)
term(13) = term(13) + r3(vrdav, a,i,c,j,d,k) * s1(d,k) * s2(b,c,j,i)
term(14) = term(14) + r3(vrdav, a,i,c,j,d,k) * s1(c,k) * s2(b,d,j,i)
term(15) = term(15) + r3(vrdav, c,j,a,i,d,k) * s1(c,k) * s2(b,d,i,j)
end do 
end do 
end do 
end do 
end do 

term(6) = term(6) * 0.3333333333333333d+0 
term(7) = term(7) * (-0.6666666666666666d+0) 
term(8) = term(8) * (-0.6666666666666666d+0) 
term(9) = term(9) * 0.3333333333333333d+0 
term(10) = term(10) * (-0.6666666666666666d+0) 
term(11) = term(11) * 0.3333333333333333d+0 
term(12) = term(12) * 1.3333333333333333d+0 
term(13) = term(13) * (-0.6666666666666666d+0) 
term(14) = term(14) * 0.3333333333333333d+0 
term(15) = term(15) * (-0.6666666666666666d+0) 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(16) = term(16) + r2(vrdav, a,i,c,j) * s2(b,c,j,i)
term(17) = term(17) + r2(vrdav, c,j,a,i) * s2(b,c,i,j)
end do 
end do 
end do 

term(16) = -term(16) 
term(17) = term(17) * 2.0d+0 

do k = 1, nocc 
do d = nocc + 1, nactive 
do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
term(18) = term(18) + r3(vrdav, c,j,a,i,d,k) * s1(d,j) * s2(b,c,k,i)
term(19) = term(19) + r3(vrdav, c,j,a,i,d,k) * s1(d,i) * s2(b,c,j,k)
term(20) = term(20) + r3(vrdav, c,j,a,i,d,k) * s1(d,j) * s2(b,c,i,k)
term(21) = term(21) + r3(vrdav, c,j,a,i,d,k) * s1(d,k) * s2(b,c,j,i)
end do 
end do 
end do 
end do 
end do 

term(18) = term(18) * 0.3333333333333333d+0 
term(19) = term(19) * 0.3333333333333333d+0 
term(20) = term(20) * (-0.6666666666666666d+0) 
term(21) = term(21) * (-0.6666666666666666d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do i = 1, nocc 
term(22) = term(22) + r1(vrdav, c,k) * t2(a,d,i,j) * t3(nocc, nactive, b,c,d,j,i,k)
term(23) = term(23) + r1(vrdav, c,j) * t2(a,d,i,k) * t3(nocc, nactive, b,c,d,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(23) = term(23) * (-1.9999999999999998d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do k = 1, nocc 
term(24) = term(24) + r1(vrdav, c,k) * t2(a,d,i,j) * t3(nocc, nactive, b,c,d,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(24) = term(24) * (-1.9999999999999998d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do d = nocc + 1, nactive 
term(25) = term(25) + r1(vrdav, c,j) * t2(a,d,i,k) * t3(nocc, nactive, b,c,d,j,k,i)
end do 
end do 
end do 
end do 
end do 


do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do d = nocc + 1, nactive 
term(26) = term(26) + r1(vrdav, c,j) * t2(a,d,i,k) * t3(nocc, nactive, b,c,d,i,k,j)
term(27) = term(27) + r1(vrdav, c,i) * t2(b,d,j,k) * t3(nocc, nactive, a,c,d,i,k,j)
term(28) = term(28) + r1(vrdav, c,j) * t2(b,d,i,k) * t3(nocc, nactive, a,c,d,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(26) = term(26) * (-1.9999999999999998d+0) 
term(28) = term(28) * (-1.9999999999999998d+0) 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do j = 1, nocc 
term(29) = term(29) + r1(vrdav, c,j) * t2(a,d,i,k) * t3(nocc, nactive, b,c,d,i,j,k)
term(30) = term(30) + r1(vrdav, c,k) * t2(b,d,j,i) * t3(nocc, nactive, a,c,d,i,j,k)
term(31) = term(31) + r1(vrdav, c,i) * t2(b,d,j,k) * t3(nocc, nactive, a,c,d,i,j,k)
term(32) = term(32) + r1(vrdav, c,j) * t2(b,d,i,k) * t3(nocc, nactive, a,c,d,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(29) = term(29) * 3.9999999999999996d+0 
term(31) = term(31) * (-1.9999999999999998d+0) 
term(32) = term(32) * 3.9999999999999996d+0 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do k = 1, nocc 
term(33) = term(33) + r1(vrdav, c,k) * t2(b,d,j,i) * t3(nocc, nactive, a,c,d,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(33) = term(33) * (-1.9999999999999998d+0) 

do i = 1, nocc 
do j = 1, nocc 
do d = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
term(34) = term(34) + r3(vrdav, c,k,d,j,a,i) * s1(b,j) * s2(c,d,k,i)
end do 
end do 
end do 
end do 
end do 

term(34) = term(34) * (-0.6666666666666666d+0) 

do i = 1, nocc 
do k = 1, nocc 
do d = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
term(35) = term(35) + r3(vrdav, c,j,d,k,a,i) * s1(b,i) * s2(c,d,j,k)
term(36) = term(36) + r3(vrdav, c,j,d,k,a,i) * s1(b,j) * s2(c,d,i,k)
term(37) = term(37) + r3(vrdav, c,j,d,k,a,i) * s1(b,j) * s2(c,d,k,i)
term(38) = term(38) + r3(vrdav, c,j,d,k,a,i) * s1(c,j) * s2(b,d,k,i)
term(39) = term(39) + r3(vrdav, c,j,d,k,a,i) * s1(c,j) * s2(b,d,i,k)
term(40) = term(40) + r3(vrdav, c,j,d,k,a,i) * s1(c,k) * s2(b,d,j,i)
term(41) = term(41) + r3(vrdav, c,j,d,k,a,i) * s1(c,i) * s2(b,d,j,k)
end do 
end do 
end do 
end do 
end do 

term(35) = term(35) * 1.3333333333333333d+0 
term(36) = term(36) * (-0.6666666666666666d+0) 
term(37) = term(37) * 0.3333333333333333d+0 
term(38) = term(38) * (-0.6666666666666666d+0) 
term(39) = term(39) * 1.3333333333333333d+0 
term(40) = term(40) * 0.3333333333333333d+0 
term(41) = term(41) * (-0.6666666666666666d+0) 

do j = 1, nocc 
do d = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
term(42) = term(42) + r3(vrdav, c,k,a,i,d,j) * s1(b,j) * s2(c,d,k,i)
end do 
end do 
end do 
end do 
end do 

term(42) = term(42) * (-0.6666666666666666d+0) 

do k = 1, nocc 
do d = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
term(43) = term(43) + r3(vrdav, c,j,a,i,d,k) * s1(b,i) * s2(c,d,j,k)
term(44) = term(44) + r3(vrdav, c,j,a,i,d,k) * s1(b,j) * s2(c,d,i,k)
term(45) = term(45) + r3(vrdav, c,j,a,i,d,k) * s1(b,j) * s2(c,d,k,i)
term(46) = term(46) + r3(vrdav, c,j,a,i,d,k) * s1(c,j) * s2(b,d,k,i)
term(47) = term(47) + r3(vrdav, c,j,a,i,d,k) * s1(c,j) * s2(b,d,i,k)
term(48) = term(48) + r3(vrdav, c,j,a,i,d,k) * s1(c,i) * s2(b,d,j,k)
term(49) = term(49) + r3(vrdav, c,j,a,i,d,k) * s1(c,k) * s2(b,d,j,i)
end do 
end do 
end do 
end do 
end do 

term(43) = term(43) * 1.3333333333333333d+0 
term(44) = term(44) * (-0.6666666666666666d+0) 
term(45) = term(45) * 0.3333333333333333d+0 
term(46) = term(46) * (-0.6666666666666666d+0) 
term(47) = term(47) * 1.3333333333333333d+0 
term(48) = term(48) * (-0.6666666666666666d+0) 
term(49) = term(49) * 0.3333333333333333d+0 

do j = 1, nocc 
do d = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do i = 1, nocc 
term(50) = term(50) + r3(vrdav, a,i,c,k,d,j) * s1(b,j) * s2(c,d,i,k)
end do 
end do 
end do 
end do 
end do 

term(50) = term(50) * 0.3333333333333333d+0 

do j = 1, nocc 
do d = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(51) = term(51) + r3(vrdav, a,i,c,k,d,j) * s1(b,j) * s2(c,d,k,i)
term(52) = term(52) + r3(vrdav, c,k,a,i,d,j) * s1(b,j) * s2(c,d,i,k)
end do 
end do 
end do 
end do 
end do 

term(51) = term(51) * (-0.6666666666666666d+0) 
term(52) = term(52) * 0.3333333333333333d+0 

do k = 1, nocc 
do d = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do i = 1, nocc 
term(53) = term(53) + r3(vrdav, a,i,c,j,d,k) * s1(b,j) * s2(c,d,i,k)
term(54) = term(54) + r3(vrdav, a,i,c,j,d,k) * s1(b,i) * s2(c,d,j,k)
term(55) = term(55) + r3(vrdav, a,i,c,j,d,k) * s1(b,i) * s2(c,d,k,j)
term(56) = term(56) + r3(vrdav, a,i,c,j,d,k) * s1(c,i) * s2(b,d,k,j)
term(57) = term(57) + r3(vrdav, a,i,c,j,d,k) * s1(c,i) * s2(b,d,j,k)
term(58) = term(58) + r3(vrdav, a,i,c,j,d,k) * s1(c,j) * s2(b,d,i,k)
term(59) = term(59) + r3(vrdav, a,i,c,j,d,k) * s1(d,i) * s2(b,c,k,j)
term(60) = term(60) + r3(vrdav, a,i,c,j,d,k) * s1(d,i) * s2(b,c,j,k)
term(61) = term(61) + r3(vrdav, a,i,c,j,d,k) * s1(d,j) * s2(b,c,i,k)
term(62) = term(62) + r3(vrdav, a,i,c,j,d,k) * s1(d,k) * s2(b,c,i,j)
term(63) = term(63) + r3(vrdav, a,i,c,j,d,k) * s1(c,k) * s2(b,d,i,j)
end do 
end do 
end do 
end do 
end do 

term(53) = term(53) * (-0.6666666666666666d+0) 
term(54) = term(54) * 1.3333333333333333d+0 
term(55) = term(55) * (-0.6666666666666666d+0) 
term(56) = term(56) * 0.3333333333333333d+0 
term(57) = term(57) * (-0.6666666666666666d+0) 
term(58) = term(58) * 1.3333333333333333d+0 
term(59) = term(59) * (-0.6666666666666666d+0) 
term(60) = term(60) * 0.3333333333333333d+0 
term(61) = term(61) * (-0.6666666666666666d+0) 
term(62) = term(62) * 1.3333333333333333d+0 
term(63) = term(63) * (-0.6666666666666666d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do d = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(64) = term(64) + r3(vrdav, c,k,d,j,a,i) * s1(b,j) * s2(c,d,i,k)
term(65) = term(65) + r3(vrdav, c,j,d,k,a,i) * s1(b,i) * s2(c,d,k,j)
term(66) = term(66) + r3(vrdav, c,j,d,k,a,i) * s1(d,k) * s2(b,c,i,j)
term(67) = term(67) + r3(vrdav, c,j,d,k,a,i) * s1(c,k) * s2(b,d,i,j)
term(68) = term(68) + r3(vrdav, c,j,d,k,a,i) * s1(d,i) * s2(b,c,k,j)
term(69) = term(69) + r3(vrdav, c,j,d,k,a,i) * s1(c,i) * s2(b,d,k,j)
end do 
end do 
end do 
end do 
end do 

term(64) = term(64) * 0.3333333333333333d+0 
term(65) = term(65) * (-0.6666666666666666d+0) 
term(66) = term(66) * 1.3333333333333333d+0 
term(67) = term(67) * (-0.6666666666666666d+0) 
term(68) = term(68) * (-0.6666666666666666d+0) 
term(69) = term(69) * 0.3333333333333333d+0 

do i = 1, nocc 
do k = 1, nocc 
do d = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
term(70) = term(70) + r3(vrdav, c,j,d,k,a,i) * s1(d,j) * s2(b,c,k,i)
term(71) = term(71) + r3(vrdav, c,j,d,k,a,i) * s1(d,k) * s2(b,c,j,i)
term(72) = term(72) + r3(vrdav, c,j,d,k,a,i) * s1(d,j) * s2(b,c,i,k)
term(73) = term(73) + r3(vrdav, c,j,d,k,a,i) * s1(d,i) * s2(b,c,j,k)
end do 
end do 
end do 
end do 
end do 

term(70) = term(70) * 0.3333333333333333d+0 
term(71) = term(71) * (-0.6666666666666666d+0) 
term(72) = term(72) * (-0.6666666666666666d+0) 
term(73) = term(73) * 0.3333333333333333d+0 


    calc_D_vv_gamma_cc3 = 0.d+0 
    do s = 0, 73
    calc_D_vv_gamma_cc3 = calc_D_vv_gamma_cc3 + term(s)
    end do

    end function calc_D_vv_gamma_cc3
    

    



    
    function calc_D_oo_xi_cc3(t2, t1, vldav, nocc, nactive, i,j) 
    double precision :: calc_D_oo_xi_cc3
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    double precision, dimension(:), intent(in) :: vldav
    integer, intent(in) :: i,j 
    
    integer :: s ,a,k,b,l,c 
    double precision, dimension(0:21) :: term 
    term = 0.d+0 
    do a = nocc + 1, nactive 
term(0) = term(0) + l1(vldav, a,j) * t1(a,i)
term(1) = term(1) + l2(vldav, a,j,a,j) * t2(a,a,i,j)
end do 

term(0) = -term(0) 
term(1) = term(1) * 0.49999999999999994d+0 

do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(2) = term(2) + l2(vldav, b,k,a,i) * t2(a,b,j,k)
term(3) = term(3) + l2(vldav, a,i,b,k) * t2(a,b,j,k)
end do 
end do 
end do 

term(2) = term(2) * (-0.49999999999999994d+0) 
term(3) = term(3) * (-0.49999999999999994d+0) 

do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(4) = term(4) + l3(vldav, a,i,c,k,b,l) * t3(nocc, nactive, a,b,c,k,j,l)
end do 
end do 
end do 
end do 
end do 

term(4) = term(4) * (-0.33333333333333326d+0) 

do k = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do l = 1, nocc 
do c = nocc + 1, nactive 
term(5) = term(5) + l3(vldav, b,k,c,l,a,i) * t3(nocc, nactive, a,b,c,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(5) = term(5) * 0.6666666666666665d+0 

do l = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(6) = term(6) + l3(vldav, c,k,b,l,a,i) * t3(nocc, nactive, a,b,c,k,j,l)
term(7) = term(7) + l3(vldav, b,k,c,l,a,i) * t3(nocc, nactive, a,b,c,k,j,l)
end do 
end do 
end do 
end do 
end do 

term(6) = term(6) * (-0.33333333333333326d+0) 
term(7) = term(7) * 0.6666666666666665d+0 

do l = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(8) = term(8) + l3(vldav, a,i,b,k,c,l) * t3(nocc, nactive, a,b,c,k,j,l)
term(9) = term(9) + l3(vldav, b,k,a,i,c,l) * t3(nocc, nactive, a,b,c,k,j,l)
end do 
end do 
end do 
end do 
end do 

term(8) = term(8) * 0.6666666666666665d+0 
term(9) = term(9) * 0.6666666666666665d+0 

do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(10) = term(10) + l3(vldav, c,k,a,i,b,l) * t3(nocc, nactive, a,b,c,k,j,l)
end do 
end do 
end do 
end do 
end do 

term(10) = term(10) * (-0.33333333333333326d+0) 

do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(11) = term(11) + l3(vldav, a,i,c,k,b,l) * t3(nocc, nactive, a,b,c,l,j,k)
end do 
end do 
end do 
end do 
end do 

term(11) = term(11) * 0.6666666666666665d+0 

do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(12) = term(12) + l3(vldav, c,k,a,i,b,l) * t3(nocc, nactive, a,b,c,l,j,k)
end do 
end do 
end do 
end do 
end do 

term(12) = term(12) * 0.6666666666666665d+0 

do l = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
term(13) = term(13) + l3(vldav, b,k,c,l,a,i) * t3(nocc, nactive, a,b,c,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(13) = term(13) * (-1.333333333333333d+0) 

do l = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(14) = term(14) + l3(vldav, a,i,b,k,c,l) * t3(nocc, nactive, a,b,c,l,j,k)
term(15) = term(15) + l3(vldav, b,k,a,i,c,l) * t3(nocc, nactive, a,b,c,j,l,k)
term(16) = term(16) + l3(vldav, b,k,a,i,c,l) * t3(nocc, nactive, a,b,c,l,j,k)
term(17) = term(17) + l3(vldav, a,i,b,k,c,l) * t3(nocc, nactive, a,b,c,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(14) = term(14) * (-0.33333333333333326d+0) 
term(15) = term(15) * 0.6666666666666665d+0 
term(16) = term(16) * (-0.33333333333333326d+0) 
term(17) = term(17) * 0.6666666666666665d+0 

do k = 1, nocc 
do a = nocc + 1, nactive 
do l = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(18) = term(18) + l3(vldav, c,k,b,l,a,i) * t3(nocc, nactive, a,b,c,l,j,k)
term(19) = term(19) + l3(vldav, b,k,c,l,a,i) * t3(nocc, nactive, a,b,c,l,j,k)
end do 
end do 
end do 
end do 
end do 

term(18) = term(18) * 0.6666666666666665d+0 
term(19) = term(19) * (-0.33333333333333326d+0) 

do l = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(20) = term(20) + l3(vldav, b,k,a,i,c,l) * t3(nocc, nactive, a,b,c,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(20) = term(20) * (-1.333333333333333d+0) 

do l = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(21) = term(21) + l3(vldav, a,i,b,k,c,l) * t3(nocc, nactive, a,b,c,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(21) = term(21) * (-1.333333333333333d+0) 


    calc_D_oo_xi_cc3 = 0.d+0 
    do s = 0, 21
    calc_D_oo_xi_cc3 = calc_D_oo_xi_cc3 + term(s)
    end do

    end function calc_D_oo_xi_cc3

      function calc_D_ov_xi_cc3()
            double precision :: calc_D_ov_xi_cc3

            calc_D_ov_xi_cc3 = 0.d+0 

      end function calc_D_ov_xi_cc3
    


    function calc_D_vo_xi_cc3(t2, t1, vldav, nocc, nactive, a,i) 
    double precision :: calc_D_vo_xi_cc3
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    double precision, dimension(:), intent(in) :: vldav 
    integer, intent(in) :: a,i 
    
    integer :: s ,j,b,k,c,l,d 
    double precision, dimension(0:49) :: term 
    term = 0.d+0 
    do j = 1, nocc 
do b = nocc + 1, nactive 
term(0) = term(0) + l1(vldav, b,j) * t2(a,b,i,j)
term(1) = term(1) + l2(vldav, b,j,b,j) * t3(nocc, nactive, a,b,b,i,j,j)
term(2) = term(2) + l2(vldav, b,j,b,j) * t3(nocc, nactive, a,b,b,j,i,j)
term(3) = term(3) + l2(vldav, b,j,b,j) * t1(a,j) * t2(b,b,i,j)
term(4) = term(4) + l2(vldav, b,j,b,j) * t1(b,i) * t2(a,b,j,j)
end do 
end do 

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * 0.5d+0 
term(3) = term(3) * 0.49999999999999994d+0 
term(4) = term(4) * 0.49999999999999994d+0 

do b = nocc + 1, nactive 
do j = 1, nocc 
term(5) = term(5) + l1(vldav, b,j) * t2(a,b,j,i)
end do 
end do 

term(5) = -term(5) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(6) = term(6) + l3(vldav, b,j,c,k,d,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(7) = term(7) + l3(vldav, b,j,c,k,d,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(8) = term(8) + l3(vldav, b,j,c,k,d,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(9) = term(9) + l3(vldav, b,j,c,k,d,l) * t2(a,d,j,l) * t2(b,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(6) = term(6) * 0.6666666666666666d+0 
term(7) = term(7) * (-1.3333333333333333d+0) 
term(8) = term(8) * 0.6666666666666666d+0 
term(9) = term(9) * (-1.3333333333333333d+0) 

term(10) = term(10) + l1(vldav, a,i)


do k = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
term(11) = term(11) + l2(vldav, b,j,c,k) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 


do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(12) = term(12) + l2(vldav, c,k,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
term(13) = term(13) + l2(vldav, b,j,c,k) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 

term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(14) = term(14) + l2(vldav, c,k,b,j) * t1(a,j) * t2(b,c,i,k)
term(15) = term(15) + l2(vldav, c,k,b,j) * t1(b,i) * t2(a,c,j,k)
end do 
end do 
end do 
end do 

term(14) = term(14) * (-0.49999999999999994d+0) 
term(15) = term(15) * (-0.49999999999999994d+0) 

do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(16) = term(16) + l2(vldav, b,j,c,k) * t1(a,j) * t2(b,c,i,k)
term(17) = term(17) + l2(vldav, b,j,c,k) * t1(b,i) * t2(a,c,j,k)
end do 
end do 
end do 
end do 

term(16) = term(16) * (-0.49999999999999994d+0) 
term(17) = term(17) * (-0.49999999999999994d+0) 

do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do d = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
term(18) = term(18) + l3(vldav, c,j,d,k,b,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(19) = term(19) + l3(vldav, c,j,d,k,b,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(20) = term(20) + l3(vldav, c,j,d,k,b,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(21) = term(21) + l3(vldav, c,j,d,k,b,l) * t2(a,d,j,k) * t2(b,c,i,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(18) = term(18) * (-0.3333333333333333d+0) 
term(19) = term(19) * 0.6666666666666666d+0 
term(20) = term(20) * (-0.3333333333333333d+0) 
term(21) = term(21) * 0.6666666666666666d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do k = 1, nocc 
do d = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(22) = term(22) + l3(vldav, c,k,d,l,b,j) * t2(a,c,j,l) * t2(b,d,i,k)
term(23) = term(23) + l3(vldav, c,k,d,l,b,j) * t2(a,c,j,k) * t2(b,d,i,l)
term(24) = term(24) + l3(vldav, c,k,d,l,b,j) * t2(a,d,j,k) * t2(b,c,i,l)
term(25) = term(25) + l3(vldav, c,k,d,l,b,j) * t2(a,d,j,l) * t2(b,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(22) = term(22) * 0.6666666666666666d+0 
term(23) = term(23) * (-1.3333333333333333d+0) 
term(24) = term(24) * 0.6666666666666666d+0 
term(25) = term(25) * (-1.3333333333333333d+0) 

do j = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(26) = term(26) + l3(vldav, c,k,b,l,d,j) * t2(a,c,j,l) * t2(b,d,i,k)
term(27) = term(27) + l3(vldav, c,k,b,l,d,j) * t2(a,c,j,k) * t2(b,d,i,l)
term(28) = term(28) + l3(vldav, c,k,b,l,d,j) * t2(a,d,j,k) * t2(b,c,i,l)
term(29) = term(29) + l3(vldav, b,k,c,l,d,j) * t2(a,d,j,k) * t2(b,c,i,l)
term(30) = term(30) + l3(vldav, b,k,c,l,d,j) * t2(a,c,j,k) * t2(b,d,i,l)
term(31) = term(31) + l3(vldav, c,k,b,l,d,j) * t2(a,d,j,l) * t2(b,c,i,k)
term(32) = term(32) + l3(vldav, b,k,c,l,d,j) * t2(a,c,j,l) * t2(b,d,i,k)
term(33) = term(33) + l3(vldav, b,k,c,l,d,j) * t2(a,d,j,l) * t2(b,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(26) = term(26) * (-0.3333333333333333d+0) 
term(27) = term(27) * 0.6666666666666666d+0 
term(28) = term(28) * (-0.3333333333333333d+0) 
term(29) = term(29) * 0.6666666666666666d+0 
term(30) = term(30) * (-0.3333333333333333d+0) 
term(31) = term(31) * 0.6666666666666666d+0 
term(32) = term(32) * 0.6666666666666666d+0 
term(33) = term(33) * (-0.3333333333333333d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
term(34) = term(34) + l3(vldav, c,j,b,k,d,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(35) = term(35) + l3(vldav, c,j,b,k,d,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(36) = term(36) + l3(vldav, c,j,b,k,d,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(37) = term(37) + l3(vldav, c,j,b,k,d,l) * t2(a,d,j,l) * t2(b,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(34) = term(34) * 0.6666666666666666d+0 
term(35) = term(35) * (-0.3333333333333333d+0) 
term(36) = term(36) * (-0.3333333333333333d+0) 
term(37) = term(37) * 0.6666666666666666d+0 

do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do d = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(38) = term(38) + l3(vldav, c,k,d,j,b,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(39) = term(39) + l3(vldav, c,k,d,j,b,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(40) = term(40) + l3(vldav, c,k,d,j,b,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(41) = term(41) + l3(vldav, c,k,d,j,b,l) * t2(a,d,j,l) * t2(b,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(38) = term(38) * (-0.3333333333333333d+0) 
term(39) = term(39) * 0.6666666666666666d+0 
term(40) = term(40) * (-0.3333333333333333d+0) 
term(41) = term(41) * 0.6666666666666666d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(42) = term(42) + l3(vldav, c,k,b,j,d,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(43) = term(43) + l3(vldav, c,k,b,j,d,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(44) = term(44) + l3(vldav, c,k,b,j,d,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(45) = term(45) + l3(vldav, b,k,c,j,d,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(46) = term(46) + l3(vldav, b,k,c,j,d,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(47) = term(47) + l3(vldav, c,k,b,j,d,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(48) = term(48) + l3(vldav, b,k,c,j,d,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(49) = term(49) + l3(vldav, b,k,c,j,d,l) * t2(a,d,j,l) * t2(b,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(42) = term(42) * 0.6666666666666666d+0 
term(43) = term(43) * (-1.3333333333333333d+0) 
term(44) = term(44) * 0.6666666666666666d+0 
term(45) = term(45) * (-0.3333333333333333d+0) 
term(46) = term(46) * 0.6666666666666666d+0 
term(47) = term(47) * (-1.3333333333333333d+0) 
term(48) = term(48) * (-0.3333333333333333d+0) 
term(49) = term(49) * 0.6666666666666666d+0 


    calc_D_vo_xi_cc3 = 0.d+0 
    do s = 0, 49
    calc_D_vo_xi_cc3 = calc_D_vo_xi_cc3 + term(s)
    end do

    end function calc_D_vo_xi_cc3
    
    function calc_D_vv_xi_cc3(t2, t1, vldav, nocc, nactive, a,b) 
    double precision :: calc_D_vv_xi_cc3
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    double precision, dimension(:), intent(in) :: vldav
    integer, intent(in) :: a,b 
    
    integer :: s ,i,j,c,k,d 
    double precision, dimension(0:21) :: term 
    term = 0.d+0 
    do i = 1, nocc 
term(0) = term(0) + l1(vldav, b,i) * t1(a,i)
term(1) = term(1) + l2(vldav, b,i,b,i) * t2(a,b,i,i)
end do 

term(1) = term(1) * (-0.49999999999999994d+0) 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(2) = term(2) + l2(vldav, c,j,a,i) * t2(b,c,i,j)
end do 
end do 
end do 

term(2) = term(2) * 0.49999999999999994d+0 

do j = 1, nocc 
do c = nocc + 1, nactive 
do i = 1, nocc 
term(3) = term(3) + l2(vldav, a,i,c,j) * t2(b,c,i,j)
end do 
end do 
end do 

term(3) = term(3) * 0.49999999999999994d+0 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do d = nocc + 1, nactive 
term(4) = term(4) + l3(vldav, c,j,d,k,a,i) * t3(nocc, nactive, b,c,d,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(4) = term(4) * 0.33333333333333326d+0 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do d = nocc + 1, nactive 
term(5) = term(5) + l3(vldav, c,j,d,k,a,i) * t3(nocc, nactive, b,c,d,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(5) = term(5) * (-0.6666666666666665d+0) 

do k = 1, nocc 
do j = 1, nocc 
do d = nocc + 1, nactive 
do c = nocc + 1, nactive 
do i = 1, nocc 
term(6) = term(6) + l3(vldav, c,k,a,i,d,j) * t3(nocc, nactive, b,c,d,j,i,k)
term(7) = term(7) + l3(vldav, a,i,c,k,d,j) * t3(nocc, nactive, b,c,d,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(6) = term(6) * 0.33333333333333326d+0 
term(7) = term(7) * 0.33333333333333326d+0 

do k = 1, nocc 
do d = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do i = 1, nocc 
term(8) = term(8) + l3(vldav, c,j,a,i,d,k) * t3(nocc, nactive, b,c,d,j,i,k)
term(9) = term(9) + l3(vldav, a,i,c,j,d,k) * t3(nocc, nactive, b,c,d,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(8) = term(8) * (-0.6666666666666665d+0) 
term(9) = term(9) * (-0.6666666666666665d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
term(10) = term(10) + l3(vldav, c,k,d,j,a,i) * t3(nocc, nactive, b,c,d,j,i,k)
term(11) = term(11) + l3(vldav, c,j,d,k,a,i) * t3(nocc, nactive, b,c,d,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(10) = term(10) * 0.33333333333333326d+0 
term(11) = term(11) * (-0.6666666666666665d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do k = 1, nocc 
term(12) = term(12) + l3(vldav, c,k,d,j,a,i) * t3(nocc, nactive, b,c,d,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(12) = term(12) * (-0.6666666666666665d+0) 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do j = 1, nocc 
term(13) = term(13) + l3(vldav, c,j,d,k,a,i) * t3(nocc, nactive, b,c,d,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(13) = term(13) * 1.333333333333333d+0 

do k = 1, nocc 
do i = 1, nocc 
do d = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
term(14) = term(14) + l3(vldav, c,j,a,i,d,k) * t3(nocc, nactive, b,c,d,j,k,i)
term(15) = term(15) + l3(vldav, a,i,c,j,d,k) * t3(nocc, nactive, b,c,d,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(14) = term(14) * 0.33333333333333326d+0 
term(15) = term(15) * 0.33333333333333326d+0 

do j = 1, nocc 
do i = 1, nocc 
do d = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
term(16) = term(16) + l3(vldav, c,k,a,i,d,j) * t3(nocc, nactive, b,c,d,j,k,i)
term(17) = term(17) + l3(vldav, a,i,c,k,d,j) * t3(nocc, nactive, b,c,d,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(16) = term(16) * (-0.6666666666666665d+0) 
term(17) = term(17) * (-0.6666666666666665d+0) 

do k = 1, nocc 
do j = 1, nocc 
do d = nocc + 1, nactive 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(18) = term(18) + l3(vldav, c,j,a,i,d,k) * t3(nocc, nactive, b,c,d,i,k,j)
term(19) = term(19) + l3(vldav, a,i,c,j,d,k) * t3(nocc, nactive, b,c,d,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(18) = term(18) * (-0.6666666666666665d+0) 
term(19) = term(19) * (-0.6666666666666665d+0) 

do k = 1, nocc 
do d = nocc + 1, nactive 
do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
term(20) = term(20) + l3(vldav, c,j,a,i,d,k) * t3(nocc, nactive, b,c,d,i,j,k)
term(21) = term(21) + l3(vldav, a,i,c,j,d,k) * t3(nocc, nactive, b,c,d,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(20) = term(20) * 1.333333333333333d+0 
term(21) = term(21) * 1.333333333333333d+0 


    calc_D_vv_xi_cc3 = 0.d+0 
    do s = 0, 21
    calc_D_vv_xi_cc3 = calc_D_vv_xi_cc3 + term(s)
    end do

    end function calc_D_vv_xi_cc3

end module density_gr_exc_functions
