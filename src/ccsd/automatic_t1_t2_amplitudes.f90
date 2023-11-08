module automatic_t1_t2_amplitudes

      use ccsd_transformed_integrals
      use t1_transformed_int
      use gparam
      use arithmetic
      use cc_gparams

      implicit none


      real(F64), dimension(:,:,:,:), allocatable, private :: x1
      real(F64), dimension(:,:), allocatable, private :: x2
      real(F64), dimension(:,:), allocatable, private :: x3
      real(F64), dimension(:,:,:,:), allocatable, private :: x5
      real(F64), dimension(:,:,:,:), allocatable, private :: x6
      real(F64), dimension(:,:), allocatable, private :: x7
      real(F64), dimension(:,:,:,:), allocatable, private :: x9
      real(F64), dimension(:,:,:,:), allocatable, private :: x12
      real(F64), dimension(:,:), allocatable, private :: x14
      real(F64), dimension(:,:,:,:), allocatable, private :: x16


contains      


      function automatic_t1(t2, t1, nocc, nactive, a, i)
            double precision :: automatic_t1
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            integer, intent(in) :: a, i 
            integer :: s ,j,b,k,c 
            double precision, dimension(0:21) :: term 


            term = 0.d+0 
            term = 0.d+0
            do j = 1, nocc
                  do b = nocc+1, nactive
                        term(0) = term(0) + vvoo(b,a,j,i) * t1(b,j)
                        term(1) = term(1) + vovo(b,j,a,i) * t1(b,j)
                  end do
            end do

            term(0) = term(0) * (-1.0d+0)
            term(1) = term(1) * 2.0d+0

            do k = 1, nocc
                  do b = nocc+1,nactive
                        do j = 1, nocc
                              do c = nocc+1,nactive
                                    term(2) = term(2) + vovo(c,j,b,k) * t1(c,k) * t1(a,j) * t1(b,i)
                                    term(3) = term(3) + vovo(c,j,b,k) * t1(c,k) * t2(a,b,j,i)
                              end do
                        end do
                  end do
            end do

            term(2) = term(2) * 1.0d+0
            term(3) = term(3) * 1.0d+0

            do j = 1, nocc
                  do b = nocc + 1, nactive
                        do k = 1, nocc
                              do c = nocc + 1, nactive
                                    term(4) = term(4) + vovo(c,k,b,j) * t1(c,k) * t1(a,j) * t1(b,i)
                                    term(5) = term(5) + vovo(c,k,b,j) * t1(c,k) * t2(a,b,i,j)
                                    term(6) = term(6) + vovo(c,k,b,j) * t1(c,k) * t2(a,b,j,i)
                              end do
                        end do
                  end do
            end do

            term(4) = term(4) * (-2.0d+0)
            term(5) = term(5) * 2.5d+0
            term(6) = term(6) * (-2.0d+0)

            do k = 1, nocc
                  do j = 1, nocc
                        do c = nocc + 1, nactive
                              do b = nocc+1,nactive
                                    term(7) = term(7) + vovo(c,k,b,j) * t1(b,i) * t2(a,c,j,k)
                                    term(8) = term(8) + vovo(c,k,b,j) * t1(a,j) * t2(b,c,i,k)
                                    term(9) = term(9) + vovo(c,k,b,j) * t1(b,j) * t2(a,c,i,k)
                              end do
                        end do
                  end do
            end do

            term(7) = term(7) * (-2.0d+0)
            term(8) = term(8) * (-2.0d+0)
            term(9) = term(9) * 1.5d+0

            do k = 1, nocc
                  do c = nocc+1,nactive
                        do b = nocc+1,nactive
                              do j = 1, nocc
                                    term(10) = term(10) + vovo(c,j,b,k) * t1(b,i) * t2(a,c,j,k)
                                    term(11) = term(11) + vovo(c,j,b,k) * t1(a,j) * t2(b,c,i,k)
                              end do
                        end do
                  end do
            end do

            term(10) = term(10) * 1.0d+0
            term(11) = term(11) * 1.0d+0

            do k = 1, nocc
                  do j = 1, nocc
                        do b = nocc + 1, nactive
                              do c = nocc + 1, nactive
                                    term(12) = term(12) + vovo(c,k,b,j) * t1(c,j) * t2(a,b,i,k)
                              end do
                        end do
                  end do
            end do

            term(12) = term(12) * (-1.0d+0)

            do j = 1, nocc
                  do c = nocc + 1, nactive
                        do b = nocc + 1, nactive
                              do k = 1, nocc
                                    term(13) = term(13) + vovo(c,k,b,j) * t1(b,k) * t2(a,c,i,j)
                              end do
                        end do
                  end do
            end do

            term(13) = term(13) * (-1.0d+0)

            do k = 1, nocc
                  do j = 1, nocc
                        do b = nocc + 1, nactive
                              term(14) = term(14) + vooo(b,j,k,i) * t1(b,k) * t1(a,j)
                        end do
                  end do
            end do

            term(14) = term(14) * 1.0d+0

            do j = 1, nocc
                  do c = nocc + 1, nactive
                        do b = nocc + 1, nactive
                              term(15) = term(15) + vvvo(b,a,c,j) * t1(b,j) * t1(c,i)
                              term(16) = term(16) + vvvo(b,a,c,j) * t1(c,j) * t1(b,i)
                              term(17) = term(17) + vvvo(b,a,c,j) * t2(b,c,j,i)
                              term(18) = term(18) + vvvo(b,a,c,j) * t2(b,c,i,j)
                        end do
                  end do
            end do

            term(15) = term(15) * (-1.0d+0)
            term(16) = term(16) * 2.0d+0
            term(17) = term(17) * (-1.0d+0)
            term(18) = term(18) * 2.0d+0

            do j = 1, nocc
                  do k = 1, nocc
                        do b = nocc + 1, nactive
                              term(19) = term(19) + vooo(b,k,j,i) * t1(b,k) * t1(a,j)
                        end do
                  end do
            end do

            term(19) = term(19) * (-2.0d+0)

            do k = 1, nocc
                  do b = nocc + 1, nactive
                        do j = 1, nocc
                              term(20) = term(20) + vooo(b,k,j,i) * t2(a,b,j,k)
                              term(21) = term(21) + vooo(b,j,k,i) * t2(a,b,j,k)
                        end do
                  end do
            end do

            term(20) = term(20) * (-2.0d+0)
            term(21) = term(21) * 1.0d+0


            automatic_t1 = 0.d+0 
            do s = 0, 21
                  automatic_t1 = automatic_t1 + term(s)
            end do

      end function automatic_t1

      ! function automatic_t2(t2, t1, nocc, nactive, a, i, b, j) 
      !       double precision :: automatic_t2
      !       integer, intent(in)  :: nocc, nactive
      !       double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
      !       double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
      !       integer, intent(in) :: a, i, b, j 
      !       integer :: s ,c,k,d,l 
      !       double precision, dimension(0:90) :: term 
      !       term = 0.d+0 
      !       term = 0.d+0
      !       term(0) = term(0) + vovo(b,j,a,i)

      !       term(0) = term(0) * 1.0d+0

      !       do c = nocc + 1, nactive
      !             term(1) = term(1) + vvvo(c,b,a,i) * t1(c,j)
      !             term(2) = term(2) + vvvo(c,a,b,j) * t1(c,i)
      !       end do

      !       term(1) = term(1) * 1.0d+0
      !       term(2) = term(2) * 1.0d+0

      !       do k = 1, nocc
      !             term(3) = term(3) + vooo(a,i,k,j) * t1(b,k)
      !             term(4) = term(4) + vooo(b,j,k,i) * t1(a,k)
      !       end do

      !       term(3) = term(3) * (-1.0d+0)
      !       term(4) = term(4) * (-1.0d+0)

      !       do k = 1, nocc
      !             do d = nocc + 1, nactive
      !                   do c = nocc + 1, nactive
      !                         term(5) = term(5) + vvvo(c,a,d,k) * t1(b,k) * t1(c,i) * t1(d,j)
      !                         term(6) = term(6) + vvvo(c,b,d,k) * t1(a,k) * t1(d,i) * t1(c,j)
      !                         term(7) = term(7) + vvvo(c,a,d,k) * t1(d,j) * t2(b,c,k,i)
      !                         term(8) = term(8) + vvvo(c,b,d,k) * t1(d,i) * t2(a,c,k,j)
      !                         term(9) = term(9) + vvvo(c,b,d,k) * t1(d,j) * t2(a,c,i,k)
      !                         term(10) = term(10) + vvvo(c,a,d,k) * t1(d,i) * t2(b,c,j,k)
      !                         term(11) = term(11) + vvvo(c,b,d,k) * t1(c,k) * t2(a,d,i,j)
      !                         term(12) = term(12) + vvvo(c,b,d,k) * t1(c,j) * t2(a,d,i,k)
      !                         term(13) = term(13) + vvvo(c,a,d,k) * t1(c,k) * t2(b,d,j,i)
      !                         term(14) = term(14) + vvvo(c,a,d,k) * t1(c,i) * t2(b,d,j,k)
      !                         term(15) = term(15) + vvvo(c,b,d,k) * t1(d,k) * t2(a,c,i,j)
      !                         term(16) = term(16) + vvvo(c,a,d,k) * t1(d,k) * t2(b,c,j,i)
      !                         term(17) = term(17) + vvvo(c,b,d,k) * t1(c,j) * t2(a,d,k,i)
      !                         term(18) = term(18) + vvvo(c,a,d,k) * t1(c,i) * t2(b,d,k,j)
      !                         term(19) = term(19) + vvvo(c,a,d,k) * t1(b,k) * t2(c,d,i,j)
      !                         term(20) = term(20) + vvvo(c,b,d,k) * t1(a,k) * t2(c,d,j,i)
      !                   end do
      !             end do
      !       end do

      !       term(5) = term(5) * (-1.0d+0)
      !       term(6) = term(6) * (-1.0d+0)
      !       term(7) = term(7) * (-1.0d+0)
      !       term(8) = term(8) * (-1.0d+0)
      !       term(9) = term(9) * (-1.0d+0)
      !       term(10) = term(10) * (-1.0d+0)
      !       term(11) = term(11) * (-1.0d+0)
      !       term(12) = term(12) * 2.0d+0
      !       term(13) = term(13) * (-1.0d+0)
      !       term(14) = term(14) * 2.0d+0
      !       term(15) = term(15) * 2.0d+0
      !       term(16) = term(16) * 2.0d+0
      !       term(17) = term(17) * (-1.0d+0)
      !       term(18) = term(18) * (-1.0d+0)
      !       term(19) = term(19) * (-1.0d+0)
      !       term(20) = term(20) * (-1.0d+0)

      !       do k = 1, nocc
      !             do c = nocc + 1, nactive
      !                   do l = 1, nocc
      !                         do d = nocc + 1, nactive
      !                               term(21) = term(21) + vovo(d,l,c,k) * t1(d,l) * t1(b,k) * t2(a,c,i,j)
      !                               term(22) = term(22) + vovo(d,l,c,k) * t1(a,k) * t1(d,j) * t2(b,c,l,i)
      !                               term(23) = term(23) + vovo(d,l,c,k) * t1(d,l) * t1(a,k) * t2(b,c,j,i)
      !                               term(24) = term(24) + vovo(d,l,c,k) * t1(d,l) * t1(c,j) * t2(a,b,i,k)
      !                               term(25) = term(25) + vovo(d,l,c,k) * t1(d,l) * t1(c,i) * t2(a,b,k,j)
      !                               term(26) = term(26) + vovo(d,l,c,k) * t1(b,l) * t1(d,j) * t2(a,c,k,i)
      !                               term(27) = term(27) + vovo(d,l,c,k) * t1(a,k) * t1(b,l) * t1(c,i) * t1(d,j)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(21) = term(21) * (-2.0d+0)
      !       term(22) = term(22) * 1.0d+0
      !       term(23) = term(23) * (-2.0d+0)
      !       term(24) = term(24) * (-2.0d+0)
      !       term(25) = term(25) * (-2.0d+0)
      !       term(26) = term(26) * 1.0d+0
      !       term(27) = term(27) * 1.0d+0

      !       do k = 1, nocc
      !             do l = 1, nocc
      !                   do c = nocc + 1, nactive
      !                         term(28) = term(28) + vooo(c,l,k,i) * t1(a,k) * t1(b,l) * t1(c,j)
      !                         term(29) = term(29) + vooo(c,k,l,j) * t1(c,l) * t2(a,b,i,k)
      !                         term(30) = term(30) + vooo(c,l,k,j) * t1(c,l) * t2(a,b,i,k)
      !                         term(31) = term(31) + vooo(c,l,k,i) * t1(c,l) * t2(a,b,k,j)
      !                   end do
      !             end do
      !       end do

      !       term(28) = term(28) * 1.0d+0
      !       term(29) = term(29) * 1.0d+0
      !       term(30) = term(30) * (-2.0d+0)
      !       term(31) = term(31) * (-2.0d+0)

      !       do l = 1, nocc
      !             do k = 1, nocc
      !                   do c = nocc + 1, nactive
      !                         term(32) = term(32) + vooo(c,k,l,j) * t1(a,k) * t1(b,l) * t1(c,i)
      !                         term(33) = term(33) + vooo(c,l,k,i) * t1(c,j) * t2(a,b,k,l)
      !                         term(34) = term(34) + vooo(c,k,l,i) * t1(c,l) * t2(a,b,k,j)
      !                         term(35) = term(35) + vooo(c,k,l,j) * t1(c,i) * t2(a,b,k,l)
      !                   end do
      !             end do
      !       end do

      !       term(32) = term(32) * 1.0d+0
      !       term(33) = term(33) * 1.0d+0
      !       term(34) = term(34) * 1.0d+0
      !       term(35) = term(35) * 1.0d+0

      !       do l = 1, nocc
      !             do k = 1, nocc
      !                   do c = nocc + 1, nactive
      !                         do d = nocc + 1, nactive
      !                               term(36) = term(36) + vovo(d,l,c,k) * t1(c,i) * t1(d,j) * t2(a,b,k,l)
      !                               term(37) = term(37) + vovo(d,k,c,l) * t1(d,l) * t1(c,j) * t2(a,b,i,k)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(36) = term(36) * 1.0d+0
      !       term(37) = term(37) * 1.0d+0

      !       do k = 1, nocc
      !             do l = 1, nocc
      !                   term(38) = term(38) + oooo(l,j,k,i) * t1(a,k) * t1(b,l)
      !             end do
      !       end do

      !       term(38) = term(38) * 1.0d+0

      !       do c = nocc + 1, nactive
      !             do d = nocc + 1, nactive
      !                   term(39) = term(39) + vvvv(d,b,c,a) * t1(c,i) * t1(d,j)
      !             end do
      !       end do

      !       term(39) = term(39) * 1.0d+0

      !       do l = 1, nocc
      !             do d = nocc + 1, nactive
      !                   do c = nocc + 1, nactive
      !                         do k = 1, nocc
      !                               term(40) = term(40) + vovo(d,k,c,l) * t2(a,b,k,j) * t2(c,d,i,l)
      !                               term(41) = term(41) + vovo(d,k,c,l) * t2(a,d,i,l) * t2(b,c,k,j)
      !                               term(42) = term(42) + vovo(d,k,c,l) * t2(a,c,k,i) * t2(b,d,j,l)
      !                               term(43) = term(43) + vovo(d,k,c,l) * t2(a,c,i,j) * t2(b,d,k,l)
      !                               term(44) = term(44) + vovo(d,k,c,l) * t2(a,d,k,l) * t2(b,c,j,i)
      !                               term(45) = term(45) + vovo(d,k,c,l) * t1(a,k) * t1(c,i) * t2(b,d,j,l)
      !                               term(46) = term(46) + vovo(d,k,c,l) * t1(b,k) * t1(c,j) * t2(a,d,i,l)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(40) = term(40) * 1.0d+0
      !       term(41) = term(41) * 1.0d+0
      !       term(42) = term(42) * 1.0d+0
      !       term(43) = term(43) * 1.0d+0
      !       term(44) = term(44) * 1.0d+0
      !       term(45) = term(45) * 1.0d+0
      !       term(46) = term(46) * 1.0d+0

      !       do c = nocc + 1, nactive
      !             do k = 1, nocc
      !                   term(47) = term(47) + vvoo(c,b,k,i) * t2(a,c,k,j)
      !                   term(48) = term(48) + vvoo(c,a,k,j) * t2(b,c,k,i)
      !                   term(49) = term(49) + vovo(c,k,a,i) * t2(b,c,k,j)
      !                   term(50) = term(50) + vovo(c,k,b,j) * t2(a,c,k,i)
      !             end do
      !       end do

      !       term(47) = term(47) * (-1.0d+0)
      !       term(48) = term(48) * (-1.0d+0)
      !       term(49) = term(49) * (-1.0d+0)
      !       term(50) = term(50) * (-1.0d+0)

      !       do k = 1, nocc
      !             do d = nocc + 1, nactive
      !                   do c = nocc + 1, nactive
      !                         do l = 1, nocc
      !                               term(51) = term(51) + vovo(d,l,c,k) * t2(a,d,k,j) * t2(b,c,l,i)
      !                               term(52) = term(52) + vovo(d,l,c,k) * t2(a,c,k,i) * t2(b,d,l,j)
      !                               term(53) = term(53) + vovo(d,l,c,k) * t1(b,l) * t1(c,i) * t2(a,d,k,j)
      !                               term(54) = term(54) + vovo(d,l,c,k) * t1(a,k) * t1(b,l) * t2(c,d,i,j)
      !                               term(55) = term(55) + vovo(d,l,c,k) * t1(a,k) * t1(c,i) * t2(b,d,l,j)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(51) = term(51) * 1.0d+0
      !       term(52) = term(52) * 1.0d+0
      !       term(53) = term(53) * 1.0d+0
      !       term(54) = term(54) * 1.0d+0
      !       term(55) = term(55) * 1.0d+0

      !       do l = 1, nocc
      !             do k = 1, nocc
      !                   do d = nocc + 1, nactive
      !                         do c = nocc + 1, nactive
      !                               term(56) = term(56) + vovo(d,l,c,k) * t2(a,b,k,l) * t2(c,d,i,j)
      !                               term(57) = term(57) + vovo(d,k,c,l) * t2(a,b,i,k) * t2(c,d,j,l)
      !                               term(58) = term(58) + vovo(d,l,c,k) * t2(a,b,i,k) * t2(c,d,j,l)
      !                               term(59) = term(59) + vovo(d,l,c,k) * t2(a,b,k,j) * t2(c,d,i,l)
      !                               term(60) = term(60) + vovo(d,l,c,k) * t2(a,c,i,j) * t2(b,d,k,l)
      !                               term(61) = term(61) + vovo(d,l,c,k) * t2(a,d,k,l) * t2(b,c,j,i)
      !                               term(62) = term(62) + vovo(d,l,c,k) * t2(a,d,i,k) * t2(b,c,j,l)
      !                               term(63) = term(63) + vovo(d,l,c,k) * t2(a,c,i,l) * t2(b,d,j,k)
      !                               term(64) = term(64) + vovo(d,l,c,k) * t2(a,c,i,k) * t2(b,d,j,l)
      !                               term(65) = term(65) + vovo(d,l,c,k) * t2(a,d,i,l) * t2(b,c,j,k)
      !                               term(66) = term(66) + vovo(d,l,c,k) * t2(a,c,k,i) * t2(b,d,j,l)
      !                               term(67) = term(67) + vovo(d,l,c,k) * t2(a,d,i,l) * t2(b,c,k,j)
      !                               term(68) = term(68) + vovo(d,l,c,k) * t1(a,k) * t1(c,i) * t2(b,d,j,l)
      !                               term(69) = term(69) + vovo(d,l,c,k) * t1(b,k) * t1(c,j) * t2(a,d,i,l)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(56) = term(56) * 1.0d+0
      !       term(57) = term(57) * 1.0d+0
      !       term(58) = term(58) * (-2.0d+0)
      !       term(59) = term(59) * (-2.0d+0)
      !       term(60) = term(60) * (-2.0d+0)
      !       term(61) = term(61) * (-2.0d+0)
      !       term(62) = term(62) * (-1.0d+0)
      !       term(63) = term(63) * (-1.0d+0)
      !       term(64) = term(64) * 2.0d+0
      !       term(65) = term(65) * 2.0d+0
      !       term(66) = term(66) * (-2.0d+0)
      !       term(67) = term(67) * (-2.0d+0)
      !       term(68) = term(68) * (-2.0d+0)
      !       term(69) = term(69) * (-2.0d+0)

      !       do l = 1, nocc
      !             do c = nocc + 1, nactive
      !                   do k = 1, nocc
      !                         do d = nocc + 1, nactive
      !                               term(70) = term(70) + vovo(d,k,c,l) * t1(d,l) * t1(b,k) * t2(a,c,i,j)
      !                               term(71) = term(71) + vovo(d,k,c,l) * t1(d,l) * t1(a,k) * t2(b,c,j,i)
      !                               term(72) = term(72) + vovo(d,k,c,l) * t1(d,l) * t1(c,i) * t2(a,b,k,j)
      !                         end do
      !                   end do
      !             end do
      !       end do

      !       term(70) = term(70) * 1.0d+0
      !       term(71) = term(71) * 1.0d+0
      !       term(72) = term(72) * 1.0d+0

      !       do l = 1, nocc
      !             do k = 1, nocc
      !                   term(73) = term(73) + oooo(l,j,k,i) * t2(a,b,k,l)
      !             end do
      !       end do

      !       term(73) = term(73) * 1.0d+0

      !       do d = nocc + 1, nactive
      !             do c = nocc + 1, nactive
      !                   term(74) = term(74) + vvvv(d,b,c,a) * t2(c,d,i,j)
      !             end do
      !       end do

      !       term(74) = term(74) * 1.0d+0

      !       do c = nocc + 1, nactive
      !             do k = 1, nocc
      !                   do l = 1, nocc
      !                         term(75) = term(75) + vooo(c,l,k,i) * t1(b,l) * t2(a,c,k,j)
      !                         term(76) = term(76) + vooo(c,l,k,i) * t1(a,k) * t2(b,c,l,j)
      !                   end do
      !             end do
      !       end do

      !       term(75) = term(75) * 1.0d+0
      !       term(76) = term(76) * 1.0d+0

      !       do l = 1, nocc
      !             do c = nocc + 1, nactive
      !                   do k = 1, nocc
      !                         term(77) = term(77) + vooo(c,k,l,i) * t1(a,k) * t2(b,c,j,l)
      !                         term(78) = term(78) + vooo(c,k,l,j) * t1(b,k) * t2(a,c,i,l)
      !                         term(79) = term(79) + vooo(c,l,k,j) * t1(b,k) * t2(a,c,i,l)
      !                         term(80) = term(80) + vooo(c,l,k,i) * t1(a,k) * t2(b,c,j,l)
      !                   end do
      !             end do
      !       end do

      !       term(77) = term(77) * 1.0d+0
      !       term(78) = term(78) * 1.0d+0
      !       term(79) = term(79) * (-2.0d+0)
      !       term(80) = term(80) * (-2.0d+0)

      !       do c = nocc + 1, nactive
      !             do l = 1, nocc
      !                   do k = 1, nocc
      !                         term(81) = term(81) + vooo(c,k,l,j) * t1(a,k) * t2(b,c,l,i)
      !                         term(82) = term(82) + vooo(c,k,l,j) * t1(b,l) * t2(a,c,k,i)
      !                   end do
      !             end do
      !       end do

      !       term(81) = term(81) * 1.0d+0
      !       term(82) = term(82) * 1.0d+0

      !       do k = 1, nocc
      !             do c = nocc + 1, nactive
      !                   term(83) = term(83) + vovo(c,k,a,i) * t1(b,k) * t1(c,j)
      !                   term(84) = term(84) + vovo(c,k,b,j) * t1(a,k) * t1(c,i)
      !                   term(85) = term(85) + vvoo(c,b,k,i) * t1(a,k) * t1(c,j)
      !                   term(86) = term(86) + vvoo(c,a,k,j) * t1(b,k) * t1(c,i)
      !                   term(87) = term(87) + vvoo(c,a,k,i) * t2(b,c,j,k)
      !                   term(88) = term(88) + vvoo(c,b,k,j) * t2(a,c,i,k)
      !                   term(89) = term(89) + vovo(c,k,a,i) * t2(b,c,j,k)
      !                   term(90) = term(90) + vovo(c,k,b,j) * t2(a,c,i,k)
      !             end do
      !       end do

      !       term(83) = term(83) * (-1.0d+0)
      !       term(84) = term(84) * (-1.0d+0)
      !       term(85) = term(85) * (-1.0d+0)
      !       term(86) = term(86) * (-1.0d+0)
      !       term(87) = term(87) * (-1.0d+0)
      !       term(88) = term(88) * (-1.0d+0)
      !       term(89) = term(89) * 2.0d+0
      !       term(90) = term(90) * 2.0d+0


      !       automatic_t2 = 0.d+0 
      !       do s = 0, 90
      !             automatic_t2 = automatic_t2 + term(s)
      !       end do

      ! end function automatic_t2

      function automatic_t1_h(t2, nocc, nactive, a, i)

            double precision :: automatic_t1_h
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 

            integer, intent(in) :: a, i 
            integer :: s ,j,b,k,c 
            double precision, dimension(0:12) :: term 
            term = 0.d+0 
            term = 0.d+0
            do j = 1, nocc
                  term(0) = term(0) + tvooo(a,j,j,i)
                  term(1) = term(1) + tvooo(a,i,j,j)
            end do

            term(0) = term(0) * (-1.0d+0)
            term(1) = term(1) * 2.0d+0

            term(2) = term(2) + tvo(a,i)

            term(2) = term(2) * 1.0d+0

            do j = 1, nocc
                  do b = nocc + 1, nactive
                        term(3) = term(3) + tov(j,b) * t2(a,b,i,j)
                  end do
            end do

            term(3) = term(3) * 2.0d+0

            do b = nocc + 1, nactive
                  do j = 1, nocc
                        term(4) = term(4) + tov(j,b) * t2(a,b,j,i)
                  end do
            end do

            term(4) = term(4) * (-1.0d+0)


            do j = 1, nocc
                  do b = nocc + 1, nactive
                        do k = 1, nocc
                              term(5) = term(5) + t2(a,b,k,j) * tovoo(j,b,k,i)
                              term(6) = term(6) + t2(a,b,k,i) * tovoo(j,b,k,j)
                              term(7) = term(7) + t2(a,b,k,j) * tovoo(k,b,j,i)
                              term(8) = term(8) + t2(a,b,k,i) * tovoo(k,b,j,j)
                        end do
                  end do
            end do

            term(5) = term(5) * (-2.0d+0)
            term(6) = term(6) * 1.0d+0
            term(7) = term(7) * 1.0d+0
            term(8) = term(8) * (-2.0d+0)

            do k = 1, nocc
                  do j = 1, nocc
                        do b = nocc + 1, nactive
                              term(9) = term(9) + t2(a,b,i,k) * tovoo(k,b,j,j)
                              term(10) = term(10) + t2(a,b,i,k) * tovoo(j,b,k,j)
                        end do
                  end do
            end do

            term(9) = term(9) * 4.0d+0
            term(10) = term(10) * (-2.0d+0)

            do c = nocc + 1, nactive
                  do j = 1, nocc
                        do b = nocc + 1, nactive
                              term(11) = term(11) + t2(b,c,j,i) * tvvov(a,b,j,c)
                        end do
                  end do
            end do

            term(11) = term(11) * (-1.0d+0)

            do b = nocc + 1, nactive
                  do c = nocc + 1, nactive
                        do j = 1, nocc
                              term(12) = term(12) + t2(b,c,j,i) * tvvov(a,c,j,b)
                        end do
                  end do
            end do

            term(12) = term(12) * 2.0d+0


            automatic_t1_h = 0.d+0 
            do s = 0, 12
                  automatic_t1_h = automatic_t1_h + term(s)
            end do

      end function automatic_t1_h


      function automatic_t2_h(t2, nocc, nactive, a, i, b, j)
            double precision :: automatic_t2_h
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            integer, intent(in) :: a, i, b, j 
            integer :: s ,c,k,l,d 
            double precision, dimension(0:39) :: term 

            term = 0.d+0 
            term = 0.d+0
            term(0) = term(0) + tvovo(b,j,a,i)

            do c = nocc + 1, nactive
                  term(1) = term(1) + tvv(b,c) * t2(a,c,i,j)
                  term(2) = term(2) + tvv(a,c) * t2(b,c,j,i)
            end do


            do k = 1, nocc
                  do l = 1, nocc
                        term(3) = term(3) + t2(a,b,i,k) * toooo(l,l,k,j)
                        term(4) = term(4) + t2(a,b,k,j) * toooo(l,l,k,i)
                  end do
            end do

            term(3) = term(3) * (-2.0d+0)
            term(4) = term(4) * (-2.0d+0)

            do k = 1, nocc
                  term(5) = term(5) + too(k,j) * t2(a,b,i,k)
                  term(6) = term(6) + too(k,i) * t2(a,b,k,j)
            end do

            term(5) = -term(5)
            term(6) = -term(6)


            do c = nocc + 1, nactive
                  do l = 1, nocc
                        do d = nocc + 1, nactive
                              do k = 1, nocc
                                    term(7) = term(7) + t2(a,b,k,l) * t2(c,d,i,j) * tovov(l,d,k,c)
                                    term(8) = term(8) + t2(a,b,k,j) * t2(c,d,i,l) * tovov(l,d,k,c)
                                    term(9) = term(9) + t2(a,c,i,j) * t2(b,d,k,l) * tovov(l,d,k,c)
                                    term(10) = term(10) + t2(a,d,k,l) * t2(b,c,j,i) * tovov(l,d,k,c)
                                    term(11) = term(11) + t2(a,c,k,i) * t2(b,d,j,l) * tovov(l,d,k,c)
                                    term(12) = term(12) + t2(a,d,i,l) * t2(b,c,k,j) * tovov(l,d,k,c)
                              end do
                        end do
                  end do
            end do

            term(8) = term(8) * (-2.0d+0)
            term(9) = term(9) * (-2.0d+0)
            term(10) = term(10) * (-2.0d+0)
            term(11) = term(11) * (-2.0d+0)
            term(12) = term(12) * (-2.0d+0)

            do d = nocc + 1, nactive
                  do l = 1, nocc
                        do k = 1, nocc
                              do c = nocc + 1, nactive
                                    term(13) = term(13) + t2(a,b,k,j) * t2(c,d,i,l) * tovov(l,c,k,d)
                                    term(14) = term(14) + t2(a,b,i,k) * t2(c,d,j,l) * tovov(l,c,k,d)
                                    term(15) = term(15) + t2(a,c,i,k) * t2(b,d,j,l) * tovov(l,c,k,d)
                              end do
                        end do
                  end do
            end do

            term(15) = term(15) * (-2.0d+0)

            do c = nocc + 1, nactive
                  do l = 1, nocc
                        do k = 1, nocc
                              do d = nocc + 1, nactive
                                    term(16) = term(16) + t2(a,b,i,k) * t2(c,d,j,l) * tovov(l,d,k,c)
                                    term(17) = term(17) + t2(a,c,i,k) * t2(b,d,j,l) * tovov(l,d,k,c)
                              end do
                        end do
                  end do
            end do

            term(16) = term(16) * (-2.0d+0)
            term(17) = term(17) * 4.0d+0

            do c = nocc + 1, nactive
                  do d = nocc + 1, nactive
                        do k = 1, nocc
                              do l = 1, nocc
                                    term(18) = term(18) + t2(a,d,k,j) * t2(b,c,l,i) * tovov(l,d,k,c)
                                    term(19) = term(19) + t2(a,c,k,i) * t2(b,d,l,j) * tovov(l,d,k,c)
                              end do
                        end do
                  end do
            end do


            do d = nocc + 1, nactive
                  do l = 1, nocc
                        do c = nocc + 1, nactive
                              do k = 1, nocc
                                    term(20) = term(20) + t2(a,d,i,l) * t2(b,c,k,j) * tovov(l,c,k,d)
                                    term(21) = term(21) + t2(a,c,k,i) * t2(b,d,j,l) * tovov(l,c,k,d)
                                    term(22) = term(22) + t2(a,c,i,j) * t2(b,d,k,l) * tovov(l,c,k,d)
                                    term(23) = term(23) + t2(a,d,k,l) * t2(b,c,j,i) * tovov(l,c,k,d)
                              end do
                        end do
                  end do
            end do

            do c = nocc + 1, nactive
                  do k = 1, nocc
                        term(24) = term(24) + t2(a,c,k,j) * tvvoo(b,c,k,i)
                        term(25) = term(25) + t2(b,c,k,i) * tvvoo(a,c,k,j)
                        term(26) = term(26) + t2(b,c,k,j) * tvoov(a,i,k,c)
                        term(27) = term(27) + t2(a,c,k,i) * tvoov(b,j,k,c)
                  end do
            end do

            term(24) = -term(24)
            term(25) = -term(25)
            term(26) = -term(26)
            term(27) = -term(27)

            do k = 1, nocc
                  do c = nocc + 1, nactive
                        term(28) = term(28) + t2(b,c,j,k) * tvvoo(a,c,k,i)
                        term(29) = term(29) + t2(a,c,i,k) * tvvoo(b,c,k,j)
                        term(30) = term(30) + t2(a,c,i,j) * tvvoo(b,c,k,k)
                        term(31) = term(31) + t2(b,c,j,i) * tvvoo(a,c,k,k)
                        term(32) = term(32) + t2(b,c,j,k) * tvoov(a,i,k,c)
                        term(33) = term(33) + t2(a,c,i,k) * tvoov(b,j,k,c)
                        term(34) = term(34) + t2(a,c,i,j) * tvoov(b,k,k,c)
                        term(35) = term(35) + t2(b,c,j,i) * tvoov(a,k,k,c)
                  end do
            end do

            term(28) = -term(28)
            term(29) = -term(29)
            term(30) = term(30) * 2.0d+0
            term(31) = term(31) * 2.0d+0
            term(32) = term(32) * 2.0d+0
            term(33) = term(33) * 2.0d+0
            term(34) = -term(34)
            term(35) = -term(35)

            do c = nocc + 1, nactive
                  do d = nocc + 1, nactive
                        term(36) = term(36) + t2(c,d,i,j) * &
                              read_ftvvvv(b, d, a, c)
!                              tvvvv(b,d,a,c)
                  end do
            end do


            do l = 1, nocc
                  do k = 1, nocc
                        term(37) = term(37) + t2(a,b,k,l) * toooo(l,j,k,i)
                        term(38) = term(38) + t2(a,b,k,j) * toooo(l,i,k,l)
                        term(39) = term(39) + t2(a,b,i,k) * toooo(l,j,k,l)
                  end do
            end do


            automatic_t2_h = 0.d+0 
            do s = 0, 39
                  automatic_t2_h = automatic_t2_h + term(s)
            end do
      end function automatic_t2_h


      function automatic_t2_h_intermediates(t2, nocc, nactive, a, i, b, j)
            double precision :: automatic_t2_h_intermediates
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            integer, intent(in) :: a, i, b, j 

            integer :: s ,c,k,l,d 
            double precision, dimension(0:39) :: term 

            term = 0.d+0 
            term = 0.d+0
            term(0) = term(0) + tvovo(b,j,a,i)

            do c = nocc + 1, nactive
                  term(1) = term(1) + tvv(b,c) * t2(a,c,i,j)
                  term(2) = term(2) + tvv(a,c) * t2(b,c,j,i)
            end do


            do k = 1, nocc
                  do l = 1, nocc
                        term(3) = term(3) + t2(a,b,i,k) * toooo(l,l,k,j)
                        term(4) = term(4) + t2(a,b,k,j) * toooo(l,l,k,i)
                  end do
            end do

            term(3) = term(3) * (-2.0d+0)
            term(4) = term(4) * (-2.0d+0)

            do k = 1, nocc
                  term(5) = term(5) + too(k,j) * t2(a,b,i,k)
                  term(6) = term(6) + too(k,i) * t2(a,b,k,j)
            end do

            term(5) = -term(5)
            term(6) = -term(6)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            do l = 1, nocc
                  do k = 1, nocc
                        term(7) = term(7) + t2(a,b,k,l) * x1(i, j, k, l)
                  end do
            end do

            do k = 1, nocc
                  term(8) = term(8) + t2(a,b,k,j) * x2(i, k)
                  term(13) = term(13) + t2(a,b,k,j) * x7(i, k)
                  term(14) = term(14) + t2(a,b,i,k) * x7(j, k)
                  term(16) = term(16) + t2(a,b,i,k) * x2(j, k)
            end do

             do c = nocc + 1, nactive
                   term(9) = term(9) + t2(a,c,i,j) * x3(b, c)
                   term(10) = term(10) + t2(b,c,j,i) * x3(a, c)
                   term(22) = term(22) + t2(a,c,i,j) * x14(b, c) !t2(b,d,k,l) * tovov(l,c,k,d)
                   term(23) = term(23) + t2(b,c,j,i) *x14(a, c)  !t2(a,d,k,l) * tovov(l,c,k,d)
             end do

             do c = nocc + 1, nactive
                   do k = 1, nocc
                         term(11) = term(11) + t2(a,c,k,i) * x5(b, j, c, k)
                         term(12) = term(12) + t2(a,c,i,k) * x6(b, j, c, k)
                         term(15) = term(15) + t2(a,c,i,k) * x9(b, j, c, k)
                         term(17) = term(17) + t2(a,c,i,k) * x5(b, j, c, k)
                         term(18) = term(18) + t2(a,c,k,j) * x16(b, i, c, k) !t2(b,c,l,i) * tovov(l,d,k,c)
                         term(19) = term(19) + t2(a,c,k,i) * x6(b, j, c, k)  !t2(b,d,l,j) * tovov(l,d,k,c)
                         term(20) = term(20) + t2(a,c,i,k) * x12(b, j, c, k) !t2(b,c,k,j) * tovov(l,c,k,d)
                         term(21) = term(21) + t2(a,c,k,i) * x9(b, j, c, k)  !t2(b,d,j,l) * tovov(l,c,k,d)
                   end do
             end do


!             do c = nocc + 1, nactive
!                   do l = 1, nocc
!                         do d = nocc + 1, nactive
!                               do k = 1, nocc
! !                                    term(7) = term(7) + t2(a,b,k,l) * t2(c,d,i,j) * tovov(l,d,k,c)
! !                                    term(8) = term(8) + t2(a,b,k,j) * t2(c,d,i,l) * tovov(l,d,k,c)
! !                                    term(9) = term(9) + t2(a,c,i,j) * t2(b,d,k,l) * tovov(l,d,k,c)
! !                                    term(10) = term(10) + t2(a,d,k,l) * t2(b,c,j,i) * tovov(l,d,k,c)
! !                                    term(11) = term(11) + t2(a,c,k,i) * t2(b,d,j,l) * tovov(l,d,k,c)
! !                                    term(12) = term(12) + t2(a,d,i,l) * t2(b,c,k,j) * tovov(l,d,k,c)
!                               end do
!                         end do
!                   end do
!             end do

            term(8) = term(8) * (-2.0d+0)
            term(9) = term(9) * (-2.0d+0)
            term(10) = term(10) * (-2.0d+0)
            term(11) = term(11) * (-2.0d+0)
            term(12) = term(12) * (-2.0d+0)

            

!             do d = nocc + 1, nactive
!                   do l = 1, nocc
!                         do k = 1, nocc
!                               do c = nocc + 1, nactive
!                                     ! term(13) = term(13) + t2(a,b,k,j) * t2(c,d,i,l) * tovov(l,c,k,d)
!                                     ! term(14) = term(14) + t2(a,b,i,k) * t2(c,d,j,l) * tovov(l,c,k,d)
! !                                    term(15) = term(15) + t2(a,c,i,k) * t2(b,d,j,l) * tovov(l,c,k,d)
!                               end do
!                         end do
!                   end do
!             end do

            term(15) = term(15) * (-2.0d+0)

!             do c = nocc + 1, nactive
!                   do l = 1, nocc
!                         do k = 1, nocc
!                               do d = nocc + 1, nactive
! !                                    term(16) = term(16) + t2(a,b,i,k) * t2(c,d,j,l) * tovov(l,d,k,c)
! !                                    term(17) = term(17) + t2(a,c,i,k) * t2(b,d,j,l) * tovov(l,d,k,c)
!                               end do
!                         end do
!                   end do
!             end do

            term(16) = term(16) * (-2.0d+0)
            term(17) = term(17) * 4.0d+0

            ! do c = nocc + 1, nactive
            !       do d = nocc + 1, nactive
            !             do k = 1, nocc
            !                   do l = 1, nocc
            !                         term(18) = term(18) + t2(a,d,k,j) * t2(b,c,l,i) * tovov(l,d,k,c)
            !                         term(19) = term(19) + t2(a,c,k,i) * t2(b,d,l,j) * tovov(l,d,k,c)
            !                   end do
            !             end do
            !       end do
            ! end do


            ! do d = nocc + 1, nactive
            !       do l = 1, nocc
            !             do c = nocc + 1, nactive
            !                   do k = 1, nocc
            !                         term(20) = term(20) + t2(a,d,i,l) * t2(b,c,k,j) * tovov(l,c,k,d)
            !                         term(21) = term(21) + t2(a,c,k,i) * t2(b,d,j,l) * tovov(l,c,k,d)
            !                         term(22) = term(22) + t2(a,c,i,j) * t2(b,d,k,l) * tovov(l,c,k,d)
            !                         term(23) = term(23) + t2(a,d,k,l) * t2(b,c,j,i) * tovov(l,c,k,d)
            !                   end do
            !             end do
            !       end do
            ! end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            do c = nocc + 1, nactive
                  do k = 1, nocc
                        term(24) = term(24) + t2(a,c,k,j) * tvvoo(b,c,k,i)
                        term(25) = term(25) + t2(b,c,k,i) * tvvoo(a,c,k,j)
                        term(26) = term(26) + t2(b,c,k,j) * tvoov(a,i,k,c)
                        term(27) = term(27) + t2(a,c,k,i) * tvoov(b,j,k,c)
                  end do
            end do

            term(24) = -term(24)
            term(25) = -term(25)
            term(26) = -term(26)
            term(27) = -term(27)

            do k = 1, nocc
                  do c = nocc + 1, nactive
                        term(28) = term(28) + t2(b,c,j,k) * tvvoo(a,c,k,i)
                        term(29) = term(29) + t2(a,c,i,k) * tvvoo(b,c,k,j)
                        term(30) = term(30) + t2(a,c,i,j) * tvvoo(b,c,k,k)
                        term(31) = term(31) + t2(b,c,j,i) * tvvoo(a,c,k,k)
                        term(32) = term(32) + t2(b,c,j,k) * tvoov(a,i,k,c)
                        term(33) = term(33) + t2(a,c,i,k) * tvoov(b,j,k,c)
                        term(34) = term(34) + t2(a,c,i,j) * tvoov(b,k,k,c)
                        term(35) = term(35) + t2(b,c,j,i) * tvoov(a,k,k,c)
                  end do
            end do

            term(28) = -term(28)
            term(29) = -term(29)
            term(30) = term(30) * 2.0d+0
            term(31) = term(31) * 2.0d+0
            term(32) = term(32) * 2.0d+0
            term(33) = term(33) * 2.0d+0
            term(34) = -term(34)
            term(35) = -term(35)

            do c = nocc + 1, nactive
                  do d = nocc + 1, nactive
                        term(36) = term(36) + t2(c,d,i,j) * &
                              read_ftvvvv(b, d, a, c)
                              !tvvvv(b,d,a,c)
                  end do
            end do


            do l = 1, nocc
                  do k = 1, nocc
                        term(37) = term(37) + t2(a,b,k,l) * toooo(l,j,k,i)
                        term(38) = term(38) + t2(a,b,k,j) * toooo(l,i,k,l)
                        term(39) = term(39) + t2(a,b,i,k) * toooo(l,j,k,l)
                  end do
            end do


            automatic_t2_h_intermediates = 0.d+0 
            do s = 0, 39
                  automatic_t2_h_intermediates = automatic_t2_h_intermediates + term(s)
            end do

      end function automatic_t2_h_intermediates

      subroutine t2_intermediates_init(nocc0, nocc1, nvirt0, nvirt1)
            integer, intent(in) :: nocc0, nocc1
            integer, intent(in) :: nvirt0, nvirt1

            allocate(x1(nocc0:nocc1, nocc0:nocc1, nocc0:nocc1, nocc0:nocc1))
            allocate(x2(nocc0:nocc1, nocc0:nocc1))
            allocate(x3(nvirt0:nvirt1, nvirt0:nvirt1))
            allocate(x5(nvirt0:nvirt1, nocc0:nocc1, nvirt0:nvirt1, nocc0:nocc1))
            allocate(x6(nvirt0:nvirt1, nocc0:nocc1, nvirt0:nvirt1, nocc0:nocc1))
            allocate(x7(nocc0:nocc1, nocc0:nocc1))
            allocate(x9(nvirt0:nvirt1, nocc0:nocc1, nvirt0:nvirt1, nocc0:nocc1))
            allocate(x12(nvirt0:nvirt1, nocc0:nocc1, nvirt0:nvirt1, nocc0:nocc1))
            allocate(x14(nvirt0:nvirt1, nvirt0:nvirt1))
            allocate(x16(nvirt0:nvirt1, nocc0:nocc1, nvirt0:nvirt1, nocc0:nocc1))
            x1 = zero
            x2 = zero
            x3 = zero
            x5 = zero
            x6 = zero
            x7 = zero
            x9 = zero
            x12 = zero
            x14 = zero
            x16 = zero
      end subroutine t2_intermediates_init

      subroutine t2_intermediates_free()
            deallocate(x1)
            deallocate(x2)
            deallocate(x3)
            deallocate(x5)
            deallocate(x6)
            deallocate(x7)
            deallocate(x9)
            deallocate(x12)
            deallocate(x14)
            deallocate(x16)

      end subroutine t2_intermediates_free

      subroutine t2_interm_1(t2, nocc0, nocc1, nvirt0, nvirt1, idoubles, ntasks)
            integer, intent(in) :: nocc0, nocc1
            integer, intent(in) :: nvirt0, nvirt1
            real(F64), dimension(nvirt0:nvirt1,nvirt0:nvirt1,nocc0:nocc1,nocc0:nocc1), intent(in) :: t2 
            integer, dimension(:,:), intent(in) :: idoubles
            integer, intent(in) :: ntasks


            integer :: i, j, k, l, p
            integer :: c, d

            !$omp parallel private(i, j, k, l, p, c, d) default(shared)
            !$omp do schedule(guided)
            do p = 1, ntasks
                  i = idoubles(p, 1)
                  j = idoubles(p, 2)
                  k = idoubles(p, 3)
                  l = idoubles(p, 4)
                  
                  x1(i, j, k, l) = zero
                  
                  do c = nvirt0, nvirt1
                        do d = nvirt0, nvirt1
                              x1(i, j, k, l) = x1(i, j, k, l) + t2(c, d, i, j) * tovov(l, d, k, c)
                        end do
                  end do
            end do
            !$omp end do                                                                                                              
            !$omp end parallel                                                                                                        
            
      end subroutine t2_interm_1


      subroutine t2_interm_2(t2, nocc0, nocc1, nvirt0, nvirt1, idoubles, ntasks)

            integer, intent(in) :: nocc0, nocc1
            integer, intent(in) :: nvirt0, nvirt1
            real(F64), dimension(nvirt0:nvirt1,nvirt0:nvirt1,nocc0:nocc1,nocc0:nocc1), intent(in) :: t2 
            integer, dimension(:,:), intent(in) :: idoubles
            integer, intent(in) :: ntasks

            integer :: b, j, c, k
            integer :: d, l, p

            !$omp parallel private(b, j, c, k, d, l, p) default(shared)
            !$omp do schedule(guided)
            
            do p = 1, ntasks
                  b = idoubles(p, 1)
                  j = idoubles(p, 2)
                  c = idoubles(p, 3)
                  k = idoubles(p, 4)
                  
                  x5(b, j, c, k) = zero
                  x6(b, j, c, k) = zero
                  x9(b, j, c, k) = zero
                  x12(b, j, c, k) = zero
                  
                  do l = nocc0, nocc1
                        do d = nvirt0, nvirt1
                              x5(b, j, c, k) = x5(b, j, c, k) + t2(b, d, j, l) * tovov(l, d, k, c)
                              x6(b, j, c, k) = x6(b, j, c, k) + t2(b, d, l, j) * tovov(l, d, k, c)
                              x9(b, j, c, k) =  x9(b, j, c, k) + t2(b, d, j, l) * tovov(l, c, k, d)
                              x12(b, j, c, k) =  x12(b, j, c, k) + t2(b, d, l, j) * tovov(l, c, k, d)
                        end do
                  end do
            end do
            !$omp end do
            !$omp end parallel
      end subroutine t2_interm_2

      subroutine t2_interm_2b(t2, nocc0, nocc1, nvirt0, nvirt1, idoubles, ntasks)

            integer, intent(in) :: nocc0, nocc1
            integer, intent(in) :: nvirt0, nvirt1
            real(F64), dimension(nvirt0:nvirt1,nvirt0:nvirt1,nocc0:nocc1,nocc0:nocc1), intent(in) :: t2 
            integer, dimension(:,:), intent(in) :: idoubles
            integer, intent(in) :: ntasks


            integer :: b, i, c, k
            integer :: d, l, p

            !$omp parallel private(b, i, c, k, d, l, p) default(shared)
            !$omp do schedule(guided)
            
            do p = 1, ntasks
                  b = idoubles(p, 1)
                  i = idoubles(p, 2)
                  c = idoubles(p, 3)
                  k = idoubles(p, 4)
                  
                  x16(b, i, c, k) = zero
                  
                  do l = nocc0, nocc1
                        do d = nvirt0, nvirt1
                              x16(b, i, c, k) =  x16(b, i, c, k) + t2(b, d, l, i) * tovov(l, c, k, d)
                        end do
                  end do
            end do
            !$omp end do
            !$omp end parallel
      end subroutine t2_interm_2b

      subroutine t2_interm_3(t2, nocc0, nocc1, nvirt0, nvirt1, isingles, ntasks)

            integer, intent(in) :: nocc0, nocc1
            integer, intent(in) :: nvirt0, nvirt1
            real(F64), dimension(nvirt0:nvirt1,nvirt0:nvirt1,nocc0:nocc1,nocc0:nocc1), intent(in) :: t2 
            integer, dimension(:,:), intent(in) :: isingles
            integer, intent(in) :: ntasks

            integer :: i, k, c, d, l, p

            !$omp parallel private(i, k, c, d, l, p) default(shared)
            !$omp do schedule(guided)

            do p = 1, ntasks
                  i = isingles(p, 1)
                  k = isingles(p, 2)

                  x2(i, k) = zero
                  x7(i, k) = zero
                  
                  do c = nvirt0, nvirt1
                        do d = nvirt0, nvirt1
                              do l = nocc0, nocc1
                                    x2(i, k) = x2(i, k) + t2(c, d, i, l) * tovov(l, d, k, c)
                                    x7(i, k) = x7(i, k) + t2(c, d, i, l) * tovov(l, c, k, d)
                              end do
                        end do
                  end do


            end do
            !$omp end do
            !$omp end parallel

      end subroutine t2_interm_3

      subroutine t2_interm_4(t2, nocc0, nocc1, nvirt0, nvirt1, isingles, ntasks)
            integer, intent(in) :: nocc0, nocc1
            integer, intent(in) :: nvirt0, nvirt1
            real(F64), dimension(nvirt0:nvirt1,nvirt0:nvirt1,nocc0:nocc1,nocc0:nocc1), intent(in) :: t2 
            integer, dimension(:,:), intent(in) :: isingles
            integer, intent(in) :: ntasks

            integer :: b, c, d, k, l, p

            !$omp parallel private(b, c, d, k, l, p) default(shared)
            !$omp do schedule(guided)

            do p = 1, ntasks
                  b = isingles(p, 1)
                  c = isingles(p, 2)

                  x3(b, c) = zero
                  x14(b, c) = zero

                  do d = nvirt0, nvirt1
                        do k = nocc0, nocc1
                              do l = nocc0, nocc1
                                    x3(b, c) = x3(b, c) + t2(b, d, k, l) * tovov(l, d, k, c)
                                    x14(b, c) = x14(b, c) + t2(b, d, k, l) * tovov(l, c, k, d)
                              end do
                        end do
                  end do


            end do
            !$omp end do
            !$omp end parallel

      end subroutine t2_interm_4




end module automatic_t1_t2_amplitudes
