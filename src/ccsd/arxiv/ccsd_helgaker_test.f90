module ccsd_helgaker_test

      use gparam
      use cc3_intermediates
      use t1_transformed_int
      use ccsd_transformed_integrals

      implicit none


contains

      function t1_amplitude_h(eorb, a, i, t2, t1, nocc, nactive)
            double precision :: t1_amplitude_h
            double precision, dimension(:), intent(in)                                  :: eorb
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
            integer, intent(in) :: a, i


            ! t1_amplitude_h = (o_g(t2, t1, nocc, nvirt, nactive, a, i) + o_h(t2, t1, nocc, nvirt, nactive, a, i)&
            !       + o_i(t2, t1, nocc, nvirt, nactive, a, i) + o_j(t2, t1, nocc, nvirt, nactive, a, i)&
            !       +t1(a,i)*(eorb(i) - eorb(a)))&
            !       / (eorb(i) - eorb(a))

            ! t1_amplitude_h = (&
            !       o_g(t2, nocc, nactive, a, i) + &
            !       o_h(t2, nocc, nactive, a, i)+ &
            !       o_i(t2, nocc, nactive, a, i) +&
            !       o_j(nocc, a, i)&
            !       +t1(a,i)*(eorb(i) - eorb(a)))&
            !       / (eorb(i) - eorb(a))

            t1_amplitude_h = (automatic_t1_h(t2, nocc, nactive, a, i) &
                  +t1(a,i)*(eorb(i) - eorb(a)))&
                  / (eorb(i) - eorb(a))


      end function t1_amplitude_h

      function t2_amplitude_h(eorb, a, b, i, j, t2,t1, nocc, nactive)
            double precision :: t2_amplitude_h
            double precision, dimension(:), intent(in)                                  :: eorb
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
            integer, intent(in) :: a, b, i, j

            ! t2_amplitude_h = (o_a(t2, t1, nocc, nvirt, nactive, a, i, b, j)&
            !       + o_b(t2, t1, nocc, nvirt, nactive, a, i, b, j)&
            !       + o_f(t2, t1, nocc, nvirt, nactive, a, i, b, j)&
            !       + 2.d+0*(o_c(t2, t1, nocc, nvirt, nactive, a, i, b, j) &
            !       + o_d(t2, t1, nocc, nvirt, nactive, a, i, b, j)&
            !       + o_e(t2, t1, nocc, nvirt, nactive, a, i, b, j))& 
            !       + 2.d+0*(o_c(t2, t1, nocc, nvirt, nactive, b, j, a, i) &
            !       + o_d(t2, t1, nocc, nvirt, nactive, b, j, a, i)&
            !       + o_e(t2, t1, nocc, nvirt, nactive, b, j, a, i))&
            !       +t2(a,b,i,j)*(eorb(i) + eorb(j) - eorb(a) - eorb(b)))&
            !       / (eorb(i) + eorb(j) - eorb(a) - eorb(b))

            ! t2_amplitude_h = (&
            !       o_a(t2,t1, nocc, nactive, a, i, b, j)&
            !       + o_b(t2,t1, nocc, nactive, a, i, b, j)&
            !       + o_c(t2,t1, nocc, nactive, a, i, b, j)&
            !       + o_d(t2,t1, nocc, nactive, a, i, b, j)&
            !       + o_e(t2,t1, nocc, nactive, a, i, b, j)& 
            !       + o_c(t2,t1, nocc, nactive, b, j, a, i)&
            !       + o_d(t2,t1, nocc, nactive, b, j, a, i)&
            !       + o_e(t2,t1, nocc, nactive, b, j, a, i)&
            !       +t2(a,b,i,j)*(eorb(i) + eorb(j) - eorb(a) - eorb(b)))&
            !       / (eorb(i) + eorb(j) - eorb(a) - eorb(b))

            t2_amplitude_h = (automatic_t2_h(t2, t1, nocc, nactive, a, i, b, j)&
                  +t2(a,b,i,j)*(eorb(i) + eorb(j) - eorb(a) - eorb(b)))&
                  / (eorb(i) + eorb(j) - eorb(a) - eorb(b))

            ! t2_amplitude_h = (desp(t2, t1, nocc, nactive, a, i, b, j)&
            !       +t2(a,b,i,j)*(eorb(i) + eorb(j) - eorb(a) - eorb(b)))&
            !       / (eorb(i) + eorb(j) - eorb(a) - eorb(b))

      end function t2_amplitude_h

      function o_a(t2,t1, nocc, nactive, a, i, b, j)
            double precision :: o_a
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
            integer, intent(in) :: a, b, i, j
            integer :: c, d

            o_a = 0.d+0
            ! do k = 1, nocc
            !       do l = 1, nocc
            !             sum = 0.d+0
            !             do c = nocc+1, nactive
            !                   do d = nocc+1, nactive
            !                        sum = sum + t2(c, d, i, j) * vovo(c, k, d, l) 
            !                   end do
            !             end do
            !             o_a = o_a + t2(a, b, k, l) * (toooo(k, i, l, j) + sum)
            !       end do
            ! end do
            ! o_a = 2.d+0 * o_a

            o_a = tvovo(a,i,b,j)

            do c = nocc+1, nactive
                  do d = nocc+1, nactive
                        o_a = o_a + t2(c, d, i, j) * tvvvv(a, c, b, d) 
                  end do
            end do

      end function o_a

      function o_b(t2,t1, nocc, nactive, a, i, b, j)
            double precision :: o_b
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
            integer, intent(in) :: a, b, i, j

            integer :: c, d, k, l, e, f
            double precision :: sum, s1, s2, s3

            o_b = 0.d+0
            ! do c = nocc+1, nactive
            !       do d = nocc+1, nactive
            !             o_b = o_b + t2(c, d, i, j) * tvvvv(a, c, b, d)
            !       end do
            ! end do
            ! o_b = 2.d+0 * o_b
            !-----------------------------------------------------------------
            ! do k = 1, nocc
            !       do l = 1, nocc
            !             sum = 0.d+0
            !             do c = nocc+1, nactive
            !                   do d = nocc+1, nactive
            !                         sum = sum + t2(c,d,i,j)*vovo(c,k,d,l)
            !                   end do
            !             end do
            !             s1 = 0.d+0
            !             s2 = 0.d+0
            !             s3 = 0.d+0
            !             do c = nocc+1, nactive
            !                   do d = nocc+ 1, nactive
            !                         s1 = s1 + t1(c,i)*t1(d,j)*vovo(c,k,d,l)
            !                   end do
            !             end do
            !             do c = nocc+1, nactive
            !                   s2 = s2 + t1(c,j)*vooo(c,l,k,i)
            !             end do
            !             do c = nocc+1, nactive
            !                   s3 = s3 + t1(c,i)* vooo(c,k,l,j)
            !             end do
            !             o_b = o_b + t2(a, b, k, l) * (oooo(k,i,l,j) + s1 + s2 + s3 + sum)
            !       end do
            ! end do
            !-----------------------------------------------------------------

            do k = 1, nocc
                  do l = 1, nocc
                        sum = 0.d+0
                        do c = nocc+1, nactive
                              do d = nocc+1, nactive
                                    sum = sum + t2(c,d,i,j)*tovov(k,c,l,d)
                              end do
                        end do
                        o_b = o_b + t2(a, b, k, l) * (toooo(k, i, l, j) + sum)
                  end do
            end do

      end function o_b

      function o_c(t2,t1, nocc, nactive, a, i, b, j)
            double precision :: o_c
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
            integer, intent(in) :: a, b, i, j
            integer :: c, k, d, l 
            double precision :: sum1, sum2, sum3, sum4, s
            double precision :: s1, s2, s3

            o_c = 0.d+0

            ! sum1 = 0.d+0
            ! do c = nocc+1, nactive
            !       do k = 1, nocc
            !             sum2 = 0.d+0
            !             do d = nocc+1, nactive
            !                   do l = 1, nocc
            !                         sum2 = sum2 + t2(a, d, l, i) * vovo(c, l, d, k)
            !                   end do
            !             end do
            !             sum1 = sum1 + t2(c, b, j, k)* (tvvoo(a, c, k, i) - 0.5d+0 * sum2)
            !       end do
            ! end do
            ! sum1 = -0.5d+0 * sum1

            ! sum3 = 0.d+0
            ! do c = nocc+1, nactive
            !       do k = 1, nocc
            !             sum4 = 0.d+0
            !             do d = nocc+1, nactive
            !                   do l = 1, nocc
            !                         sum4 = sum4 + t2(a, d, l, j) * vovo(c, l, d, k)
            !                   end do
            !             end do
            !             sum3 = sum3 + t2(c, b, i, k)* (tvvoo(a, c, k, j) - 0.5d+0 * sum2)
            !       end do
            ! end do
            ! sum3 = -sum3

            ! o_c = sum1 + sum3

            sum1 = 0.d+0
            do c = nocc+1, nactive
                  do k = 1, nocc
                        s = 0.d+0
                        do d = nocc + 1, nactive
                              do l = 1, nocc
                                    s = s + t2(a,d,l,i) * tovov(k, d, l, c)
                              end do
                        end do
                        sum1 = sum1 + t2(b,c,k,j) * (tvvoo(a,c,k,i) -0.5d+0* s)
                  end do
            end do
            sum1 = sum1*(-0.5d+0)
            sum3 = 0.d+0
            do c = nocc+1, nactive
                  do k = 1, nocc
                        sum4 = 0.d+0
                        do d = nocc+1, nactive
                              do l = 1, nocc
                                    sum4 = sum4 + t2(a, d, l, j) * tovov(k, d, l, c)
                              end do
                        end do
                        sum3 = sum3 + t2(b, c, k, i)* (tvvoo(a, c, k, j) - 0.5d+0 * sum4)
                  end do
            end do
            sum3 = -sum3

            o_c = sum1 + sum3

      end function o_c

      function o_d(t2,t1, nocc, nactive, a, i, b, j)
            double precision :: o_d
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
            integer, intent(in) :: a, b, i, j

            integer ::  c, k, d, l 
            double precision :: sum

            o_d = 0.d+0
            ! do c = nocc+1, nactive
            !       do k = 1, nocc
            !             sum = 0.d+0
            !             do d = nocc+1, nactive
            !                   do l = 1, nocc
            !                         sum = sum + (2.d+0*t2(a, d, i, l) - t2(d, a, i, l))* lovov(l, d, k, c)
            !                   end do
            !             end do
            !             o_d = o_d + (2.d+0 * t2(b, c, j, k) - t2(c, b, j, k))*(tlvoov(a, i, k, c) + 0.5d+0 * sum)
            !       end do
            ! end do
            ! o_d = o_d * 0.5d+0

            do c = nocc+1, nactive
                  do k = 1, nocc
                        sum = 0.d+0
                        do d = nocc+1, nactive
                              do l = 1, nocc
                                    sum = sum + uu(t2, nocc, nactive, a,d,i,l)* tlovov(l, d, k, c)
                              end do
                        end do
                        o_d = o_d + uu(t2, nocc, nactive, b,c,j,k)*(tlvoov(a, i, k, c) + 0.5d+0 * sum)
                  end do
            end do
            o_d = o_d * 0.5d+0

      end function o_d

      function o_e(t2,t1, nocc, nactive, a, i, b, j)
            double precision :: o_e
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
            integer, intent(in) :: a, b, i, j
            integer :: c, k, d, l
            double precision :: sum1, sum2, sum

            o_e = 0.d+0
            ! do c = nocc+1, nactive
            !       sum = 0.d+0
            !       do d = nocc+1, nactive
            !             do l = 1, nocc
            !                   do m = 1, nocc
            !                         sum = sum + t2(d, b, l, m) * lovov(l, d, m, c)
            !                   end do
            !             end do
            !       end do
            !       o_e = o_e + t2(a, c, i, j) * (in_fvv(b, c, nocc) - sum)
            ! end do

            ! do k = 1, nocc
            !       sum = 0.d+0
            !       do d = nocc+1, nactive
            !             do e = nocc+1, nactive
            !                   do m = 1, nocc
            !                         sum = sum + t2(d, e, j, m) * lovov(m, e, k, d)
            !                   end do
            !             end do
            !       end do
            !       o_e = o_e - t2(a, b, i, k)*(in_foo(k, j, nocc) + sum)
            ! end do

            sum1 = 0.d+0
            do c = nocc+1, nactive
                  sum = 0.d+0
                  do d = nocc+1, nactive
                        do l = 1, nocc
                              do k = 1, nocc
                                    sum = sum + uu(t2, nocc, nactive, b, d, k, l) * tovov(l, d, k, c)
                              end do
                        end do
                  end do
                  sum1 = sum1 + t2(a, c, i, j) * (in_fvv(b, c, nocc) - sum)
            end do

            sum2 = 0.d+0
            do k = 1, nocc
                  sum = 0.d+0
                  do d = nocc+1, nactive
                        do c = nocc+1, nactive
                              do l = 1, nocc
                                    sum = sum + uu(t2, nocc, nactive,c, d, l, j) * tovov(k, d, l, c)
                              end do
                        end do
                  end do
                  sum2 = sum2 - t2(a, b, i, k)*(in_foo(k, j, nocc) + sum)
            end do

            o_e = sum1 + sum2

      end function o_e

      ! function o_f(nocc, nactive, a, i, b, j)
      !       double precision :: o_f
      !       integer, intent(in) :: nocc, nactive
      !       integer, intent(in) :: a, b, i, j

      !       o_f = 2.d+0 * tvovo(a, i, b, j)

      ! end function o_f

      function o_g(t2, nocc, nactive, a, i)
            double precision :: o_g
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            integer, intent(in) :: a, i
            integer :: c, d, k

            o_g = 0.d+0
            ! do c = nocc+1, nactive
            !       do d = nocc+1, nactive
            !             do k = 1, nocc
            !                   o_g = o_g + t2(c, d, i, k) * tlovvv(k, d, a, c)
            !             end do
            !       end do
            ! end do
            ! o_g = o_g * 2.d+0

            do c = nocc+1, nactive
                  do d = nocc+1, nactive
                        do k = 1, nocc
                              o_g = o_g + uu(t2, nocc, nactive, c, d, k, i) * tvvov(a,d,k,c)
                        end do
                  end do
            end do

      end function o_g

      function o_h(t2, nocc, nactive, a, i)
            double precision :: o_h
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            integer, intent(in) :: a, i
            integer :: c, k, l

            o_h = 0.d+0

            ! do d = nocc+1, nactive
            !       do k = 1, nocc
            !             do l = 1, nocc
            !                   o_h = o_h + t2(a, d, k, l) * tlovoo(l, d, k, i)
            !             end do
            !       end do
            ! end do

            ! o_h = o_h * (-2.d+0)

            do c = nocc+1, nactive
                  do k = 1, nocc
                        do l = 1, nocc
                              o_h = o_h - uu(t2, nocc, nactive, a, c, k, l) * tovoo(l, c, k, i)
                        end do
                  end do
            end do

      end function o_h

      function o_i(t2, nocc, nactive, a, i)
            double precision :: o_i
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            integer, intent(in) :: a, i
            integer ::  c, k

            o_i = 0.d+0

            ! do c = nocc+1, nactive
            !       do k = 1, nocc
            !             o_i = o_i + (2.d+0 * t2(a, c, i, k) - t2(c, a, i, k))*in_fov(k, c, nocc)
            !       end do
            ! end do
            ! o_i = 2.d+0 * o_i

            do c = nocc+1, nactive
                  do k = 1, nocc
                        o_i = o_i + uu(t2, nocc, nactive, a,c,i,k)*in_fov(k, c, nocc)
                  end do
            end do

      end function o_i

      function o_j(nocc, a, i)
            double precision :: o_j
            integer, intent(in) :: nocc
            integer, intent(in) :: a, i

            !            o_j = 2.d+0 * in_fvo(a, i, nocc)

            o_j = in_fvo(a, i, nocc)


      end function o_j

      function uu(t2,nocc, nactive, a,b,i,j)
            double precision :: uu
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
            integer, intent(in) :: a, b, i, j

            uu = 2.d+0 * t2(a, b, i, j) - t2(a, b, j, i)
      end function uu


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

            !            print*, term(3)+term(4)+term(6)+term(8)+term(9)+term(10)
            !            print*, o_i(t2, nocc, nactive, a, i)

            automatic_t1_h = 0.d+0 
            do s = 0, 12
                  automatic_t1_h = automatic_t1_h + term(s)
            end do

      end function automatic_t1_h
      function automatic_t2_h(t2, t1, nocc, nactive, a, i, b, j) 
            double precision :: automatic_t2_h
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            integer, intent(in) :: nocc, nactive
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
                        term(36) = term(36) + t2(c,d,i,j) * tvvvv(b,d,a,c)
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

      !       function automatic_t2_h(t2, t1, nocc, nactive, a, i, b, j) 
      !             double precision :: automatic_t2_h
      !             integer, intent(in) :: nocc, nactive
      !             double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
      !             double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
      !             integer, intent(in) :: a, i, b, j 
      !             integer :: s ,c,k,l,d 
      !             double precision :: w
      !             double precision, dimension(0:39) :: term 
      !             term = 0.d+0 
      !             term = 0.d+0
      !             term(0) = term(0) + tvovo(b,j,a,i)

      !             do c = nocc + 1, nactive
      !                   term(1) = term(1) + tvv(b,c) * t2(a,c,i,j)
      !                   term(2) = term(2) + tvv(a,c) * t2(b,c,j,i)
      !             end do


      !             do k = 1, nocc
      !                   do l = 1, nocc
      !                         term(3) = term(3) + t2(a,b,i,k) * toooo(l,l,k,j)
      !                         term(4) = term(4) + t2(a,b,k,j) * toooo(l,l,k,i)
      !                   end do
      !             end do

      !             term(3) = term(3) * (-2.0d+0)
      !             term(4) = term(4) * (-2.0d+0)

      !             do k = 1, nocc
      !                   term(5) = term(5) + too(k,j) * t2(a,b,i,k)
      !                   term(6) = term(6) + too(k,i) * t2(a,b,k,j)
      !             end do

      !             term(5) = term(5) * (-1.0d+0)
      !             term(6) = term(6) * (-1.0d+0)

      !             do c = nocc + 1, nactive
      !                   do l = 1, nocc
      !                         do d = nocc + 1, nactive
      !                               do k = 1, nocc
      !                                     term(7) = term(7) + t2(a,b,k,l) * t2(c,d,i,j) * tovov(l,d,k,c)
      !                                     term(8) = term(8) + t2(a,b,k,j) * t2(c,d,i,l) * tovov(l,d,k,c)
      !                                     term(9) = term(9) + t2(a,c,i,j) * t2(b,d,k,l) * tovov(l,d,k,c)
      !                                     term(10) = term(10) + t2(a,d,k,l) * t2(b,c,j,i) * tovov(l,d,k,c)
      !                                     term(11) = term(11) + t2(a,c,k,i) * t2(b,d,j,l) * tovov(l,d,k,c)
      !                                     term(12) = term(12) + t2(a,d,i,l) * t2(b,c,k,j) * tovov(l,d,k,c)
      !                               end do
      !                         end do
      !                   end do
      !             end do

      !             term(8) = term(8) * (-2.0d+0)
      !             term(9) = term(9) * (-2.0d+0)
      !             term(10) = term(10) * (-2.0d+0)
      !             term(11) = term(11) * (-2.0d+0)
      !             term(12) = term(12) * (-2.0d+0)

      !             do d = nocc + 1, nactive
      !                   do l = 1, nocc
      !                         do k = 1, nocc
      !                               do c = nocc + 1, nactive
      !                                     term(13) = term(13) + t2(a,b,k,j) * t2(d,c,i,l) * tovov(l,d,k,c)
      !                                     term(14) = term(14) + t2(a,b,i,k) * t2(d,c,j,l) * tovov(l,d,k,c)
      !                                     term(15) = term(15) + t2(a,c,i,k) * t2(b,d,j,l) * tovov(l,c,k,d)
      !                               end do
      !                         end do
      !                   end do
      !             end do

      !             term(15) = term(15) * (-2.0d+0)

      !             do c = nocc + 1, nactive
      !                   do l = 1, nocc
      !                         do k = 1, nocc
      !                               do d = nocc + 1, nactive
      !                                     term(16) = term(16) + t2(a,b,i,k) * t2(c,d,j,l) * tovov(l,d,k,c)
      !                                     term(17) = term(17) + t2(a,c,i,k) * t2(b,d,j,l) * tovov(l,d,k,c)
      !                               end do
      !                         end do
      !                   end do
      !             end do

      !             term(16) = term(16) * (-2.0d+0)
      !             term(17) = term(17) * 4.0d+0

      !             do c = nocc + 1, nactive
      !                   do d = nocc + 1, nactive
      !                         do k = 1, nocc
      !                               do l = 1, nocc
      !                                     term(18) = term(18) + t2(a,d,k,j) * t2(b,c,l,i) * tovov(l,d,k,c)
      !                                     term(19) = term(19) + t2(a,c,k,i) * t2(b,d,l,j) * tovov(l,d,k,c)
      !                               end do
      !                         end do
      !                   end do
      !             end do

      !             do d = nocc + 1, nactive
      !                   do l = 1, nocc
      !                         do c = nocc + 1, nactive
      !                               do k = 1, nocc
      !                                     term(20) = term(20) + t2(a,d,i,l) * t2(b,c,k,j) * tovov(l,c,k,d)
      !                                     term(21) = term(21) + t2(a,c,k,i) * t2(b,d,j,l) * tovov(l,c,k,d)
      !                                     term(22) = term(22) + t2(a,c,i,j) * t2(b,d,k,l) * tovov(l,c,k,d)
      !                                     term(23) = term(23) + t2(a,d,k,l) * t2(b,c,j,i) * tovov(l,c,k,d)
      !                               end do
      !                         end do
      !                   end do
      !             end do

      !             do c = nocc + 1, nactive
      !                   do k = 1, nocc
      !                         term(24) = term(24) + t2(a,c,k,j) * tvvoo(b,c,k,i)
      !                         term(25) = term(25) + t2(b,c,k,i) * tvvoo(a,c,k,j)
      !                         term(26) = term(26) + t2(b,c,k,j) * tvoov(a,i,k,c)
      !                         term(27) = term(27) + t2(a,c,k,i) * tvoov(b,j,k,c)
      !                   end do
      !             end do

      !             term(24) = term(24) * (-1.0d+0)
      !             term(25) = term(25) * (-1.0d+0)
      !             term(26) = term(26) * (-1.0d+0)
      !             term(27) = term(27) * (-1.0d+0)

      !             do k = 1, nocc
      !                   do c = nocc + 1, nactive
      !                         term(28) = term(28) + t2(b,c,j,k) * tvvoo(a,c,k,i)
      !                         term(29) = term(29) + t2(a,c,i,k) * tvvoo(b,c,k,j)

      !                         term(30) = term(30) + t2(a,c,i,j) * tvvoo(b,c,k,k)
      !                         term(31) = term(31) + t2(b,c,j,i) * tvvoo(a,c,k,k)
      !                         term(32) = term(32) + t2(b,c,j,k) * tvoov(a,i,k,c)
      !                         term(33) = term(33) + t2(a,c,i,k) * tvoov(b,j,k,c)

      !                         term(34) = term(34) + t2(a,c,i,j) * tvoov(b,k,k,c)
      !                         term(35) = term(35) + t2(b,c,j,i) * tvoov(a,k,k,c)
      !                   end do
      !             end do

      !             term(28) = term(28) * (-1.0d+0)
      !             term(29) = term(29) * (-1.0d+0)
      !             term(30) = term(30) * 2.0d+0
      !             term(31) = term(31) * 2.0d+0
      !             term(32) = term(32) * 2.0d+0
      !             term(33) = term(33) * 2.0d+0
      !             term(34) = term(34) * (-1.0d+0)
      !             term(35) = term(35) * (-1.0d+0)

      !             do c = nocc + 1, nactive
      !                   do d = nocc + 1, nactive
      !                         term(36) = term(36) + t2(c,d,i,j) * tvvvv(a,c,b,d)!tvvvv(b,d,a,c)
      !                   end do
      !             end do

      !             do l = 1, nocc
      !                   do k = 1, nocc
      !                         term(37) = term(37) + t2(a,b,k,l) * toooo(l,j,k,i)
      !                         term(38) = term(38) + t2(a,b,k,j) * toooo(l,i,k,l)
      !                         term(39) = term(39) + t2(a,b,i,k) * toooo(l,j,k,l)
      !                   end do
      !             end do

      ! !             ! w = 0.d+0
      ! !             ! do s = 36, 39
      ! !             !       w = w + term(s)
      ! !             ! end do
      ! !             w = term(36)!w + term(3)+term(4)
      ! ! !            if(a.eq.6.and.i.eq.2.and.b.eq.6.and.j.eq.1)then
      ! ! !            print*, abs(desp(t2, t1, nocc, nactive, a, i, b, j) - w)
      ! !             if(abs(desp(t2, t1, nocc, nactive, a, i, b, j) - w).gt.1.d-10)then
      ! !                   print*, a, i, b, j
      ! !                   print*, desp(t2, t1, nocc, nactive, a, i, b, j) 
      ! !                   print*, w
      ! !                    stop
      ! !              end if

      !             automatic_t2_h = 0.d+0 
      !             do s = 0, 39
      !                   automatic_t2_h = automatic_t2_h + term(s)
      !             end do

      ! !            automatic_t2_h = desp(t2, t1, nocc, nactive, a, i, b, j) 

      !       end function automatic_t2_h


      function desp(t2, t1, nocc, nactive, a, i, b, j) 
            double precision :: desp
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            integer, intent(in) :: a, i, b, j 
            integer :: s ,c,k,l,d 
            double precision, dimension(0:22) :: term 
            double precision :: r1, r2, r3, q1, q2
            desp = 0.d+0

            q1 = 0.d+0

            q1 = tvovo(a,i,b,j)
            do c = nocc+1, nactive
                  q1 = q1  + t2(a,c,i,j)*tvv(b,c)&
                        +t2(b,c,j,i)*tvv(a,c)
            end do

            do c = nocc+1, nactive
                  do k = 1, nocc
                        q1 = q1 + t2(a,c,i,j)*(2.d+0*tvvoo(b,c,k,k) - tvoov(b,k,k,c))&
                              +t2(b,c,j,i)*(2.d+0*tvvoo(a,c,k,k) - tvoov(a,k,k,c))
                  end do
            end do

            do k = 1, nocc
                  q1 = q1 - t2(a,b,i,k)*too(k,j)&
                        - t2(b,a,j,k)*too(k,i)
            end do

            do k = 1, nocc
                  do l = 1, nocc
                        q1 = q1 - t2(a,b,i,k)*(2.d+0*toooo(k,j,l,l) -toooo(k,l,l,j))&
                              -t2(a,b,i,k)*(2.d+0*toooo(k,i,l,l) - toooo(k,l,l,i))
                  end do
            end do

            do c = nocc+1, nactive
                  do k = 1, nocc
                        q1 = q1 + t2(a,c,i,k)*(2.d+0*tvoov(b,j,k,c) - tvvoo(b,c,k,j))&
                              + t2(b,c,j,k)*(2.d+0*tvoov(a,i,k,c) - tvvoo(a,c,k,i))
                  end do
            end do

            q2 = 0.d+0
            do c = nocc+1, nactive
                  do k = 1, nocc
                        q2 = q2 - t2(a,c,k,j)*tvvoo(b,c,k,i)&
                              - t2(b,c,k,i)*tvvoo(a,c,k,j)&
                              - t2(a,c,k,i)*tvoov(b,j,k,c)&
                              - t2(b,c,k,j)*tvoov(a,i,k,c)
                  end do
            end do

            do c = nocc+1, nactive
                  do d = nocc+1, nactive
                        q2 = q2 + t2(c,d,i,j)*tvvvv(a,c,b,d)
                  end do
            end do

            do k = 1, nocc
                  do l = 1, nocc
                        q2 = q2 + t2(a,b,k,l)*toooo(k,i,l,j)
                  end do
            end do


            r1 = 0.d+0
            do c = nocc+1, nactive
                  do d = nocc+1, nactive
                        do k = 1, nocc
                              do l = 1, nocc
                                    r1 = r1 + t2(a,c,i,k)*t2(b,d,j,l)*2.d+0*tovov(k,c,l,d)&
                                          + t2(b,c,j,k)*t2(a,d,i,l)*2.d+0*tovov(k,c,l,d)&
                                          - t2(a,c,i,k)*t2(b,d,j,l)*tovov(k,d,l,c)&
                                          - t2(b,c,j,k)*t2(a,d,i,l)*tovov(k,d,l,c)
                              end do
                        end do
                  end do
            end do

            r2 = 0.d+0
            do c = nocc+1, nactive
                  do d = nocc+1, nactive
                        do k = 1, nocc
                              do l = 1, nocc
                                    r2 = r2 - t2(a,b,i,k)*t2(c,d,l,j)&
                                          * (2.d+0*tovov(l,c,k,d) - tovov(l,d,k,c))&
                                          - t2(b,a,j,k)*t2(c,d,l,i)&
                                          * (2.d+0*tovov(l,c,k,d) - tovov(l,d,k,c))&
                                          - t2(a,c,i,k)*t2(b,d,l,j)&
                                          * (2.d+0*tovov(k,c,l,d) - tovov(k,d,l,c))&
                                          - t2(b,c,j,k)*t2(a,d,l,i)&
                                          * (2.d+0*tovov(k,c,l,d) - tovov(k,d,l,c))&
                                          - t2(a,c,i,j)*t2(b,d,k,l)&
                                          * (2.d+0*tovov(k,c,l,d) - tovov(k,d,l,c))&
                                          - t2(b,c,j,i)*t2(a,d,k,l)&
                                          * (2.d+0*tovov(k,c,l,d) - tovov(k,d,l,c))
                              end do
                        end do
                  end do
            end do

            r3 = 0.d+0
            do c = nocc+1, nactive
                  do d = nocc+1, nactive
                        do k = 1, nocc
                              do l = 1, nocc
                                    r3 = r3 + 0.5d+0*tovov(k,c,l,d)*&
                                          (t2(a,b,k,l)*t2(c,d,i,j)&
                                          +t2(a,c,k,i)*t2(b,d,l,j)&
                                          +t2(a,d,k,j)*t2(b,c,l,i))&
                                          +0.5d+0*tovov(k,c,l,d)*&
                                          (t2(b,a,k,l)*t2(c,d,j,i)&
                                          +t2(b,c,k,j)*t2(a,d,l,i)&
                                          +t2(b,d,k,i)*t2(a,c,l,j))
                              end do
                        end do
                  end do
            end do
            desp = desp + q1 + q2 + r1 + r2 + r3

      end function desp

end module ccsd_helgaker_test
