module scuseria_intermediates

      use ccsd_transformed_integrals
!      use cc3_intermediates
      use gparam

      implicit none
      ! ---------------------------------------------------------------------------
      ! Scuseria, G.; Scheiner, A. ; Lee, T. et al. The Journal of Chemical Physics
      ! 86 no 5 p.2881-2890 (1987)
      ! 
      ! ---------------------------------------------------------------------------

contains

      function int_a1(t2, t1, nocc, nactive, beta, u)
            double precision :: int_a1
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc), intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, u


            integer :: i, j, a
            double precision :: go, gv, ddd, dd, f, eeecd
            go = 0.d+0
            gv = 0.d+0
            dd = 0.d+0
            ddd = 0.d+0
            f = 0.d+0
            eeecd = 0.d+0

            do i = 1, nocc
                  go  = go + int_a5(t2, t1, nocc, nactive,i, u)   *t1( beta, i)
            end do

            do a = nocc + 1, nactive
                  gv = gv + int_a6(t2, t1, nocc, nactive,beta, a) * t1(a, u)
            end do

            do i = 1, nocc
                  do j = 1, nocc
                        do a = nocc + 1, nactive
                              dd = dd + (2.d+0 * d1(t1, nocc, nactive,a, j, i, u)&
                                    - d1(t1, nocc, nactive,a, i, j, u))   *t1(a, j) * t1(beta, i)
                        end do
                  end do
            end do

            do i = 1, nocc
                  do a = nocc + 1, nactive
                        ddd = ddd + (2.d+0 * (d2a(t2, nocc, nactive,i, a, beta, u)&
                              - d2b(t2, nocc, nactive,i, a, beta, u)) + &
                              d2c(t2, nocc, nactive,i, a, beta, u))&
                              *t1(a, i)
                  end do
            end do

            do a = nocc + 1, nactive
                  f = f + f2a(t2, nocc, nactive,beta, a, u)
            end do

            do i = 1, nocc
                  eeecd = eeecd + 0.5d+0 * (e2a(t2, nocc, nactive, i, u, beta, i) - &
                        e2b(t2, nocc, nactive,i, u, beta, i))&
                        + e2c(t2, nocc, nactive,i, u, beta, i) + c1(t1, nocc, nactive,i, u, beta)&
                        - 2.d+0 * d1(t1, nocc, nactive,beta, u, i, i) 
            end do

            int_a1 = go - gv -dd - ddd - f + eeecd

      end function int_a1

      function int_a3(t2, t1, nocc, nactive,  beta, u, gamma, v)
            double precision :: int_a3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, u, gamma, v
            integer :: i, a
            double precision :: go, gv


            go = 0.d+0
            gv = 0.d+0

            do a = nocc + 1, nactive
                  gv = gv + int_a6(t2, t1, nocc, nactive, gamma, a) * t2(beta,a, u, v)
            end do

            do i = 1, nocc
                  go = go + int_a5(t2, t1, nocc, nactive,i, v) * t2(beta, gamma, u, i)
            end do

            int_a3 = gv - go

      end function int_a3

      function int_a4(t2, t1, nocc, nactive, beta, u, gamma, v)
            double precision :: int_a4
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, u, gamma, v

            int_a4 = s_abe(t2, t1, nocc, nactive,beta, u, gamma, v) - s_ccdf(t2, t1, nocc, nactive,beta, u, gamma, v)&
                  + s_ddd(t2, t1, nocc, nactive, beta, u, gamma, v)&
                  + s_def(t2, t1, nocc, nactive, beta, u, gamma,v)&
                  - s_ef(t2, t1, nocc, nactive, beta, u, gamma, v)

      end function int_a4

      function s_abe(t2, t1, nocc, nactive,beta, u, gamma, v)
            double precision :: s_abe
            integer, intent(in) :: beta, gamma, u, v
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2


            s_abe = &
                  0.5d+0 * a2p(t2, t1, nocc, nactive,beta, u, gamma, v) &
                  + 0.5d+0 * b2p(t2, t1, nocc, nactive,beta, u, gamma, v) &
                  - e1s(t1, nocc, nactive,beta, u, gamma, v)

      end function s_abe

      function s_ccdf(t2, t1, nocc, nactive, beta, u, gamma, v)
            double precision :: s_ccdf
            integer, intent(in) :: beta, u, gamma, v
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2



            s_ccdf = c2(t2, nocc, nactive,u, beta, gamma, v) &
                  + c2p(t2, t1, nocc, nactive,u, beta, gamma, v) - d2a(t2, nocc, nactive,u, beta, gamma,v)&
                  - f12(t1, nocc, nactive,u, beta, gamma, v)

      end function s_ccdf

      function s_ddd(t2, t1, nocc, nactive, beta, u, gamma, v)
            double precision :: s_ddd
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            double precision :: ddd1, ddd2, ddd3
            integer, intent(in) :: beta, u, gamma, v
            integer :: i, a


            ddd1 = 0.d+0
            ddd2 = 0.d+0
            ddd3 = 0.d+0

            do i = 1, nocc
                  do a = nocc + 1, nactive
                        ddd1 = ddd1 + (d2a(t2, nocc, nactive,i, a, gamma, v) &
                              - d2b(t2, nocc, nactive,i, a, gamma, v)) * (t2(beta, a, u, i)&
                              - big_t(t2, t1, nocc, nactive, a, beta, u, i))

                        ddd2 = ddd2 + 0.5d+0 * d2c(t2, nocc, nactive,i, a, gamma, v) * t2(beta, a, u, i)

                        ddd3 = ddd3 + d2c(t2, nocc, nactive,i, a, gamma, u) * &
                              big_t(t2, t1, nocc, nactive, a, beta, v, i)
                  end do
            end do

            s_ddd = ddd1 + ddd2 + ddd3

      end function s_ddd

      function s_def(t2, t1, nocc, nactive, beta, u, gamma,v)
            double precision :: s_def
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, u, gamma, v
            integer :: i, j
            double precision :: def1, def2, def3


            def1 = 0.d+0
            def2 = 0.d+0
            def3 = 0.d+0

            do i = 1, nocc
                  do j = 1, nocc
                        def1 = def1 + (0.5d+0 * d2p(t2, t1, nocc, nactive,i, u, j, v) +&
                              e1(t1, nocc, nactive,i, u, j,v))&
                              * tau2(t2, t1, nocc, nactive,beta, gamma, i, j)

                  end do

                  def2 = def2 + (d1(t1, nocc, nactive,beta, u, i, v) + &
                        f2p(t2, t1, nocc, nactive,beta, u, i, v))*&
                        t1(gamma, i)
                  def3 = def3 + (e2a(t2, nocc, nactive,i, u, gamma, v) - &
                        e2b(t2, nocc, nactive,i, u, gamma, v) &
                        - e2c(t2, nocc, nactive,i, v, gamma, u)) * t1(beta, i)
            end do

            s_def = def1 - def2 - def3

      end function s_def

      function s_ef(t2, t1, nocc, nactive, beta, u, gamma, v)
            double precision :: s_ef
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, u, gamma, v
            integer :: i, a


            s_ef = 0.d+0

            do i = 1, nocc
                  do a = nocc + 1, nactive
                        s_ef = s_ef + f11(t1, nocc, nactive,gamma, a, i, v) * t2(beta, a, u, i)&
                              + f11(t1, nocc, nactive,beta, a, i, v) * t2(a, gamma, u, i)&
                              - f12(t1, nocc, nactive,i, a, gamma, v) * tt(t2, beta, a, u, i, nocc, nactive)
                  end do
            end do

      end function s_ef


      function int_a5(t2, t1, nocc, nactive, i, u)
            integer, intent(in) :: i, u
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            double precision :: int_a5
            integer :: j


            int_a5 = 0.d+0

            do j = 1, nocc
                  int_a5 = int_a5 + &
                        2.d+0*(e1(t1, nocc, nactive,i, u, j, j) + d2p(t2, t1, nocc, nactive,i,u,j,j))&
                        - e1(t1, nocc, nactive,j, u, i, j)  - d2p(t2, t1, nocc, nactive,j, u, i, j)
            end do

      end function int_a5

      function int_a6(t2, t1, nocc, nactive,beta, a)
            integer, intent(in) :: beta, a
            double precision :: int_a6
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer :: b

            int_a6 = 0.d+0

            do b = nocc + 1, nactive
                  int_a6 = int_a6 + 2.d+0 *&
                        (f1s(t1, nocc, nactive,beta, a, b, b) - d2ps(t2, t1, nocc, nactive,beta, a, b, b))&
                        - f1s(t1, nocc, nactive,beta, b, b, a) + d2ps(t2, t1, nocc, nactive, beta, b, b, a)
            end do

      end function int_a6

      function a2p(t2, t1, nocc, nactive, beta, u, gamma, v)

            double precision :: a2p
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, gamma, u, v
            integer :: i, j

            a2p = 0.d+0

            do j = 1, nocc
                  do i = 1, nocc
                        a2p = a2p + &
                              oooo(i, u, j, v) * tau2(t2, t1, nocc, nactive, beta, gamma, i, j)

                  end do
            end do

      end function a2p

      function b2p(t2, t1, nocc, nactive, beta, u, gamma, v)
            double precision :: b2p
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, gamma, u, v
            integer :: a,  b


            b2p = 0.d+0

            do a = nocc + 1, nactive
                  do b = nocc + 1, nactive
                        b2p = b2p + vvvv(a, beta, b, gamma) * tau2(t2, t1, nocc, nactive,a, b, u, v)
                  end do
            end do

      end function b2p

      function c1(t1, nocc, nactive, i, u, beta)
            double precision :: c1
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            integer, intent(in) :: i, u, beta
            integer :: a


            c1 = 0.d+0

            do a = nocc + 1, nactive
                  c1 = c1 + vvoo(a, beta, u, i)   *t1(a, i) 
            end do


      end function c1

      function c2(t2, nocc, nactive, u, beta, gamma, v)
            double precision :: c2
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: u, beta, gamma, v
            integer :: i, a


            c2 = 0.d+0

            do i = 1, nocc
                  do a = nocc + 1, nactive
                        c2 = c2 + vvoo(beta, a, i, u) * t2(gamma, a, v,i)
                  end do
            end do

      end function c2

      function c2p(t2, t1, nocc, nactive, u, beta, gamma, v)
            double precision :: c2p
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, gamma, u, v
            integer :: i, a


            c2p = 0.d+0

            do i = 1, nocc
                  do a = nocc + 1, nactive
                        c2p = c2p + &
                              vvoo(a, gamma, i, u)* tau2(t2, t1, nocc, nactive, beta, a, i, v)
                  end do
            end do

      end function c2p

      function d1(t1, nocc, nactive, beta, u, i, v)
            double precision :: d1
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            integer, intent(in) :: beta, u, i, v
            integer :: a


            d1   = 0.d+0

            do a = nocc + 1, nactive
                  d1 = d1 + vovo(beta, u, a, i)   *t1(a, v)
            end do

      end function d1

      function d2p(t2, t1, nocc, nactive, i, u, j, v)
            double precision :: d2p
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: i, u, j, v
            integer :: a, b


            d2p  = 0.d+0

            do b = nocc + 1, nactive
                  do a = nocc + 1, nactive
                        d2p = d2p  &
                              + vovo(a, i, b, j) * tau2(t2, t1, nocc, nactive, a, b, u, v)
                  end do
            end do

      end function d2p

      function d2ps(t2, t1, nocc, nactive, beta, a, b, c)
            double precision :: d2ps
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, a, b, c
            integer :: i, j


            d2ps = 0.d+0

            do j = 1, nocc
                  do i = 1, nocc
                        d2ps = d2ps&
                              + vovo(a, i, c, j)  * tau2(t2, t1, nocc, nactive, beta, b, i, j)
                  end do
            end do

      end function d2ps

      function d2a(t2, nocc, nactive, i, a, gamma, v)
            double precision :: d2a
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: i, a, gamma, v
            integer :: j, b


            d2a  = 0.d+0

            do j = 1, nocc
                  do b = nocc + 1, nactive
                        d2a = d2a&
                              + vovo(b, j, a, i)* tt(t2, gamma, b, v, j, nocc, nactive)
                  end do
            end do

      end function d2a

      function d2b(t2, nocc, nactive, i, a, gamma, v)
            double precision :: d2b
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: i, a, gamma, v
            integer :: j, b


            d2b  = 0.d+0

            do j = 1, nocc
                  do b = nocc + 1, nactive
                        d2b = d2b&
                              + vovo(b, i, a, j) * t2(gamma, b, v, j)
                  end do
            end do

      end function d2b

      function d2c(t2, nocc, nactive, i, a, gamma, v)
            double precision :: d2c
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: i, a, gamma, v
            integer :: j, b


            d2c  = 0.d+0

            do j = 1, nocc
                  do b = nocc + 1, nactive
                        d2c = d2c + &
                              vovo(b, i, a, j) * t2(b, gamma, v, j)
                  end do
            end do

      end function d2c

      function e1(t1, nocc, nactive, i, u, j, v)
            double precision :: e1
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            integer, intent(in) :: i, u, j, v
            integer :: a


            e1  = 0.d+0

            do a = nocc + 1, nactive
                  e1 = e1 + vooo(a, j, u, i) * t1(a, v)
            end do

      end function e1

      function e1s(t1, nocc, nactive, beta, u, gamma, v)
            double precision :: e1s
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            integer, intent(in) :: beta, gamma, u, v
            integer :: i


            e1s = 0.d+0

            do i = 1, nocc
                  e1s = e1s &
                        + vooo(beta, u, i, v)   *t1(gamma, i)
            end do

      end function e1s

      function e2a(t2, nocc, nactive, i, u, gamma, v)
            double precision :: e2a
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: i, gamma, u, v
            integer :: j, b


            e2a = 0.d+0

            do j = 1, nocc
                  do b = nocc + 1, nactive
                        e2a = e2a&
                              + vooo(b, j, u, i) * tt(t2, gamma, b, v, j, nocc, nactive)
                  end do
            end do

      end function e2a

      function e2b(t2, nocc, nactive, i, u, gamma, v)
            double precision :: e2b
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: i, u, gamma, v
            integer :: j, b


            e2b = 0.d+0

            do j = 1, nocc
                  do b = nocc + 1, nactive
                        e2b = e2b &
                              + vooo(b, i, u, j) * t2(gamma, b, v, j)
                  end do
            end do

      end function e2b

      function e2c(t2, nocc, nactive, i, u, gamma, v)
            double precision :: e2c
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: i, u, gamma, v
            integer :: j, b


            e2c = 0.d+0            

            do j = 1, nocc
                  do b = nocc + 1, nactive
                        e2c =  e2c + &
                              vooo(b, i, u, j) * t2(b, gamma, v, j)
                  end do
            end do

      end function e2c

      function f11(t1, nocc, nactive, beta, b, i, v)
            double precision :: f11
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            integer, intent(in) :: beta, b, i, v
            integer :: a


            f11 = 0.d+0

            do a = nocc+1, nactive
                  f11 = f11&
                        + vvvo(b, beta, a, i)  *t1(a, v)
            end do

      end function f11

      function f12(t1, nocc, nactive, i, b, beta, v)
            double precision :: f12
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            integer, intent(in) :: i, b, beta, v
            integer :: a


            f12 = 0.d+0

            do a = nocc + 1, nactive
                  f12 = f12  &
                        + vvvo(a, beta, b, i)   *t1(a, v)
            end do

      end function f12

      function f1s(t1, nocc, nactive, beta, a, b, c)
            double precision :: f1s
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            integer, intent(in) :: beta, a, b, c
            integer :: i


            f1s = 0.d+0

            do i = 1, nocc
                  f1s = f1s  + vvvo(a, beta, c, i) * t1(b, i)
            end do

      end function f1s

      function f2a(t2, nocc, nactive, beta, a, u)
            double precision :: f2a
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, a, u
            integer :: i, b


            f2a = 0.d+0

            do i = 1, nocc
                  do b = nocc + 1, nactive
                        f2a = f2a  + vvvo(a, beta, b, i) * tt(t2,a, b, u, i, nocc, nactive) 
                  end do
            end do

      end function f2a

      function f2p(t2, t1, nocc, nactive, beta, u, i, v)
            double precision :: f2p
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, u, i, v
            integer :: b, a


            f2p = 0.d+0

            do b = nocc + 1, nactive
                  do a = nocc + 1, nactive
                        f2p = f2p &
                              + vvvo(a, beta, b, i) * tau2(t2, t1, nocc, nactive, a, b, u, v)
                  end do
            end do

      end function f2p

      function tt(t2,a, b, u, i, nocc, nactive)
            double precision :: tt
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: a, b, u, i


            tt = 2.d+0 * t2(a,b, u, i ) - t2(b, a, u, i)

      end function tt

      function big_t(t2, t1, nocc, nactive, beta, gamma, u, v)
            double precision :: big_t
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, gamma, u, v


            big_t = 0.5d+0 * t2(beta, gamma, u, v) + t1(beta, u) * t1(gamma, v)
      end function big_t

      function tau2(t2, t1, nocc, nactive, beta, gamma, u, v)
            double precision :: tau2
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc) , intent(in) :: t1
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc) , intent(in) :: t2
            integer, intent(in) :: beta, gamma, u, v


            tau2 = t2(beta, gamma, u, v) + t1(beta, u) * t1(gamma, v)
      end function tau2

end module scuseria_intermediates
