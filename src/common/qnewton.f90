module qnewton
      use math_constants

      implicit none

      integer, parameter :: nvars = 2
      integer, parameter :: maxiter = 50
      double precision, parameter :: conv = 1.d-8

contains
      
      function qndp(x, y)
            double precision :: qndp

            double precision, dimension(:), intent(in) :: x, y
            
            integer :: i

            qndp = zero
            do i = 1, nvars
                  qndp = qndp + x(i) * y(i)
            end do
      end function qndp


      subroutine qnh0v(h0, v, w)
            double precision, dimension(:, :), intent(in) :: h0
            double precision, dimension(:), intent(in) :: v
            double precision, dimension(:), intent(out) :: w

            w = matmul(h0, v(1:nvars))
      end subroutine qnh0v


      subroutine qnw(n, h0, v, d, y, delta, grad0, grad1, w)
            integer, intent(in) :: n
            double precision, dimension(:, :), intent(in) :: h0
            double precision, dimension(:), intent(in) :: v
            double precision, dimension(:), intent(in), target :: d
            double precision, dimension(:), intent(inout), target ::  delta
            double precision, dimension(:), intent(inout), target :: y
            double precision, dimension(:), intent(in) :: grad0, grad1
            double precision, dimension(:), intent(out) :: w

            integer :: i, k0
            double precision :: s1, s2, s3, s4, s5, s6
            double precision :: t0, t1, t2, t3, t4

            double precision, dimension(:), pointer :: deltanew, ynew
            double precision, dimension(:), pointer :: di, deltai, yi

            w = zero
            call qnh0v(h0, v, w)

            k0 = (n - 2) * nvars + 1
            deltanew => delta(k0:)
            ynew => y(k0:)
            ynew = zero
            deltanew = grad1 - grad0
            call qnh0v(h0, deltanew, ynew)

            k0 = 1
            do i = 1, n - 2
                  di => d(k0:)
                  deltai => delta(k0:)
                  yi => y(k0:)

                  s1 = one / qndp(di, deltai)
                  s2 = one / qndp(deltai, yi)
                  s3 = qndp(di, v)
                  s4 = qndp(yi, v)
                  s5 = qndp(di, deltanew)
                  s6 = qndp(yi, deltanew)
                  
                  t0 = one + s1 / s2
                  t2 = s1 * s3
                  t4 = s1 * s5
                  t1 = t0 * t2 - s1 * s4
                  t3 = t0 * t4 - s1 * s6
                  
                  w = w + t1 * di - t2 * yi
                  ynew = ynew + t3 * di - t4 * yi

                  k0 = k0 + nvars
            end do
            
            k0 = (n - 2) * nvars + 1
            di => d(k0:)
            deltai => delta(k0:)
            yi => y(k0:)
            
            s1 = one / qndp(di, deltai)
            s2 = one / qndp(di, yi)
            s3 = qndp(di, v)
            s4 = qndp(yi, v)

            t2 = s1 * s3
            t1 = (one + s1 / s2) * t2 - s1 * s4

            w = w + t1 * di - t2 * yi
      end subroutine qnw

      
      subroutine qnh0(h0, x)
            double precision, dimension(:, :), intent(out) :: h0
            double precision, dimension(:), intent(in) :: x

            h0(1, 1) = one / (12.d+0 * x(1)**2 + cos(x(1)))
            h0(2, 2) = one / (2.d+0)
      end subroutine qnh0
      
      
      subroutine qngrad(x, g)
            double precision, dimension(:), intent(in) :: x
            double precision, dimension(:), intent(out) :: g

            g(1) = 4.d+0 * x(1)**3 - sin(x(1))
            g(2) = 2.d+0 * x(2)
      end subroutine qngrad
      
      
      function qnnorm(x)
            double precision :: qnnorm

            double precision, dimension(:), intent(in) :: x
            integer :: i

            qnnorm = zero
            do i = 1, nvars
                  qnnorm = qnnorm + abs(x(i))
            end do
      end function


      subroutine qndriver(x0, xopt, res)
            double precision, dimension(:), intent(in) :: x0
            double precision, dimension(:), intent(out) :: xopt
            double precision, intent(out) :: res

            double precision, dimension(nvars) :: x
            double precision, dimension(nvars, nvars) :: h0
            double precision, dimension(nvars) :: grad0, grad1
            double precision, dimension(maxiter *  nvars), target :: y, d, delta
            double precision, dimension(:), pointer :: di
            integer :: n
            integer :: k0

            res = zero
            x = x0
            call qngrad(x, grad0)
            call qnh0(h0, x)
            k0 = 1
            di => d(k0:)
            call qnh0v(h0, -grad0, di)
            x = x + di(1:nvars)

            do n = 2, maxiter
                  res = qnnorm(di)
                  if (res .lt. conv) then
                        exit
                  end if

                  call qngrad(x, grad1)
                  k0 = k0 + nvars
                  di => d(k0:)
                  call qnw(n, h0, -grad1, d, y, delta, grad0, grad1, di)
                  x = x + di
                  grad0 = grad1
            end do

            xopt = x
      end subroutine qndriver
end module qnewton
