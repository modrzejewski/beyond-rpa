module oboys
      implicit none
      
      !
      ! global tables
      !
      double precision, allocatable :: F0(:)
      double precision, allocatable :: tab(:)

      !
      ! global parameters
      !

      integer, parameter            :: MMM = 1000                   ! segment number
      integer, parameter            :: Ndd = MMM + 1                   ! node number
      integer, parameter            :: polyDeg = 5                  ! degree of legendre polynomial
      double precision, parameter   :: pii = 4.d+0 * atan(1.d+0)         ! pi
      double precision, parameter   :: x0 = 1.d+0, x1 = 30.d+0      ! limits of med-x interval
      double precision, parameter   :: deltax = (x1 - x0) / MMM          ! length of segment
      integer, parameter            :: steps = 1000
      double precision, parameter   :: ddx = (50.d+0-0.d+0) / steps

 
      
contains
      
      subroutine oboys_init()
            !
            ! local variables
            !
            double precision :: xx                                   ! variable for x coordinate
            double precision :: xsqr                                 ! sqrt(xx)
            integer, parameter :: left = 1 - (polyDeg+1) / 2         ! left limit of F0 table
            integer, parameter :: right = Ndd + (polyDeg+1) / 2        ! right limit of F0 table 
            integer :: i
            allocate(F0(left:right))
            allocate(tab(left:right))
            
            xx = x0 - (polyDeg+1) / 2 * deltax
            do i = left, right
                  xsqr = sqrt(xx)
                  F0(i) = sqrt(pii / (4.d+0 * xx)) * erf(xsqr)
                  tab(i) = xx
                  xx = xx + deltax
            end do

      end subroutine oboys_init

      
      subroutine oboys_free()
            deallocate(f0)
            deallocate(tab)
      end subroutine oboys_free

      
      subroutine F_m(x, m, F)
            double precision, intent(in)                 :: x
            integer, intent(in)                          :: m
            double precision, dimension(0:), intent(out) :: F
            
            
            if(x.lt.x0)then
                  call small_x(x, m, F)
            else if(x.gt.x1)then
                  F = asymptotic(x)
            else
                  call interpol(x, F(0))
            end if

            if(m.ne.0)then
                  if(x.lt.x0)then
                        call small_x(x, m, F)
                  else
                        call med_x(x, m, F)
                  end if
            end if
                  
      end subroutine F_m

      subroutine interpol(x, F)
            double precision, intent(in)                 :: x
            double precision, intent(out) :: F
            !
            ! local variables
            !
            integer :: i, j
            integer :: idxstart
            integer :: idx
            double precision :: il
            double precision :: xa, xb
            double precision :: di, dj
            
            idx = floor((x - x0) / deltax)  + 1
            idxstart = idx - polyDeg / 2
            
            F = 0.d+0
            
            xa = (idxstart-1) * deltax + x0
            di = 0.d+0
            do i = 0, polyDeg
                  xb = (idxstart-1) * deltax + x0
                  il = 1.d+0
                  dj = 0.d+0
                  do j = 0, polyDeg
                        if(j.eq.i)then
                              il = il
                        else
                              il = il * (x - (xb + dj * deltax))/(xa + di * deltax - (xb + dj * deltax))
                        end if
                        dj = dj + 1.d+0
                  end do
                  F = F + F0(idxstart + i) * il
                  di = di + 1.d+0
            end do
      end subroutine interpol

      subroutine small_x(x, m, F)
            double precision, intent(in)                   :: x
            double precision, dimension(0:), intent(inout) :: F
            integer, intent(in)                            :: m 
            double precision :: Fn
            integer :: n
            integer :: i
            double precision :: e

            e = exp(-x)

            n = m + 10
            Fn = expansion(x, n)

            do i = n, 0, -1
                  if(i.le.m)then
                        F(i) = Fn
                  end if
                  Fn = 1.d+0 / dble(2 * i - 1) * (2.d+0 * x * Fn + e)
            end do
            
      end subroutine small_x
      
      subroutine med_x(x, m, F)
            double precision, intent(in)                   :: x
            double precision, dimension(0:), intent(inout) :: F
            integer , intent(in)                           :: m
            double precision :: e
            integer :: i
            
            e = exp(-x)

            do i = 1, m
                  F(i) = 1.d+0 / (2.d+0 * x) * (dble(2.d+0 * i - 1) * F(i-1) - e)
            end do
                       
      end subroutine med_x

      function asymptotic(x)
            double precision :: asymptotic
            double precision, intent(in)  :: x
            
            asymptotic = sqrt(pii / x) / 2.d+0
            
      end function asymptotic

      function expansion(x, m)
            double precision :: expansion
            double precision, intent(in) :: x
            integer, intent(in)          :: m
            double precision :: il
            integer, parameter :: N = 5 
            integer :: i, j
            double precision :: e

            e = exp(-x)

            expansion = 0.d+0
            do i = 0, N
                  il = 1.d+0
                  do j = 0, i
                        il = il *(2.d+0 * m + 2.d+0*j + 1.d+0)
                  end do
                  expansion = expansion + (2.d+0 * x)**i / il
            end do
            expansion = e * expansion
            
      end function expansion

end module oboys
