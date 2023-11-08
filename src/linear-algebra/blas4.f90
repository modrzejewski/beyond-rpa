! ------------------------------------------------------------
!     BASIC LINEAR ALGEBRA SUBPROGRAMS (SPECIAL CASES)
! ------------------------------------------------------------
! The code incuded in this module is designed for
! two special cases of vector/matrix  operations:
! 1) One of the dimensions is orders-of-magnitude smaller
!    than the other one.
! 2) Arrays have noncontiguous elements.
! In subroutines where summation of possibly large number of
! floating point numbers is performed, partial sums are used
! to prevent the accumulation of numerical error.
!
module blas4
      use arithmetic
      use math_constants
      use display

      implicit none


contains

  

      subroutine la_axpy_olenkifer(y, alpha, x)
            !
            ! Y <- ALPHA * X + Y
            !
            real(F64), dimension(:), intent(inout) :: y
            real(F64), intent(in)                  :: alpha
            real(F64), dimension(:), intent(in)    :: x

            integer :: k, n

            if (size(x) .ne. size(y)) then
                  call msg("INCONSISTENT DIMENSIONS ON ENTRY TO LA_AXPY", &
                        priority=MSG_ERROR)
                  call imsg("SIZE(X)", size(x), MSG_ERROR)
                  call imsg("SIZE(Y)", size(y), MSG_ERROR)
                  stop
            end if
            n = size(y)

            !$omp parallel shared(n, x, y, alpha) private(k) default(none)
            !$omp do schedule(static)
            do k = 1, n
!                  print*, 'alpha', alpha, x(k), y(k)
                  y(k) = y(k) + alpha * x(k)
            end do
            !$omp end do nowait
            !$omp end parallel
      end subroutine la_axpy_olenkifer

 
end module blas4
