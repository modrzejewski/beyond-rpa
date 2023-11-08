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
module blas3
      use arithmetic
      use math_constants
      use display

      implicit none


contains



      subroutine la_scal_olenkifer(a, alpha)
            !
            ! A <- ALPHA * A
            !
            real(F64), dimension(:), intent(inout) :: a
            real(F64), intent(in)                  :: alpha

            integer :: k, n

            n = size(a, dim=1)

            !$omp parallel shared(n, a, alpha) private(k) default(none)
            !$omp do schedule(static)
            do k = 1, n
                  a(k) = alpha * a(k)
            end do
            !$omp end do nowait
            !$omp end parallel
      end subroutine la_scal_olenkifer


end module blas3
