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
module blas1
      use arithmetic
      use math_constants
      use display

      implicit none

contains

 


      subroutine la_l2norm_olenkifer(l2norm, a)
            !
            ! L2NORM <- SQRT(A^T A)
            !
            real(F64), intent(out)              :: l2norm
            real(F64), dimension(:), intent(in) :: a

            real(F64) :: l2norm_partial
            integer :: k, l, n, r
            integer, parameter :: blocksize = 2**10

            n = size(a, dim=1)
            r = modulo(n, blocksize)
            l2norm = ZERO
            if (r .ne. 0) then
                  l2norm_partial = ZERO
                  do l = 1, r
                        l2norm_partial = l2norm_partial + a(l)**2

                  end do
                  l2norm = l2norm + l2norm_partial
            end if
            if (n >= blocksize) then
                  !$omp parallel reduction(+:l2norm) &
                  !$omp private(k, l, l2norm_partial) &
                  !$omp shared(a, n, r) default(none)
                  !$omp do
                  do k = r+1, n, blocksize
                        l2norm_partial = ZERO
                        do l = 1, blocksize
                              l2norm_partial = l2norm_partial + a(k-1+l)**2
                        end do
                        l2norm = l2norm + l2norm_partial
                  end do
                  !$omp end do nowait
                  !$omp end parallel
            end if


            l2norm = sqrt(l2norm)
      end subroutine la_l2norm_olenkifer

end module blas1
