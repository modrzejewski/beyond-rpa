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
module blas2
      use arithmetic
      use math_constants
      use display

      implicit none


contains




      subroutine la_dot_olenkifer(dot, a, b)
            !
            ! DOT <- A^T B
            !
            real(F64), intent(out)              :: dot
            real(F64), dimension(:), intent(in) :: a
            real(F64), dimension(:), intent(in) :: b

            real(F64) :: dot_partial
            integer :: k, l, n, r
            integer, parameter :: blocksize = 2**10

            if (size(a) .ne. size(b)) then
                  call msg("INCONSISTENT DIMENSIONS ON ENTRY TO LA_DOT", &
                        priority=MSG_ERROR)
                  call imsg("SIZE(A)", size(a), MSG_ERROR)
                  call imsg("SIZE(B)", size(b), MSG_ERROR)
                  stop
            end if
            n = size(a)
            r = modulo(n, blocksize)
            dot = ZERO

            if (r .ne. 0) then
                  dot_partial = ZERO
                  do l = 1, r
                        dot_partial = dot_partial + a(l) * b(l)
                  end do
                  dot = dot + dot_partial
            end if
            if (n >= blocksize) then
                  !$omp parallel reduction(+:dot) &
                  !$omp private(k, l, dot_partial) &
                  !$omp shared(a, b, n, r) default(none)
                  !$omp do
                  do k = r+1, n, blocksize
                        dot_partial = ZERO
                        do l = 1, blocksize
                              dot_partial = dot_partial + a(k-1+l) * b(k-1+l)
                        end do
                        dot = dot + dot_partial
                  end do
                  !$omp end do nowait
                  !$omp end parallel
            end if
      end subroutine la_dot_olenkifer


end module blas2
