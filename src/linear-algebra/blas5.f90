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
module blas5
      use arithmetic
      use math_constants
      use display
      use blas1
      use blas2
      use blas3
      use blas4

      implicit none


contains




      subroutine la_gramschmidt_olenkifer(colwise, a, k)
            ! -----------------------------------------------------------------------
            ! Gram-Schmidt orthonormalization.
            ! -----------------------------------------------------------------------
            ! COLWISE
            !     .TRUE. if vectors are stored in columns of A.
            !     .FALSE. if vectors are stored in rows of A.
            !
            ! A
            !     On entry, the columns of A are the vectors to be orthonormalized.
            !     On exit, the matrix A contains K + L orthonormal columns.
            ! K
            !     the first K vectors be assumed to be orthonormal.
            !     K==0 if A contains no orthonormal vectors.
            !
            logical, intent(in)                       :: colwise
            real(F64), dimension(:, :), intent(inout) :: a
            integer, intent(in)                       :: k

            integer :: n, l
            integer :: i, j
            real(F64) :: t, l2norm

            if (colwise) then
                  n = size(a, dim=1)
                  l = size(a, dim=2) - k
            else
                  n = size(a, dim=2)
                  l = size(a, dim=1) - k
            end if

            if (l < 0 .or. k < 0) then
                  call msg("INVALID ARGUMENT PASSED TO LA_GRAMSCHMIDT", &
                        priority=MSG_ERROR)
                  stop
            end if

            if (l == 0) return
            !
            ! Normalize each vector which is not yet normalized
            !
            do i = k + 1, k + l
                  if (colwise) then
                        call la_l2norm_olenkifer(l2norm, a(:, i))
                        call la_scal_olenkifer(a(:, i), ONE/l2norm)
                  else
                        call la_l2norm_olenkifer(l2norm, a(i, :))
                        call la_scal_olenkifer(a(i, :), ONE/l2norm)
                  end if
            end do
            !
            ! Project out non-orthogonal components from each
            ! of the recently appended coulumns
            !
            do i = k + 1, k + l
                  do j = 1, i - 1
                        if (colwise) then
                              call la_dot_olenkifer(t, a(:, j), a(:, i))
                              call la_axpy_olenkifer(a(:, i), -t, a(:, j))
                        else
                              call la_dot_olenkifer(t, a(j, :), a(i, :))
                              call la_axpy_olenkifer(a(i, :), -t, a(j, :))
                        end if
                  end do

                  if (colwise) then
                        call la_l2norm_olenkifer(l2norm, a(:, i))
                        call la_scal_olenkifer(a(:, i), ONE/l2norm)
                  else
                        call la_l2norm_olenkifer(l2norm, a(i, :))
                        call la_scal_olenkifer(a(i, :), ONE/l2norm)
                  end if
            end do
      end subroutine la_gramschmidt_olenkifer
      

end module blas5
