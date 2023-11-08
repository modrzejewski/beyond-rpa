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
module blas6
      use arithmetic
      use math_constants
      use display
      use blas1
      use blas2
      use blas3
      use blas4

      implicit none


contains

  


      subroutine la_biorth_olenkifer(colwise, abar, a, u)
            ! ----------------------------------------------------------
            ! First U columns/rows of A (ABAR) contain the old vectors
            ! (from previous iterations), which are assumed to be
            ! biorthonormalized on entry to this subroutine:
            !
            ! <ABAR(:,p)|A(:,q)> = \delta_{p, q} for every p, q <= U
            !
            ! X = [............... ...................]
            !     |<----- U ----->|<------- V ------->|
            !      Old, already      New, to be
            !      biorthonormal     biorthonormalized
            !
            ! X = A, ABAR
            !
            ! The next V columns/rows of A (ABAR) correspond
            ! to the recently added vectors which are not yet
            ! biorthonormal either amongst themselves or with
            ! respect to the old U vectors. After the
            ! biorthonormalization the following condition
            !
            ! <ABAR(:,p)|A(:,q)> = \delta_{p, q}
            ! for every p, q <= U+V
            !
            ! is satisfied.
            ! ----------------------------------------------------------
            ! 1. Hirao, K. and Nakatsuji, H., A generalization of the
            !    Davidson's Method to Large Nonsymmetric Eigenvalue
            !    Problems, J. Comp. Phys. 45, 246 (1982).
            ! ----------------------------------------------------------
            ! COLWISE
            !           .TRUE. if vectors are stored in columns 
            !           .FALSE. if vectors are stored in rows
            !
            ! ABAR    
            !           Left vectors.
            ! A       
            !           Right vectors.
            ! U      
            !           Number of vectors assumed biorthonormal.
            !
            logical, intent(in)                       :: colwise
            real(F64), dimension(:, :), intent(inout) :: a
            real(F64), dimension(:, :), intent(inout) :: abar
            integer, intent(in)                       :: u

            integer :: n, v
            integer :: p, q
            real(F64) :: t, scala, scalb, sab


            if ((size(a, dim=1) .ne. size(abar, dim=1)) .or. &
                  (size(a, dim=2) .ne. size(abar, dim=2))) then
                  call msg("INCONSISTENT DIMENSIONS ON ENTRY TO LA_BIORTH", &
                        priority=MSG_ERROR)
                  stop
            end if

            if (colwise) then
                  n = size(a, dim=1)
                  v = size(a, dim=2) - u
            else
                  n = size(a, dim=2)
                  v = size(a, dim=1) - u
            end if

            if (u < 0 .or. v < 0) then
                  call msg("INVALID ARGUMENT PASSED TO LA_BIORTH", &
                        priority=MSG_ERROR)
                  stop
            end if

            if (v == 0) return
            !
            ! Loop over the V new vectors 
            !
            do p = u + 1, u + v
                  !
                  ! Schmidt biorthogonalization: Eq. 11 in [1].
                  ! Project out q-th vector:
                  ! ABAR(:, P) <- (1 - ABAR(:, Q) A(:, Q)T^) ABAR(:, P)
                  !
                  do q = 1, p - 1
                        if (colwise) then
                              call la_dot_olenkifer(t, a(:, q), abar(:, p))
                              call la_axpy_olenkifer(abar(:, p), -t, abar(:, q))
                        else
                              call la_dot_olenkifer(t, a(q, :), abar(p, :))
                              call la_axpy_olenkifer(abar(p, :), -t, abar(q, :))
                        end if
                  end do
                  !
                  ! Schmidt biorthogonalization: Eq. 11 in [1].
                  ! Project out q-th vector:
                  ! A(:, P) <- (1 - A(:, Q) ABAR(:, Q)^T) A(:, P)
                  !
                  do q = 1, p - 1
                        if (colwise) then
                              call la_dot_olenkifer(t, abar(:, q), a(:, p))
                              call la_axpy_olenkifer(a(:, p), -t, a(:, q))
                        else
                              call la_dot_olenkifer(t, abar(q, :), a(p, :))
                              call la_axpy_olenkifer(a(p, :), -t, a(q, :))
                        end if
                  end do
                  !
                  ! Bi-normalize pair of left and right vector
                  ! according to Eq. 13 in [1]
                  !
                  if (colwise) then
                        call la_dot_olenkifer(sab, abar(:, p), a(:, p))
                        scalb = ONE / sqrt(abs(sab))
                        !
                        ! Sign of one of the pair of vectors is changed
                        ! if the dot product if negative (see the comment
                        ! below Eq. 15 in [1]).
                        !
                        scala = sign(scalb, sab)
                        call la_scal_olenkifer(abar(:, p), scala)
                        call la_scal_olenkifer(a(:, p), scalb)
                  else
                        call la_dot_olenkifer(sab, abar(p, :), a(p, :))
                        scalb = ONE / sqrt(abs(sab))
                        scala = sign(scalb, sab)
                        call la_scal_olenkifer(abar(p, :), scala)
                        call la_scal_olenkifer(a(p, :), scalb)
                  end if
            end do
      end subroutine la_biorth_olenkifer
end module blas6
