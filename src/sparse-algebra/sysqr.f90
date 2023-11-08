module sysqr
      use math_constants
      use sparse
      use linalg

      implicit none

contains

      subroutine tk(xk, lambda)
            !
            ! ************************************************
            ! DESCRIPTION:
            ! ************************************************
            !
            ! T_k^{(2)} = \frac 12 ( 3 * I - \lambda * X_k)
            !
            type(spmatrix) :: xk
            double precision, intent(in) :: lambda

            call spgeape(xk, -lambda / two, frac32)
      end subroutine tk


      subroutine spsysqr(s, z, y)
            !
            ! ************************************************
            ! DESCRIPTION:
            ! ************************************************
            !
            ! Calculate inverse square root and square root
            ! of sparse symmetric positive-definite matrix S.
            ! Convergence test passed if change in L1 norms of
            ! both sqare root and inverse sqare root matrices
            ! is smaller than THRESHOLD in SPARSE module
            ! 
            ! ************************************************
            ! REFERENCES:
            ! ************************************************
            !
            ! 1. Jansik, B., J. Chem. Phys. 126,
            !    124104 (2007)
            !
            ! ************************************************
            ! INPUTS:
            ! ************************************************
            !
            ! S - Input, symmetric sparse positive-definite
            !     matrix
            !
            ! Z - Output, symmetric sparse positive-definite
            !     matrix where S^{-\frac 12} is stored 
            !
            ! Y - Output, symmetric sparse positive-definite
            !     matrix where S^{\frac 12} is stored
            !
            !
            !
            type(spmatrix) :: s, z, y

            type(spmatrix) :: xk, znew, ynew
            double precision :: lambda, lambdasq
            double precision :: eps, zdelta, ydelta
            double precision :: znorm0, znorm1, ynorm0, ynorm1
            integer :: i
            logical :: converged

            double precision :: conv_factor = 0.9d+0
            integer, parameter :: conv = 30

            !
            ! Set accuracy equal to THRESHOLD in SPARSE module
            !
            call spgetthr(eps)

            call spmatrix_init(xk, s%size)
            call spmatrix_init(znew, s%size)
            call spmatrix_init(ynew, s%size)

            lambda = two / spsygct(s)
            !
            ! First iteration
            !
100         call spcpy(s, xk)
            call tk(xk, lambda)
            call spcpy(xk, znew)
            call spsymm(xk, s, ynew, 0)

            znorm0 = spsyl1norm(znew)
            ynorm0 = spsyl1norm(ynew)
            converged = .false.

            do i = 1, conv
                  call spcpy(znew, z)
                  call spcpy(ynew, y)
                  
                  call spsymm(y, z, xk, 0)
                  call tk(xk, lambda)
                  call spsymm(z, xk, znew, 0)
                  call spsymm(xk, y, ynew, 0)

                  znorm1 = spsyl1norm(znew)
                  ynorm1 = spsyl1norm(ynew)

                  zdelta = abs(znorm1 - znorm0)
                  ydelta = abs(ynorm1 - ynorm0)

                  if (max(zdelta, ydelta) .lt. eps) then
                        converged = .true.
                        exit
                  end if

                  znorm0 = znorm1
                  ynorm0 = ynorm1
            end do

            if (.not. converged) then
                  !
                  ! Poor convergence may be due to
                  ! using approximate 2-norm
                  !
                  lambda = lambda * conv_factor
                  goto 100
            end if

            call spcpy(znew, z)
            call spcpy(ynew, y)

            lambdasq = sqrt(lambda)
            call spscale(z, lambdasq)
            call spscale(y, one / lambdasq)

            call spmatrix_free(xk)
      end subroutine spsysqr
end module sysqr
