! -----------------------------------------------------------------------
!                     CONJUGATE GRADIENT SOLVER
! -----------------------------------------------------------------------
! Solve Ax = b for x, where A is a real symmetric positive-definite
! matrix. The unknown vector x is computed iteratively, by the conjugate 
! gradient method. The algorithm implemented in this module does not
! require storing the matrix A in memory.
! 
! Pseudocode for solving Ax=b, where A is n x n real symmetric
! positive definite matrix. Here, A is rebuilt in each iteration
! of the CG subroutine. CG_UPDATE can be called multiple times
! for the same matrix element, A(P, Q), if multiple contributions
! to A(P, Q) are computed non-simultaneously.
! -----------------------------------------------------------------------
! input: vector b, subroutine for computing A(p,q)
! allocate xvec, dvec, rvec, advec
! call cg_firstiter
! start CG iteration         <----------|
!      advec = ZERO                     |
!      loop over lower triangle of A    |
!           call cg_update              |
!      call cg_enditer                  |
!      is delta < eps^2?                |
!        no  ----------------------------
!        yes ---> xvec is the solution
! -----------------------------------------------------------------------
! 1. Shewchuk, J., An introduction to the conjugate gradient method
!    without the agonizing pain, Manuscript, August 1994 (see doc/cg/)
!
module cg
      implicit none

contains

      subroutine cg_update(apq, p, q, dvec, advec)
            ! ------------------------------------------------------------
            ! Update AD vector with the value of A(P,Q) or
            ! a to this matrix element. This subroutine should
            ! be called for every non-zero matrix element belonging
            ! to the lower triangle of the symmetric matrix A. If
            ! the matrix element is on the diagonal, it should be
            ! premultiplied by 1/2.
            ! ------------------------------------------------------------
            ! APQ   - Matrix element of the matrix A:
            !         APQ = A(P, Q) if P .NE. Q
            !         APQ = 1/2 A(P, Q) if P .EQ. Q
            ! P, Q  - Row/coulumn indices
            ! DVEC  - Direction vector (initiated by CG_FIRSTITER,
            !         updated by CG_ENDITER)
            ! ADVEC - A*D vector. ADVEC should be reset at the
            !         beginning of every CG iteration, and incrementally
            !         built by calling this subroutine.
            !
            double precision, intent(in)                  :: apq
            integer, intent(in)                           :: p
            integer, intent(in)                           :: q
            double precision, dimension(:), intent(in)    :: dvec
            double precision, dimension(:), intent(inout) :: advec
            !
            ! AXVEC <- Ax
            ! Assume that APQ = 1/2 A(P, Q) if P = Q
            !
            advec(p) = advec(p) + apq * dvec(q)
            advec(q) = advec(q) + apq * dvec(p)
      end subroutine cg_update


      subroutine cg_firstiter(n, xvec, bvec, dvec, rvec, delta)
            integer, intent(in) :: n
            double precision, dimension(:), intent(out) :: xvec
            double precision, dimension(:), intent(in)  :: bvec
            double precision, dimension(:), intent(out) :: dvec
            double precision, dimension(:), intent(out) :: rvec
            double precision, intent(out)               :: delta

            double precision :: d12
            external :: dnrm2
            double precision :: dnrm2

            xvec = 0.d+0
            dvec = bvec
            rvec = bvec
            d12 = dnrm2(n, rvec, 1)
            delta = d12**2
      end subroutine cg_firstiter


      subroutine cg_enditer(n, xvec, dvec, advec, rvec, delta)
            ! --------------------------------------------------------
            ! Compute new approximation to the X vector of Ax=b 
            ! equation.
            ! --------------------------------------------------------
            ! N     - Order of the matrix A
            ! XVEC  - Input/output. Approximated solution to the linear
            !         system.
            ! DVEC  - Input/ouput. Direction of CG linesearch.
            ! ADVEC - A*d vector computed incrementaly by CG_UPDATE
            ! RVEC  - Residual vector, b-Ax
            ! DELTA - Input/ouput. Norm of the residual vector
            !
            integer, intent(in)                           :: n
            double precision, dimension(:), intent(inout) :: xvec
            double precision, dimension(:), intent(inout) :: dvec
            double precision, dimension(:), intent(in)    :: advec
            double precision, dimension(:), intent(inout) :: rvec
            double precision, intent(inout)               :: delta

            integer :: k
            double precision :: alpha, beta
            double precision :: d12
            double precision :: deltaold
            double precision :: dtad
            external :: dnrm2
            external :: daxpy
            external :: ddot
            double precision :: dnrm2
            double precision :: ddot

            dtad = ddot(n, dvec, 1, advec, 1)
            alpha = delta / dtad
            !
            ! X <- X + ALPHA * D
            !
            call daxpy(n, alpha, dvec, 1, xvec, 1)
            !
            ! R <- R - ALPHA * AD
            !
            call daxpy(n, -alpha, advec, 1, rvec, 1)
            deltaold = delta
            d12 = dnrm2(n, rvec, 1)
            delta = d12**2
            beta = delta / deltaold
            !
            ! D <- R + BETA * D
            !
            do k = 1, n
                  dvec(k) = beta * dvec(k) + rvec(k)
            end do
      end subroutine cg_enditer


      subroutine cg_test()
            !
            ! Example of solving Ax=b, where A is real symmetric positive-definite
            ! matrix, and b is a given right-hand side of the linear system.
            !
            integer, parameter :: n = 5
            double precision, dimension(n) :: xvec, dvec, advec, rvec
            double precision, dimension(n, n) :: a
            double precision, dimension(n) :: bvec
            double precision :: delta
            double precision :: apq
            integer :: p, q
            integer :: iter
            double precision, parameter :: eps = 1.d-8
            !
            ! 1.00  0.01   0.02 0.80 0.90
            ! 0.01  2.00   0.01 0.00 0.90
            ! 0.02  0.01   3.00 0.00 0.00
            ! 0.80  0.00   0.00 1.00 0.00
            ! 0.90  0.90   0.00 0.00 5.00
            !
            a(:, 1) = (/1.00d+0, 0.01d+0, 0.02d+0, 0.80d+0, 0.90d+0/)
            a(:, 2) = (/0.01d+0, 2.00d+0, 0.01d+0, 0.00d+0, 0.90d+0/)
            a(:, 3) = (/0.02d+0, 0.01d+0, 3.00d+0, 0.00d+0, 0.00d+0/)
            a(:, 4) = (/0.80d+0, 0.00d+0, 0.00d+0, 1.00d+0, 0.00d+0/)
            a(:, 5) = (/0.90d+0, 0.90d+0, 0.00d+0, 0.00d+0, 5.00d+0/)
            !
            ! 1.00
            ! 0.50
            ! 0.30
            ! 10.00
            ! 50.00
            !
            bvec = (/1.00d+0, 0.50d+0, 0.30d+0, 10.d+0, 50.d+0/)

            call cg_firstiter(n, xvec, bvec, dvec, rvec, delta)
            do iter = 1, 250
                  advec = 0.d+0
                  do q = 1, n
                        do p = q, n
                              if (p .ne. q) then
                                    apq = a(p, q)
                              else
                                    apq = 0.5d+0 * a(p, q)
                              end if
                              call cg_update(apq, p, q, dvec, advec)
                        end do
                  end do
                  call cg_enditer(n, xvec, dvec, advec, rvec, delta)
                  if (delta < eps) exit
            end do
      end subroutine cg_test
end module cg
