module matexp
      use math_constants
      use arithmetic
      use display

      implicit none

contains

      subroutine maxdiff_cplx(t, a, b)
            real(F64), intent(out)                    :: t
            real(F64), dimension(:, :, :), intent(in) :: a
            real(F64), dimension(:, :, :), intent(in) :: b

            integer :: i, j
            integer :: m, n
            real(F64) :: t_re, t_im

            m = size(a, dim=1)
            n = size(a, dim=2)

            t = ZERO
            do j = 1, n
                  do i = 1, m
                        t_re = a(i, j, 1) - b(i, j, 1)
                        t_im = a(i, j, 2) - b(i, j, 2)
                        t = max(t, hypot(t_re, t_im))
                  end do
            end do
      end subroutine maxdiff_cplx


      subroutine matmul_cplx(c, conja, conjb, a, b, alpha, beta)
            !
            ! Matrix multiplication of complex nonsymmetric matrices
            ! Real part:      C(1) <- beta * C(1) + alpha * A(1) * B(1) - alpha * A(2) * B(2)
            ! Imaginary part: C(2) <- beta * C(2) + alpha * A(1) * B(2) + alpha * A(2) * B(1)
            !
            real(F64), dimension(:, :, :), contiguous, intent(inout) :: c
            logical, intent(in)                                      :: conja
            logical, intent(in)                                      :: conjb
            real(F64), dimension(:, :, :), contiguous, intent(in)    :: a
            real(F64), dimension(:, :, :), contiguous, intent(in)    :: b
            real(F64), intent(in)                                    :: alpha
            real(F64), intent(in)                                    :: beta

            character(1) :: ta, tb
            real(F64) :: pa, pb
            integer :: n
            external :: dgemm

            if (conja) then
                  ta = "T"
                  pa = -ONE
            else
                  ta = "N"
                  pa = ONE
            end if

            if (conjb) then
                  tb = "T"
                  pb = -ONE
            else
                  tb = "N"
                  pb = ONE
            end if

            n = size(c, dim=1)
            associate(a_re => a(:, :, 1), a_im => a(:, :, 2), b_re => b(:, :, 1), b_im => b(:, :, 2), &
                  c_re => c(:, :, 1), c_im => c(:, :, 2))
                  !
                  ! C_RE <- beta * C_RE + alpha * A_RE * B_RE
                  !
                  call dgemm(ta, tb, n, n, n, alpha, a_re, n, b_re, n, beta, c_re, n)
                  !
                  ! C_RE <- C_RE - alpha * A_IM * B_IM
                  !
                  call dgemm(ta, tb, n, n, n, -alpha * pa * pb, a_im, n, b_im, n, ONE, c_re, n)
                  !
                  ! C_IM <- beta * C_IM + alpha * A_RE * B_IM
                  !
                  call dgemm(ta, tb, n, n, n, alpha * pb, a_re, n, b_im, n, beta, c_im, n)
                  !
                  ! C_IM <- C_IM + alpha * A_IM * B_RE
                  !
                  call dgemm(ta, tb, n, n, n, alpha * pa, a_im, n, b_re, n, ONE, c_im, n)
            end associate
      end subroutine matmul_cplx


      subroutine matrix_1_norm_real(max_col, a)
            !
            ! Compute the matrix 1-norm of a square matrix A:
            ! ||A||_1 = max_j \sum_i |A_{ij}|.
            ! By the Gershgorin theorem, max_k |\lambda_k| <= ||A||_1,
            ! where \lambda_k is the k-th eigenvalue of A. Thus, ||A||_1
            ! bounds the induced norm of A:
            ! ||A|| := max_{||x||=1} ||A x|| <= ||A||_1.
            !
            real(F64), intent(out) :: max_col
            real(F64), dimension(:, :), contiguous, intent(in) :: a
            
            integer :: n, k, l
            real(F64) :: t
            
            n = size(a, dim=1)
            max_col = ZERO
            do k = 1, n
                  t = ZERO
                  do l = 1, n
                        t = t + abs(a(l, k))
                  end do
                  max_col = max(max_col, t)
            end do
      end subroutine matrix_1_norm_real


      subroutine matrix_1_norm_cplx(max_col, a)
            !
            ! Compute the matrix 1-norm of a square matrix A:
            ! ||A||_1 = max_j \sum_i |A_{ij}|.
            ! By the Gershgorin theorem, max_k |\lambda_k| <= ||A||_1,
            ! where \lambda_k is the k-th eigenvalue of A. Thus, ||A||_1
            ! bounds the induced norm of A:
            ! ||A|| := max_{||x||=1} ||A x|| <= ||A||_1.
            !
            real(F64), intent(out) :: max_col
            real(F64), dimension(:, :, :), contiguous, intent(in) :: a
            
            integer :: n, k, l
            real(F64) :: t
            
            n = size(a, dim=1)
            max_col = ZERO
            do k = 1, n
                  t = ZERO
                  do l = 1, n
                        t = t + hypot(a(l, k, 1), a(l, k, 2))
                  end do
                  max_col = max(max_col, t)
            end do
      end subroutine matrix_1_norm_cplx


      subroutine scale_and_square_real(e, x, k, j, work)
            real(F64), dimension(:, :), contiguous, intent(out) :: e
            real(F64), dimension(:, :), contiguous, intent(in)  :: x
            integer, intent(in)                                 :: k
            integer, intent(in)                                 :: j
            real(F64), dimension(:, :), contiguous, intent(out) :: work
            
            integer :: i
            real(F64) :: c, s
            integer :: n
            external :: dgemm

            n = size(e, dim=1)
            s = ONE / real(2**j, F64)
            !
            ! K-th order Taylor expansion of EXP(X/2^j)
            ! For example, for k = 3 the computations are done as follows, 
            ! starting from the most nested term:
            ! 1 + x * 1/1 * (1 + x * 1/2 * (1 + 1/3 * x ))
            !
            c = ONE / real(k, F64)
            work = c * s * x
            do i = k - 1, 1, -1
                  c = ONE / real(i, F64)
                  e = x
                  call dgemm("N", "N", n, n, n, c * s, work, n, x, n, c * s, e, n)
                  if (i > 1) then
                        work = e
                  end if
            end do
            
            do i = 1, n
                  e(i, i) = e(i, i) + ONE
            end do
            !
            ! Matrix squaring: EXP(X/2^j)^{2^j}
            !
            do i = 1, j
                  call dgemm("N", "N", n, n, n, ONE, e, n, e, n, ZERO, work, n)
                  e = work
            end do
      end subroutine scale_and_square_real


      subroutine scale_and_square_cplx(e, x, k, j, work)
            real(F64), dimension(:, :, :), contiguous, intent(out) :: e
            real(F64), dimension(:, :, :), contiguous, intent(in)  :: x
            integer, intent(in)                                    :: k
            integer, intent(in)                                    :: j
            real(F64), dimension(:, :, :), contiguous, intent(out) :: work
            
            integer :: i
            real(F64) :: c, s
            integer :: n

            n = size(e, dim=1)
            s = ONE / real(2**j, F64)
            !
            ! K-th order Taylor expansion of EXP(X/2^j)
            ! For example, for k = 3 the computations are done as follows, 
            ! starting from the most nested term:
            ! 1 + x * 1/1 * (1 + x * 1/2 * (1 + 1/3 * x ))
            !
            c = ONE / real(k, F64)
            work = c * s * x
            do i = k - 1, 1, -1
                  c = ONE / real(i, F64)
                  e = x
                  call matmul_cplx(e, .false., .false., work, x, c * s, c * s)
                  if (i > 1) then
                        work = e
                  end if
            end do
            
            do i = 1, n
                  e(i, i, 1) = e(i, i, 1) + ONE
            end do
            !
            ! Matrix squaring: EXP(X/2^j)^{2^j}
            !
            do i = 1, j
                  call matmul_cplx(work, .false., .false., e, e, ONE, ZERO)
                  e = work
            end do
      end subroutine scale_and_square_cplx


      subroutine matrix_exponential_real(e, x, work)
            ! --------------------------------------------------------------
            ! Compute matrix exponential by scaling and squaring technique:
            ! T_k(X) = \sum_{j=0}^{k} X^j / j!
            ! \exp(X) = (T_k(X/2^j))^{2^j}
            ! --------------------------------------------------------------
            ! 1. Cleve Moler, Charles Van Loan, Nineteen Dubious  Ways to
            !    Compute the Exponential of a Matrix, Twenty-Five Years
            !    Later, SIAM Review, 45; doi: 10.1137/S00361445024180.
            !    Optimum scaling and sqaring parameters with Taylor series
            !    approximation in Tab. 1, page 11.
            ! --------------------------------------------------------------
            ! E    - Output, matrix exponential
            ! X    - Input, the argument of the matrix exponential
            !
            real(F64), dimension(:, :), contiguous, intent(out) :: e
            real(F64), dimension(:, :), contiguous, intent(in)  :: x
            real(F64), dimension(:, :), contiguous, intent(out) :: work
            
            real(F64) :: x_norm
            !
            ! Employ a Gershgorin theorem approximation to the induced matrix 
            ! norm of page 5 in Ref. 1.
            !
            call matrix_1_norm_real(x_norm, x)
            !
            ! Optimal (k, j) pairs corresponding to 10^{-15} accuracy
            !
            if (x_norm < 1.0E-2_F64) then
                  call scale_and_square_real(e, x, 5, 1, work)
            else if (x_norm < 1.0E-1_F64) then
                  call scale_and_square_real(e, x, 5, 4, work)
            else if (x_norm < 1.0E+0_F64) then
                  call scale_and_square_real(e, x, 7, 5, work)
            else if (x_norm < 1.0E+1_F64) then
                  call scale_and_square_real(e, x, 9, 7, work)
            else if (x_norm < 1.0E+2_F64) then
                  call scale_and_square_real(e, x, 10, 10, work)
            else if (x_norm < 1.0E+3_F64) then
                  call scale_and_square_real(e, x, 8, 14, work)
            else
                  call msg("Cannot reliably compute matrix exponential", MSG_ERROR)
                  call dmsg("Matrix norm of X", x_norm, "ES10.3", MSG_ERROR)
                  error stop
            end if
      end subroutine matrix_exponential_real


      subroutine matrix_exponential_cplx(e, x, work)
            ! --------------------------------------------------------------
            ! Compute matrix exponential by scaling and squaring technique:
            ! T_k(X) = \sum_{j=0}^{k} X^j / j!
            ! \exp(X) = (T_k(X/2^j))^{2^j}
            ! --------------------------------------------------------------
            ! 1. Cleve Moler, Charles Van Loan, Nineteen Dubious  Ways to
            !    Compute the Exponential of a Matrix, Twenty-Five Years
            !    Later, SIAM Review, 45; doi: 10.1137/S00361445024180.
            !    Optimum scaling and sqaring parameters with Taylor series
            !    approximation in Tab. 1, page 11.
            ! --------------------------------------------------------------
            ! E    
            !        Output, matrix exponential
            ! X
            !        Input, the argument of the matrix exponential
            ! WORK
            !        Scratch space
            !
            real(F64), dimension(:, :, :), contiguous, intent(out) :: e
            real(F64), dimension(:, :, :), contiguous, intent(in)  :: x
            real(F64), dimension(:, :, :), contiguous, intent(out) :: work
            
            real(F64) :: x_norm
            !
            ! Employ a Gershgorin theorem approximation to the induced matrix 
            ! norm of page 5 in Ref. 1.
            !
            call matrix_1_norm_cplx(x_norm, x)
            !
            ! Optimal (k, j) pairs corresponding to 10^{-15} accuracy
            !
            if (x_norm < 1.0E-2_F64) then
                  call scale_and_square_cplx(e, x, 5, 1, work)
            else if (x_norm < 1.0E-1_F64) then
                  call scale_and_square_cplx(e, x, 5, 4, work)
            else if (x_norm < 1.0E+0_F64) then
                  call scale_and_square_cplx(e, x, 7, 5, work)
            else if (x_norm < 1.0E+1_F64) then
                  call scale_and_square_cplx(e, x, 9, 7, work)
            else if (x_norm < 1.0E+2_F64) then
                  call scale_and_square_cplx(e, x, 10, 10, work)
            else if (x_norm < 1.0E+3_F64) then
                  call scale_and_square_cplx(e, x, 8, 14, work)
            else
                  call msg("Cannot reliably compute matrix exponential", MSG_ERROR)
                  call dmsg("Matrix norm of X", x_norm, "ES10.3", MSG_ERROR)
                  error stop
            end if
      end subroutine matrix_exponential_cplx
end module matexp
