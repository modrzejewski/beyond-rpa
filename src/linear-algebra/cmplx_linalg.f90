! -------------------------------------------------------------
!            LINEAR ALGEBRA MODULE (COMPLEX NUMBERS)
! -------------------------------------------------------------
module cmplx_linalg
      use math_constants
      use arithmetic
      use display
      use string

      implicit none

      interface cmplx_dot
            module procedure :: cmplx_dot_1
            module procedure :: cmplx_dot_2
      end interface cmplx_dot

contains

      subroutine cmplx_aHbc(d, a, b, c)
            !
            ! D <- A**H B C
            !
            complex(F64), dimension(:, :), intent(out) :: d
            complex(F64), dimension(:, :), intent(in)  :: a
            complex(F64), dimension(:, :), intent(in)  :: b
            complex(F64), dimension(:, :), intent(in)  :: c
            !
            ! Compute D <- A**H B C
            !
            integer :: u, v
            complex(F64), dimension(:, :), allocatable :: w

            u = size(a, dim=2)
            v = size(b, dim=2)
            allocate(w(u, v))
            !
            ! W <- A**H B
            !
            call cmplx_aHb(w, a, b)
            !
            ! D <- W C
            !
            call cmplx_ab(d, w, c)
      end subroutine cmplx_aHbc


      subroutine cmplx_ab(c, a, b)
            !
            ! Compute C(1:u, 1:v) <- A(1:k, 1:l) B(1:m, 1:n)
            !
            complex(F64), dimension(:, :), intent(out) :: c
            complex(F64), dimension(:, :), intent(in)  :: a
            complex(F64), dimension(:, :), intent(in)  :: b
            
            integer :: m, n, k, l, u, v
            complex(F64), parameter :: alpha = (ONE, ZERO)
            complex(F64), parameter :: beta = (ZERO, ZERO)
            external :: zgemm

            c = ZERO
            k = size(a, dim=1)
            l = size(a, dim=2)
            m = size(b, dim=1)
            n = size(b, dim=2)
            u = size(c, dim=1)
            v = size(c, dim=2)
            if (l .ne. m) then
                  call msg("cmplx_ab: inconsistent dimensions of matrices A and B", MSG_ERROR)
                  stop
            end if
            if ((u .ne. k) .or. (v .ne. n)) then
                  call msg("cmplx_ab: inconsistent dimensions of matrix C", MSG_ERROR)
                  stop
            end if
            call zgemm("N", "N", u, v, l, alpha, a, k, b, m, beta, c, u)
      end subroutine cmplx_ab

      
      subroutine cmplx_aHb(c, a, b)
            !
            ! Compute C(1:u, 1:v) <- A(1:k, 1:l)**H B(1:m, 1:n)
            !
            complex(F64), dimension(:, :), intent(out) :: c
            complex(F64), dimension(:, :), intent(in)  :: a
            complex(F64), dimension(:, :), intent(in)  :: b
            
            integer :: m, n, k, l, u, v
            complex(F64), parameter :: alpha = (ONE, ZERO)
            complex(F64), parameter :: beta = (ZERO, ZERO)
            external :: zgemm

            c = ZERO
            k = size(a, dim=1)
            l = size(a, dim=2)
            m = size(b, dim=1)
            n = size(b, dim=2)
            u = size(c, dim=1)
            v = size(c, dim=2)
            if (k .ne. m) then
                  call msg("cmplx_aHb: inconsistent dimensions of matrices A and B", MSG_ERROR)
                  stop
            end if
            if ((u .ne. l) .or. (v .ne. n)) then
                  call msg("cmplx_aHb: inconsistent dimensions of matrix C", MSG_ERROR)
                  stop
            end if
            call zgemm("C", "N", u, v, k, alpha, a, k, b, m, beta, c, u)
      end subroutine cmplx_aHb


      subroutine cmplx_uvH(w, alpha, u, v)
            !
            ! Compute w <- w + alpha * u v**H, where u and v are vectors.
            !
            complex(F64), dimension(:, :), contiguous, intent(inout) :: w
            complex(F64), intent(in)                                 :: alpha
            complex(F64), dimension(:), contiguous, intent(in)       :: u
            complex(F64), dimension(:), contiguous, intent(in)       :: v

            integer :: m
            external :: zgerc

            m = size(u)
            call zgerc(m, m, alpha, u, 1, v, 1, w, m)
      end subroutine cmplx_uvH

      
      subroutine cmplx_abH(c, a, b)
            !
            ! Compute C(1:u, 1:v) <- A(1:k, 1:l) B(1:m, 1:n)**H
            !
            complex(F64), dimension(:, :), intent(out) :: c
            complex(F64), dimension(:, :), intent(in)  :: a
            complex(F64), dimension(:, :), intent(in)  :: b
            
            integer :: m, n, k, l, u, v
            complex(F64), parameter :: alpha = (ONE, ZERO)
            complex(F64), parameter :: beta = (ZERO, ZERO)
            external :: zgemm

            c = ZERO
            k = size(a, dim=1)
            l = size(a, dim=2)
            m = size(b, dim=1)
            n = size(b, dim=2)
            u = size(c, dim=1)
            v = size(c, dim=2)
            if (n .ne. l) then
                  call msg("cmplx_abH: inconsistent dimensions of matrices A and B", MSG_ERROR)
                  stop
            end if
            if ((u .ne. k) .or. (v .ne. m)) then
                  call msg("cmplx_abH: inconsistent dimensions of matrix C", MSG_ERROR)
                  stop
            end if
            call zgemm("N", "C", u, v, l, alpha, a, k, b, m, beta, c, u)
      end subroutine cmplx_abH

      
      subroutine hermitian_eigenproblem(w, a, n, compute_eigenvecs)
            !
            ! Compute eigenvalues and eigenvectors of a complex Hermitian matrix.
            ! The procedure assumes that data are stored in the lower triangle of A.
            ! The output eigenvectors are orthonormal, that is, C**H C = 1.
            ! The eigenvalues are sorted in ascending order.
            !
            real(F64), contiguous, dimension(:), intent(out)         :: w
            complex(F64), contiguous, dimension(:, :), intent(inout) :: a
            integer, intent(in)                                      :: n
            logical, intent(in)                                      :: compute_eigenvecs

            integer :: lda, lwork, lrwork, liwork, info
            character(1) :: jobz
            complex(F64), dimension(1) :: work0
            real(F64), dimension(1) :: rwork0
            integer(F64), dimension(1) :: iwork0
            complex(F64), dimension(:), allocatable :: work
            real(F64), dimension(:), allocatable :: rwork
            integer, dimension(:), allocatable :: iwork
            external :: zheevd

            lda = size(a, dim=1)
            if (compute_eigenvecs) then
                  jobz = "V"
            else
                  jobz = "N"
            end if
            !
            ! Compute the optimal size of temporary storage
            !
            call zheevd(jobz, "L", n, a, lda, w, work0, -1, rwork0, -1, iwork0, -1, info)
            lwork = ceiling(real(work0(1)))
            lrwork = ceiling(rwork0(1))
            liwork = iwork0(1)
            allocate(work(lwork))
            allocate(rwork(lrwork))
            allocate(iwork(liwork))
            call zheevd(jobz, "L", n, a, lda, w, work, lwork, rwork, lrwork, iwork, liwork, info)
            if (info .ne. 0) then
                  call msg("Eigensolver for hermitian matrices returned error code (" // str(info) // ")", MSG_ERROR)
                  stop
            end if
      end subroutine hermitian_eigenproblem


      subroutine cmplx_conditional_gramschmidt(colwise, a, thresh, max_overlap)
            !
            ! Perform Gram-Schmidt orthogonalization of complex vectors.
            ! Before orthogonalization, test if, for any i /= j, Sqrt(Ci**H Cj) > Thresh.
            !
            logical, intent(in)                       :: colwise
            complex(F64), dimension(:, :), intent(inout) :: a
            real(F64), intent(in)                     :: thresh
            real(F64), intent(out)                    :: max_overlap
            
            complex(F64) :: s_ij
            real(F64) :: sij2
            integer :: m, i, j
            
            max_overlap = ZERO
            if (colwise) then
                  m = size(a, dim=2)
                  outer1: do j = 1, m
                        do i = j + 1, m
                              call cmplx_dot(s_ij, a(:, i), a(:, j))
                              sij2 = conjg(s_ij) * s_ij
                              max_overlap = max(max_overlap, sij2)
                        end do
                  end do outer1
            else
                  m = size(a, dim=1)
                  outer2: do j = 1, m
                        do i = j + 1, m
                              call cmplx_dot(s_ij, a(i, :), a(j, :))
                              sij2 = conjg(s_ij) * s_ij
                              max_overlap = max(max_overlap, sij2)
                        end do
                  end do outer2
            end if
            max_overlap = sqrt(max_overlap)
            if (max_overlap > thresh) then
                  call cmplx_gramschmidt(colwise, a, 0)
            end if
      end subroutine cmplx_conditional_gramschmidt


      subroutine cmplx_gramschmidt(colwise, a, k)
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
            !     The first K vectors that are assumed to be already orthonormal
            !     on entry to this subroutine. Set K=0 if A contains no orthonormal
            !     vectors.
            !
            logical, intent(in)                          :: colwise
            complex(F64), dimension(:, :), intent(inout) :: a
            integer, intent(in)                          :: k

            integer :: n, l
            integer :: i, j
            real(F64) :: l2norm
            complex(F64) :: t

            if (colwise) then
                  n = size(a, dim=1)
                  l = size(a, dim=2) - k
            else
                  n = size(a, dim=2)
                  l = size(a, dim=1) - k
            end if

            if (l < 0 .or. k < 0) then
                  call msg("Invalid argument passed to CMPLX_GRAMSCHMIDT", &
                        priority=MSG_ERROR)
                  stop
            end if

            if (l == 0) return
            !
            ! Normalize vectors
            !
            do i = k + 1, k + l
                  if (colwise) then
                        call cmplx_l2norm(l2norm, a(:, i))
                        call cmplx_real_scal(a(:, i), ONE/l2norm)
                  else
                        call cmplx_l2norm(l2norm, a(i, :))
                        call cmplx_real_scal(a(i, :), ONE/l2norm)
                  end if
            end do
            !
            ! Project out the non-orthogonal components from columns
            ! K+1...K+N (these are the "new" vectors)
            !
            do i = k + 1, k + l
                  do j = 1, i - 1
                        !
                        ! A(i) <- A(i) - [A(j)**H A(i)] * A(j)
                        !
                        if (colwise) then
                              call cmplx_dot(t, a(:, j), a(:, i))
                              call cmplx_axpy(a(:, i), -t, a(:, j))
                        else
                              call cmplx_dot(t, a(j, :), a(i, :))
                              call cmplx_axpy(a(i, :), -t, a(j, :))
                        end if
                  end do

                  if (colwise) then
                        call cmplx_l2norm(l2norm, a(:, i))
                        call cmplx_real_scal(a(:, i), ONE/l2norm)
                  else
                        call cmplx_l2norm(l2norm, a(i, :))
                        call cmplx_real_scal(a(i, :), ONE/l2norm)
                  end if
            end do
      end subroutine cmplx_gramschmidt

      
      subroutine cmplx_axpy(y, alpha, x)
            !
            ! Y <- ALPHA * X + Y
            !
            complex(F64), dimension(:), intent(inout) :: y
            complex(F64), intent(in)                  :: alpha
            complex(F64), dimension(:), intent(in)    :: x

            integer :: k, n

            if (size(x) .ne. size(y)) then
                  call msg("Inconsistent dimensions on entry to cmplx_axpy", &
                        priority=MSG_ERROR)
                  call imsg("size(X)", size(x), MSG_ERROR)
                  call imsg("size(Y)", size(y), MSG_ERROR)
                  stop
            end if

            n = size(y)

            !$omp parallel shared(n, x, y, alpha) private(k) default(none)
            !$omp do schedule(static)
            do k = 1, n
                  y(k) = y(k) + alpha * x(k)
            end do
            !$omp end do nowait
            !$omp end parallel
      end subroutine cmplx_axpy


      subroutine cmplx_l2norm(l2norm, a)
            !
            ! L2NORM <- SQRT(A**H A)
            !
            real(F64), intent(out)                 :: l2norm
            complex(F64), dimension(:), intent(in) :: a

            real(F64) :: l2norm_partial
            integer :: k, l, n, r
            integer, parameter :: blocksize = 2**10

            n = size(a, dim=1)
            r = modulo(n, blocksize)
            l2norm = ZERO
            if (r .ne. 0) then
                  l2norm_partial = ZERO
                  do l = 1, r
                        l2norm_partial = l2norm_partial + conjg(a(l)) * a(l)
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
                              l2norm_partial = l2norm_partial + conjg(a(k-1+l)) * a(k-1+l)
                        end do
                        l2norm = l2norm + l2norm_partial
                  end do
                  !$omp end do nowait
                  !$omp end parallel
            end if

            l2norm = sqrt(l2norm)
      end subroutine cmplx_l2norm


      subroutine cmplx_dot_2(dot, a, b)
            !
            ! DOT <- A**H B
            !
            complex(F64), intent(out)                             :: dot
            complex(F64), dimension(:, :), contiguous, intent(in) :: a
            complex(F64), dimension(:, :), contiguous, intent(in) :: b

            integer :: n

            if (size(a) .ne. size(b)) then
                  call msg("Inconsistent dimensions on entry to cmplx_dot", &
                        priority=MSG_ERROR)
                  call imsg("size(A)", size(a), MSG_ERROR)
                  call imsg("size(B)", size(b), MSG_ERROR)
                  stop
            end if
            n = size(a)
            call cmplx_dot_n(dot, a, b, n)
      end subroutine cmplx_dot_2

      
      subroutine cmplx_dot_1(dot, a, b)
            !
            ! DOT <- A**H B
            !
            complex(F64), intent(out)                          :: dot
            complex(F64), dimension(:), contiguous, intent(in) :: a
            complex(F64), dimension(:), contiguous, intent(in) :: b

            integer :: n
            
            if (size(a) .ne. size(b)) then
                  call msg("Inconsistent dimensions on entry to cmplx_dot", &
                        priority=MSG_ERROR)
                  call imsg("size(A)", size(a), MSG_ERROR)
                  call imsg("size(B)", size(b), MSG_ERROR)
                  stop
            end if
            n = size(a)
            call cmplx_dot_n(dot, a, b, n)
      end subroutine cmplx_dot_1


      subroutine cmplx_dot_n(dot, a, b, n)
            !
            ! DOT <- A**H B
            !
            complex(F64), intent(out)              :: dot
            complex(F64), dimension(*), intent(in) :: a
            complex(F64), dimension(*), intent(in) :: b
            integer, intent(in)                    :: n

            complex(F64) :: dot_partial
            integer :: k, l, r
            integer, parameter :: blocksize = 2**10

            r = modulo(n, blocksize)
            dot = ZERO
            if (r .ne. 0) then
                  dot_partial = ZERO
                  do l = 1, r
                        dot_partial = dot_partial + conjg(a(l)) * b(l)
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
                              dot_partial = dot_partial + conjg(a(k-1+l)) * b(k-1+l)
                        end do
                        dot = dot + dot_partial
                  end do
                  !$omp end do nowait
                  !$omp end parallel
            end if
      end subroutine cmplx_dot_n


      subroutine cmplx_real_scal(a, alpha)
            !
            ! A <- ALPHA * A, where Alpha is real and A is complex.
            !
            complex(F64), dimension(:), intent(inout) :: a
            real(F64), intent(in)                     :: alpha

            integer :: k, n

            n = size(a, dim=1)

            !$omp parallel shared(n, a, alpha) private(k) default(none)
            !$omp do schedule(static)
            do k = 1, n
                  a(k) = alpha * a(k)
            end do
            !$omp end do nowait
            !$omp end parallel
      end subroutine cmplx_real_scal
end module cmplx_linalg
