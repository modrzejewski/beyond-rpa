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
module blas_olenkifer
      use arithmetic
      use math_constants
      use display
      use blas1
      use blas2
      use blas3
      use blas4
      use blas5
      use blas6

      implicit none

      interface la_vta_rect_olenkifer
            module procedure :: la_vta_rect_scal_olenkifer
            module procedure :: la_vta_rect_noscal_olenkifer
      end interface la_vta_rect_olenkifer

contains

      subroutine la_bta_rect_olenkifer(a, b)
            ! ---------------------------------------------------------
            ! A(1:M, 1:N) <- B(1:K, 1:M)^T A(1:K, 1:N),
            ! M, K << N.
            ! Use this subroutine only if M and K are small numbers.
            ! Because the matrix A stores both input and output, in
            ! some scenarios this subroutine is more memory-efficient
            ! than GEMM. A small array is allocated to temporarily
            ! store a single column of A.
            ! ---------------------------------------------------------
            ! A
            !    On entry, a rectangular matrix of dimension K x N.
            !    On exit, a rectangular matrix of dimension M x N.
            ! B
            !    On entry, a matrix of dimension K x N.
            !
            real(F64), dimension(:, :), intent(inout) :: a
            real(F64), dimension(:, :), intent(in)    :: b

            integer :: m, n, k
            integer :: p, q, w
            real(F64), dimension(:), allocatable :: aw
            real(F64) :: apw
            
            n = size(a, dim=2)
            k = size(a, dim=1)
            m = size(b, dim=2)

            if (size(b, dim=1) .ne. k) then
                  call msg("ERROR: INVALID ARGUMENT PASSED TO LA_BTA_RECT", &
                        priority=MSG_ERROR)
                  stop
            end if

            !$omp parallel private(w, aw, p, q, apw) &
            !$omp shared(a, b, m, n, k) &
            !$omp default(none)
            allocate(aw(k))
            !$omp do schedule(static)
            do w = 1, n
                  aw(:) = a(1:k, w)
                  do p = 1, m
                        apw = ZERO
                        do q = 1, k
                              apw = apw + b(q, p) * aw(q)
                        end do
                        a(p, w) = apw
                  end do
            end do
            !$omp end do nowait
            deallocate(aw)
            !$omp end parallel
      end subroutine la_bta_rect_olenkifer


      subroutine la_vta_rect_scal_olenkifer(w, a, v, alpha)
            ! -------------------------------------------------------
            ! W(1:N) <- ALPHA * V(1:K)^T A(1:K, 1:N),
            ! K << N.
            ! A is a rectangular matrix.
            ! Use this subroutine only if A is a K x N matrix
            ! where K IS A SMALL NUMBER!
            ! -------------------------------------------------------
            ! A
            !    A rectangular matrix of dimension K x N.
            ! V
            !    A vector of dimension K.
            ! W 
            !    On exit, W = V^T A. A vector of dimension N.
            ! ALPHA
            !    Input, a scalar scaling factor.
            !
            real(F64), dimension(:), intent(out)   :: w
            real(F64), dimension(:, :), intent(in) :: a
            real(F64), dimension(:), intent(in)    :: v
            real(F64), intent(in)                  :: alpha
            
            integer :: n, k
            integer :: q, r
            real(F64) :: wr
            
            n = size(a, dim=2)
            k = size(a, dim=1)

            if (size(w, dim=1) .ne. n .or. size(v, dim=1) .ne. k) then
                  call msg("ERROR: INVALID ARGUMENT PASSED TO LA_VTA_RECT", &
                        priority=MSG_ERROR)
                  stop
            end if

            !$omp parallel private(r, wr, q) &
            !$omp shared(a, v, w, n, k, alpha) &
            !$omp default(none)
            !$omp do schedule(static)
            do r = 1, n
                  wr = ZERO
                  do q = 1, k
                        wr = wr + v(q) * a(q, r)
                  end do
                  w(r) = alpha * wr
            end do
            !$omp end do nowait
            !$omp end parallel
      end subroutine la_vta_rect_scal_olenkifer


      subroutine la_vta_rect_noscal_olenkifer(w, a, v)
            ! -------------------------------------------------------
            ! W(1:N) <- V(1:K)^T A(1:K, 1:N),
            ! K << N.
            ! A is a rectangular matrix.
            ! Use this subroutine only if A is a K x N matrix
            ! where K IS A SMALL NUMBER!
            ! -------------------------------------------------------
            ! A
            !    A rectangular matrix of dimension K x N.
            ! V
            !    A vector of dimension K.
            ! W 
            !    On exit, W = V^T A. A vector of dimension N.
            ! ALPHA
            !    Input, a scalar scaling factor.
            !
            real(F64), dimension(:), intent(out)   :: w
            real(F64), dimension(:, :), intent(in) :: a
            real(F64), dimension(:), intent(in)    :: v
            
            integer :: n, k
            integer :: q, r
            real(F64) :: wr
            
            n = size(a, dim=2)
            k = size(a, dim=1)

            if (size(w, dim=1) .ne. n .or. size(v, dim=1) .ne. k) then
                  call msg("ERROR: INVALID ARGUMENT PASSED TO LA_VTA_RECT", &
                        priority=MSG_ERROR)
                  stop
            end if

            !$omp parallel private(r, wr, q) &
            !$omp shared(a, v, w, n, k) &
            !$omp default(none)
            !$omp do schedule(static)
            do r = 1, n
                  wr = ZERO
                  do q = 1, k
                        wr = wr + v(q) * a(q, r)
                  end do
                  w(r) = wr
            end do
            !$omp end do nowait
            !$omp end parallel
      end subroutine la_vta_rect_noscal_olenkifer


      subroutine la_vtapw_rect_olenkifer(w, a, v, alpha)
            ! -------------------------------------------------------
            ! W(1:N) <- W(1:N) + ALPHA * V(1:K)^T A(1:K, 1:N)
            ! K << N.
            ! A is a rectangular matrix.
            ! Use this subroutine only if A is a K x N matrix
            ! where K IS A SMALL NUMBER!
            ! -------------------------------------------------------
            ! A
            !    A rectangular matrix of dimension K x N.
            ! V
            !    A vector of dimension K.
            ! W 
            !    On exit, W = V^T A. A vector of dimension N.
            ! ALPHA
            !    Input, a scalar scaling factor.
            !
            real(F64), dimension(:), intent(inout) :: w
            real(F64), dimension(:, :), intent(in) :: a
            real(F64), dimension(:), intent(in)    :: v
            real(F64), intent(in)                  :: alpha
            
            integer :: n, k
            integer :: q, r
            real(F64) :: wr
            
            n = size(a, dim=2)
            k = size(a, dim=1)

            if (size(w, dim=1) .ne. n .or. size(v, dim=1) .ne. k) then
                  call msg("ERROR: INVALID ARGUMENT PASSED TO LA_VTA_RECT", &
                        priority=MSG_ERROR)
                  stop
            end if

            !$omp parallel private(r, wr, q) &
            !$omp shared(a, v, w, n, k, alpha) &
            !$omp default(none)
            !$omp do schedule(static)
            do r = 1, n
                  wr = ZERO
                  do q = 1, k
                        wr = wr + v(q) * a(q, r)
                  end do
                  w(r) = w(r) + alpha * wr
            end do
            !$omp end do nowait
            !$omp end parallel
      end subroutine la_vtapw_rect_olenkifer

      subroutine la_conditional_gramschmidt_olenkifer(colwise, a, thresh, max_overlap)
            logical, intent(in)                       :: colwise
            real(F64), dimension(:, :), intent(inout) :: a
            real(F64), intent(in)                     :: thresh
            real(F64), intent(out)                    :: max_overlap
            
            real(F64) :: s_ij
            integer :: m, i, j
            
            max_overlap = ZERO
            if (colwise) then
                  m = size(a, dim=2)
                  outer1: do j = 1, m
                        do i = j + 1, m
                              call la_dot_olenkifer(s_ij, a(:, i), a(:, j))
                              max_overlap = max(max_overlap, abs(s_ij))
                        end do
                  end do outer1
            else
                  m = size(a, dim=1)
                  outer2: do j = 1, m
                        do i = j + 1, m
                              call la_dot_olenkifer(s_ij, a(i, :), a(:, j))
                              max_overlap = max(max_overlap, abs(s_ij))
                        end do
                  end do outer2
            end if

            if (max_overlap > thresh) then
                  call la_gramschmidt_olenkifer(colwise, a, 0)
            end if
      end subroutine la_conditional_gramschmidt_olenkifer


end module blas_olenkifer
