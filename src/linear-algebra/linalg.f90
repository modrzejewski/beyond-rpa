module linalg
      use arithmetic
      use gparam
      use clock
      use display

      implicit none
      save
      
      double precision, dimension(:), allocatable    :: work
      double precision, dimension(:, :), allocatable :: work2
      double precision, dimension(:, :), allocatable :: overlap
      double precision, dimension(:, :), allocatable :: invsq
      integer, dimension(:), allocatable             :: iwork
      integer, dimension(:), allocatable             :: aoidx
      integer :: LWORK, LIWORK

      target  :: work2, overlap, invsq
      private :: work, work2, iwork, lwork, liwork
      private :: overlap, invsq

      interface gemmwrap
            module procedure :: gemmwrap_simple
            module procedure :: gemmwrap_explicit
      end interface gemmwrap

contains

      subroutine linalg_init()
            call dsyevdquery(NORB, LWORK, LIWORK)
            allocate(work(LWORK))
            allocate(iwork(LIWORK))
            allocate(work2(norb, norb))
            allocate(overlap(norb, norb))
            allocate(invsq(norb, norb))
            allocate(aoidx(norb))
      end subroutine linalg_init


      subroutine linalg_free()
            deallocate(work)
            deallocate(work2)
            deallocate(overlap)
            deallocate(invsq)
            deallocate(iwork)
            deallocate(aoidx)
      end subroutine linalg_free


      subroutine dgeevquery(n, lwork, jobvl, jobvr)
            !
            ! Return optimal size of WORK and IWORK
            ! arrays utilized by DGEEV subroutine.
            !
            integer, intent(in)          :: n
            integer, intent(out)         :: lwork
            character(len=1), intent(in) :: jobvl
            character(len=1), intent(in) :: jobvr

            double precision, dimension(1) :: work
            double precision, dimension(1) :: a, vl, vr, wr, wi
            integer :: info
            external :: dgeev

            call dgeev(jobvl, jobvr, n, a, n, wr, wi, vl, n, vr, n, &
                  work, -1, info)
            lwork = ceiling(work(1))
      end subroutine dgeevquery


      subroutine geevwrap(a, wr, wi, vl, vr, n, jobvl, jobvr, work)
            double precision, dimension(:, :), contiguous, intent(inout) :: a
            double precision, dimension(:), contiguous, intent(out)      :: wr
            double precision, dimension(:), contiguous, intent(out)      :: wi
            double precision, dimension(:, :), contiguous, intent(out)   :: vl
            double precision, dimension(:, :), contiguous, intent(out)   :: vr
            integer, intent(in)                                          :: n
            character(len=1), intent(in)                                 :: jobvl
            character(len=1), intent(in)                                 :: jobvr
            double precision, dimension(:), contiguous, intent(out)      :: work

            integer :: lda, ldvl, ldvr, lwork, info
            external :: dgeev

            lda = size(a, dim=1)
            ldvl = size(vl, dim=1)
            ldvr = size(vr, dim=1)
            call dgeevquery(n, lwork, jobvl, jobvr)
            if (size(work) < lwork) then
                  call msg("LINALG ERROR: NOT ENOUGH MEMORY ALLOCATED" // &
                        " FOR DGEEV SUBROUTINE", priority=MSG_ERROR)
                  call imsg("WORK SIZE [WORDS]", size(work), priority=MSG_ERROR)
                  call imsg("OPTIMAL WORK SIZE", lwork, priority=MSG_ERROR)
                  stop
            end if

            call dgeev(jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr, ldvr, &
                  work, lwork, info)
            if (info .ne. 0) then
                  call msg("LINALG ERROR: DIAGONALIZATION FAILED DURING" // &
                        " THE EXECUTION OF DGEEVWRAP SUBROUTINE", priority=MSG_ERROR)
                  call imsg("INFO VALUE", info, priority=MSG_ERROR)
                  stop
            end if
      end subroutine geevwrap


      subroutine dsyevdquery(n, lwork, liwork, jobz)
            !
            ! Return optimal size of WORK and IWORK
            ! arrays utilized by DSYEVD subroutine.
            !
            integer, intent(in)                    :: n
            integer, intent(out)                   :: lwork
            integer, intent(out)                   :: liwork
            character(len=1), optional, intent(in) :: jobz

            character(len=1) :: jobz0
            integer :: info
            double precision, dimension(1) :: a, w
            integer, dimension(1) :: iwork
            double precision, dimension(1) :: work
            external :: dsyevd

            if (present(jobz)) then
                  jobz0 = jobz
            else
                  jobz0 = "V"
            end if
            call dsyevd(jobz0, "L", n, a, n, w, work, -1, iwork, -1, info)
            lwork = ceiling(work(1))
            liwork = iwork(1)
      end subroutine dsyevdquery


      subroutine evd(a, w, n0)
            ! ---------------------------------------------------
            ! Solve eigenproblem of symmetric real matrix A:
            ! A C_i = e_i C_i.
            ! It is assumed that leading dimension of the matrix
            ! A is NORB. 
            ! ---------------------------------------------------
            ! A  - Input, output. Matrix A is destroyed and
            !      eigenvectors are passed as an output. Leading
            !      dimension of A is assumed to be NORB
            ! W  - Output, eigenvalues of matrix A
            ! N0 - Dimension of matrix A. N0 <= NORB.
            !
            double precision, dimension(:, :), contiguous, intent(inout) :: a
            double precision, dimension(:), contiguous, intent(out)      :: w
            integer, optional, intent(in)                                :: n0

            integer :: n

            if (.not. present(n0)) then
                  n = norb
            else
                  n = n0
            end if

            call dsyevdwrap(a, w, n, "V", WORK, IWORK)
      end subroutine evd

      
      subroutine evd_cc(a, w, n0)
            ! ---------------------------------------------------                                                                                                
            ! evd subroutine for coupled cluster code with
            ! NORB=CC_NORB
            double precision, dimension(:, :), contiguous, intent(inout) :: a
            double precision, dimension(:), contiguous, intent(out)      :: w
            integer, optional, intent(in)                                :: n0
            real(F64), dimension(:), allocatable                      :: work
            integer, dimension(:), allocatable                         :: iwork
            integer :: lwork, liwork

            integer :: n

            call dsyevdquery(CC_NORB, lwork, liwork)
            allocate(work(lwork))
            allocate(iwork(liwork))

            if (.not. present(n0)) then
                  n = norb
            else
                  n = n0
            end if

            call dsyevdwrap(a, w, n, "V", work, iwork)

            deallocate(work)
            deallocate(iwork)
      end subroutine evd_cc


      subroutine dsyevdwrap(a, w, n, jobz, work, iwork)
            ! ---------------------------------------------------------
            ! Simplified interface to LAPACK DSYEVD subroutine.
            ! ---------------------------------------------------------
            ! A    - On entry, lower triangle of symmetric matrix.
            !        On exit, orthonormal eigenvectors.
            ! W    - On exit, eigenvalues sorted in ascending order
            ! N    - The order of the matrix A. Note that the leading
            !        dimension of the matrix A need not be equal to N.
            !        LDA is determined automatically by a matrix 
            !        inquiry function.
            ! JOBZ - Compute eigenvalues only: JOBZ="N". Compute
            !        eigenvalues and eigenvectors: JOBZ="V"
            !
            ! WORK, IWORK
            !        Working arrays. Use QUERYEVD subroutine to
            !        determine the optimal size of WORK and IWORK.
            !
            double precision, dimension(:, :), contiguous, intent(inout) :: a
            double precision, dimension(:), contiguous, intent(out)      :: w
            integer, intent(in)                                          :: n
            character(len=1), intent(in)                                 :: jobz
            double precision, dimension(:), contiguous, intent(out)      :: work
            integer, dimension(:), contiguous, intent(out)               :: iwork

            integer :: lwork, liwork, lda, info
            type(tclock) :: t_eigen

            external :: dsyevd

            call clock_start(t_eigen)
            !
            ! Leading dimension of A
            !
            lda = size(a, dim=1)
            !
            ! Test memory allocated for temporary storage
            !

            call dsyevdquery(n, lwork, liwork, jobz=jobz)

            if (size(work) < lwork .or. size(iwork) < liwork) then
                  call msg("LINALG ERROR: NOT ENOUGH MEMORY ALLOCATED" //&
                        " FOR DSYEVD SUBROUTINE", priority=MSG_ERROR)
                  call imsg("WORK SIZE [WORDS]", size(work), priority=MSG_ERROR)
                  call imsg("OPTIMAL WORK SIZE", lwork, priority=MSG_ERROR)
                  call imsg("IWORK SIZE [WORDS]", size(iwork), priority=MSG_ERROR)
                  call imsg("OPTIMAL IWORK SIZE", liwork, priority=MSG_ERROR)
                  stop
            end if
            !
            ! Solve symmetric eigenvalue problem with
            ! LAPACK DSYEV subroutine
            !
            call dsyevd(jobz, "L", n, a, lda, w, work, lwork, &
                  iwork, liwork, info)

            if (info < 0) then
                  call msg("One of arguments on entry to DSYEVD had an illegal value", MSG_ERROR)
                  call msg("INFO value: " // str(info), MSG_ERROR)
                  stop
            end if

            if (info > 0) then
                  call msg("Eigensolver (DSYEVD) failed to converge", MSG_WARNING)
            end if
            TIMINGS(TIME_EIGEN) = TIMINGS(TIME_EIGEN) + clock_readwall(t_eigen)
      end subroutine dsyevdwrap


      subroutine gemmwrap_simple(transa, transb, m, n, k, alpha, a, b, beta, c)
            character(len=1), intent(in)                                 :: transa
            character(len=1), intent(in)                                 :: transb
            integer, intent(in)                                          :: m
            integer, intent(in)                                          :: n
            integer, intent(in)                                          :: k
            double precision, intent(in)                                 :: alpha
            double precision, dimension(:, :), contiguous, intent(in)    :: a
            double precision, dimension(:, :), contiguous, intent(in)    :: b
            double precision, intent(in)                                 :: beta
            double precision, dimension(:, :), contiguous, intent(inout) :: c

            integer :: ld_a, ld_b, ld_c
            external :: dgemm

            ld_a = size(a, dim=1)
            ld_b = size(b, dim=1)
            ld_c = size(c, dim=1)

            call dgemm(transa, transb, m, n, k, alpha, a, ld_a, b, ld_b, beta, c, ld_c)
      end subroutine gemmwrap_simple


      subroutine gemmwrap_explicit(transa, transb, m, n, k, alpha, &
            a, lda, b, ldb, beta, c, ldc)
            
            character(len=1), intent(in)                  :: transa
            character(len=1), intent(in)                  :: transb
            integer, intent(in)                           :: m
            integer, intent(in)                           :: n
            integer, intent(in)                           :: k
            double precision, intent(in)                  :: alpha
            double precision, dimension(*), intent(in)    :: a
            integer, intent(in)                           :: lda
            double precision, dimension(*), intent(in)    :: b
            integer, intent(in)                           :: ldb
            double precision, intent(in)                  :: beta
            double precision, dimension(*), intent(inout) :: c
            integer, intent(in)                           :: ldc

            external :: dgemm

            call dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
      end subroutine gemmwrap_explicit


      subroutine gemvwrap(trans, m, n, alpha, a, x, incx, beta, y, incy)
            character(len=1), intent(in)                              :: trans
            integer, intent(in)                                       :: m
            integer, intent(in)                                       :: n
            double precision, intent(in)                              :: alpha
            double precision, dimension(:, :), contiguous, intent(in) :: a
            double precision, dimension(:), contiguous, intent(in)    :: x
            integer, intent(in)                                       :: incx
            double precision, intent(in)                              :: beta
            double precision, dimension(:), contiguous, intent(inout) :: y
            integer, intent(in)                                       :: incy

            integer :: ld_a
            external :: dgemv

            ld_a = size(a, dim=1)
            call dgemv(trans, m, n, alpha, a, ld_a, x, incx, beta, y, incy)
      end subroutine gemvwrap


      subroutine syrkwrap(uplo, trans, n, k, alpha, a, beta, c)
            character(len=1), intent(in)                                 :: uplo
            character(len=1), intent(in)                                 :: trans
            integer, intent(in)                                          :: n
            integer, intent(in)                                          :: k
            double precision, intent(in)                                 :: alpha
            double precision, dimension(:, :), contiguous, intent(in)    :: a
            double precision, intent(in)                                 :: beta
            double precision, dimension(:, :), contiguous, intent(inout) :: c

            integer :: lda, ldc
            external :: dsyrk

            lda = size(a, dim=1)
            ldc = size(c, dim=1)

            call dsyrk(uplo, trans, n, k, alpha, a, lda, beta, c, ldc)
      end subroutine syrkwrap


      subroutine symmwrap(side, uplo, m, n, alpha, a, b, beta, c)
            character(len=1), intent(in)                                 :: side
            character(len=1), intent(in)                                 :: uplo
            integer, intent(in)                                          :: m
            integer, intent(in)                                          :: n
            double precision, intent(in)                                 :: alpha
            double precision, dimension(:, :), contiguous, intent(in)    :: a
            double precision, dimension(:, :), contiguous, intent(in)    :: b
            double precision, intent(in)                                 :: beta
            double precision, dimension(:, :), contiguous, intent(inout) :: c

            integer :: lda, ldb, ldc
            external :: dsymm

            lda = size(a, dim=1)
            ldb = size(b, dim=1)
            ldc = size(c, dim=1)
            call dsymm(side, uplo, m, n, alpha, a, lda, b, ldb, beta, c, ldc)
      end subroutine symmwrap


      subroutine nrm2wrap(l2norm, n, a, inc)
            !
            ! L2NORM := SQRT(A^T A)
            !
            double precision, intent(out)                          :: l2norm
            integer, intent(in)                                    :: n
            double precision, dimension(:), contiguous, intent(in) :: a
            integer, intent(in)                                    :: inc

            double precision :: dnrm2
            external :: dnrm2

            l2norm = dnrm2(n, a, inc)
      end subroutine nrm2wrap


      subroutine copywrap(n, x, incx, y, incy)
            !
            ! Copy the content of X to Y: Y <- X
            !
            integer, intent(in)                         :: n
            double precision, dimension(*), intent(in)  :: x
            integer, intent(in)                         :: incx
            double precision, dimension(*), intent(out) :: y
            integer, intent(in)                         :: incy

            external :: dcopy
            
            call dcopy(n, x, incx, y, incy)
      end subroutine copywrap


      subroutine scalwrap(n, a, alpha)
            !
            ! A <- ALPHA * A
            !
            integer, intent(in)                                       :: n
            double precision, dimension(:), contiguous, intent(inout) :: a
            double precision, intent(in)                              :: alpha

            external :: dscal

            call dscal(n, alpha, a, 1)
      end subroutine scalwrap


      subroutine dotwrap(d, n, v, w)
            double precision, intent(out)                          :: d
            integer, intent(in)                                    :: n
            double precision, dimension(:), contiguous, intent(in) :: v
            double precision, dimension(:), contiguous, intent(in) :: w

            double precision :: ddot
            external :: ddot

            d = ddot(n, v, 1, w, 1)
      end subroutine dotwrap


      subroutine subevd(a, k, w, n)
            ! --------------------------------------------------------
            ! Solve the subspace-constrained eigenvalue problem of
            ! symmetric matrix A. Constrain eigenvectors to subspace
            ! corresponding to linear combinations of N columns of
            ! matrix K:
            ! K^T A (KC_i) = e_i K^T (KC_i).
            ! Columns of K are assumed to be orthogonal. Solutions of
            ! subspace-constrained eigenvalue problem are orthonormal:
            ! (KC_i)^T (KC_j) = \delta_{ij}.
            ! --------------------------------------------------------
            ! A 
            !     On entry, symmetric real-valued matrix. On exit,
            !     stores N columns of eigenvectors (KC_i)
            ! K 
            !     Input, set of vectors (NORB x N) spanning the subspace
            !     of solutions of the eigenvalue problem
            ! W
            !     Output, eigenvalues
            ! N
            !     Input, number of vectors in the subspace spanned by
            !     the columns of K
            !
            real(F64), dimension(:, :), contiguous, intent(inout) :: a
            real(F64), dimension(:, :), contiguous, intent(in)    :: k
            real(F64), dimension(:), contiguous, intent(out)      :: w
            integer, intent(in)                                   :: n
            !
            ! A <- K^T A K
            !
            call symmwrap("L", "L", norb, n, one, a, k, zero, work2)
            call gemmwrap("T", "N", n, n, norb, one, k, work2, zero, a)
            call evd(a, w, n0=n)
            !
            ! Expand eigenectors in the linear space basis, v = KC
            !
            call gemmwrap("N", "N", NORB, n, n, ONE, k, a, ZERO, work2)
            a(:, 1:n) = work2(:, 1:n)
      end subroutine subevd


      subroutine gvd(a, b, w)
            ! ------------------------------------------------------
            ! Solve generalized eigenproblem of symmetric matrix A:
            ! A C_i = e_i B C_i.
            ! ------------------------------------------------------
            ! A  - On entry, symmetric, real-valued matrix. On exit,
            !      it contains set of eigenvectors C_i
            ! B  - On entry, symmetric positive-definite matrix
            ! W  - Output, contains eigenvalues e_i
            !
            double precision, dimension(:, :), contiguous, intent(inout) :: a
            double precision, dimension(:, :), contiguous, intent(inout) :: b
            double precision, dimension(:), contiguous, intent(out)      :: w

            double precision :: invlambda
            integer :: n, k
            integer :: lwork, liwork
            real(F64), dimension(:,:), allocatable :: work2
            real(F64), dimension(:), allocatable :: work
            integer, dimension(:), allocatable :: iwork           
            real(F64), dimension(:), allocatable :: over_w
            real(F64), dimension(:,:), allocatable :: invsq

            external :: dsyr
            external :: dsymm

            n = size(a, dim=1)
            allocate(invsq(n, n))
            allocate(work2(n, n))
            allocate(over_w(n))

            !
            ! Diagonalize the metric matrix
            !

            call dsyevdquery(n, lwork, liwork, 'V')

            allocate(work(lwork))
            allocate(iwork(liwork))

            call dsyevdwrap(b, over_w, n, 'V', work, iwork)
            
            
            deallocate(work)
            deallocate(iwork)
            !
            ! INVSQ <- B^{-1/2}
            !
            invsq = zero
            do k = 1, n
                  invlambda = one / sqrt(over_w(k))
                  call dsyr("L", n, invlambda, b(:, k), 1, invsq, n)
            end do
            
            deallocate(over_w)

            !
            ! 1. WORK2 <- B^{-1/2} A
            ! 2. A       <- (B^{-1/2} A) B^{-1/2}
            !
            call dsymm("L", "L", n, n, one, invsq, n, &
                  a, n, zero, work2, n)

            call dsymm("R", "L", n, n, one, invsq, n, &
                  work2, n, zero, a, n)

            !
            ! Diagonalize B^{-1/2} A B^{-1/2}
            !

            call dsyevdquery(n, lwork, liwork, 'V')
            allocate(work(lwork))
            allocate(iwork(liwork))

            call dsyevdwrap(a, w, n, 'V', work, iwork)
            
            deallocate(work)
            deallocate(iwork)
            !
            ! Transform eigenvectors back to the basis with metric B
            !
            work2 = a

            call dsymm("L", "L", n, n, one, invsq, n, &
                  work2, n, zero, a, n)


            deallocate(invsq)
            deallocate(work2)

      end subroutine gvd


      subroutine lowdin_ortho(c, thresh)
            ! ----------------------------------------------------
            ! Check orthogonality (norm 1) of columns stored
            ! in C matrix. If orthogonality criterion is not met,
            ! perform Lowdin symmetric orthogonalization on
            ! subset of vectors that are not orthogonal to each
            ! other. 
            ! ----------------------------------------------------
            ! C         - Input, matrix of vectors whose
            !             orthogonality is to be checked
            ! NCOLUMN0  - Input, optional, number of columns of 
            !             matrix C (number of vectors)
            !
            real(F64), dimension(:, :), contiguous, intent(inout) :: c
            real(F64), optional, intent(in)                       :: thresh

            logical :: orthogonalize
            real(F64) :: spq
            integer :: n
            integer :: i, j
            logical :: conditional_ortho

            external :: dsyrk
            external :: dsymm

            n = size(c, dim=2)
            
            if (present(thresh)) then
                  conditional_ortho = .true.
            else
                  conditional_ortho = .false.
            end if
            !
            ! Compute the overlap between input vectors,
            ! OVERLAP <- C^T C (NCOLUMN x NCOLUMN)
            !
            call dsyrk("L", "T", n, norb, one, c, norb, zero, &
                  overlap, norb)

            if (conditional_ortho) then
                  orthogonalize = .false.
                  jloop: do j = 1, n
                        do i = j + 1, n
                              spq = abs(overlap(i, j))
                              if (spq > thresh) then
                                    orthogonalize = .true.
                                    exit jloop
                              end if
                        end do
                  end do jloop
            else
                  orthogonalize = .true.
            end if

            if (orthogonalize) then
                  !
                  ! 1. INVSQ <- S^(-1/2)
                  ! 2. C <- C INVSQ
                  !
                  call sinvsq(overlap, invsq, n0=n)
                  work2(:, 1:n) = c(:, 1:n)
                  call dsymm("R", "L", norb, n, one, invsq, norb, &
                        work2, norb, zero, c, norb)
            end if
      end subroutine lowdin_ortho


      subroutine sinvsq(s, invsq, eigenval, n0)
            ! ------------------------------------------------------------
            ! Compute S^{-1/2}. Both lower and upper triangle
            ! are stored.
            ! ------------------------------------------------------------
            ! S        - On entry, S contains the overlap matrix. On exit, 
            !            it contains eigenvectors of S
            ! INVSQ    - On exit, it contains S^{-1/2} matrix
            ! EIGENVAL - On exit, it contains eigenvalues of S matrix 
            ! N0       - If specified, N0 is the dimension of S matrix,
            !            otherwise NORB is assumed
            !
            double precision, dimension(:, :), contiguous, intent(inout) :: s
            double precision, dimension(:, :), contiguous, intent(out)   :: invsq
            double precision, dimension(:), optional, intent(out)        :: eigenval
            integer, optional, intent(in)                                :: n0

            integer :: i
            integer :: n

            external :: dgemm

            if (.not. present(n0)) then
                  n = norb
            else
                  n = n0
            end if
            !
            ! Compute S = QLQ^T
            ! Use first column of INVSQ matrix as temporary variable
            !
            if (.not. present(n0)) then
                  call evd(s, invsq(:, 1))
            else
                  call evd(s, invsq(:, 1), n)
            end if

            if (present(eigenval)) then
                  eigenval = invsq(:, 1)
            end if
            !
            ! Compute WORK <- Q * 1/SQRT(L)
            !
            do i = 1, n
                  work2(1:n, i) = one / sqrt(invsq(i, 1)) * s(1:n, i)
            end do
            !
            ! Both halves of S^{-1/2} are computed
            !
            call dgemm("N", "T", n, n, n, one, work2, norb, s, norb, zero, invsq, norb)
      end subroutine sinvsq


      subroutine ssqinvsq(s, invsq, sq, eigenval)
            ! ------------------------------------------------------------
            ! Compute S^{-1/2} and S^{1/2} Both lower and upper
            ! triangle are stored. The overlap matrix S is destoryed.
            ! ------------------------------------------------------------
            ! S        - On entry, symmetric positive definite matrix.
            !            Assumed that lower triangle stores data. On exit,
            !            contains eigenvectors of matrix S
            ! INVSQ    - Output, symmetric matrix S^{-1/2}. Full matrix is
            !            stored on exit
            ! SQ       - Output, symmetric matrix S^{1/2}. Full matrix is
            !            stored on exit
            ! EIGENVAL - Output, optional, eigenvalues of the S matrix
            !
            !
            double precision, dimension(:, :), contiguous, intent(inout) :: s
            double precision, dimension(:, :), contiguous, intent(out)   :: invsq
            double precision, dimension(:, :), contiguous, intent(out)   :: sq
            double precision, dimension(:), contiguous, &
                  optional, intent(out)                                  :: eigenval

            double precision :: t
            integer :: i

            external :: dgemm
            !
            ! Compute S = QLQ^T
            ! Use first column of INVSQ matrix as temporary variable
            ! storing eigenvalues.
            !
            call evd(s, invsq(:, 1))
            if (present(eigenval)) then
                  eigenval = invsq(:, 1)
            end if
            !
            ! 1. WORK <- Q * SQRT(LAMBDA)
            ! 2. SQ   <- WORK * Q^T
            !
            do i = 1, norb
                  t = sqrt(invsq(i, 1))
                  work2(:, i) = t * s(:, i)
            end do
            call dgemm("N", "T", norb, norb, norb, one, &
                  work2, norb, s, norb, zero, sq, norb)
            !
            ! 1. WORK  <- Q * 1 / SQRT(LAMBDA)
            ! 2. INVSQ <- WORK * Q^T
            !
            do i = 1, norb
                  t = sqrt(invsq(i, 1))
                  work2(:, i) = one / t * s(:, i)
            end do
            call dgemm("N", "T", norb, norb, norb, one, &
                  work2, norb, s, norb, zero, invsq, norb)
      end subroutine ssqinvsq


      subroutine spseudoinv(s, vec, n, invsq, sq, eigenval)
            ! -----------------------------------------------------------
            ! Compute pseudo square root and pseudo inverse of pseudo
            ! sqare root of a symmetric positive definite matrix S:
            ! 1. Orthogonalize columns of VEC
            ! 2. SQ    <- VEC (VEC^T S VEC)^{1/2} VEC^T
            ! 3. INVSQ <- VEC (VEC^T S VEC)^{-1/2} VEC^T
            !
            ! SQ and INVSQ have the following properties:
            ! INVSQ * (P^T S P) = SQ,
            ! INVSQ * SQ = SQ * INVSQ = P,
            ! SQ * SQ = P^T S P,
            ! where P is projector onto SPAN(VEC):
            ! P = VEC VEC^T.
            !
            ! Solving the generalized eigenvalue problem,
            ! F C_i = e_i S C_i,
            ! with a constraint C_i \in SPAN(VEC) is equivalent
            ! to solving
            ! (INVSQ F INVSQ) (SQ C_i) = e_i (SQ C_i)
            ! with the same constraint. Appropriate choice of SPAN(VEC)
            ! makes the generalized eigenvalue problem better conditioned
            ! when the overlap matrix S is nearly singular. Columns of
            ! VEC may represent a subset of linearly independent atomic
            ! orbitals of a set of nearly linearly dependent AOs.
            ! -----------------------------------------------------------
            !
            !
            double precision, dimension(:, :), contiguous, intent(inout) :: s
            double precision, dimension(:, :), contiguous, intent(inout) :: vec
            integer, intent(in)                                          :: n
            double precision, dimension(:, :), contiguous, intent(out)   :: invsq
            double precision, dimension(:, :), contiguous, intent(out)   :: sq
            double precision, dimension(:), contiguous, intent(out)      :: eigenval

            double precision :: lambda, invlambda
            integer :: k

            external :: dsymm
            external :: dgemm
            external :: dsyrk
            external :: dsyr
            !
            ! VEC * VEC^T is positive semi-definite real
            ! symmetric matrix. Zero eigenvalues are associated
            ! with vectors that belong to the subspace orthogonal
            ! to the subspace spanned by VEC columns.
            ! --
            ! SQ  <- VEC * VEC^T
            ! VEC <- Orthogonalized vectors belonging to SPAN(VEC)
            !
            call dsyrk("L", "N", norb, n, one, vec, norb, zero, sq, norb)
            call evd(sq, eigenval)
            vec(:, 1:n) = sq(:, norb-n+1:norb)
            !
            ! SQ <- S * VEC
            ! S  <- VEC^T * SQ
            !
            call dsymm("L", "L", norb, n, one, s, norb, &
                  vec, norb, zero, sq, norb)
            call dgemm("T", "N", n, n, norb, one, vec, norb, &
                  sq, norb, zero, s, norb)
            !
            ! Diagonalize transformed S matrix
            !
            call evd(s, eigenval, n)
            !
            ! SQ    <- (VEC^T S VEC)^{1/2}
            ! INVSQ <- (VEC^T S VEC)^{-1/2}
            !
            sq = zero
            invsq = zero
            do k = 1, n
                  lambda = sqrt(eigenval(k))
                  invlambda = one / lambda
                  call dsyr("L", n, lambda, s(:, k), 1, sq, norb)
                  call dsyr("L", n, invlambda, s(:, k), 1, invsq, norb)
            end do
            !
            ! S     <- VEC SQ
            ! SQ    <- S VEC^T
            ! S     <- VEC INVSQ
            ! INVSQ <- S VEC^T
            !
            call dsymm("R", "L", norb, n, one, sq, norb, &
                  vec, norb, zero, s, norb)
            call dgemm("N", "T", norb, norb, n, one, s, norb, &
                  vec, norb, zero, sq, norb)
            call dsymm("R", "L", norb, n, one, invsq, norb, &
                  vec, norb, zero, s, norb)
            call dgemm("N", "T", norb, norb, n, one, s, norb, &
                  vec, norb, zero, invsq, norb)
      end subroutine spseudoinv


      function sydotp(a, b)
            ! -------------------------------------------------------
            ! Dot product of symmetric matrices. Only lower triangles
            ! of the matrices are referenced.
            ! -------------------------------------------------------
            ! A, B - Input, symmetric matrices. Assumed that lower
            !        triangles are stored
            !
            double precision                              :: sydotp
            double precision, dimension(:, :), intent(in) :: a, b

            double precision :: s
            integer :: i, j

            s = zero
            do j = 1, norb
                  s = s + frac12 * a(j, j) * b(j, j)
                  do i = j + 1, norb
                        s = s + a(i, j) * b(i, j)
                  end do
            end do

            sydotp = two * s
      end function sydotp


      function frobnorm(x, m0, n0)
            !
            ! Frobenius norm of a rectangular matrix
            !
            double precision :: frobnorm

            double precision, dimension(:, :), intent(in) :: x
            integer, optional, intent(in) :: m0, n0

            integer :: m, n
            integer :: i, j

            m = NORB
            n = NORB
            if (present(m0)) m = m0
            if (present(n0)) n = n0

            frobnorm = zero

            do j = 1, n
                  do i = 1, m
                        frobnorm = frobnorm + x(i, j)**2
                  end do
            end do

            frobnorm = sqrt(frobnorm)
      end function frobnorm


      function maxnorm(x, m0, n0)
            !
            ! Max norm of a rectangular matrix
            !
            double precision                              :: maxnorm
            double precision, dimension(:, :), intent(in) :: x
            integer, optional, intent(in)                 :: m0, n0

            integer :: m, n
            integer :: i, j
            double precision :: t

            m = NORB
            n = NORB
            if (present(m0)) m = m0
            if (present(n0)) n = n0

            maxnorm = zero
            do j = 1, n
                  do i = 1, m
                        t = abs(x(i, j))
                        if (t .gt. maxnorm) maxnorm = t
                  end do
            end do
      end function maxnorm


      subroutine atbc(d, a, b, c, m, n)
            ! ---------------------------------------------
            ! Perform change of basis operation:
            ! D <- A^T B C.
            ! Leading dimension of D is calculated using
            ! array inquiry function.
            ! ---------------------------------------------
            ! D       - Output, transformed matrix
            ! A, B, C - Input, two dimensional matrices
            !           Leading dimension equal to NORB
            ! M, N    - Number of columns of A and C matrices
            !
            double precision, dimension(:, :), contiguous, intent(out) :: d
            double precision, dimension(:, :), contiguous, intent(in)  :: a
            double precision, dimension(:, :), contiguous, intent(in)  :: b
            double precision, dimension(:, :), contiguous, intent(in)  :: c
            integer, intent(in)                                        :: m, n            
            integer :: ldd
            external :: dgemm


            ldd = size(d, dim=1)
            !
            ! 1. WORK2 <- A^T B
            ! 2. D <- WORK2 C
            !
            call dgemm("T", "N", m, norb, norb, one, a, norb, b, norb, &
                  zero, work2, norb)
            call dgemm("N", "N", m, n, norb, one, work2, norb, c, norb, &
                  zero, d, ldd)
      end subroutine atbc

      
      subroutine atbc3(d, a, b, c, m, n, n0)
            double precision, dimension(:, :), contiguous, intent(out) :: d
            double precision, dimension(:, :), contiguous, intent(in)  :: a
            double precision, dimension(:, :), contiguous, intent(in)  :: b
            double precision, dimension(:, :), contiguous, intent(in)  :: c
            integer, intent(in)                                        :: m, n            
            integer, optional, intent(in)                              :: n0
            integer                                                    :: nn
            integer :: ldd
            real(F64), dimension(:,:), allocatable :: work2
            external :: dgemm


            if (.not. present(n0)) then
                  nn = norb
            else
                  nn = n0
            end if

            allocate(work2(nn, nn))

            ldd = size(d, dim=1)
            !
            ! 1. WORK2 <- A^T B                           
            ! 2. D <- WORK2 C                                                  
            call dgemm("T", "N", m, nn, nn, one, a, nn, b, nn, &
                  zero, work2, nn)
            call dgemm("N", "N", m, n, nn, one, work2, nn, c, nn, &
                  zero, d, ldd)

            deallocate(work2)
      end subroutine atbc3

      
      subroutine atbc4(d, a, b, c, m, n, n0, work2)
            double precision, dimension(:, :), contiguous, intent(out) :: d
            double precision, dimension(:, :), contiguous, intent(in)  :: a
            double precision, dimension(:, :), contiguous, intent(in)  :: b
            double precision, dimension(:, :), contiguous, intent(in)  :: c
            integer, intent(in)                                        :: m, n            
            integer, optional, intent(in)                              :: n0
            real(F64), dimension(:,:), contiguous, intent(in)          :: work2
            integer                                                    :: nn
            integer :: ldd
            external :: dgemm


            if (.not. present(n0)) then
                  nn = norb
            else
                  nn = n0
            end if

            ldd = size(d, dim=1)
            !
            ! 1. WORK2 <- A^T B
            ! 2. D <- WORK2 C
            !
            call dgemm("T", "N", m, nn, nn, one, a, nn, b, nn, &
                  zero, work2, nn)
            call dgemm("N", "N", m, n, nn, one, work2, nn, c, nn, &
                  zero, d, ldd)
      end subroutine atbc4


      subroutine abct(d, a, b, c, m, n)
            ! ---------------------------------------------
            ! Perform change of basis operation:
            ! D <- A B C^T.
            ! Leading dimension of D is calculated using
            ! array inquiry function.
            ! ---------------------------------------------
            ! D       - Output, transformed matrix
            ! A, B, C - Input, two dimensional matrices
            !           Leading dimension equal to NORB
            ! M, N    - Number of columns of A and C matrices
            !
            double precision, dimension(:, :), contiguous, intent(out) :: d
            double precision, dimension(:, :), contiguous, intent(in)  :: a
            double precision, dimension(:, :), contiguous, intent(in)  :: b
            double precision, dimension(:, :), contiguous, intent(in)  :: c
            integer, intent(in)                                        :: m, n

            integer :: ldd
            external :: dgemm

            ldd = size(d, dim=1)
            !
            ! 1. WORK2 <- B C^T
            ! 2. D <- A WORK2
            !
            call dgemm("N", "T", m, norb, norb, one, a, norb, b, norb, &
                  zero, work2, norb)
            call dgemm("N", "N", m, n, norb, one, work2, norb, c, norb, &
                  zero, d, ldd)
      end subroutine abct


      subroutine atsybc(d, a, b, c, m, n)
            ! ---------------------------------------------
            ! Perform change of basis operation:
            ! D <- A^T B C,
            ! where B is a symmetrc matrix. Only lower
            ! triangle of B is referenced.
            ! ---------------------------------------------
            ! D       - Output, transformed matrix (M x N).
            !           The leading dimension of D is 
            !           determined via matrix inquiry 
            !           function.
            ! A, B, C - Input, two dimensional matrices
            !           Leading dimension equal to NORB
            ! M, N    - Number of columns of A and C matrices
            !
            double precision, dimension(:, :), contiguous, intent(out) :: d
            double precision, dimension(:, :), contiguous, intent(in)  :: a
            double precision, dimension(:, :), contiguous, intent(in)  :: b
            double precision, dimension(:, :), contiguous, intent(in)  :: c
            integer, intent(in)                                        :: m, n

            integer :: ldd
            external :: dgemm
            external :: dsymm

            ldd = size(d, dim=1)
            !
            ! WORK2 <- B C (DSYMM)
            !
            call dsymm("L", "L", NORB, n, ONE, b, NORB, c, NORB, ZERO, work2, NORB)
            !
            ! D <- A^T WORK2 (DGEMM)
            !
            call dgemm("T", "N", m, n, NORB, ONE, a, NORB, work2, NORB, ZERO, d, ldd)
      end subroutine atsybc


      subroutine atsyba(d, a, b, n)
            ! ---------------------------------------------
            ! Perform change of basis operation:
            ! D <- A^T B A,
            ! where B is a symmetrc matrix. Only lower
            ! triangles of B and D matrices are referenced.
            ! ---------------------------------------------
            ! D       - Output, transformed matrix (N x N).
            !           The leading dimension of D is 
            !           determined via matrix inquiry 
            !           function.
            ! A, B    - Input, two dimensional matrices
            !           Leading dimension equal to NORB
            ! N       - Number of columns of the matrix A 
            !
            double precision, dimension(:, :), contiguous, intent(out) :: d
            double precision, dimension(:, :), contiguous, intent(in)  :: a
            double precision, dimension(:, :), contiguous, intent(in)  :: b
            integer, intent(in)                                        :: n

            double precision :: alpha
            integer :: ldd
            external :: dsyr2k
            external :: dsymm

            ldd = size(d, dim=1)
            !
            ! WORK2 <- B A (DSYMM)
            !
            call dsymm("L", "L", NORB, n, ONE, b, NORB, a, NORB, &
                  ZERO, work2, NORB)
            !
            ! D <- A^T (BA) = 1/2 (BA)^T A + 1/2 A^T (BA)
            !
            alpha = FRAC12
            call dsyr2k("L", "T", n, NORB, alpha, work2, NORB, a, &
                  NORB, ZERO, d, ldd)
      end subroutine atsyba


      subroutine expm(e, x, k, j)
            ! --------------------------------------------------------------
            ! Compute matrix exponential by scaling and squaring technique:
            ! T_k(X) = \sum_{j=1}^{k} X^j / j!
            ! \exp(X) = (T_k(X/2^j))^{2^j}
            ! --------------------------------------------------------------
            ! 1. Cleve Moler, Charles Van Loan, Nineteen Dubious  Ways to
            !    Compute the Exponential of a Matrix, Twenty-Five Years
            !    Later, SIAM Review, 45. Optimum scaling and sqaring
            !    parameters with Taylor series approximation in Tab. 1, p.11
            ! --------------------------------------------------------------
            ! E    - Output, matrix exponential
            ! X    - Input, exponentiated matrix
            ! K, J - Input, parameters controlling accuracy. Assumed that
            !        J, K >= 1. For optimum choice see Tab. 1, p. 11 in [1]
            !
            double precision, dimension(:, :), contiguous, intent(out)   :: e
            double precision, dimension(:, :), contiguous, intent(inout) :: x
            integer, intent(in)                                          :: k, j

            double precision, dimension(:, :), contiguous, pointer :: work1, work2, pt
            double precision :: coeff1
            integer :: i
            integer :: n

            external :: dgemm
            !
            ! Use OVERLAP and INVSQ as scratch matrices
            !
            work1 => overlap
            work2 => invsq

            n = NORB

            x = one / two**j * x
            work1 = x
            !
            ! Taylor expansion of exponential
            !
            e = work1

            do i = 2, k
                  coeff1 = one / dble(i)
                  call dgemm("N", "N", n, n, n, coeff1, x, n, work1, n, zero, work2, n)
                  pt => work1
                  work1 => work2
                  work2 => pt
                  e = e + work1
            end do

            do i = 1, n
                  e(i, i) = e(i, i) + one
            end do
            !
            ! exp(X / 2**j)**(2**j)
            !
            call dgemm("N", "N", n, n, n, one, e, n, e, n, zero, work1, n)

            do i = 2, j - 1
                  call dgemm("N", "N", n, n, n, one, work1, n, work1, n, zero, work2, n)
                  pt => work1
                  work1 => work2
                  work2 => pt
            end do

            if (j .ge. 2) then
                  call dgemm("N", "N", n, n, n, one, work1, n, work1, n, zero, e, n)
            else
                  e = work1
            end if
      end subroutine expm


      subroutine exp_transform(c, x)
            ! -----------------------------------------------------
            ! Compute MO vectors transformed by an exponential
            ! operator: C(X) <- exp(-X) C. X is destroyed on exit.
            ! Assumptions: 1) X is antisymmetric, 2) both lower and
            ! upper triangles of X are stored.
            ! -----------------------------------------------------
            ! 1. Cleve Moler, Charles Van Loan, Nineteen Dubious
            !    Ways to Compute the Exponential of a Matrix,
            !    Twenty-Five Years Later, SIAM Review, 45, 3 (2003)
            !    doi: 10.1137/S00361445024180
            !    The optimal scaling and sqaring parameters for
            !    the Taylor series approximation are taken from Table 1,
            !    page 11.
            !
            double precision, dimension(:, :), contiguous, intent(inout) :: c
            double precision, dimension(:, :), contiguous, intent(inout) :: x

            double precision :: a

            external :: dgemm

            a = frobnorm(x, norb, norb)
            !
            ! WORK <- EXP(X)
            !
            if (a .le. 1.d-2) then
                  call expm(work2, x, 5, 1)
            else if (a .le. 1.d-1) then
                  call expm(work2, x, 5, 4)
            else if (a .le. 1.d+0) then
                  call expm(work2, x, 7, 5)
            else if (a .le. 1.d+1) then
                  call expm(work2, x, 9, 7)
            else if (a .le. 1.d+2) then
                  call expm(work2, x, 10, 10)
            else if (a .le. 1.d+3) then
                  call expm(work2, x, 8, 14)
            else
                  call msg("ARH ERROR: POORLY CONDITIONED MATRIX", MSG_ERROR)
                  stop
            end if
            !
            ! Perform exponential transformation:
            ! C(C) <- exp(X) C
            ! ---
            ! Note that exponential matrix is transposed:
            ! (\exp(X))^T = \exp(-X)
            ! 
            call dgemm("T", "N", norb, norb, norb, one, &
                  work2, norb, c, norb, zero, x, norb)
            c = x
      end subroutine exp_transform


      subroutine smpack(unpacked, packed, n)
            !
            ! Pack lower triangle of a symmetric matrix into
            ! a one-dimensional representation.            
            !
            double precision, dimension(:, :), intent(in) :: unpacked
            double precision, dimension(:), intent(out)   :: packed
            integer, optional, intent(in)                 :: n

            integer :: i, j, idx
            integer :: m

            if (present(n)) then
                  m = n
            else
                  m = NORB
            end if

            idx = 1
            do j = 1, m
                  do i = j, m
                        packed(idx) = unpacked(i, j)
                        idx = idx + 1
                  end do
            end do
      end subroutine smpack


      subroutine smunpack(packed, unpacked, n)
            !
            ! Unpack one-dimensional packed matrix into 
            ! two-dimensional form. Only lower triangle of 
            ! symmetric matrix is referenced.
            !
            double precision, dimension(:), intent(in)     :: packed
            double precision, dimension(:, :), intent(out) :: unpacked
            integer, optional, intent(in)                  :: n

            integer :: i, j, idx
            integer :: m

            if (present(n)) then
                  m = n
            else
                  m = NORB
            end if

            idx = 1
            do j = 1, m
                  do i = j, m
                        unpacked(i, j) = packed(idx)
                        idx = idx + 1
                  end do
            end do
      end subroutine smunpack


      subroutine smfill(a)
            !
            ! Reconstruct the upper triangle of a symmetric matrix
            !
            real(F64), dimension(:, :), intent(inout) :: a

            integer :: i, j, n

            n = size(a, dim=1)
            do j = 1, n
                  do i = j + 1, n
                        a(j, i) = a(i, j)
                  end do
            end do
      end subroutine smfill


      subroutine amfill(a)
            !
            ! Reconstruct the upper triangle of an antisymmetric matrix
            !
            real(F64), dimension(:, :), intent(inout) :: a

            integer :: i, j, n

            n = size(a, dim=1)
            do j = 1, n
                  do i = j + 1, n
                        a(j, i) = -a(i, j)
                  end do
            end do
      end subroutine amfill


      subroutine weightedsum(a, b, alpha, beta)
            ! 
            ! A, B - symmetric matrices. Upper triangle
            ! of A not referenced.
            !
            ! A <- alpha * A + beta * B
            !
            double precision, dimension(:, :), intent(inout) :: a
            double precision, dimension(:, :), intent(in)    :: b
            double precision, intent(in)                     :: alpha, beta

            double precision :: t
            integer :: p, q

            do q = 1, NORB
                  do p = q, NORB
                        t = alpha * a(p, q) + beta * b(p, q)
                        a(p, q) = t
                  end do
            end do
      end subroutine weightedsum


      subroutine anticomm(c, a, b, alpha)
            !
            ! A - Symmetric matrix (upper triange not referenced)
            ! B - General matrix (both triangles referenced)
            !
            ! C <- alpha * (AB + BA) + C
            !
            double precision, dimension(:, :), contiguous, intent(inout) :: c
            double precision, dimension(:, :), contiguous, intent(in)    :: a
            double precision, dimension(:, :), contiguous, intent(in)    :: b
            double precision, intent(in)                                 :: alpha

            external :: dsymm
            !
            ! C <- C + AB
            !
            call dsymm("L", "L", NORB, NORB, alpha, a, NORB, &
                  b, NORB, one, c, NORB)
            !
            ! C <- C + BA
            !
            call dsymm("R", "L", NORB, NORB, alpha, a, NORB, &
                  b, NORB, one, c, NORB)
      end subroutine anticomm


      subroutine oaotrans(a, invsq)
            ! -----------------------------------------------------------
            ! Transform a symmetric matrix A to the orthogonalized
            ! atomic orbital basis
            ! A <- S^{-1/2} A S^{-1/2}
            ! Only lower triangle of A is referenced as the input. Both
            ! lower and upper triangles of INVSQ are referenced. Both
            ! lower and upper triangles of A are generated as the output.
            ! -----------------------------------------------------------
            ! A     - input / ouptut symmetric matrix
            ! INVSQ - S^{-1/2}, assumed both triangles are stored
            !
            double precision, dimension(:, :), contiguous, intent(inout) :: a
            double precision, dimension(:, :), contiguous, intent(in)    :: invsq

            external :: dsymm
            !
            ! 1. WORK2 <- A S^{-1/2}
            ! 2. A     <- S^{-1/2} WORK
            !
            call dsymm("L", "L", NORB, NORB, one, a, &
                  NORB, invsq, NORB, zero, work2, NORB)
            call dsymm("L", "L", NORB, NORB, one, invsq, &
                  NORB, work2, NORB, zero, a, NORB)
      end subroutine oaotrans


      subroutine geinv(a, n)
            ! -----------------------------------------------------
            ! Invert real non-symmetric matrix A. The scratch
            ! arrays are allocated and deallocated inside this
            ! subroutine.
            ! -----------------------------------------------------
            ! A 
            !     On entry, real non-symmetric matrix. On exit,
            !     the inverse of A.
            ! N 
            !     The order of the matrix A
            !
            double precision, dimension(:, :), contiguous, intent(inout) :: a
            integer, intent(in)                                          :: n

            integer :: lda, lwork, info
            integer, dimension(:), allocatable :: ipiv
            double precision, dimension(:), allocatable :: work
            double precision, dimension(1) :: work0
            external :: dgetrf
            external :: dgetri

            lda = size(a, dim=1)
            allocate(ipiv(n))
            call dgetrf(n, n, a, lda, ipiv, info)
            call dgetri(n, a, lda, ipiv, work0, -1, info)
            lwork = ceiling(work0(1))
            allocate(work(lwork))
            call dgetri(n, a, lda, ipiv, work, lwork, info)
            deallocate(work)
            deallocate(ipiv)
      end subroutine geinv


      subroutine sminv(ainv, a, n)
            ! ------------------------------------------------------
            ! Invert real symmetric matrix A. The inversion is done
            ! with help of a diagonalization subroutine. Do not use
            ! this subroutine to invert large matrices. The leading
            ! dimension of the A matrix is determined via an inquiry
            ! function.
            ! ------------------------------------------------------
            ! AINV - Output, A^{-1} matrix. Dimension (LDA, LDA).
            !        Both upper and lower triangular parts are
            !        stored
            ! A    - Input, real symmetric matrix. It is assumed
            !        that only lower triangle stores data.
            ! N    - Input, order of matrix A
            !
            real(F64), dimension(:, :), contiguous, intent(out)   :: ainv
            real(F64), dimension(:, :), contiguous, intent(inout) :: a
            integer, intent(in)                                   :: n

            integer :: lwork
            double precision, dimension(:), allocatable :: work
            double precision, dimension(:), allocatable :: w
            double precision, dimension(:, :), allocatable :: work2
            integer :: info
            integer :: i, j, k
            integer :: lda
            external :: dsyev

            lda = size(a, dim=1)
            lwork = max(lda**2, 3 * lda - 1)
            allocate(work(lwork))
            allocate(w(lda))
            allocate(work2(lda, lda))
            !
            ! Symmetric matrix A is inverted using diagonalization
            ! subroutine due to its good numerical stability.
            !
            call dsyev("V", "L", n, a, lda, w, work, lwork, info)
            work2(:, :) = a
            do i = 1, n
                  work2(1:n, i) = work2(1:n, i) / w(i)
            end do

            do j = 1, n
                  do i = 1, n
                        ainv(i, j) = zero
                        do k = 1, n
                              ainv(i, j) = ainv(i, j) + work2(i, k) * a(j, k)
                        end do
                  end do
            end do
      end subroutine sminv


      subroutine nullspace(nullvecs, a, evals)
            real(F64), dimension(:, :), intent(out)               :: nullvecs
            real(F64), dimension(:, :), contiguous, intent(inout) :: a
            real(F64), dimension(:), contiguous, intent(out)      :: evals

            integer :: nulldim

            nulldim = size(nullvecs, dim=2)

            if (nulldim == 0) then
                  return
            end if

            call evd(a, evals)
            !
            ! Eigenvectors are ordered according to increasing eigenvalues.
            ! Thus, the eigenvalues belonging to the nullspace are the 
            ! first ones.
            !
            nullvecs(:, 1:nulldim) = a(:, 1:nulldim)
      end subroutine nullspace


      function maxdiff(d1, d2, n)
            real(F64)                              :: maxdiff
            real(F64), dimension(:, :), intent(in) :: d1, d2
            integer, intent(in)                    :: n

            double precision :: d
            integer :: i, j

            maxdiff = zero
            do j = 1, n
                  do i = j, n
                        d = abs(d1(i, j) - d2(i, j))
                        if (d .gt. maxdiff) maxdiff = d
                  end do
            end do
      end function maxdiff


      function trace(rho, a)
            ! -----------------------------------------------------
            ! Calculate the average value of an observable A:
            ! <A> = TRACE <- Tr(RHO * A), 
            ! where both RHO and A are symmetric matrices. Upper
            ! triangles of RHO and A are not referenced.
            !
            real(F64)                              :: trace
            real(F64), dimension(:, :), intent(in) :: rho
            real(F64), dimension(:, :), intent(in) :: a

            real(F64) :: t
            integer :: i, j, n

            n = size(a, dim=1)
            trace = zero
            do j = 1, n
                  trace = trace + rho(j, j) * a(j, j)
                  t = zero
                  do i = j + 1, n
                        t = t + rho(i, j) * a(i, j)
                  end do
                  trace = trace + two * t
            end do
      end function trace


      subroutine transform_orbitals(ct, a, c)
            !
            ! Change basis of orbital coefficients:
            ! C(transformed) <- A C(old basis),
            ! where A is a symmetric matrix. Only lower triangle
            ! of A is referenced.
            !
            ! Examples
            ! --------- 
            ! C(OAO) <-- S^{1/2} C(AO)
            ! C(AO)  <-- S^{-1/2} C(OAO)
            !
            real(F64), dimension(:, :), contiguous, intent(out) :: ct
            real(F64), dimension(:, :), contiguous, intent(in)  :: a
            real(F64), dimension(:, :), contiguous, intent(in)  :: c
            
            integer :: m, n

            m = size(ct, dim=1)
            n = size(ct, dim=2)

            call symmwrap("L", "L", m, n, ONE, a, c, ZERO, ct)
      end subroutine transform_orbitals
      
      
      subroutine density_matrix(rho, c, f)
            !
            ! Compute density matrix:
            ! RHO <- F C C^T,
            ! where F is a scalar representing the occupation
            ! number of orbitals stored in columns of C. Both
            ! lower and upper triangle of RHO are computed.
            !
            real(F64), dimension(:, :), contiguous, intent(out) :: rho
            real(F64), dimension(:, :), contiguous, intent(in)  :: c
            real(F64), intent(in)                               :: f

            integer :: m, n

            if (size(c, dim=1) .ne. size(rho, dim=1)) then
                  call msg("INCONSISTENT DIMENSIONS ON ENTRY TO DENSITY_MATRIX", MSG_ERROR)
                  stop
            end if

            m = size(c, dim=1)
            n = size(c, dim=2)
            !
            ! RHO <-  F C C^T (only lower triangle of RHO is referenced)
            !
            call syrkwrap("L", "N", m, n, f, c, ZERO, rho)
            !
            ! Fill the upper triangle of RHO with meaningful numbers
            !
            call smfill(rho)
      end subroutine density_matrix


      function issingular(a, n, threshold)
            !
            ! Compute an estimate of the condition number of a symmetric matrix A
            ! to check if A is singular. The check is done against a given threshold
            ! for the reciprocal of the condition number.
            !
            ! Both lower and upper triangles of A are referenced.
            !
            logical                                               :: issingular
            real(F64), dimension(:, :), contiguous, intent(inout) :: a
            integer, intent(in)                                   :: n
            real(F64), intent(in)                                 :: threshold

            real(F64) :: rcond

            call rcondition_number(rcond, a, n)
            if (rcond < threshold) then
                  issingular = .true.
            else
                  issingular = .false.
            end if
      end function issingular


      subroutine rcondition_number(rcond, a, n)
            ! -----------------------------------------------------------------------
            ! Estimate the reciprocal of the condition number of a symmetric matrix A
            ! in the 1-norm
            ! 
            ! cond = ||A||_1 * ||inv(A)||_1
            ! rcond = 1/cond
            ! 
            ! The 1-norm of inv(A) is computed using the DSYCON subroutine from
            ! LAPACK.
            !
            ! Both lower and upper triangles of A are referenced.
            ! ----------------------------------------------------------------------
            ! RCOND
            !       The reciprocal of the condition number of a symmetric matrix A
            ! A      
            !       Symmetric matrix, both triangles referenced
            ! N
            !       Order of the matrix A
            !
            real(F64), intent(out)                                :: rcond
            real(F64), dimension(:, :), contiguous, intent(inout) :: a
            integer, intent(in)                                   :: n

            real(F64) :: colsum, anorm
            integer, dimension(:), allocatable :: ipiv
            integer, dimension(:), allocatable :: iwork
            real(F64), dimension(:), allocatable :: work
            real(F64), dimension(1) :: work0
            integer :: lwork
            integer :: lda, info
            integer :: p, q            
            external :: dsytrf
            external :: dsycon
            
            allocate(ipiv(n))
            allocate(iwork(n))
            lda = size(a, dim=1)
            !
            ! Compute ||A||_1 = max_j sum_i |A_ij|
            !
            anorm = ZERO
            do q = 1, n
                  colsum = ZERO
                  do p = 1, n
                        colsum = colsum + abs(a(p, q))
                  end do
                  anorm = max(anorm, colsum)
            end do
            !
            ! LU decomposition
            !
            call dsytrf("L", n, a, lda, ipiv, work0, -1, info)
            lwork = ceiling(work0(1))
            !
            ! Take the maximum value of (lwork, 2*n), where 
            ! lwork is required by DSYTRF and 2*N is required
            ! by DSYCON
            !
            lwork = max(lwork, 2*n)
            allocate(work(lwork))
            call dsytrf("L", n, a, lda, ipiv, work, lwork, info)
            call dsycon("L", n, a, lda, ipiv, anorm, rcond, work, iwork, info)

      end subroutine rcondition_number


      subroutine scal_matrix(a, alpha)
            double precision, dimension(:, :), contiguous, intent(inout) :: a
            double precision, intent(in)                                 :: alpha

            integer :: n
            external :: dscal

            n = NORB**2
            call dscal(n, alpha, a, 1)
      end subroutine scal_matrix


      subroutine delete_row_col(aout, ain, i, j)
            !
            ! Copy Ain into Aout with the I-th row and J-th column
            ! removed
            !
            real(F64), dimension(:, :), intent(out) :: aout
            real(F64), dimension(:, :), intent(in)  :: ain
            integer, intent(in)                     :: i
            integer, intent(in)                     :: j 
            
            integer :: m, n
            integer :: p, q

            m = size(ain, dim=1)
            n = size(ain, dim=2)

            do q = 1, j - 1
                  do p = 1, i - 1
                        aout(p, q) = ain(p, q)
                  end do

                  do p = i + 1, m
                        aout(p-1, q) = ain(p, q)
                  end do
            end do
            
            do q = j + 1, n
                  do p = 1, i - 1
                        aout(p, q-1) = ain(p, q)
                  end do

                  do p = i + 1, m
                        aout(p-1, q-1) = ain(p, q)
                  end do
            end do
      end subroutine delete_row_col


      subroutine swap(a, b)
            real(F64), dimension(:), intent(inout) :: a
            real(F64), dimension(:), intent(inout) :: b

            real(F64) :: t
            integer :: k

            do k = 1, size(a)
                  t = a(k)
                  a(k) = b(k)
                  b(k) = t
            end do
      end subroutine swap


      pure function pq2compound(p, q, m)
            !
            ! Compute compound 2-index, (pq), assuming that p is always
            ! greater or equal q. Min. index: 1, max index: M.
            ! Example: P and Q enumerate, respectively, rows and columns
            ! of a symmetric matrix. The compound 2-index enumerates 
            ! consecutive elements of the lower triangle of the 5x5 matrix.
            !
            ! M = 5
            !             Q 
            !    | 1                |
            !    | 2  6             |
            ! P  | 3  7  10         |
            !    | 4  8  11  13     |
            !    | 5  9  12  14  15 |
            !
            integer             :: pq2compound
            integer, intent(in) :: p
            integer, intent(in) :: q
            integer, intent(in) :: m

            integer :: i1, i2

            i1 = ((2 * m - q + 2) * (q - 1)) / 2
            i2 = p - q + 1
            pq2compound = i1 + i2
      end function pq2compound


      pure subroutine sm_unpack(a, a_pack)
            !
            ! Unpack a symmetric matrix stored in a one-dimensinal form 
            ! using the PQ2COMPOUND ordering of indices. Only the lower
            ! triangle of A is referenced. A must be initialized on entry
            ! to this subroutine.
            !
            real(F64), dimension(:, :), intent(inout) :: a
            real(F64), dimension(:), intent(in)       :: a_pack

            integer :: n, p, q, k

            n = size(a, dim=1)
            
            k = 1
            do p = 1, n
                  do q = p, n
                        a(q, p) = a(q, p) + a_pack(k)
                        k = k + 1
                  end do
            end do
      end subroutine sm_unpack
end module linalg

