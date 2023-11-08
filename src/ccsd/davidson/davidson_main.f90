! -----------------------------------------------------------------
! DAVIDSON'S ALGORITHM FOR LARGE SYMMETRIC AND NONSYMMETRIC
! EIGENVALUE PROBLEMS
! -----------------------------------------------------------------
! Look up the DAV_UNITTEST subroutine for an example how to use
! this code. Please launch DAV_UNITTEST to verify the code whenever
! making any changes. 
! -----------------------------------------------------------------
! 1. Hirao, K. and Nakatsuji, H., A generalization of the
!    Davidson's Method to Large Nonsymmetric Eigenvalue
!    Problems, J. Comp. Phys. 45, 246 (1982).
!
! 2. Caricato, M., Trucks, G., and Frisch, M. J., A comparison of
!    Three Variants of the Generalized Davidson Algorithm for the 
!    Partial Diagonalization of Large Non-Hermitian Matrices,
!    J. Chem. Theory Comput. 6, 1966 (2010).
!
module davidson_main
      use math_constants
      use io
      use arithmetic
      use basis
      use linalg
      use blas_olenkifer
      use display
      use journal
      use blas1
      use blas2
      use blas3
      use blas4
      use blas5
      use blas6
      use gparam

      !$ use omp_lib
      
      implicit none
      save
      !
      ! Number of basis vectors stored in DAV_BASIS in current iteration
      !
      integer :: DAV_BASISDIM
      real(F64), dimension(:), allocatable :: diag
      real(F64), dimension(:), private, allocatable :: work
     !
      ! ASMALL  = A^(m)       eq. (4) in [1]
      !
      real(F64), dimension(:,:), allocatable :: asmall, asmall2, d_small
      real(F64), dimension(:,:, :), allocatable :: d_small_omp
      !
      ! C = C^(m)             eq. (7a) in [1]
      !
      real(F64), dimension(:, :), allocatable :: C
      real(F64), dimension(:, :), allocatable :: C_old
      !
      ! Real part of the eigenvalues of ASMALL
      !
      real(F64), dimension(:),   allocatable :: wrsmall
      !
      ! Imaginary part of the eigenvalues of ASMALL
      !
      real(F64), dimension(:),  allocatable :: wismall
      real(F64), dimension(:, :), allocatable :: s_ovr_real
      real(F64), dimension(:, :), allocatable :: s_ovr_cplx

      real(F64), dimension(:), allocatable   :: geev_work
      integer, dimension(:), allocatable     :: geev_iwork
      real(F64), dimension(:), allocatable   :: geev_wr
      real(F64), dimension(:), allocatable   :: geev_wi
      real(F64), dimension(:,:), allocatable :: geev_vr
      real(F64), dimension(:,:), allocatable :: geev_a
      integer, dimension(:), allocatable     :: geev_dy
      real(F64), dimension(:,:), private, allocatable :: work2
      integer, private :: update_offset
      !
      ! Accept both negative and positive eigenvales. If DAV_NEGATIVE_ROOTS==.false.,
      ! then the eigenvectors corresponding to the negative eigenvalues of the matrix
      ! ASMALL will be discarded. Note that in case the true eigenvalues are close to zero,
      ! at some stage of the iterative process the approximate eigenvalues may be negative, 
      ! and then converge to positive values. In this case, setting DAV_NEGATIVE_ROOTS to
      ! .false. would prevent the algorithm from reaching convergence.
      !
      logical :: DAV_NEGATIVE_ROOTS = .false.
      ! --------------------------------------------------------
      ! Convergence control: DAV_QCONVTHRESH and MINRCOND.
      ! If no q_k or \zeta_k vector is appended to the subspace
      ! subset of left vectors, convergence is achieved.
      ! --------------------------------------------------------
      ! Threshold for convergence of the residual q_k vectors
      ! If ||q_k||_2 < QCONVTHRESH, then the residual q_k vector
      ! is rejected. See Eq. 10 in [1].
      !
!      real(F64) :: DAV_QCONVTHRESH = 1.d-7
      !
      ! Threshold for degeneracy of energy level:
      ! if ||E_1 - E_2|| < EPS_DEG, then the eigenvalue
      ! is considered degenerate
      !
      real(F64), parameter :: EPS_DEG = 1.d-6
      !
      ! Maximum number of vectors in left or right
      ! vector subspace of vectors approximating
      ! the exact solution
      !
      integer :: DAV_MAXNVEC
      !
      ! Number of eigenvector pairs sought by Davidson's
      ! algorithm
      !
      integer :: DAV_NSOUGHT
      !
      ! Number of trial vectors used in CI by Davidson. This is
      ! usually much larger than DAV_NSOUGHT
      !
      integer :: DAV_NTRIAL
      !
      ! Order of the matrix A diagonalized by Davidson's
      ! algorithm. 
      !
      integer :: DAV_AORDER
      !
      ! Number of distinct roots searched for
      !
      integer :: DAV_NROOTS
      !
      ! Degeneracies of each root
      !
      integer, dimension(:), allocatable :: DAV_DEGENER
      !
      ! Maximum number of iterations
      !
      integer :: DAV_MAXIT = 100
      !
      ! Total number of iterations performed
      ! to converge both right and left eigenvectors
      !
      integer :: DAV_NITER = 0
      !
      ! Number of iterations performed
      ! to converge left eigenvectors
      !
      integer :: DAV_NITER_LEFT = 0
      !
      ! Convergence info
      ! ---
      ! Continue iterations (new vectors have been appended)
      !
      integer, parameter :: DAV_CONTINUE = 0
      !
      ! Both right and left eigenvectors converged
      !
      integer, parameter :: DAV_CONVERGED = 2
      !
      ! The Davidson algorithm has exceeded the maximum number
      ! of iterations without convergence
      !
      integer, parameter :: DAV_FAILURE = 3

      logical, private :: DAV_RIGHTVECS = .true.
      logical, private :: DAV_SYMMETRIC = .false.
      logical, dimension(:), allocatable :: DAV_ISCONVERGED
      !
      ! The following TJOURNAL instances keep track of which vectors stored
      ! on disk or in-memory. There is constant movement of vectors between
      ! predefined slots located inside memory or disk containers. 
      !
      type(tjournal)                          :: VECBOOK
      type(tjournal), private                          :: SIGBOOK
      type(tjournal), private :: SIGBOOK_CC3
      real(F64), dimension(:, :), allocatable :: VECROWS
      real(F64), dimension(:, :), allocatable :: VECCOLS
      real(F64), dimension(:), allocatable :: vec_test
      !
      ! The intepretation of SIGROWS depends on the current stage
      ! of computation:
      ! 1) SIGROWS = B^T A^T if converging right eigenvectors.
      ! 2) SIGROWS = B^T A if converging left eigenvectors.
      ! The columns of matrix B contain the vectors spanning the
      ! iterative subspace. In both interpratations, the first index
      ! of SIGROWS corresponds to the index of a basis vector.
      ! This memory layout optimizes the memory access when
      ! building SIGROWS.
      !
      real(F64), dimension(:, :), allocatable, private :: SIGROWS
      real(F64), dimension(:, :), allocatable, private :: SIGCOLS
      real(F64), dimension(:, :), allocatable :: SIGROWS_CC3
      real(F64), dimension(:), allocatable, private    :: DISKBUF
      type(trecgroup)                         :: VECRECS
      type(trecgroup), private                         :: SIGRECS
      type(trecgroup), private :: SIGRECS_CC3
      integer, parameter :: CONT_ROWS = 1
      integer, parameter :: CONT_COLS = 2
      integer, parameter :: CONT_DISK = 3
      integer :: NNEW = 0
      !
      ! Prefixes/infixes included in filenames of records kept on disk
      ! ---
      ! Vectors spanning the iterative subspace
      !
      character(*), parameter, private :: VECPREFIX = "dav_vec_"
      !
      ! Rows/columns of SIGMA
      !
      character(*), parameter, private :: SIGPREFIX = "dav_sig_"
      character(*), parameter, private :: SIGPREFIX_CC3 = "dav_sig_cc3_"
      !
      ! Converged left/right eigenvectors
      !
      character(:), allocatable, private :: CONVPREFIX

      

contains

      subroutine dav_init(n_ccsd, n, r3dim_small, nroots, degener, symmetric, rightvecs, negative_roots, &
            guess_coeff, qconvthresh, maxit, memspace, diskspace, preamble, convrecs, &
            prefix, cisd, guess_idx)
            ! ----------------------------------------------------------------------------
            ! Initialize Davidson's algorithm. Allocate matrices and compute the maximum
            ! dimension of the iterative subspace.
            ! ----------------------------------------------------------------------------
            ! 1. Hirao, K. and Nakatsuji, H., A generalization of the Davidson's Method
            !    to Large Nonsymmetric Eigenvalue Problems, J. Comp. Phys. 45, 246 (1982).
            ! ----------------------------------------------------------------------------
            ! N        
            !          The order of the eigensystem.
            !
            ! NROOTS   
            !          Number of target eigenvalues. If a target eigenvector is K-fold
            !          degenerate, it contributes a single root. The information on
            !          multiplicities is conveyed by the DEGENER array.
            !
            ! DEGENER  
            !          I-th element of the DEGENER array is equal to the the degeneracy
            !          of the I-th target eigenvalue. SUM(DEGENER(1:NROOTS)) is the number
            !          of target eigenvectors.
            !
            ! SYMMETRIC
            !          .TRUE.
            !                     Solve Ax = ax, where A is a symmetric matrix.
            !                     Eigenvalues and eigenvectors are computed.
            !          .FALSE.
            !                     Solve Ax = ax, y^TA = ay^T, where A is a nonsymmetric
            !                     matrix. Eigenvalues as well as right and
            !                     left eigenvectors are computed. This algorithm converges
            !                     right eigenvectors first, then the left eigenvectors.
            !                     It is assumed that eigenvectors and eigenvalues are real.
            ! RIGHTVECS
            !           .TRUE.
            !                     Initialize to compute right eigenvectors.
            !           .FALSE.
            !                     Initialize to compute left eigenvectors.
            !
            ! NEGATIVE_ROOTS
            !          Negative eigenvalues and the corresponding eigenvectors 
            !          are discarded if NEGATIVE_ROOTS == .FALSE.
            !
            ! GUESS_COEFF
            !          Coefficients of the guess vectors stored in columns. The guess
            !          vectors need not be perfectly orthogonal on entry to this
            !          subroutine. Gram-Schmidt algorithm is used to orthogonalize the
            !          guess vectors. The number of guess vectors is assumed to be
            !          the number of columns of GUESS_COEFF.
            ! 
            ! QCONVTHRESH
            !          Convergence threshold: maximum residual L2 norm, ||Ax - ax||_2.
            !
            ! MAXIT
            !          Maximum number of iterations. If convergence is not achieved
            !          within MAXIT iterations, DAV_FAILURE will be returned
            !          by DAV_ITER.
            !
            ! MEMSPACE
            !          Maximum amount of volatile memory space for iterative subspace
            !          vectors or SIGMA matrix (in bytes).
            !
            ! DISKSPACE
            !          Maximum amount of disk space for iterative subspace vectors or SIGMA
            !          matrix (in bytes). Setting DISKSPACE to zero prevents creating any
            !          files on disk.
            !
            ! CONVRECS
            !          A TRECGROUP instance used to reference the converged eigenvectors.
            !          Do not pass this argument when reinitializing to compute left
            !          eigenvectors. (See DAV_UNITTEST for an example.)
            !
            ! CISD
            !         If present, Davidson will use more trial vectors than number of requested
            !         vectors.
            !
            ! R3DIM_SMALL 
            !         Size of the reduced R3 vector 
            !
            integer, intent(in)                      :: n_ccsd
            integer, intent(in)                      :: n
            integer, intent(in)                      :: nroots
            integer, dimension(:), intent(in)        :: degener
            logical, intent(in)                      :: symmetric
            logical, intent(in)                      :: rightvecs
            logical, intent(in)                      :: negative_roots
            real(F64), dimension(:, :), intent(in)   :: guess_coeff
            real(F64), intent(in)                    :: qconvthresh
            integer, intent(in)                      :: maxit
            integer(I64), intent(in)                 :: memspace
            integer(I64), intent(in)                 :: diskspace
            logical, intent(in)                      :: preamble
            integer, intent(in)                      :: r3dim_small
            type(trecgroup), optional, intent(inout) :: convrecs
            character(*), optional, intent(in)       :: prefix
            logical, optional, intent(in)            :: cisd
            integer, dimension(:), optional, intent(in) :: guess_idx

            integer :: max_rows, max_cols, max_disk
            integer :: max_rows_mem, max_cols_mem, max_disk_mem
            integer :: max_rows_sig, max_cols_sig
            integer :: i, j
            integer :: lwork, liwork
            integer(I64) :: halfmem, halfdisk, quatmem
            integer :: vec_per_iter
            integer :: guess_dim, guess_nvecs
            integer(I64) :: singlevector
            integer :: upperbound
            integer :: num_threads

            if (present(prefix)) then
                  CONVPREFIX = prefix
            else
                  CONVPREFIX = "dav"
            end if


            guess_dim = size(guess_coeff, dim=1)
            guess_nvecs = size(guess_coeff, dim=2)

            if (r3dim_small == 0)then
                  DAV_AORDER = n
            else
                  DAV_AORDER = n_ccsd + r3dim_small
            end if

            print*, 'R3 jest od ', n_ccsd, 'do', n_ccsd+r3dim_small

            print*, 'DAV_AORDER', DAV_AORDER

            DAV_RIGHTVECS = rightvecs
            DAV_SYMMETRIC = symmetric
            DAV_QCONVTHRSH = qconvthresh
            DAV_NEGATIVE_ROOTS = negative_roots

            DAV_NSOUGHT = sum(degener(1:nroots))
            if (present(cisd)) then
                  DAV_NTRIAL = guess_nvecs
            else
                  DAV_NTRIAL = DAV_NSOUGHT
            end if
            
            DAV_MAXIT = maxit
!            print*, guess_nvecs, DAV_NSOUGHT
            if (guess_nvecs < DAV_NSOUGHT) then
                  call msg("ERROR: NOT ENOUGH GUESS VECTORS PROVIDED", &
                        priority=MSG_ERROR)
                  stop
            end if
            halfmem = memspace / 2
            halfdisk = diskspace / 2
            singlevector = vectorsize(int(DAV_AORDER, I64))
            
            quatmem = memspace / 4

            max_rows_sig = DAV_NTRIAL + 1

            !                                                                                                                         
            ! Subtract 1 from MAX_COLS to account for DISKBUF and DIAG.                                                                     
            !                                                            
            max_cols_sig = int((quatmem - max_rows_sig * singlevector) &
                  / singlevector) - 1
            if (EOM_MEM) then
                  !                                                                                                                  
                  ! Subtract 1 from MAX_COLS to account for DISKBUF and DIAG.                                                                        
                  !                                                            
                  max_rows = int(quatmem / singlevector) - 1
                  max_cols = 0!2
                  print*, 'halfmem', halfmem
                  print*, 'max_rows', max_rows
                  print*, 'single', singlevector
                  print*, 'max', max_cols
                  if (DAV_SYMMETRIC) then
                        halfdisk = halfdisk - (DAV_NTRIAL * singlevector) / 2
                        max_disk = int(halfdisk / singlevector)
                  else
                        halfdisk = halfdisk - DAV_NTRIAL * singlevector
                        max_disk = int((halfdisk / singlevector))
                  end if
            else
                  max_rows = DAV_NTRIAL
                  print*, 'max_rows', max_rows
                  print*, 'single', singlevector
                  max_cols = int((halfmem - max_rows * singlevector) &
                        / singlevector) - 1     
                  print*, 'max', max_cols
                  !
                  ! Max disk usage takes into account that the converged eigenvectors
                  ! are written to disk
                  !
                  if (DAV_SYMMETRIC) then
                        halfdisk = halfdisk - (DAV_NTRIAL * singlevector) / 2
                        max_disk = int(halfdisk / singlevector)
                  else
                        halfdisk = halfdisk - DAV_NTRIAL * singlevector
                        max_disk = int((halfdisk / singlevector)) 
                  end if
            end if

            if (max_cols < 0) then
                  call msg("NOT ENOUGH MEMORY TO ALLOCATE THE REQUIRED NUMBER OF VECTORS", MSG_ERROR)
                  stop
            end if
            vec_per_iter = DAV_NSOUGHT
!            upperbound = min(DAV_MAXIT * vec_per_iter, DAV_AORDER)
            upperbound = min(DAV_NTRIAL + (DAV_MAXIT-1) * vec_per_iter, DAV_AORDER)
            if (EOM_MEM) then
                  if (max_rows + max_cols  > upperbound) then
                        if (max_rows + max_cols > upperbound) then
                              max_rows = upperbound
                              max_cols = 0!2
                        end if
                  end if
                  if (max_rows_sig + max_cols_sig  + max_disk > upperbound) then
                        if (max_rows_sig + max_cols_sig > upperbound) then
                              max_disk = 0
                              max_cols_sig = upperbound - max_rows_sig
                        else
                              max_disk = upperbound - max_rows_sig - max_cols_sig
                        end if
                  end if

            else
                  if (max_rows + max_cols + max_disk > upperbound) then
                        if (max_rows + max_cols > upperbound) then
                              max_disk = 0
                              max_cols = upperbound - max_rows
                        else
                              max_disk = upperbound - max_rows - max_cols
                        end if
                  end if
            end if

            if (EOM_MEM) then
                  DAV_MAXNVEC = max_rows + max_cols
            else
                  DAV_MAXNVEC = max_rows + max_cols + max_disk
            end if

            print*, 'max_rows', max_rows
            print*, 'max_cols', max_cols
            print*, 'max_rows_sig', max_rows_sig
            print*, 'max_cols_sig', max_cols_sig

            !
            ! At most DAV_NSOUGHT new vectors are added during each iteration.
            ! The maximum number of iterations must take into account that
            ! the dimension of the iterative subspace must not exceed the
            ! dimension of the full-size eigensystem.
            !
            if (DAV_MAXIT * vec_per_iter > DAV_MAXNVEC) then
                  DAV_MAXIT = DAV_MAXNVEC / vec_per_iter
            end if

            if (EOM_MEM) then
                  print*, 'max_rows', max_rows
            print*, 'max_cols', max_cols
            print*, 'max_rows_sig', max_rows_sig
            print*, 'max_cols_sig', max_cols_sig
            print*, 'max disk', max_disk
            print*, 'init'
                  call VECBOOK%init([max_rows, max_cols])
                  call SIGBOOK%init([max_rows_sig, max_cols_sig])

            else
                  call VECBOOK%init([max_rows, max_cols, max_disk])
                  call SIGBOOK%init([max_rows, max_cols, max_disk])
            end if

            call io_record_initgroup(VECRECS, diskspace / 2, VECPREFIX)
            call io_record_initgroup(SIGRECS, diskspace / 2, SIGPREFIX)
 !           call io_record_initgroup(SIGRECS_CC3, diskspace / 2, SIGPREFIX_CC3)
            if (present(convrecs))then
                  if (.not. symmetric) then
                        !
                        ! Nonsymmetric eigensystem. Left and right eigenvectors are
                        ! stored on disk upon convergence.
                        !
                        call io_record_initgroup(CONVRECS, singlevector &
                              * DAV_NSOUGHT * 2, CONVPREFIX)
                  else
                        !
                        ! Symmetric matrix. Only right vectors are converged.
                        !
                        call io_record_initgroup(CONVRECS, singlevector &
                              * DAV_NSOUGHT, CONVPREFIX)
                  end if
            end if
            allocate(VECROWS(max_rows, DAV_AORDER))
            print*, 'max', max_rows, DAV_AORDER

            !            allocate(vec_test( DAV_AORDER))
            allocate(VECCOLS(DAV_AORDER, max_cols))
            if (EOM_MEM) then
                  allocate(SIGROWS(max_rows_sig, DAV_AORDER))
                  allocate(SIGCOLS(DAV_AORDER, max_cols_sig))
            else
                  allocate(SIGROWS(max_rows, DAV_AORDER))
                  allocate(SIGCOLS(DAV_AORDER, max_cols))
            end if

            allocate(SIGROWS_CC3(max_rows, DAV_AORDER))
            allocate(DISKBUF(DAV_AORDER))
            SIGROWS_CC3 = zero
            DAV_NROOTS = nroots
!            print*, 'nroots', nroots
            allocate(DAV_DEGENER(DAV_NROOTS))
            DAV_DEGENER(:) = degener(1:DAV_NROOTS)
            !
            ! Copy guess vectors
            !
            do i = 1, DAV_NTRIAL
!                  print*, 'robie lockrow, wektora', i, 'na', i
                        call lockrow(i, i, VECROWS, VECBOOK, .true.)
                        call lockrow(i, i, SIGROWS, SIGBOOK, .true.)
                  !call lockrow(i, i, SIGROWS_CC3, SIGBOOK_CC3, .true.)
                  if (present(guess_idx)) then
                     VECROWS(i, 1:guess_dim) = guess_coeff(:, guess_idx(i))
                  else

                        VECROWS(i, 1:guess_dim) = guess_coeff(1:guess_dim, i)
                     !VECROWS(i, 1:size(VECROWS,dim=2)) = guess_coeff(1:size(VECROWS,dim=2), i)
                  end if
            end do

            NNEW = DAV_NTRIAL!DAV_NSOUGHT
            update_offset = 0
            !
            ! Orthogonalize guess vectors
            !
            call la_gramschmidt_olenkifer(.false., VECROWS(1:DAV_NTRIAL, 1:guess_dim), 0)

            DAV_BASISDIM = DAV_NTRIAL
            allocate(DIAG(DAV_AORDER))
            DIAG = ZERO
            allocate(ASMALL(DAV_MAXNVEC,DAV_MAXNVEC))
            allocate(asmall2(DAV_MAXNVEC, DAV_MAXNVEC))
            allocate(d_small(DAV_MAXNVEC, DAV_MAXNVEC))

            d_small = ZERO
            ASMALL = ZERO
            asmall2 = ZERO
            allocate(C(DAV_MAXNVEC, DAV_MAXNVEC))
            allocate(C_old(DAV_MAXNVEC, DAV_NSOUGHT))
            C_OLD = ZERO
            do i = 1, DAV_NSOUGHT
                  C_OLD(i, i) = ONE
            end do
            allocate(wrsmall(DAV_MAXNVEC))
            wrsmall = zero
            allocate(wismall(DAV_MAXNVEC))
            allocate(s_ovr_real(DAV_MAXNVEC, DAV_MAXNVEC))
            allocate(s_ovr_cplx(DAV_MAXNVEC, DAV_MAXNVEC))
            ! if (DAV_SYMMETRIC) then
            !       call dsyevdquery(DAV_MAXNVEC, lwork, liwork, "V")
            !       allocate(geev_work(lwork))
            !       allocate(geev_iwork(liwork))
            ! else
            !       !
            !       ! Nonsymmetric eigenvalue problem
            !       !
            !       call dgeevquery(DAV_MAXNVEC, lwork, "N", "V")
            !       allocate(geev_work(lwork))
            !       allocate(geev_iwork(0))
            ! end if

            allocate(geev_wr(DAV_MAXNVEC))
            allocate(geev_wi(DAV_MAXNVEC))
            allocate(geev_vr(DAV_MAXNVEC, DAV_MAXNVEC))
            allocate(geev_dy(DAV_MAXNVEC))
            allocate(geev_a(DAV_MAXNVEC, DAV_MAXNVEC))
            !
            ! Matrices for root_homing subroutine
            !
            allocate(work2(DAV_MAXNVEC, max(2, DAV_MAXNVEC)))
            DAV_NITER = 0
            allocate(DAV_ISCONVERGED(DAV_NSOUGHT))
            DAV_ISCONVERGED = .false.
            if (preamble) then
                  call dav_preamble()
            end if
      end subroutine dav_init


      subroutine dav_free()
            deallocate(DAV_DEGENER)
            if (allocated(DIAG)) deallocate(DIAG)
            deallocate(asmall)
            deallocate(asmall2)
            deallocate(d_small)
            deallocate(C)
            deallocate(C_old)
            deallocate(wrsmall)
            deallocate(wismall)
            deallocate(s_ovr_real)
            deallocate(s_ovr_cplx)
!            deallocate(geev_work)
!            deallocate(geev_iwork)
            deallocate(geev_wr)
            deallocate(geev_wi)
            deallocate(geev_vr)
            deallocate(geev_dy)
            deallocate(geev_a)
            deallocate(work2)
 !           deallocate(vec_test)
            deallocate(DAV_ISCONVERGED)
            if (allocated(VECROWS)) deallocate(VECROWS)
            if (allocated(VECCOLS)) deallocate(VECCOLS)
            if (allocated(SIGROWS)) deallocate(SIGROWS)
            if (allocated(SIGCOLS)) deallocate(SIGCOLS)
            if (allocated(DISKBUF)) deallocate(DISKBUF)
            if (allocated(SIGROWS_CC3)) deallocate(SIGROWS_CC3)
            if (allocated(d_small_omp)) deallocate(d_small_omp)
            !
            ! Delete files from disk
            !
            call io_record_deletegroup(VECRECS)
            call io_record_deletegroup(SIGRECS)
!            call io_record_deletegroup(SIGRECS_CC3)
            !
            ! Deallocate journals
            !
            call SIGBOOK%destroy()
            call VECBOOK%destroy()
 !           call SIGBOOK_CC3%destroy()
      end subroutine dav_free


      function vectorsize(n)
            !
            ! Size of a single vector in bytes
            !
            integer(I64) :: vectorsize
            integer(I64), intent(in) :: n
            
            real(F64), dimension(1) :: t

            vectorsize = n * io_size_byte(t)
      end function vectorsize


      subroutine dav_diskusage(convrecs)
            type(trecgroup), optional, intent(in) :: convrecs


            integer(I64) :: bytes_read, bytes_write
            real(F64) :: seconds_read, seconds_write
            real(F64) :: gib_read, gib_write
            character(*), parameter :: df = "ES10.2"

            if (present(convrecs)) then
                  bytes_read = VECRECS%bytes_read + SIGRECS%bytes_read + convrecs%bytes_read
                  bytes_write = VECRECS%bytes_write + SIGRECS%bytes_write + convrecs%bytes_write
                  seconds_read = VECRECS%seconds_read + SIGRECS%seconds_read + convrecs%seconds_read
                  seconds_write = VECRECS%seconds_write + SIGRECS%seconds_write + convrecs%seconds_write
            else
                  bytes_read = VECRECS%bytes_read + SIGRECS%bytes_read
                  bytes_write = VECRECS%bytes_write + SIGRECS%bytes_write
                  seconds_read = VECRECS%seconds_read + SIGRECS%seconds_read
                  seconds_write = VECRECS%seconds_write + SIGRECS%seconds_write
            end if

            call msg("DISK USAGE DURING DIAGONALIZATION", underline=.true.)
            if (bytes_read > 0) then
                  gib_read = frombyte(bytes_read, GIBIBYTE)
                  call dmsg("READ VOLUME [GiB]", gib_read, fmt=df)
                  call dmsg("READ TIME [s]", seconds_read, fmt=df)
                  call dmsg("READ SPEED [GiB/s]", gib_read / seconds_read, fmt=df)
            else
                  call msg("NO DATA READ FROM DISK")
            end if

            if (bytes_write > 0) then
                  gib_write = frombyte(bytes_write, GIBIBYTE)
                  call dmsg("WRITE VOLUME [GiB]", gib_write, fmt=df)
                  call dmsg("WRITE TIME [s]", seconds_write, fmt=df)
                  call dmsg("WRITE SPEED [GiB/s]", gib_write / seconds_write, fmt=df)
            else
                  call msg("NO DATA WRITTEN TO DISK")
            end if
      end subroutine dav_diskusage


      subroutine dav_preamble()
            integer :: nvec_mem, nvec_disk
            integer(I64) :: vecsize_byte
            real(F64) :: diskusage_gib, memusage_gib, vecsize_gib
            !
            ! Size of a single vector
            !
            vecsize_byte = vectorsize(size(VECROWS, dim=2, kind=I64))
            vecsize_gib = frombyte(vecsize_byte, GIBIBYTE)
            !
            ! Total number of vectors stored in memory
            !
            nvec_mem = VECBOOK%containers(CONT_ROWS)%nmax + VECBOOK%containers(CONT_COLS)%nmax &
                  + SIGBOOK%containers(CONT_ROWS)%nmax + SIGBOOK%containers(CONT_COLS)%nmax &
                  + 2 ! DISKBUF + DIAG

            !
            ! Total number of vectors stored on disk
            !
            if (.not. EOM_MEM) then
                  nvec_disk = VECBOOK%containers(CONT_DISK)%nmax + SIGBOOK%containers(CONT_DISK)%nmax
            
                  diskusage_gib = frombyte(vecsize_byte * nvec_disk, GIBIBYTE)
            else
                  diskusage_gib = zero
            end if

            memusage_gib = frombyte(vecsize_byte * nvec_mem, GIBIBYTE)

            call blankline()
            call msg("DAVIDSON MODULE", underline=.true.)
            if (.not. DAV_SYMMETRIC) then
                  call msg("NONSYMMETRIC EIGENSYSTEM")
            else
                  call msg("SYMMETRIC EIGENSYSTEM")
            end if
            call imsg("TARGET EIGENVALUES", DAV_NROOTS)
            call imsg("TARGET EIGENVECTORS", DAV_NSOUGHT)
            call imsg("MAX. SUBSPACE DIMENSION", DAV_MAXNVEC)
            call imsg("EIGENVECTOR DIMENSION", DAV_AORDER)
            call dmsg("MAX. RESIDUAL L2 NORM", DAV_QCONVTHRSH, fmt="ES10.3")
            call imsg("MAX. ITERATIONS", DAV_MAXIT)
            call dmsg("SINGLE VECTOR SIZE [GiB]", vecsize_gib, fmt="ES10.3")
            call dmsg("MEMORY USAGE [GiB]", memusage_gib, fmt="ES10.3")
            call dmsg("DISK USAGE [GiB]", diskusage_gib, fmt="ES10.3")
            call imsg("NUMBER OF VEC STORED", nvec_disk)
      end subroutine dav_preamble


      subroutine dav_convinfo(residual_l, residual_r)
            !
            ! Display L2 norm of each residual vector, 
            ! || q_k ||_2, q_k = (A - \lambda_k) X_k.
            !
            real(F64), dimension(:), intent(in) :: residual_l
            real(F64), dimension(:), intent(in) :: residual_r

            integer :: i
            character(len=DEFLEN) :: line

            call msg("RESIDUAL L2 NORMS", underline=.true.)
            write(line, "(A4,1X,A11,10X,A12)") "#", "LEFT VECTOR", "RIGHT VECTOR"
            call msg(line)
            do i = 1, DAV_NSOUGHT
                  write(line, "(I4,1X,ES11.1,10X,ES12.1)") i, residual_l(i), residual_r(i)
                  call msg(line)
            end do
            call midrule(width=46)
            call blankline()
      end subroutine dav_convinfo


      function dav_dot_product_mem(a_item_id, a_rows, &
            b_item_id, b_rows, b_cols, b_recgroup, b_journ, diskbuf)
            
            real(F64)                              :: dav_dot_product_mem
            integer, intent(in)                    :: a_item_id
            real(F64), dimension(:, :), intent(in) :: a_rows
            integer, intent(in)                    :: b_item_id
            real(F64), dimension(:, :), intent(in) :: b_rows
            real(F64), dimension(:, :), intent(in) :: b_cols
            type(trecgroup), intent(inout)         :: b_recgroup
            type(tjournal), intent(in)             :: b_journ
            real(F64), dimension(:), intent(out)   :: diskbuf

            integer :: pos_b, cont_b
            integer :: info
            real(F64) :: dot_ab
            integer :: i


            call b_journ%read(b_item_id, pos_b, cont_b)
            dot_ab = t1(a_rows(a_item_id, :))

            dav_dot_product_mem = dot_ab
            
          contains 

            function t1(avec)
              real(F64) :: t1
              !
              ! The intent is skipped intentionally
              !
              real(F64), dimension(:) :: avec
              integer :: info
              
              if (cont_b == CONT_ROWS) then
                 call la_dot_olenkifer(t1, avec, b_rows(pos_b, :))
              else if (cont_b == CONT_COLS) then
                 call la_dot_olenkifer(t1, avec, b_cols(:, pos_b))
              else
                 call io_record_read(b_recgroup, b_item_id, diskbuf, info)
                 call la_dot_olenkifer(t1, avec, diskbuf)
              end if
            end function t1

          end function dav_dot_product_mem


          function dav_dot_product(a_item_id, a_rows, a_cols, a_recgroup, a_journ, &
               b_item_id, b_rows, b_cols, b_recgroup, b_journ, diskbuf)
            
            real(F64)                              :: dav_dot_product
            integer, intent(in)                    :: a_item_id
            real(F64), dimension(:, :), intent(in) :: a_rows
            real(F64), dimension(:, :), intent(in) :: a_cols
            type(trecgroup), intent(inout)         :: a_recgroup
            type(tjournal), intent(in)             :: a_journ
            integer, intent(in)                    :: b_item_id
            real(F64), dimension(:, :), intent(in) :: b_rows
            real(F64), dimension(:, :), intent(in) :: b_cols
            type(trecgroup), intent(inout)         :: b_recgroup
            type(tjournal), intent(in)             :: b_journ
            real(F64), dimension(:), intent(out)   :: diskbuf

            integer :: pos_a, cont_a, pos_b, cont_b
            integer :: info
            real(F64) :: dot_ab
            integer :: i
            call a_journ%read(a_item_id, pos_a, cont_a)
            call b_journ%read(b_item_id, pos_b, cont_b)
            if (cont_a == CONT_DISK .and. cont_b == CONT_DISK) then
               call msg("CANNOT COMPUTE DOT PRODUCT OF TWO VECTORS STORED ON DISK", MSG_ERROR)
               stop
            end if
            if (cont_a == CONT_ROWS) then
               dot_ab = t1(a_rows(pos_a, :))
            else if (cont_a == CONT_COLS) then
               dot_ab = t1(a_cols(:, pos_a))
            else
               call io_record_read(a_recgroup, a_item_id, diskbuf, info)
               dot_ab = t1(diskbuf)
            end if
            dav_dot_product = dot_ab

          contains 

            function t1(avec)
              real(F64) :: t1
              !
              ! The intent is skipped intentionally
              !
              real(F64), dimension(:) :: avec
              integer :: info

              if (cont_b == CONT_ROWS) then
                 call la_dot_olenkifer(t1, avec, b_rows(pos_b, :))
              else if (cont_b == CONT_COLS) then
                 call la_dot_olenkifer(t1, avec, b_cols(:, pos_b))
              else
                 call io_record_read(b_recgroup, b_item_id, diskbuf, info)
                 call la_dot_olenkifer(t1, avec, diskbuf)
              end if
            end function t1

          end function dav_dot_product


      subroutine unlockrow(j, rows, cols, recgroup, journ)
            integer, intent(in)                       :: j
            real(F64), dimension(:, :), intent(inout) :: rows
            real(F64), dimension(:, :), intent(inout) :: cols
            type(trecgroup), intent(inout)            :: recgroup
            type(tjournal), intent(inout)             :: journ
            
            integer :: target_pos, target_cont, item_id, info
            if (.not. journ%isfree(j, CONT_ROWS)) then
                  if (journ%nfree() > 0) then
                        call journ%move(j, CONT_ROWS, target_pos, target_cont)                        
                        if (target_cont == CONT_ROWS) then
                              rows(target_pos, :) = rows(j, :)
                        else if (target_cont == CONT_COLS) then
                              cols(:, target_pos) = rows(j, :)
                        else if (target_cont == CONT_DISK) then
                              item_id = journ%containers(CONT_DISK)%items(target_pos)
                              call io_record_write(recgroup, item_id, rows(j, :), info)
                        end if
                  else
                        call msg("NOT ENOUGH SPACE TO STORE ANOTHER VECTOR", MSG_ERROR)
                        stop
                  end if
            end if
      end subroutine unlockrow

      subroutine unlockrow_mem(j, rows, cols, recgroup, journ)
            integer, intent(in)                       :: j
            real(F64), dimension(:, :), intent(inout) :: rows
            real(F64), dimension(:, :), intent(inout) :: cols
            type(trecgroup), intent(inout)            :: recgroup
            type(tjournal), intent(inout)             :: journ

            integer :: target_pos, target_cont, item_id, info
            rows(target_pos, :) = rows(j, :)
      end subroutine unlockrow_mem

      
      subroutine lockrow(item_id, pos, rows, journ, zeroinit)
            integer, intent(in)                       :: item_id
            integer, intent(in)                       :: pos
            real(F64), dimension(:, :), intent(inout) :: rows
            type(tjournal), intent(inout)             :: journ
            logical, intent(in)                       :: zeroinit
            
            integer :: target_pos
            if (journ%isfree(pos, CONT_ROWS)) then
               target_pos = journ%write(item_id, CONT_ROWS)
                  if (target_pos .ne. pos) then
                        call msg("TJOURNAL%WRITE RETURNED AN UNEXPECTED RESULT", MSG_ERROR)
                        stop
                  end if
                  if (zeroinit) then
                        rows(pos, :) = ZERO
                  end if
            else
                  call msg("ATTEMPTING WRITE INTO A LOCKED POSITION", MSG_ERROR)
                  stop
            end if
      end subroutine lockrow

      subroutine lockrow_mem(item_id, pos, rows, journ, zeroinit)
            integer, intent(in)                       :: item_id
            integer, intent(in)                       :: pos
            real(F64), dimension(:, :), intent(inout) :: rows
            type(tjournal), intent(inout)             :: journ
            logical, intent(in)                       :: zeroinit

            integer :: target_pos

            target_pos = journ%write(item_id, CONT_ROWS)
!            print*, 'target pos', target_pos
            ! if (target_pos .ne. pos) then
            !       call msg("TJOURNAL%WRITE RETURNED AN UNEXPECTED RESULT", MSG_ERROR)
            !       stop
            ! end if
            
            ! if (zeroinit) then
            !       rows(pos, :) = ZERO
            ! end if
      end subroutine lockrow_mem
      
      subroutine expand_basis(j, l, m, c, best_residual, improves, converged, target_eval, j_mem)
            integer, intent(inout)                 :: j
            integer, intent(in)                    :: l
            integer, intent(in)                    :: m
            real(F64), dimension(:, :), intent(in) :: c
            real(F64), intent(inout)               :: best_residual
            logical, intent(out)                   :: improves
            logical, intent(out)                   :: converged
            real(F64), intent(in)                  :: target_eval
            integer, intent(inout) :: j_mem
            
            real(F64) :: residual
            integer :: offset

!            print*, ''
!            print*, 'expand basis: dodaje wektor na miejsce', j_mem
!            print*, ''
            if (EOM_MEM) then
                  call add_residual(j_mem, l, m, VECROWS, VECCOLS, VECRECS, VECBOOK, c, &
                        SIGROWS, SIGCOLS, SIGROWS_CC3, SIGRECS, SIGBOOK, target_eval, &
                        DIAG, residual, DAV_QCONVTHRSH)
            else
                  call add_residual(j, l, m, VECROWS, VECCOLS, VECRECS, VECBOOK, c, &
                        SIGROWS, SIGCOLS, SIGROWS_CC3, SIGRECS, SIGBOOK, target_eval, &
                        DIAG, residual, DAV_QCONVTHRSH)
            end if
!            print*, '~~residual~~', residual, best_residual, DAV_QCONVTHRSH
            if (residual >= DAV_QCONVTHRSH) then
                  converged = .false.
                  if (best_residual > residual) then
                        improves = .true.
                        best_residual = residual
                  else
!                        print*, best_residual, residual
                        improves = .false.
                  end if

                  if (EOM_MEM.eqv..false.)then
                        call lockrow(m+j, j, VECROWS, VECBOOK, .false.)
                  end if
                  
                  call lockrow(m+j, j, SIGROWS, SIGBOOK, .true.)       

                  j = j + 1
                  j_mem = j_mem + 1
                  
            else
                  best_residual = residual
                  improves = .true.
                  converged = .true.

            end if
!            print*, ''
!            print*, ' po expand basis j jest rowne:', j
!            print*, ''
      end subroutine expand_basis


      subroutine dav_iter(wr, wi, residual_l2, e_diff, convrecs, info, cisd, e_total, restart, d_small)
            ! -----------------------------------------------------------------
            ! 1. Hirao, K. and Nakatsuji, H., A generalization of the
            !    Davidson's Method to Large Nonsymmetric Eigenvalue
            !    Problems, J. Comp. Phys. 45, 246 (1982).
            ! -----------------------------------------------------------------
            ! 2. "A comparison of three variants of the generalized davidson 
            !    algorithm..." Caricato, M., Trucks, W. Frish, J.
            ! -----------------------------------------------------------------
            !
            ! WR
            !                 Real parts of the eigenvalues
            ! WR
            !                 Im. parts of the eigenvalues
            !
            ! RESIDUAL_L2
            !                 || q_k ||_2 norms of the residual vectors,
            ! E_DIFF          
            !                 Energy change corresponding to the K-th
            !                 eigenvector
            ! CONVRECS
            !                 A TRECGROUP instance by which the converged
            !                 eigenvectors can be referenced.
            ! 
            ! INFO            Convergence info. One of the following 
            !                 is returned:
            !                 DAV_CONTINUE
            !                 DAV_CONVERGED
            !                 DAV_FAILURE
            ! -----------------------------------------------------------------
            ! 1. Create  matrix asmall, from B_bar and B vector,
            ! and Jacobian matrix - eq. (4) in [1]
            !
            ! 2. Diagonalize asmall. eq. (7a) and (7b) in [1]
            !
            ! 3. Enter root_homing procedure with lsmall, C,
            ! vl, vr, B_bar, B.
            !
            ! 4. Find number of complex eigenvalues
            !
            ! 5. Compute residual vectors eq. (9) and (10) in [1]
            !    and add them to B and B_bar
            !
            ! 6. Biorthonormalize added vectors
            !
            real(F64), dimension(:), intent(inout)   :: wr
            real(F64), dimension(:), intent(inout)   :: wi
            real(F64), dimension(:), intent(inout)   :: residual_l2
            real(F64), dimension(:), intent(inout)   :: e_diff
            type(trecgroup), intent(inout)           :: convrecs
            type(trecgroup) :: convrecs_temp
            integer, intent(out)                     :: info
            logical, optional, intent(in)            :: cisd
            real(F64), optional, intent(in)          :: e_total
            logical, optional, intent(in)            :: restart
            real(F64), dimension(:,:), optional, intent(in) :: d_small
            integer :: nocc, nvirt, npair, nidx
            real(F64), dimension(:,:), allocatable :: lvec_k1
           

            logical :: improves
            real(F64) :: target_eval
            integer :: i, j, l, j_mem, lll
            integer :: m, m_new
            integer :: offset

            !
            ! Dimension of the iterative subspace
            ! at the beginning of this iteration                                                                                
            !

            m = DAV_BASISDIM

            if (DAV_NITER == 0) then
                  residual_l2 = huge(ONE)
            end if            
            !
            ! Create  ASMALL (the Jacobian matrix transformed
            ! by the trial vectors - eq. (4) in [1])
            ! ---
            ! ASMALL(1:M, 1:M) := B^T * A * B <- DAV_BASIS(1:M, :) DAV_SIGMA(1:M, :)^T
            !
            !call gemmwrap("N", "T", m, m, DAV_AORDER, ONE, DAV_BASIS, DAV_SIGMA, ZERO, ASMALL)

!            print*, 'uzupelniam asmall', m-NNEW+1, m, NNEW
            
           !  do i = 1, 331
           !        if (abs(SIGROWS(1, i)).gt.1.d-2)then
           !              print*, 'sigsig', i, SIGROWS(1, i)
           !        end if

           !  end do

           ! do i = 1, 331
           !        if (abs(VECROWS(1, i)).gt.1.d-8)then
           !              print*, 'vecvec', i, VECROWS(1, i)
           !        end if

           !  end do


            do i = m-NNEW+1, m
               do j = 1, i
                  if (EOM_MEM) then
                     asmall(i, j) = dav_dot_product_mem(i, VECROWS, &
                          j, SIGROWS, SIGCOLS, SIGRECS, SIGBOOK, DISKBUF)
                     if (i .ne. j) then
                        asmall(j, i) = dav_dot_product_mem(j, VECROWS, &
                             i, SIGROWS, SIGCOLS, SIGRECS, SIGBOOK, DISKBUF)
                     end if
                  else
                     asmall(i, j) = dav_dot_product(i, VECROWS, VECCOLS, VECRECS, VECBOOK, &
                          j, SIGROWS, SIGCOLS, SIGRECS, SIGBOOK, DISKBUF)
                     if (i .ne. j) then
                        asmall(j, i) = dav_dot_product(j, VECROWS, VECCOLS, VECRECS, VECBOOK, &
                             i, SIGROWS, SIGCOLS, SIGRECS, SIGBOOK, DISKBUF)
                     end if
!                     print*, i, j, 'asmall', asmall(i, j), asmall(j,i)
                  end if
                  end do
               end do
            !
            ! Diagonalize asmall. In result: m left and right
            ! eigenvectors (C_bar, C). eq. (7a) and (7b) in [1]
            !

            if (EOM_MEM .eqv. .true.) then
                  asmall2 = asmall - d_small
                  call dav_densesolver(asmall2, m, C, wrsmall, wismall)
            else
                  call dav_densesolver(asmall, m, C, wrsmall, wismall)
            end if

            print*, 'przed root homing'
            do i = 1, 2 * DAV_NSOUGHT
                  print*, 'wrsmall', i, wrsmall(i)
            end do
            !
            ! identify approximate eigenvectors in the current iteration
            ! with the root homing
            
            ! if (present(cisd))then
            !       if (cisd .eqv. .true.) then
            !             call cisd_sorting(m, C, wrsmall, wismall, e_total)
            !             !DAV_ISCONVERGED = .true.
            !             !j = 0
            !       else
            !              call root_homing(m, C, wrsmall, wismall, DAV_DEGENER)
            !       end if
            ! else
            call root_homing(m, C, wrsmall, wismall, DAV_DEGENER)
            ! end if
            print*, 'po root homing - bez root homing'
            do i = 1, DAV_NSOUGHT
                  print*, 'wrsmall', i, wrsmall(i) 
            end do

            
            !
            ! Compute residual vectors eq. (9) and (10) in [1]
            ! In case of complex eigenvalues, a procedure described in [2] 
            ! below eq. (9) is incorporated.
            ! ---
            ! J counts the number of residual vectors
            ! accepted into the updated set of trial vectors
            !
            if (EOM_MEM) then
                  offset = m - NNEW + 1
            else
                  offset = 0
            end if

            j = 1
            j_mem = m + 1
            residual_vecs: do l = 1, DAV_NSOUGHT
                  !
                  ! This flag will be set to .TRUE. only if the new candiate for
                  ! the L-th eigenvector improves on the residual norm, ||Ax-\lambda x||
                  ! or this is the first iteration.
                  !
                  improves = .false.

                  if (.not. DAV_ISCONVERGED(l)) then
                        !
                        ! Make room for the next ZETA_K vector. If some data
                        ! occupies J-th row, it is moved to the colmn-wise
                        ! arrays or stored on disk.
                        !
                        call unlockrow(j, VECROWS, VECCOLS, VECRECS, VECBOOK)
                        call unlockrow(j, SIGROWS, SIGCOLS, SIGRECS, SIGBOOK)
                        target_eval = wrsmall(l)
                        call expand_basis(j, l, m, c, residual_l2(l), improves, &
                              DAV_ISCONVERGED(l), target_eval, j_mem)
!                        print*, 'improves?', improves, j
!                        if (present(restart)) then
!                              print*, 'teraz robie restart'
!                              improves = .true.
!                        end if
                        if (improves) then
                              !
                              ! A candidate for the target eigenvector has decreased
                              ! the residual, ||Ax-\lambda x||. It is now accepted
                              ! as an improved approximation to the exact eigenvector.
                              !
                              C_OLD(:, l) = C(:, l)

                              e_diff(l) = abs(wr(l)-wrsmall(l))
                              wr(l) = wrsmall(l)
                              wi(l) = wismall(l)
                        end if
                  end if


            end do residual_vecs
 

            j = j - 1
            ! print*, ''
            ! print*, 'now J is equal to the number of appended residual vecotrs, j, m', j, m
            ! print*, ''
            !
            ! Now J is equal to the number of appended residual vectors.
            !
            if (j > 0) then
                  !
                  ! Orthogonalize scaled residual vectors with respect
                  ! to the old vectors and with respect to themselves.
                  ! We have observed that this orthogonalization is
                  ! required for numerical robustness near the convergence.
                  ! It is also advised by Caricato et al. [2].
                  !
                  if (EOM_MEM) then
                        call dav_gramschmidt_mem(VECROWS, m+j, m)
                        m_new = m + j
                        NNEW = j
                        update_offset = m
                  else
                        call dav_gramschmidt(VECROWS, VECCOLS, VECRECS, VECBOOK, m+j, m)
                        m_new = m + j
                        NNEW = j
                  end if
            else
                  m_new = m
                  NNEW = 0
            end if
!            print*, 'NNEW', NNEW
            
            if (j == 0) then
                  info = DAV_CONVERGED
                  if (DAV_RIGHTVECS) then
                     if (EOM_MEM) then
                        call saveconverged_mem(.true., CONVRECS, m, DAV_NSOUGHT, VECROWS, &
                             C_OLD(1:m, :), DISKBUF)
                     else
!                           print*, 'POKEPLUSZ', DAV_AORDER
                           !do lll = 1, size(VECROWS, 2)
                           !       if (abs(VECROWS(1, lll)).gt.1.d-2)then
                           !             print*, 'VECVEC1', lll, VECROWS(1, lll)
                           !       end if
                           !       if (abs(VECROWS(2, lll)).gt.1.d-2)then
                           !             print*, 'VECVEC2', lll, VECROWS(2, lll)
                           !       end if
                           ! end do
!                           print*, 'VECROWS', VECROWS(2, 1), VECROWS(2, 2)

                        call saveconverged(.true., CONVRECS, m, DAV_NSOUGHT, VECROWS, VECCOLS, &
                             VECRECS, VECBOOK, C_OLD(1:m, :), DISKBUF)

                     end if
                  else
                        
!                        print*, 'LEWY POKEPLUSZ',DAV_AORDER
                           ! do lll = 1, size(VECROWS, 2)
                           !       if (abs(VECROWS(1, lll)).gt.1.d-2)then
                           !             print*, 'VECVEC1', lll, VECROWS(1, lll)
                           !       end if
                           !       if (abs(VECROWS(2, lll)).gt.1.d-2)then
                           !             print*, 'VECVEC2', lll, VECROWS(2, lll)
                           !       end if
                           ! end do
!                           print*, 'VECROWS', VECROWS(2, 1), VECROWS(2, 2)
                        call saveconverged(.false., CONVRECS, m, DAV_NSOUGHT, VECROWS, VECCOLS, &
                              VECRECS, VECBOOK, C_OLD(1:m, :), DISKBUF)                        
                  end if

                  m_new = DAV_NSOUGHT
                  DAV_BASISDIM = DAV_NSOUGHT
            else
                  info = DAV_CONTINUE
                  DAV_BASISDIM = m_new
            end if

            !
            ! Test if number of iterations exceeds the allowed value
            !
            DAV_NITER = DAV_NITER + 1
            if (DAV_NITER > DAV_MAXIT .and. info .ne. DAV_CONVERGED) then
                  info = DAV_FAILURE
            end if

            if (info .ne. DAV_CONVERGED) then
                  DIAG = ZERO
            end if

          end subroutine dav_iter


          subroutine dav_gramschmidt_mem(rows, m, k)
            ! -------------------------------------------------------
            ! Gram-Schmidt orthonormalization
            ! -------------------------------------------------------
            ! M
            !     Total number of vectors
            ! K  
            !     Number of vectors which are orthonormal on entry
            !     to this subroutine
            !
            real(F64), dimension(:, :), intent(inout) :: rows
            integer, intent(in)                       :: m
            integer, intent(in)                       :: k

            integer :: l
            integer :: i, j
            real(F64) :: t, l2norm

            l = m - k
            ! print*, 'dav_gram'
            ! print*, 'total vectors', m
            ! print*, 'n of vec which are orthonormal', k
            ! print*, 'n for orthonormalization', l
            if (l == 0) return
            !
            ! Normalize each vector which is not yet normalized
            !
            do i = k + 1, k + l
!                  print*, 'petla po i', i, k+1, k+l
                  call la_l2norm_olenkifer(l2norm, rows(i, :))
                  call la_scal_olenkifer(rows(i, :), ONE/l2norm)
            end do

            !
            ! Project out non-orthogonal components from each
            ! of the recently appended coulumns
            !
            do i = k + 1, k + l
                  do j = 1, i - 1
                     call la_dot_olenkifer(t, rows(j, :), rows(i, :))
                     call la_axpy_olenkifer(rows(i, :), -t, rows(j, :))
                  end do
                  call la_l2norm_olenkifer(l2norm, rows(i, :))
                  call la_scal_olenkifer(rows(i, :), ONE/l2norm)
!                  print*, 'L2NORM', l2norm
            end do
          end subroutine dav_gramschmidt_mem

      
      
      subroutine dav_gramschmidt(rows, cols, recgroup, journ, m, k)
            ! -------------------------------------------------------
            ! Gram-Schmidt orthonormalization
            ! -------------------------------------------------------
            ! M
            !     Total number of vectors
            ! K  
            !     Number of vectors which are orthonormal on entry
            !     to this subroutine
            !
            real(F64), dimension(:, :), intent(inout) :: rows
            real(F64), dimension(:, :), intent(in)    :: cols
            type(trecgroup), intent(inout)            :: recgroup
            type(tjournal), intent(in)                :: journ
            integer, intent(in)                       :: m
            integer, intent(in)                       :: k

            integer :: l
            integer :: i, j
            real(F64) :: t, l2norm
            integer :: pos_i, cont_i
            integer :: pos_j, cont_j
            integer :: info

            l = m - k
            ! print*, 'dav_gram'
            ! print*, 'total vectors', m
            ! print*, 'n of vec which are orthonormal', k
            ! print*, 'n for orthonormalization', l
            if (l == 0) return
            !
            ! Normalize each vector which is not yet normalized
            !
            do i = k + 1, k + l
!                  print*, 'petla po i', i, k+1, k+l
                  call journ%read(i, pos_i, cont_i)
!                  print*, 'cont_i', cont_i
                  if (cont_i .ne. CONT_ROWS) then
                        call msg("EXPECTED VECTORS STORED IN ROWS", MSG_ERROR)
                        call imsg("CONT_I", cont_i, MSG_ERROR)
                        stop
                  end if
!                  print*, 'sw1', rows(1, 6)
                  call la_l2norm_olenkifer(l2norm, rows(pos_i, :))
!                  print*, 'sw2', rows(1, 6), l2norm
                  call la_scal_olenkifer(rows(pos_i, :), ONE/l2norm)
!                  print*, 'sw3', rows(1, 6)
            end do

            !
            ! Project out non-orthogonal components from each
            ! of the recently appended coulumns
            !
            do i = k + 1, k + l

                  call journ%read(i, pos_i, cont_i)
                  
                  do j = 1, i - 1
                        call journ%read(j, pos_j, cont_j)
                        
                        if (cont_j == CONT_ROWS) then
                              call la_dot_olenkifer(t, rows(pos_j, :), rows(pos_i, :))
                              call la_axpy_olenkifer(rows(pos_i, :), -t, rows(pos_j, :))
                        else if (cont_j == CONT_COLS) then
                              call la_dot_olenkifer(t, cols(:, pos_j), rows(pos_i, :))
                              call la_axpy_olenkifer(rows(pos_i, :), -t, cols(:, pos_j))
                        else
                              !
                              ! Disk
                              !
                              call io_record_read(recgroup, j, diskbuf, info) 
                              call la_dot_olenkifer(t, diskbuf, rows(pos_i, :))
                              call la_axpy_olenkifer(rows(pos_i, :), -t, diskbuf)
                        end if
                  end do
                  call la_l2norm_olenkifer(l2norm, rows(pos_i, :))
                  call la_scal_olenkifer(rows(pos_i, :), ONE/l2norm)
!                  print*, 'L2NORM', l2norm
            end do
      end subroutine dav_gramschmidt


      subroutine saveconverged(rightvecs, conv_recgroup, m, nconv, rows, cols, &
            basis_recgroup, journ, c, diskbuf)
            logical, intent(in)                       :: rightvecs
            type(trecgroup), intent(inout)            :: conv_recgroup
            integer, intent(in)                       :: m
            integer, intent(in)                       :: nconv
            real(F64), dimension(:, :), intent(in)    :: rows
            real(F64), dimension(:, :), intent(in)    :: cols
            type(trecgroup), intent(inout)            :: basis_recgroup
            type(tjournal), intent(in)                :: journ
            real(F64), dimension(:, :), intent(in)    :: c
            real(F64), dimension(:), intent(out)      :: diskbuf
            
            integer :: k, w
            integer :: info
            real(F64) :: alpha
            integer :: pos, cont
            
            do k = 1, nconv
                  diskbuf = ZERO
                  do w = 1, m
                        alpha = c(w, k)
                        call journ%read(w, pos, cont)
                        if (cont == CONT_ROWS) then
                              call la_axpy_olenkifer(diskbuf, alpha, rows(pos, :))
                        else if (cont == CONT_COLS) then
                              call la_axpy_olenkifer(diskbuf, alpha, cols(:, pos))
                        else
                              call io_record_axpy(basis_recgroup, w, diskbuf, alpha, info)
                        end if
                  end do
                  

!                  print*, 'gownodisk', diskbuf(2)
                  if (rightvecs) then
                        call io_record_write(conv_recgroup, 2 * k - 1, diskbuf, info)
                  else
                        call io_record_write(conv_recgroup, 2 * k, diskbuf, info)
                  end if
            end do
      end subroutine saveconverged


      subroutine saveconverged_mem(rightvecs, conv_recgroup, m, nconv, rows, &
             c, diskbuf)
            logical, intent(in)                       :: rightvecs
            type(trecgroup), intent(inout)            :: conv_recgroup
            integer, intent(in)                       :: m
            integer, intent(in)                       :: nconv
            real(F64), dimension(:, :), intent(in)    :: rows
            real(F64), dimension(:, :), intent(in)    :: c
            real(F64), dimension(:), intent(out)      :: diskbuf
            
            integer :: k, w
            integer :: info
            real(F64) :: alpha
            
            do k = 1, nconv
                  diskbuf = ZERO
                  do w = 1, m
                        alpha = c(w, k)
                        call la_axpy_olenkifer(diskbuf, alpha, rows(k, :))
                  end do
                  
                  if (rightvecs) then
                        call io_record_write(conv_recgroup, 2 * k - 1, diskbuf, info)
                  else
                        call io_record_write(conv_recgroup, 2 * k, diskbuf, info)
                  end if
            end do
          end subroutine saveconverged_mem

      subroutine assembly_converged(rightvecs, conv_recgroup, m, nconv, rows, cols, &
               basis_recgroup, journ, c, diskbuf)
            logical, intent(in)                       :: rightvecs
            type(trecgroup), intent(inout)            :: conv_recgroup
            integer, intent(in)                       :: m
            integer, intent(in)                       :: nconv
            real(F64), dimension(:, :), intent(in)    :: rows
            real(F64), dimension(:, :), intent(in)    :: cols
            type(trecgroup), intent(inout)            :: basis_recgroup
            type(tjournal), intent(in)                :: journ
            real(F64), dimension(:, :), intent(in)    :: c
            real(F64), dimension(:), intent(out)      :: diskbuf

            integer :: k, w
            integer :: info
            real(F64) :: alpha
            integer :: pos, cont

            do k = 1, nconv
                  diskbuf = ZERO
                  do w = 1, m
                        alpha = c(w, k)
                        call journ%read(w, pos, cont)
                        if (cont == CONT_ROWS) then
                              call la_axpy_olenkifer(diskbuf, alpha, rows(pos, :))
                        else if (cont == CONT_COLS) then
                              call la_axpy_olenkifer(diskbuf, alpha, cols(:, pos))
                              else
                              call io_record_axpy(basis_recgroup, w, diskbuf, alpha, info)
                        end if
                  end do

                  if (rightvecs) then
                        call io_record_write(conv_recgroup, 2 * k - 1, diskbuf, info)
                  else
                        call io_record_write(conv_recgroup, 2 * k, diskbuf, info)
                  end if
            end do
      end subroutine assembly_converged


      

      subroutine readconverged_nonsymmetric(recgroup, colwise, nconv, lvecs, rvecs)
            !
            ! Read converged left and right eigenvectors stored on disk
            ! (nonsymmetric eigensystem).
            !
            type(trecgroup), intent(inout)          :: recgroup
            logical, intent(in)                     :: colwise
            integer, intent(in)                     :: nconv
            real(F64), dimension(:, :), intent(out) :: lvecs
            real(F64), dimension(:, :), intent(out) :: rvecs

            integer :: w, k
            integer :: info

            do k = 1, nconv
                  !
                  ! Right eigenvector
                  !
                  w = 2 * k - 1
                  if (colwise) then
                        call io_record_read(recgroup, w, rvecs(:, k), info)
                  else
                        call io_record_read(recgroup, w, rvecs(k, :), info)
                  end if
                  !
                  ! Left eigenvector
                  !
                  w = 2 * k
                  if (colwise) then
                        call io_record_read(recgroup, w, lvecs(:, k), info)
                  else
                        call io_record_read(recgroup, w, lvecs(k, :), info)
                  end if
            end do
            !
            ! Left and right vectors have to be biorthonormalized to prevent
            ! accumulation of numerical error
            !
          !  call la_biorth_olenkifer(colwise, lvecs, rvecs, 0)

            call la_biorth_olenkifer(colwise, lvecs, rvecs, 0)

      end subroutine readconverged_nonsymmetric

      subroutine readconverged_nonsymmetric_right(recgroup, colwise, nconv, rvecs)
            !                                                                                                                                                         
            ! Read converged left and right eigenvectors stored on disk
            ! (nonsymmetric eigensystem).    
            !                                                                                                                                                     
            type(trecgroup), intent(inout)          :: recgroup
            logical, intent(in)                     :: colwise
            integer, intent(in)                     :: nconv
            real(F64), dimension(:, :), intent(out) :: rvecs

            integer :: w, k
            integer :: info

            do k = 1, nconv
                  !                                                                                                                                       
                  ! Right eigenvector                                                                                                      
                  !                                                                                                                                                               
                  w = 2 * k - 1
                  if (colwise) then
                        call io_record_read(recgroup, w, rvecs(:, k), info)
                  else
                        call io_record_read(recgroup, w, rvecs(k, :), info)
                  end if
            end do

      end subroutine readconverged_nonsymmetric_right

      subroutine readconverged_nonsymmetric_single(recgroup, k, lvecs, rvecs, file)
            !                                                                                                 
            ! Read converged left and right eigenvectors stored on disk                                                                           
            ! (nonsymmetric eigensystem).                                                                                                                       
            !                                                                         
            type(trecgroup), intent(inout)          :: recgroup
            integer, intent(in)                     :: k
            real(F64), dimension(:), intent(out) :: lvecs
            real(F64), dimension(:), intent(out) :: rvecs
            character(*), optional, intent(in)   :: file

            integer :: w, i, n
            integer :: info
            real(F64) :: norm

            !                                                                                                          
            ! Right eigenvector                                                                                                                   
            !               
            w = 2 * k - 1
            if (present(file))then
                  call io_record_read(recgroup, w, rvecs, info, file)
            else
                  call io_record_read(recgroup, w, rvecs, info)
            end if

            !                                                                                                   
            ! Left eigenvector                                                                                                                            
            !
            w = 2 * k
            if (present(file)) then
                  call io_record_read(recgroup, w, lvecs, info, file)
            else
                  call io_record_read(recgroup, w, lvecs, info)
            end if
            !                                                
            ! Left and right vectors have to be biorthonormalized to prevent                                                      
            ! accumulation of numerical error                                                                                                                 
            ! ! 
            !call la_biorth_olenkifer(.false., lvecs, rvecs, 0)

            n = size(lvecs)

            norm = zero
            do i = 1, n
                  norm = norm + lvecs(i) * rvecs(i)
            end do

!            print*, 'lvecs(2), rvecs(2)', lvecs(2), rvecs(2)

!            print*, 'NORM', norm
            lvecs = lvecs / sqrt(abs(norm))

            rvecs = rvecs / sqrt(abs(norm))

            norm = zero
            do i = 1, n
                  norm = norm + lvecs(i) * rvecs(i)
            end do

!            print*, 'NORM2', norm


      end subroutine readconverged_nonsymmetric_single

      subroutine readconverged_nonsymmetric_single_right(recgroup, k, rvecs, file)
            type(trecgroup), intent(inout)          :: recgroup
            integer, intent(in)                     :: k
            real(F64), dimension(:), intent(out) :: rvecs
            character(*), optional, intent(in)   :: file

            integer :: w, i, n
            integer :: info
            real(F64) :: norm
            w = 2 * k - 1
            if (present(file))then
                  call io_record_read(recgroup, w, rvecs, info, file)
            else
!                  print* ,'wwwww', w
                  call io_record_read(recgroup, w, rvecs, info)
            end if

            n = size(rvecs)
            norm = zero
            do i = 1, n
                  norm = norm + rvecs(i) * rvecs(i)
            end do
            rvecs = rvecs / sqrt(abs(norm))


      end subroutine readconverged_nonsymmetric_single_right



      subroutine readconverged_symmetric(recgroup, colwise, nconv, vecs)
            !
            ! Read converged eigenvectors stored on disk (symmetric eigensystem).
            !
            type(trecgroup), intent(inout)          :: recgroup
            logical, intent(in)                     :: colwise
            integer, intent(in)                     :: nconv
            real(F64), dimension(:, :), intent(out) :: vecs

            integer :: w, k
            integer :: info

            do k = 1, nconv
                  !
                  ! Right eigenvector
                  !
                  w = 2 * k - 1
                  if (colwise) then
                        call io_record_read(recgroup, w, vecs(:, k), info)
                  else
                        call io_record_read(recgroup, w, vecs(k, :), info)
                  end if
            end do
            !
            ! Left and right vectors have to be biorthonormalized to prevent
            ! accumulation of numerical error
            !
            call la_gramschmidt_olenkifer(colwise, vecs, 0)

      end subroutine readconverged_symmetric


      subroutine readconverged(recgroup, colwise, nconv, vecs, rightvecs)
            !
            ! Read converged eigenvectors stored on disk (symmetric eigensystem).
            !
            type(trecgroup), intent(inout)          :: recgroup
            logical, intent(in)                     :: colwise
            integer, intent(in)                     :: nconv
            real(F64), dimension(:, :), intent(out) :: vecs
            logical, intent(in)                     :: rightvecs

            integer :: w, k
            integer :: info

            do k = 1, nconv
                  if (rightvecs) then
                        w = 2 * k - 1
                  else
                        w = 2 * k
                  end if
                  
                  if (colwise) then
                        call io_record_read(recgroup, w, vecs(:, k), info)
                  else
                        call io_record_read(recgroup, w, vecs(k, :), info)
                  end if
            end do
      end subroutine readconverged


      subroutine calcqk(row, normqk, m, sig_rows, sig_cols, sig_rows_cc3, sig_recgroup, sig_journ, &
            bm_rows, bm_cols, bm_recgroup, bm_journ, ck, lambdak)
            ! ------------------------------------------------------------
            ! Compute q_k vector, Eq. 10 in [1]. Note that there are 
            ! two interpretations of the matrix SIGMA, depending
            ! on the stage of computation:
            !
            ! Converging right eigenvectors:
            ! ------------------------------
            ! Q_k = (A - \lambda_k) B C_k 
            !     = A * B * C_k - \lambda_k * B * C_k
            !     = SIGMA^T * C_k - \lambad_k * B * C_k,
            ! where SIGMA = B^T A^T.
            ! 
            ! Converging left eigenvectors:
            ! -----------------------------
            ! \bar{Q_k} = (A^T - \lambda_k) \bar{B} \bar{C_k}
            !           = A^T \bar{B} \bar{C_k} - \lambda_k * \bar{B} * \bar{C_k}
            !           = SIGMA^T * \bar{C_k} - \lambda_k * \bar{B} * \bar{C_k},
            ! where SIGMA = \bar{B}^T A.
            ! ------------------------------------------------------------
            ! Qk      - Hirao and Nakatsuji q_m vector (Eq. 10 in [1])
            ! NORMQK  - || . ||_2 norm of the computed Qk vector
            ! M       - Number of vectors stored in B^m matrix
            ! SIGMA   - B^T A^T  or \bar{B}^T A
            ! BM      - Hirao and Nakatsuji: B^m matrix of trial vectors
            ! CK      - Eigenvector of the interaction matrix (small
            !           matrix) corresponding to the k-th eigenvector
            !           searched for
            ! LAMBDAK - Eigenvalue of the interaction matrix corresponding
            !           to the C_k vector
            ! ------------------------------------------------------------
            ! 1. Hirao, K. and Nakatsuji, H., A generalization of the
            !    Davidson's Method to Large Nonsymmetric Eigenvalue
            !    Problems, J. Comp. Phys. 45, 246 (1982).
            !
            integer, intent(in)                       :: row
            real(F64), intent(out)                    :: normqk
            integer, intent(in)                       :: m
            real(F64), dimension(:, :), intent(in)    :: sig_rows
            real(F64), dimension(:, :), intent(in)    :: sig_cols
            real(F64), dimension(:, :), intent(inout) :: sig_rows_cc3
            type(trecgroup), intent(inout)            :: sig_recgroup
            type(tjournal), intent(in)                :: sig_journ
            real(F64), dimension(:, :), intent(inout) :: bm_rows
            real(F64), dimension(:, :), intent(in)    :: bm_cols
            type(trecgroup), intent(inout)            :: bm_recgroup
            type(tjournal), intent(in)                :: bm_journ
            real(F64), dimension(:), intent(in)       :: ck
            real(F64), intent(in)                     :: lambdak

            real(F64) :: alpha
            integer :: k, pos, cont, info
            ! 
            ! Qk <- SIGMA^T C_k = C_k^T SIGMA. 
            !
            bm_rows(row, :) = ZERO
!            print*,'zrrrrrrrrrrrrrrrrrr1',  bm_rows(1, 6)
            do k = 1, m
                  call sig_journ%read(k, pos, cont)
                  alpha = ck(k)
                  if (cont == CONT_ROWS) then
                        !
                        ! AXPY operation:
                        ! Y <- ALPHA * X + Y
                        !
                        if (EOM_MEM)then
                              sig_rows_cc3(k, :) = - sig_rows_cc3(k, :) + sig_rows(pos, :)
                              call la_axpy_olenkifer(bm_rows(row, :), alpha, sig_rows_cc3(k, :))
                        else
                              call la_axpy_olenkifer(bm_rows(row, :), alpha, sig_rows(pos, :))
                        end if
!                        print*,'zrrrrrrrrrrrrrrrrrr2',  bm_rows(1, 6)
                  else if (cont == CONT_COLS) then
                        if (EOM_MEM)then
                              sig_rows_cc3(k, :) = - sig_rows_cc3(k, :) + sig_cols(:, pos)
                              call la_axpy_olenkifer(bm_rows(row, :), alpha, sig_rows_cc3(k, :))
                        else
                              call la_axpy_olenkifer(bm_rows(row, :), alpha, sig_cols(:, pos))
                        end if
!                        print*,'zrrrrrrrrrrrrrrrrrr3',  bm_rows(1, 6)
                  else
                        !
                        ! Data stored on disk
                        !
                        if (EOM_MEM) then
                              call msg('DATA ON DISK, NOT IMPLEMENTED')
                              stop
                        end if
                        call io_record_axpy(sig_recgroup, k, bm_rows(row, :), alpha, info)
!                        print*,'zrrrrrrrrrrrrrrrrrr4',  bm_rows(1, 6)
                  end if
            end do
            !
            ! Qk <- Qk - \lambda_k * B^m C_k = Qk - \lambda * C_k^T B^m
            !
            do k = 1, m
                  call bm_journ%read(k, pos, cont)
                  alpha = -lambdak * ck(k)
                  if (EOM_MEM) then
                        call la_axpy_olenkifer(bm_rows(row, :), alpha, bm_rows(k, :))
                  else
                        if (cont == CONT_ROWS) then
                              !
                              ! AXPY operation:
                              ! Y <- ALPHA * X + Y
                              !
                              call la_axpy_olenkifer(bm_rows(row, :), alpha, bm_rows(pos, :))
                        else if (cont == CONT_COLS) then
                              call la_axpy_olenkifer(bm_rows(row, :), alpha, bm_cols(:, pos))
                        else
                              !
                              ! Data stored on disk
                              !
                              call io_record_axpy(bm_recgroup, k, bm_rows(row, :), alpha, info)
                        end if
                  end if
!                  print*,'zrrrrrrrrrrrrrrrrrr6',  bm_rows(1, 6)
            end do
            !
            ! Compute 2-norm of the Qk vector. It is further used
            ! for testing convergence, see discussion above Eq. 15 in [1]
            !
!            print*,'zrrrrrrrrrrrrrrrrrr7',  bm_rows(1, 6)
            call la_l2norm_olenkifer(normqk, bm_rows(row, :))
!            print*,'zrrrrrrrrrrrrrrrrrr8',  bm_rows(1, 6)

      end subroutine calcqk


      subroutine calczetak(qk, adiag, lambdak)
            ! ------------------------------------------------------------
            ! Compute Zeta_k vector, Eq. 9 in [1].
            ! ------------------------------------------------------------
            ! Qk      - On entry, Qk vector (Eq. 10 in [1]). On exit,
            !           Zeta_k vector (Eq. 10 in [1])
            ! ADIAG   - ADIAG(I) is diagonal element of the A matrix,
            !           A(I, I)
            ! LAMBDAK - Eigenvector of the interaction matrix (small A)
            !           corresponding to the eigenvector Ck
            ! ------------------------------------------------------------
            ! 1. Hirao, K. and Nakatsuji, H., A generalization of the
            !    Davidson's Method to Large Nonsymmetric Eigenvalue
            !    Problems, J. Comp. Phys. 45, 246 (1982).
            !
            real(F64), dimension(:), intent(inout) :: qk
            real(F64), dimension(:), intent(in)    :: adiag
            real(F64), intent(in)                  :: lambdak

            integer :: n, i
            real(F64) :: denom

            n = size(qk)
            if (size(qk) .ne. size(adiag, dim=1)) then
                  call msg("INCONSISTENT DIMENSIONS PASSED TO CALCZETAK", &
                        priority=MSG_ERROR)
                  stop
            end if
!             print*, ''
!             print*, 'CALCZETAK'
! print*, ''
            !$omp parallel do private(denom, i) shared(n, qk, adiag, lambdak) default(none)
            do i = 1, n
                  denom = lambdak - adiag(i)
!                  if (abs(adiag(i)).gt.1.d-5)then
!                        print*, 'lambdak', i, lambdak, adiag(i), denom
!                        print*, i, 'i', adiag(i)
!                        print*, i, ',', adiag(i)
!                  end if
                  if (abs(denom) .lt. 1.d-10)then
                        denom = 1.d-6
                  end if
                  ! if (abs(qk(i)).gt.1.d-5)then
                  !       print*, i, ',',qk(i), ',',adiag(i)
                  ! end if
                  qk(i) = qk(i) / denom

            end do
            !$omp end parallel do
      end subroutine calczetak


      subroutine add_residual(k, l, m, bm_rows, bm_cols, bm_recgroup, bm_journ, &
            cm, sig_rows, sig_cols, sig_rows_cc3, sig_recgroup, sig_journ, lambdal, adiag, &
            normqk, convthresh)
            ! -----------------------------------------------------------------------
            ! Compute the residual ZETA vectors of Eq. 9 in [1] and append them to
            ! BM and BMBAR matrices. These vectors at this stage are neither
            ! orthogonalized (Eq. 11 in [1]) nor normalized as in Eq. 13 in [1].
            ! -----------------------------------------------------------------------
            ! 1. Hirao, K. and Nakatsuji, H., A generalization of the
            !    Davidson's Method to Large Nonsymmetric Eigenvalue
            !    Problems, J. Comp. Phys. 45, 246 (1982).
            ! -----------------------------------------------------------------------
            ! K       - Column of the B^{(m)} (\bar{B}^{(m)}) matrix into which
            !           ZETA vector should be written
            !
            ! L       - Index of the eigenvector for which the ZETA vector is
            !           requested
            !
            ! M       - Number of columns stored in BM (BMBAR) matrices before
            !           appending any ZETA vector in this iteration. It is equal
            !           as order of CM (CMBAR) matrix
            !
            ! BASIS,     - B^{(m)} and \bar{B}^{(m)} matrices in [1]
            !
            ! CM      - Eigenvectors of the interaction (small) matrix
            ! 
            ! LAMBDAL - Eigenvalue of L-th eigenvector
            ! ADIAG   - The diagonal of A matrix
            !
            ! NORMQK
            !         L2 norm of the k-th residual vector
            !
            !
            integer, intent(in)                       :: k
            integer, intent(in)                       :: l
            integer, intent(in)                       :: m
            real(F64), dimension(:, :), intent(inout) :: bm_rows
            real(F64), dimension(:, :), intent(in)    :: bm_cols
            type(trecgroup), intent(inout)            :: bm_recgroup
            type(tjournal), intent(in)                :: bm_journ
            real(F64), dimension(:, :), intent(in)    :: cm
            real(F64), dimension(:, :), intent(in)    :: sig_rows
            real(F64), dimension(:, :), intent(in)    :: sig_cols
            real(F64), dimension(:, :), intent(inout)    :: sig_rows_cc3
            type(trecgroup), intent(inout)            :: sig_recgroup
            type(tjournal), intent(in)                :: sig_journ
            real(F64), intent(in)                     :: lambdal
            real(F64), dimension(:), intent(in)       :: adiag
            real(F64), intent(out)                    :: normqk
            real(F64), intent(in)                     :: convthresh
            !
            ! The residual vector Q_k of the L-th left/right eigenvector
            ! ---
            ! As described by Hirao in [1] (text above Eq. 15), the norm
            ! of the residual Qk vectors should be used to check for the
            ! eigenvectors' convergence.
            !
            integer :: i, j

            call calcqk(k, normqk, m, sig_rows, sig_cols, sig_rows_cc3, sig_recgroup, sig_journ, &
                  bm_rows, bm_cols, bm_recgroup, bm_journ, cm(1:m, l), lambdal)

            if (normqk >= convthresh) then
                  !
                  ! \zeta_k and \bar{\zeta_k} vectors as defined in Eq. 9 in 1.
                  ! Note that these vectors are neither orthogonalized against other
                  ! vectors nor biorthonormalized at this stage.
                  !

                  call calczetak(bm_rows(k, :), adiag, lambdal)

            end if

      end subroutine add_residual

      subroutine cisd_sorting(m, C, wr, wi, e_total)
            ! ----------------------------------------------------------------------------                                                                                   
            ! Sort CISD vectors in an order of ascending energy
            ! ---------------------------------------------------------------------------- 
            integer, intent(in)                       :: m
            real(F64), dimension(:, :), intent(inout) :: c
            real(F64), dimension(:), intent(inout)    :: wr
            real(F64), dimension(:), intent(inout)    :: wi
            integer, dimension(:), allocatable :: eorder
            real(F64), intent(in) :: e_total

            integer :: i

            allocate(eorder(m))

            do i = 1, m
                  eorder(i) = i
            end do

            call dsort(wr, eorder, m)


            if (abs(wr(1)) .gt. abs(e_total)) then
                  wr(1) = 1.d+5
                  call dsort(wr, eorder, m)
            end if


            WORK2(1:m, 2) = wi(1:m)
            wi = ZERO
            do i = 1, m
                  wi(i) = WORK2(eorder(i), 2)
            end do

            WORK2(1:m, 1:m) = c(1:m, 1:m)
            c = ZERO
            do i = 1, m
                  c(1:m, i) = WORK2(1:m, eorder(i))
            end do

            deallocate(eorder)
      end subroutine cisd_sorting


      subroutine root_homing(m, C, wr, wi, degener)
            ! ----------------------------------------------------------------------------
            ! Identify the the vectors in the current iterative subspace which have the
            ! the largest overlap with the true solutions of the eigenproblem.
            ! ----------------------------------------------------------------------------
            ! 1. Hirao, K. and Nakatsuji, H., A generalization of the
            !    Davidson's Method to Large Nonsymmetric Eigenvalue
            !    Problems, J. Comp. Phys. 45, 246 (1982).
            !
            ! 2. Butscher, W., Kammer, W. E. "Modification of Davidson's Method
            !    for the Calculation of Eigenvalues and Eigenvectors of Large
            !    Real-Symmetric Matrices: <<Root homing procedure>>"
            !    Journal of Computational Physics 20, 313-325 (1976)
            !-----------------------------------------------------------------------------
            ! n, m, n_lev,
            ! B, wr, degener - the same as in dav_iter
            ! C, wrsmall - described in dav_init
            ! C_old, C_old_bar......on entry C, and C_bar from previous
            !                       iteration. On exit, C and C_bar from
            !                       current iteration
            !
            integer, intent(in)                       :: m
            real(F64), dimension(:, :), intent(inout) :: C
            real(F64), dimension(:), intent(inout)    :: wr
            real(F64), dimension(:), intent(inout)    :: wi
            integer, dimension(:),  intent(in)        :: degener

            integer :: i, j, j0, j1, p, q
            integer :: nfound
            logical :: lfound
            real(F64) :: t, t0
            integer, dimension(:), allocatable :: eorder

            allocate(eorder(DAV_NSOUGHT))
            ! 
            ! Compute overlap between old approximations to the eigenvectors sought
            ! and every eigenvector of the current interaction matrix (ASMALL, A^{(m)}).
            ! Overlap integrals are simplified because the basis (columns of DAV_BASIS)
            ! is orthonormal.
            
             do j = 1, m
                   do i = 1, DAV_NSOUGHT
                         s_ovr_real(i, j) = abs(c(i, j))
 !                        print*, i, j, s_ovr_real(i, j)
                   end do
             end do

            ! do j = 1, m                                                                                                                                 
            !       do i = 1, DAV_NSOUGHT                     
            !             call la_dot_olenkifer(s_ovr_real(i, j), c(:, j)
            !             s_ovr_real(i, j) = la_dot_olenkifer(
            !             print*, i, j, s_ovr_real(i, j)                                                                                                 
            !       end do                                                                                                                              
            ! end do 

!            do i = 1, m
!                  do j = 1, m
!                        call la_dot_olenkifer(s_ovr_real(i, j), c(:, i), c(:, j))
!                  end do
!            end do

            nfound = 0
            eorder = 0
            !
            ! Loop over distinct eigenvalues
            !
            iloop: do i = 1, DAV_NROOTS
                  t = huge(ZERO)
                  lfound = .false.
                  do j = 1, m - degener(i) + 1
                        j0 = j
                        j1 = j0 + degener(i) - 1
                        !
                        ! Check if the current multiplet does not collide
                        ! with one of the multiplets found previously
                        !
                        if (.not. doescollide(j0, j1, eorder, nfound)) then
                              !
                              ! Negative eigenvalues are not accepted unless
                              ! configured otherwise by the user.
                              !
                              if (DAV_NEGATIVE_ROOTS .or. minval(wr(j0:j1)) > ZERO) then
                                    t0 = multoverlap(j, degener(i), nfound+1, s_ovr_real)
                                    t0 = abs(ONE - t0)
                                    if (t0 < t) then
                                          lfound = .true.
                                          q = j
                                          do p = nfound+1, nfound+degener(i)
                                                !
                                                ! EORDER(P) is the index of a column
                                                ! corresponding to the P-th target eigenvector
                                                !
                                                eorder(p) = q
                                                q = q + 1
                                          end do
                                          t = t0
                                    end if
                              end if
                        end if
                  end do
                  if (lfound) then
                        nfound = nfound + degener(i)
                  else
                        exit iloop
                  end if
            end do iloop
            if (nfound .ne. DAV_NSOUGHT) then
                  call msg("ERROR: ROOT HOMING FAILED.", priority=MSG_ERROR)
                  if (.not. DAV_NEGATIVE_ROOTS) then
                        call msg("THE PROBLEM MAY BE RELATED TO THE " // &
                              "REJECTION OF NEGATIVE ROOTS", priority=MSG_ERROR)
                  end if
                  stop
            end if

            WORK2(1:m, 1) = wr(1:m)
            WORK2(1:m, 2) = wi(1:m)
            wr = ZERO
            wi = ZERO
            do i = 1, DAV_NSOUGHT
                  wr(i) = WORK2(eorder(i), 1)
                  wi(i) = WORK2(eorder(i), 2)
            end do

            WORK2(1:m, 1:m) = c(1:m, 1:m)
            c = ZERO
            do i = 1, DAV_NSOUGHT
                  c(1:m, i) = WORK2(1:m, eorder(i))
            end do


            deallocate(eorder)
      end subroutine root_homing


      function conjugatepair(re1, im1, re2, im2)
            logical               :: conjugatepair
            real(F64), intent(in) :: re1
            real(F64), intent(in) :: im1
            real(F64), intent(in) :: re2
            real(F64), intent(in) :: im2

            if (compare(re1, re2, THREE) .and. compare(im1, -im2, THREE)) then
                  conjugatepair = .true.
            else
                  conjugatepair = .false.
            end if
      end function conjugatepair


      function doescollide(i0, i1, a, k)
            logical :: doescollide
            integer, intent(in)               :: i0
            integer, intent(in)               :: i1
            integer, dimension(:), intent(in) :: a
            integer, intent(in)               :: k

            integer :: i

            doescollide = .false.
            iloop: do i = 1, k
                  if (a(i) <= i1 .and. a(i) >= i0) then
                        doescollide = .true.
                        exit iloop
                  end if
            end do iloop
      end function doescollide


      function multoverlap(j, deg, st, s_ovr)
            !
            ! Finds overlap between two sets of vectors,
            ! possibly belonging to a degenerate eigenvalue.
            !
            real(F64)                              :: multoverlap
            integer, intent(in)                    :: j
            integer, intent(in)                    :: deg
            integer, intent(in)                    :: st
            real(F64), dimension(:, :), intent(in) :: S_ovr

            integer :: p0, p1, q0, q1
            integer :: p, q
            real(F64) :: s

            p0 = st
            p1 = st + deg - 1
            q0 = j
            q1 = j + deg -1
            multoverlap = ZERO
            do q = q0, q1
                  s = ZERO
                  do p = p0, p1
                        s = s + abs(s_ovr(p, q))
                  end do

                  if (q == q0) then
                        multoverlap = s
                  else
                        multoverlap = min(s, multoverlap)
                  end if
            end do
      end function multoverlap


      subroutine dav_densesolver(a, n, vrsorted, wrsorted, wisorted)
            !---------------------------------------------------------------
            ! Compute left and right orthogonal eigenvectors and eigenvalues
            ! of a small-size nonsymmetric matrix a. Sort eigenvalues
            ! and eigenvectors in ascending order.
            !---------------------------------------------------------------
            ! a................nonsymmetric square matrix
            ! n................dimension of a
            ! vrsorted.........matrix of n right eigenvectors
            !                  sorted in ascending order according
            !                  to eigenvalues
            ! wrsorted.........real part of n eigenvalues sorted 
            !                  in ascending order
            ! wisorted.........im part of n eigenvalues sorted 
            !                  in ascending order
            !
            real(F64), dimension(:,:), intent(in)  :: a
            integer, intent(in)                    :: n
            real(F64), dimension(:,:), intent(out) :: vrsorted
            real(F64), dimension(:), intent(out)   :: wrsorted
            real(F64), dimension(:), intent(out)   :: wisorted

            real(F64), dimension(1, 1) :: foo
            real(F64) :: t
            integer :: i
            integer :: lwork, liwork
            
            if (DAV_SYMMETRIC) then
                  !
                  ! Symmetric eigensystem
                  ! ---------------------
                  !
                  call dsyevdquery(n, lwork, liwork, "V")                      
                  allocate(geev_work(lwork))                                                                                                                                           
                  allocate(geev_iwork(liwork))      

                  geev_a(1:n, 1:n) = a(1:n, 1:n)
                  call dsyevdwrap(geev_a, wrsorted, n, "V", geev_work, geev_iwork)
                  wisorted = ZERO
                  vrsorted = ZERO
                  vrsorted(1:n, 1:n) = geev_a(1:n, 1:n)
                  
                  deallocate(geev_work)
                  deallocate(geev_iwork)
            else
                  !
                  ! Nonsymmetric eigensystem
                  ! ------------------------
                  !
                  call dgeevquery(n, lwork, "N", "V")
                  allocate(geev_work(lwork))

                  geev_a(1:n, 1:n) = a(1:n,1:n)
                  call geevwrap(geev_a, geev_wr, geev_wi, foo, geev_vr, &
                        n, "N", "V", geev_work)
                  deallocate(geev_work) 
                  do i = 1, n
                        geev_dy(i) = i
                  end do
                  !
                  ! Sort eigenvalues and eigenvectors according to
                  ! ascending values of the real parts of eigenvalues
                  !
                  call dsort0(geev_wr, geev_dy, n, 2)
                  wrsorted = ZERO
                  wisorted = ZERO
                  vrsorted = ZERO
                  wrsorted(1:n) = geev_wr(1:n)
                  do i = 1, n
                        wisorted(i) = geev_wi(geev_dy(i))
                        vrsorted(1:n, i) = geev_vr(1:n, geev_dy(i))
                  end do
                  !
                  ! BiorthoNORMALITY of trial vectors is not utilized
                  ! until the convergence. At this stage, each right and
                  ! left eigenvector is NORMALIZED separately. The normalization
                  ! to unity is utilized during root homing.
                  !
                  do i = 1, n
                        call la_l2norm_olenkifer(t, vrsorted(1:n, i))
                        call la_scal_olenkifer(vrsorted(1:n, i), ONE/t)
                  end do
            end if
      end subroutine dav_densesolver


      function dav_converging_right()
            logical :: dav_converging_right
            
            if (DAV_RIGHTVECS) then
                  dav_converging_right = .true.
            else
                  dav_converging_right = .false.
            end if
      end function dav_converging_right


      subroutine dav_sigma_update_right(aij, i, j)
            ! ---------------------------------------------------------------
            ! Converging right eigenvectors. The interpretation of DAV_SIGMA
            ! at this stage is DAV_SIGMA = B^T A^T, where the columns of B
            ! represent vectors spanning the iterative subspace. Use this
            ! subroutine when the number of trial vectors to update the
            ! DAV_SIGMA matrix is large. Use one of the unrolled variants
            ! of this subroutine otherwise.
            ! ---------------------------------------------------------------
            ! AIJ   
            !       The element A(I, J) of the (possibly nonsymmetric)
            !       matrix A.
            ! I, J
            !       Indices corresponding to the matrix element AIJ.
            !
            real(F64), intent(in) :: aij
            integer, intent(in)   :: i
            integer, intent(in)   :: j
            
            integer :: p
            
            if (i == j) then
                  diag(i) = diag(i) + aij
            end if
            
            do p = 1, NNEW
                  SIGROWS(p, i) = SIGROWS(p, i) + VECROWS(p + update_offset, j) * aij
            end do

      end subroutine dav_sigma_update_right


      subroutine dav_sigma_update_right_nondiag(aij, i, j)
            !
            ! An optimized version of DAV_SIGMA_UPDATE_RIGHT.
            ! Assumptions:
            ! 1. I .NE. J
            !
            real(F64), intent(in) :: aij
            integer, intent(in)   :: i
            integer, intent(in)   :: j

            integer :: p
            integer :: k
            

            do p = 1, NNEW
                  ! if (i == 5) then
                  !       print*, 'z wew', j, VECROWS(p, j) , aij
                  ! end if
                  SIGROWS(p, i) = SIGROWS(p, i) + VECROWS(p, j) * aij
            end do


      end subroutine dav_sigma_update_right_nondiag


      subroutine dav_sigma_update_diag(aii, i)
            !
            ! An optimized version of DAV_SIGMA_UPDATE_RIGHT
            ! and DAV_SIGMA_UPDATE_LEFT.
            ! Assumptions:
            ! 1. I == J
            !
            real(F64), intent(in) :: aii
            integer, intent(in)   :: i

            integer :: p

            diag(i) = diag(i) + aii

            do p = 1, NNEW
                  SIGROWS(p, i) = SIGROWS(p, i) + VECROWS(p, i) * aii
            end do
      end subroutine dav_sigma_update_diag


      subroutine dav_sigma_update_left(aij, i, j)
            ! ---------------------------------------------------------------
            ! Converging left eigenvectors. The interpretation of DAV_SIGMA
            ! at this stage is DAV_SIGMA = B^T A, where the columns of B
            ! represent vectors spanning the iterative subspace. Use this
            ! subroutine when the number of trial vectors to update the
            ! DAV_SIGMA matrix is large. Use one of the unrolled variants
            ! of this subroutine otherwise.
            ! ---------------------------------------------------------------
            ! AIJ   
            !       The element A(I, J) of the (possibly nonsymmetric)
            !       matrix A.
            ! I, J
            !       Indices corresponding to the matrix element AIJ.
            !
            real(F64), intent(in) :: aij
            integer, intent(in)   :: i
            integer, intent(in)   :: j

            integer :: p

            if (i == j) then
                  diag(i) = diag(i) + aij
            end if

            do p = 1, NNEW
                  SIGROWS(p, j) = SIGROWS(p, j) + VECROWS(p, i) * aij
            end do
      end subroutine dav_sigma_update_left


      subroutine dav_sigma_update_left_nondiag(aij, i, j)
            !
            ! An optimized version of DAV_SIGMA_UPDATE_LEFT.
            ! Assumptions:
            ! 1. I .NE. J
            !
            real(F64), intent(in) :: aij
            integer, intent(in)   :: i
            integer, intent(in)   :: j

            integer :: p

            do p = 1, NNEW
                  SIGROWS(p, j) = SIGROWS(p, j) + VECROWS(p + update_offset, i) * aij
            end do
      end subroutine dav_sigma_update_left_nondiag


      subroutine dav_unittest()
            ! ----------------------------------------------------------------
            ! Test the implementation by diagonalizing the matrix defined in 
            ! Eq. 23 of Ref. [1]:
            ! 
            ! A_{ij} = i \delta_{ij} - (i - j - k^2)     1 <= j <= k
            !        = i \delta_{ij} + (i - j - k^2)   k+1 <= j <= N
            ! where
            !        N  = 2 k
            ! The matrix A has real eigenvalues 1, 2, ..., N. The right-hand
            ! eigenvectors are columns of (I + UV^T) and left-hand
            ! eigenvectors are rows of (I - UV^T),
            !       U = (1, 1, 1, ..., 1),
            !       V = (1, 1, ..., 1, -1, -1, ..., -1) 
            ! where V has k components of 1 and k components of -1.
            ! ----------------------------------------------------------------
            ! 1. Hirao, K., Nakatsuji, H., A generalization of the Davidson's
            !    Method to Large Nonsymmetric Eigenvalue Problems,
            !    J. Comput. Phys. 45, 246 (1982)
            !
            integer :: i, j
            integer :: niter
            integer, parameter :: k = 200
            integer, parameter :: max_niter = 150
            integer, parameter :: n = 2 * k
            integer, parameter :: nroots = 3
            integer, dimension(nroots), parameter :: degener = [(1, i = 1, nroots)]
            real(F64), dimension(n, nroots) :: guess_coeff
            real(F64), parameter :: convthresh = 1.E-4_F64
            !
            ! Make the guess from the perturbed right exact eigenvectors if .TRUE.
            ! Use exact left eigenvectors if .FALSE.
            !
            logical, parameter :: rightguess = .false.
            !
            ! Maximum memory share
            !
            integer(I64) :: memspace 
            !
            ! Maximum disk share
            !
            integer(I64) :: diskspace 
            
            real(F64), dimension(n, n) :: a
            real(F64), dimension(n), parameter :: u = [(ONE, i = 1, n)]
            real(F64), dimension(n), parameter :: v = [(ONE, i = 1, k), (-ONE, i = 1, k)]
            real(F64), dimension(n, n) :: rightvecs
            real(F64), dimension(n, n) :: leftvecs
            real(F64), dimension(:, :), allocatable :: c
            real(F64), dimension(nroots) :: wr, wi, e_diff, residual_l2
            integer :: info
            real(F64), parameter :: max_noise = 0.00001E+0_F64
            real(F64), dimension(n) :: noise_vector
            type(trecgroup) :: convrecs
            logical :: right_converged
            real(F64), dimension(n, nroots) :: rightvecs_approx, rightvecs_exact
            real(F64), dimension(n, nroots) :: leftvecs_approx, leftvecs_exact
            real(F64), dimension(nroots, nroots) :: lt_a_r
            
            memspace = 4 * nroots * vectorsize(int(n, I64))
            diskspace = 100 * nroots * vectorsize(int(n, I64))

            do j = 1, k
                  do i = 1, n
                        if (i == j) then
                              a(i, j) = real(i - (i - j - k**2), F64)
                        else
                              a(i, j) = real(-(i - j - k**2), F64)
                        end if
                  end do
            end do

            do j = k+1, n
                  do i = 1, n
                        if (i == j) then
                              a(i, j) = real(i + (i - j - k**2), F64)
                        else
                              a(i, j) = real((i - j - k**2), F64)
                        end if
                  end do
            end do

            do j = 1, n
                  do i = 1, n
                        if (i == j) then
                              rightvecs(i, j) = ONE + u(i) * v(j)
                              leftvecs(i, j) = ONE - u(i) * v(j)
                        else
                              rightvecs(i, j) = u(i) * v(j)
                              leftvecs(i, j) = -u(i) * v(j)
                        end if
                  end do
            end do
            
            if (rightguess) then
                  guess_coeff = rightvecs(:, 1:nroots)
            else
                  guess_coeff = transpose(leftvecs(1:nroots, :))
            end if
            !
            ! Add random noise to guess vectors
            !
            do i = 1, nroots
                  call random_number(noise_vector)
                  noise_vector = max_noise * noise_vector
                  guess_coeff(:, i) = guess_coeff(:, i) + noise_vector
            end do

            right_converged = .false.
            call dav_init(n, n, 0, nroots, degener, .false., .true., .true., guess_coeff, &
                  convthresh, max_niter, memspace, diskspace, .true., convrecs, "dav_test_")

            allocate(c(DAV_MAXNVEC, nroots))

            do niter = 1, max_niter
                  if (dav_converging_right()) then
                        do j = 1, n
                              do i = 1, n
                                    call dav_sigma_update_right(a(i, j), i, j)
                              end do
                        end do
                  else  
                        do j = 1, n
                              do i = 1, n
                                    call dav_sigma_update_left(a(i, j), i, j)
                              end do
                        end do
                  end if
                  
                  call dav_iter(wr, wi, residual_l2, e_diff, convrecs, info)

                  if (info == DAV_CONVERGED) then
                        if (.not. right_converged) then
                              call msg("DAVIDSON UNIT TEST: RIGHT EIGENVECTORS CONVERGED")
                              call dav_free()
                              call readconverged(convrecs, .true., nroots, guess_coeff, .true.)
                              call dav_init(n, n, 0, nroots, degener, .false., .false., .true., guess_coeff, &
                                    convthresh, max_niter, GIBIBYTE, I64_ZERO, .false.)
                              right_converged = .true.
                        else
                              call msg("DAVIDSON UNIT TEST: LEFT EIGENVECTORS CONVERGED")
                              exit
                        end if
                  else if (info == DAV_FAILURE) then
                        call msg("DAVIDSON UNIT TEST: EXIT WITH DAV_FAILURE CODE")
                        exit
                  end if
            end do

            if (info == DAV_CONVERGED) then
                  call msg("DAV_TEST: UNIT TEST SUCCESSFULLY COMPLETED")
                  call imsg("EIGENPROBLEM DIMENSION", n)
                  call msg("NUMBER OF ROOTS", nroots)
                  do i = 1, nroots
                        call dmsg(str(i), wr(i))
                  end do
            else
                  call msg("DAV_UNITTEST: UNIT TEST DID NOT CONVERGE", MSG_ERROR)
                  stop
            end if

            call readconverged_nonsymmetric(convrecs, .true., nroots, leftvecs_approx, rightvecs_approx)
            rightvecs_exact = rightvecs(:, 1:nroots)
            leftvecs_exact = transpose(leftvecs(1:nroots, :))
            lt_a_r = matmul(matmul(transpose(leftvecs_approx), a), rightvecs_approx)
            call msg("L^T A R (USING APPROXIMATE EIGENVECS")
            call geprn(lt_a_r)
            
            call dav_diskusage(convrecs)
            call dav_free()
            !
            ! Delete converged eigenvectors stored on disk
            !
            call io_record_deletegroup(convrecs)
            deallocate(c)
      end subroutine dav_unittest
end module davidson_main
