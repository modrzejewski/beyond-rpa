module images
      use gparam
      use arithmetic
      use specf
      use grid
      use display
      use sort
      use linalg
!      use mpi

      implicit none
      save

      double precision, dimension(:), allocatable, private :: SENDBUF
      double precision, dimension(:), allocatable, private :: RECVBUF
      integer, private                                     :: NKSPARAM = 6
      integer, private                                     :: BUFSIZE

      logical, protected          :: IMG_ENABLED
      integer, protected          :: IMG_NALL
      integer, protected          :: IMG_RANK
      logical, protected          :: IMG_ISMASTER
      integer, parameter          :: IMG_COMM_ALL = 1 !MPI_COMM_WORLD
      integer, parameter          :: IMG_STATUS_SIZE = 1 !MPI_STATUS_SIZE
      !
      ! Rank of the master image
      !
      integer, parameter          :: IMG_MASTERRANK = 0
      !
      ! The maximum size of a short message containing integer numbers
      !
      integer, parameter          :: IMG_IMSGSIZE = 3
      !
      ! Message tags
      !
      integer, parameter          :: IMG_TAG_DEFAULT = 1
      !
      ! Messages
      !
      integer, parameter          :: IMG_MSG_NEXTTASK = 2**0
      integer, parameter          :: IMG_MSG_FOCKGEN  = 2**1
      integer, parameter          :: IMG_MSG_XCGEN    = 2**2
      integer, parameter          :: IMG_MSG_ITERDONE = 2**3
      integer, parameter          :: IMG_MSG_SCFDONE  = 2**4
      integer, parameter          :: IMG_MSG_NEXTITER = 2**5
      integer, parameter          :: IMG_MSG_DFTSW    = 2**6
      integer, parameter          :: IMG_MSG_ACCNORM  = 2**7
      integer, parameter          :: IMG_MSG_SPINPOLR = 2**8
      integer, parameter          :: IMG_MSG_WORKDONE = 2**9
      !
      ! Types of REDUCE operations
      !
      integer, parameter :: IMG_SUM = -1 !MPI_SUM
      integer, parameter :: IMG_MAX = -1 !MPI_MAX
      
      integer, dimension(:), allocatable, private :: FOCKTASK
      integer, dimension(:), allocatable, private :: XCTASK
      integer, dimension(:), allocatable, private :: COST

      interface img_toslaves
            module procedure img_itoslaves, img_dtoslaves
      end interface

      
      interface img_frommaster
            module procedure img_ifrommaster, img_dfrommaster
      end interface


contains

      subroutine img_setup()
            !
            ! Set global variables related to replicated data
            ! parallelization
            !
            integer :: ierr

            IMG_ENABLED = .false.
            IMG_NALL = 1
            IMG_RANK = IMG_MASTERRANK
            IMG_ISMASTER = .true.
            BUILDSKS = .true.

            
            ! call mpi_init(ierr)
            ! call mpi_comm_size(IMG_COMM_ALL, IMG_NALL, ierr)

            ! if (IMG_NALL .gt. 1) then
            !       IMG_ENABLED = .true.
                  ! call mpi_comm_rank(IMG_COMM_ALL, IMG_RANK, ierr)
            ! else
            IMG_ENABLED = .false.
            IMG_RANK = IMG_MASTERRANK
            ! end if

            if (IMG_RANK .eq. IMG_MASTERRANK) then
                  IMG_ISMASTER = .true.
            else
                  IMG_ISMASTER = .false.
            end if
            !
            ! Suppress displaying messages if this is
            ! not master image
            !
            if (IMG_ISMASTER) then 
                  MSG_PRIORITY_THRESH = MSG_NORMAL
            else
                  MSG_PRIORITY_THRESH = MSG_ERROR
            end if
            !
            ! Master image does not build Coulomb, exchange,
            ! and XC matrices if distributed data
            ! parallelization is enabled
            !
            if (IMG_ENABLED .and. IMG_ISMASTER) then
                  BUILDSKS = .false.
            else
                  BUILDSKS = .true.
            end if
      end subroutine img_setup


      subroutine img_finalize()
            integer :: ierr
            !            call mpi_finalize(ierr)
            continue
      end subroutine img_finalize


      subroutine tasksort(recalc)
            logical, intent(in) :: recalc

            integer :: k1, k2, l
            integer :: znum, n

            integer, save :: oldqual
            logical, save :: oldprune
            logical :: gridchange

            if (recalc) then
                  gridchange = .true.
            else
                  if ((oldqual .eq. GRD_QUALITY) .and. &
                        (oldprune .eqv. GRD_PRUNE)) then
                        gridchange = .false.
                  else
                        gridchange = .true.
                  end if
            end if

            if (gridchange) then
                  oldqual = GRD_QUALITY
                  oldprune = GRD_PRUNE
                  !
                  ! Sort tasks related to XC matrix build. Sorting is
                  ! done according to decreasing cost
                  !
                  do k1 = 1, NATOM
                        l = k1
                        XCTASK(k1) = l
                        znum = INUCLZ(l)
                        !
                        ! Relative cost is estimated as number of grid points
                        ! belonging to the particular center
                        !
                        n = GRD_ELNPT(zidx(znum))
                        COST(k1) = n
                  end do

                  call isort(COST, XCTASK, NATOM, -2)
            end if

            if (recalc) then
                  !
                  ! Sort tasks related to the Fock matrix build. Sorting is
                  ! done according to decreasing cost
                  !
                  l = 0
                  do k1 = 1, NATOM
                        do k2 = 1, k1
                              l = l + 1
                              call ipack(k1, k2, FOCKTASK(l))
                              !
                              ! Relative cost is estimated as number of orbitals
                              ! belonging to the atom pair
                              !
                              n = (IDX(k1 + 1) - IDX(k1)) * (IDX(k2 + 1) - IDX(k2))
                              COST(l) = n
                        end do
                  end do

                  call isort(COST, FOCKTASK, l, -2)
            end if
      end subroutine tasksort


      subroutine images_init()
            integer :: n

            if (IMG_ENABLED) then
                  BUFSIZE = ((NORB + 1) * NORB) / 2 + NKSPARAM
                  allocate(SENDBUF(BUFSIZE))
                  allocate(RECVBUF(BUFSIZE))
                  allocate(XCTASK(NATOM))
                  n = (NATOM * (NATOM + 1)) / 2
                  allocate(FOCKTASK(n))
                  allocate(COST(n))
                  call tasksort(.true.)
            end if
      end subroutine images_init

      
      subroutine images_free()
            if (IMG_ENABLED) then
                  deallocate(SENDBUF)
                  deallocate(RECVBUF)
                  deallocate(FOCKTASK)
                  deallocate(XCTASK)
                  deallocate(COST)
            end if
      end subroutine images_free


      subroutine img_smfrommaster(m, isfull)
            !
            ! Receive symmetric matrix from master
            ! (group operation)
            !
            double precision, dimension(:, :), intent(out) :: m
            logical, intent(in)                            :: isfull

            integer :: ierr
            integer :: count

            if (size(m) .ne. NORB**2) then
                  call msg("CANNOT RECEIVE ARRAY OF DIMENSION OTHER THAN (NORB,NORB)", MSG_ERROR)
                  stop
            end if
            
            ! count = (NORB * (NORB + 1)) / 2
            ! call mpi_bcast(RECVBUF, count, MPI_DOUBLE_PRECISION, &
            !       IMG_MASTERRANK, IMG_COMM_ALL, ierr)
            ! call smunpack(RECVBUF, m)
            ! if (isfull) call smfill(m)
      end subroutine img_smfrommaster


      subroutine img_gmfrommaster(m)
            !
            ! Receive general (nonsymmetric) matrix
            ! from the master image (group operation)
            !
            real(F64), dimension(:, :), contiguous, intent(out) :: m

            integer :: ierr
            integer :: count

            ! count = size(m)
            ! call mpi_bcast(m, count, MPI_DOUBLE_PRECISION, &
            !       IMG_MASTERRANK, IMG_COMM_ALL, ierr)
      end subroutine img_gmfrommaster


      subroutine img_smtoslaves(m)
            !
            ! Send symmetric matrix to all slave images
            !
            double precision, dimension(:, :), intent(in) :: m

            integer :: ierr
            integer :: count

            if (size(m) .ne. NORB**2) then
                  call msg("CANNOT SEND ARRAY OF DIMENSION OTHER THAN (NORB,NORB)", MSG_ERROR)
                  stop
            end if
            
            ! count = (NORB * (NORB + 1)) / 2
            ! call smpack(m, SENDBUF)
            ! call mpi_bcast(SENDBUF, count, MPI_DOUBLE_PRECISION, &
            !       IMG_MASTERRANK, IMG_COMM_ALL, ierr)
      end subroutine img_smtoslaves


      subroutine img_gmtoslaves(m)
            !
            ! Send general (nonsymmetric) matrix to all slave images
            !
            real(F64), dimension(:, :), contiguous, intent(in) :: m

            integer :: ierr
            integer :: count

            ! count = size(m)
            ! call mpi_bcast(m, count, MPI_DOUBLE_PRECISION, &
            !       IMG_MASTERRANK, IMG_COMM_ALL, ierr)
      end subroutine img_gmtoslaves


      subroutine img_itoslaves(message)
            integer, dimension(IMG_IMSGSIZE), intent(in) :: message

            integer :: k, l
            integer :: ierr
            integer, dimension(IMG_IMSGSIZE, IMG_NALL) :: inmsg
            integer, dimension(IMG_IMSGSIZE) :: outmsg
            integer, dimension(IMG_NALL) :: reqs
            integer :: nreq
            integer, dimension(IMG_STATUS_SIZE, IMG_NALL) :: stats

            ! nreq = IMG_NALL - 1
            ! l = 1
            ! do k = 0, IMG_NALL - 1
            !       if (k .ne. IMG_MASTERRANK) then
            !             call mpi_irecv(inmsg(:, l), IMG_IMSGSIZE, MPI_INTEGER, k, &
            !                   IMG_TAG_DEFAULT, IMG_COMM_ALL, reqs(l), ierr)
            !             l = l + 1
            !       end if
            ! end do

            ! call mpi_waitall(nreq, reqs, stats, ierr)
            
            ! outmsg = message
            ! l = 1
            ! do k = 0, IMG_NALL - 1
            !       if (k .ne. IMG_MASTERRANK) then
            !             call mpi_isend(outmsg, IMG_IMSGSIZE, MPI_INTEGER, k, &
            !                   IMG_TAG_DEFAULT, IMG_COMM_ALL, reqs(l), ierr)
            !             l = l + 1
            !       end if
            ! end do

            ! call mpi_waitall(nreq, reqs, stats, ierr)
      end subroutine img_itoslaves

      
      subroutine img_ifrommaster(task)
            !
            ! Called only by slave images to get next task
            !
            integer, dimension(IMG_IMSGSIZE), intent(out) :: task
            
            integer :: ierr
            integer, dimension(IMG_STATUS_SIZE) :: stat
            integer, dimension(IMG_IMSGSIZE) :: outmsg

            ! outmsg(1) = IMG_MSG_NEXTTASK
            ! outmsg(2) = IMG_RANK

            ! call mpi_send(outmsg, IMG_IMSGSIZE, MPI_INTEGER, IMG_MASTERRANK, &
            !       IMG_TAG_DEFAULT, IMG_COMM_ALL, ierr)
            ! call mpi_recv(task, IMG_IMSGSIZE, MPI_INTEGER, IMG_MASTERRANK, &
            !       IMG_TAG_DEFAULT, IMG_COMM_ALL, stat, ierr)
      end subroutine img_ifrommaster


      subroutine img_dtoslaves(f)
            !
            ! Called only by master to send small array of
            ! floating point numbers (numerical parameters
            ! to set up next task) to slaves.
            !
            real(F64), dimension(:), contiguous, intent(in) :: f
            
            integer :: n
            integer :: k, l
            integer :: ierr
            integer, dimension(IMG_IMSGSIZE, IMG_NALL) :: inmsg
            integer, dimension(IMG_NALL) :: reqs
            integer :: nreq
            integer, dimension(IMG_STATUS_SIZE, IMG_NALL) :: stats

            ! n = size(f)

            ! nreq = IMG_NALL - 1
            ! l = 1
            ! do k = 0, IMG_NALL - 1
            !       if (k .ne. IMG_MASTERRANK) then
            !             call mpi_irecv(inmsg(:, l), IMG_IMSGSIZE, MPI_INTEGER, k, &
            !                   IMG_TAG_DEFAULT, IMG_COMM_ALL, reqs(l), ierr)
            !             l = l + 1
            !       end if
            ! end do
            ! !
            ! ! Wait until IMG_NEXTTASK message is received from all slaves.
            ! ! This message means that they are ready to receive data to
            ! ! set up next task.
            ! !
            ! call mpi_waitall(nreq, reqs, stats, ierr)

            ! l = 1
            ! do k = 0, IMG_NALL - 1
            !       if (k .ne. IMG_MASTERRANK) then
            !             if (inmsg(3, l) .ne. n) then
            !                   call msg("MASTER/SLAVE ARRAYS HAVE INCONSISTENT DIMENSIONS", MSG_ERROR)
            !                   stop
            !             end if
            !             l = l + 1
            !       end if
            ! end do
            
            ! l = 1
            ! do k = 0, IMG_NALL - 1
            !       if (k .ne. IMG_MASTERRANK) then
            !             call mpi_isend(f, n, MPI_DOUBLE_PRECISION, k, &
            !                   IMG_TAG_DEFAULT, IMG_COMM_ALL, reqs(l), ierr)
            !             l = l + 1
            !       end if
            ! end do
            ! !
            ! ! Wait until the message is received by all slaves
            ! !
            ! call mpi_waitall(nreq, reqs, stats, ierr)
      end subroutine img_dtoslaves


      subroutine img_dfrommaster(f)
            !
            ! Called only by slaves to receive small array
            ! of floating point numbers (numerical parameters
            ! to set up next task).
            !
            real(F64), dimension(:), contiguous, intent(out) :: f

            integer :: ierr
            integer, dimension(IMG_STATUS_SIZE) :: stat
            integer, dimension(IMG_IMSGSIZE) :: outmsg
            integer :: n

            ! n = size(f)
            ! outmsg(1) = IMG_MSG_NEXTTASK
            ! outmsg(2) = IMG_RANK
            ! outmsg(3) = n
            ! !
            ! ! Send signal to the master image that this process is ready to receive
            ! ! data
            ! !
            ! call mpi_send(outmsg, IMG_IMSGSIZE, MPI_INTEGER, IMG_MASTERRANK, &
            !       IMG_TAG_DEFAULT, IMG_COMM_ALL, ierr)
            ! !
            ! ! Obtain the data from master
            ! !
            ! call mpi_recv(f, n, MPI_DOUBLE_PRECISION, IMG_MASTERRANK, &
            !       IMG_TAG_DEFAULT, IMG_COMM_ALL, stat, ierr)
      end subroutine img_dfrommaster


      subroutine img_ksresults(ksmatrix, eel, exc, nel, div, kin, lap, matonly)
            ! ------------------------------------------------------------
            ! Gather partial results of the Kohn-Sham iteration obtained
            ! by slave images. Perform reduction of Kohn-Sham matrices,
            ! energy components, grid diagnostics, and other (generic)
            ! integrals evaluated on the molecular grid. If the MATONLY
            ! argument is not defined, both reduction of KSMATRIX and
            ! reduction of energies will be performed.
            ! ------------------------------------------------------------
            ! KSMATRIX - Partially built Kohn-Sham matrix. 
            ! EEL      - Total electronic energy
            ! EXC      - Numerical exchange-correlation contribution to
            !            the total electronic energy
            ! NEL      - Numerically integrated number of electrons
            ! DIV      - Numerically integrated divergence of electronic
            !            density
            ! KIN      - Numerically integrated kinetic energy (non-zero
            !            when meta-GGA mode is enabled)
            ! LAP      - Numerically integrated laplacian of the
            !            electronic density
            ! MATONLY  - (Optional) Do not reduce any results except
            !            the Kohn-Sham matrix
            !
            !
            double precision, dimension(:, :), intent(inout) :: ksmatrix
            double precision, intent(inout)                  :: eel
            double precision, intent(inout)                  :: exc
            double precision, intent(inout)                  :: nel
            double precision, intent(inout)                  :: div
            double precision, intent(inout)                  :: kin
            double precision, intent(inout)                  :: lap
            logical, optional, intent(in)                    :: matonly
            
            integer :: ierr
            logical :: lmatonly

            ! if (present(matonly)) then
            !       lmatonly = matonly
            ! else
            !       lmatonly = .false.
            ! end if

            ! call smpack(ksmatrix, SENDBUF)
            ! if (.not. lmatonly) then
            !       SENDBUF(BUFSIZE)     = eel
            !       SENDBUF(BUFSIZE - 1) = exc
            !       SENDBUF(BUFSIZE - 2) = nel
            !       SENDBUF(BUFSIZE - 3) = div
            !       SENDBUF(BUFSIZE - 4) = kin
            !       SENDBUF(BUFSIZE - 5) = lap
            ! end if

            ! call mpi_reduce(SENDBUF, RECVBUF, BUFSIZE, MPI_DOUBLE_PRECISION, &
            !       MPI_SUM, IMG_MASTERRANK, IMG_COMM_ALL, ierr)

            ! if (IMG_ISMASTER) then
            !       if (.not. lmatonly) then
            !             eel = RECVBUF(BUFSIZE)
            !             exc = RECVBUF(BUFSIZE - 1)
            !             nel = RECVBUF(BUFSIZE - 2)
            !             div = RECVBUF(BUFSIZE - 3)
            !             kin = RECVBUF(BUFSIZE - 4)
            !             lap = RECVBUF(BUFSIZE - 5)
            !       end if
            !       call smunpack(RECVBUF, ksmatrix)
            ! end if
      end subroutine img_ksresults


      subroutine img_reduce(b, operation_type)
            !
            ! Reduce a one-dimensional array
            !
            real(F64), dimension(:), intent(inout) :: b
            integer, intent(in)                    :: operation_type

            real(F64), dimension(:), allocatable :: sendbuf, recvbuf
            integer :: ierr
            integer :: msg_size
            
            ! msg_size = size(b)
            ! allocate(sendbuf(msg_size))
            ! allocate(recvbuf(msg_size))

            ! sendbuf = b
            ! call mpi_reduce(sendbuf, recvbuf, msg_size, MPI_DOUBLE_PRECISION, &
            !       operation_type, IMG_MASTERRANK, IMG_COMM_ALL, ierr)
            ! if (IMG_ISMASTER) then
            !       b = recvbuf
            ! end if

            ! deallocate(sendbuf)
            ! deallocate(recvbuf)
      end subroutine img_reduce


      subroutine img_ksbalance(numint)
            ! -----------------------------------------------
            ! Control the replicated-memory MPI build
            ! of the KS matrix in a single SCF iteration.
            ! Listen to slaves and send tasks if readiness
            ! for computing next batch of integrals is
            ! signalled.
            ! -----------------------------------------------
            ! NUMINT - Input, set to .TRUE. if any numerical
            !          integrals on the molecular grid are
            !          to be computed
            !
            logical, intent(in) :: numint
            ! 
            ! Dynamically balance Kohn-Sham matrix build
            ! in replicated data environment
            !
            integer, dimension(IMG_STATUS_SIZE) :: status
            integer :: ierr, dest
            integer, dimension(IMG_IMSGSIZE) :: inmsg
            integer, dimension(IMG_IMSGSIZE) :: outmsg
            integer :: nfocktask
            integer :: k

            ! call tasksort(.false.)
            ! !
            ! ! Numerical integrals on the molecular grid: 
            ! ! contributions to the XC matrix, XC energy,
            ! ! and auxiliary integrals
            ! !
            ! if (numint) then
            !       do k = 1, NATOM
            !             !
            !             ! Receive request from a slave process for a new task
            !             ! INMSG(1) - Message type
            !             ! INMSG(2) - Rank of the message source
            !             !
            !             call mpi_recv(inmsg, IMG_IMSGSIZE, MPI_INTEGER, MPI_ANY_SOURCE, &
            !                   IMG_TAG_DEFAULT, IMG_COMM_ALL, status, ierr)
            !             !
            !             ! Send new task via non-blocking ISEND
            !             ! OUTMSG(1) - Message type
            !             ! OUTMSG(2) - Index of the task
            !             !
            !             if (inmsg(1) .eq. IMG_MSG_NEXTTASK) then
            !                   outmsg(1) = IMG_MSG_XCGEN
            !                   outmsg(2) = xctask(k)
            !                   dest = inmsg(2)
            !                   call mpi_send(outmsg, IMG_IMSGSIZE, MPI_INTEGER, dest, &
            !                         IMG_TAG_DEFAULT, IMG_COMM_ALL, ierr)
            !             end if
            !       end do
            ! end if
            ! !
            ! ! HF-like exchange and Coulomb matrices
            ! !
            ! nfocktask = (NATOM * (NATOM + 1)) / 2
            ! do k = 1, nfocktask
            !       !
            !       ! Receive request from a slave process for a new task
            !       ! INMSG(1) - Message type
            !       ! INMSG(2) - Rank of the message source
            !       !
            !       call mpi_recv(inmsg, IMG_IMSGSIZE, MPI_INTEGER, MPI_ANY_SOURCE, &
            !             IMG_TAG_DEFAULT, IMG_COMM_ALL, status, ierr)
            !       !
            !       ! Send new task via non-blocking ISEND
            !       ! OUTMSG(1) - Message type
            !       ! OUTMSG(2) - Index of the task
            !       !
            !       if (inmsg(1) .eq. IMG_MSG_NEXTTASK) then
            !             outmsg(1) = IMG_MSG_FOCKGEN
            !             call iunpack(focktask(k), outmsg(2), outmsg(3))
            !             dest = inmsg(2)
            !             call mpi_send(outmsg, IMG_IMSGSIZE, MPI_INTEGER, dest, &
            !                   IMG_TAG_DEFAULT, IMG_COMM_ALL, ierr)
            !       end if
            ! end do
      end subroutine img_ksbalance
end module images
