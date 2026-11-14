module thc_definitions
      use arithmetic
      use math_constants
      use string
      use io, only: io_text_open, io_text_readline
      use display
      use grid_definitions
      use periodic

      implicit none
      !
      ! Lightest element with a minimum Lebedev order of the THC parent
      ! grid around its nucleus. From sodium on, the core shells of p and
      ! higher angular momentum need more angular points than SG-1 gives.
      ! H-Ne have at most a 1s core and keep the SG-1 orders.
      !
      integer, parameter :: THC_CORE_FLOOR_ZMIN = 11

      type TCoulTHCGrid
            integer :: NGrid
            integer :: NGridReduced
            !
            ! Collocation matrix: X(g, p) = Phi_p(Xg, Yg, Zg)
            ! where Phi_p is an AO orbital p evaluated at a grid point g.
            !
            ! g = 1, 2, ..., NGrid (number of grid points of the THC grid)
            ! p = 1, 2, ..., NAO (number of atomic orbitals)
            ! NGrid = size(Xgp, dim=1)
            ! NAO = size(Xgp, dim=2)
            !
            ! The AO values stored in X correspond to spherically transformed
            ! GTOs if a spherical basis is used.
            !
            real(F64), dimension(:, :), allocatable :: Xgp
            !
            ! 2-index renormalized Coulomb operator
            ! -------------------------------------
            ! Two-electron Coulomb integrals are reconstructed from
            !
            ! (pq|rs) = Sum(gh) X(g,p)*X(g,q)*Z(g,h)*X(h,r)*X(h,s)
            ! g = 1, 2, ..., NGrid
            ! h = 1, 2, ..., NGrid (number of grid points of the THC grid)            
            !
            ! The full matrix Z(g,h) is not stored. Instead, Z'(g,k) is
            ! the factorized form with k corresponding to kth Cholesky
            ! vector of the Coulomb matrix
            !
            ! Z(g,h) = Sum(k) Z'(g,k)*Z'(h,k)
            ! k = 1, 2, ..., NCholesky
            ! g = 1, 2, ..., NGrid
            ! NCholesky = size(Zgk, dim=2)
            ! NGrid = size(Zgk, dim=1)
            !
            ! In numerical tests NCholesky is several times smaller
            ! than NGrid.
            !
            real(F64), dimension(:, :), allocatable :: Zgk
            real(F64), dimension(:, :), allocatable :: ZgkReduced
            !
            ! Transformed version of Z'(g,k) where the k index
            ! corresponds to the basis of Pi(u)
            ! (Eqs. 28 and 29 in Ref. 1)
            ! 
            ! ZPiU(g,k) = Z'(g,l)*G(l,k)
            ! g = 1, 2, ..., NGrid
            ! k = 1, 2, ..., NVecsPiU
            !
            ! 1. M. Modrzejewski, S. Yourdkhani, J. Klimes,
            !    J. Chem. Theory Comput. 16, 427 (2020);
            !    doi: 10.1021/acs.jctc.9b00979
            !
            real(F64), dimension(:, :), allocatable :: ZgkPiU
      end type TCoulTHCGrid

      type TTHCParams
            integer   :: BeckeGridKind = BECKE_PARAMS_SG1
            !
            ! Pivot thresholds of the SCF (QRThresh) and post-SCF
            ! (QRThreshReduced) steps
            !
            real(F64) :: QRThresh = 1.0E-4_F64
            real(F64) :: QRThreshReduced = 1.0E-3_F64
            integer   :: BlockDim = 500
            logical   :: QuadraticMemory = .true.
            !
            ! PhiSquaredThresh controls the removal of THC grid points where
            ! atomic orbitals have small values. If for all k |Psi(rk)|**2
            ! <= PhiSquaredThresh, the kth point is discarded. The SCF and
            ! post-SCF energies depend on this threshold very weakly. However,
            ! a too small PhiSquaredThresh results in poor SCF convergence.
            ! In the most difficult case I have found, trimers of benzene
            ! in aug-cc-pVDZ basis require PhiSquaredThresh=1.0E-11 to converge.
            !
            real(F64) :: PhiSquaredThresh = 1.0E-11_F64
            !
            ! Minimum Lebedev order of every radial shell of the parent
            ! grid, indexed by the atomic number. The order is the degree L
            ! of the spherical harmonics integrated exactly by the Lebedev
            ! grid (3, 5, 7, ..., 65), not the index of the grid in the
            ! Lebedev table. An order between two table values selects the
            ! next larger grid. Zero keeps the orders of BeckeGridKind.
            ! The keyword LebedevLmin sets every element. On top of it,
            ! thc_CoreLebedevLmin raises the elements from sodium on to the
            ! order their core shells need.
            !
            integer, dimension(KNOWN_ELEMENTS) :: LebedevLmin = 0
            !
            ! Number of radial shells of the parent grid. Zero keeps
            ! the number defined by BeckeGridKind.
            !
            integer   :: NRadial = 0
      contains
            procedure :: read_line => thc_ReadLine
            procedure :: read_block => thc_ReadBlock
      end type TTHCParams

contains

      subroutine thc_ReadBlock(THCParams, FilePath)
            !
            ! Read the thc block of an input file. The block is optional.
            ! Every keyword keeps its default when absent.
            !
            class(TTHCParams), intent(inout) :: THCParams
            character(*), intent(in)         :: FilePath

            logical :: InsideBlock, BlockCompleted
            integer :: u
            character(:), allocatable :: key, val
            character(:), allocatable :: line
            logical :: eof

            u = io_text_open(FilePath, "OLD")
            InsideBlock = .false.
            BlockCompleted = .false.
            lines: do
                  call io_text_readline(line, u, eof)
                  if (eof .and. len(line) == 0) exit lines
                  if (isblank(line) .or. iscomment(line)) cycle lines
                  call split(line, key, val)
                  key = uppercase(key)
                  if (key == "THC" .and. .not. InsideBlock) then
                        InsideBlock = .true.
                        cycle lines
                  else if (key == "END") then
                        if (InsideBlock) then
                              InsideBlock = .false.
                              BlockCompleted = .true.
                              exit lines
                        else
                              cycle lines
                        end if
                  else if (InsideBlock) then
                        call THCParams%read_line(line)
                  end if
            end do lines
            close(u)

            if (InsideBlock .and. .not. BlockCompleted) then
                  call msg("Unexpected end of file while reading the thc block. " // &
                        "Missing END keyword.", MSG_ERROR)
                  error stop
            end if
      end subroutine thc_ReadBlock


      subroutine thc_ReadLine(THCParams, line)
            !
            ! Read one keyword line of the thc block
            !
            class(TTHCParams), intent(inout) :: THCParams
            character(*), intent(in)         :: line

            character(:), allocatable :: key, val, s1, s2
            real(F64) :: a
            integer :: i

            call split(line, key, val)
            select case (uppercase(key))
            case ("QRTHRESH")
                  call split(val, s1, s2)
                  if (len(s2) == 0) then
                        call msg("Missing value of QRThresh: " // &
                              "use QRThresh scf|post-scf <value>", MSG_ERROR)
                        error stop
                  end if
                  read(s2, *) a
                  if (a <= ZERO) then
                        call msg("QRThresh must be positive", MSG_ERROR)
                        error stop
                  end if
                  select case (uppercase(s1))
                  case ("SCF")
                        THCParams%QRThresh = a
                  case ("POST-SCF", "POSTSCF", "POST_SCF")
                        THCParams%QRThreshReduced = a
                  case default
                        call msg("Invalid stage of QRThresh: " // s1 // &
                              " (expected scf or post-scf)", MSG_ERROR)
                        error stop
                  end select
            case ("PHISQUAREDTHRESH")
                  read(val, *) a
                  if (a <= ZERO) then
                        call msg("PhiSquaredThresh must be positive", MSG_ERROR)
                        error stop
                  end if
                  THCParams%PhiSquaredThresh = a
            case ("BLOCKDIM")
                  read(val, *) i
                  if (i <= 0) then
                        call msg("BlockDim must be positive", MSG_ERROR)
                        error stop
                  end if
                  THCParams%BlockDim = i
            case ("LEBEDEVLMIN")
                  read(val, *) i
                  if (i <= 0) then
                        call msg("LebedevLmin must be positive", MSG_ERROR)
                        error stop
                  end if
                  THCParams%LebedevLmin = i
            case ("NRADIAL")
                  read(val, *) i
                  if (i <= 0) then
                        call msg("NRadial must be positive", MSG_ERROR)
                        error stop
                  end if
                  THCParams%NRadial = i
            case ("BECKEGRIDKIND")
                  select case (uppercase(val))
                  case ("SG1", "SG-1")
                        THCParams%BeckeGridKind = BECKE_PARAMS_SG1
                  case ("MEDIUM")
                        THCParams%BeckeGridKind = BECKE_PARAMS_MEDIUM
                  case ("FINE")
                        THCParams%BeckeGridKind = BECKE_PARAMS_FINE
                  case ("XFINE")
                        THCParams%BeckeGridKind = BECKE_PARAMS_XFINE
                  case default
                        call msg("Unknown BeckeGridKind: " // val, MSG_ERROR)
                        error stop
                  end select
            case default
                  call msg("Unknown keyword in the thc block: " // key, MSG_ERROR)
                  error stop
            end select
      end subroutine thc_ReadLine
end module thc_definitions
