module TwoStepCholesky_definitions
      use arithmetic
      
      implicit none
      !
      ! Orbital pair subsets      
      !
      integer, parameter :: CHOL2_BASE = 1        ! All orbital pairs accepted after the initial Schwarz prescreening
      integer, parameter :: CHOL2_CANDIDATES = 2  ! Pivot candidates in a macroiteration of the pivot finding algorithm
      integer, parameter :: CHOL2_BATCH = 3       ! Subset of pivot candidates in a microiteration of the pivot finding algorithm
      !
      ! Orbital pair storage modes
      ! used in the ShellPairLoc array
      !
      integer, parameter :: CHOL2_FULL_STORAGE = 1        ! All orbital pairs of the BASE subset are stored
      integer, parameter :: CHOL2_COMPRESSED_STORAGE = 2  ! Only the orbital pairs of the CANDIDATES subset are stored
      integer, parameter :: CHOL2_SUBSET_STORAGE = 3      ! Orbital pairs of the BASE subset are divided into subsets
      integer, parameter :: CHOL2_SUBSET_INDEX = 4        ! of size at most equal to MaxBlockDim. Used for parallelization.

      type TChol2Vecs
            real(F64), dimension(:, :), allocatable :: Inv_L
            integer :: NVecs
            integer :: MaxSubsetDim
            integer, dimension(2) :: NSubsets
            integer :: NOrbPairs
            integer :: NShellPairs
            integer, dimension(:, :), allocatable :: ShellPairs
            integer, dimension(:, :), allocatable :: ShellPairLoc
            integer, dimension(:), allocatable :: ShellPairDim
            integer, dimension(:), allocatable :: SubsetDim
            integer, dimension(:, :), allocatable :: SubsetBounds
            integer, dimension(:), allocatable :: Pivots
            integer, dimension(:), allocatable :: PivotShellPairs
            integer, dimension(:), allocatable :: PivotShellPairLoc
            integer, dimension(:), allocatable :: PivotShellPairDim
            integer, dimension(:), allocatable :: PivotOrbPairs
            integer :: NPivotShellPairs
      end type TChol2Vecs

      type TCompressedCholVecs
            real(F64), dimension(:, :), allocatable :: L
      end type TCompressedCholVecs
end module TwoStepCholesky_definitions
