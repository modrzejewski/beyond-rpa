module rpa_definitions
      use arithmetic
      use grid_definitions
      
      implicit none
      !
      ! Mean-field part of the adiabatic connection hamiltonian
      ! ---
      !
      ! F(Lambda) = (1-Lambda)hKS + Lambda*hHF
      ! (lambda-dependent semicanonical orbitals along the adiabatic connection path)
      !
      integer, parameter :: RPA_MEAN_FIELD_KS_TYPE = 1
      !
      ! F(Lambda) = hHF(OO+VV) + Lambda*hHF(VO+OV)
      ! (constant semicanonical orbitals along the adiabatic connection path)
      !
      integer, parameter :: RPA_MEAN_FIELD_HF_TYPE = 2
      
      integer, parameter :: RPA_ORBITALS_CANONICAL = 1
      integer, parameter :: RPA_ORBITALS_SEMICANONICAL = 2
      !
      ! Approximation levels of the T1 amplitudes
      ! -----------------------------------------
      !
      integer, parameter :: RPA_T1_MEAN_FIELD = 1
      integer, parameter :: RPA_T1_DIRECT_RING_CCSD = 3
      integer, parameter :: RPA_T1_DIRECT_RING_CCSD_PLUS_EXCHANGE = 4
      integer, parameter :: RPA_T1_DIRECT_RING_CCSD_PLUS_EXCHANGE_PLUS_LADDER = 5
      !
      ! Approximation of the exchange contribution
      !
      integer, parameter :: RPA_EXCHANGE_NONE = 0
      integer, parameter :: RPA_EXCHANGE_SOSEX = 1
      integer, parameter :: RPA_EXCHANGE_CUMULANT_LINEAR = 2
      integer, parameter :: RPA_EXCHANGE_MBPT3_1 = 4
      integer, parameter :: RPA_EXCHANGE_MBPT3_2 = 5
      integer, parameter :: RPA_EXCHANGE_MBPT3_1_NUMERICAL = 6 ! For debugging only
      !
      ! Approximation of the 1RDM energy contribution
      ! to the correlation energy
      !
      integer, parameter :: RPA_Ec1RDM_NONE = 0
      integer, parameter :: RPA_Ec1RDM_LINEAR = 1
      integer, parameter :: RPA_Ec1RDM_QUADRATIC = 2
      integer, parameter :: RPA_Ec1RDM_NATURAL_REFERENCE = 5
      !
      ! Algorithm/approximation used for the computation
      ! of Ec1RDM_Quadratic
      !
      integer, parameter :: RPA_MEAN_FIELD_COUL_EXCH = 1
      !
      ! Approximation of the lambda-dependent 1RDM matrix
      !
      integer, parameter :: RPA_RHO_T1_LINEAR = 1
      integer, parameter :: RPA_RHO_T1_QUADRATIC = 5
      integer, parameter :: RPA_RHO_T1_EXPONENTIAL = 6
      integer, parameter :: RPA_RHO_S1_EXPONENTIAL = 4
      integer, parameter :: RPA_RHO_OFF_DIAGONAL_drCCSD = 7
      integer, parameter :: RPA_RHO_drCCSD = 8
      integer, parameter :: RPA_RHO_drCCSD_PLUS_EXCHANGE = 9
      !
      ! Method for restoring the N-representability of 1-RDM
      !
      integer, parameter :: RPA_PURIFY_RHO_NONE = 0
      integer, parameter :: RPA_PURIFY_RHO_KLIMES2015 = 1
      integer, parameter :: RPA_PURIFY_RHO_CANCES_PERNAL2008 = 2
      !
      ! Basis set for the effective dielectric matrix
      !
      ! Gaussian random vectors; RWR is built using accurate
      ! floating point arithmetic and tight thresholds for W
      !
      integer, parameter :: RPA_BASIS_RANDOM = 1
      !
      ! Eigenvectors of RWR; RWR is built using accurate
      ! floating point arithmetic and tight thresholds for W
      !
      integer, parameter :: RPA_BASIS_EIGEN = 2
      !
      ! Full set of Cholesky vectors. Using this option
      ! eliminates possible errors resulting from using
      ! only a subspace of the dominant eigenvectors
      ! of Pi(u). 
      !
      integer, parameter :: RPA_BASIS_FULL_CHOLESKY = 3
      !
      ! Constraints on the number of points used for
      ! the frequency and Laplace quadratures
      !
      integer, parameter :: RPA_MIN_NFREQS = 8
      integer, parameter :: RPA_MAX_NFREQS = 64
      integer, parameter :: RPA_MAX_NLAPLACE = 250

      integer, parameter :: RPA_HISTOGRAM_NBINS = 100
      ! ---------------------------------------------------------
      ! MEMORY ACCESS ON REMOTE IMAGES
      ! ---------------------------------------------------------
      !
      ! Max number of array elements transferred in a single batch
      !
      integer, parameter :: RPA_MAX_TRANSFER = 10**6
      ! ---------------------------------------------------------
      ! Singles correction
      !
      ! This setting should be used for compatibility with the RPA formulations
      ! of Klimes et al. (renormalized singles energy) and Ren at al. (rPT2).
      ! The code path for coupled-cluster formulation of RPA uses various
      ! approximations of Ec1RDM instead of this setting.
      !
      ! 1. Klimes et al.: Eq. 33 in J. Chem. Phys. 143, 102816 (2015); doi: 10.1063/1.4929346
      ! 2. Ren et al.: Eq. 25 in Phys. Rev. B 88, 035120 (2013); doi: 10.1103/PhysRevB.88.035120
      ! ---------------------------------------------------------
      integer, parameter :: RPA_SINGLES_NONE = 0
      integer, parameter :: RPA_SINGLES_KLIMES = 1
      integer, parameter :: RPA_SINGLES_REN = 2
      ! ---------------------------------------------------------
      !         RPA and beyond-RPA energy components
      !----------------------------------------------------------
      integer, parameter :: RPA_ENERGY_NCOMPONENTS = 36 ! includes some unused fields
      !
      ! Total energy of the SCF step preceeding RPA calculations
      !
      integer, parameter :: RPA_ENERGY_DFT = 1
      !
      ! EtotHF = nuclei-electrons interaction, HF Hartree, Coulomb,
      ! and exchange terms. Computed using the converged SCF orbitals.
      !
      integer, parameter :: RPA_ENERGY_HF = 2
      !
      ! Sum of all computed RPA and beyond-RPA energy components
      ! EtotRPA = EtotHF + EcRPA + all beyond-dRPA components
      !
      integer, parameter :: RPA_ENERGY_TOTAL = 3
      ! ---------------------------------------------------------
      !    1-RDM contribution to the RPA correlation energy
      ! ---------------------------------------------------------
      ! Traditional RPA codepath: singles energy correction
      ! of Klimes et al. or the correction of Ren et al.
      !
      ! Coupled-clusters codepath: DeltaRho(Lambda) contributions
      ! to the correlation energy integrated over Lambda
      !
      integer, parameter :: RPA_ENERGY_SINGLES = 4
      integer, parameter :: RPA_ENERGY_1RDM_LINEAR = 5
      integer, parameter :: RPA_ENERGY_1RDM_QUADRATIC = 6
      integer, parameter :: RPA_ENERGY_1RDM_QUADRATIC_DIRECT = 7
      integer, parameter :: RPA_ENERGY_1RDM_QUADRATIC_EXCHANGE = 8
      ! ------------------------------------------------------------------
      !                 Direct RPA correlation energy
      ! ------------------------------------------------------------------
      integer, parameter :: RPA_ENERGY_CORR = 9
      integer, parameter :: RPA_ENERGY_CORR_NUMERICAL_QUAD_PiU = 10
      integer, parameter :: RPA_ENERGY_CORR_NUMERICAL_QUAD_T2 = 11
      integer, parameter :: RPA_ENERGY_CORR_CAN_DIRECT_RPA = 12
      integer, parameter :: RPA_ENERGY_CORR_SEMI_DIRECT_RPA = 13
      ! ------------------------------------------------------------------
      ! Second- and higher-order MBPT exchange contributions
      ! The following exchange contributions exclude the exchange terms
      ! present in the 1-RDM energy.
      ! ------------------------------------------------------------------
      integer, parameter :: RPA_ENERGY_EXCHANGE = 14
      integer, parameter :: RPA_ENERGY_EXCH_MBPT3_1 = 15
      integer, parameter :: RPA_ENERGY_EXCH_MBPT3_2 = 16
      integer, parameter :: RPA_ENERGY_EXCH_SOSEX = 17
      integer, parameter :: RPA_ENERGY_EXCH_CUMULANT = 18
      ! ------------------------------------------------------------------
      ! Correlation energy contributions in the cumulant formulation
      ! ------------------------------------------------------------------
      integer, parameter :: RPA_ENERGY_DIRECT_RING = 20
      integer, parameter :: RPA_ENERGY_CUMULANT_1B = 21
      integer, parameter :: RPA_ENERGY_CUMULANT_2B = 22
      integer, parameter :: RPA_ENERGY_CUMULANT_2C = 23
      integer, parameter :: RPA_ENERGY_CUMULANT_2D = 24
      integer, parameter :: RPA_ENERGY_CUMULANT_2E = 25
      integer, parameter :: RPA_ENERGY_CUMULANT_2F = 26      
      integer, parameter :: RPA_ENERGY_CUMULANT_2G = 27
      integer, parameter :: RPA_ENERGY_CUMULANT_2H = 28
      integer, parameter :: RPA_ENERGY_CUMULANT_2I = 29
      integer, parameter :: RPA_ENERGY_CUMULANT_2J = 30
      integer, parameter :: RPA_ENERGY_CUMULANT_2K = 31
      integer, parameter :: RPA_ENERGY_CUMULANT_2L = 32
      integer, parameter :: RPA_ENERGY_CUMULANT_2M = 33
      integer, parameter :: RPA_ENERGY_CUMULANT_2N = 34
      integer, parameter :: RPA_ENERGY_CUMULANT_2O = 35
      integer, parameter :: RPA_ENERGY_CUMULANT_2P = 36

      integer, dimension(2), parameter :: RPA_CORRELATION_TERMS = [RPA_ENERGY_DIRECT_RING, RPA_ENERGY_CUMULANT_2P]

      integer, parameter :: RPA_CUMULANT_LEVEL_0 = 0          ! RPA
      integer, parameter :: RPA_CUMULANT_LEVEL_DEFAULT = 10   ! RPA + 1b (SOSEX) + 2g + 2b + 2c
      integer, parameter :: RPA_CUMULANT_LEVEL_1_HALF_THC = 1 ! RPA + 1b (SOSEX) + 2g
      integer, parameter :: RPA_CUMULANT_LEVEL_1_FULL_THC = 2 ! RPA + 1b (SOSEX) + 2g
      integer, parameter :: RPA_CUMULANT_LEVEL_2_HALF_THC = 3 ! RPA + 1b (SOSEX) + 2g + 2m + 2n + 2o + 2p
      integer, parameter :: RPA_CUMULANT_LEVEL_3_HALF_THC = 4 ! RPA + 1b (SOSEX) + 2g + 2b + 2c
      integer, parameter :: RPA_CUMULANT_LEVEL_3_FULL_THC = 5 ! RPA + 1b (SOSEX) + 2g + 2b + 2c
      integer, parameter :: RPA_CUMULANT_LEVEL_4_HALF_THC = 6 ! RPA + 1b (SOSEX) + 2g + 2b + 2c + 2e + 2h + 2k
      integer, parameter :: RPA_CUMULANT_LEVEL_4_FULL_THC = 7 ! RPA + 1b (SOSEX) + 2g + 2b + 2c + 2e + 2h + 2k
      integer, parameter :: RPA_CUMULANT_LEVEL_5_HALF_THC = 8
      
      type TRPAParams
            logical :: MOAlgorithm = .true.
            !
            ! The most important RPA energy threshold. Controls
            ! the size of the random vector basis G. The random vectors
            ! are appended to the basis set until
            !
            ! Tr(Pi(u)-G*Pi(u)*G) < TargetErrorRandom
            !
            ! The error in energy is a quadratic function
            ! of TargetErrorRandom.
            !
            real(F64) :: TargetErrorRandom = sqrt(1.0E-5_F64)
            !
            ! Threshold for screening diagonal elements,
            ! Eq. 17 in J. Chem. Phys. 150, 194112 (2019);
            ! doi: 10.1063/1.5083802
            !
            real(F64) :: CholeskyTauThresh = 1.0E-7_F64
            !
            ! Accuracy threshold for the numerical grid optimization
            ! for the Laplace transform of dai/(dai**2+u**2)
            !
            real(F64) :: TargetErrorLaplace = 1.0E-6_F64
            real(F64) :: TargetRelErrorLaplace = 1.0E-3_F64
            !
            ! Accuracy threshold for the frequency numerical grid optimization
            ! 
            real(F64) :: TargetErrorFreq = 1.0E-6_F64
            real(F64) :: TargetRelErrorFreq = 1.0E-3_F64
            !
            ! Exclude the occupied orbitals with energies Ei<CoreOrbThresh
            ! from all summations
            !
            real(F64) :: CoreOrbThresh = -3.0_F64
            !
            ! Threshold controlling the exclusion of redundant eigenvectors of T2.
            !
            real(F64) :: SmallEigenvalCutoffT2 = 1.0E-10_F64
            !
            ! Range separation parameter defining the screened
            ! interaction potential Erf(Omega*r12)/r12:
            !
            ! Kappa=1/Omega**2
            !
            ! Kappa=0 means that the full range Coulomb
            ! potential is used.
            !
            real(F64) :: Kappa = 0.0_F64
            !
            ! Maximum number of Cholesky vectors, expressed
            ! as a multiplicity of the basis set size.
            ! If the spherical AO basis set size is NAO, the max number
            ! of Cholesky vectors is MaxNCholesky*NAO.
            !
            real(F64) :: MaxNAOMult = 10.0_F64
            !
            ! Singles correction used in the direct
            ! random-phase approximation code. Note that this
            ! setting is not used by the newer code for beyond-RPA
            ! in the coupled clusters formulation.
            ! ------------------
            ! Build Hartree-Fock Hamiltonian using Kohn-Sham orbitals
            ! and diagonalize it to get the singles correction according
            ! to Eq. 33 in J. Chem. Phys. 143, 102816 (2015); doi: 10.1063/1.4929346
            !
            integer :: SinglesCorrection = RPA_SINGLES_KLIMES
            !
            ! Disable RPA correlation. Only the reminder of the RPA energy
            ! will be computed.
            !
            logical :: DisableCorrelation = .false.
            !
            ! Type of vectors used for the effective dielectric matrix
            !
            integer :: RWRBasisType = RPA_BASIS_RANDOM
            !
            ! Maximum size of a block of the matrix W(pq,rs). W is partitioned into blocks,
            ! which are then passed to the matrix multiplication subroutine to compute
            ! WRG(:, rs) <- RG(:, pq) * W(pq, rs). The size of the block should be big enough
            ! to fit into the range where the matrix multiplication subroutine achieves
            ! high floating point operations per second. In addition, larger MaxBlockDim
            ! yields better shared-memory parallel computation of W(pq,rs) within each block.
            ! 
            integer :: MaxBlockDim = 4000
            !
            ! Maximum size of a single batch of T2 eigenvectors processed at the same time.
            ! See the algorithm used for the diagonalization of the T2 amplitude matrix.
            !
            integer :: MaxBatchDimT2 = 100
            !
            ! Convergence threshold for the computation of model multiplicative potentials
            ! by inversion of the Kohn-Sham equations.
            !
            real(F64) :: TargetErrorModelVxc = 1.0E-4_F64
            !
            ! Limit orbital energy differences Ea-Ei on subsystems
            ! where ghost centers are present. If this option is enabled,
            ! the maximum difference is capped at the value of the maximum
            ! difference for the full molecular complex. Usually the orbital
            ! excitations for the subsystems are orders of magnitude higher
            ! because of the presence of atomic basis functions away from 
            ! real atoms. That phenomenon might adversely affect the numerical
            ! grid optimization. Numerical tests show that ignoring those
            ! energy differences during grid optimization is safe and does not
            ! affect the grid quality.
            !
            logical :: GridLimitDai = .true.
            !
            ! Compute the Cholesky vectors in the RPA step.
            ! Set to true if the Cholesky vectors aren't
            ! already computed at the SCF step.
            !
            logical :: ComputeCholeskyBasis = .true.
            !
            ! Correction for non-constant density along
            ! the adiabatic connection
            !
            logical :: CoupledClusters = .false.
            !
            ! Number of points of the Gauss-Legendre quadrature
            ! used for the adiabatic connection integral on the (0,1)
            ! interval
            !
            integer :: ACQuadPoints = 4
            !
            ! Approximation of the T1 amplitudes
            !
            integer :: T1Approx = RPA_T1_DIRECT_RING_CCSD_PLUS_EXCHANGE
            !
            ! Apply linear interpolation between Lambda=0 and Lambda=1
            ! to compute the T2 amplitudes that enter the singles equation
            ! and the formula for DeltaRho(Lambda). 
            !
            logical :: T2Interp = .false.
            !
            ! Approximation of the exchange contribution
            !
            integer :: ExchangeApprox = RPA_EXCHANGE_MBPT3_1
            !
            ! Approximation of the 1-RDM contribution
            !
            integer :: Ec1RDMApprox = RPA_Ec1RDM_LINEAR
            !
            ! Algorithm/approximation used for the computations of
            ! the mean-field energy change
            !
            integer :: MeanFieldApprox = RPA_MEAN_FIELD_COUL_EXCH
            !
            ! Parameter controlling the number of random vectors used
            ! as the guess for computing the eigenvectors of the RPA
            ! doubles amplitudes. The number of random vectors is computed
            ! as
            !
            ! min(NCholesky, ceiling(GuessNVecsT2*NVecsPiU))
            !
            ! This parameter will be ignored if the full Cholesky
            ! basis is enabled. In that case, the number of guess
            ! random vectors is equal to the number of
            ! the Cholesky vectors.
            !
            real(F64) :: GuessNVecsT2 = 3.0_F64
            !
            ! Approximation of the 1-RDM matrix computed
            ! for each lambda of the adiabatic connection
            !
            integer :: DensityApprox = RPA_RHO_T1_EXPONENTIAL
            !
            ! Method used for restoring N-representability of 1-RDM
            !
            integer :: Purify1RDM = RPA_PURIFY_RHO_NONE
            !
            ! Orbitals used in the amplitude equations and for the computation
            ! of the RPA correlation energy
            !
            integer :: ChiOrbitals = RPA_ORBITALS_CANONICAL
            integer :: MeanField = RPA_MEAN_FIELD_HF_TYPE
            ! --------------------------------------------------------------------
            ! Tensor hypercontraction of the Coulomb integrals
            ! --------------------------------------------------------------------
            ! 1. Robert M. Parrish, Edward G. Hohenstein, Todd J. Martinez,
            !    and C. David Sherrill, Tensor hypercontraction. II. Least-squares
            !    renormalization, J. Chem. Phys. 137, 224106 (2012);
            !     doi: 10.1063/1.4768233
            !
            ! 2. Devin A. Matthews, Improved Grid Optimization and
            !    Fitting in Least Squares Tensor Hypercontraction
            !    J. Chem. Theory Comput. 16, 1382 (2020);
            !    doi: 10.1021/acs.jctc.9b01205 
            !
            logical :: TensorHypercontraction = .false.
            integer   :: THC_BeckeGridKind = BECKE_PARAMS_SG1
            real(F64) :: THC_QRThresh = 1.0E-3_F64
            real(F64) :: THC_QRThresh_T2 = 1.0E-5_F64
            integer   :: THC_BlockDim = 500
            integer :: CumulantApprox = RPA_CUMULANT_LEVEL_1_HALF_THC
            !
            ! Use numerical integration to evaluate Ec1RDMQuad.
            ! If false, the integrand is evaluated only at Lambda=1
            ! with weight equal to 1.
            !
            logical :: AC_1RDMQuad = .false.
      end type TRPAParams

      type TRPAGrids
            !
            ! Numerical grids for the frequency and Laplace transform integrals.
            ! The grids should be kept the same for the whole system
            ! and its subsystems to have size-extensive energy differences.
            !
            logical :: ComputeGrids = .true.
            integer :: NFreqs
            real(F64), dimension(:), allocatable :: Freqs
            real(F64), dimension(:), allocatable :: FreqWeights
            integer, dimension(:), allocatable :: NLaplace
            real(F64), dimension(:, :), allocatable :: LaplaceX
            real(F64), dimension(:, :), allocatable :: LaplaceW
            real(F64), dimension(:, :), allocatable :: daiValues
            real(F64), dimension(:, :), allocatable :: daiWeights
      end type TRPAGrids
      
      type TRPABasis
            !
            ! Specification of the AO indices used to represent the basis
            ! for the effective dielectric matrix. Note that the basis vectors
            ! are kept the same for the whole system and its subsystems
            ! to have size-extensive energy differences.
            !
            logical :: ComputeRWRBasis = .true.
            integer :: NVecs = 0
      end type TRPABasis

      type TMeanField
            real(F64), dimension(:, :, :), allocatable :: OccCoeffs_ao
            real(F64), dimension(:, :, :), allocatable :: VirtCoeffs_ao
            real(F64), dimension(:, :), allocatable :: OrbEnergies
            real(F64), dimension(:, :, :), allocatable :: F_ao
            real(F64)             :: EtotHF = 0
            real(F64)             :: Ec1RDM_Linear = 0
            real(F64)             :: Ec1RDM_Quadratic = 0
            integer               :: NSpins = 0
            integer, dimension(2) :: NOcc = 0
            integer, dimension(2) :: NVirt = 0
      end type TMeanField
      
contains

      subroutine rpa_Params_Default(p)
            type(TRPAParams), intent(inout) :: p

            p%TargetErrorRandom = min(sqrt(1.0E-5_F64), p%TargetErrorRandom)
      end subroutine rpa_Params_Default

      
      subroutine rpa_Params_Tight(p)
            type(TRPAParams), intent(inout) :: p

            p%TargetErrorRandom = min(sqrt(1.0E-7_F64), p%TargetErrorRandom)
      end subroutine rpa_Params_Tight


      subroutine rpa_Params_Ludicrous(p)
            type(TRPAParams), intent(inout) :: p

            p%TargetErrorRandom = min(1.0E-5_F64, p%TargetErrorRandom)
      end subroutine rpa_Params_Ludicrous
end module rpa_definitions
