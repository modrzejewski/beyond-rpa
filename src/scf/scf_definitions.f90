module scf_definitions
      use arithmetic
      use math_constants
      use h_xcfunc
      use h_io
      
      implicit none
      !
      ! Methods of generating zeroth-iteration molecular orbitals
      ! ---
      ! Method 1: Compute eigenvectors of bare-nuclei Hamiltonian, that is,
      ! a single-electron Hamiltonian containing only kinetic term and 
      ! external potential.
      !
      integer, parameter :: SCF_GUESS_HBARE = 1
      !
      ! Method 2: First construct zeroth-iteration electronic density by
      ! superimposing spherically symmetric atomic densities. This density
      ! always corresponds to a neutral molecule. Build Kohn-Sham (Hartree-Fock)
      ! matrix. Diagonalize this matrix to get the zeroth-iteration molecular
      ! orbitals.
      !
      integer, parameter :: SCF_GUESS_ATOMIC = 2
      !
      ! Method 3. Read density matrix from file stored on disk. Build
      ! Kohn-Sham (Hartree-Fock) matrix. Diagonalize this matrix to get
      ! the zeroth-iteration molecular orbitals.
      ! 
      integer, parameter :: SCF_GUESS_TEXT_FILE = 3
      integer, parameter :: SCF_GUESS_BINARY_FILE = 6
      !
      ! Method 4. Use the molecular orbitals supplied to the SCF subprogram.
      ! In this method, no single-particle Hamiltonian is diagonalized in order
      ! to obtain the guess molecular orbitals.
      !
      integer, parameter :: SCF_GUESS_MOVEC = 4
      !
      ! Method 5. Use the density matrix supplied to the SCF subprogram. Build
      ! Kohn-Sham matrix out of this density. Diagonalize KS matrix to get the 
      ! zeroth-iteration molecular orbitals.
      !
      integer, parameter :: SCF_GUESS_RHO = 5
      integer, parameter :: SCF_GUESS_DEFAULT = SCF_GUESS_ATOMIC
      !
      ! Numerical grid settings (more infor in the grid module)
      ! 1 - SG-1
      ! 2 - MEDIUM
      ! 3 - FINE
      ! 4 - EXTRA FINE
      !
      integer, parameter          :: GRD_SG1           = 1
      integer, parameter          :: GRD_MEDIUM        = 2
      integer, parameter          :: GRD_FINE          = 3
      integer, parameter          :: GRD_XFINE         = 4
      !
      ! Grid diagnostics
      !
      type TGridDiag
            !
            ! Number of electrons integrated on the molecular grid.
            ! Exact value: the number of electrons in the system.
            !
            real(F64) :: nel
            !
            ! The divergence of the total electronic density integrated
            ! on the molecular grid. Exact value: zero.
            !
            real(F64) :: div
            !
            ! The density of the non-interacting kinetic energy integrated
            ! on the molecular graid. Exact value: non-interacting kinetic
            ! energy evaluated as a trace of the T operator.
            !
            real(F64) :: kin
            !
            ! The laplacian of the total electronic density integrated
            ! on the molecular grid. Exact value: zero.
            !
            real(F64) :: lap
      end type TGridDiag

      type TSCFOutput
            ! -------------------------------------------------------------------------
            ! Energies, one-electron matrices, and arrays of orbital energies resulting
            ! from a converged SCF step. This object is intended to be used as an input
            ! for post-SCF methods.
            ! -------------------------------------------------------------------------
            ! Total nuclear + electronic energy at the DFT level.
            ! Includes the dispersion correction.
            !
            real(F64) :: EtotDFT
            !
            ! Electronic energy: Etotal - nuclear repulsion. Includes
            ! the dispersion correction.
            !
            real(F64) :: EelDFT
            !
            ! Nuclear repulsion energy
            !
            real(F64) :: Enucl
            !
            ! Pure DFT exchange-correlation energy. The Hartree-Fock part of
            ! the exchange energy and dispersion corrections are not included.
            !
            real(F64) :: ExcDFT
            !
            ! Dispersion correction, e.g., Grimme's DFT-D3
            !
            real(F64) :: EdispDFT
            !
            ! Bare-nuclei Hamiltonian: kinetic energy operator + electron-nuclei potential
            ! (Cartesian AO basis)
            !
            real(F64), dimension(:, :), allocatable :: Hbare_cao
            !
            ! Density matrix (Cartesian AO basis) obtained from the digonalization
            ! of the converged Kohn-Sham/Fock matrix
            !
            real(F64), dimension(:, :, :), allocatable :: Rho_cao
            !
            ! Transformation matrices for transforming the AO indices from the orthogonal AO basis
            ! (OAO) to the basis of Cartesian atomic orbitals (CAO) and spherical atomic orbitals (SAO):
            ! C_cao <- MOBasisVecsCart*C_oao
            ! C_sao <- MOBasisVecsSpher*C_sao
            ! MOBasisVecsSpher is defined only if the atomic basis set definition has spherical
            ! harmonics enabled.
            !
            real(F64), dimension(:, :), allocatable :: MOBasisVecsCart
            real(F64), dimension(:, :), allocatable :: MOBasisVecsSpher
            !
            ! Number of orthogonal molecular orbitals used in the SCF variational space
            !
            integer :: Noao
            !
            ! Number of Cartesian atomic orbitals excluded from the variational space
            ! due to the Cartesian->spherical transformation and the removal of linear
            ! dependencies
            !
            integer :: Nexcluded
            !
            ! Molecular orbitals obtained from the diagonalization of the converged
            ! Kohn-Sham/Hartree-Fock matrix (orthogonal atomic orbital basis)
            !
            real(F64), dimension(:, :, :), allocatable :: C_oao
            !
            ! Number of molecular spin-orbitals
            !
            integer, dimension(2) :: NOcc
            integer, dimension(2) :: NVirt
            !
            ! Orbital energies
            !
            real(F64), dimension(:, :), allocatable :: OrbEnergies
            real(F64) :: Ehomo
            real(F64) :: Elumo
            !
            ! User-defined (auxiliary) function integrated on the molecular grid.
            ! Computed on the same grid as the exchange-correlation numerial integral.
            !
            real(F64), dimension(:), allocatable :: AUXOut
            !
            ! Flag set if the SCF algorithm successfully converged
            ! to the specified precision
            !
            logical :: Converged
      end type TSCFOutput

      type TSCFParams
            ! -----------------------------------------------------
            !              BASIS SET PARAMETERS
            ! -----------------------------------------------------
            character(:), allocatable :: AOBasisPath
            character(:), allocatable :: AOBasisName
            logical :: SpherAO = .true.
            character(:), allocatable :: F12BasisPath
            character(:), allocatable :: F12BasisName
            character(:), allocatable :: AtomicGuessDir
            ! -----------------------------------------------------
            !                PSEUDOPOTENTIAL
            ! -----------------------------------------------------
            !
            ! Path to text files containting pseudopotential
            ! parameters. Using a TStringList structure to allow
            ! for a different set of params for each element.
            !
            type(TStringList) :: ECPFile
            ! -----------------------------------------------------
            !    EXCHANGE-CORRELATION MODEL (INCL. HARTREE-FOCK)
            ! -----------------------------------------------------
            integer :: xcfunc = XCF_XC_NONE
            real(F64) :: srexx = -ONE
            real(F64) :: omega = -ONE
            !
            ! Exchange-correlation functional computed using the converged
            ! density from the preceding SCF. Use this set of
            ! variables for density-corrected DFT (Kim, M.-C., Sim, E.,
            ! Burke, K., Understanding and Reducing Errors in Density
            ! Functional Calculations, Phys. Rev. Lett. 111, 73003;
            ! doi: 10.1103/PhysRevLett.111.073003)
            !
            integer :: non_scf_xcfunc = XCF_XC_NONE
            real(F64) :: non_scf_srexx = -ONE
            real(F64) :: non_scf_omega = -ONE
            ! ----------------------------------------------------
            ! ASYMPTOTIC CORRECTION OF THE EXCHANGE-CORRELATION
            ! POTENTIAL
            ! ----------------------------------------------------
            integer :: AsympVxc = AC_NONE
            real(F64) :: AsympVxcOmega = 0.15_F64
            logical :: SlaterVxc = .false.
            logical :: non_scf_SlaterVxc = .false.
            ! ----------------------------------------------------
            !            AUXILIARY NUMERICAL INTEGRALS
            ! ----------------------------------------------------
            ! Auxiliary integral computed
            ! on the DFT molecular grid
            !
            integer :: AUXInt_Type1 = AUX_NONE
            !
            ! Auxiliary integral computed without
            ! DFT numerical grid
            !
            integer :: AUXInt_Type2 = AUX_NONE
            !
            ! Optional input for the auxiliary integrand.
            ! For example, if the function requires a density on
            ! the grid, AUXIn is the way to pass this density.
            !
            real(F64), dimension(:, :), allocatable :: AUXIn
            !
            ! Hirshfeld atoms
            !
            logical :: Hirsh = .false.
            !
            ! Volumes of isolated Hirshfeld atoms
            !
            real(F64), dimension(:), allocatable :: HirshVolumes
            character(:), allocatable :: aux_density_compare_path
            integer :: aux_density_compare_mode
            !
            ! Spacing between the points at which the XC hole is evaluated
            !
            real(F64) :: aux_xc_hole_spacing = 0.1_F64
            !
            ! Number of points at which the XC hole is evaluated
            !
            integer :: aux_xc_hole_npoints = 100
            ! ----------------------------------------------------
            ! File path for the converged DFT/HF density matrix
            ! ----------------------------------------------------
            character(:), allocatable :: save_rhoa_path
            character(:), allocatable :: save_rhob_path
            integer :: save_rho_mode = FILEMODE_NONE
            ! ----------------------------------------------------
            ! Guess for the zeroth SCF iteration
            ! ----------------------------------------------------
            character(:), allocatable :: guess_rhoa_path
            character(:), allocatable :: guess_rhob_path
            integer :: guess_type = SCF_GUESS_DEFAULT
            ! ----------------------------------------------------
            ! SCF convergence thresholds
            ! ----------------------------------------------------
            real(F64) :: ConvThreshRho = 2.0E-5_F64
            real(F64) :: ConvThreshGrad = 2.0E-5_F64
            !
            ! Threshold for removing small eigenvalues from
            ! the AO overlap matrix.
            !
            real(F64) :: LinDepThresh = 1.0E-6_F64
            ! ----------------------------------------------------
            ! Threshold for the contributions |Rho(r,s)*(pq|rs)|
            ! and |Rho(q,s)*(pq|rs)| for the Hartree-Fock/hybrid
            ! Kohn-Sham Coulomb (J) and (K) matrices.
            ! ----------------------------------------------------
            real(F64) :: ThreshFockJK = 1.0E-11_F64
            
            integer :: MaxNIters = 128
            ! ----------------------------------------------------
            ! Cholesky decomposition of the Coulomb matrix
            ! ----------------------------------------------------
            logical :: UseCholeskyBasis = .false.
            integer :: MaxBufferDimMB = 4000
            integer :: TargetBlockDim = 100
            ! ----------------------------------------------------
            ! Tensor hypercontraction
            ! ----------------------------------------------------
            logical :: UseTensorHypercontraction = .false.
            ! ----------------------------------------------------
            !                INTEGRATION GRID
            ! ----------------------------------------------------
            integer :: GridKind = GRD_FINE
            logical :: GridPruning = .true.
      contains
            procedure, pass :: init => TSCFParams_init
            procedure, pass :: non_scf => set_non_scf_xcfunc
            procedure, pass :: AUXIntegral => set_AUXIntegral
      end type TSCFParams

contains

      subroutine TSCFParams_init(this, ixcmodel, srexx, omega)
            class(TSCFParams), intent(out)   :: this
            integer, intent(in)              :: ixcmodel
            real(F64), optional, intent(in)  :: srexx, omega

            this%xcfunc = ixcmodel
            if (present(srexx)) this%srexx = srexx
            if (present(omega)) this%omega = omega
      end subroutine TSCFParams_init

      
      subroutine set_non_scf_xcfunc(this, ixcmodel, srexx, omega)
            class(TSCFParams), intent(inout) :: this
            integer, intent(in)              :: ixcmodel
            real(F64), optional, intent(in)  :: srexx, omega

            this%non_scf_xcfunc = ixcmodel
            if (present(srexx)) this%non_scf_srexx = srexx
            if (present(omega)) this%non_scf_omega = omega
      end subroutine set_non_scf_xcfunc


      subroutine set_AUXIntegral(this, AUXInt_Type1, AUXInt_Type2, AUXIn)
            class(TSCFParams), intent(inout)            :: this
            integer, intent(in)                         :: AUXInt_Type1
            integer, intent(in)                         :: AUXInt_Type2
            real(F64), dimension(:, :), allocatable, &
                  optional, intent(inout)               :: AUXIn

            this%AUXInt_Type1 = AUXInt_Type1
            this%AUXInt_Type2 = AUXInt_Type2
            if (present(AUXIn)) then
                  call move_alloc(AUXIn, this%AUXIn)
            else
                  if (allocated(this%AUXIn)) deallocate(this%AUXIn)
                  allocate(this%AUXIn(0, 0))
            end if
      end subroutine set_AUXIntegral
end module scf_definitions
