module gparam
      use arithmetic
      use math_constants
      use string
      use h_voldata
      use h_xcfunc
      use scf_definitions
      use h_io
      use rpa_definitions
      
      implicit none
      save
      !
      ! Timings (wall clock time)
      ! -------------------------
      ! Numerical integrals on the molecular 
      ! grid
      !
      real(F64), dimension(3) :: TIMINGS = ZERO
      integer :: TIME_GRID = 1
      !
      ! Two-electron integrals computed
      ! for the Fock or Kohn-Sham matrices
      !
      integer :: TIME_INTS2E = 2
      !
      ! Solving symmetric eigenvalue problems
      !
      integer :: TIME_EIGEN = 3
      integer, parameter :: KNOWN_ELEMENTS = 86
      !
      ! Default character string length (maximum length
      ! of keyword names etc.)
      !
      integer, parameter          :: DEFLEN = 80
      !
      ! Absolute path to the input file
      !
      character(len=:), allocatable :: INPUTFILE
      !
      ! Working directory where the input file is located
      ! (trailing directory separator included)
      !
      character(len=:), allocatable :: WORKDIR
      !
      ! Job identifier
      ! --------------
      ! JOBTITLE = {DATE}_{PROCID}
      !
      ! DATE is the date of starting the launcher script represented
      ! as "%Y%m%d%H%M%S".
      !
      ! PROCID is the process identifier of the launcher script.
      ! This identifier is identical for all processes spawned
      ! by the launcher script.
      !
      character(:), allocatable :: JOBTITLE
      !
      ! Optional symbol printed before the total SCF energy.
      ! Useful for scripts parsing the output text file.
      !
      character(:), allocatable :: TAG_DFT_TOTAL_ENERGY
      character(:), allocatable :: TAG_DFT_DISP
      ! ------------------------------------------------------------
      ! Molecule definition. TMOLECULE object contains information
      ! on molecule's geometry and electronic structure.
      ! ------------------------------------------------------------
      !
      type tmolecule
            !
            ! Path to the file containing molecule's coordinates
            !
            character(len=DEFLEN) :: path
            !
            ! Index of first line containing XYZ data. This data
            ! can be other than coordinates. The index of first line
            ! can be greater that 1
            !
            integer               :: start
            !
            ! Atomic XYZ coordinates (in atomic units)
            !
            real(F64), dimension(:, :), allocatable :: atomr
            !
            ! Nuclear Z numbers (not modified by ECP)
            !
            integer, dimension(:), allocatable :: inuclz
            !
            ! Number of atoms
            !
            integer               :: natom
            !
            ! Real (non-dummy) atoms
            !
            integer, dimension(2, 2) :: real_atoms
            !
            ! Charge of the molecule (can be fractional)
            !
            integer               :: charge
            integer               :: charge_frac
            !
            ! Spin multiplicity
            !
            integer               :: multiplicity
            !
            ! Number of electrons (can be fractional)
            !
            integer               :: ne
            integer               :: ne_frac
            !
            ! Number of orbitals in the open shell (ROKS)
            !
            integer               :: nopenorb
            !
            ! Number of alpa electrons in the open shell (can be fractional in ROKS)
            !
            integer               :: nopenela
            integer               :: nopenela_frac
            !
            ! Number of beta electrons in the open shell (can be fractional in ROKS)
            !
            integer               :: nopenelb
            integer               :: nopenelb_frac
            !
            ! Number of distinct elements present in the molecule
            !
            integer               :: nelement
            !
            ! ZCOUNT(K), K=1, 2,..., NELEMENT is
            ! the number of representatives of the K-th element
            !
            integer, dimension(:), allocatable :: zcount
            !
            ! ZLIST(K), Z number of the K-th element
            !
            integer, dimension(:), allocatable :: zlist
      contains
            procedure, pass :: init => molecule_init
      end type tmolecule


      type tsystemdep_params
            ! --------------------------------------------------
            ! System-specific parameters relevant for program
            ! runs multiple molecules in a single input file
            ! --------------------------------------------------
            ! Bounds on the range-separation parameter during
            ! optimization (tuning) of a range-separated (RS)
            ! exchange functional
            !
            real(F64) :: optomega_min
            real(F64) :: optomega_max
            !
            ! Kind of RS functional tuning
            !
            integer :: optomega_j2type
            !
            ! Range-separation parameter
            !
            real(F64) :: lcomega
            !
            ! Fraction of short-range HF exchange
            !
            real(F64) :: lcsrexx
            !
            ! Kind of SCF guess
            !
            integer :: scf_guess
            !
            ! Path to the SCF guess matrix
            !
            character(:), allocatable :: scf_guess_rhoa_path
            character(:), allocatable :: scf_guess_rhob_path
            !
            ! Options for storing converged density matrix (file type and location)
            !
            integer :: scf_save_rho_mode
            character(:), allocatable :: scf_save_rhoa_path
            character(:), allocatable :: scf_save_rhob_path
            !
            ! SCF convergence thresholds
            !
            real(F64) :: scf_thresh_density
            real(F64) :: scf_thresh_gradient
            logical :: main_job
      end type tsystemdep_params

      
      type tjobqueue
            type(tmolecule)           :: xyz
            type(tsystemdep_params)   :: par
            integer                   :: id
            type(tjobqueue), pointer  :: next => null()
      end type tjobqueue

      integer, parameter :: GEOM_COMPLEX = 2**0
      integer, parameter :: GEOM_MONOMER = 2**1
      integer, parameter :: GEOM_NEUTRAL = 2**2
      integer, parameter :: GEOM_CATION  = 2**3
      integer, parameter :: GEOM_ANION   = 2**4
      integer, parameter :: GEOM_HIRSHFELD_ATOM = 2**5
      !
      ! The character used by the operating system
      ! to separate pathname components. Its OS-specific
      ! value is supplied to the program by the launcher
      ! script.
      !
      character(len=1) :: DIRSEP = "/"
      !
      ! Job names
      !
      integer, parameter :: JOB_UNKNOWN     = -1
      integer, parameter :: JOB_DFT_SP      = 1
      integer, parameter :: JOB_DFT_INT     = 2
      integer, parameter :: JOB_DFT_PBDINT  = 4
      integer, parameter :: JOB_MP2_SP      = 5
      integer, parameter :: JOB_CCSD_PROP   = 9
      integer, parameter :: JOB_CC3_PROP   = 10
      integer, parameter :: JOB_CC3_MEM_PROP   = 31
      integer, parameter :: JOB_DFT_FROKS   = 11
      integer, parameter :: JOB_DFT_OPTOMEGA = 13
      integer, parameter :: JOB_CCSD_DENSITY   = 17
      integer, parameter :: JOB_CC3_DENSITY   = 18
      integer, parameter :: JOB_DFTD3_SP = 19
      integer, parameter :: JOB_DFTD3_INT = 20
      integer, parameter :: JOB_VIS_RHO_DIFF = 22
      integer, parameter :: JOB_DFT_DISP_OPTIM = 24
      integer, parameter :: JOB_RTTDDFT_POLAR = 25
      integer, parameter :: JOB_DFT_OPT_MODRZEJ2015 = 26
      integer, parameter :: JOB_CMPLX_RKS_SP = 100
      integer, parameter :: JOB_REAL_UKS_SP = 101
      integer, parameter :: JOB_REAL_UKS_INT = 102
      integer, parameter :: JOB_REAL_UKS_RPA = 103
      !
      ! Available Coupled-Cluster models
      !
      integer, parameter :: THEORY_CCSD = 1
      integer, parameter :: THEORY_CC3  = 2
      integer, parameter :: THEORY_CC3_MEM  = 3

      integer                   :: VIS_RHOA_MODE = FILEMODE_TEXT
      character(:), allocatable :: VIS_RHOA_PATH
      integer                   :: VIS_RHOB_MODE = FILEMODE_TEXT
      character(:), allocatable :: VIS_RHOB_PATH
      character(:), allocatable :: VIS_CUBEFILE_PATH
      integer                   :: VIS_CUBEFILE_SPACING = VOL_SPACING_MEDIUM
      !
      ! J^2 = (IP+EHOMO)**2 + (EA+ELUMO)**2
      !
      integer, parameter :: OPTOMEGA_J2_CNA = 1
      !
      ! J^2 = (IP+EHOMO)**2
      !
      integer, parameter :: OPTOMEGA_J2_CN = 2
      !
      ! J^2 = (EA+ELUMO)**2
      !
      integer, parameter :: OPTOMEGA_J2_NA = 3
      integer            :: OPTOMEGA_J2TYPE = OPTOMEGA_J2_CN
      !
      ! Lower and upper bounds on the optimally-tuned
      ! omega parameter
      !
      real(F64)          :: OPTOMEGA_MIN = ZERO
      real(F64)          :: OPTOMEGA_MAX = 1.5_F64
      !
      ! Dispersion correction
      !
      integer :: DFT_DISP = DISP_NONE
      !
      ! User-defined DFT-D3 damping parameter (r6).
      ! See the definition in Grimme, S., Antony, J., Ehrlich, S., and Krieg, H.,
      ! J. Chem. Phys. 132, 154104 (2010), DOI: 10.1063/1.3382344
      ! The default value for a given functional will be used if this parameter
      ! is not changed by the user.
      !
      ! This parameter is defined in Eq. 4 in the Grimme's paper (s_{r,6}).
      !
      real(F64) :: DFTD3_R6 = -ONE
      !
      ! User-defined DFT-D3 1/R^8 scaling (s8).
      ! See the definition in Grimme, S., Antony, J., Ehrlich, S., and Krieg, H.,
      ! J. Chem. Phys. 132, 154104 (2014), DOI: 10.1063/1.3382344
      ! The default value for a given functional will be used if this parameter
      ! is not changed by the user.
      !
      ! This parameter is defined in Eq. 3 in the Grimme's paper (s_8).
      !
      real(F64) :: DFTD3_S8 = -ONE
      !
      ! User-defined parameters of Becke-Johnson damping function
      ! for Grimme's DFT-D3 energy:
      ! Grimme, S., Ehrlich, S., Goerigk, L. J. Comp. Chem. 32, 1456 (2011);
      ! doi: 10.1002/jcc.21759
      !
      real(F64) :: DFTD3_BJ_A1 = -ONE
      real(F64) :: DFTD3_BJ_A2 = -ONE
      real(F64) :: DFTD3_BJ_S8 = -ONE
      !
      ! Add 3-body contribution to the DFT-D3 dispersion
      ! (defined in J. Chem. Phys. 132, 154104 (2010), DOI: 10.1063/1.3382344)
      !
      logical :: DFTD3_3BODY = .false. 
      ! -------------------------------------------------------------------
      !                   MBD-rsSCS dispersion energy
      ! -------------------------------------------------------------------
      ! The damping parameter (beta) of the MBD-rsSCS method
      ! (see Eq. 13 in J. Chem. Phys. 140, 18A508; doi: 10.1063/1.4865104)
      !
      real(F64) :: MBD_RSSCS_BETA = -ONE
      
      logical :: DOREPORT = .false.
      integer :: XCMODEL = XCF_HF
      integer :: NONSCF_XCMODEL = XCF_XC_NONE
      !
      ! Auxiliary integrals computed on the numerical grid
      !
      integer :: AUXINTEGRAL = AUX_NONE
      !
      ! Auxiliary integrals computed after SCF is completed,
      ! without the numerical grid
      !
      integer :: AUXINT_TYPE2 = AUX_NONE
      integer :: JOBTYPE = JOB_UNKNOWN
      logical :: BUILDSKS = .true.
      !
      ! Location of the file containing basis set parameters
      !
      character(:), allocatable       :: BASIS_SET_PATH
      !
      ! Location of ECP parameters
      !
      type(tstringlist) :: ECP_PARAMS_PATH
      !
      ! Displayed name of the loaded basis set, i.e. human-readable
      ! basis set name. This string can contain special characters
      ! not allowed in filenames.
      !
      character(:), allocatable       :: BASIS_SET_NAME
      ! -----------------------------------------------------------
      ! Point charges added to the external potential. Use this for
      ! electronstatic embedding of the molecule.
      ! -----------------------------------------------------------
      integer :: POINT_CHARGES_N = 0
      real(F64), dimension(:), allocatable :: POINT_CHARGES_Q
      real(F64), dimension(:, :), allocatable :: POINT_CHARGES_R
      !
      ! Location of the file containing density guess for the
      ! currently loaded basis set. Contains trailing directory
      ! separator.
      !
      character(:), allocatable :: ATOMIC_GUESS_DIR

      integer, parameter          :: UNITS_ANGS = 0
      integer, parameter          :: UNITS_BOHR = 1
      integer                     :: UNITS = UNITS_ANGS
      !
      ! Parameters controlling range-separated exchange
      ! functional
      ! ---
      ! Range-separation parameter. The default value for a given
      ! functional is used if LCOMEGA < 0.
      !
      real(F64)            :: LCOMEGA = -ONE
      real(F64)            :: NONSCF_LCOMEGA = -ONE
      !
      ! Relevant for range-separated density
      ! functionals: Portion of exact exchange
      ! in the short range. The default value
      ! for a given functional is used if
      ! LCSREXX < 0.
      !
      real(F64)            :: LCSREXX = -ONE
      real(F64)            :: NONSCF_LCSREXX = -ONE
      !
      ! Number of outer electrons in global-density-dependent range-separated
      ! (GDD RS) functionals. If the system is spin-polarized, different values
      ! for alpha and beta electrons should be provided. Otherwise, the spin
      ! components are set equal.
      !
      real(F64)            :: GDD_NOUT_ALPHA = ONE 
      real(F64)            :: GDD_NOUT_BETA = ONE
      real(F64)            :: GDD_MUMIN = 0.07d+0

      real(F64) :: MLRCS12_GPARAM_USER = -ONE
      real(F64) :: WPBESOL_MLRCS_GPARAM_USER = -ONE
      real(F64) :: WPBE_MLRCS_GPARAM_USER = -ONE
      !
      ! Regularized nuclei-electrons potential used in
      ! Misquitta's charge-transfer energy decomposition,
      !
      logical :: MISQUITTA_CT = .false.
      real(F64) :: MISQUITTA_CT_ETA = -ONE
      !
      ! Total number of single-point jobs to execute
      !
      integer                     :: NJOB = 0
      !
      ! HIRSH is set to .TRUE. if the Hirshfeld analysis
      ! is a part of any job
      !
      logical                     :: HIRSH = .false.
      !
      ! Number of Hirshfeld free-atom densities to be computed
      ! before the main single-point jobs can start.
      !
      integer                     :: NHIRSH = 0
      !
      ! List of all the jobs to be computed
      !
      type(tjobqueue), pointer    :: JOBQ
      type(tjobqueue), pointer    :: JOBQ_FIRST
      !
      ! Root directory of this program
      ! (trailing directory separator included)
      !
      character(len=:), allocatable :: ROOTDIR
      !
      ! Directory of basis set data (trailing directory
      ! separator included)
      !
      character(len=:), allocatable :: BASISDIR
      !
      !
      !
      character(len=:), allocatable :: ECPDIR
      !
      ! Directory of atomic density data needed for an
      ! orbital guess (trailing directory separator
      ! included)
      !
      character(len=:), allocatable :: GUESSDIR
      !-----------------------------------------------------------
      !                    SCF CONVERGENCE
      ! ----------------------------------------------------------
      ! Density threshold. The convergence is achieved
      ! if the maximum AO density matrix difference is less
      ! than SCF_THRESH_DENSITY. If set to -1, one of the default
      ! thresholds is employed depending on the level of theory.
      !
      real(F64) :: SCF_THRESH_DENSITY = -ONE
      !
      ! Threshold for the maximal element of the orbital gradient
      ! matrix. If set to -1, the default threshold is used.
      !
      real(F64) :: SCF_THRESH_GRADIENT = -ONE
      !
      ! Default SCF convergence threshold for maximal AO
      ! density matrix difference (DFT computations).
      ! Overridden by a user-supplied value if the appropriate
      ! keyword is present in the input.
      !
      real(F64), parameter :: SCF_THRESH_DENSITY_DEFAULT_DFT = 2.0E-5_F64
      !
      ! Default SCF convergence threshold for maximal AO density
      ! matrix difference for SCF HF computations preceding
      ! a wavefunction job. Overridden by a user-supplied value
      ! if the appropriate keyword is present in the input.
      !
      real(F64), parameter :: SCF_THRESH_DENSITY_DEFAULT_WF = 1.0E-6_F64
      !
      ! Default SCF convergence threshold (max orbital gradient norm)
      !
      real(F64), parameter :: SCF_THRESH_GRADIENT_DEFAULT = 2.0E-5_F64
      !
      ! The orbital gradient value (max norm) below which the orbital
      ! energy shift is disabled
      !
      real(F64), parameter :: SCF_DISABLE_ARH_SHIFT_POLAR = 1.0E-7_F64
      real(F64), parameter :: SCF_DISABLE_ARH_SHIFT_UNPOLAR = 1.0E-4_F64
      !
      ! Numerical convergence threshold for energy in SCF
      ! cycles. SCF is interupted if energy difference
      ! falls below this threshold, regardless density
      ! difference. 
      !
      real(F64), parameter :: SCF_ECONV = 1.0E-13_F64
      !
      ! Maximum number of iterations in SCF process
      !
      integer :: SCF_MAXIT = 128
      !
      ! Data stored on disk after converging SCF
      ! ----------------------------------------
      ! Density matrix of the ground state
      !
      integer :: SCF_SAVE_RHO_MODE = FILEMODE_NONE
      character(:), allocatable :: SCF_SAVE_RHOA_PATH
      character(:), allocatable :: SCF_SAVE_RHOB_PATH
      integer :: SCF_GUESS = SCF_GUESS_DEFAULT
      !
      ! Path to the file containing zeroth-iteration
      ! density matrix. Referenced only if
      ! SCF_GUESS == SCF_GUESS_TEXT_FILE or SCF_GUESS_BINARY_FILE
      !
      character(:), allocatable :: SCF_GUESS_RHOA_PATH
      character(:), allocatable :: SCF_GUESS_RHOB_PATH
      !
      ! Enable real-valued solid harmonics
      ! as atomic orbitals
      !
      logical :: SPHERBASIS = .true.
      !
      ! Threshold for eigenvalues of the overlap matrix.
      ! If an eigenvalue falls below this threshold, 
      ! the atomic orbitals are considered linearly
      ! dependent.
      !
      real(F64)            :: LINDEP_THRESH = 1.0E-6_F64
      !
      ! Threshold for orthogonality of MO coefficients
      ! in OAO basis. If the orthogonality criterion is
      ! not met, SCF solver performs orthogonalization
      !
      real(F64), parameter :: SCF_ORTH_THRESH = 1.E-12_F64
      ! ----------------------------------------------------------
      !        Real-Time Propagation of the Kohn-Sham Equation
      ! ----------------------------------------------------------
      !
      ! Angular frequencies (in atomic units) of frequency-dependent
      ! (hyper)polarizabilities computed via real-time propagation
      ! with a monochromatic perturbation A cos(omega t).
      !
      real(F64), dimension(:), allocatable :: RTTDDFT_POLAR_OMEGA
      !
      ! Total simulation time for the computation of (hyper)polarizabilities.
      ! The number of time steps to be done is measured with
      ! the number of cycles of the incident wave, T=2Pi/omega.
      !
      integer :: RTTDDFT_POLAR_NCYCLES = -1
      !
      ! Field strength applied for the finite-difference
      ! computation of (hyper)polarizabilities
      !
      real(F64) :: RTTDDFT_POLAR_FIELD = -ONE
      !
      ! Time step of real-time propagation (in atomic units)
      !
      real(F64) :: RTTDDFT_TIMESTEP = -ONE
      ! ----------------------------------------------------------
      !                   Hirsheld analysis
      ! ----------------------------------------------------------
      type thirshatom
            !
            ! Spherically-averaged density of a free atom
            !
            real(F64), dimension(:), allocatable :: rho
            !
            ! Z number (independent of ECP)
            !
            integer :: znum
            !
            ! Free atom Hirshfeld volume
            !
            real(F64) :: volume
      end type thirshatom
      !
      ! Hirshfeld free atom data. This object is allocated
      ! only by the main image.
      !
      type(thirshatom), dimension(:), allocatable :: HIRSHATOMS
      ! ----------------------------------------------------------------
      ! Computation of system-averaged exchange-correlation holes
      ! ----------------------------------------------------------------
      !
      ! Number of points at which the XC hole is evaluated
      !
      integer :: AUX_XC_HOLE_NPOINTS = 100
      !
      ! Spacing between the points at which the XC hole is evaluated
      !
      real(F64) :: AUX_XC_HOLE_SPACING = 0.1_F64

      real(F64) :: MCSv2_GOPP = 0.05_F64
      real(F64) :: MCSv2_GPAR = 0.05_F64
      !
      ! Mean-absolute deviations from the reference density matrix
      !
      character(:), allocatable :: DENSITY_COMPARE_PATH
      integer :: DENSITY_COMPARE_MODE = FILEMODE_NONE
      ! -------------------------------------------------
      ! Density differences
      ! 1. RHO_DIFF_SIMPLE
      !    Compute the difference Delta=Rho-RhoAB between
      !    two electron densities (gradients, laplacians),
      !    for a single system.
      ! 2. RHO_DIFF_DIMER
      !    Form the difference densities
      !    DeltaAB = RhoAB - RhoA - RhoB
      !    DeltaRefAB = RhoRefAB - RhoRefA - RhoRefB
      !    and compute the difference between DeltaAB and
      !    DeltaRefAB.
      ! -------------------------------------------------
      integer, parameter :: RHO_DIFF_SIMPLE = 1
      integer, parameter :: RHO_DIFF_DIMER = 2
      integer :: RHO_DIFF_TYPE = RHO_DIFF_DIMER
      integer, parameter :: RHO_DIFF_FILEMODE = FILEMODE_TEXT
      character(:), allocatable :: RHO_DIFF_Ref
      character(:), allocatable :: RHO_DIFF_RefAB
      character(:), allocatable :: RHO_DIFF_RefA
      character(:), allocatable :: RHO_DIFF_RefB
      character(:), allocatable :: RHO_DIFF_A
      character(:), allocatable :: RHO_DIFF_B
      character(:), allocatable :: RHO_DIFF_CSV
      real(F64), dimension(3) :: RHO_DIFF_ProbeStart
      real(F64), dimension(3) :: RHO_DIFF_ProbeStop
      integer :: RHO_DIFF_ProbeN
      integer, dimension(2) :: RHO_DIFF_MonoA = [-1, -1]
      integer, dimension(2) :: RHO_DIFF_MonoB = [-1, -1]
      ! -------------------------------------------------
      !          Spherically-averaged densities
      ! -------------------------------------------------
      ! Spherically-averaged density of isolated atoms
      ! computed at Rk = R0 + (k-1) * dr, k = 1, ..., N
      !
      real(F64) :: RHO_SPHER_R0 = ZERO
      real(F64) :: RHO_SPHER_DR = 0.01_F64
      integer :: RHO_SPHER_N = 1000
      character(:), allocatable :: RHO_SPHER_REF
      integer :: RHO_SPHER_FILEMODE = FILEMODE_NONE
      character(:), allocatable :: RHO_SPHER_CSV
      ! ----------------------------------------------------------------
      ! System-averaged meta-GGA correlation hole of Modrzejewski et al.
      ! Modrzejewski et al. J. Chem. Phys. 137, 204121 (2012);
      ! doi: 10.1063/1.4768228
      ! ----------------------------------------------------------------
      ! Adjustable parameter G (parallel-spin hole)
      !
      real(F64) :: AUX_MODRZEJ2012_C_GPAR = -ONE
      !
      ! Adjustable parameter G (opposite-spin hole)
      !
      real(F64) :: AUX_MODRZEJ2012_C_GOPP = -ONE
      !
      ! Coupling constant lambda
      !
      real(F64) :: AUX_MODRZEJ2012_C_LAMBDA = -ONE
      !
      ! Maximum number of MWORDS allocated
      ! by integral transformation subroutine
      ! in MP2 module
      !
      integer :: MP2_MWORD = 75
      !
      ! Number of excited states to compute in EOM
      ! Coupled Cluster. Degenerate eigenvalues
      ! count as one.
      !
      integer                     :: CC_NLEVELS = 0
      !
      ! Number of frozen orbitals
      !
      integer                     :: CC_FROZEN = 0
      integer                     :: CC_NON = 1
      logical                     :: CC_SPIN_ORBIT = .false.
      !
      ! JACOBIAN PARAMETERS
      !
      integer                     :: CC_BINTO = 1
      integer                     :: CC_BINTV = 1
      integer                     :: CC_KINTO = 1
      integer                     :: CC_KINTV = 1

      !
      ! Parameter controlling the number of auxiliary
      ! (non-target) guess vectors for the Davidson algorithm.
      ! These auxiliary vectors belonging to non-target
      ! eigenvelues.
      !
      integer                     :: CC_NAUXGUESS = 0
      integer                     :: CC_NSYM = 0
      integer                     :: CC_LEVEL0 = 1
      integer                     :: CC_LEVEL1 = 1
      logical                     :: CC_DEGENERACY = .true.
      logical                     :: DENSITY_INT = .true.
      logical                     :: CC_NOSYMMETRY = .false.
      character(len=DEFLEN)       :: CC_LEVDEG = "0"
      character(len=DEFLEN)       :: CC_NUMLEV_SPEC = "0"
      integer                     :: CC_MULTIP = 1
      integer                     :: cc_singlet = 1
      integer                     :: cc_triplet = 3
      integer                     :: cc_mixed = 8
      integer                     :: cc_mixed_left = 9
      logical                     :: slater_basis = .false.
      logical                     :: SCF_READ  = .false.
      logical                     :: SCF_WRITE = .false.
      logical                     :: CCSP_WRITE = .false.
      logical                     :: CCSP_READ = .false.
      logical                     :: EOM_MEM = .false.
      logical                     :: EOM_READ  = .false.
      logical                     :: EOM_WRITE = .false.
      logical                     :: PROP_GR_EXC = .false.
      logical                     :: PROP_EXC_EXC_DIP = .true.
      logical                     :: PROP_EXC_EXC_SO = .false.
      character(len=DEFLEN)       :: SLATER_FILE_1E
      character(len=DEFLEN)       :: SLATER_FILE_2E
      character(len=DEFLEN)       :: SLATER_FILE_AUX
      character(len=DEFLEN)       :: SLATER_FILE_SCF
      integer                     :: CC_NORB
      logical                     :: ONLYRIGHT = .true.
      !
      ! Density matrix of the ground state (Coupled Clusters)
      !
      logical                     :: CC_SAVERHO = .false.
      integer                     :: CC_SAVERHO_MODE = FILEMODE_TEXT
      character(:), allocatable   :: CC_SAVERHO_PATH

      integer                     :: POINT_GROUP = 0
      integer                     :: C2v = 1
      integer                     :: D2h = 2

      character(len=DEFLEN)       :: CC_IEXCI = "0"
      character(len=DEFLEN)       :: CC_IEXCI_D = "0"
      character(len=DEFLEN)       :: CC_IEXCI_S = "0"

      character(len=DEFLEN)       :: CC_LEXCI_S = "0"
      character(len=DEFLEN)       :: CC_UEXCI_S = "0"

      character(len=DEFLEN)       :: CC_LEXCI_D = "0"
      character(len=DEFLEN)       :: CC_UEXCI_D = "0"

      character(len=DEFLEN)       :: CC_UEXCI_SD = "0"
      character(len=DEFLEN)       :: CC_LEXCI_SD = "0"

      character(len=DEFLEN)       :: CC_SING_LEXCI_S = "0"
      character(len=DEFLEN)       :: CC_TRIP_LEXCI_S = "0"
      character(len=DEFLEN)       :: CC_SING_LEXCI_D = "0"
      character(len=DEFLEN)       :: CC_TRIP_LEXCI_D = "0"

      character(len=DEFLEN)       :: CC_SING_UEXCI_S = "0"
      character(len=DEFLEN)       :: CC_TRIP_UEXCI_S = "0"
      character(len=DEFLEN)       :: CC_SING_UEXCI_D = "0"
      character(len=DEFLEN)       :: CC_TRIP_UEXCI_D = "0"



      integer, parameter :: D2h_order = 8
      integer, parameter :: C2v_order = 4
      
      !
      ! S operators input 
      !
      integer, parameter :: all_s_orders = 234
      !
      ! Molecule input
      !
      integer                      :: HI = 0
      integer                      :: AV = 1
      integer                      :: CHCTR = 0
      integer                      :: MCTR = 1
      integer                      :: POINT = 3
      integer                      :: ATOMIC_MASS = 0
      integer                      :: ORIG = 0
      !
      ! Note that the following character matrix is symmetric
      !
      integer, dimension(D2h_order, D2h_order), parameter :: D2h_char_table = &
            reshape([ &
            1,  1,  1,  1,  1,  1,  1,  1, &
            1,  1, -1, -1,  1,  1, -1, -1, &
            1, -1,  1, -1,  1, -1,  1, -1, &
            1, -1, -1,  1,  1, -1, -1,  1, &
            1,  1,  1,  1, -1, -1, -1, -1, &
            1,  1, -1, -1, -1, -1,  1,  1, &
            1, -1,  1, -1, -1,  1, -1,  1, &
            1, -1, -1,  1, -1,  1,  1, -1], &
            [D2h_order, D2h_order])

      integer, dimension(D2h_order), parameter :: D2h_Ag  = D2h_char_table(:, 1) ! (/1,  1,  1,  1,  1,  1,  1,  1/)
      integer, dimension(D2h_order), parameter :: D2h_B1g = D2h_char_table(:, 2) ! (/1,  1, -1, -1,  1,  1, -1, -1/)
      integer, dimension(D2h_order), parameter :: D2h_B2g = D2h_char_table(:, 3) ! (/1, -1,  1, -1,  1, -1,  1, -1/)
      integer, dimension(D2h_order), parameter :: D2h_B3g = D2h_char_table(:, 4) ! (/1, -1, -1,  1,  1, -1, -1,  1/)
      integer, dimension(D2h_order), parameter :: D2h_Au  = D2h_char_table(:, 5) ! (/1,  1,  1,  1, -1, -1, -1, -1/)      
      integer, dimension(D2h_order), parameter :: D2h_B1u = D2h_char_table(:, 6) ! (/1,  1, -1, -1, -1, -1,  1,  1/)
      integer, dimension(D2h_order), parameter :: D2h_B2u = D2h_char_table(:, 7) ! (/1, -1,  1, -1, -1,  1, -1,  1/)
      integer, dimension(D2h_order), parameter :: D2h_B3u = D2h_char_table(:, 8) ! (/1, -1, -1,  1, -1,  1,  1, -1/)
      !
      ! Note that the following character matrix is symmetric
      !
      integer, dimension(C2v_order, C2v_order), parameter :: C2v_char_table = &
            reshape([ &
            1,  1,  1,  1, &
            1,  1, -1, -1, &
            1, -1,  1, -1, &
            1, -1, -1,  1], &
            [C2v_order, C2v_order])

      integer, dimension(C2v_order), parameter :: C2v_A1 = C2v_char_table(:, 1) ! (/1,  1,  1,  1/)
      integer, dimension(C2v_order), parameter :: C2v_A2 = C2v_char_table(:, 2) ! (/1,  1, -1, -1/)
      integer, dimension(C2v_order), parameter :: C2v_B1 = C2v_char_table(:, 3) ! (/1, -1,  1, -1/)
      integer, dimension(C2v_order), parameter :: C2v_B2 = C2v_char_table(:, 4) ! (/1, -1, -1,  1/)
      !
      ! C2v group representations
      !
      integer, parameter :: REP_A1 = 1
      integer, parameter :: REP_A2 = 2
      integer, parameter :: REP_B1 = 3
      integer, parameter :: REP_B2 = 4
      !
      ! D2h group representations
      !
      integer, parameter :: REP_Ag = 1
      integer, parameter :: REP_B1g = 2
      integer, parameter :: REP_B2g = 3
      integer, parameter :: REP_B3g = 4
      integer, parameter :: REP_Au = 5
      integer, parameter :: REP_B1u = 6
      integer, parameter :: REP_B2u = 7
      integer, parameter :: REP_B3u = 8
      ! 
      ! CISD parameters
      !
      integer          :: CISD_GUESS_S = 5
      integer          :: CISD_GUESS_D = 0
      integer          :: CISD_GUESS_SD = 0
      ! 
      ! Coupled Cluster DIIS parameters
      !
      integer                     :: CC_DIIS_NMAX = 8
      integer                     :: CC_DIIS_NSTART = 3
      integer                     :: CC_DIIS_NRELAX = 3
      !
      ! MBPT ORDER for first order one electron properties
      !
      integer                     :: MBPT_ORDER = 3
      !
      ! S OPERATOR ORDER 
      !       
      integer                     :: S_ORDER = 3
      !
      ! MBPT ORDER FOR EXCITED TRANSITIONS
      !
      integer                     :: MAXPT = 3


      !
      ! Coupled Cluster amplitude and energy thresholds
      !
      real(F64)            :: CC_AMP_THRESH = 1.d-7
      real(F64)            :: CC_E_THRESH   = 1.d-6
      !
      ! Davidson convergence
      !
      real(F64)            :: DAV_QCONVTHRSH = 1.d-7
      !
      ! Threshold for finding degenerate energy levels in EOM-CC
      !
      real(F64)            :: CC_DEGEPS = 1.d-6
      real(F64)            :: EORB_DEGEPS = 1.d-7
      real(F64)            :: EXCI_DEGEPS = 1.d-20
      real(F64)            :: T1_DIAG = 2.d-1
      !
      ! Parameters for the Davidson subroutine used
      ! for converging EOM-CC excited-state wavefunctions
      ! ---
      ! Maximum residual vector of the EOM-CC non-symmetric
      ! eigenvalue problem
      !
      real(F64) :: CC_EOM_CONV_THRESH = 1.d-5
      real(F64) :: CI_CONV_THRESH = 1.d-3
      !
      ! Maximum number of iterations for the EOM
      ! diagonalization algorithm
      !
      integer :: CC_EOM_CONV_MAXIT  = 100
      !
      ! Maximum amount disk space (non-volatile memory)
      ! for the EOM-CC diagonalization algorithm (in bytes)
      !
      integer(I64) :: CC_EOM_DISKSPACE = 8 * 10**9_I64 
      !
      ! Maximum amount of volatile memory for the EOM-CC
      ! diagonalization algorithm (in bytes)
      !
      integer(I64) :: CC_EOM_MEMSPACE = 8 * 10**9_I64
      !
      ! ECP gradient
      !
      logical                     :: ECP_GRAD = .false.
      !
      ! Coupled Cluster Minimum HOMO_LUMO gap
      !
      real(F64)            :: CC_MIN_HLGAP = 5.d-1
      real(F64)            :: CC_ETA       = 0.d+0            
      !
      ! Symmetry parameters
      !
      integer                     :: GROUP_C2v = 1
      integer                     :: GROUP_D2h = 2
      !
      ! Global accuracy level
      !
      integer, parameter          :: ACC_LOW           = 0
      integer, parameter          :: ACC_NORMAL        = 1
      !
      ! Thresholds controlling accuracy of the Fock matrix build
      ! ---
      ! HFX_THRESH___ 
      ! HFC_THRESH___
      !
      ! Thresholds controlling screening D_{rq} (pq|rs) 
      ! and D_{rs} (pq|rs) contributions to the Fock matrix,
      ! respectively. D_{pq} is the density matrix element
      ! (non-idempotent, i.e. Tr(D) = 2 * NOCC). The matrix
      ! elements are calculated if
      ! D_{rq} (pq|rs) > HFX_THRESH___ / |KSCAL|,
      ! D_{rs} (pq|rs) > HFC_THRESH___ / |JSCAL|,
      ! where KSCAL and JSCAL are weights with which the 
      ! D_{pq} (rs|tu) terms contribute to the Fock/KS matrix. 
      ! In the case of restricted closed-shell Hartree-Fock,
      ! the weights are
      ! |KSCAL| = 1/2
      ! |JSCAL| = 1
      ! In case of PBE0 hybrid GGA (exchange en.: 0.75 E_{PBEX} 
      ! + 0.25E_{HFX}), weights are
      ! |KSCAL| = 1/8
      ! |JSCAL| = 1
      ! The inclusion of 1/|KSCAL| and 1/|JSCAL| weights ensures
      ! good error balancing between Coulomb and exchange
      ! contributions.
      !
      !
      real(F64), parameter :: HFX_THRESH_LOW    = 1.d-11
      real(F64), parameter :: HFC_THRESH_LOW    = 1.d-11
      
      real(F64), parameter :: HFX_THRESH_NORMAL = 1.d-11
      real(F64), parameter :: HFC_THRESH_NORMAL = 1.d-11
      ! ----------------------------------------------------------
      ! If SEPARATEJK flag is set to .TRUE., memory is
      ! allocated so that Coulomb and HF exchange (or exchange
      ! and correlation) contributions can be stored in separate
      ! matrices.
      !
      logical                     :: SEPKSCONTRIB = .false.
      ! ----------------------------------------------------
      ! RESTRICTED OPEN-SHELL KOHN-SHAM CALCULATIONS
      ! ----------------------------------------------------
      ! ROKS_ENABLED flag is set to .TRUE. if restricted
      ! open-shell Kohn-Sham description of the system
      ! is requested. ROKS is .FALSE. if closed-shell
      ! Kohn-Sham/Hartree-Fock calculations are requested.
      !
      logical                     :: ROKS_ENABLED = .false.
      !
      ! Number of orbitals belonging to the open shell.
      ! These orbitals can be occupied, possibly fractionally,
      ! by both alpha and beta spins.
      !
      integer                     :: ROKS_NOPENORB = 0
      !
      ! Number of alpha (beta) electrons in the open shell.
      ! Number of electrons in the open shell can be a
      ! non-integer number. The unit is 1/ROKS_NEUNIT
      ! of an electron.
      !
      integer                     :: ROKS_NOPENELA = 0
      integer                     :: ROKS_NOPENELB = 0
      !
      ! Total, possibly non-integer, number of electrons in
      ! restricted open-shell Kohn-Sham calculations.
      ! The unit is 1/ROKS_NEUNIT of an electron.
      !
      integer                     :: ROKS_NE = 0
      !
      ! The unit used in accounting for non-integer electrons
      ! in restricted open-shell Kohn-Sham calculations.
      !
      integer, parameter          :: ROKS_NEUNIT = 1000
      ! !
      ! ! AO density treshold for accuracy change
      ! ! of KS matrix evaluation:
      ! ! (low quality) --> (normal quality)
      ! !
      ! real(F64), parameter :: RHOTHRESH = 1.d-1
      integer :: GACC = ACC_NORMAL
      ! -----------------------------------------------------------
      ! A cube file contains datapoints represented on
      ! the moleclar grid. The user can request a cubefile
      ! containing electron density, the Laplacian, MOs, and
      ! other functions implemented on the grid.
      ! -----------------------------------------------------------
      !
      ! Type of a function represented on the molecular grid.
      ! The subroutine for generating cube files will not be
      ! called if CUBEFILE_FUNC == VOL_FUNC_NONE.
      !
      integer :: CUBEFILE_FUNC = VOL_FUNC_NONE
      !
      ! Spacing of data points inside the grid volume
      !
      integer :: CUBEFILE_SPACING = VOL_SPACING_MEDIUM
      !
      ! Relevant for a cube file generation: Indices of
      ! the molecular orbitals requested to be represented
      ! on the grid.
      !
      integer, dimension(:), allocatable :: CUBEFILE_MO_IDX
      !
      ! Relevant for a cube file generation: Indices of
      ! atoms on which the requested atomic orbitals are
      ! centered. 
      !
      integer, dimension(:), allocatable :: CUBEFILE_AO_CENTERS
      !
      ! Number of OpenMP threads. The user-specified number
      ! of threads is set by the launcher script and subsequently
      ! read by a library function call in the main code.
      !
      integer :: OMP_NTHREAD = 1
      !
      ! Maximum angular momentum of a Cartesian Gaussian
      ! function. When changing this value, don't forget to
      ! 1) test the accuracy thresholds for the Boys function
      ! (BOYS module); 2) re-run the program for automatic
      ! two-electron repulsion integrals; 3) test if the program
      ! requires increasing the stack size for the main thread
      ! and worker threads (that's due to the increase of array
      ! dimensions).
      !
      integer, parameter :: MAX_L = 5 ! H functions
      !
      ! The maximum number of Cartesian gaussian functions
      ! corresponding to MAX_L
      !
      integer, parameter :: MAX_NFUNC   = ((MAX_L + 1) * (MAX_L + 2)) / 2
      !
      ! Maximum number of Cartesian gaussian primitives in
      ! a single contracted orbital
      !
      integer, parameter :: MAX_NPRM    = 30
      !
      ! Contraction coefficients. The contraction coefficient
      ! of I-th primitive in K-th shell should be read from
      ! CNTR(I, K), I = 1, 2, ..., NPRM(K).      
      ! Note that this contraction coefficients do not depend
      ! on the index of a angular function. In contrast, the
      ! normalization coefficients stored in the CNTRNORM 
      ! array do depend on the angular function, but do not
      ! depend on the index of a primitive Gaussian function.
      ! The contracted Gaussian function constructed with CNTR
      ! coefficients should be normalized using CNTRNORM array.
      ! 
      real(F64), dimension(:, :), allocatable :: CNTR
      !
      ! Normalization constants of contracted Gaussian orbitals.
      ! The normalization constant for I-th angular function in
      ! K-th shell is stored in
      ! CNTRNORM(I, K), I = 1, 2, ..., NFUNC(SHTYPE(K))
      ! The contracted Gaussian function shuld be contracted
      ! with the elements of CNTR.
      !
      real(F64), dimension(:, :), allocatable :: CNTRNORM
      !
      ! Number of Cartesian Gaussian atomic orbitals. NORB does
      ! not depend on the usage of a spherical basis set or the
      ! exclusion of linear-dependent vectors from the SCF
      ! variational space.
      !
      integer :: NORB
      !
      ! SHATOM(K) is the index of an atom on which K-th shell
      ! orbitals are centered.
      !
      integer, dimension(:), allocatable :: SHATOM
      !
      ! Number of elements present in the currently
      ! loaded molecule (including dummy centers)
      !
      integer :: NELEMENT
      !
      ! Z numbers of the elements present in currently
      ! loaded molecule (including dummy centers)
      !
      integer, dimension(:), allocatable :: ELEMENT
      !
      ! Mapping from atom index to element index.
      ! For example, the K-th atom Z number is equal to
      ! ELEMENT(ELEMENT_IDX(K)).
      !
      integer, dimension(:), allocatable :: ELEMENT_IDX
      !
      ! -----------------------------------------------------
      ! GEOMETRY/ELECTRONIC STRUCTURE CHARACTERISTICS
      ! -----------------------------------------------------
      ! CHARGE - Charge of molecule
      !
      ! NE     - Number of electrons
      !
      ! SH     - Index of shell from which AO's
      !          contraction coefficients,
      !          exponents and normalization
      !          constants can be derived
      !
      ! NSHELL - Number of shells in all atoms
      !
      ! MAX_ & - Max. number of shells per atom
      ! ATOM &
      ! NSHELL
      !
      ! NATOM  - Number of atoms (dummy atoms & real atoms)
      !
      ! ATOMR    - Positions of nuclei
      !
      ! CHCENTER - Charge center of the nuclei
      !
      !
      ! MCENTER - Mass center of the nuclei
      !                            
      ! ORIGIN - Molecule center (mass or charge)
      !                                     
      ! NUCLDIP - Nuclear contribution to molecular
      !          dipole moment. (Assumed origin
      !          in charge center)
      !
      ! NUCLQUAD - Nuclear contribution to molecular
      !          quadrupole moment. (Assumed origin
      !          in charge center)
      !
      ! NUCLQUADM - Nuclear contribution to molecular
      !          quadrupole moment. (Assumed origin
      !          in mass center)  
      !
      ! NUSHELL- Number of unique shells,
      !          property of basis set
      !
      ! NSHELL - Number of shells
      !
      ! DNUCLZ - Charges of nuclei (floating point)
      !
      ! INUCLZ - Charges of nuclei (integer)
      !
      ! ATOML  - ATOML(I) contains
      !          maximum Cartesian angular
      !          momentum of I-th atom
      !
      ! SH0    - SH0(I) stores index
      !          of first entry in SH
      !          belonging to I-th atom.
      !          SH(SH0(I)) would be shell
      !          of I-th atom's first AO
      !          SH0(NATOM + 1) = NSHELL + 1
      !
      ! IDX    - IDX(I) stores index
      !          of first AO belonging to
      !          I-th atom.
      !          IDX(NATOM + 1) = NORB + 1
      !
      ! LL     - L-exponent of Cartesian
      !          polynomial X^L Y^M Z^N
      !          (integer)
      !
      ! MM     - M-exponent of Cartesian
      !          polynomial X^L Y^M Z^N
      !          (integer)
      !
      ! NN     - N-exponent of Cartesian
      !          polynomial X^L Y^M Z^N
      !          (integer)
      !
      ! SHTYPE - SHTYPE(I) stores type of
      !          of I-th shell: s, p, d, f, ...
      !
      ! NPRM   - NPRIM(I) stores number of
      !          primitive Gaussians
      !          in I-th shell
      !
      ! EXPN   - EXPN(:, I) stores exponents
      !          of I-th shell's Gaussians
      !
      ! NRML   - Normalization coefficients
      !
      ! DIST   - NATOM x NATOM matrix of distances
      !          between nuclei. Sorted, see: IDIST
      !
      !
      ! DISTUN - Unsorted version of DIST
      !
      ! IDIST  - NATOM x NATOM matrix containing
      !          indices of nuclei sorted
      !          in ascending order according
      !          to DIST matrix entries
      !
      ! SHPOS  - SHPOS(M) contains index of first
      !          orbital belonging to M-th shell.
      !          SHPOS(NSHELL + 1) = NORB + 1
      !
      ! R2MAX  - R2MAX(M) stores square of maximum radii
      !          where AO derived from M-th unique is
      !          non-negligible
      !
      real(F64)                                                  :: CHARGE
      integer                                                    :: MULTIPLICITY
      integer                                                    :: NE
      integer, dimension(:), allocatable                         :: sh
      integer                                                    :: nushell
      integer                                                    :: nshell
      integer                                                    :: max_atomnshell
      integer                                                    :: natom
      integer, dimension(2, 2)                                   :: REAL_ATOMS
      real(F64), dimension(:, :), allocatable                    :: atomr
      real(F64), dimension(:), allocatable                       :: dnuclz
      integer, dimension(:), allocatable                         :: inuclz
      integer, dimension(:), allocatable                         :: atoml
      integer, dimension(:), allocatable                         :: IDX
      integer, dimension(:), allocatable                         :: sh0
      integer, dimension(MAX_NFUNC, 0:MAX_L)                     :: ll
      integer, dimension(MAX_NFUNC, 0:MAX_L)                     :: mm
      integer, dimension(MAX_NFUNC, 0:MAX_L)                     :: nn
      integer, dimension(:), allocatable                         :: shtype
      integer, dimension(:), allocatable                         :: nprm
      real(F64), dimension(:, :), allocatable                    :: expn
      real(F64), dimension(:, :, :), allocatable                 :: nrml
      real(F64), dimension(:, :), allocatable                    :: dist
      real(F64), dimension(:, :), allocatable                    :: distun
      integer, dimension(:, :), allocatable                      :: idist
      integer, dimension(:), allocatable                         :: shpos
      real(F64), dimension(3)                                    :: chcenter
      real(F64), dimension(3)                                    :: mcenter
      real(F64), dimension(3)                                    :: origin
      real(F64), dimension(3)                                    :: nucldip
      real(F64), dimension(6)                                    :: nuclquad
      real(F64), dimension(6)                                    :: nuclquadm
      real(F64), dimension(:), allocatable                       :: r2max

      target :: ll, mm, nn

contains

      pure function NRealAtoms()
            integer :: NRealAtoms
            integer :: s

            NRealAtoms = 0
            do s = 1, 2
                  NRealAtoms = NRealAtoms + REAL_ATOMS(2, s) - REAL_ATOMS(1, s) + 1
            end do
      end function NRealAtoms

      
      pure function isdummy(idx)
            !
            ! Check if IDX belongs to a dummy (fictitious, ghost) atom.
            ! A dummy atom has no charge, but there are basis functions
            ! centered on it, as well as additional numerical grid points
            ! in DFT calculations. 
            !
            logical             :: isdummy
            integer, intent(in) :: idx

            integer :: s
            
            isdummy = .true.
            do s = 1, 2
                  if (idx >= REAL_ATOMS(1, s) .and. idx <= REAL_ATOMS(2, s)) then
                        isdummy = .false.
                  end if
            end do
      end function isdummy


      subroutine gparam_free()
            ! type(tjobqueue), pointer :: prev, next

            ! prev => JOBQ%next
            ! do while (associated(prev))
            !       next => prev%next
            !       deallocate(prev)
            !       prev => next
            ! end do
      end subroutine gparam_free

      
      subroutine enqueue_job(xyz, par, id)
            type(tmolecule), intent(in) :: xyz
            type(tsystemdep_params), intent(in) :: par
            integer, intent(in)         :: id

            type(tjobqueue), pointer :: newnode

            if (NJOB .eq. 0) then
                  allocate(JOBQ)
                  call copymolecule(JOBQ%xyz, xyz)
                  JOBQ%id = id
                  JOBQ%par = par
                  JOBQ_FIRST => JOBQ
            else
                  allocate(newnode)
                  JOBQ%next => newnode
                  nullify(newnode)
                  JOBQ => JOBQ%next
                  call copymolecule(JOBQ%xyz, xyz)
                  JOBQ%id = id
                  JOBQ%par = par
            end if

            NJOB = NJOB + 1
            if (id == GEOM_HIRSHFELD_ATOM) then
                  NHIRSH = NHIRSH + 1
            end if
      end subroutine enqueue_job
      

      subroutine dequeue_job(xyz, par, k, id)
            !
            ! Search the job queue for the K-th job in the subset of jobs
            ! of the ID kind.
            !
            type(tmolecule), intent(out) :: xyz
            type(tsystemdep_params), intent(out) :: par
            integer, intent(in)          :: k
            integer, intent(in)          :: id

            integer :: m, n
            logical :: found
            type(tjobqueue), pointer :: current_job

            current_job => JOBQ_FIRST

            m = 0
            found = .false.
            jobsearch: do n = 1, NJOB
                  if (current_job%id == id) then
                        m = m + 1
                        if (m == k) then
                              found = .true.
                              exit jobsearch
                        end if
                  end if
                  current_job => current_job%next
            end do jobsearch

            if (.not. found) then
                  call msg("REQUESTED JOB NOT PRESENT IN THE JOB QUEUE", MSG_ERROR)
                  stop
            end if
            !
            ! Return XYZ data
            !
            call copymolecule(xyz, current_job%xyz)
            par = current_job%par
      end subroutine dequeue_job


      subroutine copymolecule(molout, molin)
            type(tmolecule), intent(out) :: molout
            type(tmolecule), intent(in)  :: molin

            integer :: natom

            molout%path = molin%path
            molout%start = molin%start
            natom = molin%natom
            molout%natom = natom
            if (allocated(molout%inuclz)) deallocate(molout%inuclz)
            allocate(molout%inuclz(natom))
            molout%inuclz(:) = molin%inuclz
            if (allocated(molout%atomr)) deallocate(molout%atomr)
            allocate(molout%atomr(3, natom))
            molout%atomr(:, :) = molin%atomr
            molout%real_atoms = molin%real_atoms
            molout%charge = molin%charge
            molout%multiplicity = molin%multiplicity
            molout%charge_frac = molin%charge_frac
            molout%ne = molin%ne
            molout%ne_frac = molin%ne_frac
            molout%nopenorb = molin%nopenorb
            molout%nopenela = molin%nopenela
            molout%nopenela_frac = molin%nopenela_frac
            molout%nopenelb = molin%nopenelb
            molout%nopenelb_frac = molin%nopenelb_frac
            molout%nelement = molin%nelement
            allocate(molout%zcount(molout%nelement))
            allocate(molout%zlist(molout%nelement))
            molout%zcount(:) = molin%zcount
            molout%zlist(:) = molin%zlist
      end subroutine copymolecule

      
      subroutine molecule_init(a, coords, nuclz, charge, multiplicity, &
            real_atoms, frac_conf)
            
            class(tmolecule), intent(out)               :: a
            real(F64), dimension(:, :), intent(in)      :: coords
            integer, dimension(:), intent(in)           :: nuclz
            integer, intent(in)                         :: charge
            integer, intent(in)                         :: multiplicity
            integer, dimension(2, 2), optional, intent(in) :: real_atoms
            integer, dimension(4), optional, intent(in) :: frac_conf

            integer :: zsum, NUnpair
            integer :: k, l, z
            integer, dimension(KNOWN_ELEMENTS) :: element_count

            a%natom = size(nuclz)
            allocate(a%atomr(3, a%natom))
            a%atomr(:, :) = coords
            allocate(a%inuclz(a%natom))
            a%inuclz(:) = nuclz
            
            if (present(real_atoms)) then
                  a%real_atoms = real_atoms
            else
                  a%real_atoms(:, 1) = [1, a%natom]
                  a%real_atoms(:, 2) = [1, 0]
            end if
            !
            ! Elemental composition of the molecule
            !
            a%nelement = 0
            element_count = 0
            do k = 1, a%natom
                  z = nuclz(k)
                  if (element_count(z) == 0) then
                        a%nelement = a%nelement + 1
                  end if
                  element_count(z) = element_count(z) + 1
            end do

            allocate(a%zcount(a%nelement))
            allocate(a%zlist(a%nelement))

            zsum = 0
            l = 1
            kloop: do k = 1, KNOWN_ELEMENTS
                  if (element_count(k) > 0) then
                        a%zcount(l) = element_count(k)
                        a%zlist(l) = k
                        zsum = zsum + element_count(k) * k
                        l = l + 1
                        if (l > a%nelement) exit kloop
                  end if
            end do kloop
            !
            ! Charge and open-shell configuration
            !
            a%charge = charge
            a%ne = zsum - charge
            a%multiplicity = multiplicity
            NUnpair = multiplicity - 1
            a%nopenela = NUnpair
            a%nopenelb = 0
            a%nopenorb = NUnpair
            !
            ! Restricted open-shell SCF computations with
            ! a fractional number of alpha/beta electrons and
            ! an arbitrary number of orbitals in the open 
            ! shell
            !
            if (present(frac_conf)) then
                  a%nopenorb = frac_conf(1)
                  a%charge_frac = frac_conf(2) 
                  a%nopenela_frac = frac_conf(3)
                  a%nopenelb_frac = frac_conf(4)
                  a%ne_frac = zsum * ROKS_NEUNIT - a%charge_frac
            else
                  a%charge_frac = charge * ROKS_NEUNIT
                  a%ne_frac = (zsum - charge) * ROKS_NEUNIT
                  a%nopenela_frac = NUnpair * ROKS_NEUNIT
                  a%nopenelb_frac = 0
            end if
      end subroutine molecule_init


      subroutine gencation(cation, neutral)
            !
            ! Generate cation configuration from a neutral molecule.
            ! The spin multiplicity is either singlet or doublet.
            !
            type(tmolecule), intent(out) :: cation
            type(tmolecule), intent(in)  :: neutral

            call copymolecule(cation, neutral)
            cation%charge = cation%charge + 1
            cation%charge_frac = cation%charge * ROKS_NEUNIT
            cation%ne = cation%ne - 1
            if (modulo(cation%ne, 2) == 0) then
                  cation%multiplicity = 1
            else
                  cation%multiplicity = 2
            end if
            cation%ne_frac = cation%ne * ROKS_NEUNIT
            cation%nopenorb = 1
            cation%nopenela = 1
            cation%nopenela_frac = cation%nopenela * ROKS_NEUNIT
            cation%nopenelb = 0
            cation%nopenelb_frac = cation%nopenelb * ROKS_NEUNIT
      end subroutine gencation


      subroutine genanion(anion, neutral)
            !
            ! Generate anion from a neutral molecule.
            ! The spin multiplicity is either singlet or doublet.
            !
            type(tmolecule), intent(out) :: anion
            type(tmolecule), intent(in)  :: neutral

            call copymolecule(anion, neutral)
            anion%charge = anion%charge - 1
            anion%charge_frac = anion%charge * ROKS_NEUNIT
            anion%ne = anion%ne + 1
            if (modulo(anion%ne, 2) == 0) then
                  anion%multiplicity = 1
            else
                  anion%multiplicity = 2
            end if
            anion%ne_frac = anion%ne * ROKS_NEUNIT
            anion%nopenorb = 1
            anion%nopenela = 1
            anion%nopenela_frac = anion%nopenela * ROKS_NEUNIT
            anion%nopenelb = 0
            anion%nopenelb_frac = anion%nopenelb * ROKS_NEUNIT
      end subroutine genanion


      subroutine pack_systemdep_params(p, main_job, thresh_density, thresh_gradient)
            type(tsystemdep_params), intent(out) :: p
            logical, intent(in)                  :: main_job
            real(F64), optional, intent(in)      :: thresh_density
            real(F64), optional, intent(in)      :: thresh_gradient

            p%lcomega = LCOMEGA
            p%lcsrexx = LCSREXX
            
            if (present(thresh_density)) then
                  p%scf_thresh_density = thresh_density
            else
                  p%scf_thresh_density = SCF_THRESH_DENSITY
            end if

            if (present(thresh_gradient)) then
                  p%scf_thresh_gradient = thresh_gradient
            else
                  p%scf_thresh_gradient = SCF_THRESH_GRADIENT
            end if

            if (main_job) then
                  p%main_job = .true.
                  p%optomega_min = OPTOMEGA_MIN
                  p%optomega_max = OPTOMEGA_MAX
                  p%optomega_j2type = OPTOMEGA_J2TYPE
                  p%scf_guess = SCF_GUESS
                  if (allocated(SCF_GUESS_RHOA_PATH)) &
                        p%scf_guess_rhoa_path = SCF_GUESS_RHOA_PATH
                  if (allocated(SCF_GUESS_RHOB_PATH)) &
                        p%scf_guess_rhob_path = SCF_GUESS_RHOB_PATH
                  p%scf_save_rho_mode = SCF_SAVE_RHO_MODE
                  if (allocated(SCF_SAVE_RHOA_PATH)) &
                        p%scf_save_rhoa_path = SCF_SAVE_RHOA_PATH
                  if (allocated(SCF_SAVE_RHOB_PATH)) &
                        p%scf_save_rhob_path = SCF_SAVE_RHOB_PATH
            else
                  p%main_job = .false.
            end if
      end subroutine pack_systemdep_params


      subroutine unpack_systemdep_params(p)
            type(tsystemdep_params), intent(in) :: p

            LCOMEGA = p%lcomega
            LCSREXX = p%lcsrexx
            SCF_THRESH_DENSITY = p%scf_thresh_density
            SCF_THRESH_GRADIENT = p%scf_thresh_gradient
            if (p%main_job) then
                  OPTOMEGA_MIN = p%optomega_min
                  OPTOMEGA_MAX = p%optomega_max
                  OPTOMEGA_J2TYPE = p%optomega_j2type
                  SCF_GUESS = p%scf_guess
                  if (allocated(p%scf_guess_rhoa_path)) then
                        SCF_GUESS_RHOA_PATH = p%scf_guess_rhoa_path
                  end if
                  if (allocated(p%scf_guess_rhob_path)) then
                        SCF_GUESS_RHOB_PATH = p%scf_guess_rhob_path
                  end if
                  SCF_SAVE_RHO_MODE = p%scf_save_rho_mode
                  if (allocated(p%scf_save_rhoa_path)) then
                        SCF_SAVE_RHOA_PATH = p%scf_save_rhoa_path
                  end if
                  if (allocated(p%scf_save_rhob_path)) then
                        SCF_SAVE_RHOB_PATH = p%scf_save_rhob_path
                  end if
            else
                  SCF_GUESS = SCF_GUESS_DEFAULT
                  if (allocated(SCF_GUESS_RHOA_PATH)) deallocate(SCF_GUESS_RHOA_PATH)
                  if (allocated(SCF_GUESS_RHOB_PATH)) deallocate(SCF_GUESS_RHOB_PATH)
                  if (allocated(SCF_SAVE_RHOA_PATH)) deallocate(SCF_SAVE_RHOA_PATH)
                  if (allocated(SCF_SAVE_RHOB_PATH)) deallocate(SCF_SAVE_RHOB_PATH)
                  SCF_SAVE_RHO_MODE = FILEMODE_NONE
            end if
      end subroutine unpack_systemdep_params
end module gparam

