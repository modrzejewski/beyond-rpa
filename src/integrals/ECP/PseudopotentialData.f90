module PseudopotentialData
      use arithmetic
      use auto2e
      use gparam
      
      implicit none
      !
      ! Maximum allowed angular momentum of a Gaussian orbital
      ! in ECP calculations. Add N to MAX_L (the global constraint
      ! on the max oribtal momentum of an orbital) to account
      ! for Nth order derivatives of the pseudopotential.
      !
      integer, parameter :: ECP_GTO_MAXL = AUTO2E_MAXL + 1
      !
      ! Maximum number of angular functions in a single shell
      !
      integer, parameter :: ECP_GTO_MAX_NFUNC  = ((ECP_GTO_MAXL + 1) * (ECP_GTO_MAXL + 2)) / 2
      !
      ! Parameters describing a GTO orbital
      !
      type tgtodef
            !
            ! Angular momentum
            !
            integer :: l = -1
            !
            ! Number of primitives
            !
            integer :: nprm = -1
            !
            ! Contraction coefficients (independent
            ! of angular function index)
            !
            real(F64), dimension(MAX_NPRM) :: cntr
            !
            ! Normalization coefficients for each
            ! angular function belonging to the GTO shell
            !
            real(F64), dimension(ECP_GTO_MAX_NFUNC) :: norm
            !
            ! Exponents of each primitive GTO
            !
            real(F64), dimension(MAX_NPRM) :: expn
      end type tgtodef
      !
      ! Parameters passed to an integrand in one-dimensional
      ! quadrature over the radial coordinate
      !
      type TINT_PARAMS
            !
            ! Bra orbital definition
            !
            type(tgtodef) :: phia
            !
            ! Ket orbital definition
            !
            type(tgtodef) :: phib
            !
            ! Angular momentum onto which pseudopotenial
            ! projects. L value is used to read the proper
            ! set exponents and coefficients of the ECP
            ! for U_L(R)
            !
            integer :: l
            !
            ! Index of ECP center. ECPCENTER is used to read
            ! the appropriate coefficients, exponents, NKLs,
            ! and number of Gaussian terms for the ECP
            !
            integer :: ecpcenter
            !
            ! First element of ECP_EXPN (ECP_COEFF) array
            ! from which exponents (coefficients) can be read.
            ! K0 increases by ECP_NGAUSS(2+L, ECPCENTER) every
            ! time L increases by 1. This piece of information
            ! is redundant if L is known. It is kept for convenience.
            !
            integer :: k0
            !
            ! Length of a vector pointing from ECP center C to atom A (B)
            ! where orbital \Phi_a (\Phi_b) is centered on
            !
            real(F64) :: lena
            real(F64) :: lenb
            !
            ! lambda_1 and lambda_2 of Eq. 21 in [1]
            !
            integer :: lambdaa
            integer :: lambdab
            !
            ! Alpha and Beta of Eq. 21 in [1]
            !
            integer :: alpha
            integer :: beta
            integer :: n

            integer :: iprma
            integer :: iprmb
            real(F64) :: klen
      end type TINT_PARAMS
      ! --------------------------------------------------------
      ! Array bounds for scratch arrays
      ! --------------------------------------------------------
      integer :: ECP_SLMA
      integer :: ECP_SLMB
      integer :: ECP_SLMK
      integer :: ECP_XYZWORK
      integer, dimension(4) :: ECP_TAB
      integer, dimension(3) :: ECP_TAC
      integer, dimension(2) :: ECP_TCHIAC
      integer, dimension(2) :: ECP_TCHIAB
      integer :: ECP_TCC
      ! --------------------------------------------------------
      real(F64), dimension(:), allocatable :: ECP_U
      real(F64), dimension(:, :), allocatable :: ECP_BINOM
      real(F64), dimension(:, :, :), allocatable :: ECP_OMEGA
      integer :: ECP_ULMAX
      !
      ! Angular momentum operator (3-component vector) in the basis of
      ! real spherical harmonics
      !
      real(F64), dimension(:, :, :, :), allocatable :: ECP_LVECTOR
      ! -------------------------------------------------------
      ! ADAPTIVE GAUSS-CHEBYSHEV QUADRATURE
      ! -------------------------------------------------------
      real(F64), dimension(:), allocatable :: CHEB_X1
      real(F64), dimension(:), allocatable :: CHEB_W1
      real(F64), dimension(:), allocatable :: CHEB_X2
      real(F64), dimension(:), allocatable :: CHEB_W2
      real(F64), dimension(:), allocatable :: CHEB_X3
      real(F64), dimension(:), allocatable :: CHEB_W3
      integer, parameter :: CHEB_N1 = 7
      integer, parameter :: CHEB_NMAX = 4095
      !
      ! CHEB_EPS parameter controls the accuracy of numerical integration.
      ! Numerical quadrature is used when the pseudopotential and the bra/ket
      ! orbitals are centered on different atoms.
      !
      real(F64), parameter :: CHEB_EPS = 1.0E-12_F64
      ! --------------------------------------------------------
      ! PARAMETERS OF THE EFFECTIVE CORE POTENTIAL
      ! --------------------------------------------------------
      logical                                     :: ECP_ENABLED = .false.
      logical                                     :: ECP_SPIN_ORBIT = .false.
      integer                                     :: ECP_NELEMENTS
      integer, dimension(KNOWN_ELEMENTS)          :: ECP_IELEMENT
      integer, dimension(:), allocatable          :: ECP_K0
      integer, dimension(:), allocatable          :: ECP_LMAX
      integer, dimension(:, :), allocatable       :: ECP_NGAUSS
      integer, dimension(:), allocatable          :: ECP_NKL
      integer, dimension(:), allocatable          :: ECP_NCORE
      real(F64), dimension(:), allocatable        :: ECP_COEFF
      !
      ! Expansion coefficients of the spin-orbit
      ! part of the pseudopotential. The same subroutine loads
      ! the coefficients for the spin-averaged and spin-orbit parts
      ! of the pseudopotential.
      !
      real(F64), dimension(:), allocatable        :: ECP_SO_COEFF
      real(F64), dimension(:), allocatable        :: ECP_EXPN
      integer, dimension(:), allocatable          :: ECP_ZNUM
      ! --------------------------------------------------------
      ! LIST OF ECP CENTERS PRESENT IN THE MOLECULE
      ! --------------------------------------------------------
      integer                                     :: ECP_NATOM = 0
      integer, dimension(:), allocatable          :: ECP_ATOM
      integer, dimension(:), allocatable          :: ECP_INUCLZ
      logical, dimension(:), allocatable          :: ECP_LOCALPP

      logical                                     :: ECP_CALCGRAD = .false.
      integer, dimension(:), allocatable          :: ECP_ANGFIDX
      integer, dimension(:, :), allocatable       :: ECP_LL, ECP_MM, ECP_NN
      !
      ! Chebyshev interpolation of modified spherical Bessel functions
      !
      real(F64), parameter :: INTERP_XMAX = 25.0_F64
      real(F64), parameter :: INTERP_XMIN = 0.50_F64
      integer, parameter :: INTERP_N = 9
      integer, parameter :: NINTERVALS = 800
      real(F64), parameter :: DELTAX = INTERP_XMAX / real(NINTERVALS, F64)
      real(F64), dimension(:, :, :), allocatable :: BESSEL_TABLE
      real(F64), dimension(:, :), allocatable :: BESSEL_ASYMP
end module PseudopotentialData
