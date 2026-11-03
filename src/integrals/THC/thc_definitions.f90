module thc_definitions
      use arithmetic
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
            integer   :: THC_BeckeGridKind = BECKE_PARAMS_SG1
            real(F64) :: QRThresh = 1.0E-3_F64
            real(F64) :: QRThreshReduced = 1.0E-3_F64
            integer   :: THC_BlockDim = 500
            logical   :: THC_QuadraticMemory = .false.
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
            ! next larger grid. Zero keeps the orders of THC_BeckeGridKind.
            ! The keyword THC_LebedevLmin sets every element. On top of it,
            ! thc_CoreLebedevLmin raises the elements from sodium on to the
            ! order their core shells need.
            !
            integer, dimension(KNOWN_ELEMENTS) :: LebedevLmin = 0
            !
            ! Number of radial shells of the parent grid. Zero keeps
            ! the number defined by THC_BeckeGridKind.
            !
            integer   :: NRadial = 0
      end type TTHCParams
end module thc_definitions
