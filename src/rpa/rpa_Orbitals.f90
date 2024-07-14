module rpa_Orbitals
      use arithmetic
      use real_linalg
      use math_constants
      use display
      use string
      use OneElectronInts
      use basis_sets
      use rpa_definitions

      implicit none
      
contains

      ! subroutine rpa_LocalizedOrbitals(LijLoc, Cpi, NOcc, RPAParams, AOBasis)
      !       real(F64), dimension(:, :), intent(out) :: LijLoc
      !       real(F64), dimension(:, :), intent(in)  :: Cpi
      !       integer, intent(in)                     :: NOcc
      !       type(TRPAParams), intent(in)            :: RPAParams
      !       type(TAOBasis), intent(in)              :: AOBasis

      !       real(F64), dimension(:, :), allocatable :: Spq, Qkp, Uik
      !       real(F64), dimension(:, :), allocatable :: Dpq, Dkq, Dkl
      !       real(F64), dimension(:, :), allocatable :: Cki
      !       integer :: NAO, NOAO, NOccLoc

      !       NAO = AOBasis%NAOSpher
      !       allocate(Spq(NAO, NAO))
      !       call ints1e_S(Spq, AOBasis)
      !       call real_PivotedCholesky(Qkp, Spq, NOAO, RPAParams%LOLinDepThresh)
      !       allocate(Dpq(NAO, NAO))
      !       allocate(Dkq(NOAO, NAO))
      !       allocate(Dkl(NOAO, NOAO))
      !       call real_abT(Dpq, Cpi(:, 1:NOcc), Cpi(:, 1:NOcc))
      !       call real_ab(Dkq, Qkp, Dpq)
      !       call real_abT(Dkl, Dkq, Qkp)
      !       call real_PivotedCholesky(Uik, Dkl, NOccLoc, 0.1_F64)
      !       if (NOccLoc /= NOcc) then
      !             call msg("Cholesky decomposition produced wrong number of occupied orbitals: "//str(NOccLoc), MSG_ERROR)
      !             error stop
      !       end if
      !       allocate(Cki(NOAO, NOcc))
      !       call real_ab(Cki, Qkp, Cpi(:, 1:NOcc))
      !       call real_aTbT(LijLoc, Cki, Uik)
      ! end subroutine rpa_LocalizedOrbitals


      subroutine rpa_LocalizeOrbitals_AquilanteJCP2006(LijLoc, Cpi, NOcc, RPAParams, AOBasis)
            real(F64), dimension(:, :), intent(out) :: LijLoc
            real(F64), dimension(:, :), intent(in)  :: Cpi
            integer, intent(in)                     :: NOcc
            type(TRPAParams), intent(in)            :: RPAParams
            type(TAOBasis), intent(in)              :: AOBasis

            real(F64), dimension(:, :), allocatable :: Dpq, Uip, SUpj, Spq
            integer :: NAO, NOccLoc
            integer :: p
            real(F64) :: MaxDpp
            real(F64) :: CholeskyThresh

            NAO = AOBasis%NAOSpher
            allocate(Dpq(NAO, NAO))
            call real_abT(Dpq, Cpi(:, 1:NOcc), Cpi(:, 1:NOcc))
            MaxDpp = ZERO
            !$omp parallel do private(p) reduction(max: MaxDpp)
            do p = 1, NAO
                  MaxDpp = max(MaxDpp, Dpq(p, p))
            end do
            !$omp end parallel do
            CholeskyThresh = RPAParams%LoLinDepThresh * MaxDpp
            call real_PivotedCholesky(Uip, Dpq, NOccLoc, CholeskyThresh)
            if (NOccLoc < NOcc) then
                  call msg("Cholesky decomposition produced wrong number of occupied orbitals: "//str(NOccLoc), MSG_ERROR)
                  error stop
            end if
            allocate(Spq(NAO, NAO))
            call ints1e_S(Spq, AOBasis)
            allocate(SUpj(NAO, NOcc))
            call real_abT_x(SUpj, NAO, Spq, NAO, Uip, NOccLoc, &
                  NAO, NOcc, NAO, ONE, ZERO)
            call real_aTb(LijLoc, Cpi, SUpj)
      end subroutine rpa_LocalizeOrbitals_AquilanteJCP2006

      
      subroutine rpa_ProjectOrbitals(Cpa_Projection, NMO_Projection, Cpa_Full, &
            AOBasis, Subspace, Thresh)
            
            real(F64), dimension(:, :), allocatable, intent(out) :: Cpa_Projection
            integer, intent(out)                                 :: NMO_Projection
            real(F64), dimension(:, :), intent(in)               :: Cpa_Full
            type(TAOBasis), intent(in)                           :: AOBasis
            real(F64), dimension(:, :), intent(in)               :: Subspace
            real(F64), intent(in)                                :: Thresh

            integer :: NAO, NMO_Subspace, NMO_Full
            real(F64), dimension(:, :), allocatable :: Spq, Spb, Sab, U, V
            real(F64), dimension(:), allocatable :: Sigma
            integer :: k

            NAO = size(Cpa_Full, dim=1)
            NMO_Full = size(Cpa_Full, dim=2)
            NMO_Subspace = size(Subspace, dim=2)
            allocate(Spq(NAO, NAO))
            allocate(Spb(NAO, NMO_Full))
            allocate(Sab(NMO_Subspace, NMO_Full))
            allocate(U(NMO_Subspace, NMO_Subspace))
            allocate(V(NMO_Full, NMO_Full))
            allocate(Sigma(min(NMO_Subspace, NMO_Full)))
            
            call ints1e_S(Spq, AOBasis)
            call real_ab(Spb, Spq, Cpa_Full)
            call real_aTb(Sab, Subspace, Spb)
            call real_SVD(U, V, Sigma, Sab)
            NMO_Projection = 0
            do k = 1, min(NMO_Subspace, NMO_Full)
                  if (Sigma(k) > Thresh) then
                        NMO_Projection = NMO_Projection + 1
                  else
                        exit
                  end if
            end do
            allocate(Cpa_Projection(NAO, NMO_Projection))
            call real_ab(Cpa_Projection, Cpa_Full, V(:, 1:NMO_Projection))
      end subroutine rpa_ProjectOrbitals
      

      subroutine rpa_VirtualNO(Cab, NVirtNO, Uaim, Am, NVecsT2, NOcc, &
            NVirt, NOCutoffThresh)
            !
            ! Compute RPA natural orbitals in the virtual subspace,
            ! defined as in Ref. 2, but computed using the coupled-cluster
            ! expression for the RPA 1-RDM of Ref. 1.
            !
            ! 1. D. Cieśliński, A. M. Tucholska, and M. Modrzejewski
            ! Post-Kohn-Sham Random-Phase Approximation and Correction Terms
            ! in the Expectation-Value Coupled-Cluster Formulation
            ! J. Chem. Theory Comput. 19, 6619 (2023); doi: 10.1021/acs.jctc.3c00496
            !
            ! 2. B. Ramberger, Z. Sukurma, T. Schäfer, and G. Kresse
            ! RPA natural orbitals and their application to post-Hartree-Fock
            ! electronic structure methods
            ! J. Chem. Phys. 151, 214106 (2019); doi: 10.1063/1.5128415
            !
            integer, intent(in)                                     :: NVecsT2
            integer, intent(in)                                     :: NOcc
            integer, intent(in)                                     :: NVirt
            real(F64), dimension(:, :), allocatable, intent(out)    :: Cab
            integer, intent(out)                                    :: NVirtNO
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in)  :: Uaim
            real(F64), dimension(NVecsT2), intent(in)               :: Am
            real(F64), intent(in)                                   :: NOCutoffThresh

            integer :: mu, a
            real(F64) :: Bm
            real(F64), dimension(:, :), allocatable :: RhoVV
            real(F64), dimension(:), allocatable :: OccNumbers
            real(F64) :: TraceError, ExactTrace, ApproxTrace
            logical, parameter :: OneRDM_Exchange = .false.
            logical, parameter :: CheckTraceError = .false.

            allocate(RhoVV(NVirt, NVirt))
            call rpa_RhoVV_RPAX(RhoVV, Uaim, Am, NOcc, NVirt, NVecsT2, OneRDM_Exchange)
            allocate(OccNumbers(NVirt))
            call symmetric_eigenproblem(OccNumbers, RhoVV, NVirt, .true.)
            if (CheckTraceError) then
                  ExactTrace = ZERO
                  do a = NVirt, 1, -1
                        if (OccNumbers(a) > ZERO) then
                              ExactTrace = ExactTrace + OccNumbers(a)
                        end if
                  end do
                  NVirtNO = 0
                  ApproxTrace = ZERO
                  do a = NVirt, 1, -1
                        if (OccNumbers(a) > ZERO) then
                              NVirtNO = NVirtNO + 1
                              ApproxTrace = ApproxTrace + OccNumbers(a)
                              TraceError = ExactTrace - ApproxTrace
                              if (TraceError < NOCutoffThresh * ExactTrace) then
                                    exit
                              end if
                        else
                              exit
                        end if
                  end do
            else
                  NVirtNO = 0
                  do a = NVirt, 1, -1
                        if (OccNumbers(a) >= NOCutoffThresh) then
                              NVirtNO = NVirt - a + 1
                        else
                              exit
                        end if
                  end do
            end if
            allocate(Cab(NVirt, NVirtNO))
            do a = 1, NVirtNO
                  Cab(:, a) = RhoVV(:, NVirt-a+1)
            end do
            call msg("Cutoff for occupation numbers: " // str(NOCutoffThresh, d=1))
            call msg("Full virtual space:       " // str(NVirt))
            call msg("Virtual natural orbitals: " // str(NVirtNO))
            call blankline()
      end subroutine rpa_VirtualNO


      subroutine rpa_RhoVV_RPAX(RhoVV, Uaim, Am, NOcc, NVirt, NVecsT2, XContrib)
            integer, intent(in)                                    :: NOcc, NVirt, NVecsT2
            real(F64), dimension(NVirt, NVirt), intent(out)        :: RhoVV
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in) :: Uaim
            real(F64), dimension(NVecsT2), intent(in)              :: Am
            logical, intent(in)                                    :: XContrib
            
            integer :: mu, a, b, i, j
            real(F64) :: Bm, Alpha
            real(F64), dimension(:, :), allocatable :: Tab

            RhoVV = ZERO            
            do mu = 1, NVecsT2
                  !                  
                  ! Rho(a,b) = Sum(i) Sum(bj) 4*S(ai,bj)*T(bj,bi)
                  ! = Sum(i)*Sum(mu) U(ai,mu) * 4*A(mu)**2/(1-4*A(mu)**2) * U(bi,mu)
                  ! Auxiliary amplitude: S = T/(1-4*T**2), see Eq. 59 in the SI for Ref. 1
                  !
                  Bm = FOUR * Am(mu)**2 / (ONE - FOUR * Am(mu)**2)
                  call real_abT_x(RhoVV, NVirt, Uaim(:, :, mu), NVirt, &
                        Uaim(:, :, mu), NVirt, NVirt, NVirt, NOcc, Bm, ONE)
            end do
            if (XContrib) then
                  allocate(Tab(NVirt, NVirt))
                  do j = 1, NOcc
                        do i = 1, NOcc
                              Tab = ZERO
                              do mu = 1, NVecsT2
                                    call real_vwT(Tab, Uaim(:, i, mu), Uaim(:, j, mu), Am(mu))
                              end do
                              !
                              ! Rho(a,b) = -2 * Sum(ij) Sum(c) T(ac;ij) * T(cb;ij)
                              !
                              Alpha = -TWO
                              call real_ab_x(RhoVV, NVirt, Tab, NVirt, Tab, NVirt, NVirt, NVirt, NVirt, Alpha, ONE)
                        end do
                  end do
            end if
      end subroutine rpa_RhoVV_RPAX


      subroutine rpa_OccupiedNO(Cij, Uaim, Am, NVecsT2, NOcc, NVirt)
            !
            ! Compute RPA natural orbitals in the occupied subspace,
            ! computed using the coupled-cluster
            ! expression for the RPA 1-RDM of Ref. 1.
            !
            ! 1. D. Cieśliński, A. M. Tucholska, and M. Modrzejewski
            ! Post-Kohn-Sham Random-Phase Approximation and Correction Terms
            ! in the Expectation-Value Coupled-Cluster Formulation
            ! J. Chem. Theory Comput. 19, 6619 (2023); doi: 10.1021/acs.jctc.3c00496
            !
            integer, intent(in)                                     :: NVecsT2
            integer, intent(in)                                     :: NOcc
            integer, intent(in)                                     :: NVirt
            real(F64), dimension(NOcc, NOcc), intent(out)           :: Cij
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in)  :: Uaim
            real(F64), dimension(NVecsT2), intent(in)               :: Am

            integer :: mu, i
            real(F64), dimension(:, :), allocatable :: RhoOO
            real(F64), dimension(:), allocatable :: OccNumbers
            logical, parameter :: OneRDM_Exchange = .false.

            allocate(RhoOO(NOcc, NOcc))
            allocate(OccNumbers(NOcc))
            call rpa_RhoOO_RPAX(RhoOO, Uaim, Am, NOcc, NVirt, NVecsT2, OneRDM_Exchange)
            call symmetric_eigenproblem(OccNumbers, RhoOO, NOcc, .true.)
            do i = 1, NOcc
                  Cij(:, i) = RhoOO(:, NOcc-i+1)
            end do
      end subroutine rpa_OccupiedNO

      
      subroutine rpa_RhoOO_RPAX(RhoOO, Uaim, Am, NOcc, NVirt, NVecsT2, XContrib)
            integer, intent(in)                                    :: NOcc, NVirt, NVecsT2
            real(F64), dimension(NOcc, NOcc), intent(out)          :: RhoOO
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in) :: Uaim
            real(F64), dimension(NVecsT2), intent(in)              :: Am
            logical, intent(in)                                    :: XContrib
            
            integer :: mu, a, b, i, j
            real(F64) :: Bm, Alpha
            real(F64), dimension(:, :), allocatable :: Tab

            RhoOO = ZERO
            do i = 1, NOcc
                  RhoOO(i, i) = TWO
            end do
            do mu = 1, NVecsT2
                  !                  
                  ! Rho(i,j) = Delta(i,j) + Sum(a) Sum(bk) (-4)*S(ai,bk)*T(bk,aj)
                  ! = Delta(i,j) + Sum(a)*Sum(mu) U(ai,mu) * (-4)*A(mu)**2/(1-4*A(mu)**2) * U(aj,mu)
                  ! Auxiliary amplitude: S = T/(1-4*T**2), see Eq. 59 in the SI for Ref. 1
                  !
                  Bm = (-FOUR) * Am(mu)**2 / (ONE - FOUR * Am(mu)**2)
                  call real_aTb_x(RhoOO, NOcc, Uaim(:, :, mu), NVirt, &
                        Uaim(:, :, mu), NVirt, NOcc, NOcc, NVirt, Bm, ONE)
            end do
            ! if (XContrib) then
            !       allocate(Tab(NVirt, NVirt))
            !       do j = 1, NOcc
            !             do i = 1, NOcc
            !                   Tab = ZERO
            !                   do mu = 1, NVecsT2
            !                         call real_vwT(Tab, Uaim(:, i, mu), Uaim(:, j, mu), Am(mu))
            !                   end do
            !                   !
            !                   ! Rho(a,b) = -2 * Sum(ij) Sum(c) T(ac;ij) * T(cb;ij)
            !                   !
            !                   Alpha = -TWO
            !                   call real_ab_x(RhoVV, NVirt, Tab, NVirt, Tab, NVirt, NVirt, NVirt, NVirt, Alpha, ONE)
            !             end do
            !       end do
            ! end if
      end subroutine rpa_RhoOO_RPAX
end module rpa_Orbitals
