module Slater
      use math_constants
      use arithmetic
      use scf_definitions
      use sys_definitions
      use real_linalg
      use basis_sets
      use KohnSham
      use Fock
      use clock

      implicit none

contains

      subroutine slater_OverlapInverse(Sinv_cao, S_cao, BasisVecs_cao)
            !
            ! Compute the inverse of the overlap matrix required for the computation of
            ! the Slater potential (Ref. 1). Carry out the inversion in the nonredundant
            ! basis and transform Sinv back to the full basis.
            !
            ! 1. Della Sala, F. and Gorling, A. J. Chem. Phys. 115, 5718 (2001);
            !    doi: 10.1063/1.1398093
            !
            real(F64), dimension(:, :), intent(out) :: Sinv_cao
            real(F64), dimension(:, :), intent(in)  :: S_cao
            real(F64), dimension(:, :), intent(in)  :: BasisVecs_cao

            real(F64), dimension(:, :), allocatable :: TransfWork
            real(F64), dimension(:, :), allocatable :: S_oao, Sinv_oao
            real(F64), dimension(:), allocatable :: Eig
            integer :: Noao, NAOCart
            integer :: k

            Noao = size(BasisVecs_cao, dim=2)
            NAOCart = size(BasisVecs_cao, dim=1)
            allocate(TransfWork(NAOCart, Noao))
            allocate(S_oao(Noao, Noao))
            allocate(Eig(Noao))
            call real_ab(TransfWork, S_cao, BasisVecs_cao)
            call real_aTb(S_oao, BasisVecs_cao, TransfWork)
            call symmetric_eigenproblem(Eig, S_oao, Noao, .true.)
            allocate(Sinv_oao(Noao, Noao))
            Sinv_oao = ZERO
            do k = 1, Noao
                  call real_vwT(Sinv_oao, S_oao(:, k), S_oao(:, k), ONE/Eig(k))
            end do
            call real_ab(TransfWork, BasisVecs_cao, Sinv_oao)
            call real_abT(Sinv_cao, TransfWork, BasisVecs_cao)
      end subroutine slater_OverlapInverse


      subroutine slater_F_RI(F_cao, Eel, ExcDFT, XCDef, Rho_cao, Hbare_cao, &
            S_cao, BasisVecs_cao, AOBasis, System, ThreshFockJK, GridDiag, &
            GridKind, GridPruning, time_F)
            !
            ! Build the Kohn-Sham matrix using the electron density Rho_cao
            ! and the Slater potential in place of the non-multiplicative
            ! orbital exchange operator. The Slater potential employs the resolution
            ! of the identity approximation of Della Sala and Gorling (Ref. 1).
            !
            ! 1. Della Sala, F. and Gorling, A. J. Chem. Phys. 115, 5718 (2001);
            !    doi: 10.1063/1.1398093
            !
            real(F64), dimension(:, :, :), intent(out) :: F_cao
            real(F64), intent(out)                     :: Eel
            real(F64), intent(out)                     :: ExcDFT
            type(TXCDef), intent(in)                   :: XCDef
            real(F64), dimension(:, :, :), intent(in)  :: Rho_cao
            real(F64), dimension(:, :), intent(in)     :: Hbare_cao
            real(F64), dimension(:, :), intent(in)     :: S_cao
            real(F64), dimension(:, :), intent(in)     :: BasisVecs_cao
            type(TAOBasis), intent(in)                 :: AOBasis
            type(TSystem), intent(in)                  :: System
            real(F64), intent(in)                      :: ThreshFockJK
            type(TGridDiag), intent(out)               :: GridDiag
            integer, intent(in)                        :: GridKind
            logical, intent(in)                        :: GridPruning
            real(F64), intent(inout)                   :: time_F

            integer :: NSpins
            real(F64), dimension(:, :, :), allocatable :: Vxc_cao, VxHF_cao, Vcoul_cao, RhoEff_cao
            real(F64), dimension(:, :), allocatable :: Work, Sinv_cao
            real(F64), dimension(:), allocatable :: BufferK, BufferJ
            real(F64), dimension(:), allocatable :: BufferRho1D
            real(F64), dimension(:, :, :), allocatable :: BufferTxc
            logical :: SpinUnres, CoulContrib, ExchContrib, AntisymRho, SHExchange, LCExchange
            real(F64) :: KFrac
            real(F64) :: Ecoul, ExHF
            integer :: DimTxc, DimJK, DimRho1D, NThreads
            integer :: NAOCart
            real(F64), dimension(1) :: AuxIntDummy
            real(F64), dimension(1, 1) :: AuxIntDummy2D
            integer :: s
            integer :: ThisImage, NImages
            real(F64) :: Omega
            type(TAOBasis) :: CartAOBasis
            type(TClock) :: Timer

            ThisImage = this_image()
            NImages = num_images()
            call clock_start(timer)
            NAOCart = AOBasis%NAOCart
            Omega = XCDef%rs_omega
            NSpins = size(F_cao, dim=3)
            SpinUnres = (NSpins > 1)
            !
            ! Define the Cartesian Gaussian basis set. The integration on the grid can only
            ! be done in the Cartesian AO basis so it's easier to compute the orbital exchange part
            ! in the same basis
            !
            CartAOBasis = AOBasis
            CartAOBasis%SpherAO = .false.
            ! --------------------------------------------------------------------------------
            ! Scratch space for computing the orbital exchange contrib and numerical integrals
            ! --------------------------------------------------------------------------------
            !$omp parallel default(shared)
            !$omp master
            NThreads = 1
            !$ NThreads = omp_get_num_threads()
            !$omp end master
            !$omp end parallel
            call fock_BufferDim(DimJK, DimRho1D, CartAOBasis)
            call ks_BufferDim(DimTxc, AOBasis)            
            allocate(BufferTxc(DimTxc, NSpins, NThreads))
            allocate(BufferK(DimJK))
            allocate(BufferJ(DimJK))
            allocate(BufferRho1D(DimRho1D))
            ! -------------------------------------------------------------
            !                     Coulomb matrix + HF exchange
            ! -------------------------------------------------------------
            allocate(VxHF_cao(NAOCart, NAOCart, NSpins))
            allocate(Vcoul_cao(NAOCart, NAOCart, NSpins))
            VxHF_cao = ZERO
            Vcoul_cao = ZERO
            SHExchange = xcf_get_flag(XCDef, XCF_SCREENED_HYBRID)
            LCExchange = xcf_get_flag(XCDef, XCF_RSHYB)
            if (LCExchange .or. SHExchange) then
                  KFrac = xcf_get_srexx(XCDef)
            else
                  KFrac = xcf_get_exx(XCDef)
            end if
            ExchContrib = (abs(KFrac) > ZERO .or. LCExchange .or. SHExchange)
            CoulContrib = .true.
            AntisymRho = .false.
            call fock_JK(Vcoul_cao, BufferK, BufferJ, BufferRho1D, Rho_cao, &
                  CartAOBasis, ExchContrib, KFrac, AntisymRho, LCExchange, SHExchange, &
                  Omega, CoulContrib, ThreshFockJK, VxHF_cao)
            Ecoul = ZERO
            ExHF = ZERO
            if (ThisImage == 1) then
                  Vcoul_cao = Vcoul_cao - VxHF_cao
                  do s = 1, NSpins
                        Ecoul = Ecoul + (ONE/TWO) * fock_RhoTrace(Rho_cao(:, :, s), Vcoul_cao(:, :, s))
                        ExHF = ExHF + (ONE/TWO) * fock_RhoTrace(Rho_cao(:, :, s), VxHF_cao(:, :, s))
                  end do
            end if
            if (NImages > 1) then
                  call co_broadcast(VxHF_cao, source_image=1)
            end if
            ! -------------------------------------------------------------
            ! Effective density matrix used to compute the Slater potential
            ! Eq. 23 in Ref. 1:
            ! RhoEff=1/2*Sinv*VxcGKS*Rho + 1/2*Rho*VxcGKS*Sinv
            ! -------------------------------------------------------------
            allocate(Sinv_cao(NAOCart, NAOCart))
            call slater_OverlapInverse(Sinv_cao, S_cao, BasisVecs_cao)
            allocate(Work(NAOCart, NAOCart))
            allocate(RhoEff_cao(NAOCart, NAOCart, NSpins))
            RhoEff_cao = ZERO
            do s = 1, NSpins
                  call smfill(VxHF_cao(:, :, s))
                  call real_ab(Work, VxHF_cao(:, :, s), Rho_cao(:, :, s))
                  call real_ab_x(RhoEff_cao(:, :, s), NAOCart, Sinv_cao, NAOCart, &
                        Work, NAOCart, NAOCart, NAOCart, NAOCart, ONE/TWO, ZERO)
                  call real_ab(Work, VxHF_cao(:, :, s), Sinv_cao)
                  call real_ab_x(RhoEff_cao(:, :, s), NAOCart, Rho_cao(:, :, s), NAOCart, &
                        Work, NAOCart, NAOCart, NAOCart, NAOCart, ONE/TWO, ONE)
            end do
            ! -------------------------------------------------------------
            ! Semilocal part of the GKS exchange-correlation matrix +
            ! the Slater potential
            ! -------------------------------------------------------------
            allocate(Vxc_cao(NAOCart, NAOCart, NSpins))
            call ks_Vxc(Vxc_cao, ExcDFT, GridDiag, AuxIntDummy, BufferTxc, &
                  Rho_cao, RhoEff_cao, XCDef, AuxIntDummy2D, CartAOBasis, System, &
                  GridKind, GridPruning)
            if (ThisImage == 1) then
                  !
                  ! Total electronic energy, excluding nuclear repulsion
                  !
                  Eel = Ecoul + ExcDFT
                  do s = 1, NSpins
                        Eel = Eel + fock_RhoTrace(Rho_cao(:, :, s), Hbare_cao)
                        F_cao(:, :, s) = Hbare_cao + Vcoul_cao(:, :, s) + Vxc_cao(:, :, s)
                  end do
            end if
            if (NImages > 1) then
                  call co_broadcast(Eel, source_image=1)
            end if
            time_F = time_F + clock_readwall(timer)
      end subroutine slater_F_RI
end module Slater
