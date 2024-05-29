module rpa_MeanField
      use arithmetic
      use string
      use PostSCF
      use real_linalg
      use basis_sets
      use quadratures
      use matexp
      use scf_definitions
      use sys_definitions
      use rpa_definitions
      use thc_definitions

      implicit none

contains

      subroutine rpa_MeanField_Preamble(RPAParams)
            type(TRPAParams), intent(in) :: RPAParams

            call blankline()
            call msg("Mean-field (Hartree-Fock) calculation on Kohn-Sham occupied space", underline=.true.)
            !
            call msg("Hartree-Fock (EXX) energy")
            call msg(lfield("", 30) // "Kohn-Sham orbitals, exact integrals")
            !
            call msg("1-RDM linear")
            call msg(lfield("", 30) // "mean-field 1-RDM")
            call msg(lfield("", 30) // "AC integration with " // str(RPAParams%ACQuadPoints) // " quadrature points")
            !
            call msg("1-RDM quadratic")
            call msg(lfield("", 30) // "mean-field 1-RDM")
            call msg(lfield("", 30) // "THC decomposition of Coulomb integrals")
            if (RPAParams%AC_1RDMQuad) then
                  call msg(lfield("", 30) // "AC integration with " // str(RPAParams%ACQuadPoints) // " quadrature points")
            else
                  call msg(lfield("", 30) // "Lambda=1 evaluation")
            end if
      end subroutine rpa_MeanField_Preamble

      
      subroutine rpa_MeanField_Semi(MeanField, SCFOutput, SCFParams, RPAParams, &
            AOBasis, System, THCGrid)
            !
            ! Compute the eigenvectors of the occupied-occupied and virtual-virtual
            ! blocks of the GMBPT mean-field hamiltonian of Bartlett et al.
            !
            ! F(Lambda) = hHF(OO) + hHF(VV) + Lambda*(hHF(OV)+hHF(VO))
            !
            ! where hHF is built from the Kohn-Sham canonical orbitals.
            !
            ! 1. R.J. Bartlett, I. Grabowski, S. Hirata, and S. Ivanov
            !    The exchange-correlation potential in ab initio density functional theory
            !    J. Chem. Phys. 122, 034104 (2005); doi: 10.1063/1.1809605
            !
            type(TMeanField), intent(out)  :: MeanField
            type(TSCFOutput), intent(in)   :: SCFOutput
            type(TSCFParams), intent(in)   :: SCFParams
            type(TRPAParams), intent(in)   :: RPAParams
            type(TAOBasis), intent(in)     :: AOBasis
            type(TSystem), intent(in)      :: System
            type(TCoulTHCGrid), intent(in) :: THCGrid

            integer :: NAO, MaxNVirt, MaxNOcc, NSpins, MaxNai, NMO
            integer :: i0, i1, a0, a1, s
            real(F64), dimension(:, :, :), allocatable :: Rho_ao, OccCoeffs_ao, VirtCoeffs_ao
            real(F64), dimension(:, :), allocatable :: OccEnergies, VirtEnergies
            real(F64) :: EtotHF, EHFTwoEl, EHbare, Enucl
            real(F64) :: time_F
            real(F64), dimension(:), allocatable :: Ec1RDM_Linear
            
            MeanField%NOcc = SCFOutput%NOcc
            MeanField%NVirt = SCFOutput%NVirt
            NSpins = size(SCFOutput%C_oao, dim=3)
            allocate(Ec1RDM_Linear(NSpins))
            MeanField%NSpins = NSpins
            if (AOBasis%SpherAO) then
                  NAO = AOBasis%NAOSpher
                  call postscf_Rho(OccCoeffs_ao, VirtCoeffs_ao, Rho_ao, &
                        SCFOutput%C_oao, &
                        SCFOutput%MOBasisVecsSpher, &
                        SCFOutput%NOcc, &
                        SCFOutput%NVirt)
            else
                  NAO = AOBasis%NAOCart
                  call postscf_Rho(OccCoeffs_ao, VirtCoeffs_ao, Rho_ao, &
                        SCFOutput%C_oao, &
                        SCFOutput%MOBasisVecsCart, &
                        SCFOutput%NOcc, &
                        SCFOutput%NVirt)
            end if
            allocate(MeanField%F_ao(NAO, NAO, NSpins))
            call postscf_FullFockMatrix(MeanField%F_ao, EtotHF, EHFTwoEl, EHbare, Enucl, Rho_ao, &
                  SCFParams, System, AOBasis, time_F)
            MeanField%EtotHF = EtotHF
            MaxNVirt = maxval(SCFOutput%NVirt)
            MaxNOcc = maxval(SCFOutput%NOcc)
            MaxNai = max(SCFOutput%NOcc(1)*SCFOutput%NVirt(1), SCFOutput%NOcc(2)*SCFOutput%NVirt(2))
            NMO = SCFOutput%NOcc(1) + SCFOutput%NVirt(1)
            allocate(MeanField%OccCoeffs_ao(NAO, MaxNOcc, NSpins))
            allocate(MeanField%VirtCoeffs_ao(NAO, MaxNVirt, NSpins))
            allocate(MeanField%OrbEnergies(NMO, NSpins))
            allocate(OccEnergies(MaxNOcc, NSpins))
            allocate(VirtEnergies(MaxNVirt, NSpins))
            call rpa_semi_Diagonalize( &
                  MeanField%OccCoeffs_ao, & ! Semicanonical occupied orbitals
                  MeanField%VirtCoeffs_ao, & ! Semicanonical virtual orbitals
                  OccEnergies, & ! Occupied block HF hamiltonian eigenvalues
                  VirtEnergies, & ! Virtual block HF hamiltonian eigenvalues
                  MeanField%F_ao, &
                  OccCoeffs_ao, & ! Canonical KS occupied orbitals
                  VirtCoeffs_ao, & ! Canonical KS virtual orbitals
                  SCFOutput%Nocc, &
                  SCFOutput%NVirt)
            MeanField%OrbEnergies = ZERO
            do s = 1, NSpins
                  i0 = 1
                  i1 = SCFOutput%NOcc(s)
                  a0 = SCFOutput%NOcc(s) + 1
                  a1 = SCFOutput%NOcc(s) + SCFOutput%NVirt(s)
                  if (SCFOutput%NOcc(s) > 0) then
                        MeanField%OrbEnergies(i0:i1, s) = OccEnergies(1:SCFOutput%NOcc(s), s)
                        MeanField%OrbEnergies(a0:a1, s) = VirtEnergies(1:SCFOutput%NVirt(s), s)
                  end if
            end do
            !
            ! Correlation energy corresponding to the mean-field change
            ! from the reference hamiltonian to the GMBPT hamiltonian (Ec1RDM).
            ! Computed for the full-electron system, i.e, without any frozen core.
            !
            call rpa_MeanField_Ec1RDM( &
                  MeanField%Ec1RDM_Linear, &
                  MeanField%Ec1RDM_Quadratic, &
                  MeanField%F_ao, &
                  MeanField%OccCoeffs_ao, &
                  MeanField%VirtCoeffs_ao, &
                  OccEnergies, &
                  VirtEnergies, &
                  THCGrid%Zgk, &
                  THCGrid%Xgp, &
                  MeanField%NOcc, &
                  MeanField%NVirt, &
                  RPAParams%ACQuadPoints, &
                  RPAParams%AC_1RDMQuad)
      end subroutine rpa_MeanField_Semi


      subroutine rpa_MeanField_RefineHF(MeanField, SCFOutput, SCFParams, &
            AOBasis, System, THCGrid)

            type(TMeanField), intent(out)  :: MeanField
            type(TSCFOutput), intent(in)   :: SCFOutput
            type(TSCFParams), intent(in)   :: SCFParams
            type(TAOBasis), intent(in)     :: AOBasis
            type(TSystem), intent(in)      :: System
            type(TCoulTHCGrid), intent(in) :: THCGrid

            integer :: NAO, NSpins, NMO, NGridTHC
            integer :: i0, i1, a0, a1, s
            real(F64), dimension(:, :, :), allocatable :: Rho_ao, C_ao, DeltaRho_ao
            real(F64), dimension(:, :), allocatable :: RhoMF_ao
            real(F64), dimension(:, :), allocatable :: Fpl, Fkl
            integer, dimension(2) :: NOcc, NVirt
            real(F64) :: EtotHF, EHFTwoEl, EHbare, Enucl
            real(F64) :: time_F

            NGridTHC = THCGrid%NGrid
            NOcc = SCFOutput%NOcc
            NVirt = SCFOutput%NVirt
            MeanField%NOcc = NOcc
            MeanField%NVirt = NVirt
            NSpins = size(SCFOutput%C_oao, dim=3)
            MeanField%NSpins = NSpins
            NAO = AOBasis%NAOSpher
            NMO = size(SCFOutput%MOBasisVecsSpher, dim=2)
            allocate(Rho_ao(NAO, NAO, NSpins))
            allocate(C_ao(NAO, NMO, NSpins))
            do s = 1, NSpins
                  if (NOcc(s) > 0) then
                        i0 = 1
                        i1 = SCFOutput%NOcc(s)
                        call real_ab(C_ao(:, :, s), SCFOutput%MOBasisVecsSpher, &
                              SCFOutput%C_oao(:, 1:NMO, s))
                        call real_abT(Rho_ao(:, :, s), C_ao(:, i0:i1, s), C_ao(:, i0:i1, s))
                        if (NSpins==1) then
                              Rho_ao(:, :, s) = TWO * Rho_ao(:, :, s)
                        end if
                  else
                        Rho_ao(:, :, s) = ZERO
                  end if
            end do
            allocate(MeanField%F_ao(NAO, NAO, NSpins))
            call postscf_FullFockMatrix(MeanField%F_ao, EtotHF, EHFTwoEl, EHbare, Enucl, Rho_ao, &
                  SCFParams, System, AOBasis, time_F)
            MeanField%EtotHF = EtotHF
            allocate(Fpl(NAO, NMO))
            allocate(Fkl(NMO, NMO))
            allocate(MeanField%OrbEnergies(NMO, NSpins))
            allocate(MeanField%OccCoeffs_ao(NAO, maxval(NOcc), NSpins))
            allocate(MeanField%VirtCoeffs_ao(NAO, maxval(NVirt), NSpins))
            do s = 1, NSpins
                  if (NOcc(s) > 0) then
                        i0 = 1
                        i1 = NOcc(s)
                        a0 = NOcc(s) + 1
                        a1 = NOcc(s) + NVirt(s)
                        call real_ab(Fpl, MeanField%F_ao(:, :, s), C_ao(:, :, s))
                        call real_aTb(Fkl, C_ao(:, :, s), Fpl)
                        call symmetric_eigenproblem(MeanField%OrbEnergies(:, s), Fkl, NMO, .true.)
                        call real_ab(MeanField%OccCoeffs_ao(:, 1:NOcc(s), s), C_ao(:, :, s), Fkl(:, i0:i1))
                        call real_ab(MeanField%VirtCoeffs_ao(:, 1:NVirt(s), s), C_ao(:, :, s), Fkl(:, a0:a1))
                  else
                        MeanField%OccCoeffs_ao(:, :, s) = ZERO
                        MeanField%VirtCoeffs_ao(:, :, s) = ZERO
                        MeanField%OrbEnergies(:, s) = ZERO
                  end if
            end do
            allocate(RhoMF_ao(NAO, NAO))
            allocate(DeltaRho_ao(NAO, NAO, NSpins))
            do s = 1, NSpins
                  if (NOcc(s) > 0) then
                        call real_abT(RhoMF_ao, MeanField%OccCoeffs_ao(:, 1:NOcc(s), s), &
                              MeanField%OccCoeffs_ao(:, 1:NOcc(s), s))
                        DeltaRho_ao(:, :, s) = RhoMF_ao - (ONE/TWO) * Rho_ao(:, :, s)
                  else
                        DeltaRho_ao(:, :, s) = ZERO
                  end if
            end do
            call rpa_MeanField_DeltaEtotHF_THC( &
                  MeanField%Ec1RDM_Linear, &
                  MeanField%Ec1RDM_Quadratic, &
                  DeltaRho_ao, MeanField%F_ao, NOcc, THCGrid)
      end subroutine rpa_MeanField_RefineHF


      subroutine rpa_MeanField_DeltaEtotHF_THC(Ec1RDM_Linear, Ec1RDM_Quadratic, &
            DeltaRho_ao, F_ao, NOcc, THCGrid)

            real(F64), intent(out)                    :: Ec1RDM_Linear
            real(F64), intent(out)                    :: Ec1RDM_Quadratic
            real(F64), dimension(:, :, :), intent(in) :: DeltaRho_ao
            real(F64), dimension(:, :, :), intent(in) :: F_ao
            integer, dimension(2), intent(in)         :: NOcc
            type(TCoulTHCGrid), intent(in)            :: THCGrid

            real(F64), dimension(:, :), allocatable :: X_Rho_X, Zgh, X_Rho
            real(F64), dimension(:), allocatable :: D
            real(F64), dimension(2) :: Vexch
            real(F64) :: Vcoul
            real(F64) :: TrRhoF
            integer :: NAO, NGridTHC, NCholesky, NSpins
            integer :: s, g

            NGridTHC = THCGrid%NGrid
            NCholesky = size(THCGrid%Zgk, dim=2)
            NAO = size(DeltaRho_ao, dim=1)
            NSpins = size(DeltaRho_ao, dim=3)
            allocate(X_Rho_X(NGridTHC, NGridTHC))
            allocate(Zgh(NGridTHC, NGridTHC))
            allocate(X_Rho(NGridTHC, NAO))
            allocate(D(NGridTHC))
            call real_abT(Zgh, THCGrid%Zgk, THCGrid%Zgk)
            D = ZERO
            Ec1RDM_Linear = ZERO
            Ec1RDM_Quadratic = ZERO                 
            Vexch = ZERO
            Vcoul = ZERO                  
            do s = 1, NSpins
                  if (NOcc(s) > 0) then
                        call real_vw_x(TrRhoF, DeltaRho_ao(:, :, s), F_ao, NAO**2)
                        if (NSpins == 1) then
                              Ec1RDM_Linear = TWO * TrRhoF
                        else
                              Ec1RDM_Linear = Ec1RDM_Linear + TrRhoF
                        end if
                        call rpa_THC_Ec1RDM_Vexch(Vexch(s), X_Rho_X, X_Rho, Zgh, THCGrid%Xgp, &
                              DeltaRho_ao, NAO, NGridTHC)            
                        do g = 1, NGridTHC
                              D(g) = D(g) + X_Rho_X(g, g)
                        end do
                  end if
            end do
            call rpa_THC_Ec1RDM_Vcoul(Vcoul, D, THCGrid%Zgk, NGridTHC, NCholesky)
            if (NSpins == 1) then
                  !
                  ! Closed shell case
                  !
                  Vcoul = FOUR * Vcoul
                  Vexch(2) = Vexch(1)
            end if
            Ec1RDM_Quadratic = Ec1RDM_Quadratic + (ONE/TWO) * (Vcoul - Vexch(1) - Vexch(2))
      end subroutine rpa_MeanField_DeltaEtotHF_THC


      subroutine rpa_semi_Diagonalize(SemiOccCoeffs_ao, SemiVirtCoeffs_ao, &
            Fii, Faa, hHF_ao, OccCoeffs_ao, VirtCoeffs_ao, Nocc, NVirt)
            !
            ! Compute the eigenvectors of the occupied-occupied and virtual-virtual
            ! blocks of the GMBPT mean-field hamiltonian of Bartlett et al.
            !
            ! F(Lambda) = hHF(OO) + hHF(VV) + Lambda*(hHF(OV)+hHF(VO))
            !
            ! where hHF is built from the Kohn-Sham canonical orbitals.
            !
            ! 1. R.J. Bartlett, I. Grabowski, S. Hirata, and S. Ivanov
            !    The exchange-correlation potential in ab initio density functional theory
            !    J. Chem. Phys. 122, 034104 (2005); doi: 10.1063/1.1809605
            !
            real(F64), dimension(:, :, :), intent(out)      :: SemiOccCoeffs_ao
            real(F64), dimension(:, :, :), intent(out)      :: SemiVirtCoeffs_ao
            real(F64), dimension(:, :), intent(out)         :: Fii
            real(F64), dimension(:, :), intent(out)         :: Faa
            real(F64), dimension(:, :, :), intent(in)       :: hHF_ao
            real(F64), dimension(:, :, :), intent(in)       :: OccCoeffs_ao
            real(F64), dimension(:, :, :), intent(in)       :: VirtCoeffs_ao
            integer, dimension(2), intent(in)               :: NOcc
            integer, dimension(2), intent(in)               :: NVirt

            integer :: NSpins, NAO, s

            NAO = size(hHF_ao, dim=1)
            NSpins = size(hHF_ao, dim=3)
            Fii = ZERO
            Faa = ZERO
            SemiOccCoeffs_ao = ZERO
            SemiVirtCoeffs_ao = ZERO
            do s = 1, NSpins
                  if (NOcc(s) > 0) then
                        call rpa_semi_Block(Fii(:, s), SemiOccCoeffs_ao(:, :, s), hHF_ao(:, :, s), &
                              OccCoeffs_ao(:, :, s), NOcc(s), NAO)
                        call rpa_semi_Block(Faa(:, s), SemiVirtCoeffs_ao(:, :, s), hHF_ao(:, :, s), &
                              VirtCoeffs_ao(:, :, s), NVirt(s), NAO)
                  end if
            end do
            
      contains
            
            subroutine rpa_semi_Block(Eig, C_semi_ao, F_ao, C_ao, NMO, NAO)
                  real(F64), dimension(NMO), intent(out)       :: Eig
                  real(F64), dimension(NAO, NMO), intent(out)  :: C_semi_ao
                  real(F64), dimension(NAO, NAO), intent(in)   :: F_ao
                  real(F64), dimension(NAO, NMO), intent(in)   :: C_ao
                  integer, intent(in)                          :: NMO
                  integer, intent(in)                          :: NAO

                  real(F64), dimension(:, :), allocatable :: T
                  real(F64), dimension(:, :), allocatable :: F_mo

                  allocate(T(NAO, NMO))
                  allocate(F_mo(NMO, NMO))
                  call real_ab(T, F_ao, C_ao)
                  call real_aTb(F_mo, C_ao, T)
                  call symmetric_eigenproblem(Eig, F_mo, NMO, .true.)
                  call real_ab(C_semi_ao, C_ao, F_mo)
            end subroutine rpa_semi_Block
      end subroutine rpa_semi_Diagonalize


      subroutine rpa_MeanField_Ec1RDM(Ec1RDM_Linear, Ec1RDM_Quadratic, hHF_ao, &
            SemiOccCoeffs_ao, SemiVirtCoeffs_ao, Fii, Faa, Zgk, Xgp, NOcc, NVirt, &
            NACPoints, AC_1RDMQuad)

            real(F64), intent(out)                     :: Ec1RDM_Linear
            real(F64), intent(out)                     :: Ec1RDM_Quadratic
            real(F64), dimension(:, :, :), intent(in)  :: hHF_ao
            real(F64), dimension(:, :, :), intent(in)  :: SemiOccCoeffs_ao
            real(F64), dimension(:, :, :), intent(in)  :: SemiVirtCoeffs_ao
            real(F64), dimension(:, :), intent(in)     :: Fii
            real(F64), dimension(:, :), intent(in)     :: Faa
            real(F64), dimension(:, :), intent(in)     :: Zgk
            real(F64), dimension(:, :), intent(in)     :: Xgp
            integer, dimension(2), intent(in)          :: NOcc
            integer, dimension(2), intent(in)          :: NVirt
            integer, intent(in)                        :: NACPoints
            logical, intent(in)                        :: AC_1RDMQuad
            
            integer :: NSpins, NAO, NMO, NGridTHC, NCholesky, MaxNai
            real(F64), dimension(:, :), allocatable :: DeltaRho_mo, DeltaRho_ao
            real(F64), dimension(:, :), allocatable :: X_Rho_X
            real(F64), dimension(:, :), allocatable :: Zgh
            real(F64), dimension(:, :), allocatable :: X_Rho
            real(F64), dimension(:, :), allocatable :: hHFai
            real(F64), dimension(:), allocatable :: D
            real(F64), dimension(:), allocatable :: ACPoints, ACWeights
            real(F64), dimension(2) :: Vexch
            real(F64) :: Lambda, Weight, TrFRho
            real(F64) :: Vcoul
            integer :: g, s, k
            logical :: Linear, Quadratic
            
            NSpins = size(hHF_ao, dim=3)
            NAO = size(hHF_ao, dim=1)
            NMO = NOcc(1) + NVirt(1)
            NGridTHC = size(Zgk, dim=1)
            NCholesky = size(Zgk, dim=2)
            MaxNai = max(NOcc(1)*NVirt(1), NOcc(2)*NVirt(2))
            allocate(hHFai(MaxNai, NSpins))
            allocate(DeltaRho_ao(NAO, NAO))
            allocate(DeltaRho_mo(NMO, NMO))
            allocate(X_Rho_X(NGridTHC, NGridTHC))
            allocate(Zgh(NGridTHC, NGridTHC))
            allocate(X_Rho(NGridTHC, NAO))
            allocate(D(NGridTHC))
            allocate(ACPoints(NACPoints))
            allocate(ACWeights(NACPoints))
            call quad_AdiabaticConnection(ACPoints, ACWeights, NACPoints)            
            call real_abT(Zgh, Zgk, Zgk)            
            do s = 1, NSpins
                  call rpa_MeanField_hHFai(hHFai(:, s), hHF_ao(:, :, s), SemiOccCoeffs_ao(:, :, s), &
                        SemiVirtCoeffs_ao(:, :, s), NOcc(s), NVirt(s), NAO)
            end do
            Ec1RDM_Linear = ZERO
            Ec1RDM_Quadratic = ZERO
            do k = 0, NACPoints
                  if (k > 0) then
                        Linear = .true.
                        Quadratic = AC_1RDMQuad
                        Lambda = ACPoints(k)
                        Weight = ACWeights(k)
                  else
                        Linear = .false.
                        Quadratic = (.not. AC_1RDMQuad)
                        Lambda = ONE
                        Weight = ONE
                  end if
                  if (Linear .or. Quadratic) then
                        D = ZERO
                        do s = 1, NSpins
                              call rpa_MeanField_DeltaRho_EVD(DeltaRho_ao, DeltaRho_mo, Lambda, hHFai(:, s), &
                                    Fii(:, s), Faa(:, s), SemiOccCoeffs_ao(:, :, s), SemiVirtCoeffs_ao(:, :, s), &
                                    NOcc(s), NVirt(s), NMO, NAO)
                              if (Linear) then
                                    call rpa_MeanField_Ec1RDM_Linear(TrFRho, hHFai(:, s), DeltaRho_mo, NOcc(s), NVirt(s), NMO)
                                    if (NSpins == 1) then
                                          Ec1RDM_Linear = Ec1RDM_Linear + Weight * TWO * TrFRho
                                    else
                                          Ec1RDM_Linear = Ec1RDM_Linear + Weight * TrFRho
                                    end if
                              end if
                              if (Quadratic) then
                                    call rpa_THC_Ec1RDM_Vexch(Vexch(s), X_Rho_X, X_Rho, Zgh, Xgp, &
                                          DeltaRho_ao, NAO, NGridTHC)            
                                    do g = 1, NGridTHC
                                          D(g) = D(g) + X_Rho_X(g, g)
                                    end do
                              end if
                        end do
                        if (Quadratic) then
                              call rpa_THC_Ec1RDM_Vcoul(Vcoul, D, Zgk, NGridTHC, NCholesky)
                              if (NSpins == 1) then
                                    !
                                    ! Closed shell case
                                    !
                                    Vcoul = FOUR * Vcoul
                                    Vexch(2) = Vexch(1)
                              end if
                              Ec1RDM_Quadratic = Ec1RDM_Quadratic + Weight * (ONE/TWO) * (Vcoul - Vexch(1) - Vexch(2))
                        end if
                  end if
            end do
      end subroutine rpa_MeanField_Ec1RDM


      subroutine rpa_MeanField_hHFai(hHFai, hHF_ao, SemiOccCoeffs_ao, SemiVirtCoeffs_ao, &
            NOcc, NVirt, NAO)
            
            real(F64), dimension(NVirt, NOcc), intent(out) :: hHFai
            real(F64), dimension(NAO, NAO), intent(in)     :: hHF_ao
            real(F64), dimension(NAO, NOcc), intent(in)    :: SemiOccCoeffs_ao
            real(F64), dimension(NAO, NVirt), intent(in)   :: SemiVirtCoeffs_ao
            integer, intent(in)                            :: NOcc
            integer, intent(in)                            :: NVirt
            integer, intent(in)                            :: NAO

            real(F64), dimension(:, :), allocatable :: W

            allocate(W(NAO, NOcc))
            call real_ab(W, hHF_ao, SemiOccCoeffs_ao)
            call real_aTb(hHFai, SemiVirtCoeffs_ao, W)
      end subroutine rpa_MeanField_hHFai


      subroutine rpa_MeanField_T1(Tai, hHFai, OccEnergies, VirtEnergies, NOcc, NVirt)
            real(F64), dimension(NVirt, NOcc), intent(out) :: Tai
            real(F64), dimension(NVirt, NOcc), intent(in)  :: hHFai
            real(F64), dimension(NOcc), intent(in)         :: OccEnergies
            real(F64), dimension(NVirt), intent(in)        :: VirtEnergies
            integer, intent(in)                            :: NOcc
            integer, intent(in)                            :: NVirt

            integer :: a, i

            do i = 1, NOcc
                  do a = 1, NVirt
                       Tai(a, i) = -hHFai(a, i) / (VirtEnergies(a) - OccEnergies(i))
                 end do
           end do
      end subroutine rpa_MeanField_T1

      
      subroutine rpa_MeanField_Ec1RDM_Linear(TrFRho, hHFai, DeltaRho_mo, NOcc, NVirt, NMO)
            !
            ! Compute the integrand of the linear 1-RDM correlation term
            !
            ! Sum(pq) (h(pq;Lambda=1)-h(pq;Lambda=0)) * (Rho(pq;Lambda) - Rho(pq;Lambda=0))
            ! = 2 * Sum(ai) hHF(pq) * (Rho(ai;Lambda)-Rho(ai;Lambda=0)) (semicanonical basis)
            !
            ! The resulting value should be integrated over Lambda to get Ec1RDM_Linear.
            !
            real(F64), intent(out)                        :: TrFRho
            real(F64), dimension(NVirt, NOcc), intent(in) :: hHFai
            real(F64), dimension(NMO, NMO), intent(in)    :: DeltaRho_mo
            integer, intent(in)                           :: NOcc
            integer, intent(in)                           :: NVirt
            integer, intent(in)                           :: NMO

            integer :: a, i

            TrFRho = ZERO
            do i = 1, NOcc
                  do a = 1, NVirt
                        TrFRho = TrFRho + hHFai(a, i) * DeltaRho_mo(NOcc+a, i)
                  end do
            end do
            !
            ! Take into account that there are two symmetric blocks: OV+VO
            !
            TrFRho = TWO * TrFRho
      end subroutine rpa_MeanField_Ec1RDM_Linear
      

      subroutine rpa_MeanField_DeltaRho_EVD(DeltaRho_ao, DeltaRho_mo, Lambda, hHFai, Fii, Faa, &
            SemiOccCoeffs_ao, SemiVirtCoeffs_ao, NOcc, NVirt, NMO, NAO)

            real(F64), dimension(NAO, NAO), intent(out)               :: DeltaRho_ao
            real(F64), dimension(NMO, NMO), intent(out)               :: DeltaRho_mo
            real(F64), intent(in)                                     :: Lambda
            real(F64), dimension(NVirt, NOcc), intent(in)             :: hHFai
            real(F64), dimension(NOcc), intent(in)                    :: Fii
            real(F64), dimension(NVirt), intent(in)                   :: Faa
            real(F64), dimension(NAO, NOcc), intent(in)               :: SemiOccCoeffs_ao
            real(F64), dimension(NAO, NVirt), intent(in)              :: SemiVirtCoeffs_ao
            integer, intent(in)                                       :: NOcc
            integer, intent(in)                                       :: NVirt
            integer, intent(in)                                       :: NMO
            integer, intent(in)                                       :: NAO

            integer :: a0, a1, i0, i1, a, i
            real(F64), dimension(:, :), allocatable :: F_mo
            real(F64), dimension(:, :), allocatable :: C, W
            real(F64), dimension(:), allocatable :: Eig

            allocate(F_mo(NMO, NMO))
            allocate(Eig(NMO))
            F_mo = ZERO
            do i = 1, NOcc
                  F_mo(i, i) = Fii(i)
            end do
            do a = 1, NVirt
                  F_mo(NOcc+a, NOcc+a) = Faa(a)
            end do
            i0 = 1
            i1 = NOcc
            a0 = NOcc + 1
            a1 = NOcc + NVirt
            F_mo(a0:a1, i0:i1) = Lambda * hHFai
            call symmetric_eigenproblem(Eig, F_mo, NMO, .true.)
            call real_abT(DeltaRho_mo, F_mo(:, 1:NOcc), F_mo(:, 1:NOcc))
            do i = 1, NOcc
                  DeltaRho_mo(i, i) = DeltaRho_mo(i, i) - ONE
            end do
            allocate(C(NAO, NMO))
            C(:, i0:i1) = SemiOccCoeffs_ao
            C(:, a0:a1) = SemiVirtCoeffs_ao            
            allocate(W(NMO, NAO))
            call real_abT(W, DeltaRho_mo, C)
            call real_ab(DeltaRho_ao, C, W)
      end subroutine rpa_MeanField_DeltaRho_EVD

      
      subroutine rpa_THC_Ec1RDM_Vexch(Vexch, X_Rho_X, X_Rho, Zgh, Xgp, &
            Rho, NAO, NGridTHC)
            
            real(F64), intent(out)                                :: Vexch
            real(F64), dimension(NGridTHC, NGridTHC), intent(out) :: X_Rho_X
            real(F64), dimension(NGridTHC, NAO), intent(out)      :: X_Rho            
            real(F64), dimension(NGridTHC, NGridTHC), intent(in)  :: Zgh
            real(F64), dimension(NGridTHC, NAO), intent(in)       :: Xgp
            real(F64), dimension(NAO, NAO), intent(in)            :: Rho
            integer, intent(in)                                   :: NAO
            integer, intent(in)                                   :: NGridTHC
            
            integer :: g, h
            
            call real_ab(X_Rho, Xgp, Rho)
            call real_abT(X_Rho_X, X_Rho, Xgp)
            Vexch = ZERO            
            !$omp parallel do &
            !$omp private(g, h) &
            !$omp reduction(+:Vexch)
            do h = 1, NGridTHC
                  do g = 1, NGridTHC
                        Vexch = Vexch + Zgh(g, h) * X_Rho_X(g, h)**2
                  end do
            end do
            !$omp end parallel do            
      end subroutine rpa_THC_Ec1RDM_Vexch


      subroutine rpa_THC_Ec1RDM_Vcoul(Vcoul, D, Zgk, NGridTHC, NCholesky)
            real(F64), intent(out)                                :: Vcoul
            real(F64), dimension(NGridTHC), intent(in)            :: D
            real(F64), dimension(NGridTHC, NCholesky), intent(in) :: Zgk
            integer, intent(in)                                   :: NGridTHC
            integer, intent(in)                                   :: NCholesky

            real(F64), dimension(:), allocatable :: ZD
            !
            ! ZD(1:NCholesky) <- Z(1:NGridTHC, 1:NCholesky)**T X_Rho_X(1:NGridTHC)
            !
            allocate(ZD(NCholesky))
            call real_aTv_x(ZD, Zgk, NGridTHC, D, NGridTHC, NCholesky, ONE, ZERO)
            Vcoul = dot_product(ZD, ZD)
      end subroutine rpa_THC_Ec1RDM_Vcoul


      subroutine rpa_ChangeOrbitalSpace(MeanField, OrbitalSubspace)
            type(TMeanField), intent(inout)        :: MeanField
            real(F64), dimension(:, :), intent(in) :: OrbitalSubspace

            integer :: SubspaceDim, NAO
            integer :: s
            integer :: i0, i1, a0, a1
            real(F64), dimension(:, :), allocatable :: OrbCoeffs
            
            SubspaceDim = size(OrbitalSubspace, dim=2)
            NAO = size(OrbitalSubspace, dim=1)
            do s = 1, MeanField%NSpins
                  MeanField%NVirt(s) = SubspaceDim - MeanField%NOcc(s)
            end do
            deallocate(MeanField%VirtCoeffs_ao)
            deallocate(MeanField%OrbEnergies)
            allocate(MeanField%VirtCoeffs_ao(NAO, maxval(MeanField%NVirt), MeanField%NSpins))
            allocate(MeanField%OrbEnergies(SubspaceDim, MeanField%NSpins))
            allocate(OrbCoeffs(NAO, SubspaceDim))
            do s = 1, MeanField%NSpins
                  i0 = 1
                  i1 = MeanField%NOcc(s)
                  a0 = MeanField%NOcc(s) + 1
                  a1 = MeanField%NOcc(s) + MeanField%NVirt(s)
                  call rpa_DiagonalizeFockHamiltonian(MeanField%OrbEnergies(:, s), &
                        OrbCoeffs, OrbitalSubspace, MeanField%F_ao(:, :, s))
                  MeanField%OccCoeffs_ao(:, 1:MeanField%NOcc(s), s) = OrbCoeffs(:, i0:i1)
                  MeanField%VirtCoeffs_ao(:, 1:MeanField%NVirt(s), s) = OrbCoeffs(:, a0:a1)
            end do
      end subroutine rpa_ChangeOrbitalSpace

      
      subroutine rpa_DiagonalizeFockHamiltonian(Ek, Cpk, Subspace, hHFpq)
            real(F64), dimension(:), intent(out)      :: Ek
            real(F64), dimension(:, :), intent(out)   :: Cpk
            real(F64), dimension(:, :), intent(in)    :: Subspace
            real(F64), dimension(:, :), intent(in)    :: hHFpq

            integer :: NMO_Subspace, NAO
            real(F64), dimension(:, :), allocatable :: hHFpl, hHFkl

            NMO_Subspace = size(Subspace, dim=2)
            NAO = size(Subspace, dim=1)
            allocate(hHFpl(NAO, NMO_Subspace))
            allocate(hHFkl(NMO_Subspace, NMO_Subspace))
            call real_ab(hHFpl, hHFpq, Subspace)
            call real_aTb(hHFkl, Subspace, hHFpl)
            call symmetric_eigenproblem(Ek, hHFkl, NMO_Subspace, .true.)
            call real_ab(Cpk, Subspace, hHFkl)
      end subroutine rpa_DiagonalizeFockHamiltonian
end module rpa_MeanField
