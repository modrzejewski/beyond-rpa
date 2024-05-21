module rpa_THC
      use arithmetic
      use real_linalg
      use display
      use clock
      use rpa_CC
      use rpa_CC_Singles
      use rpa_CC_Doubles
      use rpa_CC_Exchange
      use rpa_CCD_Corrections
      
      implicit none

contains

      subroutine rpa_THC_MBPT3(RPAOutput, THC_Xgp, &
            THC_Xga, THC_Xgi, THC_ZgkFull, THC_ZgkPiU, THC_BlockDim, &
            Freqs, FreqWeights, NFreqs, OccEnergies, VirtEnergies, FermiEnergy, &
            OccCoeffs, VirtCoeffs, hHF_ao, NOcc, NVirt, GuessNVecsT2, SmallEigenvalsCutoffT2, &
            MaxBatchDimT2, T2CutoffThresh, T2CutoffType, &
            T2CutoffSmoothStep, T2CutoffSmoothness, &
            T2CutoffCommonThresh, T2CouplingStrength, &
            T2AuxOrbitals, T2AuxNOCutoffThresh, T2AuxLOCutoffThresh, &
            PT_Order2, PT_Order3, NaturalOrbitals, NVirtNO, RPAParams, AOBasis)

            type(TRPAOutput), intent(inout)              :: RPAOutput
            real(F64), dimension(:, :), intent(in)       :: THC_Xgp
            real(F64), dimension(:, :, :), intent(in)    :: THC_Xga
            real(F64), dimension(:, :, :), intent(in)    :: THC_Xgi
            real(F64), dimension(:, :), intent(in)       :: THC_ZgkFull
            real(F64), dimension(:, :), intent(in)       :: THC_ZgkPiU
            integer, intent(in)                          :: THC_BlockDim
            real(F64), dimension(:), intent(in)          :: Freqs
            real(F64), dimension(:), intent(in)          :: FreqWeights
            integer, intent(in)                          :: NFreqs
            real(F64), dimension(:, :), intent(in)       :: OccEnergies
            real(F64), dimension(:, :), intent(in)       :: VirtEnergies
            real(F64), dimension(:), intent(in)          :: FermiEnergy
            real(F64), dimension(:, :, :), intent(in)    :: OccCoeffs
            real(F64), dimension(:, :, :), intent(in)    :: VirtCoeffs
            real(F64), dimension(:, :, :), intent(in)    :: hHF_ao
            integer, dimension(2), intent(in)            :: NOcc
            integer, dimension(2), intent(in)            :: NVirt
            integer, intent(in)                          :: GuessNVecsT2
            real(F64), intent(in)                        :: SmallEigenvalsCutoffT2
            integer, intent(in)                          :: MaxBatchDimT2
            real(F64), intent(in)                        :: T2CutoffThresh
            integer, intent(in)                          :: T2CutoffType
            logical, intent(in)                          :: T2CutoffSmoothStep
            real(F64), intent(in)                        :: T2CutoffSmoothness
            real(F64), intent(inout)                     :: T2CutoffCommonThresh
            real(F64), intent(in)                        :: T2CouplingStrength
            integer, intent(in)                          :: T2AuxOrbitals
            real(F64), intent(in)                        :: T2AuxNOCutoffThresh
            real(F64), intent(in)                        :: T2AuxLOCutoffThresh
            logical, intent(in)                          :: PT_Order2
            logical, intent(in)                          :: PT_Order3
            real(F64), dimension(:, :, :), allocatable, intent(out) :: NaturalOrbitals
            integer, dimension(2), intent(out)                      :: NVirtNO
            type(TRPAParams), intent(in)                         :: RPAParams
            type(TAOBasis), intent(in)                           :: AOBasis

            real(F64), dimension(:, :, :), allocatable :: PiUEigenvecs
            real(F64), dimension(:, :), allocatable :: PiUEigenvals
            real(F64), dimension(:, :, :), allocatable :: Rkai
            real(F64), dimension(:), allocatable :: Am
            real(F64), dimension(:, :), allocatable :: Uaim
            real(F64), dimension(:, :), allocatable :: THC_Zgh
            real(F64), dimension(:, :), allocatable :: NOCoeffs_mo
            real(F64), dimension(:, :), allocatable :: NOCoeffs_ao
            real(F64), dimension(:), allocatable :: NOVirtEnergies
            integer :: s, i
            integer :: ThisImage
            integer :: NVecsT2
            real(F64) :: Lambda
            type(tclock) :: timer, timer_total
            real(F64) :: t_T2, t_RPA, t_Corrections, t_NO, t_PiUDiag
            real(F64) :: EcRPA
            integer :: NMO, NAO
            integer :: NGridTHC, NCholesky, NVecsPiU
            integer :: MaxNai
            integer, parameter :: NSpins = 1

            call clock_start(timer_total)
            ThisImage = this_image()
            NCholesky = size(THC_ZgkFull, dim=2)
            NVecsPiU = size(THC_ZgkPiU, dim=2)            
            NGridTHC = size(THC_ZgkFull, dim=1)
            MaxNai = max(NOcc(1)*NVirt(1), NOcc(2)*NVirt(2))
            NMO = NOcc(1) + NVirt(1)
            NAO = size(OccCoeffs, dim=1)
            NVirtNO = 0
            s = 1
            t_T2 = ZERO
            t_RPA = ZERO
            t_Corrections = ZERO
            t_NO = ZERO
            allocate(THC_Zgh(NGridTHC, NGridTHC))
            allocate(PiUEigenvecs(NVecsPiU, NVecsPiU, NFreqs))
            allocate(PiUEigenvals(NVecsPiU, NFreqs))
            allocate(Rkai(NVecsPiU, MaxNai, NSpins))
            EcRPA = ZERO
            call real_abT(THC_Zgh, THC_ZgkFull, THC_ZgkFull)            
            !
            ! Polarizability Chi(u) and T2 amplitudes built from the semicanonical orbitals of hHF(OO+VV)
            ! RPA correlation energy, EcRPA, evaluated analytically from Pi(u)
            !
            call clock_start(timer)
            call rpa_THC_PiU(PiUEigenvecs, Rkai, THC_Xga, THC_Xgi, THC_ZgkPiU, NOcc, NVirt, &
                  Freqs, OccEnergies, VirtEnergies, THC_BlockDim, .true.)
            call rpa_CC_EcRPA_Analytic(EcRPA, PiUEigenvecs, Freqs, FreqWeights, NFreqs, NVecsPiU)
            RPAOutput%Energy(RPA_ENERGY_DIRECT_RING) = EcRPA
            t_RPA = clock_readwall(timer)
            if (RPAParams%TheoryLevel > RPA_THEORY_DIRECT_RING .or. RPAParams%ComputeNaturalOrbitals) then
                  !
                  ! T2 amplitudes. The T2 amplitudes are computed at full coupling strength, Lambda=1,
                  ! unless the T2CouplingStrength parameter has a non-default value. This should be
                  ! used only for debugging, e.g., verifying  the beyond-RPA terms against PT terms.
                  !
                  call clock_start(timer)
                  call rpa_CC_Diagonalize_PiU(PiUEigenvals, PiUEigenvecs, t_PiUDiag, NVecsPiU, NFreqs)
                  Lambda = T2CouplingStrength
                  call rpa_THC_CC_T2(Am, Uaim, RPAOutput%EigRPA, NVecsT2, PiUEigenvecs, &
                        PiUEigenvals, Rkai(:, :, s), NVecsPiU, NOcc(s), NVirt(s), &
                        Freqs, FreqWeights, NFreqs, Lambda, OccEnergies(:, s), VirtEnergies(:, s), &
                        SmallEigenvalsCutoffT2, GuessNVecsT2, MaxBatchDimT2, T2CutoffThresh, &
                        T2CutoffType, T2CutoffSmoothStep, T2CutoffSmoothness,T2CutoffCommonThresh, &
                        RPAParams)
                  deallocate(Rkai)
                  t_T2 = clock_readwall(timer)
                  if (RPAParams%ComputeNaturalOrbitals) then
                        call blankline()
                        call msg("Computing natural orbitals by diagonalization of the vv block of 1-RDM")
                        call msg("1-RDM: Table S2 in Ref. 2")
                        call msg("1. Ramberger, Sukurma, Schäfer, J. Chem. Phys. 151, 214106 (2019); doi: 10.1063/1.5128415")
                        call msg("2. Cieśliński, Tucholska, Modrzejewski, " &
                              // "J. Chem. Theory Comput. 19, 6619 (2023); doi: 10.1021/acs.jctc.3c00496")
                        call clock_start(timer)
                        call rpa_VirtualNO(NOCoeffs_mo, NVirtNO(s), Uaim, Am, NVecsT2, NOcc(s), &
                              NVirt(s), RPAParams%T2AuxNOCutoffThresh)
                        allocate(NaturalOrbitals(NAO, maxval(NVirtNO), NSpins))
                        call real_ab(NaturalOrbitals(:, 1:NVirtNO(s), s), &
                              VirtCoeffs(:, 1:NVirt(s), s), NOCoeffs_mo)
                        t_NO = clock_readwall(timer)
                  end if
                  if (RPAParams%TheoryLevel > RPA_THEORY_DIRECT_RING) then
                        ! ---------------------------------------------------------------------------------
                        ! SOSEX + higher-order contributions to the correlation energy derived from
                        ! the non-ring part of the expectation value of the hamiltonian
                        !
                        ! Variant with T's in the full canonical orbital basis
                        ! ---------------------------------------------------------------------------------
                        call clock_start(timer)
                        call rpa_Corrections(RPAOutput, THC_Zgh, THC_ZgkFull, THC_Xga(:, :, s), THC_Xgi(:, :, s), &
                              Uaim, Am, NOcc(s), NVirt(s), NVecsT2, NGridTHC, RPAParams)
                        t_Corrections = clock_readwall(timer)
                  end if
                  call move_alloc(from=Am, to=RPAOutput%Am)
                  RPAOutput%NVecsT2 = NVecsT2
            end if
            ! ---------------------------------------------------------------------------------
            ! Perturbation theory terms
            ! This is an extremely slow code and should be used only for debugging
            ! ---------------------------------------------------------------------------------
            if (PT_Order2) call rpa_PT_Order2(RPAOutput%Energy, &
                  THC_ZgkFull, THC_Xga(:, :, s), THC_Xgi(:, :, s), &
                  OccEnergies(:, s), VirtEnergies(:, s), NOcc(s), NVirt(s), NGridTHC)
            if (PT_Order3) call rpa_PT_Order3(RPAOutput%Energy, &
                  THC_ZgkFull, THC_Xga(:, :, s), THC_Xgi(:, :, s), &
                  OccEnergies(:, s), VirtEnergies(:, s), NOcc(s), NVirt(s), NGridTHC)
            call blankline()
            call msg(lfield("Total time", 50)     // str(clock_readwall(timer_total),d=1))
            call msg(lfield("RPA", 50)           // str(t_RPA,d=1))
            if (RPAParams%TheoryLevel > RPA_THEORY_DIRECT_RING .or. &
                  RPAParams%ComputeNaturalOrbitals) then
                  call msg(lfield("T2 amplitudes", 50)  // str(t_T2,d=1))
            end if
            if (RPAParams%TheoryLevel > RPA_THEORY_DIRECT_RING) then
                  call msg(lfield("beyond-RPA corrections", 50) // str(t_Corrections,d=1))
            end if
            if (RPAParams%ComputeNaturalOrbitals) then
                  call msg(lfield("natural orbitals", 50) // str(t_NO,d=1))
            end if
            call blankline()
      end subroutine rpa_THC_MBPT3


      subroutine rpa_THC_MOTransf(Xga, Xgi, Xgp, OccCoeffs_ao, VirtCoeffs_ao, NOcc, NVirt)
            !
            ! AO->MO transformation of the collocation matrices.
            !
            real(F64), dimension(:, :, :), intent(out) :: Xga
            real(F64), dimension(:, :, :), intent(out) :: Xgi
            real(F64), dimension(:, :), intent(in)     :: Xgp
            real(F64), dimension(:, :, :), intent(in)  :: OccCoeffs_ao
            real(F64), dimension(:, :, :), intent(in)  :: VirtCoeffs_ao
            integer, dimension(2), intent(in)          :: NOcc
            integer, dimension(2), intent(in)          :: NVirt

            integer :: NGrid, NAO, NSpins
            integer :: s

            NGrid = size(Xgp, dim=1)
            NAO = size(Xgp, dim=2)
            NSpins = size(OccCoeffs_ao, dim=3)
            do s = 1, NSpins
                  call real_ab_x(Xga(:, :, s), NGrid, Xgp, NGrid, VirtCoeffs_ao(:, :, s), &
                        NAO, NGrid, NVirt(s), NAO, ONE, ZERO)
                  call real_ab_x(Xgi(:, :, s), NGrid, Xgp, NGrid, OccCoeffs_ao(:, :, s), &
                        NAO, NGrid, NOcc(s), NAO, ONE, ZERO)
            end do
      end subroutine rpa_THC_MOTransf


      subroutine rpa_THC_PiU(PiU, Rkai, Xga, Xgi, Zgk, NOcc, NVirt, Freqs, &
            OccEnergies, VirtEnergies, MaxBlockDim, FullRkai)
            
            real(F64), dimension(:, :, :), intent(out)             :: PiU
            real(F64), dimension(:, :, :), intent(out)             :: Rkai
            real(F64), dimension(:, :, :), intent(in)              :: Xga
            real(F64), dimension(:, :, :), intent(in)              :: Xgi
            real(F64), dimension(:, :), intent(in)                 :: Zgk
            integer, dimension(2), intent(in)                      :: NOcc
            integer, dimension(2), intent(in)                      :: NVirt
            real(F64), dimension(:), intent(in)                    :: Freqs
            real(F64), dimension(:, :), intent(in)                 :: OccEnergies
            real(F64), dimension(:, :), intent(in)                 :: VirtEnergies
            integer, intent(in)                                    :: MaxBlockDim
            logical, intent(in)                                    :: FullRkai
            
            real(F64) :: Alpha, Beta
            integer :: s, t, NSpins
            integer :: NCholesky, NGridTHC
            integer :: NFreqs

            PiU = ZERO
            NSpins = size(Rkai, dim=3)
            NCholesky = size(Zgk, dim=2)
            NGridTHC = size(Zgk, dim=1)
            NFreqs = size(PiU, dim=3)
            Beta = ONE
            !
            ! Prefactor for scaling the matrix Pi(u):
            ! 
            ! (i) Take into account the factor of 2 which results from
            ! the summation of a complex number and its conjugate
            ! 
            ! 1/(Ea-Ei+iu)+1/(Ea-Ei-iu) = 2(Ea-Ei)/((Ea-Ei)**2 + u**2)
            ! 
            ! (ii) Multiply Pi(u) by the factor of 2 to account for the spin summation
            ! over doubly occupied closed-shell orbitals. The summation over spins
            ! is given explicitly in Eq. 58 of Ref. 1.
            !
            ! The sign is consistent with Eq. 20 in
            ! M. Modrzejewski, S. Yourdkhani, J. Klimes, J. Chem. Theory Comput. 16, 427 (2020);
            ! doi: 10.1021/acs.jctc.9b00979
            !
            if (NSpins == 1) then
                  !
                  ! Closed shell
                  !
                  Alpha = FOUR
            else
                  !
                  ! Open shell
                  !
                  Alpha = TWO
            end if
            do s = 1, NSpins
                  if (NOcc(s) > 0) then
                        if (FullRkai) then
                              t = s
                        else
                              t = 1
                        end if
                        call rpa_THC_PiU_BlockAlgo(PiU, Rkai(:, :, t), Xga(:, :, s), &
                              Xgi(:, :, s), Zgk, NOcc(s), NVirt(s), NCholesky, &
                              NGridTHC, Freqs, NFreqs, OccEnergies(:, s), VirtEnergies(:, s), &
                              Alpha, Beta, FullRkai, MaxBlockDim)
                  end if
            end do
      end subroutine rpa_THC_PiU
      

      subroutine rpa_THC_PiU_BlockAlgo(PiU, Rkai, Xga, Xgi, Zgk, NOcc, NVirt, NCholesky, &
            NGridTHC, Freqs, NFreqs, OccEnergies, VirtEnergies, Alpha, Beta, &
            FullRkai, MaxBlockDim)
            
            real(F64), dimension(:, :, :), intent(inout)           :: PiU
            real(F64), dimension(:, :), intent(out)                :: Rkai
            real(F64), dimension(:, :), intent(in)                 :: Xga
            real(F64), dimension(:, :), intent(in)                 :: Xgi
            real(F64), dimension(:, :), intent(in)                 :: Zgk
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: NCholesky
            integer, intent(in)                                    :: NGridTHC
            real(F64), dimension(:), intent(in)                    :: Freqs
            integer, intent(in)                                    :: NFreqs
            real(F64), dimension(:), intent(in)                    :: OccEnergies
            real(F64), dimension(:), intent(in)                    :: VirtEnergies            
            real(F64), intent(in)                                  :: Alpha
            real(F64), intent(in)                                  :: Beta
            logical, intent(in)                                    :: FullRkai
            integer, intent(in)                                    :: MaxBlockDim

            real(F64), dimension(:), allocatable :: W
            integer :: ai0, ai1, t, v0, v1, BlockDim
            integer :: u
            real(F64) :: Freq

            allocate(W(max(NGridTHC,NCholesky)*min(MaxBlockDim,NVirt*NOcc)))
            do t = 1, NOcc*NVirt, MaxBlockDim
                  ai0 = t
                  ai1 = min(t + MaxBlockDim - 1, NVirt*NOcc)
                  BlockDim = ai1 - ai0 + 1
                  if (FullRkai) then
                        v0 = ai0
                        v1 = ai1
                  else
                        v0 = 1
                        v1 = BlockDim
                  end if
                  call rpa_THC_Rkab(Rkai(:, v0:v1), W, ai0, ai1, Xga, Xgi, Zgk, &
                        NVirt, NOcc, NCholesky, NGridTHC, BlockDim)
                  do u = 1, NFreqs
                        Freq = Freqs(u)
                        !
                        ! W(k,ai) <- Chi(ai)*R(k,ai)
                        !
                        call rpa_THC_ChiRkai(W, Rkai(:, v0:v1), ai0, ai1, VirtEnergies, &
                              OccEnergies, Freq, NVirt, NCholesky, BlockDim)
                        !
                        ! Pi(k,l) <- Alpha * Sum(ai=ai0...ai1) R(k,ai)*ChiR(k,ai) + Beta * Pi(k, l)
                        !
                        call real_abT_x(PiU(:, :, u), NCholesky, Rkai(:, v0:v1), NCholesky, &
                              W, NCholesky, NCholesky, NCholesky, BlockDim, Alpha, Beta)
                  end do
            end do            
      end subroutine rpa_THC_PiU_BlockAlgo


      subroutine rpa_THC_ChiRkai(ChiRkai, Rkai, ai0, ai1, VirtEnergies, OccEnergies, &
            Freq, NVirt, NCholesky, BlockDim)

            integer, intent(in)                                    :: BlockDim
            real(F64), dimension(NCholesky, BlockDim), intent(out) :: ChiRkai
            real(F64), dimension(NCholesky, BlockDim), intent(in)  :: Rkai
            integer, intent(in)                                    :: ai0
            integer, intent(in)                                    :: ai1
            real(F64), dimension(:), intent(in)                    :: VirtEnergies
            real(F64), dimension(:), intent(in)                    :: OccEnergies
            real(F64), intent(in)                                  :: Freq
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: NCholesky

            integer :: ai, a, i, v
            real(F64) :: Dai, Chi
            
            !$omp parallel do private(ai, a, i, v, Dai, Chi) default(shared)
            do ai = ai0, ai1
                  ! ai = a + NVirt*(i-1) a is the faster-changing index
                  i = (ai - 1) / NVirt + 1
                  a = ai - NVirt * (i - 1)
                  v = ai - ai0 + 1
                  Dai = VirtEnergies(a) - OccEnergies(i)
                  Chi = Dai / (Dai**2 + Freq**2)
                  ChiRkai(:, v) = Chi * Rkai(:, v)
            end do
            !$omp end parallel do
      end subroutine rpa_THC_ChiRkai
      

      subroutine rpa_THC_Rkab(Rkab, Xgab, ab0, ab1, Xga, Xgb, Zgk, Na, Nb, NCholesky, &
            NGridTHC, BlockDim)

            integer, intent(in)                                    :: BlockDim
            real(F64), dimension(NCholesky, BlockDim), intent(out) :: Rkab
            real(F64), dimension(NGridTHC, BlockDim), intent(out)  :: Xgab
            integer, intent(in)                                    :: ab0
            integer, intent(in)                                    :: ab1
            real(F64), dimension(NGridTHC, Na), intent(in)         :: Xga
            real(F64), dimension(NGridTHC, Nb), intent(in)         :: Xgb
            real(F64), dimension(NGridTHC, NCholesky), intent(in)  :: Zgk
            integer, intent(in)                                    :: Na
            integer, intent(in)                                    :: Nb
            integer, intent(in)                                    :: NCholesky
            integer, intent(in)                                    :: NGridTHC

            integer :: ab, b, a, v
            
            !$omp parallel do private(ab, b, a, v) default(shared)                  
            do ab = ab0, ab1
                  ! ab = a + Na*(b-1) a is the faster-changing index
                  b = (ab - 1) / Na + 1
                  a = ab - Na * (b - 1)
                  v = ab - ab0 + 1
                  Xgab(:, v) = Xga(:, a) * Xgb(:, b)                        
            end do
            !$omp end parallel do
            call real_aTb_x(Rkab, NCholesky, Zgk, NGridTHC, Xgab, &
                  NGridTHC, NCholesky, BlockDim, NGridTHC, ONE, ZERO)
      end subroutine rpa_THC_Rkab


      subroutine rpa_THC_Rkai(Rkai, YXgai, i, Yga, Xgi, Zgk, NOcc, NVirt, NCholesky, NGridTHC)        
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: NCholesky
            integer, intent(in)                                    :: NGridTHC
            real(F64), dimension(NCholesky, NVirt), intent(out)    :: Rkai
            real(F64), dimension(NGridTHC, NVirt), intent(out)     :: YXgai
            integer, intent(in)                                    :: i
            real(F64), dimension(NGridTHC, NVirt), intent(in)      :: Yga
            real(F64), dimension(NGridTHC, NOcc), intent(in)       :: Xgi
            real(F64), dimension(NGridTHC, NCholesky), intent(in)  :: Zgk

            integer :: a
            
            !$omp parallel do private(a) default(shared)                  
            do a = 1, NVirt
                  YXgai(:, a) = Yga(:, a) * Xgi(:, i)
            end do
            !$omp end parallel do
            call real_aTb(Rkai, Zgk, YXgai)
      end subroutine rpa_THC_Rkai
end module rpa_THC
