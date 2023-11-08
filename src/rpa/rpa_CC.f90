module rpa_CC
      use arithmetic
      use math_constants
      use real_linalg
      use display
      use string
      use ParallelCholesky
      use GaussPRNG
      use PostSCF
      use quadratures
      use OptFreqQuad
      use OptLaplaceQuad
      use OrbDiffHist
      use rpa_definitions
      use rpa_core_MO
      use rpa_CC_Singles
      use rpa_CC_Exchange
      use rpa_CC_1RDM
      use rpa_CC_Doubles

      implicit none

contains
      
      subroutine rpa_CC_Summary(T1Approx, DensityApprox, Purify1RDM, ExchangeApprox, &
            Ec1RDMApprox, SmallEigenvalsCutoffT2, GuessNVecsT2, NACPoints, NumericalQuad, &
            MeanField, ChiOrbitals, T2Interp, MeanFieldApprox)
            
            integer, intent(in)   :: T1Approx
            integer, intent(in)   :: DensityApprox
            integer, intent(in)   :: Purify1RDM
            integer, intent(in)   :: ExchangeApprox
            integer, intent(in)   :: Ec1RDMApprox
            real(F64), intent(in) :: SmallEigenvalsCutoffT2
            integer, intent(in)   :: GuessNVecsT2
            integer, intent(in)   :: NACPoints
            logical, intent(in)   :: NumericalQuad
            integer, intent(in)   :: MeanField
            integer, intent(in)   :: ChiOrbitals
            logical, intent(in)   :: T2Interp
            integer, intent(in)   :: MeanFieldApprox

            call blankline()
            call msg("Beyond-direct RPA energy contributions: lambda-dependent 1-RDM and exchange terms", underline=.true.)
            call blankline()

            call msg("Partitioning of F(Lambda)")
            if (MeanField == RPA_MEAN_FIELD_KS_TYPE) then
                  call msg(lfield("", 30) // "F(Lambda) = (1-Lambda) h(Lambda=0) + Lambda hHF")
            else 
                  call msg(lfield("", 30) // "F(Lambda) = hHF(OO+VV) + Lambda hHF(VO+OV)")
            end if

            call msg("RPA density response function")
            select case (ChiOrbitals)
            case (RPA_ORBITALS_CANONICAL)
                  call msg(lfield("", 30) // "Chi(u) built from eigenstates of h(Lambda=0)")
            case (RPA_ORBITALS_SEMICANONICAL)
                  call msg(lfield("", 30) // "Chi(u) built from semicanonical orbitals of F(Lambda)")
            end select

            call rpa_CC_Quadrature_Summary(NACPoints, NumericalQuad)
            call rpa_CC_Singles_Summary(T1Approx)
            call rpa_CC_Doubles_Summary(SmallEigenvalsCutoffT2, GuessNVecsT2, T2Interp)
            call rpa_CC_Rho_Summary(DensityApprox, Purify1RDM, Ec1RDMApprox)

            if (Ec1RDMApprox /= RPA_Ec1RDM_LINEAR) then
                  call msg("Computation of Ec1RDM_Quadratic")
                  select case (MeanFieldApprox)
                  case (RPA_MEAN_FIELD_COUL_EXCH)
                        call msg(lfield("", 30) // "Exact integrals, Coulomb and exchange contributions")
                  end select
            end if
            
            call rpa_CC_Exchange_Summary(ExchangeApprox)
            call blankline()
      end subroutine rpa_CC_Summary


      subroutine rpa_CC_Quadrature_Summary(NACPoints, NumericalQuad)
            integer, intent(in) :: NACPoints
            logical, intent(in) :: NumericalQuad

            call msg("AC quadrature")
            if (NACPoints > 0) then
                  if (NumericalQuad) then
                        call msg(lfield("", 30) // "Gauss-Legendre, " // str(NACPoints) // " points")
                  else
                        call msg(lfield("", 30) // "computing energy contributions only at Lambda=1")
                        call msg(lfield("", 30) // "linear/quadratic interpolation between Lambda=0 and lambda=1")
                  end if
            else
                  call msg(lfield("", 30) // "numerical AC quadrature not used")
            end if
      end subroutine rpa_CC_Quadrature_Summary


      subroutine rpa_CC_TransformF(Fai, Fij, Fab, OccCoeffs, VirtCoeffs, F_ao, NOcc, NVirt, NAO)
            real(F64), dimension(NVirt, NOcc), intent(out)  :: Fai
            real(F64), dimension(NOcc, NOcc), intent(out)   :: Fij
            real(F64), dimension(NVirt, NVirt), intent(out) :: Fab
            real(F64), dimension(NAO, NOcc), intent(in)     :: OccCoeffs
            real(F64), dimension(NAO, NVirt), intent(in)    :: VirtCoeffs
            real(F64), dimension(NAO, NAO), intent(in)      :: F_ao
            integer, intent(in)                             :: NOcc
            integer, intent(in)                             :: NVirt
            integer, intent(in)                             :: NAO

            real(F64), dimension(:, :), allocatable :: Fpi, Fpa
            ! -------------------------------------------------------------------------------------
            ! Virtual-occupied block of the Fock matrix. Computed even if the option Semilocal
            ! is true. Note that Fai corresponds only to the active occupied orbitals.
            ! -------------------------------------------------------------------------------------
            allocate(Fpi(NAO, NOcc))
            call real_ab(Fpi, F_ao, OccCoeffs)
            call real_aTb(Fai, VirtCoeffs, Fpi)
            ! -------------------------------------------------------------------------------------
            ! Virtual-virtual, occupied-occupied blocks of the Fock matrix
            ! -------------------------------------------------------------------------------------
            call real_aTb(Fij, OccCoeffs, Fpi)
            deallocate(Fpi)
            allocate(Fpa(NAO, NVirt))
            call real_ab(Fpa, F_ao, VirtCoeffs)
            call real_aTb(Fab, VirtCoeffs, Fpa)
      end subroutine rpa_CC_TransformF

      
      subroutine rpa_CC_SemiCanonicalCoeffs(OccCoeffs_mo, VirtCoeffs_mo, Fii, Faa, h_HF_ij, h_HF_ab, &
            KS_OccEnergies, KS_VirtEnergies, Lambda, Nocc, NVirt)
            !
            ! Compute the eigenvectors of the occupied-occupied and virtual-virtual blocks of
            ! F(Lambda) = (1-Lambda)*h_KS + Lambda*h_HF
            ! (The diagonal blocks of the mean-field hamiltonian at coupling strength Lambda.)
            !
            real(F64), dimension(NOcc, NOcc), intent(out)   :: OccCoeffs_mo
            real(F64), dimension(NVirt, NVirt), intent(out) :: VirtCoeffs_mo
            real(F64), dimension(NOcc), intent(out)         :: Fii
            real(F64), dimension(NVirt), intent(out)        :: Faa
            real(F64), dimension(NOcc, NOcc), intent(in)    :: h_HF_ij
            real(F64), dimension(NVirt, NVirt), intent(in)  :: h_HF_ab
            real(F64), dimension(NOcc), intent(in)          :: KS_OccEnergies
            real(F64), dimension(NVirt), intent(in)         :: KS_VirtEnergies
            real(F64), intent(in)                           :: Lambda
            integer, intent(in)                             :: NOcc
            integer, intent(in)                             :: NVirt

            integer :: i, a
            ! ----------------------------------------------------------
            !                 Occupied-occupied block
            ! ----------------------------------------------------------
            OccCoeffs_mo = Lambda * h_HF_ij
            do i = 1, NOcc
                  OccCoeffs_mo(i, i) = OccCoeffs_mo(i, i) + (ONE - Lambda) * KS_OccEnergies(i)
            end do
            call symmetric_eigenproblem(Fii, OccCoeffs_mo, NOcc, .true.)
            ! ----------------------------------------------------------
            !                  Virtual-virtual block
            ! ----------------------------------------------------------
            VirtCoeffs_mo = Lambda * h_HF_ab
            do a = 1, NVirt
                  VirtCoeffs_mo(a, a) = VirtCoeffs_mo(a, a) + (ONE - Lambda) * KS_VirtEnergies(a)
            end do
            call symmetric_eigenproblem(Faa, VirtCoeffs_mo, NVirt, .true.)
      end subroutine rpa_CC_SemiCanonicalCoeffs      


      subroutine rpa_CC_Ec1RDM_Quadratic(Ec1RDM, Rho_mo_vo, Rho_mo_oo, Rho_mo_vv, OccCoeffs, VirtCoeffs, &
            NOcc, NVirt, NAO, ACWeights, NACPoints, Rkpq, NVecsPiU, ShellPairs, ShellPairLoc, ShellPairDim, &
            SubsetBounds, SubsetDim, NSubsets, AOBasis, ThreshFockJK, MeanFieldApprox)
            
            real(F64), intent(out)                                    :: Ec1RDM
            real(F64), dimension(NVirt, NOcc, NACPoints), intent(in)  :: Rho_mo_vo
            real(F64), dimension(NOcc, NOcc, NACPoints), intent(in)   :: Rho_mo_oo
            real(F64), dimension(NVirt, NVirt, NACPoints), intent(in) :: Rho_mo_vv
            real(F64), dimension(NAO, NOcc), intent(in)               :: OccCoeffs
            real(F64), dimension(NAO, NVirt), intent(in)              :: VirtCoeffs
            integer, intent(in)                                       :: NOcc
            integer, intent(in)                                       :: NVirt
            integer, intent(in)                                       :: NAO
            real(F64), dimension(:), intent(in)                       :: ACWeights
            integer, intent(in)                                       :: NACPoints
            real(F64), dimension(:, :, :), intent(in)                 :: Rkpq[*]
            integer, intent(in)                                       :: NVecsPiU
            integer, dimension(:, :), intent(in)                      :: ShellPairs
            integer, dimension(:, :), intent(in)                      :: ShellPairLoc
            integer, dimension(:), intent(in)                         :: ShellPairDim
            integer, dimension(:, :), intent(in)                      :: SubsetBounds
            integer, dimension(:), intent(in)                         :: SubsetDim
            integer, dimension(2), intent(in)                         :: NSubsets
            type(TAOBasis), intent(in)                                :: AOBasis
            real(F64), intent(in)                                     :: ThreshFockJK
            integer, intent(in)                                       :: MeanFieldApprox

            integer :: k
            integer :: NMO
            real(F64) :: Ec1RDM_k, Ec1RDM_Coul_k, Ec1RDM_Exch_k
            real(F64), dimension(:, :), allocatable :: Wpi, Wpa
            real(F64), dimension(:, :, :), allocatable :: Rho_ao

            NMO = NOcc + NVirt
            allocate(Rho_ao(NAO, NAO, 1))
            allocate(Wpi(NAO, NOcc))
            allocate(Wpa(NAO, NVirt))
            Ec1RDM = ZERO
            do k = 1, NACPoints
                  Rho_ao = ZERO
                  Wpi = ZERO
                  Wpa = ZERO
                  call real_ab_x(Wpi, NAO, VirtCoeffs, NAO, Rho_mo_vo(:, :, k), NVirt, NAO, NOcc, NVirt, ONE, ZERO)
                  call real_ab_x(Wpi, NAO, OccCoeffs, NAO, Rho_mo_oo(:, :, k), NOcc, NAO, NOcc, NOcc, ONE, ONE)                  
                  call real_abT_x(Rho_ao(:, :, 1), NAO, Wpi, NAO, OccCoeffs, NAO, NAO, NAO, NOcc, ONE, ZERO)
                  
                  call real_abT_x(Wpa, NAO, OccCoeffs, NAO, Rho_mo_vo(:, :, k), NVirt, NAO, NVirt, NOcc, ONE, ZERO)
                  call real_abT_x(Wpa, NAO, VirtCoeffs, NAO, Rho_mo_vv(:, :, k), NVirt, NAO, NVirt, NVirt, ONE, ONE)                  
                  call real_abT_x(Rho_ao(:, :, 1), NAO, Wpa, NAO, VirtCoeffs, NAO, NAO, NAO, NVirt, ONE, ONE)

                  select case (MeanFieldApprox)
                  case (RPA_MEAN_FIELD_COUL_EXCH)
                        call postscf_EHFTwoEl(Ec1RDM_k, Rho_ao, AOBasis, ThreshFockJK)
                  case default
                        call msg("Invalid method of mean-field energy calculation", MSG_ERROR)
                        error stop
                  end select
                  Ec1RDM = Ec1RDM + ACWeights(k) * Ec1RDM_k
            end do
      end subroutine rpa_CC_Ec1RDM_Quadratic


      subroutine rpa_CC_Ec1RDM_Linear(Ec1RDM, DeltaRho_mo_vo, DeltaRho_mo_oo, DeltaRho_mo_vv, &
            hHF_mo_vo, hHF_mo_oo, hHF_mo_vv, Ref_OccEnergies, Ref_VirtEnergies, NOcc, NVirt, &
            ACWeights, NACPoints, MeanField)
            !
            ! Compute the linear Ec1RDM term assuming the basis of hRef eigenstates for
            ! DeltaRho and hHF.
            !
            ! Ec1RDM(Linear) = Int(Lambda) Tr(DeltaRho(Lambda) dF(Lambda)/dLambda)
            !
            ! HF-type mean-field hamiltonian in the basis of hRef eigenstates
            ! ---
            ! F(Lambda) = O hHF O + V hHF V + Lambda * V hHF O + Lambda * O hHF V
            ! F(Lambda)ij = hHFij
            ! F(Lambda)ab = hHFab
            ! F(Lambda)ai = Lambda*hHFai
            ! F(Lambda)ia = Lambda*hHFia
            !
            ! KS-type mean-field hamiltonian in the basis of hKS eigenstates
            ! The perburbation contributed diagonal matrix elements
            ! ---
            ! F(Lambda)   = Lambda*hHF + (1-Lambda)*hRef
            ! F(Lambda)ij = Lambda*hHFij + (1-Lambda)Ei*delta(ij)
            ! F(Lambda)ab = Lambda*hHFab + (1-Lambda)Ea*delta(ab)
            ! F(Lambda)ai = Lambda*hHFai
            ! F(Lambda)ia = Lambda*hHFia
            ! ---
            !
            ! Note that for the HF-type semicanonical orbitals dF(Lambda)/dLambda
            ! vanishes in the occupied-occupied and virtual-virtual blocks.
            !
            real(F64), intent(out)                                    :: Ec1RDM
            real(F64), dimension(NVirt, NOcc, NACPoints), intent(in)  :: DeltaRho_mo_vo
            real(F64), dimension(NOcc, NOcc, NACPoints), intent(in)   :: DeltaRho_mo_oo
            real(F64), dimension(NVirt, NVirt, NACPoints), intent(in) :: DeltaRho_mo_vv
            real(F64), dimension(NVirt, NOcc), intent(in)             :: hHF_mo_vo
            real(F64), dimension(NOcc, NOcc), intent(in)              :: hHF_mo_oo
            real(F64), dimension(NVirt, NVirt), intent(in)            :: hHF_mo_vv
            real(F64), dimension(NOcc), intent(in)                    :: Ref_OccEnergies
            real(F64), dimension(NVirt), intent(in)                   :: Ref_VirtEnergies
            integer, intent(in)                                       :: NOcc
            integer, intent(in)                                       :: NVirt
            real(F64), dimension(:), intent(in)                       :: ACWeights
            integer, intent(in)                                       :: NACPoints
            integer, intent(in)                                       :: MeanField

            integer :: a, i, j, b, k
            real(F64) :: Trace_RhohRef, Trace_RhohHF
            real(F64), dimension(:, :), allocatable :: AvgRho_mo_vo, AvgRho_mo_oo, AvgRho_mo_vv

            allocate(AvgRho_mo_vo(NVirt, NOcc))
            AvgRho_mo_vo = ZERO
            do k = 1, NACPoints
                  AvgRho_mo_vo = AvgRho_mo_vo + ACWeights(k) * DeltaRho_mo_vo(:, :, k)
            end do
            Ec1RDM = ZERO
            do i = 1, NOcc
                  do a = 1, NVirt
                        Ec1RDM = Ec1RDM + AvgRho_mo_vo(a, i) * hHF_mo_vo(a, i)
                  end do
            end do
            Ec1RDM = TWO * Ec1RDM
            if (MeanField == RPA_MEAN_FIELD_KS_TYPE) then 
                  !
                  ! The occupied-occupied and virtual-virtual contributions in the  energy term linear
                  ! in Rho(Lambda)-Rho(0) vanish in the semicanonical basis of hHF. This is because
                  ! the occupied-occupied and virtual-virtual blocks of the mean field hamiltonian stay
                  ! constant along the adiabatic connection path. This is not the case for the KS-type
                  ! mean field.
                  !
                  allocate(AvgRho_mo_oo(NOcc, NOcc))
                  allocate(AvgRho_mo_vv(NVirt, NVirt))
                  AvgRho_mo_oo = ZERO
                  AvgRho_mo_vv = ZERO
                  !
                  ! Integrated denstity matrix
                  !
                  do k = 1, NACPoints
                        AvgRho_mo_oo(:,:) = AvgRho_mo_oo(:,:) + DeltaRho_mo_oo(:, :, k) * ACWeights(k)
                        AvgRho_mo_vv(:,:) = AvgRho_mo_vv(:,:) + DeltaRho_mo_vv(:, :, k) * ACWeights(k)
                  end do
                  !
                  ! Tr(h_KS*Rho_accumulated)
                  !
                  Trace_RhohRef=ZERO
                  
                  do i=1,NOcc
                        Trace_RhohRef = Trace_RhohRef + AvgRho_mo_oo(i,i)*Ref_OccEnergies(i)
                  end do
                  
                  do a=1,NVirt
                        Trace_RhohRef = Trace_RhohRef + AvgRho_mo_vv(a,a)*Ref_VirtEnergies(a)
                  end do
                  !
                  ! Tr(h_HF*Rho_accumulated)
                  !
                  Trace_RhohHF=ZERO
                  do j = 1, NOcc
                        do i = 1, NOcc
                              Trace_RhohHF = Trace_RhohHF + AvgRho_mo_oo(i, j) * hHF_mo_oo(i, j)
                        end do
                  end do
                  
                  do b = 1, NVirt
                        do a = 1, NVirt
                              Trace_RhohHF = Trace_RhohHF + AvgRho_mo_vv(a, b) * hHF_mo_vv(a, b)
                        end do
                  end do
                  Ec1RDM = Ec1RDM + Trace_RhohHF-Trace_RhohRef
            end if
      end subroutine rpa_CC_Ec1RDM_Linear
              

      subroutine rpa_CC_VcLambdaPiU(VcLambda, PiUEigenvals, NVecsPiU, Lambda, FreqWeights, NFreqs)
            real(F64), intent(out)                 :: VcLambda
            real(F64), dimension(:, :), intent(in) :: PiUEigenvals
            integer, intent(in)                    :: NVecsPiU
            real(F64), intent(in)                  :: Lambda
            real(F64), dimension(:), intent(in)    :: FreqWeights
            integer, intent(in)                    :: NFreqs

            integer :: k, u
            real(F64) :: VcLambdaU

            VcLambda = ZERO
            do u = 1, NFreqs
                  VcLambdaU = ZERO
                  do k = 1, NVecsPiU
                        VcLambdaU = VcLambdaU + Lambda * PiUEigenvals(k, u)**2 / (ONE + Lambda * PiUEigenvals(k, u))
                  end do
                  VcLambda = VcLambda + FreqWeights(u) * VcLambdaU
            end do
            VcLambda = -ONE/(TWO*PI) * VcLambda
      end subroutine rpa_CC_VcLambdaPiU


      subroutine rpa_CC_VcLambdaT2(VcLambda, Ak, Vkai, NVecsT2, NOcc, NVirt, Rkai, NVecsPiU)
            !
            ! Compute the RPA correlation energy by numerical
            ! integration of the cumulant
            !
            ! EcRPA = 2 * Int(Lambda) Tr(I(Lambda)*V) dLambda
            ! I(Lambda) = 2*T(Lambda)/(1-2*T(Lambda))
            !
            real(F64), intent(out)                                 :: VcLambda
            real(F64), dimension(NVecsT2), intent(in)              :: Ak
            real(F64), dimension(NVecsT2, NVirt*NOcc), intent(in)  :: Vkai
            integer, intent(in)                                    :: NVecsT2
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            real(F64), dimension(NVecsPiU, NVirt*NOcc), intent(in) :: Rkai
            integer, intent(in)                                    :: NVecsPiU

            integer :: k, l
            real(F64), dimension(:, :), allocatable :: VR

            allocate(VR(NVecsT2, NVecsPiU))
            VR = ZERO
            call real_abT_x(VR, NVecsT2, Vkai, NVecsT2, Rkai, NVecsPiU, NVecsT2, NVecsPiU, NVirt*NOcc, ONE, ZERO)
            VcLambda = ZERO
            do l = 1, NVecsPiU
                  do k = 1, NVecsT2
                        VcLambda = VcLambda + Ak(k)/(ONE-TWO*Ak(k)) * VR(k, l)**2
                  end do
            end do
            VcLambda = VcLambda * TWO * TWO
      end subroutine rpa_CC_VcLambdaT2


      subroutine rpa_CC_EcRPA_Numerical(EcRPA, VcLambda, ACWeights, NACPoints)
            real(F64), intent(out)              :: EcRPA
            real(F64), dimension(:), intent(in) :: VcLambda
            real(F64), dimension(:), intent(in) :: ACWeights
            integer, intent(in)                 :: NACPoints

            integer :: k

            EcRPA = ZERO
            do k = 1, NACPoints
                  EcRPA = EcRPA + ACWeights(k) * VcLambda(k)
            end do
      end subroutine rpa_CC_EcRPA_Numerical


      subroutine rpa_CC_EcRPA_Analytic(EcRPA, PiU, Freqs, FreqWeights, NFreqs, NVecsPiU)
            !
            ! Compute the RPA correlation energy using an analytic integral over lambda
            ! and numerical quadrature over frequencies.
            !
            real(F64), intent(out)                       :: EcRPA
            real(F64), dimension(:, :, :), intent(in)    :: PiU
            real(F64), dimension(:), intent(in)          :: Freqs
            real(F64), dimension(:), intent(in)          :: FreqWeights
            integer, intent(in)                          :: NFreqs
            integer, intent(in)                          :: NVecsPiU

            integer :: u, k
            real(F128) :: TrPiU, TrLogPiU, Eu
            real(F64), dimension(:), allocatable :: LogDetWork
            real(F64), dimension(:, :), allocatable :: W
            integer :: LogDetLWork
            integer :: ThisImage
            character(:), allocatable :: line

            ThisImage = this_image()
            allocate(W(NVecsPiU, NVecsPiU))
            EcRPA = ZERO
            if (ThisImage == 1) then
                  call real_LogDet_query(LogDetLWork, NVecsPiU, NVecsPiU)
                  allocate(LogDetWork(LogDetLWork))
                  do u = 1, NFreqs
                        !
                        ! Compute the u-frequency contribution to the correlation energy
                        ! E(u) = Sum(k=1...NVecsPiU) log(1+LambdaK) - LambdaK
                        ! where LambdaK's are the eigenvalues of Pi(u)
                        !
                        TrPiU = 0.0_F128
                        W = PiU(:, :, u)
                        do k = 1, NVecsPiU
                              TrPiU = TrPiU + real(W(k, k), F128)
                        end do
                        do k = 1, NVecsPiU
                              W(k, k) = W(k, k) + ONE
                        end do
                        call real_LogDet(TrLogPiU, W, NVecsPiU, NVecsPiU, LogDetWork, LogDetLWork)
                        Eu = TrLogPiU - TrPiU
                        EcRPA = EcRPA + ONE/(TWO*PI) * real(Eu, F128) * FreqWeights(u)
                        if (u == 1) then
                              line = lfield("#", 5) // lfield("Freq", 10) // lfield("Weight", 10) &
                                    // lfield("E(Freq)", 20)
                              call blankline()
                              call midrule(width=55)
                              call msg(line)
                              call midrule(width=55)
                        end if
                        line = lfield(str(u), 5) // lfield(str(Freqs(u),d=1), 10) // lfield(str(FreqWeights(u),d=1), 10) &
                              // lfield(str(real(Eu, F64),d=6), 20)
                        call msg(line)
                  end do
                  call blankline()
                  call msg("RPA correlation energy (a.u.): " // str(EcRPA, d=8))
            end if
            call co_broadcast(EcRPA, source_image=1)
      end subroutine rpa_CC_EcRPA_Analytic


      subroutine rpa_CC_PiU(PiU, Rkai, ChiRkai, Rkpq, OccCoeffs_ao, VirtCoeffs_ao, OccEnergies, VirtEnergies, &
            NVecsPiU, Freqs, NFreqs, OccSubsetDim, VirtSubsetDim, OccBounds, VirtBounds, &
            ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
            SubsetDim, NSubsets, NAngFunc, NAO, NSpins)
            !
            ! Transform the Cholesky vectors to the MO basis and compute the Pi(u) matrix
            !
            real(F64), dimension(:, :, :), intent(out) :: PiU
            real(F64), dimension(:, :, :), intent(out) :: Rkai
            real(F64), dimension(:, :), intent(out)    :: ChiRkai
            real(F64), dimension(:, :, :), intent(in)  :: Rkpq
            real(F64), dimension(:, :, :), intent(in)  :: OccCoeffs_ao
            real(F64), dimension(:, :, :), intent(in)  :: VirtCoeffs_ao
            real(F64), dimension(:, :), intent(in)     :: OccEnergies
            real(F64), dimension(:, :), intent(in)     :: VirtEnergies
            integer, intent(in)                        :: NVecsPiU
            real(F64), dimension(:), intent(in)        :: Freqs
            integer, intent(in)                        :: NFreqs
            integer, dimension(:, :), intent(in)       :: OccSubsetDim
            integer, dimension(:, :), intent(in)       :: VirtSubsetDim
            integer, dimension(:, :, :), intent(in)    :: OccBounds
            integer, dimension(:, :, :), intent(in)    :: VirtBounds
            integer, dimension(:, :), intent(in)       :: ShellPairs
            integer, dimension(:, :), intent(in)       :: ShellPairLoc
            integer, dimension(:), intent(in)          :: ShellPairDim
            integer, dimension(:), intent(in)          :: ShellLoc
            integer, dimension(:), intent(in)          :: ShellParamsIdx
            integer, dimension(:, :), intent(in)       :: SubsetBounds
            integer, dimension(:), intent(in)          :: SubsetDim
            integer, dimension(2), intent(in)          :: NSubsets
            integer, dimension(:), intent(in)          :: NAngFunc
            integer, intent(in)                        :: NAO
            integer, intent(in)                        :: NSpins

            integer :: u, Y

            Y = this_image()
            !
            ! Transform AO Cholesky vectors to the occupied-virtual semicanonical basis
            !
            call rpa_CRG(Rkai, Rkpq, OccCoeffs_ao, VirtCoeffs_ao, OccSubsetDim(:, Y), VirtSubsetDim(:, Y), &
                  OccBounds(:, :, Y), VirtBounds(:, :, Y), NSpins, &
                  ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
                  SubsetDim, NSubsets, NAngFunc, NAO, NVecsPiU)
            !
            ! Compute the Pi(u) matrices for all frequencies. The Pi(u) matrix is built using the polarizability Chi(u)
            ! in Lambda-dependent semicanonical orbitals and orbital energies.
            !
            PiU = ZERO
            do u = 1, NFreqs
                  call rpa_GPiUG_MO(PiU(:, :, u), Rkai, ChiRkai, OccEnergies, VirtEnergies, OccSubsetDim(:, Y), &
                        VirtSubsetDim(:, Y), OccBounds(:, :, Y), VirtBounds(:, :, Y), NSpins, Freqs(u))
            end do
            call co_sum(PiU, result_image=1)
      end subroutine rpa_CC_PiU


      subroutine rpa_CC_TransfRho(Rho_mo_vo, Rho_mo_oo, Rho_mo_vv, Rho_semi_vo, Rho_semi_oo, Rho_semi_vv, &
            SemiOccCoeffs, SemiVirtCoeffs, NOcc, NVirt)

            real(F64), dimension(:, :), intent(out)   :: Rho_mo_vo
            real(F64), dimension(:, :), intent(out)   :: Rho_mo_oo
            real(F64), dimension(:, :), intent(out)   :: Rho_mo_vv
            real(F64), dimension(:, :), intent(in)    :: Rho_semi_vo
            real(F64), dimension(:, :), intent(in)    :: Rho_semi_oo
            real(F64), dimension(:, :), intent(in)    :: Rho_semi_vv
            real(F64), dimension(:, :), intent(in)    :: SemiOccCoeffs
            real(F64), dimension(:, :), intent(in)    :: SemiVirtCoeffs
            integer, intent(in)                       :: NOcc
            integer, intent(in)                       :: NVirt

            real(F64), dimension(:), allocatable :: W
            integer :: MaxN
            integer :: m, n, k1, k2

            MaxN = max(NOcc, NVirt)
            allocate(W(MaxN**2))
            !
            ! VO
            !
            m = NVirt
            n = NOcc
            k1 = NOcc
            k2 = NVirt
            call real_abT_x(W, m, Rho_semi_vo, m, SemiOccCoeffs, n, m, n, k1, ONE, ZERO)
            call real_ab_x(Rho_mo_vo, m, SemiVirtCoeffs, m, W, m, m, n, k2, ONE, ZERO)
            !
            ! OO
            !
            m = NOcc
            n = NOcc
            k1 = NOcc
            k2 = NOcc
            call real_abT_x(W, m, Rho_semi_oo, m, SemiOccCoeffs, n, m, n, k1, ONE, ZERO)
            call real_ab_x(Rho_mo_oo, m, SemiOccCoeffs, m, W, m, m, n, k2, ONE, ZERO)
            !
            ! VV
            !
            m = NVirt
            n = NVirt
            k1 = NVirt
            k2 = NVirt
            call real_abT_x(W, m, Rho_semi_vv, m, SemiVirtCoeffs, n, m, n, k1, ONE, ZERO)
            call real_ab_x(Rho_mo_vv, m, SemiVirtCoeffs, m, W, m, m, n, k2, ONE, ZERO)
      end subroutine rpa_CC_TransfRho


      subroutine rpa_CC_Energy_2(Energy, Rkpq, ACPoints, ACWeights, &
            QuadStart, QuadEnd, DerivStart, DerivEnd, Lambda1, NVecsPiU, &
            Freqs, FreqWeights, NFreqs, OccCoeffs_ao, VirtCoeffs_ao, OccEnergies, VirtEnergies, &
            NOcc, NVirt, OccSubsetDim, VirtSubsetDim, OccBounds, VirtBounds, &
            ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
            SubsetDim, NSubsets, NAngFunc, NAO, MaxNai, T1Approx, ExchangeApprox, Ec1RDMApprox, &
            DensityApprox, Purify1RDM, GuessNVecsT2, SmallEigenvalsCutoffT2, ChiOrbitals, &
            MeanField, T2Interp, MeanFieldApprox, MaxBatchDimT2, AOBasis, hHF_ao)

            real(F64), dimension(:), intent(inout)       :: Energy
            real(F64), dimension(:, :, :), intent(in)    :: Rkpq[*]
            real(F64), dimension(:), intent(in)          :: ACPoints
            real(F64), dimension(:), intent(in)          :: ACWeights
            integer, intent(in)                          :: QuadStart
            integer, intent(in)                          :: QuadEnd
            integer, intent(in)                          :: DerivStart
            integer, intent(in)                          :: DerivEnd
            integer, intent(in)                          :: Lambda1
            integer, intent(in)                          :: NVecsPiU
            real(F64), dimension(:), intent(in)          :: Freqs
            real(F64), dimension(:), intent(in)          :: FreqWeights
            integer, intent(in)                          :: NFreqs
            real(F64), dimension(:, :, :), intent(inout) :: OccCoeffs_ao
            real(F64), dimension(:, :, :), intent(inout) :: VirtCoeffs_ao
            real(F64), dimension(:, :), intent(inout)    :: OccEnergies
            real(F64), dimension(:, :), intent(inout)    :: VirtEnergies
            integer, dimension(2), intent(in)            :: NOcc
            integer, dimension(2), intent(in)            :: NVirt
            integer, dimension(:, :), intent(in)         :: OccSubsetDim
            integer, dimension(:, :), intent(in)         :: VirtSubsetDim
            integer, dimension(:, :, :), intent(in)      :: OccBounds
            integer, dimension(:, :, :), intent(in)      :: VirtBounds
            integer, dimension(:, :), intent(in)         :: ShellPairs
            integer, dimension(:, :), intent(in)         :: ShellPairLoc
            integer, dimension(:), intent(in)            :: ShellPairDim
            integer, dimension(:), intent(in)            :: ShellLoc
            integer, dimension(:), intent(in)            :: ShellParamsIdx
            integer, dimension(:, :), intent(in)         :: SubsetBounds
            integer, dimension(:), intent(in)            :: SubsetDim
            integer, dimension(2), intent(in)            :: NSubsets
            integer, dimension(:), intent(in)            :: NAngFunc
            integer, intent(in)                          :: NAO
            integer, intent(in)                          :: MaxNai
            integer, intent(in)                          :: T1Approx
            integer, intent(in)                          :: ExchangeApprox
            integer, intent(in)                          :: Ec1RDMApprox
            integer, intent(in)                          :: DensityApprox
            integer, intent(in)                          :: Purify1RDM            
            integer, intent(in)                          :: GuessNVecsT2
            real(F64), intent(in)                        :: SmallEigenvalsCutoffT2
            integer, intent(in)                          :: ChiOrbitals
            integer, intent(in)                          :: MeanField
            logical, intent(in)                          :: T2Interp
            integer, intent(in)                          :: MeanFieldApprox
            integer, intent(in)                          :: MaxBatchDimT2
            type(TAOBasis), intent(in)                   :: AOBasis
            real(F64), dimension(:, :, :), intent(in)    :: hHF_ao

            real(F64), dimension(:, :, :), allocatable :: PiUEigenvecs
            real(F64), dimension(:, :), allocatable :: PiUEigenvals
            real(F64), dimension(:, :, :), allocatable :: Rkai
            real(F64), dimension(:, :), allocatable :: ChiRkai
            real(F64), dimension(:, :, :), allocatable :: Rho_mo_vo, Rho_mo_oo, Rho_mo_vv
            real(F64), dimension(:, :), allocatable :: Rho_semi_vo, Rho_semi_oo, Rho_semi_vv
            real(F64), dimension(:, :), allocatable :: hHF_vo, hHF_vv, hHF_oo
            real(F64), dimension(:, :, :), allocatable :: SemiOccCoeffs_mo, SemiOccCoeffs_ao
            real(F64), dimension(:, :, :), allocatable :: SemiVirtCoeffs_mo, SemiVirtCoeffs_ao
            real(F64), dimension(:, :), allocatable :: SemiFii, SemiFaa
            real(F64), dimension(:), allocatable :: A, A1
            real(F64), dimension(:, :), allocatable :: V
            real(F64), dimension(:, :), allocatable :: T1
            real(F64), dimension(:), allocatable :: VxLambda, VcLambdaT2, VcLambdaPiU
            integer :: s
            integer :: Y, k
            integer :: ThisImage
            integer :: NVecsT2
            integer :: NACPoints, NQuadPoints
            real(F64) :: Lambda
            type(tclock) :: timer_total, timer, timer_iter
            logical :: UseOperatorS
            real(F64) :: t_T2, t_T1, t_exchange, t_PiU, t_PiUDiag, t_Ec1RDMQuad, t_iter
            real(F64) :: EcExchange, EcRPA, Ec1RDM_Linear, Ec1RDM_Quadratic
            real(F64) :: EcExchange_MBPT3_1, EcExchange_MBPT3_2, EcExchange_SOSEX, EcExchange_Cumulant
            real(F64) :: EcRPANumT2, EcRPANumPiU, EcRPA_Canonical, EcRPA_Semicanonical
            real(F64) :: Alpha
            integer :: NMO
            logical :: RequiresT2Lambda
            integer, parameter :: NSpins = 1
            real(F64), parameter :: ThreshFockJK = 1.0E-11_F64

            if (T2Interp .and. MeanField == RPA_MEAN_FIELD_KS_TYPE) then
                  call msg("Cannot combine interpolation of T2(Lambda) and linear switching in the mean-field hamiltonian", MSG_ERROR)
                  error stop
            end if

            if (ExchangeApprox == RPA_EXCHANGE_MBPT3_1_NUMERICAL .and. &
                  MeanField == RPA_MEAN_FIELD_KS_TYPE .and. &
                  ChiOrbitals == RPA_ORBITALS_SEMICANONICAL) then
                  call msg("Unsupported combination of options for calculating the exchange contribution", MSG_ERROR)
                  error stop
            end if
            !
            ! Check if computing T2(Lambda) on each quadrature node is required.
            ! Note that this flag does not affect the computation of T2(Lambda=1).
            !
            if ((ExchangeApprox == RPA_EXCHANGE_MBPT3_1 .or. &
                  ExchangeApprox == RPA_EXCHANGE_MBPT3_2 .or. &
                  ExchangeApprox == RPA_EXCHANGE_SOSEX .or. &
                  ExchangeApprox == RPA_EXCHANGE_NONE) .and. &
                  (DensityApprox == RPA_RHO_T1_LINEAR .or. &
                  DensityApprox == RPA_RHO_T1_QUADRATIC .or. &
                  DensityApprox == RPA_RHO_T1_EXPONENTIAL) .and. &
                  T1Approx == RPA_T1_MEAN_FIELD) then
                  !
                  ! T2 on the quadrature nodes is not required
                  !
                  RequiresT2Lambda = .false.
            else
                  !
                  ! T2 on the quadrature nodes is required
                  !
                  RequiresT2Lambda = .true.
            end if
            call clock_start(timer_total)
            ThisImage = this_image()
            NACPoints = size(ACPoints)
            NMO = NOcc(1) + NVirt(1)
            s = 1
            Y = ThisImage
            t_T1 = ZERO
            t_exchange = ZERO
            t_PiU = ZERO
            t_PiUDiag = ZERO
            t_Ec1RDMQuad = ZERO
            t_T2 = ZERO
            allocate(PiUEigenvecs(NVecsPiU, NVecsPiU, NFreqs))
            allocate(PiUEigenvals(NVecsPiU, NFreqs))
            allocate(Rkai(NVecsPiU, MaxNai, NSpins))
            allocate(Rho_semi_vo(NVirt(s), NOcc(s)))
            allocate(Rho_semi_oo(NOcc(s), NOcc(s)))
            allocate(Rho_semi_vv(NVirt(s), NVirt(s)))
            allocate(Rho_mo_vo(NVirt(s), NOcc(s), NACPoints))
            allocate(Rho_mo_oo(NOcc(s), NOcc(s), NACPoints))
            allocate(Rho_mo_vv(NVirt(s), NVirt(s), NACPoints))
            allocate(VcLambdaT2(NACPoints))
            allocate(VcLambdaPiU(NACPoints))
            allocate(VxLambda(NACPoints))
            allocate(hHF_vv(NVirt(s), NVirt(s)))
            allocate(hHF_oo(NOcc(s), NOcc(s)))
            allocate(hHF_vo(NVirt(s), NOcc(s)))
            allocate(SemiOccCoeffs_mo(NOcc(s), NOcc(s), NSpins))
            allocate(SemiVirtCoeffs_mo(NVirt(s), NVirt(s), NSpins))
            allocate(SemiOccCoeffs_ao(NAO, NOcc(s), NSpins))
            allocate(SemiVirtCoeffs_ao(NAO, NVirt(s), NSpins))
            allocate(SemiFii(NOcc(s), NSpins))
            allocate(SemiFaa(NVirt(s), NSpins))
            allocate(ChiRkai(NVecsPiU, MaxNai))
            allocate(T1(NVirt(s), NOcc(s)))
            allocate(A(0))
            allocate(V(0, 0))
            !
            ! Print out the summary of approximations and numerical parameters
            !
            if (QuadStart > 0) then
                  NQuadPoints = QuadEnd - QuadStart + 1
            else
                  NQuadPoints = 0
            end if
            call rpa_CC_Summary(T1Approx, DensityApprox, Purify1RDM, ExchangeApprox, &
                  Ec1RDMApprox, SmallEigenvalsCutoffT2, GuessNVecsT2, NQuadPoints, &
                  .true., MeanField, ChiOrbitals, T2Interp, MeanFieldApprox)
            
            call msg("Memory allocation (in gigabytes, per image):")
            call msg("Rkai             " // str(io_size_byte(Rkai)/(1024.0_F64**3),d=1))
            call msg("ChiRkai          " // str(io_size_byte(ChiRkai)/(1024.0_F64**3),d=1))
            call msg("Pi(u) eigenvecs  " // str(io_size_byte(PiUEigenvecs)/(1024.0_F64**3),d=1))

            EcRPA = ZERO
            EcRPA_Canonical = ZERO
            EcRPA_Semicanonical = ZERO
            EcExchange = ZERO
            EcExchange_MBPT3_1 = ZERO
            EcExchange_MBPT3_2 = ZERO
            EcExchange_SOSEX = ZERO
            EcExchange_Cumulant = ZERO
            Ec1RDM_Linear = ZERO
            Ec1RDM_Quadratic = ZERO
            VcLambdaPiU = ZERO
            VcLambdaT2 = ZERO
            VxLambda = ZERO
            Rho_mo_vo = ZERO
            Rho_mo_oo = ZERO
            Rho_mo_vv = ZERO
            !
            ! Direct RPA correlation energy computed using canonical orbitals.
            ! This contribution is computed regardless of the ChiOrbitals setting.
            ! The direct RPA correlation energy is evaluated analytically from Pi(u).
            !
            if (ChiOrbitals == RPA_ORBITALS_CANONICAL) then
                  call clock_start(timer)
                  call rpa_CC_PiU(PiUEigenvecs, Rkai, ChiRkai, Rkpq, OccCoeffs_ao, VirtCoeffs_ao, OccEnergies, VirtEnergies, &
                        NVecsPiU, Freqs, NFreqs, OccSubsetDim, VirtSubsetDim, OccBounds, VirtBounds, &
                        ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
                        SubsetDim, NSubsets, NAngFunc, NAO, NSpins)
                  call rpa_CC_EcRPA_Analytic(EcRPA_Canonical, PiUEigenvecs, Freqs, FreqWeights, NFreqs, NVecsPiU)
                  t_PiU = t_PiU + clock_readwall(timer)
            end if

            call rpa_CC_TransformF(hHF_vo, hHF_oo, hHF_vv, OccCoeffs_ao(:, :, s), VirtCoeffs_ao(:, :, s), &
                  hHF_ao(:, :, s), NOcc(s), NVirt(s), NAO)
                  
            if (MeanField == RPA_MEAN_FIELD_HF_TYPE) then
                  Lambda = ONE
                  call rpa_CC_SemiCanonicalCoeffs(SemiOccCoeffs_mo(:, :, s), SemiVirtCoeffs_mo(:, :, s), &
                        SemiFii(:, s), SemiFaa(:, s), hHF_oo, hHF_vv, OccEnergies(:, s), VirtEnergies(:, s), Lambda, &
                        NOcc(s), NVirt(s))
                  call real_ab(SemiOccCoeffs_ao(:, :, s), OccCoeffs_ao(:, :, s), SemiOccCoeffs_mo(:, :, s))
                  call real_ab(SemiVirtCoeffs_ao(:, :, s), VirtCoeffs_ao(:, :, s), SemiVirtCoeffs_mo(:, :, s))
            end if

            if (MeanField == RPA_MEAN_FIELD_HF_TYPE .and. ChiOrbitals == RPA_ORBITALS_SEMICANONICAL) then
                  !
                  ! Polarizability Chi(u) and T2 amplitudes built from the semicanonical orbitals of hHF(OO+VV)
                  ! Correlation energy evaluated analytically from Pi(u)
                  !
                  call clock_start(timer)
                  call rpa_CC_PiU(PiUEigenvecs, Rkai, ChiRkai, Rkpq, SemiOccCoeffs_ao, SemiVirtCoeffs_ao, SemiFii, SemiFaa, &
                        NVecsPiU, Freqs, NFreqs, OccSubsetDim, VirtSubsetDim, OccBounds, VirtBounds, &
                        ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
                        SubsetDim, NSubsets, NAngFunc, NAO, NSpins)
                  call rpa_CC_EcRPA_Analytic(EcRPA_Semicanonical, PiUEigenvecs, Freqs, FreqWeights, NFreqs, NVecsPiU)
                  t_PiU = t_PiU + clock_readwall(timer)
            end if

            call msg(lfield("#", 3) // lfield("Lambda", 10) // lfield("Quadrature weight", 20) &
                  // lfield("NVecsT2", 10) // lfield("Time [s]", 15))
            do k = 1, NACPoints
                  call clock_start(timer_iter)
                  !
                  ! If T2 interpolation is disabled, Lambda is the kth point of the quadrature
                  ! If T2 interpolation is enabled, Lambda for k=1 equals one. The actual points
                  ! of the Gauss quadrature start at k=2 and go up to NACPoints.
                  !
                  Lambda = ACPoints(k)
                  if (MeanField == RPA_MEAN_FIELD_KS_TYPE) then
                        !
                        ! Semicanonical orbitals of F(Lambda) = (1-Lambda)*hKS + Lambda*hHF
                        ! For this type of the mean-field hamiltonian the semicanonical orbitals
                        ! require recomputation for each lambda.
                        !
                        call rpa_CC_SemiCanonicalCoeffs(SemiOccCoeffs_mo(:, :, s), SemiVirtCoeffs_mo(:, :, s), &
                              SemiFii(:, s), SemiFaa(:, s), hHF_oo, hHF_vv, OccEnergies(:, s), VirtEnergies(:, s), Lambda, &
                              NOcc(s), NVirt(s))
                        call real_ab(SemiOccCoeffs_ao(:, :, s), OccCoeffs_ao(:, :, s), SemiOccCoeffs_mo(:, :, s))
                        call real_ab(SemiVirtCoeffs_ao(:, :, s), VirtCoeffs_ao(:, :, s), SemiVirtCoeffs_mo(:, :, s))
                  end if

                  if (MeanField == RPA_MEAN_FIELD_KS_TYPE .and. ChiOrbitals == RPA_ORBITALS_SEMICANONICAL) then
                        !
                        ! If the density-response function Chi(u) is built from semicanonical orbitals
                        ! of the KS-type mean field hamiltonian, the Pi(u) matrix and the T2 amplitudes
                        ! need to be recomputed for each Lambda. This combination of options is incompatible
                        ! with linear interpolation of T2. An error message is issued earlier and the execution
                        ! never comes to this point with this set of options.
                        !
                        call clock_start(timer)
                        call rpa_CC_PiU(PiUEigenvecs, Rkai, ChiRkai, Rkpq, SemiOccCoeffs_ao, SemiVirtCoeffs_ao, SemiFii, SemiFaa, &
                              NVecsPiU, Freqs, NFreqs, OccSubsetDim, VirtSubsetDim, OccBounds, VirtBounds, &
                              ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
                              SubsetDim, NSubsets, NAngFunc, NAO, NSpins)
                        t_PiU = t_PiU + clock_readwall(timer)
                  end if

                  if ((MeanField == RPA_MEAN_FIELD_KS_TYPE .and. ChiOrbitals == RPA_ORBITALS_SEMICANONICAL) .or. k == 1) then
                        !
                        ! Diagonalize Pi(u) matrices for all frequencies.
                        ! For the HF-type mean-field hamiltonian the diagonalization is done only once.
                        ! For the KS-type mean-field hamiltonian the diagonalization is done only once if
                        ! the Chi(u) matrix is built from the canonical orbitals of hKS.
                        ! For the KS-type mean-field hamiltonian and Chi(u) built from semicanonical
                        ! orbitals, the diagonalization of Pi(u) is done for each Lambda.
                        !
                        call rpa_CC_Diagonalize_PiU(PiUEigenvals, PiUEigenvecs, t_PiUDiag, NVecsPiU, NFreqs)
                  end if
                  ! -----------------------------------------------------
                  !            T2 amplitude equation at Lambda
                  ! -----------------------------------------------------
                  if (k == Lambda1 .or. &
                        (k >= DerivStart .and. k <= DerivEnd) .or. &
                        (k >= QuadSTart .and. k <= QuadEnd .and. RequiresT2Lambda .and. (.not. T2Interp))) then
                        
                        call clock_start(timer)
                        !
                        ! If linear interpolation isn't enabled, T2 amplitudes are computed for each Lambda
                        ! regardless of the mean-field hamiltonian. This means that even if the Pi(u) matrix
                        ! stays the same, T2 needs to be diagonalized for each Lambda.
                        !
                        ! If linear interpolation is enabled, then the first point of the quadrature (k=1)
                        ! corresponds to Lambda=1 and quadrature weight equal to zero. T2 is computed
                        ! in the first iteration and then rescaled for all subsequent points
                        ! of the Gaussian quadrature.
                        !
                        if (ChiOrbitals == RPA_ORBITALS_SEMICANONICAL) then
                              call rpa_CC_T2(A, V, NVecsT2, PiUEigenvecs, PiUEigenvals, Rkai(:, :, s), &
                                    NVecsPiU, NOcc(s), NVirt(s), OccBounds(:, s, Y), VirtBounds(:, s, Y), &
                                    Freqs, FreqWeights, NFreqs, Lambda, SemiFii(:, s), SemiFaa(:, s), &
                                    SmallEigenvalsCutoffT2, GuessNVecsT2, MaxBatchDimT2)
                        else
                              call rpa_CC_T2(A, V, NVecsT2, PiUEigenvecs, PiUEigenvals, Rkai(:, :, s), &
                                    NVecsPiU, NOcc(s), NVirt(s), OccBounds(:, s, Y), VirtBounds(:, s, Y), &
                                    Freqs, FreqWeights, NFreqs, Lambda, OccEnergies(:, s), VirtEnergies(:, s), &
                                    SmallEigenvalsCutoffT2, GuessNVecsT2, MaxBatchDimT2)
                        end if
                        t_T2 = t_T2 + clock_readwall(timer)
                  end if
                  if (T2Interp .and. k==Lambda1 .and. RequiresT2Lambda) then
                        !
                        ! Save the eigenvalues of T2(Lambda=1) for subsequent iterations.
                        ! For the Lambda points of the Gaussian quadrature the T2 amplitudes
                        ! will be obtained by rescaling A1.
                        !
                        allocate(A1(NVecsT2))
                        A1 = A
                  end if
                  if (T2Interp .and. k >= QuadStart .and. k <= QuadEnd .and. RequiresT2Lambda) then
                        !
                        ! Linear interpolation is enabled. T2 amplitudes are obtained by rescaling
                        ! the T2 eigenvalues computed at Lambda=1.
                        !
                        A = Lambda * A1
                  end if
                  if (k >= QuadStart .and. k <= QuadEnd .and. .not. RequiresT2Lambda) NVecsT2 = 0
                  ! -----------------------------------------------------
                  !               T1 amplitudes at Lambda
                  ! -----------------------------------------------------
                  call clock_start(timer)
                  call rpa_CC_Singles_ringCCSD(T1, hHF_vo, Lambda, NOcc(s), NVirt(s), &
                        SemiOccCoeffs_mo(:, :, s), SemiVirtCoeffs_mo(:, :, s), &
                        SemiFii(:, s), SemiFaa(:, s), V, A, NVecsT2, Rkai(:, :, s), &
                        NVecsPiU, T1Approx, ChiOrbitals, &
                        OccCoeffs_ao(:, :, s), VirtCoeffs_ao(:, :, s), &
                        SemiOccCoeffs_ao(:, :, s), SemiVirtCoeffs_ao(:, :, s), &
                        Rkpq, ShellPairs, ShellPairLoc, ShellPairDim, &
                        ShellLoc, ShellParamsIdx, SubsetBounds, SubsetDim, NSubsets, NAngFunc, NAO)
                  t_T1 = t_T1 + clock_readwall(timer)
                  ! ----------------------------------------------------
                  !         Correlation contribution to 1-RDM
                  ! ----------------------------------------------------
                  if (ChiOrbitals == RPA_ORBITALS_SEMICANONICAL) then
                        !
                        ! Compute density matrices in the semicanonical basis
                        !
                        call rpa_CC_DeltaRho(Rho_semi_vo, Rho_semi_oo, Rho_semi_vv, &
                              T1, A, V, DensityApprox, NOcc(s), NVirt(s), NVecsT2)
                        !
                        ! Transform semicanonical density matrices to the molecular orbital basis.
                        ! The MO basis is common to all Lambda's whereas the semicanonical basis depends on Lambda.
                        !
                        call rpa_CC_TransfRho(Rho_mo_vo(:, :, k), Rho_mo_oo(:, :, k), Rho_mo_vv(:, :, k), &
                              Rho_semi_vo, Rho_semi_oo, Rho_semi_vv, SemiOccCoeffs_mo(:, :, s), SemiVirtCoeffs_mo(:, :, s), NOcc(s), NVirt(s))
                  else
                        call rpa_CC_DeltaRho(Rho_mo_vo(:, :, k), Rho_mo_oo(:, :, k), Rho_mo_vv(:, :, k), &
                              T1, A, V, DensityApprox, NOcc(s), NVirt(s), NVecsT2)
                  end if
                  if (k <= QuadEnd .and. k >= QuadStart) then
                        !
                        ! Numerically integrated correlation
                        !
                        call rpa_CC_VcLambdaPiU(VcLambdaPiU(k), PiUEigenvals, NVecsPiU, Lambda, FreqWeights, NFreqs)
                        if (RequiresT2Lambda) then
                              call rpa_CC_VcLambdaT2(VcLambdaT2(k), A, V, NVecsT2, NOcc(s), NVirt(s), Rkai(:, :, s), NVecsPiU)
                        end if
                  end if
                  ! -------------------------------------------------------
                  !                 Exchange contribution
                  ! -------------------------------------------------------
                  call clock_start(timer)
                  select case (ExchangeApprox)
                  case (RPA_EXCHANGE_CUMULANT_LINEAR)
                        if (k <= QuadEnd .and. k >= QuadStart) then
                              UseOperatorS = .true.
                              Alpha = TWO
                              call rpa_CC_Exchange_SOSEX(VxLambda(k), Alpha, V, A, Rkai, NOcc(s), NVirt(s), NVecsPiU, NVecsT2, UseOperatorS)
                        end if
                  case (RPA_EXCHANGE_SOSEX)
                        if (k == Lambda1) then
                              UseOperatorS = .false.
                              Alpha = ONE
                              call rpa_CC_Exchange_SOSEX(EcExchange_SOSEX, Alpha, V, A, Rkai, NOcc(s), NVirt(s), NVecsPiU, NVecsT2, UseOperatorS)
                              EcExchange = EcExchange_SOSEX
                        end if
                  case (RPA_EXCHANGE_MBPT3_1_NUMERICAL)
                        if (k <= DerivEnd .and. k >= DerivStart) then
                              UseOperatorS = .false.
                              Alpha = ONE
                              call rpa_CC_Exchange_SOSEX(VxLambda(k), Alpha, V, A, Rkai, NOcc(s), NVirt(s), NVecsPiU, NVecsT2, UseOperatorS)
                        end if
                  case (RPA_EXCHANGE_MBPT3_1)
                        if (k == Lambda1) then
                              if (ChiOrbitals == RPA_ORBITALS_CANONICAL) then
                                    call rpa_CC_Exchange_MBPT3_1(EcExchange_MBPT3_1, V, A, NVecsT2, Rkai(:, :, s), NVecsPiU, &
                                          OccEnergies(:, s), VirtEnergies(:, s), NOcc(s), NVirt(s))
                              else
                                    call rpa_CC_Exchange_MBPT3_1(EcExchange_MBPT3_1, V, A, NVecsT2, Rkai(:, :, s), NVecsPiU, &
                                          SemiFii(:, s), SemiFaa(:, s), NOcc(s), NVirt(s))
                              end if
                              EcExchange = EcExchange_MBPT3_1
                        end if
                  case (RPA_EXCHANGE_MBPT3_2)
                        if (k == Lambda1) then
                              if (ChiOrbitals == RPA_ORBITALS_CANONICAL) then
                                    call rpa_CC_Exchange_MBPT3_1(EcExchange_MBPT3_1, V, A, NVecsT2, Rkai(:, :, s), NVecsPiU, &
                                          OccEnergies(:, s), VirtEnergies(:, s), NOcc(s), NVirt(s))
                              else
                                    call rpa_CC_Exchange_MBPT3_1(EcExchange_MBPT3_1, V, A, NVecsT2, Rkai(:, :, s), NVecsPiU, &
                                          SemiFii(:, s), SemiFaa(:, s), NOcc(s), NVirt(s))
                              end if
                              call rpa_CC_Exchange_MBPT3_2(EcExchange_MBPT3_2, Rkai(:, :, s), V, A, NOcc(s), NVirt(s), NVecsPiU, NVecsT2)
                              EcExchange = EcExchange_MBPT3_1 + EcExchange_MBPT3_2
                        end if
                  end select
                  t_exchange = t_exchange + clock_readwall(timer)
                  if (k == 1) then
                        t_iter = clock_readwall(timer_total)
                  else
                        t_iter = clock_readwall(timer_iter)
                  end if
                  call msg(lfield(str(k), 3) // lfield(str(Lambda,d=1), 10) // lfield(str(ACWeights(k),d=1),20) &
                        // lfield(str(NVecsT2), 10) // lfield(str(t_iter, d=1), 15))
            end do
            ! -------------------------------------------------------------------------
            !           Numerically integrated RPA correlation contribution
            ! -------------------------------------------------------------------------
            EcRPANumPiU = ZERO
            EcRPANumT2 = ZERO
            if (QuadStart > 0) then
                  call rpa_CC_EcRPA_Numerical(EcRPANumPiU, VcLambdaPiU(QuadStart:QuadEnd), ACWeights(QuadStart:QuadEnd), QuadEnd-QuadStart+1)
                  call rpa_CC_EcRPA_Numerical(EcRPANumT2, VcLambdaT2(QuadStart:QuadEnd), ACWeights(QuadStart:QuadEnd), QuadEnd-QuadStart+1)
                  if (MeanField == RPA_MEAN_FIELD_KS_TYPE .and. ChiOrbitals == RPA_ORBITALS_SEMICANONICAL) then
                        EcRPA_Semicanonical = EcRPANumPiU
                  end if
            end if
            ! -------------------------------------------------------------------------
            !  Exchange contribution: integration over Lambda
            ! -------------------------------------------------------------------------
            if (ExchangeApprox == RPA_EXCHANGE_CUMULANT_LINEAR) then
                  EcExchange_Cumulant = ZERO
                  do k = QuadStart, QuadEnd
                        EcExchange_Cumulant = EcExchange_Cumulant + ACWeights(k) * VxLambda(k)
                  end do
                  EcExchange = EcExchange_Cumulant
            else if (ExchangeApprox == RPA_EXCHANGE_MBPT3_1_NUMERICAL) then
                  EcExchange_MBPT3_1 = ZERO
                  do k = DerivStart, DerivEnd
                        EcExchange_MBPT3_1 = EcExchange_MBPT3_1 + ACWeights(k) * VxLambda(k)
                  end do
                  EcExchange = EcExchange_MBPT3_1
            end if
            ! -------------------------------------------------------------------------
            ! 1-RDM contribution to the energy: integration over Lambda
            ! -------------------------------------------------------------------------
            Ec1RDM_Linear = ZERO
            Ec1RDM_Quadratic = ZERO
            if (QuadStart > 0) then
                  if (Purify1RDM /= RPA_PURIFY_RHO_NONE) then
                        call rpa_CC_Enforce_NRepresentability(Rho_mo_vo(:, :, QuadStart:QuadEnd), &
                              Rho_mo_oo(:, :, QuadStart:QuadEnd), Rho_mo_vv(:, :, QuadStart:QuadEnd), &
                              NOcc(s), NVirt(s), QuadEnd-QuadStart+1, Purify1RDM)
                  end if
                  !
                  ! Contribution to Ec1RDM linear in Rho(Lambda)-Rho(0)
                  ! The linear contribution is always enabled
                  !
                  call rpa_CC_Ec1RDM_Linear(Ec1RDM_Linear, Rho_mo_vo(:, :, QuadStart:QuadEnd), &
                        Rho_mo_oo(:, :, QuadStart:QuadEnd), Rho_mo_vv(:, :, QuadStart:QuadEnd), &
                        hHF_vo, hHF_oo, hHF_vv, OccEnergies(:, s), VirtEnergies(:, s), &
                        NOcc(s), NVirt(s), ACWeights(QuadStart:QuadEnd), &
                        QuadEnd-QuadStart+1, MeanField)
                  !
                  ! Contribution to Ec1RDM quadratic in Rho(Lambda)-Rho(0)
                  !
                  if (Ec1RDMApprox == RPA_Ec1RDM_QUADRATIC) then
                        call clock_start(timer)
                        call rpa_CC_Ec1RDM_Quadratic(Ec1RDM_Quadratic, Rho_mo_vo(:, :, QuadStart:QuadEnd), &
                              Rho_mo_oo(:, :, QuadStart:QuadEnd), Rho_mo_vv(:, :, QuadStart:QuadEnd), &
                              OccCoeffs_ao(:, :, s), VirtCoeffs_ao(:, :, s), NOcc(s), NVirt(s), NAO, &
                              ACWeights(QuadStart:QuadEnd), QuadEnd-QuadStart+1, Rkpq, NVecsPiU, ShellPairs, &
                              ShellPairLoc, ShellPairDim, SubsetBounds, SubsetDim, &
                              NSubsets, AOBasis, ThreshFockJK, MeanFieldApprox)                        
                        t_Ec1RDMQuad = t_Ec1RDMQuad + clock_readwall(timer)
                  end if
            end if
            !
            ! Energy components for further processing on higher levels of the program.
            ! Those are used, e.g., for calculating interaction energies and nonadditive
            ! interaction energies.
            !
            Energy(RPA_ENERGY_1RDM_LINEAR) = Ec1RDM_Linear
            Energy(RPA_ENERGY_1RDM_QUADRATIC) = Ec1RDM_Quadratic
            Energy(RPA_ENERGY_SINGLES) = Energy(RPA_ENERGY_1RDM_LINEAR) + Energy(RPA_ENERGY_1RDM_QUADRATIC)            
            Energy(RPA_ENERGY_CORR_NUMERICAL_QUAD_PiU) = EcRPANumPiU
            Energy(RPA_ENERGY_CORR_NUMERICAL_QUAD_T2) = EcRPANumT2
            Energy(RPA_ENERGY_CORR_CAN_DIRECT_RPA) = EcRPA_Canonical
            Energy(RPA_ENERGY_CORR_SEMI_DIRECT_RPA) = EcRPA_Semicanonical
            if (ChiOrbitals == RPA_ORBITALS_CANONICAL) then
                  Energy(RPA_ENERGY_DIRECT_RING) = EcRPA_Canonical
            else
                  Energy(RPA_ENERGY_DIRECT_RING) = EcRPA_Semicanonical
            end if            
            Energy(RPA_ENERGY_EXCH_MBPT3_1) = EcExchange_MBPT3_1
            Energy(RPA_ENERGY_EXCH_MBPT3_2) = EcExchange_MBPT3_2
            Energy(RPA_ENERGY_EXCH_SOSEX) = EcExchange_SOSEX
            Energy(RPA_ENERGY_EXCH_CUMULANT) = EcExchange_Cumulant
            Energy(RPA_ENERGY_EXCHANGE) = EcExchange
            Energy(RPA_ENERGY_CORR) = Energy(RPA_ENERGY_DIRECT_RING) + Energy(RPA_ENERGY_EXCHANGE)
            call blankline()
            call msg(lfield("Total time for AC integral", 50) // str(clock_readwall(timer_total),d=1))
            call msg(lfield("T2 amplitudes", 50) // str(t_T2,d=1))
            call msg(lfield("T1 amplitudes", 50) // str(t_T1,d=1))
            call msg(lfield("Exchange contribution", 50) // str(t_exchange,d=1))
            call msg(lfield("Pi(u)+Rkpq->Rkai transform", 50) // str(t_PiU,d=1))
            call msg(lfield("Diagonalization of Pi(u)", 50) // str(t_PiUDiag,d=1))
            call msg(lfield("Ec1RDM_Quadratic", 50) // str(t_Ec1RDMQuad,d=1))
            call blankline()
      end subroutine rpa_CC_Energy_2


      subroutine rpa_CC_Diagonalize_PiU(PiUEigenvals, PiUEigenvecs, t_PiUDiag, NVecsPiU, NFreqs)
            real(F64), dimension(:, :), intent(out)      :: PiUEigenvals
            real(F64), dimension(:, :, :), intent(inout) :: PiUEigenvecs
            real(F64), intent(inout)                     :: t_PiUDiag
            integer, intent(in)                          :: NVecsPiU
            integer, intent(in)                          :: NFreqs

            integer :: u
            integer :: ThisImage
            type(TClock) :: timer

            ThisImage = this_image()
            call clock_start(timer)
            if (ThisImage == 1) then
                  do u = 1, NFreqs
                        call symmetric_eigenproblem(PiUEigenvals(:, u), PiUEigenvecs(:, :, u), NVecsPiU, .true.)
                  end do
            end if
            call co_broadcast(PiUEigenvecs, source_image=1)
            call co_broadcast(PiUEIgenvals, source_image=1)
            t_PiUDiag = t_PiUDiag + clock_readwall(timer)
      end subroutine rpa_CC_Diagonalize_PiU


      subroutine rpa_CC_Ec1RDM_Ref2_v2(Ec1RDM_Ref2_Linear, Ec1RDM_Ref2_Quadratic, &
            hHF_mo_vo, hHF_mo_oo, hHF_mo_vv, DeltaRho_mo_vo, DeltaRho_mo_oo, &
            DeltaRho_mo_vv, OccCoeffs_ao, VirtCoeffs_ao, &
            Rkpq, NVecsPiU, ShellPairs, ShellPairLoc, ShellPairDim, &
            SubsetBounds, SubsetDim, NSubsets, &
            AOBasis, ThreshFockJK, MeanFieldApprox)

            real(F64), intent(out)                    :: Ec1RDM_Ref2_Linear
            real(F64), intent(out)                    :: Ec1RDM_Ref2_Quadratic
            real(F64), dimension(:, :), intent(in)    :: hHF_mo_vo
            real(F64), dimension(:, :), intent(in)    :: hHF_mo_oo
            real(F64), dimension(:, :), intent(in)    :: hHF_mo_vv
            real(F64), dimension(:, :), intent(in)    :: DeltaRho_mo_vo
            real(F64), dimension(:, :), intent(in)    :: DeltaRho_mo_oo
            real(F64), dimension(:, :), intent(in)    :: DeltaRho_mo_vv
            real(F64), dimension(:, :), intent(in)    :: OccCoeffs_ao
            real(F64), dimension(:, :), intent(in)    :: VirtCoeffs_ao
            real(F64), dimension(:, :, :), intent(in) :: Rkpq[*]
            integer, intent(in)                       :: NVecsPiU
            integer, dimension(:, :), intent(in)      :: ShellPairs
            integer, dimension(:, :), intent(in)      :: ShellPairLoc
            integer, dimension(:), intent(in)         :: ShellPairDim
            integer, dimension(:, :), intent(in)      :: SubsetBounds
            integer, dimension(:), intent(in)         :: SubsetDim
            integer, dimension(2), intent(in)         :: NSubsets
            type(TAOBasis), intent(in)                :: AOBasis
            real(F64), intent(in)                     :: ThreshFockJK
            integer, intent(in)                       :: MeanFieldApprox

            real(F64), dimension(:, :, :), allocatable :: Ref2DeltaRho_mo_vo
            real(F64), dimension(:, :, :), allocatable :: Ref2DeltaRho_mo_oo
            real(F64), dimension(:, :, :), allocatable :: Ref2DeltaRho_mo_vv
            integer, parameter :: NACPoints = 1
            real(F64), dimension(1), parameter :: ACWeights = [ONE]
            integer :: NOcc, NVirt, NAO
            integer :: a, b, i, j

            NOcc = size(DeltaRho_mo_vo, dim=2)
            NVirt = size(DeltaRho_mo_vo, dim=1)
            NAO = size(OccCoeffs_ao, dim=1)

            allocate(Ref2DeltaRho_mo_vo(NVirt, NOcc, 1))
            allocate(Ref2DeltaRho_mo_oo(NOcc, NOcc, 1))
            allocate(Ref2DeltaRho_mo_vv(NVirt, NVirt, 1))

            Ref2DeltaRho_mo_vo(:, :, 1) = DeltaRho_mo_vo
            Ref2DeltaRho_mo_oo(:, :, 1) = DeltaRho_mo_oo
            Ref2DeltaRho_mo_vv(:, :, 1) = DeltaRho_mo_vv
            call rpa_CC_Enforce_NRepresentability(Ref2DeltaRho_mo_vo, Ref2DeltaRho_mo_oo, Ref2DeltaRho_mo_vv, &
                  NOcc, NVirt, 1, RPA_PURIFY_RHO_KLIMES2015)
            !
            ! Linear contribution
            !
            Ec1RDM_Ref2_Linear = ZERO
            do i = 1, NOcc
                  do a = 1, NVirt
                        Ec1RDM_Ref2_Linear = Ec1RDM_Ref2_Linear + hHF_mo_vo(a, i) * Ref2DeltaRho_mo_vo(a, i, 1)
                  end do
            end do
            Ec1RDM_Ref2_Linear = TWO * Ec1RDM_Ref2_Linear
            do j = 1, NOcc
                  do i = 1, NOcc
                        Ec1RDM_Ref2_Linear = Ec1RDM_Ref2_Linear + hHF_mo_oo(i, j) * Ref2DeltaRho_mo_oo(i, j, 1) 
                  end do
            end do
            do b = 1, NVirt
                  do a = 1, NVirt
                        Ec1RDM_Ref2_Linear = Ec1RDM_Ref2_Linear + hHF_mo_vv(a, b) * Ref2DeltaRho_mo_vv(a, b, 1)
                  end do
            end do
            !
            ! Quadratic contribution
            !
            call rpa_CC_Ec1RDM_Quadratic(Ec1RDM_Ref2_Quadratic, Ref2DeltaRho_mo_vo, Ref2DeltaRho_mo_oo, &
                  Ref2DeltaRho_mo_vv, OccCoeffs_ao, VirtCoeffs_ao, NOcc, NVirt, NAO, ACWeights, NACPoints, &
                  Rkpq, NVecsPiU, ShellPairs, ShellPairLoc, ShellPairDim, &
                  SubsetBounds, SubsetDim, NSubsets,AOBasis, ThreshFockJK, MeanFieldApprox)            
      end subroutine rpa_CC_Ec1RDM_Ref2_v2
end module rpa_CC
