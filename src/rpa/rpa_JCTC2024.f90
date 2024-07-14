module rpa_JCTC2024
      use arithmetic
      use math_constants
      use rpa_definitions
      use real_linalg
      use basis_sets
      use rpa_Orbitals
      use clock

      implicit none

      type TPNOTransform
            real(F64), dimension(:, :, :), allocatable :: TaxPNO
      end type TPNOTransform

      integer, parameter :: PNO_PAIR_INDEX      = 1
      integer, parameter :: PNO_LEFT_TRANSFORM  = 2
      integer, parameter :: PNO_RIGHT_TRANSFORM = 3
      integer, parameter :: PNO_NVIRT           = 4
      
contains

      subroutine rpa_JCTC2024_Corrections(RPAOutput, Zgk, Xgi, Yga, Uaim, Am, Cpi, &
            RPAParams, AOBasis)
            
            type(TRPAOutput), intent(inout)                        :: RPAOutput
            real(F64), dimension(:, :), intent(in)                 :: Zgk
            real(F64), dimension(:, :), intent(in)                 :: Xgi
            real(F64), dimension(:, :), intent(in)                 :: Yga
            real(F64), dimension(:, :, :), intent(in)              :: Uaim
            real(F64), dimension(:), intent(in)                    :: Am
            real(F64), dimension(:, :), intent(in)                 :: Cpi
            type(TRPAParams), intent(in)                           :: RPAParams
            type(TAOBasis), intent(in)                             :: AOBasis

            real(F64), dimension(:, :), allocatable :: Tabij
            real(F64), dimension(:, :), allocatable :: Pam, Qam
            real(F64), dimension(:, :, :), allocatable :: UaimLoc
            real(F64), dimension(:, :), allocatable :: XgiLoc, Lik
            integer :: i, j, mu, x
            real(F64) :: EcRPA, Ec1b, Ec2bcd, Ec2ghij
            integer :: NVecsT2, NCholesky, NGridTHC, NOcc, NVirt
            integer :: MaxNVirtPNO, NVirtPNO
            integer :: SumNVirtPNO, AvNVirtPNO, ZeroNVirtPNO
            integer :: NOccPairs
            type(TPNOTransform), dimension(:), allocatable :: PNOTransform
            real(F64), dimension(:), allocatable :: Sigma
            real(F64), dimension(:, :), allocatable :: U, V
            integer, dimension(:, :, :), allocatable :: PNOData
            integer :: IJ
            type(TClock) :: timer_Total, timer_SVD, timer_Energy, timer_LO
            real(F64) :: t_Total, t_SVD, t_Energy, t_LO
            real(F64) :: t_SOSEX, t_G, t_TrVGabij, t_TrVGaibj, t_ZYX, t_ZXX

            call clock_start(timer_Total)
            call blankline()
            call midrule()
            call msg(cfield("Particle-Hole Corrections to Direct RPA", 76))
            call midrule()
            call blankline()
            call msg("Energy terms: EcSOSEX, Ec2b, Ec2c, Ec2d, Ec2g, Ec2h, Ec2i, Ec2j")
            call msg("Definitions: Table 2 of Ref. 1")
            call blankline()
            call msg("1. D. Cieśliński, A. M. Tucholska, and M. Modrzejewski")
            call msg("   J. Chem. Theory Comput. 19, 6619 (2023);")
            call msg("   doi: 10.1021/acs.jctc.3c00496")
            
            NGridTHC = size(Zgk, dim=1)
            NCholesky = size(Zgk, dim=2)
            NVecsT2 = size(Am)
            NOcc = size(Xgi, dim=2)
            NVirt = size(Yga, dim=2)            
            allocate(Tabij(NVirt, NVirt))
            allocate(Pam(NVirt, NVecsT2))
            allocate(Qam(NVirt, NVecsT2))
            !
            ! Transformation to the localized occupied orbital
            ! basis
            !
            call clock_start(timer_LO)
            allocate(UaimLoc(NVirt, NOcc, NVecsT2))
            allocate(XgiLoc(NGridTHC, NOcc))
            allocate(Lik(NOcc, NOcc))
            call rpa_LocalizeOrbitals_AquilanteJCP2006(Lik, Cpi, NOcc, RPAParams, AOBasis)
            call real_ab(XgiLoc, Xgi, Lik)
            !$omp parallel do private(mu)
            do mu = 1, NVecsT2
                  call real_ab(UaimLoc(:, :, mu), Uaim(:, :, mu), Lik)
            end do
            !$omp end parallel do
            t_LO = clock_readwall(timer_LO)
            !
            ! Find the number of pair-natural orbitals corresponding
            ! to CutoffThreshPNO by decomposing diagonal amplitude
            ! matrices T(ai,bi)
            !
            call clock_start(timer_SVD)
            allocate(Sigma(NVirt))
            allocate(U(NVirt, NVirt))
            allocate(V(NVirt, NVirt))
            !
            ! Singular value decomposition of T(aI,bJ)
            !
            NOccPairs = (NOcc * (NOcc + 1)) / 2
            allocate(PNOTransform(NOccPairs))
            allocate(PNOData(4, NOcc, NOcc))
            PNOData = 0
            IJ = 0
            MaxNVirtPNO = 0
            ZeroNVirtPNO = 0
            SumNVirtPNO = 0
            do j = 1, NOcc
                  do i = j, NOcc
                        call rpa_JCTC2024_Tabij(Tabij, Pam, Qam, UaimLoc, Am, i, j, &
                              NOcc, NVirt, NVecsT2)                        
                        call real_SVD(U, V, Sigma, Tabij)
                        NVirtPNO = 0
                        do x = 1, NVirt
                              if (Sigma(x) >= RPAParams%CutoffThreshPNO) then
                                    NVirtPNO = x
                              else
                                    exit
                              end if
                        end do
                        if (NVirtPNO > 0) then
                              IJ = IJ + 1
                              allocate(PNOTransform(IJ)%TaxPNO(NVirt, NVirtPNO, 2))
                              associate (TaxPNO => PNOTransform(IJ)%TaxPNO)
                                    TaxPNO(:, :, 1) = U(:, 1:NVirtPNO)
                                    do x = 1, NVirtPNO
                                          TaxPNO(:, x, 2) = Sigma(x) * V(:, x)
                                    end do
                              end associate
                              PNOData(PNO_PAIR_INDEX, I, J) = IJ
                              PNOData(PNO_LEFT_TRANSFORM, I, J) = 1
                              PNOData(PNO_RIGHT_TRANSFORM, I, J) = 2
                              PNOData(PNO_NVIRT, I, J) = NVirtPNO
                              PNOData(PNO_PAIR_INDEX, J, I) = IJ
                              PNOData(PNO_LEFT_TRANSFORM, J, I) = 2
                              PNOData(PNO_RIGHT_TRANSFORM, J, I) = 1
                              PNOData(PNO_NVIRT, J, I) = NVirtPNO
                        else
                              ZeroNVirtPNO = ZeroNVirtPNO + 1
                              PNOData(PNO_PAIR_INDEX, I, J) = -1
                              PNOData(PNO_PAIR_INDEX, J, I) = -1
                              PNOData(PNO_NVIRT, I, J) = 0
                              PNOData(PNO_NVIRT, J, I) = 0
                        end if
                        MaxNVirtPNO = max(MaxNVirtPNO, NVirtPNO)
                        SumNVirtPNO = SumNVirtPNO + NVirtPNO
                  end do
            end do
            AvNVirtPNO = nint(real(SumNVirtPNO,F64)/NOccPairs)
            t_SVD = clock_readwall(timer_SVD)
            call blankline()
            call msg("Pair-natural orbitals cutoff " // str(RPAParams%CutoffThreshPNO,d=1))
            call msg("AverageNVirtPNO              " // str(AvNVirtPNO))
            call msg("Max NVirtPNO                 " // str(MaxNVirtPNO))
            call msg("Pairs with zero NVirtPNO     " // str(ZeroNVirtPNO) // " out of " // str(NOccPairs))
            call msg("NVirt                        " // str(NVirt))
            call msg("AverageNVirtPNO/NVirt        " // str(real(AvNVirtPNO,F64)/NVirt,d=1))
            !
            ! Ec1b + Ec2b + Ec2c + Ec2d
            ! Ec2g + Ec2h + Ec2i + Ec2j
            !
            call clock_start(timer_Energy)
            call rpa_JCTC2024_Gaibj_Gabij_v2(EcRPA, Ec1b, Ec2bcd, Ec2ghij, &
                  PNOData, PNOTransform, XgiLoc, Yga, Zgk, MaxNVirtPNO, NVirt, NOcc, &
                  NGridTHC, NCholesky, t_SOSEX, t_G, t_TrVGaibj, t_TrVGabij, t_ZYX, t_ZXX)            
            t_Energy = clock_readwall(timer_Energy)
            t_Total = clock_readwall(timer_Total)
            !
            ! PNO RPA energy used only to estimate the error
            ! due to the use of PNOs
            !
            RPAOutput%Energy(RPA_ENERGY_PNO_DIRECT_RING) = EcRPA
            !
            ! Beyond-RPA corrections
            !
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_SOSEX) = (ONE/TWO) * Ec1b
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_PH3) = Ec2bcd + Ec2ghij ! 1/2(Ec2b + Ec2c) + Ec2d + Ec2g+Ec2h+Ec2i+Ec2j
            call blankline()
            call msg("Calculation of particle-hole corrections completed")
            call msg("Timings in seconds:")
            call msg("Total time           " // str(t_Total,d=1))
            call msg("Orbital localization " // str(t_LO,d=1))
            call msg("SVD                  " // str(t_SVD,d=1))
            call msg("SOSEX                " // str(t_SOSEX,d=1))
            call msg("Gabij+Gaibj          " // str(t_G,d=1))
            call msg("Tr(VGabij)           " // str(t_TrVGabij,d=1))
            call msg("Tr(VGaibj)           " // str(t_TrVGaibj,d=1))
            call msg("ZXX                  " // str(t_ZXX,d=1))
            call msg("ZYX                  " // str(t_ZYX,d=1))
            call msg("Energy terms         " // str(t_Energy,d=1))
            call blankline()
      end subroutine rpa_JCTC2024_Corrections


      subroutine rpa_JCTC2024_PackTransfMatrices(PQaxki, PQaxkiLoc, PQAxkiNum, i, &
            PNOData, PNOTransform, NOcc, NVirt)
            
            real(F64), dimension(:, :), intent(out)                         :: PQaxki
            integer, dimension(:), intent(out)                              :: PQaxkiLoc
            integer, dimension(:), intent(out)                              :: PQaxkiNum
            integer, intent(in)                                             :: i
            integer, dimension(:, :, :), intent(in)                         :: PNOData
            type(TPNOTransform), dimension(:), intent(in)                   :: PNOTransform
            integer, intent(in)                                             :: NOcc
            integer, intent(in)                                             :: NVirt

            integer :: ki0, ki1, ki, k
            integer :: Nx

            PQaxkiLoc = -1
            PQaxkiNum = 0
            ki0 = 1
            do k = 1, NOcc
                  Nx = PNOData(PNO_NVIRT, k, i)
                  if (Nx > 0) then
                        ki1 = ki0 + 2 * Nx - 1
                        ki  = PNOData(PNO_PAIR_INDEX, k, i)
                        call store_PQxaki(PQaxki(:, ki0:ki1), PNOTransform(ki)%TaxPNO, NVirt, Nx)
                        PQaxkiLoc(k) = ki0
                        PQaxkiNum(k) = Nx
                        ki0 = ki0 + 2 * Nx
                  end if
            end do
            
      contains

            subroutine store_PQxaki(PQaxki, Tax, NVirt, Nx)
                  integer, intent(in)                            :: NVirt, Nx
                  real(F64), dimension(NVirt, 2*Nx), intent(out) :: PQaxki
                  real(F64), dimension(NVirt, 2*Nx), intent(in)  :: Tax

                  PQaxki(:, :) = Tax(:, :)
            end subroutine store_PQxaki
      end subroutine rpa_JCTC2024_PackTransfMatrices


      subroutine rpa_JCTC2024_Gaibj_Gabij(EcRPA, Ec1b, Ec2bcd, Ec2ghij, &
            PNOData, PNOTransform, Xgi, Yga, Zgk, MaxNVirtPNO, NVirt, NOcc, &
            NGridTHC, NCholesky)

            integer, intent(in)                                             :: MaxNVirtPNO, NVirt, NOcc
            integer, intent(in)                                             :: NGridTHC, NCholesky
            real(F64), intent(out)                                          :: EcRPA, Ec1b
            real(F64), intent(out)                                          :: Ec2bcd, Ec2ghij
            integer, dimension(:, :, :), intent(in)                         :: PNOData
            type(TPNOTransform), dimension(:), intent(in)                   :: PNOTransform
            real(F64), dimension(NGridTHC, NOcc), intent(in)                :: Xgi
            real(F64), dimension(NGridTHC, NVirt), intent(in)               :: Yga
            real(F64), dimension(NGridTHC, NCholesky), intent(in)           :: Zgk
            
            real(F64), dimension(:, :), allocatable :: ZYXkai, ZYXkbj, ZYXkyi, ZYXkyj
            real(F64), dimension(:), allocatable :: Wg
            real(F64), dimension(:), allocatable :: XXgij, ZXXkij, ZXXgij
            integer :: SumNVirtPNOkj
            integer :: a, i, j, k
            real(F64), dimension(:, :), allocatable :: PQax_kj, Ygx_kj
            real(F64), dimension(:, :), allocatable :: PQax_ki
            real(F64), dimension(:, :), allocatable :: ZYXkx_kj
            integer, dimension(:), allocatable :: PQaxkjLoc, PQaxkjNum
            integer, dimension(:), allocatable :: PQaxkiLoc, PQaxkiNum
            real(F64), dimension(:, :), allocatable :: S_jk_ki, S_jk_ik, S_kj_ki, S_kj_ik
            real(F64), dimension(:, :), allocatable :: QS_jk_ki, QS_jk_ik, QS_kj_ki, QS_kj_ik, QS
            real(F64), dimension(:, :), allocatable :: YgQS, ZYXkQS
            integer :: Nkj, Nki, kj0, kj1, ki0, ki1
            real(F64) :: Weight, S1b, S1a

            allocate(ZYXkbj(NCholesky, NVirt))
            allocate(ZYXkyi(NCholesky, MaxNVirtPNO))
            allocate(ZYXkyj(NCholesky, MaxNVirtPNO))
            allocate(PQax_kj(NVirt, 2*MaxNVirtPNO*NOcc))
            allocate(PQax_ki(NVirt, 2*MaxNVirtPNO*NOcc))
            allocate(Ygx_kj(NGridTHC, 2*MaxNVirtPNO*NOcc))
            allocate(PQaxkjLoc(NOcc))
            allocate(PQaxkjNum(NOcc))
            allocate(PQaxkiLoc(NOcc))
            allocate(PQaxkiNum(NOcc))
            allocate(ZYXkai(NCholesky, NVirt))
            allocate(ZYXkx_kj(NCholesky, 2*MaxNVirtPNO*NOcc))
            allocate(XXgij(NGridTHC))            
            allocate(ZXXgij(NGridTHC))
            allocate(ZXXkij(NCholesky))
            allocate(Wg(NGridTHC))
            allocate(YgQS(NGridTHC, 2*MaxNVirtPNO))
            allocate(ZYXkQS(NCholesky, 2*MaxNVirtPNO))
            allocate(S_jk_ki(MaxNVirtPNO, MaxNVirtPNO))
            allocate(S_jk_ik(MaxNVirtPNO, MaxNVirtPNO))
            allocate(S_kj_ki(MaxNVirtPNO, MaxNVirtPNO))
            allocate(S_kj_ik(MaxNVirtPNO, MaxNVirtPNO))
            allocate(QS_jk_ki(NVirt, MaxNVirtPNO))
            allocate(QS_jk_ik(NVirt, MaxNVirtPNO))
            allocate(QS_kj_ki(NVirt, MaxNVirtPNO))
            allocate(QS_kj_ik(NVirt, MaxNVirtPNO))
            allocate(QS(NVirt, 2*MaxNVirtPNO))

            EcRPA = ZERO
            Ec1b = ZERO
            Ec2ghij = ZERO
            Ec2bcd = ZERO
            do j = 1, NOcc
                  
                  call rpa_JCTC2024_PackTransfMatrices(PQax_kj, PQaxkjLoc, PQAxkjNum, j, &
                        PNOData, PNOTransform, NOcc, NVirt)
                  SumNVirtPNOkj = sum(PQaxkjNum)
                  call real_ab(Ygx_kj(:, 1:2*SumNVirtPNOkj), Yga, PQax_kj(:, 1:2*SumNVirtPNOkj))
                  do a = 1, NVirt
                        Wg = Yga(:, a) * Xgi(:, j)
                        call real_Atv(ZYXkbj(:, a), Zgk, Wg)
                  end do
                  call real_ab(ZYXkx_kj(:, 1:2*SumNVirtPNOkj), ZYXkbj, PQax_kj(:, 1:2*SumNVirtPNOkj))
                  
                  do i = j, NOcc
                        if (i /= j) then
                              Weight = TWO
                              call rpa_JCTC2024_PackTransfMatrices(PQax_ki, PQaxkiLoc, PQaxkiNum, i, &
                                    PNOData, PNOTransform, NOcc, NVirt)
                        else
                              PQax_ki = PQax_kj
                              PQaxkiLoc = PQaxkjLoc
                              PQaxkiNum = PQaxkjNum
                              Weight = ONE
                        end if
                        !
                        ! Intermediates for computing transformed
                        ! two-electron integrals
                        !
                        XXgij(:) = Xgi(:, i) * Xgi(:, j)
                        call real_ATv(ZXXkij, Zgk, XXgij)
                        call real_Av(ZXXgij, Zgk, ZXXkij)
                        if (i /= j) then
                              do a = 1, NVirt
                                    Wg = Yga(:, a) * Xgi(:, i)
                                    call real_Atv(ZYXkai(:, a), Zgk, Wg)
                              end do
                        else
                              ZYXkai = ZYXkbj
                        end if
                        call Gaibj1b(S1b, PNOData(:, i, j), ZYXkai, ZYXkbj)
                        Ec1b = Ec1b - TWO * Weight * S1b
                        call Gaibj1b(S1a, PNOData(:, j, i), ZYXkai, ZYXkbj)
                        EcRPA = EcRPA + TWO * Weight * S1a
                        do k = 1, NOcc
                              Nkj = PQaxkjNum(k)
                              Nki = PQaxkiNum(k)
                              if (Nkj*Nki > 0) then
                                    kj0 = PQaxkjLoc(k)
                                    kj1 = PQaxkjLoc(k) + 2*Nkj - 1
                                    ki0 = PQaxkiLoc(k)
                                    ki1 = PQaxkiLoc(k) + 2*Nki - 1                              
                                    call rpa_JCTC2024_phRPA_jik(Ec2ghij, Ec2bcd, Wg, YgQS, ZYXkQS, &
                                          S_jk_ki, S_jk_ik, S_kj_ki, S_kj_ik, &
                                          QS_jk_ki, QS_jk_ik, QS_kj_ki, QS_kj_ik, QS, &
                                          j, i, k, &
                                          Ygx_kj(:, kj0:kj1), ZYXkx_kj(:, kj0:kj1), &
                                          PQax_kj(:, kj0:kj1), PQax_ki(:, ki0:ki1), &
                                          Yga, ZXXgij, ZYXkai, &
                                          Nkj, Nki, NGridTHC, NCholesky, NVirt)
                              end if
                        end do
                  end do
            end do

      contains

            subroutine Gaibj1b(D, Iy, ZYXkai, ZYXkbj)
                  real(F64), intent(out)                 :: D
                  integer, dimension(:), intent(in)      :: Iy
                  real(F64), dimension(:, :), intent(in) :: ZYXkai
                  real(F64), dimension(:, :), intent(in) :: ZYXkbj

                  integer :: Y, Yp, Yq, Ny

                  Ny = Iy(PNO_NVIRT)
                  if (Ny > 0) then
                        Y  = Iy(PNO_PAIR_INDEX)
                        Yp = Iy(PNO_LEFT_TRANSFORM)
                        Yq = Iy(PNO_RIGHT_TRANSFORM)
                        call TrVxiyj(D, ZYXkyi, ZYXkyj, &
                              PNOTransform(Y)%TaxPNO(:, :, Yp), PNOTransform(Y)%TaxPNO(:, :, Yq), &
                              ZYXkai, ZYXkbj, Ny)
                  else
                        D = ZERO
                  end if
            end subroutine Gaibj1b
            
            subroutine TrVxiyj(D, ZYXkyi, ZYXkyj, Py, Qy, ZYXkai, ZYXkbj, Ny)
                  integer, intent(in)                                :: Ny
                  real(F64), intent(out)                             :: D
                  real(F64), dimension(NCholesky, Ny), intent(out)   :: ZYXkyi
                  real(F64), dimension(NCholesky, Ny), intent(out)   :: ZYXkyj
                  real(F64), dimension(NVirt, Ny), intent(in)        :: Py
                  real(F64), dimension(NVirt, Ny), intent(in)        :: Qy
                  real(F64), dimension(NCholesky, NVirt), intent(in) :: ZYXkai
                  real(F64), dimension(NCholesky, NVirt), intent(in) :: ZYXkbj

                  call real_ab(ZYXkyi, ZYXkai, Qy)
                  call real_ab(ZYXkyj, ZYXkbj, Py)
                  call real_vw_x(D, ZYXkyi, ZYXkyj, NCholesky*Ny)
            end subroutine TrVxiyj
      end subroutine rpa_JCTC2024_Gaibj_Gabij


      subroutine rpa_JCTC2024_phRPA_jik(Ec2ghij, Ec2bcd, Wg, YgQS, ZYXkQS, &
            S_jk_ki, S_jk_ik, S_kj_ki, S_kj_ik, &
            QS_jk_ki, QS_jk_ik, QS_kj_ki, QS_kj_ik, QS, &
            j, i, k, Ygx_kj, ZYXkx_kj, PQ_kj, PQ_ki, Yga, ZXXgij, ZYXkai, &
            Nkj, Nki, NGridTHC, NCholesky, NVirt)

            integer, intent(in)                                 :: Nkj, Nki
            integer, intent(in)                                 :: NGridTHC, NCholesky, NVirt
            real(F64), intent(inout)                            :: Ec2ghij
            real(F64), intent(inout)                            :: Ec2bcd
            real(F64), dimension(NGridTHC), intent(out)         :: Wg
            real(F64), dimension(NGridTHC, Nkj, 2), intent(out) :: YgQS
            real(F64), dimension(NCholesky, Nkj, 2), intent(out):: ZYXkQS
            real(F64), dimension(Nkj, Nki), intent(out)         :: S_jk_ki, S_jk_ik, S_kj_ki, S_kj_ik
            real(F64), dimension(NVirt, Nkj), intent(out)       :: QS_jk_ki, QS_jk_ik, QS_kj_ki, QS_kj_ik
            real(F64), dimension(NVirt, Nkj, 2), intent(out)    :: QS
            integer, intent(in)                                 :: j, i, k
            real(F64), dimension(NGridTHC, Nkj, 2), intent(in)  :: Ygx_kj
            real(F64), dimension(NCholesky, Nkj, 2), intent(in) :: ZYXkx_kj
            real(F64), dimension(NVirt, Nkj, 2), intent(in)     :: PQ_kj
            real(F64), dimension(NVirt, Nki, 2), intent(in)     :: PQ_ki
            real(F64), dimension(NGridTHC, NVirt), intent(in)   :: Yga
            real(F64), dimension(NGridTHC), intent(in)          :: ZXXgij
            real(F64), dimension(NCholesky, NVirt), intent(in)  :: ZYXkai

            real(F64) :: S2ghij, S2bcd
            real(F64) :: c2g, c2h, c2i, c2j, c2d, c2b, c2c
            integer :: Pki, Qki, Pik, Qik
            integer :: Pkj, Qkj, Pjk, Qjk
            integer :: x

            if (k >= i) then
                  Pki = 1
                  Qki = 2
                  Pik = 2
                  Qik = 1
            else
                  Pki = 2
                  Qki = 1
                  Pik = 1
                  Qik = 2
            end if
            if (k >= j) then
                  Pkj = 1
                  Qkj = 2
                  Pjk = 2
                  Qjk = 1
            else
                  Pkj = 2
                  Qkj = 1
                  Pjk = 1
                  Qjk = 2
            end if

            c2b = -FOUR * (ONE/TWO)
            c2c = -FOUR * (ONE/TWO)
            c2d = TWO
            c2g = -FOUR
            c2h = -FOUR
            c2i = TWO
            c2j = TWO

            call real_aTb(S_jk_ki, PQ_kj(:, :, Qjk), PQ_ki(:, :, Pki))
            call real_aTb(S_jk_ik, PQ_kj(:, :, Qjk), PQ_ki(:, :, Pik))
            call real_aTb(S_kj_ki, PQ_kj(:, :, Qkj), PQ_ki(:, :, Pki))
            call real_aTb(S_kj_ik, PQ_kj(:, :, Qkj), PQ_ki(:, :, Pik))

            call real_abT(QS_jk_ki, PQ_ki(:, :, Qki), S_jk_ki)
            call real_abT(QS_jk_ik, PQ_ki(:, :, Qik), S_jk_ik)
            call real_abT(QS_kj_ki, PQ_ki(:, :, Qki), S_kj_ki)
            call real_abT(QS_kj_ik, PQ_ki(:, :, Qik), S_kj_ik)

            if (i /= j) then
                  QS(:, :, Pjk) = (TWO * c2g) * QS_jk_ki + (c2i+c2j) * QS_jk_ik  ! factor of 2 in Ec2g accounts for i<->j
                  QS(:, :, Pkj) = (TWO * c2h) * QS_kj_ik + (c2i+c2j) * QS_kj_ki  ! factor of 2 in Ec2h accounts for i<->j
                                                                                 ! Ec2i+Ec2j for i<->j
            else
                  QS(:, :, Pjk) = c2g * QS_jk_ki + (c2i+c2j) * QS_jk_ik
                  QS(:, :, Pkj) = c2h * QS_kj_ik
            end if
            call real_ab_x(YgQS, NGridTHC, Yga, NGridTHC, QS, NVirt, &
                  NGridTHC, 2*Nkj, NVirt, ONE, ZERO)
            Wg = ZERO
            do x = 1, Nkj
                  Wg(:) = Wg(:) + Ygx_kj(:, x, 1) * YgQS(:, x, 1) &
                        + Ygx_kj(:, x, 2) * YgQS(:, x, 2)
            end do
            call real_vw_x(S2ghij, Wg, ZXXgij, NGridTHC)

            if (i /= j) then
                  QS(:, :, Pkj) = (TWO * c2d) * QS_kj_ik + (c2b+c2c) * QS_kj_ki ! factor of 2 in Ec2d acounts for i<->j
                  QS(:, :, Pjk) = (c2b+c2c) * QS_jk_ik                          ! Ec2b+Ec2c for i<->j
                  call real_ab_x(ZYXkQS, NCholesky, ZYXkai, NCholesky, QS, NVirt, &
                        NCholesky, 2*Nkj, NVirt, ONE, ZERO)
                  call real_vw_x(S2bcd, ZYXkx_kj, ZYXkQS, NCholesky*2*Nkj)
            else
                  QS(:, :, Pkj) = c2d * QS_kj_ik + (c2b+c2c) * QS_kj_ki
                  call real_ab(ZYXkQS(:, :, Pkj), ZYXkai, QS(:, :, Pkj))
                  call real_vw_x(S2bcd, ZYXkx_kj(:, :, Pkj), ZYXkQS(:, :, Pkj), NCholesky*Nkj)
            end if
            Ec2ghij = Ec2ghij + S2ghij
            Ec2bcd = Ec2bcd + S2bcd
      end subroutine rpa_JCTC2024_phRPA_jik


      subroutine rpa_JCTC2024_Gaibj_Gabij_v2(EcRPA, Ec1b, Ec2bcd, Ec2ghij, &
            PNOData, PNOTransform, Xgi, Yga, Zgk, MaxNVirtPNO, NVirt, NOcc, &
            NGridTHC, NCholesky, t_SOSEX, t_G, t_TrVGaibj, t_TrVGabij, t_ZYX, t_ZXX)

            integer, intent(in)                                             :: MaxNVirtPNO, NVirt, NOcc
            integer, intent(in)                                             :: NGridTHC, NCholesky
            real(F64), intent(out)                                          :: EcRPA, Ec1b
            real(F64), intent(out)                                          :: Ec2bcd, Ec2ghij
            integer, dimension(:, :, :), intent(in)                         :: PNOData
            type(TPNOTransform), dimension(:), intent(in)                   :: PNOTransform
            real(F64), dimension(NGridTHC, NOcc), intent(in)                :: Xgi
            real(F64), dimension(NGridTHC, NVirt), intent(in)               :: Yga
            real(F64), dimension(NGridTHC, NCholesky), intent(in)           :: Zgk
            real(F64), intent(out)                                          :: t_SOSEX, t_G, t_TrVGaibj
            real(F64), intent(out)                                          :: t_TrVGabij, t_ZYX, t_ZXX
            
            real(F64), dimension(:, :), allocatable :: ZYXkbj, ZYXgbj
            real(F64), dimension(:), allocatable :: Wg
            real(F64), dimension(:, :), allocatable :: Wga, Wgb, YXgai
            real(F64), dimension(:), allocatable :: ZXXkij, ZXXgij
            integer :: SumNVirtPNOkj
            integer :: a, b, i, j, k
            real(F64), dimension(:, :), allocatable :: PQax_kj
            real(F64), dimension(:, :), allocatable :: PQax_ki
            real(F64), dimension(:, :), allocatable :: Gab_2bcd, Gab_2ghij
            integer, dimension(:), allocatable :: PQaxkjLoc, PQaxkjNum
            integer, dimension(:), allocatable :: PQaxkiLoc, PQaxkiNum
            real(F64), dimension(:, :), allocatable :: S_jk_ki, S_jk_ik, S_kj_ki, S_kj_ik
            real(F64), dimension(:, :), allocatable :: QS_jk_ki, QS_jk_ik, QS_kj_ki, QS_kj_ik, QS
            integer :: Nkj, Nki, kj0, kj1, ki0, ki1, Nij, ij0, ij1
            real(F64) :: Weight, S1b, S1a
            real(F64) :: S2ghij, S2bcd
            type(TClock) :: timer
            integer :: NPairs, ProcessedPairs, JobsDone

            allocate(ZYXkbj(NCholesky, NVirt))
            allocate(ZYXgbj(NGridTHC, NVirt))
            allocate(YXgai(NGridTHC, NVirt))
            allocate(PQax_kj(NVirt, 2*MaxNVirtPNO*NOcc))
            allocate(PQax_ki(NVirt, 2*MaxNVirtPNO*NOcc))
            allocate(Gab_2bcd(NVirt, NVirt))
            allocate(Gab_2ghij(NVirt, NVirt))
            allocate(PQaxkjLoc(NOcc))
            allocate(PQaxkjNum(NOcc))
            allocate(PQaxkiLoc(NOcc))
            allocate(PQaxkiNum(NOcc))
            allocate(ZXXgij(NGridTHC))
            allocate(ZXXkij(NCholesky))
            allocate(Wg(NGridTHC))
            allocate(Wga(NGridTHC, NVirt))
            allocate(Wgb(NGridTHC, NVirt))
            allocate(S_jk_ki(MaxNVirtPNO, MaxNVirtPNO))
            allocate(S_jk_ik(MaxNVirtPNO, MaxNVirtPNO))
            allocate(S_kj_ki(MaxNVirtPNO, MaxNVirtPNO))
            allocate(S_kj_ik(MaxNVirtPNO, MaxNVirtPNO))
            allocate(QS_jk_ki(NVirt, MaxNVirtPNO))
            allocate(QS_jk_ik(NVirt, MaxNVirtPNO))
            allocate(QS_kj_ki(NVirt, MaxNVirtPNO))
            allocate(QS_kj_ik(NVirt, MaxNVirtPNO))
            allocate(QS(NVirt, 2*MaxNVirtPNO))

            NPairs = (NOcc*(NOcc+1))/2
            t_SOSEX = ZERO
            t_G = ZERO
            t_TrVGabij = ZERO
            t_TrVGaibj = ZERO
            t_ZXX = ZERO
            t_ZYX = ZERO
            EcRPA = ZERO
            Ec1b = ZERO
            Ec2ghij = ZERO
            Ec2bcd = ZERO
            ProcessedPairs = 0
            JobsDone = 0
            do j = 1, NOcc
                  call rpa_JCTC2024_PackTransfMatrices(PQax_kj, PQaxkjLoc, PQAxkjNum, j, &
                        PNOData, PNOTransform, NOcc, NVirt)
                  SumNVirtPNOkj = sum(PQaxkjNum)
                  call clock_start(timer)
                  !$omp parallel do private(b)
                  do b = 1, NVirt
                        Wga(:, b) = Yga(:, b) * Xgi(:, j)
                  end do
                  !$omp end parallel do
                  call real_aTb(ZYXkbj, Zgk, Wga)
                  call real_ab(ZYXgbj, Zgk, ZYXkbj)                  
                  t_ZYX = t_ZYX + clock_readwall(timer)
                  do i = j, NOcc
                        if (i /= j) then
                              Weight = TWO
                              call rpa_JCTC2024_PackTransfMatrices(PQax_ki, PQaxkiLoc, PQaxkiNum, i, &
                                    PNOData, PNOTransform, NOcc, NVirt)
                        else
                              PQax_ki = PQax_kj
                              PQaxkiLoc = PQaxkjLoc
                              PQaxkiNum = PQaxkjNum
                              Weight = ONE
                        end if
                        !
                        ! Intermediates for computing transformed
                        ! two-electron integrals
                        !
                        call clock_start(timer)
                        Wg(:) = Xgi(:, i) * Xgi(:, j)
                        call real_ATv(ZXXkij, Zgk, Wg)
                        call real_Av(ZXXgij, Zgk, ZXXkij)
                        t_ZXX = t_ZXX + clock_readwall(timer)
                        call clock_start(timer)
                        !$omp parallel do
                        do a = 1, NVirt
                              YXgai(:, a) = Yga(:, a) * Xgi(:, i)
                        end do
                        !$omp end parallel do
                        t_ZYX = t_ZYX + clock_readwall(timer)
                        call clock_start(timer)
                        Nij = PQaxkjNum(i)
                        if (Nij > 0) then
                              ij0 = PQaxkjLoc(i)
                              ij1 = PQaxkjLoc(i) + 2*Nij - 1
                              call rpa_JCTC2024_S1ab(S1a, S1b, Wga, Wgb, &
                                    PQax_kj(:, ij0:ij1), YXgai, ZYXgbj, &
                                    NVirt, NGridTHC, Nij)
                              Ec1b = Ec1b - TWO * Weight * S1b
                              EcRPA = EcRPA + TWO * Weight * S1a
                        end if
                        t_SOSEX = t_SOSEX + clock_readwall(timer)
                        call clock_start(timer)
                        Gab_2bcd = ZERO
                        Gab_2ghij = ZERO
                        do k = 1, NOcc
                              Nkj = PQaxkjNum(k)
                              Nki = PQaxkiNum(k)
                              if (Nkj*Nki > 0) then
                                    kj0 = PQaxkjLoc(k)
                                    kj1 = PQaxkjLoc(k) + 2*Nkj - 1
                                    ki0 = PQaxkiLoc(k)
                                    ki1 = PQaxkiLoc(k) + 2*Nki - 1
                                    call rpa_JCTC2024_phRPA_Gab_jik( &
                                          Gab_2bcd, Gab_2ghij, &
                                          S_jk_ki, S_jk_ik, S_kj_ki, S_kj_ik, &
                                          QS_jk_ki, QS_jk_ik, QS_kj_ki, QS_kj_ik, QS, &
                                          j, i, k, &
                                          PQax_kj(:, kj0:kj1), PQax_ki(:, ki0:ki1), &
                                          Nkj, Nki, NVirt)                                    
                              end if
                        end do
                        t_G = t_G + clock_readwall(timer)
                        call clock_start(timer)
                        call TrVabijGabij(S2ghij, Wga, Gab_2ghij, Yga, ZXXgij)
                        t_TrVGabij = t_TrVGabij + clock_readwall(timer)
                        call clock_start(timer)
                        call TrVbjaiGajbi(S2bcd, Wga, Gab_2bcd, ZYXgbj, YXgai)
                        t_TrVGaibj = t_TrVGaibj + clock_readwall(timer)
                        Ec2ghij = Ec2ghij + S2ghij
                        Ec2bcd = Ec2bcd + S2bcd
                        !
                        ! Progress info
                        !
                        ProcessedPairs = ProcessedPairs + 1
                        if (10*((10*ProcessedPairs)/NPairs) > JobsDone) then
                              JobsDone = 10*((10*ProcessedPairs)/NPairs)
                              call msg(rfield(str(JobsDone), 3) // "% completed")
                  end do
            end do

      contains

            subroutine TrVabijGabij(D, Wga, Gab, Yga, ZXXgij)
                  real(F64), intent(out)                             :: D
                  real(F64), dimension(NGridTHC, NVirt), intent(out) :: Wga
                  real(F64), dimension(NVirt, NVirt), intent(in)     :: Gab
                  real(F64), dimension(NGridTHC, NVirt), intent(in)  :: Yga
                  real(F64), dimension(NGridTHC), intent(in)         :: ZXXgij

                  integer :: a
                  
                  call real_ab(Wga, Yga, Gab)
                  !$omp parallel do private(a)
                  do a = 1, NVirt
                        Wga(:, a) = ZXXgij(:) * Wga(:, a)
                  end do
                  !$omp end parallel do
                  call real_vw_x(D, Yga, Wga, NGridTHC*NVirt)
            end subroutine TrVabijGabij

            subroutine TrVbjaiGajbi(D, Wga, Gab, ZYXgbj, YXgai)
                  real(F64), intent(out)                             :: D
                  real(F64), dimension(NGridTHC, NVirt), intent(out) :: Wga
                  real(F64), dimension(NVirt, NVirt), intent(in)     :: Gab
                  real(F64), dimension(NGridTHC, NVirt), intent(in)  :: ZYXgbj
                  real(F64), dimension(NGridTHC, NVirt), intent(in)  :: YXgai
                  
                  call real_ab(Wga, ZYXgbj, Gab)
                  call real_vw_x(D, Wga, YXgai, NGridTHC*NVirt)
            end subroutine TrVbjaiGajbi
      end subroutine rpa_JCTC2024_Gaibj_Gabij_v2


      subroutine rpa_JCTC2024_S1ab(S1a, S1b, Wgx, Wgy, PQij, YXgai, ZYXgbj, NVirt, NGridTHC, Nij)
            integer, intent(in)                                :: NVirt, NGridTHC, Nij
            real(F64), intent(out)                             :: S1a, S1b
            real(F64), dimension(NGridTHC, Nij), intent(out)   :: Wgx
            real(F64), dimension(NGridTHC, Nij), intent(out)   :: Wgy
            real(F64), dimension(NVirt, Nij, 2), intent(in)    :: PQij
            real(F64), dimension(NGridTHC, NVirt), intent(in)  :: YXgai
            real(F64), dimension(:, :), intent(in)             :: ZYXgbj

            integer, parameter :: Pij = 1
            integer, parameter :: Qij = 2
            integer, parameter :: Pji = 2
            integer, parameter :: Qji = 1
            
            call real_ab(Wgx, YXgai, PQij(:, :, Qij))
            call real_ab(Wgy, ZYXgbj, PQij(:, :, Pij))
            call real_vw_x(S1b, Wgx, Wgy, NGridTHC*Nij)

            call real_ab(Wgx, YXgai, PQij(:, :, Qji))
            call real_ab(Wgy, ZYXgbj, PQij(:, :, Pji))
            call real_vw_x(S1a, Wgx, Wgy, NGridTHC*Nij)
      end subroutine rpa_JCTC2024_S1ab


      subroutine rpa_JCTC2024_phRPA_Gab_jik( &
            Gab_2bcd, Gab_2ghij, &
            S_jk_ki, S_jk_ik, S_kj_ki, S_kj_ik, &
            QS_jk_ki, QS_jk_ik, QS_kj_ki, QS_kj_ik, QS, &
            j, i, k, PQ_kj, PQ_ki, &
            Nkj, Nki, NVirt)

            integer, intent(in)                                 :: Nkj, Nki
            integer, intent(in)                                 :: NVirt
            real(F64), dimension(NVirt, NVirt), intent(inout)   :: Gab_2bcd
            real(F64), dimension(NVirt, NVirt), intent(inout)   :: Gab_2ghij
            real(F64), dimension(Nkj, Nki), intent(out)         :: S_jk_ki, S_jk_ik, S_kj_ki, S_kj_ik
            real(F64), dimension(NVirt, Nkj), intent(out)       :: QS_jk_ki, QS_jk_ik, QS_kj_ki, QS_kj_ik
            real(F64), dimension(NVirt, Nkj, 2), intent(out)    :: QS
            integer, intent(in)                                 :: j, i, k
            real(F64), dimension(NVirt, Nkj, 2), intent(in)     :: PQ_kj
            real(F64), dimension(NVirt, Nki, 2), intent(in)     :: PQ_ki

            real(F64) :: c2g, c2h, c2i, c2j, c2d, c2b, c2c
            integer :: Pki, Qki, Pik, Qik
            integer :: Pkj, Qkj, Pjk, Qjk

            if (k >= i) then
                  Pki = 1
                  Qki = 2
                  Pik = 2
                  Qik = 1
            else
                  Pki = 2
                  Qki = 1
                  Pik = 1
                  Qik = 2
            end if
            if (k >= j) then
                  Pkj = 1
                  Qkj = 2
                  Pjk = 2
                  Qjk = 1
            else
                  Pkj = 2
                  Qkj = 1
                  Pjk = 1
                  Qjk = 2
            end if

            c2b = -FOUR * (ONE/TWO)
            c2c = -FOUR * (ONE/TWO)
            c2d = TWO
            c2g = -FOUR
            c2h = -FOUR
            c2i = TWO
            c2j = TWO

            call real_aTb(S_jk_ki, PQ_kj(:, :, Qjk), PQ_ki(:, :, Pki))
            call real_aTb(S_jk_ik, PQ_kj(:, :, Qjk), PQ_ki(:, :, Pik))
            call real_aTb(S_kj_ki, PQ_kj(:, :, Qkj), PQ_ki(:, :, Pki))
            call real_aTb(S_kj_ik, PQ_kj(:, :, Qkj), PQ_ki(:, :, Pik))

            call real_abT(QS_jk_ki, PQ_ki(:, :, Qki), S_jk_ki)
            call real_abT(QS_jk_ik, PQ_ki(:, :, Qik), S_jk_ik)
            call real_abT(QS_kj_ki, PQ_ki(:, :, Qki), S_kj_ki)
            call real_abT(QS_kj_ik, PQ_ki(:, :, Qik), S_kj_ik)

            if (i /= j) then
                  QS(:, :, Pjk) = (TWO * c2g) * QS_jk_ki + (c2i+c2j) * QS_jk_ik  ! factor of 2 in Ec2g accounts for i<->j
                  QS(:, :, Pkj) = (TWO * c2h) * QS_kj_ik + (c2i+c2j) * QS_kj_ki  ! factor of 2 in Ec2h accounts for i<->j
                                                                                 ! Ec2i+Ec2j for i<->j
            else
                  QS(:, :, Pjk) = c2g * QS_jk_ki + (c2i+c2j) * QS_jk_ik
                  QS(:, :, Pkj) = c2h * QS_kj_ik
            end if
            !
            ! G(ba,ji)
            !
            call real_abT_x(Gab_2ghij, NVirt, PQ_kj, NVirt, QS, NVirt, &
                  NVirt, NVirt, 2*Nkj, ONE, ONE)
            !
            ! G(bj,ai)
            !
            if (i /= j) then
                  QS(:, :, Pkj) = (TWO * c2d) * QS_kj_ik + (c2b+c2c) * QS_kj_ki ! factor of 2 in Ec2d acounts for i<->j
                  QS(:, :, Pjk) = (c2b+c2c) * QS_jk_ik                          ! Ec2b+Ec2c for i<->j
                  call real_abT_x(Gab_2bcd, NVirt, PQ_kj, NVirt, QS, NVirt, &
                        NVirt, NVirt, 2*Nkj, ONE, ONE)
            else
                  QS(:, :, Pkj) = c2d * QS_kj_ik + (c2b+c2c) * QS_kj_ki
                  call real_abT_x(Gab_2bcd, NVirt, PQ_kj(:, :, Pkj), NVirt, QS(:, :, Pkj), NVirt, &
                        NVirt, NVirt, Nkj, ONE, ONE)
            end if
      end subroutine rpa_JCTC2024_phRPA_Gab_jik
      

      subroutine rpa_JCTC2024_Tabij(Tabij, Pam, Qam, Uaim, Am, i, j, NOcc, NVirt, NVecsT2)
            integer, intent(in)                                    :: NOcc, NVirt, NVecsT2
            real(F64), dimension(NVirt, NVirt), intent(out)        :: Tabij
            real(F64), dimension(NVirt, NVecsT2), intent(out)      :: Pam
            real(F64), dimension(NVirt, NVecsT2), intent(out)      :: Qam
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in) :: Uaim
            real(F64), dimension(NVecsT2), intent(in)              :: Am
            integer, intent(in)                                    :: i
            integer, intent(in)                                    :: j

            integer :: mu

            !$omp parallel do private(mu)
            do mu = 1, NVecsT2
                  Pam(:, mu) = Uaim(:, i, mu)
                  Qam(:, mu) = Uaim(:, j, mu) * Am(mu)
            end do
            !$omp end parallel do
            call real_abT(Tabij, Pam, Qam)
      end subroutine rpa_JCTC2024_Tabij
end module rpa_JCTC2024
