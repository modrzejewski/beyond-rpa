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
            real(F64) :: EcRPA, Ec1b, Ec2b, Ec2d, Ec2i, Ec2h, Ec2g
            integer :: NVecsT2, NCholesky, NGridTHC, NOcc, NVirt
            integer :: MaxNVirtPNO, NVirtPNO
            integer :: SumNVirtPNO, AvNVirtPNO
            integer :: NOccPairs
            type(TPNOTransform), dimension(:), allocatable :: PNOTransform
            real(F64), dimension(:), allocatable :: Sigma
            real(F64), dimension(:, :), allocatable :: U, V
            integer, dimension(:, :, :), allocatable :: PNOData
            integer :: IJ
            type(TClock) :: timer_Total, timer_SVD, timer_Energy, timer_LO
            real(F64) :: t_Total, t_SVD, t_Energy, t_LO

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
            call rpa_LocalizedOrbitals(Lik, Cpi, NOcc, RPAParams, AOBasis)
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
                              PNOData(PNO_PAIR_INDEX, I, J) = -1
                              PNOData(PNO_PAIR_INDEX, J, I) = -1
                              PNOData(PNO_NVIRT, I, J) = 0
                              PNOData(PNO_NVIRT, J, I) = 0
                        end if
                        MaxNVirtPNO = max(MaxNVirtPNO, NVirtPNO)
                        SumNVirtPNO = SumNVirtPNO + NVirtPNO
                  end do
            end do
            AvNVirtPNO = nint(real(SumNVirtPNO,F64)/((NOcc*(NOcc+1))/2))
            t_SVD = clock_readwall(timer_SVD)
            call blankline()
            call msg("Pair-natural orbitals cutoff " // str(RPAParams%CutoffThreshPNO,d=1))
            call msg("AverageNVirtPNO              " // str(AvNVirtPNO))
            call msg("Max NVirtPNO                 " // str(MaxNVirtPNO))
            call msg("NVirt                        " // str(NVirt))
            call msg("AverageNVirtPNO/NVirt        " // str(real(AvNVirtPNO,F64)/NVirt,d=1))
            !
            ! Ec1b + Ec2b + Ec2c + Ec2d
            ! Ec2g + Ec2h + Ec2i + Ec2j
            !
            call clock_start(timer_Energy)
            call rpa_JCTC2024_Gaibj_Gabij(EcRPA, Ec1b, Ec2b, Ec2d, Ec2g, Ec2h, Ec2i, &
                  PNOData, PNOTransform, XgiLoc, Yga, Zgk, MaxNVirtPNO, NVirt, NOcc, &
                  NGridTHC, NCholesky)
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
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_1B) = (ONE/TWO) * Ec1b
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_2B) = (ONE/TWO) * Ec2b
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_2C) = (ONE/TWO) * Ec2b
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_2D) = Ec2d
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_2G) = Ec2g
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_2H) = Ec2h
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_2I) = Ec2i
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_2J) = Ec2i

            call blankline()
            call msg("Calculation of particle-hole corrections completed")
            call msg("Timings in seconds:")
            call msg("Total time           " // str(t_Total,d=1))
            call msg("Orbital localization " // str(t_LO,d=1))
            call msg("SVD                  " // str(t_SVD,d=1))
            call msg("Energy terms         " // str(t_Energy,d=1))
            call blankline()
      end subroutine rpa_JCTC2024_Corrections


      subroutine rpa_JCTC2024_Gaibj_Gabij(EcRPA, Ec1b, Ec2b, Ec2d, Ec2g, Ec2h, Ec2i, &
            PNOData, PNOTransform, Xgi, Yga, Zgk, MaxNVirtPNO, NVirt, NOcc, &
            NGridTHC, NCholesky)

            integer, intent(in)                                             :: MaxNVirtPNO, NVirt, NOcc
            integer, intent(in)                                             :: NGridTHC, NCholesky
            real(F64), intent(out)                                          :: EcRPA, Ec1b, Ec2b, Ec2d
            real(F64), intent(out)                                          :: Ec2g, Ec2h, Ec2i
            integer, dimension(:, :, :), intent(in)                         :: PNOData
            type(TPNOTransform), dimension(:), intent(in)                   :: PNOTransform
            real(F64), dimension(NGridTHC, NOcc), intent(in)                :: Xgi
            real(F64), dimension(NGridTHC, NVirt), intent(in)               :: Yga
            real(F64), dimension(NGridTHC, NCholesky), intent(in)           :: Zgk
            
            real(F64), dimension(:, :), allocatable :: Ygx, Ygy
            real(F64), dimension(:, :), allocatable :: ZYXkai, ZYXkbj, ZYXkyi, ZYXkyj
            real(F64), dimension(:), allocatable :: Wg
            real(F64), dimension(:), allocatable :: XXgij, ZXXkij, ZXXgij
            real(F64), dimension(:, :), allocatable :: Sxy, PSy
            real(F64) :: S1a, S1b, S2bij, S2bji, S2d, S2g, S2h, S2iij, S2iji
            real(F64) :: Weight
            integer :: a, i, j, k
            
            allocate(Ygx(NGridTHC, MaxNVirtPNO))
            allocate(Ygy(NGridTHC, MaxNVirtPNO))
            allocate(Wg(NGridTHC))
            allocate(ZYXkai(NCholesky, NVirt))
            allocate(ZYXkbj(NCholesky, NVirt))
            allocate(ZYXkyi(NCholesky, MaxNVirtPNO))
            allocate(ZYXkyj(NCholesky, MaxNVirtPNO))
            allocate(Sxy(MaxNVirtPNO, MaxNVirtPNO))
            allocate(PSy(NVirt, MaxNVirtPNO))
            allocate(XXgij(NGridTHC))            
            allocate(ZXXgij(NGridTHC))
            allocate(ZXXkij(NCholesky))
            EcRPA = ZERO
            Ec1b = ZERO
            Ec2b = ZERO
            Ec2d = ZERO
            Ec2g = ZERO
            Ec2h = ZERO
            Ec2i = ZERO
            do j = 1, NOcc
                  do i = j, NOcc
                        if (i /= j) then
                              Weight = TWO
                        else
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
                                    Wg = Yga(:, a) * Xgi(:, j)
                                    call real_Atv(ZYXkbj(:, a), Zgk, Wg)
                              end do
                        else
                              do a = 1, NVirt
                                    Wg = Yga(:, a) * Xgi(:, i)
                                    call real_Atv(ZYXkai(:, a), Zgk, Wg)
                              end do
                              ZYXkbj = ZYXkai
                        end if
                        call Gaibj1b(S1b, PNOData(:, i, j), ZYXkai, ZYXkbj)
                        Ec1b = Ec1b - TWO * Weight * S1b
                        call Gaibj1b(S1a, PNOData(:, j, i), ZYXkai, ZYXkbj)
                        EcRPA = EcRPA + TWO * Weight * S1a
                        do k = 1, NOcc
                              call Gabij(S2g, PNOData(:, j, k), PNOData(:, k, i))
                              Ec2g = Ec2g - FOUR * Weight * S2g
                              call Gabij(S2h, PNOData(:, k, j), PNOData(:, i, k))
                              Ec2h = Ec2h - FOUR * Weight * S2h
                              call Gaibj(S2d, PNOData(:, k, i), PNOData(:, j, k), &
                                    ZYXkai, ZYXkbj)
                              Ec2d = Ec2d + TWO * Weight * S2d
                              call Gaibj(S2bij, PNOData(:, i, k), PNOData(:, j, k), &
                                    ZYXkai, ZYXkbj)
                              call Gabij(S2iij, PNOData(:, j, k), PNOData(:, i, k))
                              if (i /= j) then
                                    call Gaibj(S2bji, PNOData(:, j, k), PNOData(:, i, k), &
                                          ZYXkbj, ZYXkai)
                                    call Gabij(S2iji, PNOData(:, i, k), PNOData(:, j, k))                                    
                                    Ec2b = Ec2b - FOUR * (S2bij + S2bji)
                                    Ec2i = Ec2i + TWO * (S2iij + S2iji)
                              else
                                    Ec2b = Ec2b - FOUR * S2bij
                                    Ec2i = Ec2i + TWO * S2iij
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

            subroutine Gaibj(D, Ix, Iy, ZYXkai, ZYXkbj)
                  real(F64), intent(out)                 :: D
                  integer, dimension(:), intent(in)      :: Ix
                  integer, dimension(:), intent(in)      :: Iy
                  real(F64), dimension(:, :), intent(in) :: ZYXkai
                  real(F64), dimension(:, :), intent(in) :: ZYXkbj

                  integer :: X, Xp, Xq, Nx
                  integer :: Y, Yp, Yq, Ny

                  Nx = Ix(PNO_NVIRT)
                  Ny = Iy(PNO_NVIRT)
                  if (Nx*Ny > 0) then
                        X  = Ix(PNO_PAIR_INDEX)
                        Xp = Ix(PNO_LEFT_TRANSFORM)
                        Xq = Ix(PNO_RIGHT_TRANSFORM)
                        Y  = Iy(PNO_PAIR_INDEX)
                        Yp = Iy(PNO_LEFT_TRANSFORM)
                        Yq = Iy(PNO_RIGHT_TRANSFORM)
                        call VxiyjDotSxy(D, &
                              Sxy, PSy, ZYXkyi, ZYXkyj, &
                              PNOTransform(X)%TaxPNO(:, :, Xp), PNOTransform(X)%TaxPNO(:, :, Xq), &
                              PNOTransform(Y)%TaxPNO(:, :, Yp), PNOTransform(Y)%TaxPNO(:, :, Yq), &
                              ZYXkai, ZYXkbj, Nx, Ny)
                  else
                        D = ZERO
                  end if
            end subroutine Gaibj

            subroutine Gabij(D, Ix, Iy)
                  real(F64), intent(out)            :: D
                  integer, dimension(:), intent(in) :: Ix
                  integer, dimension(:), intent(in) :: Iy

                  integer :: X, Xp, Xq, Nx
                  integer :: Y, Yp, Yq, Ny

                  Nx = Ix(PNO_NVIRT)
                  Ny = Iy(PNO_NVIRT)
                  if (Nx*Ny > 0) then
                        X  = Ix(PNO_PAIR_INDEX)
                        Xp = Ix(PNO_LEFT_TRANSFORM)
                        Xq = Ix(PNO_RIGHT_TRANSFORM)
                        Y  = Iy(PNO_PAIR_INDEX)
                        Yp = Iy(PNO_LEFT_TRANSFORM)
                        Yq = Iy(PNO_RIGHT_TRANSFORM)
                        call VxyijDotSxy(D, Wg, Sxy, PSy, Ygx, Ygy, &
                              PNOTransform(X)%TaxPNO(:, :, Xp), PNOTransform(X)%TaxPNO(:, :, Xq), &
                              PNOTransform(Y)%TaxPNO(:, :, Yp), PNOTransform(Y)%TaxPNO(:, :, Yq), &
                              ZXXgij, Nx, Ny)
                  else
                        D = ZERO
                  end if
            end subroutine Gabij
            
            subroutine VxiyjDotSxy(D, Sxy, PSy, ZYXkyi, ZYXkyj, &
                  Px, Qx, Py, Qy, ZYXkai, ZYXkbj, Nx, Ny)
                  
                  integer, intent(in)                                :: Nx, Ny
                  real(F64), intent(out)                             :: D
                  real(F64), dimension(Nx, Ny), intent(out)          :: Sxy
                  real(F64), dimension(NVirt, Ny), intent(out)       :: PSy
                  real(F64), dimension(NCholesky, Ny), intent(out)   :: ZYXkyi
                  real(F64), dimension(NCholesky, Ny), intent(out)   :: ZYXkyj
                  real(F64), dimension(NVirt, Nx), intent(in)        :: Px
                  real(F64), dimension(NVirt, Nx), intent(in)        :: Qx
                  real(F64), dimension(NVirt, Ny), intent(in)        :: Py
                  real(F64), dimension(NVirt, Ny), intent(in)        :: Qy
                  real(F64), dimension(NCholesky, NVirt), intent(in) :: ZYXkai
                  real(F64), dimension(NCholesky, NVirt), intent(in) :: ZYXkbj

                  call real_aTb(Sxy, Qx, Py)
                  call real_ab(PSy, Px, Sxy)
                  call real_ab(ZYXkyi, ZYXkai, PSy)
                  call real_ab(ZYXkyj, ZYXkbj, Qy)
                  call real_vw_x(D, ZYXkyi, ZYXkyj, NCholesky*Ny)
            end subroutine VxiyjDotSxy
            
            subroutine VxyijDotSxy(D, Wg, Sxy, PSy, Ygx, Ygy, Px, Qx, Py, Qy, &
                  ZXXgij, Nx, Ny)
                  
                  integer, intent(in)                               :: Nx, Ny
                  real(F64), intent(out)                            :: D
                  real(F64), dimension(NGridTHC), intent(out)       :: Wg
                  real(F64), dimension(Nx, Ny), intent(out)         :: Sxy
                  real(F64), dimension(NVirt, Ny), intent(out)      :: PSy
                  real(F64), dimension(NGridTHC, Ny), intent(out)   :: Ygx
                  real(F64), dimension(NGridTHC, Ny), intent(out)   :: Ygy
                  real(F64), dimension(NVirt, Nx), intent(in)       :: Px
                  real(F64), dimension(NVirt, Nx), intent(in)       :: Qx
                  real(F64), dimension(NVirt, Ny), intent(in)       :: Py
                  real(F64), dimension(NVirt, Ny), intent(in)       :: Qy
                  real(F64), dimension(NGridTHC), intent(in)        :: ZXXgij

                  real(F64) :: Dy
                  integer :: y
                  
                  call real_aTb(Sxy, Qx, Py)
                  call real_ab(PSy, Px, Sxy)
                  call real_ab(Ygx, Yga, PSy)
                  call real_ab(Ygy, Yga, Qy)
                  Wg = ZERO
                  do y = 1, Ny
                        Wg(:) = Wg(:) + Ygx(:, y) * Ygy(:, y)                        
                  end do
                  call real_vw_x(D, Wg, ZXXgij, NGridTHC)
            end subroutine VxyijDotSxy
      end subroutine rpa_JCTC2024_Gaibj_Gabij
      
      
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
