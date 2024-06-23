module rpa_JCTC2024
      use arithmetic
      use math_constants
      use rpa_definitions
      use real_linalg
      use basis_sets
      use rpa_Orbitals
      use clock

      implicit none
      
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
            real(F64) :: Ec1b, Ec2b, Ec2d, Ec2i, Ec2h, Ec2g
            integer :: NVecsT2, NCholesky, NGridTHC, NOcc, NVirt
            integer :: MaxNVirtPNO, NVirtPNO
            integer :: NOccPairs
            real(F64), dimension(:, :, :, :), allocatable :: TaxPNOij
            real(F64), dimension(:), allocatable :: Sigma
            real(F64), dimension(:, :), allocatable :: U, V
            integer, dimension(:, :, :), allocatable :: LocIJ            
            integer :: IJ
            type(TClock) :: timer

            call clock_start(timer)
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
            !
            ! Find the number of pair-natural orbitals corresponding
            ! to CutoffThreshPNO by decomposing diagonal amplitude
            ! matrices T(ai,bi)
            !
            allocate(Sigma(NVirt))
            allocate(U(NVirt, NVirt))
            allocate(V(NVirt, NVirt))
            MaxNVirtPNO = 0            
            do i = 1, NOcc
                  call rpa_JCTC2024_Tabij(Tabij, Pam, Qam, UaimLoc, Am, i, i, &
                        NOcc, NVirt, NVecsT2)
                  call symmetric_eigenproblem(Sigma, Tabij, NVirt, .false.)
                  NVirtPNO = 0
                  do mu = 1, NVirt
                        if (Abs(Sigma(mu)) >= RPAParams%CutoffThreshPNO) then
                              NVirtPNO = NVirtPNO + 1
                        end if
                  end do
                  MaxNVirtPNO = max(MaxNVirtPNO, NVirtPNO)
            end do
            NVirtPNO = MaxNVirtPNO
            call blankline()
            call msg("Pair-natural orbitals cutoff " // str(RPAParams%CutoffThreshPNO,d=1))
            call msg("NVirtPNO                     " // str(NVirtPNO))
            call msg("NVirt                        " // str(NVirt))
            call msg("NVirtPNO/NVirt               " // str(real(NVirtPNO,F64)/NVirt,d=1))
            !
            ! Singular value decomposition of T(aI,bJ)
            !
            NOccPairs = (NOcc * (NOcc + 1)) / 2
            allocate(LocIJ(3, NOcc, NOcc))
            allocate(TaxPNOij(NVirt, NVirtPNO, 2, NOccPairs))
            LocIJ = 0
            IJ = 0
            do j = 1, NOcc
                  do i = j, NOcc
                        IJ = IJ + 1
                        call rpa_JCTC2024_Tabij(Tabij, Pam, Qam, UaimLoc, Am, i, j, &
                              NOcc, NVirt, NVecsT2)                        
                        call real_SVD(U, V, Sigma, Tabij)
                        TaxPNOij(:, :, 1, IJ) = U(:, 1:NVirtPNO)
                        do x = 1, NVirtPNO
                              TaxPNOij(:, x, 2, IJ) = Sigma(x) * V(:, x)
                        end do
                        LocIJ(1, I, J) = IJ
                        LocIJ(2, I, J) = 1
                        LocIJ(3, I, J) = 2
                        LocIJ(1, J, I) = IJ
                        LocIJ(2, J, I) = 2
                        LocIJ(3, J, I) = 1
                  end do
            end do
            !
            ! Ec1b + Ec2b + Ec2c + Ec2d
            ! Ec2g + Ec2h + Ec2i + Ec2j
            !
            call rpa_JCTC2024_Gaibj_Gabij(Ec1b, Ec2b, Ec2d, Ec2g, Ec2h, Ec2i, &
                  LocIJ, TaxPNOij, XgiLoc, Yga, Zgk, NVirtPNO, NVirt, NOcc, &
                  NOccPairs, NGridTHC, NCholesky)

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
            call msg("Total time             " // str(clock_readwall(timer),d=1) // " seconds")
            call blankline()
      end subroutine rpa_JCTC2024_Corrections



      subroutine rpa_JCTC2024_Gaibj_Gabij(Ec1b, Ec2b, Ec2d, Ec2g, Ec2h, Ec2i, &
            LocIJ, TaxPNOij, Xgi, Yga, Zgk, NVirtPNO, NVirt, NOcc, NOccPairs, &
            NGridTHC, NCholesky)

            integer, intent(in)                                             :: NVirtPNO, NVirt, NOcc
            integer, intent(in)                                             :: NOccPairs, NGridTHC, NCholesky
            real(F64), intent(out)                                          :: Ec1b, Ec2b, Ec2d
            real(F64), intent(out)                                          :: Ec2g, Ec2h, Ec2i
            integer, dimension(3, NOcc, NOcc), intent(in)                   :: LocIJ
            real(F64), dimension(NVirt, NVirtPNO, 2, NOccPairs), intent(in) :: TaxPNOij
            real(F64), dimension(NGridTHC, NOcc), intent(in)                :: Xgi
            real(F64), dimension(NGridTHC, NVirt), intent(in)               :: Yga
            real(F64), dimension(NGridTHC, NCholesky), intent(in)           :: Zgk
            
            real(F64), dimension(:, :), allocatable :: Ygx, Ygy
            real(F64), dimension(:, :), allocatable :: ZYXkai, ZYXkbj, ZYXkyi, ZYXkyj
            real(F64), dimension(:), allocatable :: Wg
            real(F64), dimension(:), allocatable :: XXgij, ZXXkij, ZXXgij
            real(F64), dimension(:, :), allocatable :: Sxy, PSy
            real(F64) :: S1b, S2bij, S2bji, S2d, S2g, S2h, S2iij, S2iji
            real(F64) :: Weight
            integer :: a, i, j, k
            integer :: IJ, IJp, IJq
            integer :: IK, IKp, IKq
            integer :: JK, JKp, JKq
            integer :: KJ, KJp, KJq
            integer :: KI, KIp, KIq
            
            allocate(Ygx(NGridTHC, NVirtPNO))
            allocate(Ygy(NGridTHC, NVirtPNO))
            allocate(Wg(NGridTHC))
            allocate(ZYXkai(NCholesky, NVirt))
            allocate(ZYXkbj(NCholesky, NVirt))
            allocate(ZYXkyi(NCholesky, NVirtPNO))
            allocate(ZYXkyj(NCholesky, NVirtPNO))
            allocate(Sxy(NVirtPNO, NVirtPNO))
            allocate(PSy(NVirt, NVirtPNO))
            allocate(XXgij(NGridTHC))            
            allocate(ZXXgij(NGridTHC))            
            allocate(ZXXkij(NCholesky))
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
                              
                        IJ = LocIJ(1, i, j)
                        IJp = LocIJ(2, i, j)
                        IJq = LocIJ(3, i, j)
                        if (IJ /= 0) then
                              call TrVxiyj(S1b, &
                                    TaxPNOij(:, :, IJp, IJ), &
                                    TaxPNOij(:, :, IJq, IJ), &
                                    ZYXkai, ZYXkbj)
                              Ec1b = Ec1b - TWO * Weight * S1b
                        end if
                        do k = 1, NOcc
                              KI = LocIJ(1, k, i)
                              KIp = LocIJ(2, k, i)
                              KIq = LocIJ(3, k, i)
                              JK = LocIJ(1, j, k)
                              JKp = LocIJ(2, j, k)
                              JKq = LocIJ(3, j, k)
                              IK = LocIJ(1, i, k)
                              IKp = LocIJ(2, i, k)
                              IKq = LocIJ(3, i, k)
                              KJ = LocIJ(1, k, j)
                              KJp = LocIJ(2, k, j)
                              KJq = LocIJ(3, k, j)
                              call VxyijDotSxy(S2g, &
                                    TaxPNOij(:, :, JKp, JK), TaxPNOij(:, :, JKq, JK), &
                                    TaxPNOij(:, :, KIp, KI), TaxPNOij(:, :, KIq, KI), i, j)
                              Ec2g = Ec2g - FOUR * Weight * S2g
                              call VxyijDotSxy(S2h, &
                                    TaxPNOij(:, :, KJp, KJ), TaxPNOij(:, :, KJq, KJ), &
                                    TaxPNOij(:, :, IKp, IK), TaxPNOij(:, :, IKq, IK), i, j)
                              Ec2h = Ec2h - FOUR * Weight * S2h
                              call VxiyjDotSxy(S2d, &
                                    TaxPNOij(:, :, KIp, KI), TaxPNOij(:, :, KIq, KI), &
                                    TaxPNOij(:, :, JKp, JK), TaxPNOij(:, :, JKq, JK), &
                                    ZYXkai, ZYXkbj)
                              Ec2d = Ec2d + TWO * Weight * S2d
                              call VxiyjDotSxy(S2bij, &
                                    TaxPNOij(:, :, IKp, IK), TaxPNOij(:, :, IKq, IK), &
                                    TaxPNOij(:, :, JKp, JK), TaxPNOij(:, :, JKq, JK), &
                                    ZYXkai, ZYXkbj)
                              call VxyijDotSxy(S2iij, &
                                    TaxPNOij(:, :, JKp, JK), TaxPNOij(:, :, JKq, JK), &
                                    TaxPNOij(:, :, IKp, IK), TaxPNOij(:, :, IKq, IK), i, j)
                              if (i /= j) then
                                    call VxiyjDotSxy(S2bji, &
                                          TaxPNOij(:, :, JKp, JK), TaxPNOij(:, :, JKq, JK), &
                                          TaxPNOij(:, :, IKp, IK), TaxPNOij(:, :, IKq, IK), &
                                          ZYXkbj, ZYXkai)
                                    call VxyijDotSxy(S2iji, &
                                          TaxPNOij(:, :, IKp, IK), TaxPNOij(:, :, IKq, IK), &
                                          TaxPNOij(:, :, JKp, JK), TaxPNOij(:, :, JKq, JK), j, i)
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

            subroutine TrVxiyj(D, Py, Qy, ZYXkai, ZYXkbj)
                  real(F64), intent(out)                             :: D
                  real(F64), dimension(NVirt, NVirtPNO), intent(in)  :: Py
                  real(F64), dimension(NVirt, NVirtPNO), intent(in)  :: Qy
                  real(F64), dimension(NCholesky, NVirt), intent(in) :: ZYXkai
                  real(F64), dimension(NCholesky, NVirt), intent(in) :: ZYXkbj

                  call real_ab(ZYXkyi, ZYXkai, Qy)
                  call real_ab(ZYXkyj, ZYXkbj, Py)
                  call real_vw_x(D, ZYXkyi, ZYXkyj, NCholesky*NVirtPNO)
            end subroutine TrVxiyj

            subroutine VxiyjDotSxy(D, Px, Qx, Py, Qy, ZYXkai, ZYXkbj)
                  real(F64), intent(out)                             :: D
                  real(F64), dimension(NVirt, NVirtPNO), intent(in)  :: Px
                  real(F64), dimension(NVirt, NVirtPNO), intent(in)  :: Qx
                  real(F64), dimension(NVirt, NVirtPNO), intent(in)  :: Py
                  real(F64), dimension(NVirt, NVirtPNO), intent(in)  :: Qy
                  real(F64), dimension(NCholesky, NVirt), intent(in) :: ZYXkai
                  real(F64), dimension(NCholesky, NVirt), intent(in) :: ZYXkbj

                  call real_aTb(Sxy, Qx, Py)
                  call real_ab(PSy, Px, Sxy)
                  call real_ab(ZYXkyi, ZYXkai, PSy)
                  call real_ab(ZYXkyj, ZYXkbj, Qy)
                  call real_vw_x(D, ZYXkyi, ZYXkyj, NCholesky*NVirtPNO)
            end subroutine VxiyjDotSxy
            
            subroutine VxyijDotSxy(D, Px, Qx, Py, Qy, i, j)
                  real(F64), intent(out)                            :: D
                  real(F64), dimension(NVirt, NVirtPNO), intent(in) :: Px
                  real(F64), dimension(NVirt, NVirtPNO), intent(in) :: Qx
                  real(F64), dimension(NVirt, NVirtPNO), intent(in) :: Py
                  real(F64), dimension(NVirt, NVirtPNO), intent(in) :: Qy
                  integer, intent(in)                               :: i
                  integer, intent(in)                               :: j

                  real(F64) :: Dy
                  integer :: y
                  
                  call real_aTb(Sxy, Qx, Py)
                  call real_ab(PSy, Px, Sxy)
                  call real_ab(Ygx, Yga, PSy)
                  call real_ab(Ygy, Yga, Qy)
                  Wg = ZERO
                  do y = 1, NVirtPNO
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
