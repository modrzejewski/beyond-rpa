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
            integer :: NVirtPNO
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
            call msg("Pair-natural orbitals cutoff: " // str(RPAParams%CutoffThreshPNO,d=1))
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
            ! Singular value decomposition of T(aI,bJ)
            !
            NVirtPNO = min(100, NVirt)
            NOccPairs = (NOcc * (NOcc + 1)) / 2
            allocate(LocIJ(3, NOcc, NOcc))
            allocate(TaxPNOij(NVirt, NVirtPNO, 2, NOccPairs))
            allocate(Sigma(NVirt))
            allocate(U(NVirt, NVirt))
            allocate(V(NVirt, NVirt))
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
            call msg("NVirtPNO               " // str(NVirtPNO))
            call msg("NVirt                  " // str(NVirt))
            call msg("NVirtPNO/NVirt         " // str(real(NVirtPNO,F64)/NVirt,d=1))
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
            real(F64), dimension(:, :), allocatable :: YXgai, ZYXkai, ZYXkbj
            real(F64), dimension(:), allocatable :: XXgij, ZXXkij
            real(F64), dimension(:, :), allocatable :: Sxy
            real(F64), dimension(:, :), allocatable :: Vay, Vxy, Vabij, Vaibj
            real(F64) :: S1b, S2bij, S2bji, S2d, S2g, S2h, S2i
            real(F64) :: Weight
            integer :: i, j, k
            integer :: IJ, IJp, IJq
            integer :: IK, IKp, IKq
            integer :: JK, JKp, JKq
            integer :: KJ, KJp, KJq
            integer :: KI, KIp, KIq
            
            allocate(Ygx(NGridTHC, NVirtPNO))
            allocate(Ygy(NGridTHC, NVirtPNO))
            allocate(Vaibj(NVirtPNO, NVirtPNO))
            allocate(Vay(NVirt, NVirtPNO))
            allocate(Vxy(NVirtPNO, NVirtPNO))
            allocate(Vabij(NVirt, NVirt))
            allocate(Sxy(NVirtPNO, NVirtPNO))
            allocate(YXgai(NGridTHC, NVirt))
            allocate(XXgij(NGridTHC))
            allocate(ZXXkij(NCholesky))
            allocate(ZYXkai(NCholesky, NVirt))
            allocate(ZYXkbj(NCholesky, NVirt))
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
                        call rpa_JCTC2024_Vabij(Vabij, XXgij, ZXXkij, &
                              YXgai, ZYXkai, Zgk, Xgi, Yga, i, j, NOcc, &
                              NVirt, NGridTHC, NCholesky)
                        IJ = LocIJ(1, i, j)
                        IJp = LocIJ(2, i, j)
                        IJq = LocIJ(3, i, j)
                        if (IJ /= 0) then
                              call real_ab(Ygx, Yga, TaxPNOij(:, :, IJq, IJ))
                              call real_ab(Ygy, Yga, TaxPNOij(:, :, IJp, IJ))
                              call rpa_JCTC2024_TrVaibj(S1b, YXgai, ZYXkai, ZYXkbj, Zgk, Xgi, Ygx, Ygy, &
                                    i, j, NOcc, NVirtPNO, NGridTHC, NCholesky)
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
                              call VxyijDotSxy(S2g, TaxPNOij(:, :, JKp, JK), TaxPNOij(:, :, JKq, JK), &
                                    TaxPNOij(:, :, KIp, KI), TaxPNOij(:, :, KIq, KI), i, j)
                              Ec2g = Ec2g - FOUR * Weight * S2g
                              call VxiyjDotSxy(S2d, TaxPNOij(:, :, KIp, KI), TaxPNOij(:, :, KIq, KI), &
                                    TaxPNOij(:, :, JKp, JK), TaxPNOij(:, :, JKq, JK), i, j)
                              Ec2d = Ec2d + TWO * Weight * S2d
                              call VxiyjDotSxy(S2bij, TaxPNOij(:, :, IKp, IK), TaxPNOij(:, :, IKq, IK), &
                                    TaxPNOij(:, :, JKp, JK), TaxPNOij(:, :, JKq, JK), i, j)
                              if (i /= j) then
                                    call VxiyjDotSxy(S2bji, TaxPNOij(:, :, JKp, JK), TaxPNOij(:, :, JKq, JK), &
                                          TaxPNOij(:, :, IKp, IK), TaxPNOij(:, :, IKq, IK), j, i)
                                    Ec2b = Ec2b - FOUR * (S2bij + S2bji)
                              else
                                    Ec2b = Ec2b - FOUR * S2bij
                              end if                              
                        end do
                  end do
            end do

      contains

            subroutine VxiyjDotSxy(D, Px, Qx, Py, Qy, i, j)
                  real(F64), intent(out)                            :: D
                  real(F64), dimension(NVirt, NVirtPNO), intent(in) :: Px
                  real(F64), dimension(NVirt, NVirtPNO), intent(in) :: Qx
                  real(F64), dimension(NVirt, NVirtPNO), intent(in) :: Py
                  real(F64), dimension(NVirt, NVirtPNO), intent(in) :: Qy
                  integer, intent(in)                               :: i
                  integer, intent(in)                               :: j

                  call real_ab(Ygx, Yga, Px)
                  call real_ab(Ygy, Yga, Qy)
                  call real_aTb(Sxy, Qx, Py)
                  call rpa_JCTC2024_Vaibj(Vaibj, YXgai, ZYXkai, ZYXkbj, &
                        Zgk, Xgi, Ygx, Ygy, i, j, NOcc, NVirtPNO, &
                        NGridTHC, NCholesky)
                  call real_vw_x(D, Vaibj, Sxy, NVirtPNO**2)
            end  subroutine VxiyjDotSxy

            
            subroutine VxyijDotSxy(D, Px, Qx, Py, Qy, i, j)
                  real(F64), intent(out)                            :: D
                  real(F64), dimension(NVirt, NVirtPNO), intent(in) :: Px
                  real(F64), dimension(NVirt, NVirtPNO), intent(in) :: Qx
                  real(F64), dimension(NVirt, NVirtPNO), intent(in) :: Py
                  real(F64), dimension(NVirt, NVirtPNO), intent(in) :: Qy
                  integer, intent(in)                               :: i
                  integer, intent(in)                               :: j

                  call real_ab(Vay, Vabij, Qy)
                  call real_aTb(Vxy, Px, Vay)
                  call real_aTb(Sxy, Qx, Py)
                  call real_vw_x(D, Vxy, Sxy, NVirtPNO**2)
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


      subroutine rpa_JCTC2024_Vaibj(Vaibj, YXgai, ZYXkai, ZYXkbj, Zgk, Xgi, Yga, Ygb, &
            i, j, NOcc, NVirt, NGridTHC, NCholesky)
            
            integer, intent(in)                                   :: NOcc
            integer, intent(in)                                   :: NVirt
            integer, intent(in)                                   :: NGridTHC
            integer, intent(in)                                   :: NCholesky
            real(F64), dimension(NVirt, NVirt), intent(out)       :: Vaibj
            real(F64), dimension(NGridTHC, NVirt), intent(out)    :: YXgai
            real(F64), dimension(NCholesky, NVirt), intent(out)   :: ZYXkai
            real(F64), dimension(NCholesky, NVirt), intent(out)   :: ZYXkbj
            real(F64), dimension(NGridTHC, NCholesky), intent(in) :: Zgk
            real(F64), dimension(NGridTHC, NOcc), intent(in)      :: Xgi
            real(F64), dimension(NGridTHC, NVirt), intent(in)     :: Yga
            real(F64), dimension(NGridTHC, NVirt), intent(in)     :: Ygb
            integer, intent(in)                                   :: i
            integer, intent(in)                                   :: j
            
            integer :: a

            !$omp parallel do private(a)
            do a = 1, NVirt
                  YXgai(:, a) = Yga(:, a) * Xgi(:, i)
            end do
            !$omp end parallel do
            call real_aTb(ZYXkai, Zgk, YXgai)
            !$omp parallel do private(a)
            do a = 1, NVirt
                  YXgai(:, a) = Ygb(:, a) * Xgi(:, j)
            end do
            !$omp end parallel do
            call real_aTb(ZYXkbj, Zgk, YXgai)
            call real_aTb(Vaibj, ZYXkai, ZYXkbj)
      end subroutine rpa_JCTC2024_Vaibj


      subroutine rpa_JCTC2024_TrVaibj(TrVaibj, YXgai, ZYXkai, ZYXkbj, Zgk, Xgi, Yga, Ygb, &
            i, j, NOcc, NVirt, NGridTHC, NCholesky)
            
            integer, intent(in)                                   :: NOcc
            integer, intent(in)                                   :: NVirt
            integer, intent(in)                                   :: NGridTHC
            integer, intent(in)                                   :: NCholesky
            real(F64), intent(out)                                :: TrVaibj
            real(F64), dimension(NGridTHC, NVirt), intent(out)    :: YXgai
            real(F64), dimension(NCholesky, NVirt), intent(out)   :: ZYXkai
            real(F64), dimension(NCholesky, NVirt), intent(out)   :: ZYXkbj
            real(F64), dimension(NGridTHC, NCholesky), intent(in) :: Zgk
            real(F64), dimension(NGridTHC, NOcc), intent(in)      :: Xgi
            real(F64), dimension(NGridTHC, NVirt), intent(in)     :: Yga
            real(F64), dimension(NGridTHC, NVirt), intent(in)     :: Ygb
            integer, intent(in)                                   :: i
            integer, intent(in)                                   :: j
            
            integer :: a

            !$omp parallel do private(a)
            do a = 1, NVirt
                  YXgai(:, a) = Yga(:, a) * Xgi(:, i)
            end do
            !$omp end parallel do
            call real_aTb(ZYXkai, Zgk, YXgai)
            !$omp parallel do private(a)
            do a = 1, NVirt
                  YXgai(:, a) = Ygb(:, a) * Xgi(:, j)
            end do
            !$omp end parallel do
            call real_aTb(ZYXkbj, Zgk, YXgai)
            call real_vw_x(TrVaibj, ZYXkai, ZYXkbj, NCholesky*NVirt)
      end subroutine rpa_JCTC2024_TrVaibj


      subroutine rpa_JCTC2024_Vabij(Vabij, XXgij, ZXXkij, YYgab, ZYYkab, &
            Zgk, Xgi, Yga, i, j, NOcc, NVirt, NGridTHC, NCholesky)
            
            integer, intent(in)                                   :: NOcc
            integer, intent(in)                                   :: NVirt
            integer, intent(in)                                   :: NGridTHC
            integer, intent(in)                                   :: NCholesky
            real(F64), dimension(NVirt, NVirt), intent(out)       :: Vabij
            real(F64), dimension(NGridTHC), intent(out)           :: XXgij
            real(F64), dimension(NCholesky), intent(out)          :: ZXXkij
            real(F64), dimension(NGridTHC, NVirt), intent(out)    :: YYgab
            real(F64), dimension(NCholesky, NVirt), intent(out)   :: ZYYkab
            real(F64), dimension(NGridTHC, NCholesky), intent(in) :: Zgk
            real(F64), dimension(NGridTHC, NOcc), intent(in)      :: Xgi
            real(F64), dimension(NGridTHC, NVirt), intent(in)     :: Yga
            integer, intent(in)                                   :: i
            integer, intent(in)                                   :: j
            
            integer :: a, b

            XXgij(:) = Xgi(:, i) * Xgi(:, j)
            call real_ATv(ZXXkij, Zgk, XXgij)
            do b = 1, NVirt
                  !$omp parallel do private(a)
                  do a = 1, NVirt
                        YYgab(:, a) = Yga(:, a) * Yga(:, b)
                  end do
                  !$omp end parallel do
                  call real_aTb(ZYYkab, Zgk, YYgab)
                  call real_ATv(Vabij(:, b), ZYYkab, ZXXkij)
            end do
      end subroutine rpa_JCTC2024_Vabij
end module rpa_JCTC2024
