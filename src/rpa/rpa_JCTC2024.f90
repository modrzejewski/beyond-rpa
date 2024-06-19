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

            real(F64), dimension(:, :), allocatable :: T2ajbi
            real(F64), dimension(:, :), allocatable :: Pam, Qam
            real(F64), dimension(:, :), allocatable :: Dab
            real(F64), dimension(:, :), allocatable :: QabPNO
            real(F64), dimension(:), allocatable :: LambdaPNO
            real(F64), dimension(:, :), allocatable :: YgaPNO
            real(F64), dimension(:, :), allocatable :: VabijPNO
            real(F64), dimension(:, :, :), allocatable :: UaimPNO
            real(F64), dimension(:, :), allocatable :: YYgabPNO, ZYYkabPNO
            real(F64), dimension(:), allocatable :: XXgij, ZXXkij, A2m
            real(F64), dimension(:, :), allocatable :: TajckPNO, TaickPNO
            real(F64), dimension(:, :), allocatable :: TakciPNO, TakcjPNO
            real(F64), dimension(:, :), allocatable :: GabPNO2i, GabPNO2h, GabPNO2g
            real(F64), dimension(:, :, :), allocatable :: UaimLoc
            real(F64), dimension(:, :), allocatable :: XgiLoc, Lik
            integer :: i, j, k, mu
            real(F64) :: Ec2i, Ec2h, Ec2g
            integer :: NVecsT2, NCholesky, NGridTHC, NOcc, NVirt
            integer :: NVirtPNO, MaxNVirtPNO
            integer(I64) :: SumNVirtPNO
            integer :: AverageNVirtPNO
            logical, parameter :: LocalizedOrbitals = .true.
            logical :: PNOAllocated
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
            MaxNVirtPNO = NVirt

            allocate(T2ajbi(NVirt, NVirt))
            allocate(Pam(NVirt, NVecsT2))
            allocate(Qam(NVirt, NVecsT2))
            allocate(Dab(NVirt, NVirt))
            allocate(QabPNO(NVirt, NVirt))
            allocate(LambdaPNO(NVirt))
            allocate(A2m(NVecsT2))
            allocate(XXgij(NGridTHC))
            allocate(ZXXkij(NCholesky))            
            !
            ! Transformation to the localized occupied orbital
            ! basis
            !
            allocate(UaimLoc(NVirt, NOcc, NVecsT2))
            allocate(XgiLoc(NGridTHC, NOcc))
            if (LocalizedOrbitals) then
                  allocate(Lik(NOcc, NOcc))
                  call rpa_LocalizedOrbitals(Lik, Cpi, NOcc, RPAParams, AOBasis)
                  call real_ab(XgiLoc, Xgi, Lik)
                  !$omp parallel do private(mu)
                  do mu = 1, NVecsT2
                        call real_ab(UaimLoc(:, :, mu), Uaim(:, :, mu), Lik)
                  end do
                  !$omp end parallel do
            else
                  XgiLoc = Xgi
                  UaimLoc = Uaim
            end if
            A2m(:) = Am(:)**2
            Ec2i = ZERO
            Ec2h = ZERO
            Ec2g = ZERO
            SumNVirtPNO = 0
            MaxNVirtPNO = 0
            PNOAllocated = .false.
            do j = 1, NOcc
                  do i = j, NOcc
                        !
                        ! Pair natural orbitals of the occupied pair i, j
                        !
                        call rpa_JCTC2024_Tabij(Dab, Pam, Qam, UaimLoc, A2m, i, j, &
                              NOcc, NVirt, NVecsT2)
                        if (i /= j) then
                              call rpa_JCTC2024_Tabij(T2ajbi, Pam, Qam, UaimLoc, A2m, j, i, &
                                    NOcc, NVirt, NVecsT2)
                              Dab(:, :) = Dab(:, :) + T2ajbi(:, :)
                              Dab = Dab / TWO
                        end if
                        call symmetric_eigenproblem(LambdaPNO, Dab, NVirt, .true.)
                        NVirtPNO = 0
                        do k = 1, NVirt
                              if (abs(LambdaPNO(k)) > RPAParams%CutoffThreshPNO) then
                                    NVirtPNO = NVirtPNO + 1
                                    QabPNO(:, NVirtPNO) = Dab(:, k)
                              end if
                        end do
                        SumNVirtPNO = SumNVirtPNO + NVirtPNO
                        if (NVirtPNO > 0) then
                              if (NVirtPNO > MaxNVirtPNO) then
                                    if (PNOAllocated) then
                                          deallocate(GabPNO2g, VabijPNO, UaimPNO, &
                                                YgaPNO, YYgabPNO, ZYYkabPNO, TajckPNO, &
                                                TaickPNO, TakciPNO, TakcjPNO, GabPNO2i, &
                                                GabPNO2h)
                                    end if
                                    allocate(VabijPNO(NVirtPNO, NVirtPNO))
                                    allocate(UaimPNO(NVirtPNO, NOcc, NVecsT2))
                                    allocate(YgaPNO(NGridTHC, NVirtPNO))
                                    allocate(YYgabPNO(NGridTHC, NVirtPNO))
                                    allocate(ZYYkabPNO(NCholesky, NVirtPNO))
                                    allocate(TajckPNO(NVirtPNO, NVirt))
                                    allocate(TaickPNO(NVirtPNO, NVirt))
                                    allocate(TakciPNO(NVirtPNO, NVirt))
                                    allocate(TakcjPNO(NVirtPNO, NVirt))
                                    allocate(GabPNO2i(NVirtPNO, NVirtPNO))
                                    allocate(GabPNO2h(NVirtPNO, NVirtPNO))
                                    allocate(GabPNO2g(NVirtPNO, NVirtPNO))
                                    PNOAllocated = .true.
                                    MaxNVirtPNO = NVirtPNO
                              end if
                              call rpa_JCTC2024_2ghij_PNO(Ec2g, Ec2h, Ec2i, TajckPNO, TaickPNO, TakciPNO, &
                                    TakcjPNO, GabPNO2i, GabPNO2h, GabPNO2g, VabijPNO, UaimPNO, YgaPNO, &
                                    XXgij, ZXXkij, YYgabPNO, ZYYkabPNO, Pam, Qam, i, j, Zgk, UaimLoc, Am, A2m, XgiLoc, &
                                    Yga, QabPNO, NOcc, NVirtPNO, NVirt, NVecsT2, NGridTHC, NCholesky)
                        end if
                  end do
            end do
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_2G) = Ec2g
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_2H) = Ec2h
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_2I) = Ec2i
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_2J) = Ec2i

            AverageNVirtPNO = nint(real(SumNVirtPNO,F64)/(NOcc*(NOcc+1)/2))
            call blankline()
            call msg("Calculation of particle-hole corrections completed")
            call msg("Total time             " // str(clock_readwall(timer),d=1) // " seconds")
            call msg("AverageNVirtPNO        " // str(AverageNVirtPNO))
            call msg("MaxNVirtPNO            " // str(MaxNVirtPNO))
            call msg("AverageNVirtPNO/NVirt  " // str(real(AverageNVirtPNO,F64)/NVirt,d=1))
            call blankline()
      end subroutine rpa_JCTC2024_Corrections


      subroutine rpa_JCTC2024_2ghij_PNO(Ec2g, Ec2h, Ec2i, TajckPNO, TaickPNO, TakciPNO, &
            TakcjPNO, GabPNO2i, GabPNO2h, GabPNO2g, VabijPNO, UaimPNO, YgaPNO, &
            XXgij, ZXXkij, YYgabPNO, ZYYkabPNO, Pam, Qam, i, j, Zgk, UaimMO, Am, A2m, Xgi, Yga, &
            QabPNO, NOcc, NVirtPNO, NVirt, NVecsT2, NGridTHC, NCholesky)

            integer, intent(in)                                        :: NOcc
            integer, intent(in)                                        :: NVirtPNO
            integer, intent(in)                                        :: NVirt
            integer, intent(in)                                        :: NVecsT2
            integer, intent(in)                                        :: NGridTHC
            integer, intent(in)                                        :: NCholesky
            real(F64), intent(inout)                                   :: Ec2g
            real(F64), intent(inout)                                   :: Ec2h
            real(F64), intent(inout)                                   :: Ec2i
            real(F64), dimension(NVirtPNO, NVirt), intent(out)         :: TajckPNO
            real(F64), dimension(NVirtPNO, NVirt), intent(out)         :: TaickPNO
            real(F64), dimension(NVirtPNO, NVirt), intent(out)         :: TakciPNO
            real(F64), dimension(NVirtPNO, NVirt), intent(out)         :: TakcjPNO
            real(F64), dimension(NVirtPNO, NVirtPNO), intent(out)      :: GabPNO2i
            real(F64), dimension(NVirtPNO, NVirtPNO), intent(out)      :: GabPNO2h
            real(F64), dimension(NVirtPNO, NVirtPNO), intent(out)      :: GabPNO2g
            real(F64), dimension(NVirtPNO, NVirtPNO), intent(out)      :: VabijPNO
            real(F64), dimension(NVirtPNO, NOcc, NVecsT2), intent(out) :: UaimPNO
            real(F64), dimension(NGridTHC, NVirtPNO), intent(out)      :: YgaPNO
            real(F64), dimension(NGridTHC), intent(out)                :: XXgij
            real(F64), dimension(NCholesky), intent(out)               :: ZXXkij
            real(F64), dimension(NGridTHC, NVirtPNO), intent(out)      :: YYgabPNO
            real(F64), dimension(NCholesky, NVirtPNO), intent(out)     :: ZYYkabPNO
            real(F64), dimension(NVirt, NVecsT2), intent(out)          :: Pam
            real(F64), dimension(NVirt, NVecsT2), intent(out)          :: Qam
            integer, intent(in)                                        :: i, j
            real(F64), dimension(NGridTHC, NCholesky), intent(in)      :: Zgk
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in)     :: UaimMO
            real(F64), dimension(NVecsT2), intent(in)                  :: Am
            real(F64), dimension(NVecsT2), intent(in)                  :: A2m
            real(F64), dimension(NGridTHC, NOcc), intent(in)           :: Xgi
            real(F64), dimension(NGridTHC, NVirt), intent(in)          :: Yga
            real(F64), dimension(NVirt, NVirtPNO), intent(in)          :: QabPNO

            integer :: k
            real(F64) :: S2g, S2h, S2i
            
            call real_ab(YgaPNO, Yga, QabPNO)
            call real_aTb_x(UaimPNO, NVirtPNO, QabPNO, NVirt, UaimMO, NVirt, &
                  NVirtPNO, NOcc*NVecsT2, NVirt)
            call rpa_JCTC2024_Vabij(VabijPNO, XXgij, ZXXkij, YYgabPNO, ZYYkabPNO, &
                  Zgk, Xgi, YgaPNO, i, j, NOcc, NVirtPNO, NGridTHC, NCholesky)
            
            GabPNO2h = ZERO
            GabPNO2i = ZERO
            do k = 1, NOcc
                  call rpa_JCTC2024_Tabij_PNO_MO(TajckPNO, Pam, Qam, UaimPNO, UaimMO, Am, &
                        j, k, NOcc, NVirtPNO, NVirt, NVecsT2)
                  call rpa_JCTC2024_Tabij_PNO_MO(TakciPNO, Pam, Qam, UaimPNO, UaimMO, Am, &
                        k, i, NOcc, NVirtPNO, NVirt, NVecsT2)
                  !
                  ! 2i contrib to the cumulant: G2i(A,B) = Sum(k)Sum(c) T(Aj,ck)*T(Bk,ci)
                  !
                  call real_abT_x(GabPNO2i, NVirtPNO, TajckPNO, NVirtPNO, TakciPNO, NVirtPNO, &
                        NVirtPNO, NVirtPNO, NVirt, ONE, ONE)
                  if (i /= j) then
                        call rpa_JCTC2024_Tabij_PNO_MO(TaickPNO, Pam, Qam, UaimPNO, UaimMO, Am, &
                              i, k, NOcc, NVirtPNO, NVirt, NVecsT2)
                        call rpa_JCTC2024_Tabij_PNO_MO(TakcjPNO, Pam, Qam, UaimPNO, UaimMO, Am, &
                              k, j, NOcc, NVirtPNO, NVirt, NVecsT2)
                        !
                        ! 2h contrib to the cumulant: G2h(A,B) = Sum(k)Sum(c) T(Ak,cj)*T(Bk,ci)
                        ! note the factor of 2 which takes into account the permutation i <-> j
                        !
                        call real_abT_x(GabPNO2h, NVirtPNO, TakcjPNO, NVirtPNO, TakciPNO, NVirtPNO, &
                              NVirtPNO, NVirtPNO, NVirt, ONE, ONE)
                        !
                        ! 2i contrib to the cumulant: G2i(A,B) = Sum(k)Sum(c) T(Ai,ck)*T(Bk,cj)
                        !
                        call real_abT_x(GabPNO2i, NVirtPNO, TaickPNO, NVirtPNO, TakcjPNO, NVirtPNO, &
                              NVirtPNO, NVirtPNO, NVirt, ONE, ONE)
                  else
                        !
                        ! 2h contrib to the cumulant: G2h(A,B) = Sum(k)Sum(c) T(Ak,cj)*T(Bk,ci)
                        ! diagonal case i = j
                        !
                        call real_abT_x(GabPNO2h, NVirtPNO, TakciPNO, NVirtPNO, TakciPNO, NVirtPNO, &
                              NVirtPNO, NVirtPNO, NVirt, ONE, ONE)
                  end if
            end do
            !
            ! 2g contrib to the cumulant G2g(A,B) = Sum(ck) T(Aj,ck)*T(Bi,ck) = (T**2)(Aj,Bi)
            !
            call rpa_JCTC2024_Tabij_PNO_PNO(GabPNO2g, Pam, Qam, UaimPNO, A2m, &
                  i, j, NOcc, NVirtPNO, NVecsT2)            
            call real_vw_x(S2g, VabijPNO, GabPNO2g, NVirtPNO**2)
            call real_vw_x(S2h, VabijPNO, GabPNO2h, NVirtPNO**2)
            call real_vw_x(S2i, VabijPNO, GabPNO2i, NVirtPNO**2)
            if (i /= j) then
                  S2g = TWO * S2g
                  S2h = TWO * S2h
            end if
            Ec2g = Ec2g - FOUR * S2g
            Ec2h = Ec2h - FOUR * S2h
            Ec2i = Ec2i + TWO * S2i
      end subroutine rpa_JCTC2024_2ghij_PNO

      

      ! subroutine rpa_JCTC2024_2ghij_v2(RPAOutput, Zgk, Xgi, Yga, Uaim, Am, Cpi, &
      !       RPAParams, AOBasis)
            
      !       type(TRPAOutput), intent(inout)                        :: RPAOutput
      !       real(F64), dimension(:, :), intent(in)                 :: Zgk
      !       real(F64), dimension(:, :), intent(in)                 :: Xgi
      !       real(F64), dimension(:, :), intent(in)                 :: Yga
      !       real(F64), dimension(:, :, :), intent(in)              :: Uaim
      !       real(F64), dimension(:), intent(in)                    :: Am
      !       real(F64), dimension(:, :), intent(in)                 :: Cpi
      !       type(TRPAParams), intent(in)                           :: RPAParams
      !       type(TAOBasis), intent(in)                             :: AOBasis

      !       real(F64), dimension(:, :), allocatable :: Tajck, Tcibk
      !       real(F64), dimension(:, :), allocatable :: TVcb
      !       real(F64), dimension(:, :), allocatable :: Vabij, YYgab, ZYYkab
      !       real(F64), dimension(:, :), allocatable :: Pam, Qam
      !       real(F64), dimension(:, :, :), allocatable :: UaimLoc
      !       real(F64), dimension(:, :), allocatable :: XgiLoc, Lik
      !       real(F64), dimension(:), allocatable :: XXgij, ZXXkij, A2m
      !       integer :: i, j, k, mu
      !       real(F64) :: S2i, S2h, S2g
      !       real(F64) :: Ec2i, Ec2h, Ec2g
      !       integer :: NVecsT2, NCholesky, NGridTHC, NOcc, NVirt
      !       logical, parameter :: LocalizedOrbitals = .true.
      !       real(F64) :: VabijMax
      !       integer :: a, b
            
      !       NGridTHC = size(Zgk, dim=1)
      !       NCholesky = size(Zgk, dim=2)
      !       NVecsT2 = size(Am)
      !       NOcc = size(Xgi, dim=2)
      !       NVirt = size(Yga, dim=2)

      !       allocate(Vabij(NVirt, NVirt))
      !       allocate(Tajck(NVirt, NVirt))
      !       allocate(Tcibk(NVirt, NVirt))
      !       allocate(XXgij(NGridTHC))
      !       allocate(ZXXkij(NCholesky))
      !       allocate(YYgab(NGridTHC, NVirt))
      !       allocate(ZYYkab(NCholesky, NVirt))
      !       allocate(Pam(NVirt, NVecsT2))
      !       allocate(Qam(NVirt, NVecsT2))
      !       allocate(TVcb(NVirt, NVirt))
      !       allocate(A2m(NVecsT2))
      !       !
      !       ! Transformation to the localized occupied orbital
      !       ! basis
      !       !
      !       allocate(UaimLoc(NVirt, NOcc, NVecsT2))
      !       allocate(XgiLoc(NGridTHC, NOcc))
      !       if (LocalizedOrbitals) then
      !             call msg("Basis for Ec2g, Ec2h, Ec2i, Ec2j: localized occupied orbitals")
      !             allocate(Lik(NOcc, NOcc))
      !             call rpa_LocalizedOrbitals(Lik, Cpi, NOcc, RPAParams, AOBasis)
      !             call real_ab(XgiLoc, Xgi, Lik)
      !             !$omp parallel do private(mu)
      !             do mu = 1, NVecsT2
      !                   call real_ab(UaimLoc(:, :, mu), Uaim(:, :, mu), Lik)
      !             end do
      !             !$omp end parallel do
      !       else
      !             call msg("Basis for Ec2g, Ec2h, Ec2i, Ec2j: canonical orbitals")
      !             XgiLoc = Xgi
      !             UaimLoc = Uaim
      !       end if
      !       A2m(:) = Am(:)**2
      !       Ec2i = ZERO
      !       Ec2h = ZERO
      !       Ec2g = ZERO
      !       do j = 1, NOcc
      !             do i = j, NOcc
      !                   call rpa_JCTC2024_Vabij(Vabij, XXgij, ZXXkij, YYgab, ZYYkab, &
      !                         Zgk, XgiLoc, Yga, i, j, NOcc, NVirt, NGridTHC, NCholesky)
      !                   VabijMax = ZERO
      !                   do b = 1, NVirt
      !                         do a = 1, NVirt
      !                               VabijMax = max(abs(Vabij(a, b)), VabijMax)
      !                         end do
      !                   end do
      !                   call msg("Maximum Vabij: " // str(VabijMax,d=1))
      !                   do k = 1, NOcc
      !                         call rpa_JCTC2024_Tabij(Tajck, Pam, Qam, UaimLoc, Am, j, k, &
      !                               NOcc, NVirt, NVecsT2)
      !                         if (i /= j) then
      !                               call rpa_JCTC2024_Tabij(Tcibk, Pam, Qam, UaimLoc, Am, i, k, &
      !                                     NOcc, NVirt, NVecsT2)
      !                         else
      !                               Tcibk = Tajck
      !                         end if
      !                         !
      !                         ! Ec2i
      !                         ! Step 1: Sum(a) T(aj,ck)*(ab|ij)
      !                         ! Step 2: Sum(cb) (Sum(a)T(aj,ck)*(ab|ij))*T(ci,bk)
      !                         !
      !                         call real_aTb(TVcb, Tajck, Vabij)
      !                         call real_vw_x(S2i, TVcb, Tcibk, NVirt**2)
      !                         Ec2i = Ec2i + TWO * S2i
      !                         if (i /= j) then
      !                               associate (TVac => TVcb, Taick => Tcibk, Tcjbk => Tajck)
      !                                     !
      !                                     ! Permutation i <-> j
      !                                     ! Step 1: Sum(b) (ab|ij)*T(cj,bk)
      !                                     ! Step 2: Sum(ac) T(ai,ck)*(Sum(b)(ab|ij)*T(cj,bk))
      !                                     !
      !                                     call real_abT(TVac, Vabij, Tcjbk)
      !                                     call real_vw_x(S2i, Taick, TVac, NVirt**2)
      !                                     Ec2i = Ec2i + TWO * S2i
      !                               end associate
      !                         end if
      !                         !
      !                         ! Ec2h
      !                         ! Step 1: Sum(a) T(cj,ak)*(ab|ij)
      !                         ! Step 2: Sum(cb) (Sum(a)T(cj,ak)*(ab|ij))*T(ci,bk)
      !                         !
      !                         call real_ab(TVcb, Tajck, Vabij)
      !                         call real_vw_x(S2h, TVcb, Tcibk, NVirt**2)
      !                         if (i /= j) then
      !                               Ec2h = Ec2h - FOUR * TWO * S2h
      !                         else
      !                               Ec2h = Ec2h - FOUR * S2h
      !                         end if
      !                   end do
      !                   ! -------------------------------------------
      !                   !                      Ec2g
      !                   ! -------------------------------------------
      !                   associate (T2ajbi => Tajck)
      !                         call rpa_JCTC2024_Tabij(T2ajbi, Pam, Qam, UaimLoc, A2m, j, i, &
      !                               NOcc, NVirt, NVecsT2)
      !                         call real_vw_x(S2g, Vabij, T2ajbi, NVirt**2)
      !                         if (i /= j) then
      !                               Ec2g = Ec2g - FOUR * TWO * S2g
      !                         else
      !                               Ec2g = Ec2g - FOUR * S2g
      !                         end if
      !                         call msg("--------- singular values of T2(aj,bi)**2 --------------")
                              
      !                         call msg("--------------------------------------------------------")
      !                   end associate
      !             end do
      !       end do
      !       RPAOutput%Energy(RPA_ENERGY_CUMULANT_2G) = Ec2g
      !       RPAOutput%Energy(RPA_ENERGY_CUMULANT_2H) = Ec2h
      !       RPAOutput%Energy(RPA_ENERGY_CUMULANT_2I) = Ec2i
      !       RPAOutput%Energy(RPA_ENERGY_CUMULANT_2J) = Ec2i
      ! end subroutine rpa_JCTC2024_2ghij_v2
      

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


      subroutine rpa_JCTC2024_Tabij_PNO_MO(Tabij, Pam, Qam, UaimPNO, UaimMO, Am, &
            i, j, NOcc, NVirtPNO, NVirt, NVecsT2)
            
            integer, intent(in)                                       :: NOcc
            integer, intent(in)                                       :: NVirtPNO
            integer, intent(in)                                       :: NVirt
            integer, intent(in)                                       :: NVecsT2
            real(F64), dimension(NVirtPNO, NVirt), intent(out)        :: Tabij
            real(F64), dimension(NVirtPNO, NVecsT2), intent(out)      :: Pam
            real(F64), dimension(NVirt, NVecsT2), intent(out)         :: Qam
            real(F64), dimension(NVirtPNO, NOcc, NVecsT2), intent(in) :: UaimPNO
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in)    :: UaimMO
            real(F64), dimension(NVecsT2), intent(in)                 :: Am
            integer, intent(in)                                       :: i
            integer, intent(in)                                       :: j

            integer :: mu

            !$omp parallel do private(mu)
            do mu = 1, NVecsT2
                  Pam(:, mu) = UaimPNO(:, i, mu)
                  Qam(:, mu) = UaimMO(:, j, mu) * Am(mu)
            end do
            !$omp end parallel do
            call real_abT(Tabij, Pam, Qam)
      end subroutine rpa_JCTC2024_Tabij_PNO_MO


      subroutine rpa_JCTC2024_Tabij_PNO_PNO(Tabij, PamPNO, QamPNO, UaimPNO, Am, &
            i, j, NOcc, NVirtPNO, NVecsT2)
            
            integer, intent(in)                                       :: NOcc, NVirtPNO, NVecsT2
            real(F64), dimension(NVirtPNO, NVirtPNO), intent(out)     :: Tabij
            real(F64), dimension(NVirtPNO, NVecsT2), intent(out)      :: PamPNO
            real(F64), dimension(NVirtPNO, NVecsT2), intent(out)      :: QamPNO
            real(F64), dimension(NVirtPNO, NOcc, NVecsT2), intent(in) :: UaimPNO
            real(F64), dimension(NVecsT2), intent(in)                 :: Am
            integer, intent(in)                                       :: i
            integer, intent(in)                                       :: j

            integer :: mu

            !$omp parallel do private(mu)
            do mu = 1, NVecsT2
                  PamPNO(:, mu) = UaimPNO(:, i, mu)
                  QamPNO(:, mu) = UaimPNO(:, j, mu) * Am(mu)
            end do
            !$omp end parallel do
            call real_abT(Tabij, PamPNO, QamPNO)
      end subroutine rpa_JCTC2024_Tabij_PNO_PNO


      subroutine rpa_JCTC2024_Vabij(Vabij, XXgij, ZXXkij, YYgab, ZYYkab, Zgk, Xgi, Yga, &
            i, j, NOcc, NVirt, NGridTHC, NCholesky)
            
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


      ! subroutine rpa_JCTC2024_Vabij_v2(Vabij, XXgij, ZXXkij, YYgab, ZYYkab, Zgk, Xgi, Yga, Ygb, &
      !       i, j, NOcc, NVirt, NGridTHC, NCholesky)
            
      !       integer, intent(in)                                   :: NOcc
      !       integer, intent(in)                                   :: NVirt
      !       integer, intent(in)                                   :: NGridTHC
      !       integer, intent(in)                                   :: NCholesky
      !       real(F64), dimension(NVirt, NVirt), intent(out)       :: Vabij
      !       real(F64), dimension(NGridTHC), intent(out)           :: XXgij
      !       real(F64), dimension(NCholesky), intent(out)          :: ZXXkij
      !       real(F64), dimension(NGridTHC, NVirt), intent(out)    :: YYgab
      !       real(F64), dimension(NCholesky, NVirt), intent(out)   :: ZYYkab
      !       real(F64), dimension(NGridTHC, NCholesky), intent(in) :: Zgk
      !       real(F64), dimension(NGridTHC, NOcc), intent(in)      :: Xgi
      !       real(F64), dimension(NGridTHC, NVirt), intent(in)     :: Yga
      !       real(F64), dimension(NGridTHC, NVirt), intent(in)     :: Ygb
      !       integer, intent(in)                                   :: i
      !       integer, intent(in)                                   :: j
            
      !       integer :: a, b

      !       XXgij(:) = Xgi(:, i) * Xgi(:, j)
      !       call real_ATv(ZXXkij, Zgk, XXgij)
      !       do b = 1, NVirt
      !             !$omp parallel do private(a)
      !             do a = 1, NVirt
      !                   YYgab(:, a) = Yga(:, a) * Ygb(:, b)
      !             end do
      !             !$omp end parallel do
      !             call real_aTb(ZYYkab, Zgk, YYgab)
      !             call real_ATv(Vabij(:, b), ZYYkab, ZXXkij)
      !       end do
      ! end subroutine rpa_JCTC2024_Vabij_v2
end module rpa_JCTC2024
