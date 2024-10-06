module rpa_Orbitals
      use arithmetic
      use real_linalg
      use math_constants
      use display
      use string
      use OneElectronInts
      use basis_sets
      use rpa_definitions
      use sys_definitions
      use Multipoles
      use matexp

      implicit none
      
contains

      subroutine rpa_LocalizeOrbitals_FosterBoys(LijCL, Cpi, RPAParams, AOBasis)
            !
            ! Compute the orthogonal matrix LijCL for the transformation of orbitals:
            !
            ! canonical occupied orbitals (C) -> Foster-Boys localized occupied orbitals (L)
            !
            ! The localiation procedure is carried out with the Boys objective
            ! function. See Ref. 1 for a review of localization methods.
            !
            ! 1. Pipek, J. and Mezey, P.G. J. Chem. Phys. 90, 4916 (1989);
            !    doi: 10.1063/1.456588
            !
            real(F64), dimension(:, :), intent(out)    :: LijCL
            real(F64), dimension(:, :), intent(in)     :: Cpi
            type(TRPAParams), intent(in)               :: RPAParams
            type(TAOBasis), intent(in)                 :: AOBasis

            real(F64), dimension(:, :, :), allocatable :: RijCC, RijLL
            real(F64), dimension(:, :), allocatable :: R2ijCC
            real(F64), dimension(:, :), allocatable :: dFdKij
            real(F64), dimension(:, :), allocatable :: Wij
            real(F64), dimension(:, :), allocatable :: OptLijCL
            real(F64) :: F, OptF, InitF
            integer :: u, NIters
            integer :: NOcc
            logical :: Converged
            
            NOcc = size(LijCL, dim=2)
            allocate(OptLijCL(NOcc, NOcc))
            allocate(RijCC(NOcc, NOcc, 3))
            allocate(RijLL(NOcc, NOcc, 3))
            allocate(R2ijCC(NOcc, NOcc))
            allocate(dFdKij(NOcc, NOcc))
            allocate(Wij(NOcc, NOcc))
            call msg("Convergence threshold: Max(ij)|dF/dKij| < " // str(RPAParams%LocBoysConvergenceThresh,d=1))
            call msg("Max number of iters: " // str(RPAParams%LocBoysMaxNIters))
            call rpa_FosterBoys_Init(LijCL, RijCC, R2ijCC, Cpi, RPAParams, AOBasis)
            call rpa_FosterBoys_Transform(RijLL, Wij, RijCC, LijCL)
            call rpa_FosterBoys_ObjectiveFunction(F, RijLL, R2ijCC)
            InitF = F
            OptF = F
            Converged = .false.
            do u = 1, RPAParams%LocBoysMaxNIters
                  call rpa_FosterBoys_Sweep(LijCL, RijLL)
                  call rpa_FosterBoys_Transform(RijLL, Wij, RijCC, LijCL)
                  call rpa_FosterBoys_ObjectiveFunction(F, RijLL, R2ijCC)
                  call rpa_FosterBoys_Grad(dFdKij, Wij, RijLL, NOcc)
                  if (F < OptF) then
                        OptF = F
                        OptLijCL = LijCL
                  end if
                  if (maxval(dFdKij) < RPAParams%LocBoysConvergenceThresh) then
                        Converged = .true.
                        NIters = u
                        exit
                  end if
            end do
            if (Converged) then
                  call msg("Localization converged in " // str(NIters) // " iterations")
            else
                  call msg("Localization not converged")
            end if
            call msg("Objective function (initial Cholesky localization): " // str(InitF, d=1))
            call msg("Objective function (minimum):                       " // str(OptF, d=1))
            LijCL = OptLijCL
      end subroutine rpa_LocalizeOrbitals_FosterBoys


      subroutine rpa_FosterBoys_Sweep(LijCL, RijLL)
            !
            ! Perform a single sweep of two-dimensinal rotations to
            ! get an estimate of the orbital transformation matrix.
            !
            ! The orbital rotation procedure of the Boys localization
            ! is programmed according to paper of Pipek and Mezey.
            !
            ! 1. Pipek, J. and Mezey, P.G. J. Chem. Phys. 90, 4916 (1989);
            !    doi: 10.1063/1.456588
            !
            real(F64), dimension(:, :), intent(inout)    :: LijCL            
            real(F64), dimension(:, :, :), intent(inout) :: RijLL

            real(F64) :: Alpha, FourAlpha
            real(F64) :: Aij, Bij, D2, SinFourAlpha, CosFourAlpha
            real(F64) :: CosAlpha, SinAlpha
            real(F64), dimension(:), allocatable :: Wi, Wj, Wk
            integer :: x
            integer :: i, j
            integer :: NOcc

            NOcc = size(LijCL, dim=1)
            allocate(Wi(NOcc))
            allocate(Wj(NOcc))
            allocate(Wk(NOcc))
            do j = 1, NOcc
                  do i = j+1, NOcc
                        Aij = ZERO
                        Bij = ZERO
                        do x = 1, 3
                              !
                              ! Aij and Bij are defined in Eqs. 15a and 15b in Ref. 1
                              !
                              Aij = Aij + RijLL(i, j, x)**2 - (ONE/FOUR) * (RijLL(i, i, x) - RijLL(j, j, x))**2
                              Bij = Bij + RijLL(i, j, x) * (RijLL(i, i, x) - RijLL(j, j, x))
                        end do
                        D2 = Aij**2 + Bij**2
                        if (D2 > ZERO) then
                              !
                              ! Compute the mixing angle according to Eqs. 16 and 40 in Ref. 1.
                              ! Note that we are maximizing the efficient variant of the objective
                              ! function, i.e., Sum(i) <i|R|i>**2, denoted as B2 in Ref. 1.
                              !
                              SinFourAlpha = Bij / Sqrt(D2)
                              CosFourAlpha = -Aij / Sqrt(D2)
                              !
                              ! The value of Acos(4*Alpha) falls within the interval (0, Pi)
                              ! according to the standard of the programming language.
                              ! To solve the system of equations 13a and 13b in Ref. 1
                              ! we need to additionally check the sign of Sin(4*Alpha)
                              ! and shift the angle accordingly. As a result we get
                              ! 0 <= Alpha <= Pi/2, Eq. 13c in Ref. 1.
                              !
                              FourAlpha = Acos(max(-ONE, min(ONE, CosFourAlpha)))
                              if (SinFourAlpha < ZERO) FourAlpha = TWO*PI - FourAlpha
                              Alpha = FourAlpha / FOUR
                        else
                              Alpha = ZERO
                        end if
                        CosAlpha = Cos(Alpha)
                        SinAlpha = Sin(Alpha)
                        Wi = LijCL(:, i)
                        Wj = LijCL(:, j)
                        Wk = CosAlpha*Wi + SinAlpha*Wj
                        LijCL(:, i) = Wk
                        Wk = -SinAlpha*Wi + CosAlpha*Wj
                        LijCL(:, j) = Wk
                        do x = 1, 3
                              Wi = RijLL(:, i, x)
                              Wj = RijLL(:, j, x)
                              Wk = CosAlpha*Wi + SinAlpha*Wj
                              RijLL(:, i, x) = Wk
                              Wk = -SinAlpha*Wi + CosAlpha*Wj
                              RijLL(:, j, x) = Wk
                              Wi = RijLL(i, :, x)
                              Wj = RijLL(j, :, x)
                              Wk = CosAlpha*Wi + SinAlpha*Wj
                              RijLL(i, :, x) = Wk
                              Wk = -SinAlpha*Wi + CosAlpha*Wj
                              RijLL(j, :, x) = Wk
                        end do
                  end do
            end do
      end subroutine rpa_FosterBoys_Sweep
      

      ! subroutine rpa_FosterBoys_LineSearch(LijCL, ExpKij, Wij, Kij, &
      !       RijCL, R2ijCL, RijLL, R2ijLL, RijCC, R2ijCC, &
      !       Hij, F0, p, PolyOrder)
            
      !       real(F64), dimension(:, :), intent(inout)  :: LijCL
      !       real(F64), dimension(:, :), intent(out)    :: ExpKij
      !       real(F64), dimension(:, :), intent(out)    :: Wij
      !       real(F64), dimension(:, :), intent(out)    :: Kij
      !       real(F64), dimension(:, :, :), intent(out) :: RijCL
      !       real(F64), dimension(:, :), intent(out)    :: R2ijCL
      !       real(F64), dimension(:, :, :), intent(out) :: RijLL
      !       real(F64), dimension(:, :), intent(out)    :: R2ijLL
      !       real(F64), dimension(:, :, :), intent(in)  :: RijCC
      !       real(F64), dimension(:, :), intent(in)     :: R2ijCC
      !       real(F64), dimension(:, :), intent(in)     :: Hij
      !       real(F64), intent(in)                      :: F0
      !       integer, intent(in)                        :: p
      !       integer, intent(in)                        :: PolyOrder

      !       real(F64) :: Tmax, Tk, LambdaMin, LambdaMax, OmegaMax
      !       integer :: k, l
      !       real(F64), dimension(PolyOrder, 1) :: b
      !       real(F64), dimension(PolyOrder, PolyOrder) :: A
      !       integer :: Rank
      !       real(F64), parameter :: RCond = 1.0E-6_F64
      !       integer :: NOcc

      !       NOcc = size(Kij, dim=1)
      !       call real_GershgorinCircle(LambdaMin, LambdaMax, Hij)
      !       OmegaMax = Max(Abs(LambdaMin), Abs(LambdaMax))
      !       Tmax = TWO*Pi/(4*p*OmegaMax)
      !       do k = 1, PolyOrder
      !             Tk = k * (Tmax/PolyOrder)
      !             if (k == 1) then
      !                   Kij = Tk * Hij
      !                   call matrix_exponential_real(ExpKij, Kij, Wij)
      !                   call real_ab(Wij, LijCL, ExpKij)
      !             else
      !                   call real_ab(Kij, Wij, ExpKij)
      !                   Wij = Kij
      !             end if
      !             call rpa_FosterBoys_Transform(RijCL, R2ijCL, RijLL, R2ijLL, RijCC, R2ijCC, Wij)
      !             call rpa_FosterBoys_ObjectiveFunction(b(k, 1), RijLL, R2ijLL)
      !             do l = 1, PolyOrder
      !                   A(k, l) = Tk**l
      !             end do
      !       end do
      !       do k = 1, PolyOrder
      !             b(k, 1) = b(k, 1) - F0
      !       end do
      !       call real_LeastSquares(b, Rank, A, RCond)
      ! end subroutine rpa_FosterBoys_LineSearch


      subroutine rpa_FosterBoys_ObjectiveFunction(F, RijLL, R2ijLL)
            real(F64), intent(out)                     :: F
            real(F64), dimension(:, :, :), intent(in)  :: RijLL
            real(F64), dimension(:, :), intent(in)     :: R2ijLL

            integer :: i
            integer :: NOcc

            NOcc = size(RijLL, dim=1)
            F = ZERO
            !$omp parallel do private(i) reduction(+:F)
            do i = 1, NOcc
                  F = F + R2ijLL(i, i) - RijLL(i, i, 1)**2 - RijLL(i, i, 2)**2 - RijLL(i, i, 3)**2
            end do
            !$omp end parallel do
      end subroutine rpa_FosterBoys_ObjectiveFunction
      

      subroutine rpa_FosterBoys_Grad(dFdKij, dFdLij, RijLL, NOcc)
            !
            ! Compute the gradient matrix of the auxiliary objective function F':
            !
            ! F' = Sum(i) Sum(q) <i|Rq|i>**2
            ! dF'/dKij = Sum(mn) (dF'/dLmn)*(dLmn/dKij) at K = 0
            !
            ! where the orthogonal transformation matrix L
            ! is parametrized by the matrix exponential
            ! of an antisymmetric matrix K
            !
            ! |i> = Sum(j') |j'> Lj'i
            ! Lj'i = (Exp(K))j'i
            ! 
            ! Remarks:
            ! (1) The gradient is computed at K=0 and j' are
            ! the localized orbitals in the current iteration.
            ! (2) This is the gradient of the auxiliary funciton
            ! F', which is maximized (see the paper of Pipek and Mezey).
            ! The first term of the objective function
            ! F = Sum(i) <i|R2|i> - Sum(i) Sum(q) <i|Rq|i>**2
            ! is a trace and does not depend on the orthogonal transformation
            ! of orbitals. F is minimized when F' is maximized.
            !
            real(F64), dimension(:, :), intent(out)    :: dFdKij
            real(F64), dimension(:, :), intent(out)    :: dFdLij
            real(F64), dimension(:, :, :), intent(in)  :: RijLL
            integer, intent(in)                        :: NOcc

            integer :: i, x

            dFdLij = ZERO
            !$omp parallel do private(i, x)
            do i = 1, NOcc
                  dFdLij(:, i) = ZERO
                  do x = 1, 3
                        dFdLij(:, i) = dFdLij(:, i) +  RijLL(:, i, x) * (FOUR*RijLL(i, i, x))
                  end do
            end do
            !$omp end parallel do
            dFdKij = -transpose(dFdLij)
            dFdKij = dFdKij + dFdLij
      end subroutine rpa_FosterBoys_Grad


      ! subroutine rpa_FosterBoys_Grad(F, dFdKij, dFdLij, LijCL, RijLL, R2ijLL, NOcc)
      !       real(F64), intent(out)                     :: F
      !       real(F64), dimension(:, :), intent(out)    :: dFdKij
      !       real(F64), dimension(:, :), intent(out)    :: dFdLij
      !       real(F64), dimension(:, :), intent(in)     :: LijCL
      !       real(F64), dimension(:, :, :), intent(in)  :: RijLL
      !       real(F64), dimension(:, :), intent(in)     :: R2ijLL
      !       integer, intent(in)                        :: NOcc

      !       integer :: i, x

      !       F = ZERO
      !       dFdLij = ZERO
      !       !$omp parallel do private(i, x) reduction(+:F)
      !       do i = 1, NOcc
      !             F = F + R2ijLL(i, i) - RijLL(i, i, 1)**2 - RijLL(i, i, 2)**2 - RijLL(i, i, 3)**2
      !             dFdLij(:, i) = TWO * R2ijLL(:, i)
      !             do x = 1, 3
      !                   dFdLij(:, i) = dFdLij(:, i) - (FOUR*RijLL(i, i, x)) *  RijLL(:, i, x)
      !             end do
      !       end do
      !       !$omp end parallel do
      !       dFdKij = -transpose(dFdLij)
      !       dFdKij = dFdKij + dFdLij
      ! end subroutine rpa_FosterBoys_Grad


      ! subroutine rpa_FosterBoys_Grad(F, dFdKij, dFdLij, LijCL, RijLL, R2ijLL, RijCL, R2ijCL, NOcc)
      !       real(F64), intent(out)                     :: F
      !       real(F64), dimension(:, :), intent(out)    :: dFdKij
      !       real(F64), dimension(:, :), intent(out)    :: dFdLij
      !       real(F64), dimension(:, :), intent(in)     :: LijCL
      !       real(F64), dimension(:, :, :), intent(in)  :: RijLL
      !       real(F64), dimension(:, :), intent(in)     :: R2ijLL
      !       real(F64), dimension(:, :, :), intent(in)  :: RijCL
      !       real(F64), dimension(:, :), intent(in)     :: R2ijCL
      !       integer, intent(in)                        :: NOcc

      !       integer :: i, x

      !       F = ZERO
      !       dFdLij = ZERO
      !       !$omp parallel do private(i, x) reduction(+:F)
      !       do i = 1, NOcc
      !             F = F + R2ijLL(i, i) - RijLL(i, i, 1)**2 - RijLL(i, i, 2)**2 - RijLL(i, i, 3)**2
      !             dFdLij(:, i) = TWO * R2ijCL(:, i)
      !             do x = 1, 3
      !                   dFdLij(:, i) = dFdLij(:, i) - (FOUR*RijLL(i, i, x)) *  RijCL(:, i, x)
      !             end do
      !       end do
      !       !$omp end parallel do
      !       dFdKij = ZERO
      !       call real_aTb_x(dFdKij, NOcc, LijCL, NOcc, dFdLij, NOcc, &
      !             NOcc, NOcc, NOcc, ONE, ZERO)
      !       call real_aTb_x(dFdKij, NOcc, dFdLij, NOcc, LijCL, NOcc, &
      !             NOcc, NOcc, NOcc, -ONE, ONE)
      ! end subroutine rpa_FosterBoys_Grad

      
      subroutine rpa_FosterBoys_Transform(RijLL, Wij, RijCC, LijCL)
            real(F64), dimension(:, :, :), intent(out) :: RijLL
            real(F64), dimension(:, :), intent(out)    :: Wij
            real(F64), dimension(:, :, :), intent(in)  :: RijCC
            real(F64), dimension(:, :), intent(in)     :: LijCL

            integer :: x

            do x = 1, 3
                  call real_ab(Wij, RijCC(:, :, x), LijCL)
                  call real_aTb(RijLL(:, :, x), LijCL, Wij)
            end do
      end subroutine rpa_FosterBoys_Transform


      ! subroutine rpa_FosterBoys_Transform(RijCL, R2ijCL, RijLL, R2ijLL, RijCC, R2ijCC, LijCL)
      !       real(F64), dimension(:, :, :), intent(out) :: RijCL
      !       real(F64), dimension(:, :), intent(out)    :: R2ijCL
      !       real(F64), dimension(:, :, :), intent(out) :: RijLL
      !       real(F64), dimension(:, :), intent(out)    :: R2ijLL
      !       real(F64), dimension(:, :, :), intent(in)  :: RijCC
      !       real(F64), dimension(:, :), intent(in)     :: R2ijCC
      !       real(F64), dimension(:, :), intent(in)     :: LijCL

      !       integer :: x

      !       do x = 1, 3
      !             call real_ab(RijCL(:, :, x), RijCC(:, :, x), LijCL)
      !             call real_aTb(RijLL(:, :, x), LijCL, RijCL(:, :, x))
      !       end do
      !       call real_ab(R2ijCL, R2ijCC, LijCL)
      !       call real_aTb(R2ijLL, LijCL, R2ijCL)
      ! end subroutine rpa_FosterBoys_Transform


      
      
      subroutine rpa_FosterBoys_Init(LijCL, RijCC, R2ijCC, Cpi, RPAParams, AOBasis)
            real(F64), dimension(:, :), intent(out)    :: LijCL
            real(F64), dimension(:, :, :), intent(out) :: RijCC
            real(F64), dimension(:, :), intent(out)    :: R2ijCC
            real(F64), dimension(:, :), intent(in)     :: Cpi
            type(TRPAParams), intent(in)               :: RPAParams
            type(TAOBasis), intent(in)                 :: AOBasis

            real(F64), dimension(:, :, :), allocatable :: D
            real(F64), dimension(:, :, :), allocatable :: Q
            real(F64), dimension(:, :), allocatable :: TrQ
            real(F64), dimension(3), parameter :: Rc = [ZERO, ZERO, ZERO]
            real(F64), dimension(:, :), allocatable :: Wpi
            integer :: NAO, NOcc
            integer :: x

            NOcc = size(LijCL, dim=2)
            NAO = AOBasis%NAOSpher
            allocate(D(NAO, NAO, 3))
            call multi_ElectronicDipole(D(:, :, 1), D(:, :, 2), D(:, :, 3), Rc, AOBasis)
            allocate(Q(NAO, NAO, 6))
            call multi_ElectronicQuadrupole(Q(:, :, 1), Q(:, :, 2), Q(:, :, 3), &
                  Q(:, :, 4), Q(:, :, 5), Q(:, :, 6), Rc, AOBasis)
            allocate(TrQ(NAO, NAO))
            TrQ = Q(:, :, 4) + Q(:, :, 5) + Q(:, :, 6)
            allocate(Wpi(NAO, NOcc))
            do x = 1, 3
                  call real_ab(Wpi, D(:, :, x), Cpi)
                  call real_aTb(RijCC(:, :, x), Cpi, Wpi)
            end do
            call real_ab(Wpi, TrQ, Cpi)
            call real_aTb(R2ijCC, Cpi, Wpi)
            call rpa_LocalizeOrbitals_AquilanteJCP2006(LijCL, Cpi, NOcc, RPAParams, AOBasis)
      end subroutine rpa_FosterBoys_Init


      !       subroutine rpa_FosterBoys_Init(LijCL, RijCC, R2ijCC, Cpi, RPAParams, AOBasis)
      !       real(F64), dimension(:, :), intent(out)    :: LijCL
      !       real(F64), dimension(:, :, :), intent(out) :: RijCC
      !       real(F64), dimension(:, :), intent(out)    :: R2ijCC
      !       real(F64), dimension(:, :), intent(in)     :: Cpi
      !       type(TRPAParams), intent(in)               :: RPAParams
      !       type(TAOBasis), intent(in)                 :: AOBasis

      !       real(F64), dimension(:, :, :), allocatable :: D
      !       real(F64), dimension(:, :, :), allocatable :: Q
      !       real(F64), dimension(:, :), allocatable :: TrQ
      !       real(F64), dimension(3), parameter :: Rc = [ZERO, ZERO, ZERO]
      !       real(F64), dimension(:, :), allocatable :: Wpi
      !       integer :: NAO, NOcc
      !       integer :: x

      !       NOcc = size(LijCL, dim=2)
      !       NAO = AOBasis%NAOSpher
      !       allocate(D(NAO, NAO, 3))
      !       call multi_ElectronicDipole(D(:, :, 1), D(:, :, 2), D(:, :, 3), Rc, AOBasis)
      !       allocate(Q(NAO, NAO, 6))
      !       call multi_ElectronicQuadrupole(Q(:, :, 1), Q(:, :, 2), Q(:, :, 3), &
      !             Q(:, :, 4), Q(:, :, 5), Q(:, :, 6), Rc, AOBasis)
      !       allocate(TrQ(NAO, NAO))
      !       TrQ = Q(:, :, 4) + Q(:, :, 5) + Q(:, :, 6)
      !       allocate(Wpi(NAO, NOcc))
      !       do x = 1, 3
      !             call real_ab(Wpi, D(:, :, x), Cpi)
      !             call real_aTb(RijCC(:, :, x), Cpi, Wpi)
      !       end do
      !       call real_ab(Wpi, TrQ, Cpi)
      !       call real_aTb(R2ijCC, Cpi, Wpi)
      !       call rpa_LocalizeOrbitals_AquilanteJCP2006(LijCL, Cpi, NOcc, RPAParams, AOBasis)
      ! end subroutine rpa_FosterBoys_Init



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
            CholeskyThresh = RPAParams%LocCholeskyLinDepThresh * MaxDpp
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

            integer :: a
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
            
            integer :: mu, i, j
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
end module rpa_Orbitals
