module rpa_CC_1RDM
      use arithmetic
      use rpa_definitions
      use math_constants
      use display
      use real_linalg
      use rpa_CC_Singles
      use matexp

      implicit none

contains

      subroutine rpa_CC_Rho_Summary(DensityApprox, Purify1RDM, Ec1RDMApprox)
            integer, intent(in) :: DensityApprox
            integer, intent(in) :: Purify1RDM
            integer, intent(in) :: Ec1RDMApprox

            call msg("1-RDM contribution (Ec1RDM)")
            select case (Ec1RDMApprox)
            case (RPA_Ec1RDM_LINEAR)
                  call msg(lfield("", 30) // "linear contribution")
            case (RPA_Ec1RDM_QUADRATIC)
                  call msg(lfield("", 30) // "linear and quadratic contributions")
            case (RPA_Ec1RDM_NATURAL_REFERENCE)
                  call msg(lfield("", 30) // "linear and quadratic contributions wrt natural orbital reference")
            case (RPA_Ec1RDM_NONE)
                  call msg(lfield("", 30) // "not included")
            end select

            call msg("Lambda-dependent 1-RDM")
            select case (DensityApprox)
            case (RPA_RHO_T1_LINEAR)
                  call msg(lfield("", 30) // "Rho=2*T1 (blocks ov, vo)")
            case (RPA_RHO_T1_QUADRATIC)
                  call msg(lfield("", 30) // "T1 through quadratic terms (blocks ov, vo, oo, vv)")
            case (RPA_RHO_T1_EXPONENTIAL)
                  call msg(lfield("", 30) // "Rho(Lambda)=exp(-X)*Rho(0)*exp(X)")
                  call msg(lfield("", 30) // "X=Sum(ai) Tai(Ci*Ca**T-Ca*Ci**T)")
                  call msg(lfield("", 30) // "(blocks ov, vo, oo, vv)")
            case (RPA_RHO_S1_EXPONENTIAL)
                  call msg(lfield("", 30) // "Rho(Lambda)=exp(-X)*Rho(0)*exp(X)")
                  call msg(lfield("", 30) // "X=Sum(ai) Sai(Ci*Ca**T-Ca*Ci**T)")
                  call msg(lfield("", 30) // "S1=T1(1-2T2)**(-1)")
                  call msg(lfield("", 30) // "(blocks ov, vo, oo, vv)")
            case (RPA_RHO_OFF_DIAGONAL_drCCSD)
                  call msg(lfield("", 30) // "infinite order direct ring Rho=2T1(1-2T2)**(-1) (blocks ov, vo)")
            case (RPA_RHO_drCCSD)
                  call msg(lfield("", 30) // "infinite order direct ring, up to quadratic terms in diagonal blocks")
                  call msg(lfield("", 30) // "S1=T1(1-2T2)**(-1)")
                  call msg(lfield("", 30) // "S2=T2/(1-4T2**2)")
                  call msg(lfield("", 30) // "Rho(ij) =-4Sum(a)(S2T2)aiaj-2Sum(a)Tai*Saj (block oo)")
                  call msg(lfield("", 30) // "Rho(ab) = 4Sum(i)(S2T2)aibi+2Sum(i)Tai*Sbi (block vv)")
                  call msg(lfield("", 30) // "Rho(ai) = 2*Sai (blocks vo, ov)")
            case (RPA_RHO_drCCSD_PLUS_EXCHANGE)
                  call msg(lfield("", 30) // "infinite order direct ring, up to quadratic terms in diagonal blocks")
                  call msg(lfield("", 30) // "lowest-order exchange term in blocks ov, vo")
                  call msg(lfield("", 30) // "S1=T1(1-2T2)**(-1)")
                  call msg(lfield("", 30) // "S2=T2/(1-4T2**2)")
                  call msg(lfield("", 30) // "Rho(ij) =-4Sum(a)(S2T2)aiaj-2Sum(a)Tai*Saj (block oo)")
                  call msg(lfield("", 30) // "Rho(ab) = 4Sum(i)(S2T2)aibi+2Sum(i)Tai*Sbi (block vv)")
                  call msg(lfield("", 30) // "Rho(ai) = 2*Sai-2Sum(bj)Tbj*Tajbi (blocks vo, ov)")
            end select

            if (Purify1RDM == RPA_PURIFY_RHO_KLIMES2015) then
                  call msg(lfield("", 30) // "method of N-representability purification:")
                  call msg(lfield("", 30) // "reconstruction from NOcc natural orbitals (Klimes et al. J. Chem. Phys. 143, 102816 (2015))")
            else if (Purify1RDM == RPA_PURIFY_RHO_CANCES_PERNAL2008) then
                  call msg(lfield("", 30) // "method of N-representability purification:")
                  call msg(lfield("", 30) // "projection algorithm, Eq. 17 in Cances and Pernal J. Chem. Phys. 128, 134108 (2008)")
            else
                  call msg(lfield("", 30) // "method of N-representability purification: none")
            end if
      end subroutine rpa_CC_Rho_Summary
      

  subroutine rpa_CC_Rho_drCCD(Rho_mo_oo, Rho_mo_vv, Ak, V, NVecsT2, i0, i1, a0, a1)
        !
        ! Compute the correlated part of the RPA/CCD density matrix
        ! in the MO basis.
        !
        real(F64), dimension(:, :), intent(inout)               :: Rho_mo_oo
        real(F64), dimension(:, :), intent(inout)               :: Rho_mo_vv
        real(F64), dimension(NVecsT2), intent(in)               :: Ak
        integer, intent(in)                                     :: i0, i1
        integer, intent(in)                                     :: a0, a1
        real(F64), dimension(NVecsT2, a0:a1, i0:i1), intent(in) :: V
        integer, intent(in)                                     :: NVecsT2

        integer :: a, b
        integer :: i, j, k
        real(F64) :: t
        real(F64), dimension(:), allocatable :: Bk
        !
        ! Eigenvalues of 4T^2/(1-4T^2)
        !
        allocate(Bk(NVecsT2))
        Bk(:) = ZERO
        do k=1, NVecsT2
              Bk(k) = FOUR * Ak(k)**2 / (ONE - FOUR * Ak(k)**2)
        end do

        do j = i0, i1
              do i = i0, i1
                    t = ZERO
                    do a = a0, a1
                          do k = 1, NVecsT2
                                t = t - V(k, a, i) * Bk(k) * V(k, a, j)
                          end do
                    end do
                    Rho_mo_oo(i, j) = Rho_mo_oo(i, j) + t
              end do
        end do

        do b = a0, a1
              do a = a0, a1
                    t = ZERO
                    do i = i0, i1
                          do k = 1, NVecsT2
                                t = t + V(k, a, i) * Bk(k) * V(k, b, i)
                          end do
                    end do
                    Rho_mo_vv(a, b) = Rho_mo_vv(a, b) + t
              end do
        end do
  end subroutine rpa_CC_Rho_drCCD


  subroutine rpa_CC_Rho_S1_Exponential(Rho_mo_vo, Rho_mo_oo, Rho_mo_vv, T1, Ak, Vkai, NVecsT2, NOcc, NVirt)
           real(F64), dimension(NVirt, NOcc), intent(out)          :: Rho_mo_vo
           real(F64), dimension(NOcc, NOcc), intent(out)           :: Rho_mo_oo
           real(F64), dimension(NVirt, NVirt), intent(out)         :: Rho_mo_vv
           real(F64), dimension(NVirt, NOcc), intent(in)           :: T1
           real(F64), dimension(NVecsT2), intent(in)               :: Ak
           real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)  :: Vkai
           integer, intent(in)                                     :: NVecsT2
           integer, intent(in)                                     :: NOcc
           integer, intent(in)                                     :: NVirt

           real(F64), dimension(:, :), allocatable :: S1

           allocate(S1(NVirt, NOcc))
           call rpa_CC_S1_drCCSD(S1, T1, Ak, Vkai, NVecsT2, NOcc, NVirt)
           call rpa_CC_Rho_T1_Exponential(Rho_mo_vo, Rho_mo_oo, Rho_mo_vv, S1, NOcc, NVirt)
     end subroutine rpa_CC_Rho_S1_Exponential

  
     subroutine rpa_CC_Rho_T1(Rho_mo_vo, Rho_mo_oo, Rho_mo_vv, T1, NOcc, NVirt, DensityApprox)
           real(F64), dimension(NVirt, NOcc), intent(out)  :: Rho_mo_vo
           real(F64), dimension(NOcc, NOcc), intent(out)   :: Rho_mo_oo
           real(F64), dimension(NVirt, NVirt), intent(out) :: Rho_mo_vv
           real(F64), dimension(NVirt, NOcc), intent(in)   :: T1
           integer, intent(in)                             :: NOcc
           integer, intent(in)                             :: NVirt
           integer, intent(in)                             :: DensityApprox

           if (DensityApprox == RPA_RHO_T1_EXPONENTIAL) then
                 call rpa_CC_Rho_T1_Exponential(Rho_mo_vo, Rho_mo_oo, Rho_mo_vv, T1, NOcc, NVirt)
           else
                 Rho_mo_vo(:, :) = TWO * T1(:, :)
                 Rho_mo_oo = ZERO
                 Rho_mo_vv = ZERO
                 if (DensityApprox == RPA_RHO_T1_QUADRATIC) then
                       call real_aTb_x(Rho_mo_oo, NOcc, T1, NVirt, T1, NVirt, NOcc, NOcc, NVirt, -TWO, ONE)
                       call real_abT_x(Rho_mo_vv, NVirt, T1, NVirt, T1, NVirt, NVirt, NVirt, NOcc, TWO, ONE)
                 end if
           end if
     end subroutine rpa_CC_Rho_T1


     subroutine rpa_CC_S1_drCCSD(S1, T1, Ak, Vkai, NVecsT2, NOcc, NVirt)
           real(F64), dimension(NVirt, NOcc), intent(out)          :: S1
           real(F64), dimension(NVirt, NOcc), intent(in)           :: T1
           real(F64), dimension(NVecsT2), intent(in)               :: Ak
           real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)  :: Vkai
           integer, intent(in)                                     :: NVecsT2
           integer, intent(in)                                     :: NOcc
           integer, intent(in)                                     :: NVirt
           
           integer :: k
           real(F64), dimension(:), allocatable :: T1V
           
           allocate(T1V(NVecsT2))
           !
           ! T1V(1:NVecsT2) = V(1:NVecsT2, 1:NVirt:NOcc) * T1(1:NVirt*NOcc)
           !
           call real_av_x(T1V, Vkai, NVecsT2, T1, NVecsT2, NVirt*NOcc, ONE, ZERO)
           do k = 1, NVecsT2
                 T1V(k) = T1V(k) * ONE/(ONE-TWO*Ak(k))
           end do
           call real_aTv_x(S1, Vkai, NVecsT2, T1V, NVecsT2, NVirt*NOcc, ONE, ZERO)
     end subroutine rpa_CC_S1_drCCSD
     
     
     subroutine rpa_CC_Rho_ringCCSD(Rho_mo_vo, Rho_mo_oo, Rho_mo_vv, T1, Ak, Vkai, NVecsT2, NOcc, NVirt, Exchange)
           real(F64), dimension(NVirt, NOcc), intent(out)          :: Rho_mo_vo
           real(F64), dimension(NOcc, NOcc), intent(out)           :: Rho_mo_oo
           real(F64), dimension(NVirt, NVirt), intent(out)         :: Rho_mo_vv
           real(F64), dimension(NVirt, NOcc), intent(in)           :: T1
           real(F64), dimension(NVecsT2), intent(in)               :: Ak
           real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)  :: Vkai
           integer, intent(in)                                     :: NVecsT2
           integer, intent(in)                                     :: NOcc
           integer, intent(in)                                     :: NVirt
           logical, intent(in)                                     :: Exchange

           real(F64), dimension(:, :), allocatable :: S1
           real(F64), dimension(:, :), allocatable :: W

           allocate(S1(NVirt, NOcc))
           call rpa_CC_S1_drCCSD(S1, T1, Ak, Vkai, NVecsT2, NOcc, NVirt)
           Rho_mo_vo = TWO * S1
           call real_aTb_x(Rho_mo_oo, NOcc, T1, NVirt, S1, NVirt, NOcc, NOcc, NVirt, -TWO, ONE)
           call real_abT_x(Rho_mo_vv, NVirt, T1, NVirt, S1, NVirt, NVirt, NVirt, NOcc, TWO, ONE)
           call rpa_CC_Rho_drCCD(Rho_mo_oo, Rho_mo_vv, Ak, Vkai, NVecsT2, 1, NOcc, 1, NVirt)
           if (Exchange) then
                 allocate(W(NVecsT2, NOcc))
                 associate (T2T1x => S1)
                       !
                       ! Lowest-order OV-VO exchange contribution to DeltaRho
                       !
                       ! T2T1x(ai) = 
                       ! DeltaRho(ai) <- -2*Sum(bj) T1(bj)T2(aj,bi)
                       !
                       call rpa_CC_Singles_T2F_Exchange(T2T1x, T1, Vkai, Ak, NOcc, NVirt, NVecsT2, W)
                       Rho_mo_vo = Rho_mo_vo - TWO * T2T1x
                 end associate
           end if
     end subroutine rpa_CC_Rho_ringCCSD


     subroutine rpa_CC_Rho_T1_Exponential(DeltaRho_vo, DeltaRho_oo, DeltaRho_vv, T1, NOcc, NVirt)
           !
           ! Compute an idempotent density matrix
           !
           ! Rho' = exp(-X) Rho exp(X) 
           !
           ! where Rho is diagonal the antisymmetric rotation matrix is built from T1 amplitudes
           !
           ! X = Sum(ai) T(ai) (Ci*Ca**T - Ca*Ci**T)
           !
           real(F64), dimension(NVirt, NOcc), intent(out)          :: DeltaRho_vo
           real(F64), dimension(NOcc, NOcc), intent(out)           :: DeltaRho_oo
           real(F64), dimension(NVirt, NVirt), intent(out)         :: DeltaRho_vv
           real(F64), dimension(NVirt, NOcc), intent(in)           :: T1
           integer, intent(in)                                     :: NOcc
           integer, intent(in)                                     :: NVirt

           real(F64), dimension(:, :), allocatable :: X, ExpMinusX, W
           integer :: NMO
           integer :: a, i
           integer :: i0, i1, a0, a1
           integer :: aa, ii

           NMO = NOcc + NVirt
           allocate(X(NMO, NMO))
           allocate(ExpMinusX(NMO, NMO))
           allocate(W(NMO, NMO))
           i0 = 1
           i1 = NOcc
           a0 = NOcc + 1
           a1 = NOcc + NVirt
           !
           ! X = Sum(ai) T(ai) (Ci*Ca**T - Ca*Ci**T)
           !
           X = ZERO
           do i = 1, NOcc
                 do a = 1, NVirt
                       aa = a0 + a - 1
                       ii = i0 + i - 1
                       X(aa, ii) = -T1(a, i)
                       X(ii, aa) = T1(a, i)
                 end do
           end do
           !
           ! Compute exp(-X)
           !
           X = -X
           call matrix_exponential_real(ExpMinusX, X, W)
           associate ( &
                 Cocc => ExpMinusX(:, 1:NOcc), &
                 D => X &
                 )
                 !
                 ! C' = exp(-X) C
                 ! Cocc' = exp(-X) Cocc = [exp(-X)](:, 1:NOcc)
                 ! D' = exp(-X)*Cocc*Cocc**T*Exp(X) = Cocc'*Cocc'**T = [exp(-X)](:, 1:NOcc) * [exp(-X)](:, 1:NOcc)**T
                 !
                 call real_abT(D, Cocc, Cocc)
                 !
                 ! DeltaD = D(Lambda)-D(0)
                 !
                 do i = 1, NOcc
                       D(i, i) = D(i, i) - ONE
                 end do
                 DeltaRho_vo = TWO * D(a0:a1, i0:i1)
                 DeltaRho_oo = TWO * D(i0:i1, i0:i1)
                 DeltaRho_vv = TWO * D(a0:a1, a0:a1)
           end associate
     end subroutine rpa_CC_Rho_T1_Exponential
     

     subroutine rpa_CC_Rho_OffDiag_drCCSD(Rho_mo_vo, T1, Ak, V, NVecsT2, NOcc, NVirt)
           !
           ! Compute the off-diagonal (ov) block of Rho(Lambda)-Rho(Lambda=0) in the infinite order
           ! direct ring approximation including mixed T1*T2 terms.
           ! 
           !
           real(F64), dimension(:, :), intent(out)                 :: Rho_mo_vo
           real(F64), dimension(NVirt, NOcc), intent(in)           :: T1
           real(F64), dimension(NVecsT2), intent(in)               :: Ak
           real(F64), dimension(NVecsT2, NVirt, NOcc), intent(in)  :: V
           integer, intent(in)                                     :: NVecsT2
           integer, intent(in)                                     :: NOcc
           integer, intent(in)                                     :: NVirt

           integer :: k
           real(F64), dimension(:), allocatable :: T1V

           allocate(T1V(NVecsT2))
           !
           ! T1V(1:NVecsT2) = V(1:NVecsT2, 1:NVirt:NOcc) * T1(1:NVirt*NOcc)
           !
           call real_av_x(T1V, V, NVecsT2, T1, NVecsT2, NVirt*NOcc, ONE, ZERO)
           do k = 1, NVecsT2
                 T1V(k) = T1V(k) * TWO/(ONE-TWO*Ak(k))
           end do
           call real_aTv_x(Rho_mo_vo, V, NVecsT2, T1V, NVecsT2, NVirt*NOcc, ONE, ZERO)
     end subroutine rpa_CC_Rho_OffDiag_drCCSD


     subroutine rpa_CC_Pernal2008_Eq17(OccNumbers, MaxOccNumber, TargetTrace, TargetRelError)
           !
           ! Purification of 1-RDM by projection onto N-representable density matrices
           ! according to Eq. 17 in Cances and Pernal JCP 128, 134108 (2008)
           !
           real(F64), dimension(:), intent(inout) :: OccNumbers
           real(F64), intent(in)                  :: MaxOccNumber
           real(F64), intent(in)                  :: TargetTrace
           real(F64), intent(in)                  :: TargetRelError

           real(F64) :: Trace
           real(F64) :: MuLeft, MuRight, Mu
           integer :: k
           logical :: converged
           integer :: MaxNIters
           real(F64), parameter :: BracketStep = 0.01_F64

           Mu = ZERO
           call rpa_CC_Pernal2008_Trace(Trace, OccNumbers, Mu, MaxOccNumber)
           if (abs(Trace-TargetTrace) >= abs(TargetTrace * TargetRelError)) then
                 MaxNIters = ceiling(MaxOccNumber / BracketStep)
                 converged = .false.
                 if (Trace-TargetTrace > ZERO) then
                       MuRight = ZERO
                       do k = 1, MaxNIters
                             MuLeft = -k * BracketStep
                             call rpa_CC_Pernal2008_Trace(Trace, OccNumbers, MuLeft, MaxOccNumber)
                             if (Trace < TargetTrace) then
                                   converged = .true.
                                   exit
                             end if
                       end do
                 else
                       MuLeft = ZERO
                       do k = 1, MaxNIters
                             MuRight = k * BracketStep
                             call rpa_CC_Pernal2008_Trace(Trace, OccNumbers, MuRight, MaxOccNumber)
                             if (Trace > TargetTrace) then
                                   converged = .true.
                                   exit
                             end if
                       end do
                 end if
                 if (.not. converged) then
                       call msg("Bracketing subroutine in rpa_CC_Pernal2008_Eq17 did not converge", MSG_ERROR)
                       error stop
                 end if
                 call rpa_CC_Pernal2008_Bisection(Mu, MuLeft, MuRight, OccNumbers, MaxOccNumber, TargetTrace, TargetRelError)
           end if
           do k = 1, size(OccNumbers)
                 if (OccNumbers(k) + Mu < MaxOccNumber) then
                       if (OccNumbers(k) + Mu > ZERO) then
                             OccNumbers(k) = OccNumbers(k) + Mu
                       else
                             OccNumbers(k) = ZERO
                       end if
                 else
                       OccNumbers(k) = MaxOccNumber
                 end if
           end do
     end subroutine rpa_CC_Pernal2008_Eq17

     
     subroutine rpa_CC_Pernal2008_Trace(Trace, OccNumbers, Mu, MaxOccNumber)
           real(F64), intent(out)              :: Trace
           real(F64), dimension(:), intent(in) :: OccNumbers
           real(F64), intent(in)               :: Mu
           real(F64), intent(in)               :: MaxOccNumber

           integer :: NOrb, k
           
           Trace = ZERO
           NOrb = size(OccNumbers)
           do k = 1, NOrb
                 if (OccNumbers(k) + Mu < MaxOccNumber) then
                       if (OccNumbers(k) + Mu > ZERO) then
                             Trace = Trace + OccNumbers(k) + Mu
                       end if
                 else
                       Trace = Trace + MaxOccNumber
                 end if
           end do
     end subroutine rpa_CC_Pernal2008_Trace


     subroutine rpa_CC_Pernal2008_Bisection(Mu, MuLeft, MuRight, OccNumbers, MaxOccNumber, TargetTrace, TargetRelError)
           real(F64), intent(out)              :: Mu
           real(F64), intent(inout)            :: MuLeft
           real(F64), intent(inout)            :: MuRight
           real(F64), dimension(:), intent(in) :: OccNumbers
           real(F64), intent(in)               :: MaxOccNumber
           real(F64), intent(in)               :: TargetTrace
           real(F64), intent(in)               :: TargetRelError

           real(F64) :: Trace, TraceLeft, TraceRight
           logical :: converged
           integer :: k
           integer, parameter :: MaxNIters = 100

           call rpa_CC_Pernal2008_Trace(TraceLeft, OccNumbers, MuLeft, MaxOccNumber)
           call rpa_CC_Pernal2008_Trace(TraceRight, OccNumbers, MuRight, MaxOccNumber)
           converged = .false.
           bisection: do k = 1, MaxNIters
                 if (abs(TraceLeft - TraceRight) < abs(TargetTrace * TargetRelError)) then
                       converged = .true.
                       exit bisection
                 end if
                 Mu = (MuLeft + MuRight) / TWO
                 call rpa_CC_Pernal2008_Trace(Trace, OccNumbers, Mu, MaxOccNumber)
                 if (Trace > TargetTrace) then
                       MuRight = Mu
                       TraceRight = Trace
                 else
                       MuLeft = Mu
                       TraceLeft = Trace
                 end if
           end do bisection
           if (.not. converged) then
                 call msg("The purification of RPA 1-RDM has not converged", MSG_ERROR)
                 error stop
           end if
     end subroutine rpa_CC_Pernal2008_Bisection


     subroutine rpa_CC_Enforce_NRepresentability(Rho_mo_vo, Rho_mo_oo, Rho_mo_vv, NOcc, NVirt, NACPoints, Method)
            real(F64), dimension(NVirt, NOcc, NACPoints), intent(inout)  :: Rho_mo_vo
            real(F64), dimension(NOcc, NOcc, NACPoints), intent(inout)   :: Rho_mo_oo
            real(F64), dimension(NVirt, NVirt, NACPoints), intent(inout) :: Rho_mo_vv
            integer, intent(in)                                          :: NOcc
            integer, intent(in)                                          :: NVirt
            integer, intent(in)                                          :: NACPoints
            integer, intent(in)                                          :: Method

            real(F64), dimension(:, :), allocatable :: Rho_full
            real(F64), dimension(:, :), allocatable :: Rho_reconstructed
            real(F64), dimension(:), allocatable :: OccNumbers
            integer :: NMO
            integer :: i0, i1
            integer :: a0, a1
            integer :: k, l
            real(F64), parameter :: TargetRelError = 1.0E-6_F64

            if (Method == RPA_PURIFY_RHO_NONE) then
                  return
            end if
            NMO = NOcc + NVirt
            i0 = 1
            i1 = NOcc
            a0 = NOcc + 1
            a1 = NOcc + NVirt
            allocate(Rho_full(NMO, NMO))
            allocate(OccNumbers(NMO))
            allocate(Rho_reconstructed(NMO, NMO))
            do k = 1, NACPoints
                  Rho_full = ZERO
                  Rho_full(i0:i1, i0:i1) = Rho_mo_oo(:, :, k)
                  Rho_full(a0:a1, a0:a1) = Rho_mo_vv(:, :, k)
                  Rho_full(a0:a1, i0:i1) = Rho_mo_vo(:, :, k)
                  do l = 1, NOcc
                        Rho_full(l, l) = Rho_full(l, l) + TWO
                  end do
                  call symmetric_eigenproblem(OccNumbers, Rho_full, NMO, .true. )
                  if (Method == RPA_PURIFY_RHO_KLIMES2015) then
                        call real_abT(Rho_reconstructed, Rho_full(:, NMO-NOcc+1:NMO), Rho_full(:, NMO-NOcc+1:NMO))
                        Rho_reconstructed = TWO * Rho_reconstructed
                  else
                        !
                        ! Method == RPA_PURIFY_RHO_CANCES_PERNAL2008
                        !
                        call rpa_CC_Pernal2008_Eq17(OccNumbers, TWO, NOcc*TWO, TargetRelError)
                        Rho_reconstructed = ZERO
                        do l = 1, NMO
                              call real_vwT(Rho_reconstructed, Rho_full(:, l), Rho_full(:, l), OccNumbers(l))
                        end do
                  end if
                  Rho_mo_oo(:, :, k) = Rho_reconstructed(i0:i1, i0:i1)
                  Rho_mo_vv(:, :, k) = Rho_reconstructed(a0:a1, a0:a1)
                  Rho_mo_vo(:, :, k) = Rho_reconstructed(a0:a1, i0:i1)
                  do l = 1, NOcc
                        Rho_mo_oo(l, l, k) = Rho_mo_oo(l, l, k) - TWO
                  end do
            end do
      end subroutine rpa_CC_Enforce_NRepresentability


     subroutine rpa_CC_DeltaRho(DeltaRho_vo, DeltaRho_oo, DeltaRho_vv, T1, A, V, DensityApprox, NOcc, NVirt, NVecsT2)
            real(F64), dimension(:, :), intent(out)   :: DeltaRho_vo
            real(F64), dimension(:, :), intent(out)   :: DeltaRho_oo
            real(F64), dimension(:, :), intent(out)   :: DeltaRho_vv
            real(F64), dimension(:, :), intent(in)    :: T1
            real(F64), dimension(:), intent(in)       :: A
            real(F64), dimension(:, :), intent(in)    :: V
            integer, intent(in)                       :: DensityApprox
            integer, intent(in)                       :: NOcc
            integer, intent(in)                       :: NVirt
            integer, intent(in)                       :: NVecsT2

            logical :: Exchange
            
            DeltaRho_vo = ZERO
            DeltaRho_oo = ZERO
            DeltaRho_vv = ZERO

            if (DensityApprox == RPA_RHO_S1_EXPONENTIAL) then
                  call rpa_CC_Rho_S1_Exponential(DeltaRho_vo, DeltaRho_oo, DeltaRho_vv, &
                        T1, A, V, NVecsT2, NOcc, NVirt)
            end if
            
            if (DensityApprox == RPA_RHO_OFF_DIAGONAL_drCCSD) then
                  call rpa_CC_Rho_OffDiag_drCCSD(DeltaRho_vo, T1, A, V, NVecsT2, NOcc, NVirt)
            end if
            
            if (DensityApprox == RPA_RHO_drCCSD .or. DensityApprox == RPA_RHO_drCCSD_PLUS_EXCHANGE) then
                  Exchange = (DensityApprox == RPA_RHO_drCCSD_PLUS_EXCHANGE)
                  call rpa_CC_Rho_ringCCSD(DeltaRho_vo, DeltaRho_oo, DeltaRho_vv, T1, A, V, &
                        NVecsT2, NOcc, NVirt, Exchange)
            end if

            if (DensityApprox == RPA_RHO_T1_LINEAR .or. &
                  DensityApprox == RPA_RHO_T1_QUADRATIC .or. &
                  DensityApprox == RPA_RHO_T1_EXPONENTIAL) then
                  !
                  ! Contributions to Rho(lambda)-Rho(lambda=0) built using only T1 amplitudes
                  !
                  call rpa_CC_Rho_T1(DeltaRho_vo, DeltaRho_oo, DeltaRho_vv, T1, &
                        NOcc, NVirt, DensityApprox)
            end if
      end subroutine rpa_CC_DeltaRho


      subroutine rpa_CC_NaturalOrbitals(OccCoeffs_ao, VirtCoeffs_ao, OccCoeffs_mo, VirtCoeffs_mo, &
            DeltaRho_mo_vo, DeltaRho_mo_oo, DeltaRho_mo_vv, NOcc, NVirt)
            real(F64), dimension(:, :), intent(inout)                 :: OccCoeffs_ao
            real(F64), dimension(:, :), intent(inout)                 :: VirtCoeffs_ao
            real(F64), dimension(NOcc+NVirt, NOcc), intent(out)       :: OccCoeffs_mo
            real(F64), dimension(NOcc+NVirt, NVirt), intent(out)      :: VirtCoeffs_mo
            real(F64), dimension(NVirt, NOcc), intent(in)             :: DeltaRho_mo_vo
            real(F64), dimension(NOcc, NOcc), intent(in)              :: DeltaRho_mo_oo
            real(F64), dimension(NVirt, NVirt), intent(in)            :: DeltaRho_mo_vv
            integer, intent(in)                                       :: NOcc
            integer, intent(in)                                       :: NVirt

            real(F64), dimension(:), allocatable :: OccNumbers
            real(F64), dimension(:, :), allocatable :: NaturalOrbitals_mo
            real(F64), dimension(:), allocatable :: V_mo
            real(F64), dimension(:, :), allocatable :: C_ao
            
            integer :: NMO, NAO
            integer :: i0, i1
            integer :: a0, a1
            integer :: l

            NMO = NOcc + NVirt
            NAO = size(OccCoeffs_ao, dim=1)
            i0 = 1
            i1 = NOcc
            a0 = NOcc + 1
            a1 = NOcc + NVirt
            allocate(OccNumbers(NMO))
            allocate(NaturalOrbitals_mo(NMO, NMO))
            NaturalOrbitals_mo = ZERO
            NaturalOrbitals_mo(i0:i1, i0:i1) = DeltaRho_mo_oo(:, :)
            NaturalOrbitals_mo(a0:a1, a0:a1) = DeltaRho_mo_vv(:, :)
            NaturalOrbitals_mo(a0:a1, i0:i1) = DeltaRho_mo_vo(:, :)
            do l = 1, NOcc
                  NaturalOrbitals_mo(l, l) = NaturalOrbitals_mo(l, l) + TWO
            end do
            call symmetric_eigenproblem(OccNumbers, NaturalOrbitals_mo, NMO, .true. )
            allocate(C_ao(NAO, NMO))
            C_ao(:, i0:i1) = OccCoeffs_ao(:, 1:NOcc)
            C_ao(:, a0:a1) = VirtCoeffs_ao(:, 1:NVirt)
            !
            ! Sort the natural orbitals from the largest to the smallest occupation number
            !
            allocate(V_mo(NMO))
            do l = 1, NMO/2
                  V_mo = NaturalOrbitals_mo(:, l)
                  NaturalOrbitals_mo(:, l) = NaturalOrbitals_mo(:, NMO-l+1)
                  NaturalOrbitals_mo(:, NMO-l+1) = V_mo
            end do
            OccCoeffs_mo = NaturalOrbitals_mo(:, i0:i1)
            VirtCoeffs_mo = NaturalOrbitals_mo(:, a0:a1)
            call real_ab(OccCoeffs_ao, C_ao, OccCoeffs_mo)
            call real_ab(VirtCoeffs_ao, C_ao, VirtCoeffs_mo)
      end subroutine rpa_CC_NaturalOrbitals
end module rpa_CC_1RDM
