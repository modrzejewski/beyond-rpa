module OptLaplaceQuad
      use arithmetic
      use math_constants
      use quadratures
      use display

      implicit none

contains

      subroutine rpa_LaplaceQuad(x, w, NPoints, Freqs, NFreqs, daiValues, &
            daiWeights, MaxNPoints, TargetRelError, TargetRMSD, GridLimitDai)

            real(F64), dimension(:, :), intent(out) :: x
            real(F64), dimension(:, :), intent(out) :: w
            integer, dimension(:), intent(out)      :: NPoints
            real(F64), dimension(:), intent(in)     :: Freqs
            integer, intent(in)                     :: NFreqs
            real(F64), dimension(:, :), intent(in)  :: daiValues
            real(F64), dimension(:, :), intent(in)  :: daiWeights
            integer, intent(in)                     :: MaxNPoints
            real(F64), intent(in)                   :: TargetRelError
            real(F64), intent(in)                   :: TargetRMSD
            logical, intent(in)                     :: GridLimitDai

            integer :: NLowFreqs, NHiFreqs
            integer :: u
            character(:), allocatable :: line
            real(F64) :: daiMin, daiMax
            real(F64) :: omega
            integer :: NBins, NDaiSets
            integer :: NPointsAv
            integer :: QuadType
            real(F64), dimension(:), allocatable :: MaxRelError, RMSD

            allocate(MaxRelError(MaxNPoints))
            allocate(RMSD(MaxNPoints))
            NBins = size(daiValues, dim=1)
            NDaiSets = size(daiValues, dim=2)
            daiMin = minval(daiValues(1, :))
            daiMax = maxval(daiValues(NBins, :))
            call msg("Optimizing quadratures for the Laplace transform of f(dai)=dai/(dai**2+u**2)")
            call msg("daiMin: " // str(daiMin, d=3))
            call msg("daiMax: " // str(daiMax, d=3))
            call msg("Target 1: RMSD(dai/(dai**2+u**2)-Int(0,Inf)cos(u*t)exp(-dai*t)dt) < " // str(TargetRMSD, d=3))
            call msg("Target 2: MaxRelError(dai/(dai**2+u**2)-Int(0,Inf)cos(u*t)exp(-dai*t)dt) < " // str(TargetRelError, d=3))
            if (GridLimitDai) then
                  if (size(daiValues, dim=2) > 1) then
                        call msg("Max dai for subsystems is capped at max dai for the full complex")
                  end if
            end if
            
            call rpa_OptimizeLaplaceQuad(x, w, NPoints, NLowFreqs, NHiFreqs, MaxRelError, &
                  RMSD, Freqs, NFreqs, daiValues, daiWeights, MaxNPoints, TargetRelError, TargetRMSD)

            line = lfield("#", 5) // lfield("Freq", 15) // lfield("NPoints", 10) // &
                  lfield("MaxRelError", 15) // lfield("RMSD", 15) // lfield("QuadType", 10)
            call midrule(width=70)
            call msg(line)
            call midrule(width=70)
            do u = 1, NFreqs
                  if (u <= NLowFreqs) then
                        QuadType = 1
                  else 
                        QuadType = 2
                  end if
                  omega = Freqs(u)
                  line = lfield(str(u), 5) // lfield(str(omega, d=3), 15) // lfield(str(NPoints(u)), 10) &
                        // lfield(str(MaxRelError(u),d=3), 15) // lfield(str(RMSD(u),d=3), 15) // str(QuadType)
                  call msg(line)
            end do
            call blankline()
            call msg("1. MiniMax-optimized quadrature of Hackbush et al.")
            call msg("   J. Chem. Phys. 129, 044112 (2008); doi: 10.1063/1.2958921")
            call msg("2. Robust double-exponential quadrature of Ooura and Mori")
            call msg("   J. Comp. and Appl. Math. 112, 229 (1999); doi: 10.1016/S0377-0427(99)00223-X")
            call blankline()
            if (minval(NPoints(1:NFreqs)) > 0) then
                  call msg("Quadratures found for all frequencies")
            else
                  call msg("Failed to find all requested quadratures", MSG_ERROR)
                  stop
            end if
            NPointsAv = nint(real(sum(NPoints), F64)/ NFreqs)
            call msg("Average number of quadrature points per frequency: " // str(NPointsAv))
      end subroutine rpa_LaplaceQuad
      

      subroutine rpa_OptimizeLaplaceQuad(x, w, NPoints, NLowFreqs, NHiFreqs, MaxRelErrVec, &
            RMSDVec, Freqs, NFreqs, daiValues, daiWeights, MaxNPoints, TargetRelError, TargetRMSD)
            real(F64), dimension(:, :), intent(out) :: x
            real(F64), dimension(:, :), intent(out) :: w
            integer, dimension(:), intent(out)      :: NPoints
            integer, intent(out)                    :: NLowFreqs, NHiFreqs
            real(F64), dimension(:), intent(out)    :: MaxRelErrVec
            real(F64), dimension(:), intent(out)    :: RMSDVec
            real(F64), dimension(:), intent(in)     :: Freqs
            integer, intent(in)                     :: NFreqs
            real(F64), dimension(:, :), intent(in)  :: daiValues
            real(F64), dimension(:, :), intent(in)  :: daiWeights
            integer, intent(in)                     :: MaxNPoints
            real(F64), intent(in)                   :: TargetRelError
            real(F64), intent(in)                   :: TargetRMSD

            integer, parameter :: NlowMin = 5
            integer, parameter :: NlowMax = 63
            integer :: m, n
            integer, parameter :: MMin = 5
            integer, parameter :: MMax = 40
            real(F64), dimension(:), allocatable :: xk, wk
            integer :: u, k, kmin, kmax, j, nmin, nmax
            real(F64) :: omega
            logical :: TryLowFreqs, TryHiFreqs
            logical :: Converged
            real(F64) :: fApprox, fApproxPrev
            real(F64) :: DoubleExpConvRel, DoubleExpConvAbs
            real(F64) :: MaxRelError, RMSD
            real(F64) :: daiMin, daiMax
            integer :: NBins
            
            allocate(xk(MaxNPoints))
            allocate(wk(MaxNPoints))
            NBins = size(daiValues, dim=1)
            daiMin = minval(daiValues(1, :))
            daiMax = maxval(daiValues(NBins, :))
            DoubleExpConvRel = min(1.0E-8_F64, TargetRelError / 100)
            DoubleExpConvAbs = TargetRMSD / 100
            NLowFreqs = 0
            NHiFreqs = 0
            NPoints = 0
            x = ZERO
            w = ZERO
            TryLowFreqs = .true.
            TryHiFreqs = .true.
            do u = 1, NFreqs
                  omega = Freqs(u)
                  Converged = .false.
                  if (TryLowFreqs) then
                        !
                        ! The low-freq quadrature achieves the target accuracy only
                        ! for a couple of smallest frequencies. The mid-freq quadrature
                        ! is subsequently applied to test if the target accuracy can
                        ! be achieved with a smaller number of nodes.
                        !
                        ! The presence of a small orbital gap is the worst-case scenario
                        ! for the low-freq quadrature.
                        !
                        kmin = min(NlowMin, MaxNPoints)
                        kmax = min(NlowMax, MaxNPoints)
                        KlowLoop: do k = kmin, kmax
                              call quad_MiniMax_1_x(xk, wk, daiMin, daiMax, k)
                              do j = 1, k
                                    wk(j) = wk(j) * cos(omega * xk(j))
                              end do
                              call rpa_LaplaceQuadError(MaxRelError, RMSD, omega, k, wk, xk, &
                                    daiValues, daiWeights)
                              if (MaxRelError < TargetRelError .and. RMSD < TargetRMSD) then
                                    w(1:k, u) = wk(1:k)
                                    x(1:k, u) = xk(1:k)
                                    MaxRelErrVec(u) = MaxRelError
                                    RMSDVec(u) = RMSD
                                    NLowFreqs = NLowFreqs + 1
                                    NPoints(u) = k
                                    Converged = .true.
                                    exit KlowLoop
                              else
                                    if (k == kmax) then
                                          !
                                          ! The low-freq quadrature isn't able to achieve the target
                                          ! accuracy for the current frequency and will not be tested
                                          ! for any higher frequency
                                          !
                                          TryLowFreqs = .false.
                                    end if
                              end if
                        end do KlowLoop
                  end if
                  if (TryHiFreqs) then
                        !
                        ! The hi-freq quadrature can achieve any (reasonable) accuracy
                        ! for both low and high frequencies, but for low freqs it requires
                        ! a much larger number of points compared to the MiniMax quadrature.
                        !
                        ! The presence of a large orbital gap is the worst-case scenario
                        ! for the mid-freq quadrature.
                        !
                        mloop: do m = MMin, MMax
                              nmin = min(m+1, (MaxNPoints-1)/2)
                              nmax = min(3*m, (MaxNPoints-1)/2)
                              nloop: do n = nmin, nmax
                                    k = 2 * n + 1
                                    call quad_DoubleExponential_Cos(xk, wk, omega, m, n)
                                    fApprox = ZERO
                                    do j = 1, k
                                          fApprox = fApprox + wk(j) * exp(-daiMax * xk(j))
                                    end do
                                    if (n > nmin) then
                                          if (abs(fApprox-fApproxPrev) < abs(fApprox * DoubleExpConvRel) .and. &
                                                abs(fApprox-fApproxPrev) < DoubleExpConvAbs) then
                                                call rpa_LaplaceQuadError(MaxRelError, RMSD, omega, k, wk, xk, &
                                                      daiValues, daiWeights)
                                                if (MaxRelError < TargetRelError .and. RMSD < TargetRMSD) then
                                                      !
                                                      ! The hi-freq quadrature is accepted if either of the following
                                                      ! conditions is satisfied:
                                                      ! 1. The low-freq quadrature is disabled
                                                      ! 2. The low-freq quadrature converged but has a larger number of nodes
                                                      !
                                                      if ((.not. TryLowFreqs) .or. (TryLowFreqs .and. k <= NPoints(u))) then
                                                            if (NPoints(u) > k) then
                                                                  w(k+1:NPoints(u), u) = ZERO
                                                                  x(k+1:NPoints(u), u) = ZERO
                                                            end if
                                                            w(1:k, u) = wk(1:k)
                                                            x(1:k, u) = xk(1:k)
                                                            MaxRelErrVec(u) = MaxRelError
                                                            RMSDVec(u) = RMSD
                                                            NHiFreqs = NHiFreqs + 1
                                                            if (NPoints(u) > 0) then
                                                                  !
                                                                  ! The points and weights of the low-freq quadrature
                                                                  ! were overwritten
                                                                  !
                                                                  NLowFreqs = NLowFreqs - 1
                                                                  TryLowFreqs = .false.
                                                            end if
                                                            NPoints(u) = k
                                                            Converged = .true.
                                                      end if
                                                      exit mloop
                                                end if
                                                exit nloop
                                          end if
                                    end if
                                    fApproxPrev = fApprox
                              end do nloop
                        end do mloop
                  end if
                  if (.not. Converged) then
                        call msg("Unable to find Laplace quadrature for u=" // str(Freqs(u),d=3), MSG_ERROR)
                        stop
                  end if
            end do
      end subroutine rpa_OptimizeLaplaceQuad


      subroutine rpa_LaplaceQuadError(MaxRelError, RMSD, omega, n, wk, xk, daiValues, daiWeights)
            real(F64), intent(out) :: MaxRelError
            real(F64), intent(out) :: RMSD
            real(F64), intent(in) :: omega
            integer, intent(in) :: n
            real(F64), dimension(:), intent(in) :: wk
            real(F64), dimension(:), intent(in) :: xk
            real(F64), dimension(:, :), intent(in) :: daiValues
            real(F64), dimension(:, :), intent(in) :: daiWeights

            real(F64) :: fApprox, fExact, dai
            real(F64), dimension(:), allocatable :: Err2, RelErr
            integer :: l, k, s
            integer :: NBins, NDaiSets

            NBins = size(daiValues, dim=1)
            NDaiSets = size(daiValues, dim=2)
            allocate(Err2(NDaiSets))
            allocate(RelErr(NDaiSets))
            do s = 1, NDaiSets
                  Err2(s) = ZERO
                  RelErr(s) = ZERO
                  do k = 1, NBins
                        if (daiWeights(k, s) > ZERO) then
                              dai = daiValues(k, s)
                              fExact = dai / (dai**2 + omega**2)
                              fApprox = ZERO
                              do l = 1, n
                                    fApprox = fApprox + wk(l) * exp(-dai * xk(l))
                              end do
                              RelErr(s) = max(RelErr(s), abs((fApprox-fExact)/fExact))
                              Err2(s) = Err2(s) + daiWeights(k, s) * (fApprox - fExact)**2
                        end if
                  end do
            end do
            RMSD = sqrt(maxval(Err2))
            MaxRelError = maxval(RelErr)
      end subroutine rpa_LaplaceQuadError
end module OptLaplaceQuad
