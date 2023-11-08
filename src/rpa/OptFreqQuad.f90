module OptFreqQuad
      use arithmetic
      use quadratures
      use display
      use string
      use uniopt

      implicit none
      
contains

      subroutine rpa_OptimizeFreqQuad(Freqs, FreqWeights, NFreqs, daiValues, daiWeights, &
            TargetError, TargetRelError, MinNFreqs, MaxNFreqs, GridLimitDai)

            real(F64), dimension(MaxNFreqs), intent(out) :: Freqs
            real(F64), dimension(MaxNFreqs), intent(out) :: FreqWeights
            integer, intent(out)                         :: NFreqs
            real(F64), dimension(:, :), intent(in)       :: daiValues
            real(F64), dimension(:, :), intent(in)       :: daiWeights
            real(F64), intent(in)                        :: TargetError
            real(F64), intent(in)                        :: TargetRelError
            integer, intent(in)                          :: MinNFreqs, MaxNFreqs
            logical, intent(in)                          :: GridLimitDai

            character(:), allocatable :: line
            real(F64) :: OptAlphaGL
            logical :: convergedGL
            real(F64) :: ErrorGL, RelErrorGL
            logical :: Converged
            integer :: n

            call blankline()
            call msg("Optimizing quadrature for the imaginary frequency grid")
            call msg("Target 1: RMSD(Pi/(2*dai)-Int(0,Inf)1/(dai**2+u**2)du) < " // str(TargetError, d=3))
            call msg("Target 2: MaxRelError(Pi/(2*dai)-Int(0,Inf)1/(dai**2+u**2)du) < " // str(TargetRelError, d=3))
            if (GridLimitDai) then
                  if (size(daiValues, dim=2) > 1) then
                        call msg("Max dai for subsystems is capped at max dai for the full complex")
                  end if
            end if
            call msg("MinNFreqs: " // str(MinNFreqs))
            call msg("MaxNFreqs: " // str(MaxNFreqs))
            call msg("Quadrature type: transformed Gauss-Legendre (GL)")
            call msg("Transformation of the GL quadrature onto the interval (0, Inf):")
            call msg("x(k) = Alpha * (ONE + xGL(k)) / (ONE - xGL(k))")
            call msg("w(k) = Alpha * TWO * wGL(k) / (ONE - xGL(k))**2")
            line = lfield("NFreqs", 10) // lfield("Alpha", 15) // lfield("RMSD", 15) // lfield("MaxRelError", 15)
            call midrule(width=55)
            call msg(line)
            call midrule(width=55)
            Converged = .false.
            do n = MinNFreqs, MaxNFreqs
                  call rpa_OptQuadScaling(Freqs(1:n), FreqWeights(1:n), OptAlphaGL, RelErrorGL, ErrorGL, n, &
                        daiValues, daiWeights)
                  line =lfield(str(n), 10) // lfield(str(OptAlphaGL, d=3), 15) // lfield(str(ErrorGL, d=3), 15) &
                        // lfield(str(RelErrorGL,d=3), 15)
                  call msg(line)                  
                  if (ErrorGL < TargetError .and. RelErrorGL < TargetRelError) then
                        NFreqs = n
                        call quad_CasimirPolder(Freqs, FreqWeights, convergedGL, n, 1.0E-14_F64, OptAlphaGL)
                        call blankline()
                        call msg("Optimization completed: transformed Gauss-Legendre, " // str(NFreqs) // " points")
                        converged = .true.
                        exit
                  end if
            end do
            call blankline()
            if (.not. Converged) then
                  call msg("Quadrature did not converge to specified accuracy", MSG_ERROR)
                  error stop
            end if
      end subroutine rpa_OptimizeFreqQuad


      subroutine rpa_OptQuadScaling(Freqs, FreqWeights, OptAlpha, OptRelError, OptRMSD, NFreqs, daiValues, daiWeights)
            real(F64), dimension(NFreqs), intent(out) :: Freqs
            real(F64), dimension(NFreqs), intent(out) :: FreqWeights
            real(F64), intent(out)                    :: OptAlpha
            real(F64), intent(out)                    :: OptRelError
            real(F64), intent(out)                    :: OptRMSD
            integer, intent(in)                       :: NFreqs
            real(F64), dimension(:, :), intent(in)    :: daiValues
            real(F64), dimension(:, :), intent(in)    :: daiWeights

            logical :: CPConverged
            real(F64), dimension(:), allocatable :: FreqsUnscaled
            real(F64), dimension(:), allocatable :: FreqWeightsUnscaled
            integer :: k
            real(F64) :: MaxRelError, RMSD
            real(F64) :: Alpha
            integer :: NDaiSets
            real(F64), parameter :: MinAlpha = 0.1_F64
            real(F64), parameter :: MaxAlpha = 15.0_F64
            integer, parameter :: NScan = 15000

            NDaiSets = size(daiValues, dim=2)
            allocate(FreqsUnscaled(NFreqs))
            allocate(FreqWeightsUnscaled(NFreqs))
            call quad_CasimirPolder(FreqsUnscaled, FreqWeightsUnscaled, &
                  CPConverged, NFreqs, 1.0E-14_F64, ONE)
            if (.not. CPConverged) then
                  call msg("Could not compute modified Gauss-Legendre quadrature for NFreqs=" // str(NFreqs), MSG_ERROR)
                  error stop
            end if
            OptRMSD = huge(ONE)
            OptRelError = ZERO
            OptAlpha = ZERO
            do k = 1, NScan
                  Alpha = MinAlpha + (MaxAlpha - MinAlpha) * real(k-1, F64) / real(NScan-1, F64)
                  Freqs = Alpha * FreqsUnscaled
                  FreqWeights = Alpha * FreqWeightsUnscaled
                  call rpa_FreqQuadError(MaxRelError, RMSD, Freqs, FreqWeights, &
                        daiValues, daiWeights, NFreqs, NDaiSets)
                  if (RMSD < OptRMSD) then
                        OptRMSD = RMSD
                        OptRelError = MaxRelError
                        OptAlpha = Alpha
                  end if
            end do
            Freqs = OptAlpha * FreqsUnscaled
            FreqWeights = OptAlpha * FreqWeightsUnscaled
      end subroutine rpa_OptQuadScaling


      subroutine rpa_FreqQuadError(MaxRelError, RMSD, Freqs, FreqWeights, daiValues, daiWeights, NFreqs, NDaiSets)
            !
            ! Compute the frequency quadrature error defined as RMSD(Pi/(2*dai)-Int(0,Inf)1/(dai**2+u**2)du)
            !
            real(F64), intent(out)                   :: MaxRelError
            real(F64), intent(out)                   :: RMSD
            real(F64), dimension(NFreqs), intent(in) :: Freqs
            real(F64), dimension(NFreqs), intent(in) :: FreqWeights
            real(F64), dimension(:, :), intent(in)   :: daiValues
            real(F64), dimension(:, :), intent(in)   :: daiWeights
            integer, intent(in)                      :: NFreqs
            integer, intent(in)                      :: NDaiSets

            integer :: k, l, s
            real(F64) :: uk, wk
            real(F64) :: dai
            real(F64) :: IntExact, IntApprox
            real(F64), dimension(NDaiSets) :: Err2, RelError
            integer :: NBins

            NBins = size(daiValues, dim=1)
            RelError = ZERO
            Err2 = ZERO
            do s = 1, NDaiSets
                  do l = 1, NBins
                        if (daiWeights(l, s) > ZERO) then
                              dai = daiValues(l, s)
                              !
                              ! Integrate(0,Inf) 1/(dai**2+u**2) du
                              !
                              IntExact = PI / (TWO * dai)
                              IntApprox = ZERO
                              do k = 1, NFreqs
                                    wk = FreqWeights(k)
                                    uk = Freqs(k)
                                    IntApprox = IntApprox + wk / (dai**2 + uk**2)
                              end do
                              RelError(s) = max(RelError(s), abs((IntApprox-IntExact)/IntExact))
                              Err2(s) = Err2(s) + daiWeights(l, s) * (IntApprox - IntExact)**2
                        end if
                  end do
            end do
            RMSD = sqrt(maxval(Err2))
            MaxRelError = maxval(RelError)
      end subroutine rpa_FreqQuadError
end module OptFreqQuad
