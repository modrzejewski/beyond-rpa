module OrbDiffHist
      use arithmetic
      use math_constants
      use display
      
      implicit none

contains

      subroutine rpa_DaiMaxThresh(daiMaxThresh, OrbEnergies, NOcc, NVirt, NSpins, CoreOrbThresh)
            !
            ! Compute the maximum orbital energy difference Ea - Ei. The max difference
            ! computed for the full complex will be the upper bound for the differences
            ! considered for subsystems when generating the numerical integration grid.
            ! (Note that it's used only for grid parameters optimization, i.e., nothing
            ! neglected during the Green's function build.)
            ! Without the threshold values, the monomer excitations localized on ghost
            ! atoms are artificially high and prevent the generation of a numerically
            ! stable grid.
            !
            real(F64), intent(out)                 :: daiMaxThresh
            real(F64), dimension(:, :), intent(in) :: OrbEnergies
            integer, dimension(2), intent(in)      :: NOcc
            integer, dimension(2), intent(in)      :: NVirt
            integer, intent(in)                    :: NSpins
            real(F64), intent(in)                  :: CoreOrbThresh

            integer :: s, i0, a1, NCore

            daiMaxThresh = ZERO
            do s = 1, NSpins
                  call rpa_NCore(NCore, OrbEnergies(:, s), NOcc(s), CoreOrbThresh)
                  if (NCore < NOcc(s)) then
                        i0 = NCore + 1
                        a1 = NOcc(s) + NVirt(s)
                        daiMaxThresh = max(daiMaxThresh, OrbEnergies(a1, s)-OrbEnergies(i0, s))
                  end if
            end do
      end subroutine rpa_DaiMaxThresh

      
      subroutine rpa_DaiHistogram(daiValues, daiWeights, OrbEnergies, NOcc, NVirt, &
            CoreOrbThresh, DaiMaxThresh)
            !
            ! Generate a histogram of orbital energy differences. Each bin of the histogram
            ! has its value and weight. In the open-shell case, this subroutine is called
            ! separately for each spin. The histogram which is the output of this subroutine
            ! is used for the generation of numerical quadratures.
            !
            real(F64), dimension(:), intent(out) :: daiValues
            real(F64), dimension(:), intent(out) :: daiWeights
            real(F64), dimension(:), intent(in)  :: OrbEnergies
            integer, intent(in)                  :: NOcc
            integer, intent(in)                  :: NVirt
            real(F64), intent(in)                :: CoreOrbThresh
            real(F64), intent(in)                :: DaiMaxThresh

            real(F64) :: daiMin, daiMax
            integer :: NBins, NCore, k
            integer :: i0, i1, a0, a1
            integer :: i, a
            real(F64) :: BinWidth, NormFactor, dai
            integer, dimension(:), allocatable :: daiCount

            NBins = size(daiValues)
            allocate(daiCount(NBins))
            daiMin = huge(ONE)
            daiMax = ZERO
            call rpa_NCore(NCore, OrbEnergies, NOcc, CoreOrbThresh)
            if (NCore < NOcc) then
                  i0 = NCore + 1
                  i1 = NOcc
                  a0 = NOcc + 1
                  a1 = NOcc + NVirt
                  daiMin = OrbEnergies(a0)-OrbEnergies(i1)
                  daiMax = min(DaiMaxThresh, OrbEnergies(a1)-OrbEnergies(i0))
                  if (.not. (daiMin > ZERO)) then
                        call msg("Found invalid orbital energy gap. Cannot decompose energy denominators.", MSG_ERROR)
                        error stop
                  end if
                  BinWidth = (daiMax - daiMin) / NBins
                  do k = 1, NBins
                        daiValues(k) = daiMin + (k - 1) * BinWidth
                  end do
                  daiCount = 0
                  do i = i0, i1
                        do a = a0, a1
                              dai = OrbEnergies(a) - OrbEnergies(i)
                              if (dai <= DaiMaxThresh) then
                                    !
                                    ! The use of nint, max, and min functions guarantees that
                                    ! daiMin and daiMax always fall into the first and
                                    ! last bin, respectively, regardless of the possible
                                    ! roundoff error.
                                    !
                                    k = 1 + nint((dai - (daiMin+BinWidth/TWO)) / BinWidth)
                                    k = max(1, k)
                                    k = min(k, NBins)
                                    daiCount(k) = daiCount(k) + 1
                              end if
                        end do                        
                  end do
                  NormFactor = real(sum(daiCount), F64)
                  do k = 1, NBins
                        daiWeights(k) = abs(daiCount(k) / NormFactor)
                  end do
            else
                  !
                  ! Set daiValues/daiWeights to special values
                  ! so that those arrays don't contribute to
                  ! the following variables:
                  ! (1) daiMax and daiMin, which are computed as
                  ! daiMin = minval(daiValues(1, :))
                  ! daiMax = maxval(daiValues(NBins, :))
                  ! (2) average errors, where
                  ! daiWeights(k, s)>ZERO is checked
                  !
                  daiValues = ZERO
                  daiValues(1) = huge(ONE)
                  daiValues(NBins) = ZERO
                  daiWeights = -ONE
            end if
      end subroutine rpa_DaiHistogram


      subroutine rpa_NCore(NCore, OrbEnergies, NOcc, CoreOrbThresh)
            !
            ! Determine the number of frozen core orbitals. In the open-shell case,
            ! NCore should be computed for each spin separately.
            !
            integer, intent(out)                :: NCore
            real(F64), dimension(:), intent(in) :: OrbEnergies
            integer, intent(in)                 :: NOcc
            real(F64), intent(in)               :: CoreOrbThresh

            integer :: k
            
            NCore = 0
            do k = 1, NOcc
                  if (OrbEnergies(k) < CoreOrbThresh) then
                        NCore = NCore + 1
                  else
                        exit
                  end if
            end do
      end subroutine rpa_NCore
end module OrbDiffHist
