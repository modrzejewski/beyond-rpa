module OrbDiffHist
      use arithmetic
      use math_constants
      use display
      use rpa_definitions, only: TRPAParams, RPA_FROZEN_UNDEFINED
      use sys_definitions, only: TSystem

      implicit none

contains

      subroutine rpa_DaiMaxThresh(daiMaxThresh, OrbEnergies, NOcc, NVirt, NCore, NSpins)
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
            integer, dimension(2), intent(in)      :: NCore
            integer, intent(in)                    :: NSpins

            integer :: s, i0, a1

            daiMaxThresh = ZERO
            do s = 1, NSpins
                  if (NCore(s) < NOcc(s)) then
                        i0 = NCore(s) + 1
                        a1 = NOcc(s) + NVirt(s)
                        daiMaxThresh = max(daiMaxThresh, OrbEnergies(a1, s)-OrbEnergies(i0, s))
                  end if
            end do
      end subroutine rpa_DaiMaxThresh

      
      subroutine rpa_DaiHistogram(daiValues, daiWeights, OrbEnergies, NOcc, NVirt, &
            NCore, DaiMaxThresh)
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
            integer, intent(in)                  :: NCore
            real(F64), intent(in)                :: DaiMaxThresh

            real(F64) :: daiMin, daiMax
            integer :: NBins, k
            integer :: i0, i1, a0, a1
            integer :: i, a
            real(F64) :: BinWidth, NormFactor, dai
            integer, dimension(:), allocatable :: daiCount

            NBins = size(daiValues)
            allocate(daiCount(NBins))
            daiMin = huge(ONE)
            daiMax = ZERO
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


      subroutine rpa_SelectNCore(NCore, RPAParams, System, OrbEnergies, NOcc, NSpins)
            !
            ! Select the number of frozen core orbitals in the active subsystem.
            !
            integer, dimension(:), intent(out)     :: NCore
            type(TRPAParams), intent(in)           :: RPAParams
            type(TSystem), intent(in)              :: System
            real(F64), dimension(:, :), intent(in) :: OrbEnergies
            integer, dimension(:), intent(in)      :: NOcc
            integer, intent(in)                    :: NSpins

            integer :: a, s

            NCore = 0
            if (any(RPAParams%NFrozenOrbitals /= RPA_FROZEN_UNDEFINED)) then
                  do s = 1, 2
                        do a = System%RealAtoms(1, s), System%RealAtoms(2, s)
                              NCore(1:NSpins) = NCore(1:NSpins) + &
                                    RPAParams%NFrozenOrbitals(System%ZNumbers(a))
                        end do
                  end do
            else
                  do s = 1, NSpins
                        call rpa_NCore(NCore(s), OrbEnergies(:, s), NOcc(s), RPAParams%CoreOrbThresh)
                  end do
            end if
            do s = 1, NSpins
                  if (NCore(s) > NOcc(s)) then
                        call msg("More frozen orbitals than occupied orbitals", MSG_ERROR)
                        error stop
                  end if
            end do
      end subroutine rpa_SelectNCore
end module OrbDiffHist
