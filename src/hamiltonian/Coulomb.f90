module Coulomb
      use arithmetic
      use math_constants

      implicit none

contains

      pure subroutine coul_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoVmax, IRhoVmax, &
            Vmax, IVmax, SubsetA, SubsetB, SubsetC, SubsetD, m1, m2, m3, m4, n1, n2, n3, n4, BitMask, &
            ShellSubsets, Thresh)
            !
            ! Determine the shell quartets which correspond nonnegligible Coulomb
            ! matrix contributions (ab|cd) Rho(c,d) > Thresh.
            !
            integer, dimension(:), intent(inout)      :: QuartetFlags
            integer, dimension(:), intent(inout)      :: QuartetLoc
            integer, intent(inout)                    :: NQuartets
            real(F64), dimension(:, :), intent(in)    :: RhoVMax
            integer, dimension(:, :), intent(in)      :: IRhoVMax
            real(F64), dimension(:, :), intent(in)    :: Vmax
            integer, dimension(:, :), intent(in)      :: IVmax
            integer, intent(in)                       :: SubsetA, SubsetB, SubsetC, SubsetD
            integer, intent(in)                       :: m1, m2, m3, m4
            integer, intent(in)                       :: n1, n2, n3, n4
            integer, intent(in)                       :: BitMask
            integer, dimension(:, :), intent(in)      :: ShellSubsets
            real(F64), intent(in)                     :: Thresh

            integer :: i, j
            integer :: ShellA, ShellB, ShellC, ShellD
            real(F64) :: AB, CD, ABCD
            logical :: Contrib
            integer :: Idx, QL

            if (SubsetA < SubsetB) return
            do ShellD = ShellSubsets(1, SubsetD), ShellSubsets(2, SubsetD)
                  ShellsC: do i = ShellSubsets(1, SubsetC), ShellSubsets(2, SubsetC)
                        ShellC = IRhoVMax(i, ShellD)
                        !
                        ! CD <- |Rho(c,d)*Sqrt((cd|cd))|, c's sorted in descending order
                        !
                        CD = RhoVMax(i, ShellD)
                        Contrib = .false.
                        ShellsB: do ShellB = ShellSubsets(1, SubsetB), ShellSubsets(2, SubsetB)
                              !
                              ! Estimate the largest possible contribution for given b,c,d
                              ! ABCD <- Sqrt((ab|ab)*(cd|cd))*|Rho(c,d)| >= |(ab|cd)*Rho(c,d)|
                              !
                              AB = VMax(ShellSubsets(1, SubsetA), ShellB)
                              ABCD = AB * CD
                              if (ABCD <= Thresh) cycle ShellsB
                              ShellsA: do j = ShellSubsets(1, SubsetA), ShellSubsets(2, SubsetA)
                                    ShellA = IVMax(j, ShellB)
                                    !
                                    ! Only lower half of the Coulomb matrix
                                    ! is referenced
                                    !
                                    if (ShellA < ShellB) cycle ShellsA
                                    AB = VMax(j, ShellB)
                                    ABCD = AB * CD
                                    if (ABCD > Thresh) then
                                          !
                                          ! The values of m1..m4 and n1..n4 are chosen is such a way that,
                                          ! for a given (ac|bd) shell quartet, idx is invariant with respect
                                          ! to the permutations a<->c, b<->d, ac<->bd, ...
                                          ! An appropriate bit mask carrying the information on the specific
                                          ! permutation considered here is added to the array QuartetFlags.
                                          !
                                          Idx = (ShellA - m1) * n1 + (ShellB - m2) * n2 + &
                                                (ShellC - m3) * n3 + (ShellD - m4) * n4 + 1
                                          if (QuartetLoc(Idx) > 0) then
                                                QL = QuartetLoc(Idx)
                                          else
                                                NQuartets = NQuartets + 1
                                                QL = NQuartets
                                                QuartetLoc(Idx) = QL
                                                QuartetFlags(QL) = ishft(Idx, 10)
                                          end if
                                          QuartetFlags(QL) = ior(QuartetFlags(QL), BitMask)
                                          Contrib = .true.
                                    else
                                          !
                                          ! Can safely exit the loop because the elements in VMax(:, ShellB)
                                          ! are sorted according to decreasing absolute values
                                          !
                                          exit ShellsA
                                    end if
                              end do ShellsA
                        end do ShellsB
                        if (.not. Contrib) then
                              !
                              ! Can safely exit the loop because the elements in RhoVMax(:, ShellD)
                              ! are sorted according to decreasing absolute values
                              !
                              exit ShellsC
                        end if
                  end do ShellsC
            end do
      end subroutine coul_ScreenIntegrals


      subroutine coul_Jab(T, Rho, Gabcd, Nab, Ncd, JCoeffAB)
            real(F64), dimension(Nab), intent(out) :: T
            real(F64), dimension(Ncd), intent(in) :: Rho
            real(F64), dimension(Ncd, Nab), intent(in)  :: Gabcd
            integer, intent(in) :: Nab, Ncd
            real(F64), intent(in) :: JCoeffAB
            
            integer :: ab, cd

            T = ZERO
            do ab = 1, Nab
                  do cd = 1, Ncd
                        T(ab) = T(ab) + Rho(cd) * Gabcd(cd, ab)
                  end do
            end do
            T = JCoeffAB * T
      end subroutine coul_Jab


      subroutine coul_Jcd(T, Rho, Gabcd, Nab, Ncd, JCoeffCD)
            real(F64), dimension(Ncd), intent(out) :: T
            real(F64), dimension(Nab), intent(in) :: Rho
            real(F64), dimension(Ncd, Nab), intent(in) :: Gabcd
            integer, intent(in) :: Nab, Ncd
            real(F64), intent(in) :: JCoeffCD

            integer :: ab, cd

            T = ZERO
            do ab = 1, Nab
                  do cd = 1, Ncd
                        T(cd) = T(cd) + Gabcd(cd, ab) * Rho(ab)
                  end do
            end do
            T = JCoeffCD * T
      end subroutine coul_Jcd
end module Coulomb
