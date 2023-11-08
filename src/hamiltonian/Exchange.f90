module Exchange
      use arithmetic
      use math_constants
      use sort

      implicit none

contains      

      pure subroutine exch_OrderDShells(Work, IWork, ShellC, RhoMax, VMax, SubsetB, SubsetD, ShellSubsets)
            !
            ! For a given shell C, order the elements of subset according to
            ! |Rho(C, D)| * Sqrt(|(B*D|B*D)|), where B* is the maximizer of (BD|BD).
            !
            real(F64), dimension(:), intent(out)   :: Work
            integer, dimension(:), intent(out)     :: IWork
            integer, intent(in)                    :: ShellC
            real(F64), dimension(:, :), intent(in) :: RhoMax
            real(F64), dimension(:, :), intent(in) :: VMax
            integer, intent(in)                    :: SubsetB
            integer, intent(in)                    :: SubsetD
            integer, dimension(:, :), intent(in)   :: ShellSubsets

            integer :: k
            integer :: ShellB, ShellD
            integer :: Nd

            ShellB = ShellSubsets(1, SubsetB)
            k = 1
            do ShellD = ShellSubsets(1, SubsetD), ShellSubsets(2, SubsetD)
                  Work(k) = RhoMax(ShellD, ShellC) * VMax(ShellB, ShellD)
                  IWork(k) = ShellD
                  k = k + 1
            end do
            !
            ! Sort the computed elements in increasing order.
            ! DSORT will return without doing anything if n <= 1
            !
            Nd = ShellSubsets(2, SubsetD) - ShellSubsets(1, SubsetD) + 1
            call dsort(Work, IWork, Nd)
      end subroutine exch_OrderDShells


      pure subroutine exch_BinarySearch(NAccepted, List, NElements, Thresh)
            !
            ! Perform a binary search on a list of elements sorted in decreasing order.
            !
            ! The output NAccepted is the largest index such that for any
            ! k=1...NAccepted, List(k) > Thresh.
            !
            ! Assumptions
            ! -----------
            ! 1. NElements > 0
            ! 2. List(1) > Thresh
            !
            integer, intent(out)                :: NAccepted
            real(F64), dimension(:), intent(in) :: List
            integer, intent(in)                 :: NElements
            real(F64), intent(in)               :: Thresh

            integer :: j, k

            if (List(NElements) > Thresh) then
                  NAccepted = NElements
            else
                  NAccepted = 1
                  j = NElements
                  bisection: do
                        k = (NAccepted + j) / 2
                        if (List(k) > Thresh) then
                              NAccepted = k
                        else
                              j = k
                        end if
                        if (NAccepted + 1 >= j) then
                              exit bisection
                        end if
                  end do bisection
            end if
      end subroutine exch_BinarySearch


      pure subroutine exch_ScreenIntegrals(QuartetFlags, QuartetLoc, NQuartets, RhoMax, &
            VMax, IVMax, SubsetA, SubsetC, SubsetB, SubsetD, m1, m3, m2, m4, n1, n3, n2, n4, BitMask, &
            Work, IWork, ShellSubsets, Thresh)
            !
            ! Generate a list of shell quartets with significant contributions to the HF exchange
            ! matrix. If the contribution |Rho(c,d)*(ac|bd)| > Thresh >= 0,
            ! an acceptance bit flag is set in the array QuartetFlags.
            !
            ! The integral screening is done according to the Schwarz inequality:
            !
            ! |Rho(c,d)*(ac|bd)| <= |Rho(c,d)|*Sqrt(|(ac|ac)|)*Sqrt(|(bd|bd)|)
            !
            ! The presence of a density matrix element in the above inequality
            ! makes this algorithm efficient in the direct SCF approach.
            !
            ! 1. Weber, V., Challacombe, M., Parallel algorithm for
            !    the computation of the Hartree-Fock exchange matrix: Gas phase
            !    and periodic parallel ONX, J. Chem. Phys, 125, 104110(2006).
            ! 2. Ochsenfeld, Ch., White, Ch., and Head-Gordon, M., Linear and 
            !    sublinear scaling formation of Hartree-Fock-type exchange
            !    matrices, J. Chem. Phys. 109, 1663(1998).
            ! 3. Schwegler, E. Challacombe, M., and Head-Gordon, M., Linear
            !    scaling computation of the Fock matrix. II. Rigorous bounds on
            !    exchange integrals and incremental Fock build, J. Chem. Phys.,
            !    106, 9708(1997).
            !
            integer, dimension(:), intent(inout)      :: QuartetFlags
            integer, dimension(:), intent(inout)      :: QuartetLoc
            integer, intent(inout)                    :: NQuartets
            real(F64), dimension(:, :), intent(in)    :: RhoMax
            real(F64), dimension(:, :), intent(in)    :: VMax
            integer, dimension(:, :), intent(in)      :: IVMax
            integer, intent(in)                       :: SubsetA, SubsetC, SubsetB, SubsetD
            integer, intent(in)                       :: m1, m3, m2, m4
            integer, intent(in)                       :: n1, n3, n2, n4
            integer, intent(in)                       :: BitMask
            real(F64), dimension(:), intent(out)      :: Work
            integer, dimension(:), intent(out)        :: IWork
            integer, dimension(:, :), intent(in)      :: ShellSubsets
            real(F64), intent(in)                     :: Thresh

            real(F64) :: RhoCD, Thresh2
            integer :: ShellA, ShellB, ShellC, ShellD
            integer :: k
            integer :: i, j
            integer :: Idx, QL
            integer :: a0, b0
            integer :: a1, b1
            integer :: Na, Nb, Nd
            logical :: Contrib
            real(F64) :: AC, BD, MaxAC, MaxBD
            integer :: NAcceptedA, NAcceptedB
            !
            ! Compute contributions (ac|bd)*Rho(c,d) for the lower triangle of the Fock matrix
            !
            if (SubsetA < SubsetB) return
            a0 = ShellSubsets(1, SubsetA)
            b0 = ShellSubsets(1, SubsetB)
            Na = ShellSubsets(2, SubsetA) - ShellSubsets(1, SubsetA) + 1
            Nb = ShellSubsets(2, SubsetB) - ShellSubsets(1, SubsetB) + 1
            Nd = ShellSubsets(2, SubsetD) - ShellSubsets(1, SubsetD) + 1
            LoopC: do ShellC = ShellSubsets(1, SubsetC), ShellSubsets(2, SubsetC)
                  !
                  ! Calculate products |Rho(c,d)| * Sqrt(|(b*d|b*d|) and sort them
                  ! in ascending order. The index c is fixed, b* maximizes |(bd|bd)|, and
                  ! the index d is variable. Having d's ordered enables the algorithm
                  ! to exit the loop over d's as early as possible, because the bound on
                  ! the contributions in the subsequent loop cycles is guaranteed to be smaller.
                  !
                  call exch_OrderDShells(Work, IWork, ShellC, RhoMax, VMax, SubsetB, SubsetD, ShellSubsets)
                  !
                  ! VMax(a0, ShellC) contains Sqrt(|(a*c|a*c)|), a* is the maximizer in the subset A.
                  ! Jump to the next c if the largest possible contibution for the current c is
                  ! below the threshold.
                  !
                  if (VMax(a0, ShellC) * Work(Nd) <= Thresh) then
                        cycle LoopC
                  end if
                  LoopD: do k = Nd, 1, -1
                        ShellD = IWork(k)
                        RhoCD = RhoMax(ShellC, ShellD)
                        MaxBD = RhoCD * VMax(b0, ShellD)
                        !
                        ! Use the fact d shells are ordered according to increasing Rho(c,d)*Sqrt(|(b*d|b*d)|).
                        ! Exit the loop if, for the current d, |Rho(c,d)*Sqrt(|(a*c|a*c)|)*SQRT(|(b*d|b*d)|) <= Thresh.
                        ! a* and b* maximize the integrals in the subsets they belong to.
                        !
                        if (VMax(a0, ShellC) * MaxBD <= Thresh) then
                              exit LoopD
                        else
                              call exch_BinarySearch(NAcceptedA, VMax(a0:, ShellC), Na, Thresh/MaxBD)
                              a1 = a0 + NAcceptedA - 1
                        end if
                        MaxAC = RhoCD * VMax(a0, ShellC)
                        !
                        ! Find the maximum index B for which Sqrt(|(bd|bd)|) > Thresh/RhoCD.
                        !
                        call exch_BinarySearch(NAcceptedB, VMax(b0:, ShellD), Nb, Thresh/MaxAC)
                        b1 = b0 + NAcceptedB - 1
                        Thresh2 = Thresh / RhoCD
                        LoopA: do i = a0, a1
                              ShellA = IVMax(i, ShellC)
                              AC = VMax(i, ShellC)
                              Contrib = .false.
                              LoopB: do j = b0, b1
                                    ShellB = IVMax(j, ShellD)
                                    !
                                    ! Only lower half of the exchange matrix
                                    ! is referenced
                                    !
                                    if (ShellA < ShellB) cycle LoopB
                                    BD = VMax(j, ShellD)
                                    if (AC*BD > Thresh2) then
                                          !
                                          ! The values of m1..m4 and n1..n4 are chosen is such a way that,
                                          ! for a given (ac|bd) shell quartet, idx is invariant with respect
                                          ! to the permutations a<->c, b<->d, ac<->bd, ...
                                          ! An appropriate bit mask carrying the information on the specific
                                          ! permutation considered here is added to the array QuartetFlags.
                                          !
                                          Idx = (ShellA - m1) * n1 + (ShellC - m3) * n3 + &
                                                (ShellB - m2) * n2 + (ShellD - m4) * n4 + 1
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
                                          exit LoopB
                                    end if
                              end do LoopB
                              if (.not. Contrib .and. SubsetA /= SubsetB) then
                                    exit LoopA
                              end if
                        end do LoopA
                  end do LoopD
            end do LoopC
      end subroutine exch_ScreenIntegrals

      
      pure subroutine exch_digest_RhoBD(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, V, ShellPairLoc)
            real(F64), dimension(Na, Nc), intent(out) :: T
            integer, intent(out) :: k0, k1
            real(F64), dimension(:), intent(in) :: Rho
            integer, intent(in) :: a, na
            integer, intent(in) :: b, nb
            integer, intent(in) :: c, nc
            integer, intent(in) :: d, nd
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            integer, dimension(:, :), intent(in) :: ShellPairLoc

            integer :: r0, r1
            
            k0 = ShellPairLoc(a, c)
            k1 = ShellPairLoc(a, c) + Na * Nc - 1
            r0 = ShellPairLoc(d, b)
            r1 = ShellPairLoc(d, b) + Nd * Nb - 1
            call exch_VRhoBD(T, V, Rho(r0:r1), Na, Nb, Nc, Nd)
      end subroutine exch_digest_RhoBD


      pure subroutine exch_VRhoBD(T, V, Rho, Na, Nb, Nc, Nd)
            real(F64), dimension(Na, Nc), intent(out) :: T
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            real(F64), dimension(Nd, Nb), intent(in) :: Rho
            integer, intent(in) :: Na, Nb, Nc, Nd

            integer :: a, b, c, d

            T = ZERO
            do a = 1, Na
                  do b = 1, Nb
                        do c = 1, Nc
                              do d = 1, Nd
                                    T(a, c) = T(a, c) + Rho(d, b) * V(d, c, b, a)
                              end do
                        end do
                  end do
            end do
      end subroutine exch_VRhoBD


      pure subroutine exch_digest_RhoBC(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, V, ShellPairLoc)
            real(F64), dimension(Na, Nd), intent(out) :: T
            integer, intent(out) :: k0, k1
            real(F64), dimension(:), intent(in) :: Rho
            integer, intent(in) :: a, na
            integer, intent(in) :: b, nb
            integer, intent(in) :: c, nc
            integer, intent(in) :: d, nd
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            integer, dimension(:, :), intent(in) :: ShellPairLoc

            integer :: r0, r1
            
            k0 = ShellPairLoc(a, d)
            k1 = ShellPairLoc(a, d) + Na * Nd - 1
            r0 = ShellPairLoc(c, b)
            r1 = ShellPairLoc(c, b) + Nc * Nb - 1
            call exch_VRhoBC(T, V, Rho(r0:r1), Na, Nb, Nc, Nd)
      end subroutine exch_digest_RhoBC


      pure subroutine exch_VRhoBC(T, V, Rho, Na, Nb, Nc, Nd)
            real(F64), dimension(Na, Nd), intent(out) :: T
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            real(F64), dimension(Nc, Nb), intent(in) :: Rho
            integer, intent(in) :: Na, Nb, Nc, Nd

            integer :: a, b, c, d

            T = ZERO
            do a = 1, Na
                  do b = 1, Nb
                        do c = 1, Nc
                              do d = 1, Nd
                                    T(a, d) = T(a, d) + Rho(c, b) * V(d, c, b, a)
                              end do
                        end do
                  end do
            end do
      end subroutine exch_VRhoBC


      pure subroutine exch_digest_RhoAD(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, V, ShellPairLoc)
            real(F64), dimension(Nb, Nc), intent(out) :: T
            integer, intent(out) :: k0, k1
            real(F64), dimension(:), intent(in) :: Rho
            integer, intent(in) :: a, na
            integer, intent(in) :: b, nb
            integer, intent(in) :: c, nc
            integer, intent(in) :: d, nd
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            integer, dimension(:, :), intent(in) :: ShellPairLoc

            integer :: r0, r1
            
            k0 = ShellPairLoc(b, c)
            k1 = ShellPairLoc(b, c) + Nb * Nc - 1
            r0 = ShellPairLoc(d, a)
            r1 = ShellPairLoc(d, a) + Nd * Na - 1
            call exch_VRhoAD(T, V, Rho(r0:r1), Na, Nb, Nc, Nd)
      end subroutine exch_digest_RhoAD


      pure subroutine exch_VRhoAD(T, V, Rho, Na, Nb, Nc, Nd)
            real(F64), dimension(Nb, Nc), intent(out) :: T
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            real(F64), dimension(Nd, Na), intent(in) :: Rho
            integer, intent(in) :: Na, Nb, Nc, Nd

            integer :: a, b, c, d

            T = ZERO
            do a = 1, Na
                  do b = 1, Nb
                        do c = 1, Nc
                              do d = 1, Nd
                                    T(b, c) = T(b, c) + Rho(d, a) * V(d, c, b, a)
                              end do
                        end do
                  end do
            end do
      end subroutine exch_VRhoAD


      pure subroutine exch_digest_RhoAC(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, V, ShellPairLoc)
            real(F64), dimension(Nb, Nd), intent(out) :: T
            integer, intent(out) :: k0, k1
            real(F64), dimension(:), intent(in) :: Rho
            integer, intent(in) :: a, na
            integer, intent(in) :: b, nb
            integer, intent(in) :: c, nc
            integer, intent(in) :: d, nd
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            integer, dimension(:, :), intent(in) :: ShellPairLoc

            integer :: r0, r1
            
            k0 = ShellPairLoc(b, d)
            k1 = ShellPairLoc(b, d) + Nb * Nd - 1
            r0 = ShellPairLoc(c, a)
            r1 = ShellPairLoc(c, a) + Nc * Na - 1
            call exch_VRhoAC(T, V, Rho(r0:r1), Na, Nb, Nc, Nd)
      end subroutine exch_digest_RhoAC


      pure subroutine exch_VRhoAC(T, V, Rho, Na, Nb, Nc, Nd)
            real(F64), dimension(Nb, Nd), intent(out) :: T
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            real(F64), dimension(Nc, Na), intent(in) :: Rho
            integer, intent(in) :: Na, Nb, Nc, Nd

            integer :: a, b, c, d

            T = ZERO
            do a = 1, Na
                  do b = 1, Nb
                        do c = 1, Nc
                              do d = 1, Nd
                                    T(b, d) = T(b, d) + Rho(c, a) * V(d, c, b, a)
                              end do
                        end do
                  end do
            end do
      end subroutine exch_VRhoAC


      pure subroutine exch_digest_RhoDB(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, V, ShellPairLoc)
            real(F64), dimension(Nc, Na), intent(out) :: T
            integer, intent(out) :: k0, k1
            real(F64), dimension(:), intent(in) :: Rho
            integer, intent(in) :: a, na
            integer, intent(in) :: b, nb
            integer, intent(in) :: c, nc
            integer, intent(in) :: d, nd
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            integer, dimension(:, :), intent(in) :: ShellPairLoc

            integer :: r0, r1
            
            k0 = ShellPairLoc(c, a)
            k1 = ShellPairLoc(c, a) + Nc * Na - 1
            r0 = ShellPairLoc(d, b)
            r1 = ShellPairLoc(d, b) + Nd * Nb - 1
            call exch_VRhoDB(T, V, Rho(r0:r1), Na, Nb, Nc, Nd)
      end subroutine exch_digest_RhoDB


      pure subroutine exch_VRhoDB(T, V, Rho, Na, Nb, Nc, Nd)
            real(F64), dimension(Nc, Na), intent(out) :: T
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            real(F64), dimension(Nd, Nb), intent(in) :: Rho
            integer, intent(in) :: Na, Nb, Nc, Nd

            integer :: a, b, c, d

            T = ZERO
            do a = 1, Na
                  do b = 1, Nb
                        do c = 1, Nc
                              do d = 1, Nd
                                    T(c, a) = T(c, a) + Rho(d, b) * V(d, c, b, a)
                              end do
                        end do
                  end do
            end do
      end subroutine exch_VRhoDB


      pure subroutine exch_digest_RhoDA(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, V, ShellPairLoc)
            real(F64), dimension(Nc, Nb), intent(out) :: T
            integer, intent(out) :: k0, k1
            real(F64), dimension(:), intent(in) :: Rho
            integer, intent(in) :: a, na
            integer, intent(in) :: b, nb
            integer, intent(in) :: c, nc
            integer, intent(in) :: d, nd
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            integer, dimension(:, :), intent(in) :: ShellPairLoc

            integer :: r0, r1
            
            k0 = ShellPairLoc(c, b)
            k1 = ShellPairLoc(c, b) + Nc * Nb - 1
            r0 = ShellPairLoc(d, a)
            r1 = ShellPairLoc(d, a) + Nd * Na - 1
            call exch_VRhoDA(T, V, Rho(r0:r1), Na, Nb, Nc, Nd)
      end subroutine exch_digest_RhoDA


      pure subroutine exch_VRhoDA(T, V, Rho, Na, Nb, Nc, Nd)
            real(F64), dimension(Nc, Nb), intent(out) :: T
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            real(F64), dimension(Nd, Na), intent(in) :: Rho
            integer, intent(in) :: Na, Nb, Nc, Nd

            integer :: a, b, c, d

            T = ZERO
            do a = 1, Na
                  do b = 1, Nb
                        do c = 1, Nc
                              do d = 1, Nd
                                    T(c, b) = T(c, b) + Rho(d, a) * V(d, c, b, a)
                              end do
                        end do
                  end do
            end do
      end subroutine exch_VRhoDA


      pure subroutine exch_digest_RhoCB(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, V, ShellPairLoc)
            real(F64), dimension(Nd, Na), intent(out) :: T
            integer, intent(out) :: k0, k1
            real(F64), dimension(:), intent(in) :: Rho
            integer, intent(in) :: a, na
            integer, intent(in) :: b, nb
            integer, intent(in) :: c, nc
            integer, intent(in) :: d, nd
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            integer, dimension(:, :), intent(in) :: ShellPairLoc

            integer :: r0, r1
            
            k0 = ShellPairLoc(d, a)
            k1 = ShellPairLoc(d, a) + Nd * Na - 1
            r0 = ShellPairLoc(c, b)
            r1 = ShellPairLoc(c, b) + Nc * Nb - 1
            call exch_VRhoCB(T, V, Rho(r0:r1), Na, Nb, Nc, Nd)
      end subroutine exch_digest_RhoCB


      pure subroutine exch_VRhoCB(T, V, Rho, Na, Nb, Nc, Nd)
            real(F64), dimension(Nd, Na), intent(out) :: T
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            real(F64), dimension(Nc, Nb), intent(in) :: Rho
            integer, intent(in) :: Na, Nb, Nc, Nd

            integer :: a, b, c, d

            T = ZERO
            do a = 1, Na
                  do b = 1, Nb
                        do c = 1, Nc
                              do d = 1, Nd
                                    T(d, a) = T(d, a) + Rho(c, b) * V(d, c, b, a)
                              end do
                        end do
                  end do
            end do
      end subroutine exch_VRhoCB


      pure subroutine exch_digest_RhoCA(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, V, ShellPairLoc)
            real(F64), dimension(Nd, Nb), intent(out) :: T
            integer, intent(out) :: k0, k1
            real(F64), dimension(:), intent(in) :: Rho
            integer, intent(in) :: a, na
            integer, intent(in) :: b, nb
            integer, intent(in) :: c, nc
            integer, intent(in) :: d, nd
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            integer, dimension(:, :), intent(in) :: ShellPairLoc

            integer :: r0, r1
            
            k0 = ShellPairLoc(d, b)
            k1 = ShellPairLoc(d, b) + Nd * Nb - 1
            r0 = ShellPairLoc(c, a)
            r1 = ShellPairLoc(c, a) + Nc * Na - 1
            call exch_VRhoCA(T, V, Rho(r0:r1), Na, Nb, Nc, Nd)
      end subroutine exch_digest_RhoCA


      pure subroutine exch_VRhoCA(T, V, Rho, Na, Nb, Nc, Nd)
            real(F64), dimension(Nd, Nb), intent(out) :: T
            real(F64), dimension(Nd, Nc, Nb, Na), intent(in) :: V
            real(F64), dimension(Nc, Na), intent(in) :: Rho
            integer, intent(in) :: Na, Nb, Nc, Nd

            integer :: a, b, c, d

            T = ZERO
            do a = 1, Na
                  do b = 1, Nb
                        do c = 1, Nc
                              do d = 1, Nd
                                    T(d, b) = T(d, b) + Rho(c, a) * V(d, c, b, a)
                              end do
                        end do
                  end do
            end do
      end subroutine exch_VRhoCA
end module Exchange
