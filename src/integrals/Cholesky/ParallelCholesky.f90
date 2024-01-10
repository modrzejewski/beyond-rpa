! -------------------------------------------------------------------------------------
! Packed-storage integral-direct Cholesky decomposition of the Coulomb matrix
! -------------------------------------------------------------------------------------
! The main subroutine, chol_CoulombMatrix, computes the decomposition factors V=R**T*R
! up to some defined maximum number of Cholesky vectors or until the convergence
! condition is satisfied. In the resulting matrices, the permutational symmetry
! of AO indices within shell pairs is used. The algorithm manipulates
! and stores only the subset of shell pairs which pass the Schwarz inequality test.
!
! The implementation is based on the algorithm of Ref. 1 with some modifications:
! i) Sparse matrix storage is added. ii) The target decomposition
! error is defined as Tr(V-R**T*R). The trace convergence condition is originally
! proposed by Harbrecht et al. in Ref. 2. iii) The computation of a new Cholesky
! vector in each microiteration if fused with the subtraction of contribs originating
! from the previous Cholesky vectors.
!
! 1. Aquilante et al. Subsection 13.5, Cholesky Decomposition Techniques in Electronic
!    Structure Theory in Linear-Scaling Techniques in Computational Chemistry and
!    Physics: Methods and Applications, 301-343, Springer 2011;
!    doi: 10.1007/978-90-481-2853-2_13
! 2. Harbrecht, H., Peters, M., and Schneider R. Appl. Num. Math. 62, 428 (2012);
!    doi: 10.1016/j.apnum.2011.10.001
!
module ParallelCholesky
      use arithmetic
      use math_constants
      use io
      use Auto2e
      use real_linalg
      use clock
      use string
      use sort
      use basis_sets
      use rpa_definitions
      use TwoStepCholesky_definitions
      
      implicit none

      integer, parameter :: SIGNIFICANT = 1
      integer, parameter :: QUALIFIED = 2
      integer, parameter :: INTERSECTION = 3
      integer, parameter :: BASE = 4

      integer, parameter :: FULL_STORAGE = 1
      integer, parameter :: SUBSET_STORAGE = 2
      integer, parameter :: SUBSET_INDEX = 3

      ! type TCholeskyBasis
      !       integer :: NVecs
      !       integer :: MaxSubsetDim
      !       integer, dimension(2) :: NSubsets
      !       integer :: NOrbPairs
      !       integer :: NShellPairs
      !       integer, dimension(:, :), allocatable :: ShellPairs
      !       integer, dimension(:, :), allocatable :: ShellPairLoc
      !       integer, dimension(:), allocatable :: ShellPairDim
      !       integer, dimension(:), allocatable :: SubsetDim
      !       integer, dimension(:, :), allocatable :: SubsetBounds
      ! end type TCholeskyBasis

contains

      subroutine chol_write_Vabcd(M, V, Nab, Ncd)
            real(F64), dimension(:, :), intent(out)    :: M
            real(F64), dimension(Nab, Ncd), intent(in) :: V
            integer, intent(in)                        :: Nab, Ncd

            M = V
      end subroutine chol_write_Vabcd


      subroutine chol_write_Vabcc(M, T, V, Nc, Nab, Ncd)
            real(F64), dimension(:, :), intent(out)       :: M
            real(F64), dimension(Nab, Ncd), intent(out)   :: T
            real(F64), dimension(Nab, Nc, Nc), intent(in) :: V
            integer, intent(in)                           :: Nc
            integer, intent(in)                           :: Nab, Ncd
            
            integer :: c, d, q

            q = 1
            do d = 1, Nc
                  do c = d, Nc
                        T(:, q) = V(:, c, d)
                        q = q + 1
                  end do
            end do
            M = T
      end subroutine chol_write_Vabcc


      subroutine chol_write_Vaacd(M, T, V, Na, Nab, Ncd)
            real(F64), dimension(:, :), intent(out)       :: M
            real(F64), dimension(Nab, Ncd), intent(out)   :: T
            real(F64), dimension(Na, Na, Ncd), intent(in) :: V
            integer, intent(in)                           :: Na
            integer, intent(in)                           :: Nab, Ncd
            
            integer :: a, b, p

            p = 1
            do b = 1, Na
                  do a = b, Na
                        T(p, :) = V(a, b, :)
                        p = p + 1
                  end do
            end do
            M = T
      end subroutine chol_write_Vaacd


      subroutine chol_write_Vaacc(M, T, V, Na, Nc, Nab, Ncd)
            real(F64), dimension(:, :), intent(out)          :: M
            real(F64), dimension(Nab, Ncd), intent(out)      :: T
            real(F64), dimension(Na, Na, Nc, Nc), intent(in) :: V
            integer, intent(in)                              :: Na, Nc
            integer, intent(in)                              :: Nab, Ncd
           
            integer :: a, b, c, d, p, q

            q = 1
            do d = 1, Nc
                  do c = d, Nc
                        p = 1
                        do b = 1, Na
                              do a = b, Na
                                    T(p, q) = V(a, b, c, d)
                                    p = p + 1
                              end do
                        end do
                        q = q + 1
                  end do
            end do
            M = T
      end subroutine chol_write_Vaacc
      
      
      subroutine chol_write_Vdiag(Vdiag, V, Na, Nb, DiagShell)
            real(F64), dimension(*), intent(out)             :: Vdiag
            real(F64), dimension(Na, Nb, Na, Nb), intent(in) :: V
            integer, intent(in)                              :: Na, Nb
            logical, intent(in)                              :: DiagShell

            integer :: a, b, pq

            pq = 1
            if (.not. DiagShell) then
                  do b = 1, Nb
                        do a = 1, Na
                              Vdiag(pq) = V(a, b, a, b)
                              pq = pq + 1
                        end do
                  end do
            else
                  do b = 1, Nb
                        do a = b, Na
                              Vdiag(pq) = V(a, b, a, b)
                              pq = pq + 1
                        end do
                  end do
            end if
      end subroutine chol_write_Vdiag
      

      subroutine chol_Vdiag(Vdiag, ShellCenters, AtomCoords, ShellParamsIdx, &
            ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, NormFactors, Kappa, &
            MaxNAngFunc, NAO, NShells, PtrOffset)
            
            real(F64), dimension((NAO*(NAO+1))/2), intent(out) :: Vdiag
            integer, dimension(NShells), intent(in)            :: ShellCenters
            real(F64), dimension(:, :), intent(in)             :: AtomCoords
            integer, dimension(:), intent(in)                  :: ShellParamsIdx
            integer, dimension(:), intent(in)                  :: ShellMomentum
            integer, dimension(:), intent(in)                  :: NAngFunc
            integer, dimension(:), intent(in)                  :: NPrimitives
            real(F64), dimension(:, :), intent(in)             :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)             :: Exponents
            real(F64), dimension(:, :), intent(in)             :: NormFactors
            real(F64), intent(in)                              :: Kappa
            integer, intent(in)                                :: MaxNAngFunc
            integer, intent(in)                                :: NAO
            integer, intent(in)                                :: NShells
            integer, intent(in)                                :: PtrOffset

            integer :: NOrbPairs, n
            integer :: AtomA, AtomB, ShA, ShB, Lb, Nb, La, Na
            integer :: ShellParamsA, ShellParamsB
            real(F64), dimension(MaxNAngFunc**4) :: V

            NOrbPairs = 0
            ShellB: do ShB = 1, NShells
                  AtomB = ShellCenters(ShB)
                  ShellParamsB = ShellParamsIdx(ShB)
                  Lb = ShellMomentum(ShellParamsB)
                  Nb = NAngFunc(ShellParamsB)
                  ShellA: do ShA = ShB, NShells
                        AtomA = ShellCenters(ShA)
                        ShellParamsA = ShellParamsIdx(ShA)
                        La = ShellMomentum(ShellParamsA)
                        Na = NAngFunc(ShellParamsA)
                                    
                        call Auto2eERI(PtrOffset+auto2e_idx(Lb, La, Lb, La))%ptr( &
                              V, &
                              !
                              ! ShellB
                              !
                              AtomCoords(:, AtomB), CntrCoeffs(:, ShellParamsB), &
                              NormFactors(:, ShellParamsB), Exponents(:, ShellParamsB), &
                              NPrimitives(ShellParamsB), &
                              !
                              ! ShellA
                              !
                              AtomCoords(:, AtomA), CntrCoeffs(:, ShellParamsA), &
                              NormFactors(:, ShellParamsA), Exponents(:, ShellParamsA), &
                              NPrimitives(ShellParamsA), &
                              !
                              ! ShellB
                              !
                              AtomCoords(:, AtomB), CntrCoeffs(:, ShellParamsB), &
                              NormFactors(:, ShellParamsB), Exponents(:, ShellParamsB), &
                              NPrimitives(ShellParamsB), &
                              !
                              ! ShellA
                              !
                              AtomCoords(:, AtomA), CntrCoeffs(:, ShellParamsA), &
                              NormFactors(:, ShellParamsA), Exponents(:, ShellParamsA), &
                              NPrimitives(ShellParamsA), &
                              Kappa)

                        if (ShA /= ShB) then
                              n = Na * Nb
                              call chol_write_Vdiag(Vdiag(NOrbPairs+1:NOrbPairs+n), &
                                    V(1:Na*Nb*Na*Nb), Na, Nb, .false.)
                        else
                              n = (Na * (Na + 1)) / 2
                              call chol_write_Vdiag(Vdiag(NOrbPairs+1:NOrbPairs+n), &
                                    V(1:Na*Nb*Na*Nb), Na, Nb, .true.)
                        end if
                        NOrbPairs = NOrbPairs + n
                  end do ShellA
            end do ShellB
      end subroutine chol_Vdiag


      subroutine chol_Base(ShellPairs, ShellPairLoc, ShellPairDim, NShellPairs, NOrbPairs, D, TraceError, &
            TauThresh, Vdiag, ShellParamsIdx, ShellMomentum, NAngFunc, NShells, NAO)
            !
            ! Generate the BASE set of shell pairs satisfying the following condition.
            !
            ! PQ is in BASE if
            !
            ! |(pq|pq)|*max|(rs|rs)| > min(TauThresh,10**(-8))**2
            ! |(pq|prs)| > min(TauThresh,10**(-8))**2/max|(rs|rs)| = PrescreenThresh
            !
            ! (Eq. 15 in Ref. 1)
            !
            ! The trace of discarded diagonal elements is passed on exit as TraceError.
            !
            ! The value of TragetTraceError should be a few orders of magnitude smaller
            ! than the primary threshold for the Cholesky decomposition.
            !
            ! Using the BASE shell pairs list will reduce both computational time
            ! and memory storage requirements at later stages of computation.
            !
            ! 1. Sarai D. Folkestad, Eirik F. Kjønstad and Henrik Koch,
            !    J. Chem. Phys. 150, 194112 (2019);
            !    doi: 10.1063/1.5083802
            !
            integer, dimension(:, :), intent(out)   :: ShellPairs
            integer, dimension(:, :), intent(out)   :: ShellPairLoc
            integer, dimension(:), intent(out)      :: ShellPairDim
            integer, dimension(:), intent(out)      :: NShellPairs
            integer, dimension(:), intent(out)      :: NOrbPairs
            real(F64), dimension(:), intent(out)    :: D
            real(F64), intent(out)                  :: TraceError
            real(F64), intent(in)                   :: TauThresh
            real(F64), dimension(:), intent(in)     :: Vdiag
            integer, dimension(:), intent(in)       :: ShellParamsIdx
            integer, dimension(:), intent(in)       :: ShellMomentum
            integer, dimension(:), intent(in)       :: NAngFunc
            integer, intent(in)                     :: NShells
            integer, intent(in)                     :: NAO

            integer :: ShellParamsA, ShellParamsB, ShA, ShB
            integer :: La, Lb, Na, Nb, p
            integer :: LocAB, ShAB, Nab
            integer :: k, l, k1
            integer, allocatable :: k0[:]
            real(F64), allocatable :: DiscardedTrace[:]
            integer :: MaxNShellPairs, MaxNOrbPairs
            real(F64) :: NextContrib
            real(F64), dimension(:), allocatable :: VdiagMax
            integer, dimension(:), allocatable :: VdiagDim, VdiagLoc, IdxMap[:]
            integer, dimension(:, :), allocatable :: VdiagPairs
            real(F64) :: Dpq, MaxDrs, PrescreenThresh
            integer :: ThisImage

            ThisImage = this_image()
            call msg("Prescreening (pq|pq) integrals for the Cholesky decomposition")
            call msg("Shell pairs will be sorted according to their max elements")
            MaxNOrbPairs = (NAO * (NAO + 1)) / 2
            MaxNShellPairs = (NShells * (NShells + 1)) / 2
            allocate(k0[*])
            allocate(DiscardedTrace[*])
            allocate(IdxMap(MaxNShellPairs)[*])
            allocate(VdiagMax(MaxNShellPairs))
            allocate(VdiagDim(MaxNShellPairs))
            allocate(VdiagLoc(MaxNShellPairs))
            allocate(VdiagPairs(2, MaxNShellPairs))
            LocAB = 1
            ShAB = 1
            do ShB = 1, NShells
                  do ShA = ShB, NShells
                        ShellParamsA = ShellParamsIdx(ShA)
                        La = ShellMomentum(ShellParamsA)
                        Na = NAngFunc(ShellParamsA)
                        ShellParamsB = ShellParamsIdx(ShB)
                        Lb = ShellMomentum(ShellParamsB)
                        Nb = NAngFunc(ShellParamsB)
                        if (ShA /= ShB) then
                              Nab = Na * Nb
                        else
                              Nab = (Na * (Na + 1)) / 2
                        end if
                        VdiagMax(ShAB) = maxval(Vdiag(LocAB:LocAB+Nab-1))
                        VdiagLoc(ShAB) = LocAB
                        VdiagDim(ShAB) = Nab
                        VdiagPairs(1, ShAB) = ShA
                        VdiagPairs(2, ShAB) = ShB
                        IdxMap(ShAB) = ShAB
                        ShAB = ShAB + 1
                        LocAB = LocAB + Nab
                  end do
            end do
            !
            ! Prescreen threshold according to Eq. 15 in Ref. 1
            !
            MaxDrs = maxval(VdiagMax)
            if (MaxDrs <= ZERO) then
                  call msg("Maximum diagonal Coulomb integral is nonpositive", MSG_ERROR)
                  error stop
            end if
            PrescreenThresh = min(TauThresh, 1.0E-8_F64)**2/MaxDrs
            call msg("Max diagonal integral MaxDrs="//str(MaxDrs,d=1))
            call msg("Applying Eq. 15 in J. Chem. Phys. 150, 194112 (2019); doi: 10.1063/1.5083802")
            call msg("Schwarz inequality: Dpq<min(10**(-8),Tau)**2/MaxDrs="//str(PrescreenThresh,d=1))
            if (ThisImage == 1) then
                  !
                  ! Sort the diagonal shells in increasing order. Take into account only
                  ! the largest integral in a shell quartet.
                  !
                  call dsort0(VdiagMax, IdxMap, MaxNShellPairs, 2)
                  !
                  ! Determine the trace discarded elements
                  !
                  DiscardedTrace = ZERO
                  k0 = 1
                  k1 = MaxNShellPairs
                  do k = 1, MaxNShellPairs
                        ShAB = IdxMap(k)
                        LocAB = VdiagLoc(ShAB)
                        Nab = VdiagDim(ShAB)
                        NextContrib = sum(Vdiag(LocAB:LocAB+Nab-1))
                        Dpq = maxval(Vdiag(LocAB:LocAB+Nab-1))
                        if (Dpq <= PrescreenThresh) then
                              DiscardedTrace = DiscardedTrace + NextContrib
                              k0 = k
                        else
                              exit
                        end if
                  end do
                  TraceError = DiscardedTrace
                  sync images(*)
            else
                  sync images(1)
                  k0 = k0[1]
                  k1 = MaxNShellPairs
                  TraceError = DiscardedTrace[1]
                  IdxMap = IdxMap(:)[1]
            end if
            NShellPairs(BASE) = k1 - k0 + 1
            NOrbPairs(BASE) = 0
            ShAB = 1
            p = 1
            D = ZERO
            do k = k0, k1
                  l = IdxMap(k)
                  Nab = VdiagDim(l)
                  ShA = VdiagPairs(1, l)
                  ShB = VdiagPairs(2, l)
                  ShellPairs(1, ShAB) = ShA
                  ShellPairs(2, ShAB) = ShB
                  ShellPairDim(ShAB) = Nab
                  ShellPairLoc(FULL_STORAGE, ShAB) = p
                  LocAB = VdiagLoc(l)
                  D(p:p+Nab-1) = Vdiag(LocAB:LocAB+Nab-1)
                  NOrbPairs(BASE) = NOrbPairs(BASE) + Nab
                  ShAB = ShAB + 1
                  p = p + Nab
            end do
            call msg("Trace of exact Coulomb matrix: " // str(sum(Vdiag), d=1))
            call msg("Trace of discarded integrals: " // str(TraceError, d=1))
            call msg("Discarded " // str(MaxNShellPairs-NShellPairs(BASE)) // " out of " // str(MaxNShellPairs) // " shell pairs")
            call msg("(" // str(MaxNOrbPairs-NOrbPairs(BASE)) // " out of " // str(MaxNOrbPairs) // " orbital pairs)")
      end subroutine chol_Base
      

      subroutine chol_Subsets(SubsetDim, SubsetBounds, ShellPairLoc, NSubsets, &
            ShellPairDim, NShellPairs, NOrbPairs, MaxBlockDim)
            integer, dimension(:), allocatable, intent(out)      :: SubsetDim
            integer, dimension(2), intent(out)                   :: NSubsets
            integer, dimension(:, :), allocatable, intent(out)   :: SubsetBounds
            integer, dimension(:, :), intent(inout)              :: ShellPairLoc
            integer, dimension(:), intent(in)                    :: ShellPairDim
            integer, dimension(:), intent(in)                    :: NShellPairs
            integer, dimension(:), intent(in)                    :: NOrbPairs
            integer, intent(in)                                  :: MaxBlockDim

            integer :: NTotal, NImages

            NImages = num_images()
            NSubsets(1) = ceiling(real(NOrbPairs(BASE),F64) / real(NImages*MaxBlockDim,F64))
            NSubsets(2) = NImages
            NTotal = NSubsets(1) * NSubsets(2)
            allocate(SubsetDim(NTotal))
            allocate(SubsetBounds(2, NTotal))
            call chol_DivideOrbPairs(SubsetDim, SubsetBounds, ShellPairLoc, NTotal, &
                  ShellPairDim, NShellPairs, NOrbPairs)
      end subroutine chol_Subsets
      

      subroutine chol_DivideOrbPairs(SubsetDim, SubsetBounds, ShellPairLoc, NSubsets, &
            ShellPairDim, NShellPairs, NOrbPairs)
            integer, dimension(:), intent(out)      :: SubsetDim
            integer, dimension(:, :), intent(out)   :: SubsetBounds
            integer, dimension(:, :), intent(inout) :: ShellPairLoc
            integer, intent(in)                     :: NSubsets
            integer, dimension(:), intent(in)       :: ShellPairDim
            integer, dimension(:), intent(in)       :: NShellPairs
            integer, dimension(:), intent(in)       :: NOrbPairs

            integer :: TargetDim
            integer :: k, s
            integer, dimension(2) :: BoundErr, BoundLoc

            TargetDim = ceiling(real(NOrbPairs(BASE), F64) / real(NSubsets, F64))
            do k = 1, NSubsets
                  if (k == 1) then
                        SubsetBounds(1, k) = 1
                  else
                        if (SubsetBounds(2, k-1) < NShellPairs(BASE) .and. &
                              SubsetBounds(2, k-1) > 0) then
                              SubsetBounds(1, k) = SubsetBounds(2, k-1) + 1
                        else
                              SubsetBounds(1, k) = 0
                        end if
                  end if
                  SubsetDim(k) = 0
                  SubsetBounds(2, k) = SubsetBounds(1, k)
                  if (SubsetBounds(1, k) > 0) then
                        if (k < NSubsets) then
                              BoundErr = NOrbPairs(BASE)
                              do s = SubsetBounds(1, k), NShellPairs(BASE)
                                    ShellPairLoc(SUBSET_STORAGE, s) = SubsetDim(k) + 1
                                    ShellPairLoc(SUBSET_INDEX, s) = k
                                    SubsetDim(k) = SubsetDim(k) + ShellPairDim(s)
                                    if (SubsetDim(k) >= TargetDim) then
                                          BoundLoc(2) = s
                                          BoundErr(2) = SubsetDim(k) - TargetDim
                                          exit
                                    else
                                          BoundLoc(1) = s
                                          BoundErr(1) = SubsetDim(k) - TargetDim
                                    end if
                              end do
                              if (abs(BoundErr(1)) < abs(BoundErr(2))) then
                                    SubsetBounds(2, k) = BoundLoc(1)
                                    SubsetDim(k) = BoundErr(1) + TargetDim
                              else
                                    SubsetBounds(2, k) = BoundLoc(2)
                                    SubsetDim(k) = BoundErr(2) + TargetDim
                              end if
                        else
                              SubsetBounds(2, k) = NShellPairs(BASE)
                              do s = SubsetBounds(1, k), SubsetBounds(2, k)
                                    ShellPairLoc(SUBSET_STORAGE, s) = SubsetDim(k) + 1
                                    ShellPairLoc(SUBSET_INDEX, s) = k
                                    SubsetDim(k) = SubsetDim(k) + ShellPairDim(s)
                              end do
                        end if
                  end if
            end do
      end subroutine chol_DivideOrbPairs


      ! subroutine chol_Significant(ShellPairMap, NShellPairs, NOrbPairs, DiscardError, ShellPairLoc, ShellPairDim, D, TargetTraceError)
      !       integer, dimension(:, :), intent(out) :: ShellPairMap[*]
      !       integer, dimension(:), intent(inout)  :: NShellPairs[*]
      !       integer, dimension(:), intent(inout)  :: NOrbPairs[*]
      !       real(F64), intent(inout)              :: DiscardError
      !       integer, dimension(:, :), intent(in)  :: ShellPairLoc
      !       integer, dimension(:), intent(in)     :: ShellPairDim
      !       real(F64), dimension(:), intent(in)   :: D
      !       real(F64), intent(in)                 :: TargetTraceError

      !       integer :: k1, k, Nab, ShAB, LocAB
      !       real(F64), dimension(:), allocatable :: MaxVals
      !       integer, dimension(:), allocatable :: IdxMap
      !       real(F64) :: PartialTrace, NextContrib
      !       integer :: ThisImage

      !       ThisImage = this_image()
      !       if (ThisImage == 1) then
      !             allocate(MaxVals(NShellPairs(SIGNIFICANT)))
      !             allocate(IdxMap(NShellPairs(SIGNIFICANT)))
      !             do k = 1, NShellPairs(SIGNIFICANT)
      !                   ShAB = ShellPairMap(k, SIGNIFICANT)
      !                   LocAB = ShellPairLoc(FULL_STORAGE, ShAB)
      !                   Nab = ShellPairDim(ShAB)
      !                   MaxVals(k) = maxval(D(LocAB:LocAB+Nab-1))
      !                   IdxMap(k) = ShAB
      !             end do
      !             !
      !             ! Sort the diagonal shells in decreasing order. Take into account only
      !             ! the largest integral in a shell quartet.
      !             !
      !             call dsort0(MaxVals, IdxMap, NShellPairs(SIGNIFICANT), -2)
      !             !
      !             ! Determine the trace discarded elements in the current step.
      !             ! Add the partial trace error to the cumulative
      !             ! trace error of the entire Cholesky decomposition
      !             !
      !             PartialTrace = ZERO
      !             k1 = NShellPairs(SIGNIFICANT)
      !             do k = NShellPairs(SIGNIFICANT), 1, -1
      !                   ShAB = IdxMap(k)
      !                   LocAB = ShellPairLoc(FULL_STORAGE, ShAB)
      !                   Nab = ShellPairDim(ShAB)
      !                   NextContrib = sum(D(LocAB:LocAB+Nab-1))
      !                   if (PartialTrace+NextContrib<TargetTraceError) then
      !                         PartialTrace = PartialTrace + NextContrib
      !                         k1 = k
      !                   else
      !                         exit
      !                   end if
      !             end do
      !             DiscardError = DiscardError + PartialTrace
      !             NShellPairs(SIGNIFICANT) = k1
      !             NOrbPairs(SIGNIFICANT) = 0
      !             do k = 1, k1
      !                   ShAB = IdxMap(k)
      !                   Nab = ShellPairDim(ShAB)
      !                   ShellPairMap(k, SIGNIFICANT) = ShAB
      !                   NOrbPairs(SIGNIFICANT) = NOrbPairs(SIGNIFICANT) + Nab
      !             end do
      !             sync images(*)
      !       else
      !             sync images(1)
      !             NOrbPairs = NOrbPairs(:)[1]
      !             NShellPairs = NShellPairs(:)[1]
      !             ShellPairMap = ShellPairMap(:, :)[1]
      !       end if
      ! end subroutine chol_Significant


      subroutine chol_Intersection(ShellPairMap, NShellPairs, NOrbPairs, ShellPairDim, SubsetBounds)
            integer, dimension(:, :), intent(inout) :: ShellPairMap
            integer, dimension(:), intent(inout)    :: NShellPairs
            integer, dimension(:), intent(inout)    :: NOrbPairs
            integer, dimension(:), intent(in)       :: ShellPairDim
            integer, dimension(2), intent(in)       :: SubsetBounds

            integer :: k, ShAB
            integer :: m, n

            m = 0
            n = 0
            do k = 1, NShellPairs(SIGNIFICANT)
                  ShAB = ShellPairMap(k, SIGNIFICANT)
                  if (SubsetBounds(1) <= ShAB .and. ShAB <= SubsetBounds(2)) then
                        m = m + 1
                        n = n + ShellPairDim(ShAB)
                        ShellPairMap(m, INTERSECTION) = ShAB
                  end if
            end do
            NShellPairs(INTERSECTION) = m
            NOrbPairs(INTERSECTION) = n
      end subroutine chol_Intersection
      

      subroutine chol_Qualified(ShellPairMap, NShellPairs, NOrbPairs, ColumnLoc, ShellPairDim, ShellPairLoc, &
            D, Dmin, MaxNQualified)
            integer, dimension(:, :), intent(inout) :: ShellPairMap[*]           
            integer, dimension(:), intent(inout)    :: NShellPairs[*]
            integer, dimension(:), intent(inout)    :: NOrbPairs[*]
            integer, dimension(:), intent(out)      :: ColumnLoc[*]
            integer, dimension(:), intent(in)       :: ShellPairDim
            integer, dimension(:, :), intent(in)    :: ShellPairLoc 
            real(F64), dimension(:), intent(in)     :: D
            real(F64), intent(in)                   :: Dmin
            integer, intent(in)                     :: MaxNQualified

            integer :: ShAB, LocAB, Nab, k
            real(F64), dimension(:), allocatable :: DiagSort
            integer, dimension(:), allocatable :: DiagISort
            real(F64) :: LargestElement
            integer :: ThisImage

            ThisImage = this_image()
            if (ThisImage == 1) then
                  allocate(DiagSort(NShellPairs(SIGNIFICANT)))
                  allocate(DiagISort(NShellPairs(SIGNIFICANT)))
                  do k = 1, NShellPairs(SIGNIFICANT)
                        ShAB = ShellPairMap(k, SIGNIFICANT)
                        LocAB = ShellPairLoc(FULL_STORAGE, ShAB)
                        Nab = ShellPairDim(ShAB)
                        DiagSort(k) = maxval(D(LocAB:LocAB+Nab-1))
                        DiagISort(k) = ShAB
                  end do
                  !
                  ! Sort the diagonal shells in descending order. Take into account only
                  ! the largest integral in a shell quartet.
                  !
                  call dsort0(DiagSort, DiagISort, NShellPairs(SIGNIFICANT), -2)
                  NShellPairs(QUALIFIED) = 0
                  NOrbPairs(QUALIFIED) = 0
                  do k = 1, NShellPairs(SIGNIFICANT)
                        LargestElement = DiagSort(k)
                        ShAB = DiagISort(k)
                        Nab = ShellPairDim(ShAB)
                        if (LargestElement > Dmin .and. NOrbPairs(QUALIFIED)+Nab <= MaxNQualified) then
                              LocAB = NOrbPairs(QUALIFIED) + 1
                              NShellPairs(QUALIFIED) = k
                              NOrbPairs(QUALIFIED) = NOrbPairs(QUALIFIED) + Nab
                              ShellPairMap(k, QUALIFIED) = ShAB
                              ColumnLoc(k) = LocAB
                        else
                              exit
                        end if
                  end do
                  sync images(*)
            else
                  sync images(1)
                  ShellPairMap = ShellPairMap(:, :)[1]
                  NShellPairs = NShellPairs(:)[1]
                  NOrbPairs = NOrbPairs(:)[1]
                  ColumnLoc = ColumnLoc(:)[1]
            end if
      end subroutine chol_Qualified
      

      subroutine chol_SubtractR(M, R, RQ, NCholesky, NOrbPairs, SubsetDim)
            !
            ! Subtract the contribution from all available Cholesky vectors from the Coulomb integrals matrix.
            ! (matrix DeltaPQ, step 5e in Ref. 1)
            !
            ! On exit, M has correct meaningful numbers for bra indices in the SIGNIFICANT
            ! set, but the data in the subset BASE-SIGNIFICANT are just artifacts.
            ! That is because the matrix M includes only the two-electron integrals
            ! with bra pairs in the SIGNIFICANT set of the current macroiteration,
            ! whereas the old vectors R from previous macroiterations, which are
            ! being subtracted from M, may contain nonzero numbers for all BASE
            ! set indices. The artifacts in the subset BASE-SIGNIFICANT are taken
            ! into account when a new Rk vector is generated in each microiteration.
            !
            ! 1. Aquilante et al. Subsection 13.5, Cholesky Decomposition Techniques in Electronic Structure Theory in
            !    Linear-Scaling Techniques in Computational Chemistry and Physics: Methods and Applications,
            !    301-343, Springer 2011; doi: 10.1007/978-90-481-2853-2_13
            !
            real(F64), dimension(:, :), intent(inout)      :: M
            real(F64), dimension(:, :), intent(in)         :: R
            real(F64), dimension(:, :), intent(in)         :: RQ
            integer, intent(in)                            :: NCholesky
            integer, dimension(:), intent(in)              :: NOrbPairs
            integer, intent(in)                            :: SubsetDim

            integer :: ldR, ldRQ, ldM

            ldR = size(R, dim=1)
            ldRQ = size(RQ, dim=1)
            ldM = size(M, dim=1)
            call real_aTb_x(M, ldM, R, ldR, RQ, ldRQ, &
                  SubsetDim, NOrbPairs(QUALIFIED), NCholesky, &
                  alpha=-ONE, beta=ONE)
      end subroutine chol_SubtractR

      
      subroutine chol_Qmax(QmaxLocS, QmaxLocQ, Qmax, D, ShellPairMap, &
            ShellPairLoc, NShellPairs, ShellPairDim, ColumnLoc, NSubsets)
            !
            ! Compute the largest diagonal element and its storage location in the QUALIFIED set.
            !
            integer, dimension(3), intent(out)   :: QmaxLocS[*]
            integer, intent(out)                 :: QmaxLocQ[*]
            real(F64), intent(out)               :: Qmax[*]
            real(F64), dimension(:), intent(in)  :: D
            integer, dimension(:, :), intent(in) :: ShellPairMap
            integer, dimension(:, :), intent(in) :: ShellPairLoc
            integer, dimension(:), intent(in)    :: NShellPairs
            integer, dimension(:), intent(in)    :: ShellPairDim
            integer, dimension(:), intent(in)    :: ColumnLoc
            integer, dimension(2), intent(in)    :: NSubsets

            integer :: k, ShAB, LocAB, Nab
            integer :: Kmax, Pmax
            integer :: p, q, qx, qy
            real(F64) ::  Qab
            integer :: ThisImage

            ThisImage = this_image()
            if (ThisImage == 1) then
                  Qmax = -ONE
                  do k = 1, NShellPairs(QUALIFIED)
                        ShAB = ShellPairMap(K, QUALIFIED)
                        LocAB = ShellPairLoc(FULL_STORAGE, ShAB)
                        Nab = ShellPairDim(ShAB)
                        do p = 1, Nab
                              Qab = D(LocAB+p-1)
                              if (Qab > Qmax) then
                                    Qmax = Qab
                                    Kmax = k
                                    Pmax = p
                              end if
                        end do
                  end do
                  ShAB = ShellPairMap(Kmax, QUALIFIED)
                  QmaxLocS(1) = ShellPairLoc(SUBSET_STORAGE, ShAB) + Pmax - 1
                  q = ShellPairLoc(SUBSET_INDEX, ShAB)
                  qy = (q - 1) / NSubsets(1) + 1
                  qx = q - (qy - 1) * NSubsets(1)
                  QmaxLocS(2) = qx
                  QmaxLocS(3) = qy
                  QmaxLocQ = ColumnLoc(Kmax) + Pmax - 1
                  sync images(*)
            else
                  sync images(1)
                  QmaxLocS = QmaxLocS(:)[1]
                  QmaxLocQ = QmaxLocQ[1]
                  Qmax = Qmax[1]
            end if
      end subroutine chol_Qmax


      subroutine chol_Dmax(Dmax, D, ShellPairMap, ShellPairLoc, NShellPairs, ShellPairDim)
            !
            ! Compute the largest diagonal element in the SIGNIFICANT set
            !
            real(F64), intent(out)               :: Dmax
            real(F64), dimension(:), intent(in)  :: D
            integer, dimension(:, :), intent(in) :: ShellPairMap
            integer, dimension(:, :), intent(in) :: ShellPairLoc
            integer, dimension(:), intent(in)    :: NShellPairs
            integer, dimension(:), intent(in)    :: ShellPairDim

            integer :: K, ShAB, LocAB, Nab
            integer :: ThisImage

            ThisImage = this_image()
            Dmax = -ONE
            if (ThisImage == 1) then
                  do K = 1, NShellPairs(SIGNIFICANT)
                        ShAB = ShellPairMap(K, SIGNIFICANT)
                        LocAB = ShellPairLoc(FULL_STORAGE, ShAB)
                        Nab = ShellPairDim(ShAB)
                        Dmax = max(Dmax, maxval(D(LocAB:LocAB+Nab-1)))
                  end do
            end if
      end subroutine chol_Dmax


      subroutine chol_Update_D(D, R, n)
            !
            ! Update the vector of resudual diagonals
            !
            real(F64), dimension(:), intent(inout) :: D
            real(F64), dimension(:), intent(in)    :: R
            integer, intent(in)                    :: n

            integer :: p

            do p = 1, n
                  D(p) = D(p) - R(p)**2
            end do
      end subroutine chol_Update_D
      

      subroutine chol_M(M, ShellPairMap, ShellPairLoc, ColumnLoc, NShellPairs, ShellPairs, ShellPairDim, &
            ShellCenters, AtomCoords, ShellParamsIdx, ShellMomentum, NAngFunc, &
            NPrimitives, CntrCoeffs, Exponents, NormFactors, Kappa, MaxNAngFunc, PtrOffset)

            real(F64), dimension(:, :), intent(out)  :: M
            integer, dimension(:, :), intent(in)     :: ShellPairMap
            integer, dimension(:, :), intent(in)     :: ShellPairLoc
            integer, dimension(:), intent(in)        :: ColumnLoc
            integer, dimension(:), intent(in)        :: NShellPairs
            integer, dimension(:, :), intent(in)     :: ShellPairs
            integer, dimension(:), intent(in)        :: ShellPairDim
            integer, dimension(:), intent(in)        :: ShellCenters
            real(F64), dimension(:, :), intent(in)   :: AtomCoords
            integer, dimension(:), intent(in)        :: ShellParamsIdx
            integer, dimension(:), intent(in)        :: ShellMomentum
            integer, dimension(:), intent(in)        :: NAngFunc
            integer, dimension(:), intent(in)        :: NPrimitives
            real(F64), dimension(:, :), intent(in)   :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)   :: Exponents
            real(F64), dimension(:, :), intent(in)   :: NormFactors
            real(F64), intent(in)                    :: Kappa
            integer, intent(in)                      :: MaxNAngFunc
            integer, intent(in)                      :: PtrOffset

            integer :: K, L, AtomA, AtomB, AtomC, AtomD
            integer :: ShellParamsA, ShellParamsB, ShellParamsC, ShellParamsD
            integer :: ShAB, ShA, ShB, p0, p1, Nab
            integer :: ShCD, ShC, ShD, q0, q1, Ncd
            integer :: La, Lb, Lc, Ld
            integer :: Na, Nb, Nc, Nd
            real(F64), dimension(MaxNAngFunc**4) :: V
            real(F64), dimension(MaxNAngFunc**4) :: T

            M = ZERO
            !$omp parallel do collapse(2) schedule(guided) &
            !$omp private(L, K) &
            !$omp private(ShAB, ShCD, ShA, ShB, ShC, ShD, AtomA, AtomB, AtomC, AtomD) &
            !$omp private(ShellParamsA, ShellParamsB, ShellParamsC, ShellParamsD) &
            !$omp private(La, Lb, Lc, Ld, Na, Nb, Nc, Nd, Nab, Ncd, p0, p1, q0, q1) &
            !$omp private(V, T) &
            !$omp default(shared)
            do L = 1, NShellPairs(QUALIFIED)
                  do K = 1, NShellPairs(INTERSECTION)
                        ShAB = ShellPairMap(K, INTERSECTION)
                        ShCD = ShellPairMap(L, QUALIFIED)
                        
                        ShA = ShellPairs(1, ShAB)
                        ShB = ShellPairs(2, ShAB)
                        ShC = ShellPairs(1, ShCD)
                        ShD = ShellPairs(2, ShCD)

                        AtomA = ShellCenters(ShA)
                        AtomB = ShellCenters(ShB)
                        AtomC = ShellCenters(ShC)
                        AtomD = ShellCenters(ShD)
                        
                        ShellParamsB = ShellParamsIdx(ShB)
                        ShellParamsA = ShellParamsIdx(ShA)
                        ShellParamsC = ShellParamsIdx(ShC)
                        ShellParamsD = ShellParamsIdx(ShD)

                        La = ShellMomentum(ShellParamsA)
                        Lb = ShellMomentum(ShellParamsB)
                        Lc = ShellMomentum(ShellParamsC)
                        Ld = ShellMomentum(ShellParamsD)

                        Na = NAngFunc(ShellParamsA)
                        Nb = NAngFunc(ShellParamsB)
                        Nc = NAngFunc(ShellParamsC)                        
                        Nd = NAngFunc(ShellParamsD)
                        
                        Nab = ShellPairDim(ShAB)
                        Ncd = ShellPairDim(ShCD)

                        p0 = ShellPairLoc(SUBSET_STORAGE, ShAB)
                        p1 = ShellPairLoc(SUBSET_STORAGE, ShAB) + Nab - 1
                        q0 = ColumnLoc(L)
                        q1 = ColumnLoc(L) + Ncd - 1

                        call Auto2eERI(PtrOffset+auto2e_idx(Ld, Lc, Lb, La))%ptr( &
                              V, &
                              !
                              ! ShellD
                              !
                              AtomCoords(:, AtomD), CntrCoeffs(:, ShellParamsD), &
                              NormFactors(:, ShellParamsD), Exponents(:, ShellParamsD), &
                              NPrimitives(ShellParamsD), &
                              !
                              ! ShellC
                              !
                              AtomCoords(:, AtomC), CntrCoeffs(:, ShellParamsC), &
                              NormFactors(:, ShellParamsC), Exponents(:, ShellParamsC), &
                              NPrimitives(ShellParamsC), &
                              !
                              ! ShellB
                              !
                              AtomCoords(:, AtomB), CntrCoeffs(:, ShellParamsB), &
                              NormFactors(:, ShellParamsB), Exponents(:, ShellParamsB), &
                              NPrimitives(ShellParamsB), &
                              !
                              ! ShellA
                              !
                              AtomCoords(:, AtomA), CntrCoeffs(:, ShellParamsA), &
                              NormFactors(:, ShellParamsA), Exponents(:, ShellParamsA), &
                              NPrimitives(ShellParamsA), &
                              Kappa)

                        if (ShC /= ShD) then
                              if (ShA /= ShB) then
                                    call chol_write_Vabcd(M(p0:p1, q0:q1), V(1:Nab*Ncd), Nab, Ncd)
                              else
                                    call chol_write_Vaacd(M(p0:p1, q0:q1), T(1:Nab*Ncd), V(1:Na*Nb*Nc*Nd), Na, Nab, Ncd)
                              end if
                        else
                              if (ShA /= ShB) then
                                    call chol_write_Vabcc(M(p0:p1, q0:q1), T(1:Nab*Ncd), V(1:Na*Nb*Nc*Nd), Nc, Nab, Ncd)
                              else
                                    call chol_write_Vaacc(M(p0:p1, q0:q1), T(1:Nab*Ncd), V(1:Na*Nb*Nc*Nd), Na, Nc, Nab, Ncd)
                              end if
                        end if
                  end do
            end do
            !$omp end parallel do
      end subroutine chol_M
      

      subroutine chol_Rk(Rk, Tk, Mk, RQk, R, j0, j1, ShellPairMap, ShellPairLoc, &
            NShellPairs, ShellPairDim, Qmax, SubsetDim)
            real(F64), dimension(:), intent(out)   :: Rk
            real(F64), dimension(:), intent(out)   :: Tk            
            real(F64), dimension(:), intent(in)    :: Mk
            real(F64), dimension(:), intent(in)    :: RQk
            real(F64), dimension(:, :), intent(in) :: R
            integer, intent(in)                    :: j0, j1
            integer, dimension(:, :), intent(in)   :: ShellPairMap
            integer, dimension(:, :), intent(in)   :: ShellPairLoc
            integer, dimension(:), intent(in)      :: NShellPairs
            integer, dimension(:), intent(in)      :: ShellPairDim
            real(F64), intent(in)                  :: Qmax
            integer, intent(in)                    :: SubsetDim

            integer :: p, j, p0, p1, ShAB, Nab, k
            real(F64) :: t

            if (j1 >= j0) then
                  !$omp parallel do private(p, t, j) default(shared)
                  do p = 1, SubsetDim
                        t = ZERO
                        do j = j0, j1
                              t = t + R(j, p) * RQk(j-j0+1)
                        end do
                        Tk(p) = Mk(p) - t
                  end do
                  !$omp end parallel do
            else
                  Tk = Mk
            end if
            !
            ! At this point Tk has correct numbers for indices in the SIGNIFICANT
            ! set, but the data in the subset BASE-SIGNIFICANT are just artifacts.
            ! That is because the matrix M includes the two-electron integrals
            ! with bra pairs only in the SIGNIFICANT set of the current macroiteration,
            ! whereas the old vectors R from previous macroiterations, which are
            ! being subtracted from M, may contain nonzero numbers for all BASE
            ! set indices. Taking it into account, the meaningful part of Tk
            ! is copied to Rk, with zeros placed for locations in BASE-SIGNIFICANT.
            !
            Rk = ZERO
            do k = 1, NShellPairs(INTERSECTION)
                  ShAB = ShellPairMap(k, INTERSECTION)
                  Nab = ShellPairDim(ShAB)
                  p0 = ShellPairLoc(SUBSET_STORAGE, ShAB)
                  p1 = ShellPairLoc(SUBSET_STORAGE, ShAB) + Nab - 1
                  Rk(p0:p1) = Tk(p0:p1) / sqrt(Qmax)
            end do
      end subroutine chol_Rk
      

      subroutine chol_MainLoop(R, NCholesky, NShellPairs, NOrbPairs, SubsetDim, SubsetBounds, &
            NSubsets, D, PrescreenError, MaxNCholesky, TauThresh, &
            ShellPairs, ShellPairLoc, ShellPairDim, ShellCenters, &
            AtomCoords, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, &
            NormFactors, Kappa, MaxNAngFunc, NAO, PtrOffset)
            !
            ! Generate the Cholesky vectors of the Coulomb matrix decomposition V = R**T * R.
            !
            ! 1. Harbrecht, H., Peters, M., and Schneider, R. Appl. Num. Math. 62, 428 (2012);
            !    doi: 10.1016/j.apnum.2011.10.001
            ! 2. Koch, H., de Meras, A.S., and Pedersen, T.B. J. Chem. Phys. 118, 9481 (2003);
            !    doi: 10.1063/1.1578621
            !
            real(F64), dimension(:, :, :), allocatable, intent(inout) :: R[:]
            integer, intent(out)                                      :: NCholesky
            integer, dimension(:), intent(inout)                      :: NShellPairs[*]
            integer, dimension(:), intent(inout)                      :: NOrbPairs[*]
            integer, dimension(:), intent(in)                         :: SubsetDim
            integer, dimension(:, :), intent(in)                      :: SubsetBounds
            integer, dimension(2), intent(in)                         :: NSubsets
            real(F64), dimension(:), intent(inout)                    :: D[*]
            real(F64), intent(in)                                     :: PrescreenError
            integer, intent(in)                                       :: MaxNCholesky
            real(F64), intent(in)                                     :: TauThresh
            integer, dimension(:, :), intent(in)                      :: ShellPairs
            integer, dimension(:, :), intent(in)                      :: ShellPairLoc
            integer, dimension(:), intent(in)                         :: ShellPairDim
            integer, dimension(:), intent(in)                         :: ShellCenters
            real(F64), dimension(:, :), intent(in)                    :: AtomCoords
            integer, dimension(:), intent(in)                         :: ShellParamsIdx
            integer, dimension(:), intent(in)                         :: ShellMomentum
            integer, dimension(:), intent(in)                         :: NAngFunc
            integer, dimension(:), intent(in)                         :: NPrimitives
            real(F64), dimension(:, :), intent(in)                    :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)                    :: Exponents
            real(F64), dimension(:, :), intent(in)                    :: NormFactors
            real(F64), intent(in)                                     :: Kappa
            integer, intent(in)                                       :: MaxNAngFunc
            integer, intent(in)                                       :: NAO
            integer, intent(in)                                       :: PtrOffset

            integer, dimension(:, :), allocatable :: ShellPairMap[:]
            integer, dimension(:), allocatable :: ColumnLoc[:]
            real(F64), dimension(:), allocatable :: Rk, Tk, RQk
            real(F64), dimension(:, :), allocatable :: RQ[:]
            real(F64), dimension(:, :, :), allocatable :: M
            real(F64) :: Dmax, Dmin
            integer :: MaxNQualified
            integer :: i, j0, j1, j
            integer :: p0, p1, pp0, pp1
            integer :: K
            integer, dimension(:), allocatable :: QmaxLocS[:]
            integer, allocatable :: QmaxLocQ[:]
            real(F64), allocatable :: Qmax[:]
            integer :: Bx, By, Bra
            integer :: l, ShAB, Nab
            integer :: MaxSubsetDim
            real(F64) :: SumD, DiscardError, TraceError
            logical :: Converged
            logical, allocatable :: DoMicroIters[:], DoMacroIters[:]
            character(:), allocatable :: line
            real(F64) :: time_rTr, time_2e, time_rk
            type(tclock) :: timer, timer_iter, DeltaT
            real(F64) :: memory_M, memory_R
            logical, parameter :: UpdateSignificant = .true.
            integer :: ThisImage, NImages

            ThisImage = this_image()
            NImages = num_images()
            MaxSubsetDim = maxval(SubsetDim)
            MaxNQualified = min(NOrbPairs(BASE), max(MaxNAngFunc**2, 1000))
            call clock_start(timer)
            time_rTr = ZERO
            time_rk = ZERO
            time_2e = ZERO
            call blankline()
            call msg("Starting pivoted Cholesky decomposition")
            call msg("Dimensions of the Coulomb matrix: (" // str(NOrbPairs(BASE)) // "," // str(NOrbPairs(BASE)) // ")")
            call msg("Threshold for diagonal matrix elements: Dpq > " // str(TauThresh, d=1))
            call msg("Maximum number of Cholesky vectors: " // str(MaxNCholesky))
            call msg("Dimensions of 2e integral storage buffer: (" // str(NOrbPairs(BASE)) // "," // str(MaxNQualified) // ")")
            if (allocated(R)) deallocate(R)
            allocate(R(MaxNCholesky, MaxSubsetDim, NSubsets(1))[*])
            allocate(RQ(MaxNCholesky, MaxNQualified)[*])
            allocate(DoMicroIters[*])
            allocate(DoMacroIters[*])
            allocate(QmaxLocS(3)[*])
            allocate(QmaxLocQ[*])
            allocate(Qmax[*])
            allocate(ShellPairMap(NShellPairs(BASE), 3)[*])
            allocate(ColumnLoc(NShellPairs(BASE))[*])
            allocate(M(MaxSubsetDim, MaxNQualified, NSubsets(1)))
            allocate(Rk(MaxSubsetDim))
            allocate(Tk(MaxSubsetDim))
            allocate(RQk(MaxNQualified))
            memory_R = io_size_byte(R) / real(1024**3, F64)
            memory_M = io_size_byte(M) / real(1024**3, F64)
            call msg("Memory allocation per image")
            call msg("Cholesky vectors (R): " // str(memory_R, d=1) // " gigabytes")
            call msg("two-electron integrals (M):  " // str(memory_M, d=1) // " gigabytes")
            DiscardError = PrescreenError
            !
            ! Initialize the set of significant shell pairs. At the beginning it's identical to the base set.
            !
            NShellPairs(SIGNIFICANT) = NShellPairs(BASE)
            NOrbPairs(SIGNIFICANT) = NOrbPairs(BASE)
            do K = 1, NShellPairs(BASE)
                  ShellPairMap(K, SIGNIFICANT) = K
            end do
            R = ZERO
            NCholesky = 0
            i = 0
            SumD = sum(D)
            TraceError = PrescreenError + SumD
            call chol_Dmax(Dmax, D, ShellPairMap, ShellPairLoc, NShellPairs, ShellPairDim)
            line = lfield("#", 5) // lfield("Tr(V-R**T*R)", 14) // lfield("Dmin", 12) // lfield("Dmax", 12) &
                  // lfield("NCholesky", 12) // lfield("Time", 12)
            call midrule(width=63)
            call msg(line)
            call midrule(width=63)
            Converged = .false.
            DoMacroIters = (MaxNCholesky > 0)
            MacroIters: do while (DoMacroIters)
                  call clock_start(timer_iter)
                  i = i + 1
                  !
                  ! Spread factor sigma=0.01 defined in Eq. 8 of J. Chem. Phys. 150, 194112 (2019);
                  ! doi: 10.1063/1.5083802
                  !
                  Dmin = max(0.01_F64 * Dmax, TauThresh)
                  call chol_Qualified(ShellPairMap, NShellPairs, NOrbPairs, ColumnLoc, ShellPairDim, ShellPairLoc, &
                        D, Dmin, MaxNQualified)
                  call clock_start(DeltaT)
                  By = ThisImage
                  do Bx = 1, NSubsets(1)
                        Bra = Bx + (By - 1) * NSubsets(1)
                        call chol_Intersection(ShellPairMap, NShellPairs, NOrbPairs, ShellPairDim, SubsetBounds(:, Bra))
                        call chol_M(M(:, :, Bx), ShellPairMap, ShellPairLoc, ColumnLoc, NShellPairs, &
                              ShellPairs, ShellPairDim, ShellCenters, AtomCoords, ShellParamsIdx, ShellMomentum, NAngFunc, &
                              NPrimitives, CntrCoeffs, Exponents, NormFactors, Kappa, MaxNAngFunc, PtrOffset)
                  end do
                  time_2e = time_2e + clock_readwall(DeltaT)
                  if (NCholesky > 0) then
                        call clock_start(DeltaT)
                        !
                        ! Arrange the columns of R corresponding to the QUALIFIED set of indices
                        ! in a contiguous local memory storage RQ. The matrix RQ will then be
                        ! passed to highly optimized linear algebra subroutines.
                        !
                        do k = 1, NShellPairs(QUALIFIED)
                              ShAB = ShellPairMap(k, QUALIFIED)
                              Bra = ShellPairLoc(SUBSET_INDEX, ShAB)
                              !
                              ! Bra = Bx + (By - 1) * NSubsets(1)
                              ! where 1 <= Bx <= NSubsets(1)
                              ! 1 <= By <= NSubsets(2)
                              !
                              By = (Bra - 1) / NSubsets(1) + 1
                              Bx = Bra - (By - 1) * NSubsets(1)
                              if (By == ThisImage) then
                                    Nab = ShellPairDim(ShAB)
                                    p0 = ShellPairLoc(SUBSET_STORAGE, ShAB)
                                    p1 = ShellPairLoc(SUBSET_STORAGE, ShAB) + Nab - 1
                                    pp0 = ColumnLoc(k)
                                    pp1 = ColumnLoc(k) + Nab - 1
                                    do l = 1, NImages
                                          RQ(:, pp0:pp1)[l] = R(:, p0:p1, Bx)
                                    end do
                              end if
                        end do
                        ! 
                        ! The remote store operations have been done
                        ! using nonblocking one-side communication.
                        ! Wait for all images to finish to have
                        ! a completely defined matrix RQ.
                        !
                        sync all
                        By = ThisImage
                        do Bx = 1, NSubsets(1)
                              Bra = Bx + (By - 1) * NSubsets(1)
                              call chol_SubtractR(M(:, :, Bx), R(:, :, Bx), RQ, NCholesky, NOrbPairs, SubsetDim(Bra))
                        end do
                        time_rTr = time_rTr + clock_readwall(DeltaT)
                  end if
                  call chol_Qmax(QmaxLocS, QmaxLocQ, Qmax, D, ShellPairMap, ShellPairLoc, &
                        NShellPairs, ShellPairDim, ColumnLoc, NSubsets)
                  j0 = NCholesky + 1
                  j = 0
                  DoMicroIters = (NCholesky < MaxNCholesky .and. NOrbPairs(QUALIFIED) > 0)
                  MicroIters: do while (DoMicroIters)
                        j = j + 1
                        j1 = NCholesky
                        NCholesky = NCholesky + 1
                        call clock_start(DeltaT)
                        if (j1 >= j0) then
                              if (QmaxLocS(3) == ThisImage) then
                                    RQk(1:j1-j0+1) = R(j0:j1, QmaxLocS(1), QmaxLocS(2))
                              else
                                    RQk(1:j1-j0+1) = R(j0:j1, QmaxLocS(1), QmaxLocS(2))[QmaxLocS(3)]
                              end if
                        end if
                        By = ThisImage
                        do Bx = 1, NSubsets(1)
                              Bra = Bx + (By - 1) * NSubsets(1)
                              call chol_Intersection(ShellPairMap, NShellPairs, NOrbPairs, ShellPairDim, SubsetBounds(:, Bra))
                              call chol_Rk(Rk, Tk, M(:, QmaxLocQ, Bx), RQk, R(:, :, Bx), j0, j1, &
                                    ShellPairMap, ShellPairLoc, NShellPairs, ShellPairDim, &
                                    Qmax, SubsetDim(Bra))
                              R(NCholesky, :, Bx) = Rk
                              p0 = ShellPairLoc(FULL_STORAGE, SubsetBounds(1, Bra))
                              p1 = p0 + SubsetDim(Bra) - 1
                              call chol_Update_D(D(p0:p1), Rk(1:SubsetDim(Bra)), SubsetDim(Bra))
                        end do
                        if (ThisImage /= 1) then
                              Bra = 1 + (By - 1) * NSubsets(1)
                              p0 = ShellPairLoc(FULL_STORAGE, SubsetBounds(1, Bra))
                              Bra = By * NSubsets(1)
                              p1 = ShellPairLoc(FULL_STORAGE, SubsetBounds(1, Bra)) + SubsetDim(Bra) - 1
                              D(p0:p1)[1] = D(p0:p1)
                        end if
                        sync all
                        time_rk = time_rk + clock_readwall(DeltaT)
                        call chol_Qmax(QmaxLocS, QmaxLocQ, Qmax, D, ShellPairMap, ShellPairLoc, &
                              NShellPairs, ShellPairDim, ColumnLoc, NSubsets)
                        SumD = sum(D)
                        TraceError = PrescreenError + SumD
                        if (ThisImage == 1) then
                              if (Qmax <= Dmin) then
                                    DoMicroIters = .false.
                              end if
                              if (j == NOrbPairs(QUALIFIED) .or. NCholesky == MaxNCholesky) then
                                    DoMicroIters = .false.
                              end if
                              sync images(*)
                        else
                              sync images(1)
                              DoMicroIters = DoMicroIters[1]
                        end if
                  end do MicroIters
                  ! if (UpdateSignificant) then
                  !       call chol_Significant(ShellPairMap, NShellPairs, NOrbPairs, DiscardError, &
                  !             ShellPairLoc, ShellPairDim, D, TargetTraceErrorPrescreen/TEN**i)             
                  ! end if
                  line = lfield(str(i), 5) // lfield(str(TraceError,d=2), 14) // lfield(str(Dmin,d=1),12) &
                        // lfield(str(Dmax,d=1),12) // lfield(str(NCholesky), 12) &
                        // lfield(str(clock_readwall(timer_iter),d=1), 12)
                  call msg(line)
                  call chol_Dmax(Dmax, D, ShellPairMap, ShellPairLoc, NShellPairs, ShellPairDim)
                  if (ThisImage == 1) then
                        if (Dmax <= TauThresh) then
                              Converged = .true.
                              DoMacroIters = .false.
                        end if
                        if (NCholesky == MaxNCholesky .or. NOrbPairs(QUALIFIED) == 0) then
                              DoMacroIters = .false.
                        end if
                        sync images(*)
                  else
                        sync images(1)
                        DoMacroIters = DoMacroIters[1]
                  end if
            end do MacroIters
            call blankline()
            call msg("Cholesky decomposition completed")
            call msg("Computed " // str(NCholesky) // " Cholesky vectors")
            call msg("Trace of discarded matrix elements: " // str(DiscardError, d=1))
            call msg("Largest residual Dpq: " // str(Dmax, d=1))
            TraceError = PrescreenError + sum(D)
            call msg("Total trace error: Tr(Vexact-R**T R) = " // str(TraceError, d=1))
            call msg("NCholesky/NAO = " // str(real(NCholesky,F64)/real(NAO,F64), d=1))
            if (NCholesky == MaxNCholesky .and. .not. Converged) then
                  call msg("Max number of Cholesky vectors reached without convergence", MSG_ERROR)
                  error stop
            end if
            call msg("Cholesky decomposition completed in " // str(clock_readwall(timer),d=1) // " seconds")
            call msg("Detailed timings in seconds")
            call msg("subtract prev contribs I (M-R**T*R): " // str(time_rTr,d=1))
            call msg("subtract prev contribs II (Rk): " // str(time_rk,d=1))
            call msg("two-electron integrals: " // str(time_2e,d=1))
            call blankline()
      end subroutine chol_MainLoop
      

      subroutine chol_CoulombMatrix_A(R, NCholesky, ShellPairs, ShellPairLoc, ShellPairDim, NShellPairs, &
            NOrbPairs, SubsetDim, SubsetBounds, NSubsets, TauThresh, ShellCenters, AtomCoords, ShellParamsIdx, &
            ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, NormFactors, Kappa, LmaxGTO, &
            NAO, NShells, MaxNAOMult, SpherAO, MaxBlockDim)
            !
            ! Generate the Cholesky vectors matrix R: V = R**T * R, where V is the matrix
            ! of two electron Coulomb integrals (pq|rs).
            !
            ! Small diagonal Coulomb integrals are removed from the computations
            ! at the prescreening phase using the Schwarz inequality. The indices (ShA, ShB)
            ! of the remaining shell pairs AB=1,2,...,NShellPairs are stored in ShellPairs(AB).
            ! The location of the first matrix element of each shell pair is stored in ShellPairLoc(:, AB).
            ! NOrbPairs is the total number of orbital pairs remaining after the prescreening step;
            ! it is also the number of rows of R.
            !
            ! The algorithm for the Cholesky vectors generation is based on Ref. 1 with added
            ! sparse matrix storage capability and the target decomposition error defined as Tr(V-R**T*R).
            ! This convergence condition is proposed by Harbrecht et al. in Ref. 2.
            !
            ! 1. Aquilante et al. Subsection 13.5, Cholesky Decomposition Techniques in Electronic Structure Theory in
            !    Linear-Scaling Techniques in Computational Chemistry and Physics: Methods and Applications,
            !    301-343, Springer 2011; doi: 10.1007/978-90-481-2853-2_13
            ! 2. Harbrecht, H., Peters, M., and Schneider R. Appl. Num. Math. 62, 428 (2012); doi: 10.1016/j.apnum.2011.10.001
            !
            real(F64), dimension(:, :, :), allocatable, intent(inout)   :: R[:]
            integer, intent(out)                                        :: NCholesky
            integer, dimension(2, (NShells*(NShells+1))/2), intent(out) :: ShellPairs
            integer, dimension(3, (NShells*(NShells+1))/2), intent(out) :: ShellPairLoc
            integer, dimension((NShells*(NShells+1))/2), intent(out)    :: ShellPairDim
            integer, intent(out)                                        :: NShellPairs
            integer, intent(out)                                        :: NOrbPairs
            integer, dimension(:), allocatable, intent(out)             :: SubsetDim
            integer, dimension(:, :), allocatable, intent(out)          :: SubsetBounds
            integer, dimension(2), intent(out)                          :: NSubsets
            real(F64), intent(in)                                       :: TauThresh
            integer, dimension(NShells), intent(in)                     :: ShellCenters
            real(F64), dimension(:, :), intent(in)                      :: AtomCoords
            integer, dimension(:), intent(in)                           :: ShellParamsIdx
            integer, dimension(:), intent(in)                           :: ShellMomentum
            integer, dimension(:), intent(in)                           :: NAngFunc
            integer, dimension(:), intent(in)                           :: NPrimitives
            real(F64), dimension(:, :), intent(in)                      :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)                      :: Exponents
            real(F64), dimension(:, :), intent(in)                      :: NormFactors
            real(F64), intent(in)                                       :: Kappa
            integer, intent(in)                                         :: LmaxGTO
            integer, intent(in)                                         :: NAO
            integer, intent(in)                                         :: NShells
            real(F64), intent(in)                                       :: MaxNAOMult
            logical, intent(in)                                         :: SpherAO
            integer, intent(in)                                         :: MaxBlockDim

            real(F64) :: PrescreenError
            integer :: PtrOffset
            integer :: MaxNOrbPairs, MaxNCholesky, MaxNAngFunc, MaxSubsetDim
            integer, dimension(:), allocatable :: NShellPairs0[:], NOrbPairs0[:]
            real(F64), dimension(:), allocatable :: Vdiag
            real(F64), dimension(:), allocatable :: D[:]
            !
            ! Select the set of two-electron integral subroutines:
            ! Cartesian or spherical AOs
            !
            if (SpherAO) then
                  PtrOffset = AUTO2E_SPHER_OFFSET
                  MaxNAngFunc = 2 * LmaxGTO + 1
            else
                  PtrOffset = 0
                  MaxNAngFunc = ((LmaxGTO + 1) * (LmaxGTO + 2)) / 2
            end if
            !
            ! Complete all diagonal two-electron itegrals, without prescreening
            !
            MaxNOrbPairs = (NAO*(NAO+1))/2
            allocate(Vdiag(MaxNOrbPairs))
            call chol_Vdiag(Vdiag, ShellCenters, AtomCoords, ShellParamsIdx, &
                  ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, NormFactors, Kappa, &
                  MaxNAngFunc, NAO, NShells, PtrOffset)
            !
            ! Use the Schwarz inequality to prescreen orbital pairs related to small integrals.
            ! Shell pairs comprised of small integrals will be removed from the subsequent
            ! storage and computational steps
            !
            allocate(D(MaxNOrbPairs)[*])
            allocate(NShellPairs0(4)[*])
            allocate(NOrbPairs0(4)[*])
            call chol_Base(ShellPairs, ShellPairLoc, ShellPairDim, NShellPairs0, NOrbPairs0, D, PrescreenError, &
                  TauThresh, Vdiag, ShellParamsIdx, ShellMomentum, NAngFunc, NShells, NAO)
            call chol_Subsets(SubsetDim, SubsetBounds, ShellPairLoc, NSubsets, &
                  ShellPairDim, NShellPairs0, NOrbPairs0, MaxBlockDim)
            NShellPairs = NShellPairs0(BASE)
            NOrbPairs = NOrbPairs0(BASE)
            MaxNCholesky = min(ceiling(NAO * MaxNAOMult), NOrbPairs)
            MaxSubsetDim = maxval(SubsetDim)
            call chol_MainLoop(R, NCholesky, NShellPairs0, NOrbPairs0, SubsetDim, SubsetBounds, &
                  NSubsets, D, PrescreenError, MaxNCholesky, TauThresh, &
                  ShellPairs, ShellPairLoc, ShellPairDim, &
                  ShellCenters, AtomCoords, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, &
                  CntrCoeffs, Exponents, NormFactors, Kappa, MaxNAngFunc, NAO, PtrOffset)
      end subroutine chol_CoulombMatrix_A


      subroutine chol_CoulombMatrix_B(R, CholeskyBasis, AOBasis, RPAParams)
            !
            ! Generate the Cholesky vectors matrix R: V = R**T * R, where V is the matrix
            ! of two electron Coulomb integrals (pq|rs).
            !
            real(F64), dimension(:, :, :), allocatable, intent(inout) :: R[:]
            type(TChol2Vecs), intent(out)                             :: CholeskyBasis
            type(TAOBasis), intent(in)                                :: AOBasis
            type(TRPAParams), intent(in)                              :: RPAParams

            integer :: MaxNShellPairs
            
            MaxNShellPairs = (AOBasis%NShells * (AOBasis%NShells + 1)) / 2
            allocate(CholeskyBasis%ShellPairs(2, MaxNShellPairs))
            allocate(CholeskyBasis%ShellPairLoc(3, MaxNShellPairs))
            allocate(CholeskyBasis%ShellPairDim(MaxNShellPairs))
            if (AOBasis%SpherAO) then
                  call chol_CoulombMatrix_A( &
                        R, &
                        CholeskyBasis%NVecs, &
                        CholeskyBasis%ShellPairs, &
                        CholeskyBasis%ShellPairLoc, &
                        CholeskyBasis%ShellPairDim, &
                        CholeskyBasis%NShellPairs, &
                        CholeskyBasis%NOrbPairs, &
                        CholeskyBasis%SubsetDim, &
                        CholeskyBasis%SubsetBounds, &
                        CholeskyBasis%NSubsets, &
                        RPAParams%CholeskyTauThresh, & 
                        AOBasis%ShellCenters, &
                        AOBasis%AtomCoords, &
                        AOBasis%ShellParamsIdx, &
                        AOBasis%ShellMomentum, &
                        AOBasis%NAngFuncSpher, &
                        AOBasis%NPrimitives, &
                        AOBasis%CntrCoeffs, &
                        AOBasis%Exponents, &
                        AOBasis%NormFactorsSpher, &
                        RPAParams%Kappa, &
                        AOBasis%LmaxGTO, &
                        AOBasis%NAOSpher, &
                        AOBasis%NShells, &
                        RPAParams%MaxNAOMult, &
                        AOBasis%SpherAO, &
                        RPAParams%MaxBlockDim)
            else
                  call chol_CoulombMatrix_A( &
                        R, &
                        CholeskyBasis%NVecs, &
                        CholeskyBasis%ShellPairs, &
                        CholeskyBasis%ShellPairLoc, &
                        CholeskyBasis%ShellPairDim, &
                        CholeskyBasis%NShellPairs, &
                        CholeskyBasis%NOrbPairs, &
                        CholeskyBasis%SubsetDim, &
                        CholeskyBasis%SubsetBounds, &
                        CholeskyBasis%NSubsets, &
                        RPAParams%CholeskyTauThresh, &
                        AOBasis%ShellCenters, &
                        AOBasis%AtomCoords, &
                        AOBasis%ShellParamsIdx, &
                        AOBasis%ShellMomentum, &
                        AOBasis%NAngFuncCart, &
                        AOBasis%NPrimitives, &
                        AOBasis%CntrCoeffs, &
                        AOBasis%Exponents, &
                        AOBasis%NormFactorsCart, &
                        RPAParams%Kappa, &
                        AOBasis%LmaxGTO, &
                        AOBasis%NAOCart, &
                        AOBasis%NShells, &
                        RPAParams%MaxNAOMult, &
                        AOBasis%SpherAO, &
                        RPAParams%MaxBlockDim)
            end if
      end subroutine chol_CoulombMatrix_B
end module ParallelCholesky
