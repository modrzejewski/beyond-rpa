! -------------------------------------------------------------------------------------
! Two-step integral-direct Cholesky decomposition of the Coulomb matrix
! -------------------------------------------------------------------------------------
! Compute the pivoted Cholesky factorization V=R**T*R with threshold Dpq>Tau.
! The R(k,pq) factors store only the p>=q AO indices. Small diagonals are removed
! during the initial Schwarz inequality test.
!
! The implementation is based on the algorithms of Refs. 1 and 2. In the first step,
! only the pivots are computed according to Ref. 2. Those are then used in
! the second step by applying a standard non-pivoted Cholesky subroutine to
! a reduced-size Coulomb matrix of dimension (NCholesky, NCholesky).
!
! 1. Aquilante et al. Subsection 13.5, Cholesky Decomposition Techniques in Electronic
!    Structure Theory in Linear-Scaling Techniques in Computational Chemistry and
!    Physics: Methods and Applications, 301-343, Springer 2011;
!    doi: 10.1007/978-90-481-2853-2_13
! 2. Sarai D. Folkestad, Eirik F. Kjønstad and Henrik Koch,
!    J. Chem. Phys. 150, 194112 (2019);
!    doi: 10.1063/1.5083802
!
module TwoStepCholesky
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
      
      implicit none
      !
      ! Orbital pair subsets      
      !
      integer, parameter :: CHOL2_BASE = 1        ! All orbital pairs accepted after the initial Schwarz prescreening
      integer, parameter :: CHOL2_CANDIDATES = 2  ! Pivot candidates in a macroiteration of the pivot finding algorithm
      integer, parameter :: CHOL2_BATCH = 3       ! Subset of pivot candidates in a microiteration of the pivot finding algorithm
      !
      ! Orbital pair storage modes
      ! used in the ShellPairLoc array
      !
      integer, parameter :: CHOL2_FULL_STORAGE = 1        ! All orbital pairs of the BASE subset are stored
      integer, parameter :: CHOL2_COMPRESSED_STORAGE = 2  ! Only the orbital pairs of the CANDIDATES subset are stored
      integer, parameter :: CHOL2_SUBSET_STORAGE = 3      ! Orbital pairs of the BASE subset are divided into subsets
      integer, parameter :: CHOL2_SUBSET_INDEX = 4        ! of size at most equal to MaxBlockDim. Used for parallelization.

      type TChol2Vecs
            integer :: NVecs
            integer :: MaxSubsetDim
            integer, dimension(2) :: NSubsets
            integer :: NOrbPairs
            integer :: NShellPairs
            integer, dimension(:, :), allocatable :: ShellPairs
            integer, dimension(:, :), allocatable :: ShellPairLoc
            integer, dimension(:), allocatable :: ShellPairDim
            integer, dimension(:), allocatable :: SubsetDim
            integer, dimension(:, :), allocatable :: SubsetBounds
            integer, dimension(:), allocatable :: Pivots
      end type TChol2Vecs

      type TCompressedCholVecs
            real(F64), dimension(:, :), allocatable :: L
      end type TCompressedCholVecs

contains

      subroutine chol2_Compress(V, D, ShellPairLoc, ShellPairMap, NShellPairs, NOrbPairs, &
            ShellPairDim, NPivots, BlockDim)
            
            type(TCompressedCholVecs), dimension(:), intent(inout) :: V
            real(F64), dimension(:), intent(inout), allocatable    :: D
            integer, dimension(:, :), intent(inout)                :: ShellPairLoc
            integer, dimension(:, :), intent(in)                   :: ShellPairMap
            integer, dimension(:), intent(in)                      :: NShellPairs
            integer, dimension(:), intent(in)                      :: NOrbPairs
            integer, dimension(:), intent(in)                      :: ShellPairDim
            integer, intent(in)                                    :: NPivots
            integer, intent(in)                                    :: BlockDim

            integer :: g, ShAB, Nab
            integer :: g0, g1, h0, h1
            integer :: b, NBlocks, NCompressed
            real(F64), dimension(:, :), allocatable :: T
            integer, dimension(:), allocatable :: NewLoc, OldLoc
            real(F64), dimension(:), allocatable :: DOld

            if (NShellPairs(CHOL2_CANDIDATES) > 0 .and. NPivots > 0) then
                  allocate(NewLoc(NShellPairs(CHOL2_CANDIDATES)))
                  allocate(OldLoc(NShellPairs(CHOL2_CANDIDATES)))
                  NCompressed = 0
                  do g = 1, NShellPairs(CHOL2_CANDIDATES)
                        ShAB = ShellPairMap(g, CHOL2_CANDIDATES)
                        Nab = ShellPairDim(ShAB)
                        NewLoc(g) = NCompressed + 1
                        OldLoc(g) = ShellPairLoc(CHOL2_COMPRESSED_STORAGE, ShAB)
                        NCompressed = NCompressed + Nab
                  end do
                  if (.not. NCompressed == NOrbPairs(CHOL2_CANDIDATES)) then
                        call msg("Inconsistent data passed to chol2_Compress", MSG_ERROR)
                        error stop
                  end if
                  call move_alloc(from=D, to=DOld)
                  allocate(D(NOrbPairs(CHOL2_CANDIDATES)))
                  do g = 1, NShellPairs(CHOL2_CANDIDATES)
                        ShAB = ShellPairMap(g, CHOL2_CANDIDATES)
                        Nab = ShellPairDim(ShAB)
                        g0 = NewLoc(g)
                        g1 = NewLoc(g) + Nab - 1
                        h0 = OldLoc(g)
                        h1 = OldLoc(g) + Nab - 1
                        D(g0:g1) = DOld(h0:h1)
                  end do
                  !
                  ! Move pivot candidate columns to the front
                  ! and remove non-candidate columns from
                  ! the storage.
                  !
                  NBlocks = NPivots / BlockDim
                  if (modulo(NPivots, BlockDim) > 0) NBlocks = NBlocks + 1
                  do b = 1, NBlocks
                        call move_alloc(from=V(b)%L, to=T)
                        !
                        ! Reallocation of L with smaller number of columns saves memory
                        !
                        allocate(V(b)%L(BlockDim, NCompressed))
                        do g = 1, NShellPairs(CHOL2_CANDIDATES)
                              ShAB = ShellPairMap(g, CHOL2_CANDIDATES)
                              Nab = ShellPairDim(ShAB)
                              g0 = NewLoc(g)
                              g1 = NewLoc(g) + Nab - 1
                              h0 = OldLoc(g)
                              h1 = OldLoc(g) + Nab - 1
                              V(b)%L(:, g0:g1) = T(:, h0:h1)
                        end do
                  end do
                  if (NBlocks >= 1) deallocate(T)
                  do g = 1, NShellPairs(CHOL2_CANDIDATES)
                        ShAB = ShellPairMap(g, CHOL2_CANDIDATES)
                        ShellPairLoc(CHOL2_COMPRESSED_STORAGE, ShAB) = NewLoc(g)
                  end do
            end if
      end subroutine chol2_Compress

      
      subroutine chol2_write_Vabcd(M, V, Nab, Ncd)
            real(F64), dimension(:, :), intent(out)    :: M
            real(F64), dimension(Nab, Ncd), intent(in) :: V
            integer, intent(in)                        :: Nab, Ncd

            M = V
      end subroutine chol2_write_Vabcd


      subroutine chol2_write_Vabcc(M, T, V, Nc, Nab, Ncd)
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
      end subroutine chol2_write_Vabcc


      subroutine chol2_write_Vaacd(M, T, V, Na, Nab, Ncd)
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
      end subroutine chol2_write_Vaacd


      subroutine chol2_write_Vaacc(M, T, V, Na, Nc, Nab, Ncd)
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
      end subroutine chol2_write_Vaacc
      
      
      subroutine chol2_write_Vdiag(Vdiag, V, Na, Nb, DiagShell)
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
      end subroutine chol2_write_Vdiag
      

      subroutine chol2_Vdiag(Vdiag, ShellCenters, AtomCoords, ShellParamsIdx, &
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
                              call chol2_write_Vdiag(Vdiag(NOrbPairs+1:NOrbPairs+n), &
                                    V(1:Na*Nb*Na*Nb), Na, Nb, .false.)
                        else
                              n = (Na * (Na + 1)) / 2
                              call chol2_write_Vdiag(Vdiag(NOrbPairs+1:NOrbPairs+n), &
                                    V(1:Na*Nb*Na*Nb), Na, Nb, .true.)
                        end if
                        NOrbPairs = NOrbPairs + n
                  end do ShellA
            end do ShellB
      end subroutine chol2_Vdiag


      subroutine chol2_DefineBase(ShellPairs, ShellPairLoc, ShellPairDim, NShellPairs, NOrbPairs, D, TraceError, &
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
            ! Using the BASE shell pairs list will reduce both computational time
            ! and memory storage requirements at later stages of computation.
            !
            ! 1. Sarai D. Folkestad, Eirik F. Kjønstad and Henrik Koch,
            !    J. Chem. Phys. 150, 194112 (2019);
            !    doi: 10.1063/1.5083802
            !
            integer, dimension(:, :), intent(out)             :: ShellPairs
            integer, dimension(:, :), intent(out)             :: ShellPairLoc
            integer, dimension(:), intent(out)                :: ShellPairDim
            integer, dimension(:), intent(out)                :: NShellPairs
            integer, dimension(:), intent(out)                :: NOrbPairs
            real(F64), dimension(:), allocatable, intent(out) :: D
            real(F64), intent(out)                            :: TraceError
            real(F64), intent(in)                             :: TauThresh
            real(F64), dimension(:), intent(in)               :: Vdiag
            integer, dimension(:), intent(in)                 :: ShellParamsIdx
            integer, dimension(:), intent(in)                 :: ShellMomentum
            integer, dimension(:), intent(in)                 :: NAngFunc
            integer, intent(in)                               :: NShells
            integer, intent(in)                               :: NAO

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
            NShellPairs(CHOL2_BASE) = k1 - k0 + 1
            NOrbPairs(CHOL2_BASE) = 0
            ShAB = 1
            p = 1
            allocate(D(NOrbPairs(CHOL2_BASE)))
            D = ZERO
            do k = k0, k1
                  l = IdxMap(k)
                  Nab = VdiagDim(l)
                  ShA = VdiagPairs(1, l)
                  ShB = VdiagPairs(2, l)
                  ShellPairs(1, ShAB) = ShA
                  ShellPairs(2, ShAB) = ShB
                  ShellPairDim(ShAB) = Nab
                  ShellPairLoc(CHOL2_FULL_STORAGE, ShAB) = p
                  LocAB = VdiagLoc(l)
                  D(p:p+Nab-1) = Vdiag(LocAB:LocAB+Nab-1)
                  NOrbPairs(CHOL2_BASE) = NOrbPairs(CHOL2_BASE) + Nab
                  ShAB = ShAB + 1
                  p = p + Nab
            end do
            call msg("Trace of exact Coulomb matrix: " // str(sum(Vdiag), d=1))
            call msg("Trace of discarded integrals: " // str(TraceError, d=1))
            call msg("Discarded " // str(MaxNShellPairs-NShellPairs(CHOL2_BASE)) // " out of " // str(MaxNShellPairs) // " shell pairs")
            call msg("(" // str(MaxNOrbPairs-NOrbPairs(CHOL2_BASE)) // " out of " // str(MaxNOrbPairs) // " orbital pairs)")
      end subroutine chol2_DefineBase
      

      subroutine chol2_DefineSubsets(SubsetDim, SubsetBounds, ShellPairLoc, NSubsets, &
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
            NSubsets(1) = ceiling(real(NOrbPairs(CHOL2_BASE),F64) / real(NImages*MaxBlockDim,F64))
            NSubsets(2) = NImages
            NTotal = NSubsets(1) * NSubsets(2)
            allocate(SubsetDim(NTotal))
            allocate(SubsetBounds(2, NTotal))
            call chol2_DivideOrbPairs(SubsetDim, SubsetBounds, ShellPairLoc, NTotal, &
                  ShellPairDim, NShellPairs, NOrbPairs)
      end subroutine chol2_DefineSubsets
      

      subroutine chol2_DivideOrbPairs(SubsetDim, SubsetBounds, ShellPairLoc, NSubsets, &
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

            TargetDim = ceiling(real(NOrbPairs(CHOL2_BASE), F64) / real(NSubsets, F64))
            do k = 1, NSubsets
                  if (k == 1) then
                        SubsetBounds(1, k) = 1
                  else
                        if (SubsetBounds(2, k-1) < NShellPairs(CHOL2_BASE) .and. &
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
                              BoundErr = NOrbPairs(CHOL2_BASE)
                              do s = SubsetBounds(1, k), NShellPairs(CHOL2_BASE)
                                    ShellPairLoc(CHOL2_SUBSET_STORAGE, s) = SubsetDim(k) + 1
                                    ShellPairLoc(CHOL2_SUBSET_INDEX, s) = k
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
                              SubsetBounds(2, k) = NShellPairs(CHOL2_BASE)
                              do s = SubsetBounds(1, k), SubsetBounds(2, k)
                                    ShellPairLoc(CHOL2_SUBSET_STORAGE, s) = SubsetDim(k) + 1
                                    ShellPairLoc(CHOL2_SUBSET_INDEX, s) = k
                                    SubsetDim(k) = SubsetDim(k) + ShellPairDim(s)
                              end do
                        end if
                  end if
            end do
      end subroutine chol2_DivideOrbPairs


      subroutine chol2_DefineCandidates(ShellPairMap, NShellPairs, NOrbPairs, ShellPairLoc, ShellPairDim, D, Tau)
            integer, dimension(:, :), intent(out) :: ShellPairMap
            integer, dimension(:), intent(inout)  :: NShellPairs
            integer, dimension(:), intent(inout)  :: NOrbPairs
            integer, dimension(:, :), intent(in)  :: ShellPairLoc
            integer, dimension(:), intent(in)     :: ShellPairDim
            real(F64), dimension(:), intent(in)   :: D
            real(F64), intent(in)                 :: Tau

            integer :: k, Nab, ShAB, LocAB
            real(F64), dimension(:), allocatable :: MaxVals
            integer, dimension(:), allocatable :: IdxMap
            integer :: NShellPairsOld

            allocate(MaxVals(NShellPairs(CHOL2_CANDIDATES)))
            allocate(IdxMap(NShellPairs(CHOL2_CANDIDATES)))
            do k = 1, NShellPairs(CHOL2_CANDIDATES)
                  ShAB = ShellPairMap(k, CHOL2_CANDIDATES)
                  LocAB = ShellPairLoc(CHOL2_COMPRESSED_STORAGE, ShAB)
                  Nab = ShellPairDim(ShAB)
                  MaxVals(k) = maxval(D(LocAB:LocAB+Nab-1))
                  IdxMap(k) = ShAB
            end do
            !
            ! Sort the diagonal shells in decreasing order. Take into account only
            ! the largest integral in a shell quartet.
            !
            call dsort0(MaxVals, IdxMap, NShellPairs(CHOL2_CANDIDATES), -2)
            NShellPairsOld = NShellPairs(CHOL2_CANDIDATES)
            NShellPairs(CHOL2_CANDIDATES) = 0
            NOrbPairs(CHOL2_CANDIDATES) = 0
            do k = 1, NShellPairsOld
                  ShAB = IdxMap(k)
                  if (MaxVals(k) > Tau) then
                        Nab = ShellPairDim(ShAB)
                        NShellPairs(CHOL2_CANDIDATES) = NShellPairs(CHOL2_CANDIDATES) + 1
                        ShellPairMap(k, CHOL2_CANDIDATES) = ShAB
                        NOrbPairs(CHOL2_CANDIDATES) = NOrbPairs(CHOL2_CANDIDATES) + Nab
                  else
                        exit
                  end if
            end do
      end subroutine chol2_DefineCandidates


      subroutine chol2_DefineBatch(ShellPairMap, NShellPairs, NOrbPairs, ColumnLoc, ShellPairDim, ShellPairLoc, &
            D, Dmin, MaxNBatch)
            integer, dimension(:, :), intent(inout) :: ShellPairMap
            integer, dimension(:), intent(inout)    :: NShellPairs
            integer, dimension(:), intent(inout)    :: NOrbPairs
            integer, dimension(:), intent(out)      :: ColumnLoc
            integer, dimension(:), intent(in)       :: ShellPairDim
            integer, dimension(:, :), intent(in)    :: ShellPairLoc 
            real(F64), dimension(:), intent(in)     :: D
            real(F64), intent(in)                   :: Dmin
            integer, intent(in)                     :: MaxNBatch

            integer :: ShAB, LocAB, Nab, k
            real(F64), dimension(:), allocatable :: DiagSort
            integer, dimension(:), allocatable :: DiagISort
            real(F64) :: LargestElement

            allocate(DiagSort(NShellPairs(CHOL2_CANDIDATES)))
            allocate(DiagISort(NShellPairs(CHOL2_CANDIDATES)))
            do k = 1, NShellPairs(CHOL2_CANDIDATES)
                  ShAB = ShellPairMap(k, CHOL2_CANDIDATES)
                  LocAB = ShellPairLoc(CHOL2_COMPRESSED_STORAGE, ShAB)
                  Nab = ShellPairDim(ShAB)
                  DiagSort(k) = maxval(D(LocAB:LocAB+Nab-1))
                  DiagISort(k) = ShAB
            end do
            !
            ! Sort the diagonal shells in descending order. Take into account only
            ! the largest integral in a shell quartet.
            !
            call dsort0(DiagSort, DiagISort, NShellPairs(CHOL2_CANDIDATES), -2)
            NShellPairs(CHOL2_BATCH) = 0
            NOrbPairs(CHOL2_BATCH) = 0
            do k = 1, NShellPairs(CHOL2_CANDIDATES)
                  LargestElement = DiagSort(k)
                  ShAB = DiagISort(k)
                  Nab = ShellPairDim(ShAB)
                  if (LargestElement > Dmin .and. NOrbPairs(CHOL2_BATCH)+Nab <= MaxNBatch) then
                        LocAB = NOrbPairs(CHOL2_BATCH) + 1
                        NShellPairs(CHOL2_BATCH) = k
                        NOrbPairs(CHOL2_BATCH) = NOrbPairs(CHOL2_BATCH) + Nab
                        ShellPairMap(k, CHOL2_BATCH) = ShAB
                        ColumnLoc(k) = LocAB
                  else
                        exit
                  end if
            end do
      end subroutine chol2_DefineBatch


      subroutine chol2_SubtractR(M, V, ShellPairMap, ShellPairDim, ShellPairLoc, ColumnLoc, NOrbPairs, &
            NShellPairs, NCholesky, BlockDim)
            !
            ! Subtract the contribution from all available Cholesky vectors from the Coulomb integrals matrix.
            ! (matrix DeltaPQ, step 5e in Ref. 1)
            !
            ! 1. Aquilante et al. Subsection 13.5, Cholesky Decomposition Techniques in Electronic Structure Theory in
            !    Linear-Scaling Techniques in Computational Chemistry and Physics: Methods and Applications,
            !    301-343, Springer 2011; doi: 10.1007/978-90-481-2853-2_13
            !
            real(F64), dimension(:, :), intent(inout)              :: M
            type(TCompressedCholVecs), dimension(:), intent(in)    :: V
            integer, dimension(:, :), intent(in)                   :: ShellPairMap
            integer, dimension(:), intent(in)                      :: ShellPairDim
            integer, dimension(:, :), intent(in)                   :: ShellPairLoc
            integer, dimension(:), intent(in)                      :: ColumnLoc
            integer, dimension(:), intent(in)                      :: NOrbPairs
            integer, dimension(:), intent(in)                      :: NShellPairs
            integer, intent(in)                                    :: NCholesky
            integer, intent(in)                                    :: BlockDim

            integer :: NBlocks, b
            integer :: k, ShAB, Nab, p0, p1, q0, q1
            real(F64), dimension(:, :), allocatable :: LB
            integer :: ldM, J

            ldM = NOrbPairs(CHOL2_CANDIDATES)
            NBlocks = NCholesky / BlockDim
            if (modulo(NCholesky, BlockDim) > 0) NBlocks = NBlocks + 1
            if (NBlocks > 0) then
                  allocate(LB(BlockDim, NOrbPairs(CHOL2_BATCH)))
                  do b = 1, NBlocks
                        !
                        ! Copy columns belonging to the BATCH subset into
                        ! a contiguous memory location. This enables efficient
                        ! matrix multiplication.
                        !
                        do k = 1, NShellPairs(CHOL2_BATCH)
                              ShAB = ShellPairMap(k, CHOL2_BATCH)
                              Nab = ShellPairDim(ShAB)
                              p0 = ShellPairLoc(CHOL2_COMPRESSED_STORAGE, ShAB)
                              p1 = ShellPairLoc(CHOL2_COMPRESSED_STORAGE, ShAB) + Nab - 1
                              q0 = ColumnLoc(k)
                              q1 = ColumnLoc(k) + Nab - 1
                              LB(:, q0:q1) = V(b)%L(:, p0:p1)
                        end do
                        if (b < NBlocks) then
                              J = BlockDim
                        else
                              !
                              ! The last stored block can have a smaller
                              ! number of data rows J <= BlockDim
                              !
                              J = NCholesky - (b - 1) * BlockDim
                        end if
                        !
                        ! M(:, :) <- M(:, :) - Sum(J) L(J, :)**T L(J, :)
                        !
                        call real_aTb_x(M, ldM, V(b)%L, BlockDim, LB, BlockDim, &
                              NOrbPairs(CHOL2_CANDIDATES), NOrbPairs(CHOL2_BATCH), J, &
                              alpha=-ONE, beta=ONE)
                  end do
            end if
      end subroutine chol2_SubtractR

      
      subroutine chol2_BatchMax(MaxDpq, MaxLoc, MaxLocBatch, D, ShellPairMap, &
            ShellPairLoc, NShellPairs, ShellPairDim, ColumnLoc)
            !
            ! Find the largest diagonal element and its storage location
            ! in the current batch.
            !
            real(F64), intent(out)               :: MaxDpq
            integer, intent(out)                 :: MaxLoc
            integer, intent(out)                 :: MaxLocBatch            
            real(F64), dimension(:), intent(in)  :: D
            integer, dimension(:, :), intent(in) :: ShellPairMap
            integer, dimension(:, :), intent(in) :: ShellPairLoc
            integer, dimension(:), intent(in)    :: NShellPairs
            integer, dimension(:), intent(in)    :: ShellPairDim
            integer, dimension(:), intent(in)    :: ColumnLoc

            integer :: k, ShAB, LocAB, Nab
            integer :: p
            real(F64) ::  Dpq

            MaxDpq = -ONE
            do k = 1, NShellPairs(CHOL2_BATCH)
                  ShAB = ShellPairMap(k, CHOL2_BATCH)
                  LocAB = ShellPairLoc(CHOL2_COMPRESSED_STORAGE, ShAB)
                  Nab = ShellPairDim(ShAB)
                  do p = 1, Nab
                        Dpq = D(LocAB+p-1)
                        if (Dpq > MaxDpq) then
                              MaxDpq = Dpq
                              MaxLoc = LocAB + p - 1
                              MaxLocBatch = ColumnLoc(k) + p - 1
                        end if
                  end do
            end do
      end subroutine chol2_BatchMax


      subroutine chol2_M(M, ShellPairMap, ShellPairLoc, ColumnLoc, NShellPairs, ShellPairs, ShellPairDim, &
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
            do L = 1, NShellPairs(CHOL2_BATCH)
                  do K = 1, NShellPairs(CHOL2_CANDIDATES)
                        ShAB = ShellPairMap(K, CHOL2_CANDIDATES)
                        ShCD = ShellPairMap(L, CHOL2_BATCH)
                        
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

                        p0 = ShellPairLoc(CHOL2_COMPRESSED_STORAGE, ShAB)
                        p1 = ShellPairLoc(CHOL2_COMPRESSED_STORAGE, ShAB) + Nab - 1
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
                                    call chol2_write_Vabcd(M(p0:p1, q0:q1), V(1:Nab*Ncd), Nab, Ncd)
                              else
                                    call chol2_write_Vaacd(M(p0:p1, q0:q1), T(1:Nab*Ncd), V(1:Na*Nb*Nc*Nd), Na, Nab, Ncd)
                              end if
                        else
                              if (ShA /= ShB) then
                                    call chol2_write_Vabcc(M(p0:p1, q0:q1), T(1:Nab*Ncd), V(1:Na*Nb*Nc*Nd), Nc, Nab, Ncd)
                              else
                                    call chol2_write_Vaacc(M(p0:p1, q0:q1), T(1:Nab*Ncd), V(1:Na*Nb*Nc*Nd), Na, Nc, Nab, Ncd)
                              end if
                        end if
                  end do
            end do
            !$omp end parallel do
      end subroutine chol2_M
      

      subroutine chol2_Pivots(Pivots, NCholesky, NShellPairs, NOrbPairs, &
            D, MaxNCholesky, TauThresh, &
            ShellPairs, ShellPairLoc, ShellPairDim, ShellCenters, &
            AtomCoords, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, &
            NormFactors, Kappa, MaxNAngFunc, NAO, PtrOffset, BlockDim)
            !
            ! Generate the Cholesky vectors of the Coulomb matrix decomposition V = R**T * R.
            !
            ! 1. Harbrecht, H., Peters, M., and Schneider, R. Appl. Num. Math. 62, 428 (2012);
            !    doi: 10.1016/j.apnum.2011.10.001
            ! 2. Koch, H., de Meras, A.S., and Pedersen, T.B. J. Chem. Phys. 118, 9481 (2003);
            !    doi: 10.1063/1.1578621
            !
            integer, dimension(:), intent(out)                        :: Pivots
            integer, intent(out)                                      :: NCholesky
            integer, dimension(:), intent(inout)                      :: NShellPairs
            integer, dimension(:), intent(inout)                      :: NOrbPairs
            real(F64), dimension(:), allocatable, intent(inout)       :: D
            integer, intent(in)                                       :: MaxNCholesky
            real(F64), intent(in)                                     :: TauThresh
            integer, dimension(:, :), intent(in)                      :: ShellPairs
            integer, dimension(:, :), intent(inout)                   :: ShellPairLoc
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
            integer, intent(in)                                       :: BlockDim

            integer, dimension(:, :), allocatable :: ShellPairMap
            integer, dimension(:), allocatable :: ColumnLoc
            real(F64), dimension(:), allocatable :: Lj, LjBatch
            real(F64), dimension(:, :), allocatable :: M
            real(F64) :: Dmax, Dmin
            integer :: MaxNBatch
            integer :: i, j0, j1, j
            integer :: p0, p1, q0, q1
            integer :: k,  ShAB, Nab
            real(F64) :: MaxDpq            
            integer :: MaxLoc, MaxLocBatch
            real(F64) :: MemoryMB, Percentage
            logical :: Converged
            logical :: DoMicroIters, DoMacroIters
            character(:), allocatable :: line
            real(F64) :: time_rTr, time_2e, time_rk, t_iter
            type(tclock) :: timer, timer_iter, DeltaT
            integer :: CurrentBlock, BlockBoundary, MaxNBlocks
            type(TCompressedCholVecs), dimension(:), allocatable   :: V

            call clock_start(timer)
            time_rTr = ZERO
            time_rk = ZERO
            time_2e = ZERO
            call blankline()
            call msg("Step 1 of Cholesky decomposition: locating pivots", underline=.true.)
            call msg("Dimensions of the Coulomb matrix: (" // str(NOrbPairs(CHOL2_BASE)) // "," // str(NOrbPairs(CHOL2_BASE)) // ")")
            call msg("Threshold for diagonal matrix elements: Dpq > " // str(TauThresh, d=1))
            call msg("Max number of Cholesky vectors: " // str(MaxNCholesky))
            MaxNBatch = min(NOrbPairs(CHOL2_BASE), max(MaxNAngFunc**2, 1000))
            call msg("Max dimension of 2e integrals buffer: (" // str(NOrbPairs(CHOL2_BASE)) // "," // str(MaxNBatch) // ")")
            allocate(ShellPairMap(NShellPairs(CHOL2_BASE), 3))
            allocate(ColumnLoc(NShellPairs(CHOL2_BASE)))
            MaxNBlocks = MaxNCholesky / BlockDim
            if (modulo(MaxNCholesky, BlockDim) > 0) MaxNBlocks = MaxNBlocks + 1
            allocate(V(MaxNBlocks))
            ! --------------------------------------------------------
            ! Initialize the CANDIDATES subset of shell pairs
            ! --------------------------------------------------------
            NShellPairs(CHOL2_CANDIDATES) = NShellPairs(CHOL2_BASE)
            NOrbPairs(CHOL2_CANDIDATES) = NOrbPairs(CHOL2_BASE)
            do k = 1, NShellPairs(CHOL2_CANDIDATES)
                  ShellPairMap(k, CHOL2_CANDIDATES) = k
                  ShellPairLoc(CHOL2_COMPRESSED_STORAGE, k) = ShellPairLoc(CHOL2_FULL_STORAGE, k)
            end do
            NCholesky = 0
            Pivots = 0
            i = 0 
            Dmax = maxval(D, dim=1)
            call blankline()
            call toprule()
            line = lfield("#", 5) // lfield("Dmax", 12) // lfield("Memory (MB)",15) //  &
                  lfield("NPivots", 12) // lfield("%", 5) // lfield("Time", 12)
            call msg(line)
            call midrule()
            CurrentBlock = 0
            BlockBoundary = 0
            Converged = .false.
            DoMacroIters = (MaxNCholesky > 0)
            MacroIters: do while (DoMacroIters)
                  call clock_start(timer_iter)
                  i = i + 1
                  ! ----------------------------------------------------------------------------------------------
                  !                Define pivot candidates in the current macro iteration
                  !
                  ! * Define the CANDIDATES subset in the current macro iteration
                  !   and determine the initial batch of AO pairs
                  ! * Reallocate arrays to a smaller dimension
                  ! ----------------------------------------------------------------------------------------------
                  call chol2_DefineCandidates(ShellPairMap, NShellPairs, NOrbPairs, ShellPairLoc, &
                        ShellPairDim, D, TauThresh)
                  call chol2_Compress(V, D, ShellPairLoc, ShellPairMap, NShellPairs, NOrbPairs, &
                        ShellPairDim, NCholesky, BlockDim)
                  MaxNBatch = min(NOrbPairs(CHOL2_CANDIDATES), max(MaxNAngFunc**2, 1000))
                  !
                  ! Spread factor sigma=0.01 defined in Eq. 8 of J. Chem. Phys. 150, 194112 (2019);
                  ! doi: 10.1063/1.5083802
                  !
                  Dmin = max(0.01_F64 * Dmax, TauThresh)
                  allocate(M(NOrbPairs(CHOL2_CANDIDATES), MaxNBatch))
                  allocate(Lj(NOrbPairs(CHOL2_CANDIDATES)))
                  allocate(LjBatch(MaxNBatch))
                  call chol2_DefineBatch(ShellPairMap, NShellPairs, NOrbPairs, ColumnLoc, ShellPairDim, ShellPairLoc, &
                        D, Dmin, MaxNBatch)
                  ! ----------------------------------------------------------------------------------------------
                  !                                  Two-electron integrals
                  ! ----------------------------------------------------------------------------------------------
                  call clock_start(DeltaT)
                  call chol2_M(M, ShellPairMap, ShellPairLoc, ColumnLoc, NShellPairs, ShellPairs, ShellPairDim, &
                        ShellCenters, AtomCoords, ShellParamsIdx, ShellMomentum, NAngFunc, &
                        NPrimitives, CntrCoeffs, Exponents, NormFactors, Kappa, MaxNAngFunc, PtrOffset)                  
                  time_2e = time_2e + clock_readwall(DeltaT)
                  ! ----------------------------------------------------------------------------------------------
                  !              Subtract contributions from all previous batches of Cholesky vectors
                  ! ----------------------------------------------------------------------------------------------
                  call clock_start(DeltaT)
                  call chol2_SubtractR(M, V, ShellPairMap, ShellPairDim, ShellPairLoc, ColumnLoc, NOrbPairs, &
                        NShellPairs, NCholesky, BlockDim)
                  time_rTr = time_rTr + clock_readwall(DeltaT)                  
                  call chol2_BatchMax(MaxDpq, MaxLoc, MaxLocBatch, D, ShellPairMap, &
                        ShellPairLoc, NShellPairs, ShellPairDim, ColumnLoc)
                  j0 = NCholesky + 1
                  j = 0
                  DoMicroIters = (NCholesky < MaxNCholesky .and. NOrbPairs(CHOL2_BATCH) > 0)
                  MicroIters: do while (DoMicroIters)
                        j = j + 1
                        j1 = NCholesky
                        ! ----------------------------------------------------------------------------------------
                        !                           Compute new Cholesky vector
                        ! ----------------------------------------------------------------------------------------
                        call clock_start(DeltaT)
                        NCholesky = NCholesky + 1
                        if (NCholesky > BlockBoundary) then
                              CurrentBlock = CurrentBlock + 1
                              allocate(V(CurrentBlock)%L(BlockDim, NOrbPairs(CHOL2_CANDIDATES)))
                              BlockBoundary = CurrentBlock * BlockDim
                        end if
                        Pivots(MaxLoc) = 1                        
                        Lj(:) = M(:, MaxLocBatch) / Sqrt(MaxDpq)
                        V(CurrentBlock)%L(NCholesky-BlockDim*(CurrentBlock-1), :) = Lj(:)
                        D(:) = D(:) - Lj(:)**2
                        time_rk = time_rk + clock_readwall(DeltaT)                        
                        ! ---------------------------------------------------------------------------
                        !                              Convergence test
                        ! ---------------------------------------------------------------------------
                        ! Conditions for ending the microiterations:
                        ! * Smallest Dpq in the batch is below Dmin
                        ! * All columns of M have been processed into new Cholesky vectors
                        ! * The maximum number of Cholesky vectors is reached
                        ! ---------------------------------------------------------------------------
                        call chol2_BatchMax(MaxDpq, MaxLoc, MaxLocBatch, D, ShellPairMap, &
                              ShellPairLoc, NShellPairs, ShellPairDim, ColumnLoc)
                        if (MaxDpq <= Dmin) then
                              DoMicroIters = .false.
                        end if
                        if (j == NOrbPairs(CHOL2_BATCH) .or. NCholesky == MaxNCholesky) then
                              DoMicroIters = .false.
                        end if
                        ! ---------------------------------------------------------------------------
                        !              Subtract contribution from the current Cholesky vector
                        !              if there is anything left in the current batch
                        ! ---------------------------------------------------------------------------
                        !
                        do k = 1, NShellPairs(CHOL2_BATCH)
                              ShAB = ShellPairMap(k, CHOL2_BATCH)
                              Nab = ShellPairDim(ShAB)
                              p0 = ShellPairLoc(CHOL2_COMPRESSED_STORAGE, ShAB)
                              p1 = ShellPairLoc(CHOL2_COMPRESSED_STORAGE, ShAB) + Nab - 1
                              q0 = ColumnLoc(k)
                              q1 = ColumnLoc(k) + Nab - 1
                              LjBatch(q0:q1) = Lj(p0:p1)
                        end do
                        !
                        ! M = M - Lj*Lj**T
                        !
                        call real_vwT(M, Lj, LjBatch, -ONE)
                  end do MicroIters                  
                  Dmax = maxval(D, dim=1)
                  if (Dmax <= TauThresh) then
                        Converged = .true.
                        DoMacroIters = .false.
                  end if
                  if (NCholesky == MaxNCholesky .or. NOrbPairs(CHOL2_BATCH) == 0) then
                        DoMacroIters = .false.
                  end if
                  deallocate(M)
                  deallocate(Lj)
                  deallocate(LjBatch)
                  MemoryMB = (storage_size(1.0_F64,kind=I64)*CurrentBlock*BlockDim*NOrbPairs(CHOL2_CANDIDATES)) &
                        / (8.0_F64*1024.0_F64*1024.0_F64)
                  Percentage = nint(100*(real(NOrbPairs(CHOL2_BASE)-NOrbPairs(CHOL2_CANDIDATES),F64)/real(NOrbPairs(CHOL2_BASE),F64)))
                  t_iter = clock_readwall(timer_iter)
                  line = lfield(str(i), 5) // lfield(str(Dmax,d=1), 12) // lfield(str(MemoryMB,d=1),15) // &
                        lfield(str(NCholesky), 12) // lfield(str(Percentage), 5) // lfield(str(t_Iter,d=1), 12)
                  call msg(line)
            end do MacroIters
            call blankline()
            call msg("Step 1 of the Cholesky decomposition completed")
            call msg("Computed " // str(NCholesky) // " pivots")
            call msg("Largest residual Dpq: " // str(Dmax, d=1))
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
      end subroutine chol2_Pivots
      

      subroutine chol2_Step1(Pivots, NCholesky, ShellPairs, ShellPairLoc, ShellPairDim, NShellPairs, &
            NOrbPairs, SubsetDim, SubsetBounds, NSubsets, TauThresh, ShellCenters, AtomCoords, ShellParamsIdx, &
            ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, NormFactors, Kappa, LmaxGTO, &
            NAO, NShells, SpherAO, OrbPairsBlock, CholVecsBlock)
            !
            ! Generate the pivots of the Cholesky factorization V = R**T * R.
            !
            ! Small diagonal Coulomb integrals are removed from the computations
            ! at the prescreening phase using the Schwarz inequality. The indices (ShA, ShB)
            ! of the remaining shell pairs AB=1,2,...,NShellPairs are stored in ShellPairs(AB).
            ! The location of the first matrix element of each shell pair is stored in ShellPairLoc(:, AB).
            ! NOrbPairs is the total number of orbital pairs remaining after the prescreening step,
            ! which is different from NAO*(NAO+1)/2.
            !
            ! The algorithm for the pivot generation is based on Refs. 1 and 2.
            !
            ! 1. S.D. Folkestad, E.F. Kjonstad, and H. Koch.,
            !    An efficient algorithm for Cholesky decomposition of electron
            !    repulsion integrals, J. Chem. Phys. 150, 194112 (2019);
            !    doi: 10.1063/1.5083802
            ! 2. Aquilante et al. Subsection 13.5, Cholesky Decomposition Techniques in Electronic Structure Theory in
            !    Linear-Scaling Techniques in Computational Chemistry and Physics: Methods and Applications,
            !    301-343, Springer 2011; doi: 10.1007/978-90-481-2853-2_13
            !
            integer, dimension(:), allocatable, intent(out)             :: Pivots
            integer, intent(out)                                        :: NCholesky
            integer, dimension(2, (NShells*(NShells+1))/2), intent(out) :: ShellPairs
            integer, dimension(4, (NShells*(NShells+1))/2), intent(out) :: ShellPairLoc
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
            logical, intent(in)                                         :: SpherAO
            integer, intent(in)                                         :: OrbPairsBlock
            integer, intent(in)                                         :: CholVecsBlock

            real(F64) :: PrescreenError
            integer :: PtrOffset
            integer :: MaxNOrbPairs, MaxNCholesky, MaxNAngFunc, MaxSubsetDim
            integer, dimension(:), allocatable :: NShellPairs0, NOrbPairs0
            real(F64), dimension(:), allocatable :: Vdiag
            real(F64), dimension(:), allocatable :: D
            integer :: ThisImage
            !
            ! Maximum number of Cholesky vectors expressed as a multiply
            ! of the number of AOs
            !
            real(F64), parameter :: MaxNAOMult = 15.0_F64

            ThisImage = this_image()
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
            call chol2_Vdiag(Vdiag, ShellCenters, AtomCoords, ShellParamsIdx, &
                  ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, NormFactors, Kappa, &
                  MaxNAngFunc, NAO, NShells, PtrOffset)
            !
            ! Use the Schwarz inequality to prescreen orbital pairs related to small integrals.
            ! Shell pairs comprised of small integrals will be removed from the subsequent
            ! storage and computational steps
            !
            allocate(NShellPairs0(4))
            allocate(NOrbPairs0(4))
            call chol2_DefineBase(ShellPairs, ShellPairLoc, ShellPairDim, NShellPairs0, NOrbPairs0, D, PrescreenError, &
                  TauThresh, Vdiag, ShellParamsIdx, ShellMomentum, NAngFunc, NShells, NAO)
            call chol2_DefineSubsets(SubsetDim, SubsetBounds, ShellPairLoc, NSubsets, &
                  ShellPairDim, NShellPairs0, NOrbPairs0, OrbPairsBlock)
            NShellPairs = NShellPairs0(CHOL2_BASE)
            NOrbPairs = NOrbPairs0(CHOL2_BASE)
            MaxNCholesky = min(ceiling(NAO * MaxNAOMult), NOrbPairs)
            MaxSubsetDim = maxval(SubsetDim)
            allocate(Pivots(NOrbPairs))
            if (ThisImage == 1) then
                  call chol2_Pivots(Pivots, NCholesky, NShellPairs0, NOrbPairs0, &
                        D, MaxNCholesky, TauThresh, &
                        ShellPairs, ShellPairLoc, ShellPairDim, ShellCenters, &
                        AtomCoords, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, Exponents, &
                        NormFactors, Kappa, MaxNAngFunc, NAO, PtrOffset, CholVecsBlock)
            end if
            call co_broadcast(Pivots, source_image=1)
      end subroutine chol2_Step1


      subroutine chol2_Step1_SimpleInterface(Chol2Vecs, AOBasis, RPAParams)
            type(TChol2Vecs), intent(out) :: Chol2Vecs
            type(TAOBasis), intent(in)    :: AOBasis
            type(TRPAParams), intent(in)  :: RPAParams

            integer :: MaxNShellPairs
            
            MaxNShellPairs = (AOBasis%NShells * (AOBasis%NShells + 1)) / 2
            allocate(Chol2Vecs%ShellPairs(2, MaxNShellPairs))
            allocate(Chol2Vecs%ShellPairLoc(4, MaxNShellPairs))
            allocate(Chol2Vecs%ShellPairDim(MaxNShellPairs))
            if (AOBasis%SpherAO) then
                  call chol2_Step1( &
                        Chol2Vecs%Pivots, &
                        Chol2Vecs%NVecs, &
                        Chol2Vecs%ShellPairs, &
                        Chol2Vecs%ShellPairLoc, &
                        Chol2Vecs%ShellPairDim, &
                        Chol2Vecs%NShellPairs, &
                        Chol2Vecs%NOrbPairs, &
                        Chol2Vecs%SubsetDim, &
                        Chol2Vecs%SubsetBounds, &
                        Chol2Vecs%NSubsets, &
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
                        AOBasis%SpherAO, &
                        RPAParams%MaxBlockDim, &
                        RPAParams%CholVecsBlock)
            else
                  call chol2_Step1( &
                        Chol2Vecs%Pivots, &
                        Chol2Vecs%NVecs, &
                        Chol2Vecs%ShellPairs, &
                        Chol2Vecs%ShellPairLoc, &
                        Chol2Vecs%ShellPairDim, &
                        Chol2Vecs%NShellPairs, &
                        Chol2Vecs%NOrbPairs, &
                        Chol2Vecs%SubsetDim, &
                        Chol2Vecs%SubsetBounds, &
                        Chol2Vecs%NSubsets, &
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
                        AOBasis%SpherAO, &
                        RPAParams%MaxBlockDim, &
                        RPAParams%CholVecsBlock)
            end if
      end subroutine chol2_Step1_SimpleInterface
end module TwoStepCholesky
