module rpa_Cholesky
      use arithmetic
      use math_constants
      use real_linalg
      use TwoStepCholesky
      use OneElectronInts
      use rpa_definitions
      use TwoStepCholesky_definitions
      use sys_definitions
      use basis_sets
      use clock
      use string

      implicit none

contains

      pure subroutine rpa_Cholesky_pq2p_ge_q(p, q, pq, n)
            !
            ! Decode a lower-triangle compound index into individual
            ! indices:
            ! PQ -> (P, Q)
            ! Assumptions:
            ! 0) P = 1, 2, ..., N,
            !    Q = 1, 2, ..., N,
            ! 1) P >= Q (diagonal indices admissible)
            !
            ! An example of how this algorithm traverses an N=3 triangle:
            !
            ! 1
            ! 2 5
            ! 3 6 4
            !
            integer, intent(out) :: p
            integer, intent(out) :: q
            integer, intent(in)  :: pq
            integer, intent(in)  :: n

            integer :: q_base
            integer :: v
            integer :: interval1
            integer :: in1, in2
            !
            ! pq = (q_base - 1) * (n + 1) + v
            !
            q_base = (pq - 1) / (n + 1) + 1
            v = pq - (n + 1) * (q_base - 1)
            !
            ! Decide if v is in interval_1 or interval_2:
            ! in1 == 1 and in2 == 0 if v <= INTERVAL1
            ! in1 == 0 and in2 == 1 if v > INTERVAL1
            !
            interval1 = n - q_base + 1
            in2 = v / (interval1 + 1)
            !
            ! 1 -> 0, 0 -> 1
            !
            in1 = ieor((in2), 1)

            p = in1 * (q_base + v - 1) + in2 * (v - interval1 + n - q_base)          
            q = in1 * q_base + in2 * interval1
      end subroutine rpa_Cholesky_pq2p_ge_q
      

      pure function rpa_Cholesky_p_ge_q2pq(p, q, m)
            !
            ! Compute compound 2-index, (pq), assuming that p is always
            ! greater or equal q. Min. index: 1, max index: M.
            ! Example: P and Q enumerate, respectively, rows and columns
            ! of a symmetric matrix. The compound 2-index enumerates 
            ! consecutive elements of the lower triangle of the 5x5 matrix.
            !
            ! M = 5
            !             Q 
            !    | 1                |
            !    | 2  6             |
            ! P  | 3  7  10         |
            !    | 4  8  11  13     |
            !    | 5  9  12  14  15 |
            !
            integer             :: rpa_Cholesky_p_ge_q2pq
            integer, intent(in) :: p
            integer, intent(in) :: q
            integer, intent(in) :: m

            integer :: i1, i2

            i1 = ((2 * m - q + 2) * (q - 1)) / 2
            i2 = p - q + 1
            rpa_Cholesky_p_ge_q2pq = i1 + i2
      end function rpa_Cholesky_p_ge_q2pq


      subroutine rpa_Cholesky_ReshapeJpq(Fpq, Jpq, p0, p1, q0, q1, Np, Nq)
            integer, intent(in)                       :: Np, Nq
            real(F64), dimension(:, :), intent(inout) :: Fpq
            real(F64), dimension(Np, Nq), intent(in)  :: Jpq
            integer, intent(in)                       :: p0, p1
            integer, intent(in)                       :: q0, q1

            Fpq(p0:p1, q0:q1) = Fpq(p0:p1, q0:q1) + Jpq
      end subroutine rpa_Cholesky_ReshapeJpq


      subroutine rpa_Cholesky_ReshapeJpqDiag(Fpq, Wpq, Jpq, p0, p1, Np)
            real(F64), dimension(:, :), intent(inout)       :: Fpq
            real(F64), dimension(Np, Np), intent(out)       :: Wpq
            real(F64), dimension((Np*(Np+1))/2), intent(in) :: Jpq
            integer, intent(in)                             :: p0, p1
            integer, intent(in)                             :: Np

            integer :: a, b, ab

            do b = 1, Np
                  do a = b, Np
                        ab = rpa_Cholesky_p_ge_q2pq(a, b, Np)
                        Wpq(a, b) = Jpq(ab)
                        Wpq(b, a) = Jpq(ab)
                  end do
            end do
            Fpq(p0:p1, p0:p1) = Fpq(p0:p1, p0:p1) + Wpq
      end subroutine rpa_Cholesky_ReshapeJpqDiag

      
      subroutine rpa_Cholesky_WriteJpq(Fpq, Jpq, SubsetBounds, ShellPairs, &
            ShellPairLoc, ShellLoc, ShellParamsIdx, NAngFunc, &
            LMaxGTO)

            real(F64), dimension(:, :), intent(inout) :: Fpq
            real(F64), dimension(:), intent(in)       :: Jpq
            integer, dimension(2), intent(in)         :: SubsetBounds
            integer, dimension(:, :), intent(in)      :: ShellPairs
            integer, dimension(:, :), intent(in)      :: ShellPairLoc
            integer, dimension(:), intent(in)         :: ShellLoc
            integer, dimension(:), intent(in)         :: ShellParamsIdx
            integer, dimension(:), intent(in)         :: NAngFunc
            integer, intent(in)                       :: LMaxGTO

            integer :: p0, q0, p1, q1
            integer :: ShAB, LocAB, Nab
            integer :: ShA, ShellParamsA, Na, LocA
            integer :: ShB, ShellParamsB, Nb, LocB
            real(F64), dimension(2*LMaxGTO+1, 2*LMaxGTO+1) :: Wab

            !$omp parallel do private(ShAB, LocAB, Wab, Nab) &
            !$omp private(ShA, ShellParamsA, Na, LocA) &
            !$omp private(ShB, ShellParamsB, Nb, LocB) &
            !$omp private(p0, p1, q0, q1) &
            !$omp default(shared)
            do ShAB = SubsetBounds(1), SubsetBounds(2)
                  LocAB = ShellPairLoc(CHOL2_SUBSET_STORAGE, ShAB)

                  ShA = ShellPairs(1, ShAB)
                  ShellParamsA = ShellParamsIdx(ShA)
                  Na = NAngFunc(ShellParamsA)
                  LocA = ShellLoc(ShA)

                  ShB = ShellPairs(2, ShAB)
                  ShellParamsB = ShellParamsIdx(ShB)
                  Nb = NAngFunc(ShellParamsB)
                  LocB = ShellLoc(ShB)

                  p0 = LocA
                  p1 = LocA + Na - 1
                  q0 = LocB
                  q1 = LocB + Nb - 1

                  if (ShA /= ShB) then
                        Nab = Na * Nb
                        call rpa_Cholesky_ReshapeJpq(Fpq, Jpq(LocAB:LocAB+Nab-1), &
                              p0, p1, q0, q1, Na, Nb)
                  else
                        Nab = (Na * (Na + 1)) / 2
                        call rpa_Cholesky_ReshapeJpqDiag(Fpq, Wab, Jpq(LocAB:LocAB+Nab-1), &
                              p0, p1, Na)
                  end if
            end do
            !$omp end parallel do
      end subroutine rpa_Cholesky_WriteJpq
      

      subroutine rpa_Cholesky_FillUpperTriangle(Apq, AOBasis)
            real(F64), dimension(:, :), intent(inout) :: Apq
            type(TAOBasis), intent(in)                :: AOBasis

            integer :: ShA, Na, LocA
            integer :: ShB, Nb, LocB
            integer :: ShAB, p0, p1, q0, q1
            
            associate ( &
                  NShells => AOBasis%NShells, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  NAngFunc => AOBasis%NAngFuncSpher, &
                  ShellLoc => AOBasis%ShellLocSpher)
                  !$omp parallel do private(ShAB, p0, p1, q0, q1) &
                  !$omp private(ShA, Na, LocA) &
                  !$omp private(ShB, Nb, LocB)
                  do ShAB = 1, (NShells * (NShells + 1)) / 2
                        call rpa_Cholesky_pq2p_ge_q(ShA, ShB, ShAB, NShells)
                        Na = NAngFunc(ShellParamsIdx(ShA))
                        LocA = ShellLoc(ShA)
                        Nb = NAngFunc(ShellParamsIdx(ShB))
                        LocB = ShellLoc(ShB)
                        p0 = LocA
                        p1 = LocA + Na - 1
                        q0 = LocB
                        q1 = LocB + Nb - 1
                        Apq(q0:q1, p0:p1) = transpose(Apq(p0:p1, q0:q1))
                  end do
                  !$omp end parallel do
            end associate
      end subroutine rpa_Cholesky_FillUpperTriangle
      
      
      subroutine rpa_Cholesky_JK_Block(Jpq, Kpq, Rkip, Rkpq, Cip, Chol2Vecs, &
            AOBasis, BlockDim, NAO, NOcc, t_Transform, t_Matmul_Coul, t_Matmul_Exch)
            
            integer, intent(in)                                            :: BlockDim
            integer, intent(in)                                            :: NAO
            integer, intent(in)                                            :: NOcc
            real(F64), dimension(NAO, NAO), intent(inout)                  :: Jpq
            real(F64), dimension(NAO, NAO), intent(inout)                  :: Kpq
            real(F64), dimension(BlockDim, NOcc, NAO), intent(out)         :: Rkip
            real(F64), dimension(:, :, :, :), intent(in)                   :: Rkpq
            real(F64), dimension(NOcc, NAO), intent(in)                    :: Cip
            type(TChol2Vecs), intent(in)                                   :: Chol2Vecs
            type(TAOBasis), intent(in)                                     :: AOBasis
            real(F64), intent(inout)                                       :: t_Transform
            real(F64), intent(inout)                                       :: t_Matmul_Coul
            real(F64), intent(inout)                                       :: t_Matmul_Exch
            

            integer :: ShAB, LocAB
            integer :: ShA, ShellParamsA, Na, LocA
            integer :: ShB, ShellParamsB, Nb, LocB
            integer :: a, b, p, q, pq
            integer :: i, Npq
            integer :: X, Y, SubsetIdx
            integer :: MaxSubsetDim
            type(TClock) :: timer_Transform, timer_Matmul_Coul, timer_Matmul_Exch
            real(F64), dimension(:), allocatable :: Rkii, JpqSubset

            associate ( &
                  NSubsets        => Chol2Vecs%NSubsets, &
                  SubsetDim       => Chol2Vecs%SubsetDim, &
                  SubsetBounds    => Chol2Vecs%SubsetBounds, &
                  ShellPairs      => Chol2Vecs%ShellPairs, &
                  ShellPairLoc    => Chol2Vecs%ShellPairLoc, &
                  ShellLoc        => AOBasis%ShellLocSpher, &
                  ShellParamsIdx  => AOBasis%ShellParamsIdx, &
                  NAngFunc        => AOBasis%NAngFuncSpher, &
                  LMaxGTO         => AOBasis%LMaxGTO &
                  )
                  call clock_start(timer_Transform)
                  MaxSubsetDim = maxval(SubsetDim)
                  allocate(Rkii(BlockDim))
                  Rkip = ZERO
                  yloop: do Y = 1, NSubsets(2)
                        xloop: do X = 1, NSubsets(1)
                              SubsetIdx = X + (Y - 1) * NSubsets(1)
                              shabloop: do ShAB = SubsetBounds(1, SubsetIdx), SubsetBounds(2, SubsetIdx)
                                    LocAB = ShellPairLoc(CHOL2_SUBSET_STORAGE, ShAB)

                                    ShA = ShellPairs(1, ShAB)
                                    ShellParamsA = ShellParamsIdx(ShA)
                                    Na = NAngFunc(ShellParamsA)
                                    LocA = ShellLoc(ShA)

                                    ShB = ShellPairs(2, ShAB)
                                    ShellParamsB = ShellParamsIdx(ShB)
                                    Nb = NAngFunc(ShellParamsB)
                                    LocB = ShellLoc(ShB)
                                    !
                                    ! R(1:BlockDim, :, p) <- R(1:BlockDim, :, p) + R(1:BlockDim, pq) * C(q, 1:NOcc)**T
                                    ! R(1:BlockDim, :, q) <- R(1:BlockDim, :, q) + R(1:BlockDim, pq) * C(p, 1:NOcc)**T
                                    !
                                    if (ShA /= ShB) then
                                          do b = 1, Nb
                                                q = LocB + b - 1
                                                !$omp parallel do collapse(2) &
                                                !$omp private(a, i, p, pq) &
                                                !$omp default(shared)
                                                do a = 1, Na
                                                      do i = 1, NOcc
                                                            p = LocA + a - 1
                                                            pq = LocAB + a-1 + Na * (b - 1)
                                                            Rkip(:, i, p) = Rkip(:, i, p) + Rkpq(:, pq, X, Y) * Cip(i, q)
                                                      end do
                                                end do
                                                !$omp end parallel do
                                          end do

                                          do a = 1, Na
                                                p = LocA + a - 1
                                                !$omp parallel do collapse(2) &
                                                !$omp private(b, i, q, pq) &
                                                !$omp default(shared)
                                                do b = 1, Nb
                                                      do i = 1, NOcc
                                                            q = LocB + b - 1
                                                            pq = LocAB + a-1 + Na * (b - 1)
                                                            Rkip(:, i, q) = Rkip(:, i, q) + Rkpq(:, pq, X, Y) * Cip(i, p)
                                                      end do
                                                end do
                                                !$omp end parallel do
                                          end do
                                    else
                                          do b = 1, Na
                                                q = LocB + b - 1
                                                !$omp parallel do collapse(2) &
                                                !$omp private(a, i, p, pq) &
                                                !$omp default(shared)
                                                do a = b, Na
                                                      do i = 1, NOcc
                                                            p = LocA + a - 1
                                                            pq = LocAB + rpa_Cholesky_p_ge_q2pq(a, b, Na) - 1
                                                            Rkip(:, i, p) = Rkip(:, i, p) + Rkpq(:, pq, X, Y) * Cip(i, q)
                                                      end do
                                                end do
                                                !$omp end parallel do
                                          end do
                                          !
                                          ! Note that this loop skips the diagonal element which has been
                                          ! already included in the loop above
                                          !
                                          do a = 1, Na
                                                p = LocA + a - 1
                                                !$omp parallel do collapse(2) &
                                                !$omp private(b, i, q, pq) &
                                                !$omp default(shared)
                                                do b = 1, a - 1
                                                      do i = 1, NOcc
                                                            q = LocB + b - 1
                                                            pq = LocAB + rpa_Cholesky_p_ge_q2pq(a, b, Na) - 1
                                                            Rkip(:, i, q) = Rkip(:, i, q) + Rkpq(:, pq, X, Y) * Cip(i, p)
                                                      end do
                                                end do
                                                !$omp end parallel do
                                          end do
                                    end if
                              end do shabloop
                        end do xloop
                  end do yloop
                  t_Transform = t_Transform + clock_readwall(timer_Transform)
                  !
                  ! Coulomb matrix
                  !
                  ! J(p,q) = J(p,q) + Sum(k) R(k,pq) * (Sum(i) R(k,ii))
                  !
                  call clock_start(timer_Matmul_Coul)
                  Rkii = ZERO
                  call real_Av_x(Rkii, Rkip, BlockDim, Cip, BlockDim, NOcc*NAO, ONE, ZERO)
                  allocate(JpqSubset(MaxSubsetDim))
                  do Y = 1, NSubsets(2)
                        do X = 1, NSubsets(1)
                              SubsetIdx = X + (Y - 1) * NSubsets(1)
                              Npq = SubsetDim(SubsetIdx)
                              call real_ATv_x(JpqSubset, Rkpq(:, :, X, Y), BlockDim, Rkii, &
                                    BlockDim, Npq, ONE, ZERO)
                              call rpa_Cholesky_WriteJpq(Jpq, JpqSubset, SubsetBounds(:, SubsetIdx), &
                                    ShellPairs, ShellPairLoc, ShellLoc, ShellParamsIdx, &
                                    NAngFunc, LMaxGTO)
                        end do
                  end do
                  t_Matmul_Coul = t_Matmul_Coul + clock_readwall(timer_Matmul_Coul)
                  !                  
                  ! Exchange matrix
                  !
                  ! K(p,q) = K(p,q) + Sum(ki) R(ki,p)*R(ki,q)
                  !
                  call clock_start(timer_Matmul_Exch)
                  call real_aTb_x(Kpq, NAO, Rkip, BlockDim*NOcc, Rkip, BlockDim*NOcc, &
                        NAO, NAO, BlockDim*NOcc, ONE, ONE)
                  t_Matmul_Exch = t_Matmul_Exch + clock_readwall(timer_Matmul_Exch)
            end associate
      end subroutine rpa_Cholesky_JK_Block


      subroutine rpa_Cholesky_JK(Jpq, Kpq, Cpi, Chol2Vecs, Chol2Params, AOBasis, RPAParams, &
            t_Transform, t_Matmul_Coul, t_Matmul_Exch)
            
            real(F64), dimension(:, :), intent(out)   :: Jpq
            real(F64), dimension(:, :), intent(out)   :: Kpq
            real(F64), dimension(:, :), intent(in)    :: Cpi
            type(TChol2Vecs), intent(in)              :: Chol2Vecs
            type(TChol2Params), intent(in)            :: Chol2Params
            type(TAOBasis), intent(in)                :: AOBasis
            type(TRPAParams), intent(in)              :: RPAParams
            real(F64), intent(out)                    :: t_Transform, t_Matmul_Coul, t_Matmul_Exch

            integer :: b, k0, k1
            integer :: NBlocks
            integer :: NOcc, NAO
            integer :: MaxSubsetDim, NCholesky
            integer :: MaxBlockDim, BlockDim
            real(F64), dimension(:, :), allocatable :: Inv_L, Wabrs, Cip
            real(F64), dimension(:, :, :, :), allocatable :: Rkpq
            real(F64), dimension(:, :, :), allocatable :: Rkip

            NOcc = size(Cpi, dim=2)
            NAO = size(Cpi, dim=1)
            NCholesky = Chol2Vecs%NVecs
            MaxBlockDim = min(RPAParams%CholVecsBlock, NCholesky)
            NBlocks = NCholesky / MaxBlockDim
            if (modulo(NCholesky, MaxBlockDim) > 0) NBlocks = NBlocks + 1
            MaxSubsetDim = maxval(Chol2Vecs%SubsetDim)
            allocate(Rkpq(MaxBlockDim, MaxSubsetDim, Chol2Vecs%NSubsets(1), Chol2Vecs%NSubsets(2)))
            allocate(Inv_L(MaxBlockDim, NCholesky))
            allocate(Cip(NOcc, NAO))
            Cip(:, :) = transpose(Cpi)
            allocate(Rkip(MaxBlockDim, NOcc, NAO))
            call chol2_AllocWorkspace(Wabrs, Chol2Vecs)
            call msg("Quadratic-memory Fock matrix algorithm")
            call msg("Cholesky vectors will be divided into " // str(NBlocks) // " batches")
            call msg("Max batch size is " // str(MaxBlockDim))
            t_Transform = ZERO
            t_Matmul_Coul = ZERO
            t_Matmul_Exch = ZERO
            Jpq = ZERO
            Kpq = ZERO
            do b = 1, NBlocks
                  k0 = 1 + (b - 1) * MaxBlockDim
                  k1 = min(NCholesky, b * MaxBlockDim)
                  BlockDim = k1 - k0 + 1
                  if (BlockDim < MaxBlockDim) then
                        deallocate(Rkpq, Inv_L, Rkip)
                        allocate(Rkpq(BlockDim, MaxSubsetDim, Chol2Vecs%NSubsets(1), Chol2Vecs%NSubsets(2)))
                        allocate(Inv_L(BlockDim, NCholesky))
                        allocate(Rkip(BlockDim, NOcc, NAO))
                  end if
                  call chol2_SelectedFullDimVectors(Rkpq, Inv_L, Wabrs, k0, k1, &
                        Chol2Vecs, AOBasis, Chol2Params)
                  call rpa_Cholesky_JK_Block(Jpq, Kpq, Rkip, Rkpq, Cip, Chol2Vecs, &
                        AOBasis, BlockDim, NAO, NOcc, t_Transform, t_Matmul_Coul, t_Matmul_Exch)
            end do
            call rpa_Cholesky_FillUpperTriangle(Jpq, AOBasis)
      end subroutine rpa_Cholesky_JK


      subroutine rpa_Cholesky_F(Fpq, EtotHF, Cpi, NOcc, Chol2Vecs, &
            Chol2Params, AOBasis, System, RPAParams)
            
            real(F64), dimension(:, :, :), intent(out) :: Fpq
            real(F64), intent(out)                     :: EtotHF
            real(F64), dimension(:, :, :), intent(in)  :: Cpi
            integer, dimension(:), intent(in)          :: NOcc
            type(TChol2Vecs), intent(in)               :: Chol2Vecs
            type(TChol2Params), intent(in)             :: Chol2Params
            type(TAOBasis), intent(in)                 :: AOBasis
            type(TSystem), intent(in)                  :: System
            type(TRPAParams), intent(in)               :: RPAParams

            integer :: NAO, NSpins
            integer :: s
            real(F64), dimension(:, :), allocatable :: W1pq, W2pq, Dpq
            real(F64) :: EHFTwoEl, Enucl, EHbare
            real(F64) :: TrDJK, TrDT, TrDV
            type(TClock) :: timer_Total
            real(F64) :: t_Transform, t_Matmul_Coul, t_Matmul_Exch

            call clock_start(timer_Total)
            NSpins = size(Cpi, dim=3)
            NAO = size(Cpi, dim=1)
            allocate(W1pq(NAO, NAO))
            allocate(W2pq(NAO, NAO))
            call sys_NuclearRepulsion(Enucl, System)
            allocate(Dpq(NAO, NAO))
            Fpq(:, :, :) = ZERO
            do s = 1, NSpins
                  if (NOcc(s) > 0) then
                        associate (Jpq => W1pq, Kpq => W2pq, Cpj => Cpi(:, 1:NOcc(s), s))
                              call rpa_Cholesky_JK(Jpq, Kpq, Cpj, Chol2Vecs, Chol2Params, &
                                    AOBasis, RPAParams, t_Transform, t_Matmul_Coul, t_Matmul_Exch)
                              if (NSpins == 1) then
                                    !
                                    ! Closed shells
                                    !
                                    Fpq(:, :, s) = Fpq(:, :, s) + TWO * Jpq(:, :)
                                    Fpq(:, :, s) = Fpq(:, :, s) - Kpq(:, :)
                              else
                                    !
                                    ! Open shells
                                    !
                                    if (NOcc(1) > 0) Fpq(:, :, 1) = Fpq(:, :, 1) + Jpq(:, :)
                                    if (NOcc(2) > 0) Fpq(:, :, 2) = Fpq(:, :, 2) + Jpq(:, :)
                                    Fpq(:, :, s) = Fpq(:, :, s) - Kpq(:, :)
                              end if
                        end associate
                  end if
            end do
            EHFTwoEl = ZERO
            EHbare = ZERO
            associate (Tpq => W1pq, Vpq => W2pq)
                  call ints1e_T(Tpq, AOBasis)
                  call ints1e_Vne(Vpq, AOBasis, System)                  
                  do s = 1, NSpins
                        if (NOcc(s) > 0) then
                              associate (Cpj => Cpi(:, 1:NOcc(s), s))
                                    call real_abT(Dpq, Cpj, Cpj)
                                    call real_vw_x(TrDJK, Dpq, Fpq(:, :, s), NAO**2)
                                    call real_vw_x(TrDV, Dpq, Vpq, NAO**2)
                                    call real_vw_x(TrDT, Dpq, Tpq, NAO**2)
                                    if (NSpins == 1) then
                                          EHFTwoEl = EHFTwoEl + TWO * (ONE/TWO) * TrDJK
                                          EHbare = EHbare + TWO * (TrDV + TrDT)
                                    else
                                          EHFTwoEl = EHFTwoEl + (ONE/TWO) * TrDJK
                                          EHbare = EHbare + TrDV + TrDT
                                    end if
                              end associate
                              Fpq(:, :, s) = Fpq(:, :, s) + Tpq(:, :)
                              Fpq(:, :, s) = Fpq(:, :, s) + Vpq(:, :)
                        end if
                  end do
            end associate
            EtotHF = Enucl + EHbare + EHFTwoEl
            call msg("* Rkpq->Rkip transform " // str(t_Transform,d=1))
            call msg("* Rkpq Rkii            " // str(t_Matmul_Coul,d=1))
            call msg("* Rkip Rkiq            " // str(t_Matmul_Exch,d=1))
            call msg("* total                " // str(clock_readwall(timer_Total),d=1))
      end subroutine rpa_Cholesky_F
end module rpa_Cholesky
