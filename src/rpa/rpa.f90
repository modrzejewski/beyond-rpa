module rpa
      use arithmetic
      use math_constants
      use quadratures
      use OptFreqQuad
      use OptLaplaceQuad
      use real_linalg
      use Auto2e
      use GaussPRNG
      use ParallelCholesky
      use TwoStepCholesky_definitions
      use SpherGTO
      use basis_sets
      use OrbDiffHist
      use rpa_core
      use rpa_core_SpinUnres
      use rpa_definitions
      use rpa_core_MO
      use rpa_CC
      use PostSCF
      use BeckeGrid
      use TensorHypercontraction
      use thc_definitions
      use rpa_THC
      use rpa_MeanField
      
      implicit none

contains

      subroutine rpa_G(G, RWR, NVecs, TargetError, NCholesky, RWRBasis, NAO)
            !
            ! Generate the random vectors basis. The basis determined in the first
            ! iteration is then used for all higher frequencies. The subspace iterations
            ! used to refine the random basis are performed according to Algorithm 1 in Ref. 1.
            !
            ! For development and testing, the option to generate the dielectric matrix
            ! eigenvector basis is available.
            !
            ! 1. Saibaba, A.K., Alexanerian, A., and Ipsen, I.C.F. Numer. Math. 137, 353 (2017);
            !    doi: 10.1007/s00211-017-0880-z
            !
            real(F64), dimension(NCholesky, NCholesky), intent(out)   :: G
            real(F64), dimension(NCholesky, NCholesky), intent(inout) :: RWR
            integer, intent(out)                                      :: NVecs
            real(F64), intent(in)                                     :: TargetError
            integer, intent(in)                                       :: NCholesky
            integer, intent(in)                                       :: RWRBasis
            integer, intent(in)                                       :: NAO

            real(F128) :: ExactTrace, AbsError, NextAbsError
            real(F64) :: EffFrac
            integer :: p, q, n, k
            real(F64), dimension(:), allocatable :: QRWork
            integer :: LQRWork
            character(:), allocatable :: line
            real(F64), dimension(:), allocatable :: Lambda
            real(F64), dimension(:, :), allocatable :: RWRG
            integer, parameter :: NSubspaceIters = 2
            integer, parameter :: NDisplayPoints = 20
            integer :: ThisImage, NImages

            ThisImage = this_image()
            NImages = num_images()
            if (ThisImage == 1) then
                  if (RWRBasis /= RPA_BASIS_FULL_CHOLESKY) then
                        call msg("Computing G for the effective dielectric matrix GRWRG")
                        call msg("Target error: Tr(Pi(exact)-Pi(eff)) < " // str(TargetError,d=1))
                  else
                        call msg("Using full space of Cholesky vectors to represent the Pi(u) matrix")
                  end if
                  ExactTrace = 0.0_F128
                  do p = 1, NCholesky
                        ExactTrace = ExactTrace + real(RWR(p, p), F128)
                  end do
                  if (RWRBasis == RPA_BASIS_EIGEN) then
                        allocate(Lambda(NCholesky))
                        call msg("Columns of G will be eigenvectors of RWR")
                        !
                        ! Diagonalize the positive-definite matrix RWR after scaling it by -1
                        ! to have the most important eigenvector at column 1. (The diagonalization
                        ! subroutine sorts the eigenvalues in ascending order.) After diagonalization
                        ! the eigenvalues are scaled back by -1.
                        !
                        G = -RWR
                        RWR = ZERO
                        call symmetric_eigenproblem(Lambda, G, NCholesky, .true.)
                        Lambda = -Lambda
                        do p = 1, NCholesky
                              RWR(p, p) = Lambda(p)
                        end do
                        line = lfield("NVecsPiU/NCholesky", 20) // lfield("Tr(Pi(exact)-Pi(eff))", 24)
                  else if (RWRBasis == RPA_BASIS_RANDOM) then                  
                        allocate(RWRG(NCholesky, NCholesky))
                        call real_QR_query(LQRWork, NCholesky, NCholesky, NCholesky)
                        allocate(QRWork(LQRWork))
                        call msg("Columns of G will be vectors of random Gaussian numbers")
                        call GRAND(G, NCholesky*NCholesky)
                        do q = 1, NSubspaceIters
                              call real_ab_x(RWRG, NCholesky, RWR, NCholesky, G, NCholesky, NCholesky, NCholesky, NCholesky)
                              call real_QR(RWRG, NCholesky, NCholesky, NCholesky, QRWork, LQRWork)
                              G = RWRG
                        end do
                        call real_ab_x(RWRG, NCholesky, RWR, NCholesky, G, NCholesky, NCholesky, NCholesky, NCholesky)
                        call real_aTb_x(RWR, NCholesky, G, NCholesky, RWRG, NCholesky, NCholesky, NCholesky, NCholesky)
                        line = lfield("NVecsPiU/NCholesky", 20) // lfield("Tr(Pi(exact)-Pi(eff))", 24)
                  else if (RWRBasis == RPA_BASIS_FULL_CHOLESKY) then
                        !
                        ! Use full space of Cholesky vectors. (For other options, the vector space is restricted
                        ! to linear combinations of Cholesky vectors that correspond to the dominant eigenvectors
                        ! of Pi(u). Use this option for debugging.
                        !
                        G = ZERO
                        do p = 1, NCholesky
                              G(p, p) = ONE
                        end do
                  else
                        call msg("Invalid basis type requested for the matrix Pi(u)=R**T*Chi(u)*R", MSG_ERROR)
                        error stop
                  end if
                  if (RWRBasis /= RPA_BASIS_FULL_CHOLESKY) then
                        !
                        ! Display the trace error as a function of the dimension
                        ! of the random vector/eigenvectors basis
                        !
                        call midrule(width=44)
                        call msg(line)
                        call midrule(width=44)
                        do k = 1, NDisplayPoints
                              n = min(NCholesky, nint(real(NCholesky,F64)/real(NDisplayPoints,F64)*k))
                              AbsError = 0.0_F128
                              do p = NCholesky, n+1, -1
                                    AbsError = AbsError + real(RWR(p, p), F128)
                              end do
                              EffFrac = real(n, F64) / real(NCholesky, F64)
                              line = cfield(str(EffFrac, d=1), 20) // cfield(str(real(AbsError,F64), d=4), 24)
                              call msg(line)
                              if (AbsError <= TargetError/100) then
                                    exit
                              end if
                        end do
                        !
                        ! Compute the number of random vectors/eigenvectors corresponding
                        ! to the requested accuracy
                        !
                        NVecs = NCholesky
                        AbsError = 0.0_F128
                        do p = NCholesky, 1, -1
                              NextAbsError = AbsError + real(RWR(p, p), F128)
                              if (NextAbsError < TargetError) then
                                    AbsError = NextAbsError
                                    NVecs = p
                              else
                                    exit
                              end if
                        end do
                  else
                        NVecs = NCholesky
                        AbsError = ZERO
                  end if
                  call blankline()
                  call msg("Dimension of the effective dielectric matrix: " // str(NVecs))
                  call msg("NVecsPiU/NCholesky = " // str(real(NVecs,F64)/real(NCholesky,F64), d=1))
                  call msg("NVecsPiU/NAO = " // str(real(NVecs,F64)/real(NAO,F64), d=1))
                  call msg("Tr(Pi(exact)) = " // str(real(ExactTrace,F64),d=1))
                  call msg("Tr(Pi(exact)-Pi(eff)) = " // str(real(AbsError,F64),d=1))
                  call blankline()
            end if
            if (NImages > 1) then
                  call co_broadcast(NVecs, source_image=1)
                  call co_broadcast(G, source_image=1)
            end if
      end subroutine rpa_G


      subroutine rpa_ReduceRWR(RWR)
            real(F64), dimension(:, :), intent(inout) :: RWR[*]

            integer :: k, l, p0, p1
            integer :: NCols, NChunks, m, n
            integer :: NImages, ThisImage

            m = size(RWR, dim=1)
            n = size(RWR, dim=2)
            NCols = max(1, RPA_MAX_TRANSFER / m)
            if (modulo(n, NCols) > 0) then
                  NChunks = n / NCols + 1
            else
                  NChunks = n / NCols
            end if
            ThisImage = this_image()
            NImages = num_images()
            if (ThisImage == 1) then
                  do l = 2, NImages
                        do k = 1, NChunks
                              p0 = 1 + (k-1) * NCols
                              p1 = min(k * NCols, n)
                              RWR(:, p0:p1) = RWR(:, p0:p1) + RWR(:, p0:p1)[l]
                        end do
                  end do
            end if
      end subroutine rpa_ReduceRWR


      subroutine rpa_ReduceGPiUG(GPiUG)
            real(F64), dimension(:, :), intent(inout) :: GPiUG[*]

            integer :: k, l, u, p0, p1
            integer :: NChunks, m
            integer :: NImages, ThisImage
            integer :: NFreqs

            m = size(GPiUG, dim=1)
            if (modulo(m, RPA_MAX_TRANSFER) > 0) then
                  NChunks = m / RPA_MAX_TRANSFER + 1
            else
                  NChunks = m / RPA_MAX_TRANSFER
            end if
            ThisImage = this_image()
            NImages = num_images()
            NFreqs = size(GPiUG, dim=2)
            if (ThisImage == 1) then
                  do l = 2, NImages
                        do u = 1, NFreqs
                              do k = 1, NChunks
                                    p0 = 1 + (k-1) * RPA_MAX_TRANSFER
                                    p1 = min(k * RPA_MAX_TRANSFER, m)
                                    GPiUG(p0:p1, u) = GPiUG(p0:p1, u) + GPiUG(p0:p1, u)[l]
                              end do
                        end do
                  end do
            end if
      end subroutine rpa_ReduceGPiUG


      subroutine rpa_SendVecs_F64(A, B, Img)
            real(F64), dimension(:, :, :), intent(inout) :: A[*]
            real(F64), dimension(:, :, :), intent(in)    :: B
            integer, intent(in)                          :: Img

            integer :: k, z, nz, p0, p1
            integer :: NCols, NChunks, m, n

            m = size(A, dim=1)
            n = size(A, dim=2)
            nz = size(A, dim=3)
            NCols = max(1, RPA_MAX_TRANSFER / m)
            if (modulo(n, NCols) > 0) then
                  NChunks = n / NCols + 1
            else
                  NChunks = n / NCols
            end if
            do z = 1, nz
                  do k = 1, NChunks
                        p0 = 1 + (k-1) * NCols
                        p1 = min(k * NCols, n)
                        A(:, p0:p1, z)[Img] = B(:, p0:p1, z)
                  end do
            end do
      end subroutine rpa_SendVecs_F64

      
      subroutine rpa_Fov(F, G, C, NMO, NAO, NLaplace)
            real(F64), dimension(NLaplace, NAO, *), intent(out) :: F
            real(F64), dimension(NLaplace, *), intent(in)       :: G
            real(F64), dimension(NAO, NMO), intent(in)          :: C
            integer, intent(in)                                 :: NMO
            integer, intent(in)                                 :: NAO
            integer, intent(in)                                 :: NLaplace

            integer :: m, p
            
            do m = 1, NMO
                  do p = 1, NAO
                        F(:, p, m) = G(:, m) * C(p, m)
                  end do
            end do
      end subroutine rpa_Fov
      

      subroutine rpa_Rho(RhoOcc, RhoVirt, Gov, Fov, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, &
            LaplaceX, NAO, NLaplace, NOcc, NVirt)
            !
            ! Compute occupied- and virtual-orbital pseudo-density matrices.
            !
            ! Trivial implementation of pseudo-density matrix computation:
            ! do q = 1, NAO
            !       do p = 1, NAO
            !             do i = 1, NOcc
            !                   RhoOcc(:, p, q) = RhoOcc(:, p, q) + Gocc(:, i) * OccCoeffs(p, i) * OccCoeffs(q, i)
            !             end do
            !       end do
            ! end do
            !
            ! Optimized implementaiton using a linear algebra library call and temporary matrix F:
            ! F(:, p, i) = GOcc(:, i) * OccCoeffs(p, i)           
            ! RhoOcc(:, p, q) = Sum(i) RhoOcc(:, p, q) <- F(:, p, i) * (OccCoeffs(q, i))**T
            !
            real(F64), dimension(NLaplace, NAO*NAO), intent(out) :: RhoOcc
            real(F64), dimension(NLaplace, NAO*NAO), intent(out) :: RhoVirt
            real(F64), dimension(NLaplace, *), intent(out)       :: Gov
            real(F64), dimension(NLaplace*NAO, *), intent(out)   :: Fov
            real(F64), dimension(NAO, NOcc), intent(in)          :: OccCoeffs
            real(F64), dimension(NAO, NVirt), intent(in)         :: VirtCoeffs
            real(F64), dimension(NOcc), intent(in)               :: OccEnergies
            real(F64), dimension(NVirt), intent(in)              :: VirtEnergies
            real(F64), dimension(NLaplace), intent(in)           :: LaplaceX
            integer, intent(in)                                  :: NAO
            integer, intent(in)                                  :: NLaplace
            integer, intent(in)                                  :: NOcc
            integer, intent(in)                                  :: NVirt

            integer :: ld, i, a, k
            ! --------------------------------------
            ! Occupied orbital pseudo density matrix
            ! --------------------------------------
            do i = 1, NOcc
                  do k = 1, NLaplace
                        Gov(k, i) = exp(OccEnergies(i) * LaplaceX(k))
                  end do
            end do
            call rpa_Fov(Fov, Gov, OccCoeffs, NOcc, NAO, NLaplace)
            ld = NLaplace * NAO
            call real_abT_x(RhoOcc, ld, Fov, ld, OccCoeffs, NAO, ld, NAO, NOcc)
            ! -------------------------------------
            ! Virtual orbital pseudo density matrix
            ! -------------------------------------
            do a = 1, NVirt
                  do k = 1, NLaplace
                        Gov(k, a) = exp(-VirtEnergies(a) * LaplaceX(k))
                  end do
            end do
            call rpa_Fov(Fov, Gov, VirtCoeffs, NVirt, NAO, NLaplace)
            ld = NLaplace * NAO
            call real_abT_x(RhoVirt, ld, Fov, ld, VirtCoeffs, NAO, ld, NAO, NVirt)
      end subroutine rpa_Rho


      subroutine rpa_ScaleAndSymmetrize(GRWRG, GRWRGT, NSpins, NVecs)
            !
            ! Scale and symmetrize the dielectric matrix GRWRG (or RWR):
            !
            ! (i) Take into account the factor of 2 which results from
            ! the summation of a complex number and its conjugate
            !
            ! 1/(Ea-Ei+iu)+1/(Ea-Ei-iu) = 2(Ea-Ei)/((Ea-Ei)**2 + u**2)
            !
            ! (ii) Multiply Pi(u) by the factor of 2 to account for the spin summation
            ! over doubly occupied closed-shell orbitals. The summation over spins
            ! is given explicitly in Eq. 58 of Ref. 1.
            !
            ! (iii) Compute Pi(u) <- GRWRG + GRWRG**T to account for the symmetry of
            ! the matrix W(pq,rs) = W(rs,pq). During the computation of WR, only
            ! half of the matrix elements W(pq,rs) are accounted for and the diagonal
            ! blocks are scaled by 1/2. 
            !
            real(F64), dimension(NVecs, NVecs), intent(inout) :: GRWRG
            real(F64), dimension(NVecs, NVecs), intent(out)   :: GRWRGT
            integer, intent(in)                               :: NSpins
            integer, intent(in)                               :: NVecs
            
            GRWRGT = transpose(GRWRG)
            GRWRG = GRWRG + GRWRGT
            if (NSpins == 1) then
                  !
                  ! Closed shells
                  !
                  GRWRG = TWO * TWO * GRWRG
            else
                  !
                  ! Open-shell case. The matrix W has been already
                  ! summed over spin indices. 
                  !
                  GRWRG = TWO * GRWRG
            end if
      end subroutine rpa_ScaleAndSymmetrize
      

      subroutine rpa_Pack(GRWRG, GRWRGFull, Scratch, NVecs)
            real(F64), dimension((NVecs*(NVecs+1))/2), intent(inout) :: GRWRG
            real(F64), dimension(NVecs, NVecs), intent(in)           :: GRWRGFull
            real(F64), dimension((NVecs*(NVecs+1))/2), intent(out)   :: Scratch
            integer, intent(in)                                      :: NVecs

            call real_Pack(Scratch, GRWRGFull, NVecs)
            GRWRG = GRWRG + Scratch
      end subroutine rpa_Pack

      
      subroutine rpa_Unpack(GRWRGFull, GRWRG, GRWRGT, NVecs)
            real(F64), dimension(NVecs, NVecs), intent(out)       :: GRWRGFull
            real(F64), dimension((NVecs*(NVecs+1))/2), intent(in) :: GRWRG
            real(F64), dimension(NVecs, NVecs), intent(out)       :: GRWRGT
            integer, intent(in)                                   :: NVecs

            integer :: k
            
            call real_Unpack(GRWRGFull, GRWRG, NVecs)
            GRWRGT = transpose(GRWRGFull)
            do k = 1, NVecs
                  GRWRGT(k, k) = ZERO
            end do
            GRWRGFull = GRWRGFull + GRWRGT
      end subroutine rpa_Unpack
      
      
      subroutine rpa_OptimizeQuads(NFreqs, Freqs, FreqWeights, NLaplace, LaplaceX, LaplaceW, &
            daiValues, daiWeights, TargetErrorFreq, TargetRelErrorFreq, TargetErrorLaplace, &
            TargetRelErrorLaplace, GridLimitDai)
            !
            ! Optimize new frequency and Laplace quadratures
            !
            integer, intent(out)                                 :: NFreqs
            real(F64), dimension(:), allocatable, intent(out)    :: Freqs
            real(F64), dimension(:), allocatable, intent(out)    :: FreqWeights
            integer, dimension(:), allocatable, intent(out)      :: NLaplace
            real(F64), dimension(:, :), allocatable, intent(out) :: LaplaceX
            real(F64), dimension(:, :), allocatable, intent(out) :: LaplaceW
            real(F64), dimension(:, :), intent(in)               :: daiValues
            real(F64), dimension(:, :), intent(in)               :: daiWeights
            real(F64), intent(in)                                :: TargetErrorFreq
            real(F64), intent(in)                                :: TargetRelErrorFreq
            real(F64), intent(in)                                :: TargetErrorLaplace
            real(F64), intent(in)                                :: TargetRelErrorLaplace
            logical, intent(in)                                  :: GridLimitDai

            real(F64), dimension(:), allocatable :: Freqs0[:], FreqWeights0[:]
            integer, dimension(:), allocatable :: NLaplace0[:]
            real(F64), dimension(:, :), allocatable :: LaplaceX0[:], LaplaceW0[:]
            integer, allocatable :: NFreqs0[:]
            integer :: MaxNLaplace
            integer :: ThisImage

            ThisImage = this_image()
            allocate(NFreqs0[*])
            allocate(Freqs0(RPA_MAX_NFREQS)[*])
            allocate(FreqWeights0(RPA_MAX_NFREQS)[*])
            allocate(NLaplace0(RPA_MAX_NFREQS)[*])
            allocate(LaplaceX0(RPA_MAX_NLAPLACE, RPA_MAX_NFREQS)[*])
            allocate(LaplaceW0(RPA_MAX_NLAPLACE, RPA_MAX_NFREQS)[*])
            if (ThisImage == 1) then
                  ! --------------------------------------------------------
                  !               Optimize frequency quadrature
                  ! --------------------------------------------------------
                  call rpa_OptimizeFreqQuad(Freqs0, FreqWeights0, NFreqs0, daiValues, daiWeights, &
                        TargetErrorFreq, TargetRelErrorFreq, RPA_MIN_NFREQS, RPA_MAX_NFREQS, GridLimitDai)
                  ! --------------------------------------------------------
                  !           Optimize Laplace transform quadrature
                  ! --------------------------------------------------------
                  ! Points and weights for the numerical quadrature employed
                  ! for the Laplace transform of dai/(dai**2+u**2).
                  ! The constant factor of two related to the complex conjugation
                  ! is accounted for later when scaling the whole dielectric matrix.
                  !
                  call rpa_LaplaceQuad(LaplaceX0, LaplaceW0, NLaplace0, Freqs0, NFreqs0, daiValues, daiWeights, &
                        RPA_MAX_NLAPLACE, TargetRelErrorLaplace, TargetErrorLaplace, GridLimitDai)
                  sync images(*)
            else
                  sync images(*)
                  NFreqs0 = NFreqs0[1]
                  Freqs0 = Freqs0(:)[1]
                  FreqWeights0 = FreqWeights0(:)[1]
                  NLaplace0 = NLaplace0(:)[1]
                  LaplaceX0 = LaplaceX0(:, :)[1]
                  LaplaceW0 = LaplaceW0(:, :)[1]
            end if
            NFreqs = NFreqs0
            allocate(Freqs(NFreqs))
            Freqs = Freqs0(1:NFreqs)
            FreqWeights = FreqWeights0(1:NFreqs)
            MaxNLaplace = maxval(NLaplace0(1:NFreqs))
            allocate(NLaplace(NFreqs))
            allocate(LaplaceX(MaxNLaplace, NFreqs))
            allocate(LaplaceW(MaxNLaplace, NFreqs))
            NLaplace = NLaplace0(1:NFreqs)
            LaplaceX = LaplaceX0(1:MaxNLaplace, 1:NFreqs)
            LaplaceW = LaplaceW0(1:MaxNLaplace, 1:NFreqs)
      end subroutine rpa_OptimizeQuads


      subroutine rpa_MatrixBlocksMap(Ky, Ly, Transpose, By, L, NSubsets)
            !
            ! Map the blocks of a symmetric matrix onto a series of iterations
            ! enumerated by L=1, 2, ..., NSubsets(2)/2+1. The map enables
            ! parallel evaluation of a symmetric matrix with work distributed
            ! into roughly equal chunks.
            !
            ! The parallel processes/images are identified by their corresponding
            ! bra vector index By.
            !
            ! Example (4 concurrent processes/images)
            ! ---------------------------------------------------------------------
            ! Iter/By      1               2            3             4
            ! ---------------------------------------------------------------------
            ! 1           (1,1)           (2,2)        (3,3)         (4,4)
            ! 2*          (1,4),(4,1)     (2,1),(1,2)  (3,2),(2,3)   (4,3),(3,4)
            ! 3           (1,3)           (2,4)        (3,1)         (4,2)
            ! * Transpose=.True.
            !
            ! Example (5 concurrent processes/images)
            ! --------------------------------------------------------------------------------
            ! Iter/By     1               2            3             4             5
            ! --------------------------------------------------------------------------------
            ! 1           (1,1)           (2,2)        (3,3)         (4,4)         (5,5)
            ! 2*          (1,5),(5,1)     (2,1),(1,2)  (3,2),(2,3)   (4,3),(3,4)   (5,4),(4,5)
            ! 3*          (1,5),(5,1)     (2,5),(5,2)  (3,1),(1,3)   (4,2),(2,4)   (5,3),(3,5)
            ! * Transpose=.True.
            !
            ! The notation (By,Ky) indicates that the process corresponding to the bra index By
            ! computes the matrix block (By,Ky). The index of the remote process which works on
            ! the vector By in the Lth iteration is stored in the output variable Ly.
            ! 
            ! The logical variable Transpose indicates that both the matrix block and its transpose
            ! should be evaluated. Computing two blocks per iteration lowers the amount of necessary
            ! communication between the processes/images.
            !
            integer, intent(out)              :: Ky
            integer, intent(out)              :: Ly
            logical, intent(out)              :: Transpose
            integer, intent(in)               :: By
            integer, intent(in)               :: L
            integer, dimension(2), intent(in) :: NSubsets
            
            Ky = 1 + modulo(By-L, NSubsets(2))
            Ly = 1 + modulo(By+L-2, NSubsets(2))
            if (modulo(NSubsets(2), 2) == 0) then
                  Transpose = (L > 1 .and. L <= NSubsets(2)/2)
            else
                  Transpose = (L > 1)
            end if
      end subroutine rpa_MatrixBlocksMap


      subroutine rpa_RWR(RWR, RBra, RhoOcc, RhoVirt, ShellPairs, ShellPairLoc, &
            ShellPairDim, ShellLoc, LaplaceW, ShellParamsIdx, NAngFunc, NAO, NSpins, NCholesky, &
            NLaplace, By, SubsetBounds, SubsetDim, NSubsets, MaxSubsetDim, MaxNCholesky, &
            time_W, time_WRG, time_GRWRG)
            
            real(F64), dimension(:, :), intent(out)                      :: RWR[*]
            real(F64), dimension(:, :, :), intent(in)                    :: RBra
            real(F64), dimension(:), intent(in)                          :: RhoOcc
            real(F64), dimension(:), intent(in)                          :: RhoVirt
            integer, dimension(:, :), intent(in)                         :: ShellPairs
            integer, dimension(:, :), intent(in)                         :: ShellPairLoc
            integer, dimension(:), intent(in)                            :: ShellPairDim
            integer, dimension(:), intent(in)                            :: ShellLoc
            real(F64), dimension(:), intent(in)                          :: LaplaceW
            integer, dimension(:), intent(in)                            :: ShellParamsIdx
            integer, dimension(:), intent(in)                            :: NAngFunc
            integer, intent(in)                                          :: NAO
            integer, intent(in)                                          :: NSpins
            integer, intent(in)                                          :: NCholesky
            integer, intent(in)                                          :: NLaplace
            integer, intent(in)                                          :: By
            integer, dimension(:, :), intent(in)                         :: SubsetBounds
            integer, dimension(:), intent(in)                            :: SubsetDim
            integer, dimension(2), intent(in)                            :: NSubsets
            integer, intent(in)                                          :: MaxSubsetDim
            integer, intent(in)                                          :: MaxNCholesky
            real(F64), intent(inout)                                     :: time_W
            real(F64), intent(inout)                                     :: time_WRG
            real(F64), intent(inout)                                     :: time_GRWRG

            real(F64), dimension(:, :, :), allocatable :: RKet[:]
            real(F64), dimension(:, :), allocatable :: WR
            real(F64), dimension(:), allocatable :: W
            integer :: L, Ky, Ly
            logical :: Transpose
            character(:), allocatable :: line
            real(F64) :: dt
            type(TClock) :: timer_Iter
            integer :: ThisImage, NImages

            ThisImage = this_image()
            NImages = num_images()
            RWR = ZERO
            if (NSubsets(2) > 1) then
                  allocate(RKet(MaxNCholesky, MaxSubsetDim, NSubsets(1))[*])
            end if
            allocate(WR(NCholesky, MaxSubsetDim))
            allocate(W(MaxSubsetDim**2))
            call msg("Memory allocation (in gigabytes, per image):")
            if (NSubsets(2) > 1) then
                  call msg("R (ket vectors)     " // str(io_size_byte(RKet)/(1024.0_F64**3),d=1))
            end if
            call msg("RWR                 " // str(io_size_byte(RWR)/(1024.0_F64**3),d=1))
            call msg("Max subset dimension: " // str(MaxSubsetDim))
            call msg("NSubsets per image: " // str(NSubsets(1)))            
            line = lfield("Time", 10) // lfield("MatrixBlocks", 12)
            call midrule(width=22)
            call msg(line)
            call midrule(width=22)
            do L = 1, NSubsets(2) / 2 + 1
                  call clock_start(timer_Iter)
                  call rpa_MatrixBlocksMap(Ky, Ly, Transpose, By, L, NSubsets)
                  if (Ky /= By) then
                        sync all
                        ! RKet(:, :, :)[Ly] = RBra
                        call rpa_SendVecs_F64(RKet, RBra, Ly)
                        sync all
                  end if
                  if (By /= Ky) then
                        if (NSpins == 1) then
                              call rpa_GPiUG(RWR, WR, W, RBra, RKet, &
                                    RhoOcc, RhoVirt, &
                                    ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, &
                                    LaplaceW, ShellParamsIdx, &
                                    NAngFunc, NAO, NCholesky, NLaplace, By, Ky, &
                                    Transpose, SubsetBounds, SubsetDim, NSubsets, time_W, time_WRG, time_GRWRG)
                        else
                              call rpa_GPiUG_SpinUnres(RWR, WR, W, RBra, RKet, &
                                    RhoOcc, RhoVirt, &
                                    ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, &
                                    LaplaceW, ShellParamsIdx, &
                                    NAngFunc, NAO, NCholesky, NLaplace, By, Ky, &
                                    Transpose, SubsetBounds, SubsetDim, NSubsets, time_W, time_WRG, time_GRWRG)
                        end if
                  else
                        if (NSpins == 1) then
                              call rpa_GPiUG(RWR, WR, W, RBra, RBra, &
                                    RhoOcc, RhoVirt, &
                                    ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, &
                                    LaplaceW, ShellParamsIdx, &
                                    NAngFunc, NAO, NCholesky, NLaplace, By, Ky, &
                                    Transpose, SubsetBounds, SubsetDim, NSubsets, time_W, time_WRG, time_GRWRG)
                        else
                              call rpa_GPiUG_SpinUnres(RWR, WR, W, RBra, RBra, &
                                    RhoOcc, RhoVirt, &
                                    ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, &
                                    LaplaceW, ShellParamsIdx, &
                                    NAngFunc, NAO, NCholesky, NLaplace, By, Ky, &
                                    Transpose, SubsetBounds, SubsetDim, NSubsets, time_W, time_WRG, time_GRWRG)
                        end if
                  end if
                  dt = clock_readwall(timer_iter)
                  if (Transpose) then
                        line = lfield(str(dt, d=1), 10) &
                              // "("//str(Ky)//","//str(By)//"), ("//str(By)//","//str(Ky)//")"
                  else
                        line = lfield(str(dt, d=1), 10) &
                              // "("//str(Ky)//","//str(By)//")"
                  end if
                  call msg(line)
            end do
            call blankline()
            sync all
            call rpa_ReduceRWR(RWR)
            sync all
            ! if (NImages > 1) then
            !       call co_sum(RWR, result_image=1)
            ! end if
      end subroutine rpa_RWR
      
      
      subroutine rpa_FreqIntegral_AO(Ecorr, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, ShellCenters, &
            AtomCoords, LmaxGTO, ShellLoc, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, &
            Exponents, NormFactors, NAO, NShells, NAtoms, NOcc, NVirt, NSpins, SpherAO, &
            
            Kappa, TargetErrorLaplace, TargetRelErrorLaplace, CholeskyTauThresh, &
            TargetErrorRandom, TargetErrorFreq, TargetRelErrorFreq, MaxNAOMult, &
            RWRBasisType, MaxBlockDim, &

            ComputeGrids, GridLimitDai, NFreqs, Freqs, FreqWeights, NLaplace, LaplaceX, LaplaceW, &
            daiValues, daiWeights, &
            
            ComputeRWRBasis, ComputeCholeskyBasis, RGBra, RBra, NVecsPiU, NCholesky, MaxSubsetDim, &
            NSubsets, NOrbPairs, NShellPairs, ShellPairs, &
            ShellPairLoc, ShellPairDim, SubsetDim, SubsetBounds, &

            time_W, time_WRG, time_GRWRG, time_LogDet, time_Density, time_Cholesky)

            real(F64), intent(out)                                       :: Ecorr
            real(F64), dimension(:, :, :), intent(in)                    :: OccCoeffs
            real(F64), dimension(:, :, :), intent(in)                    :: VirtCoeffs
            real(F64), dimension(:, :), intent(in)                       :: OccEnergies
            real(F64), dimension(:, :), intent(in)                       :: VirtEnergies
            integer, dimension(NShells), intent(in)                      :: ShellCenters
            real(F64), dimension(3, NAtoms), intent(in)                  :: AtomCoords
            integer, intent(in)                                          :: LmaxGTO
            integer, dimension(NShells), intent(in)                      :: ShellLoc
            integer, dimension(NShells), intent(in)                      :: ShellParamsIdx
            integer, dimension(:), intent(in)                            :: ShellMomentum
            integer, dimension(:), intent(in)                            :: NAngFunc
            integer, dimension(:), intent(in)                            :: NPrimitives
            real(F64), dimension(:, :), intent(in)                       :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)                       :: Exponents
            real(F64), dimension(:, :), intent(in)                       :: NormFactors
            integer, intent(in)                                          :: NAO
            integer, intent(in)                                          :: NShells
            integer, intent(in)                                          :: NAtoms
            integer, dimension(:), intent(in)                            :: NOcc
            integer, dimension(:), intent(in)                            :: NVirt
            integer, intent(in)                                          :: NSpins
            logical, intent(in)                                          :: SpherAO
            
            real(F64), intent(in)                                        :: Kappa
            real(F64), intent(in)                                        :: TargetErrorLaplace
            real(F64), intent(in)                                        :: TargetRelErrorLaplace
            real(F64), intent(in)                                        :: CholeskyTauThresh
            real(F64), intent(in)                                        :: TargetErrorRandom
            real(F64), intent(in)                                        :: TargetErrorFreq
            real(F64), intent(in)                                        :: TargetRelErrorFreq
            real(F64), intent(in)                                        :: MaxNAOMult
            integer, intent(in)                                          :: RWRBasisType
            integer, intent(in)                                          :: MaxBlockDim

            logical, intent(in)                                          :: ComputeGrids
            logical, intent(in)                                          :: GridLimitDai
            integer, intent(inout)                                       :: NFreqs
            real(F64), dimension(:), allocatable, intent(inout)          :: Freqs
            real(F64), dimension(:), allocatable, intent(inout)          :: FreqWeights
            integer, dimension(:), allocatable, intent(inout)            :: NLaplace
            real(F64), dimension(:, :), allocatable, intent(inout)       :: LaplaceX
            real(F64), dimension(:, :), allocatable, intent(inout)       :: LaplaceW
            real(F64), dimension(:, :), intent(in)                       :: daiValues
            real(F64), dimension(:, :), intent(in)                       :: daiWeights

            logical, intent(in)                                          :: ComputeRWRBasis
            logical, intent(in)                                          :: ComputeCholeskyBasis
            real(F64), dimension(:, :, :), allocatable, intent(inout)    :: RGBra[:]
            real(F64), dimension(:, :, :), allocatable, intent(inout)    :: RBra[:]
            integer, intent(inout)                                       :: NVecsPiU
            integer, intent(inout)                                       :: NCholesky
            integer, intent(inout)                                       :: MaxSubsetDim
            integer, dimension(2), intent(inout)                         :: NSubsets
            integer, intent(inout)                                       :: NOrbPairs
            integer, intent(inout)                                       :: NShellPairs
            integer, dimension(:, :), allocatable, intent(inout)         :: ShellPairs
            integer, dimension(:, :), allocatable, intent(inout)         :: ShellPairLoc
            integer, dimension(:), allocatable, intent(inout)            :: ShellPairDim
            integer, dimension(:), allocatable, intent(inout)            :: SubsetDim
            integer, dimension(:, :), allocatable, intent(inout)         :: SubsetBounds
            
            real(F64), intent(out)                                       :: time_W
            real(F64), intent(out)                                       :: time_WRG
            real(F64), intent(out)                                       :: time_GRWRG
            real(F64), intent(out)                                       :: time_LogDet
            real(F64), intent(out)                                       :: time_Density
            real(F64), intent(out)                                       :: time_Cholesky
            
            integer :: u, s, s0, s1, FirstIter, k
            real(F64) :: Omega
            real(F64) :: Eu
            real(F64) :: dt
            real(F64), dimension(:), allocatable :: time_Iter
            real(F64), dimension(:, :, :), allocatable :: RGKet[:]
            real(F64), dimension(:, :), allocatable :: WRG
            real(F64), dimension(:), allocatable :: W
            real(F64), dimension(:), allocatable :: Gov, Fov
            real(F64), dimension(:), allocatable :: RhoOcc, RhoVirt
            real(F64), dimension(:, :), allocatable :: G
            real(F64), dimension(:, :), allocatable :: GPiUG[:]
            real(F64), dimension(:, :), allocatable :: GPiUGFull, GPiUGT
            real(F64), dimension(:, :), allocatable :: RWR[:]
            integer :: MaxNShellPairs
            integer :: MaxNCholesky
            type(tclock) :: timer, timer_LogDet, timer_Freq, timer_Cholesky
            character(:), allocatable :: line
            real(F128) :: TrGPiUG, LogDet, Eu_F128
            real(F64), dimension(:), allocatable :: LogDetWork
            integer :: LogDetLWork
            integer :: DimGov, DimFov
            integer :: Bra, NBra, Bx, By, Ky, Ly, L
            logical :: Transpose
            integer :: NImages, ThisImage

            NImages = num_images()
            ThisImage = this_image()
            !
            ! Range-separation parameter for the range-separated
            ! Coulomb operator 1/r=erfc(omega*r)/r + erf(omega*r)/r
            !
            if (Kappa > ZERO) then
                  Omega = Sqrt(ONE/Kappa)
            else
                  Omega = huge(ONE)
            end if
            if (ComputeGrids) then
                  call rpa_OptimizeQuads(NFreqs, Freqs, FreqWeights, NLaplace, LaplaceX, LaplaceW, &
                        daiValues, daiWeights, TargetErrorFreq, TargetRelErrorFreq, TargetErrorLaplace, &
                        TargetRelErrorLaplace, GridLimitDai)
            else
                  call msg("Using frequency and Laplace transform quadratures from previous run")
                  call msg("Frequency integration will employ " // str(NFreqs) // " points")
            end if
            time_W = ZERO
            time_WRG = ZERO
            time_GRWRG = ZERO
            time_LogDet = ZERO
            allocate(time_Iter(NFreqs))
            time_Iter = ZERO
            time_Density = ZERO
            time_Cholesky = ZERO
            allocate(RhoOcc(NAO**2*maxval(NLaplace)*NSpins))
            allocate(RhoVirt(NAO**2*maxval(NLaplace)*NSpins))
            DimGov = 0
            DimFov = 0
            do s = 1, NSpins
                  DimGov = max(DimGov, max(NOcc(s), NVirt(s)) * maxval(NLaplace))
                  DimFov = max(DimFov, max(NOcc(s), NVirt(s)) * NAO * maxval(NLaplace))
            end do
            allocate(Gov(DimGov))
            allocate(Fov(DimFov))
            if (ComputeRWRBasis) then
                  if (ComputeCholeskyBasis) then
                        ! 
                        ! Cholesky factorization of the Coulomb matrix
                        ! 
                        ! The Cholesky subroutine will factorize the V(pq|rs) matrix
                        ! and compute the list of nonnegligible shell pairs. In the subsequent steps,
                        ! the packed storage is used and, therefore, only the nonnegligible shell
                        ! pairs are manipulated and stored in memory.
                        !
                        MaxNShellPairs = (NShells * (NShells + 1)) / 2
                        allocate(ShellPairs(2, MaxNShellPairs))
                        allocate(ShellPairLoc(3, MaxNShellPairs))
                        allocate(ShellPairDim(MaxNShellPairs))
                        call clock_start(timer_Cholesky)
                        call chol_CoulombMatrix_A(RBra, NCholesky, ShellPairs, ShellPairLoc, ShellPairDim, NShellPairs, &
                              NOrbPairs, SubsetDim, SubsetBounds, NSubsets, CholeskyTauThresh, &
                              ShellCenters, &
                              AtomCoords, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, &
                              Exponents, NormFactors, Kappa, LmaxGTO, NAO, NShells, MaxNAOMult, SpherAO, MaxBlockDim)
                        time_Cholesky = time_Cholesky + clock_readwall(timer_Cholesky)
                  end if
                  MaxSubsetDim = maxval(SubsetDim)
                  MaxNCholesky = size(RBra, dim=1)
                  ! ----------------------------------------------------------------------------
                  !  Zeroth iteration: Establish the basis for the effective dielectric matrix
                  ! ----------------------------------------------------------------------------
                  ! Compute the dielectric matrix for the lowest frequency (RWR). Use RWR to get
                  ! the orthogonal random vectors basis G and the matrix product RG. RG computed
                  ! once for the lowest frequency will be used for all higher frequencies.
                  !
                  call msg("Starting computation of RWR in full Cholesky basis")
                  call clock_start(timer)            
                  u = 1
                  !
                  ! Pseudodensities computed for the frequency u and for all Laplace quadrature
                  ! points related to u
                  !
                  do s = 1, NSpins
                        s0 = (s - 1) * NAO**2 * NLaplace(u) + 1
                        s1 = s * NAO**2 * NLaplace(u)
                        call rpa_Rho(RhoOcc(s0:s1), RhoVirt(s0:s1), &
                              Gov, Fov, OccCoeffs(:, :, s), VirtCoeffs(:, :, s), OccEnergies(:, s), VirtEnergies(:, s), &
                              LaplaceX(1:NLaplace(u), u), NAO, NLaplace(u), NOcc(s), NVirt(s))
                  end do
                  time_Density = time_Density + clock_readwall(timer)
                  By = ThisImage
                  allocate(RWR(NCholesky, NCholesky)[*])
                  call rpa_RWR(RWR, RBra, RhoOcc, RhoVirt, &
                        ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, LaplaceW(:, u), ShellParamsIdx, &
                        NAngFunc, NAO, NSpins, NCholesky, NLaplace(u), By, SubsetBounds, SubsetDim, NSubsets, &
                        MaxSubsetDim, MaxNCholesky, time_W, time_WRG, time_GRWRG)
                  allocate(G(NCholesky, NCholesky))
                  call rpa_ScaleAndSymmetrize(RWR, G, NSpins, NCholesky)
                  call rpa_G(G, RWR, NVecsPiU, TargetErrorRandom, NCholesky, RWRBasisType, NAO)
                  allocate(RGBra(NVecsPiU, MaxSubsetDim, NSubsets(1))[*])
                  By = ThisImage
                  do Bx = 1, NSubsets(1)
                        Bra = Bx + (By - 1) * NSubsets(1)
                        NBra = SubsetDim(Bra)
                        if (NBra > 0) then
                              call real_aTb_x(RGBra(:, :, Bx), NVecsPiU, G, NCholesky, RBra(:, :, Bx), &
                                    MaxNCholesky, NVecsPiU, NBra, NCholesky)
                        else
                              RGBra(:, :, Bx) = ZERO
                        end if
                  end do
                  deallocate(G)
                  deallocate(RBra)
                  time_iter(u) = clock_readwall(timer)
                  call msg("Matrix RWR computed in " // str(time_iter(u),d=1) // " seconds" )
                  call blankline()
                  ! ----------------------------------------------------------------------------
                  !    End of zeroth iteration: computation of the effective basis completed
                  ! ----------------------------------------------------------------------------
            else
                  call msg("Using matrix R*G from previous run")
            end if
            allocate(GPiUGFull(NVecsPiU, NVecsPiU))
            if (ComputeRWRBasis) then
                  GPiUGFull = RWR(1:NVecsPiU, 1:NVecsPiU)
            end if
            if (ComputeRWRBasis) then
                  deallocate(RWR)
            end if
            allocate(GPiUG((NVecsPiU*(NVecsPiU+1))/2, NFreqs)[*])
            allocate(GPiUGT(NVecsPiU, NVecsPiU))
            GPiUG = ZERO
            if (ComputeRWRBasis) then
                  FirstIter = 2
                  if (ThisImage == 1) then
                        call rpa_Pack(GPiUG(:, 1), GPiUGFull, GPiUGT, NVecsPiU)
                  end if
            else
                  FirstIter = 1
            end if
            if (NSubsets(2) > 1) then
                  allocate(RGKet(NVecsPiU, MaxSubsetDim, NSubsets(1))[*])
            end if
            allocate(WRG(NVecsPiU, MaxSubsetDim))
            allocate(W(MaxSubsetDim**2))
            call msg("Memory allocation (in gigabytes, per image):")
            call msg("RG (bra vectors) " // str(io_size_byte(RGBra)/(1024.0_F64**3),d=1))
            if (NSubsets(2) > 1) then
                  call msg("RG (ket vectors) " // str(io_size_byte(RGKet)/(1024.0_F64**3),d=1))
            end if
            call msg("GPiUG            " // str(io_size_byte(GPiUG)/(1024.0_F64**3),d=1))
            call msg(str(NOrbPairs)//" orbital pairs are divided into "//str(NSubsets(1)*NSubsets(2))//" subsets")
            call msg("Max subset dimension: " // str(MaxSubsetDim))
            call msg("NSubsets per image: " // str(NSubsets(1)))            
            line = lfield("Freq", 10)  // lfield("Time", 10) // lfield("MatrixBlocks", 12)
            call midrule(width=32)
            call msg(line)
            call midrule(width=32)
            By = ThisImage
            do L = 1, NSubsets(2) / 2 + 1
                  call rpa_MatrixBlocksMap(Ky, Ly, Transpose, By, L, NSubsets)
                  if (Ky /= By) then
                        sync all
                        ! RGKet(:, :, :)[Ly] = RGBra
                        call rpa_SendVecs_F64(RGKet, RGBra, Ly)
                        sync all
                  end if
                  FreqsLoop: do u = FirstIter, NFreqs
                        call clock_start(timer_Freq)
                        GPiUGFull = ZERO
                        !
                        ! Pseudodensities computed for the frequency u and for all Laplace quadrature
                        ! points related to u
                        !
                        do s = 1, NSpins
                              s0 = (s - 1) * NAO**2 * NLaplace(u) + 1
                              s1 = s * NAO**2 * NLaplace(u)
                              call rpa_Rho(RhoOcc(s0:s1), RhoVirt(s0:s1), &
                                    Gov, Fov, OccCoeffs(:, :, s), VirtCoeffs(:, :, s), OccEnergies(:, s), VirtEnergies(:, s), &
                                    LaplaceX(1:NLaplace(u), u), NAO, NLaplace(u), NOcc(s), NVirt(s))
                        end do
                        time_Density = time_Density + clock_readwall(timer_Freq)
                        if (By /= Ky) then
                              if (NSpins == 1) then
                                    call rpa_GPiUG(GPiUGFull, WRG, W, RGBra, &
                                          RGKet, RhoOcc, RhoVirt, ShellPairs(:, 1:NShellPairs), &
                                          ShellPairLoc, ShellPairDim, ShellLoc, &
                                          LaplaceW(1:NLaplace(u), u), ShellParamsIdx, NAngFunc, NAO, &
                                          NVecsPiU, NLaplace(u), By, Ky, Transpose, &
                                          SubsetBounds, SubsetDim, NSubsets, time_W, time_WRG, time_GRWRG)
                              else
                                    call rpa_GPiUG_SpinUnres(GPiUGFull, WRG, W, RGBra, &
                                          RGKet, RhoOcc, RhoVirt, ShellPairs(:, 1:NShellPairs), &
                                          ShellPairLoc, ShellPairDim, ShellLoc, &
                                          LaplaceW(1:NLaplace(u), u), ShellParamsIdx, NAngFunc, NAO, &
                                          NVecsPiU, NLaplace(u), By, Ky, Transpose, &
                                          SubsetBounds, SubsetDim, NSubsets, time_W, time_WRG, time_GRWRG)
                              end if
                        else
                              if (NSpins == 1) then
                                    call rpa_GPiUG(GPiUGFull, WRG, W, RGBra, &
                                          RGBra, RhoOcc, RhoVirt, ShellPairs(:, 1:NShellPairs), &
                                          ShellPairLoc, ShellPairDim, ShellLoc, &
                                          LaplaceW(1:NLaplace(u), u), ShellParamsIdx, NAngFunc, NAO, &
                                          NVecsPiU, NLaplace(u), By, Ky, Transpose, &
                                          SubsetBounds, SubsetDim, NSubsets, time_W, time_WRG, time_GRWRG)
                              else
                                    call rpa_GPiUG_SpinUnres(GPiUGFull, WRG, W, RGBra, &
                                          RGBra, RhoOcc, RhoVirt, ShellPairs(:, 1:NShellPairs), &
                                          ShellPairLoc, ShellPairDim, ShellLoc, &
                                          LaplaceW(1:NLaplace(u), u), ShellParamsIdx, NAngFunc, NAO, &
                                          NVecsPiU, NLaplace(u), By, Ky, Transpose, &
                                          SubsetBounds, SubsetDim, NSubsets, time_W, time_WRG, time_GRWRG)
                              end if
                        end if
                        call rpa_ScaleAndSymmetrize(GPiUGFull, GPiUGT, NSpins, NVecsPiU)
                        call rpa_Pack(GPiUG(:, u), GPiUGFull, GPiUGT, NVecsPiU)
                        dt = clock_readwall(timer_Freq)
                        time_iter(u) = time_iter(u) + dt
                        if (Transpose) then
                              line = lfield(str(Freqs(u),d=1), 10) // lfield(str(dt, d=1), 10) &
                                    // "("//str(Ky)//","//str(By)//"), ("//str(By)//","//str(Ky)//")"
                        else
                              line = lfield(str(Freqs(u),d=1), 10) // lfield(str(dt, d=1), 10) &
                                    // "("//str(Ky)//","//str(By)//")"
                        end if
                        call msg(line)
                  end do FreqsLoop
            end do
            ! if (NImages > 1) then
            !       call co_sum(GPiUG, result_image=1)
            ! end if
            sync all
            call rpa_ReduceGPiUG(GPiUG)
            sync all
            Ecorr = ZERO
            if (ThisImage == 1) then
                  ! ------------------------------------------------------------
                  !       Numerical integral of the RPA correlation energy
                  ! ------------------------------------------------------------
                  call real_LogDet_query(LogDetLWork, NVecsPiU, NVecsPiU)
                  allocate(LogDetWork(LogDetLWork))
                  do u = 1, NFreqs
                        !
                        ! Compute the u-frequency contribution to the correlation energy
                        ! E(u) = Sum(k=1...NVecsPiU) log(1+LambdaK) - LambdaK
                        ! where LambdaK's are the eigenvalues of GPiUG.
                        !
                        call rpa_Unpack(GPiUGFull, GPiUG(:, u), GPiUGT, NVecsPiU)
                        TrGPiUG = 0.0_F128
                        do k = 1, NVecsPiU
                              TrGPiUG = TrGPiUG + real(GPiUGFull(k, k), F128)
                        end do
                        do k = 1, NVecsPiU
                              GPiUGFull(k, k) = GPiUGFull(k, k) + ONE
                        end do
                        call clock_start(timer_LogDet)
                        call real_LogDet(LogDet, GPiUGFull, NVecsPiU, NVecsPiU, LogDetWork, LogDetLWork)
                        time_LogDet = time_LogDet + clock_readwall(timer_LogDet)
                        Eu_F128 = LogDet - TrGPiUG
                        Eu = real(Eu_F128, F64)
                        Ecorr = Ecorr + ONE/(TWO*PI) * Eu * FreqWeights(u)
                        if (u == 1) then
                              line = lfield("#", 5) // lfield("Freq", 10) // lfield("Weight", 10) &
                                    // lfield("E(Freq)", 20) // lfield("Time", 10)
                              call blankline()
                              call midrule(width=55)
                              call msg(line)
                              call midrule(width=55)
                        end if
                        line = lfield(str(u), 5) // lfield(str(Freqs(u),d=1), 10) // lfield(str(FreqWeights(u),d=1), 10) &
                              // lfield(str(Eu,d=6), 20) // lfield(str(time_iter(u), d=1), 10)
                        call msg(line)
                  end do
                  call blankline()
                  call msg("RPA correlation energy (a.u.): " // str(Ecorr, d=8))
            end if
      end subroutine rpa_FreqIntegral_AO


      subroutine rpa_FreqIntegral_MO(Energy, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, ShellCenters, &
            AtomCoords, LmaxGTO, ShellLoc, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, &
            Exponents, NormFactors, NAO, NShells, NAtoms, NOcc, NVirt, NSpins, SpherAO, &
            
            Kappa, TargetErrorLaplace, TargetRelErrorLaplace, CholeskyTauThresh, &
            TargetErrorRandom, TargetErrorFreq, TargetRelErrorFreq, MaxNAOMult, &
            RWRBasisType, MaxBlockDim, &

            ComputeGrids, GridLimitDai, NFreqs, Freqs, FreqWeights, NLaplace, LaplaceX, LaplaceW, &
            daiValues, daiWeights, &
            
            ComputeRWRBasis, ComputeCholeskyBasis, RGy, Ry, NVecsPiU, NCholesky, MaxSubsetDim, &
            NSubsets, NOrbPairs, NShellPairs, ShellPairs, &
            ShellPairLoc, ShellPairDim, SubsetDim, SubsetBounds, &

            time_W, time_WRG, time_GRWRG, time_LogDet, time_Density, time_Cholesky)

            real(F64), dimension(:), intent(inout)                       :: Energy
            real(F64), dimension(:, :, :), intent(in)                    :: OccCoeffs
            real(F64), dimension(:, :, :), intent(in)                    :: VirtCoeffs
            real(F64), dimension(:, :), intent(in)                       :: OccEnergies
            real(F64), dimension(:, :), intent(in)                       :: VirtEnergies
            integer, dimension(NShells), intent(in)                      :: ShellCenters
            real(F64), dimension(3, NAtoms), intent(in)                  :: AtomCoords
            integer, intent(in)                                          :: LmaxGTO
            integer, dimension(NShells), intent(in)                      :: ShellLoc
            integer, dimension(NShells), intent(in)                      :: ShellParamsIdx
            integer, dimension(:), intent(in)                            :: ShellMomentum
            integer, dimension(:), intent(in)                            :: NAngFunc
            integer, dimension(:), intent(in)                            :: NPrimitives
            real(F64), dimension(:, :), intent(in)                       :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)                       :: Exponents
            real(F64), dimension(:, :), intent(in)                       :: NormFactors
            integer, intent(in)                                          :: NAO
            integer, intent(in)                                          :: NShells
            integer, intent(in)                                          :: NAtoms
            integer, dimension(:), intent(in)                            :: NOcc
            integer, dimension(:), intent(in)                            :: NVirt
            integer, intent(in)                                          :: NSpins
            logical, intent(in)                                          :: SpherAO
            
            real(F64), intent(in)                                        :: Kappa
            real(F64), intent(in)                                        :: TargetErrorLaplace
            real(F64), intent(in)                                        :: TargetRelErrorLaplace
            real(F64), intent(in)                                        :: CholeskyTauThresh
            real(F64), intent(in)                                        :: TargetErrorRandom
            real(F64), intent(in)                                        :: TargetErrorFreq
            real(F64), intent(in)                                        :: TargetRelErrorFreq
            real(F64), intent(in)                                        :: MaxNAOMult
            integer, intent(in)                                          :: RWRBasisType
            integer, intent(in)                                          :: MaxBlockDim

            logical, intent(in)                                          :: ComputeGrids
            logical, intent(in)                                          :: GridLimitDai
            integer, intent(inout)                                       :: NFreqs
            real(F64), dimension(:), allocatable, intent(inout)          :: Freqs
            real(F64), dimension(:), allocatable, intent(inout)          :: FreqWeights
            integer, dimension(:), allocatable, intent(inout)            :: NLaplace
            real(F64), dimension(:, :), allocatable, intent(inout)       :: LaplaceX
            real(F64), dimension(:, :), allocatable, intent(inout)       :: LaplaceW
            real(F64), dimension(:, :), intent(in)                       :: daiValues
            real(F64), dimension(:, :), intent(in)                       :: daiWeights

            logical, intent(in)                                          :: ComputeRWRBasis
            logical, intent(in)                                          :: ComputeCholeskyBasis
            real(F64), dimension(:, :, :), allocatable, intent(inout)    :: RGy[:]
            real(F64), dimension(:, :, :), allocatable, intent(inout)    :: Ry[:]
            integer, intent(inout)                                       :: NVecsPiU
            integer, intent(inout)                                       :: NCholesky
            integer, intent(inout)                                       :: MaxSubsetDim
            integer, dimension(2), intent(inout)                         :: NSubsets
            integer, intent(inout)                                       :: NOrbPairs
            integer, intent(inout)                                       :: NShellPairs
            integer, dimension(:, :), allocatable, intent(inout)         :: ShellPairs
            integer, dimension(:, :), allocatable, intent(inout)         :: ShellPairLoc
            integer, dimension(:), allocatable, intent(inout)            :: ShellPairDim
            integer, dimension(:), allocatable, intent(inout)            :: SubsetDim
            integer, dimension(:, :), allocatable, intent(inout)         :: SubsetBounds

            real(F64), intent(out)                                       :: time_W
            real(F64), intent(out)                                       :: time_WRG
            real(F64), intent(out)                                       :: time_GRWRG
            real(F64), intent(out)                                       :: time_LogDet
            real(F64), intent(out)                                       :: time_Density
            real(F64), intent(out)                                       :: time_Cholesky
            
            integer :: u, s, FirstIter, k
            real(F64) :: Omega
            real(F64) :: Eu, EcRPA
            real(F64), dimension(:), allocatable :: time_Iter
            real(F64), dimension(:, :, :), allocatable :: CR, CRG
            real(F64), dimension(:, :), allocatable :: DCRG            
            real(F64), dimension(:, :), allocatable :: G
            real(F64), dimension(:, :), allocatable :: GPiUG[:]
            real(F64), dimension(:, :), allocatable :: GPiUGFull, GPiUGT

            real(F64), dimension(:, :), allocatable :: RWR[:]
            integer, dimension(:, :, :), allocatable :: OccBounds, VirtBounds
            integer, dimension(:, :), allocatable :: OccSubsetDim, VirtSubsetDim
            integer :: MaxNai, Nai, Npq
            integer :: MaxNShellPairs
            integer :: MaxNCholesky
            type(tclock) :: timer_LogDet, timer_Cholesky, timer_MOTransf, timer_GRWRG
            character(:), allocatable :: line
            real(F128) :: TrGPiUG, LogDet, Eu_F128
            real(F64), dimension(:), allocatable :: LogDetWork
            integer :: LogDetLWork
            integer :: X, Y, SubsetIdx
            integer :: NImages, ThisImage

            NImages = num_images()
            ThisImage = this_image()
            Y = ThisImage
            allocate(OccBounds(2, 2, NImages))
            allocate(VirtBounds(2, 2, NImages))
            allocate(OccSubsetDim(2, NImages))
            allocate(VirtSubsetDim(2, NImages))
            call rpa_OVSubsets(OccBounds, VirtBounds, OccSubsetDim, VirtSubsetDim, &
                  NOcc, NVirt, NSpins)            
            MaxNai = 0
            do s = 1, NSpins
                  MaxNai = max(MaxNai, OccSubsetDim(s, Y)*VirtSubsetDim(s, Y))
            end do
            !
            ! Range-separation parameter for the range-separated
            ! Coulomb operator 1/r=erfc(omega*r)/r + erf(omega*r)/r
            !
            if (Kappa > ZERO) then
                  Omega = Sqrt(ONE/Kappa)
            else
                  Omega = huge(ONE)
            end if
            if (ComputeGrids) then
                  call rpa_OptimizeQuads(NFreqs, Freqs, FreqWeights, NLaplace, LaplaceX, LaplaceW, &
                        daiValues, daiWeights, TargetErrorFreq, TargetRelErrorFreq, TargetErrorLaplace, &
                        TargetRelErrorLaplace, GridLimitDai)
            else
                  call msg("Using frequency and Laplace transform quadratures from previous run")
                  call msg("Frequency integration will employ " // str(NFreqs) // " points")
            end if
            time_W = ZERO
            time_WRG = ZERO
            time_GRWRG = ZERO
            time_LogDet = ZERO
            allocate(time_Iter(NFreqs))
            time_Iter = ZERO
            time_Density = ZERO
            time_Cholesky = ZERO
            if (ComputeRWRBasis) then
                  if (ComputeCholeskyBasis) then
                        ! 
                        ! Cholesky factorization of the Coulomb matrix
                        ! 
                        ! The Cholesky subroutine will factorize the V(pq|rs) matrix
                        ! and compute the list of nonnegligible shell pairs. In the subsequent steps,
                        ! the packed storage is used and, therefore, only the nonnegligible shell
                        ! pairs are manipulated and stored in memory.
                        !
                        MaxNShellPairs = (NShells * (NShells + 1)) / 2
                        allocate(ShellPairs(2, MaxNShellPairs))
                        allocate(ShellPairLoc(3, MaxNShellPairs))
                        allocate(ShellPairDim(MaxNShellPairs))
                        call clock_start(timer_Cholesky)
                        call chol_CoulombMatrix_A(Ry, NCholesky, ShellPairs, ShellPairLoc, ShellPairDim, NShellPairs, &
                              NOrbPairs, SubsetDim, SubsetBounds, NSubsets, CholeskyTauThresh, &
                              ShellCenters, &
                              AtomCoords, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, &
                              Exponents, NormFactors, Kappa, LmaxGTO, NAO, NShells, MaxNAOMult, SpherAO, MaxBlockDim)
                        time_Cholesky = time_Cholesky + clock_readwall(timer_Cholesky)
                  end if
                  MaxSubsetDim = maxval(SubsetDim)
                  MaxNCholesky = size(Ry, dim=1)
                  ! ----------------------------------------------------------------------------
                  !  Zeroth iteration: Establish the basis for the effective dielectric matrix
                  ! ----------------------------------------------------------------------------
                  ! Compute the dielectric matrix for the lowest frequency (RWR). Use RWR to get
                  ! the orthogonal random vectors basis G and the matrix product RG. RG computed
                  ! once for the lowest frequency will be used for all higher frequencies.
                  !
                  call blankline()
                  call msg("Starting AO->MO transform")
                  call clock_start(timer_MOTransf)
                  allocate(CR(NCholesky, MaxNai, NSpins))
                  call rpa_CRG(CR, Ry, OccCoeffs, VirtCoeffs, OccSubsetDim(:, Y), VirtSubsetDim(:, Y), &
                        OccBounds(:, :, Y), VirtBounds(:, :, Y), NSpins, &
                        ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
                        SubsetDim, NSubsets, NAngFunc, NAO, NCholesky)
                  time_WRG = time_WRG + clock_readwall(timer_MOTransf)
                  call msg("MO transform done in " // str(time_WRG,d=1) // " seconds" )
                  call blankline()
                  u = 1
                  call msg("Computation of RWR in full Cholesky basis")
                  call clock_start(timer_GRWRG)
                  allocate(RWR(NCholesky, NCholesky)[*])
                  allocate(DCRG(NCholesky, MaxNai))
                  RWR = ZERO
                  call rpa_GPiUG_MO(RWR, CR, DCRG, OccEnergies, VirtEnergies, OccSubsetDim(:, Y), &
                        VirtSubsetDim(:, Y), OccBounds(:, :, Y), VirtBounds(:, :, Y), NSpins, Freqs(u))
                  sync all
                  call rpa_ReduceRWR(RWR)
                  sync all
                  deallocate(DCRG)
                  allocate(G(NCholesky, NCholesky))
                  call rpa_G(G, RWR, NVecsPiU, TargetErrorRandom, NCholesky, RWRBasisType, NAO)
                  allocate(RGy(NVecsPiU, MaxSubsetDim, NSubsets(1))[*])
                  do X = 1, NSubsets(1)
                        SubsetIdx = X + (Y - 1) * NSubsets(1)
                        Npq = SubsetDim(SubsetIdx)
                        if (Npq > 0) then
                              call real_aTb_x(RGy(:, :, X), NVecsPiU, G, NCholesky, Ry(:, :, X), &
                                    MaxNCholesky, NVecsPiU, Npq, NCholesky)
                        else
                              RGy(:, :, X) = ZERO
                        end if
                  end do
                  deallocate(Ry)
                  allocate(CRG(NVecsPiU, MaxNai, NSpins))
                  do s = 1, NSpins
                        Nai = OccSubsetDim(s, Y) * VirtSubsetDim(s, Y)
                        if (Nai > 0) then
                              call real_aTb_x(CRG(:, :, s), NVecsPiU, G, NCholesky, CR(:, :, s), NCholesky, &
                                    NVecsPiU, Nai, NCholesky)
                        else
                              CRG(:, :, s) = ZERO
                        end if
                  end do
                  deallocate(CR)
                  deallocate(G)
                  time_iter(u) = clock_readwall(timer_GRWRG)
                  time_GRWRG = time_GRWRG + time_iter(u)
                  call msg("Matrix RWR computed in " // str(time_iter(u),d=1) // " seconds" )
                  call blankline()
                  ! ----------------------------------------------------------------------------
                  !    End of zeroth iteration: computation of the effective basis completed
                  ! ----------------------------------------------------------------------------
            else
                  call msg("Using matrix R*G from previous run")
                  call blankline()
                  call msg("Starting AO->MO transform")
                  call clock_start(timer_MOTransf)
                  allocate(CRG(NVecsPiU, MaxNai, NSpins))
                  call rpa_CRG(CRG, RGy, OccCoeffs, VirtCoeffs, OccSubsetDim(:, Y), VirtSubsetDim(:, Y), &
                        OccBounds(:, :, Y), VirtBounds(:, :, Y), NSpins, &
                        ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
                        SubsetDim, NSubsets, NAngFunc, NAO, NVecsPiU)
                  time_WRG = time_WRG + clock_readwall(timer_MOTransf)
                  call msg("MO transform done in " // str(time_WRG,d=1) // " seconds" )
                  call blankline()
            end if
            allocate(GPiUGFull(NVecsPiU, NVecsPiU))            
            if (ComputeRWRBasis) then
                  GPiUGFull = RWR(1:NVecsPiU, 1:NVecsPiU)
                  deallocate(RWR)
            end if
            allocate(DCRG(NVecsPiU, MaxNai))
            allocate(GPiUG((NVecsPiU*(NVecsPiU+1))/2, NFreqs)[*])
            allocate(GPiUGT(NVecsPiU, NVecsPiU))
            GPiUG = ZERO
            if (ComputeRWRBasis) then
                  FirstIter = 2
                  if (ThisImage == 1) then
                        call rpa_Pack(GPiUG(:, 1), GPiUGFull, GPiUGT, NVecsPiU)
                  end if
            else
                  FirstIter = 1
            end if
            call msg("Memory allocation (in gigabytes, per image):")
            call msg("CRG              " // str(io_size_byte(CRG)/(1024.0_F64**3),d=1))
            call msg("DCRG             " // str(io_size_byte(DCRG)/(1024.0_F64**3),d=1))
            call msg("GPiUG            " // str(io_size_byte(GPiUG)/(1024.0_F64**3),d=1))
            call msg(str(NOrbPairs)//" orbital pairs are divided into "//str(NSubsets(1)*NSubsets(2))//" subsets")
            call msg("Max subset dimension: " // str(MaxSubsetDim))
            call msg("NSubsets per image: " // str(NSubsets(1)))
            !
            ! Compute the GPi(u)G matrices for all frequencies
            !
            call blankline()
            call msg("Computing (GRC)**T*D(u)*CRG for "//str(NFreqs-FirstIter+1)//" frequencies")
            call blankline()
            line = lfield("#", 5) // lfield("Freq", 10) // lfield("Time", 10)
            call msg(line)
            FreqsLoop: do u = FirstIter, NFreqs
                  call clock_start(timer_GRWRG)
                  GPiUGFull = ZERO
                  call rpa_GPiUG_MO(GPiUGFull, CRG, DCRG, OccEnergies, VirtEnergies, OccSubsetDim(:, Y), &
                        VirtSubsetDim(:, Y), OccBounds(:, :, Y), VirtBounds(:, :, Y), NSpins, Freqs(u))
                  call rpa_Pack(GPiUG(:, u), GPiUGFull, GPiUGT, NVecsPiU)
                  time_iter(u) = clock_readwall(timer_GRWRG)
                  time_GRWRG = time_GRWRG + time_iter(u)
                  line = lfield(str(u), 5) // lfield(str(Freqs(u),d=1), 10) // lfield(str(time_iter(u), d=1), 10)
                  call msg(line)
            end do FreqsLoop
            ! if (NImages > 1) then
            !       call co_sum(GPiUG, result_image=1)
            ! end if
            sync all
            call rpa_ReduceGPiUG(GPiUG)
            sync all
            EcRPA = ZERO
            if (ThisImage == 1) then
                  ! ------------------------------------------------------------
                  !       Numerical integral of the RPA correlation energy
                  ! ------------------------------------------------------------
                  call real_LogDet_query(LogDetLWork, NVecsPiU, NVecsPiU)
                  allocate(LogDetWork(LogDetLWork))
                  do u = 1, NFreqs
                        !
                        ! Compute the u-frequency contribution to the correlation energy
                        ! E(u) = Sum(k=1...NVecsPiU) log(1+LambdaK) - LambdaK
                        ! where LambdaK's are the eigenvalues of GPiUG.
                        !
                        call rpa_Unpack(GPiUGFull, GPiUG(:, u), GPiUGT, NVecsPiU)
                        TrGPiUG = 0.0_F128
                        do k = 1, NVecsPiU
                              TrGPiUG = TrGPiUG + real(GPiUGFull(k, k), F128)
                        end do
                        do k = 1, NVecsPiU
                              GPiUGFull(k, k) = GPiUGFull(k, k) + ONE
                        end do
                        call clock_start(timer_LogDet)
                        call real_LogDet(LogDet, GPiUGFull, NVecsPiU, NVecsPiU, LogDetWork, LogDetLWork)
                        time_LogDet = time_LogDet + clock_readwall(timer_LogDet)
                        Eu_F128 = LogDet - TrGPiUG
                        Eu = real(Eu_F128, F64)
                        EcRPA = EcRPA + ONE/(TWO*PI) * Eu * FreqWeights(u)
                        if (u == 1) then
                              line = lfield("#", 5) // lfield("Freq", 10) // lfield("Weight", 10) &
                                    // lfield("E(Freq)", 20) // lfield("Time", 10)
                              call blankline()
                              call midrule(width=55)
                              call msg(line)
                              call midrule(width=55)
                        end if
                        line = lfield(str(u), 5) // lfield(str(Freqs(u),d=1), 10) // lfield(str(FreqWeights(u),d=1), 10) &
                              // lfield(str(Eu,d=6), 20) // lfield(str(time_iter(u), d=1), 10)
                        call msg(line)
                  end do
                  call blankline()
                  call msg("RPA correlation energy (a.u.): " // str(EcRPA, d=8))
                  Energy(RPA_ENERGY_CORR) = EcRPA
            end if
      end subroutine rpa_FreqIntegral_MO


      subroutine rpa_CC_Energy_1(Energy, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, ShellCenters, &
            AtomCoords, LmaxGTO, ShellLoc, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, &
            Exponents, NormFactors, NAO, NShells, NAtoms, NOcc, NVirt, NSpins, SpherAO, AOBasis, &
            
            Kappa, TargetErrorLaplace, TargetRelErrorLaplace, CholeskyTauThresh, &
            TargetErrorRandom, TargetErrorFreq, TargetRelErrorFreq, MaxNAOMult, &
            RWRBasisType, MaxBlockDim, &

            ComputeGrids, GridLimitDai, NFreqs, Freqs, FreqWeights, NLaplace, LaplaceX, LaplaceW, &
            daiValues, daiWeights, &
            
            ComputeRWRBasis, ComputeCholeskyBasis, RGy, Ry, NVecsPiU, NCholesky, MaxSubsetDim, &
            NSubsets, NOrbPairs, NShellPairs, ShellPairs, &
            ShellPairLoc, ShellPairDim, SubsetDim, SubsetBounds, &

            SmallEigenvalsCutoffT2, F_ao, NACPoints, T1Approx, ExchangeApprox, Ec1RDMApprox, &
            DensityApprox, Purify1RDM, GuessNVecsT2, ChiOrbitals, MeanField, T2Interp, &
            MeanFieldApprox, MaxBatchDimT2, &

            time_W, time_WRG, time_GRWRG, time_LogDet, time_Density, time_Cholesky)

            real(F64), dimension(:), intent(inout)                       :: Energy
            real(F64), dimension(:, :, :), intent(inout)                 :: OccCoeffs
            real(F64), dimension(:, :, :), intent(inout)                 :: VirtCoeffs
            real(F64), dimension(:, :), intent(inout)                    :: OccEnergies
            real(F64), dimension(:, :), intent(inout)                    :: VirtEnergies
            integer, dimension(NShells), intent(in)                      :: ShellCenters
            real(F64), dimension(3, NAtoms), intent(in)                  :: AtomCoords
            integer, intent(in)                                          :: LmaxGTO
            integer, dimension(NShells), intent(in)                      :: ShellLoc
            integer, dimension(NShells), intent(in)                      :: ShellParamsIdx
            integer, dimension(:), intent(in)                            :: ShellMomentum
            integer, dimension(:), intent(in)                            :: NAngFunc
            integer, dimension(:), intent(in)                            :: NPrimitives
            real(F64), dimension(:, :), intent(in)                       :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)                       :: Exponents
            real(F64), dimension(:, :), intent(in)                       :: NormFactors
            integer, intent(in)                                          :: NAO
            integer, intent(in)                                          :: NShells
            integer, intent(in)                                          :: NAtoms
            integer, dimension(:), intent(in)                            :: NOcc
            integer, dimension(:), intent(in)                            :: NVirt
            integer, intent(in)                                          :: NSpins
            logical, intent(in)                                          :: SpherAO
            type(TAOBasis), intent(in)                                   :: AOBasis
            
            real(F64), intent(in)                                        :: Kappa
            real(F64), intent(in)                                        :: TargetErrorLaplace
            real(F64), intent(in)                                        :: TargetRelErrorLaplace
            real(F64), intent(in)                                        :: CholeskyTauThresh
            real(F64), intent(in)                                        :: TargetErrorRandom
            real(F64), intent(in)                                        :: TargetErrorFreq
            real(F64), intent(in)                                        :: TargetRelErrorFreq
            real(F64), intent(in)                                        :: MaxNAOMult
            integer, intent(in)                                          :: RWRBasisType
            integer, intent(in)                                          :: MaxBlockDim

            logical, intent(in)                                          :: ComputeGrids
            logical, intent(in)                                          :: GridLimitDai
            integer, intent(inout)                                       :: NFreqs
            real(F64), dimension(:), allocatable, intent(inout)          :: Freqs
            real(F64), dimension(:), allocatable, intent(inout)          :: FreqWeights
            integer, dimension(:), allocatable, intent(inout)            :: NLaplace
            real(F64), dimension(:, :), allocatable, intent(inout)       :: LaplaceX
            real(F64), dimension(:, :), allocatable, intent(inout)       :: LaplaceW
            real(F64), dimension(:, :), intent(in)                       :: daiValues
            real(F64), dimension(:, :), intent(in)                       :: daiWeights

            logical, intent(in)                                          :: ComputeRWRBasis
            logical, intent(in)                                          :: ComputeCholeskyBasis
            real(F64), dimension(:, :, :), allocatable, intent(inout)    :: RGy[:]
            real(F64), dimension(:, :, :), allocatable, intent(inout)    :: Ry[:]
            integer, intent(inout)                                       :: NVecsPiU
            integer, intent(inout)                                       :: NCholesky
            integer, intent(inout)                                       :: MaxSubsetDim
            integer, dimension(2), intent(inout)                         :: NSubsets
            integer, intent(inout)                                       :: NOrbPairs
            integer, intent(inout)                                       :: NShellPairs
            integer, dimension(:, :), allocatable, intent(inout)         :: ShellPairs
            integer, dimension(:, :), allocatable, intent(inout)         :: ShellPairLoc
            integer, dimension(:), allocatable, intent(inout)            :: ShellPairDim
            integer, dimension(:), allocatable, intent(inout)            :: SubsetDim
            integer, dimension(:, :), allocatable, intent(inout)         :: SubsetBounds

            real(F64), intent(in)                                        :: SmallEigenvalsCutoffT2
            real(F64), dimension(:, :, :), intent(in)                    :: F_ao
            integer, intent(in)                                          :: NACPoints
            integer, intent(in)                                          :: T1Approx
            integer, intent(in)                                          :: ExchangeApprox
            integer, intent(in)                                          :: Ec1RDMApprox
            integer, intent(in)                                          :: DensityApprox
            integer, intent(in)                                          :: Purify1RDM
            real(F64), intent(in)                                        :: GuessNVecsT2
            integer, intent(in)                                          :: ChiOrbitals
            integer, intent(in)                                          :: MeanField
            logical, intent(in)                                          :: T2Interp
            integer, intent(in)                                          :: MeanFieldApprox
            integer, intent(in)                                          :: MaxBatchDimT2
            
            real(F64), intent(out)                                       :: time_W
            real(F64), intent(out)                                       :: time_WRG
            real(F64), intent(out)                                       :: time_GRWRG
            real(F64), intent(out)                                       :: time_LogDet
            real(F64), intent(out)                                       :: time_Density
            real(F64), intent(out)                                       :: time_Cholesky
            
            integer :: u, s
            real(F64) :: Omega
            real(F64), dimension(:), allocatable :: time_Iter
            real(F64), dimension(:, :, :), allocatable :: CR
            real(F64), dimension(:, :), allocatable :: DCRG            
            real(F64), dimension(:, :), allocatable :: G

            real(F64), dimension(:), allocatable :: ACPoints, ACWeights
            integer :: QuadStart, QuadEnd
            integer :: DerivStart, DerivEnd
            integer, parameter :: NDerivPoints = 4
            integer :: Lambda1
            real(F64), parameter :: DerivStep = 0.001_F64
            real(F64), dimension(:, :), allocatable :: RWR
            integer, dimension(:, :, :), allocatable :: OccBounds, VirtBounds
            integer, dimension(:, :), allocatable :: OccSubsetDim, VirtSubsetDim
            integer :: MaxNai, Npq
            integer :: MaxNShellPairs
            integer :: MaxNCholesky
            type(tclock) :: timer_Cholesky, timer_MOTransf, timer_GRWRG            
            integer :: X, Y, SubsetIdx
            integer :: NImages, ThisImage
            
            NImages = num_images()
            ThisImage = this_image()
            Y = ThisImage
            allocate(OccBounds(2, 2, NImages))
            allocate(VirtBounds(2, 2, NImages))
            allocate(OccSubsetDim(2, NImages))
            allocate(VirtSubsetDim(2, NImages))
            call rpa_OVSubsets(OccBounds, VirtBounds, OccSubsetDim, VirtSubsetDim, &
                  NOcc, NVirt, NSpins)            
            MaxNai = 0
            do s = 1, NSpins
                  MaxNai = max(MaxNai, OccSubsetDim(s, Y)*VirtSubsetDim(s, Y))
            end do
            !
            ! Range-separation parameter for the range-separated
            ! Coulomb operator 1/r=erfc(omega*r)/r + erf(omega*r)/r
            !
            if (Kappa > ZERO) then
                  Omega = Sqrt(ONE/Kappa)
            else
                  Omega = huge(ONE)
            end if
            if (ComputeGrids) then
                  call rpa_OptimizeQuads(NFreqs, Freqs, FreqWeights, NLaplace, LaplaceX, LaplaceW, &
                        daiValues, daiWeights, TargetErrorFreq, TargetRelErrorFreq, TargetErrorLaplace, &
                        TargetRelErrorLaplace, GridLimitDai)
            else
                  call msg("Using frequency and Laplace transform quadratures from previous run")
                  call msg("Frequency integration will employ " // str(NFreqs) // " points")
            end if
            !
            ! Grid of Lambda points:
            ! * a subset of points specified by (QuadStart,QuadEnd) is used for numerical integration
            ! * Lambda=1, node used for the computation of RPA 1-RDM and T2 amplitudes at Lambda=1
            ! * a subset of points specified by (DerivStart,DerivEnd) is used for numercal differentiation
            ! The storage format:
            ! ACPoints = [(DerivStart ... DerivEnd)(Lambda=1)(QuadStart ... QuadEnd)]
            !
            if ((Ec1RDMApprox == RPA_Ec1RDM_NATURAL_REFERENCE .or. Ec1RDMApprox == RPA_EC1RDM_NONE) &
                  .and. (ExchangeApprox /= RPA_EXCHANGE_CUMULANT_LINEAR) .and. &
                  .not. (ChiOrbitals==RPA_ORBITALS_SEMICANONICAL .and. MeanField==RPA_MEAN_FIELD_KS_TYPE)) then
                  !
                  ! No analytic quadrature over Lambda
                  !
                  QuadStart = 0
                  QuadEnd = 0
            else
                  QuadStart = 1
                  QuadEnd = NACPoints
            end if                  
            DerivStart = 0
            DerivEnd = 0
            Lambda1 = 0
            if (T2Interp .or. Ec1RDMApprox == RPA_Ec1RDM_NATURAL_REFERENCE .or. &
                  (ExchangeApprox /= RPA_EXCHANGE_NONE .and. ExchangeApprox /= RPA_EXCHANGE_CUMULANT_LINEAR)) then
                  Lambda1 = 1
                  if (QuadStart > 0) then
                        QuadStart = QuadStart+1
                        QuadEnd = QuadEnd+1
                  end if
            end if
            if (ExchangeApprox == RPA_EXCHANGE_MBPT3_1_NUMERICAL) then
                  DerivStart = 1
                  DerivEnd = NDerivPoints
                  if (Lambda1 /= 0) Lambda1 = Lambda1 + NDerivPoints
                  if (QuadEnd /= 0) then
                        QuadStart = QuadStart + NDerivPoints
                        QuadEnd = QuadEnd + NDerivPoints
                  end if
            end if
            allocate(ACPoints(1:max(DerivEnd,Lambda1,QuadEnd)))
            allocate(ACWeights(1:max(DerivEnd,Lambda1,QuadEnd)))
            if (Lambda1 /= 0) then
                  ACPoints(Lambda1) = ONE
                  ACWeights(Lambda1) = ZERO
            end if
            if (DerivStart /= 0) then
                  !
                  ! Lambda grid for calculating numerical derivative with O(h**4) error
                  !
                  ACPoints(DerivStart+0)  = 1-2*DerivStep
                  ACWeights(DerivStart+0) = 1/(12*DerivStep)
                  !
                  ACPoints(DerivStart+1)  = 1-DerivStep
                  ACWeights(DerivStart+1) = -8/(12*DerivStep)
                  !
                  ACPoints(DerivStart+2)  = 1+DerivStep
                  ACWeights(DerivStart+2) = 8/(12*DerivStep)
                  !
                  ACPoints(DerivStart+3)  = 1+2*DerivStep
                  ACWeights(DerivStart+3) = -1/(12*DerivStep)
            end if
            if (QuadStart > 0) then
                  call quad_AdiabaticConnection(ACPoints(QuadStart:QuadEnd), ACWeights(QuadStart:QuadEnd), QuadEnd-QuadStart+1)
            end if
            time_W = ZERO
            time_WRG = ZERO
            time_GRWRG = ZERO
            time_LogDet = ZERO
            allocate(time_Iter(NFreqs))
            time_Iter = ZERO
            time_Density = ZERO
            time_Cholesky = ZERO
            if (ComputeRWRBasis) then
                  if (ComputeCholeskyBasis) then
                        ! 
                        ! Cholesky factorization of the Coulomb matrix
                        ! 
                        ! The Cholesky subroutine will factorize the V(pq|rs) matrix
                        ! and compute the list of nonnegligible shell pairs. In the subsequent steps,
                        ! the packed storage is used and, therefore, only the nonnegligible shell
                        ! pairs are manipulated and stored in memory.
                        !
                        MaxNShellPairs = (NShells * (NShells + 1)) / 2
                        allocate(ShellPairs(2, MaxNShellPairs))
                        allocate(ShellPairLoc(3, MaxNShellPairs))
                        allocate(ShellPairDim(MaxNShellPairs))
                        call clock_start(timer_Cholesky)
                        call chol_CoulombMatrix_A(Ry, NCholesky, ShellPairs, ShellPairLoc, ShellPairDim, NShellPairs, &
                              NOrbPairs, SubsetDim, SubsetBounds, NSubsets, &
                              CholeskyTauThresh, ShellCenters, &
                              AtomCoords, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, &
                              Exponents, NormFactors, Kappa, LmaxGTO, NAO, NShells, MaxNAOMult, SpherAO, MaxBlockDim)
                        time_Cholesky = time_Cholesky + clock_readwall(timer_Cholesky)
                  end if
                  MaxSubsetDim = maxval(SubsetDim)
                  MaxNCholesky = size(Ry, dim=1)
                  ! ----------------------------------------------------------------------------
                  !  Zeroth iteration: Establish the basis for the effective dielectric matrix
                  ! ----------------------------------------------------------------------------
                  ! Compute the dielectric matrix for the lowest frequency (RWR). Use RWR to get
                  ! the orthogonal random vectors basis G and the matrix product RG. RG computed
                  ! once for the lowest frequency will be used for all higher frequencies.
                  !
                  call blankline()
                  call msg("Starting AO->MO transform")
                  call clock_start(timer_MOTransf)
                  allocate(CR(NCholesky, MaxNai, NSpins))
                  call rpa_CRG(CR, Ry, OccCoeffs, VirtCoeffs, OccSubsetDim(:, Y), VirtSubsetDim(:, Y), &
                        OccBounds(:, :, Y), VirtBounds(:, :, Y), NSpins, &
                        ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
                        SubsetDim, NSubsets, NAngFunc, NAO, NCholesky)
                  time_WRG = time_WRG + clock_readwall(timer_MOTransf)
                  call msg("MO transform done in " // str(time_WRG,d=1) // " seconds" )
                  call blankline()
                  u = 1
                  call msg("Computation of RWR in full Cholesky basis")
                  call clock_start(timer_GRWRG)
                  allocate(RWR(NCholesky, NCholesky))
                  allocate(DCRG(NCholesky, MaxNai))
                  RWR = ZERO
                  call rpa_GPiUG_MO(RWR, CR, DCRG, OccEnergies, VirtEnergies, OccSubsetDim(:, Y), &
                        VirtSubsetDim(:, Y), OccBounds(:, :, Y), VirtBounds(:, :, Y), NSpins, Freqs(u))
                  call co_sum(RWR, result_image=1)
                  deallocate(DCRG)
                  deallocate(CR)
                  allocate(G(NCholesky, NCholesky))
                  call rpa_G(G, RWR, NVecsPiU, TargetErrorRandom, NCholesky, RWRBasisType, NAO)
                  allocate(RGy(NVecsPiU, MaxSubsetDim, NSubsets(1))[*])
                  do X = 1, NSubsets(1)
                        SubsetIdx = X + (Y - 1) * NSubsets(1)
                        Npq = SubsetDim(SubsetIdx)
                        if (Npq > 0) then
                              call real_aTb_x(RGy(:, :, X), NVecsPiU, G, NCholesky, Ry(:, :, X), &
                                    MaxNCholesky, NVecsPiU, Npq, NCholesky)
                        else
                              RGy(:, :, X) = ZERO
                        end if
                  end do
                  deallocate(Ry)        
                  ! ----------------------------------------------------------------------------
                  !    End of zeroth iteration: computation of the effective basis completed
                  ! ----------------------------------------------------------------------------
            end if
            call msg(str(NOrbPairs)//" orbital pairs are divided into "//str(NSubsets(1)*NSubsets(2))//" subsets")
            call msg("Max subset dimension: " // str(MaxSubsetDim))
            call msg("NSubsets per image: " // str(NSubsets(1)))

            call rpa_CC_Energy_2(Energy, RGy, ACPoints, ACWeights, QuadStart, QuadEnd, DerivStart, DerivEnd, Lambda1, NVecsPiU, &
                  Freqs, FreqWeights, NFreqs, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, &
                  NOcc, NVirt, OccSubsetDim, VirtSubsetDim, OccBounds, VirtBounds, &
                  ShellPairs, ShellPairLoc, ShellPairDim, ShellLoc, ShellParamsIdx, SubsetBounds, &
                  SubsetDim, NSubsets, NAngFunc, NAO, MaxNai, T1Approx, ExchangeApprox, Ec1RDMApprox, &
                  DensityApprox, Purify1RDM, ceiling(GuessNVecsT2*NVecsPiU), &
                  SmallEigenvalsCutoffT2, ChiOrbitals, MeanField, T2Interp, MeanFieldApprox, &
                  MaxBatchDimT2, AOBasis, F_ao)
      end subroutine rpa_CC_Energy_1


      subroutine rpa_Ecorr_2(Energy, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, &
            NOcc, NVirt, AOBasis, RPAParams, RPAGrids, RPABasisVecs, RPABasis, &
            CholeskyVecs, CholeskyBasis, F_ao)
            
            real(F64), dimension(:), intent(inout)                    :: Energy
            real(F64), dimension(:, :, :), intent(in)                 :: OccCoeffs
            real(F64), dimension(:, :, :), intent(in)                 :: VirtCoeffs
            real(F64), dimension(:, :), intent(in)                    :: OccEnergies
            real(F64), dimension(:, :), intent(in)                    :: VirtEnergies
            integer, dimension(:), intent(in)                         :: NOcc
            integer, dimension(:), intent(in)                         :: NVirt
            type(TAOBasis), intent(in)                                :: AOBasis
            type(TRPAParams), intent(in)                              :: RPAParams
            type(TRPAGrids), intent(inout)                            :: RPAGrids
            real(F64), dimension(:, :, :), allocatable, intent(inout) :: RPABasisVecs[:]
            type(TRPABasis), intent(inout)                            :: RPABasis
            real(F64), dimension(:, :, :), allocatable, intent(inout) :: CholeskyVecs[:]
            type(TChol2Vecs), intent(inout)                           :: CholeskyBasis
            real(F64), dimension(:, :, :), intent(in)                 :: F_ao
            
            associate ( &
                  AtomCoords => AOBasis%AtomCoords, &
                  LmaxGTO => AOBasis%LmaxGTO, &
                  ShellLocCart => AOBasis%ShellLocCart, &
                  ShellLocSpher => AOBasis%ShellLocSpher, &
                  ShellCenters => AOBasis%ShellCenters, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  NAngFuncCart => AOBasis%NAngFuncCart, &
                  NAngFuncSpher => AOBasis%NAngFuncSpher, &
                  NPrimitives => AOBasis%NPrimitives, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NormFactorsCart => AOBasis%NormFactorsCart, &
                  NormFactorsSpher => AOBasis%NormFactorsSpher, &
                  SpherAO => AOBasis%SpherAO, &
                  NShellParams => AOBasis%NShellParams, &
                  NShells => AOBasis%NShells, &
                  NAOCart => AOBasis%NAOCart, &
                  NAOSpher => AOBasis%NAOSpher)
                  if (SpherAO) then
                        call rpa_Ecorr_1(Energy, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, &
                              NOcc, NVirt, AtomCoords, LmaxGTO, ShellLocSpher, ShellCenters, ShellParamsIdx, ShellMomentum, &
                              NAngFuncSpher, NPrimitives, CntrCoeffs, Exponents, NormFactorsSpher, SpherAO, AOBasis, &
                              RPAParams, RPAGrids, RPABasisVecs, RPABasis, CholeskyVecs, CholeskyBasis, F_ao)
                  else
                        call rpa_Ecorr_1(Energy, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, &
                              NOcc, NVirt, AtomCoords, LmaxGTO, ShellLocCart, ShellCenters, ShellParamsIdx, ShellMomentum, &
                              NAngFuncCart, NPrimitives, CntrCoeffs, Exponents, NormFactorsCart, SpherAO, AOBasis, &
                              RPAParams, RPAGrids, RPABasisVecs, RPABasis, CholeskyVecs, CholeskyBasis, F_ao)
                  end if
            end associate
      end subroutine rpa_Ecorr_2
      

      subroutine rpa_Ecorr_1(Energy, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, NOcc, NVirt, AtomCoords, &
            LmaxGTO, ShellLoc, ShellCenters, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, &
            Exponents, NormFactors, SpherAO, AOBasis, RPAParams, RPAGrids, RPABasisVecs, RPABasis, CholeskyVecs, &
            CholeskyBasis, F_ao)

            real(F64), dimension(:), intent(inout)                    :: Energy
            real(F64), dimension(:, :, :), intent(in)                 :: OccCoeffs
            real(F64), dimension(:, :, :), intent(in)                 :: VirtCoeffs
            real(F64), dimension(:, :), intent(in)                    :: OccEnergies
            real(F64), dimension(:, :), intent(in)                    :: VirtEnergies
            integer, dimension(:), intent(in)                         :: NOcc
            integer, dimension(:), intent(in)                         :: NVirt
            real(F64), dimension(:, :), intent(in)                    :: AtomCoords
            integer, intent(in)                                       :: LmaxGTO
            integer, dimension(:), intent(in)                         :: ShellLoc
            integer, dimension(:), intent(in)                         :: ShellCenters
            integer, dimension(:), intent(in)                         :: ShellParamsIdx
            integer, dimension(:), intent(in)                         :: ShellMomentum
            integer, dimension(:), intent(in)                         :: NAngFunc
            integer, dimension(:), intent(in)                         :: NPrimitives
            real(F64), dimension(:, :), intent(in)                    :: CntrCoeffs
            real(F64), dimension(:, :), intent(in)                    :: Exponents
            real(F64), dimension(:, :), intent(in)                    :: NormFactors
            logical, intent(in)                                       :: SpherAO
            type(TAOBasis), intent(in)                                :: AOBasis
            type(TRPAParams), intent(in)                              :: RPAParams
            type(TRPAGrids), intent(inout)                            :: RPAGrids
            real(F64), dimension(:, :, :), allocatable, intent(inout) :: RPABasisVecs[:]
            type(TRPABasis), intent(inout)                            :: RPABasis
            real(F64), dimension(:, :, :), allocatable, intent(inout) :: CholeskyVecs[:]
            type(TChol2Vecs), intent(inout)                           :: CholeskyBasis
            real(F64), dimension(:, :, :), intent(in)                 :: F_ao
            
            integer :: NSpins, NAtoms, NAO, NShells
            integer :: MaxNOcc, MaxNVirt
            integer, dimension(2) :: NCore
            integer :: s
            integer, dimension(2) :: NOccAct
            real(F64) :: Omega
            real(F64) :: Ef
            real(F64), dimension(:, :), allocatable :: Ei, Ea
            real(F64), dimension(:, :, :), allocatable :: OccActCoeffs, VirtActCoeffs
            type(tclock) :: timer
            real(F64) :: time_W, time_WRG, time_GRWRG, time_LogDet, time_Density, time_Cholesky
            real(F64) :: CoreOrbThresh
            logical :: ComputeGrids, ComputeRWRBasis
            real(F64) :: EcRPA

            call blankline()
            call msg("Direct RPA Correlation Energy", underline=.true.)
            call clock_start(timer)
            
            NSpins = size(OccEnergies, dim=2)
            NAtom = size(AtomCoords, dim=2)
            NAO = size(OccCoeffs, dim=1)
            NShells = size(ShellCenters)
            CoreOrbThresh = RPAParams%CoreOrbThresh
            if (RPAParams%Kappa > ZERO) then
                  Omega = Sqrt(ONE/RPAParams%Kappa)
                  call msg("Long-range interaction Erf(Omega r12)/r12 with Omega = " // str(Omega, d=4))
            else
                  call msg("Applying full-range Coulomb potential")
            end if
            call msg("Number of atoms (including ghost centers): " // str(NAtom))
            call msg("Atomic orbitals: " // str(NAO))
            if (SpherAO) then
                  call msg("Using spherical AOs") 
            else
                  call msg("Using Cartesian AOs")
            end if
            call msg("Threshold for inert core orbitals (a.u.): " // str(CoreOrbThresh,d=3))
            NOccAct = 0
            do s = 1, NSpins
                  if (NSpins > 1) then
                        if (s == 1) call msg("Alpha spin", underline=.true.)
                        if (s == 2) call msg("Beta spin", underline=.true.)
                  end if
                  call rpa_NCore(NCore(s), OccEnergies(:, s), NOcc(s), CoreOrbThresh)
                  NOccAct(s) = NOcc(s) - NCore(s)
                  call msg("Occupied orbitals: " // str(NOcc(s)))
                  call msg("Discarded " // str(NCore(s)) // " core orbitals")
                  call msg("Active occupied orbitals: " // str(NOccAct(s)))
                  call msg("Virtual orbitals: " // str(NVirt(s)))
            end do
            MaxNOcc = maxval(NOccAct(1:NSpins))
            MaxNVirt = maxval(NVirt(1:NSpins))
            allocate(Ei(MaxNOcc, NSpins))
            allocate(OccActCoeffs(NAO, MaxNOcc, NSpins))
            allocate(VirtActCoeffs(NAO, MaxNVirt, NSpins))
            allocate(Ea(MaxNVirt, NSpins))
            do s = 1, NSpins
                  if (NOccAct(s) > 0) then
                        Ef = (VirtEnergies(1, s)+OccEnergies(NOcc(s), s)) / TWO
                        Ei(1:NOccAct(s), s) = OccEnergies(NCore(s)+1:NCore(s)+NOccAct(s), s) - Ef
                        Ea(1:NVirt(s), s) = VirtEnergies(1:NVirt(s), s) - Ef
                        OccActCoeffs(:, 1:NOccAct(s), s) = OccCoeffs(:, NCore(s)+1:NCore(s)+NOccAct(s), s)
                        VirtActCoeffs(:, 1:NVirt(s), s) = VirtCoeffs(:, 1:NVirt(s), s)
                  else
                        Ei(:, s) = ZERO
                        Ea(:, s) = ZERO
                        OccActCoeffs(:, :, s) = ZERO
                        VirtActCoeffs(:, :, s) = ZERO
                  end if
            end do
            ComputeGrids = RPAGrids%ComputeGrids
            ComputeRWRBasis = RPABasis%ComputeRWRBasis
            !
            ! Call the main subroutine which integrates the RPA correlation energy over the frequency grid
            !
            if (RPAParams%CoupledClusters) then
                  call rpa_CC_Energy_1(Energy, OccActCoeffs, VirtActCoeffs, Ei, Ea, ShellCenters, &
                        AtomCoords, LmaxGTO, ShellLoc, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, &
                        Exponents, NormFactors, NAO, NShells, NAtoms, NOccAct, NVirt, NSpins, SpherAO, AOBasis, &
                        
                        RPAParams%Kappa, &
                        RPAParams%TargetErrorLaplace, &
                        RPAParams%TargetRelErrorLaplace, &
                        RPAParams%CholeskyTauThresh, &
                        RPAParams%TargetErrorRandom, &                  
                        RPAParams%TargetErrorFreq, &
                        RPAParams%TargetRelErrorFreq, &
                        RPAParams%MaxNAOMult, &
                        RPAParams%RWRBasisType, &
                        RPAParams%MaxBlockDim, &
                        
                        ComputeGrids, &
                        RPAParams%GridLimitDai, &
                        RPAGrids%NFreqs, &
                        RPAGrids%Freqs, &
                        RPAGrids%FreqWeights, &
                        RPAGrids%NLaplace, &
                        RPAGrids%LaplaceX, &
                        RPAGrids%LaplaceW, &
                        RPAGrids%daiValues, &
                        RPAGrids%daiWeights, &
                        
                        ComputeRWRBasis, &
                        RPAParams%ComputeCholeskyBasis, &
                        RPABasisVecs, &
                        CholeskyVecs, &
                        RPABasis%NVecs, &                        
                        CholeskyBasis%NVecs, &
                        CholeskyBasis%MaxSubsetDim, &
                        CholeskyBasis%NSubsets, &
                        CholeskyBasis%NOrbPairs, &
                        CholeskyBasis%NShellPairs, &
                        CholeskyBasis%ShellPairs, &
                        CholeskyBasis%ShellPairLoc, &
                        CholeskyBasis%ShellPairDim, &
                        CholeskyBasis%SubsetDim, &
                        CholeskyBasis%SubsetBounds, &
                        
                        RPAParams%SmallEigenvalCutoffT2, &
                        F_ao, &
                        RPAParams%ACQuadPoints, &
                        RPAParams%T1Approx, &
                        RPAParams%ExchangeApprox, &
                        RPAParams%Ec1RDMApprox, &
                        RPAParams%DensityApprox, &
                        RPAParams%Purify1RDM, &
                        RPAParams%GuessNVecsT2, &
                        RPAParams%ChiOrbitals, &
                        RPAParams%MeanField, &
                        RPAParams%T2Interp, &
                        RPAParams%MeanFieldApprox, &
                        RPAParams%MaxBatchDimT2, &
                        
                        time_W, time_WRG, time_GRWRG, time_LogDet, time_Density, time_Cholesky)
            else
                  if (RPAParams%MOAlgorithm) then                  
                        call rpa_FreqIntegral_MO(Energy, OccActCoeffs, VirtActCoeffs, Ei, Ea, ShellCenters, &
                              AtomCoords, LmaxGTO, ShellLoc, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, &
                              Exponents, NormFactors, NAO, NShells, NAtoms, NOccAct, NVirt, NSpins, SpherAO, &
                              
                              RPAParams%Kappa, &
                              RPAParams%TargetErrorLaplace, &
                              RPAParams%TargetRelErrorLaplace, &
                              RPAParams%CholeskyTauThresh, &
                              RPAParams%TargetErrorRandom, &                  
                              RPAParams%TargetErrorFreq, &
                              RPAParams%TargetRelErrorFreq, &
                              RPAParams%MaxNAOMult, &
                              RPAParams%RWRBasisType, &
                              RPAParams%MaxBlockDim, &
                              
                              ComputeGrids, &
                              RPAParams%GridLimitDai, &
                              RPAGrids%NFreqs, &
                              RPAGrids%Freqs, &
                              RPAGrids%FreqWeights, &
                              RPAGrids%NLaplace, &
                              RPAGrids%LaplaceX, &
                              RPAGrids%LaplaceW, &
                              RPAGrids%daiValues, &
                              RPAGrids%daiWeights, &
                              
                              ComputeRWRBasis, &
                              RPAParams%ComputeCholeskyBasis, &
                              RPABasisVecs, &
                              CholeskyVecs, &
                              RPABasis%NVecs, &                        
                              CholeskyBasis%NVecs, &
                              CholeskyBasis%MaxSubsetDim, &
                              CholeskyBasis%NSubsets, &
                              CholeskyBasis%NOrbPairs, &
                              CholeskyBasis%NShellPairs, &
                              CholeskyBasis%ShellPairs, &
                              CholeskyBasis%ShellPairLoc, &
                              CholeskyBasis%ShellPairDim, &
                              CholeskyBasis%SubsetDim, &
                              CholeskyBasis%SubsetBounds, &                              
                              
                              time_W, time_WRG, time_GRWRG, time_LogDet, time_Density, time_Cholesky)
                  else
                        call rpa_FreqIntegral_AO(EcRPA, OccActCoeffs, VirtActCoeffs, Ei, Ea, ShellCenters, &
                              AtomCoords, LmaxGTO, ShellLoc, ShellParamsIdx, ShellMomentum, NAngFunc, NPrimitives, CntrCoeffs, &
                              Exponents, NormFactors, NAO, NShells, NAtoms, NOccAct, NVirt, NSpins, SpherAO, &
                              
                              RPAParams%Kappa, &
                              RPAParams%TargetErrorLaplace, &
                              RPAParams%TargetRelErrorLaplace, &
                              RPAParams%CholeskyTauThresh, &
                              RPAParams%TargetErrorRandom, &                  
                              RPAParams%TargetErrorFreq, &
                              RPAParams%TargetRelErrorFreq, &
                              RPAParams%MaxNAOMult, &
                              RPAParams%RWRBasisType, &
                              RPAParams%MaxBlockDim, &
                              
                              ComputeGrids, &
                              RPAParams%GridLimitDai, &
                              RPAGrids%NFreqs, &
                              RPAGrids%Freqs, &
                              RPAGrids%FreqWeights, &
                              RPAGrids%NLaplace, &
                              RPAGrids%LaplaceX, &
                              RPAGrids%LaplaceW, &
                              RPAGrids%daiValues, &
                              RPAGrids%daiWeights, &
                              
                              ComputeRWRBasis, &
                              RPAParams%ComputeCholeskyBasis, &
                              RPABasisVecs, &
                              CholeskyVecs, &
                              RPABasis%NVecs, &
                              CholeskyBasis%NVecs, &
                              CholeskyBasis%MaxSubsetDim, &
                              CholeskyBasis%NSubsets, &
                              CholeskyBasis%NOrbPairs, &
                              CholeskyBasis%NShellPairs, &
                              CholeskyBasis%ShellPairs, &
                              CholeskyBasis%ShellPairLoc, &
                              CholeskyBasis%ShellPairDim, &
                              CholeskyBasis%SubsetDim, &
                              CholeskyBasis%SubsetBounds, &
                              
                              time_W, time_WRG, time_GRWRG, time_LogDet, time_Density, time_Cholesky)

                        Energy(RPA_ENERGY_CORR) = EcRPA
                  end if
            end if
            !
            ! Prevent recomputation of the R*G matrix and the frequency/Laplace grids
            ! in subsequent runs. The matrix R*G and the grids should be kept the same
            ! for the combined system and for its subsystems to retain size-extensive energy.
            !
            RPAGrids%ComputeGrids = .false.
            RPABasis%ComputeRWRBasis = .false.
            call msg("Total time for RPA correlation: " // str(clock_readwall(timer), d=1) // " seconds")
            call msg("Detailed timings in seconds")
            if (RPAParams%MOAlgorithm) then
                  call msg("Cholesky        " // str(time_Cholesky,d=1))
                  call msg("C*RG            " // str(time_WRG,d=1))
                  call msg("GRC*D*CRG       " // str(time_GRWRG,d=1))
                  call msg("Det(Log(GRWRG)) " // str(time_LogDet,d=1))
            else
                  call msg("Cholesky        " // str(time_Cholesky,d=1))
                  call msg("W               " // str(time_W,d=1))
                  call msg("W*RG            " // str(time_WRG,d=1))
                  call msg("GR*WRG          " // str(time_GRWRG,d=1))
                  call msg("Rho(u)          " // str(time_Density,d=1))
                  call msg("Det(Log(GRWRG)) " // str(time_LogDet,d=1))
            end if
            call blankline()
      end subroutine rpa_Ecorr_1


      subroutine rpa_THC_Ecorr_2(Energy, OccCoeffs, VirtCoeffs, OccEnergies, VirtEnergies, &
            hHF_ao, NOcc, NVirt, AOBasis, RPAParams, RPAGrids, THCGrid)

            real(F64), dimension(:), intent(inout)                    :: Energy
            real(F64), dimension(:, :, :), intent(in)                 :: OccCoeffs
            real(F64), dimension(:, :, :), intent(in)                 :: VirtCoeffs
            real(F64), dimension(:, :), intent(in)                    :: OccEnergies
            real(F64), dimension(:, :), intent(in)                    :: VirtEnergies
            real(F64), dimension(:, :, :), intent(in)                 :: hHF_ao
            integer, dimension(:), intent(in)                         :: NOcc
            integer, dimension(:), intent(in)                         :: NVirt            
            type(TAOBasis), intent(in)                                :: AOBasis
            type(TRPAParams), intent(in)                              :: RPAParams
            type(TRPAGrids), intent(inout)                            :: RPAGrids
            type(TCoulTHCGrid), intent(inout)                         :: THCGrid
            
            integer :: NSpins, NAO
            integer :: MaxNOcc, MaxNVirt
            integer, dimension(2) :: NCore
            integer :: s
            integer, dimension(2) :: NOccAct
            real(F64) :: Ef
            real(F64), dimension(:, :), allocatable :: Ei, Ea
            real(F64), dimension(:, :, :), allocatable :: OccActCoeffs, VirtActCoeffs
            type(tclock) :: timer
            real(F64) :: CoreOrbThresh
            logical :: ComputeGrids, ComputePiUVecs

            call clock_start(timer)
            call blankline()
            call msg("Orbital basis for the RPA+MBPT3 calculation", underline=.true.)            
            NSpins = size(OccEnergies, dim=2)
            NAO = size(OccCoeffs, dim=1)
            CoreOrbThresh = RPAParams%CoreOrbThresh
            if (AOBasis%SpherAO) then
                  call msg("Atomic orbitals: " // str(NAO) // " spherical")
            else
                  call msg("Atomic orbitals: " // str(NAO) // " Cartesian")
            end if
            call msg("Threshold for inert core orbitals (a.u.): " // str(CoreOrbThresh,d=3))
            NOccAct = 0
            do s = 1, NSpins
                  if (NSpins > 1) then
                        if (s == 1) call msg("Alpha spin", underline=.true.)
                        if (s == 2) call msg("Beta spin", underline=.true.)
                  end if
                  call rpa_NCore(NCore(s), OccEnergies(:, s), NOcc(s), CoreOrbThresh)
                  NOccAct(s) = NOcc(s) - NCore(s)
                  call msg("Occupied orbitals: " // str(NOcc(s)))
                  call msg("Discarded " // str(NCore(s)) // " core orbitals")
                  call msg("Active occupied orbitals: " // str(NOccAct(s)))
                  call msg("Virtual orbitals: " // str(NVirt(s)))
            end do
            MaxNOcc = maxval(NOccAct(1:NSpins))
            MaxNVirt = maxval(NVirt(1:NSpins))
            allocate(Ei(MaxNOcc, NSpins))
            allocate(OccActCoeffs(NAO, MaxNOcc, NSpins))
            allocate(VirtActCoeffs(NAO, MaxNVirt, NSpins))
            allocate(Ea(MaxNVirt, NSpins))
            do s = 1, NSpins
                  if (NOccAct(s) > 0) then
                        Ef = (VirtEnergies(1, s)+OccEnergies(NOcc(s), s)) / TWO
                        Ei(1:NOccAct(s), s) = OccEnergies(NCore(s)+1:NCore(s)+NOccAct(s), s) - Ef
                        Ea(1:NVirt(s), s) = VirtEnergies(1:NVirt(s), s) - Ef
                        OccActCoeffs(:, 1:NOccAct(s), s) = OccCoeffs(:, NCore(s)+1:NCore(s)+NOccAct(s), s)
                        VirtActCoeffs(:, 1:NVirt(s), s) = VirtCoeffs(:, 1:NVirt(s), s)
                  else
                        Ei(:, s) = ZERO
                        Ea(:, s) = ZERO
                        OccActCoeffs(:, :, s) = ZERO
                        VirtActCoeffs(:, :, s) = ZERO
                  end if
            end do
            ComputeGrids = RPAGrids%ComputeGrids
            ComputePiUVecs = (.not. allocated(THCGrid%ZgkPiU))
            !
            ! Main subroutine for correlation energy:
            ! 1. ring approximation of the correlation energy
            ! 2. MBPT3 corrections
            !
            call rpa_THC_Ecorr_1(Energy, OccActCoeffs, VirtActCoeffs, Ei, Ea, &
                  hHF_ao, NOccAct, NVirt, NSpins, &
                  
                  RPAParams%TargetErrorLaplace, &
                  RPAParams%TargetRelErrorLaplace, &
                  RPAParams%TargetErrorRandom, &                  
                  RPAParams%TargetErrorFreq, &
                  RPAParams%TargetRelErrorFreq, &
                  RPAParams%RWRBasisType, &
                  RPAParams%CumulantApprox, &
                  
                  ComputeGrids, &
                  RPAParams%GridLimitDai, &
                  RPAGrids%NFreqs, &
                  RPAGrids%Freqs, &
                  RPAGrids%FreqWeights, &
                  RPAGrids%NLaplace, &
                  RPAGrids%LaplaceX, &
                  RPAGrids%LaplaceW, &
                  RPAGrids%daiValues, &
                  RPAGrids%daiWeights, &
                  
                  ComputePiUVecs, &
                  
                  THCGrid%Xgp, &
                  THCGrid%Zgk, &
                  THCGrid%ZgkPiU, &
                  RPAParams%THC_BlockDim, &
                  RPAParams%THC_QRThresh_T2, &
                  
                  RPAParams%SmallEigenvalCutoffT2, &
                  RPAParams%GuessNVecsT2, &
                  RPAParams%MaxBatchDimT2, &

                  RPAParams%T2EigenvalueThresh, &
                  RPAParams%T2CouplingStrength, &
                  RPAParams%PT_Order2, &
                  RPAParams%PT_Order3)
            !
            ! Prevent recomputation of quadrature grids. The frequency grid is computed only
            ! once for the combined system and shared between its subsystems to retain
            ! size-extensive energy.
            !
            RPAGrids%ComputeGrids = .false.
            call msg("Total time for ring+MBPT3 correlation: " // str(clock_readwall(timer), d=1) // " seconds")
            call blankline()
      end subroutine rpa_THC_Ecorr_2

      
      subroutine rpa_THC_Ecorr_1(Energy, OccCoeffs_ao, VirtCoeffs_ao, OccEnergies, VirtEnergies, &
            hHF_ao, NOcc, NVirt, NSpins, &

            TargetErrorLaplace, TargetRelErrorLaplace, TargetErrorRandom, TargetErrorFreq, &
            TargetRelErrorFreq, RWRBasisType, CumulantApprox, &
            
            ComputeGrids, GridLimitDai, NFreqs, Freqs, FreqWeights, NLaplace, LaplaceX, LaplaceW, &
            daiValues, daiWeights, &
            
            ComputePiUVecs, &

            THC_Xgp, THC_Zgk, THC_ZgkPiU, THC_BlockDim, THC_QRThresh_T2, &

            SmallEigenvalsCutoffT2, GuessNVecsT2, MaxBatchDimT2, &
            T2EigenvalueThresh, T2CouplingStrength, PT_Order2, PT_Order3)

            real(F64), dimension(:), intent(inout)                       :: Energy
            real(F64), dimension(:, :, :), intent(in)                    :: OccCoeffs_ao
            real(F64), dimension(:, :, :), intent(in)                    :: VirtCoeffs_ao
            real(F64), dimension(:, :), intent(in)                       :: OccEnergies
            real(F64), dimension(:, :), intent(in)                       :: VirtEnergies
            real(F64), dimension(:, :, :), intent(in)                    :: hHF_ao
            integer, dimension(:), intent(in)                            :: NOcc
            integer, dimension(:), intent(in)                            :: NVirt
            integer, intent(in)                                          :: NSpins
            
            real(F64), intent(in)                                        :: TargetErrorLaplace
            real(F64), intent(in)                                        :: TargetRelErrorLaplace            
            real(F64), intent(in)                                        :: TargetErrorRandom
            real(F64), intent(in)                                        :: TargetErrorFreq
            real(F64), intent(in)                                        :: TargetRelErrorFreq
            integer, intent(in)                                          :: RWRBasisType
            integer, intent(in)                                          :: CumulantApprox

            logical, intent(in)                                          :: ComputeGrids
            logical, intent(in)                                          :: GridLimitDai
            integer, intent(inout)                                       :: NFreqs
            real(F64), dimension(:), allocatable, intent(inout)          :: Freqs
            real(F64), dimension(:), allocatable, intent(inout)          :: FreqWeights
            integer, dimension(:), allocatable, intent(inout)            :: NLaplace
            real(F64), dimension(:, :), allocatable, intent(inout)       :: LaplaceX
            real(F64), dimension(:, :), allocatable, intent(inout)       :: LaplaceW
            real(F64), dimension(:, :), intent(in)                       :: daiValues
            real(F64), dimension(:, :), intent(in)                       :: daiWeights

            logical, intent(in)                                          :: ComputePiUVecs

            real(F64), dimension(:, :), intent(in)                       :: THC_Xgp
            real(F64), dimension(:, :), intent(in)                       :: THC_Zgk
            real(F64), dimension(:, :), allocatable, intent(inout)       :: THC_ZgkPiU
            integer, intent(in)                                          :: THC_BlockDim
            real(F64), intent(in)                                        :: THC_QRThresh_T2

            real(F64), intent(in)                                        :: SmallEigenvalsCutoffT2
            real(F64), intent(in)                                        :: GuessNVecsT2
            integer, intent(in)                                          :: MaxBatchDimT2

            real(F64), intent(in)                                        :: T2EigenvalueThresh
            real(F64), intent(in)                                        :: T2CouplingStrength
            logical, intent(in)                                          :: PT_Order2
            logical, intent(in)                                          :: PT_Order3

            integer :: NAO, NCholesky, NVecsPiU, MaxNai, s
            integer :: THC_NGrid
            real(F64), dimension(:, :, :), allocatable :: THC_Xga, THC_Xgi
            real(F64), dimension(:, :, :), allocatable :: Rkai, PiU
            real(F64), dimension(:, :), allocatable :: hHFai
            real(F64), dimension(:, :), allocatable :: G
            real(F64), dimension(1) :: LowestFreq
            
            if (ComputeGrids) then
                  call rpa_OptimizeQuads(NFreqs, Freqs, FreqWeights, NLaplace, LaplaceX, LaplaceW, &
                        daiValues, daiWeights, TargetErrorFreq, TargetRelErrorFreq, TargetErrorLaplace, &
                        TargetRelErrorLaplace, GridLimitDai)
            else
                  call msg("Using frequency and Laplace transform quadratures from previous run")
                  call msg("Frequency integration will employ " // str(NFreqs) // " points")
            end if
            NCholesky = size(THC_Zgk, dim=2)
            NAO = size(THC_Xgp, dim=2)
            THC_NGrid = size(THC_Zgk, dim=1)
            allocate(THC_Xga(THC_NGrid, max(NVirt(1), NVirt(2)), NSpins))
            allocate(THC_Xgi(THC_NGrid, max(NOcc(1), NOcc(2)), NSpins))
            call rpa_THC_MOTransf(THC_Xga, THC_Xgi, THC_Xgp, OccCoeffs_ao, VirtCoeffs_ao, NOcc, NVirt)
            if (ComputePiUVecs) then
                  !
                  ! Generate basis for the Pi(u) matrix. In all tests NVecsPiU was just
                  ! a fraction of NCholesky. The Pi(u) basis is computed only once
                  ! for the full molecule (full cluster). All subsystems share
                  ! the same basis to guarantee size consistency.
                  !
                  allocate(PiU(NCholesky, NCholesky, 1))
                  allocate(Rkai(NCholesky, THC_BlockDim, 1))
                  LowestFreq = Freqs(1)
                  call rpa_THC_PiU(PiU, Rkai, THC_Xga, THC_Xgi, THC_Zgk, NOcc, NVirt, &
                        LowestFreq, OccEnergies, VirtEnergies, THC_BlockDim, .false.)
                  allocate(G(NCholesky, NCholesky))
                  call rpa_G(G, PiU(:, :, 1), NVecsPiU, TargetErrorRandom, NCholesky, RWRBasisType, NAO)
                  allocate(THC_ZgkPiU(THC_NGrid, NVecsPiU))
                  call real_ab(THC_ZgkPiU, THC_Zgk, G(:, 1:NVecsPiU))
                  deallocate(G, PiU, Rkai)
            end if
            NVecsPiU = size(THC_ZgkPiU, dim=2)
            if (CumulantApprox >= RPA_CUMULANT_LEVEL_2_HALF_THC) then
                  MaxNai = max(NVirt(1)*NOcc(1), NVirt(2)*NOcc(2))
                  allocate(hHFai(MaxNai, NSpins))
                  do s = 1, NSpins
                        call rpa_MeanField_hHFai(hHFai(:, s), hHF_ao(:, :, s), OccCoeffs_ao(:, :, s), &
                              VirtCoeffs_ao(:, :, s), NOcc(s), NVirt(s), NAO)
                  end do
            else
                  allocate(hHFai(0, 0))
            end if
            call rpa_THC_MBPT3(Energy, THC_Xgp, THC_Xga, THC_Xgi, THC_Zgk, THC_ZgkPiU, &
                  THC_BlockDim, THC_QRThresh_T2, hHFai, &
                  Freqs, FreqWeights, NFreqs, OccEnergies, VirtEnergies, &
                  NOcc, NVirt, ceiling(GuessNVecsT2*NVecsPiU), &
                  SmallEigenvalsCutoffT2, MaxBatchDimT2, CumulantApprox, &
                  T2EigenvalueThresh, T2CouplingStrength, &
                  PT_Order2, PT_Order3)
            Energy(RPA_ENERGY_CORR) = sum(Energy(RPA_CORRELATION_TERMS(1):RPA_CORRELATION_TERMS(2)))
      end subroutine rpa_THC_Ecorr_1
end module rpa
