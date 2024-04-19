module rpa_CC_Doubles
      use arithmetic
      use math_constants
      use rpa_definitions
      use real_linalg

      implicit none

contains

      subroutine rpa_CC_Doubles_Summary(SmallEigenvalsCutoffT2, GuessNVecsT2, T2Interp)
            real(F64), intent(in) :: SmallEigenvalsCutoffT2
            integer, intent(in)   :: GuessNVecsT2
            logical, intent(in)   :: T2Interp

            call msg("T2 amplitudes")
            call msg(lfield("", 30) // "direct ring approximation")
            call msg(lfield("", 30) // "cutoff for small eigenvalues of the Gram matrix: " // str(SmallEigenvalsCutoffT2,d=1))
            call msg(lfield("", 30) // "diagonalization with " // str(GuessNVecsT2) // " random guess vectors")
            if (T2Interp) then
                  call msg(lfield("", 30) // "T2 is computed only for Lambda=1")
                  call msg(lfield("", 30) // "T2 at intermediate points is interpolated as T2(Lambda)=Lambda*T2(Lambda=1)")
            else
                  call msg(lfield("", 30) // "T2 is computed at each node of the quadrature")
            end if
      end subroutine rpa_CC_Doubles_Summary


      subroutine rpa_CC_T2_SmoothStepFunction(Alpha, Lambda, Ka, Kb)
            real(F64), intent(out) :: Alpha
            real(F64), intent(in)  :: Lambda
            real(F64), intent(in)  :: Ka
            real(F64), intent(in)  :: Kb
            
            real(F64) :: mu, z
            
            mu = (2 * Lambda - (Ka + Kb)) / (Kb - Ka)
            z = (35 * mu - 35 * mu**3 + 21 * mu**5 - 5 * mu**7) / 16
            Alpha = ONE/TWO * (ONE - z)
      end subroutine rpa_CC_T2_SmoothStepFunction
      
      
      subroutine rpa_CC_T2(A, V, NVecsT2, PiUEigenvecs, PiUEigenvals, Rkai, NVecsChol, NOcc, NVirt, OccBounds, VirtBounds, &
            Freqs, FreqWeights, NFreqs, Lambda, OccEnergies, VirtEnergies, SmallEigenvalsCutoffT2, GuessNVecsT2, MaxBatchDim)
            
            real(F64), dimension(:), allocatable, intent(out)    :: A
            real(F64), dimension(:, :), allocatable, intent(out) :: V
            integer,intent(inout)                                :: NVecsT2
            real(F64), dimension(:, :, :), intent(in)            :: PiUEigenvecs
            real(F64), dimension(:, :), intent(in)               :: PiUEigenvals
            real(F64), dimension(:, :), intent(in)               :: Rkai
            integer, intent(in)                                  :: NVecsChol
            integer, intent(in)                                  :: NOcc
            integer, intent(in)                                  :: NVirt
            integer, dimension(2), intent(in)                    :: OccBounds
            integer, dimension(2), intent(in)                    :: VirtBounds
            real(F64), dimension(:), intent(in)                  :: Freqs
            real(F64), dimension(:), intent(in)                  :: FreqWeights
            integer, intent(in)                                  :: NFreqs
            real(F64), intent(in)                                :: Lambda
            real(F64), dimension(:), intent(in)                  :: OccEnergies
            real(F64), dimension(:), intent(in)                  :: VirtEnergies
            real(F64), intent(in)                                :: SmallEigenvalsCutoffT2
            integer, intent(in)                                  :: GuessNVecsT2
            integer, intent(in)                                  :: MaxBatchDim

            logical, parameter :: MemorySaving = .true.

            if (MemorySaving) then
                  call rpa_CC_T2_v5(A, V, NVecsT2, PiUEigenvecs, PiUEigenvals, Rkai, NVecsChol, NOcc, NVirt, &
                        Freqs, FreqWeights, NFreqs, Lambda, OccEnergies, VirtEnergies, SmallEigenvalsCutoffT2, GuessNVecsT2, &
                        MaxBatchDim)
            else
                  call rpa_CC_T2_v1(A, V, NVecsT2, PiUEigenvecs, PiUEigenvals, Rkai, NVecsChol, NOcc, NVirt, OccBounds, VirtBounds, &
                        Freqs, FreqWeights, NFreqs, Lambda, OccEnergies, VirtEnergies, SmallEigenvalsCutoffT2, GuessNVecsT2)
            end if
      end subroutine rpa_CC_T2


      subroutine rpa_CC_NormalizeColumns(M, NColumns)
            real(F64), dimension(:,:), intent(inout) :: M
            integer, intent(in)                      :: NColumns
            
            real(F64)                                :: Norm_i
            integer                                  :: i

            do i = 1, Ncolumns
                  Norm_i = norm2(M(:, i))
                  M(:, i) = M(:, i) / Norm_i                  
            end do
      end subroutine rpa_CC_NormalizeColumns

      
      subroutine rpa_CC_IM_v1(IM, M, NVecsT2, PiUEigenvecs, PiUEigenvals, R, NVecsChol, NOcc, NVirt, OccBounds, VirtBounds, &
            Freqs, FreqWeights, NFreqs, Lambda,OccEnergies, VirtEnergies)

            real(F64),dimension(:, :),intent(out)     :: IM
            real(F64),dimension(:, :),intent(in)      :: M
            integer,intent(in)                        :: NVecsT2
            real(F64), dimension(:, :, :), intent(in) :: PiUEigenvecs
            real(F64), dimension(:, :), intent(in)    :: PiUEigenvals
            real(F64), dimension(:, :), intent(in)    :: R
            integer, intent(in)                       :: NVecsChol
            integer, intent(in)                       :: NOcc
            integer, intent(in)                       :: NVirt
            integer, dimension(2), intent(in)         :: OccBounds
            integer, dimension(2), intent(in)         :: VirtBounds
            real(F64), dimension(:), intent(in)       :: Freqs
            real(F64), dimension(:), intent(in)       :: FreqWeights
            integer, intent(in)                       :: NFreqs
            real(F64), intent(in)                     :: Lambda
            real(F64), dimension(:), intent(in)       :: OccEnergies
            real(F64), dimension(:), intent(in)       :: VirtEnergies

            integer :: i, a, k, ai
            real(F64) :: Chi_ai, NumMult
            real(F64), dimension(:, :), allocatable :: QTRChi, MTChiRTQ
            real(F64), dimension(:), allocatable :: D

            allocate(QTRChi(NVecsChol, NVirt*NOcc))
            allocate(MTChiRTQ(NVecsT2, NVecsChol))
            allocate(D(NVirt*NOcc))
            !
            ! Orbital energy differences
            !
            do i = OccBounds(1), OccBounds(2)
                  do a = VirtBounds(1), VirtBounds(2)
                        D((a+1-VirtBounds(1)) + (i-OccBounds(1)) * NVirt) = VirtEnergies(a) - OccEnergies(i)
                  end do
            end do
            IM = ZERO
            do i = 1, NFreqs
                  associate (Q => PiUEigenvecs(:, :, i), &
                        W => PiUEigenvals(:, i))
                        !
                        ! Q**T R 
                        !
                        call real_aTb(QTRChi, Q, R)
                        !
                        ! Q**T R Chi
                        !
                        do ai = 1, NVirt*NOcc
                              Chi_ai = -(FOUR*D(ai))/(D(ai)**2+Freqs(i)**2)
                              QTRChi(:, ai) = Chi_ai * QTRChi(:, ai)
                        end do
                        !
                        ! M**T Chi R**T Q = (Q**T R Chi M)**T = M**T * (Q**T R Chi)**T
                        !
                        call real_aTbT(MTChiRTQ, M, QTRChi)
                        do k = 1, NVecsChol
                              NumMult=Lambda/(ONE+Lambda*W(k))
                              MTChiRTQ(:, k) = NumMult * MTChiRTQ(:, k)
                        end do
                        !
                        ! I*M = I*M - FreqWeights(i)/(4Pi) (Q**T R Chi)**T (M**T Chi R Q)**T
                        !
                        call real_aTbT(IM, QTRChi, MTChiRTQ, alpha=(-FreqWeights(i)/(FOUR*PI)), beta=ONE)
                  end associate
            end do
      end subroutine rpa_CC_IM_v1


      subroutine rpa_CC_T2_v1(A, V, NVecsT2, PiUEigenvecs, PiUEigenvals, Rkai, NVecsChol, NOcc, NVirt, OccBounds, VirtBounds, &
            Freqs, FreqWeights, NFreqs, Lambda,OccEnergies,VirtEnergies, SmallEigenvalsCutoffT2, GuessNVecsT2)

            real(F64), dimension(:),allocatable,intent(out)      :: A
            real(F64), dimension(:, :),allocatable,intent(out)   :: V
            integer,intent(inout)                                :: NVecsT2
            real(F64), dimension(:, :, :), intent(in)            :: PiUEigenvecs
            real(F64), dimension(:, :), intent(in)               :: PiUEigenvals
            real(F64), dimension(:, :), intent(in)               :: Rkai
            integer, intent(in)                                  :: NVecsChol
            integer, intent(in)                                  :: NOcc
            integer, intent(in)                                  :: NVirt
            integer, dimension(2), intent(in)                    :: OccBounds
            integer, dimension(2), intent(in)                    :: VirtBounds
            real(F64), dimension(:), intent(in)                  :: Freqs
            real(F64), dimension(:), intent(in)                  :: FreqWeights
            integer, intent(in)                                  :: NFreqs
            real(F64), intent(in)                                :: Lambda
            real(F64), dimension(:), intent(in)                  :: OccEnergies
            real(F64), dimension(:), intent(in)                  :: VirtEnergies
            real(F64), intent(in)                                :: SmallEigenvalsCutoffT2
            integer, intent(in)                                  :: GuessNVecsT2

            real(F64),dimension(:,:), allocatable     :: Omega
            real(F64),dimension(:,:), allocatable     :: IOmega
            real(F64),dimension(:,:), allocatable     :: S
            real(F64),dimension(:,:), allocatable     :: W
            real(F64), dimension(:),allocatable       :: SEigenValues
            integer                                   :: i, j
            real(F64),dimension(:,:), allocatable     :: T
            real(F64), dimension(:,:), allocatable    :: TIT
            real(F64),dimension(:,:), allocatable     :: IT

            allocate(Omega(Nocc*Nvirt,GuessNVecsT2))
            allocate(IOmega(Nocc*Nvirt,GuessNvecsT2))
            allocate(S(GuessNVecsT2,GuessNvecsT2))
            allocate(SEigenValues(GuessNvecsT2))
            !
            ! Generate the guess vectors for the diagonalization of the matrix I.
            ! 
            call random_init(repeatable=.true., image_distinct=.true.)
            call random_number(Omega)
            Omega = Omega - ONE/TWO
            call rpa_CC_NormalizeColumns(Omega,GuessNVecsT2)
            call rpa_CC_IM_v1(IOmega,Omega,GuessNVecsT2, PiUEigenvecs, PiUEigenvals, Rkai, NVecsChol, NOcc, NVirt, OccBounds, VirtBounds, &
                  Freqs, FreqWeights, NFreqs, Lambda,OccEnergies,VirtEnergies)
            call rpa_CC_NormalizeColumns(IOmega,GuessNVecsT2)
            call real_aTb(S,IOmega,IOmega)
            call symmetric_eigenproblem(SEigenValues, S, GuessNVecsT2, .true. )
            NVecsT2 = 0
            do i = GuessNVecsT2, 1, -1
                  if (SEigenvalues(i)>SmallEigenvalsCutoffT2) then
                        NVecsT2 = NVecsT2 + 1
                  else
                        exit
                  end if
            end do
            allocate(W(GuessNvecsT2, NVecsT2))
            !
            ! NVecsT2 eigenvectors corresponding to the largest eigenvalues.
            ! The eigenvalues are ordered from the smallest one to the largest one
            ! by the diagonalization subroutine.
            !
            j = 1
            do i = GuessNVecsT2-NVecsT2+1, GuessNVecsT2
                  W(:, j) = S(:, i) / Sqrt(SEigenvalues(i))
                  j = j + 1
            end do

            allocate (T(Nocc*NVirt, NVecsT2))
            allocate (TIT(NVecsT2,NVecsT2))
            allocate (IT(Nocc*NVirt,NVecsT2))

            call real_ab(T,IOmega,W)
            call rpa_CC_IM_v1(IT,T,NVecsT2, PiUEigenvecs, PiUEigenvals, Rkai, NVecsChol, NOcc, NVirt, OccBounds, VirtBounds, &
                  Freqs, FreqWeights, NFreqs, Lambda,OccEnergies,VirtEnergies)
            call real_aTB(TIT,T,IT)
            allocate(A(NVecsT2))
            call symmetric_eigenproblem(A,TIT, NVecsT2, .true.)
            do i = 1, NVecsT2
                  A(i) = (ONE/TWO) * (A(i) / (A(i) + ONE))
            end do
            allocate(V(NVecsT2, NVirt*NOcc))
            !
            ! V(1:NVecsCC, 1:NVirt*NOcc) = (Eigenvecs(TIT)(1:NVecsCC, 1:NVecsCC))**T T(1:NVirt*NOcc, 1:NVecsCC)**T
            !
            call real_aTbT_x(V, NVecsT2, TIT, NVecsT2, T, NVirt*NOcc, &
                  NVecsT2, NVirt*NOcc, NVecsT2, ONE, ZERO)
      end subroutine rpa_CC_T2_v1


      subroutine rpa_CC_QTRChi(QTRChi, D, Freq, NOcc, NVirt, Nq)
            real(F64), dimension(Nq, NVirt*NOcc), intent(inout) :: QTRChi
            real(F64), dimension(NVirt*NOcc), intent(in)        :: D
            real(F64), intent(in)                               :: Freq
            integer, intent(in)                                 :: NOcc
            integer, intent(in)                                 :: NVirt
            integer, intent(in)                                 :: Nq
            
            integer :: ai
            real(F64) :: Chi_ai

            !$omp parallel do &
            !$omp private(ai, Chi_ai) &
            !$omp default(shared)
            do ai = 1, NVirt*NOcc
                  Chi_ai = -(FOUR * D(ai)) / (D(ai)**2 + Freq**2)
                  QTRChi(:, ai) = Chi_ai * QTRChi(:, ai)
            end do
            !$omp end parallel do
      end subroutine rpa_CC_QTRChi
      

      subroutine rpa_CC_Chi(Chi, D, Freq, NOcc, NVirt)
            real(F64), dimension(NVirt*NOcc), intent(out) :: Chi
            real(F64), dimension(NVirt*NOcc), intent(in)  :: D
            real(F64), intent(in)                         :: Freq
            integer, intent(in)                           :: NOcc
            integer, intent(in)                           :: NVirt
            
            integer :: ai

            !$omp parallel do &
            !$omp private(ai) &
            !$omp default(shared)
            do ai = 1, NVirt*NOcc
                  Chi(ai) = -(FOUR * D(ai)) / (D(ai)**2 + Freq**2)
            end do
            !$omp end parallel do
      end subroutine rpa_CC_Chi

      
      subroutine rpa_CC_RChiM(RChiM, ChiM, R, NVecsChol, M, NVecsT2, Chi, NOcc, NVirt)
            real(F64), dimension(NVecsChol, NVecsT2), intent(out)     :: RChiM
            real(F64), dimension(NVirt*NOcc, NVecsT2), intent(out)    :: ChiM
            real(F64), dimension(NVecsChol, NVirt*NOcc), intent(in)   :: R
            integer, intent(in)                                       :: NVecsChol
            real(F64), dimension(NVirt*NOcc, NVecsT2), intent(in)     :: M
            integer, intent(in)                                       :: NVecsT2
            real(F64), dimension(NVirt*NOcc), intent(in)              :: Chi
            integer, intent(in)                                       :: NOcc
            integer, intent(in)                                       :: NVirt

            integer :: l

            !$omp parallel do &
            !$omp private(l) &
            !$omp default(shared)
            do l = 1, NVecsT2
                  ChiM(:, l) = Chi(:) * M(:, l)
            end do
            !$omp end parallel do
            call real_ab(RChiM, R, ChiM)
      end subroutine rpa_CC_RChiM
      

      subroutine rpa_CC_ChiRTQWQTRChiM(ChiRTQWQTRChiM, MTChiRTQWQT, MTChiRTQW, R, Q, Chi, NVecsChol, NVecsT2, NOcc, NVirt)
            real(F64), dimension(NVirt*NOcc, NVecsT2), intent(out)  :: ChiRTQWQTRChiM
            real(F64), dimension(NVecsT2, NVecsChol), intent(out)   :: MTChiRTQWQT
            real(F64), dimension(NVecsT2, NVecsChol), intent(in)    :: MTChiRTQW
            real(F64), dimension(NVecsChol, NVirt*NOcc), intent(in) :: R
            real(F64), dimension(NVecsChol, NVecsChol), intent(in)  :: Q
            real(F64), dimension(NVirt*NOcc), intent(in)            :: Chi
            integer, intent(in)                                     :: NVecsChol
            integer, intent(in)                                     :: NVecsT2
            integer, intent(in)                                     :: NOcc
            integer, intent(in)                                     :: NVirt

            integer :: k
            
            call real_abT(MTChiRTQWQT, MTChiRTQW, Q)
            !
            ! RTQWQTRChiM <- R**T * (MTChiRTQWQT)**T
            !
            call real_aTbT(ChiRTQWQTRChiM, R, MTChiRTQWQT)
            !$omp parallel do &
            !$omp private(k) &
            !$omp default(shared)
            do k = 1, NVecsT2
                  ChiRTQWQTRChiM(:, k) = Chi(:) * ChiRTQWQTRChiM(:, k)
            end do
            !$omp end parallel do
      end subroutine rpa_CC_ChiRTQWQTRChiM


      subroutine rpa_CC_D(D, OccEnergies, VirtEnergies, NOcc, NVirt)
            real(F64), dimension(NVirt, NOcc), intent(out) :: D
            real(F64), dimension(NOcc), intent(in)         :: OccEnergies
            real(F64), dimension(NVirt), intent(in)        :: VirtEnergies
            integer, intent(in)                            :: NOcc
            integer, intent(in)                            :: NVirt

            integer :: a, i
            
            !$omp parallel do collapse(2) &
            !$omp private(a, i) &
            !$omp default(shared)
            do i = 1, NOcc
                  do a = 1, NVirt
                        D(a, i) = VirtEnergies(a) - OccEnergies(i)
                  end do
            end do
            !$omp end parallel do
      end subroutine rpa_CC_D

      
      subroutine rpa_CC_IM_v5(IM, M, NVecsT2, PiUEigenvecs, PiUEigenvals, R, NVecsChol, NOcc, NVirt, &
            Freqs, FreqWeights, NFreqs, Lambda, D, Chi, ChiM, RChiM, MTChiRTQ)

            real(F64),dimension(NVirt*NOcc, NVecsT2), intent(out)  :: IM
            real(F64),dimension(:, :),intent(in)                   :: M
            integer,intent(in)                                     :: NVecsT2
            real(F64), dimension(:, :, :), intent(in)              :: PiUEigenvecs
            real(F64), dimension(:, :), intent(in)                 :: PiUEigenvals
            real(F64), dimension(:, :), intent(in)                 :: R
            integer, intent(in)                                    :: NVecsChol
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            real(F64), dimension(:), intent(in)                    :: Freqs
            real(F64), dimension(:), intent(in)                    :: FreqWeights
            integer, intent(in)                                    :: NFreqs
            real(F64), intent(in)                                  :: Lambda
            real(F64), dimension(NVirt*NOcc), intent(in)           :: D
            real(F64), dimension(NVirt*NOcc), intent(out)          :: Chi
            real(F64), dimension(NVirt*NOcc, NVecsT2), intent(out) :: ChiM
            real(F64), dimension(NVecsChol, NVecsT2), intent(out)  :: RChiM
            real(F64), dimension(NVecsT2, NVecsChol), intent(out)  :: MTChiRTQ
            
            integer :: i, k
            real(F64) :: NumMult
            real(F64) :: Alpha

            IM = ZERO
            do i = 1, NFreqs
                  associate (Q => PiUEigenvecs(:, :, i), &
                        W => PiUEigenvals(:, i))
                        call rpa_CC_Chi(Chi, D, Freqs(i), NOcc, NVirt)
                        call rpa_CC_RChiM(RChiM, ChiM, R, NVecsChol, M, NVecsT2, Chi, NOcc, NVirt)
                        call real_aTb(MTChiRTQ, RChiM, Q)
                        !$omp parallel do &
                        !$omp private(k, NumMult) &
                        !$omp default(shared)
                        do k = 1, NVecsChol
                              NumMult = Lambda / (ONE + Lambda * W(k))
                              MTChiRTQ(:, k) = NumMult * MTChiRTQ(:, k)
                        end do
                        !$omp end parallel do
                        associate ( &
                              MTChiRTQW => MTChiRTQ, &
                              MTChiRTQWQT => RChiM, &
                              ChiRTQWQTRChiM => ChiM &
                              )
                              call rpa_CC_ChiRTQWQTRChiM(ChiRTQWQTRChiM, MTChiRTQWQT, MTChiRTQW, &
                                    R, Q, Chi, NVecsChol, NVecsT2, NOcc, NVirt)
                              Alpha = -FreqWeights(i) / (FOUR*PI)
                              IM = IM + Alpha * ChiRTQWQTRChiM
                        end associate
                  end associate
            end do
      end subroutine rpa_CC_IM_v5


      subroutine rpa_CC_MIM_v5(MIM, M, NVecsT2, PiUEigenvecs, PiUEigenvals, R, NVecsChol, &
            NOcc, NVirt, Freqs, FreqWeights, NFreqs, Lambda, D, MaxBatchDim)

            real(F64),dimension(:, :),intent(out)        :: MIM
            real(F64),dimension(:, :),intent(in)         :: M
            integer,intent(in)                           :: NVecsT2
            real(F64), dimension(:, :, :), intent(in)    :: PiUEigenvecs
            real(F64), dimension(:, :), intent(in)       :: PiUEigenvals
            real(F64), dimension(:, :), intent(in)       :: R
            integer, intent(in)                          :: NVecsChol
            integer, intent(in)                          :: NOcc
            integer, intent(in)                          :: NVirt
            real(F64), dimension(:), intent(in)          :: Freqs
            real(F64), dimension(:), intent(in)          :: FreqWeights
            integer, intent(in)                          :: NFreqs
            real(F64), intent(in)                        :: Lambda
            real(F64), dimension(NVirt*NOcc), intent(in) :: D
            integer, intent(in)                          :: MaxBatchDim

            integer :: i, k, n
            real(F64) :: NumMult
            real(F64), dimension(:, :), allocatable :: MTChiRTQ, MTChiRTQW
            real(F64), dimension(:), allocatable :: QTRChi
            integer :: NBatches
            integer :: q0, q1, Nq

            NBatches = NVecsChol / MaxBatchDim
            if (modulo(NVecsChol, MaxBatchDim) > 0) NBatches = NBatches + 1
            allocate(QTRChi(MaxBatchDim*NVirt*NOcc))
            allocate(MTChiRTQW(NVecsT2, MaxBatchDim))
            allocate(MTChiRTQ(NVecsT2, MaxBatchDim))
            MIM = ZERO
            do i = 1, NFreqs
                  do n = 1, NBatches
                        q0 = (n-1) * MaxBatchDim + 1
                        q1 = min(n * MaxBatchDim, NVecsChol)
                        Nq = q1 - q0 + 1
                        associate (Q => PiUEigenvecs(:, q0:q1, i), &
                              W => PiUEigenvals(q0:q1, i))
                              !
                              ! Q**T R 
                              !
                              call real_aTb_x(QTRChi, Nq, Q, NVecsChol, R, NVecsChol, Nq, NVirt*NOcc, NVecsChol)
                              !
                              ! Q**T R Chi
                              !
                              call rpa_CC_QTRChi(QTRChi, D, Freqs(i), NOcc, NVirt, Nq)                         
                              !
                              ! M**T Chi R**T Q = (Q**T R Chi M)**T = M**T * (Q**T R Chi)**T
                              !
                              call real_aTbT_x(MTChiRTQ, NVecsT2, M, NVirt*NOcc, QTRChi, Nq, NVecsT2, Nq, NVirt*NOcc)
                              do k = 1, Nq
                                    NumMult = Lambda / (ONE + Lambda * W(k))
                                    MTChiRTQW(:, k) = NumMult * MTChiRTQ(:, k)
                              end do
                              !
                              ! I*M = I*M - FreqWeights(i)/(4Pi) (Q**T R Chi)**T (M**T Chi R Q)**T
                              !
                              call real_abT_x(MIM, NVecsT2, MTChiRTQ, NVecsT2, MTChiRTQW, NVecsT2, NVecsT2, &
                                    NVecsT2, Nq, alpha=(-FreqWeights(i)/(FOUR*PI)), beta=ONE)
                        end associate
                  end do
            end do
      end subroutine rpa_CC_MIM_v5
      

      subroutine rpa_CC_T2_v5(A, V, NVecsT2, PiUEigenvecs, PiUEigenvals, Rkai, NVecsChol, NOcc, NVirt, &
            Freqs, FreqWeights, NFreqs, Lambda, OccEnergies, VirtEnergies, SmallEigenvalsCutoffT2, &
            GuessNVecsT2, MaxBatchDim)
            !
            ! Compute the dominant NVecsT2 eigenvectors and eigenvalues of the double excitation
            ! amplitudes matrix T2 in the direct-ring approximation. Use the non-iterative formula
            ! based the frequency integral of Chi(Lambda)-Chi(0).
            !
            real(F64), dimension(:), allocatable, intent(out)    :: A
            real(F64), dimension(:, :), allocatable, intent(out) :: V
            integer,intent(inout)                                :: NVecsT2
            real(F64), dimension(:, :, :), intent(in)            :: PiUEigenvecs
            real(F64), dimension(:, :), intent(in)               :: PiUEigenvals
            real(F64), dimension(:, :), intent(in)               :: Rkai
            integer, intent(in)                                  :: NVecsChol
            integer, intent(in)                                  :: NOcc
            integer, intent(in)                                  :: NVirt
            real(F64), dimension(:), intent(in)                  :: Freqs
            real(F64), dimension(:), intent(in)                  :: FreqWeights
            integer, intent(in)                                  :: NFreqs
            real(F64), intent(in)                                :: Lambda
            real(F64), dimension(:), intent(in)                  :: OccEnergies
            real(F64), dimension(:), intent(in)                  :: VirtEnergies
            real(F64), intent(in)                                :: SmallEigenvalsCutoffT2
            integer, intent(in)                                  :: GuessNVecsT2
            integer, intent(in)                                  :: MaxBatchDim

            real(F64),dimension(:,:), allocatable     :: Omega
            real(F64),dimension(:,:), allocatable     :: IOmega
            real(F64),dimension(:,:), allocatable     :: S
            real(F64),dimension(:,:), allocatable     :: W
            real(F64), dimension(:),allocatable       :: SEigenValues
            integer                                   :: i, j
            real(F64),dimension(:,:), allocatable     :: T
            real(F64), dimension(:,:), allocatable    :: TIT
            real(F64), dimension(:, :), allocatable :: MTChiRTQ, RChiM, ChiM
            real(F64), dimension(:), allocatable :: D, Chi
            integer :: NBatches
            integer :: q0, q1, Nq
            !
            ! Where possible, the matrices will be computed as independent batches
            ! to save memory. The last batch will have dimension 0 < Nq < MaxBatchDim.
            !
            NBatches = GuessNVecsT2 / MaxBatchDim
            if (modulo(GuessNVecsT2, MaxBatchDim) > 0) NBatches = NBatches + 1
            !
            ! Omega <- initial random vectors (computed in batches)
            !
            allocate(Omega(Nocc*Nvirt, MaxBatchDim))
            !
            ! IOmega <- I*Omega
            !
            allocate(IOmega(Nocc*Nvirt, GuessNvecsT2))

            allocate(D(NVirt*NOcc))
            !
            ! Working arrays for the rpa_CC_IM subroutine.
            ! Allocated here to avoid repeated allocation
            ! inside the loop over batches.
            !
            allocate(Chi(NVirt*NOcc))
            allocate(ChiM(NVirt*NOcc, MaxBatchDim))
            allocate(RChiM(NVecsChol, MaxBatchDim))
            allocate(MTChiRTQ(MaxBatchDim, NVecsChol))
            call rpa_CC_D(D, OccEnergies, VirtEnergies, NOcc, NVirt)
            !
            ! Generate the guess vectors for the diagonalization of the matrix I.
            ! The physically relevant subset of eigenvectors is a small subset
            ! of the NVirt*NOcc dimensional space, but using a sufficient number
            ! of guess random vectors (controlled by GuessNVecsT2) guarantees
            ! that we probe every vector in the dominant eigenspace.
            !
            ! We checked both Gaussian and uniform random number generators.
            ! The uniform random number generator is faster, better parallelizable,
            ! and the quality of results is practically the same as for
            ! the Gaussian variant.
            !
            ! Note that initialization of the random number genrator should
            ! stay outside of the loop over batches.
            !
            call random_init(repeatable=.true., image_distinct=.true.)
            do i = 1, NBatches
                  q0 = (i-1)*MaxBatchDim + 1
                  q1 = min(GuessNVecsT2, q0+MaxBatchDim-1)
                  Nq = q1 - q0 + 1
                  associate ( &
                        Omega_i => Omega(:, 1:Nq), &
                        IOmega_i => IOmega(:, q0:q1) &
                        )
                        call random_number(Omega_i)
                        Omega_i = Omega_i - ONE/TWO
                        call rpa_CC_NormalizeColumns(Omega_i, Nq)
                        call rpa_CC_IM_v5(IOmega_i, Omega_i, Nq, PiUEigenvecs, PiUEigenvals, &
                              Rkai, NVecsChol, NOcc, NVirt, Freqs, FreqWeights, NFreqs, Lambda, &
                              D, Chi, ChiM, RChiM, MTChiRTQ)
                        call rpa_CC_NormalizeColumns(IOmega_i, Nq)
                  end associate
            end do
            deallocate(Chi)
            deallocate(ChiM)
            deallocate(RChiM)
            deallocate(MTChiRTQ)
            deallocate(Omega)
            allocate(S(GuessNVecsT2,GuessNvecsT2))
            call real_aTb(S, IOmega, IOmega)
            allocate(SEigenValues(GuessNvecsT2))
            call symmetric_eigenproblem(SEigenValues, S, GuessNVecsT2, .true. )
            NVecsT2 = 0
            do i = GuessNVecsT2, 1, -1
                  if (SEigenvalues(i)>SmallEigenvalsCutoffT2) then
                        NVecsT2 = NVecsT2 + 1
                  else
                        exit
                  end if
            end do
            allocate(W(GuessNvecsT2, NVecsT2))
            !
            ! NVecsT2 eigenvectors corresponding to the largest eigenvalues.
            ! The eigenvalues are ordered from the smallest one to the largest one
            ! by the diagonalization subroutine.
            !
            j = 1
            do i = GuessNVecsT2-NVecsT2+1, GuessNVecsT2
                  W(:, j) = S(:, i) / Sqrt(SEigenvalues(i))
                  j = j + 1
            end do
            allocate(T(Nocc*NVirt, NVecsT2))
            call real_ab(T, IOmega, W)
            deallocate(IOmega)
            allocate(TIT(NVecsT2, NVecsT2))        
            call rpa_CC_MIM_v5(TIT, T, NVecsT2, PiUEigenvecs, PiUEigenvals, Rkai, NVecsChol, NOcc, NVirt, &
                  Freqs, FreqWeights, NFreqs, Lambda, D, min(MaxBatchDim, NVecsChol))
            !
            ! Eigenvalues and eigenvectors of the matrix T2.
            ! The eigenvectors of the matrix I are the same as
            ! the eigenvectors of T2, but the eigenvalues need
            ! to be changed.
            !
            allocate(A(NVecsT2))
            call symmetric_eigenproblem(A, TIT, NVecsT2, .true.)
            do i = 1, NVecsT2
                  A(i) = (ONE/TWO) * (A(i) / (A(i) + ONE))
            end do
            allocate(V(NVecsT2, NVirt*NOcc))
            !
            ! Transform matrix columns to obtain the T2 eigenvectors expressed
            ! in the basis of Cholesky/Pi(u) vectors. 
            ! V(1:NVecsCC, 1:NVirt*NOcc) = (Eigenvecs(TIT)(1:NVecsT2, 1:NVecsT2))**T T(1:NVirt*NOcc, 1:NVecsT2)**T
            !
            call real_aTbT_x(V, NVecsT2, TIT, NVecsT2, T, NVirt*NOcc, &
                  NVecsT2, NVirt*NOcc, NVecsT2, ONE, ZERO)
      end subroutine rpa_CC_T2_v5

      
      subroutine rpa_THC_CC_T2(A, V, EigRPA, NVecsT2, PiUEigenvecs, PiUEigenvals, Rkai, NVecsChol, NOcc, NVirt, &
            Freqs, FreqWeights, NFreqs, Lambda, OccEnergies, VirtEnergies, SmallEigenvalsCutoffT2, &
            GuessNVecsT2, MaxBatchDim, T2CutoffThresh, T2CutoffType, T2CutoffSmoothStep, &
            T2CutoffSteepness, T2CutoffCommonThresh)
            !
            ! Compute the dominant NVecsT2 eigenvectors and eigenvalues of the double excitation
            ! amplitudes matrix T2 in the direct-ring approximation. Use the non-iterative formula
            ! based the frequency integral of Chi(Lambda)-Chi(0).
            !
            real(F64), dimension(:), allocatable, intent(out)    :: A
            real(F64), dimension(:, :), allocatable, intent(out) :: V
            real(F64), dimension(:), allocatable, intent(out)    :: EigRPA
            integer,intent(inout)                                :: NVecsT2
            real(F64), dimension(:, :, :), intent(in)            :: PiUEigenvecs
            real(F64), dimension(:, :), intent(in)               :: PiUEigenvals
            real(F64), dimension(:, :), intent(in)               :: Rkai
            integer, intent(in)                                  :: NVecsChol
            integer, intent(in)                                  :: NOcc
            integer, intent(in)                                  :: NVirt
            real(F64), dimension(:), intent(in)                  :: Freqs
            real(F64), dimension(:), intent(in)                  :: FreqWeights
            integer, intent(in)                                  :: NFreqs
            real(F64), intent(in)                                :: Lambda
            real(F64), dimension(:), intent(in)                  :: OccEnergies
            real(F64), dimension(:), intent(in)                  :: VirtEnergies
            real(F64), intent(in)                                :: SmallEigenvalsCutoffT2
            integer, intent(in)                                  :: GuessNVecsT2
            integer, intent(in)                                  :: MaxBatchDim
            real(F64), intent(in)                                :: T2CutoffThresh
            integer, intent(in)                                  :: T2CutoffType
            logical, intent(in)                                  :: T2CutoffSmoothStep
            real(F64), intent(in)                                :: T2CutoffSteepness
            real(F64), intent(inout)                             :: T2CutoffCommonThresh

            real(F64),dimension(:,:), allocatable     :: Omega
            real(F64),dimension(:,:), allocatable     :: IOmega
            real(F64),dimension(:,:), allocatable     :: S
            real(F64),dimension(:,:), allocatable     :: W
            real(F64), dimension(:),allocatable       :: SEigenValues
            integer                                   :: i, j
            real(F64),dimension(:,:), allocatable     :: T
            real(F64), dimension(:,:), allocatable    :: TIT
            real(F64), dimension(:, :), allocatable :: MTChiRTQ, RChiM, ChiM
            real(F64), dimension(:), allocatable :: D, Chi
            integer :: NBatches
            integer :: q0, q1, Nq

            real(F64), dimension(:), allocatable   :: Anew
            integer :: NVecsT2New, NVecsT2Scaled
            real(F64) :: SumA
            real(F64) :: K
            real(F64) :: Ratio
            logical, parameter :: PrintEigenvalues = .false.
            real(F64) :: StepFunction
            real(F64) :: Ka, Kb
            !
            ! Where possible, the matrices will be computed as independent batches
            ! to save memory. The last batch will have dimension 0 < Nq < MaxBatchDim.
            !
            NBatches = GuessNVecsT2 / MaxBatchDim
            if (modulo(GuessNVecsT2, MaxBatchDim) > 0) NBatches = NBatches + 1
            !
            ! Omega <- initial random vectors (computed in batches)
            !
            allocate(Omega(Nocc*Nvirt, MaxBatchDim))
            !
            ! IOmega <- I*Omega
            !
            allocate(IOmega(Nocc*Nvirt, GuessNvecsT2))
            allocate(D(NVirt*NOcc))
            !
            ! Working arrays for the rpa_CC_IM subroutine.
            ! Allocated here to avoid repeated allocation
            ! inside the loop over batches.
            !
            allocate(Chi(NVirt*NOcc))
            allocate(ChiM(NVirt*NOcc, MaxBatchDim))
            allocate(RChiM(NVecsChol, MaxBatchDim))
            allocate(MTChiRTQ(MaxBatchDim, NVecsChol))
            call rpa_CC_D(D, OccEnergies, VirtEnergies, NOcc, NVirt)
            !
            ! Generate the guess vectors for the diagonalization of the matrix I.
            ! The physically relevant subset of eigenvectors is a small subset
            ! of the NVirt*NOcc dimensional space, but using a sufficient number
            ! of guess random vectors (controlled by GuessNVecsT2) guarantees
            ! that we probe every vector in the dominant eigenspace.
            !
            ! We checked both Gaussian and uniform random number generators.
            ! The uniform random number generator is faster, better parallelizable,
            ! and the quality of results is practically the same as for
            ! the Gaussian variant.
            !
            ! Note that initialization of the random number genrator should
            ! stay outside of the loop over batches.
            !
            call random_init(repeatable=.true., image_distinct=.true.)
            do i = 1, NBatches
                  q0 = (i-1)*MaxBatchDim + 1
                  q1 = min(GuessNVecsT2, q0+MaxBatchDim-1)
                  Nq = q1 - q0 + 1
                  associate ( &
                        Omega_i => Omega(:, 1:Nq), &
                        IOmega_i => IOmega(:, q0:q1) &
                        )
                        call random_number(Omega_i)
                        Omega_i = Omega_i - ONE/TWO
                        call rpa_CC_NormalizeColumns(Omega_i, Nq)
                        call rpa_CC_IM_v5(IOmega_i, Omega_i, Nq, PiUEigenvecs, PiUEigenvals, &
                              Rkai, NVecsChol, NOcc, NVirt, Freqs, FreqWeights, NFreqs, Lambda, &
                              D, Chi, ChiM, RChiM, MTChiRTQ)
                        call rpa_CC_NormalizeColumns(IOmega_i, Nq)
                  end associate
            end do
            deallocate(Chi)
            deallocate(ChiM)
            deallocate(RChiM)
            deallocate(MTChiRTQ)
            deallocate(Omega)
            allocate(S(GuessNVecsT2,GuessNvecsT2))
            call real_aTb(S, IOmega, IOmega)
            allocate(SEigenValues(GuessNvecsT2))
            call symmetric_eigenproblem(SEigenValues, S, GuessNVecsT2, .true. )
            NVecsT2 = 0
            do i = GuessNVecsT2, 1, -1
                  if (SEigenvalues(i)>SmallEigenvalsCutoffT2) then
                        NVecsT2 = NVecsT2 + 1
                  else
                        exit
                  end if
            end do
            allocate(W(GuessNvecsT2, NVecsT2))
            !
            ! NVecsT2 eigenvectors corresponding to the largest eigenvalues.
            ! The eigenvalues are ordered from the smallest one to the largest one
            ! by the diagonalization subroutine.
            !
            j = 1
            do i = GuessNVecsT2-NVecsT2+1, GuessNVecsT2
                  W(:, j) = S(:, i) / Sqrt(SEigenvalues(i))
                  j = j + 1
            end do
            allocate(T(Nocc*NVirt, NVecsT2))
            call real_ab(T, IOmega, W)
            deallocate(IOmega)
            allocate(TIT(NVecsT2, NVecsT2))        
            call rpa_CC_MIM_v5(TIT, T, NVecsT2, PiUEigenvecs, PiUEigenvals, Rkai, NVecsChol, NOcc, NVirt, &
                  Freqs, FreqWeights, NFreqs, Lambda, D, min(MaxBatchDim, NVecsChol))
            !
            ! Eigenvalues and eigenvectors of the matrix T2.
            ! The eigenvectors of the matrix I are the same as
            ! the eigenvectors of T2, but the eigenvalues need
            ! to be changed.
            !
            allocate(A(NVecsT2))
            call symmetric_eigenproblem(A, TIT, NVecsT2, .true.)
            do i = 1, NVecsT2
                  A(i) = (ONE/TWO) * (A(i) / (A(i) + ONE))
            end do
            ! ------------------------------------------------------------------
            !                   Small eigenvalues cutoff
            ! ------------------------------------------------------------------
            !
            ! To guarantee size-consistent interaction energies which properly vanish
            ! at infinite separations between subsystems, the cutoff threshold K
            ! needs to be identical in the supermolecule (dimer, trimer, ...)
            ! and its subsystems.
            !
            ! Checking if the current calculation is the supermolecule or
            ! its subsystems
            ! -----------------------------------------------------------
            !
            !           T2CutoffCommonThresh < ZERO  supermolecule
            !           T2CutoffCommonThresh >= ZERO  one of subsystems
            !
            ! Supermolecule calculation
            ! -------------------------
            !
            ! T2CutoffType
            !           Method for rejecting small eigenvalues.
            !                    
            !           Value of T2CutoffType               Cutoff method for supermolecule
            !
            !           RPA_T2_CUTOFF_EIG                   K = T2CutoffThresh; reject mu if Abs(A(mu)) <= K
            !           RPA_T2_CUTOFF_EIG_DIV_MAXEIG        K = T2CutoffThresh * Maxval(Abs(A)); reject mu if Abs(A(mu)) <= K
            !           RPA_T2_CUTOFF_EIG_DIV_NELECTRON     K = T2CutoffThresh / (NOcc * 2); reject mu if Abs(A(mu)) <= K
            !           RPA_T2_CUTOFF_SUM_REJECTED          K = max(mu') abs(A(mu')) : Sum(nu=mu'...NVecsT2) Abs(A(nu)) < TCutoffThresh
            !                                               reject mu if Abs(A(mu)) <= K
            !
            !           After computing K for the supermolecule, set the common threshold
            !
            !           T2CutoffCommonThresh = K.
            !
            !           T2CutoffCommonThresh will be used in the subsystem calculations.
            !
            ! Subsystem calculation
            ! ---------------------
            !
            !           Cutoff method for subsystems (e.g., monomers in a dimer)
            !
            !           K = T2CutoffCommonThresh (same as in dimer)
            !           reject mu if Abs(A(mu)) <= K
            ! 
            !
            call msg("Removing small eigenvalues of T2")
            if (T2CutoffCommonThresh < ZERO) then
                  !
                  ! supersystem calculation; evaluate K
                  !
                  select case (T2CutoffType)
                  case (RPA_T2_CUTOFF_EIG)
                        K = T2CutoffThresh
                        call msg("remove mu if Abs(a(mu)) <= T2CutoffThresh = " // str(K,d=3))
                  case (RPA_T2_CUTOFF_EIG_DIV_MAXEIG)
                        K = T2CutoffThresh * MAXVAL(Abs(A))
                        call msg("remove mu if Abs(a(mu)) <= T2CutoffThresh*Max(Abs(a(mu')) = " // str(K,d=3))
                  case (RPA_T2_CUTOFF_EIG_DIV_NELECTRON)
                        K = T2CutoffThresh / (NOcc * 2)
                        call msg("remove mu if Abs(a(mu)) <= T2CutoffThresh/NElectrons = " // str(K,d=3))
                  case (RPA_T2_CUTOFF_SUM_REJECTED)
                        do i = 1, NVecsT2
                              if (i < NVecsT2) then
                                    SumA = sum(A(i+1:NVecsT2))
                                    if (abs(SumA) < T2CutoffThresh) then
                                          K = Abs(A(i+1))
                                          exit
                                    end if
                              else
                                    SumA = ZERO
                                    K = ZERO
                              end if
                        end do
                        call msg("remove mu if Abs(Sum(a(mu:NVecsT2))) < T2CutoffThresh = " // str(T2CutoffThresh,d=3))
                        call msg("equivalent condition:")
                        call msg("remove mu if Abs(a(mu)) <= " // str(K, d=3))
                  case default
                        call msg("Invalid value of T2CutoffThresh", MSG_ERROR)
                        error stop
                  end select
            else
                  !
                  ! subsystem calculation; take K from the supersystem calculation
                  !
                  K = T2CutoffCommonThresh
                  call msg("remove mu if Abs(a(mu)) <= supersystem threshold = " // str(T2CutoffCommonThresh,d=3))
            end if
            allocate(Anew(NVecsT2))
            !
            ! Initial value of NVecsT2: will be reduced to a smaller
            ! number if any eigenvectors are rejected
            !
            NVecsT2New = NVecsT2
            NVecsT2Scaled = 0
            if (T2CutoffSmoothStep) then
                  Ka = K
                  Kb = K * T2CutoffSteepness
                  call msg("Smooth cutoff enabled")
                  call msg("Step function interval: (" // str(Ka,d=3) // ", " // str(Kb,d=3) // ")")
            end if
            do i = 1, NVecsT2
                  if (T2CutoffSmoothStep) then
                        if (abs(A(i)) >= Ka) then
                              Anew(i) = A(i)
                        else if (abs(A(i)) > Kb) then
                              call rpa_CC_T2_SmoothStepFunction(StepFunction, abs(A(i)), Ka, Kb)
                              Anew(i) = StepFunction * A(i)
                              NVecsT2Scaled = NVecsT2Scaled + 1
                        else
                              NVecsT2New = i - 1
                              exit
                        end if
                  else
                        if (abs(A(i)) > K) then
                              Anew(i) = A(i)
                        else
                              NVecsT2New = i - 1
                              exit
                        end if
                  end if
            end do
            if (NVecsT2New > 0) then
                  if (NVecsT2New < NVecsT2) then
                        call msg("Removed " // str(nint(real(NVecsT2-NVecsT2New,F64)/NVecsT2*100)) // "% of eigenvectors")
                        call msg("Full set of eigenvecs:    " // str(NVecsT2))
                        call msg("Reduced set of eigenvecs: " // str(NVecsT2New))
                        if (NVecsT2Scaled > 0) then
                              call msg("Step function interval contains " // str(NVecsT2Scaled) // " eigenvals")
                        end if
                  else
                        call msg("No eigenvectors removed")
                  end if
            else
                  call msg("Invalid T2CutoffThresh: to eigenvectors left after cutoff", MSG_ERROR)
                  error stop
            end if
            Ratio = real(NVecsT2New,F64)/(NOcc+NVirt)
            call msg(lfield("NVecsT2/(NOcc+NVirt)", 25) // str(Ratio,d=1))
            if (PrintEigenvalues) then
                  call blankline()
                  call msg("Eigenvalues of T2", underline=.true.)
                  do i = 1, NVecsT2
                        if (i == NVecsT2New + 1) then
                              call msg("--- removed eigenvalues ---")
                        end if
                        if (i > NVecsT2New-NVecsT2Scaled .and. i <= NVecsT2New) then
                              call msg(lfield(str(i), 10) // lfield(str(A(i), d=3), 30) // lfield(str(Anew(i),d=3), 30))
                        else
                              call msg(lfield(str(i), 10) // str(A(i), d=3))
                        end if
                  end do
                  call blankline()
            end if
            deallocate(A)
            allocate(A(NVecsT2New))
            do i=1, NVecsT2New
                  A(i) = ANew(i)
            end do
            deallocate(Anew)            
            allocate(V(NVirt*NOcc, NVecsT2New))
            !
            ! Transform matrix columns to obtain the T2 eigenvectors expressed
            ! in the basis of Cholesky/Pi(u) vectors. 
            ! V(1:NVirt*NOcc,1:NVecsT2) = T(1:NVirt*NOcc, 1:NVecsT2)*Eigenvecs(TIT)(1:NVecsT2, 1:NVecsT2)
            !
            call real_ab_x(V, NVirt*NOcc, T, NVirt*NOcc, TIT, NVecsT2,  &
                  NVirt*NOcc, NVecsT2New, NVecsT2, ONE, ZERO)
            NVecsT2 = NVecsT2New
            allocate(EigRPA(NVecsT2))
            call rpa_T2_EigRPA(EigRPA, V, A, Rkai, NOcc, NVirt, NVecsT2, NVecsChol)
            !
            ! Store threshold value for the subsequent
            ! subsystem calculations
            !
            if (T2CutoffCommonThresh < ZERO) T2CutoffCommonThresh = K
      end subroutine rpa_THC_CC_T2


      subroutine rpa_T2_EigRPA(EigRPA, Uaim, Am, Rkai, NOcc, NVirt, NVecsT2, NCholesky)
            real(F64), dimension(NVecsT2), intent(out)              :: EigRPA
            real(F64), dimension(NVirt*NOcc, NVecsT2), intent(in)   :: Uaim
            real(F64), dimension(NVecsT2), intent(in)               :: Am
            real(F64), dimension(NCholesky, NVirt*NOcc), intent(in) :: Rkai
            integer, intent(in)                                     :: NOcc
            integer, intent(in)                                     :: NVirt
            integer, intent(in)                                     :: NVecsT2
            integer, intent(in)                                     :: NCholesky

            real(F64), dimension(:, :), allocatable :: RUkm
            integer :: mu
            real(F64) :: t

            allocate(RUkm(NCholesky, NVecsT2))
            call real_ab(RUkm, Rkai, Uaim)
            !$omp parallel do private(mu, t)
            do mu = 1, NVecsT2
                  call real_vw_x(t, RUkm(:, mu), RUkm(:, mu), NCholesky)
                  EigRPA(mu) = TWO * t * Am(mu)
            end do
            !$omp end parallel do
      end subroutine rpa_T2_EigRPA
      

      subroutine rpa_THC_CC_T2_Decompose(Qem, Yga, Xgi, Uaim, Am, NOcc, NVirt, &
            NVecsT2, NGridTHC, T2THCThresh)
            
            real(F64), dimension(NGridTHC, NVecsT2), intent(out)   :: Qem
            real(F64), dimension(:, :), intent(in)                 :: Yga
            real(F64), dimension(:, :), intent(in)                 :: Xgi
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in) :: Uaim
            real(F64), dimension(NVecsT2), intent(in)              :: Am
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: NVecsT2
            integer, intent(in)                                    :: NGridTHC
            real(F64), intent(in)                                  :: T2THCThresh

            real(F64), dimension(:, :), allocatable :: Sgh
            real(F64), dimension(:, :), allocatable :: XXgh
            real(F64), dimension(:, :), allocatable :: YYgh
            type(TClock) :: timer
            real(F64) :: t_total
            integer :: mu, a, i
            integer :: g
            real(F64) :: TikhonovCoeff

            call msg("Tensor hypercontraction of T2")
            call msg("NOT WORKING PROPERLY --- STILL UNDER DEVELOPMENT")
            call clock_start(timer)
            TikhonovCoeff = T2THCThresh
            call msg("Tikhonov parameter Lambda: " // str(TikhonovCoeff))
            allocate(Sgh(NGridTHC, NGridTHC))
            allocate(XXgh(NGridTHC, NGridTHC))
            allocate(YYgh(NGridTHC, NGridTHC))
            call real_abT(XXgh, Xgi, Xgi)
            call real_abT(YYgh, Yga, Yga)
            Sgh = XXgh * YYgh
            Qem = ZERO
            !$omp parallel do default(shared) &
            !$omp private(mu, a, i)
            do mu = 1, NVecsT2
                  do i = 1, NOcc
                        do a = 1, NVirt
                              Qem(:, mu) = Qem(:, mu) + Yga(:, a) * Xgi(:, i) * Uaim(a, i, mu)
                        end do
                  end do
            end do
            !$omp end parallel do
            do g = 1, NGridTHC
                  Sgh(g, g) = XXgh(g, g)*YYgh(g, g) + TikhonovCoeff**2
            end do
            call real_Axb_symmetric_sysv(Qem, Sgh)
            t_total = clock_readwall(timer)
            call msg("T2 decomposed in " // str(t_total,d=1) // " seconds")
      end subroutine rpa_THC_CC_T2_Decompose


      subroutine rpa_THC_CC_T2_Reconstruct(Uaim, Yga, Xgi, Qgm, NOcc, NVirt, NGridTHC, Wgi)
            real(F64), dimension(NVirt, NOcc), intent(out)      :: Uaim
            real(F64), dimension(NGridTHC, NVirt), intent(in)   :: Yga
            real(F64), dimension(NGridTHC, NOcc), intent(in)    :: Xgi
            real(F64), dimension(NGridTHC), intent(in)          :: Qgm
            integer, intent(in)                                 :: NOcc
            integer, intent(in)                                 :: NVirt
            integer, intent(in)                                 :: NGridTHC
            real(F64), dimension(NGridTHC, NOcc), intent(out)   :: Wgi

            integer :: i

            !$omp parallel do private(i)
            do i = 1, NOcc
                  Wgi(:, i) = Xgi(:, i) * Qgm(:)
            end do
            !$omp end parallel do
            call real_aTb(Uaim, Yga, Wgi)
      end subroutine rpa_THC_CC_T2_Reconstruct
      
      
      subroutine rpa_THC_CC_T2_Decompose_Test(Qgm, Yga, Xgi, Uaim, Ak, NOcc, NVirt, NVecsT2, NGridTHC)
            real(F64), dimension(:, :), intent(in)                 :: Qgm
            real(F64), dimension(:, :), intent(in)                 :: Yga
            real(F64), dimension(:, :), intent(in)                 :: Xgi
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in) :: Uaim
            real(F64), dimension(NVecsT2), intent(in)              :: Ak
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: NVecsT2
            integer, intent(in)                                    :: NGridTHC

            integer :: a, i, b, j, k
            real(F64) :: T2Exact, T2Approx
            real(F64), dimension(:), allocatable :: Vaik, Vbjk
            real(F64), dimension(:), allocatable :: XaiGamma, XbjDelta
            real(F64), dimension(:), allocatable :: XaiZ, XbjZ
            real(F64) :: MaxAbsError, MaxRelError

            allocate(Vaik(NVecsT2))
            allocate(Vbjk(NVecsT2))
            allocate(XaiGamma(NGridTHC))
            allocate(XbjDelta(NGridTHC))
            allocate(XaiZ(NVecsT2))
            allocate(XbjZ(NVecsT2))
            call msg("NGridTHC   = " // str(NGridTHC))
            call msg("NVirt*NOcc = " // str(NVirt*NOcc))
            call msg("NVecsT2    = " // str(NVecsT2))
            MaxAbsError = ZERO
            MaxRelError = ZERO
            do j = 1, NOcc
                  do b = 1, NVirt
                        do i = 1, NOcc
                              do a = 1, NVirt
                                    Vaik = Uaim(a, i, :)
                                    Vbjk = Uaim(b, j, :)
                                    do k = 1, NVecsT2
                                          Vaik(k) = Vaik(k) * Sqrt(-Ak(k))
                                          Vbjk(k) = Vbjk(k) * Sqrt(-Ak(k))                                          
                                    end do
                                    XaiGamma(:) = Yga(:, a) * Xgi(:, i)
                                    XbjDelta(:) = Yga(:, b) * Xgi(:, j)
                                    T2Exact = -dot_product(Vaik, Vbjk)
                                    XaiZ = matmul(XaiGamma, Qgm)
                                    XbjZ = matmul(XbjDelta, Qgm)
                                    T2Approx = ZERO
                                    do k = 1, NVecsT2
                                          T2Approx = T2Approx + XaiZ(k) * XbjZ(k) * Ak(k)
                                    end do
                                    if (abs(T2Exact) > 1.0E-3_F64) then
                                          MaxAbsError = max(MaxAbsError, Abs(T2Exact-T2Approx))
                                          MaxRelError = max(MaxRelError, Abs(T2Exact-T2Approx)/Abs(T2Exact))
                                    end if
                              end do
                        end do
                  end do
            end do
            call msg("MaxAbsError = " // str(MaxAbsError,d=1))
            call msg("MaxRelError = " // str(MaxRelError,d=1))
      end subroutine rpa_THC_CC_T2_Decompose_Test
end module rpa_CC_Doubles
