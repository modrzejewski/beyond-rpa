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


      subroutine rpa_THC_CC_T2(A, V, NVecsT2, PiUEigenvecs, PiUEigenvals, Rkai, NVecsChol, NOcc, NVirt, &
            Freqs, FreqWeights, NFreqs, Lambda, OccEnergies, VirtEnergies, SmallEigenvalsCutoffT2, &
            GuessNVecsT2, MaxBatchDim, T2CutoffThresh, T2CutoffType, T2CutoffCommonThresh)
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
            real(F64), intent(in)                                :: T2CutoffThresh
            integer, intent(in)                                  :: T2CutoffType
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
            allocate(V(NVirt*NOcc, NVecsT2))
            !
            ! Transform matrix columns to obtain the T2 eigenvectors expressed
            ! in the basis of Cholesky/Pi(u) vectors. 
            ! V(1:NVirt*NOcc,1:NVecsT2) = T(1:NVirt*NOcc, 1:NVecsT2)*Eigenvecs(TIT)(1:NVecsT2, 1:NVecsT2)
            !
            call real_ab_x(V, NVirt*NOcc, T, NVirt*NOcc, TIT, NVecsT2,  &
                  NVirt*NOcc, NVecsT2, NVecsT2, ONE, ZERO)
      end subroutine rpa_THC_CC_T2


      subroutine rpa_THC_CC_T2_Decompose(Qem, Yga, Xgi, Uaim, Ak, NOcc, NVirt, &
            NVecsT2, NGridTHC)
            
            real(F64), dimension(:, :), allocatable, intent(out)   :: Qem
            real(F64), dimension(:, :), intent(in)                 :: Yga
            real(F64), dimension(:, :), intent(in)                 :: Xgi
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in) :: Uaim
            real(F64), dimension(NVecsT2), intent(in)              :: Ak
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: NVecsT2
            integer, intent(in)                                    :: NGridTHC

            integer :: a, i, k
            real(F64), dimension(:, :), allocatable :: XXgh, YYgh
            real(F64), dimension(:, :), allocatable :: YYXXgh, QYYXX, QYYXXQ
            real(F64), dimension(:, :), allocatable :: QInvS12, InvS12
            real(F64), dimension(:), allocatable :: Lambda
            ! ------------------------------------------------------------------
            ! THC decomposition of T2 eigenvectors.
            ! At this point the THC-decomposed eigenvectors are not guaranteed
            ! to be orthonormal.
            ! ------------------------------------------------------------------
            allocate(Qem(NGridTHC, NVecsT2))
            Qem = ZERO
            !$omp parallel do default(shared) &
            !$omp private(k, a, i)
            do k = 1, NVecsT2
                  do i = 1, NOcc
                        do a = 1, NVirt
                              Qem(:, k) = Qem(:, k) + Yga(:, a) * Xgi(:, i) * Uaim(a, i, k)
                        end do
                  end do
            end do
            !$omp end parallel do
            allocate(YYXXgh(NGridTHC, NGridTHC))
            allocate(YYgh(NGridTHC, NGridTHC))
            allocate(XXgh(NGridTHC, NGridTHC))
            call real_abT(XXgh, Xgi, Xgi)
            call real_abT(YYgh, Yga, Yga)            
            YYXXgh = YYgh * XXgh            
            call real_Axb_symmetric_sysv(Qem, YYXXgh)

            ! do k = 1, NVecsT2
            !       Qem(:, k) = Qem(:, k) * Sqrt(-Ak(k))
            ! end do

            
            ! ------------------------------------------------
            ! Overlap between THC-deomposed T2 eigenvectors
            !
            ! S(mu,nu) = Sum(ai) U(ai,mu)*U(ai,nu)
            ! = Q**T {YYXX} Q
            ! ------------------------------------------------
            YYXXgh = YYgh * XXgh
            deallocate(YYgh, XXgh)
            allocate(QYYXX(NVecsT2, NGridTHC))
            call real_aTb(QYYXX, Qem, YYXXgh)
            deallocate(YYXXgh)
            allocate(QYYXXQ(NVecsT2, NVecsT2))
            call real_ab(QYYXXQ, QYYXX, Qem)
            deallocate(QYYXX)
            !
            ! Transformation matrix S**(-1/2)
            !
            allocate(Lambda(NVecsT2))            
            call symmetric_eigenproblem(Lambda, QYYXXQ, NVecsT2, .true.)
            do k = 1, NVecsT2
                  if (Lambda(k) > ZERO) then
                        QYYXXQ(:, k) = QYYXXQ(:, k) * (ONE/Sqrt(Sqrt(Lambda(k))))
                  else
                        QYYXXQ(:, k) = ZERO
                  end if
            end do
            allocate(InvS12(NVecsT2, NVecsT2))
            call real_abT(InvS12, QYYXXQ, QYYXXQ)
            deallocate(QYYXXQ)
            !
            ! Orthogonalized eigenvectors of T2
            ! Each eigenvector is scaled by Sqrt(-A(k))
            !
            allocate(QInvS12(NGridTHC, NVecsT2))
            call real_ab(QInvS12, Qem, InvS12)
            do k = 1, NVecsT2
                  Qem(:, k) = QInvS12(:, k) * Sqrt(-Ak(k))
            end do
      end subroutine rpa_THC_CC_T2_Decompose



      subroutine rpa_THC_CC_T2_Decompose_v3(Qem, Yea, Xei, Pivots, Yga, Xgi, Uaim, Am, NOcc, NVirt, &
            NVecsT2, NGridTHC, Thresh)
            
            real(F64), dimension(:, :), allocatable, intent(out)   :: Qem
            real(F64), dimension(:, :), allocatable, intent(out)   :: Yea
            real(F64), dimension(:, :), allocatable, intent(out)   :: Xei
            integer, dimension(:), allocatable, intent(out)        :: Pivots
            real(F64), dimension(:, :), intent(in)                 :: Yga
            real(F64), dimension(:, :), intent(in)                 :: Xgi
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in) :: Uaim
            real(F64), dimension(NVecsT2), intent(in)              :: Am
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: NVecsT2
            integer, intent(in)                                    :: NGridTHC
            real(F64), intent(in)                                  :: Thresh

            real(F64), dimension(:, :), allocatable :: Sgh
            real(F64), dimension(:, :), allocatable :: XXgh, YYgh
            type(TClock) :: timer
            integer :: k, a, i
            integer :: NumericalRank
            integer :: NPivots

            real(F64), dimension(:), allocatable :: NX, NY
            real(F64), dimension(:, :), allocatable :: YgaN, XgiN
            integer :: g

            allocate(NX(NGridTHC))
            allocate(NY(NGridTHC))
            do g = 1, NGridTHC
                  NY(g) = norm2(Yga(g, :))
                  NX(g) = norm2(Xgi(g, :))
            end do
            allocate(YgaN(NGridTHC, NVirt))
            allocate(XgiN(NGridTHC, NOcc))
            do g = 1, NGridTHC
                  YgaN(g, :) = Yga(g, :) / NY(g)
                  XgiN(g, :) = Xgi(g, :) / NX(g)
            end do

            call msg("Performing pivoted QR for the least-squares THC fit of T2")
            call msg("Pivoted Cholesky threshold: " // str(Thresh,d=1))
            call clock_start(timer)
            allocate(Sgh(NGridTHC, NGridTHC))
            allocate(XXgh(NGridTHC, NGridTHC))
            allocate(YYgh(NGridTHC, NGridTHC))
            call real_abT(XXgh, XgiN, XgiN)
            call real_abT(YYgh, YgaN, YgaN)
            Sgh = XXgh * YYgh
            allocate(Qem(NGridTHC, NVecsT2))
            Qem = ZERO
            !$omp parallel do default(shared) &
            !$omp private(k, a, i)
            do k = 1, NVecsT2
                  do i = 1, NOcc
                        do a = 1, NVirt
                              Qem(:, k) = Qem(:, k) + YgaN(:, a) * XgiN(:, i) * Uaim(a, i, k)
                        end do
                  end do
            end do
            !$omp end parallel do
            call real_LeastSquares(Qem, NumericalRank, Sgh, Thresh)
            allocate(Pivots(NGridTHC))
            NPivots = NGridTHC
            do k = 1, NGridTHC
                  Pivots(k) = k
            end do
            allocate(Yea(NGridTHC, NVirt))
            allocate(Xei(NGridTHC, NOcc))
            Yea = Yga
            Xei = Xgi
            do k= 1, NVecsT2
                  Qem(:, k) = Qem(:, k) * Sqrt(-Am(k))
            end do
            do g = 1, NGridTHC
                  Qem(g, :) = Qem(g, :) / (NX(g) * NY(g))
            end do
      end subroutine rpa_THC_CC_T2_Decompose_v3


      subroutine rpa_THC_CC_T2_Decompose_v2(Qem, Yea, Xei, Pivots, Yga, Xgi, Uaim, Am, NOcc, NVirt, &
            NVecsT2, NGridTHC, QRThresh)
            
            real(F64), dimension(:, :), allocatable, intent(out)   :: Qem
            real(F64), dimension(:, :), allocatable, intent(out)   :: Yea
            real(F64), dimension(:, :), allocatable, intent(out)   :: Xei
            integer, dimension(:), allocatable, intent(out)        :: Pivots
            real(F64), dimension(:, :), intent(in)                 :: Yga
            real(F64), dimension(:, :), intent(in)                 :: Xgi
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in) :: Uaim
            real(F64), dimension(NVecsT2), intent(in)              :: Am
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: NVecsT2
            integer, intent(in)                                    :: NGridTHC
            real(F64), intent(in)                                  :: QRThresh

            integer :: NPivots
            type(TClock) :: timer
            real(F64) :: t_decomp

            real(F64), dimension(:), allocatable :: NX, NY
            real(F64), dimension(:, :), allocatable :: YgaN, XgiN
            integer :: g, e

            allocate(NX(NGridTHC))
            allocate(NY(NGridTHC))
            do g = 1, NGridTHC
                  NY(g) = norm2(Yga(g, :))
                  NX(g) = norm2(Xgi(g, :))
            end do
            allocate(YgaN(NGridTHC, NVirt))
            allocate(XgiN(NGridTHC, NOcc))
            do g = 1, NGridTHC
                  YgaN(g, :) = Yga(g, :) / NY(g)
                  XgiN(g, :) = Xgi(g, :) / NX(g)
            end do

            call clock_start(timer)
            call msg("Performing pivoted QR for the least-squares THC fit of T2")
            call msg("Pivoted Cholesky threshold: " // str(QRThresh,d=1))
            call rpa_THC_T2_Grid(Yea, Xei, NPivots, Pivots, YgaN, XgiN, QRThresh)
            call msg("Base grid: " // str(NGridTHC) // " points")
            call msg("T2 pivots: " // str(NPivots)  // " points selected via pivoted Cholesky")
            call rpa_THC_CC_T2_Decompose(Qem, Yea, Xei, Uaim, Am, NOcc, NVirt, &
                  NVecsT2, NPivots)
            t_decomp = clock_readwall(timer)
            call msg("Decomposisition of T2 done in " // str(t_decomp,d=1) // " seconds")
            
            do e = 1, NPivots
                  g = Pivots(e)
                  Qem(e, :) = Qem(e, :) / (NX(g) * NY(g))
                  Yea(e, :) = Yea(e, :) * NY(g)
                  Xei(e, :) = Xei(e, :) * NX(g)
            end do
      end subroutine rpa_THC_CC_T2_Decompose_v2
      

      subroutine rpa_THC_T2_Grid(Y2ga, X2gi, NPivots, Pivots, Yga, Xgi, QRThresh)
            real(F64), dimension(:, :), allocatable, intent(out) :: Y2ga
            real(F64), dimension(:, :), allocatable, intent(out) :: X2gi
            integer, intent(out)                                 :: NPivots
            integer, dimension(:), allocatable, intent(out)      :: Pivots
            real(F64), dimension(:, :), intent(in)               :: Yga
            real(F64), dimension(:, :), intent(in)               :: Xgi
            real(F64), intent(in)                                :: QRThresh

            real(F64), dimension(:, :), allocatable :: YYXX, XX
            integer :: g, a, i, NOcc, NVirt
            real(F64) :: CholThresh, MaxDiag
            integer :: NGridTHC

            NOcc = size(Xgi, dim=2)
            NVirt = size(Yga, dim=2)
            NGridTHC = size(Yga, dim=1)
            allocate(YYXX(NGridTHC, NGridTHC))
            allocate(XX(NGridTHC, NGridTHC))
            allocate(Pivots(NGridTHC))
            call real_abT(YYXX, Yga, Yga)
            call real_abT(XX, Xgi, Xgi)
            YYXX = YYXX * XX
            MaxDiag = ZERO
            do g = 1, NGridTHC
                  MaxDiag = max(MaxDiag, YYXX(g, g))
            end do
            CholThresh = MaxDiag * QRThresh**2
            call real_PivotedCholesky(YYXX, Pivots, NPivots, CholThresh)
            allocate(Y2ga(NPivots, NVirt), X2gi(NPivots, NOcc))
            do a = 1, NVirt
                  do g = 1, NPivots
                        Y2ga(g, a) = Yga(Pivots(g), a)
                  end do
            end do
            do i = 1, NOcc
                  do g = 1, NPivots
                        X2gi(g, i) = Xgi(Pivots(g), i)
                  end do
            end do
      end subroutine rpa_THC_T2_Grid
      
      
      subroutine rpa_THC_CC_T2_Decompose_Test(Zgk, Yga, Xgi, Uaim, Ak, NOcc, NVirt, NVecsT2, NGridTHC)
            real(F64), dimension(:, :), intent(in)                 :: Zgk
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

            allocate(Vaik(NVecsT2))
            allocate(Vbjk(NVecsT2))
            allocate(XaiGamma(NGridTHC))
            allocate(XbjDelta(NGridTHC))
            allocate(XaiZ(NVecsT2))
            allocate(XbjZ(NVecsT2))
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
                                    T2Exact = dot_product(Vaik, Vbjk)
                                    XaiZ = matmul(XaiGamma, Zgk)
                                    XbjZ = matmul(XbjDelta, Zgk)
                                    T2Approx = dot_product(XaiZ, XbjZ)
                                    if (abs(T2Exact) > 1.0E-3_F64) then
                                          print *, T2Exact, T2Approx, Abs(T2Exact-T2Approx)
                                    end if
                              end do
                        end do
                  end do
            end do
      end subroutine rpa_THC_CC_T2_Decompose_Test
end module rpa_CC_Doubles
