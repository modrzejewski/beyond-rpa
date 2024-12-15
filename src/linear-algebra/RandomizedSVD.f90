module RandomizedSVD
      use arithmetic
      use real_linalg
      use string
      use display
      
      implicit none

      type TRSVDWorkspace
            integer                                 :: FullDim
            integer                                 :: SubspaceDim
            integer                                 :: NSubspaceIters
            integer                                 :: Oversampling
            real(F64)                               :: AbsThresh
            integer                                 :: SwitchoverDim
            real(F64), dimension(:, :), allocatable :: Omega
            real(F64), dimension(:, :), allocatable :: VT
            real(F64), dimension(:), allocatable    :: SVD_Work
            integer, dimension(:), allocatable      :: SVD_IWork
            real(F64), dimension(:), allocatable    :: QR_Work
            real(F64), dimension(:, :), allocatable :: AQ
            real(F64), dimension(:, :), allocatable :: QTU
            real(F64), dimension(:, :), allocatable :: QTA
            real(F64), dimension(:, :), allocatable :: Q
      end type TRSVDWorkspace

contains

      subroutine rsvd_Init(RSVDWorkspace, FullDim, SubspaceDim, &
            NSubspaceIters, Oversampling, AbsThresh, SwitchoverDim)
            
            type(TRSVDWorkspace), intent(out)       :: RSVDWorkspace
            integer, intent(in)                     :: FullDim
            integer, intent(in)                     :: SubspaceDim
            integer, intent(in)                     :: NSubspaceIters
            integer, intent(in)                     :: Oversampling
            real(F64), intent(in)                   :: AbsThresh
            integer, intent(in)                     :: SwitchoverDim

            integer :: QR_LWork, k
            real(F64) :: Norm

            RSVDWorkspace%FullDim = FullDim
            RSVDWorkspace%NSubspaceIters = NSubspaceIters
            RSVDWorkspace%Oversampling = Oversampling
            RSVDWorkspace%AbsThresh = AbsThresh
            RSVDWorkspace%SwitchoverDim = min(FullDim, max(0, SwitchoverDim))
            if (FullDim <= 0) then
                  call msg("Invalid matrix dimension in rsvd_Init", MSG_ERROR)
                  error stop
            end if
            if (SubspaceDim <= 0) then
                  call msg("Invalid subspace dimension on entry to rsvd_Init", MSG_ERROR)
                  error stop
            end if
            RSVDWorkspace%SubspaceDim = min(FullDim, SubspaceDim)
            if (RSVDWorkspace%SubspaceDim >= RSVDWorkspace%SwitchoverDim) then
                  RSVDWorkspace%SubspaceDim = FullDim
                  call msg("RandomizedSVD will use conventional full-rank SVD")
            end if
            if (RSVDWorkspace%SubspaceDim < FullDim) then
                  !
                  ! Workspace query for the decomposition of matrix (A*Q)
                  ! of dimension FullDim x SubspaceDim
                  !
                  call real_QR_query(QR_LWork, FullDim, FullDim, &
                        RSVDWorkspace%SubspaceDim)
                  allocate(RSVDWorkspace%QR_Work(QR_LWork))
                  allocate(RSVDWorkspace%Omega(FullDim, RSVDWorkspace%SubspaceDim))
                  associate (Omega => RSVDWorkspace%Omega)
                        !
                        ! We assume that the random number generator is
                        ! already initialized
                        !            
                        call random_number(Omega)
                        Omega = Omega - ONE/TWO
                        do k = 1, RSVDWorkspace%SubspaceDim
                              Norm = norm2(Omega(:, k))
                              Omega(:, k) = Omega(:, k) / Norm
                        end do
                  end associate
                  !
                  ! Workspace query for the decomposition of matrix Q**T*A
                  ! of dimension SubspaceDim x FullDim
                  !
                  call real_SVD_workspace( &
                        RSVDWorkspace%VT, &
                        RSVDWorkspace%SVD_Work, &
                        RSVDWorkspace%SVD_IWork, &
                        RSVDWorkspace%SubspaceDim, &
                        FullDim)
                  allocate(RSVDWorkspace%AQ(FullDim, RSVDWorkspace%SubspaceDim))            
                  allocate(RSVDWorkspace%QTU(RSVDWorkspace%SubspaceDim, RSVDWorkspace%SubspaceDim))
                  allocate(RSVDWorkspace%QTA(RSVDWorkspace%SubspaceDim, FullDim))
                  allocate(RSVDWorkspace%Q(FullDim, RSVDWorkspace%SubspaceDim))
            end if
      end subroutine rsvd_Init


      subroutine rsvd_Decompose(U, V, Sigma, NVecs, A, RSVDWorkspace)
            !
            ! Randomized singular value decomposition
            !
            ! 1. N. Halko, P. G. Martinsson, and J. A. Tropp,
            !    Finding Structure with Randomness: Probabilistic
            !    Algorithms for Constructing Approximate Matrix Decompositions
            !    SIAM Review, 53, 217 (2011); doi: 10.1137/090771806
            !
            real(F64), dimension(:, :), intent(out)   :: U
            real(F64), dimension(:, :), intent(out)   :: V
            real(F64), dimension(:), intent(out)      :: Sigma
            integer, intent(out)                      :: NVecs
            real(F64), dimension(:, :), intent(in)    :: A
            type(TRSVDWorkspace), intent(inout)       :: RSVDWorkspace

            integer :: QR_LWork
            integer :: j
            integer :: FullDim, SubspaceDim, Oversampling, NSubspaceIters
            integer :: SwitchoverDim
            integer :: Info
            real(F64) :: AbsThresh
            logical :: FullRankSVD, AltFullRankSVD, IncreaseSubspaceDim
            real(F64), dimension(:, :), allocatable :: B

            FullDim = size(A, dim=1)
            if (FullDim /= RSVDWorkspace%FullDim) then
                  call msg("Invalid dimension of A in rsvd_Decompose", MSG_ERROR)
                  error stop
            end if
            if (FullDim > RSVDWorkspace%SubspaceDim) then
                  FullRankSVD = .false.
            else
                  FullRankSVD = .true.
            end if
            AltFullRankSVD = .false.
            IncreaseSubspaceDim = .false.
            AbsThresh = RSVDWorkspace%AbsThresh
            SubspaceDim = RSVDWorkspace%SubspaceDim
            Oversampling = RSVDWorkspace%Oversampling
            NSubspaceIters = RSVDWorkspace%NSubspaceIters
            SwitchoverDim = RSVDWorkspace%SwitchoverDim
            if (.not. FullRankSVD) then
                  associate ( &
                        QR_Work => RSVDWorkspace%QR_Work, &
                        SVD_Work => RSVDWorkspace%SVD_Work, &
                        SVD_IWork => RSVDWorkspace%SVD_IWork, &
                        Q => RSVDWorkspace%Q, &
                        VT => RSVDWorkspace%VT, &
                        AQ => RSVDWorkspace%AQ, &
                        QTU => RSVDWorkspace%QTU, &
                        QTA => RSVDWorkspace%QTA, &
                        Omega => RSVDWorkspace%Omega &
                        )
                        QR_LWork = size(QR_Work)
                        !
                        ! Algorithm 4.4: reduce the number of columns by
                        ! right multiplication with random numbers (Omega)
                        !
                        call real_ab(AQ, A, Omega)
                        !
                        ! Algorithm 4.4: generate orthogonal columns to initialize
                        ! the subspace iterations
                        !
                        call real_QR_x(AQ, FullDim, FullDim, SubspaceDim, QR_Work, QR_lwork)
                        Q(:, :) = AQ(:, :)
                        do j = 1, NSubspaceIters
                              !
                              ! Algorithm 4.4: compute (A*A**T)**NSubspaceIters with
                              ! intermediate QR decompositions to improve
                              ! numerical stability
                              !
                              call real_aTb(AQ, A, Q)
                              call real_QR_x(AQ, FullDim, FullDim, SubspaceDim, QR_Work, QR_lwork)
                              Q(:, :) = AQ(:, :)
                              call real_ab(AQ, A, Q)
                              call real_QR_x(AQ, FullDim, FullDim, SubspaceDim, QR_Work, QR_lwork)
                              Q(:, :) = AQ(:, :)
                        end do
                        !
                        ! At this point, matrix Q contains the orthogonal
                        ! columns which span the range
                        ! of matrix A, i.e., for any vector v,
                        !
                        ! Av = (approx.) Q Q**T A v 
                        !
                        ! Now we can proceed to phase B of Prototype
                        ! for Randomized SVD, page 227.
                        !
                        call real_aTb(QTA, Q, A)
                        call real_SVD_x(QTU, V(:, 1:SubspaceDim), Sigma(1:SubspaceDim), QTA, VT, &
                              SVD_Work, SVD_IWork, Info)
                        if (Info == 0) then
                              NVecs = count(Sigma(1:SubspaceDim) > AbsThresh)
                              if (NVecs+Oversampling <= SubspaceDim) then
                                    call real_ab(U(:, 1:SubspaceDim), Q, QTU)
                              else
                                    IncreaseSubspaceDim = .true.
                                    FullRankSVD = .true.
                              end if
                        end if
                        if (Info > 0) then
                              !
                              ! Info > 0 returned from the SVD subroutine means that
                              ! the algorithm did not converge. This can happen for small
                              ! singular values. The proper way to proceed
                              ! now is to retry SVD by using a different algorithm.
                              !
                              FullRankSVD = .true.
                        end if
                  end associate
            end if
            if (FullRankSVD) then
                  allocate(B(FullDim, FullDim))
                  B(:, :) = A(:, :)
                  call real_SVD(U, V, Sigma, B, Info)
                  if (Info > 0) then
                        AltFullRankSVD = .true.
                  else
                        NVecs = count(Sigma(1:SubspaceDim) > AbsThresh)
                  end if
            end if
            if (AltFullRankSVD) then
                  call real_SVD_SignificantSubset(U, V, Sigma, NVecs, &
                        B, AbsThresh, Info)
                  if (Info /= 0) then
                        call msg("SVD algorithm did not converge", MSG_ERROR)
                        error stop
                  end if
            end if
            if (IncreaseSubspaceDim) then
                  SubspaceDim = min(FullDim, NVecs + Oversampling)
                  call msg("Increasing SVD subspace dimension to " // str(SubspaceDim))
                  call rsvd_Init(RSVDWorkspace, FullDim, SubspaceDim, &
                        NSubspaceIters, Oversampling, AbsThresh, SwitchoverDim)
            end if
      end subroutine rsvd_Decompose
end module RandomizedSVD
