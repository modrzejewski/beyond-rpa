module rpa_CCD_Corrections
      use arithmetic      
      use real_linalg
      use rpa_definitions
      use rpa_MeanField
      use rpa_CCD_Corrections_Experimental
      use rpa_CCS_Corrections
      use rpa_PT_Terms
      use clock
      use display
      
      implicit none
      
contains

      subroutine rpa_Corrections(Energy, Zgh, Zgk, Yga, Xgi, hHFai, OccEnergies, VirtEnergies, &
            Uaim, Am, NOcc, NVirt, NVecsT2, NGridTHC, CumulantApprox, T2EigenvalueThresh)
            
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: NVecsT2
            integer, intent(in)                                    :: NGridTHC
            real(F64), dimension(:), intent(inout)                 :: Energy
            real(F64), dimension(:, :), intent(in)                 :: Zgh
            real(F64), dimension(:, :), intent(in)                 :: Zgk
            real(F64), dimension(NGridTHC, NVirt), intent(in)      :: Yga
            real(F64), dimension(NGridTHC, NOcc), intent(in)       :: Xgi
            real(F64), dimension(NVirt, NOcc), intent(in)          :: hHFai
            real(F64), dimension(NOcc), intent(in)                 :: OccEnergies
            real(F64), dimension(NVirt), intent(in)                :: VirtEnergies
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in) :: Uaim
            real(F64), dimension(:), intent(in)                    :: Am
            integer, intent(in)                                    :: CumulantApprox
            real(F64), intent(in)                                  :: T2EigenvalueThresh

            real(F64), dimension(:, :), allocatable :: YXUggm
            integer :: mu0, mu1, NVecsT2_Significant
            type(TClock) :: timer_total, timer
            integer, parameter :: BlockDim = 300
            logical, parameter :: Compute_1b2g = .true.
            logical, parameter :: Compute_2bc = .true.
            logical, parameter :: Compute_2mnop = .false.

            if (CumulantApprox == RPA_CUMULANT_LEVEL_5_HALF_THC) then
                  !
                  ! Experimental code
                  !
                  call rpa_CCD_corrections_FullSet(Energy, Zgh, Zgk, Yga, Xgi, OccEnergies, VirtEnergies, &
                        Uaim, Am, NOcc, NVirt, NVecsT2, NGridTHC, size(Zgk, dim=2))
            end if            
            call msg("CCD corrections to RPA correlation energy")
            call clock_start(timer_total)
            !
            ! Compute the range of significant T2 eigenvalues
            ! Significant eigenvalues: mu in (mu0, mu1)
            ! Note that the first eigenvalue is the one most negative
            !
            ! mu0 = 1
            ! call rpa_CCD_NVecsT2(mu1, Am, T2EigenvalueThresh)
            ! NVecsT2_Significant = mu1 - mu0 + 1
            ! call msg("Total number of computed eigenvalues A(mu): " // str(NVecsT2))
            ! call msg("Condition for significant A(mu): Abs(A(mu)/A(1)) > " // str(T2EigenvalueThresh,d=1))
            ! if (NVecsT2_Significant == NVecsT2) then
            !       call msg("All eigenvalue/eigenvector pairs have been accepted as significant")
            ! else
            !       call msg("Number of significant eigenvalues A(mu): " // str(mu1-mu0+1))
            !       call msg("Fraction of eigenvectors used for the beyond-RPA terms: " &
            !             // str(real(NVecsT2_Significant,F64)/real(NVecsT2,F64),d=2))
            ! end if
            mu0 = 1
            NVecsT2_Significant = NVecsT2
            mu1 = NVecsT2_Significant
            if (Compute_1b2g) then
                  call clock_start(timer)
                  call rpa_CCD_corrections_1b2gmnop(Energy, Zgh, Xgi, Yga, Uaim(:, :, mu0:mu1), &
                        Am(mu0:mu1), hHFai, OccEnergies, VirtEnergies, BlockDim, Compute_2bc, &
                        Compute_2mnop, YXUggm)
                  call msg("SOSEX+2g computed in " // str(clock_readwall(timer),d=1) // " seconds")
            end if
            if (Compute_2bc) then
                  call clock_start(timer)
                  call rpa_CCD_corrections_2bc(Energy, Zgh, Xgi, Yga, Uaim(:, :, mu0:mu1), &
                        Am(mu0:mu1), YXUggm)
                  call msg("2b+2c computed in " // str(clock_readwall(timer),d=1) // " seconds")
            end if
            !
            ! Rescale the energy terms to get the correct MBPT prefactors.
            ! After scaling by 1/2, the 1b term is equivalent to SOSEX.
            !
            Energy(RPA_ENERGY_CUMULANT_1B) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_1B)
            Energy(RPA_ENERGY_CUMULANT_2B) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_2B)
            Energy(RPA_ENERGY_CUMULANT_2C) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_2C)
            call msg("CCD corrections computed in " // str(clock_readwall(timer_total),d=1) // " seconds")
      end subroutine rpa_Corrections

      
      subroutine rpa_CCD_corrections_1b2gmnop(Energy, Zgh, Xgi, Yga, Uaim, &
            Am, hHFai, OccEnergies, VirtEnergies, BlockDim, Intermediates_2bcd, &
            Compute_2mnop, YXUggm)
            
            real(F64), dimension(:), intent(inout)                 :: Energy
            real(F64), dimension(:, :), intent(in)                 :: Zgh
            real(F64), dimension(:, :), intent(in)                 :: Xgi
            real(F64), dimension(:, :), intent(in)                 :: Yga
            real(F64), dimension(:, :, :), intent(in)              :: Uaim
            real(F64), dimension(:), intent(in)                    :: Am
            real(F64), dimension(:, :), intent(in)                 :: hHFai
            real(F64), dimension(:), intent(in)                    :: OccEnergies
            real(F64), dimension(:), intent(in)                    :: VirtEnergies
            integer, intent(in)                                    :: BlockDim
            logical, intent(in)                                    :: Intermediates_2bcd
            logical, intent(in)                                    :: Compute_2mnop
            real(F64), dimension(:, :), allocatable, intent(out)   :: YXUggm

            real(F64), dimension(:, :), allocatable :: YXUgh
            real(F64), dimension(:, :), allocatable :: YUgi
            !
            ! -------------------- Intermediates for 2m, 2n, 2o, 2p ------------------------
            !
            real(F64), dimension(:, :), allocatable :: Tai
            real(F64), dimension(:, :), allocatable :: UTab_transposed, TUij_transposed
            real(F64), dimension(:, :), allocatable :: XZYXUXij, YZYXUYab
            real(F64), dimension(:, :), allocatable :: ZYXUXgi, ZYXUYga
            real(F64), dimension(:, :), allocatable :: XTUgi, YUTga
            real(F64), dimension(:), allocatable :: XXTUg, YYUTg
            real(F64) :: Ec2m, Ec2n, Ec2o, Ec2p
            real(F64) :: S2m, S2n, S2o, S2p
            ! ------------------------------------------------------------------------------
            integer :: mu, g
            real(F64) :: Ec1b, Ec2g
            real(F64) :: S1b, S2g
            integer :: NVecsT2, NGridTHC, NOcc, NVirt
            real(F64) :: t_YU, t_YXU, t_Z_YXU_YXU, t_ZYXU            
            type(TClock) :: timer
            logical :: CCS_T1 = .true.

            NGridTHC = size(Zgh, dim=1)
            NVecsT2 = size(Am)
            NOcc = size(Xgi, dim=2)
            NVirt = size(Yga, dim=2)
            allocate(YXUgh(NGridTHC, NGridTHC))
            allocate(YUgi(NGridTHC, NOcc))
            if (Intermediates_2bcd .or. Compute_2mnop) then
                  allocate(YXUggm(NGridTHC, NVecsT2))
            end if
            if (Compute_2mnop) then
                  allocate(Tai(NVirt, NOcc))
                  allocate(TUij_transposed(NOcc, NOcc))
                  allocate(UTab_transposed(NVirt, NVirt))
                  allocate(ZYXUXgi(NGridTHC, NOcc))
                  allocate(ZYXUYga(NGridTHC, NVirt))
                  allocate(XZYXUXij(NOcc, NOcc))
                  allocate(YZYXUYab(NVirt, NVirt))
                  allocate(XTUgi(NGridTHC, NOcc))
                  allocate(YUTga(NGridTHC, NVirt))
                  allocate(XXTUg(NGridTHC))
                  allocate(YYUTg(NGridTHC))
                  if (CCS_T1) then
                        call rpa_CCS_T1(Tai, hHFai, OccEnergies, VirtEnergies, NOcc, NVirt)
                  else
                        call rpa_PT1_T1(Tai, hHFai, OccEnergies, VirtEnergies, NOcc, NVirt)
                  end if
            end if
            t_YU = ZERO
            t_YXU = ZERO
            t_Z_YXU_YXU = ZERO
            t_ZYXU = ZERO
            Ec1b = ZERO
            Ec2g = ZERO
            Ec2m = ZERO
            Ec2n = ZERO
            Ec2o = ZERO
            Ec2p = ZERO
            do mu = 1, NVecsT2
                  !
                  ! [YU](gamma,i,mu) = Sum(a) Y(gamma,a)*U(a,i,mu)
                  !
                  call clock_start(timer)
                  call real_ab_x(YUgi, NGridTHC, Yga, NGridTHC, Uaim(:, :, mu), &
                        NVirt, NGridTHC, NOcc, NVirt, ONE, ZERO)
                  t_YU = t_YU + clock_readwall(timer)
                  !
                  ! [YXU](gamma,delta,mu) = Sum(i) X(delta,i)*[YU](gamma,i,mu)
                  !
                  call clock_start(timer)
                  call real_abT(YXUgh, YUgi, Xgi)
                  t_YXU = t_YXU + clock_readwall(timer)
                  call clock_start(timer)
                  call Zgh_YXUgh_YXUgh(S2g, S1b, Zgh, YXUgh, NGridTHC, BlockDim)
                  t_Z_YXU_YXU = t_Z_YXU_YXU + clock_readwall(timer)
                  Ec2g = Ec2g - TWO * S2g * Am(mu)**2
                  Ec1b = Ec1b - S1b * Am(mu)
                  if (Intermediates_2bcd .or. Compute_2mnop) then
                        !$omp parallel do private(g)                       
                        do g = 1, NGridTHC
                              YXUggm(g, mu) = YXUgh(g, g)
                        end do
                        !$omp end parallel do
                  end if
                  if (Compute_2mnop) then
                        call real_abT(UTab_transposed, Tai, Uaim(:, :, mu)) ! [UT](ab;mu) = Sum(i) U(ai,mu) T(b,i)
                        call real_aTb(TUij_transposed, Uaim(:, :, mu), Tai) ! [TU](ij;mu) = Sum(a) T(a,i) U(aj,mu)
                        call rpa_CCS_hadamard_Z_YXU(YXUgh, Zgh, NGridTHC)   ! [Z.YXU](g,h) = Z(g,h)*[YXU](g,h)                  
                        associate (Z_YXUgh => YXUgh)
                              call rpa_CCS_intermediate_WZYXUW(YZYXUYab, ZYXUYga, Z_YXUgh, Yga, NGridTHC, NVirt)
                              call real_vw_x(S2n, YZYXUYab, UTab_transposed, NVirt**2)
                              call rpa_CCS_intermediate_WWUT(YYUTg, YUTga, UTab_transposed, Yga, NGridTHC, NVirt)
                              call rpa_CCS_YXU_Z_WW_UT(S2m, YXUggm(:, mu), Zgh, YYUTg, NGridTHC)
                              !
                              call rpa_CCS_intermediate_WZYXUW(XZYXUXij, ZYXUXgi, Z_YXUgh, Xgi, NGridTHC, NOcc)
                              call real_vw_x(S2p, XZYXUXij, TUij_transposed, NOcc**2)
                              call rpa_CCS_intermediate_WWUT(XXTUg, XTUgi, TUij_transposed, Xgi, NGridTHC, NOcc)
                              call rpa_CCS_YXU_Z_WW_UT(S2o, YXUggm(:, mu), Zgh, XXTUg, NGridTHC)
                        end associate
                        Ec2m = Ec2m + EIGHT * Am(mu) * S2m
                        Ec2n = Ec2n - FOUR  * Am(mu) * S2n
                        Ec2o = Ec2o - EIGHT * Am(mu) * S2o
                        Ec2p = Ec2p + FOUR  * Am(mu) * S2p 
                  end if
            end do
            !
            ! Symmetry factors due to the permuation symmetry of the Coulomb integrals
            ! and the cumulant.
            !
            ! The second-order exchange term 1b is scaled by 2 due to the permutation
            ! symmetry. If only T(Lambda=1) amplitudes are used, Ec1b should be rescaled
            ! by 1/2 afterwards to get a formula equivalent to SOSEX.
            !
            Ec2g = TWO * Ec2g
            Ec1b = TWO * Ec1b
            !
            Energy(RPA_ENERGY_CUMULANT_1B) = Ec1b
            Energy(RPA_ENERGY_CUMULANT_2G) = Ec2g
            if (Compute_2mnop) then
                  Energy(RPA_ENERGY_CUMULANT_2M) = Ec2m
                  Energy(RPA_ENERGY_CUMULANT_2N) = Ec2n
                  Energy(RPA_ENERGY_CUMULANT_2O) = Ec2o
                  Energy(RPA_ENERGY_CUMULANT_2P) = Ec2p
            end if
            call msg("SOSEX+2g timings (seconds)", underline=.true.)
            call msg("[YU]          " // str(t_YU,d=1))
            call msg("[YXU]         " // str(t_YXU,d=1))
            call msg("Z*[YXU]*[YXU] " // str(t_Z_YXU_YXU,d=1))

      contains

            subroutine Zgh_YXUgh_YXUgh(S2g, S1b, Zgh, YXUgh, NGridTHC, BlockDim)
                  real(F64), intent(out)                 :: S2g
                  real(F64), intent(out)                 :: S1b
                  real(F64), dimension(:, :), intent(in) :: Zgh
                  real(F64), dimension(:, :), intent(in) :: YXUgh
                  integer, intent(in)                    :: NGridTHC
                  integer, intent(in)                    :: BlockDim

                  integer :: NBlocks, NBlockPairs
                  integer :: p, q, pq
                  integer :: g0, g1, h0, h1
                  integer :: M, N
                  real(F64) :: S2gPQ, S1bPQ
                  real(F64), dimension(BlockDim**2) :: W0, W1, W2, W3

                  S2g = ZERO
                  S1b = ZERO
                  NBlocks = NGridTHC / BlockDim
                  if (modulo(NGridTHC, BlockDim) > 0) NBlocks = NBlocks + 1
                  NBlockPairs = (NBlocks * (NBlocks + 1)) / 2
                  !$omp parallel do &
                  !$omp private(pq, p, q, M, N, g0, g1, h0, h1) &
                  !$omp private(W0, W1, W2, W3) &
                  !$omp private(S2gPQ, S1bPQ) &
                  !$omp reduction(+:S2g, S1b) &
                  !$omp default(shared)
                  do pq = 1, NBlockPairs
                        call rpa_Cumulant_block_indices(p, q, pq, NBlocks)
                        g0 = 1 + (p - 1) * BlockDim
                        g1 = min(p * BlockDim, NGridTHC)
                        h0 = 1 + (q - 1) * BlockDim
                        h1 = min(q * BlockDim, NGridTHC)
                        M = g1 - g0 + 1
                        N = h1 - h0 + 1
                        if (p /= q) then
                              call block_multiply_offdiag(S2gPQ, S1bPQ, W0, W1, W2, W3, Zgh, YXUgh, &
                                    M, N, g0, g1, h0, h1)
                        else
                              call block_multiply_diag(S2gPQ, S1bPQ, W0, W1, W2, Zgh, YXUgh, &
                                    M, N, g0, g1, h0, h1)
                        end if
                        S2g = S2g + S2gPQ
                        S1b = S1b + S1bPQ
                  end do
                  !$omp end parallel do
            end subroutine Zgh_YXUgh_YXUgh
            
            subroutine block_multiply_offdiag(S2g, S1b, W0, W1, W2, W3, Zgh, YXUgh, M, N, g0, g1, h0, h1)
                  real(F64), intent(out)                  :: S2g
                  real(F64), intent(out)                  :: S1b
                  real(F64), dimension(M, N), intent(out) :: W0
                  real(F64), dimension(M, N), intent(out) :: W1
                  real(F64), dimension(N, M), intent(out) :: W2
                  real(F64), dimension(M, N), intent(out) :: W3
                  real(F64), dimension(:, :), intent(in)  :: Zgh
                  real(F64), dimension(:, :), intent(in)  :: YXUgh
                  integer, intent(in)                     :: M, N
                  integer, intent(in)                     :: g0, g1
                  integer, intent(in)                     :: h0, h1

                  W0 = Zgh(g0:g1, h0:h1)
                  W1 = YXUgh(g0:g1, h0:h1)
                  W2 = YXUgh(h0:h1, g0:g1)
                  W3 = transpose(W2)
                  call block_multiply_loop_1(S2g, S1b, W0, W1, W3, M*N)
            end subroutine block_multiply_offdiag

            subroutine block_multiply_diag(S2g, S1b, W0, W1, W2, Zgh, YXUgh, M, N, g0, g1, h0, h1)
                  real(F64), intent(out)                  :: S2g
                  real(F64), intent(out)                  :: S1b
                  real(F64), dimension(M, N), intent(out) :: W0
                  real(F64), dimension(M, N), intent(out) :: W1
                  real(F64), dimension(M, N), intent(out) :: W2
                  real(F64), dimension(:, :), intent(in)  :: Zgh
                  real(F64), dimension(:, :), intent(in)  :: YXUgh
                  integer, intent(in)                     :: M, N
                  integer, intent(in)                     :: g0, g1
                  integer, intent(in)                     :: h0, h1

                  W0 = Zgh(g0:g1, h0:h1)
                  W1 = YXUgh(g0:g1, h0:h1)
                  W2 = transpose(W1)
                  call block_multiply_loop_2(S2g, S1b, W0, W1, W2, M*N)
            end subroutine block_multiply_diag

            subroutine block_multiply_loop_1(S2g, S1b, Z, YXU, YXU_T, L)
                  real(F64), intent(out)  :: S2g, S1b
                  real(F64), dimension(L) :: Z
                  real(F64), dimension(L) :: YXU
                  real(F64), dimension(L) :: YXU_T
                  integer, intent(in)     :: L
                  
                  integer :: k

                  S2g = ZERO
                  S1b = ZERO
                  do k = 1, L
                        S2g = S2g + Z(k) * (YXU(k)**2 + YXU_T(k)**2)
                        S1b = S1b + Z(k) * YXU(k) * YXU_T(k)
                  end do
                  S1b = TWO * S1b
            end subroutine block_multiply_loop_1

            subroutine block_multiply_loop_2(S2g, S1b, Z, YXU, YXU_T, L)
                  real(F64), intent(out)  :: S2g, S1b
                  real(F64), dimension(L) :: Z
                  real(F64), dimension(L) :: YXU
                  real(F64), dimension(L) :: YXU_T
                  integer, intent(in)     :: L
                  
                  integer :: k

                  S2g = ZERO
                  S1b = ZERO
                  do k = 1, L
                        S2g = S2g + Z(k) * YXU(k)**2
                        S1b = S1b + Z(k) * YXU(k) * YXU_T(k)
                  end do
            end subroutine block_multiply_loop_2            
      end subroutine rpa_CCD_corrections_1b2gmnop


      pure subroutine rpa_Cumulant_block_indices(p, q, pq, n)
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
      end subroutine rpa_Cumulant_block_indices


      subroutine rpa_CCD_Intermediate_UYUXU(UYUXUmg, Xgi, Yga, Uaim, Am)
            real(F64), dimension(:, :), intent(out)                :: UYUXUmg
            real(F64), dimension(:, :), intent(in)                 :: Xgi
            real(F64), dimension(:, :), intent(in)                 :: Yga
            real(F64), dimension(:, :, :), intent(in)              :: Uaim
            real(F64), dimension(:), intent(in)                    :: Am

            integer :: mu, g
            integer :: NVecsT2, NGridTHC, NOcc, NVirt
            real(F64), dimension(:, :), allocatable :: YUim, XUam, YUXUai
            real(F64), dimension(:, :), allocatable :: Xig, Yag

            NGridTHC = size(Xgi, dim=1)
            NVecsT2 = size(Am)
            NOcc = size(Xgi, dim=2)
            NVirt = size(Yga, dim=2)
            allocate(YUim(NOcc, NVecsT2))
            allocate(XUam(NVirt, NVecsT2))
            allocate(YUXUai(NVirt, NOcc))
            allocate(Xig(NOcc, NGridTHC))
            allocate(Yag(NVirt, NGridTHC))
            Xig = transpose(Xgi)
            Yag = transpose(Yga)
            do g = 1, NGridTHC
                  !$omp parallel do private(mu) default(shared)
                  do mu = 1, NVecsT2
                        !
                        ! [YU](i,mu;g) = Sum(a) Y(a,g)*U(a,i,mu)
                        ! YU(1:NOcc,mu;g) = U(1:NVirt,1:NOcc,mu)**T Y(1:NVirt, g)
                        !
                        call real_aTv_x(YUim(:, mu), Uaim(:, :, mu), NVirt, Yag(:, g), NVirt, NOcc, ONE, ZERO)
                        !
                        ! [XU](a,mu;g) = Sum(i) X(i,g)*U(a,i,mu)
                        ! [XU](1:NVirt,mu;g) = U(1:NVirt,1:NOcc,mu) X(1:NOcc, g)
                        ! Note that [XU] is scaled by the eigenvalue of T2 a(mu)
                        !
                        call real_av_x(XUam(:, mu), Uaim(:, :, mu), NVirt, Xig(:, g), NVirt, NOcc, Am(mu), ZERO)
                  end do
                  !$omp end parallel do
                  !
                  ! [YUXU](a,i;g) = Sum(mu) [XU](a,mu;g)*[YU](i,mu;g)
                  !
                  call real_abT(YUXUai, XUam, YUim)
                  !
                  ! [UYUXU](mu,g) = Sum(ai) U(a,i,mu)*[YUXU](a,i;g)
                  ! This operation can be executed as a matrix-vector multiplication
                  ! UYUXU(1:NVecsT2,g) = U(1:NVirt*NOcc,1:NVecsT2)**T * YUXU(1:NVirt*NOcc)
                  !
                  call real_aTv_x(UYUXUmg(:, g), Uaim, NVirt*NOcc, YUXUai, NVirt*NOcc, NVecsT2, ONE, ZERO)
            end do
      end subroutine rpa_CCD_Intermediate_UYUXU


      subroutine rpa_CCD_corrections_2bc(Energy, Zgh, Xgi, Yga, Uaim, Am, YXUggm)
            real(F64), dimension(:), intent(inout)                 :: Energy
            real(F64), dimension(:, :), intent(in)                 :: Zgh
            real(F64), dimension(:, :), intent(in)                 :: Xgi
            real(F64), dimension(:, :), intent(in)                 :: Yga
            real(F64), dimension(:, :, :), intent(in)              :: Uaim
            real(F64), dimension(:), intent(in)                    :: Am
            real(F64), dimension(:, :), allocatable, intent(inout) :: YXUggm

            real(F64), dimension(:, :), allocatable :: ZYXUmg, UYUXUmg
            integer :: mu
            real(F64) :: S2b
            integer :: NGridTHC, NVecsT2

            NGridTHC = size(Zgh, dim=1)
            NVecsT2 = size(Uaim, dim=3)
            allocate(ZYXUmg(NVecsT2, NGridTHC))
            do mu = 1, NVecsT2
                  YXUggm(:, mu) = Am(mu) * YXUggm(:, mu)
            end do
            call real_aTb(ZYXUmg, YXUggm, Zgh)
            deallocate(YXUggm)
            !
            ! [UYUXU](mu,g) = Sum(ai,nu) U(ai,mu)*[YU](g,i,nu)*[XU](a,g,nu)*a(nu)
            !
            allocate(UYUXUmg(NVecsT2, NGridTHC))
            call rpa_CCD_Intermediate_UYUXU(UYUXUmg, Xgi, Yga, Uaim, Am)
            !
            ! S2b = Sum(g,mu) [ZYXU](mu,g)*[UYUXU](mu,g)
            !
            call real_vw_x(S2b, ZYXUmg, UYUXUmg, NVecsT2*NGridTHC)            
            !
            ! The symmetry factor due to the permuation symmetry of the Coulomb integrals
            ! and the cumulant is included.
            !
            ! Ec2b + Ec2c should be rescaled by 1/2 after return from this
            ! subroutine to get the correct third-order diagram originating from
            ! SOSEX+2b+2c
            !
            Energy(RPA_ENERGY_CUMULANT_2B) = -FOUR * S2b
            Energy(RPA_ENERGY_CUMULANT_2C) = -FOUR * S2b
      end subroutine rpa_CCD_corrections_2bc

      
      subroutine rpa_CCD_NVecsT2(NVecsT2, Am, T2Thresh)
            integer, intent(out)                :: NVecsT2
            real(F64), dimension(:), intent(in) :: Am
            real(F64), intent(in)               :: T2Thresh

            integer :: k, NVecsT2_All

            NVecsT2_All = size(Am)
            NVecsT2 = 1
            if (abs(Am(1)) > ZERO) then
                  do k = 2, NVecsT2_All
                        if (abs(Am(k)/Am(1)) > T2Thresh) then
                              NVecsT2 = k
                        else
                              exit
                        end if
                  end do
            else
                  call msg("Invalid eigenvalues of T2", MSG_ERROR)
                  error stop
            end if
      end subroutine rpa_CCD_NVecsT2
end module rpa_CCD_Corrections
