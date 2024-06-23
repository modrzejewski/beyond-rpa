module rpa_CCD_Corrections
      use arithmetic      
      use real_linalg
      use rpa_definitions
      use rpa_MeanField
      use rpa_CCD_Corrections_Experimental
      use rpa_JCTC2024
      use rpa_CCS_Corrections
      use rpa_CC_Doubles
      use rpa_Orbitals
      use rpa_PT_Terms
      use clock
      use display
      
      implicit none
      
contains

      subroutine rpa_Corrections(RPAOutput, Zgh, Zgk, Yga, Xgi, &
            Uaim, Am, Cpi, NOcc, NVirt, NVecsT2, NGridTHC, RPAParams, AOBasis)
            
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: NVecsT2
            integer, intent(in)                                    :: NGridTHC
            type(TRPAOutput), intent(inout)                        :: RPAOutput
            real(F64), dimension(:, :), intent(in)                 :: Zgh
            real(F64), dimension(:, :), intent(in)                 :: Zgk
            real(F64), dimension(NGridTHC, NVirt), intent(in)      :: Yga
            real(F64), dimension(NGridTHC, NOcc), intent(in)       :: Xgi
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in) :: Uaim
            real(F64), dimension(:), intent(in)                    :: Am
            real(F64), dimension(:, :), intent(in)                 :: Cpi
            type(TRPAParams), intent(in)                           :: RPAParams
            type(TAOBasis), intent(in)                             :: AOBasis

            real(F64), dimension(:, :), allocatable :: YXUggm
            type(TClock) :: timer_total, timer            
            integer, parameter :: BlockDim = 300
            logical, parameter :: Compute_1b2g = .true.
            logical, parameter :: Compute_2bcd = .true.

            if (RPAParams%TheoryLevel==RPA_THEORY_JCTC2024) then
                  call rpa_JCTC2024_Corrections(RPAOutput, Zgk, Xgi, Yga, Uaim, Am, Cpi, &
                        RPAParams, AOBasis)
            else if (RPAParams%TheoryLevel==RPA_THEORY_ALL) then
                  !
                  ! Warning: this code path allocates large matrices
                  !
                  call rpa_CCD_corrections_FullSet(RPAOutput%Energy, Zgh, Zgk, Yga, Xgi, &
                        Uaim, Am, NOcc, NVirt, NVecsT2, NGridTHC, size(Zgk, dim=2))                  
            else if (RPAParams%TheoryLevel==RPA_THEORY_JCTC2023) then
                  call msg("CCD corrections to RPA correlation energy")
                  call clock_start(timer_total)
                  if (Compute_1b2g) then
                        call clock_start(timer)
                        call rpa_CCD_corrections_1b2g(RPAOutput, Zgh, Xgi, Yga, Uaim, &
                              Am, BlockDim, Compute_2bcd, YXUggm)
                        call msg("SOSEX+2g computed in " // str(clock_readwall(timer),d=1) // " seconds")
                  end if
                  if (Compute_2bcd) then
                        call clock_start(timer)
                        call rpa_CCD_corrections_2bcd(RPAOutput%Energy, Zgh, Xgi, Yga, Uaim, Am, YXUggm)
                        call msg("2b+2c+2d computed in " // str(clock_readwall(timer),d=1) // " seconds")
                  end if
                  !
                  ! Rescale the energy terms to get the correct MBPT prefactors.
                  ! After scaling by 1/2, the 1b term is equivalent to SOSEX.
                  !
                  RPAOutput%Energy(RPA_ENERGY_CUMULANT_1B) = (ONE/TWO) * RPAOutput%Energy(RPA_ENERGY_CUMULANT_1B)
                  RPAOutput%Energy(RPA_ENERGY_CUMULANT_2B) = (ONE/TWO) * RPAOutput%Energy(RPA_ENERGY_CUMULANT_2B)
                  RPAOutput%Energy(RPA_ENERGY_CUMULANT_2C) = (ONE/TWO) * RPAOutput%Energy(RPA_ENERGY_CUMULANT_2C)
                  call msg("All CCD corrections computed in " // str(clock_readwall(timer_total),d=1) // " seconds")
            end if
      end subroutine rpa_Corrections

      
      subroutine rpa_CCD_corrections_1b2g(RPAOutput, Zgh, Xgi, Yga, Uaim, &
            Am, BlockDim, Intermediates_2bcd, YXUggm)
            
            type(TRPAOutput), intent(inout)                        :: RPAOutput
            real(F64), dimension(:, :), intent(in)                 :: Zgh
            real(F64), dimension(:, :), intent(in)                 :: Xgi
            real(F64), dimension(:, :), intent(in)                 :: Yga
            real(F64), dimension(:, :, :), intent(in)              :: Uaim
            real(F64), dimension(:), intent(in)                    :: Am
            integer, intent(in)                                    :: BlockDim
            logical, intent(in)                                    :: Intermediates_2bcd
            real(F64), dimension(:, :), allocatable, intent(out)   :: YXUggm

            real(F64), dimension(:, :), allocatable :: YXUgh
            real(F64), dimension(:, :), allocatable :: YUgi
            integer :: mu, g
            real(F64) :: Ec1b, Ec2g
            real(F64) :: S1b, S2g
            integer :: NVecsT2, NGridTHC, NOcc, NVirt
            real(F64) :: t_YU, t_YXU, t_Z_YXU_YXU, t_ZYXU, t_THC_T2          
            type(TClock) :: timer

            NGridTHC = size(Zgh, dim=1)
            NVecsT2 = size(Am)
            NOcc = size(Xgi, dim=2)
            NVirt = size(Yga, dim=2)
            allocate(RPAOutput%EigSOSEX(NVecsT2))
            allocate(RPAOutput%Eig2g(NVecsT2))
            allocate(YXUgh(NGridTHC, NGridTHC))
            allocate(YUgi(NGridTHC, NOcc))
            if (Intermediates_2bcd) then
                  allocate(YXUggm(NGridTHC, NVecsT2))
            end if
            t_YU = ZERO
            t_YXU = ZERO
            t_Z_YXU_YXU = ZERO
            t_ZYXU = ZERO
            t_THC_T2 = ZERO
            Ec1b = ZERO
            Ec2g = ZERO
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
                  ! ------------------------------------------------------------
                  ! Contributions to EcSOSEX and Ec2g originaing from A(mu).
                  ! ------------------------------------------------------------
                  RPAOutput%Eig2g(mu) = -TWO * TWO * S2g * Am(mu)**2
                  RPAOutput%EigSOSEX(mu) = -S1b * Am(mu)
                  !
                  if (Intermediates_2bcd) then
                        !$omp parallel do private(g)                       
                        do g = 1, NGridTHC
                              YXUggm(g, mu) = YXUgh(g, g)
                        end do
                        !$omp end parallel do
                  end if
            end do
            !
            ! Symmetry factors due to the permuation symmetry of the Coulomb integrals
            ! and the cumulant.
            !
            ! The second-order exchange term 1b is scaled by 2 due to the permutation
            ! symmetry. Ec1b should be rescaled by 1/2 afterwards to get the SOSEX energy
            !
            Ec2g = TWO * Ec2g
            Ec1b = TWO * Ec1b
            !
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_1B) = Ec1b
            RPAOutput%Energy(RPA_ENERGY_CUMULANT_2G) = Ec2g
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
      end subroutine rpa_CCD_corrections_1b2g


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


      subroutine rpa_CCD_corrections_2d(Energy, UYUXUmg, Xgi, Yga, Uaim, Am, Zgh)
            real(F64), dimension(:), intent(inout)                 :: Energy
            real(F64), dimension(:, :), intent(out)                :: UYUXUmg
            real(F64), dimension(:, :), intent(in)                 :: Xgi
            real(F64), dimension(:, :), intent(in)                 :: Yga
            real(F64), dimension(:, :, :), intent(in)              :: Uaim
            real(F64), dimension(:), intent(in)                    :: Am
            real(F64), dimension(:, :), intent(in)                 :: Zgh

            integer :: mu, g
            integer :: NVecsT2, NGridTHC, NOcc, NVirt
            real(F64), dimension(:, :), allocatable :: YUim, XUam
            real(F64), dimension(:, :), allocatable :: Xig, Yag
            real(F64), dimension(:, :), allocatable :: YUXUai
            real(F64), dimension(:, :, :), allocatable :: YUXUaig
            real(F64), dimension(:, :), allocatable :: YUXUYUXUgh
            real(F64) :: S2d

            NGridTHC = size(Xgi, dim=1)
            NVecsT2 = size(Am)
            NOcc = size(Xgi, dim=2)
            NVirt = size(Yga, dim=2)
            allocate(YUim(NOcc, NVecsT2))
            allocate(XUam(NVirt, NVecsT2))
            allocate(YUXUaig(NVirt, NOcc, NGridTHC))
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
                        call real_aTv_x(YUim(:, mu), Uaim(:, :, mu), NVirt, Yag(:, g), &
                              NVirt, NOcc, ONE, ZERO)
                        !
                        ! [XU](a,mu;g) = Sum(i) X(i,g)*U(a,i,mu)
                        ! [XU](1:NVirt,mu;g) = U(1:NVirt,1:NOcc,mu) X(1:NOcc, g)
                        ! Note that [XU] is scaled by the eigenvalue of T2 a(mu)
                        !
                        call real_av_x(XUam(:, mu), Uaim(:, :, mu), NVirt, Xig(:, g), &
                              NVirt, NOcc, Am(mu), ZERO)
                  end do
                  !$omp end parallel do
                  !
                  ! [YUXU](a,i;g) = Sum(mu) [XU](a,mu;g)*[YU](i,mu;g)
                  !
                  call real_abT(YUXUaig(:, :, g), XUam, YUim)
                  !
                  ! [UYUXU](mu,g) = Sum(ai) U(a,i,mu)*[YUXU](a,i;g)
                  ! This operation can be executed as a matrix-vector multiplication
                  ! UYUXU(1:NVecsT2,g) = U(1:NVirt*NOcc,1:NVecsT2)**T * YUXU(1:NVirt*NOcc)
                  !
                  call real_aTv_x(UYUXUmg(:, g), Uaim, NVirt*NOcc, YUXUaig(:, :, g), &
                        NVirt*NOcc, NVecsT2, ONE, ZERO)
            end do
            !
            ! [YUXUYUXU](g, h) = Sum(ai) [YUXU](a,i,g)*[YUXU](a,i,h)
            !
            allocate(YUXUYUXUgh(NGridTHC, NGridTHC))
            call real_aTb_x(YUXUYUXUgh, NGridTHC, YUXUaig, NVirt*NOcc, YUXUaig, NVirt*NOcc, &
                  NGridTHC, NGridTHC, NVirt*NOcc, ONE, ZERO)
            call real_vw_x(S2d, YUXUYUXUgh, Zgh, NGridTHC**2)
            Energy(RPA_ENERGY_CUMULANT_2D) = TWO * S2d
      end subroutine rpa_CCD_corrections_2d
      

      subroutine rpa_CCD_corrections_2bcd(Energy, Zgh, Xgi, Yga, Uaim, Am, YXUggm)
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
            call rpa_CCD_corrections_2d(Energy, UYUXUmg, Xgi, Yga, Uaim, Am, Zgh)
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
      end subroutine rpa_CCD_corrections_2bcd
end module rpa_CCD_Corrections
