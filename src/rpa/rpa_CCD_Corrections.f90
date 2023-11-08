module rpa_CCD_Corrections
      use arithmetic      
      use real_linalg
      use rpa_definitions
      use rpa_MeanField
      use clock      
      
      implicit none
      
contains

      subroutine rpa_Cumulant_HalfTHC(Energy, Zgh, Zgk, Yga, Xgi, hHFai, OccEnergies, VirtEnergies, &
            Uaim, Am, NOcc, NVirt, NVecsT2, NGridTHC, CumulantApprox)
            
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

            real(F64), dimension(:, :, :), allocatable :: YUgim, XUgam
            real(F64), dimension(:, :), allocatable :: ZYXUkm
            logical :: Compute_1b2g, Compute_2bcd
            integer :: mu
            type(TClock) :: timer_total, timer
            integer, parameter :: BlockDim = 300

            call msg("CCD corrections to RPA correlation energy")
            call clock_start(timer_total)
            call clock_start(timer)
            Compute_1b2g = .false.
            Compute_2bcd = .false.
            if (CumulantApprox >= RPA_CUMULANT_LEVEL_1_HALF_THC) Compute_1b2g = .true.
            if (CumulantApprox >= RPA_CUMULANT_LEVEL_3_HALF_THC) Compute_2bcd = .true.

            if (Compute_1b2g) then
                  call clock_start(timer)
                  call rpa_CCD_corrections_1b_2g_HalfTHC(Energy, Zgh, Zgk, Xgi, Yga, Uaim, &
                        Am, BlockDim, Compute_2bcd, ZYXUkm)
                  call msg("1b(SOSEX)+2g computed in " // str(clock_readwall(timer),d=1) // " seconds")
            end if

            if (Compute_2bcd) then
                  allocate(YUgim(NGridTHC, NOcc, NVecsT2))
                  allocate(XUgam(NGridTHC, NVirt, NVecsT2))            
                  do mu = 1, NVecsT2
                        !
                        ! [YU](gamma,i,mu) = Sum(a) X(gamma,a)*U(a,i,mu)
                        !
                        call real_ab(YUgim(:, :, mu), Yga, Uaim(:, :, mu))
                        !
                        ! [XU](a,gamma,mu) = Sum(i) U(a,i,mu)*X(gamma,i)
                        !
                        call real_abT(XUgam(:, :, mu), Xgi, Uaim(:, :, mu))
                        XUgam(:, :, mu) = XUgam(:, :, mu) * Sqrt(-Am(mu))
                        YUgim(:, :, mu) = YUgim(:, :, mu) * Sqrt(-Am(mu))
                  end do
                  call msg("Intermediates [XU] and [YU] built in " // str(clock_readwall(timer),d=1) // " seconds")
                  call clock_start(timer)
                  call rpa_CCD_corrections_2bcd_HalfTHC(Energy, Zgk, YUgim, XUgam, &
                        Uaim, Am, ZYXUkm)
                  call msg("2b+2c+2d computed in " // str(clock_readwall(timer),d=1) // " seconds")
            end if
            !
            ! Rescale the energy terms to get the correct MBPT prefactors.
            ! After scaling by 1/2, the 1b term is equivalent to SOSEX.
            !
            Energy(RPA_ENERGY_CUMULANT_1B) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_1B)
            Energy(RPA_ENERGY_CUMULANT_2B) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_2B)
            Energy(RPA_ENERGY_CUMULANT_2C) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_2C)
            call msg("CCD corrections computed in " // str(clock_readwall(timer_total),d=1) // " seconds")
      end subroutine rpa_Cumulant_HalfTHC

      
      subroutine rpa_CCD_corrections_1b_2g_HalfTHC(Energy, Zgh, Zgk, Xgi, Yga, Uaim, &
            Am, BlockDim, Intermediates_2bcd, ZYXUkm)
            
            real(F64), dimension(:), intent(inout)                 :: Energy
            real(F64), dimension(:, :), intent(in)                 :: Zgh
            real(F64), dimension(:, :), intent(in)                 :: Zgk
            real(F64), dimension(:, :), intent(in)                 :: Xgi
            real(F64), dimension(:, :), intent(in)                 :: Yga
            real(F64), dimension(:, :, :), intent(in)              :: Uaim
            real(F64), dimension(:), intent(in)                    :: Am
            integer, intent(in)                                    :: BlockDim
            logical, intent(in)                                    :: Intermediates_2bcd
            real(F64), dimension(:, :), allocatable, intent(out)   :: ZYXUkm

            real(F64), dimension(:, :), allocatable :: YXUgh
            real(F64), dimension(:), allocatable :: YXUgg
            real(F64), dimension(:, :), allocatable :: YUgi
            integer :: mu, g
            real(F64) :: Ec1b, Ec2g
            real(F64) :: S1b, S2g
            integer :: NVecsT2, NGridTHC, NCholesky, NOcc, NVirt
            real(F64) :: t_YU, t_YXU, t_Z_YXU_YXU, t_ZYXU
            type(TClock) :: timer

            NGridTHC = size(Zgh, dim=1)
            NVecsT2 = size(Am)
            NCholesky = size(Zgk, dim=2)
            NOcc = size(Xgi, dim=2)
            NVirt = size(Yga, dim=2)
            allocate(YXUgh(NGridTHC, NGridTHC))
            allocate(YUgi(NGridTHC, NOcc))
            if (Intermediates_2bcd) then
                  allocate(YXUgg(NGridTHC))
                  allocate(ZYXUkm(NCholesky, NVecsT2))
            end if
            t_YU = ZERO
            t_YXU = ZERO
            t_Z_YXU_YXU = ZERO
            t_ZYXU = ZERO
            Ec1b = ZERO
            Ec2g = ZERO
            do mu = 1, NVecsT2
                  !
                  ! [YU](gamma,i,mu) = Sum(a) Y(gamma,a)*U(a,i,mu)*Sqrt(-A(mu))
                  !
                  call clock_start(timer)
                  call real_ab_x(YUgi, NGridTHC, Yga, NGridTHC, Uaim(:, :, mu), &
                        NVirt, NGridTHC, NOcc, NVirt, Sqrt(-Am(mu)), ZERO)
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
                  Ec2g = Ec2g - TWO * S2g * Abs(Am(mu))
                  Ec1b = Ec1b + S1b
                  if (Intermediates_2bcd) then
                        call clock_start(timer)
                        do g = 1, NGridTHC
                              YXUgg(g) = YXUgh(g, g)
                        end do
                        call real_aTv(ZYXUkm(:, mu), Zgk, YXUgg) ! [ZYXU](k,mu) = Sum(g) Z(g,k)*[YXU](g,g;mu)
                        t_ZYXU = t_ZYXU + clock_readwall(timer)
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

            call msg("Level-1 timings (seconds)", underline=.true.)
            call msg("[YU]          " // str(t_YU,d=1))
            call msg("[YXU]         " // str(t_YXU,d=1))
            call msg("Z*[YXU]*[YXU] " // str(t_Z_YXU_YXU,d=1))
            call msg("[ZYXU]        " // str(t_ZYXU,d=1))

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
                  if (modulo(NBlocks, BlockDim) > 0) NBlocks = NBlocks + 1
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
      end subroutine rpa_CCD_corrections_1b_2g_HalfTHC
     
      
      subroutine rpa_CCD_corrections_2bcd_HalfTHC(Energy, Zgk, YUgim, XUgam, Uaim, Am, ZYXUkm) 
            real(F64), dimension(:), intent(inout)                 :: Energy
            real(F64), dimension(:, :), intent(in)                 :: Zgk
            real(F64), dimension(:, :, :), intent(in)              :: YUgim
            real(F64), dimension(:, :, :), intent(in)              :: XUgam
            real(F64), dimension(:, :, :), intent(in)              :: Uaim
            real(F64), dimension(:), intent(in)                    :: Am
            real(F64), dimension(:, :), intent(in)                 :: ZYXUkm

            real(F64), dimension(:, :), allocatable :: YUXUga, ZYUXUka, UZYUXUkm
            integer :: mu, i, a
            real(F64) :: Ec2b, Ec2c, Ec2d
            real(F64) :: S2b, S2d
            integer :: NVecsT2, NGridTHC, NCholesky, NOcc, NVirt

            NGridTHC = size(Zgk, dim=1)
            NVecsT2 = size(YUgim, dim=3)
            NCholesky = size(Zgk, dim=2)
            NOcc = size(YUgim, dim=2)
            NVirt = size(XUgam, dim=2)
            allocate(YUXUga(NGridTHC, NVirt))
            allocate(ZYUXUka(NCholesky, NVirt))
            allocate(UZYUXUkm(NCholesky, NVecsT2))
            Ec2b = ZERO
            Ec2c = ZERO
            Ec2d = ZERO
            UZYUXUkm = ZERO
            do i = 1, NOcc
                  !
                  ! [YUXU](g,a,g,i) = Sum(mu) [YU](g,a,mu)*[XU](g,i,mu)
                  !
                  YUXUga = ZERO
                  !$omp parallel do private(a, mu) default(shared)
                  do a = 1, NVirt
                        do mu = 1, NVecsT2
                              YUXUga(:, a) = YUXUga(:, a) + XUgam(:, a, mu) * YUgim(:, i, mu)
                        end do
                  end do
                  !$omp end parallel do
                  !
                  ! [ZYUXU](k,a,i) = Sum(g) Z(g,k)*[YUXU](g,a,g,i)
                  !
                  call real_aTb(ZYUXUka, Zgk, YUXUga)
                  call real_vw_x(S2d, ZYUXUka, ZYUXUka, NCholesky*NVirt)
                  Ec2d = Ec2d + S2d
                  !
                  ! [UZYUXU](k, mu) = Sum(ai) [ZYUXU](k,a,i)*U(a,i,mu)
                  !
                  do mu = 1, NVecsT2
                        call real_av_x(UZYUXUkm(:, mu), ZYUXUka, NCholesky, Uaim(:, i, mu), &
                              NCholesky, NVirt, Sqrt(-Am(mu)), ONE)
                  end do
            end do
            call real_vw_x(S2b, ZYXUkm, UZYUXUkm, NCholesky*NVecsT2)
            Ec2b = -TWO * S2b
            Ec2c = -TWO * S2b
            !
            ! Symmetry factors due to the permuation symmetry of the Coulomb integrals
            ! and the cumulant.
            !
            ! Note regarding SOSEX, 2b, 2c
            ! The SOSEX term is Ec1b(Lambda=1) rescaled by 1/2. In the third order,
            ! SOSEX contributes half of the exact MBPT prefactor of Ec2b+Ec2c.
            ! Thus, Ec2b(Lambda=1) + Ec2c(Lambda=1) should be rescaled by 1/2 afterwards
            ! if used alongside SOSEX.
            !
            Ec2b = TWO * Ec2b
            Ec2c = TWO * Ec2c
            Ec2d = TWO * Ec2d
            !
            Energy(RPA_ENERGY_CUMULANT_2B) = Ec2b
            Energy(RPA_ENERGY_CUMULANT_2C) = Ec2c
            Energy(RPa_ENERGY_CUMULANT_2D) = Ec2d
      end subroutine rpa_CCD_corrections_2bcd_HalfTHC


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
end module rpa_CCD_Corrections
