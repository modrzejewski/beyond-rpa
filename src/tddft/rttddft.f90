module rttddft
      use arithmetic
      use math_constants
      use gparam
      use h_xcfunc 
      use real_linalg
      use xcfunc   
      use scf
      use cmplx_scf
      use real_scf
      use ints
      use display
      use multipole
      use matexp
      use spherh
      use linalg
      use basis_sets

      implicit none

contains

      subroutine propagate_mmut(rho_curr, rho_prev, expx, x, update_prev, work)
            !
            ! Propagate the one-electron density matrix in time according to
            ! RHO(t_{n+1}) <- EXP(X) RHO(t_{n-1}) EXP(X)^H
            ! where X = 2i\Delta t F(t_n).
            !
            real(F64), dimension(:, :, :), contiguous, intent(inout) :: rho_curr
            real(F64), dimension(:, :, :), contiguous, intent(inout) :: rho_prev
            real(F64), dimension(:, :, :), contiguous, intent(out)   :: expx
            real(F64), dimension(:, :, :), contiguous, intent(in)    :: x
            logical, intent(in)                                      :: update_prev
            real(F64), dimension(:, :, :), contiguous, intent(out)   :: work
            !
            ! 1. Compute EXP(X)
            ! 2. WORK <- EXP(X) RHO_PREV
            ! 3. RHO_PREV <- RHO_CURR
            ! 4. RHO_CURR <- WORK EXP(X)^H
            !
            call matrix_exponential_cplx(expx, x, work)
            call matmul_cplx(work, .false., .false., expx, rho_prev, ONE, ZERO)
            if (update_prev) then
                  rho_prev = rho_curr
            end if
            call matmul_cplx(rho_curr, .false., .true., work, expx, ONE, ZERO)
      end subroutine propagate_mmut


      subroutine initial_rho(rho_oao, nocc, f_ao, oao_transf, transf_work)
            real(F64), dimension(:, :), contiguous, intent(out) :: rho_oao
            integer, intent(in)                                 :: nocc
            real(F64), dimension(:, :), contiguous, intent(in)  :: f_ao
            real(F64), dimension(:, :), contiguous, intent(in)  :: oao_transf
            real(F64), dimension(:, :), contiguous, intent(out) :: transf_work

            integer :: n, lwork, liwork
            integer, dimension(:), allocatable :: iwork_evd
            real(F64), dimension(:), allocatable :: work_evd
            real(F64), dimension(:), allocatable :: eig

            n = size(oao_transf, dim=2)
            associate(f_oao => rho_oao)
                  call atba_transform(f_oao, oao_transf, f_ao, transf_work)
                  call dsyevdquery(n, lwork, liwork, "V")
                  allocate(work_evd(lwork))
                  allocate(iwork_evd(liwork))
                  allocate(eig(n))
                  call dsyevdwrap(f_oao, eig, n, "V", work_evd, iwork_evd)
                  transf_work(:, 1:n) = f_oao
            end associate
            
            call syrkwrap("L", "N", n, nocc, TWO, transf_work, ZERO, rho_oao)
            call smfill(rho_oao)
      end subroutine initial_rho


      subroutine real_time_polar(omega, xcmodel, nocc, rho_t0, AOBasis, lindep, ncyc, timestep, field)
            ! ----------------------------------------------------------------------------
            ! Compute frequency-dependent polarizability alpha(-omega; omega) and
            ! hyperpolarizabilities beta(0; omega, -omega) and beta(-2omega; omega, omega)
            ! via real-time simulation.
            ! ----------------------------------------------------------------------------
            ! 1. Ding, F., Kuiken, B. E. Van, Eichinger, B. E., Li, X.,
            !    J. Chem. Phys. 138, 064104 (2013); doi: 10.1063/1.4790583
            ! 2. Lopata, K. and Govind N., J. Chem. Theory Comput. 7, 1344 (2011);
            !    doi: 10.1021/ct200137z
            ! 3. Li, X., Smith, S.M., Markevitch, A.N., Romanov, D.A., Levis, R.J.,
            !    and Schlegel, H.B., Phys. Chem. Chem. Phys. 7, 233 (2005);
            !    doi: 10.1039/b415849k
            !
            real(F64), intent(in)                                 :: omega
            type(txcdef), intent(in)                              :: xcmodel
            integer, intent(in)                                   :: nocc
            real(F64), dimension(:, :), contiguous, intent(in)    :: rho_t0
            type(TAOBasis), intent(in)                            :: AOBasis
            real(F64), intent(in)                                 :: lindep
            integer, intent(in)                                   :: ncyc
            real(F64), intent(in)                                 :: timestep
            real(F64), intent(in)                                 :: field

            real(F64) :: dt, t_cycle, a
            integer :: nsteps, ninit, nfit
            real(F64), dimension(:, :, :), allocatable :: mu_a, mu_2a
            real(F64), dimension(:, :, :), allocatable :: mu_ma, mu_m2a
            real(F64), dimension(:), allocatable :: mu1, mu2
            integer :: i, j
            real(F64), dimension(3, 3) :: alpha, beta_or, beta_shg
            integer :: ncycles
            !
            ! Total simulation time in cycles of the incident wave.
            ! The recommended time for production run is four cycles of
            ! init + production phases (Ref. 1, page 4). 
            !
            if (ncyc < 0) then
                  ncycles = 4
            else
                  ncycles = ncyc
            end if
            !
            ! Field strength. According to Ref. 1, the recommended range
            ! of values is A = 0.001 ... 0.005. The default value of 0.002
            ! is employed for all computations in Ref. 1.
            !
            if (field < ZERO) then
                  a = 0.002_F64
            else
                  a = field
            end if
            !
            ! Time step. The time step employed by Li et al.
            ! in their first paper on the MMUT algorithm is 0.1 a.u. (Ref. 3)
            ! Lopata et al. used a time step of 0.5 a.u. (0.012 fs) for Magnus'
            ! second order propagator which is quite similar to MMUT. 
            !
            if (timestep < ZERO) then
                  dt = 0.5_F64
            else
                  dt = timestep
            end if
            !
            ! Single period of the incident monochromatic radiation
            !
            t_cycle = TWO * PI / omega
            !
            ! Number of steps used for the initial ramp of the
            ! monochromatic wave. Data generated from these steps
            ! are not used for the least-squares fitting.
            !
            ninit = ceiling(t_cycle / dt)
            !
            ! Total number of simulation steps (init + production).
            !
            nfit = ceiling((ncycles - 1) * t_cycle / dt)
            nsteps = nfit + ninit

            allocate(mu_a(3, 0:nsteps, 3))
            allocate(mu_ma(3, 0:nsteps, 3))
            allocate(mu_2a(3, 0:nsteps, 3))
            allocate(mu_m2a(3, 0:nsteps, 3))
            allocate(mu1(nfit))
            allocate(mu2(nfit))

            call msg("Finite-difference computation of polarizabilities")
            call dmsg("Angular frequency", omega, fmt="F10.4")
            call dmsg("Wavelength [nm]", omega_to_lambda_nm(omega), fmt="F10.1")

            call real_time_dft(mu_a(:, :, 1), omega, [a, ZERO, ZERO], dt, nsteps, xcmodel, nocc, rho_t0, AOBasis, lindep)
            call real_time_dft(mu_a(:, :, 2), omega, [ZERO, a, ZERO], dt, nsteps, xcmodel, nocc, rho_t0, AOBasis, lindep)
            call real_time_dft(mu_a(:, :, 3), omega, [ZERO, ZERO, a], dt, nsteps, xcmodel, nocc, rho_t0, AOBasis, lindep)

            call real_time_dft(mu_ma(:, :, 1), omega, [-a, ZERO, ZERO], dt, nsteps, xcmodel, nocc, rho_t0, AOBasis, lindep)
            call real_time_dft(mu_ma(:, :, 2), omega, [ZERO, -a, ZERO], dt, nsteps, xcmodel, nocc, rho_t0, AOBasis, lindep)
            call real_time_dft(mu_ma(:, :, 3), omega, [ZERO, ZERO, -a], dt, nsteps, xcmodel, nocc, rho_t0, AOBasis, lindep)

            call real_time_dft(mu_2a(:, :, 1), omega, [TWO*a, ZERO, ZERO], dt, nsteps, xcmodel, nocc, rho_t0, AOBasis, lindep)
            call real_time_dft(mu_2a(:, :, 2), omega, [ZERO, TWO*a, ZERO], dt, nsteps, xcmodel, nocc, rho_t0, AOBasis, lindep)
            call real_time_dft(mu_2a(:, :, 3), omega, [ZERO, ZERO, TWO*a], dt, nsteps, xcmodel, nocc, rho_t0, AOBasis, lindep)

            call real_time_dft(mu_m2a(:, :, 1), omega, [-TWO*a, ZERO, ZERO], dt, nsteps, xcmodel, nocc, rho_t0, AOBasis, lindep)
            call real_time_dft(mu_m2a(:, :, 2), omega, [ZERO, -TWO*a, ZERO], dt, nsteps, xcmodel, nocc, rho_t0, AOBasis, lindep)
            call real_time_dft(mu_m2a(:, :, 3), omega, [ZERO, ZERO, -TWO*a], dt, nsteps, xcmodel, nocc, rho_t0, AOBasis, lindep)

            do j = 1, 3
                  do i = 1, 3
                        !
                        ! Compute \alpha_{ij}(-\omega; \omega)
                        !
                        call response_mu1(mu1, i, j, ninit, mu_a, mu_ma, mu_2a, mu_m2a, a)
                        call leastsq_alpha(alpha(i, j), omega, ninit, dt, mu1)
                        !
                        ! Compute \beta_{ijj}(0; \omega, -\omega) (optical rectification)
                        ! and \beta_{ijj}(-2\omega; \omega, \omega) (second harmonic generation)
                        !
                        call response_mu2(mu2, i, j, ninit, mu_a, mu_ma, mu_2a, mu_m2a, a)
                        call leastsq_beta(beta_or(i, j), beta_shg(i, j), omega, ninit, dt, mu2)
                  end do
            end do

            print *, "-------------- ALPHA(-OMEGA; OMEGA) ------------"
            do i = 1, 3
                  print *, alpha(i, :)
            end do
            print *, "------------------------------------------------"


            print *, "-------------- BETA(0; OMEGA, -OMEGA) ----------"
            do i = 1, 3
                  print *, beta_or(i, :)
            end do
            print *, "------------------------------------------------"

            print *, "-------------- BETA(-2OMEGA; OMEGA, OMEGA) ----------"
            do i = 1, 3
                  print *, beta_shg(i, :)
            end do
            print *, "------------------------------------------------"

      end subroutine real_time_polar


      subroutine leastsq_alpha(alpha, omega, ninit, dt, mu1)
            ! -------------------------------------------------------------
            ! Compute \alpha(-\omega; \omega) via least-squares fitting.
            ! The least-squares matrix equation reduces to a 1-dimensional
            ! case:
            ! X^T X alpha = X^T \mu
            ! X_k = cos(omega t_k)
            ! alpha <- X^T \mu / X^T X
            ! \mu is defined via equation 15 in Ref. 1.
            ! -------------------------------------------------------------
            ! 1. Ding, F., Kuiken, B. E. Van, Eichinger, B. E., Li, X.,
            !    J. Chem. Phys. 138, 064104 (2013); doi: 10.1063/1.4790583
            !
            real(F64), intent(out)              :: alpha
            real(F64), intent(in)               :: omega
            integer, intent(in)                 :: ninit
            real(F64), intent(in)               :: dt
            real(F64), dimension(:), intent(in) :: mu1

            real(F64) :: xtx, xtmu
            real(F64) :: tk, cos_tk
            integer :: n, k

            n = size(mu1)
            xtx = ZERO
            xtmu = ZERO
            do k = 1, n
                  tk = real(ninit + k, F64) * dt
                  cos_tk = cos(omega * tk)
                  xtx = xtx + cos_tk**2
                  xtmu = xtmu + cos_tk * mu1(k)
            end do
            alpha = xtmu / xtx
      end subroutine leastsq_alpha


      subroutine leastsq_beta(beta_or, beta_shg, omega, ninit, dt, mu2)
            ! ----------------------------------------------------------------------
            ! Compute \beta(0; \omega, -\omega) and \beta(-2\omega; \omega, \omega) 
            ! via least-squares fitting:
            ! X^T X \beta = X^T \mu
            ! \mu is defined via equation 16 in Ref. 1
            ! ----------------------------------------------------------------------
            ! 1. Ding, F., Kuiken, B. E. Van, Eichinger, B. E., Li, X.,
            !    J. Chem. Phys. 138, 064104 (2013); doi: 10.1063/1.4790583
            !
            real(F64), intent(out)              :: beta_or
            real(F64), intent(out)              :: beta_shg
            real(F64), intent(in)               :: omega
            integer, intent(in)                 :: ninit
            real(F64), intent(in)               :: dt
            real(F64), dimension(:), intent(in) :: mu2

            real(F64), dimension(2, 2) :: xtx
            real(F64), dimension(2) :: xtmu
            real(F64) :: tk, cos_tk, det
            integer :: n, k

            n = size(mu2)
            xtx = ZERO
            xtmu = ZERO
            do k = 1, n
                  tk = real(ninit + k, F64) * dt
                  cos_tk = cos(TWO * omega * tk)
                  xtx(2, 2) = xtx(2, 2) + cos_tk**2
                  xtx(2, 1) = xtx(2, 1) + cos_tk
                  xtmu(1) = xtmu(1) + mu2(k)
                  xtmu(2) = xtmu(2) + cos_tk * mu2(k)
            end do
            xtx(1, 2) = xtx(2, 1)
            xtx(1, 1) = real(n, F64)
            xtx = xtx / real(16, F64)
            xtmu = xtmu / real(4, F64)
            !
            ! Expclicitly solve the linear system
            !
            det = xtx(1, 1) * xtx(2, 2) - xtx(2, 1)**2
            beta_or = (xtmu(1) * xtx(2, 2) - xtmu(2) * xtx(2, 1)) / det
            beta_shg = (xtmu(2) * xtx(1, 1) - xtmu(1) * xtx(2, 1)) / det
      end subroutine leastsq_beta


      subroutine response_mu1(mu1_ij, i, j, ninit, mu_a, mu_ma, mu_2a, mu_m2a, a)
            real(F64), dimension(:), intent(out)       :: mu1_ij
            integer, intent(in)                        :: i, j
            integer, intent(in)                        :: ninit
            real(F64), dimension(:, 0:, :), intent(in) :: mu_a
            real(F64), dimension(:, 0:, :), intent(in) :: mu_ma
            real(F64), dimension(:, 0:, :), intent(in) :: mu_2a
            real(F64), dimension(:, 0:, :), intent(in) :: mu_m2a
            real(F64), intent(in)                      :: a

            integer :: n, k
            real(F64) :: t1, t2, t3, t4

            n = size(mu1_ij)
            do k = 1, n
                  t1 = mu_a(i, ninit+k, j)
                  t2 = mu_ma(i, ninit+k, j)
                  t3 = mu_2a(i, ninit+k, j)
                  t4 = mu_m2a(i, ninit+k, j)
                  mu1_ij(k) = (8.0_F64 * (t1 - t2) - (t3 - t4)) / (12.0_F64 * a)
            end do
      end subroutine response_mu1


      subroutine response_mu2(mu2_ijj, i, j, ninit, mu_a, mu_ma, mu_2a, mu_m2a, a)
            real(F64), dimension(:), intent(out)       :: mu2_ijj
            integer, intent(in)                        :: i, j
            integer, intent(in)                        :: ninit
            real(F64), dimension(:, 0:, :), intent(in) :: mu_a
            real(F64), dimension(:, 0:, :), intent(in) :: mu_ma
            real(F64), dimension(:, 0:, :), intent(in) :: mu_2a
            real(F64), dimension(:, 0:, :), intent(in) :: mu_m2a
            real(F64), intent(in)                      :: a

            integer :: n, k
            real(F64) :: t1, t2, t3, t4, t5

            n = size(mu2_ijj)
            do k = 1, n
                  t1 = mu_a(i, ninit+k, j)
                  t2 = mu_ma(i, ninit+k, j)
                  t3 = mu_2a(i, ninit+k, j)
                  t4 = mu_m2a(i, ninit+k, j)
                  t5 = mu_a(i, 0, j)
                  mu2_ijj(k) = (16.0_F64 * (t1 + t2) - (t3 + t4) - 30.0_F64 * t5) / (24.0_F64 * a**2)
            end do
      end subroutine response_mu2


      subroutine real_time_dft(dipt, omega, amplitude, dt, nsteps, xcmodel, nocc, rho_t0, AOBasis, lindep)
            ! -----------------------------------------------------------------
            ! Modified midpoint and unitary transformation (MMUT)
            ! -----------------------------------------------------------------
            ! U(2i\Delta t F(t_n))
            ! P(t_{n+1}) = U P(t_{n-1}) U^H
            !
            !
            ! F_ao
            ! --------------------------------------------------------------------
            ! 1. Lopata, K. and Govind N., J. Chem. Theory Comput. 7, 1344 (2011);
            !    doi: 10.1021/ct200137z
            ! 2. Ding, F., Kuiken, B. E. Van, Eichinger, B. E., Li, X.,
            !    J. Chem. Phys. 138, 064104 (2013); doi: 10.1063/1.4790583
            !
            real(F64), dimension(:, 0:), intent(out)              :: dipt
            real(F64), intent(in)                                 :: omega
            real(F64), dimension(3), intent(in)                   :: amplitude
            real(F64), intent(in)                                 :: dt
            integer, intent(in)                                   :: nsteps
            type(txcdef), intent(in)                              :: xcmodel
            integer, intent(in)                                   :: nocc
            real(F64), dimension(:, :), contiguous, intent(in)    :: rho_t0
            type(TAOBasis), intent(in)                            :: AOBasis
            real(F64), intent(in)                                 :: lindep

            type(tgriddiag) :: gdiag
            real(F64) :: eel, exc
            type(txcdef) :: bare_exchange
            real(F64), dimension(:, :), allocatable :: dipx, dipy, dipz
            real(F64) :: t
            real(F64), dimension(:, :), allocatable :: f_ao
            real(F64), dimension(:, :), allocatable :: f_oao_t0
            real(F64), dimension(:, :), allocatable :: hbare
            real(F64), dimension(:, :), allocatable :: rho_ao
            real(F64), dimension(:, :, :), allocatable :: expx_oao
            real(F64), dimension(:, :, :), allocatable :: x_oao
            real(F64), dimension(:, :, :), allocatable :: rho_oao
            real(F64), dimension(:, :, :), allocatable :: rho_oao_prev
            real(F64), dimension(:, :, :), allocatable :: rho_oao_t1
            real(F64), dimension(:, :), allocatable :: overlap, kinetic, attraction
            real(F64), dimension(:, :), allocatable :: transf_work
            real(F64), dimension(:, :, :), allocatable :: work
            real(F64), dimension(:, :), allocatable :: BasisVecs_cao
            real(F64), dimension(:, :), allocatable :: BasisVecs_sao
            real(F64), dimension(3) :: elfield
            real(F64), dimension(1, 1) :: dummy2
            real(F64), dimension(1) :: dummy1
            integer :: nvecs
            integer :: k
            real(F64) :: rho_diff
            integer, parameter :: max_conv = 15
            real(F64), parameter :: conv_thresh = 1.0E-5_F64

            call msg("Starting propagation for " // str(nsteps) // " time steps")
            call dmsg("Step size [a.u.]", dt, fmt="ES10.3")
            call msg("Propagator: modified midpoint and unitary transformation")
            call msg("Phys. Chem. Chem. Phys. 7, 233 (2005); doi: 10.1039/b415849k")

            allocate(overlap(NORB, NORB))
            allocate(kinetic(NORB, NORB))
            allocate(attraction(NORB, NORB))
            call stv(overlap, kinetic, attraction)
            call smfill(overlap)
            !
            ! Compute the orthogonal basis
            !
            call basis_OAO(BasisVecs_cao, BasisVecs_sao, overlap, AOBasis, lindep)
            nvecs = size(BasisVecs_cao, dim=2)
            allocate(transf_work(nvecs, NORB))
            allocate(work(nvecs, nvecs, 2))
            allocate(rho_oao_prev(nvecs, nvecs, 2))
            allocate(rho_oao_t1(nvecs, nvecs, 2))
            allocate(rho_oao(nvecs, nvecs, 2))
            allocate(rho_ao(NORB, NORB))
            allocate(hbare(NORB, NORB))
            allocate(f_ao(NORB, NORB))
            allocate(f_oao_t0(nvecs, nvecs))
            allocate(x_oao(nvecs, nvecs, 2))
            allocate(expx_oao(nvecs, nvecs, 2))
            !
            ! Compute the initial stationary density matrix
            ! in the orthogonal basis to get the molecular orbitals
            ! and the density matrix in the orthogonal, nonredundant
            ! basis
            !
            hbare = kinetic + attraction
            call ksgen(xcmodel, f_ao, eel, exc, rho_t0, hbare, dummy2, gdiag, dummy1, &
                  dummy2, oaotransf=.false.)
            call smfill(f_ao)
            call atba_transform(f_oao_t0, BasisVecs_cao, f_ao, transf_work)
            call initial_rho(rho_oao(:, :, 1), nocc, f_ao, BasisVecs_cao, transf_work)
            rho_oao(:, :, 2) = ZERO
            rho_oao_prev = rho_oao
            allocate(dipx(NORB, NORB))
            allocate(dipy(NORB, NORB))
            allocate(dipz(NORB, NORB))
            call dipole(dipx, dipy, dipz, ORIGIN)
            dipt(1, 0) = -trace(rho_t0, dipx)
            dipt(2, 0) = -trace(rho_t0, dipy)
            dipt(3, 0) = -trace(rho_t0, dipz)
            !
            ! Define the component of the given exchange-correlation functional
            ! which depends on the imaginary (antisymmetric) part of the density
            ! matrix
            !
            call xcf_define(bare_exchange, XCF_HF, AUX_NONE, .false.)
            call xcf_set_exx(bare_exchange, xcf_get_exx(xcmodel))
            if (xcf_get_flag(xcmodel, XCF_RSHYB)) then
                  call xcf_set_flag(bare_exchange, XCF_RSHYB, .true.)
                  call xcf_set_omega(bare_exchange, xcf_get_omega(xcmodel))
                  call xcf_set_srexx(bare_exchange, xcf_get_srexx(xcmodel))
            end if
            call xcf_set_flag(bare_exchange, XCF_BAREH, .false.)
            call xcf_set_flag(bare_exchange, XCF_HARTREE, .false.)
            call xcf_set_flag(bare_exchange, XCF_IMAGINARY_DENSITY, .true.)
            !
            ! Initial self-consistent computation of F(DeltaT/2)
            !
            call msg("Computing self-consistently future density matrix for t=dt")
            do k = 1, max_conv
                  if (k == 1) then
                        x_oao(:, :, 1) = ZERO
                        call field_monochromatic_ramp(elfield, amplitude, dt/TWO, omega)
                        hbare = elfield(1) * dipx + elfield(2) * dipy + elfield(3) * dipz
                        call smfill(hbare)
                        call atba_transform(x_oao(:, :, 2), BasisVecs_cao, hbare, transf_work)
                        x_oao(:, :, 2) = dt/TWO * (x_oao(:, :, 2) + f_oao_t0)
                        call propagate_mmut(rho_oao, rho_oao_prev, expx_oao, x_oao, .false., work)
                  else
                        call field_monochromatic_ramp(elfield, amplitude, dt/TWO, omega)
                        hbare = kinetic + attraction + TWO * (elfield(1) * dipx + elfield(2) * dipy + elfield(3) * dipz)
                        call comp_x_re(x_oao(:, :, 1), rho_ao, f_ao, bare_exchange, dt/TWO, &
                              rho_oao(:, :, 2), BasisVecs_cao, transf_work)
                        call comp_x_im(x_oao(:, :, 2), rho_ao, f_ao, xcmodel, dt/TWO, hbare, rho_oao(:, :, 1), &
                              BasisVecs_cao, transf_work)
                        !
                        ! Interpolate F(DeltaT/2) = 1/2(F(T0) + F(DeltaT)). Note that we interpolate
                        ! only the density-dependent part: hbare is computed exactly for t=dt/2.
                        !
                        x_oao(:, :, 1) = FRAC12 * x_oao(:, :, 1)
                        x_oao(:, :, 2) = FRAC12 * x_oao(:, :, 2) + FRAC12 * dt/TWO * f_oao_t0
                        !
                        ! Compute Rho(DeltaT) with the interpolated F(DeltaT/2)
                        !
                        rho_oao_t1 = rho_oao
                        call propagate_mmut(rho_oao, rho_oao_prev, expx_oao, x_oao, .false., work)
                        call maxdiff_cplx(rho_diff, rho_oao_t1, rho_oao)
                        if (rho_diff < conv_thresh) then
                              exit
                        end if
                  end if
            end do
            call dmsg("Max absolute difference in Rho(t=dt)", rho_diff, fmt="ES10.3")
            call abat_transform(rho_ao, BasisVecs_cao, rho_oao(:, :, 1), transf_work)
            dipt(1, 1) = -trace(rho_ao, dipx)
            dipt(2, 1) = -trace(rho_ao, dipy)
            dipt(3, 1) = -trace(rho_ao, dipz)
            !
            ! Main time propagation
            !
            do k = 1, nsteps
                  t = real(k, F64) * dt
                  !
                  ! Real part of X = 2 * deltaT * i F (corresponds to the imaginary part of F)
                  !
                  call comp_x_re(x_oao(:, :, 1), rho_ao, f_ao, bare_exchange, dt, &
                        rho_oao(:, :, 2), BasisVecs_cao, transf_work)
                  !
                  ! Imaginary part of X = 2 * deltaT * i F (corresponds to the real part of F) 
                  !
                  call field_monochromatic_ramp(elfield, amplitude, t, omega)
                  hbare = kinetic + attraction + elfield(1) * dipx + elfield(2) * dipy + elfield(3) * dipz
                  call comp_x_im(x_oao(:, :, 2), rho_ao, f_ao, xcmodel, dt, hbare, rho_oao(:, :, 1), &
                        BasisVecs_cao, transf_work)
                  !
                  ! Compute time-dependent dipole moments
                  !
                  dipt(1, k) = -trace(rho_ao, dipx)
                  dipt(2, k) = -trace(rho_ao, dipy)
                  dipt(3, k) = -trace(rho_ao, dipz)
                  !
                  ! Compute the propagator exp(2 * deltaT * i F) = exp(X) and
                  ! propagate the density matrix in orthogonal basis using the
                  ! MMUT algorithm of Li et al.
                  ! rho <- exp(X) rho_prev exp(X)^H (Eq. 5 in Ref. 2)
                  !
                  call propagate_mmut(rho_oao, rho_oao_prev, expx_oao, x_oao, .true., work)
            end do
      end subroutine real_time_dft


      subroutine comp_x_re(x_oao_re, rho_ao, f_ao, bare_exchange, dt, rho_oao_im, BasisVecs_cao, transf_work)
            !
            ! Compute the real part of X = 2 * deltaT * i F
            ! (corresponds to the imaginary part of F)
            !
            real(F64), dimension(:, :), contiguous, intent(out) :: x_oao_re
            real(F64), dimension(:, :), contiguous, intent(out) :: rho_ao
            real(F64), dimension(:, :), contiguous, intent(out) :: f_ao
            type(txcdef), intent(in)                            :: bare_exchange
            real(F64), intent(in)                               :: dt
            real(F64), dimension(:, :), contiguous, intent(in)  :: rho_oao_im
            real(F64), dimension(:, :), contiguous, intent(in)  :: BasisVecs_cao
            real(F64), dimension(:, :), contiguous, intent(out) :: transf_work

            type(tgriddiag) :: gdiag
            real(F64) :: eel, exc
            real(F64), dimension(1, 1) :: dummy2
            real(F64), dimension(1) :: dummy1

            call abat_transform(rho_ao, BasisVecs_cao, rho_oao_im, transf_work)
            call ksgen(bare_exchange, f_ao, eel, exc, rho_ao, dummy2, dummy2, gdiag, dummy1, &
                  dummy2, oaotransf=.false.)
            call amfill(f_ao)
            call atba_transform(x_oao_re, BasisVecs_cao, f_ao, transf_work)
            x_oao_re = -TWO * dt * x_oao_re
      end subroutine comp_x_re


      subroutine comp_x_im(x_oao_im, rho_ao, f_ao, xcmodel, dt, hbare, rho_oao_re, BasisVecs_cao, transf_work)
            !
            ! Compute the imaginary part of X = 2 * deltaT * i F
            ! (corresponds to the real part of F) 
            !
            real(F64), dimension(:, :), contiguous, intent(out) :: x_oao_im
            real(F64), dimension(:, :), contiguous, intent(out) :: rho_ao
            real(F64), dimension(:, :), contiguous, intent(out) :: f_ao
            type(txcdef), intent(in)                            :: xcmodel
            real(F64), intent(in)                               :: dt
            real(F64), dimension(:, :), contiguous, intent(in)  :: hbare
            real(F64), dimension(:, :), contiguous, intent(in)  :: rho_oao_re
            real(F64), dimension(:, :), contiguous, intent(in)  :: BasisVecs_cao
            real(F64), dimension(:, :), contiguous, intent(out) :: transf_work

            type(tgriddiag) :: gdiag
            real(F64) :: eel, exc
            real(F64), dimension(1, 1) :: dummy2
            real(F64), dimension(1) :: dummy1

            call abat_transform(rho_ao, BasisVecs_cao, rho_oao_re, transf_work)
            call ksgen(xcmodel, f_ao, eel, exc, rho_ao, hbare, dummy2, gdiag, dummy1, &
                  dummy2, oaotransf=.false.)
            call smfill(f_ao)
            call atba_transform(x_oao_im, BasisVecs_cao, f_ao, transf_work)
            x_oao_im = TWO * dt * x_oao_im
      end subroutine comp_x_im


      subroutine field_monochromatic_ramp(elfield, amplitude, t, omega)
            real(F64), dimension(3), intent(out) :: elfield
            real(F64), dimension(3), intent(in)  :: amplitude
            real(F64), intent(in)                :: t
            real(F64), intent(in)                :: omega

            if (t < TWO * PI / omega) then
                  elfield = omega * t / (TWO * PI) * amplitude * cos(omega * t)
            else
                  elfield = amplitude * cos(omega * t)
            end if
      end subroutine field_monochromatic_ramp
end module rttddft
