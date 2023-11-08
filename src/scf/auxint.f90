! -------------------------------------------------------------------
! AUXILIARY (NON-XC) INTEGRALS ON THE MOLECULAR GRID
! -------------------------------------------------------------------
! The subroutines contained in this module should be used to compute
! numerical integrals other than DFT exchange-correlation energy
! on the molecular grid. These integrals will be called auxiliary
! integrals on the molecular grid. The numerical intgration is
! performed on the same grid as the one used for exchange-correlation
! functionals. Numerical integrals of the functions defined in this
! module are  computed in every SCF iteration alongside parallel
! computation of DFT energy.
!
module auxint
      use gparam
      use arithmetic
      use periodic
      use math_constants
      use br89
      use multipole
      use linalg
      use tiledmatrix
      use gridfunc
      use hirshfeld
      use ecpint
      use modrzej2016_x_hole
      use modrzej2016_c_energy
      use modrzej2016_xc_auxiliary
      use pbe_xc_auxiliary
      use tpss_xc_auxiliary
      use h_xcfunc
      use io
      use mgga
      use AtomicDensities
      
      implicit none
      save
      
      real(F64), parameter, private :: HIRSHFELD_RHO_TOL = 1.0E-8_F64
      real(F64), parameter, private :: BR89_RHO_TOL = 1.0E-8_F64
      real(F64), parameter, private :: BR89_TAU_TOL = 1.0E-8_F64
      real(F64), parameter, private :: EXCHDIP_EXX_RHO_TOL = 1.0E-8_F64
      real(F64), parameter, private :: NORM_EXCHDIP_EXX_RHO_TOL = 1.0E-8_F64
      real(F64), parameter, private :: NORM_EXCHDIP_EXX_TAU_TOL = 1.0E-8_F64
      real(F64), parameter, private :: TAU_HEG_RHO_TOL = 1.0E-8_F64
      real(F64), parameter, private :: GDD_GRAC_RHO_TOL = 1.0E-8_F64
      real(F64), parameter, private :: RHO_MIN_XHOLE_DIST_FUKUI = 1.0E-8_F64
      real(F64), parameter :: GDD_GRAC_ALPHA = 4.0_F64
      real(F64), parameter :: GDD_GRAC_BETA = 15.0_F64

      real(F64), dimension(:, :), allocatable, private :: dipx
      real(F64), dimension(:, :), allocatable, private :: dipy
      real(F64), dimension(:, :), allocatable, private :: dipz
      real(F64), dimension(:, :), allocatable, private :: dmatax
      real(F64), dimension(:, :), allocatable, private :: dmatay
      real(F64), dimension(:, :), allocatable, private :: dmataz
      real(F64), dimension(:, :), allocatable, private :: dmatbx
      real(F64), dimension(:, :), allocatable, private :: dmatby
      real(F64), dimension(:, :), allocatable, private :: dmatbz
      real(F64), dimension(:, :), allocatable, private :: FUKUI_MATRIX
      real(F64), dimension(:, :), allocatable, private :: RHO_ATOMIC
      real(F64), dimension(:, :), allocatable, private :: DENSITY_COMPARE_RHO

      integer, parameter, private :: NORM_EXCHDIP_NPOINT = 182
      integer, private :: k
      !
      ! [HUGE, 0.750, 0.745, 0.740, ..., 0.105, 0.100, 0.098, 0.096, ..., 0.002, TINY]
      !
      real(F64), dimension(NORM_EXCHDIP_NPOINT), parameter, private :: SHIFT_PARAM = &
            [huge(0.00d+0), (0.750d+0-dble(k)*0.005d+0, k=0,130), &
            (0.100d+0-dble(k)*0.002d+0, k=1,49), tiny(0.00d+0)]

contains

      subroutine aux_setauxint(auxint_id, spin_uncomp, rhoa_ao, rhob_ao, matrix_input)
            ! -------------------------------------------------------------------
            ! Set up the calculation of auxiliary integrals on the molecular
            ! grid. These integrals are usually not associated with any
            ! exchange correlation functional; for example, they may
            ! be used for data visualization.
            !
            ! The auxiliary integrals are evaluated on the same grid as
            ! the exchange-correlation functional.
            ! -------------------------------------------------------------------
            ! AUXINT_ID
            !             Identifier of the requested auxiliary integral.
            ! SPIN_UNCOMP
            !             If SPIN_UNCOMP == .TRUE., use different
            !             densities for spins alpha and beta.
            ! RHOA_AO
            !             If SPIN_UNCOMP == .TRUE., alpha-spin density in
            !             AO basis.
            !             If SPIN_UNCOMP == .FALSE., total electronic
            !             density (alpha + beta) in the AO basis.
            ! RHOB_AO
            !             If SPIN_UNCOMP == .TRUE., beta-spin density in 
            !             AO basis.
            !             If SPIN_UNCOMP == .FALSE., not referenced.
            ! MATRIX_INPUT
            !             Matrix input for the integrand. For example, 
            !             a matrix representation of an operator
            !             represented on the molecular grid can be passed
            !             as MATRIX_INPUT. Not referenced if the integrand
            !             does not require MATRIX_INPUT.
            !
            integer, intent(in)                    :: auxint_id
            logical, intent(in)                    :: spin_uncomp
            real(F64), dimension(:, :), intent(in) :: rhoa_ao
            real(F64), dimension(:, :), intent(in) :: rhob_ao
            real(F64), dimension(:, :), intent(in) :: matrix_input

            real(F64), dimension(3), parameter :: dip_origin = [ZERO, ZERO, ZERO]

            select case (auxint_id)
            ! case (AUX_HIRSHFELD_VOLUME_FREE)
            !       if (.not. allocated(RHO_ATOMIC)) then
            !             m = ((MAX_ATOMNSHELL + 1) * MAX_ATOMNSHELL) / 2
            !             allocate(RHO_ATOMIC(m, 1))
            !       end if
            !       if (spin_uncomp) then
            !             allocate(rho_tot(NORB, NORB))
            !             rho_tot = rhoa_ao + rhob_ao
            !             call RhoSpherCoeffs(RHO_ATOMIC(:, 1), rho_tot)
            !             deallocate(rho_tot)
            !       else
            !             call RhoSpherCoeffs(RHO_ATOMIC(:, 1), rhoa_ao)
            !       end if
                  
            ! case (AUX_HIRSHFELD_VOLUME, AUX_HIRSHFELD_POPULATION)
            !       if (.not. allocated(RHO_ATOMIC)) then
            !             m = ((MAX_ATOMNSHELL + 1) * MAX_ATOMNSHELL) / 2
            !             allocate(RHO_ATOMIC(m, NELEMENT))
            !       end if
            !       RHO_ATOMIC(:, :) = matrix_input

            case (AUX_DENSITY_COMPARE)
                  if (.not. allocated(DENSITY_COMPARE_RHO)) then
                        allocate(DENSITY_COMPARE_RHO(NORB, NORB))
                  end if
                  select case (DENSITY_COMPARE_MODE)
                  case (FILEMODE_BINARY)
                        call io_binary_read(DENSITY_COMPARE_RHO, DENSITY_COMPARE_PATH)
                  case (FILEMODE_TEXT)
                        call io_text_read(DENSITY_COMPARE_RHO, DENSITY_COMPARE_PATH)
                  case default
                        call msg("Unsupported storage mode for DENSITY_COMPARE", MSG_ERROR)
                        stop
                  end select
                  
            case (AUX_EXCHDIP_EXX, AUX_GDD_OMEGA, &
                  AUX_GDD_GRAC, AUX_XHOLE_DIST_FUKUI)
                  
                  ! if (auxint_id == AUX_XHOLE_DIST_FUKUI) then
                  !       if (.not. allocated(FUKUI_MATRIX)) allocate(FUKUI_MATRIX(NORB, NORB))
                  !       if (spin_uncomp) then
                  !             FUKUI_MATRIX = rhoa_ao + rhob_ao - matrix_input
                  !       else
                  !             FUKUI_MATRIX = rhoa_ao - matrix_input
                  !       end if
                  ! end if

                  if (.not. allocated(dipx)) then
                        allocate(dipx(NORB, NORB))
                        allocate(dipy(NORB, NORB))
                        allocate(dipz(NORB, NORB))
                        call dipole(dipx, dipy, dipz, dip_origin)
                  end if

                  if (.not. allocated(dmatax)) then
                        allocate(dmatax(NORB, NORB))
                        allocate(dmatay(NORB, NORB))
                        allocate(dmataz(NORB, NORB))
                  end if

                  if (.not. allocated(dmatbx) .and. spin_uncomp) then
                        allocate(dmatbx(NORB, NORB))
                        allocate(dmatby(NORB, NORB))
                        allocate(dmatbz(NORB, NORB))
                  end if

                  if (spin_uncomp) then
                        !
                        ! DMATAX <- RHOA_AO^T DIPX RHOA_AO
                        !
                        call atsyba(dmatax, rhoa_ao, dipx, NORB)
                        call atsyba(dmatay, rhoa_ao, dipy, NORB)
                        call atsyba(dmataz, rhoa_ao, dipz, NORB)
                        call atsyba(dmatbx, rhob_ao, dipx, NORB)
                        call atsyba(dmatby, rhob_ao, dipy, NORB)
                        call atsyba(dmatbz, rhob_ao, dipz, NORB)
                        call smfill(dmatax)
                        call smfill(dmatay)
                        call smfill(dmataz)
                        call smfill(dmatbx)
                        call smfill(dmatby)
                        call smfill(dmatbz)
                  else
                        call atsyba(dmatax, rhoa_ao, dipx, NORB)
                        call atsyba(dmatay, rhoa_ao, dipy, NORB)
                        call atsyba(dmataz, rhoa_ao, dipz, NORB)
                        !
                        ! Note that in spin-unpolarized case the RHOA_AO
                        ! matrix is equal to 2 * C_{occ} C_{occ}^T. 
                        !
                        call scal_matrix(dmatax, FRAC14)
                        call scal_matrix(dmatay, FRAC14)
                        call scal_matrix(dmataz, FRAC14)
                        call smfill(dmatax)
                        call smfill(dmatay)
                        call smfill(dmataz)
                  end if
            end select
      end subroutine aux_setauxint


      subroutine aux_free()
            if (allocated(RHO_ATOMIC)) then
                  deallocate(RHO_ATOMIC)
            end if
            if (allocated(dipx)) then
                  deallocate(dipx)
                  deallocate(dipy)
                  deallocate(dipz)
            end if
            if (allocated(dmatax)) then
                  deallocate(dmatax)
                  deallocate(dmatay)
                  deallocate(dmataz)
            end if
            if (allocated(dmatbx)) then
                  deallocate(dmatbx)
                  deallocate(dmatby)
                  deallocate(dmatbz)
            end if
            if (allocated(FUKUI_MATRIX)) then
                  deallocate(FUKUI_MATRIX)
            end if
            if (allocated(DENSITY_COMPARE_RHO)) then
                  deallocate(DENSITY_COMPARE_RHO)
            end if
      end subroutine aux_free


      pure subroutine aux_matrix_input_dim(m, n, aux_id)
            integer, intent(out) :: m
            integer, intent(out) :: n
            integer, intent(in)  :: aux_id

            select case (aux_id)
            case (AUX_HIRSHFELD_VOLUME_FREE)
                  m = 0
                  n = 0
            case (AUX_HIRSHFELD_VOLUME, AUX_HIRSHFELD_POPULATION)
                  m = ((MAX_ATOMNSHELL + 1) * MAX_ATOMNSHELL) / 2
                  n = NELEMENT
            case (AUX_XHOLE_DIST_FUKUI, AUX_DENSITY_COMPARE)
                  m = NORB
                  n = NORB
            case default
                  m = 0
                  n = 0
            end select
      end subroutine aux_matrix_input_dim


      pure function aux_arraydim(aux_id, spin_uncomp)
            !
            ! Dimension of the array computed as an auxiliary integral
            !
            integer             :: aux_arraydim
            integer, intent(in) :: aux_id
            logical, intent(in) :: spin_uncomp

            select case (aux_id)
            case (AUX_HIRSHFELD_VOLUME_FREE)
                  aux_arraydim = 1
            case (AUX_HIRSHFELD_VOLUME, AUX_HIRSHFELD_POPULATION)
                  !
                  ! N integrals corresponding to N effective volumes/effective charges
                  ! of non-dummy centers + N array elements for intermediates
                  ! (scratch array)
                  !
                  aux_arraydim = 2 * NRealAtoms()
            case (AUX_XHOLE_DIST_FUKUI)
                  aux_arraydim = 4
            case (AUX_GDD_GRAC)
                  if (spin_uncomp) then
                        aux_arraydim = 4
                  else
                        aux_arraydim = 2
                  end if
            case (AUX_GDD_OMEGA)
                  aux_arraydim = 2 * NORM_EXCHDIP_NPOINT
            case (AUX_TAU_HEG)
                  if (spin_uncomp) then
                        aux_arraydim = 2
                  else
                        aux_arraydim = 1
                  end if
            case (AUX_REGULARIZED_VNUCL)
                  aux_arraydim = (NORB * (NORB - 1)) / 2 + NORB   
            case (AUX_MODRZEJ2012_C_HOLE)
                  if (spin_uncomp) then
                        aux_arraydim = 3 * AUX_XC_HOLE_NPOINTS
                  else
                        aux_arraydim = 2 * AUX_XC_HOLE_NPOINTS
                  end if
            case (AUX_BR_X_HOLE, AUX_PBE_X_HOLE, AUX_TPSS_X_HOLE)
                  if (spin_uncomp) then
                        aux_arraydim = 2 * AUX_XC_HOLE_NPOINTS
                  else
                        aux_arraydim = AUX_XC_HOLE_NPOINTS
                  end if
            case (AUX_MCSv2_VXC_INF, AUX_PBE_VXC_INF, AUX_TPSS_VXC_INF)
                  aux_arraydim = 1
            case (AUX_MCSv1_VC_LAMBDA, AUX_MCSv2_VC_LAMBDA)
                  aux_arraydim = 100
            case (AUX_DENSITY_COMPARE)
                  aux_arraydim = 3
            case default
                  aux_arraydim = 1
            end select
      end function aux_arraydim

      
      subroutine aux_gdd_finalize(auxint, nouter, mumin)
            real(F64), dimension(:), intent(inout) :: auxint
            real(F64), intent(in)                  :: nouter
            real(F64), intent(in)                  :: mumin
            
            real(F64) :: dxs2
            real(F64) :: mu
            real(F64) :: normerr_prev, normerr
            integer :: k, l, m, k_min
            real(F64), parameter :: c_wpbe = 0.90_F64
            real(F64), parameter :: c_wpbeh = 0.75_F64

            call msg("Global-density dependent range separation parameter", underline=.true.)
            call msg("J. Phys. Chem. A 117, 11580 (2013); doi: 10.1021/jp4088404")
            call dmsg("Target number of outer electrons", nouter, priority=MSG_DEBUG)

            k = 2
            normerr_prev = huge(nouter)
            do l = 1, NORM_EXCHDIP_NPOINT
                  m = 2 * l
                  normerr = abs(auxint(m) - nouter)
                  if (normerr < normerr_prev) then
                        k = m
                        normerr_prev = normerr
                  end if
            end do
            dxs2 = auxint(k-1) / auxint(k)
            mu = SHIFT_PARAM(k/2)

            call dmsg("<d_{xs}^2>", dxs2, fmt="F10.3")
            call dmsg("Actual number of electrons", auxint(k), fmt="F10.6", priority=MSG_DEBUG)
            call dmsg("TAU(UEG,SIGMA/TAU(SIGMA)", mu, fmt="F10.4", priority=MSG_DEBUG)
            
            if (mu < mumin) then
                  call msg("TAU(UEG,SIGMA/TAU(SIGMA) is below threshold")
                  call dmsg("MU_{MIN}", mumin, fmt="F10.4")
                  
                  k_min = 1
                  lloop: do l = NORM_EXCHDIP_NPOINT, 2, -1
                        if (abs(SHIFT_PARAM(l-1)-mumin) > abs(SHIFT_PARAM(l)-mumin)) then
                              k_min = l
                              exit lloop
                        end if
                  end do lloop

                  dxs2 = auxint(2*k_min-1) / auxint(2*k_min)
                  call dmsg("<d_{xs}^2>(MU_{MIN})", dxs2)
            end if

            call dmsg("Range-separation parameter for wPBE", c_wpbe / sqrt(dxs2), fmt="F10.2")
            call dmsg("Range-separation parameter for wPBEh", c_wpbeh / sqrt(dxs2), fmt="F10.2")
            call msg("wPBE (range-separated hybrid of Henderson et al.) includes zero short-range HF exchange")
            call msg("wPBEh includes 20% of short-range HF exchange")

            auxint(1) = dxs2
      end subroutine aux_gdd_finalize


      subroutine density_compare_display(a)
            real(F64), dimension(:), intent(in) :: a
            real(F64) :: RhoDiff, SigDiff, LapDiff

            RhoDiff = Sqrt(a(1))
            SigDiff = Sqrt(a(2))
            LapDiff = Sqrt(a(3))
            call msg("||X-Xref||=Sqrt(Integral(X(r)-XRef(r))**2 dr)")
            call msg("||Rho-RhoRef|| = " // str(RhoDiff, d=4))
            call msg("||Sqrt(NablaRho**2)-Sqrt(NablaRhoRef**2)|| = " // str(SigDiff, d=4))
            call msg("||LaplRho-LaplRhoRef|| = " // str(LapDiff, d=4))
      end subroutine density_compare_display

      
      subroutine modrzej2012_c_hole_finalize(a, spin_uncompensated)
            real(F64), dimension(:), intent(in) :: a
            logical, intent(in)   :: spin_uncompensated
            character(80) :: line
            integer :: n, k
            real(F64) :: u
            real(F64) :: hc_aa, hc_ab, hc_bb

            call msg("System-averaged meta-GGA correlation hole of Modrzejewski et al.")
            call msg("J. Chem. Phys. 137, 204121 (2012); doi: 10.1063/1.4768228")
            call dmsg("Adjustable parameter G (parallel-spin)", AUX_MODRZEJ2012_C_GPAR, fmt="F8.5")
            call dmsg("Adjustable parameter G (opposite-spin)", AUX_MODRZEJ2012_C_GOPP, fmt="F8.5")
            call dmsg("Coupling constant lambda", AUX_MODRZEJ2012_C_LAMBDA, fmt="F8.5")
            call msg("Definition of the system-averaged correlation hole:")
            call msg("<hc_ss'>(u) = int rho_s hc_ss'(r, u) dr")
            call msg("hc_aa = u**2 * (a_aa + b_aa * u + c_aa * u**2) * exp(-d_aa * u)")
            call msg("hc_ab = (a_ab + b_ab * u + c_ab * u**2) * exp(-d_ab * u)")
            call msg("Ec^ss' = int_0^inf 4Pi u**2 * <hc_ss'>(u) / (2u)")

            if (spin_uncompensated) then
                  write(line, "(A7,1X,A20,1X,A20,1X,A20)") "u", "<hc_aa>(u)", "<hc_ab>(u)", "<hc_bb>(u)"
                  call msg(line)
                  call midrule(width=70)
                  n = AUX_XC_HOLE_NPOINTS
                  do k = 1, n
                        u = (k-1) * AUX_XC_HOLE_SPACING
                        hc_aa = a(k)
                        hc_ab = a(n+k)
                        hc_bb = a(2*n+k)
                        write(line, "(F7.4,1X,F20.10,1X,F20.10,1X,F20.10)") u, hc_aa, hc_ab, hc_bb
                        call msg(line)
                  end do
            else
                  write(line, "(A7,1X,A20,1X,A20)") "u", "<hc_aa>(u)", "<hc_ab>(u)"
                  call msg(line)
                  call midrule(width=49)
                  n = AUX_XC_HOLE_NPOINTS
                  do k = 1, n
                        u = (k-1) * AUX_XC_HOLE_SPACING
                        hc_aa = a(k)
                        hc_ab = a(n+k)
                        write(line, "(F7.4,1X,F20.10,1X,F20.10)") u, hc_aa, hc_ab
                        call msg(line)
                  end do
            end if
      end subroutine modrzej2012_c_hole_finalize


      subroutine modrzej2016_x_hole_finalize(a, spin_uncompensated)
            real(F64), dimension(:), intent(in) :: a
            logical, intent(in)   :: spin_uncompensated
            character(80) :: line
            integer :: n, k
            real(F64) :: u
            real(F64) :: hx_a, hx_b

            select case (AUXINTEGRAL)
            case (AUX_BR_X_HOLE)
                  call msg("System-averaged Becke-Roussel exchange hole")
                  call msg("Phys. Rev. A, 39, 3761(1989); doi: 10.1103/PhysRevA.39.3761")
            case (AUX_PBE_X_HOLE)
                  call msg("System-averaged PBE/Becke-Roussel exchange hole")
                  call msg("hx integrates to the PBE exchange energy density, but the normalization constraint is relaxed")
            case (AUX_TPSS_X_HOLE)
                  call msg("System-averaged TPSS/Becke-Roussel exchange hole")
                  call msg("hx integrates to the PBE exchange energy density, but the normalization constraint is relaxed")
            case default
                  call msg("Invalid type of auxiliary integral", MSG_ERROR)
                  stop
            end select

            call msg("Definition of the system-averaged exchange hole:")
            call msg("<HxAlpha>(u) = Int RhoAlpha(r) HxAlpha(r, u) dr")
            call msg("ExAlpha = Int(0, Inf) 4Pi u**2 * <HxAlpha>(u) / (2u)")

            if (spin_uncompensated) then
                  call midrule(width=70)
                  write(line, "(A7,1X,A20,1X,A20)") "u [a.u.]", "<HxAlpha>(u)", "<HxBeta>(u)"
                  call msg(line)
                  call midrule(width=70)
                  n = AUX_XC_HOLE_NPOINTS
                  do k = 1, n
                        u = (k-1) * AUX_XC_HOLE_SPACING
                        hx_a = a(k)
                        hx_b = a(n+k)
                        write(line, "(F8.4,1X,F20.10,1X,F20.10)") u, hx_a, hx_b
                        call msg(line)
                  end do
            else
                  write(line, "(A8,1X,A20)") "u [a.u.]", "<HxAlpha>(u)"
                  call msg(line)
                  call midrule(width=49)
                  n = AUX_XC_HOLE_NPOINTS
                  do k = 1, n
                        u = (k-1) * AUX_XC_HOLE_SPACING
                        hx_a = a(k)
                        write(line, "(F8.4,1X,F20.10,1X)") u, hx_a
                        call msg(line)
                  end do
            end if
            call midrule(width=70)
      end subroutine modrzej2016_x_hole_finalize
      

      subroutine aux_xhole_dist_fukui_finalize(a, spin_uncompensated)
            real(F64), dimension(:), intent(inout) :: a
            logical, intent(in)                    :: spin_uncompensated

            real(F64) :: fukui_norm, norm
            real(F64) :: dx2_alpha, dx2_beta

            call msg("AVERAGE ELECTRON-EXCHANGE HOLE DISTANCE")
            call msg("WEIGHT FUNCTION: THE FUKUI FUNCTION")
            !
            ! Total norm of the Fukui function. This should be equal to 1.0.
            ! If not, there is a problem with the numerical integration.
            !
            fukui_norm = a(4)
            norm = a(3)

            if (spin_uncompensated) then
                  dx2_alpha = a(1) / norm
                  dx2_beta = a(2) / norm
                  call dmsg("<DX^2_ALPHA>", dx2_alpha)
                  call dmsg("<DX^2_BETA>", dx2_beta)
            else
                  dx2_alpha = a(1) / norm
                  dx2_beta = a(1) / norm
                  call dmsg("<DX^2>", dx2_alpha)
            end if
            call dmsg("FUKUI_NORM", fukui_norm)
            call dmsg("WEIGHT NORM", norm)

            a(1) = dx2_alpha
            a(2) = dx2_beta
      end subroutine aux_xhole_dist_fukui_finalize
      

      subroutine aux_gdd_grac_finalize(a, spin_uncompensated)
            real(F64), dimension(:), intent(inout)  :: a
            logical, intent(in)                     :: spin_uncompensated

            real(F64) :: average_dx2

            if (spin_uncompensated) then
                  !
                  ! Unequal alpha- and beta-spin contributions.
                  ! The contributions to <dx^2> will be weighted by
                  ! by spin-densities.
                  !
                  average_dx2 = (a(1) + a(3)) / (a(2) + a(4))
            else
                  !
                  ! Closed shell case
                  !
                  average_dx2 = a(1) / a(2)
            end if

            call msg("AVERAGE SQUARED ELECTRON-EXCHANGE HOLE DISTANCE")
            call msg("WEIGHTING FUNCTION: GRAC")
            call dmsg("ALPHA", GDD_GRAC_ALPHA)
            call dmsg("BETA", GDD_GRAC_BETA)
            if (spin_uncompensated) then
                  call dmsg("NORMALIZATION INTEGRAL", a(2) + a(4))
                  call msg("<dx^2> IS A WEIGHTED AVERAGE OF SPIN-RESOLVED CONTRIBUTIONS")
            else
                  call dmsg("NORMALIZATION INTEGRAL", a(2))
            end if
            call dmsg("<dx^2>", average_dx2)

            a(1) = average_dx2
      end subroutine aux_gdd_grac_finalize


      subroutine gga_auxint(aux, auxname, x0, y0, z0, r, x_spher, y_spher, z_spher, &
            nbatch, weights, rho, AUXIn, AtomElementMap, AOBasis, System)

            real(F64), dimension(:), intent(inout) :: aux
            integer, intent(in)                    :: auxname
            real(F64), intent(in)                  :: x0
            real(F64), intent(in)                  :: y0
            real(F64), intent(in)                  :: z0
            real(F64), intent(in)                  :: r
            real(F64), dimension(:), intent(in)    :: x_spher
            real(F64), dimension(:), intent(in)    :: y_spher
            real(F64), dimension(:), intent(in)    :: z_spher
            integer, intent(in)                    :: nbatch
            real(F64), dimension(:), intent(in)    :: weights
            real(F64), dimension(:), intent(in)    :: rho
            real(F64), dimension(:, :), intent(in) :: AUXIn
            integer, dimension(:), intent(in)      :: AtomElementMap
            type(TAOBasis), intent(in)             :: AOBasis
            type(TSystem), intent(in)              :: System

            real(F64) :: rho_tot
            real(F64) :: x, y, z
            integer :: k

            associate( &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NPrimitives => AOBasis%NPrimitives, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  AtomShellMap => AOBasis%AtomShellMap, &
                  AtomShellN => AOBasis%AtomShellN, &
                  MaxNShells => AOBasis%MaxNShells, &
                  AtomCoords => System%AtomCoords, &
                  RealAtoms => System%RealAtoms)

                  select case (auxname)
                  case (AUX_HIRSHFELD_VOLUME_FREE)
                        !
                        ! Hirshfeld volume of a free atom
                        !
                        do k = 1, nbatch
                              rho_tot = rho(k)
                              if (rho_tot > HIRSHFELD_RHO_TOL) then
                                    x = x0 + r * x_spher(k)
                                    y = y0 + r * y_spher(k)
                                    z = z0 + r * z_spher(k)
                                    call hirshfeld_volume_free(aux, weights(k), x, y, z, AtomCoords(:, 1), &
                                          AUXIn(:, 1), ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, &
                                          ShellMomentum, AtomShellMap, AtomShellN, MaxNShells)
                              end if
                        end do

                  case (AUX_HIRSHFELD_VOLUME)
                        !
                        ! Hirshfeld volumes of atoms in a molecule
                        !
                        do k = 1, nbatch
                              rho_tot = rho(k)
                              if (rho_tot > HIRSHFELD_RHO_TOL) then
                                    x = x0 + r * x_spher(k)
                                    y = y0 + r * y_spher(k)
                                    z = z0 + r * z_spher(k)
                                    call hirshfeld_volume(aux, weights(k), x, y, z, rho_tot, AUXIn, &
                                          ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, ShellMomentum, &
                                          AtomShellMap, AtomShellN, MaxNShells, RealAtoms, AtomCoords, AtomElementMap)
                              end if
                        end do

                  case (AUX_HIRSHFELD_POPULATION)
                        !
                        ! Hirshfeld charges of atoms in a molecule
                        !
                        do k = 1, nbatch
                              rho_tot = rho(k)
                              if (rho_tot > HIRSHFELD_RHO_TOL) then
                                    x = x0 + r * x_spher(k)
                                    y = y0 + r * y_spher(k)
                                    z = z0 + r * z_spher(k)
                                    call hirshfeld_population(aux, weights(k), x, y, z, rho_tot, AUXIn, &
                                          ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, ShellMomentum, &
                                          AtomShellMap, AtomShellN, MaxNShells, RealAtoms, AtomCoords, AtomElementMap)
                              end if
                        end do
                  end select
            end associate
      end subroutine gga_auxint


      subroutine ugga_auxint(aux, auxname, x0, y0, z0, r, x_spher, y_spher, z_spher, &
            nbatch, weights, rho, AUXIn, AtomElementMap, AOBasis, System)

            real(F64), dimension(:), intent(inout) :: aux
            integer, intent(in)                    :: auxname
            real(F64), intent(in)                  :: x0
            real(F64), intent(in)                  :: y0
            real(F64), intent(in)                  :: z0
            real(F64), intent(in)                  :: r
            real(F64), dimension(:), intent(in)    :: x_spher
            real(F64), dimension(:), intent(in)    :: y_spher
            real(F64), dimension(:), intent(in)    :: z_spher
            integer, intent(in)                    :: nbatch
            real(F64), dimension(:), intent(in)    :: weights
            real(F64), dimension(:, :), intent(in) :: rho
            real(F64), dimension(:, :), intent(in) :: AUXIn
            integer, dimension(:), intent(in)      :: AtomElementMap
            type(TAOBasis)                         :: AOBasis
            type(TSystem)                          :: System

            real(F64) :: x, y, z
            integer :: k
            real(F64) :: rho_tot

            associate( &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NPrimitives => AOBasis%NPrimitives, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  AtomShellMap => AOBasis%AtomShellMap, &
                  AtomShellN => AOBasis%AtomShellN, &
                  MaxNShells => AOBasis%MaxNShells, &
                  AtomCoords => System%AtomCoords, &
                  RealAtoms => System%RealAtoms)

                  select case (auxname)
                  case (AUX_HIRSHFELD_VOLUME_FREE)
                        !
                        ! The volume of a free atom.
                        ! The following code should be used only for single-atom systems.
                        ! Note that the computed density at the current grid point isn't used;
                        ! instead, the subroutine uses a spherically averaged density.
                        !
                        do k = 1, nbatch
                              rho_tot = rho(1, k) + rho(2, k)
                              if (rho_tot > HIRSHFELD_RHO_TOL) then
                                    x = x0 + r * x_spher(k)
                                    y = y0 + r * y_spher(k)
                                    z = z0 + r * z_spher(k)
                                    call hirshfeld_volume_free(aux, weights(k), x, y, z, AtomCoords(:, 1), &
                                          AUXIn(:, 1), ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, &
                                          ShellMomentum, AtomShellMap, AtomShellN, MaxNShells)                              
                              end if
                        end do

                  case (AUX_HIRSHFELD_VOLUME)
                        !
                        ! Hirshfeld volumes of atoms in a molecule
                        !
                        do k = 1, nbatch
                              rho_tot = rho(1, k) + rho(2, k)
                              if (rho_tot > HIRSHFELD_RHO_TOL) then
                                    x = x0 + r * x_spher(k)
                                    y = y0 + r * y_spher(k)
                                    z = z0 + r * z_spher(k)
                                    call hirshfeld_volume(aux, weights(k), x, y, z, rho_tot, AUXIn, &
                                          ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, ShellMomentum, &
                                          AtomShellMap, AtomShellN, MaxNShells, RealAtoms, AtomCoords, AtomElementMap)
                              end if
                        end do

                  case (AUX_HIRSHFELD_POPULATION)
                        !
                        ! Hirshfeld charges of atoms in a molecule
                        !
                        do k = 1, nbatch
                              rho_tot = rho(1, k) + rho(2, k)
                              if (rho_tot > HIRSHFELD_RHO_TOL) then
                                    x = x0 + r * x_spher(k)
                                    y = y0 + r * y_spher(k)
                                    z = z0 + r * z_spher(k)
                                    call hirshfeld_population(aux, weights(k), x, y, z, rho_tot, AUXIn, &
                                          ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, ShellMomentum, &
                                          AtomShellMap, AtomShellN, MaxNShells, RealAtoms, AtomCoords, AtomElementMap)
                              end if
                        end do
                  end select
            end associate
      end subroutine ugga_auxint


      subroutine mgga_auxint(aux, auxname, x0, y0, z0, r, x_spher, &
            y_spher, z_spher, nbatch, weights, rho, sigma, lapl, &
            tau, orbval, shellidx, n0, deltak)
            
            real(F64), dimension(:), intent(inout)             :: aux
            integer, intent(in)                                :: auxname
            real(F64), intent(in)                              :: x0
            real(F64), intent(in)                              :: y0
            real(F64), intent(in)                              :: z0
            real(F64), intent(in)                              :: r
            real(F64), dimension(:), intent(in)                :: x_spher
            real(F64), dimension(:), intent(in)                :: y_spher
            real(F64), dimension(:), intent(in)                :: z_spher
            integer, intent(in)                                :: nbatch
            real(F64), dimension(:), intent(in)                :: weights
            real(F64), dimension(:), intent(in)                :: rho
            real(F64), dimension(:), intent(in)                :: sigma
            real(F64), dimension(:), intent(in)                :: lapl
            real(F64), dimension(:), intent(in)                :: tau
            real(F64), dimension(:, :), intent(in)             :: orbval
            integer, dimension(:, :), intent(in)               :: shellidx
            integer, dimension(:), intent(in)                  :: n0
            integer, intent(in)                                :: deltak

            real(F64) :: x, y, z
            real(F64) :: rhoa, sigma_aa, lapla, taua, vxc_inf, vc0
            real(F64) :: d2
            integer :: k

            select case (auxname)
            case (AUX_EXCHDIP_BR89)
                  !
                  ! Becke-Roussel approximation to the exchange-hole
                  ! dipole moment
                  !
                  do k = 1, nbatch
                        rhoa = frac12 * rho(k)
                        taua = frac12 * tau(k)
                        if (rhoa > br89_rho_tol .and. taua > br89_tau_tol) then
                              sigma_aa = frac14 * sigma(k)
                              lapla = frac12 * lapl(k)
                              call exchdipole_br89(d2, rhoa, sigma_aa, lapla, taua)
                              aux(1) = aux(1) + weights(k) * TWO * d2
                        end if
                  end do

            case (AUX_GDD_OMEGA)
                  !
                  ! Exchange-hole dipole moment of the exact exchange hole integrated
                  ! with a normalized probability distribution
                  !
                  do k = 1, nbatch
                        if (n0(k) > 0) then
                              rhoa = FRAC12 * rho(k)
                              taua = FRAC12 * tau(k)
                              if (rhoa > NORM_EXCHDIP_EXX_RHO_TOL .and. &
                                    taua > NORM_EXCHDIP_EXX_TAU_TOL) then
                                    sigma_aa = FRAC14 * sigma(k)
                                    x = x0 + r * x_spher(k)
                                    y = y0 + r * y_spher(k)
                                    z = z0 + r * z_spher(k)
                                    call norm_exchdipole_exx(aux, weights(k), x, y, z, rhoa, &
                                          taua, orbval(:, k), shellidx(:, k), n0(k), &
                                          deltak, DMATAX, DMATAY, DMATAZ)
                              end if
                        end if
                  end do

            case (AUX_DENSITY_COMPARE)
                  do k = 1, nbatch
                        if (n0(k) > 0) then
                              call density_rmsd(aux, DENSITY_COMPARE_RHO, Rho(k), Sigma(k), Lapl(k), weights(k), &
                                    orbval(:, k), shellidx(:, k), n0(k))
                        end if
                  end do

            case (AUX_XHOLE_DIST_FUKUI)
                  !
                  ! Average electron-exchange hole distance (average exchange-hole dipole moment)
                  ! for the exact exchange hole. The Fukui function is the weight function for
                  ! this average.
                  !
                  do k = 1, nbatch
                        if (n0(k) > 0) then
                              rhoa = FRAC12 * rho(k)
                              if (rhoa > RHO_MIN_XHOLE_DIST_FUKUI) then
                                    x = x0 + r * x_spher(k)
                                    y = y0 + r * y_spher(k)
                                    z = z0 + r * z_spher(k)
                                    call xhole_dist_fukui(aux, weights(k), x, y, z, rhoa, orbval(:, k), &
                                          shellidx(:, k), n0(k), deltak, DMATAX, DMATAY, DMATAZ, &
                                          FUKUI_MATRIX)
                              end if
                        end if
                  end do

            case (AUX_MODRZEJ2012_C_HOLE)
                  !
                  ! System-averaged meta-GGA correlation hole of Modrzejewski et al.
                  ! J. Chem. Phys. 137, 204121 (2012); doi: 10.1063/1.4768228
                  !
                  do k = 1, nbatch
                        call modrzej2012_c_hole_average(aux, AUX_XC_HOLE_NPOINTS, &
                              AUX_XC_HOLE_SPACING, rho(k), sigma(k), tau(k), weights(k), &
                              AUX_MODRZEJ2012_C_GPAR, AUX_MODRZEJ2012_C_GOPP, AUX_MODRZEJ2012_C_LAMBDA)
                  end do

            case (AUX_MCSv2_VXC_INF)
                  do k = 1, nbatch
                        call modrzej2016_vxc_inf(vxc_inf, rho(k)/TWO, rho(k)/TWO, sigma(k)/FOUR, sigma(k)/FOUR)
                        aux(1) = aux(1) + weights(k) * vxc_inf
                  end do

            case (AUX_TPSS_VXC_INF)
                  do k = 1, nbatch
                        call u_tpss_vxc_inf(vxc_inf, rho(k)/TWO, rho(k)/TWO, sigma(k)/FOUR, sigma(k)/FOUR, &
                              sigma(k)/FOUR, tau(k)/TWO, tau(k)/TWO)
                        aux(1) = aux(1) + weights(k) * vxc_inf
                  end do

            case (AUX_MCSv1_VC_LAMBDA)
                  do k = 1, nbatch
                        call modrzej2012_vc_curve(aux, weights(k), rho(k)/TWO, rho(k)/TWO, sigma(k)/FOUR, sigma(k)/FOUR, &
                              sigma(k)/FOUR, tau(k)/TWO, tau(k)/TWO, AUX_MODRZEJ2012_C_GPAR, AUX_MODRZEJ2012_C_GOPP)
                  end do

            case (AUX_MCSv2_VC_LAMBDA)
                  do k = 1, nbatch
                        call modrzej2016_vc_curve(aux, weights(k), rho(k)/TWO, rho(k)/TWO, sigma(k)/FOUR, sigma(k)/FOUR, &
                              sigma(k)/FOUR, tau(k)/TWO, tau(k)/TWO)
                  end do

            case (AUX_MCSv1_VC0)
                  do k = 1, nbatch
                        call modrzej2012_vc_0_deriv(vc0, rho(k)/TWO, rho(k)/TWO, sigma(k)/FOUR, sigma(k)/FOUR, sigma(k)/FOUR, &
                              tau(k)/TWO, tau(k)/TWO, AUX_MODRZEJ2012_C_GPAR, AUX_MODRZEJ2012_C_GOPP)
                        aux(1) = aux(1) + weights(k) * vc0
                  end do
                  
            case (AUX_BR_X_HOLE)
                  !
                  ! System-averaged exchange hole of Becke, A.D., Roussel, M.R.,
                  ! Phys. Rev. A, 39, 3761(1989); doi: 10.1103/PhysRevA.39.3761
                  !
                  do k = 1, nbatch
                        call br89_x_hole_average(aux(1:AUX_XC_HOLE_NPOINTS), &
                              AUX_XC_HOLE_NPOINTS, AUX_XC_HOLE_SPACING, rho(k), &
                              sigma(k), lapl(k), tau(k), weights(k))
                  end do

            case (AUX_PBE_X_HOLE)
                  !
                  ! System-averaged generalized Becke-Roussel exchange hole
                  ! which integrates to the PBE exchange energy density
                  !
                  do k = 1, nbatch
                        call pbe_x_hole_average(aux(1:AUX_XC_HOLE_NPOINTS), &
                              AUX_XC_HOLE_NPOINTS, AUX_XC_HOLE_SPACING, rho(k), &
                              sigma(k), lapl(k), tau(k), weights(k))
                  end do

            case (AUX_TPSS_X_HOLE)
                  !
                  ! System-averaged generalized Becke-Roussel exchange hole
                  ! which integrates to the TPSS exchange energy density
                  !
                  do k = 1, nbatch
                        call tpss_x_hole_average(aux(1:AUX_XC_HOLE_NPOINTS), &
                              AUX_XC_HOLE_NPOINTS, AUX_XC_HOLE_SPACING, rho(k), &
                              sigma(k), lapl(k), tau(k), weights(k))
                  end do
            end select
      end subroutine mgga_auxint


      subroutine umgga_auxint(aux, auxname, x0, y0, z0, r, x_spher, &
            y_spher, z_spher, nbatch, weights, rho, sigma, lapl, tau, &
            orbval, shellidx, n0, deltak)
            
            real(F64), dimension(:), intent(inout)             :: aux
            integer, intent(in)                                :: auxname
            real(F64), intent(in)                              :: x0
            real(F64), intent(in)                              :: y0
            real(F64), intent(in)                              :: z0
            real(F64), intent(in)                              :: r
            real(F64), dimension(:), intent(in)                :: x_spher
            real(F64), dimension(:), intent(in)                :: y_spher
            real(F64), dimension(:), intent(in)                :: z_spher
            integer, intent(in)                                :: nbatch
            real(F64), dimension(:), intent(in)                :: weights
            real(F64), dimension(:, :), intent(in)             :: rho
            real(F64), dimension(:, :), intent(in)             :: sigma
            real(F64), dimension(:, :), intent(in)             :: lapl
            real(F64), dimension(:, :), intent(in)             :: tau
            real(F64), dimension(:, :), intent(in)             :: orbval
            integer, dimension(:, :), intent(in)               :: shellidx
            integer, dimension(:), intent(in)                  :: n0
            integer, intent(in)                                :: deltak

            real(F64) :: x, y, z
            real(F64) :: rhoa, sigma_aa, lapla, taua
            real(F64) :: rhob, sigma_bb, laplb, taub
            real(F64) :: d2
            real(F64) :: vxc_inf, vc_aa, vc_ab, vc_bb, vc0
            integer :: k
            
            select case (auxname)
            case (AUX_EXCHDIP_BR89)
                  !
                  ! Becke-Roussel approximation to the exchange-hole
                  ! dipole moment
                  !
                  do k = 1, nbatch
                        rhoa = rho(1, k)
                        taua = tau(1, k)
                        if (rhoa > br89_rho_tol .and. taua > br89_tau_tol) then
                              sigma_aa = sigma(1, k)
                              lapla = lapl(1, k)
                              call exchdipole_br89(d2, rhoa, sigma_aa, lapla, taua)
                              aux(1) = aux(1) + weights(k) * d2
                        end if

                        rhob = rho(2, k)
                        taub = tau(2, k)
                        if (rhob > br89_rho_tol .and. taub > br89_tau_tol) then
                              sigma_bb = sigma(3, k)
                              laplb = lapl(2, k)
                              call exchdipole_br89(d2, rhob, sigma_bb, laplb, taub)
                              aux(1) = aux(1) + weights(k) * d2
                        end if
                  end do

            case (AUX_GDD_OMEGA)
                  do k = 1, nbatch
                        if (n0(k) > 0) then
                              !
                              ! Exchange-hole dipole moment of the exact exchange hole integrated
                              ! with a normalized probability distribution
                              ! ---
                              ! Alpha spin
                              !
                              rhoa = rho(1, k)
                              taua = tau(1, k)
                              if (rhoa > NORM_EXCHDIP_EXX_RHO_TOL .and. &
                                    taua > NORM_EXCHDIP_EXX_TAU_TOL) then
                                    sigma_aa = sigma(1, k)
                                    x = x0 + r * x_spher(k)
                                    y = y0 + r * y_spher(k)
                                    z = z0 + r * z_spher(k)
                                    call norm_exchdipole_exx(aux, weights(k), x, y, z, rhoa, &
                                          taua, orbval(:, k), shellidx(:, k), n0(k), &
                                          deltak, DMATAX, DMATAY, DMATAZ)
                              end if
                              !
                              ! Beta spin
                              !
                              rhob = rho(2, k)
                              taub = tau(2, k)
                              if (rhob > NORM_EXCHDIP_EXX_RHO_TOL .and. &
                                    taub > NORM_EXCHDIP_EXX_TAU_TOL) then
                                    sigma_bb = sigma(3, k)
                                    x = x0 + r * x_spher(k)
                                    y = y0 + r * y_spher(k)
                                    z = z0 + r * z_spher(k)
                                    call norm_exchdipole_exx(aux, weights(k), x, y, z, rhob, &
                                          taub, orbval(:, k), shellidx(:, k), n0(k), &
                                          deltak, DMATBX, DMATBY, DMATBZ)
                              end if
                        end if
                  end do

            case (AUX_MODRZEJ2012_C_HOLE)
                  !
                  ! System-averaged meta-GGA correlation hole of Modrzejewski et al.
                  ! J. Chem. Phys. 137, 204121 (2012); doi: 10.1063/1.4768228
                  !
                  do k = 1, nbatch
                        call umodrzej2012_c_hole_average(aux, AUX_XC_HOLE_NPOINTS, &
                              AUX_XC_HOLE_SPACING, rho(1, k), rho(2, k), sigma(1, k), &
                              sigma(2, k), sigma(3, k), tau(1, k), tau(2, k), weights(k), &
                              AUX_MODRZEJ2012_C_GPAR, AUX_MODRZEJ2012_C_GOPP, &
                              AUX_MODRZEJ2012_C_LAMBDA)
                  end do

            case (AUX_MCSv2_VXC_INF)
                  do k = 1, nbatch
                        call modrzej2016_vxc_inf(vxc_inf, rho(1, k), rho(2, k), sigma(1, k), sigma(3, k))
                        aux(1) = aux(1) + weights(k) * vxc_inf
                  end do

            case (AUX_TPSS_VXC_INF)
                  do k = 1, nbatch
                        call u_tpss_vxc_inf(vxc_inf, rho(1, k), rho(2, k), sigma(1, k), sigma(2, k), &
                              sigma(3, k), tau(1, k), tau(2, k))
                        aux(1) = aux(1) + weights(k) * vxc_inf
                  end do

            case (AUX_MCSv1_VC_LAMBDA)
                  do k = 1, nbatch
                        call modrzej2012_vc_lambda(vc_aa, vc_ab, vc_bb, rho(1, k), rho(2, k), sigma(1, k), sigma(2, k), &
                              sigma(3, k), tau(1, k), tau(2, k), AUX_MODRZEJ2012_C_GPAR, AUX_MODRZEJ2012_C_GOPP, &
                              AUX_MODRZEJ2012_C_LAMBDA)
                        aux(1) = aux(1) + weights(k) * vc_aa
                        aux(2) = aux(2) + weights(k) * vc_ab
                        aux(3) = aux(3) + weights(k) * vc_bb
                  end do

            case (AUX_MCSv1_VC0)
                  do k = 1, nbatch
                        call modrzej2012_vc_0_deriv(vc0, rho(1, k), rho(2, k), sigma(1, k), sigma(2, k), sigma(3, k), &
                              tau(1, k), tau(2, k), AUX_MODRZEJ2012_C_GPAR, AUX_MODRZEJ2012_C_GOPP)
                        aux(1) = aux(1) + weights(k) * vc0
                  end do
                  
            case (AUX_BR_X_HOLE)
                  !
                  ! System-averaged exchange hole of Becke, A.D., Roussel, M.R.,
                  ! Phys. Rev. A, 39, 3761(1989); doi: 10.1103/PhysRevA.39.3761
                  !
                  do k = 1, nbatch
                        rhoa = rho(1, k)
                        rhob = rho(2, k)
                        sigma_aa = sigma(1, k)
                        sigma_bb = sigma(3, k)
                        lapla = lapl(1, k)
                        laplb = lapl(2, k)
                        taua = tau(1, k)
                        taub = tau(2, k)
                        call br89_x_hole_average(aux(1:AUX_XC_HOLE_NPOINTS), &
                              AUX_XC_HOLE_NPOINTS, AUX_XC_HOLE_SPACING, TWO*rhoa, &
                              FOUR*sigma_aa, TWO*lapla, TWO*taua, weights(k))
                        call br89_x_hole_average(aux(AUX_XC_HOLE_NPOINTS+1:), &
                              AUX_XC_HOLE_NPOINTS, AUX_XC_HOLE_SPACING, TWO*rhob, &
                              FOUR*sigma_bb, TWO*laplb, TWO*taub, weights(k))
                  end do

            case (AUX_PBE_X_HOLE)
                  do k = 1, nbatch
                        rhoa = rho(1, k)
                        rhob = rho(2, k)
                        sigma_aa = sigma(1, k)
                        sigma_bb = sigma(3, k)
                        lapla = lapl(1, k)
                        laplb = lapl(2, k)
                        taua = tau(1, k)
                        taub = tau(2, k)
                        call pbe_x_hole_average(aux(1:AUX_XC_HOLE_NPOINTS), &
                              AUX_XC_HOLE_NPOINTS, AUX_XC_HOLE_SPACING, TWO*rhoa, &
                              FOUR*sigma_aa, TWO*lapla, TWO*taua, weights(k))
                        call pbe_x_hole_average(aux(AUX_XC_HOLE_NPOINTS+1:), &
                              AUX_XC_HOLE_NPOINTS, AUX_XC_HOLE_SPACING, TWO*rhob, &
                              FOUR*sigma_bb, TWO*laplb, TWO*taub, weights(k))
                  end do

            case (AUX_TPSS_X_HOLE)
                  do k = 1, nbatch
                        rhoa = rho(1, k)
                        rhob = rho(2, k)
                        sigma_aa = sigma(1, k)
                        sigma_bb = sigma(3, k)
                        lapla = lapl(1, k)
                        laplb = lapl(2, k)
                        taua = tau(1, k)
                        taub = tau(2, k)
                        call tpss_x_hole_average(aux(1:AUX_XC_HOLE_NPOINTS), &
                              AUX_XC_HOLE_NPOINTS, AUX_XC_HOLE_SPACING, TWO*rhoa, &
                              FOUR*sigma_aa, TWO*lapla, TWO*taua, weights(k))
                        call tpss_x_hole_average(aux(AUX_XC_HOLE_NPOINTS+1:), &
                              AUX_XC_HOLE_NPOINTS, AUX_XC_HOLE_SPACING, TWO*rhob, &
                              FOUR*sigma_bb, TWO*laplb, TWO*taub, weights(k))
                  end do
            end select
      end subroutine umgga_auxint


      pure subroutine regularized_vnucl(a, weight, x, y, z, orbval, shellidx, n0, deltak, eta, RealAtoms)
            real(F64), dimension(:), intent(inout) :: a
            real(F64), intent(in)                  :: weight
            real(F64), intent(in)                  :: x
            real(F64), intent(in)                  :: y
            real(F64), intent(in)                  :: z
            real(F64), dimension(:), intent(in)    :: orbval
            integer, dimension(:), intent(in)      :: shellidx
            integer, intent(in)                    :: n0
            integer, intent(in)                    :: deltak
            real(F64), intent(in)                  :: eta
            integer, dimension(2, 2), intent(in)   :: RealAtoms

            integer :: k, l, u, uu, p, q, v, vv
            integer :: compound_idx
            real(F64) :: p_val, q_val
            real(F64) :: t, t_p_val

            t = weight * vnucl_reg(x, y, z, eta, RealAtoms)
            k = 1
            shloop1: do uu = 1, n0
                  u = shellidx(uu)
                  ploop: do p =  shpos(u), shpos(u + 1) - 1
                        p_val  = orbval(k)
                        t_p_val = t * p_val
                        l = k
                        k = k + deltak
                        !
                        ! Diagonal element
                        !
                        compound_idx = pq2compound(p, p, NORB)
                        a(compound_idx) = a(compound_idx) + p_val * t_p_val
                        l = l + deltak
                        !
                        ! Diagonal block
                        !
                        do q = p + 1, shpos(u + 1) - 1
                              q_val  = orbval(l)
                              l = l + deltak
                              compound_idx = pq2compound(q, p, NORB)
                              a(compound_idx) = a(compound_idx) + q_val * t_p_val
                        end do
                        do vv = uu + 1, n0
                              v = shellidx(vv)
                              do q = shpos(v), shpos(v + 1) - 1
                                    q_val  = orbval(l)
                                    l = l + deltak
                                    compound_idx = pq2compound(q, p, NORB)
                                    a(compound_idx) = a(compound_idx) + q_val * t_p_val
                              end do
                        end do
                  end do ploop
            end do shloop1
      end subroutine regularized_vnucl


      pure function vnucl_reg(x, y, z, eta, RealAtoms)
            real(F64)                            :: vnucl_reg
            real(F64), intent(in)                :: x
            real(F64), intent(in)                :: y
            real(F64), intent(in)                :: z
            real(F64), intent(in)                :: eta
            integer, dimension(2, 2), intent(in) :: RealAtoms
            
            real(F64) :: w, fk, t0, t1, t2, t3, t4, t5
            real(F64) :: znum, r, r2
            integer :: k, s
            real(F64), parameter :: r_thresh = 1.0E-2_F64

            vnucl_reg = ZERO

            do s = 1, 2
                  do k = RealAtoms(1, s), RealAtoms(2, s)
                        znum = real(ECP_INUCLZ(k), F64)
                        r2 = (x - ATOMR(1, k))**2 + (y - ATOMR(2, k))**2 + (z - ATOMR(3, k))**2 
                        r = sqrt(r2)
                        w = eta * r2
                        !
                        ! Use Taylor series for small eta*r**2 to improve 
                        ! numerical stability
                        !
                        if (w > r_thresh) then
                              fk = -znum * (ONE - exp(-w)) / r
                        else
                              t0 = w * (-ONE/720.0_F64)
                              t1 = w * (ONE/120.0_F64 + t0)
                              t2 = w * (-ONE/24.0_F64 + t1)
                              t3 = w * (ONE/SIX + t2)
                              t4 = w * (-ONE/TWO + t3)
                              t5 = eta * r * (ONE + t4)
                              fk = -znum * t5
                        end if
                        vnucl_reg = vnucl_reg + fk
                  end do
            end do
      end function vnucl_reg


      pure subroutine exchdipole_br89(d2, rhoa, sigma_aa, lapla, taua)
            ! ------------------------------------------------------------------------
            ! Compute contribution to the mean square of the exchange-hole
            ! dipole moment, <d_x^2>, at a given grid point. Here, the dipole moment
            ! of the Becke-Roussel exchange hole and the reference electron is
            ! used to approximate the exact dipole, see Refs [1, 2, 3].
            ! -------------------------------------------------------------------------
            ! 1. Becke, A., Roussel, M., Exchange holes in inhomogeneous
            !    systems: A coordinate-space model, Phys. Rev. A, 39, 3761 (1989)
            ! 2. Becke, A. D., Johnson, E. R., A density-functional model of
            !    the dispersion interaction, J. Chem. Phys. 123, 154101 (2005)
            ! 3. Gori-Giorgi, P., Angyan, J. G., and Savin, A., Charge
            !    density reconstitution from approximate exchange-
            !    correlation holes, Can. J. Chem. 87, 1444 (2009)
            !
            real(F64), intent(out) :: d2
            real(F64), intent(in)  :: rhoa
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: lapla
            real(F64), intent(in)  :: taua

            real(F64) :: eps, vrho, vsigma, vlapl, vtau
            real(F64) :: ux, urho, usigma, ulapl, utau, dipole

            call uxbr89(rhoa, sigma_aa, lapla, taua, eps, vrho, &
                  vsigma, vlapl, vtau, ux, urho, usigma, ulapl, utau, &
                  dipole)
            d2 = rhoa * dipole**2
      end subroutine exchdipole_br89


      pure subroutine exchdipole_exx(d2, x, y, z, rhoa, orbval, shellidx, n0, deltak, &
            dmatax, dmatay, dmataz)
            ! ------------------------------------------------------------------------
            ! Compute contribution to the mean square of the exchange-hole
            ! dipole moment, <d_x^2>, at a given grid point. Here, the exact HF-like
            ! exchange hole is used to compute the dipole moment, see Ref. [1].
            ! This subroutine is applicable to spin-unpolarized systems.
            ! -------------------------------------------------------------------------
            ! 1. Becke, A. D., Johnson, E. R., A density-functional model of
            !    the dispersion interaction, J. Chem. Phys. 123, 154101 (2005)
            !
            real(F64), intent(out)                 :: d2
            real(F64), intent(in)                  :: x
            real(F64), intent(in)                  :: y
            real(F64), intent(in)                  :: z
            real(F64), intent(in)                  :: rhoa
            real(F64), dimension(:), intent(in)    :: orbval
            integer, dimension(:), intent(in)      :: shellidx
            integer, intent(in)                    :: n0
            integer, intent(in)                    :: deltak
            real(F64), dimension(:, :), intent(in) :: dmatax
            real(F64), dimension(:, :), intent(in) :: dmatay
            real(F64), dimension(:, :), intent(in) :: dmataz
            
            real(F64), dimension(3) :: dx
            
            if (rhoa > EXCHDIP_EXX_RHO_TOL) then
                  call vector_rho(dx, dmatax, dmatay, dmataz, orbval, shellidx, n0, deltak)
                  !
                  ! DX equal to the vector defined in Eq. [15] in Ref. [1].
                  ! dx = (\frac{1}{\rho_\sigma} \sum_{ij} r_{ij\sigma} \psi_{i\sigma} \psi_{j\sigma}) - r
                  !
                  dx(1) = dx(1) / rhoa - x
                  dx(2) = dx(2) / rhoa - y
                  dx(3) = dx(3) / rhoa - z
                  !
                  ! D2 is the function defined in Eq. 24 in Ref. [1].
                  !
                  d2 = rhoa * (dx(1)**2 + dx(2)**2 + dx(3)**2)
                  !
                  ! This subroutine is applicable to spin-polarized systems.
                  ! Alpha and beta-spin contributions are equal. The returned
                  ! value is the contribution to D2 from both spins.
                  !
                  d2 = TWO * d2
            else
                  d2 = ZERO
            end if
      end subroutine exchdipole_exx


      pure subroutine taus_heg(taus, rhos)
            !
            ! The density of the sigma-spin kinetic energy
            ! of the homogeneous electron gas. The 1/2 factor
            ! is included.
            !
            real(F64), intent(out) :: taus
            real(F64), intent(in)  :: rhos

            taus = THREE / TEN * (SIX * PI**2)**FRAC23 * rhos**FRAC53
      end subroutine taus_heg


      pure subroutine norm_exchdipole_exx(a, weight, x, y, z, rhoa, taua, orbval, &
            shellidx, n0, deltak, dmatax, dmatay, dmataz)
            ! ---------------------------------------------------------------------------
            ! Compute contribution to the mean square of the exchange-hole
            ! dipole moment, <d_x^2>, at a given grid point. The exchange-hole dipole
            ! moment is averaged with a normalized probability distribution. The exact
            ! HF-like exchange hole is used to compute the dipole moment, see Ref. [1].
            !
            ! This subroutine should be called only if RHOA and TAUA are numerically
            ! non-negligible.
            ! ---------------------------------------------------------------------------
            ! 1. Becke, A. D., Johnson, E. R., A density-functional model of
            !    the dispersion interaction, J. Chem. Phys. 123, 154101 (2005)
            ! 2. Becke, A. D., Simulation of delocalized exchange by local density
            !    functionals, J. Chem. Phys. 112, 4020 (2000)
            !
            real(F64), dimension(:), intent(inout) :: a
            real(F64), intent(in)                  :: weight
            real(F64), intent(in)                  :: x
            real(F64), intent(in)                  :: y
            real(F64), intent(in)                  :: z
            real(F64), intent(in)                  :: rhoa
            real(F64), intent(in)                  :: taua
            real(F64), dimension(:), intent(in)    :: orbval
            integer, dimension(:), intent(in)      :: shellidx
            integer, intent(in)                    :: n0
            integer, intent(in)                    :: deltak
            real(F64), dimension(:, :), intent(in) :: dmatax
            real(F64), dimension(:, :), intent(in) :: dmatay
            real(F64), dimension(:, :), intent(in) :: dmataz

            real(F64) :: ta
            real(F64) :: dipcontrib, normcontrib
            integer :: idx, l, m
            real(F64), dimension(3) :: dx
            real(F64) :: d2
            !
            ! TA is the ratio of the UEG kinetic energy and the
            ! exact kinetic energy at a given point, see Eq. 23
            ! in Ref. [2]. The range of the TA variable is semi-infinite,
            ! 0 <= TA <= INFINITY. In exponential tails, TA approaches
            ! 0, which enables us to detect the part of the electron density
            ! which contributes chiefly to the exchange-hole dipole moment.
            ! Note that the density of the kinetic energy is defined without
            ! 1/2 factor.
            !
            ta = THREE / FIVE * (SIX * PI**2)**FRAC23 * rhoa**FRAC53
            ta = ta / taua
            !
            ! Find out to which bins the current grid point will contribute
            !
            call binary_search(SHIFT_PARAM, NORM_EXCHDIP_NPOINT, ta, idx)
            !
            ! IDX is now equal to the largest integer I satisfying
            ! SHIFT_PARAM(I) >= TA.
            ! Note that numbers in SHIFT_PARAM array are sorted in
            ! decreasing order.
            ! ---
            ! Compute dipole moment vector of the exact exchange hole
            !
            call vector_rho(dx, dmatax, dmatay, dmataz, orbval, shellidx, n0, deltak)
            !
            ! DX is the vector defined in Eq. [15] in Ref. [1].
            ! dx = (\frac{1}{\rho_\sigma} \sum_{ij} r_{ij\sigma} \psi_{i\sigma} \psi_{j\sigma}) - r
            !
            dx(1) = dx(1) / rhoa - x
            dx(2) = dx(2) / rhoa - y
            dx(3) = dx(3) / rhoa - z
            !
            ! D2 is the function defined in Eq. 24 in Ref. [1].
            !
            d2 = rhoa * (dx(1)**2 + dx(2)**2 + dx(3)**2)
            dipcontrib = weight * d2
            normcontrib = weight * rhoa
            do l = 1, idx
                  m = 2 * l
                  !
                  ! Integral of the exchange-hole dipole moment
                  !
                  a(m-1) = a(m-1) + dipcontrib
                  !
                  ! Normalizing constant
                  !
                  a(m) = a(m) + normcontrib
            end do
      end subroutine norm_exchdipole_exx
      

      pure subroutine xhole_dist_fukui(a, grid_weight, x, y, z, rhoa, orbval, &
            shellidx, n0, deltak, dmatax, dmatay, dmataz, fukui_matrix)
            ! ---------------------------------------------------------------------------
            ! Compute the average exchange hole-electron distance (i.e. exchange-hole
            ! dipole moment). The Fukui function is the weight function for this average.
            ! The exact HF-like exchange hole is used to compute the dipole moment, see
            ! Ref. [1].
            !
            ! This subroutine should be called only if RHOA and TAUA are numerically
            ! non-negligible.
            ! ---------------------------------------------------------------------------
            ! 1. Becke, A. D., Johnson, E. R., A density-functional model of
            !    the dispersion interaction, J. Chem. Phys. 123, 154101 (2005)
            !
            real(F64), dimension(:), intent(inout) :: a
            real(F64), intent(in)                  :: grid_weight
            real(F64), intent(in)                  :: x
            real(F64), intent(in)                  :: y
            real(F64), intent(in)                  :: z
            real(F64), intent(in)                  :: rhoa
            real(F64), dimension(:), intent(in)    :: orbval
            integer, dimension(:), intent(in)      :: shellidx
            integer, intent(in)                    :: n0
            integer, intent(in)                    :: deltak
            real(F64), dimension(:, :), intent(in) :: dmatax
            real(F64), dimension(:, :), intent(in) :: dmatay
            real(F64), dimension(:, :), intent(in) :: dmataz
            real(F64), dimension(:, :), intent(in) :: fukui_matrix

            real(F64), dimension(3) :: dx
            real(F64) :: d2, fukui, weight
            real(F64) :: step_function
            real(F64), parameter :: beta = 1000.0_F64
            !
            ! Compute the dipole moment vector
            !
            call vector_rho(dx, dmatax, dmatay, dmataz, orbval, shellidx, n0, deltak)
            !
            ! DX is the vector defined in Eq. 15 in Ref. [1].
            ! dx = (\frac{1}{\rho_\sigma} \sum_{ij} r_{ij\sigma} \psi_{i\sigma} \psi_{j\sigma}) - r
            !
            dx(1) = dx(1) / rhoa - x
            dx(2) = dx(2) / rhoa - y
            dx(3) = dx(3) / rhoa - z
            !
            ! D2 is the function defined in Eq. 24 in Ref. [1].
            !
            d2 = rhoa * (dx(1)**2 + dx(2)**2 + dx(3)**2)
            
            call scalar_rho(fukui, fukui_matrix, orbval, shellidx, n0, deltak)

            step_function = ONE - ONE / (exp(beta * (fukui / (TWO * rhoa) - FRAC12)) + ONE)

            weight = grid_weight * rhoa * step_function
            a(1) = a(1) + grid_weight * rhoa * d2
            a(3) = a(3) + grid_weight * rhoa
            a(4) = a(4) + grid_weight * fukui
      end subroutine xhole_dist_fukui


      pure subroutine gdd_grac_damping(numerator, denominator, weight, x, y, z, rhoa, &
            rho_tot, sigma_tot, orbval, shellidx, n0, deltak, dmatax, dmatay, dmataz)
            ! -----------------------------------------------------------------------------
            ! Compute contribution to the mean square of the exchange-hole
            ! dipole moment, <d_x^2>, at the given grid point. The exchange-hole dipole
            ! moment is averaged with a normalized probability distribution. The exact
            ! HF-like exchange hole is used to compute the dipole moment, see Ref. [1].
            !
            ! This variant of the GDD method uses the GRAC damping function, Ref. [2]
            !
            ! This subroutine should be called only if RHOA is numerically non-negligible.
            ! -----------------------------------------------------------------------------
            ! 1. Becke, A. D., Johnson, E. R., A density-functional model of
            !    the dispersion interaction, J. Chem. Phys. 123, 154101 (2005)
            !    DOI: 10.1063/1.2065267
            ! 2. M. Gruning et al., Shape corrections to exchange-correlation
            !    potentials by gradient-regulated seamless connection of model
            !    potentials for inner and outer region, J. Chem. Phys. 114, 652 (2001),
            !    DOI: 10.1063/1.1327260
            !
            real(F64), intent(inout)               :: numerator
            real(F64), intent(inout)               :: denominator
            real(F64), intent(in)                  :: weight
            real(F64), intent(in)                  :: x
            real(F64), intent(in)                  :: y
            real(F64), intent(in)                  :: z
            real(F64), intent(in)                  :: rhoa
            real(F64), intent(in)                  :: rho_tot
            real(F64), intent(in)                  :: sigma_tot
            real(F64), dimension(:), intent(in)    :: orbval
            integer, dimension(:), intent(in)      :: shellidx
            integer, intent(in)                    :: n0
            integer, intent(in)                    :: deltak
            real(F64), dimension(:, :), intent(in) :: dmatax
            real(F64), dimension(:, :), intent(in) :: dmatay
            real(F64), dimension(:, :), intent(in) :: dmataz

            real(F64) :: f
            real(F64), dimension(3) :: dx
            real(F64) :: d2
            real(F64), parameter :: alpha = GDD_GRAC_ALPHA
            real(F64), parameter :: beta = GDD_GRAC_BETA
            !
            ! Damping function (0.0 in the bulk density, 1.0 in the density tail).
            ! Note that the total density and total sigma is passed to GRAC_DAMPING.
            !
            call grac_damping(f, alpha, beta, rho_tot, sigma_tot)
            !
            ! Dipole moment vector of the exact exchange hole
            !
            call vector_rho(dx, dmatax, dmatay, dmataz, orbval, shellidx, n0, deltak)
            !
            ! DX is the vector defined in Eq. [15] in Ref. [1].
            ! dx = (\frac{1}{\rho_\sigma} \sum_{ij} r_{ij\sigma} \psi_{i\sigma} \psi_{j\sigma}) - r
            !
            dx(1) = dx(1) / rhoa - x
            dx(2) = dx(2) / rhoa - y
            dx(3) = dx(3) / rhoa - z
            !
            ! D2 is the function defined in Eq. 24 in Ref. [1].
            !
            d2 = rhoa * (dx(1)**2 + dx(2)**2 + dx(3)**2)
            
            numerator = numerator + weight * d2 * f
            denominator = denominator + weight * rhoa * f
      end subroutine gdd_grac_damping


      pure subroutine uexchdipole_exx(d2, x, y, z, rhoa, rhob, orbval, shellidx, n0, deltak, &
            dmatax, dmatay, dmataz, dmatbx, dmatby, dmatbz)
            ! ------------------------------------------------------------------------
            ! Compute contribution to the mean square of the exchange-hole
            ! dipole moment, <d_x^2>, at a given grid point. Here, the exact HF-like
            ! exchange hole is used to compute the dipole moment, see Ref. [1].
            ! This subroutine is applicable to spin-polarized systems.
            ! -------------------------------------------------------------------------
            ! 1. Becke, A. D., Johnson, E. R., A density-functional model of
            !    the dispersion interaction, J. Chem. Phys. 123, 154101 (2005)
            !
            real(F64), intent(out)                 :: d2
            real(F64), intent(in)                  :: x
            real(F64), intent(in)                  :: y
            real(F64), intent(in)                  :: z
            real(F64), intent(in)                  :: rhoa
            real(F64), intent(in)                  :: rhob
            real(F64), dimension(:), intent(in)    :: orbval
            integer, dimension(:), intent(in)             :: shellidx
            integer, intent(in)                           :: n0
            integer, intent(in)                           :: deltak
            real(F64), dimension(:, :), intent(in) :: dmatax
            real(F64), dimension(:, :), intent(in) :: dmatay
            real(F64), dimension(:, :), intent(in) :: dmataz
            real(F64), dimension(:, :), intent(in) :: dmatbx
            real(F64), dimension(:, :), intent(in) :: dmatby
            real(F64), dimension(:, :), intent(in) :: dmatbz
            
            real(F64), dimension(3) :: dax, dbx
            real(F64) :: d2a, d2b
            
            if (rhoa > EXCHDIP_EXX_RHO_TOL) then
                  if (rhob > EXCHDIP_EXX_RHO_TOL) then
                        call uvector_rho(dax, dbx, dmatax, dmatay, dmataz, dmatbx, &
                              dmatby, dmatbz, orbval, shellidx, n0, deltak)
                        !
                        ! DSX equal to the vector defined in Eq. [15] in Ref. [1].
                        ! dsx = (\frac{1}{\rho_\sigma} \sum_{ij} r_{ij\sigma} \psi_{i\sigma} \psi_{j\sigma}) - r
                        !
                        dax(1) = dax(1) / rhoa - x
                        dax(2) = dax(2) / rhoa - y
                        dax(3) = dax(3) / rhoa - z
                        !
                        ! D2 is the function defined in Eq. 24 in Ref. [1].
                        !
                        d2a = rhoa * (dax(1)**2 + dax(2)**2 + dax(3)**2)
                        
                        dbx(1) = dbx(1) / rhob - x
                        dbx(2) = dbx(2) / rhob - y
                        dbx(3) = dbx(3) / rhob - z
                        d2b = rhob * (dbx(1)**2 + dbx(2)**2 + dbx(3)**2)
                  else
                        call vector_rho(dax, dmatax, dmatay, dmataz, orbval, shellidx, n0, deltak)
                        dax(1) = dax(1) / rhoa - x
                        dax(2) = dax(2) / rhoa - y
                        dax(3) = dax(3) / rhoa - z
                        d2a = rhoa * (dax(1)**2 + dax(2)**2 + dax(3)**2)
                        d2b = ZERO
                  end if
            else
                  if (rhob > EXCHDIP_EXX_RHO_TOL) then
                        call vector_rho(dbx, dmatbx, dmatby, dmatbz, orbval, shellidx, n0, deltak)
                        dbx(1) = dbx(1) / rhob - x
                        dbx(2) = dbx(2) / rhob - y
                        dbx(3) = dbx(3) / rhob - z
                        d2a = ZERO
                        d2b = rhob * (dbx(1)**2 + dbx(2)**2 + dbx(3)**2)
                  else
                        d2a = ZERO
                        d2b = ZERO
                  end if
            end if
            d2 = d2a + d2b
      end subroutine uexchdipole_exx


      pure subroutine exchdipole_homo_br89(d2, rho_homoa, rhoa, sigma_aa, lapla, taua)
            ! -------------------------------------------------------------------
            ! 1. Becke, A., Roussel, M., Exchange holes in inhomogeneous
            !    systems: A coordinate-space model, Phys. Rev. A, 39, 3761 (1989)
            !
            real(F64), intent(out) :: d2
            real(F64), intent(in)  :: rho_homoa
            real(F64), intent(in)  :: rhoa
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: lapla
            real(F64), intent(in)  :: taua

            real(F64) :: eps, vrho, vsigma, vlapl, vtau
            real(F64) :: ux, urho, usigma, ulapl, utau, dipole

            call uxbr89(rhoa, sigma_aa, lapla, taua, eps, vrho, &
                  vsigma, vlapl, vtau, ux, urho, usigma, ulapl, utau, &
                  dipole)
            d2 = rho_homoa * dipole**2
      end subroutine exchdipole_homo_br89


      pure subroutine vector_rho(rho, dmatx, dmaty, dmatz, orbval, shellidx, n0, deltak)
            !
            ! Compute the vector function RHO defined at a grid point:
            ! RHO(1) = \sum_{PQ} \phi_p DMATX(P, Q) \phi_q,
            ! RHO(2) = \sum_{PQ} \phi_p DMATY(P, Q) \phi_q,
            ! RHO(3) = \sum_{PQ} \phi_p DMATZ(P, Q) \phi_q,
            ! where DMATX, DMATY, DMATZ are symmetric real matrices. \phi_p
            ! and \phi_q denote the values of P-th and Q-th Cartesian
            ! Gaussian AOs at a given grid point. Both lower and upper
            ! triangles of the DMAT matrices are referenced.
            !
            real(F64), dimension(3), intent(out)   :: rho
            real(F64), dimension(:, :), intent(in) :: dmatx
            real(F64), dimension(:, :), intent(in) :: dmaty
            real(F64), dimension(:, :), intent(in) :: dmatz
            real(F64), dimension(:), intent(in)    :: orbval
            integer, dimension(:), intent(in)      :: shellidx
            integer, intent(in)                    :: n0
            integer, intent(in)                    :: deltak

            integer :: k, l, u, uu, p, q, v, vv
            real(F64) :: p_val, q_val
            real(F64) :: sumx_0, sumy_0, sumz_0
            real(F64) :: tx, ty, tz

            rho = ZERO
            k = 1
            shloop1: do uu = 1, n0
                  u = shellidx(uu)
                  ploop: do p =  shpos(u), shpos(u + 1) - 1
                        p_val  = orbval(k)
                        l = k
                        k = k + deltak
                        !
                        ! Diagonal element
                        !
                        tx = FRAC12 * dmatx(p, p)
                        ty = FRAC12 * dmaty(p, p)
                        tz = FRAC12 * dmatz(p, p)
                        sumx_0 = tx * p_val
                        sumy_0 = ty * p_val
                        sumz_0 = tz * p_val
                        l = l + deltak
                        !
                        ! Diagonal block
                        !
                        do q = p + 1, shpos(u + 1) - 1
                              q_val  = orbval(l)
                              l = l + deltak
                              sumx_0 = sumx_0 + dmatx(q, p) * q_val
                              sumy_0 = sumy_0 + dmaty(q, p) * q_val
                              sumz_0 = sumz_0 + dmatz(q, p) * q_val
                        end do
                        do vv = uu + 1, n0
                              v = shellidx(vv)
                              do q = shpos(v), shpos(v + 1) - 1
                                    q_val  = orbval(l)
                                    l = l + deltak
                                    sumx_0 = sumx_0 + dmatx(q, p) * q_val
                                    sumy_0 = sumy_0 + dmaty(q, p) * q_val
                                    sumz_0 = sumz_0 + dmatz(q, p) * q_val
                              end do
                        end do
                        rho(1) = rho(1) + p_val * sumx_0
                        rho(2) = rho(2) + p_val * sumy_0
                        rho(3) = rho(3) + p_val * sumz_0
                  end do ploop
            end do shloop1
            rho(1) = TWO * rho(1)
            rho(2) = TWO * rho(2)
            rho(3) = TWO * rho(3)
      end subroutine vector_rho


      pure subroutine uvector_rho(rhoa, rhob, dmatax, dmatay, dmataz, dmatbx, &
            dmatby, dmatbz, orbval, shellidx, n0, deltak)
            !
            ! Compute the vector function RHO vector defined at every
            ! grid point as:
            ! RHOS(1) = \sum_{PQ} \phi_p DMATSX(P, Q) \phi_q,
            ! RHOS(2) = \sum_{PQ} \phi_p DMATSY(P, Q) \phi_q,
            ! RHOS(3) = \sum_{PQ} \phi_p DMATSZ(P, Q) \phi_q,
            ! where DMATSX, DMATSY, DMATSZ are symmetric real matrices.
            ! S is the spin index, S = A, B. \phi_p and \phi_q denote
            ! the values of P-th and Q-th Cartesian Gaussian AOs at a
            ! given grid point. Both lower and upper triangles of the
            ! DMAT matrices are referenced.
            !
            real(F64), dimension(3), intent(out)   :: rhoa
            real(F64), dimension(3), intent(out)   :: rhob
            real(F64), dimension(:, :), intent(in) :: dmatax
            real(F64), dimension(:, :), intent(in) :: dmatay
            real(F64), dimension(:, :), intent(in) :: dmataz
            real(F64), dimension(:, :), intent(in) :: dmatbx
            real(F64), dimension(:, :), intent(in) :: dmatby
            real(F64), dimension(:, :), intent(in) :: dmatbz
            real(F64), dimension(:), intent(in)    :: orbval
            integer, dimension(:), intent(in)             :: shellidx
            integer, intent(in)                           :: n0
            integer, intent(in)                           :: deltak

            integer :: k, l, u, uu, p, q, v, vv
            real(F64) :: p_val, q_val
            real(F64) :: sumax_0, sumay_0, sumaz_0
            real(F64) :: sumbx_0, sumby_0, sumbz_0
            real(F64) :: tax, tay, taz
            real(F64) :: tbx, tby, tbz

            rhoa = ZERO
            rhob = ZERO
            k = 1
            shloop1: do uu = 1, n0
                  u = shellidx(uu)
                  ploop: do p =  shpos(u), shpos(u + 1) - 1
                        p_val  = orbval(k)
                        l = k
                        k = k + deltak
                        !
                        ! Diagonal element
                        !
                        tax = FRAC12 * dmatax(p, p)
                        tay = FRAC12 * dmatay(p, p)
                        taz = FRAC12 * dmataz(p, p)

                        tbx = FRAC12 * dmatbx(p, p)
                        tby = FRAC12 * dmatby(p, p)
                        tbz = FRAC12 * dmatbz(p, p)

                        sumax_0 = tax * p_val
                        sumay_0 = tay * p_val
                        sumaz_0 = taz * p_val

                        sumbx_0 = tbx * p_val
                        sumby_0 = tby * p_val
                        sumbz_0 = tbz * p_val

                        l = l + deltak
                        !
                        ! Diagonal block
                        !
                        do q = p + 1, shpos(u + 1) - 1
                              q_val  = orbval(l)
                              l = l + deltak
                              sumax_0 = sumax_0 + dmatax(q, p) * q_val
                              sumay_0 = sumay_0 + dmatay(q, p) * q_val
                              sumaz_0 = sumaz_0 + dmataz(q, p) * q_val

                              sumbx_0 = sumbx_0 + dmatbx(q, p) * q_val
                              sumby_0 = sumby_0 + dmatby(q, p) * q_val
                              sumbz_0 = sumbz_0 + dmatbz(q, p) * q_val
                        end do
                        do vv = uu + 1, n0
                              v = shellidx(vv)
                              do q = shpos(v), shpos(v + 1) - 1
                                    q_val  = orbval(l)
                                    l = l + deltak
                                    sumax_0 = sumax_0 + dmatax(q, p) * q_val
                                    sumay_0 = sumay_0 + dmatay(q, p) * q_val
                                    sumaz_0 = sumaz_0 + dmataz(q, p) * q_val

                                    sumbx_0 = sumbx_0 + dmatbx(q, p) * q_val
                                    sumby_0 = sumby_0 + dmatby(q, p) * q_val
                                    sumbz_0 = sumbz_0 + dmatbz(q, p) * q_val
                              end do
                        end do
                        rhoa(1) = rhoa(1) + p_val * sumax_0
                        rhoa(2) = rhoa(2) + p_val * sumay_0
                        rhoa(3) = rhoa(3) + p_val * sumaz_0

                        rhob(1) = rhob(1) + p_val * sumbx_0
                        rhob(2) = rhob(2) + p_val * sumby_0
                        rhob(3) = rhob(3) + p_val * sumbz_0
                  end do ploop
            end do shloop1
            rhoa(1) = TWO * rhoa(1)
            rhoa(2) = TWO * rhoa(2)
            rhoa(3) = TWO * rhoa(3)

            rhob(1) = TWO * rhob(1)
            rhob(2) = TWO * rhob(2)
            rhob(3) = TWO * rhob(3)
      end subroutine uvector_rho


      pure subroutine scalar_rho(rho, d, orbval, shellidx, n0, deltak)
            !
            ! Compute the scalar function RHO defined at every grid point as:
            ! RHO = \sum_{PQ} \phi_p D(P, Q) \phi_q,
            ! where D is a real symmetric matrix. \phi_p and \phi_q denote
            ! the values of P-th and Q-th atomic orbitals at the given grid point.
            ! Both lower and upper triangles of D are referenced.
            !
            real(F64), intent(out)                 :: rho
            real(F64), dimension(:, :), intent(in) :: d
            real(F64), dimension(:), intent(in)    :: orbval
            integer, dimension(:), intent(in)      :: shellidx
            integer, intent(in)                    :: n0
            integer, intent(in)                    :: deltak

            integer :: k, l, u, uu, p, q, v, vv
            real(F64) :: p_val, q_val
            real(F64) :: sum_0

            rho = ZERO
            k = 1
            shloop1: do uu = 1, n0
                  u = shellidx(uu)
                  ploop: do p =  shpos(u), shpos(u + 1) - 1
                        p_val  = orbval(k)
                        l = k
                        k = k + deltak
                        !
                        ! Diagonal element
                        !
                        sum_0 = FRAC12 * d(p, p) * p_val
                        l = l + deltak
                        !
                        ! Diagonal block
                        !
                        do q = p + 1, shpos(u + 1) - 1
                              q_val  = orbval(l)
                              l = l + deltak
                              sum_0 = sum_0 + d(q, p) * q_val
                        end do
                        do vv = uu + 1, n0
                              v = shellidx(vv)
                              do q = shpos(v), shpos(v + 1) - 1
                                    q_val  = orbval(l)
                                    l = l + deltak
                                    sum_0 = sum_0 + d(q, p) * q_val
                              end do
                        end do
                        rho = rho + p_val * sum_0
                  end do ploop
            end do shloop1
            rho = TWO * rho
      end subroutine scalar_rho


      pure subroutine binary_search(list, n, thresh, idx)
            ! ----------------------------------------------------------
            ! Perform binary search in a list of numbers sorted in
            ! decreasing order. The output, IDX value, is the index
            ! of the smallest value belonging to LIST which is equal
            ! of greater than THRESH. IDX equal to N is returned if
            ! all numbers belonging to LIST are equal or greater than
            ! THRESH. If multiple numbers are exactly equal to THRESH,
            ! then the IDX output is set equal to the largest possible
            ! index satisfying LIST(IDX) >= THRESH.
            ! (Monospaced font should be used to read the following
            ! text.)
            ! THRESH = 0.4
            ! LIST = [0.9, 0.8, 0.5, 0.4, 0.1, 0.0]
            !        IDX=4 ------------>| |<-- BELOW THRESHOLD
            ! THRESH = 0.45
            ! LIST = [0.9, 0.8, 0.5, 0.4, 0.1, 0.0]
            !        IDX=3 ------->| |<---- BELOW THRESHOLD
            ! THRESH = 0.1
            ! LIST = [0.2, 0.1, 0.1, 0.0]
            ! IDX=3 -------------->| |<----- BELOW THRESHOLD
            !
            !
            real(F64), dimension(:), intent(in) :: list
            integer, intent(in) :: n
            real(F64), intent(in) :: thresh
            integer, intent(out) :: idx

            integer :: j, k

            if (list(n) .ge. thresh) then
                  idx = n
            else
                  idx = 1
                  j = n
                  bisection: do
                        k = (idx + j) / 2
                        if (thresh .le. list(k)) then
                              idx = k
                        else
                              j = k
                        end if
                        if (idx + 1 .ge. j) then
                              exit bisection
                        end if
                  end do bisection
            end if
      end subroutine binary_search


      pure subroutine grac_damping(f, alpha, beta, rho, sigma)
            ! ---------------------------------------------------------------------------
            ! Compute the damping function of the gradient-regulated connection scheme
            ! by Gruning et al. (Ref. 1). This damping function vanishes in the bulk
            ! density and is close to 1.0 in the density tail:
            !
            ! f = 1 / (1 + exp(-alpha * (x - beta))),
            ! x = |\nabla rho| / rho^{4/3},
            !
            ! where Gruning et al. recommended the following numerical parameters:
            !
            ! alpha = 0.5,
            ! beta = 40.0.
            ! ---------------------------------------------------------------------------
            ! F
            !        Value of the GRAC damping function at the given point
            ! ALPHA
            ! BETA
            !        Numerical parameters
            ! RHO
            !        Electron density
            ! SIGMA
            !        Square of the density gradient, SIGMA = (\nabla rho) * (\nabla rho)
            ! ---------------------------------------------------------------------------
            ! 1. M. Gruning et al., Shape corrections to exchange-correlation
            !    potentials by gradient-regulated seamless connection of model
            !    potentials for inner and outer region, J. Chem. Phys. 114, 652 (2001),
            !    DOI: 10.1063/1.1327260
            !
            real(F64), intent(out) :: f
            real(F64), intent(in)  :: alpha
            real(F64), intent(in)  :: beta
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            
            real(F64) :: rho43, g, e

            rho43 = rho**FRAC43
            g = sqrt(sigma)
            e = exp(-alpha * (g / rho43 - beta))
            f = ONE / (ONE + e)
      end subroutine grac_damping
end module auxint
